/******************************************************************************
*
* Copyright Saab AB, 2012 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
*
*******************************************************************************
*
* This file is part of Safir SDK Core.
*
* Safir SDK Core is free software: you can redistribute it and/or modify
* it under the terms of version 3 of the GNU General Public License as
* published by the Free Software Foundation.
*
* Safir SDK Core is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/
#include <Safir/Dob/Internal/Communication.h>
#include <Safir/Utilities/ProcessInfo.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Utilities/Internal/MakeUnique.h>
#include <Safir/Utilities/CrashReporter.h>
#include <Safir/Utilities/Internal/ConfigReader.h>
#include <Safir/Dob/Internal/SystemPicture.h>
#include <Safir/Utilities/Internal/Id.h>
#include <iostream>
#include <boost/atomic.hpp>
#include <fstream>
#include <map>
#include <boost/lexical_cast.hpp>

//disable warnings in boost
#if defined _MSC_VER
#  pragma warning (push)
#  pragma warning (disable : 4100 4267 4505)
#endif

#include <boost/program_options.hpp>
#include <boost/asio.hpp>
#include <boost/thread.hpp>

#if defined _MSC_VER
#  pragma warning (pop)
//and disable a warning that happens on instantiation
#  pragma warning (disable : 4505)
#endif


std::wostream& operator<<(std::wostream& out, const std::string& str)
{
    return out << str.c_str();
}

std::wostream& operator<<(std::wostream& out, const boost::program_options::options_description& opt)
{
    std::ostringstream ostr;
    ostr << opt;
    return out << ostr.str().c_str();
}

class ProgramOptions
{
public:
    ProgramOptions(int argc, char* argv[])
        : parseOk(false)
    {
        using namespace boost::program_options;
        options_description options("Options");
        options.add_options()
            ("help,h", "show help message")
            ("control-address,c",
             value<std::string>(&controlAddress)->default_value("127.0.0.1:30000"),
             "Address and port of the control channel")
            ("data-address,d",
             value<std::string>(&dataAddress)->default_value("127.0.0.1:40000"),
             "Address and port of the data channel")
            ("seed,s",
             value<std::vector<std::string> >(&seeds),
             "Seed address (can be specified multiple times). Pairs of address:port to control channel.")
            ("name,n",
             value<std::string>(&name)->default_value("<not set>", ""),
             "A nice name for the node, for presentation purposes only")
            ("force-id",
             value<boost::int64_t>(&id)->default_value(LlufId_GenerateRandom64(), ""),
             "Override the automatically generated node id. For debugging/testing purposes only.")
            ("check-incarnation",
             bool_switch(&checkIncarnation)->default_value(false),
             "Perform checking on incarnation ids");

        variables_map vm;

        try
        {
            store(command_line_parser(argc, argv).
                  options(options).run(), vm);
            notify(vm);
        }
        catch (const std::exception& exc)
        {
            std::wcerr << "Error parsing command line: " << exc.what() << "\n" << std::endl;
            ShowHelp(options);
            return;
        }

        if (vm.count("help"))
        {
            ShowHelp(options);
            return;
        }


        parseOk = true;
    }
    bool parseOk;

    std::string controlAddress;
    std::string dataAddress;
    std::vector<std::string> seeds;
    boost::int64_t id;
    std::string name;
    bool checkIncarnation;
private:
    static void ShowHelp(const boost::program_options::options_description& desc)
    {
        std::wcout << std::boolalpha
                   << "Usage: control_stub [OPTIONS]\n"
                   << desc << "\n"
                   << std::endl;
    }

};

class IncarnationChecker
{
public:
    explicit IncarnationChecker(bool enabled)
        : m_enabled(enabled)
        , m_filename("last_incarnation" + Safir::Utilities::Internal::Expansion::GetSafirInstanceSuffix() + ".txt")
        , m_calls(0)
        , m_lastIncarnation(0)
    {
        std::ifstream f(m_filename);
        if (f.good())
        {
            f >> m_lastIncarnation;
            std::wcout << "Last incarnation read from file: " << m_lastIncarnation << std::endl;
        }
    }

    bool Check(const int64_t id)
    {
        ++m_calls;
        if (m_calls > 1)
        {
            throw std::logic_error("Expect only one call to IncarnationChecker::Check");
        }

        std::wcout << "Checking incarnation id " << id << std::endl;
        if (!m_enabled)
        {
            std::wcout << "Checking disabled, return true" << std::endl;
            return true;
        }

        if (m_lastIncarnation == 0)
        {
            std::wcout << "Have no previous incarnation, writing to file and returning true" << std::endl;
            std::ofstream f(m_filename);
            f << id;
            return true;
        }
        else if (m_lastIncarnation == id)
        {
            return true;
        }
        else
        {
            throw std::logic_error("Incarnation mismatch! Expected "
                                   + boost::lexical_cast<std::string>(m_lastIncarnation)
                                   + ", got "
                                   + boost::lexical_cast<std::string>(id));
        }
    }
private:
    bool m_enabled;
    std::string m_filename;
    int m_calls;
    int64_t m_lastIncarnation;
};

int main(int argc, char * argv[])
{
    std::wcout << "Pid: " << Safir::Utilities::ProcessInfo::GetPid() << std::endl;

    try
    {
        //ensure call to CrashReporter::Stop at application exit
        boost::shared_ptr<void> crGuard(static_cast<void*>(0),
                                        [](void*){Safir::Utilities::CrashReporter::Stop();});
        Safir::Utilities::CrashReporter::Start();

        const ProgramOptions options(argc, argv);
        if (!options.parseOk)
        {
            return 1;
        }

        boost::asio::io_service ioService;
        //make some work to stop io_service from exiting.
        auto work = Safir::make_unique<boost::asio::io_service::work>(ioService);

        std::vector<Safir::Dob::Internal::Com::NodeTypeDefinition> commNodeTypes;
        std::map<boost::int64_t, Safir::Dob::Internal::SP::NodeType> spNodeTypes;

        
        commNodeTypes.push_back(Safir::Dob::Internal::Com::NodeTypeDefinition(1,
                                                                              "NodeTypeA",
                                                                              "", //no multicast
                                                                              "", //no multicast
                                                                              1000,
                                                                              20,
                                                                              15));   

        spNodeTypes.insert(std::make_pair(1,
                                          Safir::Dob::Internal::SP::NodeType(1,
                                                                             "NodeTypeA",
                                                                             false,
                                                                             boost::chrono::milliseconds(1000),
                                                                             15,
                                                                             boost::chrono::milliseconds(20))));

        commNodeTypes.push_back(Safir::Dob::Internal::Com::NodeTypeDefinition(2,
                                                                              "NodeTypeB",
                                                                              "", //no multicast
                                                                              "", //no multicast
                                                                              2000,
                                                                              50,
                                                                              8));

        spNodeTypes.insert(std::make_pair(2,
                                          Safir::Dob::Internal::SP::NodeType(2,
                                                                             "NodeTypeB",
                                                                             false,
                                                                             boost::chrono::milliseconds(2000),
                                                                             8,
                                                                             boost::chrono::milliseconds(50))));


        std::wcout << "Creating Communication instance" << std::endl;
        Safir::Dob::Internal::Com::Communication communication(Safir::Dob::Internal::Com::controlModeTag,
                                                               ioService,
                                                               options.name,
                                                               options.id,
                                                               1,
                                                               options.controlAddress,
                                                               options.dataAddress,
                                                               commNodeTypes);

        std::wcout << "Injecting seeds" << std::endl;
        communication.InjectSeeds(options.seeds);

        IncarnationChecker incarnationChecker(options.checkIncarnation);

        std::wcout << "Creating SystemPicture instance" << std::endl;
        Safir::Dob::Internal::SP::SystemPicture sp(Safir::Dob::Internal::SP::master_tag,
                                                   ioService,
                                                   communication,
                                                   options.name,
                                                   options.id,
                                                   1,
                                                   std::move(spNodeTypes),
                                                   [&incarnationChecker](const int64_t id){return incarnationChecker.Check(id);});

        std::wcout << "Starting SystemState subscription" << std::endl;

        // Start subscription to system state changes from SP
        sp.StartStateSubscription([](const Safir::Dob::Internal::SP::SystemState& data)
                                  {
                                      std::wcout << "Got new state:\n" << data << std::endl;
                                  });


        std::wcout << "Starting Communication" << std::endl;
        communication.Start();

        boost::asio::signal_set signalSet(ioService);

#if defined (_WIN32)
        signalSet.add(SIGABRT);
        signalSet.add(SIGBREAK);
        signalSet.add(SIGINT);
        signalSet.add(SIGTERM);
#else
        signalSet.add(SIGQUIT);
        signalSet.add(SIGINT);
        signalSet.add(SIGTERM);
#endif

        std::wcout << "Registering signal handler" << std::endl;
        signalSet.async_wait([&sp,&work,&communication](const boost::system::error_code& error,
                                                        const int signal_number)
                             {
                                 std::wcout << "Got signal " << signal_number << std::endl;
                                 if (!!error) //fix for ws2012 warning
                                 {
                                     SEND_SYSTEM_LOG(Error,
                                                     << "Got a signals error: " << error);
                                 }
                                 std::wcout << "Stopping SystemPicture" << std::endl;
                                 sp.Stop();
                                 std::wcout << "Stopping Communication" << std::endl;
                                 communication.Stop();
                                 std::wcout << "Resetting work" << std::endl;
                                 work.reset();

                             }
                             );

        boost::atomic<bool> success(true);

        const auto run = [&ioService,&success]
            {
                try
                {
                    ioService.run();
                    std::wcout << "Thread exiting" << std::endl;
                    return;
                }
                catch (const std::exception & exc)
                {
                    SEND_SYSTEM_LOG(Alert,
                                    << "Caught 'std::exception' exception from io_service.run(): "
                                    << "  '" << exc.what() << "'.");
                    success.exchange(false);
                }
                catch (...)
                {
                    SEND_SYSTEM_LOG(Alert,
                                    << "Caught '...' exception from io_service.run().");
                    success.exchange(false);
                }
            };



        std::wcout << "Launching io_service" << std::endl;
        boost::thread_group threads;
        for (int i = 0; i < 2; ++i)
        {
            threads.create_thread(run);
        }

        run();

        threads.join_all();
        std::wcout << "Exiting..." << std::endl;
        return 0;
    }
    catch(std::exception& e)
    {
        std::wcout << "Caught exception: " << e.what() << std::endl;
        return 1;
    }
}
