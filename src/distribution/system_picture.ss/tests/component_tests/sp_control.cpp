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
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Utilities/Internal/MakeUnique.h>
#include <Safir/Dob/Internal/SystemPicture.h>
#include <Safir/Utilities/Internal/Id.h>
#include <iostream>
#include <map>
#include <boost/thread.hpp>

//disable warnings in boost
#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable : 4100 4267)
#endif

#include <boost/program_options.hpp>
#include <boost/asio.hpp>

#if defined _MSC_VER
  #pragma warning (pop)
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
             value<std::string>(&controlAddress)->default_value("0.0.0.0:30000"),
             "Address and port of the control channel")
            ("data-address,d",
             value<std::string>(&dataAddress)->default_value("0.0.0.0:40000"),
             "Address and port of the data channel")
            ("seed,s",
             value<std::vector<std::string> >(&seeds),
             "Seed address (can be specified multiple times). Pairs of address:port to control channel.")
            ("name,n",
             value<std::string>(&name)->default_value("<not set>", ""),
             "A nice name for the node, for presentation purposes only")
            ("force-id",
             value<boost::int64_t>(&id)->default_value(LlufId_GenerateRandom64(), ""),
             "Override the automatically generated node id. For debugging/testing purposes only.");

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
private:
    static void ShowHelp(const boost::program_options::options_description& desc)
    {
        std::wcout << std::boolalpha
                   << "Usage: control_stub [OPTIONS]\n"
                   << desc << "\n"
                   << std::endl;
    }

};

int main(int argc, char * argv[])
{
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

    commNodeTypes.push_back({1,
                "NodeTypeA",
                "", //no multicast
                "", //no multicast
                1000,
                20});

    spNodeTypes.insert(std::make_pair(1,
                                      Safir::Dob::Internal::SP::NodeType(1,
                                                                         "NodeTypeA",
                                                                         false,
                                                                         boost::chrono::milliseconds(1000),
                                                                         5,
                                                                         boost::chrono::milliseconds(20))));

    commNodeTypes.push_back({2,
                "NodeTypeB",
                "", //no multicast
                "", //no multicast
                2000,
                50});

    spNodeTypes.insert(std::make_pair(2,
                                      Safir::Dob::Internal::SP::NodeType(2,
                                                                         "NodeTypeB",
                                                                         false,
                                                                         boost::chrono::milliseconds(2000),
                                                                         5,
                                                                         boost::chrono::milliseconds(50))));



    Safir::Dob::Internal::Com::Communication communication(Safir::Dob::Internal::Com::controlModeTag,
                                                           ioService,
                                                           options.name,
                                                           options.id,
                                                           1,
                                                           options.controlAddress,
                                                           options.dataAddress,
                                                           commNodeTypes);

    communication.InjectSeeds(options.seeds);



    Safir::Dob::Internal::SP::SystemPicture sp(Safir::Dob::Internal::SP::master_tag,
                                               ioService,
                                               communication,
                                               options.name,
                                               options.id,
                                               1,
                                               options.controlAddress,
                                               options.dataAddress,
                                               std::move(spNodeTypes));


    // Start subscription to system state changes from SP
    sp.StartStateSubscription([](const Safir::Dob::Internal::SP::SystemState& /*data*/)
                              {

                              });



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

    signalSet.async_wait([&sp,&work,&communication](const boost::system::error_code& error,
                                                    const int signal_number)
                         {
                           lllog(0) << "CTRL: Got signal " << signal_number << std::endl;
                           if (error)
                           {
                               SEND_SYSTEM_LOG(Error,
                                               << "Got a signals error: " << error);
                           }
                           lllog(0) << "CTRL: Stopping SystemPicture" << std::endl;
                           sp.Stop();
                           lllog(0) << "CTRL: Stopping Communication" << std::endl;
                           communication.Stop();
                           lllog(0) << "CTRL: Resetting work" << std::endl;
                           work.reset();
                       }
                       );



    boost::thread_group threads;
    for (int i = 0; i < 9; ++i)
    {
        threads.create_thread([&ioService]{ioService.run();});
    }

    ioService.run();

    lllog(0) << "CTRL: Joining threads" << std::endl;
    threads.join_all();

    lllog(0) << "CTRL: Exiting..." << std::endl;
    return 0;
}
