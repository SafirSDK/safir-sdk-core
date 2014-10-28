/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safir.sourceforge.net)
*
* Created by: Anders Widén / anders.widen@consoden.se
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
#  pragma warning (push)
#  pragma warning (disable : 4100)
#  pragma warning (disable : 4267)
#endif

#include <boost/program_options.hpp>
#include <boost/asio.hpp>
#include <boost/asio/steady_timer.hpp>

#if defined _MSC_VER
#  pragma warning (pop)
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
            ("data-address,d",
             value<std::string>(&dataAddress)->default_value("0.0.0.0:40000"),
             "Address and port of the data channel")
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

    std::string dataAddress;
    boost::int64_t id;
    std::string name;
private:
    static void ShowHelp(const boost::program_options::options_description& desc)
    {
        std::wcout << std::boolalpha
                   << "Usage: dose_main_stub [OPTIONS]\n"
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

    // Make some work to stop io_service from exiting.
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



    Safir::Dob::Internal::Com::Communication communication(Safir::Dob::Internal::Com::dataModeTag,
                                                           ioService,
                                                           options.name,
                                                           options.id,
                                                           1,
                                                           options.dataAddress,
                                                           commNodeTypes);




    Safir::Dob::Internal::SP::SystemPicture sp(Safir::Dob::Internal::SP::slave_tag,
                                               ioService,
                                               communication,
                                               options.name,
                                               options.id,
                                               1,
                                               options.dataAddress,
                                               std::move(spNodeTypes));

    std::set<int64_t> injectedNodes = {options.id}; //consider ourselves already injected
    boost::asio::io_service::strand injectStrand(ioService);

    // Start subscription to system state changes from SP
    sp.StartStateSubscription(injectStrand.wrap
        ([options, &communication, &injectedNodes](const Safir::Dob::Internal::SP::SystemState& data)
         {
             for (int i = 0; i < data.Size(); ++i)
             {
                 //don't inject already injected nodes
                 if (injectedNodes.find(data.Id(i)) != injectedNodes.end())
                 {
                     continue;
                 }
                 lllog(5) << "Injecting node " << data.Name(i) << "(" << data.Id(i)
                          << ") of type " << data.NodeTypeId(i)
                          << " with address " << data.DataAddress(i) << std::endl;
                 communication.InjectNode(data.Name(i),
                                          data.Id(i),
                                          data.NodeTypeId(i),
                                          data.DataAddress(i));

                 injectedNodes.insert(data.Id(i));
             }
         }));

    communication.SetDataReceiver([](const int64_t fromNodeId,
                                     const int64_t fromNodeType,
                                     const boost::shared_ptr<char[]>& data,
                                     const size_t size)
                                  {
                                      if (size != 10000)
                                      {
                                          throw std::logic_error("Received incorrectly sized data!");
                                      }
                                      for (size_t i = 0; i < size; ++i)
                                      {
                                          if (data[i] != 3)
                                          {
                                              throw std::logic_error("Received corrupt data!");
                                          }
                                      }

                                  },
                                  1000100222);

    communication.Start();


    boost::asio::steady_timer sendTimer(ioService);

    const std::function<void(const boost::system::error_code& error)> send =
        [&communication, &sendTimer, &send](const boost::system::error_code& error)
        {
            if (error)
            {
                return;
            }

            const size_t size = 10000;
            const boost::shared_ptr<char[]> data(new char[size]);
            memset(data.get(), 3, size);
            //send the data to both node types.
            communication.Send(0,1,data,size,1000100222,true);
            communication.Send(0,2,data,size,1000100222,true);

            sendTimer.expires_from_now(boost::chrono::milliseconds(10));
            sendTimer.async_wait(send);
        };

    sendTimer.expires_from_now(boost::chrono::milliseconds(10));
    sendTimer.async_wait(send);


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

    signalSet.async_wait([&sp,&work,&communication,&signalSet,&sendTimer](const boost::system::error_code& error,
                                                           const int signal_number)
                       {
                           lllog(3) << "Got signal " << signal_number << std::endl;
                           if (error)
                           {
                               SEND_SYSTEM_LOG(Error,
                                               << "Got a signals error: " << error);
                           }
                           sp.Stop();
                           communication.Stop();
                           sendTimer.cancel();
                           work.reset();
                       }
                       );




    boost::thread_group threads;
    for (int i = 0; i < 9; ++i)
    {
        threads.create_thread([&ioService]{ioService.run();});
    }

    ioService.run();

    threads.join_all();

    lllog(3) << "Exiting..." << std::endl;
    return 0;
}
