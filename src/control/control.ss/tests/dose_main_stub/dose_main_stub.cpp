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
#include <Safir/Dob/Internal/ControlConfig.h>
#include <Safir/Dob/Internal/DoseMainCmd.h>
#include <iostream>
#include <map>
#include <boost/thread.hpp>

//disable warnings in boost
#if defined _MSC_VER
#  pragma warning (push)
#  pragma warning (disable : 4100 4267)
#endif

#include <boost/program_options.hpp>
#include <boost/asio.hpp>
#include <boost/asio/steady_timer.hpp>

#if defined _MSC_VER
#  pragma warning (pop)
#endif

namespace SP = Safir::Dob::Internal::SP;
namespace Com = Safir::Dob::Internal::Com;
namespace Control = Safir::Dob::Internal::Control;

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
                ("node-type,t",
                 value<std::string>(&nodeType)->default_value("Server"),
                 "Node type of this node")
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

        for (const auto& nt: config.GetNodeTypes())
        {
            if (nt.name == nodeType)
            {
                nodeTypeId = nt.id;
                break;
            }
        }

        if (nodeTypeId == -1)
        {
            std::wcerr << "Invalid nodeType specified" << std::endl;
            return;
        }

        parseOk = true;
    }

    bool parseOk;
    
    const Safir::Dob::Internal::Control::Config config;

    std::string dataAddress;
    boost::int64_t id;
    std::string name;
    std::string nodeType;
    boost::int64_t nodeTypeId = -1;
private:
    static void ShowHelp(const boost::program_options::options_description& desc)
    {
        std::wcout << std::boolalpha
                   << "Usage: dose_main_stub [OPTIONS]\n"
                   << desc << "\n"
                   << "Examples:\n"
                   << "  Listen to loopback address and port 43000.\n"
                   << "    dose_main_stub --data-address='127.0.0.1:43000'\n"
                   << "  As above, but all addresses.\n"
                   << "    dose_main_stub --data-address='0.0.0.0:43000'\n"
                   << "  Listen to ipv6 loopback.\n"
                   << "    dose_main_stub --data-address='[::1]:43000'\n"
                   << std::endl;
    }

};

// Communication callbacks
void QueueNotFullCb(int64_t nodeTypeId)
{
    std::wcout << "dose_main got queue not full indication for nodetypeId " << nodeTypeId << std::endl;
}

boost::shared_ptr<char[]> StrToPtr(const std::string& s)
{
    boost::shared_ptr<char[]> ptr(new char[s.length()]);

    memcpy(ptr.get(), s.c_str(), s.length());
    return ptr;
}

int main(int argc, char * argv[])
{
    const ProgramOptions options(argc, argv);
    if (!options.parseOk)
    {
        return 1;
    }

    bool running {true};

    boost::asio::io_service ioService;

    boost::asio::strand strand{ioService};

    boost::asio::steady_timer timer{ioService};

    // Make some work to stop io_service from exiting.
    auto work = Safir::make_unique<boost::asio::io_service::work>(ioService);

    std::vector<Com::NodeTypeDefinition> commNodeTypes;

    for (const auto& nt: options.config.GetNodeTypes())
    {
        commNodeTypes.push_back({nt.id, 
                                 nt.name,
                                 nt.multicastAddressControl,
                                 nt.multicastAddressData,
                                 nt.heartbeatInterval,
                                 nt.retryTimeout});
    }
    
    Com::Communication communication(Com::dataModeTag,
                                     ioService,
                                     options.name,
                                     options.id,
                                     options.nodeTypeId,
                                     options.dataAddress,
                                     commNodeTypes);
    

    std::map<boost::int64_t, SP::NodeType> spNodeTypes;
    
    for (const auto& nt: options.config.GetNodeTypes())
    {
        spNodeTypes.insert(std::make_pair(nt.id,
                                          SP::NodeType(nt.id,
                                                       nt.name,
                                                       nt.isLight,
                                                       boost::chrono::milliseconds(nt.heartbeatInterval),
                                                       nt.maxLostHeartbeats,
                                                       boost::chrono::milliseconds(nt.retryTimeout))));
    }


    SP::SystemPicture sp(SP::slave_tag,
                         ioService,
                         communication,
                         options.name,
                         options.id,
                         options.nodeTypeId,
                         options.dataAddress,
                         std::move(spNodeTypes));

    communication.SetDataReceiver([](int64_t fromNodeId,
                                     int64_t fromNodeType,
                                     const boost::shared_ptr<char[]>& data,
                                     size_t size)
                                    {
                                        std::string msg(data.get(), size);
                                        std::wcout << " dose_main_stub received " << msg << " from Node Id " << fromNodeId
                                                   << " of Node Type " << fromNodeType << std::endl;
                                    },
                                    12345);

    communication.Start();

    int64_t savedNodeTypeId;
    int counter;

    timer.expires_from_now(boost::chrono::milliseconds(2000));

    std::function<void (const boost::system::error_code&)> onTimeout =
            [&running, &timer, &strand, &onTimeout, &communication, &savedNodeTypeId, &counter]
            (const boost::system::error_code& error)
            {
               if (!running)
               {
                   return;
               }

               if (error)
               {
                   std::wcout << "OnTimeout error!" << std::endl;
               }

               std::ostringstream output;
               output << "Kalle" << counter;
               ++counter;
               communication.Send(0, // Not sending to specific node type
                                  savedNodeTypeId,
                                  StrToPtr(output.str()),
                                  5,
                                  12345,
                                  true);

               timer.expires_from_now(boost::chrono::milliseconds(2000));
               timer.async_wait(strand.wrap(onTimeout));
            };

    Control::DoseMainCmdReceiver doseMainCmdReceiver(ioService,
                                                     [&communication,
                                                      &timer,
                                                      &strand,
                                                      &onTimeout,
                                                      &savedNodeTypeId]
                                                     (int64_t requestId,
                                                      const std::string& nodeName,
                                                      int64_t nodeId,
                                                      int64_t nodeTypeId,
                                                      const std::string& dataAddress)
                                                     {
                                                        communication.InjectNode(nodeName,
                                                                                 nodeId,
                                                                                 nodeTypeId,
                                                                                 dataAddress);
                                                        // save nodeTypeId
                                                        savedNodeTypeId = nodeTypeId;

                                                        timer.async_wait(strand.wrap(onTimeout));
                                                     });

    doseMainCmdReceiver.Start();

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

    signalSet.async_wait(strand.wrap([&sp,&work,&communication,&signalSet,&running, &doseMainCmdReceiver]
                                     (const boost::system::error_code& error,
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
                           doseMainCmdReceiver.Stop();
                           work.reset();
                           running = false;
                       }
                       ));




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
