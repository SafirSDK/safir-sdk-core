/******************************************************************************
*
* Copyright Saab AB, 2012 (http://safir.sourceforge.net)
* Copyright Consoden AB, 2014 (http://www.consoden.se)
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
#include "SystemStateHandler.h"
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
#  pragma warning (disable : 4100 4267 4251)
#endif

#include <boost/program_options.hpp>
#include <boost/asio.hpp>
#include "boost/process.hpp"
#include "boost/process/mitigate.hpp"

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
            ("node-type,t", 
             value<std::string>(&nodeType)->default_value("Server"), 
             "Node type of this node")
            ("dosemain-name",
             value<std::string>(&doseMainName)->default_value("dose_main_test_stub"),
             "Name of dose_main executable (without file extension)")
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

    std::string controlAddress;
    std::string dataAddress;
    std::vector<std::string> seeds;
    boost::int64_t id;
    std::string name;
    std::string nodeType;
    std::string doseMainName;
    boost::int64_t nodeTypeId = -1;
private:
    static void ShowHelp(const boost::program_options::options_description& desc)
    {
        std::wcout << std::boolalpha
                   << "Usage: control [OPTIONS]\n"
                   << desc << "\n"
                   << "Examples:\n"
                   << "  Listen to loopback address and ports 33000 and 43000.\n"
                   << "    control --control-address='127.0.0.1:33000' --data-address='127.0.0.1:43000'\n"
                   << "  As above, but all addresses.\n"
                   << "    control --control-address='0.0.0.0:33000' --data-address='0.0.0.0:43000'\n"
                   << "  Listen to ipv6 loopback.\n"
                   << "    control --control-address='[::1]:33000' --data-address='[::1]:43000'\n"
                   << std::endl;
    }

};


int main(int argc, char * argv[])
{
    lllog(3) << "CTRL: Started" << std::endl;

    const ProgramOptions options(argc, argv);
    if (!options.parseOk)
    {
        return 1;
    }

    boost::asio::io_service ioService;

    // Make some work to stop io_service from exiting.
    auto work = Safir::make_unique<boost::asio::io_service::work>(ioService);

#if defined(linux) || defined(__linux) || defined(__linux__)
    boost::asio::signal_set sigchldSet(ioService, SIGCHLD);
    sigchldSet.async_wait(
        [](const boost::system::error_code& error, int signalNumber)
        {
            lllog(3) << "CTRL: Got signal " << signalNumber << " ... dose_main has stopped" << std::endl;
            if (error)
            {
                SEND_SYSTEM_LOG(Error,
                                << "Got a signals error: " << error);
            }
            // Wait for dose_main to exit before exit the control program.
            int exitCode;
            ::wait(&exitCode);

            if (exitCode != 0)
            {
                SEND_SYSTEM_LOG(Critical, << "dose_main exited with return code " << exitCode);
            }

        }
    );
#endif

    // Locate and start dose_main
#if defined(_UNICODE) || defined(UNICODE)
    // For now we assume that the name of the dose_main executable contains only ascii characters.
    std::wstring doseMainName = std::wstring(options.doseMainName.begin(), options.doseMainName.end());
    std::wstring doseMainPath;
#else
    std::string doseMainName = options.doseMainName;
    std::string doseMainPath;

#endif
    doseMainPath = boost::process::search_path(doseMainName);
    if (doseMainPath.empty())
    {
        std::ostringstream os;
        os << "CTRL: Can't find " << options.doseMainName << " in PATH" << std::endl;
        SEND_SYSTEM_LOG(Error, << os.str().c_str());
        throw std::logic_error(os.str().c_str());
    }

    boost::system::error_code ec;

    boost::process::child dose_main =
    boost::process::execute
            (boost::process::initializers::run_exe(doseMainPath),
             boost::process::initializers::set_on_error(ec),
             boost::process::initializers::inherit_env()
#if defined(linux) || defined(__linux) || defined(__linux__)
             ,boost::process::initializers::notify_io_service(ioService)
#endif
            );

    (void)dose_main;  // to keep compilers from warning about unused variable

    if (ec)
    {
        std::cout << "Error run_exe: " << ec.message() << std::endl;
    }

#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
    boost::asio::windows::object_handle handle(ioService, dose_main.process_handle());

    handle.async_wait(
        [&handle](const boost::system::error_code&)
        {
            lllog(3) << "CTRL: dose_main has stopped" << std::endl;

            DWORD exitCode;
            ::GetExitCodeProcess(handle.native(), &exitCode);

            if (exitCode != 0)
            {
                SEND_SYSTEM_LOG(Critical, << "dose_main exited with return code " << exitCode);
            }
        }
    );
#endif

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
    
    Com::Communication communication(Com::controlModeTag,
                                     ioService,
                                     options.name,
                                     options.id,
                                     options.nodeTypeId,
                                     options.controlAddress,
                                     options.dataAddress,
                                     commNodeTypes);
    
    communication.InjectSeeds(options.seeds);

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


    SP::SystemPicture sp(SP::master_tag,
                         ioService,
                         communication,
                         options.name,
                         options.id,
                         options.nodeTypeId,
                         options.controlAddress,
                         options.dataAddress,
                         std::move(spNodeTypes));


    std::unique_ptr<Control::SystemStateHandler> stateHandler;
    std::unique_ptr<Control::DoseMainCmdSender> doseMainCmdSender;

    doseMainCmdSender.reset(new Control::DoseMainCmdSender
                            (ioService,
                             // This is what we do when dose_main is started
                             [&sp, &communication, &doseMainCmdSender, &options, &stateHandler]()
                             {
                                 // Send info about own node to dose_main
                                 doseMainCmdSender->InjectOwnNode(0, // request id currently not used
                                                                  options.name,
                                                                  options.id,
                                                                  options.nodeTypeId,
                                                                  options.dataAddress);

                                 sp.StartStateSubscription
                                         ([&stateHandler](const SP::SystemState& newState)
                                          {
                                              stateHandler->SetNewState(newState);
                                          });

                                 communication.Start();
                             })
                            );

    stateHandler.reset(new Control::SystemStateHandler
                                    (Control::Node{options.name,  // Insert own node
                                                   options.id,
                                                   options.nodeTypeId,
                                                   options.controlAddress,
                                                   options.dataAddress},

                                    // Node included callback
                                    [&doseMainCmdSender](const Control::Node& node)
                                    {
                                        doseMainCmdSender->InjectNode(0, // request id currently not used
                                                                      node.name,
                                                                      node.nodeId,
                                                                      node.nodeTypeId,
                                                                      node.dataAddress);
                                    },
                                    // Node down callback
                                    [](const int64_t /*nodeId*/)
                                    {
                                        // TODO: What to do here?
                                    }));

    boost::asio::signal_set signalSet(ioService);

#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
    signalSet.add(SIGABRT);
    signalSet.add(SIGBREAK);
    signalSet.add(SIGINT);
    signalSet.add(SIGTERM);
#elif defined(linux) || defined(__linux) || defined(__linux__)
    signalSet.add(SIGQUIT);
    signalSet.add(SIGINT);
    signalSet.add(SIGTERM);
#endif

    signalSet.async_wait([&sp,&work,&communication, &doseMainCmdSender]
                         (const boost::system::error_code& error,
                          const int signalNumber)
                          {
                              lllog(3) << "CTRL: Got signal " << signalNumber << " ... stop sequence initiated." << std::endl;
                              if (error)
                              {
                                  SEND_SYSTEM_LOG(Error,
                                                  << "Got a signals error: " << error);
                              }
                              sp.Stop();
                              communication.Stop();

                              // Send stop order to dose_main
                              doseMainCmdSender->StopDoseMain(0);  // request id currently not used

                              // Stop the doseMainCmdSender itself
                              doseMainCmdSender->Stop();

                              work.reset();
                          }
                         );


    doseMainCmdSender->Start();

    boost::thread_group threads;
    for (int i = 0; i < 9; ++i)
    {
        threads.create_thread([&ioService]{ioService.run();});
    }

    ioService.run();

    threads.join_all();

    lllog(3) << "CTRL: Exiting..." << std::endl;
    return 0;
}