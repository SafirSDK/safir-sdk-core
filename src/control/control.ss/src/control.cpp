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

#include <boost/filesystem.hpp>
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
                ("dose-main-path",
                 value<std::string>(&doseMainPath)->required(),
                 "Absolute or relative path to dose_main executable.")
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
    std::string doseMainPath;
    boost::int64_t id;

private:
    static void ShowHelp(const boost::program_options::options_description& desc)
    {
        std::wcout << std::boolalpha
                   << "Usage: control [OPTIONS]\n"
                   << desc << std::endl;
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

    // Fetch dou parameters
    Control::Config conf;

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
    namespace fs = boost::filesystem;

    fs::path path(options.doseMainPath);

    if (fs::exists(path))
    {
        if (fs::is_directory(path) || !fs::is_regular_file(path))
        {
            std::ostringstream os;
            os << "CTRL: " << options.doseMainPath << " is a directory or a non regular file!" << std::endl;
            SEND_SYSTEM_LOG(Error, << os.str().c_str());
            throw std::logic_error(os.str().c_str());
        }
    }
    else
    {
        std::ostringstream os;
        os << "CTRL: Can't find " << options.doseMainPath << std::endl;
        SEND_SYSTEM_LOG(Error, << os.str().c_str());
        throw std::logic_error(os.str().c_str());
    }

    boost::system::error_code ec;

    boost::process::child dose_main =
    boost::process::execute
            (boost::process::initializers::run_exe(path),
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

    for (const auto& nt: conf.nodeTypesParam)
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
                                     conf.thisNodeParam.name,
                                     options.id,
                                     conf.thisNodeParam.nodeTypeId,
                                     conf.thisNodeParam.controlAddress,
                                     conf.thisNodeParam.dataAddress,
                                     commNodeTypes);
    
    if (!conf.thisNodeParam.seeds.empty())
    {
        communication.InjectSeeds(conf.thisNodeParam.seeds);
    }

    std::map<boost::int64_t, SP::NodeType> spNodeTypes;
    
    for (const auto& nt: conf.nodeTypesParam)
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
                         conf.thisNodeParam.name,
                         options.id,
                         conf.thisNodeParam.nodeTypeId,
                         conf.thisNodeParam.controlAddress,
                         conf.thisNodeParam.dataAddress,
                         std::move(spNodeTypes));


    std::unique_ptr<Control::SystemStateHandler> stateHandler;
    std::unique_ptr<Control::DoseMainCmdSender> doseMainCmdSender;

    doseMainCmdSender.reset(new Control::DoseMainCmdSender
                            (ioService,
                             // This is what we do when dose_main is ready to receive commands
                             [&sp, &communication, &stateHandler, &doseMainCmdSender, &conf, &options]()
                             {
                                 doseMainCmdSender->StartDoseMain(conf.thisNodeParam.name,
                                                                  options.id,
                                                                  conf.thisNodeParam.nodeTypeId,
                                                                  conf.thisNodeParam.dataAddress);

                                 sp.StartStateSubscription
                                         ([&stateHandler](const SP::SystemState& newState)
                                          {
                                              stateHandler->SetNewState(newState);
                                          });

                                 communication.Start();
                             })
                            );

    stateHandler.reset(new Control::SystemStateHandler
                                    (options.id,

                                    // Node included callback
                                    [&doseMainCmdSender](const Control::Node& node)
                                    {
                                        doseMainCmdSender->InjectNode(node.name,
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
                              doseMainCmdSender->StopDoseMain();

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
