/******************************************************************************
*
* Copyright Saab AB, 2015 (http://safir.sourceforge.net)
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

#include "ControlApp.h"
#include "CommandExecutor.h"
#include <Safir/Control/Parameters.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Utilities/Internal/MakeUnique.h>
#include <Safir/Utilities/CrashReporter.h>
#include <Safir/Utilities/Internal/Id.h>
#include <iostream>
#include <map>
#include <boost/regex.hpp>
#include <boost/atomic.hpp>

//disable warnings in boost
#if defined _MSC_VER
#  pragma warning (push)
#  pragma warning (disable : 4100 4267 4251)
#endif

#include <boost/filesystem.hpp>

#if defined _MSC_VER
#  pragma warning (pop)
#endif

#if defined(linux) || defined(__linux) || defined(__linux__)
#include <unistd.h>
#endif


ControlApp::ControlApp(boost::asio::io_service&         ioService,
                       const boost::filesystem::path&   doseMainPath,
                       const boost::int64_t             id,
                       const bool                       ignoreControlCmd)
    : m_ioService(ioService)
    , m_signalSet(ioService)
    , m_strand(ioService)
    , m_nodeId(id)
    , m_terminationTimer(ioService)
    , m_incarnationBlackListHandler(m_conf.incarnationBlacklistFileName)
    , m_controlInfoReceiverReady(false)
    , m_ctrlStopped(false)
    , m_doseMainRunning(false)
    , m_incarnationIdStorage(new AlignedStorage())
    , m_incarnationId(reinterpret_cast<boost::atomic<int64_t>&>(*m_incarnationIdStorage))
#if defined(linux) || defined(__linux) || defined(__linux__)
    , m_sigchldSet(ioService, SIGCHLD)
#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
    , m_handle(ioService)
#endif
{
    new (m_incarnationIdStorage.get()) boost::atomic<uint64_t>(0);

    // Make some work to stop io_service from exiting.
    m_work = Safir::make_unique<boost::asio::io_service::work>(ioService);

    // Initiate Communication

    std::vector<Com::NodeTypeDefinition> commNodeTypes;

    for (auto nt = m_conf.nodeTypesParam.cbegin(); nt != m_conf.nodeTypesParam.cend(); ++nt)
    {
        commNodeTypes.push_back(Com::NodeTypeDefinition(nt->id,
                                                        nt->name,
                                                        nt->multicastAddressControl,
                                                        nt->multicastAddressData,
                                                        nt->heartbeatInterval,
                                                        nt->retryTimeout,
                                                        nt->maxLostHeartbeats));
    }

    m_communication.reset(new Com::Communication(Com::controlModeTag,
                                                 m_ioService,
                                                 m_conf.thisNodeParam.name,
                                                 id,
                                                 m_conf.thisNodeParam.nodeTypeId,
                                                 m_conf.thisNodeParam.controlAddress,
                                                 m_conf.thisNodeParam.dataAddress,
                                                 commNodeTypes));

    if (!m_conf.thisNodeParam.seeds.empty())
    {
        m_communication->InjectSeeds(m_conf.thisNodeParam.seeds);
    }

    // Initiate SystemPicture

    std::map<boost::int64_t, SP::NodeType> spNodeTypes;

    for (auto nt = m_conf.nodeTypesParam.cbegin(); nt != m_conf.nodeTypesParam.cend(); ++nt)
    {
        spNodeTypes.insert(std::make_pair(nt->id,
                                          SP::NodeType(nt->id,
                                                       nt->name,
                                                       false,
                                                       boost::chrono::milliseconds(nt->heartbeatInterval),
                                                       nt->maxLostHeartbeats,
                                                       boost::chrono::milliseconds(nt->retryTimeout))));
    }


    m_sp.reset(new SP::SystemPicture(SP::master_tag,
                                     ioService,
                                     *m_communication,
                                     m_conf.thisNodeParam.name,
                                     id,
                                     m_conf.thisNodeParam.nodeTypeId,
                                     std::move(spNodeTypes),
                                     [this](const int64_t incarnationId) -> bool
                                     {
                                         if (m_incarnationId == 0 &&
                                             m_incarnationBlackListHandler.ValidateIncarnationId(incarnationId))
                                         {
                                             m_incarnationId = incarnationId;

                                             auto this_ = this;

                                             m_strand.post([this_]{this_->SendControlInfo();});

                                             return true;
                                         }
                                         else
                                         {
                                             return false;
                                         }
                                     }));

    m_doseMainCmdSender.reset(new Control::DoseMainCmdSender
                              (ioService,
                               // This is what we do when dose_main is ready to receive commands
                               [this, id]()
                               {
                                   m_doseMainRunning = true;

                                   m_doseMainCmdSender->StartDoseMain(m_conf.thisNodeParam.name,
                                                                      id,
                                                                      m_conf.thisNodeParam.nodeTypeId,
                                                                      m_conf.thisNodeParam.dataAddress);

                                   auto this_ = this;
                                   m_sp->StartStateSubscription
                                           ([this_](const SP::SystemState& newState)
                                   {
                                       this_->m_stateHandler->SetNewState(newState);
                                   });

                                   m_communication->Start();
                                }));

    m_stopHandler.reset(new Control::StopHandler(ioService,
                                                 *m_communication,
                                                 *m_sp,
                                                 m_conf,
                                                 [this]()
                                                 {
                                                     StopThisNode();
                                                 },
                                                 [this]()
                                                 {
                                                     Shutdown();
                                                 },
                                                 [this]()
                                                 {
                                                     Reboot();
                                                 },
                                                 [this]()
                                                 {
                                                     if (m_incarnationId != 0)
                                                     {
                                                         m_incarnationBlackListHandler.AddIncarnationId(m_incarnationId);
                                                     }
                                                 },
                                                 ignoreControlCmd));


    m_controlInfoSender.reset(new Control::ControlInfoSender
                              (ioService,
                               // This is what we do when a receiver is ready
                               m_strand.wrap([this]()
                                             {
                                                 m_controlInfoReceiverReady = true;
                                                 SendControlInfo();
                                             })));

    m_stateHandler.reset(new Control::SystemStateHandler
                         (id,

    // Node included callback
    [this](const Control::Node& node)
    {
        m_doseMainCmdSender->InjectNode(node.name,
                                        node.nodeId,
                                        node.nodeTypeId,
                                        node.dataAddress);

        m_stopHandler->AddNode(node.nodeId, node.nodeTypeId);
    },

    // Node down callback
    [this](const int64_t nodeId, const int64_t nodeTypeId)
    {
        m_doseMainCmdSender->ExcludeNode(nodeId, nodeTypeId);

        m_stopHandler->RemoveNode(nodeId);
    }));

#if defined(linux) || defined(__linux) || defined(__linux__)
    SetSigchldHandler();
#endif

    // Start dose_main
    boost::system::error_code ec;

    m_doseMain.reset(new boost::process::child(boost::process::execute
                                                (boost::process::initializers::run_exe(doseMainPath),
                                                 boost::process::initializers::set_on_error(ec),
                                                 boost::process::initializers::inherit_env()
#if defined(linux) || defined(__linux) || defined(__linux__)
                                                 ,boost::process::initializers::notify_io_service(ioService)
#endif
                                               )));

    if (ec)
    {
        SEND_SYSTEM_LOG(Error,
                        << "CTRL: Error run_exe: " << ec);
    }


#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
    m_handle.assign(m_doseMain->process_handle());

    m_handle.async_wait(
                [this](const boost::system::error_code&)
    {

        DWORD exitCode;
        auto gotExitCode = ::GetExitCodeProcess(m_handle.native(), &exitCode);

        if (!gotExitCode)
        {
            SEND_SYSTEM_LOG(Critical,
                            << "CTRL: It seems that dose_main has exited but CTRL"
                               " can't retrieve the exit code. GetExitCodeProcess failed"
                               "with error code "  << ::GetLastError());
            std::wcout << "CTRL: It seems that dose_main has exited but CTRL"
                          " can't retrieve the exit code. GetExitCodeProcess failed"
                          "with error code "  << ::GetLastError() << std::endl;

        }
        else if (exitCode == STILL_ACTIVE)
        {
            SEND_SYSTEM_LOG(Critical,
                            << "CTRL: Got an indication that dose_main has exited, however the exit code"
                               " indicates STILL_ALIVE!");
            std::wcout << "CTRL: Got an indication that dose_main has exited, however the exit code"
                          " indicates STILL_ALIVE!" << std::endl;
        }
        else
        {
            lllog(1) << "CTRL: dose_main has exited" << std::endl;

            // dose_main has exited, we can stop our timer that will slay dose_main
            m_terminationTimer.cancel();

            if (exitCode != 0)
            {
                // dose_main has exited unexpectedly

                SEND_SYSTEM_LOG(Critical, << "CTRL: dose_main has exited with exit code "  << exitCode);
                std::wcout << "CTRL: dose_main has exited with exit code "  << exitCode  << std::endl;
            }
        }

        m_doseMainRunning = false;

        if (!m_ctrlStopped)
        {
            StopControl();
        }
    }
    );
#endif

#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
    m_signalSet.add(SIGABRT);
    m_signalSet.add(SIGBREAK);
    m_signalSet.add(SIGINT);
    m_signalSet.add(SIGTERM);
#elif defined(linux) || defined(__linux) || defined(__linux__)
    m_signalSet.add(SIGQUIT);
    m_signalSet.add(SIGINT);
    m_signalSet.add(SIGTERM);
#endif

    m_signalSet.async_wait(m_strand.wrap([this]
                                           (const boost::system::error_code& error,
                                            const int signalNumber)
    {
        if (!!error) //fix for vs2012 warning
        {
            if (error == boost::asio::error::operation_aborted)
            {
                return;
            }
            else
            {
                std::ostringstream os;
                os << "Got a signals error (m_signalSet): " << error;
                throw std::logic_error(os.str());
            }
        }

        lllog(1) << "CTRL: Got signal " << signalNumber << " ... stop sequence initiated." << std::endl;
        std::wcout << "CTRL: Got signal " << signalNumber << " ... stop sequence initiated." << std::endl;

        StopThisNode();
    }
    ));

    m_doseMainCmdSender->Start();
    m_stopHandler->Start();
    m_controlInfoSender->Start();
}

ControlApp::~ControlApp()
{

}

void ControlApp::StopDoseMain()
{
    // Set up a timer that will kill dose_main the hard way if it doesn't stop within a reasonable time.
    m_terminationTimer.expires_from_now(boost::chrono::seconds(10));

    m_terminationTimer.async_wait([this]
                                  (const boost::system::error_code& error)
                                  {
                                      if (error == boost::asio::error::operation_aborted)
                                      {
                                          return;
                                      }

                                      SEND_SYSTEM_LOG(Critical,
                                                      << "CTRL: Can't stop dose_main in a controlled fashion"
                                                      << "... killing it!");

                                      // Kill dose_main the hard way
                                      boost::system::error_code ec;
                                      boost::process::terminate(*m_doseMain, ec);
                                      // We don't care about the error code from terminate. dose_main might
                                      // have exited by itself (which is good) and that will give an error.
                                  });

    // Send stop order to dose_main
    m_doseMainCmdSender->StopDoseMain();
}

void ControlApp::StopControl()
{
    m_sp->Stop();
    m_communication->Stop();
    m_controlInfoSender->Stop();
    m_stopHandler->Stop();
    m_doseMainCmdSender->Stop();
    m_signalSet.cancel();
    m_work.reset();
}

void ControlApp::StopThisNode()
{
    if (m_doseMainRunning)
    {
        StopDoseMain();
    }

    if (!m_ctrlStopped)
    {
        StopControl();
    }
}

void ControlApp::Shutdown()
{
    std::string shutdownCmd =
            Safir::Dob::Typesystem::Utilities::ToUtf8(Safir::Control::Parameters::ShutdownCommand());

    if (shutdownCmd.empty())
    {
        SEND_SYSTEM_LOG(Informational,
                        << "CTRL: Can't execute a shutdown, Safir.Control.Parameters.ShutdownCommand is empty");
        std::wcout << "CTRL: Can't execute a shutdown, Safir.Control.Parameters.ShutdownCommand is empty" << std::endl;
        return;
    }

    Control::ExecuteCmd(shutdownCmd, "Safir.Control.Parameters.ShutdownCommand");

}

void ControlApp::Reboot()
{
    std::string rebootCmd =
            Safir::Dob::Typesystem::Utilities::ToUtf8(Safir::Control::Parameters::RebootCommand());

    if (rebootCmd.empty())
    {
        SEND_SYSTEM_LOG(Informational,
                        << "CTRL: Can't execute a reboot, Safir.Control.Parameters.RebootCommand is empty");
        std::wcout << "CTRL: Can't execute a reboot, Safir.Control.Parameters.rebootCommand is empty" << std::endl;
        return;
    }

    Control::ExecuteCmd(rebootCmd, "Safir.Control.Parameters.RebootCommand");
}

void ControlApp::SendControlInfo()
{
    if (m_incarnationId != 0 && m_controlInfoReceiverReady)
    {
        m_controlInfoSender->SendInfo(m_incarnationId, m_nodeId);
    }
}

#if defined(linux) || defined(__linux) || defined(__linux__)
void ControlApp::SetSigchldHandler()
{
    m_sigchldSet.async_wait(m_strand.wrap([this](const boost::system::error_code& error, int /*signalNumber*/)
    {
        if (error)
        {
            if (error == boost::asio::error::operation_aborted)
            {
                return;
            }
            else
            {
                std::ostringstream os;
                os << "Got a signals error (m_sigchldSet): " << error;
                throw std::logic_error(os.str());
            }
        }

        auto doseMainExited = false;

        int statusCode;
        const pid_t result = ::waitpid(0, &statusCode, WNOHANG | WUNTRACED | WCONTINUED);

        if (result == -1)
        {
            throw std::logic_error("Call to waitpid failed!");
        }

        if (WIFEXITED(statusCode))
        {
            doseMainExited = true;

            auto status = WEXITSTATUS(statusCode);

            if (status != 0)
            {
                SEND_SYSTEM_LOG(Critical, << "CTRL: dose_main has exited with status code "  << status);
                std::wcout << "CTRL: dose_main has exited with status code "  << status  << std::endl;
            }
        }
        else if (WIFSTOPPED(statusCode) || WIFCONTINUED(statusCode))
        {
            // dose_main is stopped or continued, set up the handler to fetch subsequent signals
            SetSigchldHandler();
        }
        else if (WIFSIGNALED(statusCode))
        {
            doseMainExited = true;

            auto signal = WTERMSIG(statusCode);

            SEND_SYSTEM_LOG(Critical,
                            << "CTRL: dose_main has exited due to signal "
                            << strsignal(signal) << " ("  << signal << ")");
            std::wcout << "CTRL: dose_main has exited due to signal "
                       << strsignal(signal) << " ("  << signal << ")" << std::endl;
        }
        else
        {
            throw std::logic_error("Unexpected status code returned from waitpid!");
        }

        if (doseMainExited)
        {
            lllog(1) << "CTRL: dose_main has exited" << std::endl;

            m_doseMainRunning = false;
            m_terminationTimer.cancel();

            if (!m_ctrlStopped)
            {
                StopControl();
            }
        }
    }));
}
#endif
