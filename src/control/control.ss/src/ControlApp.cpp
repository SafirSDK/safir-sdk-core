/******************************************************************************
*
* Copyright Saab AB, 2015 (http://safirsdkcore.com)
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
#include <Safir/Utilities/Internal/StringEncoding.h>
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
#include <boost/thread.hpp>

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
    , m_stopped(false)
    , m_resolutionStartTime(boost::chrono::steady_clock::now())
    , m_strand(ioService)
    , m_wcoutStrand(ioService)
    , m_doseMainPath(doseMainPath)
    , m_ignoreControlCmd(ignoreControlCmd)
    , m_startTimer(ioService)
    , m_terminationTimer(ioService)
    , m_incarnationBlackListHandler(m_conf.incarnationBlacklistFileName)
    , m_controlInfoReceiverReady(false)
    , m_doseMainRunning(false)
    , m_requiredForStart(false)
    , m_incarnationIdStorage(new AlignedStorage())
    , m_incarnationId(reinterpret_cast<boost::atomic<int64_t>&>(*m_incarnationIdStorage))
#if defined(linux) || defined(__linux) || defined(__linux__)
    , m_sigchldSet(ioService, SIGCHLD)
#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
    , m_handle(ioService)
#endif
{
    m_terminateHandler = Safir::make_unique<TerminateHandler>(ioService,
                                                              m_strand.wrap([this]{StopThisNode();}),
                                                              [this](const std::string& str){LogStatus(str);});

    for (auto it = m_conf.nodeTypesParam.cbegin(); it < m_conf.nodeTypesParam.cend(); ++it)
    {
        if (m_conf.thisNodeParam.nodeTypeId == it->id)
        {
            m_requiredForStart = it->requiredForStart;
            break;
        }
    }

    if (id == 0)
    {
        for (;;)
        {
            // Generate a positive node id if the node is of a type that is allowed to form as system, or
            // a negative node id if the node is of a type that is NOT allowd to form a system.
            boost::int64_t nodeId = LlufId_GenerateRandom64();

            if ((m_requiredForStart && nodeId > 0) || (!m_requiredForStart && nodeId < 0))
            {
                m_nodeId = nodeId;
                break;
            }
        }
    }
    else
    {

        m_nodeId = id;
    }

    new (m_incarnationIdStorage.get()) boost::atomic<uint64_t>(0);

    // Make some work to stop io_service from exiting.
    m_work = Safir::make_unique<boost::asio::io_service::work>(ioService);

    //Call the Start method, which will retry itself if need be (e.g. the local interface
    //addresses cannot be resolved).
    m_strand.post([this]{Start();});
}

ControlApp::~ControlApp()
{
    //everything should already be shut down when we get here, so anything we actually
    //do here is just a last resort.
    if (m_doseMainRunning)
    {
        SEND_SYSTEM_LOG(Critical,
                        << "CTRL: safir_control is shutting down in an uncontrolled manner, killing dose_main!");

        // Kill dose_main the hard way
        boost::system::error_code ec;
        boost::process::terminate(*m_doseMain, ec);
        // We don't care about the error code from terminate. We're in panic mode after all.

        m_doseMainRunning = false;
    }

    StopControl();
}

void ControlApp::LogStatus(const std::string& str)
{
    lllog(1) << str.c_str() << std::endl;
    m_wcoutStrand.dispatch([str]
                           {
                               std::wcout << str.c_str() << std::endl;
                           });
}


//must be called from within strand
void ControlApp::Start()
{
    const auto addresses = ResolveAddresses();

    //if addresses can not be resolved we can't go on. ResolveAddresses is responsible
    //for making retries which will cause Start to be called again.
    if (!(addresses.first.Ok() && addresses.second.Ok()))
    {
        return;
    }

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
                                                 m_nodeId,
                                                 m_conf.thisNodeParam.nodeTypeId,
                                                 addresses.first,
                                                 addresses.second,
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


    // Note that the two callbacks that are called by SP are synchronous and return a
    // value. This means that the operations can't be protected by the strand and
    // therefore you need to be cautious when modifying the callback code. In the future,
    // if more elaborated things have to be done in the callback code, it might be
    // necessary to turn this into ordinary asynchronous calls.
    m_sp.reset(new SP::SystemPicture(SP::master_tag,
                                     m_ioService,
                                     *m_communication,
                                     m_conf.thisNodeParam.name,
                                     m_nodeId,
                                     m_conf.thisNodeParam.nodeTypeId,
                                     std::move(spNodeTypes),
                                     // Join system callback
                                     [this](const int64_t incarnationId) -> bool
                                     {
                                         if (m_incarnationId == 0 &&
                                             m_incarnationBlackListHandler.ValidateIncarnationId(incarnationId))
                                         {
                                             m_incarnationId = incarnationId;

                                             auto this_ = this;

                                             m_strand.post([this_]{this_->SendControlInfo();});

                                             std::ostringstream os;
                                             os << "CTRL: Joined system with incarnation id " << incarnationId;
                                             LogStatus(os.str());

                                             return true;
                                         }
                                         else
                                         {
                                             lllog(1) << "CTRL: Not ok to join incarnation " << incarnationId << std::endl;

                                             return false;
                                         }
                                     },
                                     // Form system callback
                                     [this](const int64_t incarnationId) -> bool
                                     {
                                         // Check if this node is of a type that is allowed to form systems
                                         if (m_requiredForStart)
                                         {
                                             m_incarnationId = incarnationId;

                                             auto this_ = this;

                                             m_strand.post([this_]{this_->SendControlInfo();});

                                             std::ostringstream os;
                                             os << "CTRL: Starting system with incarnation id " << incarnationId;
                                             LogStatus(os.str());

                                             return true;
                                         }
                                         else
                                         {
                                             std::ostringstream os;
                                             os << "CTRL: Waiting for system start";
                                             LogStatus(os.str());

                                             return false;
                                         }
                                     }));

    m_doseMainCmdSender.reset(new Control::DoseMainCmdSender
                              (m_ioService,
                               // This is what we do when dose_main is ready to receive commands
                               [this]()
                               {
                                   m_doseMainRunning = true;

                                   m_doseMainCmdSender->StartDoseMain(m_conf.thisNodeParam.name,
                                                                      m_nodeId,
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

    m_stopHandler.reset(new Control::StopHandler(m_ioService,
                                                 *m_communication,
                                                 *m_sp,
                                                 *m_doseMainCmdSender,
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
                                                 m_ignoreControlCmd));


    m_controlInfoSender.reset(new Control::ControlInfoSender
                              (m_ioService,
                               // This is what we do when a receiver is ready
                               m_strand.wrap([this]()
                                             {
                                                 m_controlInfoReceiverReady = true;
                                                 SendControlInfo();
                                             })));

    m_stateHandler.reset(new Control::SystemStateHandler
                         (m_nodeId,

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
                                                (boost::process::initializers::run_exe(m_doseMainPath),
                                                 boost::process::initializers::set_on_error(ec),
                                                 boost::process::initializers::inherit_env()
#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
                                                 ,boost::process::initializers::show_window(SW_HIDE)
#elif defined(linux) || defined(__linux) || defined(__linux__)
                                                 ,boost::process::initializers::notify_io_service(m_ioService)
#endif
                                               )));

    if (ec)
    {
        SEND_SYSTEM_LOG(Error,
                        << "CTRL: Error run_exe: " << ec);
    }


#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
    m_handle.assign(m_doseMain->process_handle());

    m_handle.async_wait(m_strand.wrap([this](const boost::system::error_code&)
    {
        DWORD exitCode;
        auto gotExitCode = ::GetExitCodeProcess(m_handle.native(), &exitCode);

        if (!gotExitCode)
        {
            std::ostringstream ostr;
            ostr << "CTRL: It seems that dose_main has exited but Control"
                " can't retrieve the exit code. GetExitCodeProcess failed"
                "with error code "  << ::GetLastError();
            SEND_SYSTEM_LOG(Critical, << ostr.str().c_str());
            LogStatus(ostr.str());

        }
        else if (exitCode == STILL_ACTIVE)
        {
            std::ostringstream ostr;
            ostr << "CTRL: Got an indication that dose_main has exited, however the exit code"
                " indicates STILL_ALIVE!";
            SEND_SYSTEM_LOG(Critical, << ostr.str().c_str());
            LogStatus(ostr.str());
        }
        else
        {
            lllog(1) << "CTRL: dose_main has exited" << std::endl;

            // dose_main has exited, we can stop our timer that will slay dose_main
            m_terminationTimer.cancel();

            if (exitCode != 0)
            {
                // dose_main has exited unexpectedly
                std::ostringstream ostr;
                ostr << "CTRL: dose_main has exited with exit code "  << exitCode;
                SEND_SYSTEM_LOG(Critical, << ostr.str().c_str());
                LogStatus(ostr.str());
            }
        }

        m_doseMainRunning = false;

        StopControl();
    }
    ));
#endif

    m_doseMainCmdSender->Start();
    m_stopHandler->Start();
    m_controlInfoSender->Start();
}


std::pair<Com::ResolvedAddress,Com::ResolvedAddress> ControlApp::ResolveAddresses()
{
    //resolve local interfaces.
    const Com::ResolvedAddress controlAddress(m_conf.thisNodeParam.controlAddress);
    const Com::ResolvedAddress dataAddress(m_conf.thisNodeParam.dataAddress);

    //if both addresses are not resolved ok we need to retry
    if (!(controlAddress.Ok() && dataAddress.Ok()))
    {
        //well, unless we've been trying to start for long enough
        if (m_resolutionStartTime + m_conf.localInterfaceTimeout < boost::chrono::steady_clock::now())
        {
            std::ostringstream os;
            os << "CTRL: Failed to resolve local address/interface ";
            if (!controlAddress.Ok())
            {
                os << m_conf.thisNodeParam.controlAddress.c_str() << " (ControlAddress)";
            }
            if (!dataAddress.Ok())
            {
                if (!controlAddress.Ok())
                {
                    os << " and ";
                }
                os << m_conf.thisNodeParam.dataAddress.c_str() << " (DataAddress)";
            }
            os << ". Have retried for the period configured in Safir.Dob.NodeParameters.LocalInterfaceTimeout.";

            SEND_SYSTEM_LOG(Critical, << os.str().c_str());
            LogStatus(os.str());

            StopControl();
        }
        else
        {
            //log a warning
            static bool warned = false;
            if (!warned)
            {
                warned = true;
                std::ostringstream os;
                os << "CTRL: Failed to resolve local address/interface ";
                if (!controlAddress.Ok())
                {
                    os << m_conf.thisNodeParam.controlAddress.c_str() << " (ControlAddress)";
                }
                if (!dataAddress.Ok())
                {
                    if (!controlAddress.Ok())
                    {
                        os << " and ";
                    }
                    os << m_conf.thisNodeParam.dataAddress.c_str() << " (DataAddress)";
                }
                os << ". Will retry.";

                SEND_SYSTEM_LOG(Warning, << os.str().c_str());
                LogStatus(os.str());
            }

            //ok, set up the retry timer
            m_startTimer.expires_from_now(boost::chrono::seconds(1));
            m_startTimer.async_wait(m_strand.wrap([this](const boost::system::error_code& error)
                                                  {
                                                      if (!error && !m_stopped)
                                                      {
                                                          Start();
                                                      }
                                                  }));
        }
    }

    return std::make_pair(controlAddress,dataAddress);
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
    if (m_stopped)
    {
        return;
    }
    m_stopped = true;

    m_startTimer.cancel();

    if (m_sp != nullptr)
    {
        m_sp->Stop();
    }

    if (m_communication != nullptr)
    {
        m_communication->Stop();
    }

    if (m_controlInfoSender != nullptr)
    {
        m_controlInfoSender->Stop();
    }

    if (m_stopHandler != nullptr)
    {
        m_stopHandler->Stop();
    }

    if (m_doseMainCmdSender != nullptr)
    {
        m_doseMainCmdSender->Stop();
    }

    m_terminateHandler->Stop();
    m_work.reset();
}

void ControlApp::StopThisNode()
{
    if (m_doseMainRunning)
    {
        StopDoseMain();
    }

    StopControl();
}

void ControlApp::Shutdown()
{
    std::string shutdownCmd =
            Safir::Utilities::Internal::ToUtf8(Safir::Control::Parameters::ShutdownCommand());

    if (shutdownCmd.empty())
    {
        std::ostringstream ostr;
        ostr << "CTRL: Can't execute a shutdown, Safir.Control.Parameters.ShutdownCommand is empty";
        SEND_SYSTEM_LOG(Informational, << ostr.str().c_str());
        LogStatus(ostr.str());
        return;
    }

    Control::ExecuteCmd(shutdownCmd, "Safir.Control.Parameters.ShutdownCommand",[this](const std::string& str){LogStatus(str);});

}

void ControlApp::Reboot()
{
    std::string rebootCmd =
            Safir::Utilities::Internal::ToUtf8(Safir::Control::Parameters::RebootCommand());

    if (rebootCmd.empty())
    {
        std::ostringstream ostr;
        ostr << "CTRL: Can't execute a reboot, Safir.Control.Parameters.RebootCommand is empty";

        SEND_SYSTEM_LOG(Informational, << ostr.str().c_str());
        LogStatus(ostr.str());
        return;
    }

    Control::ExecuteCmd(rebootCmd, "Safir.Control.Parameters.RebootCommand",[this](const std::string& str){LogStatus(str);});
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
                std::ostringstream ostr;
                ostr << "CTRL: dose_main has exited with status code "  << status;

                SEND_SYSTEM_LOG(Critical, << ostr.str().c_str());
                LogStatus(ostr.str());
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

            std::ostringstream ostr;
            ostr << "CTRL: dose_main has exited due to signal "
                 << strsignal(signal) << " ("  << signal << ")";

            SEND_SYSTEM_LOG(Critical, << ostr.str().c_str());
            LogStatus(ostr.str());
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

            StopControl();
        }
    }));
}
#endif
