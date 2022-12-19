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
#include <Safir/Utilities/Internal/AsioStrandWrap.h>
#include <Safir/Utilities/CrashReporter.h>
#include <Safir/Utilities/Internal/Id.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <iostream>
#include <map>
#include <atomic>

#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
#include <boost/process/windows.hpp>
#endif

//disable warnings in boost
#if defined _MSC_VER
#  pragma warning (push)
#  pragma warning (disable : 4100 4267 4251 4005)
#endif

#include <boost/regex.hpp>
#include <boost/filesystem.hpp>
#include <boost/process/environment.hpp>
#include <boost/process/env.hpp>
#include <boost/process/async.hpp>
#include <boost/thread.hpp>

#if defined _MSC_VER
#  pragma warning (pop)
#endif

namespace
{
    int64_t GenerateNodeId(const int64_t forcedId, const bool requiredForStart)
    {
        if (forcedId != 0)
        {
            return forcedId;
        }

        for (;;)
        {
            // Generate a positive node id if the node is of a type that is allowed to form as system, or
            // a negative node id if the node is of a type that is NOT allowd to form a system.
            // This will cause SystemPicture to always prefer nodes with RequiredForStart set to be
            // Coordinator.
            const std::int64_t nodeId = LlufId_GenerateRandom64();

            if ((requiredForStart && nodeId > 0) || (!requiredForStart && nodeId < 0))
            {
                return nodeId;
            }
        }
    }

    bool GetRequiredForStart(const Control::Config& conf)
    {
        for (auto it = conf.nodeTypesParam.cbegin(); it < conf.nodeTypesParam.cend(); ++it)
        {
            if (conf.thisNodeParam.nodeTypeId == it->id)
            {
                return it->requiredForStart;
            }
        }
        return false;
    }
}

ControlApp::ControlApp(boost::asio::io_context&         io,
                       const boost::filesystem::path&   doseMainPath,
                       const std::int64_t               id,
                       const bool                       ignoreControlCmd)
    : m_io(io)
    , m_work(boost::asio::make_work_guard(io))
    , m_stopped(false)
    , m_resolutionStartTime(boost::chrono::steady_clock::now())
    , m_strand(io)
    , m_wcoutStrand(io)
    , m_requiredForStart(GetRequiredForStart(m_conf))
    , m_nodeId(GenerateNodeId(id, m_requiredForStart))
    , m_doseMainPath(doseMainPath)
    , m_ignoreControlCmd(ignoreControlCmd)
    , m_startTimer(io)
    , m_terminationTimer(io)
    , m_incarnationBlackListHandler(m_conf.incarnationBlacklistFileName)
    , m_controlInfoReceiverReady(false)
    , m_doseMainRunning(false)
    , m_incarnationIdStorage(new AlignedStorage())
    , m_incarnationId(reinterpret_cast<std::atomic<int64_t>&>(*m_incarnationIdStorage))
{
    m_terminateHandler = Safir::make_unique<TerminateHandler>(io,
                                                              Safir::Utilities::Internal::WrapInStrand(m_strand, [this]{StopThisNode();}),
                                                              [this](const std::string& str){LogStatus(str);});

    new (m_incarnationIdStorage.get()) std::atomic<uint64_t>(0);

    //Call the Start method, which will retry itself if need be (e.g. the local interface
    //addresses cannot be resolved).
    boost::asio::post(m_strand, [this]{Start();});
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
        std::error_code ec;
        m_doseMain->terminate(ec);
        // We don't care about any error code from terminate. We're in panic mode after all.

        m_doseMainRunning = false;
    }

    StopControl();
}

void ControlApp::LogStatus(const std::string& str)
{
    lllog(1) << str.c_str() << std::endl;
    boost::asio::dispatch(m_wcoutStrand, [str]
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

    // Read NodeType configuration
    std::set<int64_t> lightNodeTypeIds;
    std::vector<Com::NodeTypeDefinition> commNodeTypes;
    std::map<std::int64_t, SP::NodeType> spNodeTypes;
    bool isLightNode = false;
    for (const auto& nt : m_conf.nodeTypesParam)
    {
        if (nt.isLightNode)
        {
            lightNodeTypeIds.insert(nt.id);
        }

        if (m_conf.thisNodeParam.nodeTypeId == nt.id)
        {
            isLightNode = nt.isLightNode;
        }

        // communication stuff
        commNodeTypes.push_back(Com::NodeTypeDefinition(nt.id,
                                                        nt.name,
                                                        nt.multicastAddressControl,
                                                        nt.multicastAddressData,
                                                        nt.isLightNode,
                                                        nt.heartbeatInterval,
                                                        nt.maxLostHeartbeats,
                                                        nt.slidingWindowSize,
                                                        nt.ackRequestThreshold,
                                                        nt.retryTimeout));

        // system picture stuff
        std::vector<boost::chrono::steady_clock::duration> retryTimeouts;
        for (auto rt : nt.retryTimeout)
        {
            retryTimeouts.push_back(boost::chrono::milliseconds(rt));
        }

        spNodeTypes.insert(std::make_pair(nt.id,
                                          SP::NodeType(nt.id,
                                                       nt.name,
                                                       nt.isLightNode,
                                                       boost::chrono::milliseconds(nt.heartbeatInterval),
                                                       nt.maxLostHeartbeats,
                                                       retryTimeouts)));
    }

    // Initiate Communication
    m_communication.reset(new Com::Communication(Com::controlModeTag,
                                                 m_io,
                                                 m_conf.thisNodeParam.name,
                                                 m_nodeId,
                                                 m_conf.thisNodeParam.nodeTypeId,
                                                 addresses.first,
                                                 addresses.second,
                                                 commNodeTypes,
                                                 m_conf.fragmentSize));

    if (!m_conf.thisNodeParam.seeds.empty())
    {
        m_communication->InjectSeeds(m_conf.thisNodeParam.seeds);
    }

    // Initiate SystemPicture

    // Note that the two callbacks that are called by SP are synchronous and return a
    // value. This means that the operations can't be protected by the strand and
    // therefore you need to be cautious when modifying the callback code. In the future,
    // if more elaborated things have to be done in the callback code, it might be
    // necessary to turn this into ordinary asynchronous calls.
    m_sp.reset(new SP::SystemPicture(SP::master_tag,
                                     m_io,
                                     *m_communication,
                                     m_conf.thisNodeParam.name,
                                     m_nodeId,
                                     m_conf.thisNodeParam.nodeTypeId,
                                     std::move(spNodeTypes),
                                     m_conf.aloneTimeout,

    // --- Join system callback ---
    [this, isLightNode](const int64_t incarnationId) -> bool
    {
        lllog(1) << L"CTRL: Got JoinSystemCallback, incarnationId: " << incarnationId << std::endl;
        ENSURE(incarnationId != m_incarnationId, << "CTRL: Join system called with same incarnationId that we are already part of, " << incarnationId);
        ENSURE(isLightNode || m_nodePristine, << "CTRL: Join/Form system callback should not occur more than once for non light node");

        if (!m_incarnationBlackListHandler.ValidateIncarnationId(incarnationId))
        {
            lllog(1) << "CTRL: Not ok to join incarnation " << incarnationId << std::endl;
            return false;
        }

        if (m_nodePristine)
        {
            m_doseMainCmdSender->NodeStateChanged(Control::NodeState::JoinedSystem);
            m_nodePristine = false;
        }

        if (isLightNode)
        {
            m_stateHandler->SetDetached(false); // We are not in detached mode after a join.
            if (m_detachedFromIncarnationId == incarnationId)
            {
                m_doseMainCmdSender->NodeStateChanged(Control::NodeState::AttachedSameSystem);
                std::ostringstream os;
                os << "CTRL: Attached to the same system again as we have been detached from for a while. InncarnationId=" << incarnationId << std::endl;
                os << "CTRL: This node has id " << m_nodeId;
                LogStatus(os.str());
            }
            else
            {
                m_doseMainCmdSender->NodeStateChanged(Control::NodeState::AttachedNewSystem);
                std::ostringstream os;
                os << "CTRL: Attached to a new system that we have not seen before. InncarnationId=" << incarnationId << std::endl;
                os << "CTRL: This node has id " << m_nodeId;
                LogStatus(os.str());
            }
        }
        else // normal node
        {
            std::ostringstream os;
            os << "CTRL: Joined system with incarnation id " << incarnationId << std::endl;
            os << "CTRL: This node has id " << m_nodeId;
            LogStatus(os.str());
        }

        m_incarnationId = incarnationId;
        boost::asio::post(m_strand, [this]{SendControlInfo();});
        return true;
    },
    // --- Form system callback ---
    [this, isLightNode](const int64_t incarnationId) -> bool
    {
        lllog(1) << L"CTRL: Got FormSystemCallback, incarnationId: " << incarnationId << std::endl;
        ENSURE(incarnationId != m_incarnationId, << "CTRL: Form system called with same incarnationId that we are already part of, " << incarnationId);
        ENSURE(isLightNode || m_nodePristine, << "CTRL: Join/Form system callback should not occur more than once for non light node");

        if (!isLightNode && !m_requiredForStart)
        {
            // Node is not allowed to form a system
            LogStatus("CTRL: Waiting for system start");
            return false;
        }

        if (m_nodePristine)
        {
            // One time even light nodes must call Form or Join since it start things in dose_main
            m_doseMainCmdSender->NodeStateChanged(Control::NodeState::FormedSystem);
            m_nodePristine = false;
        }

        // Lightnode getting formSystemCb, means detached mode
        if (isLightNode)
        {
            // Lightnode in detached mode
            m_detachedFromIncarnationId = m_incarnationId; // save old incarnation so we can determine if next attach is AttachNewSystem or AttachSameSystem
            m_stateHandler->SetDetached(true);
            m_doseMainCmdSender->NodeStateChanged(Control::NodeState::DetachedFromSystem);
            std::ostringstream os;
            os << "CTRL: Light node in detached mode is forming a system on its own with incarnation id " << incarnationId << std::endl;
            os << "CTRL: This node has id " << m_nodeId;
            LogStatus(os.str());
        }
        else
        {
            m_nodePristine = false;
            std::ostringstream os;
            os << "CTRL: Starting system with incarnation id " << incarnationId << std::endl;
            os << "CTRL: This node has id " << m_nodeId;
            LogStatus(os.str());
        }

        m_incarnationId = incarnationId;
        boost::asio::post(m_strand, [this]{ SendControlInfo(); });
        return true;
    }));

    m_doseMainCmdSender.reset(new Control::DoseMainCmdSender
                              (m_io,
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

    m_stopHandler.reset(new Control::StopHandler(m_io,
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
                              (m_io,
                               // This is what we do when a receiver is ready
                               Safir::Utilities::Internal::WrapInStrand(m_strand, [this]()
    {
        m_controlInfoReceiverReady = true;
        SendControlInfo();
    })));

    m_stateHandler.reset(new Control::SystemStateHandler(m_nodeId, isLightNode, lightNodeTypeIds,
    // --- Node included callback ---
    [this](const Control::Node& node)
    {
        lllog(6) << L"CTRL: NodeUp '" << node.name.c_str() << "', nodeId=" << node.nodeId << std::endl;
        m_doseMainCmdSender->InjectNode(node.name,
                                        node.nodeId,
                                        node.nodeTypeId,
                                        node.dataAddress);

        m_stopHandler->AddNode(node.nodeId, node.nodeTypeId);
    },

    // --- Node down callback ---
    [this](const int64_t nodeId, const int64_t nodeTypeId)
    {
        lllog(6) << L"CTRL: NodeDown nodeId=" << nodeId << std::endl;
        m_doseMainCmdSender->ExcludeNode(nodeId, nodeTypeId);
        m_stopHandler->RemoveNode(nodeId);
    }));

    // Start dose_main
    std::error_code error;

    m_doseMain = std::make_unique<boost::process::child>
            (m_doseMainPath,
         #if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
             boost::process::windows::hide,
         #endif
             m_io,
             error,
             boost::process::on_exit=[this](int exitCode, const std::error_code& error)
    {HandleDoseMainExit(exitCode,error);});

    if (error)
    {
        SEND_SYSTEM_LOG(Error,
                        << "CTRL: Error launching dose_main: " << error);
    }

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
            m_startTimer.expires_after(boost::chrono::seconds(1));
            m_startTimer.async_wait(boost::asio::bind_executor(m_strand, [this](const boost::system::error_code& error)
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
    m_terminationTimer.expires_after(boost::chrono::seconds(10));

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
        std::error_code ec;
        m_doseMain->terminate(ec);
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


void ControlApp::HandleDoseMainExit(int exitCode, const std::error_code& error)
{
    if (error)
    {
        std::ostringstream ostr;
        ostr << "CTRL: Got an error in on_exit: " << error;
        SEND_SYSTEM_LOG(Critical, << ostr.str().c_str());
        LogStatus(ostr.str());
    }

#if defined(linux) || defined(__linux) || defined(__linux__)
    const auto nativeExitCode = m_doseMain->native_exit_code();
    if (WIFEXITED(nativeExitCode))
    {
        if (exitCode != 0)
        {
            std::ostringstream ostr;
            ostr << "CTRL: dose_main has exited with status code "  << exitCode;

            SEND_SYSTEM_LOG(Critical, << ostr.str().c_str());
            LogStatus(ostr.str());
        }
    }
    else if (WIFSIGNALED(nativeExitCode))
    {
        std::ostringstream ostr;
        ostr << "CTRL: dose_main has exited due to signal "
             << strsignal(exitCode) << " ("  << exitCode << ")";

        SEND_SYSTEM_LOG(Critical, << ostr.str().c_str());
        LogStatus(ostr.str());
    }
    else
    {
        std::stringstream ostr;
        ostr << "CTRL: dose_main has exited with unexpected status code ("
             << exitCode << ", " <<nativeExitCode << ")";

        SEND_SYSTEM_LOG(Critical, << ostr.str().c_str());
        LogStatus(ostr.str());
    }
#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
    if (exitCode != 0)
    {
        std::ostringstream ostr;
        ostr << "CTRL: dose_main has exited with status code "  << exitCode;

        SEND_SYSTEM_LOG(Critical, << ostr.str().c_str());
        LogStatus(ostr.str());
    }
#else
#error "Control does not support this platform yet"
#endif

    lllog(1) << "CTRL: dose_main has exited" << std::endl;

    m_doseMainRunning = false;
    m_terminationTimer.cancel();

    StopControl();
}
