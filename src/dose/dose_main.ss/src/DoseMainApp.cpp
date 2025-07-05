/******************************************************************************
*
* Copyright Saab AB, 2007-2013,2015 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / stlrha
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
#include "DoseMainApp.h"
#include <Safir/Dob/Internal/ControlConfig.h>
#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Dob/Internal/InternalDefs.h>
#include <Safir/Dob/Internal/Initialize.h>
#include <Safir/Dob/OverflowException.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Utilities/CrashReporter.h>
#include <Safir/Utilities/Internal/AsioStrandWrap.h>
#include <iostream>

namespace Safir
{
namespace Dob
{
namespace Internal
{

    namespace //anonymous namespace
    {
        void DumpFunc(const char* const dumpPath)
        {
            SEND_SYSTEM_LOG(Alert,
                            << "dose_main has generated a dump to:\n"
                            << dumpPath << "\n"
                            << "Please send this file to your nearest Dob developer, along with\n"
                            << "relevant information about what version of Safir SDK you are using");
        }

#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
        std::function<void()> ConsoleCtrlHandlerFcn;
        BOOL WINAPI ConsoleCtrlHandler(DWORD event)
        {
            switch (event)
            {
            case CTRL_CLOSE_EVENT:
            case CTRL_LOGOFF_EVENT:
            case CTRL_SHUTDOWN_EVENT:
                {
                    ConsoleCtrlHandlerFcn();

                    //We could sleep forever here, since the function will be terminated when
                    //our main function returns. Anyway, we only have something like
                    //five seconds before the process gets killed anyway.
                    //So the below code is just to ensure we sleep for a while and
                    //dont generate any compiler errors...
                    for(int i = 0; i < 10; ++i)
                    {
                        std::this_thread::sleep_for(std::chrono::seconds(1));
                    }
                }
                return TRUE;
            default:
                return FALSE;
            }
        }
#endif
    }
    DoseMainApp::DoseMainApp(boost::asio::io_context& ioContext):
        m_stopped(false),
        m_ioContext(ioContext),
        m_strand(ioContext),
        m_wcoutStrand(ioContext),
        m_work(boost::asio::make_work_guard(ioContext)),
        m_nodeId(0),
        m_distribution(),
        m_signalSet(ioContext)
    {
        m_cmdReceiver.reset(new Control::DoseMainCmdReceiver( ioContext,

                                                              // StartDoseMain
                                                              Safir::Utilities::Internal::WrapInStrand(m_strand, [this](const std::string& nodeName, int64_t nodeId, int64_t nodeTypeId, const std::string& dataAddress)
                                                                            {
                                                                                lllog(1) << "DOSE_MAIN: Got Start command from control"<< std::endl;
                                                                                Start(nodeName,nodeId, nodeTypeId, dataAddress);
                                                                            }),

                                                              // InjectNode
                                                              Safir::Utilities::Internal::WrapInStrand(m_strand, [this](const std::string& nodeName, int64_t nodeId, int64_t nodeTypeId, const std::string& dataAddress)
                                                                            {
                                                                                lllog(1) << "DOSE_MAIN: Got InjectNode command from control for node " << nodeId << std::endl;
                                                                                InjectNode(nodeName, nodeId, nodeTypeId, dataAddress);
                                                                            }),

                                                              // ExcludeNode
                                                              Safir::Utilities::Internal::WrapInStrand(m_strand, [this](int64_t nodeId, int64_t nodeTypeId)
                                                                            {
                                                                                lllog(1) << "DOSE_MAIN: Got ExcludeNode command from control for node " << nodeId << std::endl;
                                                                                ExcludeNode(nodeId, nodeTypeId);
                                                                            }),

                                                              // StoppedNodeIndication
                                                              Safir::Utilities::Internal::WrapInStrand(m_strand, [this](int64_t nodeId)
                                                                            {
                                                                                lllog(1) << "DOSE_MAIN: Got StoppedNodeIndication command from control for node " << nodeId << std::endl;
                                                                                StoppedNodeIndication(nodeId);
                                                                            }),

                                                              // StopDoseMain
                                                              Safir::Utilities::Internal::WrapInStrand(m_strand, [this]()
                                                                            {
                                                                                lllog(1) << "DOSE_MAIN: Got Stop command from control"<< std::endl;
                                                                                Stop();
                                                                            }),

                                                              // NodeStateChanged
                                                              Safir::Utilities::Internal::WrapInStrand(m_strand, [this](Safir::Dob::Internal::Control::NodeState nodeState)
                                                                            {
                                                                                lllog(1) << "DOSE_MAIN: Got NodeStateChanged command from control, state=" << Control::NodeStateToString(nodeState).c_str() << std::endl;
                                                                                NodeStateChanged(nodeState);
                                                                            })));

#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
        m_signalSet.add(SIGABRT);
        m_signalSet.add(SIGBREAK);
        m_signalSet.add(SIGINT);
        m_signalSet.add(SIGTERM);

        //We install a ConsoleCtrlHandler to handle presses of the Close button
        //on the console window. This is a little different from handling signals
        //since there is no way to ignore it...
        ConsoleCtrlHandlerFcn = Safir::Utilities::Internal::WrapInStrand(m_strand, [this]()
        {
            lllog(1) << "DOSE_MAIN: Got console Close button Stop"<< std::endl;
            Stop();
        });
        ::SetConsoleCtrlHandler(ConsoleCtrlHandler,TRUE);
#elif defined(linux) || defined(__linux) || defined(__linux__)
        m_signalSet.add(SIGQUIT);
        m_signalSet.add(SIGINT);
        m_signalSet.add(SIGTERM);
#endif

        m_signalSet.async_wait(Safir::Utilities::Internal::WrapInStrand(m_strand, [](const boost::system::error_code& error, const int /*signalNumber*/)
                                            {
                                                if (error)
                                                {
                                                    if (error == boost::asio::error::operation_aborted)
                                                    {
                                                        // dose_main got stopped via a command from Control, do nothing
                                                        return;
                                                    }
                                                    else
                                                    {
                                                       SEND_SYSTEM_LOG(Error,
                                                                        << "DOSE_MAIN: Got a signals error: " << error);
                                                    }
                                                }

                                                // dose_main expects to be stopped by safir_control and
                                                // therefore ignores all "termination" signals
                                            }));

        Safir::Utilities::CrashReporter::RegisterCallback(DumpFunc);
        Safir::Utilities::CrashReporter::Start();

        // Start reception of commands from Control
        m_cmdReceiver->Start();
    }

    DoseMainApp::~DoseMainApp()
    {
        lllog(9) << "DoseMainApp: In destructor" << std::endl;
        Stop();
        lllog(9) << "DoseMainApp: Destructor finished" << std::endl;
    }

    void DoseMainApp::Start(const std::string& nodeName, int64_t nodeId, int64_t nodeTypeId, const std::string& dataAddress)
    {
        m_nodeId = nodeId;
        m_memoryMonitor.reset(new MemoryMonitor(m_ioContext));
        m_distribution.reset(new Distribution(m_ioContext, nodeName, nodeId, nodeTypeId, dataAddress));
        InitializeDoseInternalFromDoseMain(nodeId);
        m_lockMonitor.reset(new LockMonitor());
        m_messageHandler.reset(new MessageHandler(*m_distribution));
        m_requestHandler.reset(new RequestHandler(m_strand.context(), *m_distribution));
        m_pendingRegistrationHandler.reset(new PendingRegistrationHandler(m_ioContext, *m_distribution));
        m_connectionHandler.reset(new ConnectionHandler(m_ioContext,
                                                        *m_distribution,
                                                        [this](const ConnectionPtr& connection, bool disconnecting){OnAppEvent(connection, disconnecting);},
                                                        [this](int64_t tid){m_pendingRegistrationHandler->CheckForPending(tid);},
                                                        [this](const std::string& str){LogStatus(str);}));

        m_distribution->Start();
    }

    void DoseMainApp::Stop()
    {
        const bool wasStopped = m_stopped.exchange(true);
        lllog(9) << "DoseMainApp::Stop: wasStopped = "<< wasStopped << std::endl;
        if (!wasStopped)
        {
            lllog(9) << "DoseMainApp::Stop: Sending stop orders" << std::endl;
            m_connectionKiller.SendStopOrders();

            lllog(9) << "DoseMainApp::Stop: Stopping cmdReceiver" << std::endl;
            m_cmdReceiver->Stop();

            lllog(9) << "DoseMainApp::Stop: Stopping lockMonitor" << std::endl;
            if (m_lockMonitor != nullptr)
            {
                m_lockMonitor->Stop();
            }

            lllog(9) << "DoseMainApp::Stop: Stopping distribution" << std::endl;
            if (m_distribution != nullptr)
            {
                m_distribution->Stop();
            }

            lllog(9) << "DoseMainApp::Stop: Stopping connection handler" << std::endl;
            if (m_connectionHandler != nullptr)
            {
                m_connectionHandler->Stop();
            }

            lllog(9) << "DoseMainApp::Stop: Stopping request handler" << std::endl;
            if (m_requestHandler != nullptr)
            {
                m_requestHandler->Stop();
            }

            lllog(9) << "DoseMainApp::Stop: Stopping pending registration handler" << std::endl;
            if (m_pendingRegistrationHandler != nullptr)
            {
                m_pendingRegistrationHandler->Stop();
            }

            lllog(9) << "DoseMainApp::Stop: Stop listening to signals" << std::endl;
            m_signalSet.cancel();

            lllog(9) << "DoseMainApp::Stop: Stopping memory monitor" << std::endl;
            if (m_memoryMonitor != nullptr)
            {
                m_memoryMonitor->Stop();
            }

            lllog(9) << "DoseMainApp::Stop: Resetting work" << std::endl;
            m_work.reset();

            lllog(1) << "DoseMainApp::Stop finished" << std::endl;
        }
    }

    void DoseMainApp::NodeStateChanged(Control::NodeState nodeState)
    {
        switch (nodeState)
        {
            case Control::NodeState::FormedSystem:
            case Control::NodeState::JoinedSystem:
            {
                m_connectionHandler->Start();
                LogStatus("dose_main running...");
            }
            break;

            case Control::NodeState::AttachedNewSystem:
            {
                ENSURE (m_distribution->IsLightNode(), << "DoseMain: AttachedNewSystem called for non-lightNode! NodeId=" << m_nodeId);
                m_distribution->SetAttached(false); // Will notify subscribers that we are no longer in detached mode.
            }
            break;

            case Control::NodeState::AttachedSameSystem:
            {
                ENSURE (m_distribution->IsLightNode(), << "DoseMain: AttachedSameSystem called for non-lightNode! NodeId=" << m_nodeId);
                m_distribution->SetAttached(true); // Will notify subscribers that we are no longer in detached mode.
            }
            break;

            case Control::NodeState::DetachedFromSystem:
            {
                ENSURE (m_distribution->IsLightNode(), << "DoseMain: DetachedFromSystem called for non-lightNode! NodeId=" << m_nodeId);
                m_distribution->SetDetached(); // Will exclude all nodes and notify subscribers.
                LogStatus("dose_main is now running in detached mode!");
            }
            break;
        }
    }

    void DoseMainApp::InjectNode(const std::string& nodeName, int64_t nodeId, int64_t nodeTypeId, const std::string& dataAddress)
    {
        ENSURE (m_distribution != nullptr, << "InjectNode cmd received before StartDoseMain cmd!");
        if (m_distribution->GetCommunication().Id() == nodeId)
        {
            return;
        }

        lllog(1) << "DOSE_MAIN: InjectNode."<<
                  " NodeName=" << nodeName.c_str() <<
                  " NodeId=" <<  nodeId <<
                  " NodeTypeId=" << nodeTypeId <<
                  " DataAddress=" << dataAddress.c_str() << std::endl;

        m_distribution->InjectNode(nodeName, nodeId, nodeTypeId, dataAddress);
    }

    void DoseMainApp::ExcludeNode(int64_t nodeId, int64_t nodeTypeId)
    {
        lllog(1) << "DOSE_MAIN: ExcludeNode. "<< " NodeId=" <<  nodeId << " NodeTypeId=" << nodeTypeId << std::endl;
        m_distribution->ExcludeNode(nodeId, nodeTypeId);
    }

    void DoseMainApp::StoppedNodeIndication(int64_t nodeId)
    {
        lllog(1) << "DOSE_MAIN: StoppedNodeIndication cmd received." << " NodeId=" <<  nodeId << std::endl;
        m_distribution->ExcludeNode(nodeId);
    }

    void DoseMainApp::OnAppEvent(const ConnectionPtr & connection, bool disconnecting)
    {
        HandleAppEventHelper(connection);
        if (disconnecting)
        {
            m_pendingRegistrationHandler->RemovePendingRegistrations(connection->Id());
            m_requestHandler->HandleDisconnect(connection);
        }
    }

    void DoseMainApp::HandleAppEventHelper(const ConnectionPtr & connection)
    {
        lllout << "HandleAppEventHelper for connection " << connection->NameWithCounter() << ", id = " << connection->Id() << std::endl;

        //Handle queued requests
        m_requestHandler->HandleRequests(connection);

        //Send messages
        m_messageHandler->DistributeMessages(connection);

        //Handle pending registrations
        m_pendingRegistrationHandler->CheckForNewOrRemovedPendingRegistration(connection);
    }

    void DoseMainApp::LogStatus(const std::string& str)
    {
        lllog(1) << str.c_str() << std::endl;
        boost::asio::dispatch(m_wcoutStrand, [str]{std::wcout << str.c_str() << std::endl;});
    }
}
}
}
