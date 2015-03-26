/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
* Copyright Consoden AB, 2015 (http://www.consoden.se)
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

#include "dose_main_app.h"
#include "dose_main_timers.h"
#include <Safir/Dob/Internal/ControlConfig.h>
#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Dob/Internal/InternalDefs.h>
#include <Safir/Dob/Internal/NodeStatuses.h>
#include <Safir/Dob/Internal/Initialize.h>
#include <Safir/Dob/OverflowException.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Utilities/CrashReporter.h>
#include <boost/bind.hpp>
#include <iostream>

namespace Safir
{
namespace Dob
{
namespace Internal
{

    namespace //anonymous namespace
    {
        void SetDiedIfPidEquals(const ConnectionPtr& connection, const pid_t pid)
        {
            if (connection->Pid() == pid)
            {
                connection->Died();
            }
        }

        void ProcessExited(const pid_t& pid)
        {
            Connections::Instance().ForEachConnectionPtr(boost::bind(SetDiedIfPidEquals,_1,pid));
        }

        void DumpFunc(const char* const dumpPath)
        {
            std::wostringstream ostr;
            SEND_SYSTEM_LOG(Alert,
                            << "dose_main has generated a dump to:\n"
                            << dumpPath << "\n"
                            << "Please send this file to your nearest Dob developer, along with\n"
                            << "relevant information about what version of Safir SDK you are using");
        }
    }

    DoseApp::DoseApp(boost::asio::io_service::strand& strand):
        m_connectEvent(0),
        m_connectionOutEvent(0),
        m_strand(strand),
        m_work(new boost::asio::io_service::work(m_strand.get_io_service())),
        m_distribution(),
        m_cmdReceiver(m_strand.get_io_service(),
                      m_strand.wrap([this](const std::string& nodeName,
                                           int64_t nodeId,
                                           int64_t nodeTypeId,
                                           const std::string& dataAddress)
                                    {
                                        Start(nodeName,
                                              nodeId,
                                              nodeTypeId,
                                              dataAddress);
                                    }),
                      m_strand.wrap([this](const std::string& nodeName,
                                           int64_t nodeId,
                                           int64_t nodeTypeId,
                                           const std::string& dataAddress)
                                    {
                                        InjectNode(nodeName,
                                                   nodeId,
                                                   nodeTypeId,
                                                   dataAddress);
                                    }),
                      m_strand.wrap([this]()
                                    {
                                        Stop();
                                    })),


        m_signalSet(m_strand.get_io_service()),
        m_timerHandler(m_strand),
        m_processMonitor(m_strand.get_io_service(),ProcessExited,boost::chrono::seconds(1)),
        m_HandleEvents_notified(0),
        m_DispatchOwnConnection_notified(0)
    {
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

        m_signalSet.async_wait(m_strand.wrap([this](const boost::system::error_code& error,
                                                    const int signalNumber)
                                            {
                                                HandleSignal(error, signalNumber);
                                            }));

        Safir::Utilities::CrashReporter::RegisterCallback(DumpFunc);
        Safir::Utilities::CrashReporter::Start();

        // Start reception of commands from Control
        m_cmdReceiver.Start();
    }

    DoseApp::~DoseApp()
    {
        //TODO stewart: should we try to call Stop here, in case it hasn't been
        //called correctly? I.e. we're dying through an exception.
    }

    void DoseApp::ConnectionThread()
    {
        try
        {
            for (;;)
            {
                bool connect, connectionOut;
                Connections::Instance().WaitForDoseMainSignal(connect, connectionOut);

                boost::this_thread::interruption_point();

                //Note that we cannot just do this->m_connectionOut = connectionOut, since that might clear flags that
                //have not been handled yet.
                if (connect)
                {
                    m_connectEvent = 1;
                }
                if (connectionOut)
                {
                    m_connectionOutEvent = 1;
                }

                if (m_HandleEvents_notified == 0)
                {
                    m_HandleEvents_notified = 1;
                    m_strand.post([this]()
                                  {
                                      HandleEvents();
                                  });
                }
            }
        }
        catch (const boost::thread_interrupted&)
        {
            //do nothing, just exit
        }
    }

    void DoseApp::Start(const std::string& nodeName,
                        int64_t nodeId,
                        int64_t nodeTypeId,
                        const std::string& dataAddress)
    {
        m_distribution.reset(new Distribution(m_strand.get_io_service(),
                                              nodeName,
                                              nodeId,
                                              nodeTypeId,
                                              dataAddress));

        InitializeDoseInternalFromDoseMain(nodeId);

        m_lockMonitor.reset(new LockMonitor());
        // Collect all node type ids
        NodeTypeIds nodeTypeIds;
        for (const auto& nt: m_distribution->GetNodeTypeConfiguration().nodeTypesParam)
        {
            nodeTypeIds.insert(nt.id);
        }

        m_messageHandler.reset(new MessageHandler(m_distribution->GetCommunication(),
                                                  nodeTypeIds));

        m_responseHandler.reset(new ResponseHandler(m_timerHandler,
                                                    m_blockingHandler,
                                                    m_distribution->GetCommunication()));

        m_requestHandler.reset(new RequestHandler(m_timerHandler,
                                                  m_blockingHandler,
                                                  *m_responseHandler,
                                                  m_distribution->GetCommunication()));

        m_pendingRegistrationHandler.reset(new PendingRegistrationHandler(m_timerHandler,nodeId));

        m_poolHandler.reset(new PoolHandler(m_strand.get_io_service(),
                                            *m_distribution,
                                            [this](int64_t tid){m_pendingRegistrationHandler->CheckForPending(tid);}));

        m_connectionHandler.reset(new ConnectionHandler(m_strand.get_io_service(),
                                                        m_distribution->GetCommunication(),
                                                        *m_requestHandler,
                                                        *m_pendingRegistrationHandler));

        //setup pdComplete sub

        m_connectionThread = boost::thread([this]() {ConnectionThread();});


#ifndef NDEBUG
        std::wcout<<"dose_main running (debug)..." << std::endl;
#else
        std::wcout<<"dose_main running (release)..." << std::endl;
#endif

    }

    void DoseApp::InjectNode(const std::string& nodeName,
                             int64_t nodeId,
                             int64_t nodeTypeId,
                             const std::string& dataAddress)
    {
        lllog(1) << "DOSE_MAIN: InjectNode cmd received."<<
                  " NodeName=" << nodeName.c_str() <<
                  " NodeId=" <<  nodeId <<
                  " NodeTypeId=" << nodeTypeId <<
                  " DataAddress=" << dataAddress.c_str() << std::endl;

        if (m_distribution == nullptr)
        {
            throw std::logic_error("InjectNode cmd received before StartDoseMain cmd!");
        }

        if (m_distribution->GetCommunication().Id() == nodeId)
        {
            lllog(1) << "DOSE_MAIN: Own node injected, starting distribution components!" << std::endl;

            // Own node has been included in the system state, now its time to start
            // the distribution mechanism.
            m_distribution->Start();
            m_poolHandler->Start([this]
            {
                //OnPdComplete: Probably more things to do here
                std::wcout<<"PD complete"<<std::endl;
                m_connectionHandler->OnPoolDistributionComplete();
            });
        }
        else
        {
            lllog(1) << "DOSE_MAIN: Injecting Node."<<
                      " NodeName=" << nodeName.c_str() <<
                      " NodeId=" <<  nodeId <<
                      " NodeTypeId=" << nodeTypeId <<
                      " DataAddress=" << dataAddress.c_str() << std::endl;

            m_distribution->InjectNode(nodeName,
                                       nodeId,
                                       nodeTypeId,
                                       dataAddress);
        }
    }

    void DoseApp::Stop()
    {
        m_cmdReceiver.Stop();

        m_timerHandler.Stop();

        m_lockMonitor->Stop();

        m_processMonitor.Stop();

        if (m_memoryMonitorThread.get_id() != boost::thread::id())
        {
            m_memoryMonitorThread.interrupt();
            m_memoryMonitorThread.join();
            m_memoryMonitorThread = boost::thread();
        }

        if (m_connectionThread.get_id() != boost::thread::id())
        {
            //set the interrupt state so that when we generate the spurious signal
            //the thread will be interrupted at the interruption_point.
            m_connectionThread.interrupt();
            Connections::Instance().GenerateSpuriousConnectOrOutSignal();
            m_connectionThread.join();
            m_connectionThread = boost::thread();
        }

        if (m_distribution != nullptr)
        {
            m_distribution->Stop();
        }

        m_poolHandler->Stop();
        m_ownConnection.Close();

        m_signalSet.cancel();

        m_work.reset();
    }

    void DoseApp::HandleSignal(const boost::system::error_code& error,
                               const int signalNumber)
    {
        std::wcout << "Got signal " << signalNumber << " shutting down." << std::endl;

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

        Stop();
    }

    void DoseApp::OnDoDispatch()
    {
        if (m_DispatchOwnConnection_notified == 0)
        {
            m_DispatchOwnConnection_notified = 1;
            m_strand.post([this]()
                          {
                              DispatchOwnConnection();
                          });
        }
    }

    void DoseApp::DispatchOwnConnection()
    {
        //dispatch own connection!
        m_DispatchOwnConnection_notified = 0;
        m_ownConnection.Dispatch();
    }


    void DoseApp::HandleEvents()
    {
        m_HandleEvents_notified = 0;
        int numEvents = 0;

        bool gotConnectEvent = false;
        bool gotConnectOutEvent = false;
        //if we have a connect event we want to ensure that
        //we handle any outstanding disconnects ("died" flags in the
        //connections), so we fake a connectionOutEvent.
        //we must "harvest" the shared flag only once in this routine, so we
        //use a local variable to avoid having a connector signal the connect
        //event after we've already passed the out event handling code, but before
        //we get to the connect event handling code.

        const boost::uint32_t oldConnectEvent = m_connectEvent.compare_exchange(0, 1);
        if (oldConnectEvent == 1)
        {
            m_connectionOutEvent = 1;
            gotConnectEvent = true;
        }

        const boost::uint32_t oldConnectOutEvent = m_connectionOutEvent.compare_exchange(0, 1);
        if (oldConnectOutEvent == 1)
        {
            gotConnectOutEvent = true;
        }

        if (gotConnectOutEvent)
        {
            ++numEvents;
            std::vector<ConnectionPtr> deadConnections;
            Connections::Instance().HandleConnectionOutEvents(boost::bind(&DoseApp::HandleConnectionOutEvent,this,_1,boost::ref(deadConnections)));

            for (std::vector<ConnectionPtr>::iterator it = deadConnections.begin();
                 it != deadConnections.end(); ++it)
            {
                HandleDisconnect(*it);
                Connections::Instance().RemoveConnection(*it);
            }
        }

        //we do this after connectionOutEvents
        if (gotConnectEvent)
        {
            ++numEvents;
            Connections::Instance().HandleConnect(*this);
        }

#if 0 //stewart TODO If there are handlers that need to be notified of node status changes this should be
      //             done with a subscription mechanism provided by the NodeStatus class.

        if (m_nodeStatusChangedEvent != 0)
        {
            ++numEvents;
            m_nodeStatusChangedEvent = 0;


            lllout << "Got NodeStatusChanged event" << std::endl;
            m_nodeHandler.HandleNodeStatusChanges();

            const NodeStatuses::Status nodeStatuses = NodeStatuses::Instance().GetNodeStatuses();
            const bool atLeastOneNodeIsUp = std::count(nodeStatuses.begin(),nodeStatuses.end(),NodeStatus::Started) >= 1;

            //if there is at least one UP node we will have received persistence data from someone else
            if (!m_persistHandler.IsPersistentDataReady() && //we havent already signalled persistence available
                atLeastOneNodeIsUp) //someone else than me up
            {
                m_persistHandler.SetPersistentDataReady();
            }

            if (m_persistHandler.IsPersistentDataReady())
            {

                lllout << "Calling SetOkToSignalPDComplete, since this node has now fulfilled the requirements for signalling PD complete" << std::endl;

                m_ecom.SetOkToSignalPDComplete();

            }
            m_connectionHandler.MaybeSignalConnectSemaphore();
            m_pendingRegistrationHandler.CheckForPending();
        }
#endif

    }

    ConnectResult DoseApp::CanAddConnection(const std::string & connectionName, const pid_t pid, const long /*context*/)
    {

        return Success;
#if 0 //stewart
        switch (m_processInfoHandler.CanAddConnectionFromProcess(pid))
        {
        case TooManyProcesses:
            {
                SEND_SYSTEM_LOG(Critical,
                                << "Could not let new connection '" << connectionName.c_str()
                                << "' from process with pid = " << pid
                                << " connect since there are too many processes connected. "
                                << "Increase Safir.Dob.ProcessInfo.MaxNumberOfInstances.");
                return TooManyProcesses;
            }
            break;

        case TooManyConnectionsInProcess:
            {
                SEND_SYSTEM_LOG(Critical,
                                << "Could not let new connection '" << connectionName.c_str()
                                << "' from process with pid = " << pid
                                << " connect since there are too many connections from that process. "
                                << "Increase length of Safir.Dob.ProcessInfo.ConnectionNames.");
                return TooManyConnectionsInProcess;
            }
            break;

        case Success:
            return Success;

        default:
            ENSURE(false, << "Got unexpected result from ProcessInfoHandler::CanAddConnectionFromProcess!");
            return Undefined;
        }
#endif
    }


    void DoseApp::AllocateStatic()
    {
//TODO
//        m_connectionHandler.Init(
//#if 0 //stewart
//                                 m_ecom,
//#endif
//                                 m_processInfoHandler,
//                                 m_requestHandler,
//                                 m_pendingRegistrationHandler,
//                                 m_nodeHandler,
//                                 m_persistHandler);

//        const bool otherNodesExistAtStartup = false;
//#if 0 //stewart
//            m_ecom.Init(boost::bind(&DoseApp::HandleIncomingData, this, _1, _2),
//                        boost::bind(&DoseApp::QueueNotFull, this),
//                        boost::bind(&DoseApp::NodeStatusChangedNotifier, this),
//                        boost::bind(&DoseApp::StartPoolDistribution,this),
//                        boost::bind(&DoseApp::RequestPoolDistribution,this, _1));
//#endif

//        //we notify so that even if there were no new nodes we trigger
//        //the call to MaybeSignal...() to start letting applications connect.
//        //this also takes care of the case where we're running Standalone without
//        //persistence.
//        NodeStatusChangedNotifier();

//        m_ownConnection.Open(L"dose_main",L"own",0,NULL,this);

//        m_poolHandler.Init(m_pendingRegistrationHandler,
//                           m_persistHandler,
//                           m_connectionHandler);


//        m_processInfoHandler.Init(m_processMonitor);

//        m_nodeHandler.Init (m_requestHandler, m_poolHandler);

//        m_connectionThread = boost::thread(boost::bind(&DoseApp::ConnectionThread,this));

//        m_persistHandler.Init(m_connectionHandler,m_nodeHandler,otherNodesExistAtStartup);

//        m_memoryMonitorThread = boost::thread(&DoseApp::MemoryMonitorThread);
    }

    void DoseApp::HandleConnect(const ConnectionPtr & connection)
    {
        lllout << "ConnectionHandler::HandleConnect: New connection from " << connection->NameWithCounter() << " id = " << connection->Id() << std::endl;
        m_connectionHandler->HandleConnect(connection);

        // stewart
        //m_processInfoHandler.ConnectionAdded(connection);
    }

    void DoseApp::HandleDisconnect(const ConnectionPtr & connection)
    {
        lllout << "ConnectionHandler::HandleDisconnect: Disconnected " << connection->NameWithCounter() << " id = " << connection->Id() << std::endl;

        //try to handle some outstanding stuff (this does not guarantee that all gets handled,
        // e.g. dose_com overflow may stop something in here.).
        std::vector<ConnectionPtr> dummy;
        HandleConnectionOutEvent(connection,dummy);

        //if message out queue is not empty we've failed to send the msgs
        //because of dose_com overflow. We will have been added to the blocking handler
        //and so we can just leave the connection in here for the time being
        //and the blocking handler will make sure that we retry the disconnect
        if (!connection->GetMessageOutQueue().empty())
        {
            return;
        }

        m_connectionHandler->HandleDisconnect(connection);

        // Remove the connection from the processInfo structure
        //stewart
        //m_processInfoHandler.ConnectionRemoved(connection);

        // Classes have been unregistered, inform waiting connections
        int recLevel=0;
        HandleWaitingConnections(connection->Id().m_id, recLevel);

        // Remove any remaining traces of the connection from the blocking
        // structure
        m_blockingHandler.RemoveConnection(connection->Id().m_id);
    }


    void DoseApp::HandleConnectionOutEvent(const ConnectionPtr & connection, std::vector<ConnectionPtr>& deadConnections)
    {
        //        lllout << "Handling event from " << connection->Name() << " id = " << connection->Id() << std::endl;

        int recLevel=0;
        HandleAppEventHelper(connection, recLevel);

        if (connection->IsDead())
        {
            lllout << "Connection is dead: " << connection->NameWithCounter() << ", disconnecting."<< std::endl;
            deadConnections.push_back(connection);
        }
    }


    void DoseApp::HandleAppEventHelper(const ConnectionPtr & connection, int & recursionLevel)
    {
        lllout << "HandleAppEventHelper for connection " << connection->NameWithCounter() << ", id = " << connection->Id() << std::endl;

        //---- Handle queued requests ----
        m_responseHandler->DistributeResponses(connection);
        m_requestHandler->DistributeRequests(connection);

        //Send messages
        m_messageHandler->DistributeMessages(connection);

        //Handle pending registrations
        m_pendingRegistrationHandler->CheckForNewOrRemovedPendingRegistration(connection);

        //Check in queues, and notify waiting applications
        HandleWaitingConnections(connection->Id().m_id, recursionLevel);
    }

    //Check if blockingApp has non-full inQueues. If thats the case, it handles applications that are blocked
    //by blockingApp
    void DoseApp::HandleWaitingConnections(const Identifier blockingApp,
                                           int & recursionLevel)
    {
        const int MAX_RECURSION_LEVEL=4;
        recursionLevel++;
        if (recursionLevel>MAX_RECURSION_LEVEL)
        {
            return;
        }

        IdentifierSet waiting;
        if (m_blockingHandler.Response().GetWaitingConnections(blockingApp, waiting))
        {
            WaitingConnectionsHelper(blockingApp, waiting, recursionLevel);
        }

        waiting.clear();
        if (m_blockingHandler.Request().GetWaitingConnections(blockingApp, waiting))
        {
            WaitingConnectionsHelper(blockingApp, waiting, recursionLevel);
        }

        waiting.clear();
        if (m_blockingHandler.Message().GetWaitingConnections(blockingApp, waiting))
        {
            //traverse the message queues of the apps that have been waiting for the dosecom
            //queue to empty
            for (IdentifierSet::iterator it = waiting.begin();
                it != waiting.end(); ++it)
            {
                const ConnectionPtr connection = Connections::Instance().GetConnection(ConnectionId(ThisNodeParameters::NodeNumber(), -1, *it));
                m_messageHandler->DistributeMessages(connection);

                //If the connection is dead it might be a zombie that has been waiting for dosecom.
                //signal it so that we try to finish removing it again.
                if (connection->IsDead())
                {
                    connection->SignalOut();
                }
            }
        }
    }

    void DoseApp::WaitingConnectionsHelper(const Identifier /*blockingApp*/,
                                           IdentifierSet & waiting,
                                           int & recursionLevel)
    {
        ConnectionId tmpId;
        tmpId.m_node=Dob::ThisNodeParameters::NodeNumber();
        for (IdentifierSet::const_iterator it=waiting.begin();
             it!=waiting.end(); ++it)
        {
            tmpId.m_id=*it;

#if 0 //stewart
            if (tmpId.m_id == ExternNodeCommunication::DoseComVirtualConnectionId)
            {
                // dose_com virtual connection has been waiting for the blocking app
                m_requestHandler.HandlePendingExternalRequest(blockingApp);
            }
            else
            {
#endif
                HandleAppEventHelper(Connections::Instance().GetConnection(tmpId), recursionLevel);
#if 0 //stewart
            }
#endif
        }
    }




    //----------------------------------------------------------------
    // Handling of Dose_Communication events
    //----------------------------------------------------------------
    void DoseApp::HandleIncomingData(const DistributionData & /*data*/, const bool /*isAckedData*/)
    {
#if 0 //stewart
        switch (data.GetType())
        {
        case DistributionData::Action_Connect:
            {
                m_connectionHandler.HandleConnectFromDoseCom(data);
                m_poolHandler.HandleConnectFromDoseCom(data.GetSenderId());
            }
            break;

        case DistributionData::Action_Disconnect:
            {
                m_connectionHandler.HandleDisconnectFromDoseCom(data);
                m_poolHandler.HandleDisconnectFromDoseCom(data.GetSenderId());
            }
            break;

        case DistributionData::RegistrationState:
        case DistributionData::EntityState:
            {
                m_poolHandler.HandleStateFromDoseCom(data, isAckedData);
            }
            break;

        case DistributionData::Action_PendingRegistrationRequest:
        case DistributionData::Action_PendingRegistrationResponse:
            {
                m_pendingRegistrationHandler.HandleMessageFromDoseCom(data);
            }
            break;

        case DistributionData::Action_HavePersistenceDataRequest:
        case DistributionData::Action_HavePersistenceDataResponse:
            {
                m_persistHandler.HandleMessageFromDoseCom(data);
            }
            break;

        case DistributionData::Action_RequestPoolDistribution:
            {
                m_poolHandler.HandleMessageFromDoseCom(data);
            }
            break;

            //----------------------------------
            // Requests
            //----------------------------------
        case DistributionData::Request_Service:
        case DistributionData::Request_EntityCreate:
        case DistributionData::Request_EntityUpdate:
        case DistributionData::Request_EntityDelete:
            m_requestHandler.HandleRequestFromDoseCom(data);
            break;

            //----------------------------------
            // Replies
            //----------------------------------
        case DistributionData::Response:
            m_responseHandler.HandleResponseFromDoseCom(data);
            break;

            //----------------------------------
            // Messages
            //----------------------------------
        case DistributionData::Message:
            m_messageHandler.HandleMessageFromDoseCom(data);
            break;

        default: //Corrupted message
            {
                SEND_SYSTEM_LOG(Alert,
                                << "ERROR: Received corrupt data from DoseCom (Type = " << data.GetType() << ")");
            }
            break;
        }
#endif
    }

    void DoseApp::QueueNotFull()
    {
        lllout << "DoseApp::QueueNotFull: Calling HandleUnsent()" << std::endl;
        m_connectionHandler->HandleUnsent();

#if 0 //stewart
        lllout << "DoseApp::QueueNotFull: Calling HandleWaitingConnections(...)" << std::endl;
        int recLevel=0;
        HandleWaitingConnections(ExternNodeCommunication::DoseComVirtualConnectionId, recLevel);
#endif
    }

    class MemoryMonitor:
        public SharedMemoryObject,
        private boost::noncopyable
    {
    public:
        MemoryMonitor():
            m_capacity(GetSharedMemory().get_size()),
            m_warningPercent(20)
        {

        }

        void Check()
        {
            try
            {
                try
                {
                    const size_t free = GetSharedMemory().get_free_memory();
                    const double percentFree = static_cast<double>(free)/static_cast<double>(m_capacity) * 100;
                    if (percentFree < m_warningPercent)
                    {
                        SEND_SYSTEM_LOG(Alert,
                                        << "Less than " << m_warningPercent << "% of the Dob shared memory is available!"
                                        << "This probably means that you're close to running out of memory!"
                                        << "Increase Safir.Dob.NodeParameters.SharedMemorySize.");
                    }
                }
                catch (const std::exception& exc)
                {
                    SEND_SYSTEM_LOG(Alert,
                                    << "Got exception in dose_main MemoryMonitor: "
                                    << exc.what());
                }
                catch (...)
                {
                    SEND_SYSTEM_LOG(Alert,
                                    << "Got ... exception in dose_main MemoryMonitor!");
                }
            }
            catch(...)
            {

            }
        }
    private:
        const size_t m_capacity;
        const double m_warningPercent;
    };

    void DoseApp::MemoryMonitorThread()
    {
        try
        {
            MemoryMonitor monitor;
            for (;;)
            {
                boost::this_thread::sleep_for(boost::chrono::seconds(5));
                monitor.Check();
            }
        }
        catch (const boost::thread_interrupted&)
        {
            //do nothing, just exit
        }
    }

}
}
}
