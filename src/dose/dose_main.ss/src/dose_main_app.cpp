/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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
#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Dob/Internal/InternalDefs.h>
#include <Safir/Dob/Internal/MessageTypes.h>
#include <Safir/Dob/Internal/ServiceTypes.h>
#include <Safir/Dob/Internal/EntityTypes.h>
#include <Safir/Dob/Internal/EndStates.h>
#include <Safir/Dob/Internal/InjectionKindTable.h>
#include <Safir/Dob/Internal/ContextSharedTable.h>
#include <Safir/Dob/Internal/NodeStatuses.h>
#include <Safir/Dob/OverflowException.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/PanicLogging.h>
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
        bool initiateTimerHandler(boost::asio::io_service& ioService)
        {
            TimerHandler::Instantiate(ioService);
            return true;
        }

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
            std::ostringstream ostr;
            ostr << "dose_main has generated a dump to:\n" 
                 << dumpPath << "\n"
                 << "Please send this file to your nearest Dob developer, along with\n"
                 << "relevant information about what version of Safir SDK you are using" << std::endl;
            lllerr << ostr.str().c_str();
            Safir::Utilities::Internal::PanicLogging::Log(ostr.str());
        }
    }

    DoseApp::DoseApp():
        m_connectEvent(0),
        m_connectionOutEvent(0),
        m_nodeStatusChangedEvent(0),
        m_signalHandler(m_ioService),
        m_timerHandlerInitiated(initiateTimerHandler(m_ioService)),
        m_poolHandler(m_ioService),
        m_pendingRegistrationHandler(m_ecom, m_nodeHandler),
        m_ecom(m_ioService),
        m_HandleEvents_notified(0),
        m_DispatchOwnConnection_notified(0)
    {
        Safir::Utilities::CrashReporter::RegisterCallback(DumpFunc);
        Safir::Utilities::CrashReporter::Start();

        m_processMonitor.Init(ProcessExited);
    }

    DoseApp::~DoseApp()
    {
        if (m_memoryMonitorThread != boost::thread())
        {
            m_memoryMonitorThread.interrupt();
            m_memoryMonitorThread.join();
            m_memoryMonitorThread = boost::thread();
        }

        if (m_connectionThread != boost::thread())
        {
            //set the interrupt state so that when we generate the spurious signal
            //the thread will be interrupted at the interruption_point.
            m_connectionThread.interrupt();
            Connections::Instance().GenerateSpuriousConnectOrOutSignal();
            m_connectionThread.join();
            m_connectionThread = boost::thread();
        }

    }


    void DoseApp::Run()
    {
        AllocateStatic();

        // Start monitoring of this thread (that is, the main thread)
        m_mainThreadId = boost::this_thread::get_id();
        m_threadMonitor.StartWatchdog(m_mainThreadId, "dose_main main thread");

        // Schedule a timer so that the main thread will kick the watchdog.
        TimerInfoPtr timerInfo(new EmptyTimerInfo(TimerHandler::Instance().RegisterTimeoutHandler(L"dose_main watchdog timer", *this)));
        TimerHandler::Instance().Set(Discard,
                                     timerInfo,
                                     GetUtcTime() + 5.0);

        // enter main loop
#ifndef NDEBUG
        lllerr<<"dose_main running (debug)..." << std::endl;
#else
        lllerr<<"dose_main running (release)..." << std::endl;
#endif

        //we want the io service to keep running for ever.
        boost::asio::io_service::work keepRunning(m_ioService);
        
        m_ioService.run();
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
                    m_ioService.post(boost::bind(&DoseApp::HandleEvents,this));
                }
            }
        }
        catch (const boost::thread_interrupted&)
        {
            //do nothing, just exit
        }
    }

    void DoseApp::OnDoDispatch()
    {
        if (m_DispatchOwnConnection_notified == 0)
        {
            m_DispatchOwnConnection_notified = 1;
            m_ioService.post(boost::bind(&DoseApp::DispatchOwnConnection,this));
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
    }

    void DoseApp::HandleTimeout(const TimerInfoPtr& timer)
    {
        m_threadMonitor.KickWatchdog(m_mainThreadId);

        TimerHandler::Instance().Set(Discard,
                                     timer,
                                     GetUtcTime() + 5.0);
    }

    ConnectResult DoseApp::CanAddConnection(const std::string & connectionName, const pid_t pid, const long /*context*/)
    {
        switch (m_processInfoHandler.CanAddConnectionFromProcess(pid))
        {
        case TooManyProcesses:
            {
                lllerr << "Could not let new connection '" << connectionName.c_str()
                    << "' from process with pid = " << pid <<
                    " connect since there are too many processes connected." << std::endl
                    << "Please increase the parameter Safir.Dob.ProcessInfo.MaxNumberOfInstances." << std::endl;
                return TooManyProcesses;
            }
            break;

        case TooManyConnectionsInProcess:
            {
                lllerr << "Could not let new connection from process with pid = " << pid <<
                    " connect since there are too many connections '" << connectionName.c_str()
                    << "'from that process." << std::endl
                    << "Please increase the size of the ConnectionNames array of class Safir.Dob.ProcessInfo." << std::endl;
                return TooManyConnectionsInProcess;
            }
            break;

        case Success:
            return Success;

        default:
            ENSURE(false, << "Got unexpected result from ProcessInfoHandler::CanAddConnectionFromProcess!");
            return Undefined;
        }
    }


    void DoseApp::AllocateStatic()
    {
        Connections::Cleanup();
        ContextSharedTable::Initialize();
        MessageTypes::Initialize(/*iAmDoseMain = */ true);
        EndStates::Initialize();
        ServiceTypes::Initialize(/*iAmDoseMain = */ true);
        InjectionKindTable::Initialize();
        NodeStatuses::Initialize();
        EntityTypes::Initialize(/*iAmDoseMain = */ true);
        
        m_connectionHandler.Init(m_ecom,
                                 m_processInfoHandler,
                                 m_requestHandler,
                                 m_pendingRegistrationHandler,
                                 m_nodeHandler,
                                 m_persistHandler);
        
        const bool otherNodesExistAtStartup =
            m_ecom.Init(boost::bind(&DoseApp::HandleIncomingData, this, _1, _2),
                        boost::bind(&DoseApp::QueueNotFull, this),
                        boost::bind(&DoseApp::NodeStatusChangedNotifier, this),
                        boost::bind(&DoseApp::StartPoolDistribution,this),
                        boost::bind(&DoseApp::RequestPoolDistribution,this, _1));
        
        //we notify so that even if there were no new nodes we trigger
        //the call to MaybeSignal...() to start letting applications connect.
        //this also takes care of the case where we're running Standalone without
        //persistence.
        NodeStatusChangedNotifier();
        
        m_messageHandler.Init(m_blockingHandler,m_ecom);
        
        m_responseHandler.Init(m_blockingHandler, m_ecom);
        m_requestHandler.Init(m_blockingHandler, m_ecom, m_responseHandler);
        
        m_ownConnection.Open(L"dose_main",L"own",0,NULL,this);
        
        m_poolHandler.Init(m_blockingHandler,
                           m_ecom,
                           m_pendingRegistrationHandler,
                           m_persistHandler,
                           m_connectionHandler,
                           m_threadMonitor);
        
        
        m_processInfoHandler.Init(m_ecom,m_processMonitor);
        
        m_nodeHandler.Init (m_ecom, m_requestHandler, m_poolHandler);
        
        m_connectionThread = boost::thread(boost::bind(&DoseApp::ConnectionThread,this));
        
        m_persistHandler.Init(m_ecom,m_connectionHandler,m_nodeHandler,otherNodesExistAtStartup);
        
        m_memoryMonitorThread = boost::thread(&DoseApp::MemoryMonitorThread);
    }

    void DoseApp::HandleConnect(const ConnectionPtr & connection)
    {
        lllout << "ConnectionHandler::HandleConnect: New connection from " << connection->NameWithCounter() << " id = " << connection->Id() << std::endl;
        m_connectionHandler.HandleConnect(connection);
        m_processInfoHandler.ConnectionAdded(connection);
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

        m_connectionHandler.HandleDisconnect(connection);

        // Remove the connection from the processInfo structure
        m_processInfoHandler.ConnectionRemoved(connection);

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
        m_responseHandler.DistributeResponses(connection);
        m_requestHandler.DistributeRequests(connection);

        //Send messages
        m_messageHandler.DistributeMessages(connection);

        //Handle pending registrations
        m_pendingRegistrationHandler.CheckForNewOrRemovedPendingRegistration(connection);

        //Send injections to other nodes. (the injection itself has already been performed)
        //m_poolHandler.SendInjections(connection);

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
        if (m_blockingHandler.State().GetWaitingConnections(blockingApp, waiting))
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
                m_messageHandler.DistributeMessages(connection);
                
                //If the connection is dead it might be a zombie that has been waiting for dosecom.
                //signal it so that we try to finish removing it again.
                if (connection->IsDead())
                {
                    connection->SignalOut();
                }
            }
        }
    }

    void DoseApp::WaitingConnectionsHelper(const Identifier blockingApp,
                                           IdentifierSet & waiting,
                                           int & recursionLevel)
    {
        ConnectionId tmpId;
        tmpId.m_node=Dob::ThisNodeParameters::NodeNumber();
        for (IdentifierSet::const_iterator it=waiting.begin();
             it!=waiting.end(); ++it)
        {
            tmpId.m_id=*it;

            if (tmpId.m_id == ExternNodeCommunication::DoseComVirtualConnectionId)
            {
                // dose_com virtual connection has been waiting for the blocking app
                m_requestHandler.HandlePendingExternalRequest(blockingApp);
            }
            else
            {
                HandleAppEventHelper(Connections::Instance().GetConnection(tmpId), recursionLevel);
            }
        }
    }

 


    //----------------------------------------------------------------
    // Handling of Dose_Communication events
    //----------------------------------------------------------------
    void DoseApp::HandleIncomingData(const DistributionData & data, const bool isAckedData)
    {
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
                lllerr << "ERROR: Received corrupt data from DoseCom (Type = " << data.GetType() << ")" <<std::endl;
                lllerr << "Please contact your nearest DOB developer!" << std::endl;
            }
            break;
        }
    }

    void DoseApp::NodeStatusChangedNotifier()
    {
        m_nodeStatusChangedEvent = true;
        if (m_HandleEvents_notified == 0)
        {
            m_HandleEvents_notified = 1;
            m_ioService.post(boost::bind(&DoseApp::HandleEvents,this));
        }
    }

    void DoseApp::QueueNotFull()
    {
        lllout << "DoseApp::QueueNotFull: Calling HandleUnsent()" << std::endl;
        m_connectionHandler.HandleUnsent();

        lllout << "DoseApp::QueueNotFull: Calling HandleWaitingConnections(...)" << std::endl;
        int recLevel=0;
        HandleWaitingConnections(ExternNodeCommunication::DoseComVirtualConnectionId, recLevel);

        lllout << "DoseApp::QueueNotFull: Calling DistributePoolChanges()" << std::endl;
        m_poolHandler.DistributeStates();
        lllout << "DoseApp::QueueNotFull: End" << std::endl;
    }

    void DoseApp::StartPoolDistribution()
    {
        m_poolHandler.StartPoolDistribution();
    }

    void DoseApp::RequestPoolDistribution(const int nodeId)
    {
       m_poolHandler.RequestPoolDistribution(nodeId);
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
                        std::wostringstream ostr;
                        ostr << "Less than " << m_warningPercent << "% of the Dob shared memory is available!" << std::endl
                             << "This probably means that you're close to running out of memory!" << std::endl
                             << "Please increase the parameter Safir.Dob.NodeParameters.SharedMemorySize.";
                        Safir::Utilities::Internal::PanicLogging::Log(Safir::Dob::Typesystem::Utilities::ToUtf8(ostr.str()));
                        lllerr << ostr.str() << std::endl;
                    }
                }
                catch (const std::exception& exc)
                {
                    std::ostringstream ostr;
                    ostr << "Got exception in dose_main MemoryMonitor: " << std::endl
                        << exc.what();
                    Safir::Utilities::Internal::PanicLogging::Log(ostr.str());
                }
                catch (...)
                {
                    std::wostringstream ostr;
                    ostr << "Got ... exception in dose_main MemoryMonitor!";
                    Safir::Utilities::Internal::PanicLogging::Log(Safir::Dob::Typesystem::Utilities::ToUtf8(ostr.str()));
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
                boost::this_thread::sleep(boost::posix_time::seconds(5));
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

