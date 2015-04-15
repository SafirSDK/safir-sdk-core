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
#include <Safir/Dob/Internal/ControlConfig.h>
#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Dob/Internal/InternalDefs.h>
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
        m_strand(strand),
        m_wcoutStrand(m_strand.get_io_service()),
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
                      m_strand.wrap([this](int64_t nodeId, int64_t nodeTypeId)
                                    {
                                        ExcludeNode(nodeId, nodeTypeId);
                                    }),
                      m_strand.wrap([this]()
                                    {
                                        Stop();
                                    })),


        m_signalSet(m_strand.get_io_service())
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

    void DoseApp::Start(const std::string& nodeName,
                        int64_t nodeId,
                        int64_t nodeTypeId,
                        const std::string& dataAddress)
    {
        m_nodeId = nodeId;

        m_memoryMonitor.reset(new MemoryMonitor(m_strand.get_io_service()));

        m_distribution.reset(new Distribution(m_strand.get_io_service(),
                                              nodeName,
                                              nodeId,
                                              nodeTypeId,
                                              dataAddress));

        InitializeDoseInternalFromDoseMain(nodeId);

        m_lockMonitor.reset(new LockMonitor());

        m_messageHandler.reset(new MessageHandler(*m_distribution));

        m_requestHandler.reset(new RequestHandler(m_distribution->GetCommunication()));

        m_pendingRegistrationHandler.reset(new PendingRegistrationHandler(m_strand.get_io_service(),
                                                                          *m_distribution));

        m_poolHandler.reset(new PoolHandler(m_strand.get_io_service(),
                                            *m_distribution,
                                            [this](int64_t tid){m_pendingRegistrationHandler->CheckForPending(tid);},
                                            [this](const std::string& str){LogStatus(str);}));

        m_connectionHandler.reset(new ConnectionHandler(m_strand.get_io_service(),
                                                        *m_distribution,
                                                        [this](const ConnectionPtr& connection, bool disconnecting){OnAppEvent(connection, disconnecting);}));

        m_nodeInfoHandler.reset(new NodeInfoHandler(m_strand.get_io_service(), *m_distribution));
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

        ENSURE (m_distribution != nullptr, << "InjectNode cmd received before StartDoseMain cmd!");

        if (m_distribution->GetCommunication().Id() == nodeId)
        {
            lllog(1) << "DOSE_MAIN: Own node injected, starting distribution components!" << std::endl;

            // Own node has been included in the system state, now its time to start
            // the distribution mechanism.
            m_distribution->Start();
            m_poolHandler->Start();

            LogStatus("dose_main running...");
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

    void DoseApp::ExcludeNode(int64_t nodeId, int64_t nodeTypeId)
    {
        lllog(1) << "DOSE_MAIN: ExcludeNode cmd received."<<
                    " NodeId=" <<  nodeId <<
                    " NodeTypeId=" << nodeTypeId << std::endl;

        m_distribution->ExcludeNode(nodeId, nodeTypeId);
    }

    void DoseApp::Stop()
    {
        m_cmdReceiver.Stop();
        m_nodeInfoHandler->Stop();
        m_lockMonitor->Stop();

        m_connectionHandler->Stop();

        m_distribution->Stop();

        m_poolHandler->Stop();

        m_pendingRegistrationHandler->Stop();

        m_signalSet.cancel();

        m_memoryMonitor->Stop();

        m_work.reset();
    }

    void DoseApp::HandleSignal(const boost::system::error_code& error,
                               const int signalNumber)
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

        LogStatus("dose_main got signal " + boost::lexical_cast<std::string>(signalNumber) + ", shutting down.");

        Stop();
    }


//    void DoseApp::HandleEvents()
//    {
//        m_HandleEvents_notified = 0;
//        int numEvents = 0;

//        bool gotConnectEvent = false;
//        bool gotConnectOutEvent = false;
//        //if we have a connect event we want to ensure that
//        //we handle any outstanding disconnects ("died" flags in the
//        //connections), so we fake a connectionOutEvent.
//        //we must "harvest" the shared flag only once in this routine, so we
//        //use a local variable to avoid having a connector signal the connect
//        //event after we've already passed the out event handling code, but before
//        //we get to the connect event handling code.

//        const boost::uint32_t oldConnectEvent = m_connectEvent.compare_exchange(0, 1);
//        if (oldConnectEvent == 1)
//        {
//            m_connectionOutEvent = 1;
//            gotConnectEvent = true;
//        }

//        const boost::uint32_t oldConnectOutEvent = m_connectionOutEvent.compare_exchange(0, 1);
//        if (oldConnectOutEvent == 1)
//        {
//            gotConnectOutEvent = true;
//        }

//        if (gotConnectOutEvent)
//        {
//            ++numEvents;
//            std::vector<ConnectionPtr> deadConnections;
//            Connections::Instance().HandleConnectionOutEvents(boost::bind(&DoseApp::HandleConnectionOutEvent,this,_1,boost::ref(deadConnections)));

//            for (std::vector<ConnectionPtr>::iterator it = deadConnections.begin();
//                 it != deadConnections.end(); ++it)
//            {
//                HandleDisconnect(*it);
//                Connections::Instance().RemoveConnection(*it);
//            }
//        }

//        //we do this after connectionOutEvents
//        if (gotConnectEvent)
//        {
//            ++numEvents;
//            Connections::Instance().HandleConnect(*this);
//        }



#if 0 //stewart TODO If there are handlers that need to be notified of node status changes this should be
      //             done with a subscription mechanism provided by the NodeStatus class.

        if (m_nodeStatusChangedEvent != 0)
        {
            ++numEvents;
            m_nodeStatusChangedEvent = 0;


            lllout << "Got NodeStatusChanged event" << std::endl;

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
        }
#endif


    //    void DoseApp::AllocateStatic()
    //{
//TODO
//        m_connectionHandler.Init(
//#if 0 //stewart
//                                 m_ecom,
//#endif
//                                 m_processInfoHandler,
//                                 m_requestHandler,
//                                 m_pendingRegistrationHandler,
//                                 m_persistHandler);

//        const bool otherNodesExistAtStartup = false;

//        //we notify so that even if there were no new nodes we trigger
//        //the call to MaybeSignal...() to start letting applications connect.
//        //this also takes care of the case where we're running Standalone without
//        //persistence.
//        NodeStatusChangedNotifier();

//        m_persistHandler.Init(m_connectionHandler,m_nodeHandler,otherNodesExistAtStartup);
    //}

    void DoseApp::OnAppEvent(const ConnectionPtr & connection, bool disconnecting)
    {
        int recLevel=0;
        HandleAppEventHelper(connection, recLevel);
        if (disconnecting)
        {
            m_pendingRegistrationHandler->RemovePendingRegistrations(connection->Id());
            m_requestHandler->HandleDisconnect(connection);
#if 0 //stewart
            m_blockingHandler.RemoveConnection(connection->Id().m_id);
#endif
        }
    }

    void DoseApp::HandleAppEventHelper(const ConnectionPtr & connection, int & recursionLevel)
    {
        lllout << "HandleAppEventHelper for connection " << connection->NameWithCounter() << ", id = " << connection->Id() << std::endl;

        //---- Handle queued requests ----
        m_requestHandler->DistributeRequests(connection);

        //Send messages
        m_messageHandler->DistributeMessages(connection);

        //Handle pending registrations
        m_pendingRegistrationHandler->CheckForNewOrRemovedPendingRegistration(connection);

#if 0 //stewart
        //Check in queues, and notify waiting applications
        HandleWaitingConnections(connection->Id().m_id, recursionLevel);
#endif
    }

#if 0 //stewart This functionality is , or shall be, removed or moved to the specific handler.
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
                const ConnectionPtr connection = Connections::Instance().GetConnection
                    (ConnectionId(m_nodeId, -1, *it));
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
        tmpId.m_node=m_nodeId;
        for (IdentifierSet::const_iterator it=waiting.begin();
             it!=waiting.end(); ++it)
        {
            tmpId.m_id=*it;
            HandleAppEventHelper(Connections::Instance().GetConnection(tmpId), recursionLevel);
        }
    }
#endif

    //----------------------------------------------------------------
    // Handling of Dose_Communication events
    //----------------------------------------------------------------
    void DoseApp::HandleIncomingData(const DistributionData & /*data*/, const bool /*isAckedData*/)
    {
#if 0 //stewart
        switch (data.GetType())
        {
        case DistributionData::Action_HavePersistenceDataRequest:
        case DistributionData::Action_HavePersistenceDataResponse:
            {
                m_persistHandler.HandleMessageFromDoseCom(data);
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


        default: //Corrupted message
            {
                SEND_SYSTEM_LOG(Alert,
                                << "ERROR: Received corrupt data from DoseCom (Type = " << data.GetType() << ")");
            }
            break;
        }
#endif
    }


    void DoseApp::LogStatus(const std::string& str)
    {
        lllog(1) << str.c_str() << std::endl;
        m_wcoutStrand.dispatch([str]
                               {
                                   std::wcout << str.c_str() << std::endl;
                               });
    }

}
}
}
