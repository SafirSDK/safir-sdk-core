/******************************************************************************
*
* Copyright Consoden AB, 2015 (http://www.consoden.se)
*
* Created by: Joel Ottosson / joel.ottosson@consoden.se
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
#include <Safir/Dob/NodeParameters.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include "ConnectionHandler.h"
#include "PendingRegistrationHandler.h"
#include "RequestHandler.h"

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace
{
    static const int64_t ConnectionMessageDataTypeId=4477521173098643793; //DoseMain.ConnectionMessage
}

    ConnectionHandler::ConnectionHandler(boost::asio::io_service& ioService,
                                         Com::Communication& communication,
                                         const std::unordered_set<int64_t>& nodeTypeIds,
                                         RequestHandler& requesthandler,
                                         PendingRegistrationHandler& prh)
        : m_strand(ioService),
          m_communication(communication),
          m_requestHandler(requesthandler),
          m_pendingRegistrationHandler(prh),
          m_processInfoHandler()
    {
        for (auto nt : nodeTypeIds)
        {
            m_sendQueues.insert(std::make_pair(nt, SendQueue()));
            m_communication.SetQueueNotFullCallback([=](int64_t){HandleSendQueues();}, nt);
        }

        m_communication.SetDataReceiver([=](int64_t /*fromNodeId*/, int64_t /*fromNodeType*/, const char *data, size_t /*size*/)
        {
            const DistributionData state=DistributionData::ConstConstructor(new_data_tag, data);
            DistributionData::DropReference(data);
            if (state.GetType()==DistributionData::Action_Connect)
            {
                Connections::Instance().AddConnection(state.GetConnectionName(), state.GetCounter(), state.GetSenderId().m_contextId, state.GetSenderId());
            }
            else if (state.GetType()==DistributionData::Action_Disconnect)
            {
                const ConnectionPtr connection = Connections::Instance().GetConnection(state.GetSenderId(), std::nothrow);
                m_requestHandler.HandleDisconnect(connection); //Handle outstanding requests towards the disconnected app ...
                Connections::Instance().RemoveConnection(connection);
            }
        }, ConnectionMessageDataTypeId, [=](size_t s){return DistributionData::NewData(s);});

        //start connect thread
        m_connectEvent=false;
        m_connectionOutEvent=false;
        m_handleEventsNotified=false;
        m_connectionThread = boost::thread([this]() {ConnectionThread();});
    }

    void ConnectionHandler::Stop()
    {
        if (m_connectionThread.get_id() != boost::thread::id())
        {
            //set the interrupt state so that when we generate the spurious signal
            //the thread will be interrupted at the interruption_point.
            m_connectionThread.interrupt();
            Connections::Instance().GenerateSpuriousConnectOrOutSignal();
            m_connectionThread.join();
            m_connectionThread = boost::thread();
        }
    }

    void ConnectionHandler::OnPoolDistributionComplete()
    {
        m_poolDistributionComplete=true;

        lllog(1) << "We have received persistence data (either from DOPE or other node), "
                    "ok to let apps connect!" << std::endl;
        Connections::Instance().AllowConnect(-1);
        Connections::Instance().AllowConnect(0);
    }

    void ConnectionHandler::SendAll(const std::pair<boost::shared_ptr<const char[]>, size_t>& data)
    {
        for (auto& vt : m_sendQueues)
        {
            vt.second.push(data);
        }
        HandleSendQueues();
    }

    void ConnectionHandler::HandleSendQueues()
    {
        for (auto& ntQ : m_sendQueues) //ntQ = pair<nodeType, SendQueue>
        {
            while (!ntQ.second.empty())
            {
                const auto& msg=ntQ.second.front();

                if (m_communication.Send(0, ntQ.first, msg.first, msg.second, ConnectionMessageDataTypeId, true))
                {
                    ntQ.second.pop();
                }
                else
                {
                    break;
                }
            }
        }
    }

    void ConnectionHandler::ConnectionThread()
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
                    m_connectEvent=true;
                }
                if (connectionOut)
                {
                    m_connectionOutEvent=true;
                }

                if (m_handleEventsNotified==false)
                {
                    m_handleEventsNotified=true;
                    m_strand.post([this]{HandleEvents();});
                }
            }
        }
        catch (const boost::thread_interrupted&)
        {
            //do nothing, just exit
        }
    }

    void ConnectionHandler::HandleEvents()
    {
//        m_handleEventsNotified=false;
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

//        bool oldConnectEvent = m_connectEvent.exchange(false);
//        if (oldConnectEvent)
//        {
//            m_connectionOutEvent=true;
//            gotConnectEvent = true;
//        }

//        bool oldConnectOutEvent=m_connectionOutEvent.exchange(false);
//        if (oldConnectOutEvent)
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
    }

    ConnectResult ConnectionHandler::CanAddConnection(const std::string & connectionName, const pid_t pid, const long /*context*/)
    {
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
    }

    void ConnectionHandler::HandleConnect(const ConnectionPtr & connection)
    {
        lllout << "ConnectionHandler::HandleConnect: New connection from " << connection->NameWithCounter() << " id = " << connection->Id() << std::endl;

        SendAll(ConnectDataPtr(connection->Id(), connection->NameWithoutCounter(), connection->Counter()));

        m_processInfoHandler.ConnectionAdded(connection);
    }

    void ConnectionHandler::HandleDisconnect(const ConnectionPtr & connection)
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

        //remove pending registrations
        m_pendingRegistrationHandler.RemovePendingRegistrations(connection->Id());

        // Handle outstanding requests towards the disconnected app ...
        m_requestHandler.HandleDisconnect(connection);

        //Distribute the disconnection to dose_com if Connection resides on this node
        if (connection->Id().m_node==m_communication.Id())
        {
            if (std::string(connection->NameWithoutCounter()).find(";dose_main;") != std::string::npos)
            {
                return;
            }

            SendAll(DisconnectDataPtr(connection->Id()));
        }

        // Remove the connection from the processInfo structure
        m_processInfoHandler.ConnectionRemoved(connection);

        //TODO: fire an event and handle i main

        // Classes have been unregistered, inform waiting connections
        //int recLevel=0;
        //HandleWaitingConnections(connection->Id().m_id, recLevel);

        // Remove any remaining traces of the connection from the blocking
        // structure
        //m_blockingHandler.RemoveConnection(connection->Id().m_id);
    }


    void ConnectionHandler::HandleConnectionOutEvent(const ConnectionPtr & connection, std::vector<ConnectionPtr>& deadConnections)
    {
        int recLevel=0;

        HandleAppEventHelper(connection, recLevel);

        if (connection->IsDead())
        {
            lllout << "Connection is dead: " << connection->NameWithCounter() << ", disconnecting."<< std::endl;
            deadConnections.push_back(connection);
        }
    }

    void ConnectionHandler::HandleAppEventHelper(const ConnectionPtr & connection, int & recursionLevel)
    {
        lllout << "HandleAppEventHelper for connection " << connection->NameWithCounter() << ", id = " << connection->Id() << std::endl;

        //---- Handle queued requests ----
        m_responseHandler->DistributeResponses(connection);
        m_requestHandler->DistributeRequests(connection);

        //Send messages
        m_messageHandler->DistributeMessages(connection);

        //Handle pending registrations
        m_pendingRegistrationHandler.CheckForNewOrRemovedPendingRegistration(connection);

        //Check in queues, and notify waiting applications
        HandleWaitingConnections(connection->Id().m_id, recursionLevel);
    }


#if 0 //stewart from dose_main_node_handler.cpp

    //================================================
    //================================================
    //================================================

    void NodeHandler::HandleNodeStatusChanges()
    {
        dcom_ulong32 id;        //out range 0-63
        dcom_ulong32 ns = 0;        //out = NODESTATUS_UP/DOWN
        dcom_ulong32 addr;

        while (DoseCom_GetNodeChange(id, ns, addr))
        {
            switch (ns)
            {
            case NODESTATUS_DOWN:
                {
                    DeleteConnections(id);
                    m_poolHandler->RemoveStatesWaitingForNode(static_cast<Typesystem::Int32>(id));
                }
                break;
            default:
            }
        }


        if (!NodeStatuses::Instance().AnyNodeHasStatus(NodeStatus::Starting) &&
            !NodeStatuses::Instance().AnyNodeHasStatus(NodeStatus::Failed))
        {
            // Ok, it seems that all nodes are either Started or Expected (has never been started).
            // In this case we know that we have got the pools, and thus the ghosts, from all nodes so
            // we can clean-up the ghosts and only save the ones from the newest registration.

            EntityTypes::Instance().CleanGhosts();

            // Kick all connections so they will do an dispatch.
            Connections::Instance().ForEachConnectionPtr(boost::bind(&NodeHandler::KickConnection,this,_1));

        }
    }


    void NodeHandler::HandleDisconnect(const ConnectionPtr& connection, const int64_t /*TODO stewart: node*/)
    {
        if (!connection->IsLocal() && NodeStatuses::Instance().GetNodeStatus(node) == Dob::NodeStatus::Failed)
        {
            connection->SetNodeDown();
        }
        m_requestHandler->HandleDisconnect(connection);
    }

    void NodeHandler::DeleteConnections(const int64_t node)
    {
        Connections::Instance().RemoveConnectionFromNode(node, boost::bind(&NodeHandler::HandleDisconnect,this,_1,node));
    }

    void NodeHandler::KickConnection(const ConnectionPtr& connection)
    {
        connection->SignalIn();
    }

#endif
}
}
}
