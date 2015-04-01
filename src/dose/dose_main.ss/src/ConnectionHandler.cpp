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
#include "dose_main_request_handler.h"

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
          m_pendingRegistrationHandler(prh)
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
    }

    void ConnectionHandler::OnPoolDistributionComplete()
    {
        m_poolDistributionComplete=true;

        lllog(1) << "We have received persistence data (either from DOPE or other node), "
                    "ok to let apps connect!" << std::endl;
        Connections::Instance().AllowConnect(-1);
        Connections::Instance().AllowConnect(0);
    }

    void ConnectionHandler::HandleConnect(const ConnectionPtr& con)
    {
        SendAll(ConnectDataPtr(con->Id(), con->NameWithoutCounter(), con->Counter()));
    }

    void ConnectionHandler::HandleDisconnect(const ConnectionPtr& con)
    {
        //remove pending registrations
        m_pendingRegistrationHandler.RemovePendingRegistrations(con->Id());

        // Handle outstanding requests towards the disconnected app ...
        m_requestHandler.HandleDisconnect(con);

        //Distribute the disconnection to dose_com if Connection resides on this node
        if (con->Id().m_node==m_communication.Id())
        {
            if (std::string(con->NameWithoutCounter()).find(";dose_main;") != std::string::npos)
            {
                return;
            }

            SendAll(DisconnectDataPtr(con->Id()));
        }
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

#if 0 //stewart from dose_main_node_handler.cpp


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
