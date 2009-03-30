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

#include "dose_main_connection_handler.h"
#include "dose_main_node_handler.h"

#include "dose_main_communication.h"
#include "dose_main_process_info_handler.h"
#include "dose_main_pending_registration_handler.h"
#include "dose_main_pool_handler.h"
#include "dose_main_request_handler.h"
#include "dose_main_persist_handler.h"
#include "dose_main_end_states_handler.h"
#include <Safir/Dob/ThisNodeParameters.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>


namespace Safir
{
namespace Dob
{
namespace Internal
{

    ConnectionHandler::ConnectionHandler():
        m_ecom(NULL),
        m_processInfoHandler(NULL),
        m_requestHandler(NULL),
        m_pendingRegistrationHandler(NULL),
        m_nodeHandler(NULL),
        m_persistHandler(NULL),
        m_endStates(NULL),
        m_connectSemHasBeenSignalled(false)
    {

    }

    ConnectionHandler::~ConnectionHandler()
    {

    }


    void ConnectionHandler::Init(ExternNodeCommunication & ecom,
                                 ProcessInfoHandler & processInfoHandler,
                                 RequestHandler & requestHandler,
                                 PendingRegistrationHandler & prh,
                                 NodeHandler & nh,
                                 PersistHandler & persistHandler,
                                 EndStatesHandler& endStates)
    {
        m_ecom = &ecom;
        m_processInfoHandler = &processInfoHandler;
        m_requestHandler = &requestHandler;
        m_pendingRegistrationHandler = &prh;
        m_nodeHandler = &nh;
        m_persistHandler = &persistHandler;
        m_endStates = &endStates;
    }

    void ConnectionHandler::HandleConnect(const ConnectionPtr & connection)
    {
        DistributionData connMsg(connect_message_tag,
                                 connection->Id(),
                                 connection->NameWithoutCounter(),
                                 connection->Counter());

        lllout << "Sending a new connection to dose_com: " << connection->NameWithCounter() << std::endl;
        if (!m_ecom->Send(connMsg))
        {
            lllout << "Nope, overflow in dose_com, adding to m_unsent" << std::endl;
            m_unsent.push_back(connMsg);
        }
    }

    void ConnectionHandler::HandleDisconnect(const ConnectionPtr & connection)
    {
        //remove pending registrations
        m_pendingRegistrationHandler->RemovePendingRegistrations(connection->Id());

        // Handle outstanding requests towards the disconnected app ...
        m_requestHandler->HandleDisconnect(connection);

        //Distribute the disconnection to dose_com if Connection resides on this node
        if (connection->Id().m_node == Dob::ThisNodeParameters::NodeNumber())
        {
            if (std::string(connection->NameWithoutCounter()).find(";dose_main;") != std::string::npos)
            {
                return;
            }

            DistributionData msg(disconnect_message_tag, connection->Id());
            if (!m_ecom->Send(msg))
            {
                m_unsent.push_back(msg);
            }
        }
    }


    void ConnectionHandler::HandleUnsent()
    {
        while (!m_unsent.empty())
        {
            if (m_ecom->Send(m_unsent.front()))
            {
                m_unsent.pop_front();
            }
            else
            {
                break;
            }
        }
    }

    //-----------------------
    //From dose_com
    //-----------------------
    void ConnectionHandler::HandleConnectFromDoseCom(const DistributionData & connectMsg)
    {
        const std::string name = connectMsg.GetConnectionName();
        const ConnectionId id = connectMsg.GetSenderId();
        lllout << "Got a Connect message from DoseCom: '" << name.c_str() << "' " << id << std::endl;


        //we don't know the context of remote connections, so we give a strange context number...
        Connections::Instance().AddConnection(name,connectMsg.GetCounter(), -2, id);

        lllout << "New connection from dose_com added successfully" << std::endl;
    }

    void ConnectionHandler::HandleDisconnectFromDoseCom(const DistributionData & disconnectMsg)
    {
        const ConnectionId id = disconnectMsg.GetSenderId();

        //put the connection id in endstates so we can discard any states that
        //the pool handler get "out of order".
        m_endStates->AddDisconnect(id);

        const ConnectionPtr connection = Connections::Instance().GetConnection(id, std::nothrow);
        if (connection == NULL)
        {
            lllerr << "Got a Disconnect from dosecom for a connection that I dont have! id = " << id << std::endl;
            return;
        }

        // Handle outstanding requests towards the disconnected app ...
        m_requestHandler->HandleDisconnect(connection);

        // ... before removing the Connection object
        Connections::Instance().RemoveConnection(connection);
    }



    void ConnectionHandler::MaybeSignalConnectSemaphore()
    {
        lllout << "MaybeSignalConnectSemaphore: " << std::endl;

        if (m_connectSemHasBeenSignalled)
        {
            lllout << "  Connect Semaphore has already been signalled" << std::endl;
            return;
        }

        const NodeHandler::NodeStatuses & nodeStatuses = m_nodeHandler->GetNodeStatuses();

        if (std::find(nodeStatuses.begin(), nodeStatuses.end(), NodeStatus::Starting) != nodeStatuses.end())
        {
            lllout << "  Can't let apps connect, Node "
                         << static_cast<int>(std::distance(m_nodeHandler->GetNodeStatuses().begin(),
                                                           std::find(nodeStatuses.begin(), nodeStatuses.end(), NodeStatus::Starting)))
                         << " (and maybe others) are still Starting" << std::endl;
            return;
        }
        lllout << "  SystemHasPersistence = " << m_persistHandler->SystemHasPersistence() << std::endl;

        if (m_persistHandler->SystemHasPersistence())
        {
            if (m_persistHandler->IsPersistentDataReady())
            {
                lllout << "  We have received persistence data (either from DOPE or other node), ok to let apps connect!" << std::endl;
                Connections::Instance().AllowConnect(0);
                m_connectSemHasBeenSignalled = true;
            }
            else
            {
                lllout << "  No persistence data, so can't let apps connect" << std::endl;
            }
        }
        else
        {
            lllout << "  There are no new nodes, so we can let apps connect!"  << std::endl;
            Connections::Instance().AllowConnect(0);
            m_connectSemHasBeenSignalled = true;
        }
    }

}
}
}
