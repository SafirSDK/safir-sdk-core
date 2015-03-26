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

#include "ConnectionHandler.h"
#include "PendingRegistrationHandler.h"
#include "dose_main_request_handler.h"
#include <Safir/Dob/Internal/Communication.h>
#include <Safir/Dob/NodeParameters.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Dob/Internal/NodeStatuses.h>
#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Utilities/Internal/SystemLog.h>


namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace
{
    boost::shared_ptr<const char[]> ToPtr(const DistributionData& d)
    {
        boost::shared_ptr<const char[]> p(d.GetReference(), [=](const char* ptr){DistributionData::DropReference(ptr);});
        return p;
    }
}

    ConnectionHandler::ConnectionHandler(boost::asio::io_service& ioService,
                                         Com::Communication& communication,
                                         RequestHandler& requesthandler,
                                         PendingRegistrationHandler& prh)
        : m_strand(ioService),
          m_communication(communication),
          m_requestHandler(requesthandler),
          m_pendingRegistrationHandler(prh)
    {
    }

    void ConnectionHandler::OnPoolDistributionComplete()
    {
        m_poolDistributionComplete=true;

        lllog(1) << "We have received persistence data (either from DOPE or other node), "
                    "ok to let apps connect!" << std::endl;
        Connections::Instance().AllowConnect(-1);
        Connections::Instance().AllowConnect(0);
    }

    void ConnectionHandler::HandleConnect(const ConnectionPtr& connection)
    {
        DistributionData connMsg(connect_message_tag,
                                 connection->Id(),
                                 connection->NameWithoutCounter(),
                                 connection->Counter());

        
        if (!HandleUnsent())
        {
            // There are unsent connect/disconnect messages, can't continue.
            m_unsent.push_back(connMsg);
            return;
        }

//--- Stewart
//        if (m_ecom->Send(connMsg))
//        {
//            lllout << "Sent a new connection to dose_com: " << connection->NameWithCounter() << std::endl;
//        }
//        else
//        {
//            lllout << "Overflow when sending new connection to dose_com, adding to m_unsent: "
//                   << connection->NameWithCounter() << std::endl;
//            m_unsent.push_back(connMsg);
//        }

    }

    void ConnectionHandler::HandleDisconnect(const ConnectionPtr& connection)
    {
        //remove pending registrations
        m_pendingRegistrationHandler.RemovePendingRegistrations(connection->Id());

        // Handle outstanding requests towards the disconnected app ...
        m_requestHandler.HandleDisconnect(connection);

#if 0 //stewart
        //Distribute the disconnection to dose_com if Connection resides on this node
        if (connection->Id().m_node == Dob::ThisNodeParameters::NodeNumber())
        {
            if (std::string(connection->NameWithoutCounter()).find(";dose_main;") != std::string::npos)
            {
                return;
            }

            DistributionData msg(disconnect_message_tag, connection->Id());

            if (!HandleUnsent())
            {
                // There are unsent connect/disconnect messages, can't continue.
                m_unsent.push_back(msg);
                return;
            }


            if (!m_ecom->Send(msg))
            {
                m_unsent.push_back(msg);
            }

        }
#endif
    }


    bool ConnectionHandler::HandleUnsent()
    {
#if 0 //stewart
        while (!m_unsent.empty())
        {
            if (m_ecom->Send(m_unsent.front()))
            {
                m_unsent.pop_front();
            }
            else
            {
                return false;
            }
        }
#endif
        return true;
    }

#if 0 //stewart
    //-----------------------
    //From dose_com
    //-----------------------
    void ConnectionHandler::HandleConnectFromDoseCom(const DistributionData & connectMsg)
    {
        const std::string name = connectMsg.GetConnectionName();
        const ConnectionId id = connectMsg.GetSenderId();

        ENSURE(id.m_contextId >= 0 && id.m_contextId < NodeParameters::NumberOfContexts(),
               << "Received a connect with context " << id.m_contextId << " but this node is configured for "
               << NodeParameters::NumberOfContexts() << " contexts.");

        lllout << "Got a Connect message from DoseCom: '" << name.c_str() << "' " << id << std::endl;

        //we don't know the context of remote connections, so we give a strange context number...
        Connections::Instance().AddConnection(name,connectMsg.GetCounter(), id.m_contextId, id);

        lllout << "New connection from dose_com added successfully" << std::endl;
    }

    void ConnectionHandler::HandleDisconnectFromDoseCom(const DistributionData & disconnectMsg)
    {
        const ConnectionId id = disconnectMsg.GetSenderId();

        const ConnectionPtr connection = Connections::Instance().GetConnection(id, std::nothrow);
        if (connection == NULL)
        {
            SEND_SYSTEM_LOG(Error,
                            << "Got a Disconnect from dosecom for a connection that I dont have! id = " << id);
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

        const NodeStatuses::Status nodeStatuses = NodeStatuses::Instance().GetNodeStatuses();

        if (std::find(nodeStatuses.begin(), nodeStatuses.end(), NodeStatus::Starting) != nodeStatuses.end())
        {
            lllout << "  Can't let apps connect, Node "
                         << static_cast<int>(std::distance(nodeStatuses.begin(),
                                                           std::find(nodeStatuses.begin(), nodeStatuses.end(), NodeStatus::Starting)))
                         << " (and maybe others) are still Starting" << std::endl;
            return;
        }

        if (m_persistHandler->IsPersistentDataReady())
        {
            lllout << "  We have received persistence data (either from DOPE or other node), ok to let apps connect!" << std::endl;
            Connections::Instance().AllowConnect(-1);
            Connections::Instance().AllowConnect(0);
            m_connectSemHasBeenSignalled = true;
        }
        else
        {
            lllout << "  No persistence data, so can't let apps connect" << std::endl;
        }

    }
#endif

}
}
}
