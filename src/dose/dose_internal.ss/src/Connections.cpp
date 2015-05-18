/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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
#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/NodeParameters.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/ProcessInfo.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <boost/lexical_cast.hpp>
#include "Signals.h"

#include <vector>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    using boost::move; //we need to get hold of the boost move, in case we're
    //using boost >= 1.49. This way our code is backwards compatible.

    Connections* Connections::m_instance = NULL;

    Connections& Connections::Instance()
    {
        ENSURE(m_instance != NULL, << "Connections::Instance was called before Initialize!!!");
        return *m_instance;
    }

    Connections::Connections(private_constructor_t, const int64_t nodeId):
        m_nodeId(nodeId),
        m_maxNumConnections(Safir::Dob::NodeParameters::MaxNumberOfConnections()),
        m_connectionOutIds(m_maxNumConnections), //default constructed (-1,-1)
        m_lastUsedSlot(0),
        m_connectSignal(0),
        m_connectSem(0),
        m_connectMinusOneSem(0),
        m_connectResponseEvent(0),
        m_connectionCounter(0),
        m_connectMinusOneSemSignalled(false),
        m_connectSemSignalled(false)
    {
        ENSURE(nodeId != 0, << "Connections must be constructed with valid nodeId");

        m_connectionOutSignals =
            static_cast<Safir::Utilities::Internal::AtomicUint32*>
            (GetSharedMemory().allocate(sizeof(Safir::Utilities::Internal::AtomicUint32)* m_maxNumConnections));

        for (int i = 0; i < m_maxNumConnections; ++i)
        {
            m_connectionOutSignals.get()[i] = 0;
        }
    }

    void Connections::Initialize(const bool iAmDoseMain, const int64_t nodeId)
    {
        m_instance = GetSharedMemory().find_or_construct<Connections>("Connections")(private_constructor_t(), nodeId);

        if (iAmDoseMain)
        {
            Signals::RemoveConnectOrOut();
        }
    }


    Connections::~Connections()
    {

    }

    void Connections::WaitForDoseMainSignal(bool & connect, bool & connectionOut)
    {
        connect = false;
        connectionOut = false;
        Signals::Instance().WaitForConnectOrOut();
        //get the events
        const boost::uint32_t oldconnectSignal = m_connectSignal.compare_exchange(0, 1);

        if (oldconnectSignal != 0)
        {
            connect = true;
        }

        //we could loop through the connection signals, but currently we just assume that we might as well
        //tell dose_main to do it for us even if no signals have been set....
        connectionOut = true;
        //std::wcout << "WaitForDoseMainSignal: connect = " << std::boolalpha << connect << ", connectionOut = " << connectionOut << std::endl;
    }

    const boost::function<void(void)> Connections::GetConnectionSignalWaiter(const ConnectionId & connectionId)
    {
        return Signals::Instance().GetConnectionSignalWaiter(connectionId);
    }



    void Connections::Connect(const std::string & connectionName,
                              const ContextId contextId,
                              const bool isMinusOneConnection,
                              ConnectResult & result,
                              ConnectionPtr & connection)
    {
        if (connectionName.length() > MAX_CONNECTION_NAME_LENGTH)
        {
            std::wostringstream ostr;
            ostr << "Connection name is too long '" <<
                connectionName.c_str() << "'";
            throw Safir::Dob::Typesystem::IllegalValueException(ostr.str(),__WFILE__,__LINE__);
        }

        //is this a connect from within dose_main's main thread? if so, bypass normal connection handling
        if (connectionName.find(";dose_main;") != connectionName.npos)
        {
            ConnectDoseMain(connectionName,contextId,result,connection);
            return;
        }

        //guard against connections before dose_main has said that it is ok to start connecting.
        if (isMinusOneConnection)
        {
            lllout << "Waiting on m_connectMinusOneSem" << std::endl;
            m_connectMinusOneSem.wait();
            m_connectMinusOneSem.post();
        }
        else
        {
            lllout << "Waiting on m_connectSem" << std::endl;
            m_connectSem.wait();
            m_connectSem.post();
        }

        //this means that only one process can attempt to connect at a time.
        ScopedConnectLock lck(m_connectLock);

        const pid_t pid = Safir::Utilities::ProcessInfo::GetPid();

        m_connectMessage.Set(connect_tag, connectionName, contextId, pid);

        m_connectSignal = 1;
        Signals::Instance().SignalConnectOrOut();
        //wait for response
        m_connectResponseEvent.wait();

        m_connectResponse.GetAndClear(connect_tag, result, connection);

        ENSURE(result == Success ||
               (result != Success && connection == NULL), <<"failed to insert connection!!");

        ENSURE(result != Undefined, << "Connect response was not set correctly!!");
    }

    void Connections::ConnectDoseMain(const std::string & connectionName,
                                      const ContextId contextId,
                                      ConnectResult & result,
                                      ConnectionPtr & connection)
    {
        lllout << "Handling a Connect for one of dose_mains own connections. name = " << connectionName.c_str() << std::endl;

        connection.reset();
        result = Success;
        boost::interprocess::scoped_lock<ConnectionsTableLock> lck(m_connectionTablesLock);

        bool foundName = false;
        for (ConnectionTable::const_iterator it = m_connections.begin();
             it != m_connections.end(); ++it)
        {
            if (it->second->NameWithoutCounter() == connectionName)
            {
                foundName = true;
                break;
            }
        }

        if (foundName)
        {
            lllout << "Connection already exists. name = " << connectionName.c_str() << std::endl;

            result = ConnectionNameAlreadyExists;
            return;
        }
        else
        {
            const pid_t pid = Safir::Utilities::ProcessInfo::GetPid();

            connection = ConnectionPtr(GetSharedMemory().construct<Connection>
                                       (boost::interprocess::anonymous_instance)
                                       (connectionName, m_connectionCounter++, m_nodeId, contextId, pid, true));

            const bool success = m_connections.insert
                (std::make_pair(connection->Id(), connection)).second;

            ENSURE(success, << "ConnectDoseMain: Failed to insert connection in map! name = " << connectionName.c_str());

            AddToSignalHandling(connection);

            //We don't call the connect handler, since we assume that dose_main is able to
            //do the bookkeeping for its own connection by itself.
        }
    }

    void Connections::Disconnect(const ConnectionPtr & connection)
    {
        ENSURE(connection != NULL, << "Cannot disconnect a NULL connection");
        connection->Died();
    }

    void Connections::HandleConnect(const std::function<void(const ConnectionPtr& connection)>& handleConnect)
    {
       if (m_connectMessage.IsConnect())
        {
           lllout << "Handling a Connect" << std::endl;

            std::string connectionName;
            ContextId context;
            pid_t pid;

            m_connectMessage.GetAndClear(connect_tag, connectionName, context, pid);

            boost::interprocess::scoped_lock<ConnectionsTableLock> lck(m_connectionTablesLock);

            bool foundName = false;
            for (ConnectionTable::const_iterator it = m_connections.begin();
                 it != m_connections.end(); ++it)
            {
                if (it->second->NameWithoutCounter() == connectionName)
                {
                    foundName = true;
                    break;
                }
            }

            if (foundName)
            {
                lllout << "Connection with name '" << connectionName.c_str() << "' already exists, returning an error code" << std::endl;
                m_connectResponse.Set(connect_tag,ConnectionNameAlreadyExists,ConnectionPtr(/*NULL*/));
            }
            else
            {
                ConnectionPtr connection;
                {
                    connection = ConnectionPtr(GetSharedMemory().construct<Connection>
                                               (boost::interprocess::anonymous_instance)
                                               (connectionName, m_connectionCounter++, m_nodeId, context, pid, true));

                    const bool success = m_connections.insert(std::make_pair(connection->Id(), connection)).second;

                    ENSURE(success, << "HandleConnect: Failed to insert connection in map! name = " << connectionName.c_str());

                    AddToSignalHandling(connection);
                }

                lck.unlock();

                //call the connect handler
                handleConnect(connection);

                m_connectResponse.Set(connect_tag, Success, connection);
            }

        }
        //lllout << "Signalling the connector that the response is available" << std::endl;
        m_connectResponseEvent.post();
    }

    void Connections::AddConnection(const std::string & connectionName,
                                    const Typesystem::Int32 counter,
                                    const long context,
                                    const ConnectionId & id)
    {
        ENSURE(id.m_node != m_nodeId, << "AddConnection is meant for remote connections only");
        lllout << "Adding a connection (from other node?) name = '" << connectionName.c_str()
               << "', context = " << context
               << ", id = " << id << std::endl;

        boost::interprocess::scoped_lock<ConnectionsTableLock> lck(m_connectionTablesLock);

        ConnectionTable::const_iterator findIt = m_connections.find(id);

        if (findIt != m_connections.end())
        {
            // The connection already exists on this node.

            lllout << "Connection already exists on this node!!!! name = " << connectionName.c_str()
                << ", id = " << id << std::endl;

            return;
        }

        //remote connections have pid = -1
        ConnectionPtr connection(GetSharedMemory().construct<Connection>
                                 (boost::interprocess::anonymous_instance)
                                 (connectionName, counter, id.m_node, context, -1, false));

        const bool success = m_connections.insert(std::make_pair(connection->Id(), connection)).second;

        ENSURE(success, << "AddConnection: Failed to insert connection in map! name = " << connectionName.c_str());
    }


    void Connections::RemoveConnection(const ConnectionPtr & connection)
    {
        {
            boost::interprocess::scoped_lock<ConnectionsTableLock> lck(m_connectionTablesLock);

            if (connection->IsLocal())
            {
                RemoveFromSignalHandling(connection);
            }
            m_connections.erase(connection->Id());
        }
        connection->Cleanup();
    }


    void Connections::AllowConnect(const long context)
    {
        lllout << "Signalling to allow connections on context " << context  << std::endl;
        if (context == -1)
        {
            if (m_connectMinusOneSemSignalled)
            {
                lllout << "  already signalled, ignoring call"  << std::endl;
            }
            else
            {
                m_connectMinusOneSem.post();
                m_connectMinusOneSemSignalled = true;
            }
        }
        else
        {
            ENSURE(context == 0, << "Only contexts -1 and 0 are allowed");
            if (m_connectSemSignalled)
            {
                lllout << "  already signalled, ignoring call"  << std::endl;
            }
            else
            {
                m_connectSem.post();
                m_connectSemSignalled = true;
            }
        }
    }

    void Connections::AddToSignalHandling(const ConnectionPtr& connection)
    {
        //find a free slot
        Containers<ConnectionId>::vector::iterator findIt = std::find(m_connectionOutIds.begin(),
                                                                      m_connectionOutIds.end(),
                                                                      ConnectionId());
        ENSURE (findIt != m_connectionOutIds.end(), << "No free slot was found in the connection handler arrays!");

        *findIt = connection->Id();
        const size_t index = std::distance(m_connectionOutIds.begin(),findIt);
        m_connectionOutSignals[index] = 0;
        connection->SetOutSignal(&m_connectionOutSignals[index]);

        m_lastUsedSlot = std::max(m_lastUsedSlot,index);
    }


    void Connections::RemoveFromSignalHandling(const ConnectionPtr& connection)
    {
        Containers<ConnectionId>::vector::iterator findIt =
            std::find(m_connectionOutIds.begin(),
                      m_connectionOutIds.end(),
                      connection->Id());

        ENSURE(findIt != m_connectionOutIds.end(), << "Could not find connection " << connection->Id() << " to remove from signalling vector!");

        //set the signal to false and remove the connection from the vector
        const size_t index = std::distance(m_connectionOutIds.begin(),findIt);
        m_connectionOutSignals[index] = false;
        *findIt = ConnectionId();

        //if it was the last slot that we're no longer using we need to find
        //the last one that is in use now.
        if (index == m_lastUsedSlot)
        {
            size_t i = index;
            while(m_connectionOutIds[i] == ConnectionId() && i > 0)
            {
                --i;
            }
            m_lastUsedSlot = i;
        }
    }



    void Connections::HandleConnectionOutEvents(const boost::function<void(const ConnectionPtr&)>& handler)
    {
        std::vector<ConnectionPtr> signalledConnections;

        //Take the lock while we loop through the signal vectors, but release it before we
        //call the handler for the signalled connections.
        {
            boost::interprocess::scoped_lock<ConnectionsTableLock> lck(m_connectionTablesLock);
            const Safir::Utilities::Internal::AtomicUint32 * end = m_connectionOutSignals.get() + m_lastUsedSlot + 1;
            for (Safir::Utilities::Internal::AtomicUint32 * it = m_connectionOutSignals.get();
                 it != end; ++it)
            {
                if (*it != 0)
                {
                    *it = 0;

                    const ConnectionId& connId = m_connectionOutIds[std::distance(m_connectionOutSignals.get(), it)];

                    if (connId == ConnectionId()) //compare with dummy-connection
                    {
                        SEND_SYSTEM_LOG(Warning, << "A signal was set for a slot that has no valid connection id! Resetting it!");
                    }
                    else
                    {
                        const auto findIt = m_connections.find(connId);
                        ENSURE(findIt != m_connections.end(), << "Connections::GetConnection: Failed to find connection! connectionId = " << connId);
                        signalledConnections.push_back(findIt->second);
                    }
                }
            }
        }

        //The lock is released here, so at all points in the following loop where we're not holding the lock,

        for (const auto& conn: signalledConnections)
        {
            handler(conn);
        }
    }

    bool
    Connections::IsPendingAccepted(const Typesystem::TypeId typeId, const Typesystem::HandlerId & handlerId, const ContextId contextId) const
    {
        boost::interprocess::scoped_lock<ConnectionsTableLock> lck(m_connectionTablesLock);
        for (ConnectionTable::const_iterator it = m_connections.begin();
             it != m_connections.end(); ++it)
        {
            if (it->second->Id().m_contextId == contextId && it->second->IsPendingAccepted(typeId,handlerId))
            {
                return true;
            }
        }
        return false;
    }

    const ConnectionPtr
    Connections::GetConnection(const ConnectionId & connId) const
    {
        boost::interprocess::scoped_lock<ConnectionsTableLock> lck(m_connectionTablesLock);
        const auto findIt = m_connections.find(connId);
        ENSURE(findIt != m_connections.end(), << "Connections::GetConnection: Failed to find connection! connectionId = " << connId);
        return findIt->second;
    }

    const ConnectionPtr
    Connections::GetConnection(const ConnectionId & connId, std::nothrow_t) const
    {
        boost::interprocess::scoped_lock<ConnectionsTableLock> lck(m_connectionTablesLock);
        const auto findIt = m_connections.find(connId);
        if (findIt != m_connections.end())
        {
            return findIt->second;
        }
        else
        {
            return ConnectionPtr();
        }
    }

    const ConnectionPtr
    Connections::GetConnectionByName(const std::string & connectionName) const
    {
        boost::interprocess::scoped_lock<ConnectionsTableLock> lock(m_connectionTablesLock);
        for (ConnectionTable::const_iterator it = m_connections.begin();
            it != m_connections.end(); ++it)
        {
            if (it->second->NameWithCounter() == connectionName)
            {
                return it->second;
            }
        }
        ENSURE(false, << "Failed to find connection by name! connectionName = " << connectionName.c_str());
        return ConnectionPtr(); //Keep compiler happy
    }

    void
    Connections::GetConnections(const pid_t pid, std::vector<ConnectionPtr>& connections) const
    {
        boost::interprocess::scoped_lock<ConnectionsTableLock> lck(m_connectionTablesLock);

        connections.clear();

        for(ConnectionTable::const_iterator it = m_connections.begin();
            it != m_connections.end();
            ++it)
        {
            if ((*it).second->Pid() == pid)
            {
                connections.push_back((*it).second);
            }
        }
    }

    const std::string Connections::GetConnectionName(const ConnectionId & connectionId) const
    {
        boost::interprocess::scoped_lock<ConnectionsTableLock> lck(m_connectionTablesLock);
        ConnectionTable::const_iterator findIt = m_connections.find(connectionId);

        if (findIt != m_connections.end())
        {
            return findIt->second->NameWithCounter();
        }
        else
        {
            return std::string();
        }
    }

    void Connections::RemoveConnectionFromNode(const int64_t node, const boost::function<void(const ConnectionPtr & connection)> & connectionFunc)
    {
        std::vector<ConnectionPtr> removeConnections;

        // Remove from m_connections and store in removeConnections
        {
            boost::interprocess::scoped_lock<ConnectionsTableLock> lck(m_connectionTablesLock);

            ConnectionTable::const_iterator it = m_connections.begin();

            while(it != m_connections.end())
            {
                ConnectionTable::const_iterator removeIt = it;
                ++it;

                if(removeIt->first.m_node == node)
                {
                    removeConnections.push_back(removeIt->second);
                    if (removeIt->second->IsLocal())
                    {
                        RemoveFromSignalHandling(removeIt->second);
                    }
                    m_connections.erase(removeIt);
                }
            }
        }


        // Now loop over the connection to be removed and do the work
        for(std::vector<ConnectionPtr>::const_iterator it = removeConnections.begin();
            it != removeConnections.end();
            ++it)
        {
            ConnectionPtr removeConnection = (*it);

            connectionFunc(removeConnection);

            removeConnection->Cleanup();
        }
    }


    void Connections::ForEachConnection(const boost::function<void(const Connection & connection)> & connectionFunc) const
    {
        boost::interprocess::scoped_lock<ConnectionsTableLock> lck(m_connectionTablesLock);
        for (ConnectionTable::const_iterator it = m_connections.begin();
             it != m_connections.end(); ++it)
        {
            connectionFunc(*it->second);
        }
    }

    void Connections::ForEachConnectionPtr(const boost::function<void(const ConnectionPtr & connection)> & connectionFunc) const
    {
        boost::interprocess::scoped_lock<ConnectionsTableLock> lck(m_connectionTablesLock);
        for (ConnectionTable::const_iterator it = m_connections.begin();
             it != m_connections.end(); ++it)
        {
            connectionFunc(it->second);
        }
    }

    void Connections::ForSpecificConnection(const ConnectionId& connectionId,
                                            const boost::function<void(const ConnectionPtr & connection)> & connectionFunc) const
    {
        boost::interprocess::scoped_lock<ConnectionsTableLock> lck(m_connectionTablesLock);

        ConnectionTable::const_iterator it = m_connections.find(connectionId);

        if (it != m_connections.end())
        {
            connectionFunc(it->second);
        }
    }

    void Connections::GenerateSpuriousConnectOrOutSignal() const
    {
        Signals::Instance().SignalConnectOrOut();
    }
}
}
}
