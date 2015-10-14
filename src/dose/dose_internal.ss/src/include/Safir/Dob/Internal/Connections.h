/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safirsdkcore.com)
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

#ifndef __DOSE_CONNECTIONS_H__
#define __DOSE_CONNECTIONS_H__

#include <Safir/Dob/Internal/InternalDefs.h>
#include <Safir/Dob/Internal/InternalExportDefs.h>
#include <Safir/Dob/Internal/SharedMemoryObject.h>
#include <Safir/Dob/Internal/Connection.h>
#include <Safir/Dob/Internal/Semaphore.h>
#include <boost/interprocess/sync/interprocess_mutex.hpp>
#include <Safir/Dob/Internal/ConnectRequest.h>
#include <Safir/Dob/Internal/LeveledLock.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class DOSE_INTERNAL_API Connections:
        public SharedMemoryObject,
        private boost::noncopyable
    {
    private:
        //This is to make sure that only Instance can call the constructor even though the constructor
        //itself has to be public (limitation of boost::interprocess)
        struct private_constructor_t {};
    public:
        static void Initialize(const bool iAmDoseMain, const int64_t nodeId);

        /**
         * Get the singleton instance.
         * Note that this is not a singleton in the usual sense of the word,
         * it is the single instance of something that resides in shared memory.
         */
        static Connections & Instance();

        //The constructor and destructor have to be public for the boost::interprocess internals to be able to call
        //them, but we can make the constructor "fake-private" by making it require a private type as argument.
        Connections(private_constructor_t, const int64_t nodeId);
        ~Connections();

        /** Get the id of the current node */
        int64_t NodeId() const {return m_nodeId;}

        /**
         * This is for applications waiting for "things" to happen to its connection.
         * This blocks forever until SignalIn for that connection has been called.
         * This should ONLY be used for waiting on your own connection. All other use will yield
         * undefined results...
         * Is intended to be called from the dispatch thread in dose_dll.
         */
        const boost::function<void(void)> GetConnectionSignalWaiter(const ConnectionId & connectionId);

        /**
         * This is for DoseMain to wait for events from connections.
         * This method will block forever.
         *
         * Note that all booleans may be set to true at once!
         *
         * @param connect [out] - An app is trying to connect or disconnect. Call Connections::HandleConnect
         * @param connection [out] - An app has signalled that it has done something that needs attention ...
         *                            (It has called it's SignalOut() method)
         */
        void WaitForDoseMainSignal(bool & connect, bool & connectionOut);

        /** For applications to connect to the DOB.
         * If the connectionName contains the string ";dose_main;" (ie the connectionNameCommonPart is "dose_main")
         *        it means that the connection is being made from within dose_main itself
         *        (for dose_mains own connection, not for connections from other nodes).
         *         No callbacks to a connectionConsumer will be made for this.
         **/
        void Connect(const std::string & connectionName,
                     const ContextId contextId,
                     const bool isMinusOneConnection,
                     ConnectResult & result,
                     ConnectionPtr & connection);

        /** For applications to disconnect from the DOB. */
        void Disconnect(const ConnectionPtr & connection);

        /**
         * Handle connects and disconnects.
         * Note that handles either one connect or disconnect per call.
         * It is illegal to call this method when there is noone trying to connect
         * (I.e when the WaitForDoseMainSignal hasnt said that there is a connect waiting)
         *
         * The handleConnect function will be called when an application is trying to connect.
         */
        void HandleConnect(const std::function<void(const ConnectionPtr& connection)>& handleConnect);

        /**
         * This function is used to add connections from within dose_main.
         * Meant to be used for connections from other nodes.
         */
        void AddConnection(const std::string & connectionNameWithoutCouter,
                           const Typesystem::Int32 counter,
                           const long context,
                           const ConnectionId & id);


        /**
         * This function is used to remove connections from within dose_main.
         * Meant to be used for connections from other nodes or for connections
         * belonging to applications that terminate unexpectedly.
         */
        void RemoveConnection(const ConnectionPtr & connection);

        /**
         * Get a connection from a connection id.
         * It throws a SoftwareViolationException if connection is not found!
         */
        const ConnectionPtr GetConnection(const ConnectionId & connId) const;

        /**
         * Get a connection from a connection id.
         * Returns NULL if not found
         */
        const ConnectionPtr GetConnection(const ConnectionId & connId, std::nothrow_t) const;


        /**
         * Get a connection from a connection name.
         * Note that this is a fairly expensive operation, since the lock has to be held
         * while the table is searched for the right connection.
         * It throws a SoftwareViolationException if connection is not found!
         */
        const ConnectionPtr GetConnectionByName(const std::string & connectionName) const;

        /**
         * Get all connections from a pid.
         */
        void GetConnections(const pid_t pid, std::vector<ConnectionPtr>& connections) const;

        /**
         * Get the name of a connection.
         * May return empty string if the connection does not exist (it may have just been removed)
         * The connection list is locked while this is called (reader lock)
         */
        const std::string GetConnectionName(const ConnectionId & connectionId) const;

        /**
         * For dose_main to call when it wants to start allowing applications to connect
         *
         * @param context The context to allow connects on (currently only -1 and 0 are allowed)
         */
        void AllowConnect(const long context);

        void HandleConnectionOutEvents(const boost::function<void(const ConnectionPtr&)> & handler);

        //look through all connections and see if there is a pending reg that is accepted
        bool IsPendingAccepted(const Typesystem::TypeId typeId, const Typesystem::HandlerId & handlerId, const ContextId contextId) const;

        // Removes connection(s) from specified node.
        void RemoveConnectionFromNode(const int64_t node, const boost::function<void(const ConnectionPtr & connection)> & connectionFunc);

        //A reader lock on the connection vector will be taken during the looping and the callback!
        void ForEachConnection(const boost::function<void(const Connection & connection)> & connectionFunc) const;
        void ForEachConnectionPtr(const boost::function<void(const ConnectionPtr & connection)> & connectionFunc) const;

        // A reader lock will be taken during the callback.
        void ForSpecificConnection(const ConnectionId& connectionId,
                                   const boost::function<void(const ConnectionPtr & connection)> & connectionFunc) const;

        //send the ConnectOrOut signal without setting any of the event flags.
        //This is ONLY useful for dose_main when it wants to stop the thread that
        //listens to this event.
        void GenerateSpuriousConnectOrOutSignal() const;

    private:
        void AddToSignalHandling(const ConnectionPtr& connection);
        void RemoveFromSignalHandling(const ConnectionPtr& connection);


        void ConnectDoseMain(const std::string & connectionName,
                             const ContextId contextId,
                             ConnectResult & result,
                             ConnectionPtr & connection);

        const int64_t m_nodeId;
        const int m_maxNumConnections;

        typedef PairContainers<ConnectionId, ConnectionPtr>::map ConnectionTable;
        ConnectionTable m_connections;

        //these two vectors handle handle out-signals from the connections.
        //The positions in the vectors correspond to each other (i.e. the signal at position 20 in
        //m_connectionOutSignals represent the out signal of the app in position 20 in m_connectionOutIds).
        //They have a static size, and a connId of -1,-1 (default constructed ConnectionId) means that that "slot" is not occupied
        //THIS DATA STRUCTURE CAN NOT BE RESIZED!! EACH CONNECTION HOLDS A POINTER TO IT'S SIGNAL!
        //RESIZING/MOVING THE STRUCTURE WILL INVALIDATE THE POINTERS!

        boost::interprocess::offset_ptr<Safir::Utilities::Internal::AtomicUint32> m_connectionOutSignals;
        Containers<ConnectionId>::vector m_connectionOutIds;
        size_t m_lastUsedSlot; //slot that marks the end of the region of used slots. optimization


        //Signal for when an application is trying to connect
        Safir::Utilities::Internal::AtomicUint32 m_connectSignal;

        Semaphore m_connectSem;
        Semaphore m_connectMinusOneSem;

        typedef Safir::Dob::Internal::LeveledLock<boost::interprocess::interprocess_mutex,
                                                  CONNECT_LOCK_LEVEL,
                                                  NO_MASTER_LEVEL_REQUIRED> ConnectLock;
        mutable ConnectLock m_connectLock;
        typedef boost::interprocess::scoped_lock<ConnectLock> ScopedConnectLock;

        Semaphore m_connectResponseEvent;

        //lock for m_connections, m_connectionOutIds, and m_connectionOutSignals
        typedef Safir::Dob::Internal::LeveledLock<boost::interprocess::interprocess_mutex,
                                                  CONNECTIONS_TABLE_LOCK_LEVEL,
                                                  NO_MASTER_LEVEL_REQUIRED> ConnectionsTableLock;
        mutable ConnectionsTableLock m_connectionTablesLock;

        ConnectRequest m_connectMessage;
        ConnectResponse m_connectResponse;

        Typesystem::Int32 m_connectionCounter;

        bool m_connectMinusOneSemSignalled;
        bool m_connectSemSignalled;

        static Connections* m_instance;
    };
}
}
}
#endif
