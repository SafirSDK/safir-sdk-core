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

#ifndef _dose_main_app_h
#define _dose_main_app_h

#include "dose_main_blocking_handler.h"
#include "dose_main_communication.h"
#include "dose_main_connection_handler.h"
#include "dose_main_message_handler.h"
#include "dose_main_node_handler.h"
#include "dose_main_pending_registration_handler.h"
#include "dose_main_persist_handler.h"
#include "dose_main_pool_handler.h"
#include "dose_main_process_info_handler.h"
#include "dose_main_response_handler.h"
#include "dose_main_request_handler.h"
#include "dose_main_end_states_handler.h"
#include "dose_main_thread_monitor.h"
#include "dose_main_lock_monitor.h"
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Utilities/ProcessMonitor.h>
#include <ace/Event_Handler.h>



namespace Safir
{
namespace Dob
{
namespace Internal
{
    class DoseApp:
        public ACE_Event_Handler,
        public Connections::ConnectionConsumer,
        public Safir::Dob::Dispatcher,
        public TimeoutHandler,
        private boost::noncopyable
    {
    public:
        DoseApp();

        ~DoseApp();

        /**
         * Start the main loop of dose_main
         */
        int Run();

        /** Handle atexit call.
        *
        * Called on atexit from the runtime. Indicates that dose_main is exiting.
        * Will send stop orders to all connections.
        */
        //       void HandleAtExit();

    private:
        //Handler for dispatching own connection
        virtual int handle_input(ACE_HANDLE);

        //Handler for all other events in dose_main
        virtual int handle_exception(ACE_HANDLE);
        AtomicUint32 m_connectEvent;
        AtomicUint32 m_connectionOutEvent;
        AtomicUint32 m_nodeStatusChangedEvent;

        //Timeout handler
        virtual void HandleTimeout(const TimerInfoPtr& timer);

        static ACE_THR_FUNC_RETURN ConnectionThread(void *);

        void OnDoDispatch();

        //implementation of Connections::ConnectionHandler
        virtual ConnectResult CanAddConnection(const std::string & connectionName, const pid_t pid, const long context);
        virtual void HandleConnect(const ConnectionPtr & connection);
        
        void HandleDisconnect(const ConnectionPtr & connection);

        bool AllocateStatic();

        void HandleConnectionOutEvent(const ConnectionPtr & connection, std::vector<ConnectionPtr>& deadConnections);

        void NodeStatusChangedNotifier();
        void QueueNotFull();
        void StartPoolDistribution();
        void RequestPoolDistribution(const int nodeId);

        void HandleIncomingData(const DistributionData & data, const bool isAckedData);

        void HandleAppEventHelper(const ConnectionPtr & connecction, int & recursionLevel);

        void HandleWaitingConnections(const Identifier blockingApp,
                                      int & recursionLevel);

        void WaitingConnectionsHelper(const Identifier blockingApp,
                                      IdentifierSet & waiting,
                                      int & recursionLevel);


        static ACE_THR_FUNC_RETURN MemoryMonitorThread(void *);

        EndStatesHandler m_endStates;

        // Shared memory queue message handlers
        ConnectionHandler   m_connectionHandler;

        BlockingHandlers    m_blockingHandler;

        MessageHandler      m_messageHandler;

        RequestHandler      m_requestHandler;
        ResponseHandler     m_responseHandler;

        PoolHandler         m_poolHandler;

        //Pending Registrations
        PendingRegistrationHandler m_pendingRegistrationHandler;


        //Persistent data service and state
        PersistHandler m_persistHandler;

        //Extern node communication
        ExternNodeCommunication m_ecom;

        // Process info
        ProcessInfoHandler m_processInfoHandler;

        NodeHandler m_nodeHandler;

        Safir::Dob::Connection m_ownConnection;

        // For monitoring processes
        Safir::Utilities::ProcessMonitor m_processMonitor;

        // For monitoring abandoned shared memory locks
        // The lock monitor is an active object so we keep it in the form
        // of a pointer. This enables us to execute the constructor (which wiil
        // start the built in thread) any time we want.
        boost::shared_ptr<LockMonitor> m_lockMonitorPtr;

        // For monitoring dose_main:s own threads
        // Se comment above why a pointer is used.
        boost::shared_ptr<ThreadMonitor> m_threadMonitorPtr;
        boost::thread::id m_mainThreadId;

        AtomicUint32 m_handle_exception_notified;
        AtomicUint32 m_handle_input_notified;

    };

}
}
}

#endif

