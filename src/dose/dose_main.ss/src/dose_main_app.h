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
#pragma once

#include <memory>

#include "Distribution.h"
#include "dose_main_blocking_handler.h"
#include "dose_main_communication.h"
#include "dose_main_connection_handler.h"
#include "dose_main_message_handler.h"
#include "dose_main_node_handler.h"
#include "dose_main_pending_registration_handler.h"
#include "dose_main_persist_handler.h"
#include "PoolHandler.h"
#include "dose_main_process_info_handler.h"
#include "dose_main_response_handler.h"
#include "dose_main_request_handler.h"
#include "dose_main_end_states_handler.h"
#include "dose_main_lock_monitor.h"
#include "dose_main_connection_killer.h"
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Dob/Internal/DoseMainCmd.h>
#include <Safir/Utilities/ProcessMonitor.h>

//disable warnings in boost
#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable : 4244 4267)
#endif

#include <boost/thread.hpp>
#include <boost/asio.hpp>

#if defined _MSC_VER
  #pragma warning (pop)
#endif

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class DoseApp:
        public Connections::ConnectionConsumer,
        public Safir::Dob::Dispatcher,
        private boost::noncopyable
    {
    public:
        explicit DoseApp(boost::asio::io_service::strand& strand);

        ~DoseApp();

        void Init();

    private:

        void Start(const std::string& nodeName,
                   int64_t nodeId,
                   int64_t nodeTypeId,
                   const std::string& dataAddress);

        void InjectNode(const std::string& nodeName,
                        int64_t nodeId,
                        int64_t nodeTypeId,
                        const std::string& dataAddress);

        //This is called when dose_main is expected to shut down
        void Stop();

        void HandleSignal(const boost::system::error_code& error,
                          const int signalNumber);

        //Handler for dispatching own connection
        void DispatchOwnConnection();

        //Handler for all other events in dose_main
        void HandleEvents();
        Safir::Utilities::Internal::AtomicUint32 m_connectEvent;
        Safir::Utilities::Internal::AtomicUint32 m_connectionOutEvent;
        Safir::Utilities::Internal::AtomicUint32 m_nodeStatusChangedEvent;

        void ConnectionThread();

        void OnDoDispatch();

        //implementation of Connections::ConnectionHandler
        virtual ConnectResult CanAddConnection(const std::string & connectionName, const pid_t pid, const long context);
        virtual void HandleConnect(const ConnectionPtr & connection);

        void HandleDisconnect(const ConnectionPtr & connection);

        void AllocateStatic();

        void HandleConnectionOutEvent(const ConnectionPtr & connection, std::vector<ConnectionPtr>& deadConnections);

        void NodeStatusChangedNotifier();

        void QueueNotFull();

        void HandleIncomingData(const DistributionData & data, const bool isAckedData);

        void HandleAppEventHelper(const ConnectionPtr & connecction, int & recursionLevel);

        void HandleWaitingConnections(const Identifier blockingApp,
                                      int & recursionLevel);

        void WaitingConnectionsHelper(const Identifier blockingApp,
                                      IdentifierSet & waiting,
                                      int & recursionLevel);


        static void MemoryMonitorThread();

        boost::asio::io_service::strand& m_strand;
        boost::shared_ptr<boost::asio::io_service::work> m_work;

        std::unique_ptr<Distribution> m_distribution;

        Control::DoseMainCmdReceiver m_cmdReceiver;

        boost::asio::signal_set m_signalSet;

        TimerHandler m_timerHandler;

        EndStatesHandler m_endStates;

        BlockingHandlers    m_blockingHandler;

        std::unique_ptr<MessageHandler> m_messageHandler;

        // Shared memory queue message handlers
        ConnectionHandler   m_connectionHandler;       

        std::unique_ptr<RequestHandler>      m_requestHandler;
        std::unique_ptr<ResponseHandler>     m_responseHandler;
        std::unique_ptr<PoolHandler>         m_poolHandler;

        //Pending Registrations
        PendingRegistrationHandler m_pendingRegistrationHandler;


        //Persistent data service and state
        PersistHandler m_persistHandler;

#if 0 //stewart
        //Extern node communication
        ExternNodeCommunication m_ecom;
#endif

        // Process info
        ProcessInfoHandler m_processInfoHandler;

        NodeHandler m_nodeHandler;

        Safir::Dob::Connection m_ownConnection;

        // For monitoring processes
        Safir::Utilities::ProcessMonitor m_processMonitor;

        // For monitoring abandoned shared memory locks
        LockMonitor m_lockMonitor;

        Safir::Utilities::Internal::AtomicUint32 m_HandleEvents_notified;
        Safir::Utilities::Internal::AtomicUint32 m_DispatchOwnConnection_notified;

        boost::thread m_connectionThread;
        boost::thread m_memoryMonitorThread;

        //this class should be declared last, so that when the app
        //is destroyed all connections will be marked as dead and stop
        //orders sent before any more destruction is done.
        ConnectionKiller m_connectionKiller;
    };

}
}
}
