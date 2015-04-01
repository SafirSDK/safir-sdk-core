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
#include "ConnectionHandler.h"
#include "dose_main_message_handler.h"
#include "NodeInfoHandler.h"
#include "PendingRegistrationHandler.h"
#include "PoolHandler.h"
#include "dose_main_process_info_handler.h"
#include "dose_main_response_handler.h"
#include "dose_main_request_handler.h"
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


        void Start(const std::string& nodeName,
                   int64_t nodeId,
                   int64_t nodeTypeId,
                   const std::string& dataAddress);

        void InjectNode(const std::string& nodeName,
                        int64_t nodeId,
                        int64_t nodeTypeId,
                        const std::string& dataAddress);

        void Stop();

    private:
        void LogStatus(const std::string& str);

        void HandleSignal(const boost::system::error_code& error,
                          const int signalNumber);

        //Handler for dispatching own connection
        void DispatchOwnConnection();

        //Handler for all other events in dose_main
        void HandleEvents();
        Safir::Utilities::Internal::AtomicUint32 m_connectEvent;
        Safir::Utilities::Internal::AtomicUint32 m_connectionOutEvent;

        void ConnectionThread();

        void OnDoDispatch();

        //implementation of Connections::ConnectionHandler
        virtual ConnectResult CanAddConnection(const std::string & connectionName, const pid_t pid, const long context);
        virtual void HandleConnect(const ConnectionPtr & connection);

        void HandleDisconnect(const ConnectionPtr & connection);

        //TODO void AllocateStatic();

        void HandleConnectionOutEvent(const ConnectionPtr & connection, std::vector<ConnectionPtr>& deadConnections);

        void HandleIncomingData(const DistributionData & data, const bool isAckedData);

        void HandleAppEventHelper(const ConnectionPtr & connecction, int & recursionLevel);

        void HandleWaitingConnections(const Identifier blockingApp,
                                      int & recursionLevel);

        void WaitingConnectionsHelper(const Identifier blockingApp,
                                      IdentifierSet & waiting,
                                      int & recursionLevel);


        static void MemoryMonitorThread();

        boost::asio::io_service::strand& m_strand;
        boost::asio::io_service::strand m_wcoutStrand;
        boost::shared_ptr<boost::asio::io_service::work> m_work;

        int64_t m_nodeId{0};
        std::unique_ptr<Distribution> m_distribution;

        Control::DoseMainCmdReceiver m_cmdReceiver;

        boost::asio::signal_set m_signalSet;

        TimerHandler m_timerHandler;

        BlockingHandlers    m_blockingHandler;

        std::unique_ptr<MessageHandler>      m_messageHandler;
        std::unique_ptr<RequestHandler>      m_requestHandler;
        std::unique_ptr<ResponseHandler>     m_responseHandler;
        std::unique_ptr<PoolHandler>         m_poolHandler;
        std::unique_ptr<ConnectionHandler>   m_connectionHandler;

        //Pending Registrations
        std::unique_ptr<PendingRegistrationHandler> m_pendingRegistrationHandler;

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
        std::unique_ptr<LockMonitor> m_lockMonitor;

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
