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

#include "ConnectionHandler.h"
#include "Distribution.h"
#include "MessageHandler.h"
#include "NodeInfoHandler.h"
#include "MemoryMonitor.h"
#include "PendingRegistrationHandler.h"
#include "RequestHandler.h"
#include "BlockingHandler.h"
#include "ConnectionKiller.h"
#include "LockMonitor.h"
#include <Safir/Dob/Internal/DoseMainCmd.h>

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
    class DoseApp : private boost::noncopyable
    {
    public:
        explicit DoseApp(boost::asio::io_service& ioService);

        ~DoseApp();

        void Stop();

    private:
        void Start(const std::string& nodeName,
                   int64_t nodeId,
                   int64_t nodeTypeId,
                   const std::string& dataAddress);

        void InjectNode(const std::string& nodeName,
                        int64_t nodeId,
                        int64_t nodeTypeId,
                        const std::string& dataAddress);

        void ExcludeNode(int64_t nodeId, int64_t nodeTypeId);

        void LogStatus(const std::string& str);

        void HandleSignal(const boost::system::error_code& error,
                          const int signalNumber);

        void HandleIncomingData(const DistributionData & data, const bool isAckedData);

        void OnAppEvent(const ConnectionPtr & connection, bool disconnecting);
        void HandleAppEventHelper(const ConnectionPtr & connection);

        void HandleEvents();

        std::atomic<bool> m_stopped{false};
        boost::asio::io_service& m_ioService;

        boost::asio::io_service::strand m_strand;
        boost::asio::io_service::strand m_wcoutStrand;
        boost::shared_ptr<boost::asio::io_service::work> m_work;

        int64_t m_nodeId{0};
        std::unique_ptr<Distribution> m_distribution;

        Control::DoseMainCmdReceiver m_cmdReceiver;

        boost::asio::signal_set m_signalSet;

        BlockingHandlers    m_blockingHandler;

        std::unique_ptr<MessageHandler>      m_messageHandler;
        std::unique_ptr<RequestHandler>      m_requestHandler;
        std::unique_ptr<ConnectionHandler>   m_connectionHandler;
        std::unique_ptr<NodeInfoHandler>     m_nodeInfoHandler;

        //Pending Registrations
        std::unique_ptr<PendingRegistrationHandler> m_pendingRegistrationHandler;

        // For monitoring abandoned shared memory locks
        std::unique_ptr<LockMonitor> m_lockMonitor;

        // For monitoring memory usage
        std::unique_ptr<MemoryMonitor> m_memoryMonitor;

        //this class should be declared last, so that when the app
        //is destroyed all connections will be marked as dead and stop
        //orders sent before any more destruction is done.
        ConnectionKiller m_connectionKiller;
    };

}
}
}
