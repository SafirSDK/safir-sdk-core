/******************************************************************************
*
* Copyright Saab AB, 2007-2013,2015 (http://safir.sourceforge.net)
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

#include <Safir/Dob/ErrorResponse.h>
#include <Safir/Dob/Internal/ConnectionConsumerPair.h>
#include <Safir/Dob/Internal/DistributionData.h>
#include <deque>
#include <unordered_map>
#include <boost/functional/hash.hpp>
#include <memory>
#include "BlockingHandler.h"
#include "ResponseHandler.h"

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#endif

#include <boost/asio.hpp>
#include <boost/asio/steady_timer.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

namespace Safir
{
namespace Dob
{
namespace Internal
{

    namespace Com
    {
        class Communication;
    }

    /**
     * This class handles distribution of requests within local and remote nodes.
     *
     * All methods are thread safe.
     */
    class RequestHandler
    {
    public:

        RequestHandler(boost::asio::io_service& ioService,
                       Distribution&            distribution);

        // Handle outgoing and incoming requests for this connection
        void HandleRequests(const ConnectionPtr& connection);

        void HandleDisconnect(const ConnectionPtr& deletedConnection);

        void Stop();

    private:

        void DispatchRequests(const ConnectionPtr& connection);

        void DispatchRequest(DistributionData request,
                             bool& handled,
                             const ConnectionPtr& sender,
                             ConnectionIdSet& skipList);

        //Returns true if the request was successfully posted to someone else, and false otherwise
        bool DistributeRequest(const DistributionData& request,
                               const ConnectionConsumerPair& receiver);

        //called from HandleDisconnect
        void FinalizeOutstandingRequests(const ConnectionPtr& toConnection, const ConnectionPtr& fromConnection);

        bool DistributeRequestLocalSenderLocalReceiver(const DistributionData& request,
                                                       const ConnectionPtr& sender,
                                                       const ConnectionConsumerPair& receiver);

        bool DistributeRequestLocalSenderExternReceiver(const DistributionData& request,
                                                        const ConnectionPtr& sender,
                                                        const ConnectionConsumerPair& receiver);

        bool DistributeRequestExternSenderLocalReceiver(const DistributionData& request,
                                                        const ConnectionConsumerPair& receiver);

        void StartOutReqTimer(const ConnectionPtr& sender,
                              const DistributionData& request,
                              boost::asio::steady_timer& timer);

        bool PostRequest(const ConnectionConsumerPair& receiver,
                         const DistributionData& request); //is not const-ref since it needs to set the responseId

        void SendAutoResponse(const Dob::ResponsePtr&      resp,
                              const InternalRequestId&     reqId,
                              const ConnectionPtr&         sender);

        void HandleMessageFromRemoteNode(int64_t        fromNodeId,
                                         int64_t        fromNodeType,
                                         const char*    data);

        void ReleaseAllBlocked(int64_t blockingConnection);

        void HandlePendingExternalRequest(int64_t blockingConnection);

        void SetShortTimeout(const DistributionData& request,
                             const ConnectionPtr& deletedConn,
                             const ConnectionPtr & fromConnection);

        void AddPendingRequest(const int64_t blockingConn, const DistributionData & request);
        void RemovePendingRequest(const int64_t blockingConn, const InternalRequestId& requestId);
        bool ReceiverHasOtherPendingRequest(const int64_t receiver, const InternalRequestId& requestId) const;

        typedef std::pair<DistributionData, std::unique_ptr<boost::asio::steady_timer>> PendingRequest;

        typedef std::deque<PendingRequest> PendingRequestsQueue;

        typedef boost::unordered_map<int64_t, PendingRequestsQueue> PendingRequestTable;

        boost::asio::io_service&            m_ioService;
        boost::asio::io_service::strand     m_strand;
        Distribution&                       m_distribution;
        Com::Communication&                 m_communication;
        const int64_t                       m_dataTypeIdentifier;
        const int64_t                       m_communicationVirtualConnectionId;
        std::map<int64_t,int64_t>           m_liveNodes;
        PendingRequestTable                 m_pendingRequests;

        std::unordered_map<std::pair<int64_t, InternalRequestId>,
                           std::unique_ptr<boost::asio::steady_timer>,
                           boost::hash<std::pair<int64_t, InternalRequestId>>> m_outReqTimers;

        boost::chrono::milliseconds GetTimeout(const Safir::Dob::Typesystem::TypeId typeId) const;
        typedef boost::unordered_map<Typesystem::TypeId, Typesystem::Si64::Second> TimeoutTable;
        mutable TimeoutTable m_timeoutTable;

        BlockingHandlers m_blockingHandler;
        std::unique_ptr<ResponseHandler> m_responseHandler;
    };
}
}
}

