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

#ifndef _dose_main_request_handler_h
#define _dose_main_request_handler_h

#include "dose_main_timers.h"
#include <Safir/Dob/ErrorResponse.h>
#include <deque>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    //forward declarations
    class BlockingHandlers;
    class ExternNodeCommunication;
    class ResponseHandler;

    class RequestHandler:
        public TimeoutHandler
    {
    public:
        RequestHandler();
        virtual ~RequestHandler();

        void Init(BlockingHandlers & blockingHandler,
                  ExternNodeCommunication & ecom,
                  ResponseHandler & responseHandler);


        void DistributeRequests(const ConnectionPtr & connection);

        void HandleRequestFromDoseCom(const DistributionData & request);

        void HandleDisconnect(const ConnectionPtr & connection);

        //when a request is blocking on an overflow to dose_com, this gets called on not overflow.
        void HandlePendingExternalRequest(const Identifier blockingConnection);

    private:

        void DispatchRequest(DistributionData request,
                             bool & handled,
                             const ConnectionPtr & sender,
                             ConnectionIdSet & skipList);

        //Returns true if the request was successfully posted to someone else, and false otherwise
        bool HandleRequest(const DistributionData & request,
                           const ConnectionConsumerPair& receiver);


        //called from HandleDisconnect
        void FinalizeOutstandingRequests(const ConnectionPtr & toConnection, const ConnectionPtr & fromConnection);

        bool HandleRequest_LocalSender_LocalReceiver(const DistributionData & request,
                                                     const ConnectionPtr& sender,
                                                     const ConnectionConsumerPair& receiver);

        bool HandleRequest_LocalSender_ExternReceiver(const DistributionData & request,
                                                      const ConnectionPtr & sender);

        bool HandleRequest_ExternSender_LocalReceiver(const DistributionData & request,
                                                      const ConnectionConsumerPair & receiver);

        void StartTimer(const ConnectionPtr & sender,
                        const DistributionData & request);

        virtual void HandleTimeout(const TimerInfoPtr& timer);

        bool PostRequest(const ConnectionConsumerPair& receiver,
                         const DistributionData& request); //is not const-ref since it needs to set the responseId

        void SendAutoResponse(const Dob::ResponsePtr&      resp,
                              const InternalRequestId&     reqId,
                              const ConnectionPtr&         sender);

        void AddPendingRequest(const Identifier blockingConn, const DistributionData & request);
        void RemovePendingRequest(const Identifier blockingConn, const InternalRequestId& requestId);
        bool ReceiverHasOtherPendingRequest(const Identifier receiver, const InternalRequestId& requestId) const;



        typedef std::deque<DistributionData> PendingRequests;
        typedef unordered_map<Identifier, PendingRequests> PendingRequestTable;

        PendingRequestTable m_pendingRequests;

        Dob::Typesystem::Si64::Second GetTimeout(const Safir::Dob::Typesystem::TypeId typeId) const;
        typedef unordered_map<Typesystem::TypeId, Typesystem::Si64::Second> TimeoutTable;
        mutable TimeoutTable m_timeoutTable;

        ExternNodeCommunication* m_ecom;
        BlockingHandlers* m_blockingHandler;
        ResponseHandler * m_responseHandler;
    };
}
}
}
#endif

