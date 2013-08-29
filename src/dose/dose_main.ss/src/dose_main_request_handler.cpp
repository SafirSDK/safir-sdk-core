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

#include "dose_main_request_handler.h"

#include "dose_main_blocking_handler.h"
#include "dose_main_communication.h"
#include "dose_main_request_timers.h"
#include "dose_main_response_handler.h"
#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Dob/Internal/ServiceTypes.h>
#include <Safir/Dob/Internal/EntityTypes.h>
#include <Safir/Dob/Internal/ContextSharedTable.h>
#include <Safir/Dob/Service.h>
#include <Safir/Dob/Entity.h>
#include <Safir/Dob/InstanceIdPolicy.h>
#include <Safir/Dob/RequestTimeoutOverrideProperty.h>
#include <Safir/Dob/RequestTimeoutProperty.h>
#include <Safir/Dob/ResponseGeneralErrorCodes.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include <Safir/Dob/Typesystem/ObjectFactory.h>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Dob/NotFoundException.h>
#include <Safir/Dob/SuccessResponse.h>
#include <boost/bind.hpp>
#include <Safir/Utilities/Internal/SystemLog.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    static const Dob::Typesystem::Si32::Second deletedConnReqTimeout = 0.5;

    RequestHandler::RequestHandler()
    {
        RequestTimers::m_localReqTimerId =
            TimerHandler::Instance().RegisterTimeoutHandler
            (L"Safir::Dob::Internal::RequestHandler::localReqTimeout",
             *this);

        RequestTimers::m_externalReqTimerId =
            TimerHandler::Instance().RegisterTimeoutHandler
            (L"Safir::Dob::Internal::RequestHandler::externalReqTimeout",
             *this);
    }

    RequestHandler::~RequestHandler()
    {

    }

    void RequestHandler::Init(BlockingHandlers & blockingHandler,
                              ExternNodeCommunication  & ecom,
                              ResponseHandler & responseHandler)
    {
        m_blockingHandler = &blockingHandler;
        m_ecom = &ecom;
        m_responseHandler = &responseHandler;
    }

    const ConnectionConsumerPair GetRegistrationOwner(const DistributionData& request)
    {
        const Typesystem::TypeId typeId = request.GetTypeId();

        // If it is a ContextShared type we serach for the owner in context 0, otherwise
        // we search in the same context as the sender 
        ContextId context;
        if (ContextSharedTable::Instance().IsContextShared(typeId))
        {
            context = 0;
        }
        else
        {
            context = request.GetSenderId().m_contextId;
        }

        if (Typesystem::Operations::IsOfType(typeId,Service::ClassTypeId))
        {
            // Service
            return ServiceTypes::Instance().GetRegisterer(typeId, request.GetHandlerId(), context);
        }
        else
        {
            // Entity
            return EntityTypes::Instance().GetRegisterer(typeId, request.GetHandlerId(), context);
        }

    }

    void RequestHandler::DispatchRequest(DistributionData request,
                                         bool & handled,
                                         const ConnectionPtr & sender,
                                         ConnectionIdSet & skipList)
    {
        const Dob::Typesystem::TypeId typeId = request.GetTypeId();

        lllout << "DispatchRequest of type " << Typesystem::Operations::GetName(typeId)
            <<", requestId = " << request.GetRequestId() << std::endl;
        handled = false;

        //this is filled in inside the switch statement to be used later.
        ConnectionConsumerPair receiver;

        const DistributionData::Type requestType = request.GetType();

        // Find out which context to use.
        ContextId context =
            ContextSharedTable::Instance().IsContextShared(typeId) ? 0 : request.GetSenderId().m_contextId;

        switch (requestType)
        {
        case DistributionData::Request_Service:
            {
                // Service request
                receiver = ServiceTypes::Instance().GetRegisterer(typeId, request.GetHandlerId(), context);

                if (receiver.connection == NULL)
                {
                    // No registered handler, generate an automatic response.
                    std::wostringstream ostr;
                    ostr << "The handler " << request.GetHandlerId() << " is not registered for the type "
                         << Typesystem::Operations::GetName(request.GetTypeId()) << "!";

                    Dob::ErrorResponsePtr errorResponse = Dob::ErrorResponse::CreateErrorResponse
                        (Dob::ResponseGeneralErrorCodes::SafirNotRegistered(),
                         ostr.str());

                    SendAutoResponse(errorResponse, request.GetRequestId(), sender);

                    handled = true;
                    return;
                }
            }
            break;

        case DistributionData::Request_EntityUpdate:
        case DistributionData::Request_EntityDelete: //BEWARE of the fallthrough below!!! THERE IS NO break INTENTIONALLY
            {
                try
                {
                    //set the handler correctly so that we have all the needed info when we fall through
                    //to the next case statement.
                    const Typesystem::HandlerId handler = EntityTypes::Instance().GetHandlerOfInstance(request.GetEntityId(), context);
                    request.SetHandlerId(handler);
                }
                catch (const NotFoundException&)
                {
                    // No such instance, generate an automatic response.

                    std::wostringstream ostr;
                    ostr << "The instance " << request.GetEntityId() << " does not exist!";
                    Dob::ErrorResponsePtr errorResponse = Dob::ErrorResponse::CreateErrorResponse
                        (Dob::ResponseGeneralErrorCodes::SafirNotRegistered(),
                         ostr.str());

                    SendAutoResponse(errorResponse, request.GetRequestId(), sender);

                    handled = true;
                    return;
                }
            }//BEWARE of the fallthrough here!!! THERE IS NO break INTENTIONALLY

        case DistributionData::Request_EntityCreate: //NOTE the fallthrough above!
            {
                InstanceIdPolicy::Enumeration instanceIdPolicy;

                EntityTypes::Instance().GetRegisterer(request.GetTypeId(), request.GetHandlerId(), context, receiver, instanceIdPolicy);

                if (receiver.connection == NULL)
                {
                    // No registered handler, generate an automatic response.
                    std::wostringstream ostr;
                    ostr << "The handler " << request.GetHandlerId() << " is not registered for the type "
                         << Typesystem::Operations::GetName(request.GetTypeId()) << "!";

                    Dob::ErrorResponsePtr errorResponse = Dob::ErrorResponse::CreateErrorResponse
                        (Dob::ResponseGeneralErrorCodes::SafirNotRegistered(),
                         ostr.str());

                    SendAutoResponse(errorResponse, request.GetRequestId(), sender);

                    handled = true;
                    return;
                }

                if (request.GetType() == DistributionData::Request_EntityCreate)
                {
                    // It's an entity create request, check that the request is valid for
                    // the instance id policy applied by the handler.
                    switch (instanceIdPolicy)
                    {
                    case Dob::InstanceIdPolicy::HandlerDecidesInstanceId:
                        {
                            if (request.HasInstanceId())
                            {
                                std::wostringstream ostr;
                                ostr << "The handler " << request.GetHandlerId() << " for type "
                                     << Typesystem::Operations::GetName(request.GetTypeId())
                                     << " doesn't expect instance id to be specified by the requestor.";

                                Dob::ErrorResponsePtr errorResponse = Dob::ErrorResponse::CreateErrorResponse
                                    (Dob::ResponseGeneralErrorCodes::SafirReqErr(),
                                     ostr.str());

                                SendAutoResponse(errorResponse, request.GetRequestId(), sender);
                                handled = true;
                                return;
                            }
                        }
                        break;

                    case Dob::InstanceIdPolicy::RequestorDecidesInstanceId:
                        {
                            if (!request.HasInstanceId())
                            {
                                std::wostringstream ostr;
                                ostr << "The handler " << request.GetHandlerId() << " for type "
                                     << Typesystem::Operations::GetName(request.GetTypeId())
                                     << " expects instance id to be specified by the requestor.";

                                Dob::ErrorResponsePtr errorResponse = Dob::ErrorResponse::CreateErrorResponse
                                    (Dob::ResponseGeneralErrorCodes::SafirReqErr(),
                                     ostr.str());

                                SendAutoResponse(errorResponse, request.GetRequestId(), sender);
                                handled = true;
                                return;
                            }
                        }
                        break;

                    default:
                        ENSURE(false, << "Unknown instance id policy!");
                    }
                }
            }
            break;

        default:
            ENSURE(false,<< "Got unexpected DistributionData type in DispatchRequest " << requestType);
        };

        //if it is in the skip list we skip it!
        if (skipList.find(receiver.connection->Id()) != skipList.end())
        {
            return;
        }

        if (HandleRequest(request, receiver))
        {
            handled = true;
            StartTimer(sender, request);
        }
        else
        {
            skipList.insert(receiver.connection->Id());
            StartTimer(sender, request);
        }
    }

    void RequestHandler::DistributeRequests(const ConnectionPtr & connection)
    {
        lllout << "Distributing requests (out) for connection " << connection->Id() << "." << std::endl;
        ConnectionIdSet skipList;
        connection->GetRequestOutQueue().DispatchRequests
            (boost::bind(&RequestHandler::DispatchRequest,
                         this, _1, _2,
                         boost::cref(connection),
                         boost::ref(skipList)));
    }


    void RequestHandler::StartTimer(const ConnectionPtr & sender, const DistributionData & request)
    {
        // Start timer
        RequestTimerInfo timeoutInfo = RequestTimerInfo(sender->Id().m_id,
                                                        request.GetRequestId(),
                                                        request.GetTypeId(),
                                                        request.GetHandlerId());

        const Dob::Typesystem::Si64::Second timeoutTime =
            GetUtcTime() + GetTimeout(request.GetTypeId());

        TimerHandler::Instance().Set(Discard, //discard the timer if it is already set
                                     TimerInfoPtr(new ReqTimer(
                                                         RequestTimers::m_localReqTimerId,
                                                         timeoutInfo)),
                                                         timeoutTime);
    }


    bool RequestHandler::HandleRequest(const DistributionData & request,
                                       const ConnectionConsumerPair& receiver)
    {
        const ConnectionPtr sender = Connections::Instance().GetConnection(request.GetSenderId(), std::nothrow);

        const bool senderOnThisNode = (sender != NULL && sender->IsLocal());
        const bool receiverOnThisNode = receiver.connection->IsLocal();

        if (senderOnThisNode && receiverOnThisNode)
        {
            return HandleRequest_LocalSender_LocalReceiver(request, sender, receiver);
        }
        else if (senderOnThisNode && !receiverOnThisNode)
        {
            return HandleRequest_LocalSender_ExternReceiver(request, sender);
        }
        else if (!senderOnThisNode && receiverOnThisNode)
        {
            return HandleRequest_ExternSender_LocalReceiver(request, receiver);
        }
        else
        {
            SEND_SYSTEM_LOG(Error,
                            << "RequestHandler::HandleRequest: Got a request that was neither sent to or from this node!");
            // "This does not really cause any problems, but it is unexpected,
            return true; //Always OK, request not for us.
        }
    }

    bool RequestHandler::HandleRequest_LocalSender_LocalReceiver(const DistributionData & request,
                                                                 const ConnectionPtr& sender,
                                                                 const ConnectionConsumerPair& receiver)
    {
        if (!PostRequest(receiver, request))
        {
            m_blockingHandler->Request().AddWaitingConnection(receiver.connection->Id().m_id,
                                                              sender->Id().m_id);
            return false;
        }
        return true;
    }

    bool RequestHandler::HandleRequest_LocalSender_ExternReceiver(const DistributionData & request,
                                                      const ConnectionPtr & sender)
    {
        if (!m_ecom->Send(request))
        {
            m_blockingHandler->Request().AddWaitingConnection(ExternNodeCommunication::DoseComVirtualConnectionId,
                                                              sender->Id().m_id);

            return false;
        }
        return true;
    }


    bool RequestHandler::HandleRequest_ExternSender_LocalReceiver(const DistributionData & request,
                                                                  const ConnectionConsumerPair & receiver)
    {
        if (ReceiverHasOtherPendingRequest(receiver.connection->Id().m_id, request.GetRequestId()))
        {
            lllout << "ReceiverHasOtherPendingRequest, so we'll add it to pending!" << std::endl;
            // To guarantee the request order, requests from external nodes can't
            // be posted to the receiver's requestInQ if there are other requests
            // already waiting. In this case the request has to be stored in
            // the pending queue.
            AddPendingRequest(receiver.connection->Id().m_id, request);
            return false;
        }

        if (PostRequest(receiver, request))
        {
            return true;
        }
        else
        {
            // Failed to deliver to receiver
            // Save the request in pending request queue (if not already saved)
            AddPendingRequest(receiver.connection->Id().m_id, request);

            return false;
        }
    }

    void RequestHandler::HandleTimeout(const TimerInfoPtr& timer)
    {
        if (timer->GetTimerId() == RequestTimers::m_localReqTimerId)
        {
            // A request in the sender node has expired, generate a
            // response.

            const RequestTimerInfo& timerInfo = boost::static_pointer_cast<ReqTimer>(timer)->UserData();

            const ConnectionPtr sender = Connections::Instance().GetConnection
                (ConnectionId(ThisNodeParameters::NodeNumber(),
                              -1,
                              timerInfo.m_connectionIdentifier),
                 std::nothrow);

            if (sender == NULL)
            {
                // It seems that the sender is no longer there.
                return;
            }

            std::wostringstream ostr;
            ostr << "The handler " << timerInfo.m_handlerId << " did not respond to the request of type "
                << Typesystem::Operations::GetName(timerInfo.m_typeId) << "!";

            //create timeout response
            Dob::ErrorResponsePtr errorResponse =
                Dob::ErrorResponse::CreateErrorResponse
                (Dob::ResponseGeneralErrorCodes::SafirTimeout(),
                 ostr.str());

            //TODO: write directly to shared memory
            Typesystem::BinarySerialization bin;
            Typesystem::Serialization::ToBinary(errorResponse,bin);

            //convert response to Shared Message
            DistributionData response(response_tag,
                                      sender->Id(),
                                      sender->Id(),
                                      timerInfo.m_requestId,
                                      &bin[0]);

            //set the request handled and increase the timeouts count.
            sender->GetRequestOutQueue().RequestTimeout(timerInfo.m_requestId);

            //Post the response
            m_responseHandler->HandleResponse(response);

            sender->SignalIn();
        }
        else if (timer->GetTimerId() == RequestTimers::m_externalReqTimerId)
        {
            // A request in the receiver's node (external node) has expired. We just remove
            // the request since the corresponding timout in the sender node will take care
            // of the response generation.
            RequestTimerInfo timerInfo = boost::static_pointer_cast<ReqTimer>(timer)->UserData();

            RemovePendingRequest(timerInfo.m_connectionIdentifier, timerInfo.m_requestId);
        }
    }

    void PushRequest(const DistributionData & request, RequestInQueue& queue, bool & success)
    {
        success = queue.PushRequest(request);
        lllout << "  Pushing request (id = " << request.GetRequestId() << ". success = " << success << std::endl;
    }

    bool RequestHandler::PostRequest(const ConnectionConsumerPair & receiver,
                                     const DistributionData & request)
    {
        bool success;
        receiver.connection->ForSpecificRequestInQueue
            (receiver.consumer,
            boost::bind(PushRequest, boost::cref(request),_2,boost::ref(success)));

        // Always kick the receiver. Theoretically a kick shouldn't be necessary when there
        // is an overflow, but since there have been reports of applications not dispatching
        // their in-queues we make this "extra" kick.
        receiver.connection->SignalIn();

        if (!success)
        {
            lllout << "Overflow in a RequestInQueue. " << receiver.connection->NameWithCounter()
                << " " << receiver.connection->Id() << std::endl;
            return false;
        }
        return true;
    }

    void RequestHandler::SendAutoResponse(const Dob::ResponsePtr& resp,
                                           const InternalRequestId&     reqId,
                                           const ConnectionPtr&         sender)
    {
        //TODO: write directly to shared memory
        Typesystem::BinarySerialization bin;
        Typesystem::Serialization::ToBinary(resp,bin);

        //convert response to Shared Message
        DistributionData response(response_tag,
                                  sender->Id(),
                                  sender->Id(),
                                  reqId,
                                  &bin[0]);

        //Post the response
        m_responseHandler->HandleResponse(response);

        // Remove timeout
        RequestTimerInfo timeoutInfo = RequestTimerInfo(sender->Id().m_id, reqId);
        TimerHandler::Instance().Remove(TimerInfoPtr(new ReqTimer(RequestTimers::m_localReqTimerId,
                                                                  timeoutInfo)));

        sender->SignalIn();
    }

    void RequestHandler::HandleRequestFromDoseCom(const DistributionData & request)
    {
        lllout << "Got request from dose_com: type = "
            << Safir::Dob::Typesystem::Operations::GetName(request.GetTypeId())
            << ", handler = " << request.GetHandlerId() << std::endl;
        // If the request can not be put in the receiver's req in queue
        // it will be added to the pending map.

        const ConnectionConsumerPair receiver = GetRegistrationOwner(request);
        if (receiver.connection == NULL)
        {
            return; //ignore the request and let the timeout on the other node take care of it.
        }

        HandleRequest(request, receiver);
    }

    void RequestHandler::HandlePendingExternalRequest(const Identifier blockingConnection)
    {
        lllout << "HandlePendingExternalRequest for app " << blockingConnection <<std::endl;
        PendingRequestTable::iterator it = m_pendingRequests.find(blockingConnection);

        if (it == m_pendingRequests.end())
        {
            return;
        }

        while (!it->second.empty())
        {
            const DistributionData & request = it->second.front();

            const ConnectionConsumerPair receiver = GetRegistrationOwner(request);
            if (receiver.connection == NULL)
            {
                // if no receiver ignore the request and let the timeout on the other node take care of it.
                it->second.pop_front();
            }
            else
            {
                if (HandleRequest(request,receiver))
                {
                    it->second.pop_front();
                }
                else
                {
                    break;
                }
            }
        }

        if (it->second.empty())
        {
            // We managed to send all pending requests.
            m_pendingRequests.erase(it);
        }
    }


    void SetShortTimeout(const DistributionData& request,
                         const ConnectionPtr& deletedConn,
                         const ConnectionPtr & fromConnection)
    {
        const ConnectionPtr regOwner = GetRegistrationOwner(request).connection;

        if (regOwner == NULL || regOwner->Id() == deletedConn->Id())
        {
            // Found a request towards an unregistered entity/service or an entity/service
            // that is owned by the deleted connection.

            const InternalRequestId reqId = request.GetRequestId();
            // Set the running timer for this request to expire in a short time from now giving
            // any response sent from the deleted connection a chance to be received.

            RequestTimerInfo timeoutInfo = RequestTimerInfo(fromConnection->Id().m_id, reqId, request.GetTypeId(),request.GetHandlerId());

            const Dob::Typesystem::Si64::Second timeoutTime = GetUtcTime() + deletedConnReqTimeout;

            TimerHandler::Instance().Set(Replace,
                                         TimerInfoPtr(new ReqTimer
                                                      (RequestTimers::m_localReqTimerId,
                                                       timeoutInfo)),
                                         timeoutTime);
        }
    }

    void RequestHandler::FinalizeOutstandingRequests(const ConnectionPtr & deletedConn,
                                                     const ConnectionPtr & fromConnection)
    {
        if (!fromConnection->IsLocal())
        {
            return;
        }

        //two different tasks were performed in the previous version of this method:
        //1. All requests that have not yet been dispatched (picked up by dose_main) should be answered with an Unregistered response
        //2. All requests that have been dispatched should have their timeout shortened, so that if the deleted connection (on a remote node)
        //    has not already sent a response (which we have not yet received) the request will timeout shortly.

        //task 1 is not neccessary, since when that request is dispatched the handler will not be registered any longer, and will be responded
        //to with an 'unregistered'!

        fromConnection->GetRequestOutQueue().ForEachDispatchedRequest
            (boost::bind(SetShortTimeout,_1,boost::cref(deletedConn), boost::cref(fromConnection)));
    }

    void RequestHandler::HandleDisconnect(const ConnectionPtr & connection)
    {
        Connections::Instance().ForEachConnectionPtr(boost::bind(&RequestHandler::FinalizeOutstandingRequests,this,connection,_1));
    }

    void RequestHandler::AddPendingRequest(const Identifier blockingConn, const DistributionData & request)
    {
        lllout << "AddPendingRequest for app " << blockingConn <<std::endl;

        PendingRequestTable::iterator it = m_pendingRequests.find(blockingConn);
        if(it != m_pendingRequests.end())
        {
            InternalRequestId reqId = request.GetRequestId();
            bool reqFound = false;
            PendingRequests::iterator requestIt = it->second.begin();
            while (requestIt != it->second.end())
            {
                if (requestIt->GetRequestId() == reqId)
                {
                    reqFound = true;
                }
                ++requestIt;
            }

            if (!reqFound)
            {
                it->second.push_back(request);
            }
        }
        else
        {
            // No previous request for blockingConn
            PendingRequests requests;
            requests.push_back(request);
            m_pendingRequests.insert(std::make_pair(blockingConn, requests));
        }

        // Set timeout timer for request
        RequestTimerInfo timeoutInfo = RequestTimerInfo(blockingConn,
                                                        request.GetRequestId(),
                                                        request.GetTypeId(),
                                                        request.GetHandlerId());

        const Dob::Typesystem::Si64::Second timeoutTime = GetUtcTime()
            + GetTimeout(request.GetTypeId());

        TimerHandler::Instance().Set(Discard,   //discard if already set
                                     TimerInfoPtr(new ReqTimer(RequestTimers::m_externalReqTimerId,
                                                               timeoutInfo)),
                                     timeoutTime);

        // Set that dose_main is waiting for the receiver
        m_blockingHandler->Request().AddWaitingConnection(blockingConn,
                                                          ExternNodeCommunication::DoseComVirtualConnectionId);
    }


    bool IsRequestIdEqual(const DistributionData& request, const InternalRequestId& requestId)
    {
        return request.GetRequestId() == requestId;
    }

    void RequestHandler::RemovePendingRequest(const Identifier blockingConn, const InternalRequestId& requestId)
    {
        PendingRequestTable::iterator it = m_pendingRequests.find(blockingConn);
        if(it != m_pendingRequests.end())
        {
            PendingRequests::iterator newEnd = std::remove_if(it->second.begin(),
                                                              it->second.end(),
                                                              boost::bind(IsRequestIdEqual,_1,boost::cref(requestId)));
            it->second.erase(newEnd,it->second.end());

            if (it->second.empty())
            {
                m_pendingRequests.erase(it);
            }
        }
    }

    bool RequestHandler::ReceiverHasOtherPendingRequest(const Identifier receiver,
                                                        const InternalRequestId& requestId) const
    {
        PendingRequestTable::const_iterator findIt = m_pendingRequests.find(receiver);
        if(findIt != m_pendingRequests.end())
        {
            //if this request is first in the queue it means that we're not waiting for another request
            //to be sent before sending this one.
            if (!findIt->second.empty() && findIt->second.front().GetRequestId() == requestId)
            {
                return false;
            }
            return true;
        }
        else
        {
            return false;
        }
    }

    Dob::Typesystem::Si64::Second
    RequestHandler::GetTimeout(const Safir::Dob::Typesystem::TypeId typeId) const
    {
        TimeoutTable::iterator findIt = m_timeoutTable.find(typeId);
        if (findIt != m_timeoutTable.end())
        {
            return findIt->second;
        }
        //nope, it wasnt in the table, we need to get the value and insert it.

        Dob::Typesystem::Si64::Second timeout = 10.0;
        try
        {
            bool hasProperty, isInherited;
            Typesystem::Operations::HasProperty
                (typeId, RequestTimeoutOverrideProperty::ClassTypeId, hasProperty, isInherited);
            if ( hasProperty && !isInherited)
            { //has override property
                Typesystem::ObjectPtr obj =
                    Typesystem::ObjectFactory::Instance().CreateObject(typeId);

                timeout = RequestTimeoutOverrideProperty::GetTimeout(obj);
            }
            else if (Typesystem::Operations::HasProperty
                (typeId, RequestTimeoutProperty::ClassTypeId))
            { //normal timeout property
                Typesystem::ObjectPtr obj =
                    Typesystem::ObjectFactory::Instance().CreateObject(typeId);

                timeout = RequestTimeoutProperty::GetTimeout(obj);
            }
            else
            {
                ENSURE(false, << L"Failed to get request timeout time for object "
                       << Safir::Dob::Typesystem::Operations::GetName(typeId));
            }
        }
        catch (const Safir::Dob::Typesystem::NullException &)
        {
            ENSURE(false, << L"Failed to get request timeout time for object "
                          << Safir::Dob::Typesystem::Operations::GetName(typeId));
        }
        m_timeoutTable.insert(std::make_pair(typeId,timeout));
        return timeout;
    }

}
}
}
