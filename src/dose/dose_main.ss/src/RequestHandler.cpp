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

#include "RequestHandler.h"

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
#include <Safir/Utilities/Internal/MakeUnique.h>
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

// anonymous namespace for internal non-member functions
namespace
{
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

}

    RequestHandler::RequestHandler(boost::asio::io_service& ioService,
                                   Distribution&            distribution)
        : m_ioService(ioService),
          m_strand(ioService),
          m_distribution(distribution),
          m_communication(distribution.GetCommunication()),
          m_dataTypeIdentifier(LlufId_Generate64("RequestHandler")),
          m_communicationVirtualConnectionId(LlufId_Generate64("CommunicationVirtualConnectionId"))
    {
        m_responseHandler.reset(new ResponseHandler(m_strand,
                            distribution,
                            [this]
                            (const ConnectionId& connectionId,
                             const InternalRequestId requestId)
                            {
                                m_outReqTimers.erase(std::make_pair(connectionId.m_id, requestId));
                            }));

        m_distribution.SubscribeNodeEvents(
                    // Executed when a 'node included' cb is received
                    m_strand.wrap([this](const std::string& /*nodeName*/,
                                         const int64_t nodeId,
                                         const int64_t nodeTypeId,
                                         const std::string& /*dataAddress*/)
                    {
                        if (nodeId != m_distribution.GetNodeId())
                        {
                            m_liveNodes.insert(std::make_pair(nodeId,
                                                              nodeTypeId));
                        }
                    }),
                    // Executed when a 'node excluded' cb is received
                    m_strand.wrap([this](const int64_t nodeId,
                                         const int64_t /*nodeTypeId*/)
                    {
                        m_liveNodes.erase(nodeId);
                    }));

        m_communication.SetQueueNotFullCallback(
                    // Executed when Communication indicates 'queue not full'
                    m_strand.wrap([this] (int64_t /*nodeTypeId*/)
                    {
                        // Retry all connections that are waiting for Communication
                        ReleaseAllBlocked(m_communicationVirtualConnectionId);
                    }),
                    0); // All node types

        m_communication.SetDataReceiver(
                    // Executed when a request message is received from external node
                    m_strand.wrap([this]
                                  (int64_t fromNodeId,
                                   int64_t fromNodeType,
                                   const char* data,
                                   size_t /*size*/)
                    {
                        HandleMessageFromRemoteNode(fromNodeId, fromNodeType, data);
                    }),
                    m_dataTypeIdentifier,
                    DistributionData::NewData,
                    DistributionData::DropReference);
    }

    void RequestHandler::HandleRequests(const ConnectionPtr& connection)
    {
        m_strand.dispatch([this, connection] ()
        {
            // First, try to distribute responses for this connection
            m_responseHandler->DistributeResponses(connection);

            // Second, try to distribute out requests for this connection
            lllog(8) << "DOSE_MAIN: Distributing requests (out) for connection "
                     << connection->Id() << "." << std::endl;

            DispatchRequests(connection);

            // Third, it could be that this connection now has empty slots on its in queue
            // Get any connection blocking on this connection and try to distribute its requests.
            ReleaseAllBlocked(connection->Id().m_id);
        });
    }

    void RequestHandler::HandleDisconnect(const ConnectionPtr& deletedConnection)
    {
        m_strand.dispatch([this, deletedConnection] ()
        {
            auto this_ = this; //fixes for vs 2010 issues with lambda
            auto& deletedConnection_ = deletedConnection;

            // All requests that have been dispatched should have their timeout shortened, so that if
            // the deleted connection (on a remote node) has not already sent a response
            // (which we have not yet received) the request will timeout shortly.
            Connections::Instance().ForEachConnectionPtr(
                        [this_, deletedConnection_]
                        (const ConnectionPtr& fromConnection)
                        {
                            if (!fromConnection->IsLocal())
                            {
                                return;
                            }

                            auto this__ = this_; //fixes for vs 2010 issues with lambda
                            auto& deletedConnection__ = deletedConnection_; 

                            fromConnection->GetRequestOutQueue().ForEachDispatchedRequest(
                                        [this__, deletedConnection__, fromConnection]
                                        (const DistributionData& request)
                                        {
                                            this__->SetShortTimeout(request, deletedConnection__, fromConnection);
                                        });
                        });

            m_blockingHandler.RemoveConnection(deletedConnection->Id().m_id);
        });
    }

    void RequestHandler::Stop()
    {
        m_strand.post([this]()
                      {
                          // Clear structures that hold timers
                          m_outReqTimers.clear();
                          m_pendingRequests.clear();
                      });
    }

    void RequestHandler::DispatchRequests(const ConnectionPtr& connection)
    {
        ConnectionIdSet skipList;
        connection->GetRequestOutQueue().DispatchRequests([this, connection, &skipList]
                                                          (const DistributionData& request,
                                                           bool& handled)
                                                          {
                                                              DispatchRequest(request,
                                                                              handled,
                                                                              connection,
                                                                              skipList);
                                                          });
    }

    void RequestHandler::DispatchRequest(DistributionData request,
                                         bool& handled,
                                         const ConnectionPtr& sender,
                                         ConnectionIdSet& skipList)
    {
        const Dob::Typesystem::TypeId typeId = request.GetTypeId();

        lllog(8) << "DOSE_MAIN: DispatchRequest of type " << Typesystem::Operations::GetName(typeId)
                 <<", requestId = " << request.GetRequestId() << std::endl;

        handled = false;

        //this is filled in inside the switch statement to be used later.
        ConnectionConsumerPair receiver;

        const auto requestType = request.GetType();

        // Find out which context to use.
        auto context =
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
            case DistributionData::Request_EntityDelete: //BEWARE of the fallthrough below!!!
                                                         //THERE IS NO break INTENTIONALLY
            {
                try
                {
                    //set the handler correctly so that we have all the needed info when we fall through
                    //to the next case statement.
                    const auto handler =
                            EntityTypes::Instance().GetHandlerOfInstance(request.GetEntityId(), context);
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

                EntityTypes::Instance().GetRegisterer(request.GetTypeId(),
                                                      request.GetHandlerId(),
                                                      context,
                                                      receiver,
                                                      instanceIdPolicy);

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
        }

        //if it is in the skip list we skip it!
        if (skipList.find(receiver.connection->Id()) != skipList.end())
        {
            return;
        }

        if (DistributeRequest(request, receiver))
        {
            handled = true;
        }
        else
        {
            skipList.insert(receiver.connection->Id());
        }

        auto timer = Safir::make_unique<boost::asio::steady_timer>(m_ioService);

        timer->expires_from_now(GetTimeout(request.GetTypeId()));

        StartOutReqTimer(sender, request, *timer);

        m_outReqTimers.insert(std::make_pair(std::make_pair(sender->Id().m_id, request.GetRequestId()),
                                             std::move(timer)));
    }

    void RequestHandler::StartOutReqTimer(const ConnectionPtr& sender,
                                          const DistributionData& request,
                                          boost::asio::steady_timer& timer)
    {
        auto senderId = sender->Id().m_id;
        auto reqId = request.GetRequestId();
        auto typeId = request.GetTypeId();
        auto handlerId = request.GetHandlerId();

        timer.async_wait(m_strand.wrap([this, senderId, reqId, typeId, handlerId]
                                       (const boost::system::error_code& error)
        {
            // This is the lambda that is executed when a request in the sender node has expired.

            if (error == boost::asio::error::operation_aborted)
            {
                return;
            }

            m_outReqTimers.erase(std::make_pair(senderId, reqId));

            // Generate a response.

            const ConnectionPtr sender = Connections::Instance().GetConnection
                                         (ConnectionId(Connections::Instance().NodeId(),
                                                       -1,
                                                       senderId),
                                          std::nothrow);

            if (sender == NULL)
            {
                // It seems that the sender is no longer there.
                return;
            }

            std::wostringstream ostr;
            ostr << "The handler " << handlerId << " did not respond to the request of type "
                 << Typesystem::Operations::GetName(typeId) << "!";

            //create timeout response
            Dob::ErrorResponsePtr errorResponse =
                    Dob::ErrorResponse::CreateErrorResponse
                    (Dob::ResponseGeneralErrorCodes::SafirTimeout(),
                     ostr.str());

            Typesystem::BinarySerialization bin;
            Typesystem::Serialization::ToBinary(errorResponse,bin);

            //convert response to Shared Message
            DistributionData response(response_tag,
                                      sender->Id(),
                                      sender->Id(),
                                      reqId,
                                      &bin[0]);

            //set the request handled and increase the timeouts count.
            sender->GetRequestOutQueue().RequestTimeout(reqId);

            //Post the response
            m_responseHandler->SendLocalResponse(response);

            sender->SignalIn();
        }));
    }

    bool RequestHandler::DistributeRequest(const DistributionData& request,
                                           const ConnectionConsumerPair& receiver)
    {
        const ConnectionPtr sender = Connections::Instance().GetConnection(request.GetSenderId(), std::nothrow);

        const bool senderOnThisNode = (sender != NULL && sender->IsLocal());
        const bool receiverOnThisNode = receiver.connection->IsLocal();

        if (senderOnThisNode && receiverOnThisNode)
        {
            return DistributeRequestLocalSenderLocalReceiver(request, sender, receiver);
        }
        else if (senderOnThisNode && !receiverOnThisNode)
        {
            return DistributeRequestLocalSenderExternReceiver(request, sender, receiver);
        }
        else if (!senderOnThisNode && receiverOnThisNode)
        {
            return DistributeRequestExternSenderLocalReceiver(request, receiver);
        }
        else
        {
            SEND_SYSTEM_LOG(Error,
                            << "DOSE_MAIN: Got a request that was neither sent to or from this node!");
            // "This does not really cause any problems, but it is unexpected,
            return true; //Always OK, request not for us.
        }
    }

    bool RequestHandler::DistributeRequestLocalSenderLocalReceiver(const DistributionData& request,
                                                                   const ConnectionPtr&    sender,
                                                                   const ConnectionConsumerPair& receiver)
    {
        if (!PostRequest(receiver, request))
        {
            m_blockingHandler.Request().AddWaitingConnection(receiver.connection->Id().m_id,
                                                             sender->Id().m_id);
            return false;
        }
        return true;
    }

    bool RequestHandler::DistributeRequestLocalSenderExternReceiver(const DistributionData& request,
                                                                    const ConnectionPtr& sender,
                                                                    const ConnectionConsumerPair& receiver)
    {
        // If the receiver node is gone we just discard the response
        const auto nodeIt = m_liveNodes.find(receiver.connection->Id().m_node);
        if (nodeIt == m_liveNodes.end())
        {
            return true;
        }

        auto data = boost::shared_ptr<const char[]> (request.GetReference(),
                                                     [](const char* data)
                                                     {
                                                         DistributionData::DropReference(data);
                                                     });

        auto ok = m_communication.Send(receiver.connection->Id().m_node,
                                       nodeIt->second,  //nodeTypeId
                                       data,
                                       request.Size(),
                                       m_dataTypeIdentifier,
                                       true); // delivery guarantee
        if (!ok)
        {
            m_blockingHandler.Request().AddWaitingConnection(m_communicationVirtualConnectionId,
                                                              sender->Id().m_id);
            return false;
        }

        return true;
    }


    bool RequestHandler::DistributeRequestExternSenderLocalReceiver(const DistributionData& request,
                                                                    const ConnectionConsumerPair& receiver)
    {
        if (ReceiverHasOtherPendingRequest(receiver.connection->Id().m_id, request.GetRequestId()))
        {
            lllog(7) << "DOSE_MAIN: ReceiverHasOtherPendingRequest, so we'll add it to pending!" << std::endl;

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

    bool RequestHandler::PostRequest(const ConnectionConsumerPair& receiver,
                                     const DistributionData& request)
    {
        bool success;
        receiver.connection->ForSpecificRequestInQueue
            (receiver.consumer,
             [this, request, &success](const ConsumerId& /*consumer*/, RequestInQueue& queue)
             {
                 success = queue.PushRequest(request);
                 lllog(8) << "DOSE_MAIN: Pushing request (id = " << request.GetRequestId()
                          << ". success = " << success << std::endl;
             });

        // Always kick the receiver. Theoretically a kick shouldn't be necessary when there
        // is an overflow, but since there have been reports of applications not dispatching
        // their in-queues we make this "extra" kick.
        receiver.connection->SignalIn();

        if (!success)
        {
            lllog(7) << "Overflow in a RequestInQueue. " << receiver.connection->NameWithCounter()
                << " " << receiver.connection->Id() << std::endl;
            return false;
        }
        return true;
    }

    void RequestHandler::SendAutoResponse(const Dob::ResponsePtr&   resp,
                                          const InternalRequestId&  reqId,
                                          const ConnectionPtr&      sender)
    {
        Typesystem::BinarySerialization bin;
        Typesystem::Serialization::ToBinary(resp,bin);

        //convert response to Shared Message
        DistributionData response(response_tag,
                                  sender->Id(),
                                  sender->Id(),
                                  reqId,
                                  &bin[0]);
        //Post the response
        m_responseHandler->SendLocalResponse(response);

        //Remove timer
        m_outReqTimers.erase(std::make_pair(sender->Id().m_id, reqId));

        sender->SignalIn();
    }

    void RequestHandler::HandleMessageFromRemoteNode(int64_t        /*fromNodeId*/,
                                                     int64_t        /*fromNodeType*/,
                                                     const char*    data)
    {
        const DistributionData request =
                DistributionData::ConstConstructor(new_data_tag, data);

        DistributionData::DropReference(data);

        lllog(8) << "DOSE_MAIN: Got request from remote node type="
                 << Safir::Dob::Typesystem::Operations::GetName(request.GetTypeId())
                 << ", handler = " << request.GetHandlerId() << std::endl;

        const ConnectionConsumerPair receiver = GetRegistrationOwner(request);
        if (receiver.connection == nullptr)
        {
            return; //ignore the request and let the timeout on the other node take care of it.
        }

        DistributeRequest(request, receiver);
    }

    void RequestHandler::ReleaseAllBlocked(int64_t blockingConnection)
    {

        std::set<int64_t> waitingConnections;
        if (!m_blockingHandler.Request().GetWaitingConnections(blockingConnection, waitingConnections))
        {
            // There are no connections waiting for blockingConnection
            return;
        }

        ConnectionId tmpConnectionId;
        tmpConnectionId.m_node = m_communication.Id();
        for (auto it = waitingConnections.begin(); it != waitingConnections.end(); ++it)
        {
            tmpConnectionId.m_id = *it;

            if (tmpConnectionId.m_id == m_communicationVirtualConnectionId)
            {
                // This indicates that we have a request from an external node that is waiting for
                // blockingConnection
                HandlePendingExternalRequest(blockingConnection);
            }
            else
            {
                DispatchRequests(Connections::Instance().GetConnection(tmpConnectionId));
            }
        }
    }

    void RequestHandler::HandlePendingExternalRequest(int64_t blockingConnection)
    {
        lllog(7) << "DOSE_MAIN: HandlePendingExternalRequest for app " << blockingConnection << std::endl;

        auto it = m_pendingRequests.find(blockingConnection);

        if (it == m_pendingRequests.end())
        {
            return;
        }

        while (!it->second.empty())
        {
            const DistributionData& request = it->second.front().first;

            const ConnectionConsumerPair receiver = GetRegistrationOwner(request);
            if (receiver.connection == nullptr)
            {
                // if no receiver ignore the request and let the timeout on the other node take care of it.
                it->second.pop_front();
            }
            else
            {
                if (DistributeRequest(request, receiver))
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


    void RequestHandler::SetShortTimeout(const DistributionData& request,
                                         const ConnectionPtr& deletedConn,
                                         const ConnectionPtr & fromConnection)
    {
        const ConnectionPtr regOwner = GetRegistrationOwner(request).connection;

        if (regOwner == nullptr || regOwner->Id() == deletedConn->Id())
        {
            // Found a request towards an unregistered entity/service or an entity/service
            // that is owned by the deleted connection.

            const InternalRequestId reqId = request.GetRequestId();

            // Set the running timer for this request to expire in a short time from now giving
            // any response sent from the deleted connection a chance to be received.

            auto timerIt = m_outReqTimers.find(std::make_pair(fromConnection->Id().m_id, reqId));

            if (timerIt == m_outReqTimers.end())
            {
                // Can't find timer, just quit
                return;
            }

            timerIt->second->expires_from_now(boost::chrono::milliseconds(500));

            StartOutReqTimer(fromConnection, request, *timerIt->second);
        }
    }

    void RequestHandler::AddPendingRequest(const int64_t blockingConn, const DistributionData& request)
    {
        lllog(7) << "DOSE_MAIN: AddPendingRequest for app " << blockingConn <<std::endl;

        auto timer = Safir::make_unique<boost::asio::steady_timer>(m_ioService);

        auto reqId = request.GetRequestId();

        timer->expires_from_now(GetTimeout(request.GetTypeId()));

        timer->async_wait(m_strand.wrap([this, blockingConn, reqId]
                                        (const boost::system::error_code& error)
        {
            // This is the lambda that is executed when a pending external request has expired

            if (error == boost::asio::error::operation_aborted)
            {
                // The timer has been canceled.
                return;
            }

            // We just remove the request since the corresponding timout in the sender node will take care
            // of the response generation.
            RemovePendingRequest(blockingConn, reqId);
        }));

        auto it = m_pendingRequests.find(blockingConn);
        if(it != m_pendingRequests.end())
        {            
            bool reqFound = false;
            auto requestIt = it->second.begin();
            while (requestIt != it->second.end())
            {
                if (requestIt->first.GetRequestId() == reqId)
                {
                    reqFound = true;
                    break;
                }
                ++requestIt;
            }

            if (!reqFound)
            {
                it->second.push_back(std::make_pair(request, std::move(timer)));
            }
        }
        else
        {
            // No previous request for blockingConn
            PendingRequestsQueue requestQueue;
            requestQueue.push_back(std::make_pair(request, std::move(timer)));
            m_pendingRequests.insert(std::make_pair(blockingConn, std::move(requestQueue)));
        }

        // Set that an external request is waiting for the receiver
        m_blockingHandler.Request().AddWaitingConnection(blockingConn,
                                                         m_communicationVirtualConnectionId);

    }

    void RequestHandler::RemovePendingRequest(const int64_t blockingConn, const InternalRequestId& requestId)
    {
        auto it = m_pendingRequests.find(blockingConn);
        if(it != m_pendingRequests.end())
        {
            auto newEnd = std::remove_if(it->second.begin(),
                                         it->second.end(),
                                         [&requestId](const PendingRequest& request)
                                         {
                                             return request.first.GetRequestId() == requestId;
                                         });

            it->second.erase(newEnd,it->second.end());

            if (it->second.empty())
            {
                m_pendingRequests.erase(it);
            }
        }
    }

    bool RequestHandler::ReceiverHasOtherPendingRequest(const int64_t receiver,
                                                        const InternalRequestId& requestId) const
    {
        auto findIt = m_pendingRequests.find(receiver);
        if(findIt != m_pendingRequests.end())
        {
            if (!findIt->second.empty() && findIt->second.front().first.GetRequestId() == requestId)
            {
                // This request is first in the queue which means that we're not waiting for another request
                // to be sent before sending this one.
                return false;
            }
            return true;
        }
        else
        {
            return false;
        }
    }

    boost::chrono::milliseconds
    RequestHandler::GetTimeout(const Safir::Dob::Typesystem::TypeId typeId) const
    {
        TimeoutTable::iterator findIt = m_timeoutTable.find(typeId);
        if (findIt != m_timeoutTable.end())
        {
            boost::chrono::milliseconds(static_cast<int>(findIt->second * 1000));
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
        return boost::chrono::milliseconds(static_cast<int>(timeout * 1000));
    }

}
}
}
