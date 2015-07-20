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

#include "PendingRegistrationHandler.h"

#include <Safir/Dob/Internal/ServiceTypes.h>
#include <Safir/Dob/Internal/EntityTypes.h>
#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Dob/Service.h>
#include <iomanip>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include <Safir/Utilities/Internal/MakeUnique.h>
#include <boost/make_shared.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    namespace
    {
        bool IsRegistered(const Dob::Typesystem::TypeId typeId,
                          const Dob::Typesystem::HandlerId& handlerId,
                          const ContextId contextId)
        {
            if (Safir::Dob::Typesystem::Operations::IsOfType(typeId,Safir::Dob::Service::ClassTypeId))
            {
                return ServiceTypes::Instance().IsRegistered(typeId,handlerId, contextId);
            }
            else
            {
                return EntityTypes::Instance().IsRegistered(typeId, handlerId, contextId);
            }
        }

        bool IsPendingAccepted(const Dob::Typesystem::TypeId typeId,
                               const Typesystem::HandlerId& handlerId,
                               const ContextId contextId)
        {
            return Connections::Instance().IsPendingAccepted(typeId, handlerId, contextId);
        }

    }


    PendingRegistrationHandler::PendingRegistrationHandler(boost::asio::io_service& ioService,
                                                           Distribution& distribution)
        : m_stopped(false)
        , m_strand(ioService)
        , m_distribution(distribution)
        , m_dataTypeIdentifier(LlufId_Generate64("PendingRegistrationHandler"))
        , m_nextId(1)
        , m_pendingRegistrationClock(distribution.GetNodeId())
    {
        //ioService is started after our constructor, so we do not need to be reentrant.
        distribution.SubscribeNodeEvents(m_strand.wrap([this](const std::string& /*nodeName*/,
                                                              const int64_t nodeId,
                                                              const int64_t nodeTypeId,
                                                              const std::string& /*dataAddress*/)
                                                       {
                                                           if (nodeId != m_distribution.GetNodeId())
                                                           {
                                                               m_liveNodes.insert(std::make_pair(nodeId,nodeTypeId));
                                                               CheckForPending();
                                                           }
                                                       }),
                                         m_strand.wrap([this](const int64_t nodeId,
                                                              const int64_t /*nodeTypeId*/)
                                                       {
                                                           m_liveNodes.erase(nodeId);
                                                           CheckForPending();
                                                       }));
        m_distribution.GetCommunication().SetDataReceiver([this]
                                                          (const int64_t fromNodeId,
                                                           int64_t fromNodeType,
                                                           const char* data,
                                                           size_t /*size*/)
                                                          {
                                                              const DistributionData msg =
                                                                  DistributionData::ConstConstructor(new_data_tag, data);

                                                              DistributionData::DropReference(data);

                                                              auto this_ = this; //fix for vs 2010 lamba issues
                                                              m_strand.dispatch([this_,msg,fromNodeId,fromNodeType]{this_->HandleRequest(msg,fromNodeId,fromNodeType);});
                                                          },
                                                          m_dataTypeIdentifier,
                                                          DistributionData::NewData,
                                                          DistributionData::DropReference);
    }

    void PendingRegistrationHandler::Stop()
    {
        const bool was_stopped = m_stopped.exchange(true);
        if (!was_stopped)
        {
            m_strand.dispatch([this]
                              {
                                  for (auto reg = m_pendingRegistrations.cbegin(); reg != m_pendingRegistrations.cend(); ++reg)
                                  {
                                      reg->second->timer.cancel();
                                  }
                              });
        }
    }

    //must be called in strand
    void PendingRegistrationHandler::TryPendingRegistration(const long requestId)
    {
        PendingRegistrations::iterator findIt = m_pendingRegistrations.find(requestId);
        if (findIt != m_pendingRegistrations.end())
        {
            if (!IsRegistered(findIt->second->typeId, findIt->second->handlerId, findIt->second->connectionId.m_contextId))
            {
                const bool completed = HandleCompletion(requestId);
                if (!completed)
                {
                    SendRequest(requestId);
                }
            }
            else
            {
                lllout << "Request " << requestId << " is still pending"<<std::endl;
            }
        }
    }


    void
    PendingRegistrationHandler::CheckForNewOrRemovedPendingRegistration(const ConnectionPtr & connection)
    {
        m_strand.dispatch([this,connection]
        {
            //Check for new pending registrations
            {
                PendingRegistration reg;
                while (connection->GetNextNewPendingRegistration(m_nextId, reg))
                {
                    std::pair<PendingRegistrationHandler::PendingRegistrations::iterator, bool> result =
                        m_pendingRegistrations.emplace
                        (std::make_pair(reg.id,
                                        Safir::make_unique<PendingRegistrationInfo>(m_strand.get_io_service(),
                                                                                    connection->Id(),
                                                                                    reg.typeId,
                                                                                    reg.handlerId.GetHandlerId())));

                    lllout << "Inserted a new pending RegistrationRequest for type: "
                           << Dob::Typesystem::Operations::GetName(reg.typeId) << ", connection: " << connection->NameWithCounter()
                           << ", handler: " << reg.handlerId.GetHandlerId()  << ", context: " << connection->Id().m_contextId << std::endl;

                    ENSURE(result.second, << "Inserting new pending registration request failed!");

                    //count up nextId until we find a free spot (most likely after just one try)
                    while(m_pendingRegistrations.find(++m_nextId) != m_pendingRegistrations.end())
                    {
                        //do nothing
                    }

                    TryPendingRegistration(reg.id);

                }

            }

            //check for removed prs
            {
                PendingRegistrationVector prv = connection->GetRemovedPendingRegistrations();

                for (PendingRegistrationVector::iterator it = prv.begin();
                     it!= prv.end(); ++it)
                {
                    lllout << "Removing RegistrationRequest for " << Dob::Typesystem::Operations::GetName(it->typeId)
                           << " as request id " << it->id <<std::endl;
                    ENSURE(it->remove, << "The PR does not have the remove flag!");
                    PendingRegistrationHandler::PendingRegistrations::iterator findIt = m_pendingRegistrations.find(it->id);

                    if (findIt != m_pendingRegistrations.end())
                    { //it may have managed to get completed while we were doing other stuff...
                        m_pendingRegistrations.erase(findIt);
                    }
                }
            }
        });
    }

    //must be called in strand
    bool PendingRegistrationHandler::HandleCompletion(const long requestId)
    {
        PendingRegistrations::iterator findIt = m_pendingRegistrations.find(requestId);

        ENSURE (findIt != m_pendingRegistrations.end(), << "PendingRegistrationHandler::HandleCompletion: Request id not found!");

        //check if we have the response from all nodes that are up
        PendingRegistrationInfo & reg = *findIt->second;
        bool gotAll = true;

        if (!m_distribution.IsLocal(findIt->second->typeId))
        {
            for (auto node = m_liveNodes.cbegin(); node != m_liveNodes.cend(); ++node)
            {
                if (reg.acceptedNodes.find(node->first) == reg.acceptedNodes.end())
                {
                    gotAll = false;
                    lllout << "Request " << requestId << " needs accept from node " << node->first << std::endl;
                }
                else
                {
                    lllout << "Request " << requestId << " has accept from node " << node->first << std::endl;
                }
            }
        }

        if (gotAll)
        {
            lllout << "Request " << requestId << " got accept from all nodes. Erasing it from m_pendingRegistrations" << std::endl;
            const ConnectionPtr conn = Connections::Instance().GetConnection(reg.connectionId);
            conn->SetPendingRegistrationAccepted(requestId);
            conn->SignalIn();
            m_pendingRegistrations.erase(findIt);
            return true;
        }
        else
        {
            lllout << "Request " << requestId << " does not have accept from all nodes" << std::endl;
            return false;
        }
    }

    //must be called in strand
    void PendingRegistrationHandler::SendRequest(const long requestId)
    {
        PendingRegistrations::iterator findIt = m_pendingRegistrations.find(requestId);

        ENSURE (findIt != m_pendingRegistrations.end(),
                << "PendingRegistrationHandler::SendRequest: Request id not found!");

        const boost::chrono::steady_clock::time_point now = boost::chrono::steady_clock::now();

        findIt->second->lastRequestTimestamp = m_pendingRegistrationClock.GetNewTimestamp();
        findIt->second->rejected = false;
        findIt->second->acceptedNodes.clear();

        lllout << "Sending Smt_Action_PendingRegistrationRequest requestId = " << requestId
               << ", type = " << Dob::Typesystem::Operations::GetName(findIt->second->typeId)
               << std::setprecision(20) << "timestamp = " << findIt->second->lastRequestTimestamp << std::endl;

        DistributionData msg(pending_registration_request_tag,
                             findIt->second->connectionId,
                             findIt->second->typeId,
                             findIt->second->handlerId,
                             findIt->second->lastRequestTimestamp,
                             findIt->first);

        boost::shared_ptr<const char[]> msgP(msg.GetReference(),
                                             [](const char* data)
                                             {
                                                 DistributionData::DropReference(data);
                                             });

        bool success = true;
        // Send message to all node types
        for (auto nodeType = m_distribution.GetNodeTypeIds().cbegin(); nodeType != m_distribution.GetNodeTypeIds().cend(); ++nodeType)
        {
            success = success &&
                m_distribution.GetCommunication().Send(0,  // All nodes of the type
                                                       *nodeType,
                                                       msgP,
                                                       msg.Size(),
                                                       m_dataTypeIdentifier,
                                                       true);

        }

        if (success)
        {
            // Set the first timeout to now + 1.0, the second to now + 1.5, and so on. This is to handle
            // any node that for some reason is permanently slow.
            findIt->second->timer.expires_at(now +
                                             boost::chrono::milliseconds(1000 + findIt->second->nbrOfSentRequests * 500));

            ++findIt->second->nbrOfSentRequests;
        }
        else
        {
            findIt->second->timer.expires_at(now + boost::chrono::milliseconds(10));
        }

        findIt->second->timer.async_wait(m_strand.wrap([this, requestId](const boost::system::error_code& error)
                                                       {
                                                           if (error || m_stopped)
                                                           {
                                                               return;
                                                           }

                                                           TryPendingRegistration(requestId);
                                                       }));
    }

    void
    PendingRegistrationHandler::CheckForPending(const Safir::Dob::Typesystem::TypeId typeId)
    {
        m_strand.dispatch([this,typeId]
        {
            std::vector<long> affectedRequestIds;

            for (auto elem = m_pendingRegistrations.cbegin(); elem != m_pendingRegistrations.cend(); ++elem)
            {
                if (elem->second->typeId == typeId)
                {
                    affectedRequestIds.push_back(elem->first);
                }
            }

            for (auto affectedRequestId = affectedRequestIds.cbegin(); affectedRequestId != affectedRequestIds.cend(); ++affectedRequestId)
            {
                TryPendingRegistration(*affectedRequestId);
            }
        });
    }

    void
    PendingRegistrationHandler::CheckForPending()
    {
        m_strand.dispatch([this]
        {
            for (PendingRegistrationHandler::PendingRegistrations::iterator it = m_pendingRegistrations.begin();
                 it != m_pendingRegistrations.end();)  // iterator incrementation done below
            {
                // Important to increment iterator before we call TryPendingRegistration since
                // the pointed to object might get erased in that routine.
                PendingRegistrationHandler::PendingRegistrations::iterator tmpIt = it;
                ++it;
                TryPendingRegistration(tmpIt->first);
            }
        });
    }

    //must be called in strand
    void PendingRegistrationHandler::HandleRequest(const DistributionData & msg,
                                                   const int64_t fromNodeId,
                                                   const int64_t fromNodeType)
    {
        switch (msg.GetType())
        {
        case DistributionData::Action_PendingRegistrationRequest:
            {
                const Typesystem::TypeId typeId = msg.GetTypeId();
                const Typesystem::HandlerId handlerId = msg.GetHandlerId();
                const ContextId contextId = msg.GetSenderId().m_contextId;
                const LamportTimestamp timestamp = msg.GetPendingRequestTimestamp();
                m_pendingRegistrationClock.UpdateCurrentTimestamp(timestamp);
                const long requestId = msg.GetPendingRequestId();

                lllout << "Got Smt_Action_PendingRegistrationRequest from node " << msg.GetSenderId().m_node
                         << ", requestId = " << requestId
                         << ", type = " << Dob::Typesystem::Operations::GetName(typeId)
                         << ", handler = " << handlerId
                         << ", context = " << contextId
                         << std::setprecision(20) << ", timestamp = " << timestamp << std::endl;

                //create a response
                DistributionData resp(pending_registration_response_tag,
                                      msg,
                                      ConnectionId(m_distribution.GetNodeId(),
                                                   contextId,
                                                   -1),
                                      true);

                //check if we have a reason to say no!

                //do we have an outstanding pending that is older!
                for (auto elem = m_pendingRegistrations.cbegin(); elem != m_pendingRegistrations.cend(); ++elem)
                {
                    if (elem->second->typeId == typeId &&
                        elem->second->handlerId == handlerId &&
                        elem->second->connectionId.m_contextId == contextId)
                    {
                        if (!elem->second->rejected && elem->second->lastRequestTimestamp < timestamp)
                        {//no, mine is older!
                            lllout << "No, I believe I have an older pending request!" <<std::endl;
                            resp.SetPendingResponse(false);
                            break;
                        }
                        // or that there is an accepted registration on its way through!
                    }
                }

                //check that it is not registered on this machine
                if (IsRegistered(typeId, handlerId, contextId))
                {
                    lllout << "No, that type/handler is registered on my machine!" <<std::endl;
                    resp.SetPendingResponse(false);
                }

                //Check if it is accepted but not yet registered.
                if (IsPendingAccepted(typeId, handlerId, contextId))
                {
                    lllout << "No, I've got an accepted pending for that ObjectId!" <<std::endl;
                    resp.SetPendingResponse(false);
                }

                lllout << "Sending response " << std::boolalpha << msg.GetPendingResponse() << std::endl;
                boost::shared_ptr<const char[]> respP(resp.GetReference(),
                                                      [](const char* data)
                                                      {
                                                          DistributionData::DropReference(data);
                                                      });

                m_distribution.GetCommunication().Send(fromNodeId,
                                                       fromNodeType,
                                                       respP,
                                                       resp.Size(),
                                                       m_dataTypeIdentifier,
                                                       true);
                //If sending fails he will ask me again to approve of his registration, so ignore failures
            }
            break;

        case DistributionData::Action_PendingRegistrationResponse:
            {
                const ConnectionId sender = msg.GetSenderId();
                const Typesystem::TypeId typeId = msg.GetTypeId();
                const Typesystem::HandlerId handlerId = msg.GetHandlerId();
                const LamportTimestamp timestamp = msg.GetPendingRequestTimestamp();
                m_pendingRegistrationClock.UpdateCurrentTimestamp(timestamp);
                const long requestId = msg.GetPendingRequestId();
                const bool response = msg.GetPendingResponse();
                lllout << "Got Smt_Action_PendingRegistrationResponse from node " << sender.m_node
                         << ", requestId = " << requestId
                         << ", typeId = " << Typesystem::Operations::GetName(typeId)
                         << ", handlerId = " << handlerId
                         << ", timestamp = " << timestamp
                         << ", response = " << std::boolalpha << response<< std::endl;

                PendingRegistrations::iterator findIt = m_pendingRegistrations.find(requestId);
                if (findIt == m_pendingRegistrations.end())
                {
                    lllout << "Request id not found, discarding (probably not a request from this node)"<<std::endl;
                    return;
                }

                if (findIt->second->connectionId != msg.GetPendingOriginator())
                {
                    lllout << "Originator does not correspond to pending connection id"<<std::endl
                        << " - Expected (" << findIt->second->connectionId.m_id << ", "
                        << findIt->second->connectionId.m_node << ") but got ("
                        <<    msg.GetPendingOriginator().m_id<< ", "
                        <<    msg.GetPendingOriginator().m_node<< ")"
                        <<    std::endl;
                    return;
                }

                if (findIt->second->lastRequestTimestamp != timestamp)
                {
                    lllout << "Request time did not match, discarding"<<std::endl;
                    return;
                }

                if (findIt->second->typeId != typeId)
                {
                    lllout << "Type id did not match, discarding"<<std::endl;
                    return;
                }

                if (findIt->second->handlerId != handlerId)
                {
                    lllout << "HandlerId did not match, discarding"<<std::endl;
                    return;
                }

                if (response)
                {
                    lllout << "Accept from "<< sender.m_node << ", " << sender.m_id<<std::endl;
                    findIt->second->acceptedNodes.insert(sender.m_node);
                    HandleCompletion(requestId);
                }
                else
                {
                    lllout << "Reject from "<< sender.m_node << ", " << sender.m_id<<std::endl;
                    findIt->second->rejected = true;
                }
            }
            break;
        default:
            ENSURE(false,<<"Got unexpected type of DistributionData in "
                   << "PendingRegistrationHandler::HandleRequest. type = " << msg.GetType());
        }
    }

    void PendingRegistrationHandler::RemovePendingRegistrations(const ConnectionId & id)
    {
        m_strand.dispatch([this,id]
        {
            lllout << "RemovePendingRegistrations for " << id << std::endl;
            for (PendingRegistrationHandler::PendingRegistrations::iterator it = m_pendingRegistrations.begin();
                 it != m_pendingRegistrations.end();)
            {
                if (it->second->connectionId == id)
                {
                    lllout << "  " << it->second->handlerId << std::endl;

                    m_pendingRegistrations.erase(it++);
                }
                else
                {
                    ++it;
                }
            }
        });
    }
}
}
}
