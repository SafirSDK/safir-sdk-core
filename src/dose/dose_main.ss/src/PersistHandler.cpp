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

#include "PersistHandler.h"
#include <Safir/Dob/PersistenceParameters.h>
#include <Safir/Dob/PersistentDataReady.h>
#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Dob/SuccessResponse.h>
#include <Safir/Dob/ErrorResponse.h>
#include <Safir/Dob/Internal/EntityTypes.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include <Safir/Dob/ResponseGeneralErrorCodes.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Utilities/Internal/Id.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>


namespace Safir
{
namespace Dob
{
namespace Internal
{

    PersistHandler::PersistHandler(boost::asio::io_service& ioService,
                                   Distribution& distribution,
                                   const std::function<void(const std::string& str)>& logStatus,
                                   const std::function<void()>& persistentDataReadyCb,
                                   const std::function<void()>& persistentDataAllowedCb)
        : m_strand(ioService),
          m_logStatus(logStatus),
          m_systemFormed(false),
          m_distribution(distribution),
          m_communication(distribution.GetCommunication()),
          m_dispatcher(m_connection, m_strand),
          m_persistentDataReady(false),
          m_persistentDataAllowed(false),
          m_dataTypeIdentifier(LlufId_Generate64("Safir.Dob.HavePersistenceData"))
    {
        m_strand.dispatch([this, logStatus, persistentDataReadyCb, persistentDataAllowedCb]()
        {
            m_persistentDataReadyCb.push_back(persistentDataReadyCb);
            m_persistentDataAllowedCb.push_back(persistentDataAllowedCb);

            if(Dob::PersistenceParameters::TestMode())
            {
                logStatus("RUNNING IN PERSISTENCE TEST MODE! PLEASE CHANGE PARAMETER "
                          "Safir.Dob.PersistenceParameters.TestMode IF THIS IS NOT WHAT YOU EXPECTED!");
                ENSURE(Dob::PersistenceParameters::Backend() == Dob::PersistenceBackend::Enumeration::None,
                       << "Safir.Dob.PersistenceBackend must be None when running in persistence test mode");
            }

            logStatus("dose_main is waiting for persistence data!");

            m_connection.Open(L"dose_main", L"persist_handler", 0, nullptr, &m_dispatcher);

            m_connection.RegisterServiceHandler
                (Dob::PersistentDataReady::ClassTypeId,
                 Typesystem::HandlerId(),
                 this);
        });

        m_distribution.SubscribeNodeEvents([this]
                                           (const std::string&    /*nodeName*/,
                                            int64_t                nodeId,
                                            int64_t                nodeTypeId,
                                           const std::string&     /*dataAddress*/)
        {
            m_strand.post([this, nodeId, nodeTypeId]
                          {
                              if (m_systemFormed || m_persistentDataReady)
                              {
                                  return;
                              }

                              m_nodes.insert(std::make_pair(nodeId, nodeTypeId));
                          });
        },
                                           [this]
                                           (int64_t   nodeId,
                                            int64_t   nodeTypeId)
        {
            m_strand.post([this, nodeId, nodeTypeId]
                          {
                              if (m_persistentDataReady)
                              {
                                  return;
                              }

                              m_nodes.erase(std::make_pair(nodeId, nodeTypeId));

                             CheckResponseStatus();
                          });


        });

        m_communication.SetQueueNotFullCallback(m_strand.wrap([this]
                                                              (int64_t /*nodeTypeId*/)
                                                {
                                                    Resend();
                                                }),
                                                0); // All node types

        m_communication.SetDataReceiver(m_strand.wrap([this]
                                                      (int64_t fromNodeId,
                                                       int64_t fromNodeType,
                                                       const char* data,
                                                       size_t /*size*/)
                                        {
                                            HandleMessageFromRemoteNode(fromNodeId, fromNodeType, data);
                                        }),
                m_dataTypeIdentifier,
                DistributionData::NewData);

    }

    void PersistHandler::Start()
    {
        m_strand.post([this]
                      {
                          m_systemFormed = true;

                          if (m_nodes.size() > 0)
                          {
                              // Send requests to all other nodes asking if someone has
                              // persistence
                              RequestPersistenceInfo();
                          }
                          else
                          {
                              // There are no other nodes
                              SetPersistentDataAllowed();
                          }
                      });
    }

    void PersistHandler::Stop()
    {
        m_strand.post([this]
                      {
                        m_connection.Close();
                      });
    }

    void PersistHandler::SetPersistentDataReady()
    {
        m_strand.dispatch([this] ()
        {
            if (m_persistentDataReady)
            {
                return; //persist data was already ready
            }

            m_logStatus("dose_main persistence data is ready!");

            m_persistentDataReady = true;

            if (!Dob::PersistenceParameters::TestMode())
            {
                //disallow more persistence data injections
                EntityTypes::Instance().DisallowInitialSet();
            }

            SetPersistentDataAllowed();

            for (auto& cb : m_persistentDataReadyCb)
            {
                cb();
            }

            //We could be inside a Dob callback, so we need
            //to post the call to Close, or else we might be
            //killing the connection that is dispatching...
            m_strand.post([this]
                          {
                              // We don't need the connection now
                              m_connection.Close();
                          });
        });

    }

    // To be called only when strand is taken
    void PersistHandler::SetPersistentDataAllowed()
    {
        if (m_persistentDataAllowed)
        {
            return; // injection already allowed
        }

        m_persistentDataAllowed = true;

        for (auto& cb : m_persistentDataAllowedCb)
        {
            cb();
        }
    }

    // OnDoDispatch guarantees that strand is taken when this method is called
    void PersistHandler::OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId    /*typeId*/,
                                               const Safir::Dob::Typesystem::HandlerId& /*handlerId*/)
    {
        SEND_SYSTEM_LOG(Alert,
                        << "Someone overregistered Safir::Dob::PersistentDataReady, "
                           "so I'm not going to be able to allow other connections to connect!");
    }

    // OnDoDispatch guarantees that strand is taken when this method is called
    void PersistHandler::OnServiceRequest(const Safir::Dob::ServiceRequestProxy serviceRequestProxy,
                                          Safir::Dob::ResponseSenderPtr   responseSender)
    {
        responseSender->Send(Dob::SuccessResponse::Create());

        // This service request indicates that persistent data is ready
        if (!m_persistentDataReady)
        {
            SetPersistentDataReady();
        }
    }

    // To be called only when strand is taken
    void PersistHandler::RequestPersistenceInfo()
    {
        auto request = CreateRequest();

        // Send the request to all known nodes
        for (const auto& node : m_nodes)
        {
            auto ok = m_communication.Send(node.first, // nodeId
                                           node.second, // nodetypeId
                                           request.first,
                                           request.second, // size
                                           m_dataTypeIdentifier,
                                           true); // delivery guarantee
            if (ok)
            {
                lllog(3) << "DOSE_MAIN: Sent HavePersistenceDataRequest to nodeId " << node.first << std::endl;
            }
            else
            {
               m_unsentRequests.insert(node);
            }
        }
    }

    // To be called only when strand is taken
    void PersistHandler::HandleMessageFromRemoteNode(const int64_t  fromNodeId,
                                                     const int64_t  fromNodeType,
                                                     const char*    data)
    {
        const DistributionData msg =
                DistributionData::ConstConstructor(new_data_tag, data);

        DistributionData::DropReference(data);

        if (msg.GetType() == DistributionData::Action_HavePersistenceDataRequest)
        {
            auto response = CreateResponse();

            auto ok = m_communication.Send(fromNodeId,
                                           fromNodeType,
                                           response.first,
                                           response.second,  // size
                                           m_dataTypeIdentifier,
                                           true); // delivery guarantee
             if (ok)
             {
                 lllog(3) << "DOSE_MAIN: Sent HavePersistenceDataResponse=" << m_persistentDataReady
                          << " to nodeId " << fromNodeId << std::endl;
             }
             else
             {
                m_unsentResponses.insert(std::make_pair(fromNodeId, fromNodeType));
             }
        }
        else if (msg.GetType() == DistributionData::Action_HavePersistenceDataResponse)
        {
            lllog(3) << "DOSE_MAIN: Got an Action_HavePersistenceDataResponse" << std::endl;

            if (msg.GetIHavePersistenceData())
            {
                lllog(3) << "DOSE_MAIN: Node " << msg.GetSenderId().m_node
                         << " has persistence. Disallow initial set and let -1:s connect." << std::endl;

                EntityTypes::Instance().DisallowInitialSet();
                SetPersistentDataAllowed();

                m_unsentRequests.clear();
            }
            else
            {
                lllog(3) << "DOSE_MAIN: Node " << msg.GetSenderId().m_node
                         << " doesnt have persistence." << std::endl;

                m_nodes.erase(std::make_pair(fromNodeId, fromNodeType));

                CheckResponseStatus();
            }
        }
        else
        {
            ENSURE (false, << "PersistHandler: Received unexpected message!");
        }
    }

    void PersistHandler::Resend()
    {
        // Resend any unsent requests
        if (!m_unsentRequests.empty())
        {
            auto request = CreateRequest();

            for (auto it = m_unsentRequests.begin(); it != m_unsentRequests.end();)
            {
                auto ok = m_communication.Send(it->first, //nodeId
                                               it->second, //nodeTypeId
                                               request.first,
                                               request.second,  // size
                                               m_dataTypeIdentifier,
                                               true); // delivery guarantee
                 if (ok)
                 {
                    lllog(3) << "DOSE_MAIN: Sent HavePersistenceDataRequest to nodeId " << it->first << std::endl;

                    it = m_unsentRequests.erase(it);
                 }
                 else
                 {
                    ++it;
                 }
            }
        }

        // Resend any unsent responses
        if (!m_unsentResponses.empty())
        {
            auto response = CreateResponse();

            for (auto it = m_unsentResponses.begin(); it != m_unsentResponses.end();)
            {
                auto ok = m_communication.Send(it->first, //nodeId
                                               it->second, //nodeTypeId
                                               response.first,
                                               response.second,  // size
                                               m_dataTypeIdentifier,
                                               true); // delivery guarantee
                 if (ok)
                 {
                     lllog(3) << "DOSE_MAIN: Sent HavePersistenceDataResponse=" << m_persistentDataReady
                              << " to nodeId " << it->first << std::endl;

                    it = m_unsentResponses.erase(it);
                 }
                 else
                 {
                    ++it;
                 }
            }
        }
    }

    void PersistHandler::CheckResponseStatus()
    {
        if (m_nodes.empty())
        {
            lllog(3) << "DOSE_MAIN: Noone has persistence!"
                        " Allow -1:s to connect (without disabling initialset)." << std::endl;

            // Everyone has responded saying that they have not seen persistence data
            // allow initial sets to start.
            SetPersistentDataAllowed();
        }
    }

    std::pair<boost::shared_ptr<const char[]>, size_t> PersistHandler::CreateRequest() const
    {
        DistributionData request
                (have_persistence_data_request_tag,
                 ConnectionId(m_communication.Id(),
                              0,     //use context 0 for this request
                              -1));  //dummy identifier since it is a dose_main only thing.

        return std::make_pair(boost::shared_ptr<const char[]> (request.GetReference(),
                                                [](const char* data)
                                                {
                                                    DistributionData::DropReference(data);
                                                }),
                              request.Size());
    }

    std::pair<boost::shared_ptr<const char[]>, size_t> PersistHandler::CreateResponse() const
    {
        DistributionData response
                (have_persistence_data_response_tag,
                 ConnectionId(m_communication.Id(),
                              0,    //use context 0 for this response
                              -1),  //dummy identifier since it is a dose_main only thing.
                 m_persistentDataReady);

        return std::make_pair(boost::shared_ptr<const char[]> (response.GetReference(),
                                                [](const char* data)
                                                {
                                                    DistributionData::DropReference(data);
                                                }),
                              response.Size());
    }
}
}
}
