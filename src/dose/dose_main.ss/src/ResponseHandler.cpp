/******************************************************************************
*
* Copyright Saab AB, 2007-2013,2015 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
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

#include "ResponseHandler.h"

#include <Safir/Dob/Internal/Connection.h>
#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    ResponseHandler::ResponseHandler(boost::asio::io_service::strand& strand,
                                     Distribution& distribution,
                                     const std::function<void(const ConnectionId& connectionId,
                                                              const InternalRequestId requestId)>& responsePostedCallback)
        : m_strand(strand)
        , m_distribution(distribution)
        , m_dataTypeIdentifier(LlufId_Generate64("ResponseHandler"))
        , m_responsePostedCallback(responsePostedCallback)
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
                                                           }
                                                       }),
                                         m_strand.wrap([this](const int64_t nodeId,
                                                              const int64_t /*nodeTypeId*/)
                                                       {
                                                           m_liveNodes.erase(nodeId);
                                                       }));
        m_distribution.GetCommunication().SetDataReceiver(m_strand.wrap([this] (const int64_t /*fromNodeId*/,
                                                                                int64_t /*fromNodeType*/,
                                                                                const char* data,
                                                                                size_t /*size*/)
        {
            const DistributionData msg =
                DistributionData::ConstConstructor(new_data_tag, data);

            DistributionData::DropReference(data);

            SendLocalResponse(msg);
        }),
                                                          m_dataTypeIdentifier,
                                                          DistributionData::NewData,
                                                          DistributionData::DropReference);

        for (auto nodeTypeId = m_distribution.GetNodeTypeIds().cbegin(); nodeTypeId != m_distribution.GetNodeTypeIds().cend(); ++nodeTypeId)
        {
            m_distribution.GetCommunication().SetQueueNotFullCallback
                (m_strand.wrap([this](const int64_t /*nodeTypeId*/)
                               {
                                   const auto pending = std::move(m_waitingConnections);
                                   for (auto conn = pending.cbegin(); conn != pending.cend(); ++conn)
                                   {
                                       const ConnectionPtr to = Connections::Instance().
                                           GetConnection(*conn,std::nothrow);
                                       if (to != NULL) //if receiver of the response is dead, theres nothing to do
                                       {
                                           DistributeResponses(to);
                                       }
                                   }
                               }),
                 *nodeTypeId);
        }
    }


    void ResponseHandler::DistributeResponses(const ConnectionPtr& sender)
    {
        m_strand.dispatch([this, sender]
        {
            const auto senderId = sender->Id();

            auto this_ = this; //fix for vs2010 issue with lambdas

            //loop over all the RequestInQueues in the connection
            sender->ForEachRequestInQueue([this_, senderId](const ConsumerId& /*consumer*/, RequestInQueue& queue)
            {
                auto this__ = this_;
                auto & senderId_ = senderId;

                //loop over all responses in the connection
                queue.DispatchResponses([this__, senderId_](const DistributionData& response, bool& dontRemove)
                {
                    //Try to send the response
                    const bool success = this__->SendResponseInternal(response);
                    dontRemove = !success;

                    if (!success)
                    {
                        this__->m_waitingConnections.insert(senderId_);
                    }
                });
            });
        });
    }


    //must be called in strand
    void ResponseHandler::SendLocalResponse(const DistributionData& response)
    {
        const ConnectionId toConnection=response.GetReceiverId();

        ENSURE (toConnection.m_node == m_distribution.GetNodeId(),
                << "SendLocalResponse can only be used for local responses!");

        const ConnectionPtr to = Connections::Instance().GetConnection(toConnection,std::nothrow);

        if (to != NULL) //if receiver of the response is dead, theres nothing to do
        {
            to->GetRequestOutQueue().AttachResponse(response);

            m_responsePostedCallback(response.GetReceiverId(),response.GetRequestId());

            to->SignalIn();
        }

    }

    //must be called in strand
    bool ResponseHandler::SendResponseInternal(const DistributionData& response)
    {
       lllout << "HandleResponse: " << Typesystem::Operations::GetName(response.GetTypeId()) << std::endl;

       const ConnectionId fromConnection=response.GetSenderId();
       const ConnectionId toConnection=response.GetReceiverId();

       if (toConnection.m_node == m_distribution.GetNodeId())
       {
           SendLocalResponse(response);
           return true; //Can't fail due to overflow.
       }

       ENSURE (fromConnection.m_node == m_distribution.GetNodeId(),
               << "RequestInQueue should only contain responses from this node!");

       //Response to another node
       lllout << "Sending the response to node " << toConnection.m_node << std::endl;

       Safir::Utilities::Internal::SharedConstCharArray responseP(response.GetReference(),
                                                 [](const char* data)
                                                 {
                                                     DistributionData::DropReference(data);
                                                 });

       //if the requestor node is gone we just discard the response
       const auto nodeType = m_liveNodes.find(toConnection.m_node);
       if (nodeType == m_liveNodes.end())
       {
           return true;
       }

       return m_distribution.GetCommunication().Send(toConnection.m_node,
                                                     nodeType->second,
                                                     responseP,
                                                     response.Size(),
                                                     m_dataTypeIdentifier,
                                                     true);
    }


}
}
}
