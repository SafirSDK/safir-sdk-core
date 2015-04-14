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

#include "ResponseHandler.h"

#if 0 //stewart
#include "dose_main_blocking_handler.h"
#endif
#include "dose_main_request_timers.h"
#include <Safir/Dob/Internal/Connection.h>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include "Distribution.h"

namespace Safir
{
namespace Dob
{
namespace Internal
{
    ResponseHandler::ResponseHandler(Distribution& distribution,
                                     const std::function<void(const ConnectionId& connectionId,
                                                              const InternalRequestId requestId)>& responsePostedCallback)
        : m_distribution(distribution)
        , m_responsePostedCallback(responsePostedCallback)
    {
    }

    void ResponseHandler::DispatchResponse(const DistributionData& response,
                                           bool & dontRemove,
                                           bool & doseComOverflowed,
                                           const ConnectionPtr & /*sender*/)
    {
        //if dosecom is overflowed and response is for remote node we skip it.
        if (doseComOverflowed && response.GetReceiverId().m_node != m_distribution.GetNodeId())
        {
            dontRemove = true;
            return;
        }

        //Try to send the response
        if (SendResponse(response))
        {
            dontRemove = false;
        }
        else
        {
#if 0 //stewart
            //This can only occur if response was for connection on another node, and DoseCom sendQ was full.
            m_blockingHandler->Response().AddWaitingConnection
                (ExternNodeCommunication::DoseComVirtualConnectionId,
                sender->Id().m_id);
            doseComOverflowed = true;
            dontRemove = true;
#endif
        }
    }

    void ResponseHandler::DispatchResponsesFromRequestInQueue(RequestInQueue & queue, const ConnectionPtr & sender)
    {
        bool doseComOverflowed = false;
        queue.DispatchResponses([this,sender,&doseComOverflowed](const DistributionData& response, bool& dontRemove)
                                {
                                    DispatchResponse(response,dontRemove,doseComOverflowed,sender);
                                });
    }

    void ResponseHandler::DistributeResponses(const ConnectionPtr & sender)
    {
        sender->ForEachRequestInQueue([this,sender](const ConsumerId& /*consumer*/, RequestInQueue& queue)
                                      {
                                          DispatchResponsesFromRequestInQueue(queue, sender);
                                      });
    }

    bool ResponseHandler::SendResponse(const DistributionData & response)
    {
       lllout << "HandleResponse: " << Typesystem::Operations::GetName(response.GetTypeId()) << std::endl;

       const ConnectionId fromConnection=response.GetSenderId();
       const ConnectionId toConnection=response.GetReceiverId();

       if (toConnection.m_node == m_distribution.GetNodeId())
       {
           //Can't fail due to overflow.
           const ConnectionPtr to = Connections::Instance().GetConnection(toConnection,std::nothrow);
           if (to != NULL) //if receiver of the response is dead, theres nothing to do
           {
               PostResponse(to, response);
           }
           return true;
       }
       else if (fromConnection.m_node == m_distribution.GetNodeId())
       {
#if 0 //stewart
           //Response to another node
           lllout << "Sending the response to node " << toConnection.m_node << std::endl;
           return m_ecom->Send(response);
#endif
       }

       return true;
    }

    void ResponseHandler::PostResponse(const ConnectionPtr & toConnection,
                                       const DistributionData & response)
    {
        toConnection->GetRequestOutQueue().AttachResponse(response);

        m_responsePostedCallback(response.GetReceiverId(),response.GetRequestId());

        toConnection->SignalIn();
    }

}
}
}
