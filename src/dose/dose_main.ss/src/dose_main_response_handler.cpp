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

#include "dose_main_response_handler.h"


#include "dose_main_blocking_handler.h"
#include "dose_main_communication.h"
#include "dose_main_request_timers.h"
#include <Safir/Dob/Internal/Connection.h>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    ResponseHandler::ResponseHandler()
        : m_thisNode(Dob::ThisNodeParameters::NodeNumber())
    {
    }

    ResponseHandler::~ResponseHandler()
    {
    }


    void ResponseHandler::Init(BlockingHandlers & blockingHandler, ExternNodeCommunication & ecom)
    {
        m_blockingHandler = &blockingHandler;
        m_ecom = &ecom;
    }

    void ResponseHandler::DispatchResponse(const DistributionData& response,
                                           bool & dontRemove,
                                           bool & doseComOverflowed,
                                           const ConnectionPtr & sender)
    {
        //if dosecom is overflowed and response is for remote node we skip it.
        if (doseComOverflowed && response.GetReceiverId().m_node != Dob::ThisNodeParameters::NodeNumber())
        {
            dontRemove = true;
            return;
        }

        //Try to send the response
        if (HandleResponse(response))
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
        queue.DispatchResponses(boost::bind(&ResponseHandler::DispatchResponse,this,_1,_2,boost::ref(doseComOverflowed),
            boost::cref(sender)));
    }

    void ResponseHandler::DistributeResponses(const ConnectionPtr & sender)
    {
        sender->ForEachRequestInQueue(boost::bind(&ResponseHandler::DispatchResponsesFromRequestInQueue,this,_2, boost::cref(sender)));
    }

    bool ResponseHandler::HandleResponse(const DistributionData & response)
    {
        lllout << "HandleResponse: " << Typesystem::Operations::GetName(response.GetTypeId()) << std::endl;

        const ConnectionId fromConnection=response.GetSenderId();
        const ConnectionId toConnection=response.GetReceiverId();

        if (toConnection.m_node == m_thisNode)
        {
            //Can't fail due to overflow.
            const ConnectionPtr to = Connections::Instance().GetConnection(toConnection,std::nothrow);
            if (to != NULL) //if receiver of the response is dead, theres nothing to do
            {
                PostResponse(to, response);
            }
            return true;
        }
        else if (fromConnection.m_node == m_thisNode)
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
        InternalRequestId reqId = response.GetRequestId();
        toConnection->GetRequestOutQueue().AttachResponse(response);

        //Create a timeoutinfo object so that we can remove the timer
        const RequestTimerInfo timeoutInfo = RequestTimerInfo(response.GetReceiverId().m_id, reqId);
        TimerHandler::Instance().Remove(TimerInfoPtr(new ReqTimer(RequestTimers::m_localReqTimerId,
                                                                  timeoutInfo)));
        toConnection->SignalIn();
    }

}
}
}
