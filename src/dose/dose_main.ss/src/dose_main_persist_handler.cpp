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

#include "dose_main_persist_handler.h"

#include "dose_main_defs.h"
#include "dose_main_communication.h"
#include "dose_main_connection_handler.h"
#include "dose_main_node_handler.h"
#include <Safir/Dob/NodeParameters.h>
#include <Safir/Dob/PersistentDataReady.h>
#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Dob/SuccessResponse.h>
#include <Safir/Dob/ErrorResponse.h>
#include <Safir/Dob/Internal/EntityTypes.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include <Safir/Dob/ResponseGeneralErrorCodes.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    PersistHandler::PersistHandler():
        m_ecom(NULL),
        m_connectionHandler(NULL),
        m_nodeHandler(NULL),
        m_persistDataReady(false)
    {
        m_timerId = TimerHandler::Instance().RegisterTimeoutHandler(L"End States Timer", *this);

        if (SystemHasPersistence())
        {
            if (ThisNodeIsPersistanceNode())
            {
                std::wcout << "dose_main is waiting for persistence data!" << std::endl;
                lllout << "dose_main is waiting for persistence data!" << std::endl;
            }
            else
            {
                std::wcout << "dose_main is waiting for persistence data from node "
                    << Dob::NodeParameters::PersistentDbNode() << std::endl;

                lllout << "dose_main is waiting for persistence data from node "
                    << Dob::NodeParameters::PersistentDbNode() << std::endl;
            }
        }

    }

    void PersistHandler::Init(ExternNodeCommunication& ecom,
                              ConnectionHandler& connectionHandler,
                              NodeHandler& nodeHandler,
                              const bool otherNodesExistAtStartup)
    {
        m_ecom = &ecom;
        m_connectionHandler = &connectionHandler;
        m_nodeHandler = &nodeHandler;

        if (SystemHasPersistence() && ThisNodeIsPersistanceNode())
        {
            m_connection.Attach();

            // Register service
            m_connection.RegisterServiceHandler
                (Dob::PersistentDataReady::ClassTypeId,
                 Typesystem::HandlerId(),
                 this);
        }

        if (!SystemHasPersistence())
        {
            if(Dob::NodeParameters::PersistentDbNode() == -2)
            {
                std::wcout << "RUNNING IN PERSISTENCE TEST MODE! PLEASE CHANGE PARAMETER "
                    << "Safir.Dob.NodeParameters.PersistentDbNode IF THIS IS NOT WHAT YOU EXPECTED!" << std::endl;
            }
            else
            {
                EntityTypes::Instance().DisallowInitialSet();
            }
        }
        else if (otherNodesExistAtStartup)
        {
            //Get the statuses of the other nodes
            RequestPersistenceInfo();
        }
    }

    void PersistHandler::RequestPersistenceInfo()
    {
        DistributionData request
            (have_persistence_data_request_tag,
             ConnectionId(ThisNodeParameters::NodeNumber(),-1)); //dummy connection id, since it is a dose_main only thing.

        const bool result = m_ecom->Send(request);
        lllout << "Sent HavePersistanceDataRequest (send result = " << result << ")" << std::endl;

        m_waitingForResponsesFromNodes.clear();

        const NodeHandler::NodeStatuses ns = m_nodeHandler->GetNodeStatuses();
        for (NodeHandler::NodeStatuses::const_iterator it = ns.begin();
             it != ns.end(); ++it)
        {
            if (*it == NodeStatus::Started)
            {
                //aha! A node is up, then we know that it has persistence so
                //we can stop initial sets and allow -1 connects.
                lllout << "A node is up! So we disallow initial sets and let -1:s connect (node = "
                       << std::distance(ns.begin(),it) << ")" << std::endl;

                EntityTypes::Instance().DisallowInitialSet();
                Connections::Instance().AllowConnect(-1);
                m_waitingForResponsesFromNodes.clear();
                return; //no need to do anything else!
            }

            //if it is starting (i.e. NEW) we need to get its response.
            if (*it == NodeStatus::Starting)
            {
                lllout << "We must wait for response from node "
                       << std::distance(ns.begin(),it) << std::endl;
                m_waitingForResponsesFromNodes.insert(static_cast<Typesystem::Int32>(std::distance(ns.begin(),it)));
            }
        }
        TimerInfoPtr timerInfo(new EmptyTimerInfo(m_timerId));

        if (result)
        {
            //successful send, wait for responses for 100ms
            TimerHandler::Instance().Set(Discard,
                                         timerInfo,
                                         GetUtcTime() + 0.1); //time out in 100 milliseconds*/
        }
        else
        {
            //failed to send (dosecom overflow), retry in 10ms
            TimerHandler::Instance().Set(Discard,
                                         timerInfo,
                                         GetUtcTime() + 0.01); //time out in 10 milliseconds*/
        }
    }


    void PersistHandler::HandleMessageFromDoseCom(const DistributionData& data)
    {
        if (data.GetType() == DistributionData::Action_HavePersistenceDataRequest)
        {
            lllout << "Got an Action_HavePersistenceDataRequest, responding with " << m_persistDataReady << std::endl;
            DistributionData response
                (have_persistence_data_response_tag,
                 ConnectionId(ThisNodeParameters::NodeNumber(),-1), //dummy connection id, since it is a dose_main only thing.
                 m_persistDataReady);

            m_ecom->Send(response);

            //we ignore any overflow, since the other node will resend if it doesnt get the response.
        }
        else //DistributionData::Action_HavePersistenceDataResponse
        {
            lllout << "Got an Action_HavePersistenceDataResponse" << std::endl;

            if (data.GetIHavePersistenceData())
            {
                lllout << "  Node " << data.GetSenderId().m_node << " has persistence. Disallow initial set and let -1:s connect." << std::endl;
                //it has persistence so we can stop initial sets and allow -1 connects.
                EntityTypes::Instance().DisallowInitialSet();
                Connections::Instance().AllowConnect(-1);
                //cancel the timer
                TimerHandler::Instance().Remove(m_timerId);
                m_waitingForResponsesFromNodes.clear();
                return; //no need to do anything else!
            }
            else
            {
                lllout << "  Node " << data.GetSenderId().m_node << " doesnt have persistence." << std::endl;

                m_waitingForResponsesFromNodes.erase(data.GetSenderId().m_node);

                if (m_waitingForResponsesFromNodes.empty())
                {
                    lllout << "  Noone has persistence! Allow -1:s to connect (without disabling initialset)." << std::endl;
                    //everyone has responded saying that they have not seen persistence data
                    //allow initial sets to start.
                    Connections::Instance().AllowConnect(-1);
                    TimerHandler::Instance().Remove(m_timerId);
                    return; //no need to do anything else!
                }
            }
        }
    }

    void PersistHandler::HandleTimeout(const TimerInfoPtr & /*timer*/)
    {
        lllout << "Timeout! Calling RequestPersistenceInfo."  <<std::endl;
        RequestPersistenceInfo();
    }

    void PersistHandler::OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId    /*typeId*/,
                                               const Safir::Dob::Typesystem::HandlerId& /*handlerId*/)
    {
        lllerr << "Someone overregistered Safir::Dob::PersistentDataReady, so I'm not going to be able to allow other connections to connect!" << std::endl;
    }

    void PersistHandler::OnServiceRequest(const Safir::Dob::ServiceRequestProxy serviceRequestProxy,
                                          Safir::Dob::ResponseSenderPtr   responseSender)
    {
        if (!m_persistDataReady)
        {
            if (EntityTypes::Instance().IsInitialSetAllowed())
            {
                SetPersistentDataReady();

                lllout << "Calling SetOkToSignalPDComplete, since this node has now fulfilled the requirements for signalling PD complete (we got persistance data from local app)" << std::endl;
                m_ecom->SetOkToSignalPDComplete();
                m_connectionHandler->MaybeSignalConnectSemaphore();

                //disallow more persistence data
                EntityTypes::Instance().DisallowInitialSet();
            }

            // Generate a success response
            responseSender->Send(Dob::SuccessResponse::Create());
        }
        else
        {
            responseSender->Send(Dob::ErrorResponse::CreateErrorResponse
                (Safir::Dob::ResponseGeneralErrorCodes::SafirNoPermission(),
                 L"Persistence data has already been loaded! Do not run multiple persistence handlers!"));
        }

        //unregister so we can't be called again!
        //TODO: add this again when #193 is fixed
        //then we can get rid of the dual-calling above.
//        m_connection.UnregisterHandler(Dob::PersistentDataReady::ClassTypeId,
//                                       Typesystem::HandlerId());
    }


    bool PersistHandler::SystemHasPersistence()
    {
        return Dob::NodeParameters::PersistentDbNode() >= 0;
    }



    void PersistHandler::SetPersistentDataReady()
    {
        std::wcout << "dose_main persistence data is ready!" << std::endl;
        lllout << "dose_main persistence data is ready!" << std::endl;
        ENSURE(SystemHasPersistence(), << "This system does not have persistence, it is an error to call SetPersistentDataReady");

        m_persistDataReady = true;
    }

    bool PersistHandler::IsPersistentDataReady() const
    {
        ENSURE(SystemHasPersistence(), << "This system does not have persistence, it is an error to call IsPersistentDataReady");
        return m_persistDataReady;
    }

    bool PersistHandler::ThisNodeIsPersistanceNode()
    {
        ENSURE(SystemHasPersistence(), << "This system does not have persistence, it is an error to call ThisNodeIsPersistanceNode");
        return Dob::NodeParameters::PersistentDbNode() == Dob::ThisNodeParameters::NodeNumber();
    }
}
}
}
