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
#include "NodeInfoHandler.h"
#include <Safir/Dob/ErrorResponse.h>
#include <Safir/Dob/NodeInfo.h>
#include <Safir/Dob/ResponseGeneralErrorCodes.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <boost/asio.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    NodeInfoHandler::NodeInfoHandler(boost::asio::io_service& ioService,
                                     const Distribution& distribution)
        : m_dispatcher(m_connection, ioService),
          m_distribution(distribution)
    {
        m_dispatcher.Strand().post([this]
        {
            m_connection.Open(L"DoseMain",  // Note the name. We want this to be handled as a normal connection.
                              L"NodeInfoHandler",
                              0,
                              nullptr,
                              &m_dispatcher);

            m_connection.RegisterEntityHandler(Dob::NodeInfo::ClassTypeId,
                                               Typesystem::HandlerId(m_distribution.GetNodeId()),
                                               Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId,
                                               this);

            Dob::NodeInfoPtr ni = Dob::NodeInfo::Create();

            ni->NodeName().SetVal(ThisNodeParameters::Name());

            //extract the address from data we get from Communication.
            //The value we get from DataAddress ends with :port, so
            //we need to strip that.
            const auto address = m_distribution.GetCommunication().DataAddress();
            const auto ip = address.substr(0,address.find_last_of(":"));
            ni->IpAddress().SetVal(Safir::Dob::Typesystem::Utilities::ToWstring(ip));

            ni->NodeType().SetVal(ThisNodeParameters::NodeType());

            m_connection.SetAll(ni,
                                Typesystem::InstanceId(m_distribution.GetNodeId()),
                                Typesystem::HandlerId(m_distribution.GetNodeId()));
        });
    }

    void NodeInfoHandler::Stop()
    {
        m_dispatcher.Strand().dispatch([this]
        {
            m_connection.Close();
        });
    }


    void NodeInfoHandler::OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId    /*typeId*/,
                                                const Safir::Dob::Typesystem::HandlerId& /*handlerId*/)
    {
        SEND_SYSTEM_LOG(Error,
                        << "Someone overregistered Safir::Dob::NodeStatus, so I'm not "
                        << "going to be able to update this entity any longer!");
    }


    void NodeInfoHandler::OnCreateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                          Safir::Dob::ResponseSenderPtr    responseSender)
    {
        responseSender->Send(Safir::Dob::ErrorResponse::CreateErrorResponse
                             (Safir::Dob::ResponseGeneralErrorCodes::SafirReqErr(),
             L"It is not possible to send create requests on Safir::Dob::NodeStatus"));
    }

    void NodeInfoHandler::OnUpdateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                          Safir::Dob::ResponseSenderPtr    responseSender)
    {
        responseSender->Send(Safir::Dob::ErrorResponse::CreateErrorResponse
                             (Safir::Dob::ResponseGeneralErrorCodes::SafirReqErr(),
                              L"It is not possible to send update requests on Safir::Dob::NodeStatus"));
    }

    void NodeInfoHandler::OnDeleteRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                          Safir::Dob::ResponseSenderPtr    responseSender)
    {
        responseSender->Send(Safir::Dob::ErrorResponse::CreateErrorResponse
                             (Safir::Dob::ResponseGeneralErrorCodes::SafirReqErr(),
                              L"It is not possible to send delete requests on Safir::Dob::NodeStatus"));
    }
}
}
}
