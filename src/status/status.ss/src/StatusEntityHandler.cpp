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
#include "StatusEntityHandler.h"
#include <Safir/Dob/ErrorResponse.h>
#include <Safir/Control/Status.h>
#include <Safir/Dob/ResponseGeneralErrorCodes.h>
#include <Safir/Utilities/Internal/SystemLog.h>


namespace Safir
{
namespace Control
{
    StatusEntityHandler::StatusEntityHandler(boost::asio::io_service& ioService)
    {
        m_controlInfoReceiver.reset(
                    new Safir::Dob::Internal::Control::ControlInfoReceiver(ioService,
                                     [this](int64_t incarnationId, int64_t nodeId){SetValues(incarnationId,nodeId);} ));

    }

    void StatusEntityHandler::Start()
    {
        m_dobConnection.Attach();

        m_dobConnection.RegisterEntityHandler(Status::ClassTypeId,
                                                       Safir::Dob::Typesystem::HandlerId(),
                                                       Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId,
                                                       this);

        m_controlInfoReceiver->Start();
    }

    void StatusEntityHandler::Stop()
    {
        m_controlInfoReceiver->Stop();
    }

    void StatusEntityHandler::SetValues(int64_t incarnationId, int64_t nodeId)
    {
        StatusPtr status = Status::Create();

        status->NodeId().SetVal(nodeId);
        status->SystemIncarnation().SetVal(incarnationId);

        m_dobConnection.SetAll(status, Safir::Dob::Typesystem::InstanceId(0), Safir::Dob::Typesystem::HandlerId());
    }

    void StatusEntityHandler::OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId /*typeId*/,
                                                const Safir::Dob::Typesystem::HandlerId& /*handlerId*/)
    {
        SEND_SYSTEM_LOG(Error,
                        << "Someone overregistered Safir::Control::Status, so I'm not "
                        << "going to be able to update this entity any longer!");
    }


    void StatusEntityHandler::OnCreateRequest(const Safir::Dob::EntityRequestProxy /*entityRequestProxy*/,
                                          Safir::Dob::ResponseSenderPtr responseSender)
    {
        responseSender->Send(Safir::Dob::ErrorResponse::CreateErrorResponse
                             (Safir::Dob::ResponseGeneralErrorCodes::SafirReqErr(),
                              L"It is not possible to send create requests on Safir::Control::Status"));
    }

    void StatusEntityHandler::OnUpdateRequest(const Safir::Dob::EntityRequestProxy /*entityRequestProxy*/,
                                          Safir::Dob::ResponseSenderPtr responseSender)
    {
        responseSender->Send(Safir::Dob::ErrorResponse::CreateErrorResponse
                             (Safir::Dob::ResponseGeneralErrorCodes::SafirReqErr(),
                              L"It is not possible to send update requests on Safir::Control::Status"));
    }

    void StatusEntityHandler::OnDeleteRequest(const Safir::Dob::EntityRequestProxy /*entityRequestProxy*/,
                                          Safir::Dob::ResponseSenderPtr responseSender)
    {
        responseSender->Send(Safir::Dob::ErrorResponse::CreateErrorResponse
                             (Safir::Dob::ResponseGeneralErrorCodes::SafirReqErr(),
                              L"It is not possible to send delete requests on Safir::Control::Status"));
    }
}
}
