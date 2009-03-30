/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
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

#include "InjectionTimestampHandler.h"
#include <Safir/Dob/Connection.h>
#include <DoseTest/LastInjectionTimestamp.h>
#include <Safir/Dob/ErrorResponse.h>
#include <Safir/Dob/SuccessResponse.h>
#include <Safir/Dob/ResponseGeneralErrorCodes.h>
#include <iostream>

InjectionTimestampHandler::InjectionTimestampHandler():
    m_hasInstance(false)
{
    m_connection.Attach();
    m_connection.RegisterEntityHandlerInjection(DoseTest::LastInjectionTimestamp::ClassTypeId,
                                                Safir::Dob::Typesystem::HandlerId(),
                                                Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId,
                                                this);
}


void InjectionTimestampHandler::OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId     /*typeId*/,
                                                      const Safir::Dob::Typesystem::HandlerId& /*handlerId*/)
{
    std::wcout << "Someone overregistered my handler for LastInjectionTimestamp!" << std::endl;
    throw Safir::Dob::Typesystem::SoftwareViolationException(L"Someone overregistered my handler for LastInjectionTimestamp!",__WFILE__,__LINE__);
}

void InjectionTimestampHandler::OnCreateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                                Safir::Dob::ResponseSenderPtr        responseSender)
{
    responseSender->Send(Safir::Dob::ErrorResponse::CreateErrorResponse
        (Safir::Dob::ResponseGeneralErrorCodes::SafirReqErr(),
         L"Not possible to create instances of LastInjectionTimestamp"));
}

void InjectionTimestampHandler::OnUpdateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                                Safir::Dob::ResponseSenderPtr        responseSender)
{
    m_connection.SetChanges
        (entityRequestProxy.GetRequest(),
         Safir::Dob::Typesystem::InstanceId(DoseTest::LastInjectionTimestamp::ClassTypeId),
         Safir::Dob::Typesystem::HandlerId());
    responseSender->Send(Safir::Dob::SuccessResponse::Create());
}

void InjectionTimestampHandler::OnDeleteRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                                Safir::Dob::ResponseSenderPtr        responseSender)
{
    responseSender->Send(Safir::Dob::ErrorResponse::CreateErrorResponse
        (Safir::Dob::ResponseGeneralErrorCodes::SafirReqErr(),
         L"Not possible to delete instances of LastInjectionTimestamp"));
}

void InjectionTimestampHandler::OnInitialInjectionsDone(const Safir::Dob::Typesystem::TypeId /*typeId*/,
                                                        const Safir::Dob::Typesystem::HandlerId& /*handlerId*/)
{
    if (!m_hasInstance)
    {
        DoseTest::LastInjectionTimestampPtr ent = DoseTest::LastInjectionTimestamp::Create();
        ent->Timestamp() = 630720000000000LL;
        m_connection.SetAll
            (ent,
             Safir::Dob::Typesystem::InstanceId(DoseTest::LastInjectionTimestamp::ClassTypeId),
             Safir::Dob::Typesystem::HandlerId());
    }
}
