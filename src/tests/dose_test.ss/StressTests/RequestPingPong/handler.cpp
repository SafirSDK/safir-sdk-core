/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safir.sourceforge.net)
*
* Created by: Mikael Wennerberg/ stmiwn
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


#include "handler.h"
#include <DoseTest/SynchronousVolatileEntity.h>
#include <iostream>
#include <Safir/Dob/EntityIdResponse.h>
#include <Safir/Dob/ErrorResponse.h>
#include <Safir/Dob/SuccessResponse.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>

#include <Safir/Dob/Typesystem/Parameters.h>

void Handler::Start()
{

//    Safir::Dob::Typesystem::ObjectPtr op = Safir::Dob::Typesystem::Parameters::GetObject(111,1,1);


    m_Connection.Attach();

    m_Connection.RegisterEntityHandlerInjection(DoseTest::SynchronousVolatileEntity::ClassTypeId, 
        m_handlerId, Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId, this);

    lllog(1) << "Handler started" <<std::endl;
}

void Handler::OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId /*typeId*/, const Safir::Dob::Typesystem::HandlerId& /*handlerId*/)
{
    lllog(1) << "Handler::OnRevokedRegistration" <<std::endl;

    m_Connection.RegisterEntityHandlerInjection(DoseTest::SynchronousVolatileEntity::ClassTypeId, 
        m_handlerId, Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId, this);
}


void Handler::OnCompletedRegistration(const Safir::Dob::Typesystem::TypeId /*typeId*/, const Safir::Dob::Typesystem::HandlerId& /*handlerId*/)
{
    lllog(1) << "Handler::OnCompletedRegistration" <<std::endl;
}

void Handler::OnInitialInjectionsDone(const Safir::Dob::Typesystem::TypeId /*typeId*/, const Safir::Dob::Typesystem::HandlerId& /*handlerId*/)
{
    lllog(1) << "Handler::OnInitialInjectionsDone" <<std::endl;
}

void Handler::OnCreateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy, Safir::Dob::ResponseSenderPtr responseSender)
{
    const Safir::Dob::Typesystem::InstanceId id = Safir::Dob::Typesystem::InstanceId::GenerateRandom();
    const Safir::Dob::Typesystem::EntityId entityId = Safir::Dob::Typesystem::EntityId(DoseTest::SynchronousVolatileEntity::ClassTypeId, id);
    m_Connection.SetAll(entityRequestProxy.GetRequest(), id, m_handlerId);

    responseSender->Send(Safir::Dob::EntityIdResponse::CreateResponse(entityId));

    //    lllog(1) << "Handler::Created " <<  entityId << std::endl;

}


void Handler::OnUpdateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy, Safir::Dob::ResponseSenderPtr responseSender)
{
    responseSender->Send(Safir::Dob::ErrorResponse::Create());
}


void Handler::OnDeleteRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy, Safir::Dob::ResponseSenderPtr responseSender)
{
    m_Connection.Delete(entityRequestProxy.GetEntityId(), m_handlerId);
    responseSender->Send(Safir::Dob::SuccessResponse::Create());

    //    lllog(1) << "Handler::Deleted " <<  entityRequestProxy.GetEntityId() << std::endl;

}



