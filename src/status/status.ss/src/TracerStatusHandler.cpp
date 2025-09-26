/******************************************************************************
*
* Copyright Saab AB, 2025 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / lars@foldspace.nu
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
#include "TracerStatusHandler.h"

#include <Safir/Dob/ConnectionAspectMisc.h>
#include <Safir/Dob/ErrorResponse.h>
#include <Safir/Dob/LowMemoryException.h>
#include <Safir/Application/TracerStatus.h>
#include <Safir/Dob/NodeInfo.h>
#include <Safir/Dob/ResponseGeneralErrorCodes.h>
#include <Safir/Logging/Log.h>

namespace Safir
{
namespace Control
{

    TracerStatusHandler::TracerStatusHandler()
    {
    }

    void TracerStatusHandler::Start()
    {
        m_connection.Attach();

        Safir::Dob::ConnectionAspectMisc misc(m_connection);

        if (!misc.IsLightNode())
        {
            m_connection.RegisterEntityHandlerPending(Safir::Application::TracerStatus::ClassTypeId,
                                                      Safir::Dob::Typesystem::HandlerId(),
                                                      Safir::Dob::InstanceIdPolicy::RequestorDecidesInstanceId,
                                                      this);
        }
    }

    void TracerStatusHandler::Stop()
    {
        m_connection.Detach();
    }

    void TracerStatusHandler::OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId     /*typeId*/,
                                                    const Safir::Dob::Typesystem::HandlerId& /*handlerId*/)
    {

    }

    void TracerStatusHandler::OnCompletedRegistration(const Safir::Dob::Typesystem::TypeId     /*typeId*/,
                                                      const Safir::Dob::Typesystem::HandlerId& /*handlerId*/)
    {

    }

    void TracerStatusHandler::OnInjectedNewEntity(const Safir::Dob::InjectedEntityProxy /*injectedEntityProxy*/)
    {

    }

    void TracerStatusHandler::OnInitialInjectionsDone(const Safir::Dob::Typesystem::TypeId /*typeId*/,
                                                      const Safir::Dob::Typesystem::HandlerId& /*handlerId*/)
    {

    }


    void TracerStatusHandler::OnCreateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                              Safir::Dob::ResponseSenderPtr        responseSender)
    {
        m_connection.SetChanges(entityRequestProxy.GetRequest(),
                                entityRequestProxy.GetInstanceId(),
                                entityRequestProxy.GetReceivingHandlerId());
        responseSender->Send(Safir::Dob::SuccessResponse::Create());
    }

    void TracerStatusHandler::OnUpdateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                           Safir::Dob::ResponseSenderPtr        responseSender)
    {
        m_connection.SetChanges(entityRequestProxy.GetRequest(),
                                entityRequestProxy.GetInstanceId(),
                                entityRequestProxy.GetReceivingHandlerId());
        responseSender->Send(Safir::Dob::SuccessResponse::Create());
    }

    void TracerStatusHandler::OnDeleteRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                              Safir::Dob::ResponseSenderPtr        responseSender)
    {
        m_connection.Delete(entityRequestProxy.GetEntityId(),
                            entityRequestProxy.GetReceivingHandlerId());
        responseSender->Send(Safir::Dob::SuccessResponse::Create());
    }



}
}
