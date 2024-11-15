/******************************************************************************
*
* Copyright Saab AB, 2024 (http://safirsdkcore.com)
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
#include "NodeInfoMirrorer.h"

#include <Safir/Dob/ConnectionAspectMisc.h>
#include <Safir/Dob/ErrorResponse.h>
#include <Safir/Dob/LowMemoryException.h>
#include <Safir/Dob/MirroredNodeInfo.h>
#include <Safir/Dob/NodeInfo.h>
#include <Safir/Dob/ResponseGeneralErrorCodes.h>
#include <Safir/Logging/Log.h>

namespace Safir
{
namespace Control
{

    NodeInfoMirrorer::NodeInfoMirrorer()
    {
    }

    void NodeInfoMirrorer::Start()
    {
        m_connection.Attach();

        Safir::Dob::ConnectionAspectMisc misc(m_connection);

        if (!misc.IsLightNode())
        {
            m_connection.RegisterEntityHandlerPending(Safir::Dob::MirroredNodeInfo::ClassTypeId,
                                                      Safir::Dob::Typesystem::HandlerId(),
                                                      Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId,
                                                      this);
        }
    }

    void NodeInfoMirrorer::Stop()
    {
        m_connection.UnsubscribeEntity(Safir::Dob::NodeInfo::ClassTypeId,this);
        m_connection.Detach();
    }

    void NodeInfoMirrorer::OnNewOrUpdatedEntity(const Safir::Dob::EntityProxy& entityProxy)
    {
        try
        {
            auto mirror = Safir::Dob::MirroredNodeInfo::Create();

            // Call the correct assignment operator by doing some magic casting.
            // This relies on MirroredNodeInfo inheriting from NodeInfo, and not adding
            // any members.
            static_cast<Safir::Dob::NodeInfo&>(*mirror) =
                static_cast<const Safir::Dob::NodeInfo&>(*entityProxy.GetEntity());

            m_connection.SetAll(mirror,
                                entityProxy.GetInstanceId(),
                                Safir::Dob::Typesystem::HandlerId());
        }
        catch (Safir::Dob::LowMemoryException&)
        {
            Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                          L"Failed to update or create MirroredNodeInfo instance due to low memory.");
        }
    }

    void NodeInfoMirrorer::OnNewEntity(const Safir::Dob::EntityProxy entityProxy)
    {
        OnNewOrUpdatedEntity(entityProxy);
    }

    void NodeInfoMirrorer::OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy)
    {
        OnNewOrUpdatedEntity(entityProxy);
    }

    void NodeInfoMirrorer::OnDeletedEntity(const Safir::Dob::EntityProxy entityProxy,
                                           const bool)
    {
        try
        {
            m_connection.Delete(Safir::Dob::Typesystem::EntityId(Safir::Dob::MirroredNodeInfo::ClassTypeId,
                                                                 entityProxy.GetInstanceId()),
                                Safir::Dob::Typesystem::HandlerId());
        }
        catch (Safir::Dob::LowMemoryException&)
        {
            Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                          L"Failed to delete MirroredNodeInfo instance due to low memory.");
        }
    }

    void NodeInfoMirrorer::OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId     /*typeId*/,
                                                 const Safir::Dob::Typesystem::HandlerId& /*handlerId*/)
    {
        m_connection.UnsubscribeEntity(Safir::Dob::NodeInfo::ClassTypeId,this);
    }

    void NodeInfoMirrorer::OnCompletedRegistration(const Safir::Dob::Typesystem::TypeId     /*typeId*/,
                                                   const Safir::Dob::Typesystem::HandlerId& /*handlerId*/)
    {

    }

    void NodeInfoMirrorer::OnInjectedNewEntity(const Safir::Dob::InjectedEntityProxy injectedEntityProxy)
    {
        //Delete any instances that do not exist rn.
        if (!m_connection.IsCreated(Safir::Dob::Typesystem::EntityId(Safir::Dob::NodeInfo::ClassTypeId,
                                                                     injectedEntityProxy.GetEntityId().GetInstanceId())))
        {
            m_connection.Delete(injectedEntityProxy.GetEntityId(), Safir::Dob::Typesystem::HandlerId());
        }
    }

    void NodeInfoMirrorer::OnInitialInjectionsDone(const Safir::Dob::Typesystem::TypeId /*typeId*/,
                                                   const Safir::Dob::Typesystem::HandlerId& /*handlerId*/)
    {
        m_connection.SubscribeEntity(Safir::Dob::NodeInfo::ClassTypeId,
                                     true,false,true,this);
    }


    void NodeInfoMirrorer::OnCreateRequest(const Safir::Dob::EntityRequestProxy /*entityRequestProx*/,
                                           Safir::Dob::ResponseSenderPtr        responseSender)
    {
        responseSender->Send(Safir::Dob::ErrorResponse::CreateErrorResponse
                             (Safir::Dob::ResponseGeneralErrorCodes::SafirReqErr(),
                              L"It is not possible to send create requests on Safir::Dob::MirroredNodeInfo"));
    }

    void NodeInfoMirrorer::OnUpdateRequest(const Safir::Dob::EntityRequestProxy /*entityRequestProxy*/,
                                           Safir::Dob::ResponseSenderPtr        responseSender)
    {
        responseSender->Send(Safir::Dob::ErrorResponse::CreateErrorResponse
                             (Safir::Dob::ResponseGeneralErrorCodes::SafirReqErr(),
                              L"It is not possible to send update requests on Safir::Dob::MirroredNodeInfo"));
    }

    void NodeInfoMirrorer::OnDeleteRequest(const Safir::Dob::EntityRequestProxy /*entityRequestProxy*/,
                                           Safir::Dob::ResponseSenderPtr        responseSender)
    {
        responseSender->Send(Safir::Dob::ErrorResponse::CreateErrorResponse
                             (Safir::Dob::ResponseGeneralErrorCodes::SafirReqErr(),
                              L"It is not possible to send delete requests on Safir::Dob::MirroredNodeInfo"));
    }



}
}
