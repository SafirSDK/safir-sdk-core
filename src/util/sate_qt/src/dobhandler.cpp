/******************************************************************************
*
* Copyright Saab AB, 2024 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson
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
#include "dobhandler.h"

#include <QtConcurrent/QtConcurrent>
#include <Safir/Dob/NotOpenException.h>
#include <Safir/Dob/ConnectionAspectMisc.h>

#include <Safir/Dob/SuccessResponse.h>
#include <Safir/Dob/ErrorResponse.h>
#include <Safir/Dob/ResponseGeneralErrorCodes.h>

#include <QDebug>

namespace {
    QString Name(int64_t typeId)
    {
        return QString::fromStdWString(sdt::Operations::GetName(typeId));
    }

    template<class T>
    void Remove(int64_t typeId, std::vector<T>& vec)
    {
        vec.erase(std::remove_if(vec.begin(), vec.end(), [typeId](const auto& v){return v.typeId == typeId;}), vec.end());
    }

    template<class T>
    void Remove(int64_t typeId, int64_t id, std::vector<T>& vec)
    {
        vec.erase(std::remove_if(vec.begin(), vec.end(), [typeId, id](const auto& v){return v.typeId == typeId && v.id == id;}), vec.end());
    }
}
DobHandler::DobHandler() : m_dobConnection()
{
    // DOB signal handling
    connect(this, &DobHandler::DispatchSignal, this, [this]{
        m_dobConnection.Dispatch();
    });
}

void DobHandler::Open(const QString& name, int context)
{
    // Connect to DOB in another thread. Signals the ConnectedToDobSignal when done.
    auto dummy = QtConcurrent::run([this, name, context]
                                   {
                                       int instancePart = 0;
                                       while (true)
                                       {
                                           try
                                           {
                                               m_dobConnection.Open(name.toStdWString(), std::to_wstring(instancePart), context, this, this);
                                               break;
                                           }
                                           catch (const Safir::Dob::NotOpenException&)
                                           {
                                               ++instancePart;
                                           }
                                       }

                                       emit DobInterface::ConnectedToDob(QString::fromStdWString(Safir::Dob::ConnectionAspectMisc(this->m_dobConnection).GetConnectionName()));
                                   });

}


void DobHandler::Close()
{
    m_dobConnection.Close();
    emit DobInterface::ConnectionClosed();
}

void DobHandler::SubscribeMessage(int64_t typeId, const sdt::ChannelId& channel, bool includeSubclasses)
{
    m_dobConnection.SubscribeMessage(typeId, channel, includeSubclasses, this);
    DobInterface::SubscriptionInfo info{typeId, channel.GetRawValue(), includeSubclasses};
    m_subscriptions.push_back(info);
    emit DobInterface::SubscriptionStarted(info);
}

void DobHandler::UnsubscribeMessage(int64_t typeId)
{
    m_dobConnection.UnsubscribeMessage(typeId, Safir::Dob::Typesystem::ChannelId::ALL_CHANNELS, this);
    Remove(typeId, m_subscriptions);
    emit DobInterface::SubscriptionStopped(typeId);
}

void DobHandler::SubscribeEntity(int64_t typeId, const Safir::Dob::Typesystem::InstanceId &instance, bool includeSubclasses)
{
    if (instance == Safir::Dob::Typesystem::InstanceId())
    {
        m_dobConnection.SubscribeEntity(typeId, true, includeSubclasses, true, this);
    }
    else
    {
        m_dobConnection.SubscribeEntity(Safir::Dob::Typesystem::EntityId(typeId, instance), true, true, this);
    }
    DobInterface::SubscriptionInfo info{typeId, instance.GetRawValue(), includeSubclasses};
    m_subscriptions.push_back(info);
    emit DobInterface::SubscriptionStarted(info);
}

void DobHandler::UnsubscribeEntity(int64_t typeId)
{
    m_dobConnection.UnsubscribeEntity(typeId, this);
    Remove(typeId, m_subscriptions);
    emit DobInterface::SubscriptionStopped(typeId);
}

void DobHandler::SubscribeRegistrations(int64_t typeId, const Safir::Dob::Typesystem::HandlerId &handler, bool includeSubclasses)
{
    m_dobConnection.SubscribeRegistration(typeId, handler, includeSubclasses, true, this);
}

void DobHandler::UnsubscribeRegistrations(int64_t typeId)
{
    m_dobConnection.UnsubscribeEntity(typeId, this);
}

void DobHandler::RegisterEntityHandler(int64_t typeId, const Safir::Dob::Typesystem::HandlerId &handler, Safir::Dob::InstanceIdPolicy::Enumeration instanceIdPolicy, bool pending, bool injection)
{
    if (injection)
    {
        m_dobConnection.RegisterEntityHandlerInjection(typeId, handler, instanceIdPolicy, this);
    }
    else if (pending)
    {
        m_dobConnection.RegisterEntityHandlerPending(typeId, handler, instanceIdPolicy, this);
    }
    else
    {
        m_dobConnection.RegisterEntityHandler(typeId, handler, instanceIdPolicy, this);
    }

    DobInterface::RegistrationInfo info{typeId, handler.GetRawValue(), pending, injection, instanceIdPolicy};
    m_registrations.push_back(info);
    emit DobInterface::OnRegistered(info);
}

void DobHandler::RegisterServiceHandler(int64_t typeId, const Safir::Dob::Typesystem::HandlerId &handler, bool pending)
{
    if (pending)
    {
        m_dobConnection.RegisterServiceHandlerPending(typeId, handler, this);
    }
    else
    {
        m_dobConnection.RegisterServiceHandler(typeId, handler, this);
    }

    DobInterface::RegistrationInfo info{typeId, handler.GetRawValue(), false, false, Safir::Dob::InstanceIdPolicy::RequestorDecidesInstanceId};
    m_registrations.push_back(info);
    emit DobInterface::OnRegistered(info);
}

void DobHandler::Unregister(int64_t typeId)
{
    m_dobConnection.UnregisterHandler(typeId, Safir::Dob::Typesystem::HandlerId::ALL_HANDLERS);
    Remove(typeId, m_registrations);
    emit DobInterface::OnUnregistered(typeId);
}

void DobHandler::SendMessage(const Safir::Dob::MessagePtr &message, const Safir::Dob::Typesystem::ChannelId &channel)
{
    m_dobConnection.Send(message, channel, this);
}

void DobHandler::SendServiceRequest(const Safir::Dob::ServicePtr &request, const Safir::Dob::Typesystem::HandlerId &handler)
{
    m_dobConnection.ServiceRequest(request, handler, this);
}

void DobHandler::CreateRequest(const Safir::Dob::EntityPtr &entity, const Safir::Dob::Typesystem::InstanceId &instance, const Safir::Dob::Typesystem::HandlerId &handler)
{
    auto policy = m_dobConnection.GetInstanceIdPolicy(entity->GetTypeId(), handler);
    if (policy == Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId)
    {
        m_dobConnection.CreateRequest(entity, handler, this);
    }
    else
    {
        m_dobConnection.CreateRequest(entity, instance, handler, this);
    }
}

void DobHandler::UpdateRequest(const Safir::Dob::EntityPtr &entity, const Safir::Dob::Typesystem::InstanceId &instance)
{
    m_dobConnection.UpdateRequest(entity, instance, this);
}

void DobHandler::DeleteRequest(const Safir::Dob::Typesystem::EntityId &entityId)
{
    m_dobConnection.DeleteRequest(entityId, this);
}

void DobHandler::SetChanges(const Safir::Dob::EntityPtr &entity, const Safir::Dob::Typesystem::InstanceId &instance, const Safir::Dob::Typesystem::HandlerId &handler)
{
    m_dobConnection.SetChanges(entity, instance, handler);
}
void DobHandler::SetAll(const Safir::Dob::EntityPtr& entity, const sdt::InstanceId& instance, const sdt::HandlerId& handler)
{
    m_dobConnection.SetAll(entity, instance, handler);
}

void DobHandler::Delete(const Safir::Dob::Typesystem::EntityId &entityId, const Safir::Dob::Typesystem::HandlerId &handler)
{
    m_dobConnection.Delete(entityId, handler);
}

void DobHandler::DeleteAll(int64_t typeId, const Safir::Dob::Typesystem::HandlerId &handler)
{
    m_dobConnection.DeleteAllInstances(typeId, handler);
}

//------------------------------------------------------
// Dob consumer callbacks
//------------------------------------------------------
// Dispatcher interface
void DobHandler::OnDoDispatch()
{
    emit DispatchSignal();
}

// StopHandler interface
void DobHandler::OnStopOrder()
{
    Close();
}

// EntityRequestBase interface
void DobHandler::OnCreateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy, Safir::Dob::ResponseSenderPtr responseSender)
{
    auto typeId = entityRequestProxy.GetTypeId();
    auto handlerId = entityRequestProxy.GetReceivingHandlerId();
    auto it = std::find_if(m_registrations.begin(), m_registrations.end(), [typeId, handlerId](const auto& ri){return ri.typeId == typeId && ri.id == handlerId.GetRawValue();});
    auto instance = (it != m_registrations.end() && it->instanceIdPolicy == Safir::Dob::InstanceIdPolicy::RequestorDecidesInstanceId) ?
                        entityRequestProxy.GetInstanceId() : sdt::InstanceId::GenerateRandom();
    m_dobConnection.SetAll(entityRequestProxy.GetRequest(), instance, handlerId);
    emit DobInterface::OnRequest(entityRequestProxy.GetRequest(), DobInterface::CreateEntity);
    responseSender->Send(Safir::Dob::SuccessResponse::Create());
}

void DobHandler::OnUpdateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy, Safir::Dob::ResponseSenderPtr responseSender)
{
    m_dobConnection.SetChanges(entityRequestProxy.GetRequest(), entityRequestProxy.GetInstanceId(), entityRequestProxy.GetReceivingHandlerId());
    emit DobInterface::OnRequest(entityRequestProxy.GetRequest(), DobInterface::UpdateEntity);
    responseSender->Send(Safir::Dob::SuccessResponse::Create());
}

void DobHandler::OnDeleteRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy, Safir::Dob::ResponseSenderPtr responseSender)
{
    m_dobConnection.Delete(entityRequestProxy.GetEntityId(), entityRequestProxy.GetReceivingHandlerId());
    emit DobInterface::OnRequest(entityRequestProxy.GetRequest(), DobInterface::DeleteEntity);
    responseSender->Send(Safir::Dob::SuccessResponse::Create());
}

// RevokedRegistrationBase interface
void DobHandler::OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId typeId, const Safir::Dob::Typesystem::HandlerId &handlerId)
{
    emit DobInterface::Info("Revoked registration", QString("Registration of %1 was revoked.").arg(Name(typeId)), false);
    Remove(typeId, handlerId.GetRawValue(), m_registrations);
    emit DobInterface::OnUnregistered(typeId);
}

// EntityInjectionBase interface
void DobHandler::OnInjectedNewEntity(const Safir::Dob::InjectedEntityProxy injectedEntityProxy)
{
    emit DobInterface::Info("Injected new entity", QString::fromStdWString(injectedEntityProxy.GetEntityId().ToString()), false);
    emit DobInterface::OnEntity(injectedEntityProxy.GetEntityId(), sdt::HandlerId(), injectedEntityProxy.GetInjection(), DobInterface::NewEntity);
}

void DobHandler::OnInjectedUpdatedEntity(const Safir::Dob::InjectedEntityProxy injectedEntityProxy)
{
    emit DobInterface::Info("Injected updated entity", QString::fromStdWString(injectedEntityProxy.GetEntityId().ToString()), false);
    emit DobInterface::OnEntity(injectedEntityProxy.GetEntityId(), sdt::HandlerId(), injectedEntityProxy.GetInjection(), DobInterface::UpdatedEntity);
}

void DobHandler::OnInjectedDeletedEntity(const Safir::Dob::InjectedEntityProxy injectedEntityProxy)
{
    emit DobInterface::Info("Injected deleted entity", QString::fromStdWString(injectedEntityProxy.GetEntityId().ToString()), false);
    emit DobInterface::OnEntity(injectedEntityProxy.GetEntityId(), sdt::HandlerId(), nullptr, DobInterface::DeletedEntity);
}

void DobHandler::OnInitialInjectionsDone(const Safir::Dob::Typesystem::TypeId typeId, const Safir::Dob::Typesystem::HandlerId &/*handlerId*/)
{
    emit DobInterface::Info("Initial injection done", QString("Initial injection of %1 done.").arg(Name(typeId)), false);
}

// CompletedRegistrationBase interface
void DobHandler::OnCompletedRegistration(const Safir::Dob::Typesystem::TypeId typeId, const Safir::Dob::Typesystem::HandlerId &handlerId)
{
    DobInterface::RegistrationInfo info{typeId, handlerId.GetRawValue(), false, false, GetInstanceIdPolicy(typeId, handlerId)};
    emit DobInterface::OnRegistered(info);
    emit DobInterface::Info("Registration completed", QString("Registered handler of %1").arg(Name(typeId)), false);
}

// ServiceRequestBase interface
void DobHandler::OnServiceRequest(const Safir::Dob::ServiceRequestProxy serviceRequestProxy, Safir::Dob::ResponseSenderPtr responseSender)
{
    emit DobInterface::OnRequest(serviceRequestProxy.GetRequest(), DobInterface::Service);
    responseSender->Send(Safir::Dob::SuccessResponse::Create());
}

// Requestor interface
void DobHandler::OnResponse(const Safir::Dob::ResponseProxy responseProxy)
{
    emit DobInterface::OnResponse(responseProxy.GetResponse());
}

void DobHandler::OnNotRequestOverflow() {}

// MessageSender interface
void DobHandler::OnNotMessageOverflow() {}

// RegistrationSubscriber interface
void DobHandler::OnRegistered(const Safir::Dob::Typesystem::TypeId typeId, const Safir::Dob::Typesystem::HandlerId &handlerId)
{
    DobInterface::RegistrationInfo info{typeId, handlerId.GetRawValue(), false, false, Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId};
    emit DobInterface::OnRegistered(info);
}

void DobHandler::OnUnregistered(const Safir::Dob::Typesystem::TypeId typeId, const Safir::Dob::Typesystem::HandlerId &/*handlerId*/)
{
    emit DobInterface::OnUnregistered(typeId);
}

// MessageSubscriber interface
void DobHandler::OnMessage(const Safir::Dob::MessageProxy messageProxy)
{
    emit DobInterface::OnMessage(messageProxy.GetTypeId(), messageProxy.GetChannelIdWithStringRepresentation(), messageProxy.GetMessage());
}

// EntitySubscriber interface
void DobHandler::OnNewEntity(const Safir::Dob::EntityProxy entityProxy)
{
    emit DobInterface::OnEntity(entityProxy.GetEntityId(), entityProxy.GetOwner(), entityProxy.GetEntity(), DobInterface::NewEntity);
}
void DobHandler::OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy)
{
    emit DobInterface::OnEntity(entityProxy.GetEntityId(), entityProxy.GetOwner(), entityProxy.GetEntity(), DobInterface::UpdatedEntity);
}

void DobHandler::OnDeletedEntity(const Safir::Dob::EntityProxy entityProxy, const bool /*deprecated*/)
{    
    emit DobInterface::OnEntity(entityProxy.GetEntityId(), entityProxy.GetOwner(), nullptr, DobInterface::DeletedEntity);
}

Safir::Dob::InstanceIdPolicy::Enumeration DobHandler::GetInstanceIdPolicy(int64_t typeId, const sdt::HandlerId& handler) const
{
    auto id = handler.GetRawValue();
    auto it = std::find_if(m_registrations.begin(), m_registrations.end(), [typeId, id](const auto& v){return v.typeId == typeId && v.id == id;});
    return it != m_registrations.end() ? it->instanceIdPolicy : Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId;
}
