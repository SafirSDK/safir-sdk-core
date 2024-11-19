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
#include <Safir/Dob/Typesystem/Exceptions.h>

#include <QDebug>

namespace {
    QString Str(int64_t typeId) {return QString::fromStdWString(sdt::Operations::GetName(typeId));}
    QString Str(const std::wstring& s) { return QString::fromStdWString(s);}
    QString Str(const sdt::HandlerId& v) { return QString(", handler=%1").arg(Str(v.ToString())); }
    QString Str(const sdt::ChannelId& v) { return QString(", channel=%1").arg(Str(v.ToString())); }
    QString Str(const sdt::InstanceId& v) { return QString(", instance=%1").arg(Str(v.ToString())); }
    QString Str(const sdt::EntityId& v) { return Str(v.ToString()); }
}
DobHandler::DobHandler() : m_dobConnection()
{
    // DOB signal handling
    connect(this, &DobHandler::DispatchSignal, this, [this]{ m_dobConnection.Dispatch(); });
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

                                       emit DobInterface::ConnectedToDob(Str(Safir::Dob::ConnectionAspectMisc(this->m_dobConnection).GetConnectionName()));
                                       emit DobInterface::Info("<b>Connected to DOB!</b>");
                                   });

}


void DobHandler::Close()
{
    try
    {
        m_dobConnection.Close();
        emit DobInterface::ConnectionClosed();
    }
    catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase& e)
    {
        emit DobInterface::Info(QString("<span style='color:red'>%1</span>").arg(Str(e.GetMessage())));
    }
}

void DobHandler::SubscribeMessage(int64_t typeId, const sdt::ChannelId& channel, bool includeSubclasses)
{
    try
    {
        m_dobConnection.SubscribeMessage(typeId, channel, includeSubclasses, this);
        DobInterface::SubscriptionInfo info{typeId, channel, includeSubclasses};
        m_subscriptions.push_back(info);
        emit DobInterface::SubscriptionStarted(info);
        emit DobInterface::Info("Subscription message: " + Str(typeId) + Str(channel));
    }
    catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase& e)
    {
        emit DobInterface::Info(QString("<span style='color:red'>%1</span>").arg(Str(e.GetMessage())));
    }
}

void DobHandler::UnsubscribeMessage(int64_t typeId)
{
    try
    {
        m_dobConnection.UnsubscribeMessage(typeId, Safir::Dob::Typesystem::ChannelId::ALL_CHANNELS, this);
        RemoveSubscriptions(typeId);
        emit DobInterface::SubscriptionStopped(typeId);
        emit DobInterface::Info("Unsubscribe message: " + Str(typeId));
    }
    catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase& e)
    {
        emit DobInterface::Info(QString("<span style='color:red'>%1</span>").arg(Str(e.GetMessage())));
    }
}

void DobHandler::SubscribeEntity(int64_t typeId, const Safir::Dob::Typesystem::InstanceId &instance, bool includeSubclasses)
{
    try
    {
        if (instance == Safir::Dob::Typesystem::InstanceId())
        {
            m_dobConnection.SubscribeEntity(typeId, true, includeSubclasses, true, this);
            emit DobInterface::Info("Subscription entity: " + Str(typeId));
        }
        else
        {
            Safir::Dob::Typesystem::EntityId eid(typeId, instance);
            m_dobConnection.SubscribeEntity(eid, true, true, this);
            emit DobInterface::Info("Subscription entity: " + Str(eid.ToString()));
        }
        DobInterface::SubscriptionInfo info{typeId, sdt::ChannelId(), includeSubclasses};
        m_subscriptions.push_back(info);
        emit DobInterface::SubscriptionStarted(info);

    }
    catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase& e)
    {
        emit DobInterface::Info(QString("<span style='color:red'>%1<span>").arg(Str(e.GetMessage())));
    }
}

void DobHandler::UnsubscribeEntity(int64_t typeId)
{
    try
    {
        m_dobConnection.UnsubscribeEntity(typeId, this);
        RemoveSubscriptions(typeId);
        emit DobInterface::SubscriptionStopped(typeId);
        emit DobInterface::Info("Unsubscribe entity: " + Str(typeId));
    }
    catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase& e)
    {
        emit DobInterface::Info(QString("<span style='color:red'>%1<span>").arg(Str(e.GetMessage())));
    }
}

void DobHandler::SubscribeRegistrations(int64_t typeId, const Safir::Dob::Typesystem::HandlerId &handler, bool includeSubclasses)
{
    try
    {
        m_dobConnection.SubscribeRegistration(typeId, handler, includeSubclasses, true, this);
        emit DobInterface::Info("Subscribe for registrations: " + Str(typeId) + Str(handler));

    }
    catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase& e)
    {
        emit DobInterface::Info(QString("<span style='color:red'>%1<span>").arg(Str(e.GetMessage())));
    }
}

void DobHandler::UnsubscribeRegistrations(int64_t typeId)
{
    try
    {
        m_dobConnection.UnsubscribeRegistration(typeId, sdt::HandlerId::ALL_HANDLERS, true, this);
        emit DobInterface::Info("Unsubscribe registrations: " + Str(typeId));
    }
    catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase& e)
    {
        emit DobInterface::Info(QString("<span style='color:red'>%1<span>").arg(Str(e.GetMessage())));
    }
}

void DobHandler::RegisterEntityHandler(int64_t typeId, const Safir::Dob::Typesystem::HandlerId &handler, Safir::Dob::InstanceIdPolicy::Enumeration instanceIdPolicy, bool pending, bool injection)
{
    try
    {
        if (injection)
        {
            m_dobConnection.RegisterEntityHandlerInjection(typeId, handler, instanceIdPolicy, this);
            emit DobInterface::Info("Register injection entity handler: " + Str(typeId) + Str(handler));
        }
        else if (pending)
        {
            m_dobConnection.RegisterEntityHandlerPending(typeId, handler, instanceIdPolicy, this);
            emit DobInterface::Info("Register pending entity handler: " + Str(typeId) + Str(handler));
        }
        else
        {
            m_dobConnection.RegisterEntityHandler(typeId, handler, instanceIdPolicy, this);
            emit DobInterface::Info("Register entity handler: " + Str(typeId) + Str(handler));
        }

        DobInterface::RegistrationInfo info{typeId, handler, pending, injection, instanceIdPolicy};
        m_registrations.push_back(info);
        emit DobInterface::OnRegistered(info);
    }
    catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase& e)
    {
        emit DobInterface::Info(QString("<span style='color:red'>%1<span>").arg(Str(e.GetMessage())));
    }
}

void DobHandler::RegisterServiceHandler(int64_t typeId, const Safir::Dob::Typesystem::HandlerId &handler, bool pending)
{
    try
    {
        if (pending)
        {
            m_dobConnection.RegisterServiceHandlerPending(typeId, handler, this);
            emit DobInterface::Info("Register pending service handler: " + Str(typeId) + Str(handler));
        }
        else
        {
            m_dobConnection.RegisterServiceHandler(typeId, handler, this);
            emit DobInterface::Info("Register service handler: " + Str(typeId) + Str(handler));
        }

        DobInterface::RegistrationInfo info{typeId, handler, false, false, Safir::Dob::InstanceIdPolicy::RequestorDecidesInstanceId};
        m_registrations.push_back(info);
        emit DobInterface::OnRegistered(info);
    }
    catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase& e)
    {
        emit DobInterface::Info(QString("<span style='color:red'>%1<span>").arg(Str(e.GetMessage())));
    }
}

void DobHandler::Unregister(int64_t typeId)
{
    try
    {
        m_dobConnection.UnregisterHandler(typeId, Safir::Dob::Typesystem::HandlerId::ALL_HANDLERS);
        RemoveRegistrations(typeId);
        emit DobInterface::OnUnregistered(typeId);
        emit DobInterface::Info("Unregister handler: " + Str(typeId));
    }
    catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase& e)
    {
        emit DobInterface::Info(QString("<span style='color:red'>%1<span>").arg(Str(e.GetMessage())));
    }
}

void DobHandler::SendMessage(const Safir::Dob::MessagePtr &message, const Safir::Dob::Typesystem::ChannelId &channel)
{
    try
    {
        m_dobConnection.Send(message, channel, this);
        emit DobInterface::Info("Send message: " + Str(message->GetTypeId()) + Str(channel));
    }
    catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase& e)
    {
        emit DobInterface::Info(QString("<span style='color:red'>%1<span>").arg(Str(e.GetMessage())));
    }
}

void DobHandler::SendServiceRequest(const Safir::Dob::ServicePtr &request, const Safir::Dob::Typesystem::HandlerId &handler)
{
    try
    {
        m_dobConnection.ServiceRequest(request, handler, this);
        emit DobInterface::Info("Send service request: " + Str(request->GetTypeId()) + Str(handler));
    }
    catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase& e)
    {
        emit DobInterface::Info(QString("<span style='color:red'>%1<span>").arg(Str(e.GetMessage())));
    }
}

void DobHandler::CreateRequest(const Safir::Dob::EntityPtr &entity, const Safir::Dob::Typesystem::InstanceId &instance, const Safir::Dob::Typesystem::HandlerId &handler)
{
    try
    {
        auto policy = m_dobConnection.GetInstanceIdPolicy(entity->GetTypeId(), handler);
        if (policy == Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId)
        {
            m_dobConnection.CreateRequest(entity, handler, this);
            emit DobInterface::Info("Send create request (HandlerDecidesInstanceId): " + Str(entity->GetTypeId()) + Str(handler));
        }
        else
        {
            m_dobConnection.CreateRequest(entity, instance, handler, this);
            emit DobInterface::Info("Send create request (RequestorDecidesInstanceId): " + Str(entity->GetTypeId()) + Str(handler));
        }
    }
    catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase& e)
    {
        emit DobInterface::Info(QString("<span style='color:red'>%1<span>").arg(Str(e.GetMessage())));
    }
}

void DobHandler::UpdateRequest(const Safir::Dob::EntityPtr &entity, const Safir::Dob::Typesystem::InstanceId &instance)
{
    try
    {
        m_dobConnection.UpdateRequest(entity, instance, this);
        emit DobInterface::Info("Send update request: " + Str(entity->GetTypeId()) + Str(instance));
    }
    catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase& e)
    {
        emit DobInterface::Info(QString("<span style='color:red'>%1<span>").arg(Str(e.GetMessage())));
    }
}

void DobHandler::DeleteRequest(const Safir::Dob::Typesystem::EntityId &entityId)
{
    try
    {
        m_dobConnection.DeleteRequest(entityId, this);
        emit DobInterface::Info("Send delete request: " + Str(entityId));
    }
    catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase& e)
    {
        emit DobInterface::Info(QString("<span style='color:red'>%1<span>").arg(Str(e.GetMessage())));
    }
}

void DobHandler::SetChanges(const Safir::Dob::EntityPtr &entity, const Safir::Dob::Typesystem::InstanceId &instance, const Safir::Dob::Typesystem::HandlerId &handler)
{
    try
    {
        m_dobConnection.SetChanges(entity, instance, handler);
        emit DobInterface::Info("Set changes: " + Str(entity->GetTypeId()) + Str(instance) + Str(handler));
    }
    catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase& e)
    {
        emit DobInterface::Info(QString("<span style='color:red'>%1<span>").arg(Str(e.GetMessage())));
    }
}

void DobHandler::SetAll(const Safir::Dob::EntityPtr& entity, const sdt::InstanceId& instance, const sdt::HandlerId& handler)
{
    try
    {
        m_dobConnection.SetAll(entity, instance, handler);
        emit DobInterface::Info("Set all: " + Str(entity->GetTypeId()) + Str(instance) + Str(handler));
    }
    catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase& e)
    {
        emit DobInterface::Info(QString("<span style='color:red'>%1<span>").arg(Str(e.GetMessage())));
    }
}

void DobHandler::Delete(const Safir::Dob::Typesystem::EntityId &entityId, const Safir::Dob::Typesystem::HandlerId &handler)
{
    try
    {
        m_dobConnection.Delete(entityId, handler);
        emit DobInterface::Info("Delete entity: " + Str(entityId) + Str(handler));
    }
    catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase& e)
    {
        emit DobInterface::Info(QString("<span style='color:red'>%1<span>").arg(Str(e.GetMessage())));
    }
}

void DobHandler::DeleteAll(int64_t typeId, const Safir::Dob::Typesystem::HandlerId &handler)
{
    try
    {
        m_dobConnection.DeleteAllInstances(typeId, handler);
        emit DobInterface::Info("Delete entity all instances: " + Str(typeId) + Str(handler));
    }
    catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase& e)
    {
        emit DobInterface::Info(QString("<span style='color:red'>%1<span>").arg(Str(e.GetMessage())));
    }
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
    emit DobInterface::Info("Got stop order, close connection");
    Close();
}

// EntityRequestBase interface
void DobHandler::OnCreateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy, Safir::Dob::ResponseSenderPtr responseSender)
{
    auto typeId = entityRequestProxy.GetTypeId();
    auto handlerId = entityRequestProxy.GetReceivingHandlerId();
    auto it = std::find_if(m_registrations.begin(), m_registrations.end(), [typeId, handlerId](const auto& ri){return ri.typeId == typeId && ri.handler.GetRawValue() == handlerId.GetRawValue();});
    auto instance = (it != m_registrations.end() && it->instanceIdPolicy == Safir::Dob::InstanceIdPolicy::RequestorDecidesInstanceId) ?
                        entityRequestProxy.GetInstanceId() : sdt::InstanceId::GenerateRandom();

    emit DobInterface::Info("Received create request: " + Str(typeId) + Str(handlerId));
    m_dobConnection.SetAll(entityRequestProxy.GetRequest(), instance, handlerId);
    emit DobInterface::OnRequest(entityRequestProxy.GetRequest(), DobInterface::CreateEntity);
    responseSender->Send(Safir::Dob::SuccessResponse::Create());
}

void DobHandler::OnUpdateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy, Safir::Dob::ResponseSenderPtr responseSender)
{
    emit DobInterface::Info("Received update request: " + Str(entityRequestProxy.GetEntityId()));
    m_dobConnection.SetChanges(entityRequestProxy.GetRequest(), entityRequestProxy.GetInstanceId(), entityRequestProxy.GetReceivingHandlerId());
    emit DobInterface::OnRequest(entityRequestProxy.GetRequest(), DobInterface::UpdateEntity);
    responseSender->Send(Safir::Dob::SuccessResponse::Create());
}

void DobHandler::OnDeleteRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy, Safir::Dob::ResponseSenderPtr responseSender)
{
    emit DobInterface::Info("Received delete request: " + Str(entityRequestProxy.GetEntityId()));
    m_dobConnection.Delete(entityRequestProxy.GetEntityId(), entityRequestProxy.GetReceivingHandlerId());
    emit DobInterface::OnRequest(entityRequestProxy.GetRequest(), DobInterface::DeleteEntity);
    responseSender->Send(Safir::Dob::SuccessResponse::Create());
}

// RevokedRegistrationBase interface
void DobHandler::OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId typeId, const Safir::Dob::Typesystem::HandlerId &handlerId)
{
    emit DobInterface::Info("Revoked registration: " + Str(typeId) + Str(handlerId));
    RemoveRegistration(typeId, handlerId);
    emit DobInterface::OnUnregistered(typeId);
}

// EntityInjectionBase interface
void DobHandler::OnInjectedNewEntity(const Safir::Dob::InjectedEntityProxy injectedEntityProxy)
{
    emit DobInterface::Info("Injected new entity: " + Str(injectedEntityProxy.GetEntityId()));
    emit DobInterface::OnEntity(injectedEntityProxy.GetEntityId(), sdt::HandlerId(), injectedEntityProxy.GetInjection(), DobInterface::NewEntity);
}

void DobHandler::OnInjectedUpdatedEntity(const Safir::Dob::InjectedEntityProxy injectedEntityProxy)
{
    emit DobInterface::Info("Injected updated entity: " + Str(injectedEntityProxy.GetEntityId()));
    emit DobInterface::OnEntity(injectedEntityProxy.GetEntityId(), sdt::HandlerId(), injectedEntityProxy.GetInjection(), DobInterface::UpdatedEntity);
}

void DobHandler::OnInjectedDeletedEntity(const Safir::Dob::InjectedEntityProxy injectedEntityProxy)
{
    emit DobInterface::Info("Injected deleted entity: " + Str(injectedEntityProxy.GetEntityId()));
    emit DobInterface::OnEntity(injectedEntityProxy.GetEntityId(), sdt::HandlerId(), nullptr, DobInterface::DeletedEntity);
}

void DobHandler::OnInitialInjectionsDone(const Safir::Dob::Typesystem::TypeId typeId, const Safir::Dob::Typesystem::HandlerId &handlerId)
{
    emit DobInterface::Info("Initial injection done: " + Str(typeId) + Str(handlerId)) ;
}

// CompletedRegistrationBase interface
void DobHandler::OnCompletedRegistration(const Safir::Dob::Typesystem::TypeId typeId, const Safir::Dob::Typesystem::HandlerId &handlerId)
{
    emit DobInterface::Info("Registration completed: " + Str(typeId) + Str(handlerId));
    DobInterface::RegistrationInfo info{typeId, handlerId, false, false, GetInstanceIdPolicy(typeId, handlerId)};
    emit DobInterface::OnRegistered(info);
}

// ServiceRequestBase interface
void DobHandler::OnServiceRequest(const Safir::Dob::ServiceRequestProxy serviceRequestProxy, Safir::Dob::ResponseSenderPtr responseSender)
{
    emit DobInterface::Info("Received service request: " + Str(serviceRequestProxy.GetTypeId()));
    emit DobInterface::OnRequest(serviceRequestProxy.GetRequest(), DobInterface::Service);
    responseSender->Send(Safir::Dob::SuccessResponse::Create());
}

// Requestor interface
void DobHandler::OnResponse(const Safir::Dob::ResponseProxy responseProxy)
{
    emit DobInterface::Info("Received response: " + Str(responseProxy.GetTypeId()));
    emit DobInterface::OnResponse(responseProxy.GetResponse());
}

void DobHandler::OnNotRequestOverflow()
{
    emit DobInterface::Info("OnNotRequestOverflow");
}

// MessageSender interface
void DobHandler::OnNotMessageOverflow()
{
    emit DobInterface::Info("OnNotMessageOverflow");
}

// RegistrationSubscriber interface
void DobHandler::OnRegistered(const Safir::Dob::Typesystem::TypeId typeId, const Safir::Dob::Typesystem::HandlerId &handlerId)
{
    emit DobInterface::Info("OnRegistered: " + Str(typeId) + Str(handlerId));
    DobInterface::RegistrationInfo info{typeId, handlerId, false, false, Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId};
    emit DobInterface::OnRegistered(info);
}

void DobHandler::OnUnregistered(const Safir::Dob::Typesystem::TypeId typeId, const Safir::Dob::Typesystem::HandlerId &handlerId)
{
    emit DobInterface::Info("OnUnregistered: " + Str(typeId) + Str(handlerId));
    emit DobInterface::OnUnregistered(typeId);
}

// MessageSubscriber interface
void DobHandler::OnMessage(const Safir::Dob::MessageProxy messageProxy)
{
    emit DobInterface::Info("Received message: " + Str(messageProxy.GetTypeId()));
    emit DobInterface::OnMessage(messageProxy.GetTypeId(), messageProxy.GetChannelIdWithStringRepresentation(), messageProxy.GetMessage());
}

// EntitySubscriber interface
void DobHandler::OnNewEntity(const Safir::Dob::EntityProxy entityProxy)
{
    emit DobInterface::Info("Received new entity: " + Str(entityProxy.GetEntityId()));
    emit DobInterface::OnEntity(entityProxy.GetEntityId(), entityProxy.GetOwner(), entityProxy.GetEntity(), DobInterface::NewEntity);
}
void DobHandler::OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy)
{
    emit DobInterface::Info("Received updated entity: " + Str(entityProxy.GetEntityId()));
    emit DobInterface::OnEntity(entityProxy.GetEntityId(), entityProxy.GetOwner(), entityProxy.GetEntity(), DobInterface::UpdatedEntity);
}

void DobHandler::OnDeletedEntity(const Safir::Dob::EntityProxy entityProxy, const bool /*deprecated*/)
{
    emit DobInterface::Info("Received deleted entity: " + Str(entityProxy.GetEntityId()));
    emit DobInterface::OnEntity(entityProxy.GetEntityId(), entityProxy.GetOwner(), nullptr, DobInterface::DeletedEntity);
}

Safir::Dob::InstanceIdPolicy::Enumeration DobHandler::GetInstanceIdPolicy(int64_t typeId, const sdt::HandlerId& handler) const
{
    auto id = handler.GetRawValue();
    auto it = std::find_if(m_registrations.begin(), m_registrations.end(), [typeId, id](const auto& v){return v.typeId == typeId && v.handler.GetRawValue() == id;});
    return it != m_registrations.end() ? it->instanceIdPolicy : Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId;
}
