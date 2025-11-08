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
#include "dobnative.h"

#include <QtConcurrent/QtConcurrent>
#include <Safir/Dob/NotOpenException.h>
#include <Safir/Dob/NotFoundException.h>
#include <Safir/Dob/ConnectionAspectMisc.h>

#include <Safir/Dob/ErrorResponse.h>
#include <Safir/Dob/ResponseGeneralErrorCodes.h>
#include <Safir/Dob/Typesystem/Exceptions.h>

#include <QDebug>

namespace
{
    QString Str(int64_t typeId) {return QString::fromStdWString(sdt::Operations::GetName(typeId));}
    QString Str(const std::wstring& s) { return QString::fromStdWString(s);}
    QString Str(const sdt::HandlerId& v) { return QString(", handler=%1").arg(Str(v.ToString())); }
    QString Str(const sdt::ChannelId& v) { return QString(", channel=%1").arg(Str(v.ToString())); }
    QString Str(const sdt::InstanceId& v) { return QString(", instance=%1").arg(Str(v.ToString())); }
    QString Str(const Safir::Dob::InstanceIdPolicy::Enumeration v) { return QString(", %1").arg(Str(Safir::Dob::InstanceIdPolicy::ToString(v))); }
    QString Str(const sdt::EntityId& v) { return Str(v.ToString()); }
}

DobNative::DobNative()
    : m_dobConnection()
    , m_isDispatchSignalled()
{
}

bool DobNative::IsOpen() const
{
    return m_dobConnection.IsOpen();
}

// Only used for manual calls to dispatch
void DobNative::Dispatch()
{
    try
    {
        if (m_dobConnection.IsOpen())
        {
            m_dobConnection.Dispatch();
        }
    }
    catch(const std::exception&) {}
}

void DobNative::Open(const QString& name, int context)
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
                                       emit DobInterface::Output("Connected to DOB!", QtWarningMsg);
                                   });
}


void DobNative::Close()
{
    try
    {
        m_dobConnection.Close();
        emit DobInterface::ConnectionClosed();
        emit DobInterface::Output("Disconnected from DOB!", QtWarningMsg);
    }
    catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase& e)
    {
        emit DobInterface::Output(QString::fromStdWString(e.GetMessage()), QtCriticalMsg);
    }
}

void DobNative::SubscribeMessage(int64_t typeId, const sdt::ChannelId& channel, bool includeSubclasses)
{
    try
    {
        m_dobConnection.SubscribeMessage(typeId, channel, includeSubclasses, this);
        DobInterface::SubscriptionInfo info{typeId, channel, includeSubclasses};
        m_subscriptions.push_back(info);
        emit DobInterface::SubscriptionStarted(info);
        emit DobInterface::Output("Subscribe message: " + Str(typeId) + Str(channel), QtInfoMsg);
    }
    catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase& e)
    {
        emit DobInterface::Output(QString::fromStdWString(e.GetMessage()), QtCriticalMsg);
    }
}

void DobNative::UnsubscribeMessage(int64_t typeId)
{
    try
    {
        m_dobConnection.UnsubscribeMessage(typeId, Safir::Dob::Typesystem::ChannelId::ALL_CHANNELS, this);
        RemoveSubscriptions(typeId);
        emit DobInterface::SubscriptionStopped(typeId);
        emit DobInterface::Output("Unsubscribe message: " + Str(typeId), QtInfoMsg);
    }
    catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase& e)
    {
        emit DobInterface::Output(QString::fromStdWString(e.GetMessage()), QtCriticalMsg);
    }
}

void DobNative::SubscribeEntity(int64_t typeId, const Safir::Dob::Typesystem::InstanceId &instance, bool includeSubclasses)
{
    try
    {
        if (instance == Safir::Dob::Typesystem::InstanceId())
        {
            m_dobConnection.SubscribeEntity(typeId, true, includeSubclasses, true, this);
            AddInstanceCounter(typeId, includeSubclasses);
            emit DobInterface::Output("Subscribe entity: " + Str(typeId), QtInfoMsg);
        }
        else
        {
            Safir::Dob::Typesystem::EntityId eid(typeId, instance);
            m_dobConnection.SubscribeEntity(eid, true, true, this);
            AddInstanceCounter(typeId, false);
            emit DobInterface::Output("Subscribe entity: " + Str(eid.ToString()), QtInfoMsg);
        }
        DobInterface::SubscriptionInfo info{typeId, sdt::ChannelId(), includeSubclasses};
        m_subscriptions.push_back(info);
        emit DobInterface::SubscriptionStarted(info);

    }
    catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase& e)
    {
        emit DobInterface::Output(QString::fromStdWString(e.GetMessage()), QtCriticalMsg);
    }
}

void DobNative::UnsubscribeEntity(int64_t typeId)
{
    try
    {
        m_dobConnection.UnsubscribeEntity(typeId, this);
        RemoveSubscriptions(typeId);
        RemoveInstanceCounterRecursively(typeId);
        emit DobInterface::SubscriptionStopped(typeId);
        emit DobInterface::Output("Unsubscribe entity: " + Str(typeId), QtInfoMsg);
    }
    catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase& e)
    {
        emit DobInterface::Output(QString::fromStdWString(e.GetMessage()), QtCriticalMsg);
    }
}

void DobNative::SubscribeRegistrations(int64_t typeId, const Safir::Dob::Typesystem::HandlerId &handler, bool includeSubclasses)
{
    try
    {
        m_dobConnection.SubscribeRegistration(typeId, handler, includeSubclasses, true, this);
        emit DobInterface::Output("Subscribe for registrations: " + Str(typeId) + Str(handler), QtInfoMsg);

    }
    catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase& e)
    {
        emit DobInterface::Output(QString::fromStdWString(e.GetMessage()), QtCriticalMsg);
    }
}

void DobNative::UnsubscribeRegistrations(int64_t typeId)
{
    try
    {
        m_dobConnection.UnsubscribeRegistration(typeId, sdt::HandlerId::ALL_HANDLERS, true, this);
        emit DobInterface::Output("Unsubscribe registrations: " + Str(typeId), QtInfoMsg);
    }
    catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase& e)
    {
        emit DobInterface::Output(QString::fromStdWString(e.GetMessage()), QtCriticalMsg);
    }
}

void DobNative::RegisterEntityHandler(int64_t typeId, const Safir::Dob::Typesystem::HandlerId &handler, Safir::Dob::InstanceIdPolicy::Enumeration instanceIdPolicy, bool pending, bool injection)
{
    try
    {
        if (injection)
        {
            m_dobConnection.RegisterEntityHandlerInjection(typeId, handler, instanceIdPolicy, this);
            emit DobInterface::Output("Register injection entity handler: " + Str(typeId) + Str(handler) + Str(instanceIdPolicy), QtInfoMsg);
        }
        else if (pending)
        {
            m_dobConnection.RegisterEntityHandlerPending(typeId, handler, instanceIdPolicy, this);
            emit DobInterface::Output("Register pending entity handler: " + Str(typeId) + Str(handler) + Str(instanceIdPolicy), QtInfoMsg);
        }
        else
        {
            m_dobConnection.RegisterEntityHandler(typeId, handler, instanceIdPolicy, this);
            emit DobInterface::Output("Register entity handler: " + Str(typeId) + Str(handler) + Str(instanceIdPolicy), QtInfoMsg);
        }

        DobInterface::RegistrationInfo info{typeId, handler, pending, injection, instanceIdPolicy};
        m_registrations.push_back(info);
        emit DobInterface::OnRegistered(info);
    }
    catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase& e)
    {
        emit DobInterface::Output(QString::fromStdWString(e.GetMessage()), QtCriticalMsg);
    }
}

void DobNative::RegisterServiceHandler(int64_t typeId, const Safir::Dob::Typesystem::HandlerId &handler, bool pending)
{
    try
    {
        if (pending)
        {
            m_dobConnection.RegisterServiceHandlerPending(typeId, handler, this);
            emit DobInterface::Output("Register pending service handler: " + Str(typeId) + Str(handler), QtInfoMsg);
        }
        else
        {
            m_dobConnection.RegisterServiceHandler(typeId, handler, this);
            emit DobInterface::Output("Register service handler: " + Str(typeId) + Str(handler), QtInfoMsg);
        }

        DobInterface::RegistrationInfo info{typeId, handler, pending, false, Safir::Dob::InstanceIdPolicy::RequestorDecidesInstanceId};
        m_registrations.push_back(info);
        emit DobInterface::OnRegistered(info);
    }
    catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase& e)
    {
        emit DobInterface::Output(QString::fromStdWString(e.GetMessage()), QtCriticalMsg);
    }
}

void DobNative::Unregister(int64_t typeId)
{
    try
    {        
        m_dobConnection.UnregisterHandler(typeId, Safir::Dob::Typesystem::HandlerId::ALL_HANDLERS);
        RemoveRegistrations(typeId);
        emit DobInterface::OnUnregistered(typeId);
        emit DobInterface::Output("Unregister handler: " + Str(typeId), QtInfoMsg);
    }
    catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase& e)
    {
        emit DobInterface::Output(QString::fromStdWString(e.GetMessage()), QtCriticalMsg);
    }
}

bool DobNative::SendMessage(const Safir::Dob::MessagePtr &message, const Safir::Dob::Typesystem::ChannelId &channel)
{
    try
    {
        m_dobConnection.Send(message, channel, this);
        emit DobInterface::Output("Send message: " + Str(message->GetTypeId()) + Str(channel), QtInfoMsg);
        return true;
    }
    catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase& e)
    {
        emit DobInterface::Output(QString::fromStdWString(e.GetMessage()), QtCriticalMsg);
        return false;
    }
}

bool DobNative::SendServiceRequest(const Safir::Dob::ServicePtr &request, const Safir::Dob::Typesystem::HandlerId &handler)
{
    try
    {
        m_dobConnection.ServiceRequest(request, handler, this);
        emit DobInterface::Output("Send service request: " + Str(request->GetTypeId()) + Str(handler), QtInfoMsg);
        return true;
    }
    catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase& e)
    {
        emit DobInterface::Output(QString::fromStdWString(e.GetMessage()), QtCriticalMsg);
        return false;
    }    
}

bool DobNative::CreateRequest(const Safir::Dob::EntityPtr &entity, const Safir::Dob::Typesystem::InstanceId &instance, const Safir::Dob::Typesystem::HandlerId &handler)
{
    try
    {
        // It is possible to check in advance if instance should be sent, but it is not that easy in websocket connection so we try
        // keep the behaviour similair.
        //auto policy = m_dobConnection.GetInstanceIdPolicy(entity->GetTypeId(), handler);

        if (instance == Safir::Dob::Typesystem::InstanceId())
        {
            // No valid instance, suppose afir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId
            m_dobConnection.CreateRequest(entity, handler, this);
            emit DobInterface::Output("Send create request: " + Str(entity->GetTypeId()) + Str(handler), QtInfoMsg);

        }
        else // Hopefully RequestorDecidesInstanceId
        {
            m_dobConnection.CreateRequest(entity, instance, handler, this);
            emit DobInterface::Output("Send create request: " + Str(entity->GetTypeId()) + Str(instance) + Str(handler), QtInfoMsg);
        }
    }
    catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase& e)
    {
        emit DobInterface::Output(QString::fromStdWString(e.GetMessage()), QtCriticalMsg);
        return false;
    }
    return true;
}

bool DobNative::UpdateRequest(const Safir::Dob::EntityPtr &entity, const Safir::Dob::Typesystem::InstanceId &instance)
{
    try
    {
        m_dobConnection.UpdateRequest(entity, instance, this);
        emit DobInterface::Output("Send update request: " + Str(entity->GetTypeId()) + Str(instance), QtInfoMsg);
        return true;
    }
    catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase& e)
    {
        emit DobInterface::Output(QString::fromStdWString(e.GetMessage()), QtCriticalMsg);
        return false;
    }
}

bool DobNative::DeleteRequest(const Safir::Dob::Typesystem::EntityId &entityId)
{
    try
    {
        m_dobConnection.DeleteRequest(entityId, this);
        emit DobInterface::Output("Send delete request: " + Str(entityId), QtInfoMsg);
        return true;
    }
    catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase& e)
    {
        emit DobInterface::Output(QString::fromStdWString(e.GetMessage()), QtCriticalMsg);
        return false;
    }
}

void DobNative::SetChanges(const Safir::Dob::EntityPtr &entity, const Safir::Dob::Typesystem::InstanceId &instance, const Safir::Dob::Typesystem::HandlerId &handler)
{
    try
    {
        m_dobConnection.SetChanges(entity, instance, handler);
        emit DobInterface::Output("Set changes: " + Str(entity->GetTypeId()) + Str(instance) + Str(handler), QtInfoMsg);
    }
    catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase& e)
    {
        emit DobInterface::Output(QString::fromStdWString(e.GetMessage()), QtCriticalMsg);
    }
}

void DobNative::SetAll(const Safir::Dob::EntityPtr& entity, const sdt::InstanceId& instance, const sdt::HandlerId& handler)
{
    try
    {
        m_dobConnection.SetAll(entity, instance, handler);
        emit DobInterface::Output("Set all: " + Str(entity->GetTypeId()) + Str(instance) + Str(handler), QtInfoMsg);
    }
    catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase& e)
    {
        emit DobInterface::Output(QString::fromStdWString(e.GetMessage()), QtCriticalMsg);
    }
}

void DobNative::Delete(const Safir::Dob::Typesystem::EntityId &entityId, const Safir::Dob::Typesystem::HandlerId &handler)
{
    try
    {
        m_dobConnection.Delete(entityId, handler);
        emit DobInterface::Output("Delete entity: " + Str(entityId) + Str(handler), QtInfoMsg);
    }
    catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase& e)
    {
        emit DobInterface::Output(QString::fromStdWString(e.GetMessage()), QtCriticalMsg);
    }
}

void DobNative::DeleteAll(int64_t typeId, const Safir::Dob::Typesystem::HandlerId &handler)
{
    try
    {
        m_dobConnection.DeleteAllInstances(typeId, handler);
        emit DobInterface::Output("Delete entity all instances: " + Str(typeId) + Str(handler), QtInfoMsg);
    }
    catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase& e)
    {
        emit DobInterface::Output(QString::fromStdWString(e.GetMessage()), QtCriticalMsg);
    }
}

void DobNative::ReadEntity(const sdt::EntityId& entityId)
{
    if (!sdt::Operations::Exists(entityId.GetTypeId()))
    {
        emit DobInterface::Output("Failed to read entity! The specified typeId doesn't exist. EntityId: " +  QString::fromStdWString(entityId.ToString()), QtCriticalMsg);
        return;
    }

    try
    {
        auto proxy = m_dobConnection.Read(entityId);
        emit DobInterface::OnReadEntity(proxy.GetEntity(), proxy.GetInstanceId());
    }
    catch (const Safir::Dob::NotFoundException&)
    {
        emit DobInterface::Output("ReadEntity failed! Entity doesn't exist. EntityId: " +  QString::fromStdWString(entityId.ToString()), QtCriticalMsg);
    }
    catch (const Safir::Dob::Typesystem::FundamentalException& e)
    {
        emit DobInterface::Output(QString::fromStdWString(e.GetMessage()), QtCriticalMsg);
    }
    catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase& e)
    {
        emit DobInterface::Output(QString::fromStdWString(e.GetMessage()), QtCriticalMsg);
    }
}

//------------------------------------------------------
// Dob consumer callbacks
//------------------------------------------------------
// Dispatcher interface
void DobNative::OnDoDispatch()
{
    if (!m_isDispatchSignalled.test_and_set())
    {
        QMetaObject::invokeMethod(this, [this]() {
            m_isDispatchSignalled.clear();
            if (m_behaviorOptions.dispatch)
            {
                m_dobConnection.Dispatch();
            }
        }, Qt::QueuedConnection);
    }
}

// StopHandler interface
void DobNative::OnStopOrder()
{
    emit DobInterface::Output("Got stop order, close connection", QtInfoMsg);
    Close();
}

// EntityRequestBase interface
void DobNative::OnCreateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy, Safir::Dob::ResponseSenderPtr responseSender)
{
    auto typeId = entityRequestProxy.GetTypeId();
    auto handlerId = entityRequestProxy.GetReceivingHandlerId();
    auto it = std::find_if(m_registrations.begin(), m_registrations.end(), [typeId, handlerId](const auto& ri){return ri.typeId == typeId && ri.handler.GetRawValue() == handlerId.GetRawValue();});

    emit DobInterface::OnCreateRequest(entityRequestProxy.GetRequest(), handlerId, entityRequestProxy.GetInstanceId());

    auto instance = (it != m_registrations.end() && it->instanceIdPolicy == Safir::Dob::InstanceIdPolicy::RequestorDecidesInstanceId) ?
                        entityRequestProxy.GetInstanceId() : sdt::InstanceId::GenerateRandom();

    if (m_behaviorOptions.createEntities)
    {
        m_dobConnection.SetAll(entityRequestProxy.GetRequest(), instance, handlerId);
    }
    else
    {
        emit DobInterface::Output("Create request received but entity creation is disabled in settings.", QtWarningMsg);
    }

    if (m_behaviorOptions.sendResponse)
    {
        responseSender->Send(GetResponse());
        emit DobInterface::Output("Respnse sent and entity created: " + Str(typeId) + Str(instance) + Str(handlerId), QtInfoMsg);
    }
    else
    {
        DobInterface::Output("No response sent for create request as per settings.", QtInfoMsg);
    }
}

void DobNative::OnUpdateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy, Safir::Dob::ResponseSenderPtr responseSender)
{
    auto handlerId = entityRequestProxy.GetReceivingHandlerId();
    auto instanceId = entityRequestProxy.GetEntityId().GetInstanceId();
    emit DobInterface::OnUpdateRequest(entityRequestProxy.GetRequest(), handlerId, instanceId);

    if (m_behaviorOptions.updateEntities)
    {
        m_dobConnection.SetChanges(entityRequestProxy.GetRequest(), entityRequestProxy.GetInstanceId(), entityRequestProxy.GetReceivingHandlerId());
    }
    else
    {
        emit DobInterface::Output("Update request received but entity updating is disabled in settings.", QtWarningMsg);
    }

    if (m_behaviorOptions.sendResponse)
    {
        responseSender->Send(GetResponse());
    }
    else
    {
        DobInterface::Output("No response sent for update request as per settings.", QtInfoMsg);
    }
}

void DobNative::OnDeleteRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy, Safir::Dob::ResponseSenderPtr responseSender)
{
    emit DobInterface::OnDeleteRequest(entityRequestProxy.GetEntityId(), entityRequestProxy.GetReceivingHandlerId());

    if (m_behaviorOptions.deleteEntities)
    {
        m_dobConnection.Delete(entityRequestProxy.GetEntityId(), entityRequestProxy.GetReceivingHandlerId());
    }
    else
    {
        emit DobInterface::Output("Delete request received but entity deletion is disabled in settings.", QtWarningMsg);
    }

    if (m_behaviorOptions.sendResponse)
    {
        responseSender->Send(GetResponse());
    }
    else
    {
        DobInterface::Output("No response sent for delete request as per settings.", QtInfoMsg);
    }
}

// RevokedRegistrationBase interface
void DobNative::OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId typeId, const Safir::Dob::Typesystem::HandlerId &handlerId)
{
    emit DobInterface::Output("Revoked registration: " + Str(typeId) + Str(handlerId), QtInfoMsg);
    RemoveRegistration(typeId, handlerId);
    emit DobInterface::OnUnregistered(typeId);
}

// EntityInjectionBase interface
void DobNative::OnInjectedNewEntity(const Safir::Dob::InjectedEntityProxy injectedEntityProxy)
{
    emit DobInterface::Output("Injected new entity: " + Str(injectedEntityProxy.GetEntityId()), QtInfoMsg);
}

void DobNative::OnInjectedUpdatedEntity(const Safir::Dob::InjectedEntityProxy injectedEntityProxy)
{
    emit DobInterface::Output("Injected updated entity: " + Str(injectedEntityProxy.GetEntityId()), QtInfoMsg);
}

void DobNative::OnInjectedDeletedEntity(const Safir::Dob::InjectedEntityProxy injectedEntityProxy)
{
    emit DobInterface::Output("Injected deleted entity: " + Str(injectedEntityProxy.GetEntityId()), QtInfoMsg);
}

void DobNative::OnInitialInjectionsDone(const Safir::Dob::Typesystem::TypeId typeId, const Safir::Dob::Typesystem::HandlerId &handlerId)
{
    emit DobInterface::Output("Initial injection done: " + Str(typeId) + Str(handlerId), QtInfoMsg);
}

// CompletedRegistrationBase interface
void DobNative::OnCompletedRegistration(const Safir::Dob::Typesystem::TypeId typeId, const Safir::Dob::Typesystem::HandlerId &handlerId)
{
    emit DobInterface::Output("Registration completed: " + Str(typeId) + Str(handlerId), QtInfoMsg);
    auto reg = DobInterface::GetRegistration(typeId, handlerId);
    if (reg != nullptr)
    {
        reg->pending = false;
        emit DobInterface::OnRegistered(*reg);
    }
    else
    {
        auto policy = m_dobConnection.GetInstanceIdPolicy(typeId, handlerId);
        DobInterface::RegistrationInfo info{typeId, handlerId, false, false, policy};
        m_registrations.push_back(info);
    }
}

// ServiceRequestBase interface
void DobNative::OnServiceRequest(const Safir::Dob::ServiceRequestProxy serviceRequestProxy, Safir::Dob::ResponseSenderPtr responseSender)
{
    emit DobInterface::OnServiceRequest(serviceRequestProxy.GetRequest(), serviceRequestProxy.GetReceivingHandlerId());
    if (m_behaviorOptions.sendResponse)
    {
        responseSender->Send(GetResponse());
    }
    else
    {
        DobInterface::Output("No response sent for service request as per settings.", QtInfoMsg);
    }
}

// Requestor interface
void DobNative::OnResponse(const Safir::Dob::ResponseProxy responseProxy)
{
    emit DobInterface::OnResponse(responseProxy.GetResponse());
}

void DobNative::OnNotRequestOverflow()
{
    emit DobInterface::OnNotRequestOverflow();
    emit DobInterface::Output("OnNotRequestOverflow", QtInfoMsg);
}

// MessageSender interface
void DobNative::OnNotMessageOverflow()
{
    emit DobInterface::OnNotMessageOverflow();
    emit DobInterface::Output("OnNotMessageOverflow", QtInfoMsg);
}

// RegistrationSubscriber interface
void DobNative::OnRegistered(const Safir::Dob::Typesystem::TypeId typeId, const Safir::Dob::Typesystem::HandlerId &handlerId)
{
    emit DobInterface::Output("OnRegistered: " + Str(typeId) + Str(handlerId), QtInfoMsg);
    DobInterface::RegistrationInfo info{typeId, handlerId, false, false, Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId};
    emit DobInterface::OnRegistered(info);
}

void DobNative::OnUnregistered(const Safir::Dob::Typesystem::TypeId typeId, const Safir::Dob::Typesystem::HandlerId &handlerId)
{
    emit DobInterface::Output("OnUnregistered: " + Str(typeId) + Str(handlerId), QtInfoMsg);
    emit DobInterface::OnUnregistered(typeId);
}

// MessageSubscriber interface
void DobNative::OnMessage(const Safir::Dob::MessageProxy messageProxy)
{
    emit DobInterface::OnMessage(messageProxy.GetChannelIdWithStringRepresentation(), messageProxy.GetMessage());
}

// EntitySubscriber interface
void DobNative::OnNewEntity(const Safir::Dob::EntityProxy entityProxy)
{
    IncreaseInstanceCounter(entityProxy.GetEntityId());
    emit DobInterface::OnEntity(entityProxy.GetEntityId(), entityProxy.GetOwner(), entityProxy.GetEntityWithChangeInfo(), DobInterface::NewEntity);
}
void DobNative::OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy)
{
    emit DobInterface::OnEntity(entityProxy.GetEntityId(), entityProxy.GetOwner(), entityProxy.GetEntityWithChangeInfo(), DobInterface::UpdatedEntity);
}

void DobNative::OnDeletedEntity(const Safir::Dob::EntityProxy entityProxy, const bool /*deprecated*/)
{
    DecreaseInstanceCounter(entityProxy.GetEntityId());
    emit DobInterface::OnEntity(entityProxy.GetEntityId(), entityProxy.GetOwner(), nullptr, DobInterface::DeletedEntity);
}

Safir::Dob::InstanceIdPolicy::Enumeration DobNative::GetInstanceIdPolicy(int64_t typeId, const sdt::HandlerId& handler) const
{
    auto id = handler.GetRawValue();
    auto it = std::find_if(m_registrations.begin(), m_registrations.end(), [typeId, id](const auto& v){return v.typeId == typeId && v.handler.GetRawValue() == id;});
    return it != m_registrations.end() ? it->instanceIdPolicy : Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId;
}
