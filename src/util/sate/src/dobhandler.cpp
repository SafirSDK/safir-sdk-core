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
* but WITHOUT ANY WARRANTY
{

}
 without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/
#include "dobhandler.h"
#include "dobnative.h"
#include "dobwebsocket.h"
#include "dobcalltojson.h"
#include <QDebug>
#include <memory>

DobHandler::DobHandler()
{
}

bool DobHandler::IsInitiated()
{
    if (!m_dob)
    {
        emit Output("Not connected to DOB!", QtCriticalMsg);
        return false;
    }
    return true;
}

void DobHandler::SetupSignalSlots()
{
    if (m_dob)
    {
        connect(m_dob.get(), &DobInterface::ConnectedToDob, this, &DobHandler::ConnectedToDob);
        connect(m_dob.get(), &DobInterface::ConnectionClosed, this, &DobHandler::ConnectionClosed);
        connect(m_dob.get(), &DobInterface::OnMessage, this, &DobHandler::OnMessage);
        connect(m_dob.get(), &DobInterface::OnEntity, this, &DobHandler::OnEntity);
        connect(m_dob.get(), &DobInterface::OnResponse, this, &DobHandler::OnResponse);
        connect(m_dob.get(), &DobInterface::OnCreateRequest, this, &DobHandler::OnCreateRequest);
        connect(m_dob.get(), &DobInterface::OnUpdateRequest, this, &DobHandler::OnUpdateRequest);
        connect(m_dob.get(), &DobInterface::OnDeleteRequest, this, &DobHandler::OnDeleteRequest);
        connect(m_dob.get(), &DobInterface::OnServiceRequest, this, &DobHandler::OnServiceRequest);
        connect(m_dob.get(), &DobInterface::SubscriptionStarted, this, &DobHandler::SubscriptionStarted);
        connect(m_dob.get(), &DobInterface::SubscriptionStopped, this, &DobHandler::SubscriptionStopped);
        connect(m_dob.get(), &DobInterface::OnRegistered, this, &DobHandler::OnRegistered);
        connect(m_dob.get(), &DobInterface::OnUnregistered, this, &DobHandler::OnUnregistered);
        connect(m_dob.get(), &DobInterface::OnNotRequestOverflow, this, &DobHandler::OnNotRequestOverflow);
        connect(m_dob.get(), &DobInterface::OnNotMessageOverflow, this, &DobHandler::OnNotMessageOverflow);
        connect(m_dob.get(), &DobInterface::OnReadEntity, this, &DobHandler::OnReadEntity);
        connect(m_dob.get(), &DobInterface::NumberOfInstancesChanged, this, &DobHandler::NumberOfInstancesChanged);
        connect(m_dob.get(), &DobInterface::Output, this, &DobHandler::Output);
    }
}

bool DobHandler::IsOpen() const
{
    if (!m_dob)
    {
        return false;
    }
    return m_dob->IsOpen();
}

void DobHandler::Dispatch()
{
    if (!IsInitiated())
    {
        return;
    }
    m_dob->Dispatch();
}

bool DobHandler::IsNativeConnection() const
{
    return !m_dob || dynamic_cast<DobNative*>(m_dob.get()) != nullptr;
}

void DobHandler::OpenNativeConnection(const QString& name, int context)
{
    if (IsOpen())
    {
        m_dob->Close();
        m_dob.reset();
    }

    m_dob = std::unique_ptr<DobInterface>(new DobNative());
    m_dob->SetBehaviorOptions(m_behaviorOptions);
    SetupSignalSlots();

    if (m_numberOfInterfaceListeners > 0) //optimize: only create JSON if there are listeners
    {
        emit InterfaceListener(DobCallToJson::Open(name, context));
    }

    m_dob->Open(name, context);
}

void DobHandler::OpenWebsocketConnection(const QString& address, int port, const QString& name, int context)
{
    if (IsOpen())
    {
        m_dob->Close();
        m_dob.reset();
    }

    m_dob = std::unique_ptr<DobInterface>(new DobWebSocket(address, port));
    m_dob->SetBehaviorOptions(m_behaviorOptions);
    SetupSignalSlots();

    if (m_numberOfInterfaceListeners > 0) //optimize: only create JSON if there are listeners
    {
        emit InterfaceListener(DobCallToJson::Open(name, context));
    }

    m_dob->Open(name, context);
}

void DobHandler::Close()
{
    if (!IsInitiated())
    {
        return;
    }

    if (m_numberOfInterfaceListeners > 0) //optimize: only create JSON if there are listeners
    {
        emit InterfaceListener(DobCallToJson::Close());
    }

    m_dob->Close();
}


void DobHandler::SubscribeMessage(int64_t typeId, const sdt::ChannelId& channel, bool includeSubclasses)
{
    if (!IsInitiated())
    {
        return;
    }

    if (m_numberOfInterfaceListeners > 0) //optimize: only create JSON if there are listeners
    {
        emit InterfaceListener(DobCallToJson::SubscribeMessage(typeId, channel, includeSubclasses));
    }

    m_dob->SubscribeMessage(typeId, channel, includeSubclasses);
}

void DobHandler::UnsubscribeMessage(int64_t typeId)
{
    if (!IsInitiated())
    {
        return;
    }

    if (m_numberOfInterfaceListeners > 0) //optimize: only create JSON if there are listeners
    {
        emit InterfaceListener(DobCallToJson::UnsubscribeMessage(typeId));
    }

    m_dob->UnsubscribeMessage(typeId);
}


void DobHandler::SubscribeEntity(int64_t typeId, const Safir::Dob::Typesystem::InstanceId &instance, bool includeSubclasses)
{
    if (!IsInitiated())
    {
        return;
    }

    if (m_numberOfInterfaceListeners > 0) //optimize: only create JSON if there are listeners
    {
        emit InterfaceListener(DobCallToJson::SubscribeEntity(typeId, instance, includeSubclasses));
    }

    m_dob->SubscribeEntity(typeId, instance, includeSubclasses);
}

void DobHandler::UnsubscribeEntity(int64_t typeId)
{
    if (!IsInitiated())
    {
        return;
    }

    if (m_numberOfInterfaceListeners > 0) //optimize: only create JSON if there are listeners
    {
        emit InterfaceListener(DobCallToJson::UnsubscribeEntity(typeId));
    }

    m_dob->UnsubscribeEntity(typeId);
}


void DobHandler::SubscribeRegistrations(int64_t typeId, const Safir::Dob::Typesystem::HandlerId &handler, bool includeSubclasses)
{
    if (!IsInitiated())
    {
        return;
    }

    if (m_numberOfInterfaceListeners > 0) //optimize: only create JSON if there are listeners
    {
        emit InterfaceListener(DobCallToJson::SubscribeRegistrations(typeId, handler, includeSubclasses));
    }

    m_dob->SubscribeRegistrations(typeId, handler, includeSubclasses);
}

void DobHandler::UnsubscribeRegistrations(int64_t typeId)
{
    if (m_numberOfInterfaceListeners > 0) //optimize: only create JSON if there are listeners
    {
        emit InterfaceListener(DobCallToJson::UnsubscribeRegistrations(typeId));
    }

    m_dob->UnsubscribeRegistrations(typeId);
}


void DobHandler::RegisterEntityHandler(int64_t typeId, const Safir::Dob::Typesystem::HandlerId &handler, Safir::Dob::InstanceIdPolicy::Enumeration instanceIdPolicy, bool pending,  bool injection)
{
    if (!IsInitiated())
    {
        return;
    }

    if (m_numberOfInterfaceListeners > 0) //optimize: only create JSON if there are listeners
    {
        emit InterfaceListener(DobCallToJson::RegisterEntityHandler(typeId, handler, instanceIdPolicy, pending, injection));
    }

    m_dob->RegisterEntityHandler(typeId, handler, instanceIdPolicy, pending, injection);
}

void DobHandler::RegisterServiceHandler(int64_t typeId, const Safir::Dob::Typesystem::HandlerId &handler, bool pending)
{
    if (!IsInitiated())
    {
        return;
    }

    if (m_numberOfInterfaceListeners > 0) //optimize: only create JSON if there are listeners
    {
        emit InterfaceListener(DobCallToJson::RegisterServiceHandler(typeId, handler, pending));
    }

    m_dob->RegisterServiceHandler(typeId, handler, pending);
}

void DobHandler::Unregister(int64_t typeId)
{
    if (!IsInitiated())
    {
        return;
    }

    if (m_numberOfInterfaceListeners > 0) //optimize: only create JSON if there are listeners
    {
        emit InterfaceListener(DobCallToJson::Unregister(typeId));
    }

    m_dob->Unregister(typeId);
}


bool DobHandler::SendMessage(const Safir::Dob::MessagePtr &message, const Safir::Dob::Typesystem::ChannelId &channel)
{
    if (!IsInitiated())
    {
        return false;
    }

    if (m_numberOfInterfaceListeners > 0) //optimize: only create JSON if there are listeners
    {
        emit InterfaceListener(DobCallToJson::SendMessage(message, channel));
    }

    return m_dob->SendMessage(message, channel);
}

bool DobHandler::SendServiceRequest(const Safir::Dob::ServicePtr &request, const Safir::Dob::Typesystem::HandlerId &handler)
{
    if (!IsInitiated())
    {
        return false;
    }

    if (m_numberOfInterfaceListeners > 0) //optimize: only create JSON if there are listeners
    {
        emit InterfaceListener(DobCallToJson::SendServiceRequest(request, handler));
    }

    return m_dob->SendServiceRequest(request, handler);
}


bool DobHandler::CreateRequest(const Safir::Dob::EntityPtr &entity, const Safir::Dob::Typesystem::InstanceId &instance, const Safir::Dob::Typesystem::HandlerId &handler)
{
    if (!IsInitiated())
    {
        return false;
    }

    if (m_numberOfInterfaceListeners > 0) //optimize: only create JSON if there are listeners
    {
        emit InterfaceListener(DobCallToJson::CreateRequest(entity, instance, handler));
    }

    return m_dob->CreateRequest(entity, instance, handler);
}

bool DobHandler::UpdateRequest(const Safir::Dob::EntityPtr &entity, const Safir::Dob::Typesystem::InstanceId &instance)
{
    if (!IsInitiated())
    {
        return false;
    }

    if (m_numberOfInterfaceListeners > 0) //optimize: only create JSON if there are listeners
    {
        emit InterfaceListener(DobCallToJson::UpdateRequest(entity, instance));
    }

    return m_dob->UpdateRequest(entity, instance);
}

bool DobHandler::DeleteRequest(const Safir::Dob::Typesystem::EntityId &entityId)
{
    if (!IsInitiated())
    {
        return false;
    }

    if (m_numberOfInterfaceListeners > 0) //optimize: only create JSON if there are listeners
    {
        emit InterfaceListener(DobCallToJson::DeleteRequest(entityId));
    }

    return m_dob->DeleteRequest(entityId);
}


void DobHandler::SetChanges(const Safir::Dob::EntityPtr &entity, const Safir::Dob::Typesystem::InstanceId &instance, const Safir::Dob::Typesystem::HandlerId &handler)
{
    if (!IsInitiated())
    {
        return;
    }

    if (m_numberOfInterfaceListeners > 0) //optimize: only create JSON if there are listeners
    {
        emit InterfaceListener(DobCallToJson::SetChanges(entity, instance, handler));
    }

    m_dob->SetChanges(entity, instance, handler);
}

void DobHandler::SetAll(const Safir::Dob::EntityPtr& entity, const sdt::InstanceId& instance, const sdt::HandlerId& handler)
{
    if (!IsInitiated())
    {
        return;
    }

    if (m_numberOfInterfaceListeners > 0) //optimize: only create JSON if there are listeners
    {
        emit InterfaceListener(DobCallToJson::SetAll(entity, instance, handler));
    }

    m_dob->SetAll(entity, instance, handler);
}

void DobHandler::Delete(const Safir::Dob::Typesystem::EntityId &entityId, const Safir::Dob::Typesystem::HandlerId &handler)
{
    if (!IsInitiated())
    {
        return;
    }

    if (m_numberOfInterfaceListeners > 0) //optimize: only create JSON if there are listeners
    {
        emit InterfaceListener(DobCallToJson::Delete(entityId, handler));
    }

    m_dob->Delete(entityId, handler);
}

void DobHandler::DeleteAll(int64_t typeId, const Safir::Dob::Typesystem::HandlerId &handler)
{
    if (!IsInitiated())
    {
        return;
    }

    if (m_numberOfInterfaceListeners > 0) //optimize: only create JSON if there are listeners
    {
        emit InterfaceListener(DobCallToJson::DeleteAll(typeId, handler));
    }

    m_dob->DeleteAll(typeId, handler);
}

void DobHandler::ReadEntity(const sdt::EntityId& entityId)
{
    if (!IsInitiated())
    {
        return;
    }

    if (m_numberOfInterfaceListeners > 0) //optimize: only create JSON if there are listeners
    {
        emit InterfaceListener(DobCallToJson::ReadEntity(entityId));
    }

    m_dob->ReadEntity(entityId);
}

const DobInterface::RegistrationInfo* DobHandler::GetMyRegistration(int64_t typeId) const
{
    if (m_dob)
    {
        return m_dob->GetRegistration(typeId);
    }

    return nullptr;
}

const DobInterface::SubscriptionInfo* DobHandler::GetMySubscription(int64_t typeId) const
{
    if (m_dob)
    {
        return m_dob->GetSubscription(typeId);
    }

    return nullptr;
}


int64_t DobHandler::NumberOfInstances(const int64_t typeId) const
{
    if (m_dob)
    {
        return m_dob->NumberOfInstances(typeId);
    }
    return -1;
}

size_t DobHandler::NumberOfSubscriptions() const
{
    if (m_dob)
    {
        return m_dob->NumberOfSubscriptions();
    }
    return 0;
}

void DobHandler::SetBehaviorOptions(const DobInterface::BehaviorOptions& options)
{
    m_behaviorOptions = options;
    if (m_dob)
    {
        m_dob->SetBehaviorOptions(options);
    }
}

void DobHandler::SetResponse(const Safir::Dob::ResponsePtr& response)
{
    m_response = response;
    if (m_dob)
    {
        m_dob->SetResponse(response);
    }
}