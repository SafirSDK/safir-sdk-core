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
#include <QDebug>
#include <memory>

DobHandler::DobHandler()
{
}

bool DobHandler::IsInitiated()
{
    if (!m_dob)
    {
        emit Info("Not connected to DOB!", QtCriticalMsg);
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
        connect(m_dob.get(), &DobInterface::Info, this, &DobHandler::Info);
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

bool DobHandler::IsNativeConnection() const
{
    return !m_dob || dynamic_cast<DobNative*>(m_dob.get()) != nullptr;
}

void DobHandler::OpenNativeConnection(const QString& name, int context)
{
    if (m_dob && m_dob->IsOpen())
    {
        m_dob->Close();
        m_dob.reset();
    }

    m_dob = std::unique_ptr<DobInterface>(new DobNative());
    SetupSignalSlots();
    m_dob->Open(name, context);
}

void DobHandler::OpenWebsocketConnection(const QString& address, int port, const QString& name, int context)
{
    if (m_dob && m_dob->IsOpen())
    {
        m_dob->Close();
        m_dob.reset();
    }

    m_dob = std::unique_ptr<DobInterface>(new DobWebSocket(address, port));
    SetupSignalSlots();
    m_dob->Open(name, context);
}

void DobHandler::Close()
{
    m_dob->Close();
}


void DobHandler::SubscribeMessage(int64_t typeId, const sdt::ChannelId& channel, bool includeSubclasses)
{
    if (!IsInitiated())
    {
        return;
    }

    m_dob->SubscribeMessage(typeId, channel, includeSubclasses);
}

void DobHandler::UnsubscribeMessage(int64_t typeId)
{
    if (!IsInitiated())
    {
        return;
    }
    m_dob->UnsubscribeMessage(typeId);
}


void DobHandler::SubscribeEntity(int64_t typeId, const Safir::Dob::Typesystem::InstanceId &instance, bool includeSubclasses)
{
    if (!IsInitiated())
    {
        return;
    }
    m_dob->SubscribeEntity(typeId, instance, includeSubclasses);
}

void DobHandler::UnsubscribeEntity(int64_t typeId)
{
    if (!IsInitiated())
    {
        return;
    }
    m_dob->UnsubscribeEntity(typeId);
}


void DobHandler::SubscribeRegistrations(int64_t typeId, const Safir::Dob::Typesystem::HandlerId &handler, bool includeSubclasses)
{
    if (!IsInitiated())
    {
        return;
    }
    m_dob->SubscribeRegistrations(typeId, handler, includeSubclasses);
}

void DobHandler::UnsubscribeRegistrations(int64_t typeId)
{
    m_dob->UnsubscribeRegistrations(typeId);
}


void DobHandler::RegisterEntityHandler(int64_t typeId, const Safir::Dob::Typesystem::HandlerId &handler, Safir::Dob::InstanceIdPolicy::Enumeration instanceIdPolicy, bool pending,  bool injection)
{
    if (!IsInitiated())
    {
        return;
    }
    m_dob->RegisterEntityHandler(typeId, handler, instanceIdPolicy, pending, injection);
}

void DobHandler::RegisterServiceHandler(int64_t typeId, const Safir::Dob::Typesystem::HandlerId &handler, bool pending)
{
    if (!IsInitiated())
    {
        return;
    }
    m_dob->RegisterServiceHandler(typeId, handler, pending);
}

void DobHandler::Unregister(int64_t typeId)
{
    if (!IsInitiated())
    {
        return;
    }
    m_dob->Unregister(typeId);
}


void DobHandler::SendMessage(const Safir::Dob::MessagePtr &message, const Safir::Dob::Typesystem::ChannelId &channel)
{
    if (!IsInitiated())
    {
        return;
    }
    m_dob->SendMessage(message, channel);
}

void DobHandler::SendServiceRequest(const Safir::Dob::ServicePtr &request, const Safir::Dob::Typesystem::HandlerId &handler)
{
    if (!IsInitiated())
    {
        return;
    }
    m_dob->SendServiceRequest(request, handler);
}


void DobHandler::CreateRequest(const Safir::Dob::EntityPtr &entity, const Safir::Dob::Typesystem::InstanceId &instance, const Safir::Dob::Typesystem::HandlerId &handler)
{
    if (!IsInitiated())
    {
        return;
    }
    m_dob->CreateRequest(entity, instance, handler);
}

void DobHandler::UpdateRequest(const Safir::Dob::EntityPtr &entity, const Safir::Dob::Typesystem::InstanceId &instance)
{
    if (!IsInitiated())
    {
        return;
    }
    m_dob->UpdateRequest(entity, instance);
}

void DobHandler::DeleteRequest(const Safir::Dob::Typesystem::EntityId &entityId)
{
    if (!IsInitiated())
    {
        return;
    }
    m_dob->DeleteRequest(entityId);
}


void DobHandler::SetChanges(const Safir::Dob::EntityPtr &entity, const Safir::Dob::Typesystem::InstanceId &instance, const Safir::Dob::Typesystem::HandlerId &handler)
{
    if (!IsInitiated())
    {
        return;
    }
    m_dob->SetChanges(entity, instance, handler);
}

void DobHandler::SetAll(const Safir::Dob::EntityPtr& entity, const sdt::InstanceId& instance, const sdt::HandlerId& handler)
{
    if (!IsInitiated())
    {
        return;
    }
    m_dob->SetAll(entity, instance, handler);
}

void DobHandler::Delete(const Safir::Dob::Typesystem::EntityId &entityId, const Safir::Dob::Typesystem::HandlerId &handler)
{
    if (!IsInitiated())
    {
        return;
    }
    m_dob->Delete(entityId, handler);
}

void DobHandler::DeleteAll(int64_t typeId, const Safir::Dob::Typesystem::HandlerId &handler)
{
    if (!IsInitiated())
    {
        return;
    }
    m_dob->DeleteAll(typeId, handler);
}

const DobInterface::RegistrationInfo* DobHandler::GetMyRegistration(int64_t typeId) const
{
    if (m_dob)
    {
        return m_dob->GetMyRegistration(typeId);
    }

    return nullptr;
}

const DobInterface::SubscriptionInfo* DobHandler::GetMySubscription(int64_t typeId) const
{
    if (m_dob)
    {
        return m_dob->GetMySubscription(typeId);
    }

    return nullptr;

}


