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
#pragma once

#include <QObject>
#include <Safir/Dob/Typesystem/ChannelId.h>
#include <Safir/Dob/Entity.h>
#include <Safir/Dob/Message.h>
#include <Safir/Dob/Entity.h>
#include <Safir/Dob/Response.h>
#include <Safir/Dob/Service.h>
#include <Safir/Dob/InstanceIdPolicy.h>
#include <vector>

namespace sdt = Safir::Dob::Typesystem;

class DobInterface : public QObject
{
    Q_OBJECT
public:
    enum EntityOperation { NewEntity, UpdatedEntity, DeletedEntity};

    struct RegistrationInfo
    {
        int64_t typeId;
        sdt::HandlerId handler;
        bool pending;
        bool injection;
        Safir::Dob::InstanceIdPolicy::Enumeration instanceIdPolicy;
    };

    struct SubscriptionInfo
    {
        int64_t typeId;
        sdt::ChannelId channel;
        bool includeSubclasses;
    };

    virtual bool IsOpen() const = 0;
    virtual void Open(const QString& name, int context) = 0;
    virtual void Close() = 0;

    // Subscriptions
    virtual void SubscribeMessage(int64_t typeId, const sdt::ChannelId& channel, bool includeSubclasses) = 0;
    virtual void UnsubscribeMessage(int64_t typeId) = 0;
    virtual void SubscribeEntity(int64_t typeId, const sdt::InstanceId& instance, bool includeSubclasses) = 0;
    virtual void UnsubscribeEntity(int64_t typeId) = 0;
    virtual void SubscribeRegistrations(int64_t typeId, const sdt::HandlerId& handler, bool includeSubclasses) = 0;
    virtual void UnsubscribeRegistrations(int64_t typeId) = 0;

    // Registrations
    virtual void RegisterEntityHandler(int64_t typeId, const sdt::HandlerId& handler, Safir::Dob::InstanceIdPolicy::Enumeration instanceIdPolicy, bool pending, bool injection) = 0;
    virtual void RegisterServiceHandler(int64_t typeId, const sdt::HandlerId& handler, bool pending) = 0;
    virtual void Unregister(int64_t typeId) = 0;

    virtual void SendMessage(const Safir::Dob::MessagePtr& message, const sdt::ChannelId& channel) = 0;
    virtual void SendServiceRequest(const Safir::Dob::ServicePtr& request, const sdt::HandlerId& handler) = 0;

    virtual void CreateRequest(const Safir::Dob::EntityPtr& entity, const sdt::InstanceId& instance, const sdt::HandlerId& handler) = 0;
    virtual void UpdateRequest(const Safir::Dob::EntityPtr& entity, const sdt::InstanceId& instance) = 0;
    virtual void DeleteRequest(const sdt::EntityId& entityId) = 0;

    virtual void SetChanges(const Safir::Dob::EntityPtr& entity, const sdt::InstanceId& instance, const sdt::HandlerId& handler) = 0;
    virtual void SetAll(const Safir::Dob::EntityPtr& entity, const sdt::InstanceId& instance, const sdt::HandlerId& handler) = 0;
    virtual void Delete(const sdt::EntityId& entityId, const sdt::HandlerId& handler) = 0;
    virtual void DeleteAll(int64_t typeId, const sdt::HandlerId& handler) = 0;

    RegistrationInfo* GetRegistration(int64_t typeId)
    {
        auto it = std::find_if(m_registrations.begin(), m_registrations.end(), [typeId](const auto& ri){return ri.typeId == typeId;});
        return it == m_registrations.end() ? nullptr : &(*it);
    }

    RegistrationInfo* GetRegistration(int64_t typeId, const sdt::HandlerId& handler)
    {
        auto it = std::find_if(m_registrations.begin(), m_registrations.end(), [typeId, &handler](const auto& ri){return ri.typeId == typeId && ri.handler == handler;});
        return it == m_registrations.end() ? nullptr : &(*it);
    }

    void RemoveRegistrations(int64_t typeId)
    {
        m_registrations.erase(std::remove_if(m_registrations.begin(), m_registrations.end(), [typeId](const auto& v){return v.typeId == typeId;}), m_registrations.end());
    }

    void RemoveRegistration(int64_t typeId, const sdt::HandlerId& handler)
    {
        m_registrations.erase(std::remove_if(m_registrations.begin(), m_registrations.end(), [typeId, &handler](const auto& v){return v.typeId == typeId && v.handler == handler;}), m_registrations.end());
    }

    const SubscriptionInfo* GetSubscription(int64_t typeId) const
    {
        auto it = std::find_if(m_subscriptions.begin(), m_subscriptions.end(), [typeId](const auto& si){return si.typeId == typeId;});
        return it == m_subscriptions.end() ? nullptr : &(*it);
    }

    const SubscriptionInfo* GetSubscription(int64_t typeId, sdt::ChannelId channel) const
    {
        auto it = std::find_if(m_subscriptions.begin(), m_subscriptions.end(), [typeId, &channel](const auto& si){return si.typeId == typeId && si.channel == channel;});
        return it == m_subscriptions.end() ? nullptr : &(*it);
    }

    void RemoveSubscriptions(int64_t typeId)
    {
        m_subscriptions.erase(std::remove_if(m_subscriptions.begin(), m_subscriptions.end(), [typeId](const auto& v){return v.typeId == typeId;}), m_subscriptions.end());
    }

signals:
    void ConnectedToDob(const QString& connectionName);
    void ConnectionClosed();

    void OnMessage(const sdt::ChannelId& channel, const Safir::Dob::MessagePtr& message);
    void OnEntity(const sdt::EntityId& entityId, const sdt::HandlerId& handler, const Safir::Dob::EntityPtr& entity, EntityOperation operation);
    void OnResponse(const Safir::Dob::ResponsePtr& response);

    void OnCreateRequest(const Safir::Dob::EntityPtr& request, const sdt::HandlerId& handler, const sdt::InstanceId& instance);
    void OnUpdateRequest(const Safir::Dob::EntityPtr& request, const sdt::HandlerId& handler, const sdt::InstanceId& instance);
    void OnDeleteRequest(const Safir::Dob::Typesystem::EntityId& entityId, const sdt::HandlerId& handler);
    void OnServiceRequest(const Safir::Dob::ServicePtr& request, const sdt::HandlerId& handler);

    void SubscriptionStarted(const DobInterface::SubscriptionInfo& info);
    void SubscriptionStopped(int64_t typeId);

    void OnRegistered(const DobInterface::RegistrationInfo& info);
    void OnUnregistered(int64_t typeId);

    void Output(const QString& msg, const QtMsgType msgType);

protected:
    std::vector<DobInterface::RegistrationInfo> m_registrations;
    std::vector<DobInterface::SubscriptionInfo> m_subscriptions;
};
