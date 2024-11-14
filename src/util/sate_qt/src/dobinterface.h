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
#include <Safir/Dob/Message.h>
#include <Safir/Dob/Entity.h>
#include <Safir/Dob/Response.h>
#include <Safir/Dob/Service.h>
#include <Safir/Dob/InstanceIdPolicy.h>

namespace sdt = Safir::Dob::Typesystem;

class DobInterface : public QObject
{
    Q_OBJECT
public:
    enum EntityOperation { NewEntity, UpdatedEntity, DeletedEntity};
    enum RequestCategory { CreateEntity, UpdateEntity, DeleteEntity, Service};

    struct RegistrationInfo
    {
        int64_t typeId;
        int64_t id; //HandlerId
        bool pending;
        bool injection;
        Safir::Dob::InstanceIdPolicy::Enumeration instanceIdPolicy;
    };

    struct SubscriptionInfo
    {
        int64_t typeId;
        int64_t id; //HandlerId or ChannelId
        bool includeSubclasses;
    };

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

signals:
    void ConnectedToDob(const QString& connectionName);
    void ConnectionClosed();

    void OnMessage(int64_t typeId, const sdt::ChannelId& channel, const Safir::Dob::MessagePtr& message);
    void OnEntity(const sdt::EntityId& entityId, const sdt::HandlerId& handler, const Safir::Dob::EntityPtr& entity, EntityOperation operation);
    void OnResponse(const Safir::Dob::ResponsePtr& response);
    void OnRequest(const Safir::Dob::Typesystem::ObjectPtr request, RequestCategory category);

    void SubscriptionStarted(const DobInterface::SubscriptionInfo& info);
    void SubscriptionStopped(int64_t typeId);

    void OnRegistered(const DobInterface::RegistrationInfo& info);
    void OnUnregistered(int64_t typeId);

    void Info(const QString& label, const QString& message, bool error);

};
