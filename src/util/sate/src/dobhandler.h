/******************************************************************************
*
* Copyright Saab AB, 2025 (http://safirsdkcore.com)
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
#include <QJsonObject>
#include "dobinterface.h"

class DobHandler : public QObject
{
    Q_OBJECT
public:

    DobHandler();

    bool IsOpen() const;
    bool IsNativeConnection() const; // True if open connection is native Dob, false if WS connection

    void OpenNativeConnection(const QString& name, int context);
    void OpenWebsocketConnection(const QString& address, int port, const QString& name, int context);

    void Close();

    void SubscribeMessage(int64_t typeId, const sdt::ChannelId& channel, bool includeSubclasses);
    void UnsubscribeMessage(int64_t typeId);

    void SubscribeEntity(int64_t typeId, const Safir::Dob::Typesystem::InstanceId &instance, bool includeSubclasses);
    void UnsubscribeEntity(int64_t typeId);

    void SubscribeRegistrations(int64_t typeId, const Safir::Dob::Typesystem::HandlerId &handler, bool includeSubclasses);
    void UnsubscribeRegistrations(int64_t typeId);

    void RegisterEntityHandler(int64_t typeId, const Safir::Dob::Typesystem::HandlerId &handler, Safir::Dob::InstanceIdPolicy::Enumeration instanceIdPolicy, bool pending,  bool injection);
    void RegisterServiceHandler(int64_t typeId, const Safir::Dob::Typesystem::HandlerId &handler, bool pending);
    void Unregister(int64_t typeId);

    bool SendMessage(const Safir::Dob::MessagePtr &message, const Safir::Dob::Typesystem::ChannelId &channel);
    bool SendServiceRequest(const Safir::Dob::ServicePtr &request, const Safir::Dob::Typesystem::HandlerId &handler);

    bool CreateRequest(const Safir::Dob::EntityPtr &entity, const Safir::Dob::Typesystem::InstanceId &instance, const Safir::Dob::Typesystem::HandlerId &handler);
    bool UpdateRequest(const Safir::Dob::EntityPtr &entity, const Safir::Dob::Typesystem::InstanceId &instance);
    bool DeleteRequest(const Safir::Dob::Typesystem::EntityId &entityId);

    void SetChanges(const Safir::Dob::EntityPtr &entity, const Safir::Dob::Typesystem::InstanceId &instance, const Safir::Dob::Typesystem::HandlerId &handler);
    void SetAll(const Safir::Dob::EntityPtr& entity, const sdt::InstanceId& instance, const sdt::HandlerId& handler);
    void Delete(const Safir::Dob::Typesystem::EntityId &entityId, const Safir::Dob::Typesystem::HandlerId &handler);
    void DeleteAll(int64_t typeId, const Safir::Dob::Typesystem::HandlerId &handler);

    void ReadEntity(const sdt::EntityId& entityId);

    // Manual dispatch call
    void Dispatch();

    const DobInterface::RegistrationInfo* GetMyRegistration(int64_t typeId) const;
    const DobInterface::SubscriptionInfo* GetMySubscription(int64_t typeId) const;

    int64_t NumberOfInstances(const int64_t typeId) const;
    size_t NumberOfSubscriptions() const;

    void InterfaceListenerAdded() { m_numberOfInterfaceListeners++; }
    void InterfaceListenerRemoved() { m_numberOfInterfaceListeners--; }

    // Set dispatching and response behavior
    void SetBehaviorOptions(const DobInterface::BehaviorOptions& options);
    
    // Set response object used for incoming requests
    void SetResponse(const Safir::Dob::ResponsePtr& response);

signals:
    void ConnectedToDob(const QString& connectionName);
    void ConnectionClosed();

    void OnMessage(const sdt::ChannelId& channel, const Safir::Dob::MessagePtr& message);
    void OnEntity(const sdt::EntityId& entityId, const sdt::HandlerId& handler, const Safir::Dob::EntityPtr& entity, DobInterface::EntityOperation operation);
    void OnResponse(const Safir::Dob::ResponsePtr& response);

    void OnCreateRequest(const Safir::Dob::EntityPtr& request, const sdt::HandlerId& handler, const sdt::InstanceId& instance);
    void OnUpdateRequest(const Safir::Dob::EntityPtr& request, const sdt::HandlerId& handler, const sdt::InstanceId& instance);
    void OnDeleteRequest(const Safir::Dob::Typesystem::EntityId& entityId, const sdt::HandlerId& handler);
    void OnServiceRequest(const Safir::Dob::ServicePtr& request, const sdt::HandlerId& handler);

    void SubscriptionStarted(const DobInterface::SubscriptionInfo& info);
    void SubscriptionStopped(int64_t typeId);

    void OnRegistered(const DobInterface::RegistrationInfo& info);
    void OnUnregistered(int64_t typeId);

    void OnNotRequestOverflow();
    void OnNotMessageOverflow();

    void OnReadEntity(const Safir::Dob::EntityPtr& entity, const sdt::InstanceId& instance);

    void NumberOfInstancesChanged(const int64_t typeId);

    void Output(const QString& msg, const QtMsgType msgType);

    void InterfaceListener(const QJsonObject& dobCall);

private:
    std::unique_ptr<DobInterface> m_dob;
    int m_numberOfInterfaceListeners = 0;
    DobInterface::BehaviorOptions m_behaviorOptions{};
    Safir::Dob::ResponsePtr m_response = nullptr;

    bool IsInitiated();
    void SetupSignalSlots();
};
