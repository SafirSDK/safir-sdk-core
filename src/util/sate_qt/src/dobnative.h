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
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/Consumer.h>
#include "dobinterface.h"

class DobNative : public DobInterface,
                   public Safir::Dob::Dispatcher,
                   public Safir::Dob::StopHandler,
                   public Safir::Dob::EntityHandler,
                   public Safir::Dob::EntityHandlerInjection,
                   public Safir::Dob::EntityHandlerPending,
                   public Safir::Dob::ServiceHandler,
                   public Safir::Dob::ServiceHandlerPending,
                   public Safir::Dob::Requestor,
                   public Safir::Dob::MessageSender,
                   public Safir::Dob::RegistrationSubscriber,
                   public Safir::Dob::MessageSubscriber,
                   public Safir::Dob::EntitySubscriber
{
    Q_OBJECT
public:

    DobNative();

    bool IsOpen() const override;
    void Open(const QString& name, int context) override;
    void Close() override;

    void SubscribeMessage(int64_t typeId, const sdt::ChannelId& channel, bool includeSubclasses) override;
    void UnsubscribeMessage(int64_t typeId) override;

    void SubscribeEntity(int64_t typeId, const Safir::Dob::Typesystem::InstanceId &instance, bool includeSubclasses) override;
    void UnsubscribeEntity(int64_t typeId) override;

    void SubscribeRegistrations(int64_t typeId, const Safir::Dob::Typesystem::HandlerId &handler, bool includeSubclasses) override;
    void UnsubscribeRegistrations(int64_t typeId) override;

    void RegisterEntityHandler(int64_t typeId, const Safir::Dob::Typesystem::HandlerId &handler, Safir::Dob::InstanceIdPolicy::Enumeration instanceIdPolicy, bool pending,  bool injection) override;
    void RegisterServiceHandler(int64_t typeId, const Safir::Dob::Typesystem::HandlerId &handler, bool pending) override;
    void Unregister(int64_t typeId) override;

    void SendMessage(const Safir::Dob::MessagePtr &message, const Safir::Dob::Typesystem::ChannelId &channel) override;
    void SendServiceRequest(const Safir::Dob::ServicePtr &request, const Safir::Dob::Typesystem::HandlerId &handler) override;

    void CreateRequest(const Safir::Dob::EntityPtr &entity, const Safir::Dob::Typesystem::InstanceId &instance, const Safir::Dob::Typesystem::HandlerId &handler) override;
    void UpdateRequest(const Safir::Dob::EntityPtr &entity, const Safir::Dob::Typesystem::InstanceId &instance) override;
    void DeleteRequest(const Safir::Dob::Typesystem::EntityId &entityId) override;

    void SetChanges(const Safir::Dob::EntityPtr &entity, const Safir::Dob::Typesystem::InstanceId &instance, const Safir::Dob::Typesystem::HandlerId &handler) override;
    void SetAll(const Safir::Dob::EntityPtr& entity, const sdt::InstanceId& instance, const sdt::HandlerId& handler) override;
    void Delete(const Safir::Dob::Typesystem::EntityId &entityId, const Safir::Dob::Typesystem::HandlerId &handler) override;
    void DeleteAll(int64_t typeId, const Safir::Dob::Typesystem::HandlerId &handler) override;

signals:
    // For internal use only. Signals can't be declared private. All public signals are declared in DobInterface.
    void DispatchSignal();

private:
    Safir::Dob::InstanceIdPolicy::Enumeration GetInstanceIdPolicy(int64_t typeId, const sdt::HandlerId& handler) const;

    // Dispatcher interface
    void OnDoDispatch() override;

    // StopHandler interface
    void OnStopOrder() override;

    // EntityRequestBase interface
    void OnCreateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy, Safir::Dob::ResponseSenderPtr responseSender) override;
    void OnUpdateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy, Safir::Dob::ResponseSenderPtr responseSender) override;
    void OnDeleteRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy, Safir::Dob::ResponseSenderPtr responseSender) override;

    // RevokedRegistrationBase interface
    void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId typeId, const Safir::Dob::Typesystem::HandlerId &handlerId) override;

    // EntityInjectionBase interface
    void OnInjectedNewEntity(const Safir::Dob::InjectedEntityProxy injectedEntityProxy) override;
    void OnInjectedUpdatedEntity(const Safir::Dob::InjectedEntityProxy injectedEntityProxy) override;
    void OnInjectedDeletedEntity(const Safir::Dob::InjectedEntityProxy injectedEntityProxy) override;
    void OnInitialInjectionsDone(const Safir::Dob::Typesystem::TypeId typeId, const Safir::Dob::Typesystem::HandlerId &handlerId) override;

    // CompletedRegistrationBase interface
    void OnCompletedRegistration(const Safir::Dob::Typesystem::TypeId typeId, const Safir::Dob::Typesystem::HandlerId &handlerId) override;

    // ServiceRequestBase interface
    void OnServiceRequest(const Safir::Dob::ServiceRequestProxy serviceRequestProxy, Safir::Dob::ResponseSenderPtr responseSender) override;

    // Requestor interface
    void OnResponse(const Safir::Dob::ResponseProxy responseProxy) override;
    void OnNotRequestOverflow() override;

    // MessageSender interface
    void OnNotMessageOverflow() override;

    // RegistrationSubscriber interface
    void OnRegistered(const Safir::Dob::Typesystem::TypeId typeId, const Safir::Dob::Typesystem::HandlerId &handlerId) override;
    void OnUnregistered(const Safir::Dob::Typesystem::TypeId typeId, const Safir::Dob::Typesystem::HandlerId &handlerId) override;

    // MessageSubscriber interface
    void OnMessage(const Safir::Dob::MessageProxy messageProxy) override;

    // EntitySubscriber interface
    void OnNewEntity(const Safir::Dob::EntityProxy entityProxy) override;
    void OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy) override;
    void OnDeletedEntity(const Safir::Dob::EntityProxy entityProxy, const bool /*deprecated*/) override;

    Safir::Dob::Connection m_dobConnection;
    std::atomic_flag m_isDispatchSignalled;
};
