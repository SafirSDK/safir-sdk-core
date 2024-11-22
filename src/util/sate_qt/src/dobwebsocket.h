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

#include <QtWebSockets/QWebSocket>
#include <QUrl>

class QJsonObject;

class DobWebSocket : public DobInterface
{
    Q_OBJECT
public:

    DobWebSocket(const QString& address, int port);

    void Open(const QString& name, int context) override;
    void Close() override;

    void SubscribeMessage(int64_t typeId, const sdt::ChannelId& channel, bool includeSubclasses) override;
    void UnsubscribeMessage(int64_t typeId) override;

    void SubscribeEntity(int64_t typeId, const Safir::Dob::Typesystem::InstanceId& instance, bool includeSubclasses) override;
    void UnsubscribeEntity(int64_t typeId) override;

    void SubscribeRegistrations(int64_t typeId, const Safir::Dob::Typesystem::HandlerId& handler, bool includeSubclasses) override;
    void UnsubscribeRegistrations(int64_t typeId) override;

    void RegisterEntityHandler(int64_t typeId, const Safir::Dob::Typesystem::HandlerId& handler, Safir::Dob::InstanceIdPolicy::Enumeration instanceIdPolicy, bool pending, bool injection) override;
    void RegisterServiceHandler(int64_t typeId, const Safir::Dob::Typesystem::HandlerId& handler, bool pending) override;
    void Unregister(int64_t typeId) override;

    void SendMessage(const Safir::Dob::MessagePtr& message, const Safir::Dob::Typesystem::ChannelId& channel) override;
    void SendServiceRequest(const Safir::Dob::ServicePtr& request, const Safir::Dob::Typesystem::HandlerId& handler) override;

    void CreateRequest(const Safir::Dob::EntityPtr& entity, const Safir::Dob::Typesystem::InstanceId& instance, const Safir::Dob::Typesystem::HandlerId& handler) override;
    void UpdateRequest(const Safir::Dob::EntityPtr& entity, const Safir::Dob::Typesystem::InstanceId& instance) override;
    void DeleteRequest(const Safir::Dob::Typesystem::EntityId& entityId) override;

    void SetChanges(const Safir::Dob::EntityPtr& entity, const Safir::Dob::Typesystem::InstanceId& instance, const Safir::Dob::Typesystem::HandlerId& handler) override;
    void SetAll(const Safir::Dob::EntityPtr& entity, const sdt::InstanceId& instance, const sdt::HandlerId& handler) override;
    void Delete(const Safir::Dob::Typesystem::EntityId& entityId, const Safir::Dob::Typesystem::HandlerId& handler) override;
    void DeleteAll(int64_t typeId, const Safir::Dob::Typesystem::HandlerId& handler) override;

private:
    QWebSocket m_webSocket;
    QUrl m_url;
    bool m_isConnected = false;
    bool m_reconnect = false;
    QString m_name = "sate";
    int m_context = 0;

    void WsConnected();
    void WsDisconnected();
    void WsRecv(const QString& data);

    void HandleResult(const QJsonObject& j);
    void HandleNotification(const QJsonObject& j);
    void HandleError(const QJsonObject& j);

    void Send(const QJsonObject& j);
    void Error(const QString& msg);
};
