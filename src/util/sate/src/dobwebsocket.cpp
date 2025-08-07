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
#include "dobwebsocket.h"
#include "dobcalltojson.h"
#include <QJsonObject>
#include <QJsonArray>
#include <QJsonDocument>
#include <QtConcurrent/QtConcurrent>
#include <QDebug>
#include <QTimer>

#include <Safir/Dob/Typesystem/Exceptions.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Dob/Response.h>
#include <Safir/Dob/Entity.h>
#include <Safir/Dob/Message.h>
#include <Safir/Dob/Service.h>
#include <Safir/Dob/SuccessResponse.h>

namespace
{
    template <class T>
    T ToHashVal(const QString& val)
    {
        bool ok = false;
        auto num = val.toLongLong(&ok);
        if (ok)
        {
            return T(num);
        }

        return T(val.toStdWString());
    }

    template <class T>
    void SetHashVal(QJsonObject& jobj, const QString& name, const T& val)
    {
        if (val.GetRawString().empty())
        {
            jobj[name] = static_cast<qint64>(val.GetRawValue());
        }
        else
        {
            jobj[name] = QString::fromStdWString(val.GetRawString());
        }
    }

    template <class T>
    T JsonToHash(const QJsonValueRef& val)
    {
        if (val.isString())
        {
            return T(val.toString().toStdWString());
        }
        
        return T(val.toVariant().toLongLong());
    }

    QUrl ToUrl(const QString& address, int port)
    {
        QString url = (address.startsWith("ws") ? address : QString("ws://%1").arg(address)) + ":" + QString::number(port);
        return QUrl(url);
    }

    QString Str(int64_t typeId)
    {
        try
        {
            auto name = sdt::Operations::GetName(typeId);
            return QString::fromStdWString(name);
        }
        catch (const sdt::IllegalValueException&)
        {
            return QString();
        }
    }

    QString Str(const std::wstring& s) { return QString::fromStdWString(s);}

    QString Str(const QJsonObject& j) { return QString(QJsonDocument(j).toJson(QJsonDocument::Compact)); }

    QJsonObject ToJsonObject(const Safir::Dob::Typesystem::ObjectPtr& ptr)
    {
        return QJsonDocument::fromJson(QString::fromStdWString(sdt::Serialization::ToJson(ptr)).toUtf8()).object();
    }
    sdt::ObjectPtr ToObjectPtr(const QJsonObject& jobj)
    {
        return sdt::Serialization::ToObjectFromJson(Str(jobj).toStdWString());
    }


}

DobWebSocket::DobWebSocket(const QString& address, int port)
    : m_webSocket()
    , m_url(ToUrl(address, port))
{
    connect(&m_webSocket, &QWebSocket::connected, this, &DobWebSocket::WsConnected);
    connect(&m_webSocket, &QWebSocket::disconnected, this, &DobWebSocket::WsDisconnected);
    connect(&m_webSocket, &QWebSocket::textMessageReceived, this, &DobWebSocket::WsRecv);
}

void DobWebSocket::WsConnected()
{
    m_isConnected = true;
    m_hasReportedConnectionProblem = false;

    emit DobInterface::Output("Connected to websocket " + m_url.toString(), QtInfoMsg);
    QJsonObject j = DobCallToJson::Open(m_name, m_context);
    Send(j);
}

void DobWebSocket::WsDisconnected()
{
    m_isConnected = false;
    emit DobInterface::ConnectionClosed();

    if (m_reconnect)
    {
        if (!m_hasReportedConnectionProblem)
        {
            m_hasReportedConnectionProblem = true;
            LogError("Websocket connection closed. Try to reconnect to " + m_url.toString());
        }
        // We did not explicitly call close, then try to reconnect.
        QTimer::singleShot(1000, [this]{m_webSocket.open(m_url);});
    }
    else
    {
        emit DobInterface::Output("Disconnected from DOB websocket on " + m_url.toString(), QtInfoMsg);
    }
}

void DobWebSocket::WsRecv(const QString& data)
{
    emit DobInterface::Output("WsRecv: " + data, QtDebugMsg);

    QJsonDocument doc = QJsonDocument::fromJson(data.toUtf8());
    if (doc.isNull() || !doc.isObject())
    {
        LogError("Recv unexpected data: " + data);
        return;
    }
    auto j = doc.object();

    if (j.contains("method"))
    {
        // Notification, spontaneous data, i.e onMessage or onNewEntity.
        HandleNotification(j);
    }
    else if (j.contains("result"))
    {
        // Operation result, result of a method call by Sate.
        HandleResult(j);
    }
    else if (j.contains("error"))
    {
        // Operation result, result of a method call by Sate.
        HandleError(j);
    }
    else
    {
        LogError("Recv unexpected data: " + data);
    }
}

bool DobWebSocket::IsOpen() const
{
    return m_isConnected;
}

void DobWebSocket::Open(const QString& name, int context)
{
    if (m_isConnected)
    {
        emit DobInterface::Output("Already connected to " + m_url.toString(), QtInfoMsg);
        return;
    }

    m_name = name;
    m_context = context;
    m_reconnect = true;
    emit DobInterface::Output("Trying to connect to websocket " + m_url.toString(), QtInfoMsg);
    m_webSocket.open(m_url);
}

void DobWebSocket::Close()
{
    m_reconnect = false;
    m_webSocket.close();
}

void DobWebSocket::SubscribeMessage(int64_t typeId, const sdt::ChannelId& channel, bool includeSubclasses)
{
    if (!m_isConnected)
    {
        LogError("Not connected!");
        return;
    }

    QJsonObject j = DobCallToJson::SubscribeMessage(typeId, channel, includeSubclasses);
    Send(j);
}

void DobWebSocket::UnsubscribeMessage(int64_t typeId)
{
    if (!m_isConnected)
    {
        LogError("Not connected!");
        return;
    }

    QJsonObject j = DobCallToJson::UnsubscribeMessage(typeId);
    Send(j);
}

void DobWebSocket::SubscribeEntity(int64_t typeId, const Safir::Dob::Typesystem::InstanceId& instance, bool includeSubclasses)
{
    if (!m_isConnected)
    {
        LogError("Not connected!");
        return;
    }

    QJsonObject j = DobCallToJson::SubscribeEntity(typeId, instance, includeSubclasses);
    Send(j);
}

void DobWebSocket::UnsubscribeEntity(int64_t typeId)
{
    if (!m_isConnected)
    {
        LogError("Not connected!");
        return;
    }

    QJsonObject j = DobCallToJson::UnsubscribeEntity(typeId);
    Send(j);
}

void DobWebSocket::SubscribeRegistrations(int64_t typeId, const Safir::Dob::Typesystem::HandlerId& handler, bool includeSubclasses)
{
    if (!m_isConnected)
    {
        LogError("Not connected!");
        return;
    }

    QJsonObject j = DobCallToJson::SubscribeRegistrations(typeId, handler, includeSubclasses);
    Send(j);
}

void DobWebSocket::UnsubscribeRegistrations(int64_t typeId)
{
    if (!m_isConnected)
    {
        LogError("Not connected!");
        return;
    }

    QJsonObject j = DobCallToJson::UnsubscribeRegistrations(typeId);
    Send(j);
}

void DobWebSocket::RegisterEntityHandler(int64_t typeId, const Safir::Dob::Typesystem::HandlerId& handler, Safir::Dob::InstanceIdPolicy::Enumeration instanceIdPolicy, bool pending, bool injection)
{
    if (!m_isConnected)
    {
        LogError("Not connected!");
        return;
    }

    QJsonObject j = DobCallToJson::RegisterEntityHandler(typeId, handler, instanceIdPolicy, pending, injection);
    Send(j);
}

void DobWebSocket::RegisterServiceHandler(int64_t typeId, const Safir::Dob::Typesystem::HandlerId& handler, bool pending)
{
    if (!m_isConnected)
    {
        LogError("Not connected!");
        return;
    }

    QJsonObject j = DobCallToJson::RegisterServiceHandler(typeId, handler, pending);
    Send(j);
}

void DobWebSocket::Unregister(int64_t typeId)
{
    if (!m_isConnected)
    {
        LogError("Not connected!");
        return;
    }

    QJsonObject j = DobCallToJson::Unregister(typeId);
    Send(j);
}

void DobWebSocket::SendMessage(const Safir::Dob::MessagePtr& message, const Safir::Dob::Typesystem::ChannelId& channel)
{
    if (!m_isConnected)
    {
        LogError("Not connected!");
        return;
    }

    QJsonObject j = DobCallToJson::SendMessage(message, channel);
    Send(j);
}

void DobWebSocket::SendServiceRequest(const Safir::Dob::ServicePtr& request, const Safir::Dob::Typesystem::HandlerId& handler)
{
    if (!m_isConnected)
    {
        LogError("Not connected!");
        return;
    }

    QJsonObject j = DobCallToJson::SendServiceRequest(request, handler);
    Send(j);
}

void DobWebSocket::CreateRequest(const Safir::Dob::EntityPtr& entity, const Safir::Dob::Typesystem::InstanceId& instance, const Safir::Dob::Typesystem::HandlerId& handler)
{
    if (!m_isConnected)
    {
        LogError("Not connected!");
        return;
    }

    QJsonObject j = DobCallToJson::CreateRequest(entity, instance, handler);
    Send(j);
}

void DobWebSocket::UpdateRequest(const Safir::Dob::EntityPtr& entity, const Safir::Dob::Typesystem::InstanceId& instance)
{
    if (!m_isConnected)
    {
        LogError("Not connected!");
        return;
    }

    QJsonObject j = DobCallToJson::UpdateRequest(entity, instance);
    Send(j);
}

void DobWebSocket::DeleteRequest(const Safir::Dob::Typesystem::EntityId& entityId)
{
    if (!m_isConnected)
    {
        LogError("Not connected!");
        return;
    }

    QJsonObject j = DobCallToJson::DeleteRequest(entityId);
    Send(j);
}

void DobWebSocket::SetChanges(const Safir::Dob::EntityPtr& entity, const Safir::Dob::Typesystem::InstanceId& instance, const Safir::Dob::Typesystem::HandlerId& handler)
{
    if (!m_isConnected)
    {
        LogError("Not connected!");
        return;
    }

    QJsonObject j = DobCallToJson::SetChanges(entity, instance, handler);
    Send(j);
}

void DobWebSocket::SetAll(const Safir::Dob::EntityPtr& entity, const sdt::InstanceId& instance, const sdt::HandlerId& handler)
{
    if (!m_isConnected)
    {
        LogError("Not connected!");
        return;
    }

    QJsonObject j = DobCallToJson::SetAll(entity, instance, handler);
    Send(j);
}

void DobWebSocket::Delete(const Safir::Dob::Typesystem::EntityId& entityId, const Safir::Dob::Typesystem::HandlerId& handler)
{
    if (!m_isConnected)
    {
        LogError("Not connected!");
        return;
    }

    QJsonObject j = DobCallToJson::Delete(entityId, handler);
    Send(j);
}

void DobWebSocket::DeleteAll(int64_t typeId, const Safir::Dob::Typesystem::HandlerId& handler)
{
    if (!m_isConnected)
    {
        LogError("Not connected!");
        return;
    }

    QJsonObject j = DobCallToJson::DeleteAll(typeId, handler);
    Send(j);
}

void DobWebSocket::ReadEntity(const sdt::EntityId& entityId)
{
    if (!m_isConnected)
    {
        LogError("Not connected!");
        return;
    }

    auto typeName = Str(entityId.GetTypeId());
    if (typeName.isEmpty())
    {
        LogError("Failed to read entity! The specified typeId doesn't exist. EntityId: " + QString::fromStdWString(entityId.ToString()));
        return;
    }

    QJsonObject j = DobCallToJson::ReadEntity(entityId);
    Send(j);
}

void DobWebSocket::HandleResult(const QJsonObject& j)
{
    auto idValue = j["id"];
    if (idValue.isNull())
    {
        return; // Anonymous result
    }

    auto idList = idValue.toString().split(';', Qt::SkipEmptyParts);
    auto id = idList[0];
    auto result = j["result"];

    if (id == "open")
    {
        if (result.isString() && result.toString() == "OK")
        {
            emit DobInterface::ConnectedToDob(m_name);
            emit DobInterface::Output("Connected to DOB via websocket!", QtWarningMsg);
        }
        else
        {
            LogError("Open failed: " + result.toString());
        }
    }
    else if (id == "sendMessage")
    {
        if (result.isString() && result.toString() == "OK")
        {
            emit DobInterface::Output(QString("Message sent OK: %1, channel=%2").arg(idList[1], idList[2]), QtInfoMsg);
        }
        else
        {
            LogError("sendMessage failed: " + result.toString());
        }
    }
    else if (id == "setEntityChanges")
    {
        if (result.isString() && result.toString() == "OK")
        {
            emit DobInterface::Output(QString("Set entity changes OK: %1, instance=%2, handler=%3").arg(idList[1], idList[2], idList[3]), QtInfoMsg);
        }
        else
        {
            LogError("SetEntityChanges failed: " + result.toString());
        }
    }
    else if (id == "setEntity")
    {
        if (result.isString() && result.toString() == "OK")
        {
            emit DobInterface::Output(QString("Set entity OK: %1, instance=%2, handler=%3").arg(idList[1], idList[2], idList[3]), QtInfoMsg);
        }
        else
        {
            LogError("SetEntity failed: " + result.toString());
        }
    }
    else if (id == "deleteEntity")
    {
        if (result.isString() && result.toString() == "OK")
        {
            emit DobInterface::Output(QString("Delete entity OK: %1, instance=%2, handler=%3").arg(idList[1], idList[2], idList[3]), QtInfoMsg);
        }
        else
        {
            LogError("DeleteEntity failed: " + result.toString());
        }
    }
    else if (id == "deleteAllInstances")
    {
        if (result.isString() && result.toString() == "OK")
        {
            emit DobInterface::Output(QString("Delete all instances OK: %1, handler=%2").arg(idList[1], idList[2]), QtInfoMsg);
        }
        else
        {
            LogError("DeleteAllInstances failed: " + result.toString());
        }
    }
    else if (id == "createRequest")
    {
        auto resp = result["response"];
        if (!resp.isUndefined() && resp.isObject())
        {
            auto response = std::dynamic_pointer_cast<Safir::Dob::Response>(ToObjectPtr(resp.toObject()));
            emit DobInterface::OnResponse(response);
            emit DobInterface::Output(QString("Received response for entity create request, handler=%1").arg(idList[1]), QtInfoMsg);
        }
        else
        {
            LogError("CreateRequest failed: " + Str(j));
        }
    }
    else if (id == "updateRequest")
    {
        auto resp = result["response"];
        if (!resp.isUndefined() && resp.isObject())
        {
            auto response = std::dynamic_pointer_cast<Safir::Dob::Response>(ToObjectPtr(resp.toObject()));
            emit DobInterface::OnResponse(response);
            emit DobInterface::Output(QString("Received response for entity update request, instance=%1").arg(idList[1]), QtInfoMsg);
        }
        else
        {
            LogError("UpdateRequest failed: " + Str(j));
        }
    }
    else if (id == "deleteRequest")
    {
        auto resp = result["response"];
        if (!resp.isUndefined() && resp.isObject())
        {
            auto response = std::dynamic_pointer_cast<Safir::Dob::Response>(ToObjectPtr(resp.toObject()));
            emit DobInterface::OnResponse(response);
            emit DobInterface::Output(QString("Received response for entity delete request: %1, instance=%2").arg(idList[1], idList[2]), QtInfoMsg);
        }
        else
        {
            LogError("DeleteRequest failed: " + Str(j));
        }
    }
    else if (id == "serviceRequest")
    {
        auto resp = result["response"];
        if (!resp.isUndefined() && resp.isObject())
        {
            auto response = std::dynamic_pointer_cast<Safir::Dob::Response>(ToObjectPtr(resp.toObject()));
            emit DobInterface::OnResponse(response);
            emit DobInterface::Output(QString("Received response for service request, handler=%1").arg(idList[1]), QtInfoMsg);
        }
        else
        {
            LogError("ServiceRequest failed: " + Str(j));
        }
    }
    else if (id == "subscribeMessage")
    {
        if (result.isString() && result.toString() == "OK")
        {
            auto typeId = sdt::Operations::GetTypeId(idList[1].toStdWString());
            auto channel = ToHashVal<sdt::ChannelId>(idList[2]);
            auto includeSubclasses = idList[3] == "1";
            DobInterface::SubscriptionInfo info{typeId, channel, includeSubclasses};
            m_subscriptions.push_back(info);
            emit DobInterface::SubscriptionStarted(info);
            emit DobInterface::Output(QString("Subscribe message OK: %1, channel=%2, recursive=%3").arg(idList[1], idList[2], idList[3]), QtInfoMsg);
        }
        else
        {
            LogError("SubscribeMessage failed: " + result.toString());
        }
    }
    else if (id == "unsubscribeMessage")
    {
        if (result.isString() && result.toString() == "OK")
        {
            auto typeId = sdt::Operations::GetTypeId(idList[1].toStdWString());
            RemoveSubscriptions(typeId);
            emit DobInterface::SubscriptionStopped(typeId);
            emit DobInterface::Output(QString("Unsubscribe message OK: %1").arg(idList[1]), QtInfoMsg);
        }
        else
        {
            LogError("UnsubscribeMessage failed: " + result.toString());
        }
    }
    else if (id == "subscribeEntity")
    {
        if (result.isString() && result.toString() == "OK")
        {
            auto typeId = sdt::Operations::GetTypeId(idList[1].toStdWString());
            auto includeSubclasses = idList[2] == "1";
            DobInterface::SubscriptionInfo info{typeId, sdt::ChannelId(), includeSubclasses};
            m_subscriptions.push_back(info);
            emit DobInterface::SubscriptionStarted(info);
            AddInstanceCounter(typeId,includeSubclasses);
            emit DobInterface::Output(QString("Subscribe entity OK: %1, recursvie=%2").arg(idList[1], idList[2]), QtInfoMsg);
        }
        else
        {
            LogError("subscribeEntity failed: " + result.toString());
        }
    }
    else if (id == "unsubscribeEntity")
    {
        if (result.isString() && result.toString() == "OK")
        {
            auto typeId = sdt::Operations::GetTypeId(idList[1].toStdWString());
            RemoveSubscriptions(typeId);
            RemoveInstanceCounterRecursively(typeId);
            emit DobInterface::SubscriptionStopped(typeId);
            emit DobInterface::Output(QString("Unsubscribe entity OK: %1").arg(idList[1]), QtInfoMsg);
        }
        else
        {
            LogError("UnsubscribeEntity failed: " + result.toString());
        }
    }
    else if (id == "subscribeRegistration")
    {
        if (result.isString() && result.toString() == "OK")
        {
            emit DobInterface::Output(QString("Subscribe registrations OK: %1, recursvie=%2").arg(idList[1], idList[2]), QtInfoMsg);
        }
        else
        {
            LogError("SubscribeRegistration failed: " + result.toString());
        }
    }
    else if (id == "unsubscribeRegistration")
    {
        if (result.isString() && result.toString() == "OK")
        {
            emit DobInterface::Output(QString("Unsubscribe registration OK: %1").arg(idList[1]), QtInfoMsg);
        }
        else
        {
            LogError("UnsubscribeRegistration failed: " + result.toString());
        }
    }
    else if (id == "registerEntityHandler")
    {
        // j["id"] = QString("registerEntityHandler;%1;%2;%3;%4;%5").arg(Str(typeId), Str(handler.ToString()), instPolicy, (pending ? "1" : "0"), (injection ? "1" : "0"));
        if (result.isString() && result.toString() == "OK")
        {
            auto msg = QString("Register entity handler OK: %1, handler=%2, %3").arg(idList[1], idList[2], idList[3]);
            if (idList[4] == "1")
            {
                msg += ", pending";
            }
            if (idList[5] == "1")
            {
                msg += ", injectionHandler";
            }
            emit DobInterface::Output(msg, QtInfoMsg);


            auto typeId = sdt::Operations::GetTypeId(idList[1].toStdWString());
            auto handler = ToHashVal<sdt::HandlerId>(idList[2]);
            auto instanceIdPolicy = Safir::Dob::InstanceIdPolicy::ToValue(idList[3].toStdWString());
            auto pending = idList[4] == "1";
            auto injection = idList[5] == "1";
            DobInterface::RegistrationInfo info{typeId, handler, pending, injection, instanceIdPolicy};
            m_registrations.push_back(info);
            emit DobInterface::OnRegistered(info);
        }
        else
        {
            LogError("RegisterEntityHandler failed: " + result.toString());
        }
    }
    else if (id == "registerServiceHandler")
    {
        if (result.isString() && result.toString() == "OK")
        {
            auto msg = QString("Register service handler OK: %1, handler=%2").arg(idList[1], idList[2]);
            if (idList[3] == "1")
            {
                msg += ", pending";
            }
            emit DobInterface::Output(msg, QtInfoMsg);

            auto typeId = sdt::Operations::GetTypeId(idList[1].toStdWString());
            auto handler = ToHashVal<sdt::HandlerId>(idList[2]);
            auto pending = idList[3] == "1";
            DobInterface::RegistrationInfo info{typeId, handler, pending, false, Safir::Dob::InstanceIdPolicy::RequestorDecidesInstanceId};
            m_registrations.push_back(info);
            emit DobInterface::OnRegistered(info);
        }
        else
        {
            LogError("RegisterServiceHandler failed: " + result.toString());
        }
    }
    else if (id == "unregisterHandler")
    {
        if (result.isString() && result.toString() == "OK")
        {
            auto typeId = sdt::Operations::GetTypeId(idList[1].toStdWString());
            RemoveRegistrations(typeId);
            emit DobInterface::OnUnregistered(typeId);

            emit DobInterface::Output(QString("Unregister handler OK: %1").arg(idList[1]), QtInfoMsg);
        }
        else
        {
            LogError("UnregisterHandler failed: " + result.toString());
        }
    }
    else if (id == "readEntity")
    {
        if (result.isObject())
        {
            auto instance = ToHashVal<sdt::InstanceId>(idList[2]);
            auto entity = std::dynamic_pointer_cast<Safir::Dob::Entity>(ToObjectPtr(result.toObject()));
            emit DobInterface::OnReadEntity(entity, instance);
        }
        else
        {
            LogError("ReadEntity failed: " + result.toString());
        }
    }
}

void DobWebSocket::HandleNotification(const QJsonObject& j)
{
    auto method = j["method"].toString();
    auto p = j["params"].toObject();

    if (method == "onMessage")
    {
        auto channel = JsonToHash<sdt::ChannelId>(p["channelId"]);
        auto message = std::dynamic_pointer_cast<Safir::Dob::Message>(ToObjectPtr(p["message"].toObject()));
        emit DobInterface::OnMessage(channel, message);

    }
    else if (method == "onUpdatedEntity")
    {
        auto instance = JsonToHash<sdt::InstanceId>(p["instanceId"]);
        auto entity = std::dynamic_pointer_cast<Safir::Dob::Entity>(ToObjectPtr(p["entity"].toObject()));
        sdt::EntityId eid(entity->GetTypeId(), instance);
        emit DobInterface::OnEntity(eid, sdt::HandlerId(), entity, DobInterface::UpdatedEntity);

    }
    else if (method == "onNewEntity")
    {
        auto instance = JsonToHash<sdt::InstanceId>(p["instanceId"]);
        auto entity = std::dynamic_pointer_cast<Safir::Dob::Entity>(ToObjectPtr(p["entity"].toObject()));
        sdt::EntityId eid(entity->GetTypeId(), instance);
        IncreaseInstanceCounter(eid);
        emit DobInterface::OnEntity(eid, sdt::HandlerId(), entity, DobInterface::NewEntity);
    }
    else if (method == "onDeletedEntity")
    {
        auto instance = JsonToHash<sdt::InstanceId>(p["instanceId"]);
        auto entity = std::dynamic_pointer_cast<Safir::Dob::Entity>(ToObjectPtr(p["entity"].toObject()));
        sdt::EntityId eid(entity->GetTypeId(), instance);
        DecreaseInstanceCounter(eid);
        emit DobInterface::OnEntity(eid, sdt::HandlerId(), nullptr, DobInterface::DeletedEntity);
    }
    else if (method == "onUpdateRequest")
    {
        auto handler = JsonToHash<sdt::HandlerId>(p["handlerId"]);
        auto instance = JsonToHash<sdt::InstanceId>(p["instanceId"]);
        auto entity = std::dynamic_pointer_cast<Safir::Dob::Entity>(ToObjectPtr(p["entity"].toObject()));

        emit DobInterface::OnUpdateRequest(entity, handler, instance);

        SetChanges(entity, instance, handler);

        // Create response
        auto success = Safir::Dob::SuccessResponse::Create();
        QJsonObject response;
        response["id"] = j["id"];
        response["result"] = ToJsonObject(success);
        Send(response);
    }
    else if (method == "onCreateRequest")
    {
        auto handler = JsonToHash<sdt::HandlerId>(p["handlerId"]);
        auto entity = std::dynamic_pointer_cast<Safir::Dob::Entity>(ToObjectPtr(p["entity"].toObject()));
        auto instance = p.contains("instanceId") ? JsonToHash<sdt::InstanceId>(p["instanceId"]) : sdt::InstanceId::GenerateRandom();

        emit DobInterface::OnCreateRequest(entity, handler, instance);

        SetAll(entity, instance, handler);

        // Create response
        auto success = Safir::Dob::SuccessResponse::Create();
        QJsonObject response;
        response["id"] = j["id"];
        response["result"] = ToJsonObject(success);
        Send(response);
    }
    else if (method == "onDeleteRequest")
    {
        auto handler = JsonToHash<sdt::HandlerId>(p["handlerId"]);
        auto typeId = sdt::Operations::GetTypeId(p["typeId"].toString().toStdWString());
        auto instance = JsonToHash<sdt::InstanceId>(p["instanceId"]);
        sdt::EntityId eid(typeId, instance);

        emit DobInterface::OnDeleteRequest(eid, handler);

        Delete(eid, handler);

        // Create response
        auto success = Safir::Dob::SuccessResponse::Create();
        QJsonObject response;
        response["id"] = j["id"];
        response["result"] = ToJsonObject(success);
        Send(response);
    }
    else if (method == "onServiceRequest")
    {
        auto handler = JsonToHash<sdt::HandlerId>(p["handlerId"]);
        auto request = std::dynamic_pointer_cast<Safir::Dob::Service>(ToObjectPtr(p["request"].toObject()));

        emit DobInterface::OnServiceRequest(request, handler);

        // Create response
        auto success = Safir::Dob::SuccessResponse::Create();
        QJsonObject response;
        response["id"] = j["id"];
        response["result"] = ToJsonObject(success);
        Send(response);
    }
    else if (method == "onRegistered")
    {
        auto handler = JsonToHash<sdt::HandlerId>(p["handlerId"]);
        auto typeId = sdt::Operations::GetTypeId(p["typeId"].toString().toStdWString());

        emit DobInterface::Output("OnRegistered: " + Str(typeId) + ", handler=" + Str(handler.ToString()), QtInfoMsg);
        DobInterface::RegistrationInfo info{typeId, handler, false, false, Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId};
        emit DobInterface::OnRegistered(info);
    }
    else if (method == "onUnregistered")
    {
        auto handler = JsonToHash<sdt::HandlerId>(p["handlerId"]);
        auto typeId = sdt::Operations::GetTypeId(p["typeId"].toString().toStdWString());
        emit DobInterface::Output("OnUnregistered: " + Str(typeId) + ", handler=" + Str(handler.ToString()), QtInfoMsg);
        emit DobInterface::OnUnregistered(typeId);
    }
    else if (method == "onRevokedRegistration")
    {
        auto handler = JsonToHash<sdt::HandlerId>(p["handlerId"]);
        auto typeId = sdt::Operations::GetTypeId(p["typeId"].toString().toStdWString());

        emit DobInterface::Output("Revoked registration: " + Str(typeId) + ", handler=" + Str(handler.ToString()), QtInfoMsg);
        RemoveRegistration(typeId, handler);
        emit DobInterface::OnUnregistered(typeId);

    }
    else if (method == "onCompletedRegistration")
    {
        auto handler = JsonToHash<sdt::HandlerId>(p["handlerId"]);
        auto typeId = sdt::Operations::GetTypeId(p["typeId"].toString().toStdWString());
        emit DobInterface::Output("Registration completed: " + Str(typeId) + ", handler=" + Str(handler.ToString()), QtInfoMsg);
        auto reg = DobInterface::GetRegistration(typeId, handler);
        if (reg != nullptr)
        {
            reg->pending = false;
            emit DobInterface::OnRegistered(*reg);
        }
        else
        {
            auto policy = Safir::Dob::InstanceIdPolicy::Enumeration::HandlerDecidesInstanceId;
            DobInterface::RegistrationInfo info{typeId, handler, false, false, policy};
            m_registrations.push_back(info);
        }
    }
    else if (method == "onInitialInjectionsDone")
    {
        auto handler = JsonToHash<sdt::HandlerId>(p["handlerId"]);
        auto typeId = sdt::Operations::GetTypeId(p["typeId"].toString().toStdWString());
        emit DobInterface::Output("Initial injection done: " + Str(typeId) + ", handler=" + Str(handler.ToString()), QtInfoMsg);
    }
    else if (method == "onInjectedNewEntity")
    {
        auto entity = std::dynamic_pointer_cast<Safir::Dob::Entity>(ToObjectPtr(p["entity"].toObject()));
        auto instance = p.contains("instanceId") ? JsonToHash<sdt::InstanceId>(p["instanceId"]) : sdt::InstanceId::GenerateRandom();
        sdt::EntityId eid(entity->GetTypeId(), instance);

        emit DobInterface::Output("Injected new entity: " + Str(eid.ToString()), QtInfoMsg);
        emit DobInterface::OnEntity(eid, sdt::HandlerId(), entity, DobInterface::NewEntity);
    }
    else if (method == "onInjectedUpdatedEntity")
    {
        auto entity = std::dynamic_pointer_cast<Safir::Dob::Entity>(ToObjectPtr(p["entity"].toObject()));
        auto instance = p.contains("instanceId") ? JsonToHash<sdt::InstanceId>(p["instanceId"]) : sdt::InstanceId::GenerateRandom();
        sdt::EntityId eid(entity->GetTypeId(), instance);

        emit DobInterface::Output("Injected updated entity: " + Str(eid.ToString()), QtInfoMsg);
        emit DobInterface::OnEntity(eid, sdt::HandlerId(), entity, DobInterface::UpdatedEntity);
    }
    else if (method == "onInjectedDeletedEntity")
    {
        auto entity = std::dynamic_pointer_cast<Safir::Dob::Entity>(ToObjectPtr(p["entity"].toObject()));
        auto instance = p.contains("instanceId") ? JsonToHash<sdt::InstanceId>(p["instanceId"]) : sdt::InstanceId::GenerateRandom();
        sdt::EntityId eid(entity->GetTypeId(), instance);

        emit DobInterface::Output("Injected deleted entity: " + Str(eid.ToString()), QtInfoMsg);
        emit DobInterface::OnEntity(eid, sdt::HandlerId(), entity, DobInterface::DeletedEntity);
    }
    else if (method == "onNotMessageOverflow")
    {
        emit DobInterface::Output("OnNotMessageOverflow", QtInfoMsg);
    }
    else if (method == "onNotRequestOverflow")
    {
        emit DobInterface::Output("OnNotRequestOverflow", QtInfoMsg);
    }
}

void DobWebSocket::HandleError(const QJsonObject& j)
{
    LogError("Received error: " + Str(j));
}

void DobWebSocket::Send(const QJsonObject& j)
{
    auto json = Str(j);
    emit DobInterface::Output(QString("WsSend: %1").arg(json), QtDebugMsg);
    m_webSocket.sendTextMessage(Str(j));
}

void DobWebSocket::LogError(const QString& msg)
{
    emit DobInterface::Output(msg, QtCriticalMsg);
}
