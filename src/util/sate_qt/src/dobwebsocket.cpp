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
#include <QJsonObject>
#include <QJsonArray>
#include <QJsonDocument>
#include <QtConcurrent/QtConcurrent>
#include <QDebug>
#include <QTimer>

#include <Safir/Dob/Typesystem/Serialization.h>

namespace
{
	QUrl ToUrl(const QString& address, int port)
	{
		QString url = (address.startsWith("ws") ? address : QString("ws://%1").arg(address)) + ":" + QString::number(port);
		return QUrl(url);
	}

    QString Str(int64_t typeId) {return QString::fromStdWString(sdt::Operations::GetName(typeId));}
    QString Str(const std::wstring& s) { return QString::fromStdWString(s);}
    QString Str(const sdt::HandlerId& v) { return QString(", handler=%1").arg(Str(v.ToString())); }
    QString Str(const sdt::ChannelId& v) { return QString(", channel=%1").arg(Str(v.ToString())); }
    QString Str(const sdt::InstanceId& v) { return QString(", instance=%1").arg(Str(v.ToString())); }
    QString Str(const QJsonObject& j) { return QString(QJsonDocument(j).toJson(QJsonDocument::Compact)); }
    QJsonObject ToJsonObject(const Safir::Dob::Typesystem::ObjectPtr& ptr)
    {
        return QJsonDocument::fromJson(QString::fromStdWString(sdt::Serialization::ToJson(ptr)).toUtf8()).object();
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

    emit DobInterface::Info("Connected to websocket " + m_url.toString());
    QJsonObject j;
    j["method"] = "open";
    j["id"] = "open";

    QJsonObject p;
    p["connectionName"] = m_name;
    j["params"] = p;

    Send(j);
}

void DobWebSocket::WsDisconnected()
{
    m_isConnected = false;

    if (m_reconnect)
    {
        emit DobInterface::Info("try reconnect " + m_url.toString());
        // We did not explicitly call close, then try to reconnect.
        QTimer::singleShot(1000, [this]{m_webSocket.open(m_url);});
    }
    else
    {
        emit DobInterface::ConnectionClosed();
        emit DobInterface::Info("Disconnected from DOB websocket on " + m_url.toString());
    }
}

void DobWebSocket::WsRecv(const QString& data)
{
    emit DobInterface::Info("WsRecv: " + data);

    QJsonDocument doc = QJsonDocument::fromJson(data.toUtf8());
    if (doc.isNull() || !doc.isObject())
    {
        Error("Recv unexpected data: " + data);
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
        Error("Recv unexpected data: " + data);
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
        emit DobInterface::Info("Already connected to " + m_url.toString());
        return;
    }

    m_name = name;
    m_context = context;
    m_reconnect = true;
    emit DobInterface::Info("Trying to connect to websocket " + m_url.toString());
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
        Error("Not connected!");
        return;
    }

    // Params
    QJsonObject p;
    p["typeId"] = Str(typeId);
    p["channelId"] = Str(channel);
    p["includeSubclasses"] = includeSubclasses;

    // Method object
    QJsonObject j;
    j["method"] = "subscribeMessage";
    j["id"] = QString("subscribeMessage;%1;%2;%3").arg(Str(typeId), Str(channel), (includeSubclasses ? "1" : "0"));
    j["params"] = p;

    Send(j);
}

void DobWebSocket::UnsubscribeMessage(int64_t typeId)
{
    if (!m_isConnected)
    {
        Error("Not connected!");
        return;
    }

    // Params
    QJsonObject p;
    p["typeId"] = Str(typeId);

    // Method object
    QJsonObject j;
    j["method"] = "unsubscribeMessage";
    j["id"] = QString("unsubscribeMessage;%1;").arg(Str(typeId));
    j["params"] = p;

    Send(j);
}

void DobWebSocket::SubscribeEntity(int64_t typeId, const Safir::Dob::Typesystem::InstanceId& instance, bool includeSubclasses)
{
    if (!m_isConnected)
    {
        Error("Not connected!");
        return;
    }

    // Params
    QJsonObject p;
    p["typeId"] = Str(typeId);

    if (instance == sdt::InstanceId())
    {
        p["instanceId"] = Str(instance);

    }
    else
    {
        p["includeSubclasses"] = includeSubclasses;
    }

    // Method object
    QJsonObject j;
    j["method"] = "subscribeEntity";
    j["id"] = QString("subscribeEntity;%1;%2").arg(Str(typeId), (includeSubclasses ? "1" : "0"));
    j["params"] = p;

    Send(j);
}

void DobWebSocket::UnsubscribeEntity(int64_t typeId)
{
    if (!m_isConnected)
    {
        Error("Not connected!");
        return;
    }

    // Params
    QJsonObject p;
    p["typeId"] = Str(typeId);

    // Method object
    QJsonObject j;
    j["method"] = "unsubscribeEntity";
    j["id"] = QString("unsubscribeEntity;%1;").arg(Str(typeId));
    j["params"] = p;

    Send(j);
}

void DobWebSocket::SubscribeRegistrations(int64_t typeId, const Safir::Dob::Typesystem::HandlerId& handler, bool includeSubclasses)
{
    if (!m_isConnected)
    {
        Error("Not connected!");
        return;
    }

    // Params
    QJsonObject p;
    p["typeId"] = Str(typeId);
    p["handlerId"] = Str(handler);
    p["includeSubclasses"] = includeSubclasses;

    // Method object
    QJsonObject j;
    j["method"] = "subscribeRegistration";
    j["id"] = QString("subscribeRegistration;%1;%2").arg(Str(typeId), (includeSubclasses ? "1" : "0"));
    j["params"] = p;

    Send(j);
}

void DobWebSocket::UnsubscribeRegistrations(int64_t typeId)
{
    if (!m_isConnected)
    {
        Error("Not connected!");
        return;
    }

    // Params
    QJsonObject p;
    p["typeId"] = Str(typeId);

    // Method object
    QJsonObject j;
    j["method"] = "unsubscribeRegistration";
    j["id"] = QString("unsubscribeRegistration;%1;").arg(Str(typeId));
    j["params"] = p;

    Send(j);
}

void DobWebSocket::RegisterEntityHandler(int64_t typeId, const Safir::Dob::Typesystem::HandlerId& handler, Safir::Dob::InstanceIdPolicy::Enumeration instanceIdPolicy, bool pending, bool injection)
{
    if (!m_isConnected)
    {
        Error("Not connected!");
        return;
    }

    // Params
    auto instPolicy = instanceIdPolicy == Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId ? "HandlerDecidesInstanceId" : "RequestorDecidesInstanceId";
    QJsonObject p;
    p["typeId"] = Str(typeId);
    p["handlerId"] = Str(handler);
    p["instanceIdPolicy"] = instPolicy;
    p["pending"] = pending;
    p["injectionHandler"] = injection;

    // Method object
    QJsonObject j;
    j["method"] = "registerEntityHandler";
    j["id"] = QString("registerEntityHandler;%1;%2;%3;%4;%5").arg(Str(typeId), Str(handler), instPolicy, (pending ? "1" : "0"), (injection ? "1" : "0"));
    j["params"] = p;

    Send(j);
}

void DobWebSocket::RegisterServiceHandler(int64_t typeId, const Safir::Dob::Typesystem::HandlerId& handler, bool pending)
{
    if (!m_isConnected)
    {
        Error("Not connected!");
        return;
    }

    // Params
    QJsonObject p;
    p["typeId"] = Str(typeId);
    p["handlerId"] = Str(handler);
    p["pending"] = pending;

    // Method object
    QJsonObject j;
    j["method"] = "registerServiceHandler";
    j["id"] = QString("registerServiceHandler;%1;%2;%3").arg(Str(typeId), Str(handler), (pending ? "1" : "0"));
    j["params"] = p;

    Send(j);
}

void DobWebSocket::Unregister(int64_t typeId)
{
    if (!m_isConnected)
    {
        Error("Not connected!");
        return;
    }

    // Params
    QJsonObject p;
    p["typeId"] = Str(typeId);

    // Method object
    QJsonObject j;
    j["method"] = "unregisterHandler";
    j["id"] = QString("unregisterHandler;%1").arg(Str(typeId));
    j["params"] = p;

    Send(j);
}

void DobWebSocket::SendMessage(const Safir::Dob::MessagePtr& message, const Safir::Dob::Typesystem::ChannelId& channel)
{
    if (!m_isConnected)
    {
        Error("Not connected!");
        return;
    }

    // Params
    QJsonObject p;
    p["channelId"] = Str(channel);
    p["message"] = ToJsonObject(message);

    // Method object
    QJsonObject j;
    j["method"] = "sendMessage";
    j["id"] = QString("sendMessage;%1;%2").arg(Str(message->GetTypeId()), Str(channel));
    j["params"] = p;

    Send(j);
}

void DobWebSocket::SendServiceRequest(const Safir::Dob::ServicePtr& request, const Safir::Dob::Typesystem::HandlerId& handler)
{
    if (!m_isConnected)
    {
        Error("Not connected!");
        return;
    }

    // Params
    QJsonObject p;
    p["handlerId"] = Str(handler);
    p["service"] = ToJsonObject(request);

    // Method object
    QJsonObject j;
    j["method"] = "serviceRequest";
    j["id"] = "serviceRequest";
    j["params"] = p;

    Send(j);
}

void DobWebSocket::CreateRequest(const Safir::Dob::EntityPtr& entity, const Safir::Dob::Typesystem::InstanceId& instance, const Safir::Dob::Typesystem::HandlerId& handler)
{
    if (!m_isConnected)
    {
        Error("Not connected!");
        return;
    }

    // Params
    QJsonObject p;
    p["handlerId"] = Str(handler);
    p["entity"] = ToJsonObject(entity);
    if (instance != sdt::InstanceId())
    {
        p["instanceId"] = Str(instance);
    }

    // Method object
    QJsonObject j;
    j["method"] = "createRequest";
    j["id"] = "createRequest";
    j["params"] = p;

    Send(j);
}

void DobWebSocket::UpdateRequest(const Safir::Dob::EntityPtr& entity, const Safir::Dob::Typesystem::InstanceId& instance)
{
    if (!m_isConnected)
    {
        Error("Not connected!");
        return;
    }

    // Params
    QJsonObject p;
    p["entity"] = ToJsonObject(entity);
    p["instanceId"] = Str(instance);

    // Method object
    QJsonObject j;
    j["method"] = "updateRequest";
    j["id"] = "updateRequest";
    j["params"] = p;

    Send(j);
}

void DobWebSocket::DeleteRequest(const Safir::Dob::Typesystem::EntityId& entityId)
{
    if (!m_isConnected)
    {
        Error("Not connected!");
        return;
    }

    // Params
    QJsonObject p;
    p["typeId"] = Str(entityId.GetTypeId());
    p["instanceId"] = Str(entityId.GetInstanceId());

    // Method object
    QJsonObject j;
    j["method"] = "deleteRequest";
    j["id"] = "deleteRequest";
    j["params"] = p;

    Send(j);
}

void DobWebSocket::SetChanges(const Safir::Dob::EntityPtr& entity, const Safir::Dob::Typesystem::InstanceId& instance, const Safir::Dob::Typesystem::HandlerId& handler)
{
    if (!m_isConnected)
    {
        Error("Not connected!");
        return;
    }

    // Params
    QJsonObject p;
    p["entity"] = ToJsonObject(entity);
    p["instanceId"] = Str(instance);
    p["handlerId"] = Str(handler);

    // Method object
    QJsonObject j;
    j["method"] = "setEntityChanges";
    j["id"] = QString("setEntityChanges;%1;%2;%3").arg(Str(entity->GetTypeId()), Str(instance), Str(handler));
    j["params"] = p;

    Send(j);
}

void DobWebSocket::SetAll(const Safir::Dob::EntityPtr& entity, const sdt::InstanceId& instance, const sdt::HandlerId& handler)
{
    if (!m_isConnected)
    {
        Error("Not connected!");
        return;
    }

    // Params
    QJsonObject p;
    p["entity"] = ToJsonObject(entity);
    p["instanceId"] = Str(instance);
    p["handlerId"] = Str(handler);

    // Method object
    QJsonObject j;
    j["method"] = "setEntity";
    j["id"] = QString("setEntity;%1;%2;%3").arg(Str(entity->GetTypeId()), Str(instance), Str(handler));
    j["params"] = p;

    Send(j);
}

void DobWebSocket::Delete(const Safir::Dob::Typesystem::EntityId& entityId, const Safir::Dob::Typesystem::HandlerId& handler)
{
    if (!m_isConnected)
    {
        Error("Not connected!");
        return;
    }

    // Params
    QJsonObject p;
    p["typeId"] = Str(entityId.GetTypeId());
    p["instanceId"] = Str(entityId.GetInstanceId());
    p["handlerId"] = Str(handler);

    // Method object
    QJsonObject j;
    j["method"] = "deleteEntity";
    j["id"] = QString("deleteEntity;%1;%2;%3").arg(Str(entityId.GetTypeId()), Str(entityId.GetInstanceId()), Str(handler));
    j["params"] = p;

    Send(j);
}

void DobWebSocket::DeleteAll(int64_t typeId, const Safir::Dob::Typesystem::HandlerId& handler)
{
    if (!m_isConnected)
    {
        Error("Not connected!");
        return;
    }

    // Params
    QJsonObject p;
    p["typeId"] = Str(typeId);
    p["handlerId"] = Str(handler);

    // Method object
    QJsonObject j;
    j["method"] = "deleteAllInstances";
    j["id"] = QString("deleteAllInstances;%1;%2;%3").arg(Str(typeId), Str(handler));
    j["params"] = p;

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
    auto result = j["result"].toString();

    if (id == "open")
    {
        if (result == "OK")
        {
            emit DobInterface::ConnectedToDob(m_name);
            emit DobInterface::Info("<b>Connected to DOB (websocket)!</b>");
        }
        else
        {
            Error("Open failed: " + result);
        }
    }
    else if (id == "sendMessage")
    {
        if (result == "OK")
        {
            emit DobInterface::Info(QString("Message sent OK: %1, channel=%2").arg(idList[1], idList[2]));
        }
        else
        {
            Error("sendMessage failed: " + result);
        }
    }
    else if (id == "setEntityChanges")
    {
        if (result == "OK")
        {
            emit DobInterface::Info(QString("Set entity changes OK: %1, instance=%2, handler=%3").arg(idList[1], idList[2], idList[3]));
        }
        else
        {
            Error("SetEntityChanges failed: " + result);
        }
    }
    else if (id == "setEntity")
    {
        if (result == "OK")
        {
            emit DobInterface::Info(QString("Set entity OK: %1, instance=%2, handler=%3").arg(idList[1], idList[2], idList[3]));
        }
        else
        {
            Error("SetEntity failed: " + result);
        }
    }
    else if (id == "deleteEntity")
    {
        if (result == "OK")
        {
            emit DobInterface::Info(QString("Delete entity OK: %1, instance=%2, handler=%3").arg(idList[1], idList[2], idList[3]));
        }
        else
        {
            Error("DeleteEntity failed: " + result);
        }
    }
    else if (id == "deleteAllInstances")
    {
        if (result == "OK")
        {
            emit DobInterface::Info(QString("Delete all instances OK: %1, handler=%2").arg(idList[1], idList[2]));
        }
        else
        {
            Error("DeleteAllInstances failed: " + result);
        }
    }

    /*
subscribeMessage
unsubscribeMessage
subscribeEntity
unsubscribeEntity
subscribeRegistration
unsubscribeRegistration
registerEntityHandler
registerServiceHandler
unregisterHandler
createRequest
updateRequest
deleteRequest
serviceRequest
     */

}

void DobWebSocket::HandleNotification(const QJsonObject& j)
{
}

void DobWebSocket::HandleError(const QJsonObject& j)
{
    Error("Received error: " + Str(j));
}

void DobWebSocket::Send(const QJsonObject& j)
{
    m_webSocket.sendTextMessage(Str(j));
}

void DobWebSocket::Error(const QString& msg)
{
    emit DobInterface::Info(QString("<span style='color:red'>%1</span>").arg(msg));
}
