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
#include "dobcalltojson.h"
#include <Safir/Dob/Typesystem/Serialization.h>

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

QJsonObject ToJsonObject(const Safir::Dob::Typesystem::ObjectPtr& ptr)
{
    return QJsonDocument::fromJson(QString::fromStdWString(sdt::Serialization::ToJson(ptr)).toUtf8()).object();
}

}

QJsonObject DobCallToJson::Open(const QString &name, int context, bool skipId)
{
    QJsonObject j;
    j["method"] = "open";
    if (!skipId)
    {
        j["id"] = "open";
    }

    QJsonObject p;
    p["connectionName"] = name;
    p["context"] = context;
    j["params"] = p;

    return j;
}

QJsonObject DobCallToJson::Close(bool skipId)
{
    QJsonObject j;
    j["method"] = "close";
    if (!skipId)
    {
        j["id"] = "close";
    }
    return j;
}

QJsonObject DobCallToJson::SubscribeMessage(int64_t typeId, const Safir::Dob::Typesystem::ChannelId &channel, bool includeSubclasses, bool skipId)
{
    // Params
    QJsonObject p;
    p["typeId"] = Str(typeId);
    SetHashVal(p, "channelId", channel);
    p["includeSubclasses"] = includeSubclasses;

    // Method object
    QJsonObject j;
    j["method"] = "subscribeMessage";
    if (!skipId)
    {
        j["id"] = QString("subscribeMessage;%1;%2;%3").arg(Str(typeId), Str(channel.ToString()), (includeSubclasses ? "1" : "0"));
    }
    j["params"] = p;

    return j;
}

QJsonObject DobCallToJson::UnsubscribeMessage(int64_t typeId, bool skipId)
{
    // Params
    QJsonObject p;
    p["typeId"] = Str(typeId);

    // Method object
    QJsonObject j;
    j["method"] = "unsubscribeMessage";
    if (!skipId)
    {
        j["id"] = QString("unsubscribeMessage;%1;").arg(Str(typeId));
    }
    j["params"] = p;

    return j;
}

QJsonObject DobCallToJson::SubscribeEntity(int64_t typeId, const Safir::Dob::Typesystem::InstanceId &instance, bool includeSubclasses, bool skipId)
{
    // Params
    QJsonObject p;
    p["typeId"] = Str(typeId);

    if (instance != sdt::InstanceId())
    {
        SetHashVal(p, "instanceId", instance);
        includeSubclasses = false;
    }
    else
    {
        p["includeSubclasses"] = includeSubclasses;
    }

    // Method object
    QJsonObject j;
    j["method"] = "subscribeEntity";
    if (!skipId)
    {
        j["id"] = QString("subscribeEntity;%1;%2").arg(Str(typeId), (includeSubclasses ? "1" : "0"));
    }
    j["params"] = p;

    return j;
}

QJsonObject DobCallToJson::UnsubscribeEntity(int64_t typeId, bool skipId)
{
    // Params
    QJsonObject p;
    p["typeId"] = Str(typeId);

    // Method object
    QJsonObject j;
    j["method"] = "unsubscribeEntity";
    if (!skipId)
    {
        j["id"] = QString("unsubscribeEntity;%1;").arg(Str(typeId));
    }
    j["params"] = p;

    return j;
}

QJsonObject DobCallToJson::SubscribeRegistrations(int64_t typeId, const Safir::Dob::Typesystem::HandlerId &handler, bool includeSubclasses, bool skipId)
{
    // Params
    QJsonObject p;
    SetHashVal(p, "handlerId", handler);
    p["typeId"] = Str(typeId);
    p["includeSubclasses"] = includeSubclasses;

    // Method object
    QJsonObject j;
    j["method"] = "subscribeRegistration";
    if (!skipId)
    {
        j["id"] = QString("subscribeRegistration;%1;%2;%3").arg(Str(typeId), Str(handler.ToString()), (includeSubclasses ? "1" : "0"));
    }
    j["params"] = p;

    return j;
}

QJsonObject DobCallToJson::UnsubscribeRegistrations(int64_t typeId, bool skipId)
{
    // Params
    QJsonObject p;
    p["typeId"] = Str(typeId);

    // Method object
    QJsonObject j;
    j["method"] = "unsubscribeRegistration";
    if (!skipId)
    {
        j["id"] = QString("unsubscribeRegistration;%1;").arg(Str(typeId));
    }
    j["params"] = p;

    return j;
}

QJsonObject DobCallToJson::RegisterEntityHandler(int64_t typeId, const Safir::Dob::Typesystem::HandlerId &handler, Safir::Dob::InstanceIdPolicy::Enumeration instanceIdPolicy, bool pending, bool injection, bool skipId)
{
    // Params
    auto instPolicy = instanceIdPolicy == Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId ? "HandlerDecidesInstanceId" : "RequestorDecidesInstanceId";
    QJsonObject p;
    SetHashVal(p, "handlerId", handler);
    p["typeId"] = Str(typeId);
    p["instanceIdPolicy"] = instPolicy;
    p["pending"] = pending;
    p["injectionHandler"] = injection;

    // Method object
    QJsonObject j;
    j["method"] = "registerEntityHandler";
    if (!skipId)
    {
        j["id"] = QString("registerEntityHandler;%1;%2;%3;%4;%5").arg(Str(typeId), Str(handler.ToString()), instPolicy, (pending ? "1" : "0"), (injection ? "1" : "0"));
    }
    j["params"] = p;

    return j;
}

QJsonObject DobCallToJson::RegisterServiceHandler(int64_t typeId, const Safir::Dob::Typesystem::HandlerId &handler, bool pending, bool skipId)
{
    // Params
    QJsonObject p;
    p["typeId"] = Str(typeId);
    SetHashVal(p, "handlerId", handler);
    p["pending"] = pending;

    // Method object
    QJsonObject j;
    j["method"] = "registerServiceHandler";
    if (!skipId)
    {
        j["id"] = QString("registerServiceHandler;%1;%2;%3").arg(Str(typeId), Str(handler.ToString()), (pending ? "1" : "0"));
    }
    j["params"] = p;

    return j;
}

QJsonObject DobCallToJson::Unregister(int64_t typeId, bool skipId)
{
    // Params
    QJsonObject p;
    p["typeId"] = Str(typeId);

    // Method object
    QJsonObject j;
    j["method"] = "unregisterHandler";
    if (!skipId)
    {
        j["id"] = QString("unregisterHandler;%1").arg(Str(typeId));
    }
    j["params"] = p;

    return j;
}

QJsonObject DobCallToJson::SendMessage(const Safir::Dob::MessagePtr &message, const Safir::Dob::Typesystem::ChannelId &channel, bool skipId)
{
    // Params
    QJsonObject p;
    SetHashVal(p, "channelId", channel);
    p["message"] = ToJsonObject(message);

    // Method object
    QJsonObject j;
    j["method"] = "sendMessage";
    if (!skipId)
    {
        j["id"] = QString("sendMessage;%1;%2").arg(Str(message->GetTypeId()), Str(channel.ToString()));
    }
    j["params"] = p;

    return j;
}

QJsonObject DobCallToJson::SendServiceRequest(const Safir::Dob::ServicePtr &request, const Safir::Dob::Typesystem::HandlerId &handler, bool skipId)
{
    // Params
    QJsonObject p;
    SetHashVal(p, "handlerId", handler);
    p["request"] = ToJsonObject(request);

    // Method object
    QJsonObject j;
    j["method"] = "serviceRequest";
    if (!skipId)
    {
        j["id"] = QString("serviceRequest;%1").arg(Str(handler.ToString()));
    }
    j["params"] = p;

    return j;
}

QJsonObject DobCallToJson::CreateRequest(const Safir::Dob::EntityPtr &entity, const Safir::Dob::Typesystem::InstanceId &instance, const Safir::Dob::Typesystem::HandlerId &handler, bool skipId)
{
    // Params
    QJsonObject p;
    SetHashVal(p, "handlerId", handler);
    p["entity"] = ToJsonObject(entity);
    if (instance != sdt::InstanceId())
    {
        SetHashVal(p, "instanceId", instance);
    }

    // Method object
    QJsonObject j;
    j["method"] = "createRequest";
    if (!skipId)
    {
        j["id"] = QString("createRequest;%1").arg(Str(handler.ToString()));
    }
    j["params"] = p;

    return j;
}

QJsonObject DobCallToJson::UpdateRequest(const Safir::Dob::EntityPtr &entity, const Safir::Dob::Typesystem::InstanceId &instance, bool skipId)
{
    // Params
    QJsonObject p;
    p["entity"] = ToJsonObject(entity);
    SetHashVal(p, "instanceId", instance);

    // Method object
    QJsonObject j;
    j["method"] = "updateRequest";
    if (!skipId)
    {
        j["id"] = QString("updateRequest;%1").arg(Str(instance.ToString()));
    }
    j["params"] = p;

    return j;
}

QJsonObject DobCallToJson::DeleteRequest(const Safir::Dob::Typesystem::EntityId &entityId, bool skipId)
{
    // Params
    QJsonObject p;
    p["typeId"] = Str(entityId.GetTypeId());
    SetHashVal(p, "instanceId", entityId.GetInstanceId());

    // Method object
    QJsonObject j;
    j["method"] = "deleteRequest";
    if (!skipId)
    {
        j["id"] = QString("deleteRequest;%1;%2").arg(Str(entityId.GetTypeId()), Str(entityId.GetInstanceId().ToString()));
    }
    j["params"] = p;

    return j;
}

QJsonObject DobCallToJson::SetChanges(const Safir::Dob::EntityPtr &entity, const Safir::Dob::Typesystem::InstanceId &instance, const Safir::Dob::Typesystem::HandlerId &handler, bool skipId)
{
    // Params
    QJsonObject p;
    p["entity"] = ToJsonObject(entity);
    SetHashVal(p, "instanceId", instance);
    SetHashVal(p, "handlerId", handler);

    // Method object
    QJsonObject j;
    j["method"] = "setEntityChanges";
    if (!skipId)
    {
        j["id"] = QString("setEntityChanges;%1;%2;%3").arg(Str(entity->GetTypeId()), Str(instance.ToString()), Str(handler.ToString()));
    }
    j["params"] = p;

    return j;
}

QJsonObject DobCallToJson::SetAll(const Safir::Dob::EntityPtr &entity, const Safir::Dob::Typesystem::InstanceId &instance, const Safir::Dob::Typesystem::HandlerId &handler, bool skipId)
{
    // Params
    QJsonObject p;
    p["entity"] = ToJsonObject(entity);
    SetHashVal(p, "instanceId", instance);
    SetHashVal(p, "handlerId", handler);

    // Method object
    QJsonObject j;
    j["method"] = "setEntity";
    if (!skipId)
    {
        j["id"] = QString("setEntity;%1;%2;%3").arg(Str(entity->GetTypeId()), Str(instance.ToString()), Str(handler.ToString()));
    }
    j["params"] = p;

    return j;
}

QJsonObject DobCallToJson::Delete(const Safir::Dob::Typesystem::EntityId &entityId, const Safir::Dob::Typesystem::HandlerId &handler, bool skipId)
{
    // Params
    QJsonObject p;
    p["typeId"] = Str(entityId.GetTypeId());
    SetHashVal(p, "instanceId", entityId.GetInstanceId());
    SetHashVal(p, "handlerId", handler);

    // Method object
    QJsonObject j;
    j["method"] = "deleteEntity";
    if (!skipId)
    {
        j["id"] = QString("deleteEntity;%1;%2;%3").arg(Str(entityId.GetTypeId()), Str(entityId.GetInstanceId().ToString()), Str(handler.ToString()));
    }
    j["params"] = p;

    return j;
}

QJsonObject DobCallToJson::DeleteAll(int64_t typeId, const Safir::Dob::Typesystem::HandlerId &handler, bool skipId)
{
    // Params
    QJsonObject p;
    p["typeId"] = Str(typeId);
    SetHashVal(p, "handlerId", handler);

    // Method object
    QJsonObject j;
    j["method"] = "deleteAllInstances";
    if (!skipId)
    {
        j["id"] = QString("deleteAllInstances;%1;%2;%3").arg(Str(typeId), Str(handler.ToString()));
    }
    j["params"] = p;

    return j;
}

QJsonObject DobCallToJson::ReadEntity(const Safir::Dob::Typesystem::EntityId &entityId, bool skipId)
{
    // Params
    QJsonObject p;
    p["typeId"] = Str(entityId.GetTypeId());
    SetHashVal(p, "instanceId", entityId.GetInstanceId());

    // Method object
    QJsonObject j;
    j["method"] = "readEntity";
    if (!skipId)
    {
        j["id"] = QString("readEntity;%1;%2").arg(Str(entityId.GetTypeId()), Str(entityId.GetInstanceId().ToString()));
    }
    j["params"] = p;

    return j;
}
