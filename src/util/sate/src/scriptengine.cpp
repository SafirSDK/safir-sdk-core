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
#include "scriptengine.h"
#include "dobhandler.h"
#include "utilities.h"
#include <QJsonDocument>
#include <QJsonArray>
#include <QJsonObject>
#include <QJsonValue>
#include <QJsonValueRef>
#include <QTimer>
#include <QRandomGenerator>
#include <QRegularExpression>
#include <Safir/Dob/Typesystem/Serialization.h>

namespace
{
    enum SafirValidType
    {
        SafirBool,
        SafirNumber,
        SafirString,
        SafirStringOrNumber,
        SafirObject,
        SafirInstanceIdPolicy
    };
    enum SafirParamRequirement
    {
        SafirMandatory,
        SafirOptional
    };

    int64_t GetInt64(const QJsonValue &item)
    {
        return item.toVariant().toLongLong();
    }

    int64_t GetTypeId(const QJsonValue &item)
    {
        if (item.isString())
        {
            return Safir::Dob::Typesystem::Operations::GetTypeId(item.toString().toStdWString());
        }
        return GetInt64(item);
    }

    template <class HashT>
    HashT GetHashedValue(const QJsonValue &item)
    {
        if (item.isUndefined())
        {
            return HashT();
        }

        if (item.isString())
        {
            return HashT(item.toString().toStdWString());
        }

        return HashT(item.toVariant().toLongLong());
    }

    QString Str(const QJsonObject &j) { return QString(QJsonDocument(j).toJson(QJsonDocument::Compact)); }

    template <class DobType>
    std::shared_ptr<DobType> ToDobObj(const QJsonObject &jobj)
    {
        auto obj = Safir::Dob::Typesystem::Serialization::ToObjectFromJson(Str(jobj).toStdWString());
        auto typedObj = std::dynamic_pointer_cast<DobType>(obj);
        return typedObj;
    }

    template <class DobType>
    void ValidateObjParameter(const QJsonValue &item, int itemIndex, const QString name, QStringList &errors)
    {
        const auto namedPar = item["params"][name];
        if (!namedPar.isObject())
        {
            errors.append(QString("Script item number %1 is not valid. Json parameter '%2' has wrong type. Expected type is Object.").arg(QString::number(itemIndex), name));
        }
        else
        {
            try
            {
                auto obj = ToDobObj<DobType>(namedPar.toObject());
                if (!obj)
                {
                    auto expectedType = QString::fromStdWString(Safir::Dob::Typesystem::Operations::GetName(DobType::ClassTypeId));
                    errors.append(QString("Script item number %1 is not valid. Json parameter '%2' could not be parsed as a Dob object of type ").arg(QString::number(itemIndex), name, expectedType));
                }
            }
            catch (Safir::Dob::Typesystem::IllegalValueException &ex)
            {
                errors.append(QString("Script item number %1 is not valid. Json parameter '%2' could not be parsed as a Dob object. %3").arg(QString::number(itemIndex), name, QString::fromStdWString(ex.GetMessage())));
            }
        }
    }

    void ValidateParameter(const QJsonValue &item, int itemIndex, const QString name, SafirValidType safirType, SafirParamRequirement requirement, QStringList &errors)
    {
        const auto namedPar = item["params"][name];
        if (namedPar.isUndefined())
        {
            if (requirement == SafirMandatory)
            {
                errors.append(QString("Script item number %1 is not valid. Json parameter '%2' is missing.").arg(QString::number(itemIndex), name));
            }
            return;
        }

        switch (safirType)
        {

        case SafirBool:
        {
            if (namedPar.type() != QJsonValue::Bool)
            {
                errors.append(QString("Script item number %1 is not valid. Json parameter '%2' has wrong type. Expected type is boolean.").arg(QString::number(itemIndex), name));
            }
        }
        break;

        case SafirNumber:
        {
            if (namedPar.type() != QJsonValue::Double)
            {
                errors.append(QString("Script item number %1 is not valid. Json parameter '%2' has wrong type. Expected type is number.").arg(QString::number(itemIndex), name));
            }
        }
        break;

        case SafirString:
        {
            if (namedPar.type() != QJsonValue::String)
            {
                errors.append(QString("Script item number %1 is not valid. Json parameter '%2' has wrong type. Expected type is string.").arg(QString::number(itemIndex), name));
            }
        }
        break;

        case SafirStringOrNumber:
        {
            if (namedPar.type() != QJsonValue::String && namedPar.type() != QJsonValue::Double)
            {
                errors.append(QString("Script item number %1 is not valid. Json parameter '%2' has wrong type. Expected type is string or number.").arg(QString::number(itemIndex), name));
            }
        }
        break;

        case SafirObject:
        {
            ValidateObjParameter<Safir::Dob::Typesystem::Object>(item, itemIndex, name, errors);
        }
        break;

        case SafirInstanceIdPolicy:
        {
            auto str = namedPar.toString();
            if (str != "RequestorDecidesInstanceId" && str != "HandlerDecidesInstanceId")
            {
                errors.append(QString("Script item number %1 is not valid. Json parameter '%2' must have one of the following values: RequestorDecidesInstanceId, HandlerDecidesInstanceId").arg(QString::number(itemIndex), name));
            }
        }
        break;
        }
    }

    void ValidateScript(QJsonArray &items, QStringList &errors)
    {
        QVector<int> commentRows;
        int i = 0;
        for (const auto &item : items)
        {
            if (!item.isObject())
            {
                errors.append(QString("Script item number %1 is not valid. All script items must be Json objects.").arg(QString::number(i)));
                continue;
            }

            const auto method = item.toObject().value(QString("method"));
            if (method.isUndefined() || !method.isString())
            {
                errors.append(QString("Script item number %1. Json value 'method' is missing or is not a string value.").arg(QString::number(i)));
                continue;
            }

            auto ms = method.toString();

            if (ms.startsWith("//"))
            {
                // Comment line, no parameters to validate
                commentRows.append(i);
                ++i;
                continue;
            }

            if (ms == "open")
            {
                ValidateParameter(item, i, "connectionName", SafirString, SafirMandatory, errors);
                ValidateParameter(item, i, "context", SafirNumber, SafirOptional, errors);
            }
            else if (ms == "close")
            {
                // No parameters
            }
            else if (ms == "subscribeMessage")
            {
                ValidateParameter(item, i, "typeId", SafirStringOrNumber, SafirMandatory, errors);
                ValidateParameter(item, i, "channelId", SafirStringOrNumber, SafirOptional, errors);
                ValidateParameter(item, i, "includeSubclasses", SafirBool, SafirOptional, errors);
            }
            else if (ms == "unsubscribeMessage")
            {
                ValidateParameter(item, i, "typeId", SafirStringOrNumber, SafirMandatory, errors);
                ValidateParameter(item, i, "channelId", SafirStringOrNumber, SafirOptional, errors);
                ValidateParameter(item, i, "includeSubclasses", SafirBool, SafirOptional, errors);
            }
            else if (ms == "subscribeEntity")
            {
                ValidateParameter(item, i, "typeId", SafirStringOrNumber, SafirMandatory, errors);
                ValidateParameter(item, i, "instanceId", SafirStringOrNumber, SafirOptional, errors);
                ValidateParameter(item, i, "includeUpdates", SafirBool, SafirOptional, errors);
                ValidateParameter(item, i, "restartSubscription", SafirBool, SafirOptional, errors);
                ValidateParameter(item, i, "includeSubclasses", SafirBool, SafirOptional, errors);
            }
            else if (ms == "unsubscribeEntity")
            {
                ValidateParameter(item, i, "typeId", SafirStringOrNumber, SafirMandatory, errors);
                ValidateParameter(item, i, "instanceId", SafirStringOrNumber, SafirOptional, errors);
                ValidateParameter(item, i, "includeSubclasses", SafirBool, SafirOptional, errors);
            }
            else if (ms == "subscribeRegistration")
            {
                ValidateParameter(item, i, "typeId", SafirStringOrNumber, SafirMandatory, errors);
                ValidateParameter(item, i, "handlerId", SafirStringOrNumber, SafirOptional, errors);
                ValidateParameter(item, i, "includeSubclasses", SafirBool, SafirOptional, errors);
                ValidateParameter(item, i, "restartSubscription", SafirBool, SafirOptional, errors);
            }
            else if (ms == "unsubscribeRegistration")
            {
                ValidateParameter(item, i, "typeId", SafirStringOrNumber, SafirMandatory, errors);
                ValidateParameter(item, i, "handlerId", SafirStringOrNumber, SafirOptional, errors);
                ValidateParameter(item, i, "includeSubclasses", SafirBool, SafirOptional, errors);
            }
            else if (ms == "registerEntityHandler")
            {
                ValidateParameter(item, i, "typeId", SafirStringOrNumber, SafirMandatory, errors);
                ValidateParameter(item, i, "handlerId", SafirStringOrNumber, SafirOptional, errors);
                ValidateParameter(item, i, "instanceIdPolicy", SafirInstanceIdPolicy, SafirOptional, errors);
                ValidateParameter(item, i, "injectionHandler", SafirBool, SafirOptional, errors);
                ValidateParameter(item, i, "pending", SafirBool, SafirOptional, errors);
            }
            else if (ms == "registerServiceHandler")
            {
                ValidateParameter(item, i, "typeId", SafirStringOrNumber, SafirMandatory, errors);
                ValidateParameter(item, i, "handlerId", SafirStringOrNumber, SafirOptional, errors);
                ValidateParameter(item, i, "pending", SafirBool, SafirOptional, errors);
            }
            else if (ms == "unregisterHandler")
            {
                ValidateParameter(item, i, "typeId", SafirStringOrNumber, SafirMandatory, errors);
                ValidateParameter(item, i, "handlerId", SafirStringOrNumber, SafirOptional, errors);
            }
            else if (ms == "sendMessage")
            {
                ValidateObjParameter<Safir::Dob::Message>(item, i, "message", errors);
                ValidateParameter(item, i, "channelId", SafirStringOrNumber, SafirOptional, errors);
            }
            else if (ms == "serviceRequest")
            {
                ValidateObjParameter<Safir::Dob::Service>(item, i, "request", errors);
                ValidateParameter(item, i, "handlerId", SafirStringOrNumber, SafirOptional, errors);
            }
            else if (ms == "createRequest")
            {
                ValidateObjParameter<Safir::Dob::Entity>(item, i, "entity", errors);
                ValidateParameter(item, i, "instanceId", SafirStringOrNumber, SafirOptional, errors);
                ValidateParameter(item, i, "handlerId", SafirStringOrNumber, SafirOptional, errors);
            }
            else if (ms == "updateRequest")
            {
                ValidateObjParameter<Safir::Dob::Entity>(item, i, "entity", errors);
                ValidateParameter(item, i, "instanceId", SafirStringOrNumber, SafirMandatory, errors);
            }
            else if (ms == "deleteRequest")
            {
                ValidateParameter(item, i, "typeId", SafirStringOrNumber, SafirMandatory, errors);
                ValidateParameter(item, i, "instanceId", SafirStringOrNumber, SafirMandatory, errors);
            }
            else if (ms == "setEntity")
            {
                ValidateObjParameter<Safir::Dob::Entity>(item, i, "entity", errors);
                ValidateParameter(item, i, "instanceId", SafirStringOrNumber, SafirMandatory, errors);
                ValidateParameter(item, i, "handlerId", SafirStringOrNumber, SafirOptional, errors);
            }
            else if (ms == "setEntityChanges")
            {
                ValidateObjParameter<Safir::Dob::Entity>(item, i, "entity", errors);
                ValidateParameter(item, i, "instanceId", SafirStringOrNumber, SafirMandatory, errors);
                ValidateParameter(item, i, "handlerId", SafirStringOrNumber, SafirOptional, errors);
            }
            else if (ms == "deleteEntity")
            {
                ValidateParameter(item, i, "typeId", SafirStringOrNumber, SafirMandatory, errors);
                ValidateParameter(item, i, "instanceId", SafirStringOrNumber, SafirMandatory, errors);
                ValidateParameter(item, i, "handlerId", SafirStringOrNumber, SafirOptional, errors);
            }
            else if (ms == "deleteAllInstances")
            {
                ValidateParameter(item, i, "typeId", SafirStringOrNumber, SafirMandatory, errors);
                ValidateParameter(item, i, "handlerId", SafirStringOrNumber, SafirOptional, errors);
            }
            else if (ms == "readEntity")
            {
                ValidateParameter(item, i, "typeId", SafirStringOrNumber, SafirMandatory, errors);
                ValidateParameter(item, i, "instanceId", SafirStringOrNumber, SafirMandatory, errors);
            }
            else if (ms == "delay")
            {
                ValidateParameter(item, i, "time", SafirNumber, SafirMandatory, errors);
            }
            else
            {
                errors.append(QString("Script item number %1 has invalid method: %2").arg(QString::number(i), ms));
            }

            ++i;
        }

        // Remove comment rows
        for (int j = commentRows.size() - 1; j >= 0; --j)
        {
            items.removeAt(commentRows[j]);
        }
    }

    QString HandleInstanceIdMacro(const QString &text)
    {
        QString result = text;
        QRegularExpression regex(R"("_INSTANCEID_")");
        QRegularExpressionMatchIterator it = regex.globalMatch(text);

        // Process matches in reverse order to maintain correct positions
        QVector<QRegularExpressionMatch> matches;
        while (it.hasNext())
        {
            matches.prepend(it.next());
        }

        for (const auto &match : matches)
        {
            auto randomValue = Safir::Dob::Typesystem::InstanceId::GenerateRandom().GetRawValue();
            result.replace(match.capturedStart(), match.capturedLength(), QString::number(randomValue));
        }

        return result;
    }

    QString HandleNumberMacro(const QString &text)
    {
        QString result = text;
        QRegularExpression regex(R"("_NUMBER_(-?\d+)_(-?\d+)_")");
        QRegularExpressionMatchIterator it = regex.globalMatch(text);

        // Process matches in reverse order to maintain correct positions
        QVector<QRegularExpressionMatch> matches;
        while (it.hasNext())
        {
            matches.prepend(it.next());
        }

        for (const auto &match : matches)
        {
            bool ok1, ok2;
            int64_t low = match.captured(1).toLongLong(&ok1);
            int64_t high = match.captured(2).toLongLong(&ok2);

            if (ok1 && ok2 && low <= high)
            {
                int64_t randomValue = Utilities::RandomInt64(low, high);
                result.replace(match.capturedStart(), match.capturedLength(), QString::number(randomValue));
            }
        }

        return result;
    }

    QString HandleStringMacro(const QString &text)
    {
        QString result = text;
        QRegularExpression regex(R"(_STRING_(\d+)_(\d+)_)");
        QRegularExpressionMatchIterator it = regex.globalMatch(text);

        // Process matches in reverse order to maintain correct positions
        QVector<QRegularExpressionMatch> matches;
        while (it.hasNext())
        {
            matches.prepend(it.next());
        }

        for (const auto &match : matches)
        {
            bool ok1, ok2;
            int minLength = match.captured(1).toInt(&ok1);
            int maxLength = match.captured(2).toInt(&ok2);

            if (ok1 && ok2 && minLength >= 0 && minLength <= maxLength)
            {
                int length = (minLength == maxLength) ? minLength : Utilities::RandomInt64(minLength, maxLength);
                QString randomString;
                randomString.reserve(length);

                // Generate random alphanumeric string
                static const QString chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
                for (int i = 0; i < length; ++i)
                {
                    int index = Utilities::RandomInt64(0, chars.length() - 1);
                    randomString.append(chars[index]);
                }

                result.replace(match.capturedStart(), match.capturedLength(), randomString);
            }
        }

        return result;
    }

    QString HandleMacros(const QString& text)
    {
        auto result = HandleInstanceIdMacro(text);
        result = HandleNumberMacro(result);
        result = HandleStringMacro(result);
        return result;
    }
}

ScriptEngine::ScriptEngine(DobHandler* dobHandler)
    : m_dobHandler(dobHandler)
    , m_items()
{
    if (m_dobHandler == nullptr)
    {
        m_errors.append("No DobHandler provided to ScriptEngine.");
        return;
    }
    connect(m_dobHandler, &DobHandler::OnNotRequestOverflow, this, &ScriptEngine::ExecuteNext);
    connect(m_dobHandler, &DobHandler::OnNotMessageOverflow, this, &ScriptEngine::ExecuteNext);
}

void ScriptEngine::LoadScript(const QString& json)
{
    if (json.isEmpty())
    {
        // Empty script
        return;
    }

    auto text = HandleMacros(json);

    QJsonParseError err;
    auto doc = QJsonDocument::fromJson(text.toUtf8(), &err);
    if (err.error != QJsonParseError::NoError)
    {
        m_errors.append(err.errorString());
        return;
    }

    if (doc.isNull())
    {
        m_errors.append("Failed to parse script.");
        return;
    }

    if (!doc.isArray())
    {
        m_errors.append("The root of the script must be an array.");
        return;
    }

    m_items = doc.array();

    ValidateScript(m_items, m_errors);
}

void ScriptEngine::Record()
{
    m_state = Recording;
    m_dobHandler->InterfaceListenerAdded();
    connect(m_dobHandler, &DobHandler::InterfaceListener, this, [this](const auto& jobj){ AppendScriptItem(jobj); });
}

void ScriptEngine::Execute()
{
    if (m_state == Executing)
    {
        return;
    }

    m_state = Executing;
    ExecuteNext();
}

void ScriptEngine::ExecuteNext()
{
    if (m_state == Executing)
    {
        auto time = ExecuteIndex(m_currentIndex);
        if (time < 0)
        {
            // time < 0 indicates overflow, so we stop execution here and wait for OnNotOverflow
            return;
        }

        m_currentIndex++;
        if (!IsFinished())
        {
            emit IndexFinished(m_currentIndex - 1);
            QTimer::singleShot(time, [this]
                               { ExecuteNext(); });
        }
        else
        {
            QTimer::singleShot(time, [this]{ emit IndexFinished(m_currentIndex - 1); });
        }
    }
}

// Execute specific index without modifying currentIndex
int ScriptEngine::ExecuteIndex(int index)
{
    if (index < 0 || index >= m_items.size())
    {
        return 0;
    }

    const auto item = m_items.at(index).toObject();
    const auto ms = item[QString("method")].toString();

    // Handle methods with no parameters first, then we can safely get the params object.
    if (ms == "close")
    {
        m_dobHandler->Close();
        return 10;
    }

    // Handle methods with parameters.
    const auto par = item["params"].toObject();

    if (ms == "open")
    {
        if (!m_dobHandler->IsOpen())
        {
            connect(m_dobHandler, &DobHandler::ConnectedToDob, this, [this](const QString& /*name*/) {
                // Connected, proceed to next step
                m_currentIndex++;
                ExecuteNext();
            });


            const auto connectionName = par["connectionName"].toString();
            const auto context = par["context"].toInt(0);
            m_dobHandler->OpenNativeConnection(connectionName, context);
            return -1; // Dont continue until connected
        }
    }
    else if (ms == "subscribeMessage")
    {
        auto typeId = GetTypeId(par["typeId"]);
        auto channelId = GetHashedValue<Safir::Dob::Typesystem::ChannelId>(par["channelId"]);
        auto includeSubclasses = par["includeSubclasses"].toBool(true);
        m_dobHandler->SubscribeMessage(typeId, channelId, includeSubclasses);
    }
    else if (ms == "unsubscribeMessage")
    {
        auto typeId = GetTypeId(par["typeId"]);
        m_dobHandler->UnsubscribeMessage(typeId);
    }
    else if (ms == "subscribeEntity")
    {
        auto typeId = GetTypeId(par["typeId"]);
        auto instanceId = GetHashedValue<Safir::Dob::Typesystem::InstanceId>(par["instanceId"]);
        auto includeSubclasses = par["includeSubclasses"].toBool(true);
        m_dobHandler->SubscribeEntity(typeId, instanceId, includeSubclasses);
    }
    else if (ms == "unsubscribeEntity")
    {
        auto typeId = GetTypeId(par["typeId"]);
        m_dobHandler->UnsubscribeEntity(typeId);
    }
    else if (ms == "subscribeRegistration")
    {
        auto typeId = GetTypeId(par["typeId"]);
        auto handlerId = GetHashedValue<Safir::Dob::Typesystem::HandlerId>(par["handlerId"]);
        auto includeSubclasses = par["includeSubclasses"].toBool(true);
        m_dobHandler->SubscribeRegistrations(typeId, handlerId, includeSubclasses);
    }
    else if (ms == "unsubscribeRegistration")
    {
        auto typeId = GetTypeId(par["typeId"]);
        m_dobHandler->UnsubscribeRegistrations(typeId);
    }
    else if (ms == "registerEntityHandler")
    {
        auto typeId = GetTypeId(par["typeId"]);
        auto handlerId = GetHashedValue<Safir::Dob::Typesystem::HandlerId>(par["handlerId"]);
        auto instanceIdPolicy = Safir::Dob::InstanceIdPolicy::ToValue(par["instanceIdPolicy"].toString("RequestorDecidesInstanceId").toStdWString());
        auto injectionHandler = par["injectionHandler"].toBool(false);
        auto pending = par["pending"].toBool(false);
        m_dobHandler->RegisterEntityHandler(typeId, handlerId, instanceIdPolicy, injectionHandler, pending);
    }
    else if (ms == "registerServiceHandler")
    {
        auto typeId = GetTypeId(par["typeId"]);
        auto handlerId = GetHashedValue<Safir::Dob::Typesystem::HandlerId>(par["handlerId"]);
        auto pending = par["pending"].toBool(false);
        m_dobHandler->RegisterServiceHandler(typeId, handlerId, pending);
    }
    else if (ms == "unregisterHandler")
    {
        auto typeId = GetTypeId(par["typeId"]);
        m_dobHandler->Unregister(typeId);
    }
    else if (ms == "sendMessage")
    {
        auto message = ToDobObj<Safir::Dob::Message>(par["message"].toObject());
        auto channelId = GetHashedValue<Safir::Dob::Typesystem::ChannelId>(par["channelId"]);
        if (!m_dobHandler->SendMessage(message, channelId))
        {
            return -1; // Indicate overflow
        }
    }
    else if (ms == "serviceRequest")
    {
        auto request = ToDobObj<Safir::Dob::Service>(par["request"].toObject());
        auto handlerId = GetHashedValue<Safir::Dob::Typesystem::HandlerId>(par["handlerId"]);
        if (!m_dobHandler->SendServiceRequest(request, handlerId))
        {
            return -1; // Indicate overflow
        }
    }
    else if (ms == "createRequest")
    {
        auto entity = ToDobObj<Safir::Dob::Entity>(par["entity"].toObject());
        auto instanceId = GetHashedValue<Safir::Dob::Typesystem::InstanceId>(par["instanceId"]);
        auto handlerId = GetHashedValue<Safir::Dob::Typesystem::HandlerId>(par["handlerId"]);
        if (!m_dobHandler->CreateRequest(entity, instanceId, handlerId))
        {
            return -1; // Indicate overflow
        }
    }
    else if (ms == "updateRequest")
    {
        auto entity = ToDobObj<Safir::Dob::Entity>(par["entity"].toObject());
        auto instanceId = GetHashedValue<Safir::Dob::Typesystem::InstanceId>(par["instanceId"]);
        if (!m_dobHandler->UpdateRequest(entity, instanceId))
        {
            return -1; // Indicate overflow
        }
    }
    else if (ms == "deleteRequest")
    {
        auto typeId = GetTypeId(par["typeId"]);
        auto instanceId = GetHashedValue<Safir::Dob::Typesystem::InstanceId>(par["instanceId"]);
        if (!m_dobHandler->DeleteRequest({typeId, instanceId}))
        {
            return -1; // Indicate overflow
        }
    }
    else if (ms == "setEntity")
    {
        auto entity = ToDobObj<Safir::Dob::Entity>(par["entity"].toObject());
        auto instanceId = GetHashedValue<Safir::Dob::Typesystem::InstanceId>(par["instanceId"]);
        auto handlerId = GetHashedValue<Safir::Dob::Typesystem::HandlerId>(par["handlerId"]);
        m_dobHandler->SetAll(entity, instanceId, handlerId);
    }
    else if (ms == "setEntityChanges")
    {
        auto entity = ToDobObj<Safir::Dob::Entity>(par["entity"].toObject());
        auto instanceId = GetHashedValue<Safir::Dob::Typesystem::InstanceId>(par["instanceId"]);
        auto handlerId = GetHashedValue<Safir::Dob::Typesystem::HandlerId>(par["handlerId"]);
        m_dobHandler->SetChanges(entity, instanceId, handlerId);
    }
    else if (ms == "deleteEntity")
    {
        auto typeId = GetTypeId(par["typeId"]);
        auto instanceId = GetHashedValue<Safir::Dob::Typesystem::InstanceId>(par["instanceId"]);
        auto handlerId = GetHashedValue<Safir::Dob::Typesystem::HandlerId>(par["handlerId"]);
        m_dobHandler->Delete({typeId, instanceId}, handlerId);
    }
    else if (ms == "deleteAllInstances")
    {
        auto typeId = GetTypeId(par["typeId"]);
        auto handlerId = GetHashedValue<Safir::Dob::Typesystem::HandlerId>(par["handlerId"]);
        m_dobHandler->DeleteAll(typeId, handlerId);
    }
    else if (ms == "readEntity")
    {
        auto typeId = GetTypeId(par["typeId"]);
        auto instanceId = GetHashedValue<Safir::Dob::Typesystem::InstanceId>(par["instanceId"]);
        m_dobHandler->ReadEntity({typeId, instanceId});
    }
    else if (ms == "delay")
    {
        auto time = par["time"].toInt();
        return time;
    }

    return 10;
}

// Reset execution and set currentIndex to 0
void ScriptEngine::Reset()
{
    if (m_state == Recording)
    {
        m_dobHandler->InterfaceListenerRemoved();
        disconnect(m_dobHandler, &DobHandler::InterfaceListener, this, nullptr);
    }
    m_state = Stopped;
    m_currentIndex = 0;
}

// Stop execution but do not reset currentIndes.
void ScriptEngine::Pause()
{
    m_state = Stopped;
}

bool ScriptEngine::IsValid() const
{
    return m_errors.isEmpty();
}

const QStringList &ScriptEngine::Errors() const
{
    return m_errors;
}

int ScriptEngine::Size() const
{
    return m_items.size();
}

QJsonObject ScriptEngine::GetIndexObject(int index) const
{
    if (index >= 0 && index < m_items.size())
    {
        return m_items.at(index).toObject();
    }
    return QJsonObject();
}

void ScriptEngine::DeleteIndex(int index)
{
    if (index >= 0 && index < m_items.size())
    {
        m_items.removeAt(index);

        // If current index is beyond the deleted index, adjust it
        if (m_currentIndex > index)
        {
            m_currentIndex--;
        }
        // If we deleted the current index and it's at or past the end, stop execution
        else if (m_currentIndex == index && m_currentIndex >= m_items.size())
        {
            Reset();
        }
    }
}

QString ScriptEngine::GetScript() const
{
    QJsonDocument doc;
    doc.setArray(m_items);
    return QString::fromUtf8(doc.toJson(QJsonDocument::Indented));
}

void ScriptEngine::AppendScriptItem(const QJsonObject& item)
{
    m_items.append(item);
    emit ItemRecorded(item);
}

bool ScriptEngine::IsFinished() const
{
    return m_currentIndex >= Size();
}
