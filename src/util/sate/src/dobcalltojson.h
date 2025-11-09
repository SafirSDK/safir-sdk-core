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

#include <QJsonObject>
#include <QJsonArray>
#include <QJsonDocument>

#include <Safir/Dob/Typesystem/ChannelId.h>
#include <Safir/Dob/Typesystem/InstanceId.h>
#include <Safir/Dob/Typesystem/HandlerId.h>
#include <Safir/Dob/Typesystem/EntityId.h>
#include <Safir/Dob/InstanceIdPolicy.h>
#include <Safir/Dob/Entity.h>
#include <Safir/Dob/Message.h>
#include <Safir/Dob/Entity.h>
#include <Safir/Dob/Service.h>

namespace sdt = Safir::Dob::Typesystem;

class DobCallToJson
{
public:
    static QJsonObject Open(const QString& name, int context, bool skipId = false);
    static QJsonObject Close(bool skipId = false);

    // Subscriptions
    static QJsonObject SubscribeMessage(int64_t typeId, const sdt::ChannelId& channel, bool includeSubclasses, bool skipId = false);
    static QJsonObject UnsubscribeMessage(int64_t typeId, bool skipId = false);
    static QJsonObject SubscribeEntity(int64_t typeId, const sdt::InstanceId& instance, bool includeSubclasses, bool skipId = false);
    static QJsonObject UnsubscribeEntity(int64_t typeId, bool skipId = false);
    static QJsonObject SubscribeRegistrations(int64_t typeId, const sdt::HandlerId& handler, bool includeSubclasses, bool skipId = false);
    static QJsonObject UnsubscribeRegistrations(int64_t typeId, bool skipId = false);

    // Registrations
    static QJsonObject RegisterEntityHandler(int64_t typeId, const sdt::HandlerId& handler, Safir::Dob::InstanceIdPolicy::Enumeration instanceIdPolicy, bool pending, bool injection, bool skipId = false);
    static QJsonObject RegisterServiceHandler(int64_t typeId, const sdt::HandlerId& handler, bool pending, bool skipId = false);
    static QJsonObject Unregister(int64_t typeId, bool skipId = false);

    static QJsonObject SendMessage(const Safir::Dob::MessagePtr& message, const sdt::ChannelId& channel, bool skipId = false);
    static QJsonObject SendServiceRequest(const Safir::Dob::ServicePtr& request, const sdt::HandlerId& handler, bool skipId = false);

    static QJsonObject CreateRequest(const Safir::Dob::EntityPtr& entity, const sdt::InstanceId& instance, const sdt::HandlerId& handler, bool skipId = false);
    static QJsonObject UpdateRequest(const Safir::Dob::EntityPtr& entity, const sdt::InstanceId& instance, bool skipId = false);
    static QJsonObject DeleteRequest(const sdt::EntityId& entityId, bool skipId = false);

    static QJsonObject SetChanges(const Safir::Dob::EntityPtr& entity, const sdt::InstanceId& instance, const sdt::HandlerId& handler, bool skipId = false);
    static QJsonObject SetAll(const Safir::Dob::EntityPtr& entity, const sdt::InstanceId& instance, const sdt::HandlerId& handler, bool skipId = false);
    static QJsonObject Delete(const sdt::EntityId& entityId, const sdt::HandlerId& handler, bool skipId = false);
    static QJsonObject DeleteAll(int64_t typeId, const sdt::HandlerId& handler, bool skipId = false);

    static QJsonObject ReadEntity(const sdt::EntityId& entityId, bool skipId = false);
};
