/******************************************************************************
*
* Copyright Saab AB, 2026 (http://safirsdkcore.com)
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

#include <algorithm>
#include <vector>
#include <Safir/Dob/Message.h>
#include <Safir/Dob/Entity.h>
#include <Safir/Dob/Service.h>
#include <Safir/Dob/Response.h>
#include <Safir/Logging/Log.h>
#include "JsonRpcRequest.h"


namespace CommandValidator
{
    inline void ValidateOpen(const JsonRpcRequest& req)
    {
        if (!req.HasConnectionName() || req.ConnectionName().empty())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'open'", "connectionName is mandatory in command 'open'");
    }

    inline void ValidateSubscribeMessage(const JsonRpcRequest& req)
    {
        if (!req.HasTypeId())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'subscribeMessage'", "typeId is mandatory in command 'subscribeMessage'");

        if (!Safir::Dob::Typesystem::Operations::IsOfType(req.TypeId(), Safir::Dob::Message::ClassTypeId))
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'subscribeMessage'", "typeId must refer to a subtype of Safir.Dob.Message in command 'SubscribeMessage'");
    }

    inline void ValidateSendMessage(const JsonRpcRequest& req)
    {
        if (!req.HasMessage())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'sendMessage'", "message is mandatory in command 'sendMessage'");
    }

    inline void ValidateUnsubscribeMessage(const JsonRpcRequest& req)
    {
        if (!req.HasTypeId())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'unsubscribeMessage'", "typeId is mandatory in command 'unsubscribeMessage'");
    }

    inline void ValidateSubscribeEntity(const JsonRpcRequest& req)
    {
        if (!req.HasTypeId())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'subscribeEntity'", "typeId is mandatory in command 'subscribeEntity'");

        if (req.HasInstanceId() && req.HasIncludeSubclasses())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'subscribeEntity'", "Not allowed to specify both instance and includeSubclasses for the same subscription. Command 'subscribeEntity'");
    }

    inline void ValidateUnsubscribeEntity(const JsonRpcRequest& req)
    {
        if (!req.HasTypeId())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'unsubscribeEntity'", "typeId is mandatory in command 'unsubscribeEntity'");

        if (req.HasInstanceId() && req.HasIncludeSubclasses())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'unsubscribeEntity'", "Not allowed to specify both Instance and IncludeSubclasses for the same subscription. Command 'UnsubscribeEntity'");
    }

    inline void ValidateRegisterEntityHandler(const JsonRpcRequest& req)
    {
        if (!req.HasTypeId())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'registerEntityHandler'", "typeId is mandatory in command 'registerEntityHandler'");

        if (!Safir::Dob::Typesystem::Operations::IsOfType(req.TypeId(), Safir::Dob::Entity::ClassTypeId))
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'registerEntityHandler'", "typeId must refer to a subtype of Safir.Dob.Entity in command 'RegisterEntityHandler'");

        if (req.HasInjectionHandler() && req.HasPending() && req.InjectionHandler() && req.Pending())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'registerEntityHandler'", "Not allowed to specify both pending and injectionHandler for the same registration. is mandatory in command 'RegisterEntityHandler'");
    }

    inline void ValidateRegisterServiceHandler(const JsonRpcRequest& req)
    {
        if (!req.HasTypeId())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'registerServiceHandler'", "typeId is mandatory in command 'registerServiceHandler'");

        if (!Safir::Dob::Typesystem::Operations::IsOfType(req.TypeId(), Safir::Dob::Service::ClassTypeId))
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'registerServiceHandler'", "typeId must refer to a subtype of Safir.Dob.Service in command 'RegisterServiceHandler'");
    }

    inline void ValidateUnregisterHandler(const JsonRpcRequest& req)
    {
        if (!req.HasTypeId())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'unregisterHandler'", "typeId is mandatory in command 'unregisterHandler'");
    }

    inline void ValidateSubscribeRegistration(const JsonRpcRequest& req)
    {
        if (!req.HasTypeId())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'subscribeRegistration'", "typeId is mandatory in command 'subscribeRegistration'");
        if (!Safir::Dob::Typesystem::Operations::IsOfType(req.TypeId(), Safir::Dob::Entity::ClassTypeId) &&
            !Safir::Dob::Typesystem::Operations::IsOfType(req.TypeId(), Safir::Dob::Service::ClassTypeId))
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'subscribeRegistration'", "typeId must refer to a subtype of Safir.Dob.Entity or Safir.Dob.Service in command 'subscribeRegistration'");
    }

    inline void ValidateUnsubscribeRegistration(const JsonRpcRequest& req)
    {
        if (!req.HasTypeId())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'unsubscribeRegistration'", "typeId is mandatory in command 'unsubscribeRegistration'");
        if (!Safir::Dob::Typesystem::Operations::IsOfType(req.TypeId(), Safir::Dob::Entity::ClassTypeId) &&
            !Safir::Dob::Typesystem::Operations::IsOfType(req.TypeId(), Safir::Dob::Service::ClassTypeId))
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'unsubscribeRegistration'", "typeId must refer to a subtype of Safir.Dob.Entity or Safir.Dob.Service in command 'unsubscribeRegistration'");
    }

    inline void ValidateCreateRequest(const JsonRpcRequest& req)
    {
        if (!req.HasEntity())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'createRequest'", "entity is mandatory in command 'createRequest'");
    }

    inline void ValidateUpdateRequest(const JsonRpcRequest& req)
    {
        if (!req.HasEntity())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'updateRequest'", "entity is mandatory in command 'updateRequest'");
        if (!req.HasInstanceId())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'updateRequest'", "instanceId is mandatory in command 'updateRequest'");
    }

    inline void ValidateDeleteRequest(const JsonRpcRequest& req)
    {
        if (!req.HasTypeId())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'deleteRequest'", "typeId is mandatory in command 'deleteRequest'");
        if (!Safir::Dob::Typesystem::Operations::IsOfType(req.TypeId(), Safir::Dob::Entity::ClassTypeId))
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'deleteRequest'", "typeId must refer to a subtype of Safir.Dob.Entity in command 'deleteRequest'");
        if (!req.HasInstanceId())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'deleteRequest'", "instanceId is mandatory in command 'deleteRequest'");
    }

    inline void ValidateServiceRequest(const JsonRpcRequest& req)
    {
        if (!req.HasRequest())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'serviceRequest'", "request is mandatory in command 'serviceRequest'");
    }

    inline void ValidateSetEntityChanges(const JsonRpcRequest& req)
    {
        if (!req.HasEntity())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'setEntityChanges'", "entity is mandatory in command 'setEntityChanges'");
        if (!req.HasInstanceId())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'setEntityChanges'", "instanceId is mandatory in command 'setEntityChanges'");
    }

    inline void ValidateSetEntity(const JsonRpcRequest& req)
    {
        if (!req.HasEntity())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'setEntity'", "entity is mandatory in command 'setEntity'");
        if (!req.HasInstanceId())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'setEntity'", "instanceId is mandatory in command 'setEntity'");
    }

    inline void ValidateDeleteEntity(const JsonRpcRequest& req)
    {
        if (!req.HasTypeId())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'deleteEntity'", "typeId is mandatory in command 'deleteEntity'");
        if (!Safir::Dob::Typesystem::Operations::IsOfType(req.TypeId(), Safir::Dob::Entity::ClassTypeId))
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'deleteEntity'", "typeId must refer to a subtype of Safir.Dob.Entity in command 'deleteEntity'");
        if (!req.HasInstanceId())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'deleteEntity'", "instanceId is mandatory in command 'deleteEntity'");
    }

    inline void ValidateDeleteAllInstances(const JsonRpcRequest& req)
    {
        if (!req.HasTypeId())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'deleteAllInstances'", "typeId is mandatory in command 'deleteAllInstances'");
        if (!Safir::Dob::Typesystem::Operations::IsOfType(req.TypeId(), Safir::Dob::Entity::ClassTypeId))
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'deleteAllInstances'", "typeId must refer to a subtype of Safir.Dob.Entity in command 'deleteAllInstances'");
    }

    inline void ValidateReadEntity(const JsonRpcRequest& req)
    {
        if (!req.HasTypeId())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'readEntity'", "typeId is mandatory in command 'readEntity'");
        if (!Safir::Dob::Typesystem::Operations::IsOfType(req.TypeId(), Safir::Dob::Entity::ClassTypeId))
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'readEntity'", "typeId must refer to a subtype of Safir.Dob.Entity in command 'readEntity'");
        if (!req.HasInstanceId())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'readEntity'", "instanceId is mandatory in command 'readEntity'");
    }

    inline void ValidateIsCreated(const JsonRpcRequest& req)
    {
        if (!req.HasTypeId())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'isCreated'", "typeId is mandatory in command 'isCreated'");
        if (!Safir::Dob::Typesystem::Operations::IsOfType(req.TypeId(), Safir::Dob::Entity::ClassTypeId))
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'isCreated'", "typeId must refer to a subtype of Safir.Dob.Entity in command 'isCreated'");
        if (!req.HasInstanceId())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'isCreated'", "instanceId is mandatory in command 'isCreated'");
    }

    inline void ValidateGetNumberOfInstances(const JsonRpcRequest& req)
    {
        if (!req.HasTypeId())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'getNumberOfInstances'", "typeId is mandatory in command 'getNumberOfInstances'");
    }

    inline void ValidateGetAllInstanceIds(const JsonRpcRequest& req)
    {
        if (!req.HasTypeId())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'getAllInstanceIds'", "typeId is mandatory in command 'getAllInstanceIds'");
        if (!Safir::Dob::Typesystem::Operations::IsOfType(req.TypeId(), Safir::Dob::Entity::ClassTypeId))
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'getAllInstanceIds'", "typeId must refer to a subtype of Safir.Dob.Entity in command 'getAllInstanceIds'");
    }

    inline void ValidateGetInstanceIdPolicy(const JsonRpcRequest& req)
    {
        if (!req.HasTypeId())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'getInstanceIdPolicy'", "typeId is mandatory in command 'getInstanceIdPolicy'");
    }

    inline void ValidateSendSystemLog(const JsonRpcRequest& req)
    {
        static const std::vector<std::string> validSeverities = {
            "Emergency", "Alert", "Critical", "Error",
            "Warning", "Notice", "Informational", "Debug"
        };
        static const std::vector<std::string> validFacilities = {
            "Kernel", "User", "Mail", "Daemon", "Auth", "Syslog",
            "Lpr", "News", "Uucp", "Cron", "Authpriv", "Ftp",
            "Local0", "Local1", "Local2", "Local3",
            "Local4", "Local5", "Local6", "Local7"
        };

        if (!req.HasSeverityStr())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'sendSystemLog'", "severity is mandatory in command 'sendSystemLog'");
        if (std::find(validSeverities.begin(), validSeverities.end(), req.SeverityStr()) == validSeverities.end())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'sendSystemLog'", "severity has an invalid value: '" + req.SeverityStr() + "'");

        if (req.HasFacilityStr() && std::find(validFacilities.begin(), validFacilities.end(), req.FacilityStr()) == validFacilities.end())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'sendSystemLog'", "facility has an invalid value: '" + req.FacilityStr() + "'");

        if (!req.HasText())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'sendSystemLog'", "text is mandatory in command 'sendSystemLog'");
    }

    inline void ValidateResponse(const JsonRpcRequest& req)
    {
        if (req.Id().IsNull())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Send response failed", "Id can not be null when sending a response.");
        if (!req.Id().HasInt())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Send response failed", "All Safir.RequestIds are numbers. Sending responses with string id's will never match a request.");
    }

    inline Safir::Logging::Severity ParseSeverity(const std::string& s)
    {
        if (s == "Emergency")     return Safir::Logging::Emergency;
        if (s == "Alert")         return Safir::Logging::Alert;
        if (s == "Critical")      return Safir::Logging::Critical;
        if (s == "Error")         return Safir::Logging::Error;
        if (s == "Warning")       return Safir::Logging::Warning;
        if (s == "Notice")        return Safir::Logging::Notice;
        if (s == "Informational") return Safir::Logging::Informational;
        if (s == "Debug")         return Safir::Logging::Debug;
        throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid severity value: " + s);
    }

    inline Safir::Logging::Facility ParseFacility(const std::string& s)
    {
        if (s == "Kernel")   return Safir::Logging::Kernel;
        if (s == "User")     return Safir::Logging::User;
        if (s == "Mail")     return Safir::Logging::Mail;
        if (s == "Daemon")   return Safir::Logging::Daemon;
        if (s == "Auth")     return Safir::Logging::Auth;
        if (s == "Syslog")   return Safir::Logging::Syslog;
        if (s == "Lpr")      return Safir::Logging::Lpr;
        if (s == "News")     return Safir::Logging::News;
        if (s == "Uucp")     return Safir::Logging::Uucp;
        if (s == "Cron")     return Safir::Logging::Cron;
        if (s == "Authpriv") return Safir::Logging::Authpriv;
        if (s == "Ftp")      return Safir::Logging::Ftp;
        if (s == "Local0")   return Safir::Logging::Local0;
        if (s == "Local1")   return Safir::Logging::Local1;
        if (s == "Local2")   return Safir::Logging::Local2;
        if (s == "Local3")   return Safir::Logging::Local3;
        if (s == "Local4")   return Safir::Logging::Local4;
        if (s == "Local5")   return Safir::Logging::Local5;
        if (s == "Local6")   return Safir::Logging::Local6;
        if (s == "Local7")   return Safir::Logging::Local7;
        throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid facility value: " + s);
    }
}
