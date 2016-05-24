/******************************************************************************
*
* Copyright Saab AB, 2016 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson / joel.ottosson@consoden.se
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

#include <Safir/Dob/Message.h>
#include <Safir/Dob/Entity.h>
#include <Safir/Dob/Service.h>
#include <Safir/Dob/Response.h>
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
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Invalid parameter in method 'registerServiceHandler", "typeId is mandatory in command 'registerServiceHandler'");

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

    inline void ValidateResponse(const JsonRpcRequest& req)
    {
        if (req.Id().IsNull())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Send response failed", "Id can not be null when sending a response.");
        if (!req.Id().HasInt())
            throw RequestErrorException(JsonRpcErrorCodes::InvalidParams, "Send response failed", "All Safir.RequestIds are numbers. Sending responses with string id's will never match a request.");
    }
}
