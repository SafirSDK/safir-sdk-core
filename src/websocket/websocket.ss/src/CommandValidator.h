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
            throw RequestErrorException("connectionName is mandatory in command 'Open'", RequestErrorException::InvalidParams);
    }

    inline void ValidateSubscribeMessage(const JsonRpcRequest& req)
    {
        if (!req.HasTypeId())
            throw RequestErrorException("typeId is mandatory in command 'SubscribeMessage'", RequestErrorException::InvalidParams);

        if (!Safir::Dob::Typesystem::Operations::IsOfType(req.TypeId(), Safir::Dob::Message::ClassTypeId))
            throw RequestErrorException("typeId must refer to a subtype of Safir.Dob.Message in command 'SubscribeMessage'", RequestErrorException::InvalidParams);
    }

    inline void ValidateSendMessage(const JsonRpcRequest& req)
    {
        if (!req.HasMessage())
            throw RequestErrorException("message is mandatory in command 'SendMessage'", RequestErrorException::InvalidParams);
    }

    inline void ValidateUnsubscribeMessage(const JsonRpcRequest& req)
    {
        if (!req.HasTypeId())
            throw RequestErrorException("typeId is mandatory in command 'UnsubscribeMessage'", RequestErrorException::InvalidParams);
    }

    inline void ValidateSubscribeEntity(const JsonRpcRequest& req)
    {
        if (!req.HasTypeId())
            throw RequestErrorException("typeId is mandatory in command 'SubscribeEntity'", RequestErrorException::InvalidParams);

        if (req.HasInstanceId() && req.HasIncludeSubclasses())
            throw RequestErrorException("Not allowed to specify both instance and includeSubclasses for the same subscription. Command 'SubscribeEntity'", RequestErrorException::InvalidParams);
    }

    inline void ValidateUnsubscribeEntity(const JsonRpcRequest& req)
    {
        if (!req.HasTypeId())
            throw RequestErrorException("typeId is mandatory in command 'UnsubscribeEntity'", RequestErrorException::InvalidParams);

        if (req.HasInstanceId() && req.HasIncludeSubclasses())
            throw RequestErrorException("Not allowed to specify both Instance and IncludeSubclasses for the same subscription. Command 'UnsubscribeEntity'", RequestErrorException::InvalidParams);
    }

    inline void ValidateRegisterEntityHandler(const JsonRpcRequest& req)
    {
        if (!req.HasTypeId())
            throw RequestErrorException("typeId is mandatory in command 'RegisterEntityHandler'", RequestErrorException::InvalidParams);

        if (!Safir::Dob::Typesystem::Operations::IsOfType(req.TypeId(), Safir::Dob::Entity::ClassTypeId))
            throw RequestErrorException("typeId must refer to a subtype of Safir.Dob.Entity in command 'RegisterEntityHandler'", RequestErrorException::InvalidParams);

        if (req.HasInjectionHandler() && req.HasPending() && req.InjectionHandler() && req.Pending())
            throw RequestErrorException("Not allowed to specify both pending and injectionHandler for the same registration. is mandatory in command 'RegisterEntityHandler'", RequestErrorException::InvalidParams);
    }

    inline void ValidateUnregisterHandler(const JsonRpcRequest& req)
    {
        if (!req.HasTypeId())
            throw RequestErrorException("typeId is mandatory in command 'unregisterHandler'", RequestErrorException::InvalidParams);
    }

    inline void ValidateSubscribeRegistration(const JsonRpcRequest& req)
    {
        if (!req.HasTypeId())
            throw RequestErrorException("typeId is mandatory in command 'subscribeRegistration'", RequestErrorException::InvalidParams);
        if (!Safir::Dob::Typesystem::Operations::IsOfType(req.TypeId(), Safir::Dob::Entity::ClassTypeId) &&
            !Safir::Dob::Typesystem::Operations::IsOfType(req.TypeId(), Safir::Dob::Service::ClassTypeId))
            throw RequestErrorException("typeId must refer to a subtype of Safir.Dob.Entity or Safir.Dob.Service in command 'subscribeRegistration'", RequestErrorException::InvalidParams);
    }

    inline void ValidateUnsubscribeRegistration(const JsonRpcRequest& req)
    {
        if (!req.HasTypeId())
            throw RequestErrorException("typeId is mandatory in command 'unsubscribeRegistration'", RequestErrorException::InvalidParams);
        if (!Safir::Dob::Typesystem::Operations::IsOfType(req.TypeId(), Safir::Dob::Entity::ClassTypeId) &&
            !Safir::Dob::Typesystem::Operations::IsOfType(req.TypeId(), Safir::Dob::Service::ClassTypeId))
            throw RequestErrorException("typeId must refer to a subtype of Safir.Dob.Entity or Safir.Dob.Service in command 'unsubscribeRegistration'", RequestErrorException::InvalidParams);
    }

    inline void ValidateCreateRequest(const JsonRpcRequest& req)
    {
        if (!req.HasEntity())
            throw RequestErrorException("entity is mandatory in command 'createRequest'", RequestErrorException::InvalidParams);
    }

    inline void ValidateUpdateRequest(const JsonRpcRequest& req)
    {
        if (!req.HasEntity())
            throw RequestErrorException("entity is mandatory in command 'updateRequest'", RequestErrorException::InvalidParams);
        if (!req.HasInstanceId())
            throw RequestErrorException("instanceId is mandatory in command 'updateRequest'", RequestErrorException::InvalidParams);
    }

    inline void ValidateDeleteRequest(const JsonRpcRequest& req)
    {
        if (!req.HasTypeId())
            throw RequestErrorException("typeId is mandatory in command 'deleteRequest'", RequestErrorException::InvalidParams);
        if (!Safir::Dob::Typesystem::Operations::IsOfType(req.TypeId(), Safir::Dob::Entity::ClassTypeId))
            throw RequestErrorException("typeId must refer to a subtype of Safir.Dob.Entity in command 'deleteRequest'", RequestErrorException::InvalidParams);
        if (!req.HasInstanceId())
            throw RequestErrorException("instanceId is mandatory in command 'deleteRequest'", RequestErrorException::InvalidParams);
    }

    inline void ValidateServiceRequest(const JsonRpcRequest& req)
    {
        if (!req.HasRequest())
            throw RequestErrorException("request is mandatory in command 'serviceRequest'", RequestErrorException::InvalidParams);
    }

    inline void ValidateSetEntityChanges(const JsonRpcRequest& req)
    {
        if (!req.HasEntity())
            throw RequestErrorException("entity is mandatory in command 'setEntityChanges'", RequestErrorException::InvalidParams);
        if (!req.HasInstanceId())
            throw RequestErrorException("instanceId is mandatory in command 'setEntityChanges'", RequestErrorException::InvalidParams);
    }

    inline void ValidateSetEntity(const JsonRpcRequest& req)
    {
        if (!req.HasEntity())
            throw RequestErrorException("entity is mandatory in command 'setEntity'", RequestErrorException::InvalidParams);
        if (!req.HasInstanceId())
            throw RequestErrorException("instanceId is mandatory in command 'setEntity'", RequestErrorException::InvalidParams);
    }

    inline void ValidateDeleteEntity(const JsonRpcRequest& req)
    {
        if (!req.HasTypeId())
            throw RequestErrorException("typeId is mandatory in command 'deleteEntity'", RequestErrorException::InvalidParams);
        if (!Safir::Dob::Typesystem::Operations::IsOfType(req.TypeId(), Safir::Dob::Entity::ClassTypeId))
            throw RequestErrorException("typeId must refer to a subtype of Safir.Dob.Entity in command 'deleteEntity'", RequestErrorException::InvalidParams);
        if (!req.HasInstanceId())
            throw RequestErrorException("instanceId is mandatory in command 'deleteEntity'", RequestErrorException::InvalidParams);
    }

    inline void ValidateDeleteAllInstances(const JsonRpcRequest& req)
    {
        if (!req.HasTypeId())
            throw RequestErrorException("typeId is mandatory in command 'deleteAllInstances'", RequestErrorException::InvalidParams);
        if (!Safir::Dob::Typesystem::Operations::IsOfType(req.TypeId(), Safir::Dob::Entity::ClassTypeId))
            throw RequestErrorException("typeId must refer to a subtype of Safir.Dob.Entity in command 'deleteAllInstances'", RequestErrorException::InvalidParams);
    }

    inline void ValidateReadEntity(const JsonRpcRequest& req)
    {
        if (!req.HasTypeId())
            throw RequestErrorException("typeId is mandatory in command 'readEntity'", RequestErrorException::InvalidParams);
        if (!Safir::Dob::Typesystem::Operations::IsOfType(req.TypeId(), Safir::Dob::Entity::ClassTypeId))
            throw RequestErrorException("typeId must refer to a subtype of Safir.Dob.Entity in command 'readEntity'", RequestErrorException::InvalidParams);
        if (!req.HasInstanceId())
            throw RequestErrorException("instanceId is mandatory in command 'readEntity'", RequestErrorException::InvalidParams);
    }

    inline void ValidateIsCreated(const JsonRpcRequest& req)
    {
        if (!req.HasTypeId())
            throw RequestErrorException("typeId is mandatory in command 'isCreated'", RequestErrorException::InvalidParams);
        if (!Safir::Dob::Typesystem::Operations::IsOfType(req.TypeId(), Safir::Dob::Entity::ClassTypeId))
            throw RequestErrorException("typeId must refer to a subtype of Safir.Dob.Entity in command 'isCreated'", RequestErrorException::InvalidParams);
        if (!req.HasInstanceId())
            throw RequestErrorException("instanceId is mandatory in command 'isCreated'", RequestErrorException::InvalidParams);
    }

    inline void ValidateGetNumberOfInstances(const JsonRpcRequest& req)
    {
        if (!req.HasTypeId())
            throw RequestErrorException("typeId is mandatory in command 'getNumberOfInstances'", RequestErrorException::InvalidParams);
    }

    inline void ValidateGetAllInstanceIds(const JsonRpcRequest& req)
    {
        if (!req.HasTypeId())
            throw RequestErrorException("typeId is mandatory in command 'getAllInstanceIds'", RequestErrorException::InvalidParams);
        if (!Safir::Dob::Typesystem::Operations::IsOfType(req.TypeId(), Safir::Dob::Entity::ClassTypeId))
            throw RequestErrorException("typeId must refer to a subtype of Safir.Dob.Entity in command 'getAllInstanceIds'", RequestErrorException::InvalidParams);
    }

    inline void ValidateGetInstanceIdPolicy(const JsonRpcRequest& req)
    {
        if (!req.HasTypeId())
            throw RequestErrorException("typeId is mandatory in command 'getInstanceIdPolicy'", RequestErrorException::InvalidParams);
    }

    inline void ValidateResponse(const JsonRpcRequest& req)
    {
        if (req.Id().IsNull())
            throw RequestErrorException("Id can not be null when sending a response.", RequestErrorException::InvalidParams);
        if (!req.Id().HasInt())
            throw RequestErrorException("All Safir.RequestIds are numbers. Sending responses with string id's will never match a request.", RequestErrorException::InvalidParams);
    }
}
