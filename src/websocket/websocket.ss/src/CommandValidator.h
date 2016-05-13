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
            throw RequestErrorException("typeId is mandatory in command 'nregisterHandler'", RequestErrorException::InvalidParams);
    }

}
