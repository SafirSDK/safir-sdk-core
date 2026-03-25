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
#include "../../src/CommandValidator.h"

inline void CommandValidatorTest()
{
    // ---- ValidateOpen ----
    {
        // missing connectionName (no params)
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"open\"}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateOpen(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // connectionName present but empty
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"open\",\"params\":{\"connectionName\":\"\"}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateOpen(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // valid connectionName
        auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"open\",\"params\":{\"connectionName\":\"myConnection\"}}";
        JsonRpcRequest r(json);
        r.Validate();
        CommandValidator::ValidateOpen(r); // must not throw
    }

    // ---- ValidateSubscribeMessage ----
    {
        // missing typeId
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"subscribeMessage\",\"params\":{}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateSubscribeMessage(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // typeId is Entity, not a Message subtype
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"subscribeMessage\",\"params\":{\"typeId\":\"Safir.Dob.Entity\"}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateSubscribeMessage(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // valid Message typeId
        auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"subscribeMessage\",\"params\":{\"typeId\":\"Safir.Dob.Message\"}}";
        JsonRpcRequest r(json);
        r.Validate();
        CommandValidator::ValidateSubscribeMessage(r); // must not throw
    }

    // ---- ValidateSendMessage ----
    {
        // missing message
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"sendMessage\",\"params\":{}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateSendMessage(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // valid message
        auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"sendMessage\",\"params\":{\"message\":{\"_DouType\":\"Safir.Dob.Message\"}}}";
        JsonRpcRequest r(json);
        r.Validate();
        CommandValidator::ValidateSendMessage(r); // must not throw
    }

    // ---- ValidateUnsubscribeMessage ----
    {
        // missing typeId
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"unsubscribeMessage\",\"params\":{}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateUnsubscribeMessage(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // valid typeId
        auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"unsubscribeMessage\",\"params\":{\"typeId\":\"Safir.Dob.Message\"}}";
        JsonRpcRequest r(json);
        r.Validate();
        CommandValidator::ValidateUnsubscribeMessage(r); // must not throw
    }

    // ---- ValidateSubscribeEntity ----
    {
        // missing typeId
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"subscribeEntity\",\"params\":{}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateSubscribeEntity(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // both instanceId and includeSubclasses specified
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"subscribeEntity\",\"params\":"
                        "{\"typeId\":\"Safir.Dob.Entity\",\"instanceId\":\"test\",\"includeSubclasses\":true}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateSubscribeEntity(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // valid typeId only
        auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"subscribeEntity\",\"params\":{\"typeId\":\"Safir.Dob.Entity\"}}";
        JsonRpcRequest r(json);
        r.Validate();
        CommandValidator::ValidateSubscribeEntity(r); // must not throw
    }

    // ---- ValidateUnsubscribeEntity ----
    {
        // missing typeId
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"unsubscribeEntity\",\"params\":{}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateUnsubscribeEntity(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // both instanceId and includeSubclasses specified
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"unsubscribeEntity\",\"params\":"
                        "{\"typeId\":\"Safir.Dob.Entity\",\"instanceId\":\"test\",\"includeSubclasses\":true}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateUnsubscribeEntity(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // valid typeId
        auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"unsubscribeEntity\",\"params\":{\"typeId\":\"Safir.Dob.Entity\"}}";
        JsonRpcRequest r(json);
        r.Validate();
        CommandValidator::ValidateUnsubscribeEntity(r); // must not throw
    }

    // ---- ValidateRegisterEntityHandler ----
    {
        // missing typeId
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"registerEntityHandler\",\"params\":{}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateRegisterEntityHandler(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // typeId is Service, not an Entity subtype
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"registerEntityHandler\",\"params\":{\"typeId\":\"Safir.Dob.Service\"}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateRegisterEntityHandler(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // both pending=true and injectionHandler=true
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"registerEntityHandler\",\"params\":"
                        "{\"typeId\":\"Safir.Dob.Entity\",\"pending\":true,\"injectionHandler\":true}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateRegisterEntityHandler(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // valid Entity typeId
        auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"registerEntityHandler\",\"params\":{\"typeId\":\"Safir.Dob.Entity\"}}";
        JsonRpcRequest r(json);
        r.Validate();
        CommandValidator::ValidateRegisterEntityHandler(r); // must not throw
    }
    {
        // pending=true, injectionHandler=false — allowed combination
        auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"registerEntityHandler\",\"params\":"
                    "{\"typeId\":\"Safir.Dob.Entity\",\"pending\":true,\"injectionHandler\":false}}";
        JsonRpcRequest r(json);
        r.Validate();
        CommandValidator::ValidateRegisterEntityHandler(r); // must not throw
    }

    // ---- ValidateRegisterServiceHandler ----
    {
        // missing typeId
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"registerServiceHandler\",\"params\":{}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateRegisterServiceHandler(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // typeId is Entity, not a Service subtype
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"registerServiceHandler\",\"params\":{\"typeId\":\"Safir.Dob.Entity\"}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateRegisterServiceHandler(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // valid Service typeId
        auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"registerServiceHandler\",\"params\":{\"typeId\":\"Safir.Dob.Service\"}}";
        JsonRpcRequest r(json);
        r.Validate();
        CommandValidator::ValidateRegisterServiceHandler(r); // must not throw
    }

    // ---- ValidateUnregisterHandler ----
    {
        // missing typeId
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"unregisterHandler\",\"params\":{}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateUnregisterHandler(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // valid typeId
        auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"unregisterHandler\",\"params\":{\"typeId\":\"Safir.Dob.Entity\"}}";
        JsonRpcRequest r(json);
        r.Validate();
        CommandValidator::ValidateUnregisterHandler(r); // must not throw
    }

    // ---- ValidateSubscribeRegistration ----
    {
        // missing typeId
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"subscribeRegistration\",\"params\":{}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateSubscribeRegistration(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // typeId is Message — neither Entity nor Service subtype
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"subscribeRegistration\",\"params\":{\"typeId\":\"Safir.Dob.Message\"}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateSubscribeRegistration(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // valid Entity typeId
        auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"subscribeRegistration\",\"params\":{\"typeId\":\"Safir.Dob.Entity\"}}";
        JsonRpcRequest r(json);
        r.Validate();
        CommandValidator::ValidateSubscribeRegistration(r); // must not throw
    }
    {
        // valid Service typeId
        auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"subscribeRegistration\",\"params\":{\"typeId\":\"Safir.Dob.Service\"}}";
        JsonRpcRequest r(json);
        r.Validate();
        CommandValidator::ValidateSubscribeRegistration(r); // must not throw
    }

    // ---- ValidateUnsubscribeRegistration ----
    {
        // missing typeId
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"unsubscribeRegistration\",\"params\":{}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateUnsubscribeRegistration(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // typeId is Message — neither Entity nor Service subtype
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"unsubscribeRegistration\",\"params\":{\"typeId\":\"Safir.Dob.Message\"}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateUnsubscribeRegistration(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // valid Entity typeId
        auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"unsubscribeRegistration\",\"params\":{\"typeId\":\"Safir.Dob.Entity\"}}";
        JsonRpcRequest r(json);
        r.Validate();
        CommandValidator::ValidateUnsubscribeRegistration(r); // must not throw
    }

    // ---- ValidateCreateRequest ----
    {
        // missing entity
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"createRequest\",\"params\":{}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateCreateRequest(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // valid entity
        auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"createRequest\",\"params\":{\"entity\":{\"_DouType\":\"Safir.Dob.Entity\"}}}";
        JsonRpcRequest r(json);
        r.Validate();
        CommandValidator::ValidateCreateRequest(r); // must not throw
    }

    // ---- ValidateUpdateRequest ----
    {
        // missing entity
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"updateRequest\",\"params\":{}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateUpdateRequest(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // entity present but missing instanceId
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"updateRequest\",\"params\":{\"entity\":{\"_DouType\":\"Safir.Dob.Entity\"}}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateUpdateRequest(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // valid entity + instanceId
        auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"updateRequest\",\"params\":"
                    "{\"entity\":{\"_DouType\":\"Safir.Dob.Entity\"},\"instanceId\":\"test\"}}";
        JsonRpcRequest r(json);
        r.Validate();
        CommandValidator::ValidateUpdateRequest(r); // must not throw
    }

    // ---- ValidateDeleteRequest ----
    {
        // missing typeId
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"deleteRequest\",\"params\":{}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateDeleteRequest(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // typeId is Service, not an Entity subtype
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"deleteRequest\",\"params\":"
                        "{\"typeId\":\"Safir.Dob.Service\",\"instanceId\":\"test\"}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateDeleteRequest(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // Entity typeId but missing instanceId
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"deleteRequest\",\"params\":{\"typeId\":\"Safir.Dob.Entity\"}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateDeleteRequest(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // valid Entity typeId + instanceId
        auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"deleteRequest\",\"params\":"
                    "{\"typeId\":\"Safir.Dob.Entity\",\"instanceId\":\"test\"}}";
        JsonRpcRequest r(json);
        r.Validate();
        CommandValidator::ValidateDeleteRequest(r); // must not throw
    }

    // ---- ValidateServiceRequest ----
    {
        // missing request
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"serviceRequest\",\"params\":{}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateServiceRequest(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // valid request
        auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"serviceRequest\",\"params\":{\"request\":{\"_DouType\":\"Safir.Dob.Service\"}}}";
        JsonRpcRequest r(json);
        r.Validate();
        CommandValidator::ValidateServiceRequest(r); // must not throw
    }

    // ---- ValidateSetEntityChanges ----
    {
        // missing entity
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"setEntityChanges\",\"params\":{}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateSetEntityChanges(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // entity present but missing instanceId
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"setEntityChanges\",\"params\":{\"entity\":{\"_DouType\":\"Safir.Dob.Entity\"}}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateSetEntityChanges(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // valid entity + instanceId
        auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"setEntityChanges\",\"params\":"
                    "{\"entity\":{\"_DouType\":\"Safir.Dob.Entity\"},\"instanceId\":\"test\"}}";
        JsonRpcRequest r(json);
        r.Validate();
        CommandValidator::ValidateSetEntityChanges(r); // must not throw
    }

    // ---- ValidateSetEntity ----
    {
        // missing entity
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"setEntity\",\"params\":{}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateSetEntity(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // entity present but missing instanceId
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"setEntity\",\"params\":{\"entity\":{\"_DouType\":\"Safir.Dob.Entity\"}}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateSetEntity(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // valid entity + instanceId
        auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"setEntity\",\"params\":"
                    "{\"entity\":{\"_DouType\":\"Safir.Dob.Entity\"},\"instanceId\":\"test\"}}";
        JsonRpcRequest r(json);
        r.Validate();
        CommandValidator::ValidateSetEntity(r); // must not throw
    }

    // ---- ValidateDeleteEntity ----
    {
        // missing typeId
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"deleteEntity\",\"params\":{}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateDeleteEntity(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // typeId is Service, not an Entity subtype
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"deleteEntity\",\"params\":"
                        "{\"typeId\":\"Safir.Dob.Service\",\"instanceId\":\"test\"}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateDeleteEntity(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // Entity typeId but missing instanceId
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"deleteEntity\",\"params\":{\"typeId\":\"Safir.Dob.Entity\"}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateDeleteEntity(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // valid Entity typeId + instanceId
        auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"deleteEntity\",\"params\":"
                    "{\"typeId\":\"Safir.Dob.Entity\",\"instanceId\":\"test\"}}";
        JsonRpcRequest r(json);
        r.Validate();
        CommandValidator::ValidateDeleteEntity(r); // must not throw
    }

    // ---- ValidateDeleteAllInstances ----
    {
        // missing typeId
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"deleteAllInstances\",\"params\":{}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateDeleteAllInstances(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // typeId is Service, not an Entity subtype
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"deleteAllInstances\",\"params\":{\"typeId\":\"Safir.Dob.Service\"}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateDeleteAllInstances(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // valid Entity typeId
        auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"deleteAllInstances\",\"params\":{\"typeId\":\"Safir.Dob.Entity\"}}";
        JsonRpcRequest r(json);
        r.Validate();
        CommandValidator::ValidateDeleteAllInstances(r); // must not throw
    }

    // ---- ValidateReadEntity ----
    {
        // missing typeId
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"readEntity\",\"params\":{}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateReadEntity(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // typeId is Service, not an Entity subtype
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"readEntity\",\"params\":"
                        "{\"typeId\":\"Safir.Dob.Service\",\"instanceId\":\"test\"}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateReadEntity(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // Entity typeId but missing instanceId
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"readEntity\",\"params\":{\"typeId\":\"Safir.Dob.Entity\"}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateReadEntity(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // valid Entity typeId + instanceId
        auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"readEntity\",\"params\":"
                    "{\"typeId\":\"Safir.Dob.Entity\",\"instanceId\":\"test\"}}";
        JsonRpcRequest r(json);
        r.Validate();
        CommandValidator::ValidateReadEntity(r); // must not throw
    }

    // ---- ValidateIsCreated ----
    {
        // missing typeId
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"isCreated\",\"params\":{}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateIsCreated(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // typeId is Service, not an Entity subtype
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"isCreated\",\"params\":"
                        "{\"typeId\":\"Safir.Dob.Service\",\"instanceId\":\"test\"}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateIsCreated(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // Entity typeId but missing instanceId
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"isCreated\",\"params\":{\"typeId\":\"Safir.Dob.Entity\"}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateIsCreated(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // valid Entity typeId + instanceId
        auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"isCreated\",\"params\":"
                    "{\"typeId\":\"Safir.Dob.Entity\",\"instanceId\":\"test\"}}";
        JsonRpcRequest r(json);
        r.Validate();
        CommandValidator::ValidateIsCreated(r); // must not throw
    }

    // ---- ValidateGetNumberOfInstances ----
    {
        // missing typeId
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"getNumberOfInstances\",\"params\":{}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateGetNumberOfInstances(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // valid typeId
        auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"getNumberOfInstances\",\"params\":{\"typeId\":\"Safir.Dob.Entity\"}}";
        JsonRpcRequest r(json);
        r.Validate();
        CommandValidator::ValidateGetNumberOfInstances(r); // must not throw
    }

    // ---- ValidateGetAllInstanceIds ----
    {
        // missing typeId
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"getAllInstanceIds\",\"params\":{}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateGetAllInstanceIds(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // typeId is Service, not an Entity subtype
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"getAllInstanceIds\",\"params\":{\"typeId\":\"Safir.Dob.Service\"}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateGetAllInstanceIds(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // valid Entity typeId
        auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"getAllInstanceIds\",\"params\":{\"typeId\":\"Safir.Dob.Entity\"}}";
        JsonRpcRequest r(json);
        r.Validate();
        CommandValidator::ValidateGetAllInstanceIds(r); // must not throw
    }

    // ---- ValidateGetInstanceIdPolicy ----
    {
        // missing typeId
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"getInstanceIdPolicy\",\"params\":{}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateGetInstanceIdPolicy(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // valid typeId
        auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"getInstanceIdPolicy\",\"params\":{\"typeId\":\"Safir.Dob.Entity\"}}";
        JsonRpcRequest r(json);
        r.Validate();
        CommandValidator::ValidateGetInstanceIdPolicy(r); // must not throw
    }

    // ---- ValidateResponse ----
    {
        // null id (no id field in request)
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"sendResponse\",\"params\":{}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateResponse(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // string id — not a valid Safir RequestId (must be integer)
        try
        {
            auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"sendResponse\",\"id\":\"strId\",\"params\":{}}";
            JsonRpcRequest r(json);
            r.Validate();
            CommandValidator::ValidateResponse(r);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }
    {
        // valid integer id
        auto json = "{\"jsonrpc\":\"2.0\",\"method\":\"sendResponse\",\"id\":42,\"params\":{}}";
        JsonRpcRequest r(json);
        r.Validate();
        CommandValidator::ValidateResponse(r); // must not throw
    }
}
