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
#include <iostream>
#include "../../src/JsonRpcRequest.h"
#include "../../src/JsonRpcResponse.h"

#define CHECK(expr) {if (!(expr)) { std::cout<<"Test failed! Line: "<<__LINE__<<", expr: "<< #expr <<std::endl; exit(1);}}

inline void JsonRpcTest()
{
    //-------------------------------------------
    // Test JsonRpcRequest
    //-------------------------------------------
    {
        //all values set - parse ok
        //--------------------------
        auto json = "{\"jsonrpc\": \"2.0\", \"method\": \"open\", \"id\": \"myId\", \"params\": {"
                "\"connectionName\": \"test\","
                "\"context\": 1,"
                "\"typeId\": \"Safir.Dob.Entity\","
                "\"instanceId\": \"myInstance\","
                "\"handlerId\": 1234567890,"
                "\"channelId\": \"myChannel\","
                "\"entity\": {\"_DouType\":\"Safir.Dob.Entity\"},"
                "\"message\": {\"_DouType\":\"Safir.Dob.Message\"},"
                "\"request\": {\"_DouType\":\"Safir.Dob.Service\"},"
                "\"response\": {\"_DouType\":\"Safir.Dob.Response\"},"
                "\"pending\": true,"
                "\"injectionHandler\": false,"
                "\"includeChangeInfo\": true,"
                "\"includeSubclasses\": false,"
                "\"restartSubscription\": true,"
                "\"includeUpdates\": false"
                "}}";

        JsonRpcRequest r(json);
        CHECK(r.Method()=="open");
        CHECK(r.RpcIdType()==JsonRpcRequest::IdStr);
        CHECK(r.RpcIdStringVal()=="myId");

        CHECK(r.HasConnectionName());
        CHECK(r.ConnectionName()=="test");
        CHECK(r.HasTypeId());
        CHECK(r.TypeId()==-2411771300820206595LL);
        CHECK(r.HasInstanceId());
        CHECK(r.InstanceId().GetRawString()==L"myInstance");
        CHECK(r.HasHandlerId());
        CHECK(r.HandlerId().GetRawValue()==1234567890LL);
        CHECK(r.HasChannelId());
        CHECK(r.ChannelId().GetRawString()==L"myChannel");
        CHECK(r.HasEntity());
        CHECK(r.Entity()=="{\"_DouType\":\"Safir.Dob.Entity\"}");
        CHECK(r.HasMessage());
        CHECK(r.Message()=="{\"_DouType\":\"Safir.Dob.Message\"}");
        CHECK(r.HasRequest());
        CHECK(r.Request()=="{\"_DouType\":\"Safir.Dob.Service\"}");
        CHECK(r.HasResponse());
        CHECK(r.Response()=="{\"_DouType\":\"Safir.Dob.Response\"}");
        CHECK(r.HasPending());
        CHECK(r.Pending()==true);
        CHECK(r.HasInjectionHandler());
        CHECK(r.InjectionHandler()==false);
        CHECK(r.HasIncludeChangeInfo());
        CHECK(r.IncludeChangeInfo()==true);
        CHECK(r.HasIncludeSubclasses());
        CHECK(r.IncludeSubclasses()==false);
        CHECK(r.HasRestartSubscription());
        CHECK(r.RestartSubscription()==true);
        CHECK(r.HasIncludeUpdates());
        CHECK(r.IncludeUpdates()==false);
    }

    {
        //some values not set - parse ok
        //-------------------------------
        auto json = "{\"jsonrpc\": \"2.0\", \"method\": \"open\", \"id\": 3, \"params\": {"
                "\"context\": 1,"
                "\"instanceId\": \"myInstance\","
                "\"entity\": {\"_DouType\":\"Safir.Dob.Entity\"},"
                "\"pending\": true"
                "}}";

        JsonRpcRequest r(json);
        CHECK(r.Method()=="open");
        CHECK(r.RpcIdType()==JsonRpcRequest::IdInt);
        CHECK(r.RpcIdIntVal()==3);

        CHECK(!r.HasConnectionName());
        CHECK(!r.HasTypeId());
        CHECK(r.HasInstanceId());
        CHECK(r.InstanceId().GetRawString()==L"myInstance");
        CHECK(!r.HasHandlerId());
        CHECK(!r.HasChannelId());
        CHECK(r.HasEntity());
        CHECK(r.Entity()=="{\"_DouType\":\"Safir.Dob.Entity\"}");
        CHECK(!r.HasMessage());
        CHECK(!r.HasRequest());
        CHECK(!r.HasResponse());
        CHECK(r.HasPending());
        CHECK(r.Pending()==true);
        CHECK(!r.HasInjectionHandler());
        CHECK(!r.HasIncludeChangeInfo());
        CHECK(!r.HasIncludeSubclasses());
        CHECK(!r.HasRestartSubscription());
        CHECK(!r.HasIncludeUpdates());
    }

    {
        //invalid json
        //-------------------------------
        try
        {
            auto json = "{\"method\": open}";
            JsonRpcRequest r(json);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==RequestErrorException::ParseError);
        }
    }

    {
        //jsonrpc is missing
        //-------------------------------
        try
        {
            auto json = "{\"method\": \"open\"}";
            JsonRpcRequest r(json);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==RequestErrorException::InvalidRequest);
        }
    }

    {
        //jsonrpc wrong value
        //-------------------------------
        try
        {
            auto json = "{\"jsonrpc\": \"33.0\", \"method\": \"open\", \"id\": \"myId\"}";
            JsonRpcRequest r(json);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==RequestErrorException::InvalidRequest);
        }
    }

    {
        //method is missing
        //-------------------------------
        try
        {
            auto json = "{\"jsonrpc\": \"2.0\", \"id\": 3}";
            JsonRpcRequest r(json);
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==RequestErrorException::InvalidRequest);
        }
    }

    //-------------------------------------------
    // Test JsonRpcResponse
    //-------------------------------------------
    {
        //error
        //----------
        auto json = JsonRpcResponse::Error("Me", 123, "fail");
        CHECK(json=="{\"jsonrpc\":\"2.0\",\"error\":{\"code\":123,\"message\":\"fail\"},\"id\":\"Me\"}");
    }
    {
        //string value
        //------------
        auto json = JsonRpcResponse::String(-124534534, "Hello world");
        CHECK(json=="{\"jsonrpc\":\"2.0\",\"result\":\"Hello world\",\"id\":-124534534}");
    }
    {
        //int value
        //------------
        auto json = JsonRpcResponse::Int("Me", 100);
        CHECK(json=="{\"jsonrpc\":\"2.0\",\"result\":100,\"id\":\"Me\"}");
    }
    {
        //bool value
        //------------
        auto json = JsonRpcResponse::Bool(123, true);
        CHECK(json=="{\"jsonrpc\":\"2.0\",\"result\":true,\"id\":123}");
    }
    {
        //JSON value
        //------------
        auto json = JsonRpcResponse::Json("Mr Donk", "{_DouType:\"Safir.Dob.Entity\"}");
        CHECK(json=="{\"jsonrpc\":\"2.0\",\"result\":{_DouType:\"Safir.Dob.Entity\"},\"id\":\"Mr Donk\"}");
    }

    std::cout<<"Test passed!"<<std::endl;
}
