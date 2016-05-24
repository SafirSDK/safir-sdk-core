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
#include "../../src/JsonHelpers.h"
#include "../../src/JsonRpcRequest.h"
#include "../../src/JsonRpcResponse.h"
#include "../../src/JsonRpcNotification.h"

#ifdef _MSC_VER
#pragma warning (disable: 4127)
#endif

#define CHECK(expr) {if (!(expr)) { std::cout<<"Test failed! Line: "<<__LINE__<<", expr: "<< #expr <<std::endl; exit(1);}}

inline void JsonRpcTest()
{
    //-------------------------------------------
    // Test JsonHelpers
    //-------------------------------------------
    {
        auto json="[1, 2, [1,2]]";
        CHECK(JsonHelpers::IsArray(json));

        json="[1, 2, [1,2]]";
        CHECK(!JsonHelpers::IsObject(json));

        json="{\"param\":1, \"[1,2]\"}";
        CHECK(!JsonHelpers::IsArray(json));

        json="{\"param\":1, \"[1,2]\"}";
        CHECK(JsonHelpers::IsObject(json));
    }
    {
        std::ostringstream os;
        os<<ts::InstanceId(L"Test");
        CHECK(os.str()=="\"Test\"");
    }
    {
        std::ostringstream os;
        os<<ts::InstanceId(123);
        CHECK(os.str()=="123");
    }
    {
        std::ostringstream os;
        ts::ChannelId i;
        os<<ts::ChannelId();
        CHECK(os.str()=="\"DEFAULT_CHANNEL\"");
    }
    {
        std::ostringstream os;
        os<<ts::ChannelId(L"Test");
        CHECK(os.str()=="\"Test\"");
    }
    {
        std::ostringstream os;
        os<<ts::ChannelId(123);
        CHECK(os.str()=="123");
    }
    {
        std::ostringstream os;
        ts::HandlerId i;
        os<<ts::HandlerId();
        CHECK(os.str()=="\"DEFAULT_HANDLER\"");
    }
    {
        std::ostringstream os;
        os<<ts::HandlerId(L"Test");
        CHECK(os.str()=="\"Test\"");
    }
    {
        std::ostringstream os;
        os<<ts::HandlerId(123);
        CHECK(os.str()=="123");
    }

    //-------------------------------------------
    // Test JsonRpcRequest
    //-------------------------------------------
    {
        auto json=JsonRpcRequest::Json("myMethod", "{\"myVar\":3}", JsonRpcId(1));
        CHECK(json=="{\"jsonrpc\":\"2.0\",\"method\":\"myMethod\",\"params\":{\"myVar\":3},\"id\":1}");

        json=JsonRpcRequest::Json("myMethod", "{\"myVar\":3}", JsonRpcId("myId"));
        CHECK(json=="{\"jsonrpc\":\"2.0\",\"method\":\"myMethod\",\"params\":{\"myVar\":3},\"id\":\"myId\"}");

        json=JsonRpcRequest::Json("myMethod", "{\"myVar\":3}", JsonRpcId());
        CHECK(json=="{\"jsonrpc\":\"2.0\",\"method\":\"myMethod\",\"params\":{\"myVar\":3}}");
    }
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
                "\"pending\": true,"
                "\"injectionHandler\": false,"
                "\"includeChangeInfo\": true,"
                "\"includeSubclasses\": false,"
                "\"restartSubscription\": true,"
                "\"includeUpdates\": false"
                "}}";

        JsonRpcRequest r(json);
        r.Validate();
        CHECK(r.Method()=="open");
        CHECK(r.Id().HasStr());
        CHECK(r.Id().String()=="myId");

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
        r.Validate();
        CHECK(r.Method()=="open");
        CHECK(r.Id().HasInt())
        CHECK(r.Id().Int()==3);

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
            r.Validate();
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::ParseError);
        }
    }

    {
        //jsonrpc is missing
        //-------------------------------
        try
        {
            auto json = "{\"method\": \"open\"}";
            JsonRpcRequest r(json);
            r.Validate();
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidRequest);
        }
    }

    {
        //jsonrpc wrong value
        //-------------------------------
        try
        {
            auto json = "{\"jsonrpc\": \"33.0\", \"method\": \"open\", \"id\": \"myId\"}";
            JsonRpcRequest r(json);
            r.Validate();
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidRequest);
        }
    }

    {
        //method is missing
        //-------------------------------
        try
        {
            auto json = "{\"jsonrpc\": \"2.0\", \"id\": 3}";
            JsonRpcRequest r(json);
            r.Validate();
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidRequest);
        }
    }

    //-------------------------------------------
    // Test JsonRpcResponse
    //-------------------------------------------

    // ---- Test receive JsonRpcResponse
    {
        //Receive response - all values set - parse ok
        //---------------------------------------------
        auto json = "{\"jsonrpc\": \"2.0\", \"id\": \"myId\","
                "\"result\": {\"_DouType\":\"Safir.Dob.Response\"}}";

        JsonRpcRequest r(json);
        r.Validate();
        CHECK(r.IsResponse());
        CHECK(r.Result()=="{\"_DouType\":\"Safir.Dob.Response\"}");
        CHECK(r.Id().HasStr());
        CHECK(r.Id().String()=="myId");
    }
    {
        //Receive response - wrong type
        //-------------------------------
        try
        {
            auto json = "{\"jsonrpc\": \"2.0\", \"id\": \"myId\","
                    "\"result\": \"strVal\"}";

            JsonRpcRequest r(json);
            r.Validate();
            CHECK(false);
        }
        catch (const RequestErrorException& e)
        {
            CHECK(e.Code()==JsonRpcErrorCodes::InvalidParams);
        }
    }

    // ---- Test send JsonRpcResponse
    {
        //error
        //----------
        auto json = JsonRpcResponse::Error(JsonRpcId("Me"), 123, "msg", "data");
        CHECK(json=="{\"jsonrpc\":\"2.0\",\"error\":{\"code\":123,\"message\":\"msg\",\"data\":\"data\"},\"id\":\"Me\"}");
    }
    {
        //error
        //----------
        auto json = JsonRpcResponse::Error(JsonRpcId(), 123, "fail", "");
        CHECK(json=="{\"jsonrpc\":\"2.0\",\"error\":{\"code\":123,\"message\":\"fail\"},\"id\":null}");
    }
    {
        //string value
        //------------
        auto json = JsonRpcResponse::String(JsonRpcId(-124534534), "Hello world");
        CHECK(json=="{\"jsonrpc\":\"2.0\",\"result\":\"Hello world\",\"id\":-124534534}");
    }
    {
        //int value
        //------------
        auto json = JsonRpcResponse::Int(JsonRpcId("Me"), 100);
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
        auto json = JsonRpcResponse::Json(JsonRpcId("Mr Donk"), "{\"_DouType\":\"Safir.Dob.Entity\"}");
        CHECK(json=="{\"jsonrpc\":\"2.0\",\"result\":{\"_DouType\":\"Safir.Dob.Entity\"},\"id\":\"Mr Donk\"}");
    }
    {
        //string array
        //------------
        std::vector<std::string> v;
        v.push_back("a");
        v.push_back("b");
        v.push_back("c");
        auto json = JsonRpcResponse::QuotedArray(JsonRpcId("Mr Donk"), v);
        CHECK(json=="{\"jsonrpc\":\"2.0\",\"result\":[\"a\",\"b\",\"c\"],\"id\":\"Mr Donk\"}");
        v.clear();
        json = JsonRpcResponse::QuotedArray(JsonRpcId("Mr Donk"), v);
        CHECK(json=="{\"jsonrpc\":\"2.0\",\"result\":[],\"id\":\"Mr Donk\"}");
    }
    {
        //int array
        //------------
        std::vector<int> v;
        v.push_back(1);
        v.push_back(2);
        v.push_back(3);
        auto json = JsonRpcResponse::UnquotedArray(JsonRpcId("Mr Donk"), v);
        CHECK(json=="{\"jsonrpc\":\"2.0\",\"result\":[1,2,3],\"id\":\"Mr Donk\"}");
        v.clear();
        json = JsonRpcResponse::UnquotedArray(JsonRpcId("Mr Donk"), v);
        CHECK(json=="{\"jsonrpc\":\"2.0\",\"result\":[],\"id\":\"Mr Donk\"}");
    }
    {
        //instanceId array
        //------------
        std::vector<ts::InstanceId> v;
        v.push_back(ts::InstanceId(1));
        v.push_back(ts::InstanceId(L"id2"));
        auto json = JsonRpcResponse::UnquotedArray(JsonRpcId("Mr Donk"), v);
        CHECK(json=="{\"jsonrpc\":\"2.0\",\"result\":[1,\"id2\"],\"id\":\"Mr Donk\"}");
        v.clear();
        json = JsonRpcResponse::UnquotedArray(JsonRpcId("Mr Donk"), v);
        CHECK(json=="{\"jsonrpc\":\"2.0\",\"result\":[],\"id\":\"Mr Donk\"}");
    }


    //-------------------------------------------
    // Test JsonRpcNotification
    //-------------------------------------------
    {
        auto json=JsonRpcNotification::Json("myMethod", "{\"myVar\":3}");
        CHECK(json=="{\"jsonrpc\":\"2.0\",\"method\":\"myMethod\",\"params\":{\"myVar\":3}}");

        json=JsonRpcNotification::Empty("myMethod");
        CHECK(json=="{\"jsonrpc\":\"2.0\",\"method\":\"myMethod\"}");
    }
}
