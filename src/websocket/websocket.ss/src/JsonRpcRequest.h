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
#include <boost/optional.hpp>
#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/InstanceIdPolicy.h>
#include <Safir/Dob/Typesystem/ToolSupport/TypeUtilities.h>
#include "rapidjson/document.h"
#include "rapidjson/stringbuffer.h"
#include "rapidjson/writer.h"
#include "RequestErrorException.h"
#include "JsonRpcId.h"
#include "JsonHelpers.h"

namespace rj = rapidjson;
namespace ts = Safir::Dob::Typesystem;

class JsonRpcRequest
{
public:

    static std::string Json(const std::string& method, const std::string& json, const JsonRpcId& id)
    {
        std::ostringstream os;
        if (!id.IsNull())
            os<<"{"<<SAFIR_WS_STR("jsonrpc","2.0")<<","<<SAFIR_WS_STR("method",method)<<","<<SAFIR_WS_OBJ("params", json)<<","<<id<<"}";
        else
            os<<"{"<<SAFIR_WS_STR("jsonrpc","2.0")<<","<<SAFIR_WS_STR("method",method)<<","<<SAFIR_WS_OBJ("params", json)<<"}";

        return std::move(os.str());
    }

    JsonRpcRequest(const std::string& json)
        :m_doc()
        ,m_params(nullptr)
    {
        Init(json);
    }

    const JsonRpcId& Id() const {return m_id;}

    bool IsResponse() const {return m_doc.HasMember("result");}
    std::string Result() const
    {
        rj::StringBuffer sb;
        rj::Writer<rj::StringBuffer> writer(sb);
        m_doc["result"].Accept(writer);
        return sb.GetString();
    }


    std::string Method() const {return m_doc["method"].GetString();}

    void Validate() const {ValidateInternal();}
    //params
    bool HasConnectionName() const {return HasParam("connectionName");}
    std::string ConnectionName() const {return m_doc["params"]["connectionName"].GetString();}

    bool HasContext() const {return HasParam("context");}
    int Context() const {return m_doc["params"]["context"].GetInt();}

    bool HasTypeId() const {return HasParam("typeId");}
    DotsC_TypeId TypeId() const
    {
        const rj::Value& t=m_doc["params"]["typeId"];
        if (t.IsString())
            return Safir::Dob::Typesystem::ToolSupport::TypeUtilities::CalculateTypeId(t.GetString());
        return t.GetInt64();
    }

    bool HasInstanceId() const {return HasParam("instanceId");}
    ts::InstanceId InstanceId() const {return GetHashedVal<ts::InstanceId>("instanceId");}

    bool HasHandlerId() const {return HasParam("handlerId");}
    ts::HandlerId HandlerId() const {return GetHashedVal<ts::HandlerId>("handlerId");}

    bool HasChannelId() const {return HasParam("channelId");}
    ts::ChannelId ChannelId() const {return GetHashedVal<ts::ChannelId>("channelId");}

    bool HasInstanceIdPolicy() const {return HasParam("instanceIdPolicy");}
    Safir::Dob::InstanceIdPolicy::Enumeration InstanceIdPolicy() const
    {
        if (m_doc["params"]["instanceIdPolicy"]=="HandlerDecidesInstanceId")
            return Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId;
        else
            return Safir::Dob::InstanceIdPolicy::RequestorDecidesInstanceId;
    }

    bool HasEntity() const {return HasParam("entity");}
    std::string Entity() const {return GetObject("entity");}

    bool HasMessage() const {return HasParam("message");}
    std::string Message() const {return GetObject("message");}

    bool HasRequest() const {return HasParam("request");}
    std::string Request() const {return GetObject("request");}

    bool HasPending() const {return HasParam("pending");}
    bool Pending() const {return m_doc["params"]["pending"].GetBool();}

    bool HasInjectionHandler() const {return HasParam("injectionHandler");}
    bool InjectionHandler() const {return m_doc["params"]["injectionHandler"].GetBool();}

    bool HasIncludeChangeInfo() const {return HasParam("includeChangeInfo");}
    bool IncludeChangeInfo() const {return m_doc["params"]["includeChangeInfo"].GetBool();}

    bool HasIncludeSubclasses() const {return HasParam("includeSubclasses");}
    bool IncludeSubclasses() const {return m_doc["params"]["includeSubclasses"].GetBool();}

    bool HasRestartSubscription() const {return HasParam("restartSubscription");}
    bool RestartSubscription() const {return m_doc["params"]["restartSubscription"].GetBool();}

    bool HasIncludeUpdates() const {return HasParam("includeUpdates");}
    bool IncludeUpdates() const {return m_doc["params"]["includeUpdates"].GetBool();}

private:
    rj::Document m_doc;
    JsonRpcId m_id;
    mutable const rj::Value* m_params;

    inline void Init(const std::string& json)
    {
        m_doc.Parse(json.c_str());
        if (m_doc.HasParseError())
            throw RequestErrorException("Could not parse JSON.", RequestErrorException::ParseError);

        if (m_doc.HasMember("id"))
        {
            if (m_doc["id"].IsString())
                m_id=JsonRpcId(m_doc["id"].GetString());
            else if (m_doc["id"].IsInt64())
                m_id=JsonRpcId(m_doc["id"].GetInt64());
            else if (!m_doc["id"].IsNull())
                throw RequestErrorException("Id must be a string value or an integer value.", RequestErrorException::InvalidRequest);
        }
    }

    inline bool HasParam(const char* name) const {return m_params!=nullptr ? m_params->HasMember(name) : false;}

    inline std::string GetObject(const char* name) const
    {
        rj::StringBuffer sb;
        rj::Writer<rj::StringBuffer> writer(sb);
        m_doc["params"][name].Accept(writer);
        return sb.GetString();
    }

    template<class T>
    T GetHashedVal(const char* name) const
    {
        if ((*m_params)[name].IsInt64())
            return T((*m_params)[name].GetInt64());

        return T(ts::Utilities::ToWstring((*m_params)[name].GetString()));
    }

    inline void ValidateInternal() const
    {
        if (!m_doc.HasMember("jsonrpc"))
            throw RequestErrorException("Missing jsonrpc version.", RequestErrorException::InvalidRequest);

        if (m_doc["jsonrpc"]!="2.0")
            throw RequestErrorException("JSON-RPC version is not supported.", RequestErrorException::InvalidRequest);

        //If this is actually a response and not a request, validate as response instead.
        if (IsResponse())
        {
            if (!m_doc.HasMember("result"))
                throw RequestErrorException("Response must contain the member 'result'.", RequestErrorException::InvalidParams);
            if (!m_doc["result"].IsObject())
                throw RequestErrorException("Member 'result' has wrong type. It must be an object of type Safir.Dob.Response", RequestErrorException::InvalidParams);

            return; //response is valid
        }

        if (!m_doc.HasMember("method"))
            throw RequestErrorException("Missing method.", RequestErrorException::InvalidRequest);

        if (!m_doc["method"].IsString())
            throw RequestErrorException("Method must be a string.", RequestErrorException::InvalidRequest);

        if (m_doc.HasMember("params"))
            m_params=&m_doc["params"];
        else
            return; //nothing more to validate, the rest is params

        if (m_params->HasMember("connectionName") && !(*m_params)["connectionName"].IsString())
            throw RequestErrorException("Param 'connectionName' has wrong type.", RequestErrorException::InvalidParams);

        if (m_params->HasMember("context") && !(*m_params)["context"].IsInt())
            throw RequestErrorException("Param 'context' has wrong type.", RequestErrorException::InvalidParams);

        if (m_params->HasMember("typeId") && !(*m_params)["typeId"].IsString() && !(*m_params)["typeId"].IsInt64())
            throw RequestErrorException("Param 'typeId' has wrong type.", RequestErrorException::InvalidParams);

        if (m_params->HasMember("instanceId") && !(*m_params)["instanceId"].IsInt64() && !(*m_params)["instanceId"].IsString())
            throw RequestErrorException("Param 'instanceId' has wrong type.", RequestErrorException::InvalidParams);

        if (m_params->HasMember("handlerId") && !(*m_params)["handlerId"].IsInt64() && !(*m_params)["handlerId"].IsString())
            throw RequestErrorException("Param 'handlerId' has wrong type.", RequestErrorException::InvalidParams);

        if (m_params->HasMember("channelId") && !(*m_params)["channelId"].IsInt64() && !(*m_params)["channelId"].IsString())
            throw RequestErrorException("Param 'channelId' has wrong type.", RequestErrorException::InvalidParams);

        if (m_params->HasMember("instanceIdPolicy"))
        {
            if (!(*m_params)["instanceIdPolicy"].IsString())
                throw RequestErrorException("Param 'instanceIdPolicy' has wrong type.", RequestErrorException::InvalidParams);

            if ((*m_params)["instanceIdPolicy"]!="HandlerDecidesInstanceId" && (*m_params)["instanceIdPolicy"]!="RequestorDecidesInstanceId")
                throw RequestErrorException("Param 'instanceIdPolicy' has an invalid value.", RequestErrorException::InvalidParams);
        }

        if (m_params->HasMember("entity") && !(*m_params)["entity"].IsObject())
            throw RequestErrorException("Param 'entity' has wrong type.", RequestErrorException::InvalidParams);

        if (m_params->HasMember("message") && !(*m_params)["message"].IsObject())
            throw RequestErrorException("Param 'message' has wrong type.", RequestErrorException::InvalidParams);

        if (m_params->HasMember("request") && !(*m_params)["request"].IsObject())
            throw RequestErrorException("Param 'request' has wrong type.", RequestErrorException::InvalidParams);

        if (m_params->HasMember("response") && !(*m_params)["response"].IsObject())
            throw RequestErrorException("Param 'response' has wrong type.", RequestErrorException::InvalidParams);

        if (m_params->HasMember("pending") && !(*m_params)["pending"].IsBool())
            throw RequestErrorException("Param 'pending' has wrong type.", RequestErrorException::InvalidParams);

        if (m_params->HasMember("injectionHandler") && !(*m_params)["injectionHandler"].IsBool())
            throw RequestErrorException("Param 'injectionHandler' has wrong type.", RequestErrorException::InvalidParams);

        if (m_params->HasMember("includeChangeInfo") && !(*m_params)["includeChangeInfo"].IsBool())
            throw RequestErrorException("Param 'includeChangeInfo' has wrong type.", RequestErrorException::InvalidParams);

        if (m_params->HasMember("includeSubclasses") && !(*m_params)["includeSubclasses"].IsBool())
            throw RequestErrorException("Param 'includeSubclasses' has wrong type.", RequestErrorException::InvalidParams);

        if (m_params->HasMember("restartSubscription") && !(*m_params)["restartSubscription"].IsBool())
            throw RequestErrorException("Param 'restartSubscription' has wrong type.", RequestErrorException::InvalidParams);

        if (m_params->HasMember("includeUpdates") && !(*m_params)["includeUpdates"].IsBool())
            throw RequestErrorException("Param 'includeUpdates' has wrong type.", RequestErrorException::InvalidParams);

    }
};
