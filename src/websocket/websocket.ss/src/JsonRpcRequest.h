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


namespace rj = rapidjson;
namespace ts = Safir::Dob::Typesystem;

class JsonRpcRequest
{
public:
    enum IdType {IdStr, IdInt, IdNull, IdInvalid};

    JsonRpcRequest(const std::string& json)
        :m_doc()
        ,m_idType(IdNull)
    {
        m_doc.Parse(json.c_str());
        if (m_doc.HasParseError())
            throw RequestErrorException("Could not parse JSON.", RequestErrorException::ParseError);

        Validate();
    }

    IdType RpcIdType() const {return m_idType;}
    std::string RpcIdStringVal() const {return m_doc["id"].GetString();}
    boost::int64_t RpcIdIntVal() const {return m_doc["id"].GetInt64();}

    std::string Method() const {return m_doc["method"].GetString();}

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

    bool HasResponse() const {return HasParam("response");}
    std::string Response() const {return GetObject("response");}

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

    bool m_valid;
    IdType m_idType;
    std::string m_method;

    inline bool HasParam(const char* name) const
    {
        if (m_doc.HasMember("params"))
        {
            return m_doc["params"].HasMember(name);
        }
        return false;
    }

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
        const rj::Value& p=m_doc["params"];
        if (p[name].IsInt64())
            return T(p[name].GetInt64());

        return T(ts::Utilities::ToWstring(p[name].GetString()));
    }


    inline void Validate()
    {
        if (!m_doc.HasMember("jsonrpc") || m_doc["jsonrpc"]!="2.0")
            throw RequestErrorException("Missing jsonrpc version.", RequestErrorException::InvalidRequest);

        if (!m_doc.HasMember("method") || !m_doc["method"].IsString())
            throw RequestErrorException("Missing method.", RequestErrorException::InvalidRequest);

        if (m_doc.HasMember("id"))
        {
            if (m_doc["id"].IsString())
                m_idType=IdStr;
            else if (m_doc["id"].IsInt64())
                m_idType=IdInt;
            else if (!m_doc["id"].IsNull())
                throw RequestErrorException("Id must be a string value or an integer value.", RequestErrorException::InvalidRequest);
        }

        if (!m_doc.HasMember("params"))
            return;

        const rj::Value& p = m_doc["params"];

        if (p.HasMember("connectionName") && !p["connectionName"].IsString())
            throw RequestErrorException("Param 'connectionName' has wrong type.", RequestErrorException::InvalidParams);

        if (p.HasMember("context") && !p["context"].IsInt())
            throw RequestErrorException("Param 'context' has wrong type.", RequestErrorException::InvalidParams);

        if (p.HasMember("typeId") && !p["typeId"].IsString() && !p["typeId"].IsInt64())
            throw RequestErrorException("Param 'typeId' has wrong type.", RequestErrorException::InvalidParams);

        if (p.HasMember("instanceId") && !p["instanceId"].IsInt64() && !p["instanceId"].IsString())
            throw RequestErrorException("Param 'instanceId' has wrong type.", RequestErrorException::InvalidParams);

        if (p.HasMember("handlerId") && !p["handlerId"].IsInt64() && !p["handlerId"].IsString())
            throw RequestErrorException("Param 'handlerId' has wrong type.", RequestErrorException::InvalidParams);

        if (p.HasMember("channelId") && !p["channelId"].IsInt64() && !p["channelId"].IsString())
            throw RequestErrorException("Param 'channelId' has wrong type.", RequestErrorException::InvalidParams);

        if (p.HasMember("instanceIdPolicy"))
        {
            if (!p["instanceIdPolicy"].IsString())
                throw RequestErrorException("Param 'instanceIdPolicy' has wrong type.", RequestErrorException::InvalidParams);

            if (p["instanceIdPolicy"]!="HandlerDecidesInstanceId" && p["instanceIdPolicy"]!="RequestorDecidesInstanceId")
                throw RequestErrorException("Param 'instanceIdPolicy' has an invalid value.", RequestErrorException::InvalidParams);
        }

        if (p.HasMember("entity") && !p["entity"].IsObject())
            throw RequestErrorException("Param 'entity' has wrong type.", RequestErrorException::InvalidParams);

        if (p.HasMember("message") && !p["message"].IsObject())
            throw RequestErrorException("Param 'message' has wrong type.", RequestErrorException::InvalidParams);

        if (p.HasMember("request") && !p["request"].IsObject())
            throw RequestErrorException("Param 'request' has wrong type.", RequestErrorException::InvalidParams);

        if (p.HasMember("response") && !p["response"].IsObject())
            throw RequestErrorException("Param 'response' has wrong type.", RequestErrorException::InvalidParams);

        if (p.HasMember("pending") && !p["pending"].IsBool())
            throw RequestErrorException("Param 'pending' has wrong type.", RequestErrorException::InvalidParams);

        if (p.HasMember("injectionHandler") && !p["injectionHandler"].IsBool())
            throw RequestErrorException("Param 'injectionHandler' has wrong type.", RequestErrorException::InvalidParams);

        if (p.HasMember("includeChangeInfo") && !p["includeChangeInfo"].IsBool())
            throw RequestErrorException("Param 'includeChangeInfo' has wrong type.", RequestErrorException::InvalidParams);

        if (p.HasMember("includeSubclasses") && !p["includeSubclasses"].IsBool())
            throw RequestErrorException("Param 'includeSubclasses' has wrong type.", RequestErrorException::InvalidParams);

        if (p.HasMember("restartSubscription") && !p["restartSubscription"].IsBool())
            throw RequestErrorException("Param 'restartSubscription' has wrong type.", RequestErrorException::InvalidParams);

        if (p.HasMember("includeUpdates") && !p["includeUpdates"].IsBool())
            throw RequestErrorException("Param 'includeUpdates' has wrong type.", RequestErrorException::InvalidParams);

    }
};
