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

#include <string>
#include <vector>
#include <sstream>
#include <boost/algorithm/string.hpp>
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/Typesystem/HandlerId.h>
#include <Safir/Dob/Typesystem/ChannelId.h>
#include <Safir/Dob/Typesystem/InstanceId.h>

namespace sd = Safir::Dob;
namespace ts = Safir::Dob::Typesystem;

#ifndef SAFIR_WS_QUOTE
#define SAFIR_WS_QUOTE(x) "\""<<x<<"\""
#define SAFIR_WS_STR(k, v) SAFIR_WS_QUOTE(k)<<":"<<SAFIR_WS_QUOTE(v)
#define SAFIR_WS_NUM(k, v) SAFIR_WS_QUOTE(k)<<":"<<v
#define SAFIR_WS_OBJ(k, v) SAFIR_WS_QUOTE(k)<<":"<<v
#define SAFIR_WS_BOOL(k, v) SAFIR_WS_QUOTE(k)<<":"<<(v?"true":"false")
#endif

namespace JsonHelpers
{
    template <class T>
    std::ostream& AddHashedVal(std::ostream& os, const std::string& name, const T& hash)
    {
        if (hash.Utf8StringLength()>0)
        {
            os<<SAFIR_WS_STR(name, hash.Utf8String());
        }
        else
        {
            os<<SAFIR_WS_NUM(name, hash.GetRawValue());
        }

        return os;
    }

    inline bool IsArray(const std::string& json)
    {
        return json[0]=='[' && json[json.length()-1]==']';
    }

    inline bool IsObject(const std::string& json)
    {
        return json[0]=='{' && json[json.length()-1]=='}';
    }

    inline std::vector<std::string> SplitArrayOfObjects(const std::string& json)
    {
        std::vector<std::string> result;

        size_t uncloseBracketCount=0;
        size_t startIndex=0;
        for (size_t i=0; i<json.length(); i++)
        {
            if (json[i]=='{')
            {
                if (uncloseBracketCount==0)
                {
                    startIndex=i;
                }
                ++uncloseBracketCount;
            }
            else if (json[i]=='}')
            {
                --uncloseBracketCount;
                if (uncloseBracketCount==0)
                {
                    result.emplace_back(json.substr(startIndex, i-startIndex+1));
                }
            }
        }

        return result;
    }
}

inline std::ostream& operator<<(std::ostream& os, const ts::HandlerId& hash) {return JsonHelpers::AddHashedVal(os,"handlerId", hash);}
inline std::ostream& operator<<(std::ostream& os, const ts::ChannelId& hash) {return JsonHelpers::AddHashedVal(os,"channelId", hash);}
inline std::ostream& operator<<(std::ostream& os, const ts::InstanceId& hash) {return JsonHelpers::AddHashedVal(os,"instanceId", hash);}

class ProxyToJson
{
public:
    enum EntityRequestType {CreateReqType, UpdateReqType, DeleteReqType};

    ProxyToJson(const boost::function< std::string(ts::TypeId) >& typeIdToName,
                const boost::function< sd::InstanceIdPolicy::Enumeration(ts::TypeId, const ts::HandlerId&) >& getInstIdPolicy)
        :m_typeIdToName(typeIdToName)
        ,m_getInstIdPolicy(getInstIdPolicy)
    {
    }

    std::string ToJson(const sd::EntityProxy& proxy, bool previousEntity=false) const
    {
        std::ostringstream os;
        os<<"{"<<proxy.GetInstanceId()<<","<<SAFIR_WS_OBJ("entity",ts::Internal::ToJson((previousEntity ? proxy.GetPrevious().GetBlob() : proxy.GetBlob())))<<"}";
        return std::move(os.str());
    }

    std::string ToJson(const sd::MessageProxy& proxy) const
    {
        std::ostringstream os;
        os<<"{"<<proxy.GetChannelId()<<","<<SAFIR_WS_OBJ("message",ts::Internal::ToJson(proxy.GetBlob()))<<"}";
        return std::move(os.str());
    }

    std::string ToJson(const sd::ResponseProxy& proxy) const
    {
        std::ostringstream os;
        os<<"{"<<SAFIR_WS_BOOL("isSuccess",proxy.IsSuccess())<<","<<SAFIR_WS_OBJ("response", ts::Internal::ToJson(proxy.GetBlob()))<<"}";
        return std::move(os.str());
    }

    std::string ToJson(const sd::ServiceRequestProxy& proxy) const
    {
        std::ostringstream os;
        os<<"{"<<proxy.GetReceivingHandlerId()<<","<<SAFIR_WS_OBJ("request",ts::Internal::ToJson(proxy.GetBlob()))<<"}";
        return std::move(os.str());
    }

    std::string ToJson(const Safir::Dob::EntityRequestProxy &proxy, EntityRequestType reqType) const
    {
        std::ostringstream os;
        os<<"{"<<proxy.GetReceivingHandlerId()<<",";

        switch (reqType)
        {
            case CreateReqType:
            {
                if (m_getInstIdPolicy(proxy.GetTypeId(), proxy.GetReceivingHandlerId())==sd::InstanceIdPolicy::RequestorDecidesInstanceId)
                {
                    os<<proxy.GetInstanceId()<<",";
                }
                os<<SAFIR_WS_OBJ("request",ts::Internal::ToJson(proxy.GetBlob()))<<"}";
            }
                break;

            case UpdateReqType:
            {
                os<<proxy.GetInstanceId()<<",";
                os<<SAFIR_WS_OBJ("request",ts::Internal::ToJson(proxy.GetBlob()))<<"}";
            }
                break;

            case DeleteReqType:
            {
                os<<SAFIR_WS_STR("typeId", m_typeIdToName(proxy.GetTypeId()))<<","<<proxy.GetInstanceId()<<"}";
            }
                break;
        }

        return std::move(os.str());
    }

    std::string ToJson(const Safir::Dob::InjectedEntityProxy &proxy) const
    {
        std::ostringstream os;
        os<<"{"<<proxy.GetInstanceId()<<","<<SAFIR_WS_OBJ("entity",ts::Internal::ToJson(proxy.GetInjectionBlob()))<<"}";
        return std::move(os.str());
    }

    std::string ToJson(Safir::Dob::Typesystem::TypeId typeId, const Safir::Dob::Typesystem::HandlerId &handler) const
    {
        std::ostringstream os;
        os<<"{"<<SAFIR_WS_STR("typeId", m_typeIdToName(typeId))<<","<<handler<<"}";
        return std::move(os.str());
    }

private:
    boost::function< std::string(ts::TypeId) > m_typeIdToName;
    boost::function< sd::InstanceIdPolicy::Enumeration(ts::TypeId, const ts::HandlerId&) > m_getInstIdPolicy;
};
