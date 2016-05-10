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
#include <sstream>

#ifndef SAFIR_WS_QUOTE
#define SAFIR_WS_QUOTE(x) "\""<<x<<"\""
#define SAFIR_WS_STR(k, v) SAFIR_WS_QUOTE(k)<<":"<<SAFIR_WS_QUOTE(v)
#define SAFIR_WS_NUM(k, v) SAFIR_WS_QUOTE(k)<<":"<<v
#define SAFIR_WS_BOOL(k, v) SAFIR_WS_QUOTE(k)<<":"<<(v?"true":"false")
#endif

class JsonRpcNotification
{
public:
    template <class IdType>
    static std::string Error(const IdType& id, int code, const std::string& message)
    {
        std::ostringstream os;
        os<<"{"<<SAFIR_WS_STR("jsonrpc","2.0")<<","
            <<SAFIR_WS_QUOTE("error")<<":{"<<SAFIR_WS_NUM("code", code)<<","<<SAFIR_WS_STR("message", message)
            <<"},";
        End(os, id);
        return std::move(os.str());
    }

    template <class IdType>
    static std::string Bool(const IdType& id, bool result)
    {
        std::ostringstream os;
        os<<"{"<<SAFIR_WS_STR("jsonrpc","2.0")<<","<<SAFIR_WS_BOOL("result", result)<<",";
        End(os, id);
        return std::move(os.str());
    }

    template <class IdType>
    static std::string Int(const IdType& id, int result)
    {
        std::ostringstream os;
        os<<"{"<<SAFIR_WS_STR("jsonrpc","2.0")<<","<<SAFIR_WS_NUM("result", result)<<",";
        End(os, id);
        return std::move(os.str());
    }

    template <class IdType>
    static std::string String(const IdType& id, const std::string& result)
    {
        std::ostringstream os;
        os<<"{"<<SAFIR_WS_STR("jsonrpc","2.0")<<","<<SAFIR_WS_STR("result", result)<<",";
        End(os, id);
        return std::move(os.str());
    }

    template <class IdType>
    static std::string Json(const IdType& id, const std::string& json)
    {
        std::ostringstream os;
        os<<"{"<<SAFIR_WS_STR("jsonrpc","2.0")<<","<<SAFIR_WS_QUOTE("result")<<":"<<json<<",";
        End(os, id);
        return std::move(os.str());
    }


private:

    static void End(std::ostringstream& os, const std::string& id)
    {
        os<<SAFIR_WS_STR("id",id)<<"}";
    }

    static void End(std::ostringstream& os, boost::int64_t id)
    {
        os<<SAFIR_WS_NUM("id",id)<<"}";
    }
};
