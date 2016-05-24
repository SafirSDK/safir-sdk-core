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
#include "JsonRpcId.h"
#include "JsonHelpers.h"

class JsonRpcResponse
{
public:
    static std::string Error(const JsonRpcId& id, int code, const std::string& message)
    {
        std::ostringstream os;
        os<<"{"<<SAFIR_WS_STR("jsonrpc","2.0")<<","
            <<SAFIR_WS_QUOTE("error")<<":{"<<SAFIR_WS_NUM("code", code)<<","<<SAFIR_WS_STR("message", message)
            <<"},"<<id<<"}";
        return std::move(os.str());
    }

    static std::string Bool(const JsonRpcId& id, bool result)
    {
        std::ostringstream os;
        os<<"{"<<SAFIR_WS_STR("jsonrpc","2.0")<<","<<SAFIR_WS_BOOL("result", result)<<","<<id<<"}";
        return std::move(os.str());
    }

    static std::string Int(const JsonRpcId& id, boost::int64_t result)
    {
        std::ostringstream os;
        os<<"{"<<SAFIR_WS_STR("jsonrpc","2.0")<<","<<SAFIR_WS_NUM("result", result)<<","<<id<<"}";
        return std::move(os.str());
    }

    static std::string String(const JsonRpcId& id, const std::string& result)
    {
        std::ostringstream os;
        os<<"{"<<SAFIR_WS_STR("jsonrpc","2.0")<<","<<SAFIR_WS_STR("result", result)<<","<<id<<"}";
        return std::move(os.str());
    }

    static std::string Json(const JsonRpcId& id, const std::string& json)
    {
        std::ostringstream os;
        os<<"{"<<SAFIR_WS_STR("jsonrpc","2.0")<<","<<SAFIR_WS_OBJ("result", json)<<","<<id<<"}";
        return std::move(os.str());
    }

    template <class T>
    static std::string QuotedArray(const JsonRpcId& id, const std::vector<T>& array)
    {
        std::ostringstream os;
        os<<"{"<<SAFIR_WS_STR("jsonrpc","2.0")<<","<<SAFIR_WS_QUOTE("result")<<":[";
        auto comma=false;
        for (auto it = array.begin(); it != array.end(); ++it)
        {
            if (comma)
                os<<","<<SAFIR_WS_QUOTE(*it);
            else
                os<<SAFIR_WS_QUOTE(*it);

            comma=true;
        }
        os<<"],"<<id<<"}";
        return std::move(os.str());
    }

    template <class T>
    static std::string UnquotedArray(const JsonRpcId& id, const std::vector<T>& array)
    {
        std::ostringstream os;
        os<<"{"<<SAFIR_WS_STR("jsonrpc","2.0")<<","<<SAFIR_WS_QUOTE("result")<<":[";
        auto comma=false;
        for (auto it = array.begin(); it != array.end(); ++it)
        {
            if (comma)
            {
                os<<","<<*it;
            }
            else
            {
                os<<*it;
            }

            comma=true;
        }
        os<<"],"<<id<<"}";
        return std::move(os.str());
    }

private:

};
