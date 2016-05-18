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
#define SAFIR_WS_OBJ(k, v) SAFIR_WS_QUOTE(k)<<":"<<v
#define SAFIR_WS_BOOL(k, v) SAFIR_WS_QUOTE(k)<<":"<<(v?"true":"false")
#endif

class JsonRpcNotification
{
public:

    static std::string Empty(const std::string& method)
    {
        std::ostringstream os;
        os<<"{"<<SAFIR_WS_STR("jsonrpc","2.0")<<","<<SAFIR_WS_STR("method",method)<<"}";
        return std::move(os.str());
    }

    static std::string Json(const std::string& method, const std::string& json)
    {
        std::ostringstream os;
        os<<"{"<<SAFIR_WS_STR("jsonrpc","2.0")<<","<<SAFIR_WS_STR("method",method)<<","<<SAFIR_WS_OBJ("params", json)<<"}";
        return std::move(os.str());
    }


private:


};
