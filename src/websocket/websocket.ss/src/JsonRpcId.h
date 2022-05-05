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

#include <iostream>
#include <string>
#include <cstdint>

class JsonRpcId
{
public:
    JsonRpcId() : m_type(0) {}
    JsonRpcId(std::int64_t id) : m_type(1), m_i(id) {}
    JsonRpcId(const std::string& id) : m_type(2), m_s(id) {}

    bool IsNull() const {return m_type==0;}
    bool HasInt() const {return m_type==1;}
    bool HasStr() const {return m_type==2;}

    std::int64_t Int() const {return m_i;}
    const std::string& String() const {return m_s;}

private:
    int m_type; //0=null, 1=int, 2=str
    std::int64_t m_i;
    std::string m_s;
};

inline std::ostream& operator<<(std::ostream& os, const JsonRpcId& id)
{
    if (id.HasStr())
        os<<"\"id\":\""<<id.String()<<"\"";
    else if (id.HasInt())
        os<<"\"id\":"<<id.Int();
    else
        os<<"\"id\":null";
    return os;
}
