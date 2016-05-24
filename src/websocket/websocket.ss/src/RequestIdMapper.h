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

#include <boost/unordered_map.hpp>
#include <Safir/Dob/Defs.h>
#include "JsonRpcId.h"

class RequestIdMapper
{
public:
    void Add(Safir::Dob::RequestId reqId, const JsonRpcId& id)
    {
        m_map[reqId]=id;
    }

    JsonRpcId Get(Safir::Dob::RequestId reqId)
    {
        auto it=m_map.find(reqId);
        if (it!=m_map.end())
        {
            auto id=it->second;
            m_map.erase(it);
            return id;
        }

        return JsonRpcId(); //not found, then return a null id
    }

    size_t Count() const {return m_map.size();}

private:
    boost::unordered_map<Safir::Dob::RequestId, JsonRpcId> m_map;
};
