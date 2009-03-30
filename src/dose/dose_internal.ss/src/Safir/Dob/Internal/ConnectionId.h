/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
*
* Created by: Lars Hagström / stlrha
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

#ifndef __DOSE_CONNECTION_ID_H__
#define __DOSE_CONNECTION_ID_H__

#include <Safir/Dob/Typesystem/Defs.h>
#include <set>
#include <iostream>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    typedef Dob::Typesystem::Int32 NodeNumber;

    typedef Dob::Typesystem::Int64 Identifier;
    typedef std::set<Identifier> IdentifierSet;

#pragma pack (push)
#pragma pack (1)
    //POD type that is possible to store in shared memory.
    //Since it is a POD type it does not inherit from SharedMemoryObject!
    struct ConnectionId
    {
        ConnectionId(): m_node(-1),m_id(-1){}
        ConnectionId(const NodeNumber node, const Identifier id): m_node(node), m_id(id) {}

        NodeNumber m_node;
        Identifier m_id; //enough to identify a connection globally among connected DOB's


        //pass other as value since this is a POD type. should be cheaper than a deref
        bool operator ==(const ConnectionId & other) const
        {
            return
                m_node == other.m_node &&
                m_id == other.m_id;
        }

        bool operator !=(const ConnectionId & other) const
        {
            return !(*this == other);
        }

        bool operator < (const ConnectionId & other) const
        {
            if (m_id != other.m_id)
            {
                return m_id < other.m_id;
            }
            else
            {
                return m_node < other.m_node;
            }
        }


    };

#pragma pack (pop)

    typedef std::set<ConnectionId> ConnectionIdSet;

    static inline std::wostream & operator << (std::wostream & out, const ConnectionId id)
    {
        return out << "(" << id.m_id << ", " << id.m_node << ")";
    }
}
}
}

#endif
