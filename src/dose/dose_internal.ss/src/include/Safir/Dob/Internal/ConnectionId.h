/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safirsdkcore.com)
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

#include <Safir/Dob/Defs.h>
#include <Safir/Dob/Typesystem/Defs.h>
#include <set>
#include <iostream>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    typedef Dob::Typesystem::Int64 Identifier;
    typedef std::set<Identifier> IdentifierSet;

    //POD type that is possible to store in shared memory.
    //Since it is a POD type it does not inherit from SharedMemoryObject!
    struct ConnectionId
    {
        ConnectionId(): m_node(-1), m_id(-1), m_contextId(-1) {} //NOLINT
        ConnectionId(const int64_t node, const ContextId contextId, const Identifier id): m_node(node),  m_id(id), m_contextId(contextId) {} //NOLINT

#pragma pack (push)
#pragma pack (4)
        int64_t m_node;
        Identifier m_id; //enough to identify a connection globally among connected DOB's
        ContextId  m_contextId;
        int32_t  m_padding;
#pragma pack (pop)

        bool operator ==(const ConnectionId & other) const
        {
            // m_context isn't part of the comparison since it is included in m_id.
            // (So is m_node, but sometimes we need to compare connection ids where the id is the same (-1)
            //  but node differs)
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


    typedef std::set<ConnectionId> ConnectionIdSet;

    static inline std::wostream & operator << (std::wostream & out, const ConnectionId id)
    {
        return out << "(" << id.m_id << ", " << id.m_node << ", " << id.m_contextId << ")";
    }
}
}
}

#endif
