/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
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

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace SP
{

    class RawChanges
    {
    public:
        enum Flags
        {
            NEW_REMOTE_DATA = 0x1,
            NODES_CHANGED = 0x2,
            ELECTION_ID_CHANGED = 0x4,
        };

        RawChanges(const int flags)
            : m_flags(flags)
        {

        }

        bool NewRemoteData() const {return (m_flags & NEW_REMOTE_DATA) != 0;}
        bool NodesChanged() const {return (m_flags & NODES_CHANGED) != 0;}
        bool ElectionIdChanged() const {return (m_flags & ELECTION_ID_CHANGED) != 0;}

        void Print(std::wostream& out) const
        {
            out << std::boolalpha
                << "NewRemoteData = " << NewRemoteData()
                << ", NodesChanged = " << NodesChanged()
                << ", ElectionIdChanged = " << ElectionIdChanged();
        }

    private:
        int m_flags;
    };

    std::wostream& operator<<(std::wostream& out, const RawChanges& flags)
    {
        flags.Print(out);
        return out;
    }
}
}
}
}
