/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safirsdkcore.com)
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
            NEW_REMOTE_STATISTICS = 0x1,
            NEW_DATA_CHANNEL_STATISTICS = 0x2,
            NODES_CHANGED = 0x4,
            METADATA_CHANGED = 0x8,
        };

        RawChanges(const int flags)
            : m_flags(flags)
        {

        }

        bool NewRemoteStatistics() const {return (m_flags & NEW_REMOTE_STATISTICS) != 0;}
        bool NewDataChannelStatistics() const {return (m_flags & NEW_DATA_CHANNEL_STATISTICS) != 0;}
        bool NodesChanged() const {return (m_flags & NODES_CHANGED) != 0;}
        bool MetadataChanged() const {return (m_flags & METADATA_CHANGED) != 0;}

        void Print(std::wostream& out) const
        {
            out << std::boolalpha
                << "NewRemoteStatistics = " << NewRemoteStatistics()
                << ", NewDataChannelStatistics = " << NewDataChannelStatistics()
                << ", NodesChanged = " << NodesChanged()
                << ", MetadataChanged = " << MetadataChanged();
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
