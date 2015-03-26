/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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
#pragma once

#include <boost/noncopyable.hpp>
#include <iosfwd>
#include <Safir/Dob/Internal/InternalExportDefs.h>
#include <atomic>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class DOSE_INTERNAL_API LamportTimestamp
    {
    public:
        // A default initialized timestamp will always be older than one that is acquired with GetNewTimestamp,
        // since the first timestamp GetNewTimestamp will return will be 1.
        LamportTimestamp(): m_clock(0), m_nodeId(0) {}

        bool operator < (const LamportTimestamp& other) const
        {
            if (m_clock == other.m_clock)
            {
                return m_nodeId < other.m_nodeId;
            }
            else
            {
                return m_clock < other.m_clock;
            }
        }

        bool operator != (const LamportTimestamp& other) const
        {return m_clock != other.m_clock || m_nodeId != other.m_nodeId;}

    private:
        LamportTimestamp(const uint64_t clock, const int64_t nodeId)
            : m_clock(clock)
            , m_nodeId(nodeId)
        {

        }

        uint64_t GetClock() const { return m_clock;}

        friend class LamportClock;

        friend DOSE_INTERNAL_API std::wostream& operator << (std::wostream& out, const LamportTimestamp& timestamp);

        uint64_t m_clock;
        int64_t m_nodeId;
    };

    DOSE_INTERNAL_API std::wostream& operator << (std::wostream& out, const LamportTimestamp& timestamp);

    class DOSE_INTERNAL_API LamportClock:
        private boost::noncopyable
    {
    public:
        explicit LamportClock(const int64_t nodeId);

        void UpdateCurrentTimestamp(const LamportTimestamp& timestamp);

        const LamportTimestamp GetNewTimestamp();

    private:
        std::atomic<uint64_t> m_currentClock;
        const int64_t m_nodeId;
    };

}
}
}


