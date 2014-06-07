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

#ifndef __DOSE_LAMPORT_CLOCKS_H__
#define __DOSE_LAMPORT_CLOCKS_H__

#include <boost/cstdint.hpp>
#include <boost/noncopyable.hpp>
#include <iosfwd>
#include <climits>
#include <Safir/Dob/Internal/InternalExportDefs.h>
#include <Safir/Dob/Internal/Atomic.h>

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
        // since the first timestamp it will return will be 1.
        LamportTimestamp(): m_timestamp(0) {}

        bool operator < (const LamportTimestamp & other) const
        {
            if (m_timestamp < other.m_timestamp)
            {
                return (other.m_timestamp - m_timestamp) <= WRAP_INTERVAL;
            }
            else
            {
                return (m_timestamp - other.m_timestamp) > WRAP_INTERVAL;
            }
        }

        bool operator != (const LamportTimestamp & other) const {return m_timestamp != other.m_timestamp;}

    private:
        explicit LamportTimestamp(const boost::uint32_t clock);

        static const boost::uint64_t WRAP_INTERVAL =
            (static_cast<boost::uint64_t>(0x7fffffff) << 6);

        boost::uint32_t GetClock() const;

        friend class LamportClock;

        friend DOSE_INTERNAL_API std::wostream & operator << (std::wostream & out, const LamportTimestamp & timestamp);

        boost::uint64_t m_timestamp;
    };

    DOSE_INTERNAL_API std::wostream & operator << (std::wostream & out, const LamportTimestamp & timestamp);

    class DOSE_INTERNAL_API LamportClock:
        private boost::noncopyable
    {
    public:
        LamportClock();
        ~LamportClock();

        const LamportTimestamp GetCurrentTimestamp() const;
        void UpdateCurrentTimestamp(const LamportTimestamp & timestamp);

        const LamportTimestamp GetNewTimestamp();

    private:
        AtomicUint32 m_currentClock;
    };

}
}
}


#endif

