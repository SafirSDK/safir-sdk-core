/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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

#ifndef __LLUF_ATOMIC_H__
#define __LLUF_ATOMIC_H__

#include <boost/cstdint.hpp>
#include <boost/noncopyable.hpp>
#include <atomic>

namespace Safir
{
namespace Utilities
{
namespace Internal
{
    class AtomicUint32:
        private boost::noncopyable
    {
    public:
        AtomicUint32()
        {
            m_value.store(0);
        }

        explicit AtomicUint32(const boost::uint32_t initialValue)
        {
            m_value.store(initialValue);
        }


        //atomic write
        inline void operator=(const boost::uint32_t value)
        {
            m_value.store(value);
        }

        //atomic post increment
        inline boost::uint32_t operator++(int)
        {
            return m_value++;
        }

        //atomic post decrement
        inline boost::uint32_t operator--(int)
        {
            return m_value--;
        }

        inline boost::uint32_t value() const
        {
            return m_value.load();
        }

        inline boost::uint32_t compare_exchange(const boost::uint32_t with, boost::uint32_t cmp)
        {

            m_value.compare_exchange_strong(cmp, with);
            return cmp;
        }

        inline bool operator ==(const boost::uint32_t other) const
        {
            return value() == other;
        }

        inline bool operator !=(const boost::uint32_t other) const
        {
            return value() != other;
        }

    private:
        std::atomic<boost::uint32_t> m_value;
    };
}
}
}

#endif

