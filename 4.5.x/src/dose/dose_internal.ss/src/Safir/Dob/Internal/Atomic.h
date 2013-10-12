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

#ifndef __DOSE_ATOMIC_H__
#define __DOSE_ATOMIC_H__

#include <boost/version.hpp>
#include <boost/interprocess/detail/atomic.hpp>
#include <boost/noncopyable.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    namespace Atomics 
    {
        //the namespace changed name in 1.48
#if ((BOOST_VERSION / 100000) >= 1 && (BOOST_VERSION / 100 % 1000) > 47)
        using boost::interprocess::ipcdetail::atomic_inc32;
        using boost::interprocess::ipcdetail::atomic_dec32;
        using boost::interprocess::ipcdetail::atomic_read32;
        using boost::interprocess::ipcdetail::atomic_write32;
        using boost::interprocess::ipcdetail::atomic_cas32;
#else
        using boost::interprocess::detail::atomic_inc32;
        using boost::interprocess::detail::atomic_dec32;
        using boost::interprocess::detail::atomic_read32;
        using boost::interprocess::detail::atomic_write32;
        using boost::interprocess::detail::atomic_cas32;
#endif
    }

    class AtomicUint32:
        private boost::noncopyable
    {
    public:
        AtomicUint32():
            m_value(0) {}

        explicit AtomicUint32(const boost::uint32_t initialValue):
            m_value(initialValue) {}

           
        //atomic write
        inline void operator=(const boost::uint32_t value)
        {
            Atomics::atomic_write32(&m_value,value);
        }
        
        //atomic post increment
        inline boost::uint32_t operator++(int)
        {
            return Atomics::atomic_inc32(&m_value);
        }

        //atomic post decrement
        inline boost::uint32_t operator--(int)
        {
            return Atomics::atomic_dec32(&m_value);
        }

        inline boost::uint32_t value() const
        {
            return Atomics::atomic_read32(const_cast<boost::uint32_t*>(&m_value));
        }

        inline boost::uint32_t compare_exchange(const boost::uint32_t with, const boost::uint32_t cmp)
        {
            return Atomics::atomic_cas32(&m_value,with,cmp);
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
        volatile boost::uint32_t m_value;
    };


}
}
}

#endif

