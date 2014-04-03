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

#if defined(_MSC_VER)
#define LLUF_ATOMIC_USE_INTERLOCKED
#endif

#if defined (LLUF_ATOMIC_USE_INTERLOCKED)
#include <boost/detail/interlocked.hpp>
#endif
//TODO: use c++11 atomics when boost and msvc support it.

namespace Safir
{
namespace Utilities
{
namespace Internal
{
#if defined (LLUF_ATOMIC_USE_INTERLOCKED)
    namespace Win32
    {
        inline boost::uint32_t atomic_inc32(volatile boost::uint32_t *mem)
        {  return BOOST_INTERLOCKED_INCREMENT(reinterpret_cast<volatile long*>(mem)) - 1;  }

        inline boost::uint32_t atomic_dec32(volatile boost::uint32_t *mem)
        {  return BOOST_INTERLOCKED_DECREMENT(reinterpret_cast<volatile long*>(mem)) + 1;  }

        inline boost::uint32_t atomic_read32(volatile boost::uint32_t *mem)
        {  
            //we use interlocked op to get memory barrier.
            volatile boost::uint32_t res;
            BOOST_INTERLOCKED_EXCHANGE(reinterpret_cast<volatile long*>(&res),
                                       *reinterpret_cast<volatile long*>(mem));
            return res;   
        }
        
        inline void atomic_write32(volatile boost::uint32_t *mem, boost::uint32_t val)
        {  BOOST_INTERLOCKED_EXCHANGE(reinterpret_cast<volatile long*>(mem), val); }
        
        inline boost::uint32_t atomic_cas32
        (volatile boost::uint32_t *mem, boost::uint32_t with, boost::uint32_t cmp)
        {  return BOOST_INTERLOCKED_COMPARE_EXCHANGE(reinterpret_cast<volatile long*>(mem), with, cmp);  }

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
            Win32::atomic_write32(&m_value,value);
        }
        
        //atomic post increment
        inline boost::uint32_t operator++(int)
        {
            return Win32::atomic_inc32(&m_value);
        }

        //atomic post decrement
        inline boost::uint32_t operator--(int)
        {
            return Win32::atomic_dec32(&m_value);
        }

        inline boost::uint32_t value() const
        {
            return Win32::atomic_read32(const_cast<boost::uint32_t*>(&m_value));
        }

        inline boost::uint32_t compare_exchange(const boost::uint32_t with, const boost::uint32_t cmp)
        {
            return Win32::atomic_cas32(&m_value,with,cmp);
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

#else

    class AtomicUint32:
        private boost::noncopyable
    {
    public:
        AtomicUint32():
            m_value(0) {}
        
        explicit AtomicUint32(const boost::uint32_t initialValue):
            m_value(initialValue) {}

           
        //atomic write
        inline void operator=(boost::uint32_t value)
        {
            __asm__ __volatile__ ("" ::: "memory");
            __asm__ (
                     "xchgl %0, %1"
                     : "+r" (value), "+m" (m_value)
                     );
            __asm__ __volatile__ ("" ::: "memory");
        }
        
        //atomic post increment
        inline boost::uint32_t operator++(int)
        {
            return __sync_fetch_and_add(&m_value,1);
        }

        //atomic post decrement
        inline boost::uint32_t operator--(int)
        {
            return __sync_fetch_and_sub(&m_value,1);
        }

        inline boost::uint32_t value() const
        {
            const boost::uint32_t v = m_value;
            __asm__ __volatile__ ("" ::: "memory");
            return v;
        }

        inline boost::uint32_t compare_exchange(const boost::uint32_t with, boost::uint32_t expected)
        {
            return __sync_val_compare_and_swap(&m_value,expected,with);
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
#endif
}
}
}

#endif

