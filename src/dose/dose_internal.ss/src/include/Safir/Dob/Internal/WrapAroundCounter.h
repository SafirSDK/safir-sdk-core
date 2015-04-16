/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
*
* Created by: Anders Widén / stawi
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

#ifndef _dose_wrap_around_counter_h
#define _dose_wrap_around_counter_h


#include <iostream>
#include <boost/limits.hpp>
#include <boost/functional/hash.hpp>
#include <Safir/Dob/Typesystem/Defs.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    /**
     *  A simple wrap around counter.
     *  will count from std::numeric_limits<Typesystem::Int32>::min() to 
     *  std::numeric_limits<Typesystem::Int32>::max()
     */
    class WrapAroundCounter
    {
    public:
        WrapAroundCounter() : m_counter(min()) {}
        explicit WrapAroundCounter(const Safir::Dob::Typesystem::Int32 counter) 
            : m_counter(counter) 
        {
        
        }

        bool operator==(const WrapAroundCounter& rhs) const 
        {
            return this->m_counter == rhs.m_counter;
        }

        bool operator!=(const WrapAroundCounter& rhs) const 
        {
            return !operator==(rhs);
        }

        const WrapAroundCounter& operator++()
        {
            if (m_counter < max())
            {
                ++m_counter;
            }
            else
            {
                m_counter = min();
            }
            
            return *this;
        }

        const WrapAroundCounter operator++(int)
        {
            const WrapAroundCounter oldVal = *this;
            ++(*this);
            return oldVal;
        }

        const WrapAroundCounter& operator--()
        {
            if (m_counter > min())
            {
                --m_counter;
            }
            else
            {
                m_counter = max();
            }
            
            return *this;
        }


        Safir::Dob::Typesystem::Int32 GetCounter() const {return m_counter;}
        void Reset() {m_counter = min();}

    private:
        static inline Safir::Dob::Typesystem::Int32 min()
        {return std::numeric_limits<Safir::Dob::Typesystem::Int32>::min();}

        static inline Safir::Dob::Typesystem::Int32 max() 
        {return std::numeric_limits<Safir::Dob::Typesystem::Int32>::max();}

        Safir::Dob::Typesystem::Int32 m_counter;
    };

    inline std::wostream& operator << (std::wostream& out, const WrapAroundCounter& counter)
    {
        return out << counter.GetCounter();
    }

    inline std::size_t hash_value(Safir::Dob::Internal::WrapAroundCounter const& c)
    {
        boost::hash<int> hasher;
        return hasher(c.GetCounter());
    }
}
}
}


#endif
