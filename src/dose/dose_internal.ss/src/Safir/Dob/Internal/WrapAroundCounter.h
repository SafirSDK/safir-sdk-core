/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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

namespace Safir
{
namespace Dob
{
namespace Internal
{
    /**
     *  A simple wrap around counter template.
     *  Note that only integer scalar types are allowed.
     *  will count from std::numeric_limits<T>::min() to std::numeric_limits<T>::max()
     */
    template <typename T>
    class WrapAroundCounter
    {
    public:
        WrapAroundCounter() : m_counter(min()) {}
        explicit WrapAroundCounter(const T counter) : m_counter(counter) {}

        bool operator<(const WrapAroundCounter& rhs) const;
        bool operator<=(const WrapAroundCounter& rhs) const {return operator==(rhs) || operator<(rhs);};
        bool operator==(const WrapAroundCounter& rhs) const {return this->m_counter == rhs.m_counter;}
        bool operator!=(const WrapAroundCounter& rhs) const {return !operator==(rhs);}
        const WrapAroundCounter& operator++();
        const WrapAroundCounter operator++(int);
        const WrapAroundCounter& operator--();

        T GetCounter() const {return m_counter;}
        void Reset() {m_counter = min();}

    private:
        static inline T min() {return std::numeric_limits<T>::min();}
        static inline T max() {return std::numeric_limits<T>::max();}

        T m_counter;
    };

    template <typename T>
    bool WrapAroundCounter<T>::operator<(const WrapAroundCounter& rhs) const
    {
        if (this->m_counter < rhs.m_counter)
        {
            if (rhs.m_counter - this->m_counter > (max()-min())/2)
            {
                return false;
            }
            else
            {
                return true;
            }
        }
        else
        {
            if (this->m_counter - rhs.m_counter > (max()-min())/2)
            {
                return true;
            }
            else
            {
                return false;
            }
        }
    }

    template <typename T>
    const WrapAroundCounter<T>& WrapAroundCounter<T>::operator++()
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

    template <typename T>
    const WrapAroundCounter<T> WrapAroundCounter<T>::operator++(int)
    {
        const WrapAroundCounter oldVal = *this;
        ++(*this);
        return oldVal;
    }

    template <typename T>
    const WrapAroundCounter<T>& WrapAroundCounter<T>::operator--()
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


    template <typename T>
    std::wostream & operator << (std::wostream & out, const WrapAroundCounter<T> & counter)
    {
        return out << counter.GetCounter();
    }
}
}
}

#endif
