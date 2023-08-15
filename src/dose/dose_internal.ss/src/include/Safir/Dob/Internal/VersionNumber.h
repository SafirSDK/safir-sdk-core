/******************************************************************************
*
* Copyright Saab AB, 2008-2023 (http://safirsdkcore.com)
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
#ifndef __VERSIONNUMBER_H__
#define __VERSIONNUMBER_H__

#include <iostream>
#include <cstdint>
#include <limits>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class VersionNumber
    {
    public:
        VersionNumber():m_counter(0){}
        VersionNumber(uint16_t initialValue):m_counter(initialValue){}

        VersionNumber& operator++(){++m_counter; return *this;}

        VersionNumber& operator--(){--m_counter; return *this;}

        bool operator<(const VersionNumber & other) const
        {
            if (this->m_counter < other.m_counter)
            {
                if (other.m_counter - this->m_counter > max()/2)
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
                if (this->m_counter - other.m_counter > max()/2)
                {
                    return true;
                }
                else
                {
                    return false;
                }
            }
        }

        bool operator != (const VersionNumber & other) const {return m_counter != other.m_counter;}

        bool IsDiffGreaterThanOne(const VersionNumber & other) const
        {
            return ((other.m_counter - m_counter) % max()) > 1;
        }

        std::uint16_t GetRawValue() const { return m_counter; }

    private:
        static inline std::uint16_t max() {return std::numeric_limits<std::uint16_t>::max();}

        friend std::wostream & operator << (std::wostream & out, const VersionNumber & version)
        {
            return out << version.m_counter;
        }

        std::uint16_t m_counter;
    };
/*
    static inline std::wostream & operator << (std::wostream & out, const VersionNumber & version)
    {

    }*/
}
}
}

#endif

