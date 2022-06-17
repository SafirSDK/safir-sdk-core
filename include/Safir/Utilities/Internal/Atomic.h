/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safirsdkcore.com)
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

#ifndef SAFIR_NO_DEPRECATED

#include <cstdint>
#include <atomic>

namespace Safir
{
namespace Utilities
{
namespace Internal
{
    class AtomicUint32
    {
    public:
        AtomicUint32()
        {
            m_value.store(0);
        }

        explicit AtomicUint32(const std::uint32_t initialValue)
        {
            m_value.store(initialValue);
        }

        AtomicUint32(const AtomicUint32&) = delete;
        AtomicUint32& operator=(const AtomicUint32&) = delete;

        //atomic write
        inline void operator=(const std::uint32_t value)
        {
            m_value.store(value);
        }

        //atomic post increment
        inline std::uint32_t operator++(int)
        {
            return m_value++;
        }

        //atomic post decrement
        inline std::uint32_t operator--(int)
        {
            return m_value--;
        }

        inline std::uint32_t value() const
        {
            return m_value.load();
        }

        inline std::uint32_t compare_exchange(const std::uint32_t with, std::uint32_t cmp)
        {

            m_value.compare_exchange_strong(cmp, with);
            return cmp;
        }

        inline bool operator ==(const std::uint32_t other) const
        {
            return value() == other;
        }

        inline bool operator !=(const std::uint32_t other) const
        {
            return value() != other;
        }

    private:
        std::atomic<std::uint32_t> m_value;
    };
}
}
}

#endif
