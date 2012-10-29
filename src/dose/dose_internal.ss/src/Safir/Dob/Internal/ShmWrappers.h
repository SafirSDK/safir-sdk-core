/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
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

#ifndef __DOSE_INTERNAL_SHM_WRAPPERS_H__
#define __DOSE_INTERNAL_SHM_WRAPPERS_H__

#include <Safir/Dob/Internal/SharedMemoryObject.h>
#include <Safir/Dob/Typesystem/HandlerId.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class ShmHandlerId:
        public SharedMemoryObject
    {
    public:
        explicit ShmHandlerId(const Dob::Typesystem::HandlerId& handlerId):
            m_handlerId(handlerId.GetRawValue()),
            m_handlerIdStr(handlerId.GetRawString().begin(),handlerId.GetRawString().end()) {}

        const Typesystem::HandlerId GetHandlerId() const {return Typesystem::HandlerId(m_handlerId,
                                                                                       std::wstring(m_handlerIdStr.begin(), m_handlerIdStr.end()));}

        ShmHandlerId& operator=(const ShmHandlerId& other)
        {
            m_handlerId = other.m_handlerId;
            m_handlerIdStr = other.m_handlerIdStr;
            return *this;
        }

        /**
         * Equality operator.
         *
         * @param other [in] - The HandlerId to compare with.
         */
        bool operator ==(const ShmHandlerId & other) const
        {
            return m_handlerId == other.m_handlerId;
        }

        /**
         * Inequality operator.
         *
         * @param other [in] - The HandlerId to compare with.
         */
        bool operator !=(const ShmHandlerId & other) const
        {
            return !(*this==other);
        }

        /**
         * Equality operator.
         *
         * @param other [in] - The HandlerId to compare with.
         */
        bool operator ==(const Typesystem::HandlerId & other) const
        {
            return m_handlerId == other.GetRawValue();
        }

        /**
         * Inequality operator.
         *
         * @param other [in] - The HandlerId to compare with.
         */
        bool operator !=(const Typesystem::HandlerId & other) const
        {
            return !(*this==other);
        }


        /**
         * Less-than operator.
         * This is provided to allow HandlerIds to be stored in STL containers that need strict weak ordering.
         *
         * @param other [in] - The HandlerId to compare with.
         */
        bool operator < (const ShmHandlerId & other) const
        {
            return m_handlerId < other.m_handlerId;
        }

        typedef Dob::Typesystem::HandlerId::UnderlyingType UnderlyingType;
        UnderlyingType GetRawValue() const {return m_handlerId;}


        const wchar_t * GetRawString() const {return m_handlerIdStr.c_str();}

    private:
        UnderlyingType m_handlerId;
        ShmWString m_handlerIdStr;
    };

    static inline bool operator ==(const Typesystem::HandlerId & lhs, const ShmHandlerId & rhs)
    {return rhs == lhs;}
    static inline bool operator !=(const Typesystem::HandlerId & lhs, const ShmHandlerId & rhs)
    {return rhs != lhs;}
}
}
}


#endif

