/******************************************************************************
*
* Copyright Saab AB, 2004-2013 (http://safir.sourceforge.net)
* 
* Created by: Joel Ottosson / stjoot
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

#ifndef _dots_parameter_description_h
#define _dots_parameter_description_h

#include "dots_internal_defs.h"
#include "dots_basic_types.h"
#include "dots_allocation_helper.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    /**
     * Contains information about a parameter value stored in the Dob.
     */
    class ParameterDescription
    {
    public:
        //if type != object ==> objType must be null!
        ParameterDescription(const std::string & name,
                             const MemberType type,
                             const Size arrayLength,
                             //                             const TypeId objType,
                             const ParameterOffsetConst & offset,
                             AllocationHelper & allocHelper);

        ParameterDescription(const ParameterDescription& other);
        ParameterDescription& operator=(const ParameterDescription& other);


        ~ParameterDescription();


        const char * Name() const {return m_name.c_str();}

        template <class T>
        const boost::interprocess::offset_ptr<const T>
        ValuePtr(const ArrayIndex index = 0) const;
        
        template <class T>
        T Value(const ArrayIndex index = 0) const;

        Size ArrayLength() const {return m_arrayLength;}
        MemberType GetMemberType() const {return m_memberType;}

        typedef std::pair<const ParameterOffsetConst, Size> BinaryParameterValue;
        const BinaryParameterValue BinaryValue(ArrayIndex index = 0) const;

        template<class T>
        void ValueWithOptionalString(const ArrayIndex index,
                                     T & val,
                                     const char * & strVal) const;

    private:
        ShmString m_name;
        MemberType m_memberType;          //element type
        Size m_arrayLength;         //m_arrayLength=1 -> normal memer, m_arrayLength>1 -> array
        //        TypeId m_objType;           //valid if m_type=object

        ParameterOffsetConst m_offset;
    };

    //Collection type for Parameters
    typedef AllocationHelper::Containers<ParameterDescription>::vector ParameterVector;

    template <class T>
    T ParameterDescription::Value(const ArrayIndex index) const
    {
        switch (m_memberType)
        {
        case ObjectMemberType:
        case StringMemberType:
        case BinaryMemberType:
        case EntityIdMemberType:
        case InstanceIdMemberType:
        case ChannelIdMemberType:
        case HandlerIdMemberType:
            ENSURE(false, << "Someone tried to call the general version of ParameterDescription::Value on parameter of type " << m_memberType);
            throw std::logic_error("never to be thrown, look at ENSURE above"); //keep compiler happy

        default:
#ifdef NO_UNALIGNED_ACCESS
            T val;
            memcpy(&val,ParameterOffsetCast<const T>(m_offset + index * BasicTypes::SizeOfType(m_memberType)).get(), sizeof(T));
            return val;
#else
            return *ParameterOffsetCast<const T>(m_offset + index * BasicTypes::SizeOfType(m_memberType));
#endif
        }
    }

    template<class T>
    void ParameterDescription::ValueWithOptionalString(const ArrayIndex index,
                                                       T & val,
                                                       const char * & strVal) const
    {
        switch (m_memberType)
        {
        case EntityIdMemberType:
        case InstanceIdMemberType:
        case ChannelIdMemberType:
        case HandlerIdMemberType:
            {
#ifdef NO_UNALIGNED_ACCESS
                const char* const offsetPtrLocation = (const char*)(m_offset.get() + index * sizeof(ParameterOffset));
                ptrdiff_t offsetPtrValue = 0;
                memcpy(&offsetPtrValue, offsetPtrLocation, sizeof(ptrdiff_t));
                const ParameterOffsetConst dataLocation(offsetPtrLocation + offsetPtrValue);

                memcpy(&val, dataLocation.get(), sizeof(T));
#else
                const ParameterOffsetConst dataLocation =
                    *ParameterOffsetCast<const ParameterOffsetConst>(m_offset + index * sizeof(ParameterOffset));
                val = *ParameterOffsetCast<T>(dataLocation);
#endif
                strVal = dataLocation.get() + sizeof(T);
                if (strVal[0] == 0) //strlen == 0
                {
                    strVal = NULL;
                }
            }
            break;
        default:
            ENSURE(false, << "Someone tried to call ParameterDescription::ValueWithOptionalString on parameter of type " << m_memberType);
        }
    }

    template <>
    const boost::interprocess::offset_ptr<const char>
    ParameterDescription::ValuePtr<char>(const ArrayIndex index) const;

    template <>
    char ParameterDescription::Value<char>(const ArrayIndex index) const; //illegal!
}
}
}
}
#endif


