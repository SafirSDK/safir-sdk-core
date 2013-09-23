/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safir.sourceforge.net)
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

#ifndef __DOTS_KERNEL_DEFS_H__
#define __DOTS_KERNEL_DEFS_H__

#include <boost/cstdint.hpp>

//--------------------------------------------------
// Base types
//--------------------------------------------------
typedef boost::int32_t DotsC_Int32;
typedef boost::int64_t DotsC_Int64;
typedef float          DotsC_Float32;
typedef double         DotsC_Float64;

typedef DotsC_Int64 DotsC_TypeId;

typedef struct
{
    DotsC_TypeId typeId;
    DotsC_Int64 instanceId;
} DotsC_EntityId;

typedef DotsC_Int32 DotsC_MemberIndex;

typedef DotsC_Int32 DotsC_ArrayIndex;

typedef DotsC_Int32 DotsC_ParameterIndex;

typedef DotsC_Int32 DotsC_EnumerationValue;

typedef enum
{
    BooleanMemberType=0,
    EnumerationMemberType,
    Int32MemberType,
    Int64MemberType,
    Float32MemberType,
    Float64MemberType,
    TypeIdMemberType,
    InstanceIdMemberType,
    EntityIdMemberType,
    ChannelIdMemberType,
    HandlerIdMemberType,
    StringMemberType,
    ObjectMemberType,
    BinaryMemberType,

    //  SI32 Types
    Ampere32MemberType,
    CubicMeter32MemberType,
    Hertz32MemberType,
    Joule32MemberType,
    Kelvin32MemberType,
    Kilogram32MemberType,
    Meter32MemberType,
    MeterPerSecond32MemberType,
    MeterPerSecondSquared32MemberType,
    Newton32MemberType,
    Pascal32MemberType,
    Radian32MemberType,
    RadianPerSecond32MemberType,
    RadianPerSecondSquared32MemberType,
    Second32MemberType,
    SquareMeter32MemberType,
    Steradian32MemberType,
    Volt32MemberType,
    Watt32MemberType,

    //  SI Long Types
    Ampere64MemberType,
    CubicMeter64MemberType,
    Hertz64MemberType,
    Joule64MemberType,
    Kelvin64MemberType,
    Kilogram64MemberType,
    Meter64MemberType,
    MeterPerSecond64MemberType,
    MeterPerSecondSquared64MemberType,
    Newton64MemberType,
    Pascal64MemberType,
    Radian64MemberType,
    RadianPerSecond64MemberType,
    RadianPerSecondSquared64MemberType,
    Second64MemberType,
    SquareMeter64MemberType,
    Steradian64MemberType,
    Volt64MemberType,
    Watt64MemberType
} DotsC_MemberType;


typedef enum
{
    NoError=0,
    ReadOnlyProperty,           //when reading properties straight from blobs
    UnableToDereferenceProperty, //when reading properties straight from blobs
    IllegalValue,
    //TODO: do all other functions that can return errors!
} DotsC_ErrorCode;


typedef enum
{
    MappedToNull,
    MappedToMember,
    MappedToParameter,
} DotsC_PropertyMappingKind;

class DotsC_MemberStatus
{
public:

    DotsC_MemberStatus()
        :m_status(DotsC_MemberStatus::NULL_FLAG_MASK) {}

    DotsC_MemberStatus(char status)
        :m_status(status) {}

    char RawValue() const {return m_status;}

    bool HasChanged() const {return (m_status & CHANGE_FLAG_MASK) != 0;}
    bool IsNull() const {return (m_status & NULL_FLAG_MASK) != 0;}
    bool HasDynamicPart() const {return (m_status & SEMIDYNAMIC_FLAG_MASK) != 0;}

    void SetChanged(bool changed) {Set(changed, DotsC_MemberStatus::CHANGE_FLAG_MASK);}
    void SetNull(bool isNull) {Set(isNull, DotsC_MemberStatus::NULL_FLAG_MASK);}
    void SetDynamicPart(bool hasDynamicPart) {Set(hasDynamicPart, DotsC_MemberStatus::SEMIDYNAMIC_FLAG_MASK);}

private:
    char m_status;
    static const char NULL_FLAG_MASK=0x1;
    static const char CHANGE_FLAG_MASK=0x2;
    static const char SEMIDYNAMIC_FLAG_MASK=0x4; //means that a HashedId member or EntityId has a string.
    void Set(bool val, char mask)
    {
        if (val)
        {
            m_status |= mask;
        }
        else
        {
            m_status &= (0xff ^ mask);
        }
    }
};

#endif

