/******************************************************************************
*
* Copyright Saab AB, 2004-2015 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson / joot
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
* GNU General Public License for more Internals.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/
#ifndef __DOTS_LANGUAGE_INTERFACE_DEFS_H__
#define __DOTS_LANGUAGE_INTERFACE_DEFS_H__

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
    SingleValueCollectionType,
    ArrayCollectionType,
    SequenceCollectionType,
    DictionaryCollectionType
} DotsC_CollectionType;

typedef enum
{
    MappedToNull,
    MappedToMember,
    MappedToParameter
} DotsC_PropertyMappingKind;

#endif
