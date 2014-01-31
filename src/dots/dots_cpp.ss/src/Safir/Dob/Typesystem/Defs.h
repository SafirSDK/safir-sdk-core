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

#ifndef __SAFIR_DOB_TYPESYSTEM_DEFS_H__
#define __SAFIR_DOB_TYPESYSTEM_DEFS_H__

#if defined _MSC_VER
    #if defined DOTS_EXPORTS
        #define DOTS_API __declspec(dllexport)
    #else
        #define DOTS_API __declspec(dllimport)
        #define SAFIR_LIBRARY_NAME "dots_cpp"
        #include <Safir/Utilities/Internal/AutoLink.h>
    #endif
#elif defined __GNUC__
    #define DOTS_API
    #define __cdecl
#endif

#include <Safir/Utilities/Internal/UnorderedMap.h>
#include <Safir/Dob/Typesystem/ToolSupport/KernelDefs.h>
#include <string>
#include <vector>
/**
 * This namespace contains all the functionality and definitions of the SAFIR SDK.
 */
namespace Safir
{
/**
 * This namespace contains all functionality of the DOB (Components DOSE and DOTS).
 */
namespace Dob
{
/**
 * This namespace contains the DOB Typesystem functionality and definitions.
 */
namespace Typesystem
{
    /**
     * @name Base Types
     * These are the types that all numeric types in the DOB are based on.
     * They are guaranteed to be of the correct size on all supported platforms.
     */
    /** @{ */

    /** 32 bit integer type.
     * This type is guaranteed to be 32 bits on all platforms.*/
    typedef DotsC_Int32   Int32;
    /** 64 bit integer type.
     * This type is guaranteed to be 64 bits on all platforms.*/
    typedef DotsC_Int64   Int64;
    /** 32 bit floating point type.
     * This type is guaranteed to be 32 bits on all platforms.*/
    typedef DotsC_Float32 Float32;
    /** 64 bit floating point type.
     * This type is guaranteed to be 64 bits on all platforms.*/
    typedef DotsC_Float64 Float64;
    /** @} */

    /**
     * 32 bit SI-types.
     *
     * This namespace contains definitions of the 32 bit SI types.
     * It also contains definitions for Containers and ArrayContainers
     * of these types.
     */
    namespace Si32
    {
        /** 32 bit representation of Ampere. */
        typedef Float32 Ampere;

        /** 32 bit representation of CubicMeter. */
        typedef Float32 CubicMeter;

        /** 32 bit representation of Hertz. */
        typedef Float32 Hertz;

        /** 32 bit representation of Joule. */
        typedef Float32 Joule;

        /** 32 bit representation of Kelvin. */
        typedef Float32 Kelvin;

        /** 32 bit representation of Kilogram. */
        typedef Float32 Kilogram;

        /** 32 bit representation of Meter. */
        typedef Float32 Meter;

        /** 32 bit representation of MeterPerSecond. */
        typedef Float32 MeterPerSecond;

        /** 32 bit representation of MeterPerSecondSquared. */
        typedef Float32 MeterPerSecondSquared;

        /** 32 bit representation of Newton. */
        typedef Float32 Newton;

        /** 32 bit representation of Pascal. */
        typedef Float32 Pascal;

        /** 32 bit representation of Radian. */
        typedef Float32 Radian;

        /** 32 bit representation of RadianPerSecond. */
        typedef Float32 RadianPerSecond;

        /** 32 bit representation of RadianPerSecondSquared. */
        typedef Float32 RadianPerSecondSquared;

        /** 32 bit representation of Second. */
        typedef Float32 Second;

        /** 32 bit representation of SquareMeter. */
        typedef Float32 SquareMeter;

        /** 32 bit representation of Steradian. */
        typedef Float32 Steradian;

        /** 32 bit representation of Volt. */
        typedef Float32 Volt;

        /** 32 bit representation of Watt. */
        typedef Float32 Watt;
    }

    /**
     * 64 bit SI-types.
     *
     * This namespace contains definitions of the 64 bit SI types.
     * It also contains definitions for Containers and ArrayContainers
     * of these types.
     */
    namespace Si64
    {
        /** 64 bit representation of Ampere. */
        typedef Float64 Ampere;

        /** 64 bit representation of CubicMeter. */
        typedef Float64 CubicMeter;

        /** 64 bit representation of Hertz. */
        typedef Float64 Hertz;

        /** 64 bit representation of Joule. */
        typedef Float64 Joule;

        /** 64 bit representation of Kelvin. */
        typedef Float64 Kelvin;

        /** 64 bit representation of Kilogram. */
        typedef Float64 Kilogram;

        /** 64 bit representation of Meter. */
        typedef Float64 Meter;

        /** 64 bit representation of MeterPerSecond. */
        typedef Float64 MeterPerSecond;

        /** 64 bit representation of MeterPerSecondSquared. */
        typedef Float64 MeterPerSecondSquared;

        /** 64 bit representation of Newton. */
        typedef Float64 Newton;

        /** 64 bit representation of Pascal. */
        typedef Float64 Pascal;

        /** 64 bit representation of Radian. */
        typedef Float64 Radian;

        /** 64 bit representation of RadianPerSecond. */
        typedef Float64 RadianPerSecond;

        /** 64 bit representation of RadianPerSecondSquared. */
        typedef Float64 RadianPerSecondSquared;

        /** 64 bit representation of Second. */
        typedef Float64 Second;

        /** 64 bit representation of SquareMeter. */
        typedef Float64 SquareMeter;

        /** 64 bit representation of Steradian. */
        typedef Float64 Steradian;

        /** 64 bit representation of Volt. */
        typedef Float64 Volt;

        /** 64 bit representation of Watt. */
        typedef Float64 Watt;
    }

    /**
     * A unique type identifier.
     *
     * A 64 bit integer that uniquely identifies a Dob type.
     * This is in fact 64 bits of an MD5 of the type name and its namespace.
     */
    typedef DotsC_TypeId TypeId;

    /** A vector of TypeIds. */
    typedef std::vector<TypeId> TypeIdVector;

    /**
     * @name Type indices.
     * Types for indices into types.
     */
    /** @{ */

    /**
     * The index of a member in an object.
     *
     * Members in objects are ordered according to the order they appear in the dou-file.
     * However, do not rely on this, but use the Member-operations to get hold of indices if you need them.
     * Note: Even though this is a signed 32 bit integer only the positive values are valid.
     * @see Dob::Typesystem::Members
     */
    typedef DotsC_MemberIndex MemberIndex;

    /**
     * Index into an array.
     *
     * DOB-object arrays are indexed using this type.
     * Note: Even though this is a signed 32 bit integer only the positive values are valid.
     * @see Dob::Typesystem::Members
     */
    typedef DotsC_ArrayIndex ArrayIndex;

    /**
     * The index of a parameter in an object.
     *
     * Parameters in objects are ordered according to the order they appear in the dou-file.
     * However, do not rely on this, but use the Parameter-operations to get hold of indices if you need them.
     * Note: Even though this is a signed 32 bit integer only the positive values are valid.
     * @see Dob::Typesystem::Parameters
     */
    typedef DotsC_ParameterIndex ParameterIndex;

    /** @} */

    /**
     * The ordinal value of an enumeration.
     *
     * When you use the set or get the ordinal on an EnumerationContainer you will get values of this kind.
     * Normally you will not need to use this, instead use the real enumeration values through
     * GetVal() and SetVal().
     * Note: Even though this is a signed 32 bit integer only the positive values are valid.
     * @see Dob::Typesystem::EnumerationContainerBase::GetOrdinal
     * @see Dob::Typesystem::EnumerationContainerBase::SetOrdinal
     */
    typedef DotsC_EnumerationValue EnumerationValue;

    /**
     * An enumeration of all possible types of an object member.
     *
     * The values of this enumeration is defined in the file Safir/Dob/Typesystem/ToolSupport/KernelDefs.h.
     * @see Safir/Dob/Typesystem/ToolSupport/KernelDefs.h
     */
    typedef DotsC_MemberType MemberType;

    /**
     * A type to contain binary serializations of DOB objects.
     *
     * Note: If you need to get hold of a "raw" C-pointer to the data * use &binary[0].
     * See Effective STL Item 16 for more info.
     */
    typedef std::vector<char> BinarySerialization;

     /**
     * A type to contain binary data.
     *
     * TODO: maybe Binary and BinarySerialization should be replaced by only one type.
     */
    typedef std::vector<char> Binary;
}
}
}
#endif
