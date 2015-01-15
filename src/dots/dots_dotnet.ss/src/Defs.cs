/******************************************************************************
*
* Copyright Saab AB, 2005-2013 (http://safir.sourceforge.net)
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

using System;

namespace Safir.Dob.Typesystem
{
    /// <summary>
    /// Member types.
    /// </summary>
    public enum MemberType
    {
        /// <summary>Boolean member.</summary>
        BooleanMemberType = 0,

        /// <summary>Enumeration member.</summary>
        EnumerationMemberType,

        /// <summary>Int32 member.</summary>
        Int32MemberType,

        /// <summary>Int64 member.</summary>
        Int64MemberType,

        /// <summary>Float32 member.</summary>
        Float32MemberType,

        /// <summary>Float64 member.</summary>
        Float64MemberType,

        /// <summary>TypeId member.</summary>
        TypeIdMemberType,

        /// <summary>InstanceId member.</summary>
        InstanceIdMemberType,

        /// <summary>EntityId member.</summary>
        EntityIdMemberType,

        /// <summary>ChannelId member.</summary>
        ChannelIdMemberType,

        /// <summary>HandlerId member.</summary>
        HandlerIdMemberType,

        /// <summary>String member.</summary>
        StringMemberType,

        /// <summary>Object member.</summary>
        ObjectMemberType,

        /// <summary>Binary member.</summary>
        BinaryMemberType,

        /// <summary>Ampere32 member.</summary>
        Ampere32MemberType,

        /// <summary>CubicMeter32 member.</summary>
        CubicMeter32MemberType,

        /// <summary>Hertz32 member.</summary>
        Hertz32MemberType,

        /// <summary>Joule32 member.</summary>
        Joule32MemberType,

        /// <summary>Kelvin32 member.</summary>
        Kelvin32MemberType,

        /// <summary>Kilogram32 member.</summary>
        Kilogram32MemberType,

        /// <summary>Meter32 member.</summary>
        Meter32MemberType,

        /// <summary>MeterPerSecond32 member.</summary>
        MeterPerSecond32MemberType,

        /// <summary>MeterPerSecondSquared32 member.</summary>
        MeterPerSecondSquared32MemberType,

        /// <summary>Newton32 member.</summary>
        Newton32MemberType,

        /// <summary>Pascal32 member.</summary>
        Pascal32MemberType,

        /// <summary>Radian32 member.</summary>
        Radian32MemberType,

        /// <summary>RadianPerSecond32 member.</summary>
        RadianPerSecond32MemberType,

        /// <summary>RadianPerSecondSquared32 member.</summary>
        RadianPerSecondSquared32MemberType,

        /// <summary>Second32 member.</summary>
        Second32MemberType,

        /// <summary>SquareMeter32 member.</summary>
        SquareMeter32MemberType,

        /// <summary>Steradian32 member.</summary>
        Steradian32MemberType,

        /// <summary>Volt32 member.</summary>
        Volt32MemberType,

        /// <summary>Watt32 member.</summary>
        Watt32MemberType,

        /// <summary>Ampere64 member.</summary>
        Ampere64MemberType,

        /// <summary>CubicMeter64 member.</summary>
        CubicMeter64MemberType,

        /// <summary>Hertz64 member.</summary>
        Hertz64MemberType,

        /// <summary>Joule64 member.</summary>
        Joule64MemberType,

        /// <summary>Kelvin64 member.</summary>
        Kelvin64MemberType,

        /// <summary>Kilogram64 member.</summary>
        Kilogram64MemberType,

        /// <summary>Meter64 member.</summary>
        Meter64MemberType,

        /// <summary>MeterPerSecond64 member.</summary>
        MeterPerSecond64MemberType,

        /// <summary>MeterPerSecondSquared64 member.</summary>
        MeterPerSecondSquared64MemberType,

        /// <summary>Newton64 member.</summary>
        Newton64MemberType,

        /// <summary>Pascal64 member.</summary>
        Pascal64MemberType,

        /// <summary>Radian64 member.</summary>
        Radian64MemberType,

        /// <summary>RadianPerSecond64 member.</summary>
        RadianPerSecond64MemberType,

        /// <summary>RadianPerSecondSquared64 member.</summary>
        RadianPerSecondSquared64MemberType,

        /// <summary>Second64 member.</summary>
        Second64MemberType,

        /// <summary>SquareMeter64 member.</summary>
        SquareMeter64MemberType,

        /// <summary>Steradian64 member.</summary>
        Steradian64MemberType,

        /// <summary>Volt64 member.</summary>
        Volt64MemberType,

        /// <summary>Watt64 member.</summary>
        Watt64MemberType
    }

	public enum CollectionType
	{
		SingleValueCollectionType = 0,
		ArrayCollectionType,
		SequenceCollectionType,
		DictionaryCollectionType
	} 
}
