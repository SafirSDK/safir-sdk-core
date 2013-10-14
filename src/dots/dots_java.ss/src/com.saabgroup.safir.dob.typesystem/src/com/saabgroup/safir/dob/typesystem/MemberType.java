// -*- coding: utf-8 -*-
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

package com.saabgroup.safir.dob.typesystem;

/**
 * An enumeration of all possible types of an object member.
 */
public enum MemberType {
    /**
     * Boolean member.
     */
    BOOLEAN_MEMBER_TYPE,

    /**
     * Enumeration member .
     */
    ENUMERATION_MEMBER_TYPE,

    /**
     * Int32 member.
     */
    INT32_MEMBER_TYPE,

    /**
     * Int64 member.
     */
    INT64_MEMBER_TYPE,

    /**
     * Float32 member.
     */
    FLOAT32_MEMBER_TYPE,

    /**
     * Float64 member.
     */
    FLOAT64_MEMBER_TYPE,

    /**
     * TypeId member.
     */
    TYPE_ID_MEMBER_TYPE,

    /**
     * InstanceId member.
     */
    INSTANCE_ID_MEMBER_TYPE,

    /**
     * EntityId member.
     */
    ENTITY_ID_MEMBER_TYPE,

    /**
     * ChannelId member.
     */
    CHANNEL_ID_MEMBER_TYPE,

    /**
     * HandlerId member.
     */
    HANDLER_ID_MEMBER_TYPE,

    /**
     * String member.
     */
    STRING_MEMBER_TYPE,

    /**
     * Object member.
     */
    OBJECT_MEMBER_TYPE,

    /**
     * Binary member.
     */
    BINARY_MEMBER_TYPE,


        //SI32 types

    /**
     * Ampere32 member.
     */
    AMPERE32_MEMBER_TYPE,

    /**
     * CubicMeter32 member.
     */
    CUBIC_METER32_MEMBER_TYPE,

    /**
     * Hertz32 member.
     */
    HERTZ32_MEMBER_TYPE,

    /**
     * Joule32 member.
     */
    JOULE32_MEMBER_TYPE,

    /**
     * Kelvin32 member.
     */
    KELVIN32_MEMBER_TYPE,

    /**
     * Kilogram32 member.
     */
    KILOGRAM32_MEMBER_TYPE,

    /**
     * Meter32 member.
     */
    METER32_MEMBER_TYPE,

    /**
     * MeterPerSecond32 member.
     */
    METER_PER_SECOND32_MEMBER_TYPE,

    /**
     * MeterPerSecondSquared32 member.
     */
    METER_PER_SECOND_SQUARED32_MEMBER_TYPE,

    /**
     * Newton32 member.
     */
    NEWTON32_MEMBER_TYPE,

    /**
     * Pascal32 member.
     */
    PASCAL32_MEMBER_TYPE,

    /**
     * Radian32 member.
     */
    RADIAN32_MEMBER_TYPE,

    /**
     * RadianPerSecond32 member.
     */
    RADIAN_PER_SECOND32_MEMBER_TYPE,

    /**
     * RadianPerSecondSquared32 member.
     */
    RADIAN_PER_SECOND_SQUARED32_MEMBER_TYPE,

    /**
     * Second32 member.
     */
    SECOND32_MEMBER_TYPE,

    /**
     * SquareMeter32 member.
     */
    SQUARE_METER32_MEMBER_TYPE,

    /**
     * Steradian32 member.
     */
    STERADIAN32_MEMBER_TYPE,

    /**
     * Volt32 member.
     */
    VOLT32_MEMBER_TYPE,

    /**
     * Watt32 member.
     */
    WATT32_MEMBER_TYPE,

        //SI64 types

    /**
     * Ampere64 member.
     */
    AMPERE64_MEMBER_TYPE,

    /**
     * CubicMeter64 member.
     */
    CUBIC_METER64_MEMBER_TYPE,

    /**
     * Hertz64 member.
     */
    HERTZ64_MEMBER_TYPE,

    /**
     * Joule64 member.
     */
    JOULE64_MEMBER_TYPE,

    /**
     * Kelvin64 member.
     */
    KELVIN64_MEMBER_TYPE,

    /**
     * Kilogram64 member.
     */
    KILOGRAM64_MEMBER_TYPE,

    /**
     * Meter64 member.
     */
    METER64_MEMBER_TYPE,

    /**
     * MeterPerSecond64 member.
     */
    METER_PER_SECOND64_MEMBER_TYPE,

    /**
     * MeterPerSecondSquared64 member.
     */
    METER_PER_SECOND_SQUARED64_MEMBER_TYPE,

    /**
     * Newton64 member.
     */
    NEWTON64_MEMBER_TYPE,

    /**
     * Pascal64 member.
     */
    PASCAL64_MEMBER_TYPE,

    /**
     * Radian64 member.
     */
    RADIAN64_MEMBER_TYPE,

    /**
     * RadianPerSecond64 member.
     */
    RADIAN_PER_SECOND64_MEMBER_TYPE,

    /**
     * RadianPerSecondSquared64 member.
     */
    RADIAN_PER_SECOND_SQUARED64_MEMBER_TYPE,

    /**
     * Second64 member.
     */
    SECOND64_MEMBER_TYPE,

    /**
     * SquareMeter64 member.
     */
    SQUARE_METER64_MEMBER_TYPE,

    /**
     * Steradian64 member.
     */
    STERADIAN64_MEMBER_TYPE,

    /**
     * Volt64 member.
     */
    VOLT64_MEMBER_TYPE,

    /**
     * Watt64 member.
     */
    WATT64_MEMBER_TYPE,
}
