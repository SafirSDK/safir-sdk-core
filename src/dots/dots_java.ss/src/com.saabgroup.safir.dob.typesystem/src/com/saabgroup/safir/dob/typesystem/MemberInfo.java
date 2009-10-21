// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
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
 * Class that encapsulates member info returned from Members.getInfo()
 */
public class MemberInfo
{
   /**
    * Get the type of the member.
    * @return The type of the member.
    */
    public MemberType getMemberType() {return m_memberType;};

    /**
     * Get the name of the member.
     * @return The name of the member.
     */
    public String getMemberName() {return m_memberName;}

    /**
     * if memberType is object or enumeration, this is the typeId of that type.
     * If memberType is something else the value is -1.
     *
     * @return TypeId of member.
     */
    public long getMemberTypeId() {return m_memberTypeId;}

    /**
     * If memberType is string and the type is a class
     * (not property) then this is the length of the string.
     * @return Max length of a string member.
     */
    public int getStringLength() {return m_stringLength;}

    /**
     * True if member is an array. Not applicable if type id is a property.
     *
     * @return Is the member an array.
     */
    public boolean getIsArray() {return m_isArray;};

    /**
     * Maximum capacity of array if the member is an array (1 if not an array).
     * Not applicable if type id is a property.
     * @return Length of array.
     */
    public int getArrayLength() {return m_arrayLength;};


    MemberInfo(MemberType memberType,
               String memberName,
               long memberTypeId,
               int stringLength,
               boolean isArray,
               int arrayLength)
    {
        m_memberType = memberType;
        m_memberName = memberName;
        m_memberTypeId = memberTypeId;
        m_stringLength = stringLength;
        m_isArray = isArray;
        m_arrayLength = arrayLength;
    }

    private MemberType m_memberType;
    private String m_memberName;
    private long m_memberTypeId;
    private int m_stringLength;
    private boolean m_isArray;
    private int m_arrayLength;
}

