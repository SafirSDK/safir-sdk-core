// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2005-2013 (http://safirsdkcore.com)
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
    * Get the typeId of the member.
    * @return The typeId of the member.
    */
    public long getTypeId() {return m_typeId;};

    /**
    * Get the member index of the member.
    * @return The member index of the member.
    */
    public int getMemberIndex() {return m_memberIndex;};

    /**
    * Get the type of the member.
    * @return The type of the member.
    */
    public MemberType getMemberType() {return m_memberType;};

    /**
    * Get the type of the key if the member is a dictionary.
    * @return The type of the key.
    */
    public MemberType getKeyType() {return m_keyType;};

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
     * if member is a dictionary and keyType is enumeration, this is the typeId of that type.
     *
     * @return TypeId of key.
     */
    public long getKeyTypeId() {return m_keyTypeId;}

    /**
     * If memberType is string and the type is a class
     * (not property) then this is the length of the string.
     * @return Max length of a string member.
     */
    public int getStringLength() {return m_stringLength;}

    /**
     * Get the collection type of the member.
     *
     * @return Collection type.
     */
    public CollectionType getCollectionType() {return m_collectionType;};

    /**
     * Maximum capacity of array if the member is an array (1 if not an array).
     * Not applicable if type id is a property.
     * @return Length of array.
     */
    public int getArrayLength() {return m_arrayLength;};


    MemberInfo(long typeId,
               int memberIndex,
               MemberType memberType,
               MemberType keyType,
               String memberName,
               long memberTypeId,
               long keyTypeId,
               int stringLength,
               CollectionType collectionType,
               int arrayLength)
    {
        m_typeId = typeId;
        m_memberIndex = memberIndex;
        m_memberType = memberType;
        m_keyType = keyType;
        m_memberName = memberName;
        m_memberTypeId = memberTypeId;
        m_keyTypeId = keyTypeId;
        m_stringLength = stringLength;
        m_collectionType = collectionType;
        m_arrayLength = arrayLength;
    }

    private long m_typeId;
    private int m_memberIndex;
    private MemberType m_memberType;
    private MemberType m_keyType;
    private String m_memberName;
    private long m_memberTypeId;
    private long m_keyTypeId;
    private int m_stringLength;
    private CollectionType m_collectionType;
    private int m_arrayLength;
}

