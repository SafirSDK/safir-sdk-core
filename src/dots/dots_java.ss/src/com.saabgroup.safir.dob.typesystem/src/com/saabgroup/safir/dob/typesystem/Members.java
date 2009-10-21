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
 * Functions for getting member information from types.
 *
 * With these operations you can get information on types regarding
 * their members. You can get member names and indexes. You can
 * get TypeIds of members etc.
 */
public class Members {

    /**
     * Get the number of members for a class or property.
     *
     * Parameters are not included.
     *
     * @param typeId TypeId of class or property.
     * @return Number of members in the class.
     * @throws IllegalValueException There is no such type defined.
     */
    public static int getNumberOfMembers(long typeId)
    {
        int result = Kernel.GetNumberOfMembers(typeId);
        if (result == -1)
        {
            throw new IllegalValueException("No such type");
        }
        else
        {
            return result;
        }
    }

    /**
     * Get the member index of a named member.
     *
     * @param typeId TypeId of class or property.
     * @param memberName  name of member as specified in xml description, case sensitive.
     * @return Member index of the member.
     * @throws IllegalValueException There is no such type defined or there is no such member
     *                               in the type.
     */
    public static int getIndex(long typeId, String memberName)
    {
        int result = Kernel.GetMemberId(typeId, memberName);
        if (result == -1)
        {
            throw new IllegalValueException("There is no such type or member defined");
        }
        else
        {
            return result;
        }
    }

    /**
     * Get the name of the specified member as it was defined in the xml description.
     *
     * @param typeId TypeId of class or property.
     * @param member Index of member.
     * @return Name of member.
     * @throws IllegalValueException There is no such type defined or there is no such member
     *                               in the type.
     */
    public static String getName(long typeId, int member)
    {
        String result = Kernel.GetMemberName(typeId, member);
        if (result == null)
        {
            throw new IllegalValueException("There is no such type or member defined");
        }
        else
        {
            return result;
        }
    }

    /**
     * Get type id of object or enumeration member.
     *
     * If a member is of type object or enumeration, this method can be used to get the
     * typeId for the class or enum that the member is of.
     *
     * @param typeId TypeId of class or property.
     * @param member Index of member.
     * @return The TypeId for the object or enumeration member.
     * @throws IllegalValueException There is no such type defined or there is no such member
     *                               in the type or the member is not an enum or object.
     */
    public static long getTypeId(long typeId, int member)
    {
        long result = Kernel.GetComplexMemberTypeId(typeId, member);
        if (result == -1)
        {
            throw new IllegalValueException("There is no such type or member defined");
        }
        else
        {
            return result;
        }
    }

    /**
     * Get the type of a member.
     *
     * getMemberInfo is used in other languages. In Java each selector is made
     * available. getMemberType is the only missing in other languages.
     *
     * @param typeId TypeId of class or property.
     * @param member Index of member.
     * @return The MemberType of the member.
     * @throws IllegalValueException There is no such type defined or there is no such member in the type.
     */
    public static MemberInfo getInfo(long typeId, int member)
    {
        int memberType[] = new int[1];
        String memberName[] = new String[1];
        long memberTypeId[] = new long [1];
        int stringLength[] = new int [1];
        boolean isArray[] = new boolean [1];
        int arrayLength[] = new int [1];
        try {
            Kernel.GetMemberInfo(typeId,
                                                      member,
                                                      memberType,
                                                      memberName,
                                                      memberTypeId,
                                                      stringLength,
                                                      isArray,
                                                      arrayLength);
        }
        catch (NullPointerException e)
        {
            throw new IllegalValueException("There is no such type or member defined");
        }

        return new MemberInfo(MemberType.values()[memberType[0]],
                              memberName[0],
                              memberTypeId[0],
                              stringLength[0],
                              isArray[0],
                              arrayLength[0]);
    }

    /**
     * Get the array size of a member.
     *
     * @param typeId TypeId of class.
     * @param member Index of member.
     * @return The array size of the member.
     * @throws IllegalValueException There is no such class defined or there is no such member
     *                               in the type.
     */
    public static int getArraySize(long typeId, int member)
    {
        int result = Kernel.GetMemberArraySize(typeId, member);
        if (result == -1)
        {
            throw new IllegalValueException("No such type or array defined");
        }
        else
        {
            return result;
        }
    }

    /**
     * Get the maximum string length of a member.
     *
     * @param typeId TypeId of class.
     * @param member Index of member.
     * @return The maximum length of the string member.
     * @throws IllegalValueException There is no such class defined or there is no such member
     *                               in the type or the member is not a string.
     */
    public static int getMaxStringLength(long typeId, int member)
    {
        int result = Kernel.GetStringMemberMaxLength(typeId, member);
        if (result == -1)
        {
            throw new IllegalValueException("No such type or member defined");
        }
        else
        {
            return result;
        }
    }

    /**
     * Get the name of the type as it was defined in the xml description.
     *
     * @param typeId TypeId of class.
     * @param member Index of member.
     * @return The name of the type.
     * @throws IllegalValueException There is no such class defined or there is no such member
     *                               in the type or the member is not a string.
     */
    public static String getTypeName(long typeId, int member)
    {
        String result = Kernel.GetMemberTypeName(typeId, member);
        if (result == null)
        {
            throw new IllegalValueException("There is no such type or member defined");
        }
        else
        {
            return result;
        }
    }
}
