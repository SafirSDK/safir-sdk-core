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

import com.saabgroup.safir.dob.typesystem.Object;
/**
 * Functions for getting property information from types and objects.
 *
 * With these operations you can get and set members on objects using properties.
 * If you need member information (names, typeids etc) from a property, use the
 * functions in Members. (For array sizes, use the method in this class though...).
 */
public class Properties {

    /**
     * Get the information of how a property member is mapped for a class.
     *
     * @param classId [in] -  type id of a class that supports the specified property.
     * @param propertyId [in] - type id of the property
     * @param propertyMember [in] - index of the property member.
     * @return The property mapping kind.
     * @throws IllegalValueException There is no such type or member defined.
     */
    public static PropertyMappingKind getMappingKind(long classId,
                                                     long propertyId,
                                                     int propertyMember)
    {
        int result = Kernel.GetPropertyMappingKind(classId, propertyId, propertyMember);
        if (result<0) {
            throw new IllegalValueException("That object is not mapped to that property!");
        }

        return PropertyMappingKind.values()[result];
    }

    /**
     * Get the array size of a property member.
     *
     * @param classId [in] -  type id of a class that supports the specified property.
     * @param propertyId [in] - type id of the property
     * @param propertyMember [in] - index of the property member.
     * @return The array size of the property member.
     * @throws IllegalValueException There is no such type or member defined.
     */
    public static int getArraySize(long classId,
                                   long propertyId,
                                   int propertyMember)
    {
        int result = Kernel.GetMemberArraySizeProperty(classId, propertyId, propertyMember);
        if (result == -1)
        {
            throw new IllegalValueException("No such type or array or mapping defined");
        }
        else
        {
            return result;
        }
    }

    /**
     * Set a property member to null.
     *
     * @param obj [in,out] - The object to modify.
     * @param propertyId [in] - TypeId of the property to use.
     * @param member [in] - Index of the property member to modify.
     * @param index [in] - Array index.
     * @throws ReadOnlyException If the property member is read-only.
     */
    public static void setNull(Object obj,
                               long propertyId,
                               int member,
                               int index)
    {
        switch(getMappingKind(obj.getTypeId(),propertyId,member))
        {
        case MAPPED_TO_NULL:
            return;

        case MAPPED_TO_PARAMETER:
            throw new ReadOnlyException("Property member is mapped to parameter");

        case MAPPED_TO_MEMBER:
            {
                int[] classMemberRef = getClassMemberReference(obj.getTypeId(),
                                                               propertyId,
                                                               member);
                if (classMemberRef == null || classMemberRef.length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container [] = new ContainerBase[1];
                boolean parentIsChanged [] = new boolean [1];
                parentIsChanged[0] = false;

                dereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container[0] == null)
                {
                    return; //parent is null
                }
                else
                {
                    container[0].setNull();
                }
            }
            break;
        }
    }

    /**
     * Is the property member null.
     *
     * @param obj [in] - The object to check inside.
     * @param propertyId [in] - TypeId of the property to use.
     * @param member [in] - Index of the property member.
     * @param index [in] - Array index.
     * @return True if the property member (or a parent item of it) was null.
     */
    public static boolean isNull(Object obj,
                                 long propertyId,
                                 int member,
                                 int index)
    {
        switch (getMappingKind(obj.getTypeId(), propertyId, member))
        {
        case MAPPED_TO_NULL:
            return true;

        case MAPPED_TO_PARAMETER:
            return false;

        case MAPPED_TO_MEMBER:
            {
                int[] classMemberRef = getClassMemberReference(obj.getTypeId(),
                                                               propertyId,
                                                               member);
                if (classMemberRef == null || classMemberRef.length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container [] = new ContainerBase[1];
                boolean parentIsChanged [] = new boolean [1];
                parentIsChanged[0] = false;

                dereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container[0] == null || container[0].isNull())
                {
                    return true; //parent or container is null
                }
                else
                {
                    return false;
                }
            }
        }
        throw new SoftwareViolationException("Coding error in Properties.IsNull");
    }

    /**
     * Is the property member changed.
     *
     * @param obj [in] - The object to check inside.
     * @param propertyId [in] - TypeId of the property to use.
     * @param member [in] - Index of the property member.
     * @param index [in] - Array index.
     * @return True if the property member (or a parent item of it) was changed.
     */
    public static boolean isChanged(Object obj,
                                    long propertyId,
                                    int member,
                                    int index)
    {
        switch (getMappingKind(obj.getTypeId(), propertyId, member))
        {
        case MAPPED_TO_NULL:
            return false;

        case MAPPED_TO_PARAMETER:
            return false;

        case MAPPED_TO_MEMBER:
            {
                int[] classMemberRef = getClassMemberReference(obj.getTypeId(),
                                                               propertyId,
                                                               member);

                if (classMemberRef == null || classMemberRef.length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container [] = new ContainerBase[1];
                boolean parentIsChanged [] = new boolean [1];
                parentIsChanged[0] = false;

                dereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                container,
                                                parentIsChanged);
                if (parentIsChanged[0])
                {
                    return true;
                }
                if (container[0] != null && container[0].isChanged())
                {
                    return true;
                }
                else
                {
                    return false;
                }
            }
        }
        throw new SoftwareViolationException("Coding error in Properties.IsChanged");
    }

    /**
     * Is the property member read-only.
     *
     * A property member is read-only if it
     *
     *   1. is mapped to null,
     *
     *   2. is mapped to a parameter, or
     *
     *   3. the item containing the member in the object is null.
     *
     * @param obj [in] - The object to check inside.
     * @param propertyId [in] - TypeId of the property to use.
     * @param member [in] - Index of the property member.
     * @param index [in] - Array index.
     * @return True if the property member is read only.
     */
    public static boolean isReadOnly(Object obj,
                                     long propertyId,
                                     int member,
                                     int index)
    {
        switch (getMappingKind(obj.getTypeId(), propertyId, member))
        {
        case MAPPED_TO_NULL:
            return true;

        case MAPPED_TO_PARAMETER:
            return true;

        case MAPPED_TO_MEMBER:
            {
                int[] classMemberRef = getClassMemberReference(obj.getTypeId(),
                                                               propertyId,
                                                               member);

                if (classMemberRef == null || classMemberRef.length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container [] = new ContainerBase[1];
                boolean parentIsChanged [] = new boolean [1];
                parentIsChanged[0] = false;

                dereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                container,
                                                parentIsChanged);

                if (container[0] == null)
                {
                    return true;
                }
                else
                {
                    return false;
                }
            }
        }
        throw new SoftwareViolationException("Coding error in Properties.IsChanged");
    }

    /**
     * Set a boolean property member in the object using a property.
     *
     * @param obj The object to modify.
     * @param propertyId TypeId of the property to use.
     * @param val The value to set the member to.
     * @param member Index of the property member to modify.
     * @param index Array index.
     * @throws ReadOnlyException If the property member is read-only.
     */
    public static void set(Object obj,
                           long propertyId,
                           boolean val,
                           int member,
                           int index)
    {
        switch(getMappingKind(obj.getTypeId(),propertyId,member))
        {
        case MAPPED_TO_NULL:
            throw new ReadOnlyException("Property member is mapped to null");

        case MAPPED_TO_PARAMETER:
            throw new ReadOnlyException("Property member is mapped to parameter");

        case MAPPED_TO_MEMBER:
            {
                int[] classMemberRef = getClassMemberReference(obj.getTypeId(),
                                                               propertyId,
                                                               member);
                if (classMemberRef == null || classMemberRef.length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container [] = new ContainerBase[1];
                boolean parentIsChanged [] = new boolean[1];
                parentIsChanged[0] = false;
                dereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container[0] == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    ((BooleanContainer)container[0]).setVal(val);
                }
            }
            break;
        }
    }

    /**
     * Get a boolean property member from the object using a property.
     *
     * @param obj The object to read from.
     * @param propertyId TypeId of the property to use.
     * @param member Index of the property member to read from.
     * @param index Array index.
     *
     * @return The value of the member.
     *
     * @throws ReadOnlyException The member is inaccessible. Some "parent" item is null.
     * @throws NullException The member is null.
     */
    public static boolean getBoolean(Object obj,
                                     long propertyId,
                                     int member,
                                     int index)
    {
        switch(getMappingKind(obj.getTypeId(),propertyId,member))
        {
        case MAPPED_TO_NULL:
            throw new NullException("Property member is mapped to null");

        case MAPPED_TO_PARAMETER:
            {
                long[] paramClassTypeId=new long[1];
                int[] paramIndex=new int[1];
                int[] valueIndex=new int[1];
                Kernel.GetPropertyParameterReference(obj.getTypeId(), propertyId, member, index, paramClassTypeId, paramIndex, valueIndex);

                boolean val [] = new boolean [1];
                Kernel.GetBooleanParameter(paramClassTypeId[0], paramIndex[0], valueIndex[0], val);

                return val[0];
            }

        case MAPPED_TO_MEMBER:
            int[] classMemberRef = getClassMemberReference(obj.getTypeId(),
                                                           propertyId,
                                                           member);
            if (classMemberRef == null || classMemberRef.length == 0)
            {
                throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
            }

            ContainerBase container [] = new ContainerBase[1];
            boolean parentIsChanged [] = new boolean [1];
            parentIsChanged[0] = false;
            dereferenceClassMemberReference(obj,
                                            classMemberRef,
                                            0,
                                            index,
                                            container,
                                            parentIsChanged);
            if (container[0] == null)
            {
                throw new ReadOnlyException("Unable to dereference property, some parent is null");
            }
            else
            {
                return ((BooleanContainer)container[0]).getVal();
            }
        }
        throw new SoftwareViolationException("Reached end of getBoolean! Internal Error");
    }

    /**
     * Set an Enumeration property member in the object using a property.
     *
     * @param obj The object to modify.
     * @param propertyId TypeId of the property to use.
     * @param val The value to set the member to.
     * @param member Index of the property member to modify.
     * @param index Array index.
     * @throws ReadOnlyException If the property member is read-only.
     */
    public static void setEnum(Object obj,
                               long propertyId,
                               int val,
                               int member,
                               int index)
    {
        switch(getMappingKind(obj.getTypeId(),propertyId,member))
        {
        case MAPPED_TO_NULL:
            throw new ReadOnlyException("Property member is mapped to null");

        case MAPPED_TO_PARAMETER:
            throw new ReadOnlyException("Property member is mapped to parameter");

        case MAPPED_TO_MEMBER:
            {
                int[] classMemberRef = getClassMemberReference(obj.getTypeId(),
                                                               propertyId,
                                                               member);
                if (classMemberRef == null || classMemberRef.length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container [] = new ContainerBase[1];
                boolean parentIsChanged [] = new boolean [1];
                parentIsChanged[0] = false;
                dereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container[0] == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    ((EnumerationContainerBase<?>)container[0]).setOrdinal(val);
                }
            }
            break;
        }
    }

    /**
     * Get an Enumeration property member from the object using a property.
     *
     * @param obj The object to read from.
     * @param propertyId TypeId of the property to use.
     * @param member Index of the property member to read from.
     * @param index Array index.
     *
     * @return The value of the member.
     *
     * @throws ReadOnlyException The member is inaccessible. Some "parent" item is null.
     * @throws NullException The member is null.
     */
    public static int getEnum(Object obj,
                              long propertyId,
                              int member,
                              int index)
    {
        switch(getMappingKind(obj.getTypeId(),propertyId,member))
        {
        case MAPPED_TO_NULL:
            throw new NullException("Property member is mapped to null");

        case MAPPED_TO_PARAMETER:
            {
                long[] paramClassTypeId=new long[1];
                int[] paramIndex=new int[1];
                int[] valueIndex=new int[1];
                Kernel.GetPropertyParameterReference(obj.getTypeId(), propertyId, member, index, paramClassTypeId, paramIndex, valueIndex);

                int val [] = new int [1];
                Kernel.GetInt32Parameter(paramClassTypeId[0], paramIndex[0], valueIndex[0], BlobOperations.VALUE_MODE, val);

                return val[0];
            }

        case MAPPED_TO_MEMBER:
            {
                int[] classMemberRef = getClassMemberReference(obj.getTypeId(),
                                                               propertyId,
                                                               member);
                if (classMemberRef == null || classMemberRef.length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container [] = new ContainerBase[1];
                boolean parentIsChanged [] = new boolean [1];
                parentIsChanged[0] = false;
                dereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container[0] == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    return ((EnumerationContainerBase<?>)container[0]).getOrdinal();
                }
            }
        }
        throw new SoftwareViolationException("Reached end of getEnum! Internal Error");
    }


    /**
     * Set an Int32 property member in the object using a property.
     *
     * @param obj The object to modify.
     * @param propertyId TypeId of the property to use.
     * @param val The value to set the member to.
     * @param member Index of the property member to modify.
     * @param index Array index.
     * @throws ReadOnlyException If the property member is read-only.
     */
    public static void set(Object obj,
                           long propertyId,
                           int val,
                           int member,
                           int index)
    {
        switch(getMappingKind(obj.getTypeId(),propertyId,member))
        {
        case MAPPED_TO_NULL:
            throw new ReadOnlyException("Property member is mapped to null");

        case MAPPED_TO_PARAMETER:
            throw new ReadOnlyException("Property member is mapped to parameter");

        case MAPPED_TO_MEMBER:
            {
                int[] classMemberRef = getClassMemberReference(obj.getTypeId(),
                                                               propertyId,
                                                               member);
                if (classMemberRef == null || classMemberRef.length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container [] = new ContainerBase[1];
                boolean parentIsChanged [] = new boolean [1];
                parentIsChanged[0] = false;
                dereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container[0] == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    ((Int32Container)container[0]).setVal(val);
                }
            }
            break;
        }
    }


    /**
     * Get an Int32 property member from the object using a property.
     *
     * @param obj The object to read from.
     * @param propertyId TypeId of the property to use.
     * @param member Index of the property member to read from.
     * @param index Array index.
     *
     * @return The value of the member.
     *
     * @throws ReadOnlyException The member is inaccessible. Some "parent" item is null.
     * @throws NullException The member is null.
     */
    public static int getInt32(Object obj,
                               long propertyId,
                               int member,
                               int index)
    {
        switch(getMappingKind(obj.getTypeId(),propertyId,member))
        {
        case MAPPED_TO_NULL:
            throw new NullException("Property member is mapped to null");

        case MAPPED_TO_PARAMETER:
            {
                long[] paramClassTypeId=new long[1];
                int[] paramIndex=new int[1];
                int[] valueIndex=new int[1];
                Kernel.GetPropertyParameterReference(obj.getTypeId(), propertyId, member, index, paramClassTypeId, paramIndex, valueIndex);

                int val [] = new int [1];
                Kernel.GetInt32Parameter(paramClassTypeId[0], paramIndex[0], valueIndex[0], BlobOperations.VALUE_MODE, val);

                return val[0];
            }

        case MAPPED_TO_MEMBER:
            {
                int[] classMemberRef = getClassMemberReference(obj.getTypeId(),
                                                               propertyId,
                                                               member);
                if (classMemberRef == null || classMemberRef.length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container [] = new ContainerBase[1];
                boolean parentIsChanged [] = new boolean [1];
                parentIsChanged[0] = false;
                dereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container[0] == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    return ((Int32Container)container[0]).getVal();
                }
            }
        }
        throw new SoftwareViolationException("Reached end of getInt32! Internal Error");
    }


    /**
     * Set an Int64 or TypeId property member in the object using a property.
     *
     * @param obj The object to modify.
     * @param propertyId TypeId of the property to use.
     * @param val The value to set the member to.
     * @param member Index of the property member to modify.
     * @param index Array index.
     * @throws ReadOnlyException If the property member is read-only.
     */
    public static void set(Object obj,
                           long propertyId,
                           long val,
                           int member,
                           int index)
    {
        switch(getMappingKind(obj.getTypeId(),propertyId,member))
        {
        case MAPPED_TO_NULL:
            throw new ReadOnlyException("Property member is mapped to null");

        case MAPPED_TO_PARAMETER:
            throw new ReadOnlyException("Property member is mapped to parameter");

        case MAPPED_TO_MEMBER:
            {
                int[] classMemberRef = getClassMemberReference(obj.getTypeId(),
                                                               propertyId,
                                                               member);
                if (classMemberRef == null || classMemberRef.length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container [] = new ContainerBase[1];
                boolean parentIsChanged [] = new boolean [1];
                parentIsChanged[0] = false;
                dereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container[0] == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    ((Int64Container)container[0]).setVal(val);
                }
            }
            break;
        }
    }

    /**
     * Get an Int64 property member from the object using a property.
     *
     * @param obj The object to read from.
     * @param propertyId TypeId of the property to use.
     * @param member Index of the property member to read from.
     * @param index Array index.
     *
     * @return The value of the member.
     *
     * @throws ReadOnlyException The member is inaccessible. Some "parent" item is null.
     * @throws NullException The member is null.
     */
    public static long getInt64(Object obj,
                                long propertyId,
                                int member,
                                int index)
    {
        switch(getMappingKind(obj.getTypeId(),propertyId,member))
        {
        case MAPPED_TO_NULL:
            throw new NullException("Property member is mapped to null");

        case MAPPED_TO_PARAMETER:
            {
                long[] paramClassTypeId=new long[1];
                int[] paramIndex=new int[1];
                int[] valueIndex=new int[1];
                Kernel.GetPropertyParameterReference(obj.getTypeId(), propertyId, member, index, paramClassTypeId, paramIndex, valueIndex);

                long val [] = new long [1];
                Kernel.GetInt64Parameter(paramClassTypeId[0], paramIndex[0], valueIndex[0], BlobOperations.VALUE_MODE, val);
                return val[0];
            }

        case MAPPED_TO_MEMBER:
            {
                int[] classMemberRef = getClassMemberReference(obj.getTypeId(),
                                                               propertyId,
                                                               member);
                if (classMemberRef == null || classMemberRef.length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container [] = new ContainerBase[1];
                boolean parentIsChanged [] = new boolean [1];
                parentIsChanged[0] = false;
                dereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container[0] == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    return ((Int64Container)container[0]).getVal();
                }
            }
        }
        throw new SoftwareViolationException("Reached end of getInt64! Internal Error");
    }

    /**
     * Set a Float32 property member in the object using a property.
     *
     * @param obj The object to modify.
     * @param propertyId TypeId of the property to use.
     * @param val The value to set the member to.
     * @param member Index of the property member to modify.
     * @param index Array index.
     * @throws ReadOnlyException If the property member is read-only.
     */
    public static void set(Object obj,
                           long propertyId,
                           float val,
                           int member,
                           int index)
    {
        switch(getMappingKind(obj.getTypeId(),propertyId,member))
        {
        case MAPPED_TO_NULL:
            throw new ReadOnlyException("Property member is mapped to null");

        case MAPPED_TO_PARAMETER:
            throw new ReadOnlyException("Property member is mapped to parameter");

        case MAPPED_TO_MEMBER:
            {
                int[] classMemberRef = getClassMemberReference(obj.getTypeId(),
                                                               propertyId,
                                                               member);
                if (classMemberRef == null || classMemberRef.length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container [] = new ContainerBase[1];
                boolean parentIsChanged [] = new boolean [1];
                parentIsChanged[0] = false;
                dereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container[0] == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    ((Float32Container)container[0]).setVal(val);
                }
            }
            break;
        }
    }


    /**
     * Get a Float32 property member from the object using a property.
     *
     * @param obj The object to read from.
     * @param propertyId TypeId of the property to use.
     * @param member Index of the property member to read from.
     * @param index Array index.
     *
     * @return The value of the member.
     *
     * @throws ReadOnlyException The member is inaccessible. Some "parent" item is null.
     * @throws NullException The member is null.
     */
    public static float getFloat32(Object obj,
                                   long propertyId,
                                   int member,
                                   int index)
    {
        switch(getMappingKind(obj.getTypeId(),propertyId,member))
        {
        case MAPPED_TO_NULL:
            throw new NullException("Property member is mapped to null");

        case MAPPED_TO_PARAMETER:
            {
                long[] paramClassTypeId=new long[1];
                int[] paramIndex=new int[1];
                int[] valueIndex=new int[1];
                Kernel.GetPropertyParameterReference(obj.getTypeId(), propertyId, member, index, paramClassTypeId, paramIndex, valueIndex);

                float val [] = new float [1];
                Kernel.GetFloat32Parameter(paramClassTypeId[0], paramIndex[0], valueIndex[0], val);

                return val[0];
            }

        case MAPPED_TO_MEMBER:
            {
                int[] classMemberRef = getClassMemberReference(obj.getTypeId(),
                                                               propertyId,
                                                               member);
                if (classMemberRef == null || classMemberRef.length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container [] = new ContainerBase[1];
                boolean parentIsChanged [] = new boolean [1];
                parentIsChanged[0] = false;
                dereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container[0] == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    return ((Float32Container)container[0]).getVal();
                }
            }
        }
        throw new SoftwareViolationException("Reached end of getFloat32! Internal Error");
    }

   /**
     * Set a Float64 property member in the object using a property.
     *
     * @param obj The object to modify.
     * @param propertyId TypeId of the property to use.
     * @param val The value to set the member to.
     * @param member Index of the property member to modify.
     * @param index Array index.
     * @throws ReadOnlyException If the property member is read-only.
     */
    public static void set(Object obj,
                           long propertyId,
                           double val,
                           int member,
                           int index)
    {
        switch(getMappingKind(obj.getTypeId(),propertyId,member))
        {
        case MAPPED_TO_NULL:
            throw new ReadOnlyException("Property member is mapped to null");

        case MAPPED_TO_PARAMETER:
            throw new ReadOnlyException("Property member is mapped to parameter");

        case MAPPED_TO_MEMBER:
            {
                int[] classMemberRef = getClassMemberReference(obj.getTypeId(),
                                                               propertyId,
                                                               member);
                if (classMemberRef == null || classMemberRef.length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container [] = new ContainerBase[1];
                boolean parentIsChanged [] = new boolean [1];
                parentIsChanged[0] = false;
                dereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container[0] == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    ((Float64Container)container[0]).setVal(val);
                }
            }
            break;
        }
    }


    /**
     * Get a Float64 property member from the object using a property.
     *
     * @param obj The object to read from.
     * @param propertyId TypeId of the property to use.
     * @param member Index of the property member to read from.
     * @param index Array index.
     *
     * @return The value of the member.
     *
     * @throws ReadOnlyException The member is inaccessible. Some "parent" item is null.
     * @throws NullException The member is null.
     */
    public static double getFloat64(Object obj,
                                    long propertyId,
                                    int member,
                                    int index)
    {
        switch(getMappingKind(obj.getTypeId(),propertyId,member))
        {
        case MAPPED_TO_NULL:
            throw new NullException("Property member is mapped to null");

        case MAPPED_TO_PARAMETER:
            {
                long[] paramClassTypeId=new long[1];
                int[] paramIndex=new int[1];
                int[] valueIndex=new int[1];
                Kernel.GetPropertyParameterReference(obj.getTypeId(), propertyId, member, index, paramClassTypeId, paramIndex, valueIndex);

                double val [] = new double [1];
                Kernel.GetFloat64Parameter(paramClassTypeId[0], paramIndex[0], valueIndex[0], val);

                return val[0];
            }

        case MAPPED_TO_MEMBER:
            {
                int[] classMemberRef = getClassMemberReference(obj.getTypeId(),
                                                               propertyId,
                                                               member);
                if (classMemberRef == null || classMemberRef.length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container [] = new ContainerBase[1];
                boolean parentIsChanged [] = new boolean [1];
                parentIsChanged[0] = false;
                dereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container[0] == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    return ((Float64Container)container[0]).getVal();
                }
            }
        }
        throw new SoftwareViolationException("Reached end of getFloat64! Internal Error");
    }

    /**
     * Set an InstanceId property member in the object using a property.
     *
     * @param obj The object to modify.
     * @param propertyId TypeId of the property to use.
     * @param val The value to set the member to.
     * @param member Index of the property member to modify.
     * @param index Array index.
     * @throws ReadOnlyException If the property member is read-only.
     */
    public static void set(Object obj,
                           long propertyId,
                           InstanceId val,
                           int member,
                           int index)
    {
        switch(getMappingKind(obj.getTypeId(),propertyId,member))
        {
        case MAPPED_TO_NULL:
            throw new ReadOnlyException("Property member is mapped to null");

        case MAPPED_TO_PARAMETER:
            throw new ReadOnlyException("Property member is mapped to parameter");

        case MAPPED_TO_MEMBER:
            {
                int[] classMemberRef = getClassMemberReference(obj.getTypeId(),
                                                               propertyId,
                                                               member);
                if (classMemberRef == null || classMemberRef.length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container [] = new ContainerBase[1];
                boolean parentIsChanged [] = new boolean [1];
                parentIsChanged[0] = false;
                dereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container[0] == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    ((InstanceIdContainer)container[0]).setVal(val);
                }
            }
            break;
        }
    }


    /**
     * Get an InstanceId property member from the object using a property.
     *
     * @param obj The object to read from.
     * @param propertyId TypeId of the property to use.
     * @param member Index of the property member to read from.
     * @param index Array index.
     *
     * @return The value of the member.
     *
     * @throws ReadOnlyException The member is inaccessible. Some "parent" item is null.
     * @throws NullException The member is null.
     */
    public static InstanceId getInstanceId(Object obj,
                                           long propertyId,
                                           int member,
                                           int index)
    {
        switch(getMappingKind(obj.getTypeId(),propertyId,member))
        {
        case MAPPED_TO_NULL:
            throw new NullException("Property member is mapped to null");

        case MAPPED_TO_PARAMETER:
            {
                long[] paramClassTypeId=new long[1];
                int[] paramIndex=new int[1];
                int[] valueIndex=new int[1];
                Kernel.GetPropertyParameterReference(obj.getTypeId(), propertyId, member, index, paramClassTypeId, paramIndex, valueIndex);

                long val [] = new long [1];
                String str[]=new String[1];
                Kernel.GetHashedIdParameter(paramClassTypeId[0], paramIndex[0], valueIndex[0], BlobOperations.VALUE_MODE, val, str);

                if (str[0]!=null)
                    return new InstanceId(val[0], str[0]);
                else
                    return new InstanceId(val[0]);
            }

        case MAPPED_TO_MEMBER:
            {
                int[] classMemberRef = getClassMemberReference(obj.getTypeId(),
                                                               propertyId,
                                                               member);
                if (classMemberRef == null || classMemberRef.length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container [] = new ContainerBase[1];
                boolean parentIsChanged [] = new boolean [1];
                parentIsChanged[0] = false;
                dereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container[0] == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    return ((InstanceIdContainer)container[0]).getVal();
                }
            }
        }
        throw new SoftwareViolationException("Reached end of getInstanceId! Internal Error");
    }




    /**
     * Get a TypeId property member from the object using a property.
     *
     * @param obj The object to read from.
     * @param propertyId TypeId of the property to use.
     * @param member Index of the property member to read from.
     * @param index Array index.
     *
     * @return The value of the member.
     *
     * @throws ReadOnlyException The member is inaccessible. Some "parent" item is null.
     * @throws NullException The member is null.
     */
    public static long getTypeId(Object obj,
            long propertyId,
            int member,
            int index) {
        return getInt64(obj,propertyId,member,index);
    }

    /**
     * Set an EntityId property member in the object using a property.
     *
     * @param obj The object to modify.
     * @param propertyId TypeId of the property to use.
     * @param val The value to set the member to.
     * @param member Index of the property member to modify.
     * @param index Array index.
     * @throws ReadOnlyException If the property member is read-only.
     */
    public static void set(Object obj,
                           long propertyId,
                           EntityId val,
                           int member,
                           int index)
    {
        switch(getMappingKind(obj.getTypeId(),propertyId,member))
        {
        case MAPPED_TO_NULL:
            throw new ReadOnlyException("Property member is mapped to null");

        case MAPPED_TO_PARAMETER:
            throw new ReadOnlyException("Property member is mapped to parameter");

        case MAPPED_TO_MEMBER:
            {
                int[] classMemberRef = getClassMemberReference(obj.getTypeId(),
                                                               propertyId,
                                                               member);
                if (classMemberRef == null || classMemberRef.length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container [] = new ContainerBase[1];
                boolean parentIsChanged [] = new boolean [1];
                parentIsChanged[0] = false;
                dereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container[0] == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    ((EntityIdContainer)container[0]).setVal(val);
                }
            }
            break;
        }
    }


    /**
     * Get an EntityId property member from the object using a property.
     *
     * @param obj The object to read from.
     * @param propertyId TypeId of the property to use.
     * @param member Index of the property member to read from.
     * @param index Array index.
     *
     * @return The value of the member.
     *
     * @throws ReadOnlyException The member is inaccessible. Some "parent" item is null.
     * @throws NullException The member is null.
     */
    public static EntityId getEntityId(Object obj,
                                       long propertyId,
                                       int member,
                                       int index)
    {
        switch(getMappingKind(obj.getTypeId(),propertyId,member))
        {
        case MAPPED_TO_NULL:
            throw new NullException("Property member is mapped to null");

        case MAPPED_TO_PARAMETER:
            {
                long[] paramClassTypeId=new long[1];
                int[] paramIndex=new int[1];
                int[] valueIndex=new int[1];
                Kernel.GetPropertyParameterReference(obj.getTypeId(), propertyId, member, index, paramClassTypeId, paramIndex, valueIndex);

                long typeId[] = new long[1];
                long inst [] = new long [1];
                String str[]=new String[1];
                Kernel.GetEntityIdParameter(paramClassTypeId[0], paramIndex[0], valueIndex[0], BlobOperations.VALUE_MODE, typeId, inst, str);

                if (str[0]!=null)
                    return new EntityId(typeId[0], new InstanceId(inst[0], str[0]));
                else
                    return new EntityId(typeId[0], new InstanceId(inst[0]));
            }

        case MAPPED_TO_MEMBER:
            {
                int[] classMemberRef = getClassMemberReference(obj.getTypeId(),
                                                               propertyId,
                                                               member);
                if (classMemberRef == null || classMemberRef.length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container [] = new ContainerBase[1];
                boolean parentIsChanged [] = new boolean [1];
                parentIsChanged[0] = false;
                dereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container[0] == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    return ((EntityIdContainer)container[0]).getVal();
                }
            }
        }
        throw new SoftwareViolationException("Reached end of getEntityId! Internal Error");
    }

    /**
     * Set a ChannelId property member in the object using a property.
     *
     * @param obj The object to modify.
     * @param propertyId TypeId of the property to use.
     * @param val The value to set the member to.
     * @param member Index of the property member to modify.
     * @param index Array index.
     * @throws ReadOnlyException If the property member is read-only.
     */
    public static void set(Object obj,
                           long propertyId,
                           ChannelId val,
                           int member,
                           int index)
    {
        switch (getMappingKind(obj.getTypeId(), propertyId, member))
        {
        case MAPPED_TO_NULL:
            throw new ReadOnlyException("Property member is mapped to null");

        case MAPPED_TO_PARAMETER:
            throw new ReadOnlyException("Property member is mapped to parameter");

        case MAPPED_TO_MEMBER:
            {
                int[] classMemberRef = getClassMemberReference(obj.getTypeId(),
                                                               propertyId,
                                                               member);
                if (classMemberRef == null || classMemberRef.length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container [] = new ContainerBase[1];
                boolean parentIsChanged [] = new boolean [1];
                parentIsChanged[0] = false;
                dereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container[0] == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    ((ChannelIdContainer)container[0]).setVal(val);
                }
            }
            break;
        }
    }


    /**
     * Get a ChannelId property member from the object using a property.
     *
     * @param obj The object to read from.
     * @param propertyId TypeId of the property to use.
     * @param member Index of the property member to read from.
     * @param index Array index.
     *
     * @return The value of the member.
     *
     * @throws ReadOnlyException The member is inaccessible. Some "parent" item is null.
     * @throws NullException The member is null.
     */
    public static ChannelId getChannelId(Object obj,
                                         long propertyId,
                                         int member,
                                         int index)
    {
        switch (getMappingKind(obj.getTypeId(), propertyId, member))
        {
        case MAPPED_TO_NULL:
            throw new NullException("Property member is mapped to null");

        case MAPPED_TO_PARAMETER:
            {
                long[] paramClassTypeId=new long[1];
                int[] paramIndex=new int[1];
                int[] valueIndex=new int[1];
                Kernel.GetPropertyParameterReference(obj.getTypeId(), propertyId, member, index, paramClassTypeId, paramIndex, valueIndex);

                long val [] = new long [1];
                String str[]=new String[1];
                Kernel.GetHashedIdParameter(paramClassTypeId[0], paramIndex[0], valueIndex[0], BlobOperations.VALUE_MODE, val, str);

                if (str[0]!=null)
                    return new ChannelId(val[0], str[0]);
                else
                    return new ChannelId(val[0]);
            }

        case MAPPED_TO_MEMBER:
            {
                int[] classMemberRef = getClassMemberReference(obj.getTypeId(),
                                                               propertyId,
                                                               member);
                if (classMemberRef == null || classMemberRef.length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container [] = new ContainerBase[1];
                boolean parentIsChanged [] = new boolean [1];
                parentIsChanged[0] = false;
                dereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container[0] == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    return ((ChannelIdContainer)container[0]).getVal();
                }
            }
        }
        throw new SoftwareViolationException("Reached end of getChannelId! Internal Error");
    }

    /**
     * Set a HandlerId property member in the object using a property.
     *
     * @param obj The object to modify.
     * @param propertyId TypeId of the property to use.
     * @param val The value to set the member to.
     * @param member Index of the property member to modify.
     * @param index Array index.
     * @throws ReadOnlyException If the property member is read-only.
     */
    public static void set(Object obj,
                           long propertyId,
                           HandlerId val,
                           int member,
                           int index)
    {
        switch (getMappingKind(obj.getTypeId(), propertyId, member))
        {
        case MAPPED_TO_NULL:
            throw new ReadOnlyException("Property member is mapped to null");

        case MAPPED_TO_PARAMETER:
            throw new ReadOnlyException("Property member is mapped to parameter");

        case MAPPED_TO_MEMBER:
            {
                int[] classMemberRef = getClassMemberReference(obj.getTypeId(),
                                                               propertyId,
                                                               member);
                if (classMemberRef == null || classMemberRef.length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container [] = new ContainerBase[1];
                boolean parentIsChanged [] = new boolean [1];
                parentIsChanged[0] = false;
                dereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container[0] == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    ((HandlerIdContainer)container[0]).setVal(val);
                }
            }
            break;
        }
    }


    /**
     * Get a HandlerId property member from the object using a property.
     *
     * @param obj The object to read from.
     * @param propertyId TypeId of the property to use.
     * @param member Index of the property member to read from.
     * @param index Array index.
     *
     * @return The value of the member.
     *
     * @throws ReadOnlyException The member is inaccessible. Some "parent" item is null.
     * @throws NullException The member is null.
     */
    public static HandlerId getHandlerId(Object obj,
                                         long propertyId,
                                         int member,
                                         int index)
    {
        switch (getMappingKind(obj.getTypeId(), propertyId, member))
        {
        case MAPPED_TO_NULL:
            throw new NullException("Property member is mapped to null");

        case MAPPED_TO_PARAMETER:
            {
                long[] paramClassTypeId=new long[1];
                int[] paramIndex=new int[1];
                int[] valueIndex=new int[1];
                Kernel.GetPropertyParameterReference(obj.getTypeId(), propertyId, member, index, paramClassTypeId, paramIndex, valueIndex);

                long val [] = new long [1];
                String str[]=new String[1];
                Kernel.GetHashedIdParameter(paramClassTypeId[0], paramIndex[0], valueIndex[0], BlobOperations.VALUE_MODE, val, str);

                if (str[0]!=null)
                    return new HandlerId(val[0], str[0]);
                else
                    return new HandlerId(val[0]);
            }

        case MAPPED_TO_MEMBER:
            {
                int[] classMemberRef = getClassMemberReference(obj.getTypeId(),
                                                               propertyId,
                                                               member);
                if (classMemberRef == null || classMemberRef.length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container [] = new ContainerBase[1];
                boolean parentIsChanged [] = new boolean [1];
                parentIsChanged[0] = false;
                dereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container[0] == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    return ((HandlerIdContainer)container[0]).getVal();
                }
            }
        }
        throw new SoftwareViolationException("Reached end of getHandlerId! Internal Error");
    }


    /**
     * Set a String property member in the object using a property.
     *
     * @param obj The object to modify.
     * @param propertyId TypeId of the property to use.
     * @param val The value to set the member to.
     * @param member Index of the property member to modify.
     * @param index Array index.
     * @throws ReadOnlyException If the property member is read-only.
     */
    public static void set(Object obj,
                           long propertyId,
                           String val,
                           int member,
                           int index)
    {
        switch(getMappingKind(obj.getTypeId(),propertyId,member))
        {
        case MAPPED_TO_NULL:
            throw new ReadOnlyException("Property member is mapped to null");

        case MAPPED_TO_PARAMETER:
            throw new ReadOnlyException("Property member is mapped to parameter");

        case MAPPED_TO_MEMBER:
            {
                int[] classMemberRef = getClassMemberReference(obj.getTypeId(),
                                                               propertyId,
                                                               member);
                if (classMemberRef == null || classMemberRef.length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container [] = new ContainerBase[1];
                boolean parentIsChanged [] = new boolean [1];
                parentIsChanged[0] = false;
                dereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container[0] == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    ((StringContainer)container[0]).setVal(val);
                }
            }
            break;
        }
    }


    /**
     * Get a String property member from the object using a property.
     *
     * @param obj The object to read from.
     * @param propertyId TypeId of the property to use.
     * @param member Index of the property member to read from.
     * @param index Array index.
     *
     * @return The value of the member.
     *
     * @throws ReadOnlyException The member is inaccessible. Some "parent" item is null.
     * @throws NullException The member is null.
     */
    public static String getString(Object obj,
                                   long propertyId,
                                   int member,
                                   int index)
    {
        switch(getMappingKind(obj.getTypeId(),propertyId,member))
        {
        case MAPPED_TO_NULL:
            throw new NullException("Property member is mapped to null");

        case MAPPED_TO_PARAMETER:
            {
                long[] paramClassTypeId=new long[1];
                int[] paramIndex=new int[1];
                int[] valueIndex=new int[1];
                Kernel.GetPropertyParameterReference(obj.getTypeId(), propertyId, member, index, paramClassTypeId, paramIndex, valueIndex);

                String val [] = new String [1];
                Kernel.GetStringParameter(paramClassTypeId[0], paramIndex[0], valueIndex[0], BlobOperations.VALUE_MODE, val);
                return val[0];
            }

        case MAPPED_TO_MEMBER:
            {
                int[] classMemberRef = getClassMemberReference(obj.getTypeId(),
                                                               propertyId,
                                                               member);
                if (classMemberRef == null || classMemberRef.length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container [] = new ContainerBase[1];
                boolean parentIsChanged [] = new boolean [1];
                parentIsChanged[0] = false;
                dereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container[0] == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    return ((StringContainer)container[0]).getVal();
                }
            }
        }
        throw new SoftwareViolationException("Reached end of getString! Internal Error");
    }

    /**
     * Set an Object property member in the object using a property.
     *
     * @param obj The object to modify.
     * @param propertyId TypeId of the property to use.
     * @param val The value to set the member to.
     * @param member Index of the property member to modify.
     * @param index Array index.
     * @throws ReadOnlyException If the property member is read-only.
     */
    public static void set(Object obj,
                           long propertyId,
                           Object val,
                           int member,
                           int index)
    {
        switch(getMappingKind(obj.getTypeId(),propertyId,member))
        {
        case MAPPED_TO_NULL:
            throw new ReadOnlyException("Property member is mapped to null");

        case MAPPED_TO_PARAMETER:
            throw new ReadOnlyException("Property member is mapped to parameter");

        case MAPPED_TO_MEMBER:
            {
                int[] classMemberRef = getClassMemberReference(obj.getTypeId(),
                                                               propertyId,
                                                               member);
                if (classMemberRef == null || classMemberRef.length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container [] = new ContainerBase[1];
                boolean parentIsChanged [] = new boolean [1];
                parentIsChanged[0] = false;
                dereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container[0] == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    ((ObjectContainerBase)container[0]).setObjInternal(val);
                    ((ObjectContainerBase)container[0]).m_isChanged = true;
                }
            }
            break;
        }
    }


    /**
     * Get an Object property member from the object using a property.
     *
     * @param obj The object to read from.
     * @param propertyId TypeId of the property to use.
     * @param member Index of the property member to read from.
     * @param index Array index.
     *
     * @return The value of the member.
     *
     * @throws ReadOnlyException The member is inaccessible. Some "parent" item is null.
     * @throws NullException The member is null.
     */
    public static Object getObject(Object obj,
                                   long propertyId,
                                   int member,
                                   int index)
    {
        switch(getMappingKind(obj.getTypeId(),propertyId,member))
        {
        case MAPPED_TO_NULL:
            throw new NullException("Property member is mapped to null");

        case MAPPED_TO_PARAMETER:
            {
                long[] paramClassTypeId=new long[1];
                int[] paramIndex=new int[1];
                int[] valueIndex=new int[1];
                Kernel.GetPropertyParameterReference(obj.getTypeId(), propertyId, member, index, paramClassTypeId, paramIndex, valueIndex);

                java.nio.ByteBuffer val [] = new java.nio.ByteBuffer [1];
                Kernel.GetObjectParameter(paramClassTypeId[0], paramIndex[0], valueIndex[0], val);

                Object result = ObjectFactory.getInstance().createObject(val[0]);
                result.setChanged(false);
                return result;
            }

        case MAPPED_TO_MEMBER:
            {
                int[] classMemberRef = getClassMemberReference(obj.getTypeId(),
                                                               propertyId,
                                                               member);
                if (classMemberRef == null || classMemberRef.length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container [] = new ContainerBase[1];
                boolean parentIsChanged [] = new boolean [1];
                parentIsChanged[0] = false;
                dereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container[0] == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    if (((ObjectContainerBase)container[0]).isNull())
                    {
                        throw new NullException("Object is null");
                    }
                    else
                    {
                        return ((ObjectContainerBase)container[0]).getObjInternal();
                    }
                }
            }
        }
        throw new SoftwareViolationException("Reached end of getObject! Internal Error");
    }

    /**
     * Set a Binary property member in the object using a property.
     *
     * @param obj The object to modify.
     * @param propertyId TypeId of the property to use.
     * @param val The value to set the member to.
     * @param member Index of the property member to modify.
     * @param index Array index.
     * @throws ReadOnlyException If the property member is read-only.
     */
    public static void set(Object obj,
                           long propertyId,
                           byte[] val,
                           int member,
                           int index)
    {
        switch (getMappingKind(obj.getTypeId(), propertyId, member))
        {
        case MAPPED_TO_NULL:
            throw new ReadOnlyException("Property member is mapped to null");

        case MAPPED_TO_PARAMETER:
            throw new ReadOnlyException("Property member is mapped to parameter");

        case MAPPED_TO_MEMBER:
            {
                int[] classMemberRef = getClassMemberReference(obj.getTypeId(),
                                                               propertyId,
                                                               member);
                if (classMemberRef == null || classMemberRef.length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container [] = new ContainerBase[1];
                boolean parentIsChanged [] = new boolean [1];
                parentIsChanged[0] = false;
                dereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container[0] == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    ((BinaryContainer)container[0]).setVal(val);
                }
            }
            break;
        }
    }

    /**
     * Get a Binary property member from the object using a property.
     *
     * @param obj The object to read from.
     * @param propertyId TypeId of the property to use.
     * @param member Index of the property member to read from.
     * @param index Array index.
     *
     * @return The value of the member.
     *
     * @throws ReadOnlyException The member is inaccessible. Some "parent" item is null.
     * @throws NullException The member is null.
     */
    public static byte[] getBinary(Object obj,
                                   long propertyId,
                                   int member,
                                   int index)
    {
        switch (getMappingKind(obj.getTypeId(), propertyId, member))
        {
        case MAPPED_TO_NULL:
            throw new NullException("Property member is mapped to null");

        case MAPPED_TO_PARAMETER:
            {
                long[] paramClassTypeId=new long[1];
                int[] paramIndex=new int[1];
                int[] valueIndex=new int[1];
                Kernel.GetPropertyParameterReference(obj.getTypeId(), propertyId, member, index, paramClassTypeId, paramIndex, valueIndex);

                java.nio.ByteBuffer val [] = new java.nio.ByteBuffer [1];
                Kernel.GetBinaryParameter(paramClassTypeId[0], paramIndex[0], valueIndex[0], val);

                byte[] result = new byte [val[0].capacity()];
                val[0].clear(); //reset position
                val[0].get(result);
                return result;
            }

        case MAPPED_TO_MEMBER:
            {
                int[] classMemberRef = getClassMemberReference(obj.getTypeId(),
                                                               propertyId,
                                                               member);
                if (classMemberRef == null || classMemberRef.length == 0)
                {
                    throw new SoftwareViolationException("Failed to get class member reference from dots_kernel");
                }

                ContainerBase container [] = new ContainerBase[1];
                boolean parentIsChanged [] = new boolean [1];
                parentIsChanged[0] = false;
                dereferenceClassMemberReference(obj,
                                                classMemberRef,
                                                0,
                                                index,
                                                container,
                                                parentIsChanged);
                if (container[0] == null)
                {
                    throw new ReadOnlyException("Unable to dereference property, some parent is null");
                }
                else
                {
                    return ((BinaryContainer)container[0]).getVal();
                }
            }
        }
        throw new SoftwareViolationException("Reached end of getBinary! Internal Error");
    }

    /**
     * Get information needed to read the value of a property that is mapped to a parameter without the need of an ObjectPtr.
     * This method is only allowed to be called if mappingKind is 'MAPPED_TO_PARAMETER'.
     *
     * @param classId Type id of a class that supports the specified property.
     * @param propertyId Type id of the property.
     * @param propertyMember Index of the property member.
     * @param propertyIndex Array index of the property.
     *
     * @return The parameter referencs as an array [parameterTypeId, parameterIndex, parameterArrayIndex]
     *
     * @throws IllegalValueException The member is inaccessible. Some "parent" item is null.
     */
    public static Number[] getParameterReference(long classId,
                                             long propertyId,
                                             int propertyMember,
                                             int propertyIndex)
    {
        if (getMappingKind(classId, propertyId, propertyMember) != PropertyMappingKind.MAPPED_TO_PARAMETER)
        {
            throw new IllegalValueException("That property member is not mapped to a parameter!");
        }

        long[] paramClassTypeId=new long[1];
        int[] paramIndex=new int[1];
        int[] valueIndex=new int[1];
        Kernel.GetPropertyParameterReference(classId, propertyId, propertyMember, propertyIndex, paramClassTypeId, paramIndex, valueIndex);

        return new Number[]{paramClassTypeId[0], paramIndex[0], valueIndex[0]};
    }


    private static int[] getClassMemberReference(long typeId,
                                                 long propertyId,
                                                 int member)
    {
        java.nio.ByteBuffer classMemberRef[] = new java.nio.ByteBuffer[1];

        Kernel.GetClassMemberReference(typeId,
                                       propertyId,
                                       member,
                                       classMemberRef);
        classMemberRef[0].order(java.nio.ByteOrder.nativeOrder());

        java.nio.IntBuffer ints = classMemberRef[0].asIntBuffer();
        int[] result = new int[ints.capacity()];
        ints.get(result);
        return result;
    }

    //if container[0] == null then a parent was null
    private static void dereferenceClassMemberReference(com.saabgroup.safir.dob.typesystem.Object obj,
                                                        int [] classmemberref,
                                                        int pos,
                                                        int index,
                                                        ContainerBase container [], //out parameter
                                                        boolean parentIsChanged []) //out parameter
    {
        if (classmemberref.length - pos > 2) //we need to recurse into child objects
        {
            ContainerBase member = obj.getMember(classmemberref[pos],classmemberref[pos + 1]);

            if (member.isChanged())
            {
                parentIsChanged[0] = true;
            }

            if (member.isNull())
            {
                container = null;
            }
            else
            {
                dereferenceClassMemberReference(((ObjectContainerBase)member).getObjInternal(),
                                                classmemberref,
                                                pos + 2,
                                                index,
                                                container,
                                                parentIsChanged);
            }
        }
        else
        {
            if (classmemberref[pos + 1] == -1)//pointing at an array, use the index from the function call
            {
                container[0] = obj.getMember(classmemberref[pos],index);
            }
            else
            {
                if (index != 0)
                {
                    throw new SoftwareViolationException("CMR says that the member is not an array, but I got passed an index != 0");
                }
                container[0] = obj.getMember(classmemberref[pos],classmemberref[pos+1]);
            }
        }
    }

}
