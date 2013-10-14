// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2009-2013 (http://safir.sourceforge.net)
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

import  com.saabgroup.safir.dob.typesystem.si64.*;
/**
 * Operations on blobs.
 *
 * Functions for getting information from blobs and setting and getting
 * values in blobs.
 * Currently this functionality is meant mainly for internal use, but the functions
 * can be used to modify binary serializations of objects.
 * But be careful if you do, and talk to your closest DOB expert first.
 *
 * Note: Most of these methods have no checks on them to make sure that everything
 *       has worked okay. They will just return unexpected values if something went wrong.
 */
public class BlobOperations {


    /**
     * Extract the TypeId from a blob
     *
     * @param blob [in] - The blob to read from.
     * @return The TypeId from the blob.
     */
    public static long getTypeId(java.nio.ByteBuffer blob){
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        return Kernel.GetTypeId(blob);
    }

    /**
     * Get the size of the blob contained by this object
     *
     * @param blob [in] - the blob.
     * @return the size of the blob, in bytes.
     */
    public static int getSize(java.nio.ByteBuffer blob){
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }
        return Kernel.GetSize(blob);
    }

    /**
     * Check if any member is changed.
     *
     * This method will recursively check if any member in the blob has its change flag set.
     *
     * @param blob the blob to check.
     * @return True if any member has changed.
     */
    public static boolean isChanged(java.nio.ByteBuffer blob){
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }
        return Kernel.IsAnythingChanged(blob);
    }

    /**
     * Find out if a member is changed.
     *
     * @param blob - Blob to look in.
     * @param member - The member to check.
     * @param index - Array index in member to check. Shall be 0 if the member is not an array.
     * @return true if member is changed.
     */

    public static boolean isChanged(java.nio.ByteBuffer blob,
                                    int member,
                                    int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }
        return Kernel.IsChangedMember(blob,member,index);
    }


    /**
     * Set a member to null.
     *
     * This methods sets a given member (with index) to null in a blob.
     * If the member is not an array the index must be 0.
     *
     * @param blob - Blob to set the member in.
     * @param member - The member to be set.
     * @param index - Array index in member to set. Shall be 0 if the member is not an array.
     */

    public static void setNull(java.nio.ByteBuffer blob,
                               int member,
                               int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }
        Kernel.SetNullMember(blob, member, index);
    }

    /**
     * Get the static blob size of a type, but excluding the size that is inherited from parent classes.
     *
     * This is very much an internal function!
     * Unless you have a really good reason to use this function you should stay clear of it.
     *
     * @param typeId The TypeId of a DOB class.
     * @return static blob size of an type
     */
    public static int getInitialSize(long typeId)
    {
        return Kernel.GetInitialSize(typeId);
    }

    /**
     * Get a Boolean from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static BooleanContainer getBoolean(java.nio.ByteBuffer blob,
                                            int member,
                                            int index){
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        boolean value[] = new boolean[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetBooleanMember(blob, member, index, value, isNull, isChanged);
        return new BooleanContainer(value[0],isNull[0],isChanged[0]);
    }

    /**
     * Set a Boolean in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setBoolean(BooleanContainer value,
                                  java.nio.ByteBuffer blob,
                                  int member,
                                  int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }
        Kernel.SetBooleanMemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get an enumeration from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * NOTE THAT THIS METHOD HAS AN OUT PARAMETER. It was just too difficult to do it any other way...
     *
     * @param enumContainer OUT PARAMETER! The container to put the values into.
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     */
    public static void getEnumeration(EnumerationContainerBase<?> enumContainer,
                                      java.nio.ByteBuffer blob,
                                      int member,
                                      int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        int value[] = new int[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetInt32Member(blob, member, index, value, isNull, isChanged);
        enumContainer.m_Value = value[0];
        enumContainer.m_bIsNull = isNull[0];
        enumContainer.m_isChanged = isChanged[0];
    }

    /**
     * Set an Enumeration in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setEnumeration(EnumerationContainerBase<?> value,
                                      java.nio.ByteBuffer blob,
                                      int member,
                                      int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetInt32MemberInPreallocated
               (value.m_Value,
                value.m_bIsNull,
                value.m_isChanged,
                blob,
                member,
                index);
    }

    /**
     * Set an Int32 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setInt32(Int32Container value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetInt32MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get an Int32 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static Int32Container getInt32(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        int value[] = new int[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetInt32Member(blob, member, index, value, isNull, isChanged);
        return new Int32Container(value[0],isNull[0],isChanged[0]);
    }

    /**
     * Set an Int64 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setInt64(Int64Container value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetInt64MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get an Int64 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static Int64Container getInt64(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        long value[] = new long[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetInt64Member(blob, member, index, value, isNull, isChanged);
        return new Int64Container(value[0],isNull[0],isChanged[0]);
    }

    /**
     * Set a Float32 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setFloat32(Float32Container value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetFloat32MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a Float32 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static Float32Container getFloat32(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetFloat32Member(blob, member, index, value, isNull, isChanged);
        return new Float32Container(value[0],isNull[0],isChanged[0]);
    }

    /**
     * Set a Float64 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setFloat64(Float64Container value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetFloat64MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a Float64 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static Float64Container getFloat64(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetFloat64Member(blob, member, index, value, isNull, isChanged);
        return new Float64Container(value[0],isNull[0],isChanged[0]);
    }


    /**
     * Set a TypeId in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setTypeId(TypeIdContainer value,
                                 java.nio.ByteBuffer blob,
                                 int member,
                                 int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetInt64MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a TypeId from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static TypeIdContainer getTypeId(java.nio.ByteBuffer blob,
                                            int member,
                                            int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        long value[] = new long[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetInt64Member(blob, member, index, value, isNull, isChanged);
        return new TypeIdContainer(value[0],isNull[0],isChanged[0]);
    }

    /**
     * Set an InstanceId in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param container The container whose values to use.
     * @param blob Blob to set the member in.
     * @param beginningOfUnused Beginning of unused part of dynamic part of blob.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     * @return The new value for beginningOfUnused.
     */
    public static int setInstanceId(InstanceIdContainer container,
                                    java.nio.ByteBuffer blob,
                                    int beginningOfUnused,
                                    int member,
                                    int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        if (!container.isNull())
        {
            int beginningOfUnusedInOut [] = new int [1];
            beginningOfUnusedInOut[0] = beginningOfUnused;
            Kernel.SetHashedIdMemberInPreallocated(container.getVal().getRawValue(),
                                                   container.getVal().getRawString(),
                                                   container.getVal().utf8StringLength(),
                                                   container.isNull(),
                                                   container.isChanged(),
                                                   blob,
                                                   member,
                                                   index,
                                                   beginningOfUnusedInOut);

            beginningOfUnused = beginningOfUnusedInOut[0];
        }
        else if (container.isChanged())
        {
            setNull(blob,member,index);
        }
        return beginningOfUnused;
    }

    /**
     * Get a InstanceId from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static InstanceIdContainer getInstanceId(java.nio.ByteBuffer blob,
                                                    int member,
                                                    int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        long hashVal[] = new long[1];
        String strVal[] = new String[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.GetHashedIdMember(blob, member, index, hashVal, strVal, isNull, isChanged);
        if (isNull[0]) {
            return new InstanceIdContainer(null,isNull[0],isChanged[0]);
        }

        if (strVal[0] != null) {
            return new InstanceIdContainer(new InstanceId(hashVal[0],strVal[0]),isNull[0],isChanged[0]);
        }
        else {
            return new InstanceIdContainer(new InstanceId(hashVal[0]),isNull[0],isChanged[0]);
        }
    }

    /**
     * Set an ChannelId in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param container The container whose values to use.
     * @param blob Blob to set the member in.
     * @param beginningOfUnused Beginning of unused part of dynamic part of blob.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     * @return The new value for beginningOfUnused.
     */
    public static int setChannelId(ChannelIdContainer container,
                                                    java.nio.ByteBuffer blob,
                                                    int beginningOfUnused,
                                                    int member,
                                                    int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        if (!container.isNull())
        {
            int beginningOfUnusedInOut [] = new int [1];
            beginningOfUnusedInOut[0] = beginningOfUnused;
            Kernel.SetHashedIdMemberInPreallocated(container.getVal().getRawValue(),
                                                   container.getVal().getRawString(),
                                                   container.getVal().utf8StringLength(),
                                                   container.isNull(),
                                                   container.isChanged(),
                                                   blob,
                                                   member,
                                                   index,
                                                   beginningOfUnusedInOut);

            beginningOfUnused = beginningOfUnusedInOut[0];
        }
        else if (container.isChanged())
        {
            setNull(blob,member,index);
        }
        return beginningOfUnused;
    }

    /**
     * Get a ChannelId from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static ChannelIdContainer getChannelId(java.nio.ByteBuffer blob,
                                                    int member,
                                                    int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        long hashVal[] = new long[1];
        String strVal[] = new String[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetHashedIdMember(blob, member, index, hashVal, strVal, isNull, isChanged);
        if (isNull[0]) {
            return new ChannelIdContainer(null,isNull[0],isChanged[0]);
        }

        if (strVal[0] != null) {
            return new ChannelIdContainer(new ChannelId(hashVal[0],strVal[0]),isNull[0],isChanged[0]);
        }
        else {
            return new ChannelIdContainer(new ChannelId(hashVal[0]),isNull[0],isChanged[0]);
        }
    }

    /**
     * Set an HandlerId in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param container The container whose values to use.
     * @param blob Blob to set the member in.
     * @param beginningOfUnused Beginning of unused part of dynamic part of blob.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     * @return The new value for beginningOfUnused.
     */
    public static int setHandlerId(HandlerIdContainer container,
                                                    java.nio.ByteBuffer blob,
                                                    int beginningOfUnused,
                                                    int member,
                                                    int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        if (!container.isNull())
        {
            int beginningOfUnusedInOut [] = new int [1];
            beginningOfUnusedInOut[0] = beginningOfUnused;
            Kernel.SetHashedIdMemberInPreallocated(container.getVal().getRawValue(),
                                                   container.getVal().getRawString(),
                                                   container.getVal().utf8StringLength(),
                                                   container.isNull(),
                                                   container.isChanged(),
                                                   blob,
                                                   member,
                                                   index,
                                                   beginningOfUnusedInOut);

            beginningOfUnused = beginningOfUnusedInOut[0];
        }
        else if (container.isChanged())
        {
            setNull(blob,member,index);
        }
        return beginningOfUnused;
    }

    /**
     * Get a HandlerId from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static HandlerIdContainer getHandlerId(java.nio.ByteBuffer blob,
                                                    int member,
                                                    int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        long hashVal[] = new long[1];
        String strVal[] = new String[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetHashedIdMember(blob, member, index, hashVal, strVal, isNull, isChanged);
        if (isNull[0]) {
            return new HandlerIdContainer(null,isNull[0],isChanged[0]);
        }

        if (strVal[0] != null) {
            return new HandlerIdContainer(new HandlerId(hashVal[0],strVal[0]),isNull[0],isChanged[0]);
        }
        else {
            return new HandlerIdContainer(new HandlerId(hashVal[0]),isNull[0],isChanged[0]);
        }
    }


    /**
     * Set an EntityId in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param container The container whose values to use.
     * @param blob Blob to set the member in.
     * @param beginningOfUnused Beginning of unused part of dynamic part of blob.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     * @return The new value for beginningOfUnused.
     */
    public static int setEntityId(EntityIdContainer container,
                                                  java.nio.ByteBuffer blob,
                                                  int beginningOfUnused,
                                                  int member,
                                                  int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        if (!container.isNull())
        {
            int beginningOfUnusedInOut [] = new int [1];
            beginningOfUnusedInOut[0] = beginningOfUnused;
            Kernel.SetEntityIdMemberInPreallocated(container.getVal().getTypeId(),
                                                   container.getVal().getInstanceId().getRawValue(),
                                                   container.getVal().getInstanceId().getRawString(),
                                                   container.getVal().getInstanceId().utf8StringLength(),
                                                   container.isNull(),
                                                   container.isChanged(),
                                                   blob,
                                                   member,
                                                   index,
                                                   beginningOfUnusedInOut);

            beginningOfUnused = beginningOfUnusedInOut[0];
        }
        else if (container.isChanged())
        {
            setNull(blob,member,index);
        }
        return beginningOfUnused;
    }

    /**
     * Get a EntityId from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static EntityIdContainer getEntityId(java.nio.ByteBuffer blob,
                                                    int member,
                                                    int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        long typeId[] = new long[1];
        long instanceId[] = new long[1];
        String strVal[] = new String[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetEntityIdMember(blob, member, index, typeId, instanceId, strVal, isNull, isChanged);

        if (isNull[0]) {
            return new EntityIdContainer(null,isNull[0],isChanged[0]);
        }

        if (strVal[0] != null) {
            return new EntityIdContainer(new EntityId(typeId[0],new InstanceId(instanceId[0],strVal[0])),isNull[0],isChanged[0]);
        }
        else {
            return new EntityIdContainer(new EntityId(typeId[0],new InstanceId(instanceId[0])),isNull[0],isChanged[0]);
        }
    }


    /**
     * Set an String in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param container The container whose values to use.
     * @param blob Blob to set the member in.
     * @param beginningOfUnused Beginning of unused part of dynamic part of blob.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     * @return The new value for beginningOfUnused.
     */
    public static int setString(StringContainer container,
                                java.nio.ByteBuffer blob,
                                int beginningOfUnused,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        if (!container.isNull())
        {
            int strLength = container.utf8StringLength();
            int stringStart = beginningOfUnused;
            int beginningOfUnusedInOut [] = new int [1];
            beginningOfUnusedInOut[0] = beginningOfUnused;
            Kernel.CreateStringMember(blob,
                                      strLength,
                                      member,
                                      index,
                                      container.isChanged(),
                                      beginningOfUnusedInOut);
            blob.position(stringStart);
            blob.put(container.utf8String(),0,strLength-1);
            blob.put((byte)0);
            beginningOfUnused = beginningOfUnusedInOut[0];
        }
        else if (container.isChanged())
        {
            setNull(blob,member,index);
        }
        return beginningOfUnused;
    }

    /**
     * Get a String from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static StringContainer getString(java.nio.ByteBuffer blob,
                                            int member,
                                            int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        String value[] = new String[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetStringMember(blob, member, index, value, isNull, isChanged);
        if (isNull[0]) {
            return new StringContainer(null,isNull[0],isChanged[0]);
        }
        else {
            return new StringContainer(value[0],isNull[0],isChanged[0]);
        }
    }


    /**
     * Set a Binary in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param container The container whose values to use.
     * @param blob Blob to set the member in.
     * @param beginningOfUnused Beginning of unused part of dynamic part of blob.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     * @return The new value for beginningOfUnused.
     */
    public static int setBinary(BinaryContainer container,
                                java.nio.ByteBuffer blob,
                                int beginningOfUnused,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        if (!container.isNull())
        {
            int binaryStart = beginningOfUnused;
            int beginningOfUnusedInOut [] = new int [1];
            beginningOfUnusedInOut[0] = beginningOfUnused;
            Kernel.CreateBinaryMember(blob,
                                      container.getVal().length,
                                      member,
                                      index,
                                      container.isChanged(),
                                      beginningOfUnusedInOut);

            blob.position(binaryStart);
            blob.put(container.getVal(),0,container.getVal().length);
            beginningOfUnused = beginningOfUnusedInOut[0];
        }
        else if (container.isChanged())
        {
            setNull(blob,member,index);
        }
        return beginningOfUnused;
    }

    /**
     * Get a Binary from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static BinaryContainer getBinary(java.nio.ByteBuffer blob,
                                            int member,
                                            int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        java.nio.ByteBuffer [] value = new java.nio.ByteBuffer[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetBinaryMember(blob, member, index, value, isNull, isChanged);
        if (isNull[0]) {
            return new BinaryContainer(null,isNull[0],isChanged[0]);
        }
        else {
            byte [] result = new byte[value[0].capacity()];
            value[0].clear(); //reset position
            value[0].get(result);
            return new BinaryContainer(result,isNull[0],isChanged[0]);
        }
    }


    /**
     * Set a Ampere32 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setAmpere32(com.saabgroup.safir.dob.typesystem.si32.AmpereContainer value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetFloat32MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a Ampere32 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static com.saabgroup.safir.dob.typesystem.si32.AmpereContainer getAmpere32(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetFloat32Member(blob, member, index, value, isNull, isChanged);
        return new com.saabgroup.safir.dob.typesystem.si32.AmpereContainer(value[0],isNull[0],isChanged[0]);
    }


    /**
     * Set a CubicMeter32 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setCubicMeter32(com.saabgroup.safir.dob.typesystem.si32.CubicMeterContainer value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetFloat32MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a CubicMeter32 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static com.saabgroup.safir.dob.typesystem.si32.CubicMeterContainer getCubicMeter32(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetFloat32Member(blob, member, index, value, isNull, isChanged);
        return new com.saabgroup.safir.dob.typesystem.si32.CubicMeterContainer(value[0],isNull[0],isChanged[0]);
    }


    /**
     * Set a Hertz32 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setHertz32(com.saabgroup.safir.dob.typesystem.si32.HertzContainer value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetFloat32MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a Hertz32 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static com.saabgroup.safir.dob.typesystem.si32.HertzContainer getHertz32(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetFloat32Member(blob, member, index, value, isNull, isChanged);
        return new com.saabgroup.safir.dob.typesystem.si32.HertzContainer(value[0],isNull[0],isChanged[0]);
    }


    /**
     * Set a Joule32 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setJoule32(com.saabgroup.safir.dob.typesystem.si32.JouleContainer value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetFloat32MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a Joule32 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static com.saabgroup.safir.dob.typesystem.si32.JouleContainer getJoule32(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetFloat32Member(blob, member, index, value, isNull, isChanged);
        return new com.saabgroup.safir.dob.typesystem.si32.JouleContainer(value[0],isNull[0],isChanged[0]);
    }


    /**
     * Set a Kelvin32 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setKelvin32(com.saabgroup.safir.dob.typesystem.si32.KelvinContainer value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetFloat32MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a Kelvin32 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static com.saabgroup.safir.dob.typesystem.si32.KelvinContainer getKelvin32(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetFloat32Member(blob, member, index, value, isNull, isChanged);
        return new com.saabgroup.safir.dob.typesystem.si32.KelvinContainer(value[0],isNull[0],isChanged[0]);
    }


    /**
     * Set a Kilogram32 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setKilogram32(com.saabgroup.safir.dob.typesystem.si32.KilogramContainer value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetFloat32MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a Kilogram32 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static com.saabgroup.safir.dob.typesystem.si32.KilogramContainer getKilogram32(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetFloat32Member(blob, member, index, value, isNull, isChanged);
        return new com.saabgroup.safir.dob.typesystem.si32.KilogramContainer(value[0],isNull[0],isChanged[0]);
    }


    /**
     * Set a Meter32 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setMeter32(com.saabgroup.safir.dob.typesystem.si32.MeterContainer value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetFloat32MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a Meter32 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static com.saabgroup.safir.dob.typesystem.si32.MeterContainer getMeter32(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetFloat32Member(blob, member, index, value, isNull, isChanged);
        return new com.saabgroup.safir.dob.typesystem.si32.MeterContainer(value[0],isNull[0],isChanged[0]);
    }


    /**
     * Set a MeterPerSecond32 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setMeterPerSecond32(com.saabgroup.safir.dob.typesystem.si32.MeterPerSecondContainer value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetFloat32MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a MeterPerSecond32 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static com.saabgroup.safir.dob.typesystem.si32.MeterPerSecondContainer getMeterPerSecond32(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetFloat32Member(blob, member, index, value, isNull, isChanged);
        return new com.saabgroup.safir.dob.typesystem.si32.MeterPerSecondContainer(value[0],isNull[0],isChanged[0]);
    }


    /**
     * Set a MeterPerSecondSquared32 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setMeterPerSecondSquared32(com.saabgroup.safir.dob.typesystem.si32.MeterPerSecondSquaredContainer value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetFloat32MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a MeterPerSecondSquared32 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static com.saabgroup.safir.dob.typesystem.si32.MeterPerSecondSquaredContainer getMeterPerSecondSquared32(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetFloat32Member(blob, member, index, value, isNull, isChanged);
        return new com.saabgroup.safir.dob.typesystem.si32.MeterPerSecondSquaredContainer(value[0],isNull[0],isChanged[0]);
    }


    /**
     * Set a Newton32 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setNewton32(com.saabgroup.safir.dob.typesystem.si32.NewtonContainer value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetFloat32MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a Newton32 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static com.saabgroup.safir.dob.typesystem.si32.NewtonContainer getNewton32(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetFloat32Member(blob, member, index, value, isNull, isChanged);
        return new com.saabgroup.safir.dob.typesystem.si32.NewtonContainer(value[0],isNull[0],isChanged[0]);
    }


    /**
     * Set a Pascal32 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setPascal32(com.saabgroup.safir.dob.typesystem.si32.PascalContainer value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetFloat32MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a Pascal32 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static com.saabgroup.safir.dob.typesystem.si32.PascalContainer getPascal32(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetFloat32Member(blob, member, index, value, isNull, isChanged);
        return new com.saabgroup.safir.dob.typesystem.si32.PascalContainer(value[0],isNull[0],isChanged[0]);
    }


    /**
     * Set a Radian32 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setRadian32(com.saabgroup.safir.dob.typesystem.si32.RadianContainer value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetFloat32MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a Radian32 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static com.saabgroup.safir.dob.typesystem.si32.RadianContainer getRadian32(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetFloat32Member(blob, member, index, value, isNull, isChanged);
        return new com.saabgroup.safir.dob.typesystem.si32.RadianContainer(value[0],isNull[0],isChanged[0]);
    }


    /**
     * Set a RadianPerSecond32 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setRadianPerSecond32(com.saabgroup.safir.dob.typesystem.si32.RadianPerSecondContainer value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetFloat32MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a RadianPerSecond32 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static com.saabgroup.safir.dob.typesystem.si32.RadianPerSecondContainer getRadianPerSecond32(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetFloat32Member(blob, member, index, value, isNull, isChanged);
        return new com.saabgroup.safir.dob.typesystem.si32.RadianPerSecondContainer(value[0],isNull[0],isChanged[0]);
    }


    /**
     * Set a RadianPerSecondSquared32 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setRadianPerSecondSquared32(com.saabgroup.safir.dob.typesystem.si32.RadianPerSecondSquaredContainer value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetFloat32MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a RadianPerSecondSquared32 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static com.saabgroup.safir.dob.typesystem.si32.RadianPerSecondSquaredContainer getRadianPerSecondSquared32(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetFloat32Member(blob, member, index, value, isNull, isChanged);
        return new com.saabgroup.safir.dob.typesystem.si32.RadianPerSecondSquaredContainer(value[0],isNull[0],isChanged[0]);
    }


    /**
     * Set a Second32 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setSecond32(com.saabgroup.safir.dob.typesystem.si32.SecondContainer value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetFloat32MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a Second32 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static com.saabgroup.safir.dob.typesystem.si32.SecondContainer getSecond32(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetFloat32Member(blob, member, index, value, isNull, isChanged);
        return new com.saabgroup.safir.dob.typesystem.si32.SecondContainer(value[0],isNull[0],isChanged[0]);
    }


    /**
     * Set a SquareMeter32 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setSquareMeter32(com.saabgroup.safir.dob.typesystem.si32.SquareMeterContainer value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetFloat32MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a SquareMeter32 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static com.saabgroup.safir.dob.typesystem.si32.SquareMeterContainer getSquareMeter32(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetFloat32Member(blob, member, index, value, isNull, isChanged);
        return new com.saabgroup.safir.dob.typesystem.si32.SquareMeterContainer(value[0],isNull[0],isChanged[0]);
    }


    /**
     * Set a Steradian32 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setSteradian32(com.saabgroup.safir.dob.typesystem.si32.SteradianContainer value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetFloat32MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a Steradian32 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static com.saabgroup.safir.dob.typesystem.si32.SteradianContainer getSteradian32(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetFloat32Member(blob, member, index, value, isNull, isChanged);
        return new com.saabgroup.safir.dob.typesystem.si32.SteradianContainer(value[0],isNull[0],isChanged[0]);
    }


    /**
     * Set a Volt32 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setVolt32(com.saabgroup.safir.dob.typesystem.si32.VoltContainer value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetFloat32MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a Volt32 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static com.saabgroup.safir.dob.typesystem.si32.VoltContainer getVolt32(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetFloat32Member(blob, member, index, value, isNull, isChanged);
        return new com.saabgroup.safir.dob.typesystem.si32.VoltContainer(value[0],isNull[0],isChanged[0]);
    }


    /**
     * Set a Watt32 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setWatt32(com.saabgroup.safir.dob.typesystem.si32.WattContainer value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetFloat32MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a Watt32 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static com.saabgroup.safir.dob.typesystem.si32.WattContainer getWatt32(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetFloat32Member(blob, member, index, value, isNull, isChanged);
        return new com.saabgroup.safir.dob.typesystem.si32.WattContainer(value[0],isNull[0],isChanged[0]);
    }




    /**
     * Set a Ampere64 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setAmpere64(com.saabgroup.safir.dob.typesystem.si64.AmpereContainer value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetFloat64MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a Ampere64 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static com.saabgroup.safir.dob.typesystem.si64.AmpereContainer getAmpere64(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetFloat64Member(blob, member, index, value, isNull, isChanged);
        return new AmpereContainer(value[0],isNull[0],isChanged[0]);
    }


    /**
     * Set a CubicMeter64 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setCubicMeter64(com.saabgroup.safir.dob.typesystem.si64.CubicMeterContainer value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetFloat64MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a CubicMeter64 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static com.saabgroup.safir.dob.typesystem.si64.CubicMeterContainer getCubicMeter64(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetFloat64Member(blob, member, index, value, isNull, isChanged);
        return new com.saabgroup.safir.dob.typesystem.si64.CubicMeterContainer(value[0],isNull[0],isChanged[0]);
    }


    /**
     * Set a Hertz64 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setHertz64(com.saabgroup.safir.dob.typesystem.si64.HertzContainer value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetFloat64MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a Hertz64 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static com.saabgroup.safir.dob.typesystem.si64.HertzContainer getHertz64(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetFloat64Member(blob, member, index, value, isNull, isChanged);
        return new com.saabgroup.safir.dob.typesystem.si64.HertzContainer(value[0],isNull[0],isChanged[0]);
    }


    /**
     * Set a Joule64 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setJoule64(com.saabgroup.safir.dob.typesystem.si64.JouleContainer value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetFloat64MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a Joule64 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static com.saabgroup.safir.dob.typesystem.si64.JouleContainer getJoule64(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetFloat64Member(blob, member, index, value, isNull, isChanged);
        return new com.saabgroup.safir.dob.typesystem.si64.JouleContainer(value[0],isNull[0],isChanged[0]);
    }


    /**
     * Set a Kelvin64 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setKelvin64(com.saabgroup.safir.dob.typesystem.si64.KelvinContainer value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetFloat64MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a Kelvin64 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static com.saabgroup.safir.dob.typesystem.si64.KelvinContainer getKelvin64(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetFloat64Member(blob, member, index, value, isNull, isChanged);
        return new com.saabgroup.safir.dob.typesystem.si64.KelvinContainer(value[0],isNull[0],isChanged[0]);
    }


    /**
     * Set a Kilogram64 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setKilogram64(com.saabgroup.safir.dob.typesystem.si64.KilogramContainer value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetFloat64MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a Kilogram64 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static com.saabgroup.safir.dob.typesystem.si64.KilogramContainer getKilogram64(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetFloat64Member(blob, member, index, value, isNull, isChanged);
        return new com.saabgroup.safir.dob.typesystem.si64.KilogramContainer(value[0],isNull[0],isChanged[0]);
    }


    /**
     * Set a Meter64 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setMeter64(com.saabgroup.safir.dob.typesystem.si64.MeterContainer value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetFloat64MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a Meter64 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static com.saabgroup.safir.dob.typesystem.si64.MeterContainer getMeter64(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetFloat64Member(blob, member, index, value, isNull, isChanged);
        return new com.saabgroup.safir.dob.typesystem.si64.MeterContainer(value[0],isNull[0],isChanged[0]);
    }


    /**
     * Set a MeterPerSecond64 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setMeterPerSecond64(com.saabgroup.safir.dob.typesystem.si64.MeterPerSecondContainer value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetFloat64MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a MeterPerSecond64 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static com.saabgroup.safir.dob.typesystem.si64.MeterPerSecondContainer getMeterPerSecond64(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetFloat64Member(blob, member, index, value, isNull, isChanged);
        return new com.saabgroup.safir.dob.typesystem.si64.MeterPerSecondContainer(value[0],isNull[0],isChanged[0]);
    }


    /**
     * Set a MeterPerSecondSquared64 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setMeterPerSecondSquared64(com.saabgroup.safir.dob.typesystem.si64.MeterPerSecondSquaredContainer value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetFloat64MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a MeterPerSecondSquared64 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static com.saabgroup.safir.dob.typesystem.si64.MeterPerSecondSquaredContainer getMeterPerSecondSquared64(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetFloat64Member(blob, member, index, value, isNull, isChanged);
        return new com.saabgroup.safir.dob.typesystem.si64.MeterPerSecondSquaredContainer(value[0],isNull[0],isChanged[0]);
    }


    /**
     * Set a Newton64 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setNewton64(com.saabgroup.safir.dob.typesystem.si64.NewtonContainer value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetFloat64MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a Newton64 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static com.saabgroup.safir.dob.typesystem.si64.NewtonContainer getNewton64(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetFloat64Member(blob, member, index, value, isNull, isChanged);
        return new com.saabgroup.safir.dob.typesystem.si64.NewtonContainer(value[0],isNull[0],isChanged[0]);
    }


    /**
     * Set a Pascal64 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setPascal64(com.saabgroup.safir.dob.typesystem.si64.PascalContainer value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetFloat64MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a Pascal64 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static com.saabgroup.safir.dob.typesystem.si64.PascalContainer getPascal64(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetFloat64Member(blob, member, index, value, isNull, isChanged);
        return new com.saabgroup.safir.dob.typesystem.si64.PascalContainer(value[0],isNull[0],isChanged[0]);
    }


    /**
     * Set a Radian64 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setRadian64(com.saabgroup.safir.dob.typesystem.si64.RadianContainer value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetFloat64MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a Radian64 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static com.saabgroup.safir.dob.typesystem.si64.RadianContainer getRadian64(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetFloat64Member(blob, member, index, value, isNull, isChanged);
        return new com.saabgroup.safir.dob.typesystem.si64.RadianContainer(value[0],isNull[0],isChanged[0]);
    }


    /**
     * Set a RadianPerSecond64 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setRadianPerSecond64(com.saabgroup.safir.dob.typesystem.si64.RadianPerSecondContainer value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetFloat64MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a RadianPerSecond64 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static com.saabgroup.safir.dob.typesystem.si64.RadianPerSecondContainer getRadianPerSecond64(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetFloat64Member(blob, member, index, value, isNull, isChanged);
        return new com.saabgroup.safir.dob.typesystem.si64.RadianPerSecondContainer(value[0],isNull[0],isChanged[0]);
    }


    /**
     * Set a RadianPerSecondSquared64 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setRadianPerSecondSquared64(com.saabgroup.safir.dob.typesystem.si64.RadianPerSecondSquaredContainer value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetFloat64MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a RadianPerSecondSquared64 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static com.saabgroup.safir.dob.typesystem.si64.RadianPerSecondSquaredContainer getRadianPerSecondSquared64(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetFloat64Member(blob, member, index, value, isNull, isChanged);
        return new com.saabgroup.safir.dob.typesystem.si64.RadianPerSecondSquaredContainer(value[0],isNull[0],isChanged[0]);
    }


    /**
     * Set a Second64 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setSecond64(com.saabgroup.safir.dob.typesystem.si64.SecondContainer value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetFloat64MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a Second64 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static com.saabgroup.safir.dob.typesystem.si64.SecondContainer getSecond64(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetFloat64Member(blob, member, index, value, isNull, isChanged);
        return new com.saabgroup.safir.dob.typesystem.si64.SecondContainer(value[0],isNull[0],isChanged[0]);
    }


    /**
     * Set a SquareMeter64 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setSquareMeter64(com.saabgroup.safir.dob.typesystem.si64.SquareMeterContainer value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetFloat64MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a SquareMeter64 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static com.saabgroup.safir.dob.typesystem.si64.SquareMeterContainer getSquareMeter64(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetFloat64Member(blob, member, index, value, isNull, isChanged);
        return new com.saabgroup.safir.dob.typesystem.si64.SquareMeterContainer(value[0],isNull[0],isChanged[0]);
    }


    /**
     * Set a Steradian64 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setSteradian64(com.saabgroup.safir.dob.typesystem.si64.SteradianContainer value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetFloat64MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a Steradian64 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static com.saabgroup.safir.dob.typesystem.si64.SteradianContainer getSteradian64(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetFloat64Member(blob, member, index, value, isNull, isChanged);
        return new com.saabgroup.safir.dob.typesystem.si64.SteradianContainer(value[0],isNull[0],isChanged[0]);
    }


    /**
     * Set a Volt64 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setVolt64(com.saabgroup.safir.dob.typesystem.si64.VoltContainer value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetFloat64MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a Volt64 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static com.saabgroup.safir.dob.typesystem.si64.VoltContainer getVolt64(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetFloat64Member(blob, member, index, value, isNull, isChanged);
        return new com.saabgroup.safir.dob.typesystem.si64.VoltContainer(value[0],isNull[0],isChanged[0]);
    }


    /**
     * Set a Watt64 in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param value The container whose values to use.
     * @param blob Blob to set the member in.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     */
    public static void setWatt64(com.saabgroup.safir.dob.typesystem.si64.WattContainer value,
                                java.nio.ByteBuffer blob,
                                int member,
                                int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        Kernel.SetFloat64MemberInPreallocated
                                (value.m_value,
                                 value.m_isNull,
                                 value.m_isChanged,
                                 blob,
                                 member,
                                 index);
    }

    /**
     * Get a Watt64 from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     * @return The value and associated flags.
     */
    public static com.saabgroup.safir.dob.typesystem.si64.WattContainer getWatt64(java.nio.ByteBuffer blob,
                                          int member,
                                          int index)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.GetFloat64Member(blob, member, index, value, isNull, isChanged);
        return new com.saabgroup.safir.dob.typesystem.si64.WattContainer(value[0],isNull[0],isChanged[0]);
    }

    /**
     * Set an Object in a blob.
     *
     * If the container is null then the member will be set to null in the blob.
     * The change flag from the container will be set in the blob.
     *
     * @param container The container whose values to use.
     * @param blob Blob to set the member in.
     * @param beginningOfUnused Beginning of unused part of dynamic part of blob.
     * @param member The member to be set.
     * @param index Array index in member to set. Shall be 0 if the member is not an array.
     * @return The new value for beginningOfUnused.
     */
    public static int setObject(ObjectContainerBase container,
                                 java.nio.ByteBuffer blob,
                                 int beginningOfUnused,
                                 int member,
                                 int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }

        if (!container.isNull())
        {
            int childBlobSize = container.getObjInternal().calculateBlobSize();
            blob.position(beginningOfUnused);
            java.nio.ByteBuffer childBlob = blob.slice();
            childBlob.limit(childBlobSize);

            int beginningOfUnusedInOut [] = new int [1];
            beginningOfUnusedInOut[0] = beginningOfUnused;
            Kernel.CreateObjectMember(blob,
                                      childBlobSize,
                                      container.getObjInternal().getTypeId(),
                                      member,
                                      index,
                                      container.isChangedHere(),
                                      beginningOfUnusedInOut);

            int childBeginningOfUnused = beginningOfUnusedInOut[0] - beginningOfUnused;
            childBeginningOfUnused = container.getObjInternal().writeToBlob(childBlob, childBeginningOfUnused);

            beginningOfUnused = beginningOfUnused + childBeginningOfUnused;
        }
        else if (container.isChangedHere())
        {
            setNull(blob,member,index);
        }
        return beginningOfUnused;
    }

    /**
     * Get an Object from a blob.
     *
     * This method will get the member and the associated isNull and isChange values from a blob and
     * return them as a container.
     *
     * NOTE THAT THIS METHOD HAS AN OUT PARAMETER. It was just too difficult to do it any other way...
     *
     * @param objectContainer OUT PARAMETER! The container to put the values into.
     * @param blob Blob to get the member from.
     * @param member The member to get.
     * @param index Array index in member to get. Shall be 0 if the member is not an array.
     */
    public static void getObject(ObjectContainerImpl<?> objectContainer,
                                 java.nio.ByteBuffer blob,
                                 int member,
                                 int index) {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }


        java.nio.ByteBuffer childBlob [] = new java.nio.ByteBuffer[1];
        boolean childIsNull [] = new boolean [1];
        boolean childIsChanged [] = new boolean [1];
        Kernel.GetObjectMember(blob, member, index, childBlob, childIsNull, childIsChanged);
        objectContainer.m_isChanged = childIsChanged[0];

        if (childIsNull[0]){
            objectContainer.setObjInternal(null);
        }
        else {
            objectContainer.setObjInternal(ObjectFactory.getInstance().createObject(childBlob[0]));
        }
    }






}
