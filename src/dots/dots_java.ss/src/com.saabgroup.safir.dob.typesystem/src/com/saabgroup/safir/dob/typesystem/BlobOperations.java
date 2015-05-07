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

    public static final int KEY_MODE = 0;
    public static final int VALUE_MODE = 1;

    /// <summary>
    /// Writes to BLOB.
    /// </summary>
    /// <returns>The to BLOB.</returns>
    /// <param name="obj">Object.</param>
    public static java.nio.ByteBuffer writeToBlob(com.saabgroup.safir.dob.typesystem.Object obj)
    {
        long handle = Kernel.CreateBlobWriter(obj.getTypeId());
        obj.writeToBlob (handle);
        int size = Kernel.CalculateBlobSize (handle);
        java.nio.ByteBuffer blob=java.nio.ByteBuffer.allocateDirect(size);
        Kernel.WriteBlob(handle, blob);
        Kernel.DeleteBlobWriter(handle);
        blob.clear(); //reset read position
        return blob;
    }

    /**
     * Extract the TypeId from a blob
     *
     * @param blob [in] - The blob to read from.
     * @return The TypeId from the blob.
     */
    public static long getTypeId(java.nio.ByteBuffer blob){
        return Kernel.GetTypeId(blob);
    }

    /**
     * Get the size of the blob contained by this object
     *
     * @param blob [in] - the blob.
     * @return the size of the blob, in bytes.
     */
    public static int getSize(java.nio.ByteBuffer blob){

        return Kernel.GetSize(blob);
    }

    /**
     * Get the number of values contained in a member.
     *
     * @return The number of member values.
     */
    public static int numberOfMemberValues(long handle, int member)
    {
        return Kernel.GetNumberOfMemberValues (handle, member);
    }

    public static boolean readTopLevelChangeFlag(long handle, int member)
    {
        return Kernel.ReadTopLevelChangeFlag(handle,member);
    }

    public static void writeTopLevelChangeFlag(long handle, int member, boolean isChanged)
    {
        Kernel.WriteTopLevelChangeFlag(handle,member,isChanged);
    }

    //**********************************************************************
    // Get/Set containers
    //**********************************************************************
    //boolean
    public static void get(BooleanContainer container, long handle, int member, int index)
    {
        boolean value[] = new boolean[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadBooleanMember(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }

    public static boolean getBoolean(long handle, int member, int index, int keyValMode) {
        boolean value[] = new boolean[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadBooleanMember(handle, value, isNull, isChanged, member, index, keyValMode);
        return value[0];
    }

    public static void set(BooleanContainer container, long handle, int member, int index)
    {
        Kernel.WriteBooleanMember(handle,
                container.m_value,
                container.m_isNull,
                container.m_isChanged,
                member,
                index,
                VALUE_MODE);
    }

    public static void set(boolean value, long handle, int member, int index, int keyValMode)
    {
        Kernel.WriteBooleanMember(handle, value, false, false, member, index, keyValMode);
    }

    //enum
    public static void get(EnumerationContainerBase<?> enumContainer, long handle, int member, int index)
    {
        int value[] = new int[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];
        Kernel.ReadInt32Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        enumContainer.m_Value = value[0];
        enumContainer.m_bIsNull = isNull[0];
        enumContainer.m_isChanged = isChanged[0];
    }

    public static void set(EnumerationContainerBase<?> container, long handle, int member, int index)
    {
        Kernel.WriteInt32Member(handle,
                container.m_Value,
                container.m_bIsNull,
                container.m_isChanged,
                member,
                index,
                VALUE_MODE);
    }

    //int32
    public static void get(Int32Container container, long handle, int member, int index)
    {
        int value[] = new int[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadInt32Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }

    public static int getInt32(long handle, int member, int index, int keyValMode)
    {
        int value[] = new int[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadInt32Member(handle, value, isNull, isChanged, member, index, keyValMode);
        return value[0];
    }

    public static void set(Int32Container container, long handle, int member, int index)
    {
        Kernel.WriteInt32Member(handle,
                container.m_value,
                container.m_isNull,
                container.m_isChanged,
                member,
                index,
                VALUE_MODE);
    }

    public static void set(int val, long handle, int member, int index, int keyValMode)
    {
        Kernel.WriteInt32Member(handle, val, false, false, member, index, keyValMode);
    }

    //int64
    public static void get(Int64Container container, long handle, int member, int index)
    {
        long value[] = new long[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadInt64Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }

    public static long getInt64(long handle, int member, int index, int keyValMode)
    {
        long value[] = new long[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadInt64Member(handle, value, isNull, isChanged, member, index, keyValMode);
        return value[0];
    }

    public static void set(Int64Container container, long handle, int member, int index)
    {
        Kernel.WriteInt64Member(handle,
                container.m_value,
                container.m_isNull,
                container.m_isChanged,
                member,
                index,
                VALUE_MODE);
    }

    public static void set(long val, long handle, int member, int index, int keyValMode)
    {
        Kernel.WriteInt64Member(handle, val, false, false, member, index, keyValMode);
    }

    //float32
    public static void get(Float32Container container, long handle, int member, int index)
    {
        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat32Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }

    public static float getFloat32(long handle, int member, int index, int keyValMode)
    {
        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat32Member(handle, value, isNull, isChanged, member, index, keyValMode);
        return value[0];
    }

    public static void set(Float32Container container, long handle, int member, int index)
    {
        Kernel.WriteFloat32Member(handle,
                container.m_value,
                container.m_isNull,
                container.m_isChanged,
                member,
                index,
                VALUE_MODE);
    }

    public static void set(float value, long handle, int member, int index, int keyValMode)
    {
        Kernel.WriteFloat32Member(handle, value, false, false, member, index, keyValMode);
    }

    //float64
    public static void get(Float64Container container, long handle, int member, int index)
    {
        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat64Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }

    public static double getFloat64(long handle, int member, int index, int keyValMode)
    {
        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat64Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        return value[0];
    }

    public static void set(Float64Container container, long handle, int member, int index)
    {
        Kernel.WriteFloat64Member(handle,
                container.m_value,
                container.m_isNull,
                container.m_isChanged,
                member,
                index,
                VALUE_MODE);
    }

    public static void set(double val, long handle, int member, int index, int keyValMode)
    {
        Kernel.WriteFloat64Member(handle, val, false, false, member, index, keyValMode);
    }

    //typeId
    public static void get(TypeIdContainer container, long handle, int member, int index)
    {
        long value[] = new long[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadInt64Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }

    public static long getTypeId(long handle, int member, int index, int keyValMode)
    {
        long value[] = new long[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadInt64Member(handle, value, isNull, isChanged, member, index, keyValMode);
        return value[0];
    }

    public static void set(TypeIdContainer container, long handle, int member, int index)
    {
        Kernel.WriteInt64Member(handle,
                container.m_value,
                container.m_isNull,
                container.m_isChanged,
                member,
                index,
                VALUE_MODE);
    }

    //instanceId
    public static void get(InstanceIdContainer container, long handle, int member, int index)
    {
        long hashVal[] = new long[1];
        String[] strVal=new String[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadHashedMember(handle, hashVal, strVal, isNull, isChanged, member, index, VALUE_MODE);

        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];

        if (!container.m_isNull) {
            if (!isNullOrEmpty(strVal[0])) {
                container.m_value=new InstanceId(hashVal[0],strVal[0]);
            }
            else {
                container.m_value=new InstanceId(hashVal[0]);
            }
        }
    }

    public static InstanceId getInstanceId(long handle, int member, int index, int keyValMode)
    {
        long hashVal[] = new long[1];
        String[] strVal=new String[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadHashedMember(handle, hashVal, strVal, isNull, isChanged, member, index, keyValMode);

        if (!isNullOrEmpty(strVal[0])) {
            return new InstanceId(hashVal[0],strVal[0]);
        }
        else {
            return new InstanceId(hashVal[0]);
        }
    }

    public static void set(InstanceIdContainer container, long handle, int member, int index)
    {
        if (container.m_isNull) {
            Kernel.WriteHashedMember(handle, 0, null, container.m_isNull, container.m_isChanged, member, index, VALUE_MODE);
            return;
        }

        if (isNullOrEmpty(container.m_value.getRawString())){
            Kernel.WriteHashedMember(handle, container.m_value.getRawValue(), null, container.m_isNull, container.m_isChanged, member, index, VALUE_MODE);
        }
        else{
            Kernel.WriteHashedMember(handle, container.m_value.getRawValue(), container.m_value.getRawString(), container.m_isNull, container.m_isChanged, member, index, VALUE_MODE);
        }
    }

    public static void set(InstanceId val, long handle, int member, int index, int keyValMode)
    {
        if (isNullOrEmpty(val.getRawString())){
            Kernel.WriteHashedMember(handle, val.getRawValue(), null, false, false, member, index, keyValMode);
        }
        else{
            Kernel.WriteHashedMember(handle, val.getRawValue(), val.getRawString(), false, false, member, index, keyValMode);
        }
    }

    //handlerId
    public static void get(HandlerIdContainer container, long handle, int member, int index)
    {
        long hashVal[] = new long[1];
        String[] strVal=new String[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadHashedMember(handle, hashVal, strVal, isNull, isChanged, member, index, VALUE_MODE);

        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];

        if (!container.m_isNull) {
            if (!isNullOrEmpty(strVal[0])) {
                container.m_value=new HandlerId(hashVal[0],strVal[0]);
            }
            else {
                container.m_value=new HandlerId(hashVal[0]);
            }
        }
    }

    public static HandlerId getHandlerId(long handle, int member, int index, int keyValMode)
    {
        long hashVal[] = new long[1];
        String[] strVal=new String[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadHashedMember(handle, hashVal, strVal, isNull, isChanged, member, index, keyValMode);

        if (!isNullOrEmpty(strVal[0])) {
            return new HandlerId(hashVal[0],strVal[0]);
        }
        else {
            return new HandlerId(hashVal[0]);
        }
    }

    public static void set(HandlerIdContainer container, long handle, int member, int index)
    {
        if (container.m_isNull) {
            Kernel.WriteHashedMember(handle, 0, null, container.m_isNull, container.m_isChanged, member, index, VALUE_MODE);
            return;
        }

        if (isNullOrEmpty(container.m_value.getRawString())){
            Kernel.WriteHashedMember(handle, container.m_value.getRawValue(), null, container.m_isNull, container.m_isChanged, member, index, VALUE_MODE);
        }
        else{
            Kernel.WriteHashedMember(handle, container.m_value.getRawValue(), container.m_value.getRawString(), container.m_isNull, container.m_isChanged, member, index, VALUE_MODE);
        }
    }

    public static void set(HandlerId val, long handle, int member, int index, int keyValMode)
    {
        if (isNullOrEmpty(val.getRawString())){
            Kernel.WriteHashedMember(handle, val.getRawValue(), null, false, false, member, index, keyValMode);
        }
        else{
            Kernel.WriteHashedMember(handle, val.getRawValue(), val.getRawString(), false, false, member, index, keyValMode);
        }
    }

    //channelId
    public static void get(ChannelIdContainer container, long handle, int member, int index)
    {
        long hashVal[] = new long[1];
        String[] strVal=new String[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadHashedMember(handle, hashVal, strVal, isNull, isChanged, member, index, VALUE_MODE);

        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];

        if (!container.m_isNull) {
            if (!isNullOrEmpty(strVal[0])) {
                container.m_value=new ChannelId(hashVal[0],strVal[0]);
            }
            else {
                container.m_value=new ChannelId(hashVal[0]);
            }
        }
    }

    public static ChannelId getChannelId(long handle, int member, int index, int keyValMode)
    {
        long hashVal[] = new long[1];
        String[] strVal=new String[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadHashedMember(handle, hashVal, strVal, isNull, isChanged, member, index, keyValMode);

        if (!isNullOrEmpty(strVal[0])) {
            return new ChannelId(hashVal[0],strVal[0]);
        }
        else {
            return new ChannelId(hashVal[0]);
        }
    }

    public static void set(ChannelIdContainer container, long handle, int member, int index)
    {
        if (container.m_isNull) {
            Kernel.WriteHashedMember(handle, 0, null, container.m_isNull, container.m_isChanged, member, index, VALUE_MODE);
            return;
        }

        if (isNullOrEmpty(container.m_value.getRawString())){
            Kernel.WriteHashedMember(handle, container.m_value.getRawValue(), null, container.m_isNull, container.m_isChanged, member, index, VALUE_MODE);
        }
        else{
            Kernel.WriteHashedMember(handle, container.m_value.getRawValue(), container.m_value.getRawString(), container.m_isNull, container.m_isChanged, member, index, VALUE_MODE);
        }
    }

    public static void set(ChannelId val, long handle, int member, int index, int keyValMode)
    {
        if (isNullOrEmpty(val.getRawString())){
            Kernel.WriteHashedMember(handle, val.getRawValue(), null, false, false, member, index, keyValMode);
        }
        else{
            Kernel.WriteHashedMember(handle, val.getRawValue(), val.getRawString(), false, false, member, index, keyValMode);
        }
    }

    //entityId
    public static void get(EntityIdContainer container, long handle, int member, int index)
    {
        long typeVal[] = new long[1];
        long hashVal[] = new long[1];
        String[] strVal=new String[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadEntityIdMember(handle, typeVal, hashVal, strVal, isNull, isChanged, member, index, VALUE_MODE);

        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];

        if (!container.m_isNull) {
            if (!isNullOrEmpty(strVal[0])) {
                container.m_value=new EntityId(typeVal[0], new InstanceId(hashVal[0],strVal[0]));
            }
            else {
                container.m_value=new EntityId(typeVal[0], new InstanceId(hashVal[0]));
            }
        }
    }

    public static EntityId getEntityId(long handle, int member, int index, int keyValMode)
    {
        long typeVal[] = new long[1];
        long hashVal[] = new long[1];
        String[] strVal=new String[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadEntityIdMember(handle, typeVal, hashVal, strVal, isNull, isChanged, member, index, keyValMode);

       if (!isNullOrEmpty(strVal[0])) {
            return new EntityId(typeVal[0], new InstanceId(hashVal[0],strVal[0]));
        }
        else {
            return new EntityId(typeVal[0], new InstanceId(hashVal[0]));
        }
    }

    public static void set(EntityIdContainer container, long handle, int member, int index)
    {
        if (container.m_isNull) {
            Kernel.WriteEntityIdMember(handle, 0, 0, null, container.m_isNull, container.m_isChanged, member, index, VALUE_MODE);
            return;
        }

        if (isNullOrEmpty(container.m_value.getInstanceId().getRawString())){
            Kernel.WriteEntityIdMember(handle,
                            container.m_value.getTypeId(),
                            container.m_value.getInstanceId().getRawValue(),
                            null,
                            container.m_isNull,
                            container.m_isChanged,
                            member,
                            index,
                            VALUE_MODE);
        }
        else{
            Kernel.WriteEntityIdMember(handle,
                            container.m_value.getTypeId(),
                            container.m_value.getInstanceId().getRawValue(),
                            container.m_value.getInstanceId().getRawString(),
                            container.m_isNull,
                            container.m_isChanged,
                            member,
                            index,
                            VALUE_MODE);
        }
    }

    public static void set(EntityId val, long handle, int member, int index, int keyValMode)
    {
        if (isNullOrEmpty(val.getInstanceId().getRawString())){
            Kernel.WriteEntityIdMember(handle,
                            val.getTypeId(),
                            val.getInstanceId().getRawValue(),
                            null,
                            false,
                            false,
                            member,
                            index,
                            keyValMode);
        }
        else{
            Kernel.WriteEntityIdMember(handle,
                            val.getTypeId(),
                            val.getInstanceId().getRawValue(),
                            val.getInstanceId().getRawString(),
                            false,
                            false,
                            member,
                            index,
                            keyValMode);
        }
    }

    //string
    public static void get(StringContainer container, long handle, int member, int index)
    {
        String value[] = new String[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadStringMember(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
        if (!container.m_isNull)
            container.m_value=value[0];
        else
            container.m_value=null;
    }

    public static String getString(long handle, int member, int index, int keyValMode)
    {
        String value[] = new String[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadStringMember(handle, value, isNull, isChanged, member, index, keyValMode);
        return value[0];
    }

    public static void set(StringContainer container, long handle, int member, int index)
    {
        Kernel.WriteStringMember(handle,
                container.m_value,
                container.m_isNull,
                container.m_isChanged,
                member,
                index,
                VALUE_MODE);
    }

    public static void set(String val, long handle, int member, int index, int keyValMode)
    {
        Kernel.WriteStringMember(handle, val, false, false, member, index, keyValMode);
    }

    //Object
    public static void get(ObjectContainerImpl<?> container, long handle, int member, int index)
    {
        java.nio.ByteBuffer value[] = new java.nio.ByteBuffer[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadObjectMember(handle, value, isNull, isChanged, member, index, VALUE_MODE);

        if (!isNull[0]) {
            container.setObjInternal(ObjectFactory.getInstance().createObject(value[0]));
        }
        else {
            container.setObjInternal(null);
        }

        container.m_isChanged=isChanged[0];
    }

    public static Object getObject(long handle, int member, int index, int keyValMode)
    {
        java.nio.ByteBuffer value[] = new java.nio.ByteBuffer[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadObjectMember(handle, value, isNull, isChanged, member, index, keyValMode);

        if (!isNull[0]) {
            return ObjectFactory.getInstance().createObject(value[0]);
        }
        else {
            return null;
        }
    }

    public static void set(ObjectContainerImpl<?> container, long handle, int member, int index)
    {
        if (container.isNull()) {
            Kernel.WriteObjectMember(handle,
                            null,
                            true,
                            container.m_isChanged,
                            member,
                            index,
                            VALUE_MODE);
        }
        else {
            java.nio.ByteBuffer blob=writeToBlob(container.getObjInternal());
            Kernel.WriteObjectMember(handle,
                            blob,
                            false,
                            container.m_isChanged,
                            member,
                            index,
                            VALUE_MODE);
        }
    }

    public static void set(Object obj, long handle, int member, int index, int keyValMode)
    {
        if (obj==null) {
            throw new SoftwareViolationException("Value not allowed to be null");
        }

        java.nio.ByteBuffer blob=writeToBlob(obj);
        Kernel.WriteObjectMember(handle, blob, false, false, member, index, keyValMode);
    }


    //binary
    public static void get(BinaryContainer container, long handle, int member, int index)
    {
        java.nio.ByteBuffer value[] = new java.nio.ByteBuffer[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadBinaryMember(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];

        if (!container.m_isNull) {
            container.m_value = new byte[value[0].capacity()];
            value[0].clear(); //reset position
            value[0].get(container.m_value);
        }
        else {
            container.m_value = null;
        }


    }

    public static Byte[] getBinary(long handle, int member, int index, int keyValMode)
    {
        java.nio.ByteBuffer value[] = new java.nio.ByteBuffer[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadBinaryMember(handle, value, isNull, isChanged, member, index, keyValMode);

        byte[] bytes = new byte[value[0].capacity()];
        value[0].clear(); //reset position
        value[0].get(bytes);

        Byte[] byteObjects = new Byte[bytes.length];
        for (int i=0; i<bytes.length; i++) {
            byteObjects[i] = bytes[i];
        }

        return byteObjects;
    }

    public static void set(BinaryContainer container, long handle, int member, int index)
    {
        if (container.m_isNull || container.m_value.length == 0) {
            Kernel.WriteBinaryMember(handle,
                            null, 0,
                            container.m_isNull,
                            container.m_isChanged,
                            member,
                            index,
                            VALUE_MODE);
        }
        else {
            java.nio.ByteBuffer bin=java.nio.ByteBuffer.allocateDirect(container.m_value.length);
            bin.put(container.m_value);
            Kernel.WriteBinaryMember(handle,
                            bin,
                            container.m_value.length,
                            container.m_isNull,
                            container.m_isChanged,
                            member,
                            index,
                            VALUE_MODE);
        }
    }

    public static void set(byte[] val, long handle, int member, int index, int keyValMode)
    {
        if (val==null)
            throw new SoftwareViolationException("Value not allowed to be null");

        java.nio.ByteBuffer bin=java.nio.ByteBuffer.allocateDirect(val.length);
            bin.put(val);
            Kernel.WriteBinaryMember(handle, bin, val.length, false, false, member, index, keyValMode);
    }

    public static void set(Byte[] val, long handle, int member, int index, int keyValMode)
    {
        if (val==null)
            throw new SoftwareViolationException("Value not allowed to be null");

        java.nio.ByteBuffer bin=java.nio.ByteBuffer.allocateDirect(val.length);
        for (Byte b : val) {
            bin.put(b.byteValue());
        }

        Kernel.WriteBinaryMember(handle, bin, val.length, false, false, member, index, keyValMode);
    }

    //si32
    public static void get(com.saabgroup.safir.dob.typesystem.si32.AmpereContainer container, long handle, int member, int index)
    {
        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat32Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }
    public static void set(com.saabgroup.safir.dob.typesystem.si32.AmpereContainer container, long handle, int member, int index)
    {
        Kernel.WriteFloat32Member(handle,
                        container.m_value,
                        container.m_isNull,
                        container.m_isChanged,
                        member,
                        index,
                        VALUE_MODE);
    }

    public static void get(com.saabgroup.safir.dob.typesystem.si32.CubicMeterContainer container, long handle, int member, int index)
    {
        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat32Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }
    public static void set(com.saabgroup.safir.dob.typesystem.si32.CubicMeterContainer container, long handle, int member, int index)
    {
        Kernel.WriteFloat32Member(handle,
                        container.m_value,
                        container.m_isNull,
                        container.m_isChanged,
                        member,
                        index,
                        VALUE_MODE);
    }

    public static void get(com.saabgroup.safir.dob.typesystem.si32.HertzContainer container, long handle, int member, int index)
    {
        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat32Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }
    public static void set(com.saabgroup.safir.dob.typesystem.si32.HertzContainer container, long handle, int member, int index)
    {
        Kernel.WriteFloat32Member(handle,
                        container.m_value,
                        container.m_isNull,
                        container.m_isChanged,
                        member,
                        index,
                        VALUE_MODE);
    }

    public static void get(com.saabgroup.safir.dob.typesystem.si32.JouleContainer container, long handle, int member, int index)
    {
        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat32Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }
    public static void set(com.saabgroup.safir.dob.typesystem.si32.JouleContainer container, long handle, int member, int index)
    {
        Kernel.WriteFloat32Member(handle,
                        container.m_value,
                        container.m_isNull,
                        container.m_isChanged,
                        member,
                        index,
                        VALUE_MODE);
    }

    public static void get(com.saabgroup.safir.dob.typesystem.si32.KelvinContainer container, long handle, int member, int index)
    {
        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat32Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }
    public static void set(com.saabgroup.safir.dob.typesystem.si32.KelvinContainer container, long handle, int member, int index)
    {
        Kernel.WriteFloat32Member(handle,
                        container.m_value,
                        container.m_isNull,
                        container.m_isChanged,
                        member,
                        index,
                        VALUE_MODE);
    }

    public static void get(com.saabgroup.safir.dob.typesystem.si32.KilogramContainer container, long handle, int member, int index)
    {
        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat32Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }
    public static void set(com.saabgroup.safir.dob.typesystem.si32.KilogramContainer container, long handle, int member, int index)
    {
        Kernel.WriteFloat32Member(handle,
                        container.m_value,
                        container.m_isNull,
                        container.m_isChanged,
                        member,
                        index,
                        VALUE_MODE);
    }

    public static void get(com.saabgroup.safir.dob.typesystem.si32.MeterContainer container, long handle, int member, int index)
    {
        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat32Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }
    public static void set(com.saabgroup.safir.dob.typesystem.si32.MeterContainer container, long handle, int member, int index)
    {
        Kernel.WriteFloat32Member(handle,
                        container.m_value,
                        container.m_isNull,
                        container.m_isChanged,
                        member,
                        index,
                        VALUE_MODE);
    }

    public static void get(com.saabgroup.safir.dob.typesystem.si32.MeterPerSecondContainer container, long handle, int member, int index)
    {
        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat32Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }
    public static void set(com.saabgroup.safir.dob.typesystem.si32.MeterPerSecondContainer container, long handle, int member, int index)
    {
        Kernel.WriteFloat32Member(handle,
                        container.m_value,
                        container.m_isNull,
                        container.m_isChanged,
                        member,
                        index,
                        VALUE_MODE);
    }

    public static void get(com.saabgroup.safir.dob.typesystem.si32.MeterPerSecondSquaredContainer container, long handle, int member, int index)
    {
        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat32Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }
    public static void set(com.saabgroup.safir.dob.typesystem.si32.MeterPerSecondSquaredContainer container, long handle, int member, int index)
    {
        Kernel.WriteFloat32Member(handle,
                        container.m_value,
                        container.m_isNull,
                        container.m_isChanged,
                        member,
                        index,
                        VALUE_MODE);
    }

    public static void get(com.saabgroup.safir.dob.typesystem.si32.NewtonContainer container, long handle, int member, int index)
    {
        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat32Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }
    public static void set(com.saabgroup.safir.dob.typesystem.si32.NewtonContainer container, long handle, int member, int index)
    {
        Kernel.WriteFloat32Member(handle,
                        container.m_value,
                        container.m_isNull,
                        container.m_isChanged,
                        member,
                        index,
                        VALUE_MODE);
    }

    public static void get(com.saabgroup.safir.dob.typesystem.si32.PascalContainer container, long handle, int member, int index)
    {
        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat32Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }
    public static void set(com.saabgroup.safir.dob.typesystem.si32.PascalContainer container, long handle, int member, int index)
    {
        Kernel.WriteFloat32Member(handle,
                        container.m_value,
                        container.m_isNull,
                        container.m_isChanged,
                        member,
                        index,
                        VALUE_MODE);
    }

    public static void get(com.saabgroup.safir.dob.typesystem.si32.RadianContainer container, long handle, int member, int index)
    {
        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat32Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }
    public static void set(com.saabgroup.safir.dob.typesystem.si32.RadianContainer container, long handle, int member, int index)
    {
        Kernel.WriteFloat32Member(handle,
                        container.m_value,
                        container.m_isNull,
                        container.m_isChanged,
                        member,
                        index,
                        VALUE_MODE);
    }

    public static void get(com.saabgroup.safir.dob.typesystem.si32.RadianPerSecondContainer container, long handle, int member, int index)
    {
        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat32Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }
    public static void set(com.saabgroup.safir.dob.typesystem.si32.RadianPerSecondContainer container, long handle, int member, int index)
    {
        Kernel.WriteFloat32Member(handle,
                        container.m_value,
                        container.m_isNull,
                        container.m_isChanged,
                        member,
                        index,
                        VALUE_MODE);
    }

    public static void get(com.saabgroup.safir.dob.typesystem.si32.RadianPerSecondSquaredContainer container, long handle, int member, int index)
    {
        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat32Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }
    public static void set(com.saabgroup.safir.dob.typesystem.si32.RadianPerSecondSquaredContainer container, long handle, int member, int index)
    {
        Kernel.WriteFloat32Member(handle,
                        container.m_value,
                        container.m_isNull,
                        container.m_isChanged,
                        member,
                        index,
                        VALUE_MODE);
    }

    public static void get(com.saabgroup.safir.dob.typesystem.si32.SecondContainer container, long handle, int member, int index)
    {
        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat32Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }
    public static void set(com.saabgroup.safir.dob.typesystem.si32.SecondContainer container, long handle, int member, int index)
    {
        Kernel.WriteFloat32Member(handle,
                        container.m_value,
                        container.m_isNull,
                        container.m_isChanged,
                        member,
                        index,
                        VALUE_MODE);
    }

    public static void get(com.saabgroup.safir.dob.typesystem.si32.SquareMeterContainer container, long handle, int member, int index)
    {
        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat32Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }
    public static void set(com.saabgroup.safir.dob.typesystem.si32.SquareMeterContainer container, long handle, int member, int index)
    {
        Kernel.WriteFloat32Member(handle,
                        container.m_value,
                        container.m_isNull,
                        container.m_isChanged,
                        member,
                        index,
                        VALUE_MODE);
    }

    public static void get(com.saabgroup.safir.dob.typesystem.si32.SteradianContainer container, long handle, int member, int index)
    {
        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat32Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }
    public static void set(com.saabgroup.safir.dob.typesystem.si32.SteradianContainer container, long handle, int member, int index)
    {
        Kernel.WriteFloat32Member(handle,
                        container.m_value,
                        container.m_isNull,
                        container.m_isChanged,
                        member,
                        index,
                        VALUE_MODE);
    }

    public static void get(com.saabgroup.safir.dob.typesystem.si32.VoltContainer container, long handle, int member, int index)
    {
        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat32Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }
    public static void set(com.saabgroup.safir.dob.typesystem.si32.VoltContainer container, long handle, int member, int index)
    {
        Kernel.WriteFloat32Member(handle,
                        container.m_value,
                        container.m_isNull,
                        container.m_isChanged,
                        member,
                        index,
                        VALUE_MODE);
    }

    public static void get(com.saabgroup.safir.dob.typesystem.si32.WattContainer container, long handle, int member, int index)
    {
        float value[] = new float[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat32Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }
    public static void set(com.saabgroup.safir.dob.typesystem.si32.WattContainer container, long handle, int member, int index)
    {
        Kernel.WriteFloat32Member(handle,
                        container.m_value,
                        container.m_isNull,
                        container.m_isChanged,
                        member,
                        index,
                        VALUE_MODE);
    }

    //si64
    public static void get(com.saabgroup.safir.dob.typesystem.si64.AmpereContainer container, long handle, int member, int index)
    {
        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat64Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }
    public static void set(com.saabgroup.safir.dob.typesystem.si64.AmpereContainer container, long handle, int member, int index)
    {
        Kernel.WriteFloat64Member(handle,
                        container.m_value,
                        container.m_isNull,
                        container.m_isChanged,
                        member,
                        index,
                        VALUE_MODE);
    }

    public static void get(com.saabgroup.safir.dob.typesystem.si64.CubicMeterContainer container, long handle, int member, int index)
    {
        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat64Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }
    public static void set(com.saabgroup.safir.dob.typesystem.si64.CubicMeterContainer container, long handle, int member, int index)
    {
        Kernel.WriteFloat64Member(handle,
                        container.m_value,
                        container.m_isNull,
                        container.m_isChanged,
                        member,
                        index,
                        VALUE_MODE);
    }

    public static void get(com.saabgroup.safir.dob.typesystem.si64.HertzContainer container, long handle, int member, int index)
    {
        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat64Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }
    public static void set(com.saabgroup.safir.dob.typesystem.si64.HertzContainer container, long handle, int member, int index)
    {
        Kernel.WriteFloat64Member(handle,
                        container.m_value,
                        container.m_isNull,
                        container.m_isChanged,
                        member,
                        index,
                        VALUE_MODE);
    }

    public static void get(com.saabgroup.safir.dob.typesystem.si64.JouleContainer container, long handle, int member, int index)
    {
        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat64Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }
    public static void set(com.saabgroup.safir.dob.typesystem.si64.JouleContainer container, long handle, int member, int index)
    {
        Kernel.WriteFloat64Member(handle,
                        container.m_value,
                        container.m_isNull,
                        container.m_isChanged,
                        member,
                        index,
                        VALUE_MODE);
    }

    public static void get(com.saabgroup.safir.dob.typesystem.si64.KelvinContainer container, long handle, int member, int index)
    {
        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat64Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }
    public static void set(com.saabgroup.safir.dob.typesystem.si64.KelvinContainer container, long handle, int member, int index)
    {
        Kernel.WriteFloat64Member(handle,
                        container.m_value,
                        container.m_isNull,
                        container.m_isChanged,
                        member,
                        index,
                        VALUE_MODE);
    }

    public static void get(com.saabgroup.safir.dob.typesystem.si64.KilogramContainer container, long handle, int member, int index)
    {
        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat64Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }
    public static void set(com.saabgroup.safir.dob.typesystem.si64.KilogramContainer container, long handle, int member, int index)
    {
        Kernel.WriteFloat64Member(handle,
                        container.m_value,
                        container.m_isNull,
                        container.m_isChanged,
                        member,
                        index,
                        VALUE_MODE);
    }

    public static void get(com.saabgroup.safir.dob.typesystem.si64.MeterContainer container, long handle, int member, int index)
    {
        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat64Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }
    public static void set(com.saabgroup.safir.dob.typesystem.si64.MeterContainer container, long handle, int member, int index)
    {
        Kernel.WriteFloat64Member(handle,
                        container.m_value,
                        container.m_isNull,
                        container.m_isChanged,
                        member,
                        index,
                        VALUE_MODE);
    }

    public static void get(com.saabgroup.safir.dob.typesystem.si64.MeterPerSecondContainer container, long handle, int member, int index)
    {
        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat64Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }
    public static void set(com.saabgroup.safir.dob.typesystem.si64.MeterPerSecondContainer container, long handle, int member, int index)
    {
        Kernel.WriteFloat64Member(handle,
                        container.m_value,
                        container.m_isNull,
                        container.m_isChanged,
                        member,
                        index,
                        VALUE_MODE);
    }

    public static void get(com.saabgroup.safir.dob.typesystem.si64.MeterPerSecondSquaredContainer container, long handle, int member, int index)
    {
        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat64Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }
    public static void set(com.saabgroup.safir.dob.typesystem.si64.MeterPerSecondSquaredContainer container, long handle, int member, int index)
    {
        Kernel.WriteFloat64Member(handle,
                        container.m_value,
                        container.m_isNull,
                        container.m_isChanged,
                        member,
                        index,
                        VALUE_MODE);
    }

    public static void get(com.saabgroup.safir.dob.typesystem.si64.NewtonContainer container, long handle, int member, int index)
    {
        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat64Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }
    public static void set(com.saabgroup.safir.dob.typesystem.si64.NewtonContainer container, long handle, int member, int index)
    {
        Kernel.WriteFloat64Member(handle,
                        container.m_value,
                        container.m_isNull,
                        container.m_isChanged,
                        member,
                        index,
                        VALUE_MODE);
    }

    public static void get(com.saabgroup.safir.dob.typesystem.si64.PascalContainer container, long handle, int member, int index)
    {
        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat64Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }
    public static void set(com.saabgroup.safir.dob.typesystem.si64.PascalContainer container, long handle, int member, int index)
    {
        Kernel.WriteFloat64Member(handle,
                        container.m_value,
                        container.m_isNull,
                        container.m_isChanged,
                        member,
                        index,
                        VALUE_MODE);
    }

    public static void get(com.saabgroup.safir.dob.typesystem.si64.RadianContainer container, long handle, int member, int index)
    {
        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat64Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }
    public static void set(com.saabgroup.safir.dob.typesystem.si64.RadianContainer container, long handle, int member, int index)
    {
        Kernel.WriteFloat64Member(handle,
                        container.m_value,
                        container.m_isNull,
                        container.m_isChanged,
                        member,
                        index,
                        VALUE_MODE);
    }

    public static void get(com.saabgroup.safir.dob.typesystem.si64.RadianPerSecondContainer container, long handle, int member, int index)
    {
        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat64Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }
    public static void set(com.saabgroup.safir.dob.typesystem.si64.RadianPerSecondContainer container, long handle, int member, int index)
    {
        Kernel.WriteFloat64Member(handle,
                        container.m_value,
                        container.m_isNull,
                        container.m_isChanged,
                        member,
                        index,
                        VALUE_MODE);
    }

    public static void get(com.saabgroup.safir.dob.typesystem.si64.RadianPerSecondSquaredContainer container, long handle, int member, int index)
    {
        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat64Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }
    public static void set(com.saabgroup.safir.dob.typesystem.si64.RadianPerSecondSquaredContainer container, long handle, int member, int index)
    {
        Kernel.WriteFloat64Member(handle,
                        container.m_value,
                        container.m_isNull,
                        container.m_isChanged,
                        member,
                        index,
                        VALUE_MODE);
    }

    public static void get(com.saabgroup.safir.dob.typesystem.si64.SecondContainer container, long handle, int member, int index)
    {
        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat64Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }
    public static void set(com.saabgroup.safir.dob.typesystem.si64.SecondContainer container, long handle, int member, int index)
    {
        Kernel.WriteFloat64Member(handle,
                        container.m_value,
                        container.m_isNull,
                        container.m_isChanged,
                        member,
                        index,
                        VALUE_MODE);
    }

    public static void get(com.saabgroup.safir.dob.typesystem.si64.SquareMeterContainer container, long handle, int member, int index)
    {
        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat64Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }
    public static void set(com.saabgroup.safir.dob.typesystem.si64.SquareMeterContainer container, long handle, int member, int index)
    {
        Kernel.WriteFloat64Member(handle,
                        container.m_value,
                        container.m_isNull,
                        container.m_isChanged,
                        member,
                        index,
                        VALUE_MODE);
    }

    public static void get(com.saabgroup.safir.dob.typesystem.si64.SteradianContainer container, long handle, int member, int index)
    {
        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat64Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }
    public static void set(com.saabgroup.safir.dob.typesystem.si64.SteradianContainer container, long handle, int member, int index)
    {
        Kernel.WriteFloat64Member(handle,
                        container.m_value,
                        container.m_isNull,
                        container.m_isChanged,
                        member,
                        index,
                        VALUE_MODE);
    }

    public static void get(com.saabgroup.safir.dob.typesystem.si64.VoltContainer container, long handle, int member, int index)
    {
        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat64Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }
    public static void set(com.saabgroup.safir.dob.typesystem.si64.VoltContainer container, long handle, int member, int index)
    {
        Kernel.WriteFloat64Member(handle,
                        container.m_value,
                        container.m_isNull,
                        container.m_isChanged,
                        member,
                        index,
                        VALUE_MODE);
    }

    public static void get(com.saabgroup.safir.dob.typesystem.si64.WattContainer container, long handle, int member, int index)
    {
        double value[] = new double[1];
        boolean isNull[] = new boolean[1];
        boolean isChanged[] = new boolean[1];

        Kernel.ReadFloat64Member(handle, value, isNull, isChanged, member, index, VALUE_MODE);
        container.m_value=value[0];
        container.m_isNull=isNull[0];
        container.m_isChanged=isChanged[0];
    }
    public static void set(com.saabgroup.safir.dob.typesystem.si64.WattContainer container, long handle, int member, int index)
    {
        Kernel.WriteFloat64Member(handle,
                        container.m_value,
                        container.m_isNull,
                        container.m_isChanged,
                        member,
                        index,
                        VALUE_MODE);
    }

    private static boolean isNullOrEmpty(String str) {
        return str == null || str.isEmpty();
    }

}
