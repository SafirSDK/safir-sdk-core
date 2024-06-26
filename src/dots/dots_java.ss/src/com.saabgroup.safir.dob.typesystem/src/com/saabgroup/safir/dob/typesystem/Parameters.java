// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2005-2013, 2024 (http://safirsdkcore.com)
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
 * Functions for getting parameter information from types.
 *
 * With these operations you can get parameter values from types.
 * You can also get information about the parameters in a type, such as
 * parameter names and indexes, TypeIds of parameters etc.
 */
public class Parameters {
    private Parameters() {}

    /**
     * Get the number of parameters defined in a class.
     *
     * @param typeId [in] - TypeId of class.
     * @return The number of parameters.
     * @throws IllegalValueException There is no such type defined.
     */
    public static int getNumberOfParameters(long typeId)
    {
        int result = Kernel.GetNumberOfParameters(typeId);
        if (result == -1)
        {
            throw new IllegalValueException("There is no such type or parameter defined");
        }
        else
        {
            return result;
        }
    }

    /**
     * Gets index of a named parameter.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameterName [in] - Name of parameter.
     * @return index of the named parameter.
     * @throws IllegalValueException There is no such type or parameter defined.
     */
    public static int getIndex(long typeId, String parameterName)
    {
        int result = Kernel.GetParameterId(typeId, parameterName);
        if (result == -1)
        {
            String typeName = Kernel.GetTypeName(typeId);
            if (typeName != null)
            {
                throw new IllegalValueException("The class '" + typeName + "' does not have a parameter called '"  + parameterName + "'!");
            }

            throw new IllegalValueException("Trying to read parameter '"  + parameterName + "' from a class that does not exist. TypeId=" + typeId);
        }
        else
        {
            return result;
        }
    }

    /**
     * Get the name of the specified parameter as it was defined in the xml description.
     *
     * If the parameter does not exist the returned value is undefined. Use
     * #GetIndex to get a valid ParameterIndex, which is guaranteed to exist.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @return The name of the parameter.
     */
    public static String getName(long typeId, int parameter)
    {
        int[] memberType=new int[1];
        int[] keyType=new int[1];
        String[] parameterName=new String[1];
        long[] complexTypeId=new long[1];
        long[] keyTypeId=new long[1];
        int[] collectionType=new int[1];
        int[] numberOfValues=new int[1];
        Kernel.GetParameterInfo(typeId, parameter, memberType, keyType, parameterName, complexTypeId, keyTypeId, collectionType, numberOfValues);

        return parameterName[0];
    }

    /**
     * Gets a string representation of the type of a parameter.
     *
     * If the parameter is not an object or enumeration the result is undefined.
     *
     * If the parameter does not exist the returned value is undefined. Use
     * #GetIndex to get a valid ParameterIndex, which is guaranteed to exist.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @return Name of the parameter type.
     */
    public static String getTypeName(long typeId, int parameter)
    {
        int[] memberType=new int[1];
        int[] keyType=new int[1];
        String[] parameterName=new String[1];
        long[] complexTypeId=new long[1];
        long[] keyTypeId=new long[1];
        int[] collectionType=new int[1];
        int[] numberOfValues=new int[1];
        Kernel.GetParameterInfo(typeId, parameter, memberType, keyType, parameterName, complexTypeId, keyTypeId, collectionType, numberOfValues);

        MemberType mt=MemberType.values()[memberType[0]];
        if (mt==MemberType.OBJECT_MEMBER_TYPE || mt==MemberType.ENUMERATION_MEMBER_TYPE)
        {
            return Kernel.GetTypeName(complexTypeId[0]);
        }
        else
        {
            return Kernel.MemberTypeName(memberType[0]);
        }
    }

    /**
     * Get the type of a parameter or the value type of a dictionary parameter.
     *
     * If the parameter does not exist the returned value is undefined. Use
     * #GetIndex to get a valid ParameterIndex, which is guaranteed to exist.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @return The type of the parameter.
     */
    public static MemberType getType(long typeId, int parameter)
    {
        int[] memberType=new int[1];
        int[] keyType=new int[1];
        String[] parameterName=new String[1];
        long[] complexTypeId=new long[1];
        long[] keyTypeId=new long[1];
        int[] collectionType=new int[1];
        int[] numberOfValues=new int[1];
        Kernel.GetParameterInfo(typeId, parameter, memberType, keyType, parameterName, complexTypeId, keyTypeId, collectionType, numberOfValues);

        return MemberType.values()[memberType[0]];
    }

    /**
     * Get the key type of a dictionary parameter.
     *
     * If the parameter is not a dictionary the result is undefined.
     *
     * If the parameter does not exist the returned value is undefined. Use
     * #GetIndex to get a valid ParameterIndex, which is guaranteed to exist.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @return The type of the parameter dictionary key.
     */
    public static MemberType getDictionaryKeyType(long typeId, int parameter)
    {
        int[] memberType=new int[1];
        int[] keyType=new int[1];
        String[] parameterName=new String[1];
        long[] complexTypeId=new long[1];
        long[] keyTypeId=new long[1];
        int[] collectionType=new int[1];
        int[] numberOfValues=new int[1];
        Kernel.GetParameterInfo(typeId, parameter, memberType, keyType, parameterName, complexTypeId, keyTypeId, collectionType, numberOfValues);

        return MemberType.values()[keyType[0]];
    }

    /**
     * Get the type id of a parameter.
     *
     * If parameterType is object or enumeration, this is the typeId of that type.
     * If parameterType is something else the value is -1. For dictionaries this is the type of the value.
     *
     * If the parameter does not exist the returned value is undefined. Use
     * #GetIndex to get a valid ParameterIndex, which is guaranteed to exist.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @return The type id of the parameter.
     */
    public static long getParameterTypeId(long typeId, int parameter)
    {
        int[] memberType=new int[1];
        int[] keyType=new int[1];
        String[] parameterName=new String[1];
        long[] complexTypeId=new long[1];
        long[] keyTypeId=new long[1];
        int[] collectionType=new int[1];
        int[] numberOfValues=new int[1];
        Kernel.GetParameterInfo(typeId, parameter, memberType, keyType, parameterName, complexTypeId, keyTypeId, collectionType, numberOfValues);

        return complexTypeId[0];
    }

    /**
     * Get the type id of a dictionary parameter key.
     *
     * If the parameter is a dictionary and the key type is an enumeration this is
     * the type id of the enumeration. Otherwise undefined.
     *
     * If the parameter does not exist the returned value is undefined. Use
     * #GetIndex to get a valid ParameterIndex, which is guaranteed to exist.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @return The type id of the parameter key.
     */
    public static long getDictionaryKeyTypeId(long typeId, int parameter)
    {
        int[] memberType=new int[1];
        int[] keyType=new int[1];
        String[] parameterName=new String[1];
        long[] complexTypeId=new long[1];
        long[] keyTypeId=new long[1];
        int[] collectionType=new int[1];
        int[] numberOfValues=new int[1];
        Kernel.GetParameterInfo(typeId, parameter, memberType, keyType, parameterName, complexTypeId, keyTypeId, collectionType, numberOfValues);

        return keyTypeId[0];
    }

    /**
     * Get the collection type of a parameter.
     *
     * The parameter collection type can be single value, array, sequence or dictionary.
     *
     * If the parameter does not exist the returned value is undefined. Use
     * #GetIndex to get a valid ParameterIndex, which is guaranteed to exist.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @return The collection type of the parameter.
     */
    public static CollectionType getCollectionType(long typeId, int parameter)
    {
        int[] memberType=new int[1];
        int[] keyType=new int[1];
        String[] parameterName=new String[1];
        long[] complexTypeId=new long[1];
        long[] keyTypeId=new long[1];
        int[] collectionType=new int[1];
        int[] numberOfValues=new int[1];
        Kernel.GetParameterInfo(typeId, parameter, memberType, keyType, parameterName, complexTypeId, keyTypeId, collectionType, numberOfValues);

        return CollectionType.values()[collectionType[0]];
    }

    /**
     * Get the array size of a parameter.
     *
     * This function is identical to GetCollectionSize, which is the new name for this function.
     *
     * If the parameter does not exist the returned value is undefined. Use
     * #GetIndex to get a valid ParameterIndex, which is guaranteed to exist.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @return The array size of the parameter, or 1 if it is not an array.
     */
    public static int getArraySize(long typeId, int parameter)
    {
        return getCollectionSize(typeId, parameter);
    }

    /**
     * Get the number of elements in a collection parameter.
     *
     * This function works on arrays, sequences and dictionaries.
     *
     * If the parameter does not exist the returned value is undefined. Use
     * #GetIndex to get a valid ParameterIndex, which is guaranteed to exist.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @return The size of the parameter, or 1 if it is not a collection.
     */
    public static int getCollectionSize(long typeId, int parameter)
    {
        int[] memberType=new int[1];
        int[] keyType=new int[1];
        String[] parameterName=new String[1];
        long[] complexTypeId=new long[1];
        long[] keyTypeId=new long[1];
        int[] collectionType=new int[1];
        int[] numberOfValues=new int[1];
        Kernel.GetParameterInfo(typeId, parameter, memberType, keyType, parameterName, complexTypeId, keyTypeId, collectionType, numberOfValues);

        return numberOfValues[0];
    }


    //************************************************************************************
    //* Functions for retrieval of parameters
    //************************************************************************************

    /**
     * Get a boolean parameter value.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Array index. If parameter is not an array this shall be 0.
     * @return Parameter value.
     */
    public static boolean getBoolean(long typeId, int parameter, int index)
    {
        boolean val [] = new boolean [1];
        Kernel.GetBooleanParameter(typeId, parameter, index, val);
        return val[0];
    }

    /**
     * Get an enumeration parameter value.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Array index. If parameter is not an array this shall be 0.
     * @return Parameter value.
     */
    public static int getEnumeration(long typeId, int parameter, int index)
    {
        int val[] = new int[1];
        Kernel.GetEnumerationParameter(typeId, parameter, index, BlobOperations.VALUE_MODE, val);
        return val[0];
    }

    /**
     * Get an Int32 parameter value.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Array index. If parameter is not an array this shall be 0.
     * @return Parameter value.
     */
    public static int getInt32(long typeId, int parameter, int index)
    {
        int val [] = new int [1];
        Kernel.GetInt32Parameter(typeId, parameter, index, BlobOperations.VALUE_MODE, val);
        return val[0];
    }

    /**
     * Get an Int64 parameter value.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Array index. If parameter is not an array this shall be 0.
     * @return Parameter value.
     */
    public static long getInt64(long typeId, int parameter, int index)
    {
        long val [] = new long [1];
        Kernel.GetInt64Parameter(typeId, parameter, index, BlobOperations.VALUE_MODE, val);
        return val[0];
    }

    /**
     * Get a Float32 parameter value.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Array index. If parameter is not an array this shall be 0.
     * @return Parameter value.
     */
    public static float getFloat32(long typeId, int parameter, int index)
    {
        float val [] = new float [1];
        Kernel.GetFloat32Parameter(typeId, parameter, index, val);
        return val[0];
    }

    /**
     * Get a Float64 parameter value.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Array index. If parameter is not an array this shall be 0.
     * @return Parameter value.
     */
    public static double getFloat64(long typeId, int parameter, int index)
    {
        double val [] = new double [1];
        Kernel.GetFloat64Parameter(typeId, parameter, index, val);
        return val[0];
    }

    /**
     * Get a string parameter value.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Array index. If parameter is not an array this shall be 0.
     * @return Parameter value.
     */
    public static String getString(long typeId, int parameter, int index)
    {
        String val [] = new String [1];
        Kernel.GetStringParameter(typeId, parameter, index, BlobOperations.VALUE_MODE, val);
        return val[0];
    }

    /**
     * Get a TypeId parameter value.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Array index. If parameter is not an array this shall be 0.
     * @return Parameter value.
     */
    public static long getTypeId(long typeId, int parameter, int index)
    {
        long val [] = new long [1];
        Kernel.GetTypeIdParameter(typeId, parameter, index, BlobOperations.VALUE_MODE, val);
        return val[0];
    }

    /**
     * Get a InstanceId parameter value.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Array index. If parameter is not an array this shall be 0.
     * @return Parameter value.
     */
    public static InstanceId getInstanceId(long typeId, int parameter, int index)
    {
        long hashVal[] = new long[1];
        String strVal[] = new String[1];

        Kernel.GetHashedIdParameter(typeId, parameter, index, BlobOperations.VALUE_MODE, hashVal, strVal);
        if (!isNullOrEmpty(strVal[0])) {
            return new InstanceId(hashVal[0], strVal[0]);
        }
        else {
            return new InstanceId(hashVal[0]);
        }
    }

    /**
     * Get an EntityId parameter value.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Array index. If parameter is not an array this shall be 0.
     * @return Parameter value.
     */
    public static EntityId getEntityId(long typeId, int parameter, int index)
    {
        long typeIdOut[] = new long [1];
        long instanceId[] = new long [1];
        String strVal[] = new String[1];
        Kernel.GetEntityIdParameter(typeId, parameter, index, BlobOperations.VALUE_MODE, typeIdOut, instanceId , strVal);
        if (!isNullOrEmpty(strVal[0])) {
            return new EntityId(typeIdOut[0], new InstanceId(instanceId[0], strVal[0]));
        }
        else {
            return new EntityId(typeIdOut[0], new InstanceId(instanceId[0]));
        }
    }

    /**
     * Get a ChannelId parameter value.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Array index. If parameter is not an array this shall be 0.
     * @return Parameter value.
     */
    public static ChannelId getChannelId(long typeId, int parameter, int index)
    {
        long hashVal[] = new long[1];
        String strVal[] = new String[1];

        Kernel.GetHashedIdParameter(typeId, parameter, index, BlobOperations.VALUE_MODE, hashVal, strVal);
        if (!isNullOrEmpty(strVal[0])) {
            return new ChannelId(hashVal[0], strVal[0]);
        }
        else {
            return new ChannelId(hashVal[0]);
        }
    }

    /**
     * Get a HandlerId parameter value.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Array index. If parameter is not an array this shall be 0.
     * @return Parameter value.
     */
    public static HandlerId getHandlerId(long typeId, int parameter, int index)
    {
        long hashVal[] = new long[1];
        String strVal[] = new String[1];

        Kernel.GetHashedIdParameter(typeId, parameter, index, BlobOperations.VALUE_MODE, hashVal, strVal);
        if (!isNullOrEmpty(strVal[0])) {
            return new HandlerId(hashVal[0], strVal[0]);
        }
        else {
            return new HandlerId(hashVal[0]);
        }
    }


    /**
     * Get an Object parameter value.
     *
     * This method will return a smart pointer to a new copy of the parameter.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Array index. If parameter is not an array this shall be 0.
     * @return Parameter value.
     */
    public static com.saabgroup.safir.dob.typesystem.Object getObject(long typeId, int parameter, int index)
    {
        java.nio.ByteBuffer blob [] = new java.nio.ByteBuffer[1];
        Kernel.GetObjectParameter(typeId, parameter, index, blob);
        com.saabgroup.safir.dob.typesystem.Object obj=ObjectFactory.getInstance().createObject(blob[0]);
        obj.setChanged(false);
        return obj;
    }

    /**
     * Get a Binary parameter value.
     *
     * This method will retrieve a pointer to the binary data and the number of bytes the binary data contains.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Array index. If parameter is not an array this shall be 0.
     * @return Parameter value.
     */
    public static byte[] getBinary(long typeId, int parameter, int index)
    {
        java.nio.ByteBuffer val [] = new java.nio.ByteBuffer [1];
        Kernel.GetBinaryParameter(typeId, parameter, index, val);
        byte[] result = new byte [val[0].capacity()];
        val[0].clear(); //reset position
        val[0].get(result);
        return result;
    }

    /**
     * Get the index of a Int32 dictionary key.
     *
     * The index can then be used with the Get-mehtods above to retrieve the parameter value.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param key [in] - Dictionary key.
     * @return Index or -1 if key does not exist.
     */
    public static int dictionaryKeyToIndex(long typeId, int parameter, int key) {
        return Kernel.DictionaryInt32KeyToIndex(typeId, parameter, key);
    }

    /**
     * Get the index of a Int64 dictionary key.
     *
     * The index can then be used with the Get-mehtods above to retrieve the parameter value.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param key [in] - Dictionary key.
     * @return Index or -1 if key does not exist.
     */
    public static int dictionaryKeyToIndex(long typeId, int parameter, long key) {
        return Kernel.DictionaryInt64KeyToIndex(typeId, parameter, key);
    }

    /**
     * Get the index of a string dictionary key.
     *
     * The index can then be used with the Get-mehtods above to retrieve the parameter value.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param key [in] - Dictionary key.
     * @return Index or -1 if key does not exist.
     */
    public static int dictionaryKeyToIndex(long typeId, int parameter, String key) {
        return Kernel.DictionaryStringKeyToIndex(typeId, parameter, key);
    }

    /**
     * Get the index of a EntityId dictionary key.
     *
     * The index can then be used with the Get-mehtods above to retrieve the parameter value.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param key [in] - Dictionary key.
     * @return Index or -1 if key does not exist.
     */
    public static int dictionaryKeyToIndex(long typeId, int parameter, EntityId key) {
        return Kernel.DictionaryEntityIdKeyToIndex(typeId, parameter, key.getTypeId(), key.getInstanceId().getRawValue());
    }

    /**
     * Get the index of a InstanceId dictionary key.
     *
     * The index can then be used with the Get-mehtods above to retrieve the parameter value.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param key [in] - Dictionary key.
     * @return Index or -1 if key does not exist.
     */
    public static int dictionaryKeyToIndex(long typeId, int parameter, InstanceId key) {
        return Kernel.DictionaryInt64KeyToIndex(typeId, parameter, key.getRawValue());
    }

    /**
     * Get the index of a HandlerId dictionary key.
     *
     * The index can then be used with the Get-mehtods above to retrieve the parameter value.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param key [in] - Dictionary key.
     * @return Index or -1 if key does not exist.
     */
    public static int dictionaryKeyToIndex(long typeId, int parameter, HandlerId key) {
        return Kernel.DictionaryInt64KeyToIndex(typeId, parameter, key.getRawValue());
    }

    /**
     * Get the index of a ChannelId dictionary key.
     *
     * The index can then be used with the Get-mehtods above to retrieve the parameter value.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param key [in] - Dictionary key.
     * @return Index or -1 if key does not exist.
     */
    public static int dictionaryKeyToIndex(long typeId, int parameter, ChannelId key) {
        return Kernel.DictionaryInt64KeyToIndex(typeId, parameter, key.getRawValue());
    }

    /**
     * Get an enumeration dictionary key.
     *
     * Undefined behaviour if the parameter is not an appropriate dictionary.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Parameter dictionary index.
     * @return Parameter dictionary key.
     */
    public static int getEnumerationDictionaryKey(long typeId, int parameter, int index)
    {
        int val[] = new int[1];
        Kernel.GetEnumerationParameter(typeId, parameter, index, BlobOperations.KEY_MODE, val);
        return val[0];
    }

    /**
     * Get an Int32 dictionary key.
     *
     * Undefined behaviour if the parameter is not an appropriate dictionary.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Parameter dictionary index.
     * @return Parameter dictionary key.
     */
    public static int getInt32DictionaryKey(long typeId, int parameter, int index)
    {
        int val [] = new int [1];
        Kernel.GetInt32Parameter(typeId, parameter, index, BlobOperations.KEY_MODE, val);
        return val[0];
    }

    /**
     * Get an Int64 dictionary key.
     *
     * Undefined behaviour if the parameter is not an appropriate dictionary.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Parameter dictionary index.
     * @return Parameter dictionary key.
     */
    public static long getInt64DictionaryKey(long typeId, int parameter, int index)
    {
        long val [] = new long [1];
        Kernel.GetInt64Parameter(typeId, parameter, index, BlobOperations.KEY_MODE, val);
        return val[0];
    }


    /**
     * Get a string dictionary key.
     *
     * Undefined behaviour if the parameter is not an appropriate dictionary.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Parameter dictionary index.
     * @return Parameter dictionary key.
     */
    public static String getStringDictionaryKey(long typeId, int parameter, int index)
    {
        String val [] = new String [1];
        Kernel.GetStringParameter(typeId, parameter, index, BlobOperations.KEY_MODE, val);
        return val[0];
    }

    /**
     * Get a TypeId dictionary key.
     *
     * Undefined behaviour if the parameter is not an appropriate dictionary.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Parameter dictionary index.
     * @return Parameter dictionary key.
     */
    public static long getTypeIdDictionaryKey(long typeId, int parameter, int index)
    {
        long val [] = new long [1];
        Kernel.GetTypeIdParameter(typeId, parameter, index, BlobOperations.KEY_MODE, val);
        return val[0];
    }

    /**
     * Get a InstanceId dictionary key.
     *
     * Undefined behaviour if the parameter is not an appropriate dictionary.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Parameter dictionary index.
     * @return Parameter dictionary key.
     */
    public static InstanceId getInstanceIdDictionaryKey(long typeId, int parameter, int index)
    {
        long hashVal[] = new long[1];
        String strVal[] = new String[1];

        Kernel.GetHashedIdParameter(typeId, parameter, index, BlobOperations.KEY_MODE, hashVal, strVal);
        if (!isNullOrEmpty(strVal[0])) {
            return new InstanceId(hashVal[0], strVal[0]);
        }
        else {
            return new InstanceId(hashVal[0]);
        }
    }

    /**
     * Get an EntityId dictionary key.
     *
     * Undefined behaviour if the parameter is not an appropriate dictionary.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Parameter dictionary index.
     * @return Parameter dictionary key.
     */
    public static EntityId getEntityIdDictionaryKey(long typeId, int parameter, int index)
    {
        long typeIdOut[] = new long [1];
        long instanceId[] = new long [1];
        String strVal[] = new String[1];
        Kernel.GetEntityIdParameter(typeId, parameter, index, BlobOperations.KEY_MODE, typeIdOut, instanceId , strVal);
        if (!isNullOrEmpty(strVal[0])) {
            return new EntityId(typeIdOut[0], new InstanceId(instanceId[0], strVal[0]));
        }
        else {
            return new EntityId(typeIdOut[0], new InstanceId(instanceId[0]));
        }
    }

    /**
     * Get a ChannelId dictionary key.
     *
     * Undefined behaviour if the parameter is not an appropriate dictionary.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Parameter dictionary index.
     * @return Parameter dictionary key.
     */
    public static ChannelId getChannelIdDictionaryKey(long typeId, int parameter, int index)
    {
        long hashVal[] = new long[1];
        String strVal[] = new String[1];

        Kernel.GetHashedIdParameter(typeId, parameter, index, BlobOperations.KEY_MODE, hashVal, strVal);
        if (!isNullOrEmpty(strVal[0])) {
            return new ChannelId(hashVal[0], strVal[0]);
        }
        else {
            return new ChannelId(hashVal[0]);
        }
    }

    /**
     * Get a HandlerId dictionary key.
     *
     * Undefined behaviour if the parameter is not an appropriate dictionary.
     *
     * @param typeId [in] - TypeId of class.
     * @param parameter [in] - Index of parameter.
     * @param index [in] - Parameter dictionary index.
     * @return Parameter dictionary key.
     */
    public static HandlerId getHandlerIdDictionaryKey(long typeId, int parameter, int index)
    {
        long hashVal[] = new long[1];
        String strVal[] = new String[1];

        Kernel.GetHashedIdParameter(typeId, parameter, index, BlobOperations.KEY_MODE, hashVal, strVal);
        if (!isNullOrEmpty(strVal[0])) {
            return new HandlerId(hashVal[0], strVal[0]);
        }
        else {
            return new HandlerId(hashVal[0]);
        }
    }
    
    private static boolean isNullOrEmpty(String str) {
        return str == null || str.isEmpty();
    }

    
}
