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
 * Functions for getting parameter information from types.
 *
 * With these operations you can get parameter values from types.
 * You can also get information about the parameters in a type, such as
 * parameter names and indexes, TypeIds of parameters etc.
 */
public class Parameters {

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
            throw new IllegalValueException("There is no such type or parameter defined");
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
        return Kernel.GetParameterName(typeId, parameter);
    }

    /**
     * Get the type of a parameter.
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
        return MemberType.values()[Kernel.GetParameterType(typeId, parameter)];
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
        return Kernel.GetParameterTypeName(typeId, parameter);
    }

    /**
     * Get the array size of a parameter.
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
        return Kernel.GetParameterArraySize(typeId, parameter);
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
        Kernel.GetInt32Parameter(typeId, parameter, index, val);
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
        Kernel.GetInt32Parameter(typeId, parameter, index, val);
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
        Kernel.GetInt64Parameter(typeId, parameter, index, val);
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
        Kernel.GetStringParameter(typeId, parameter, index, val);
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
        Kernel.GetInt64Parameter(typeId, parameter, index, val);
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

        Kernel.GetHashedIdParameter(typeId, parameter, index, hashVal, strVal);
        if (strVal[0] != null) {
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
        Kernel.GetEntityIdParameter(typeId, parameter, index, typeIdOut, instanceId , strVal);
        if (strVal[0] != null) {
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

        Kernel.GetHashedIdParameter(typeId, parameter, index, hashVal, strVal);
        if (strVal[0] != null) {
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

        Kernel.GetHashedIdParameter(typeId, parameter, index, hashVal, strVal);
        if (strVal[0] != null) {
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
        return ObjectFactory.getInstance().createObject(blob[0]);
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
}
