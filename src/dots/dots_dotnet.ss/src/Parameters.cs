/* ****************************************************************************
*
* Copyright Saab AB, 2005-2015 (http://safir.sourceforge.net)
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
using Safir.Dob.Typesystem.Internal;
using System.Runtime.InteropServices;

namespace Safir.Dob.Typesystem
{
    /// <summary>
    /// Functions for getting parameter information from types.
    /// <para>
    /// With these operations you can get parameter values from types.
    /// You can also get information about the parameters in a type, such as
    /// parameter names and indexes, TypeIds of parameters etc.
    /// </para>
    /// </summary>
    public class Parameters
    {
        #region ParameterInfo
        //********************************************************
        //* Functions handling parameters
        //********************************************************

        /// <summary>
        /// Get the number of parameters defined in a class.
        /// </summary>
        /// <param name="typeId">TypeId of class.</param>
        /// <returns>The number of parameters.</returns>
        /// <exception cref="Safir.Dob.Typesystem.IllegalValueException">There is no such type defined.</exception>
        public static int GetNumberOfParameters(System.Int64 typeId)
        {
            return Kernel.DotsC_GetNumberOfParameters(typeId);
        }

        /// <summary>
        /// Gets index of a named parameter.
        /// </summary>
        /// <param name="typeId">TypeId of class.</param>
        /// <param name="parameterName">Name of parameter.</param>
        /// <returns>Index of given parameter.</returns>
        /// <exception cref="Safir.Dob.Typesystem.IllegalValueException">There is no such type or parameter defined.</exception>
        public static int GetIndex(System.Int64 typeId,
                                         string parameterName)
        {
            System.IntPtr sp = Internal.InternalOperations.CStringOf(parameterName);
            int constId = Kernel.DotsC_GetParameterId(typeId, sp);
            Marshal.FreeHGlobal(sp);
            if (constId == -1)
            {
                throw new IllegalValueException("There is no such type or parameter defined");
            }
            else
            {
                return constId;
            }
        }

        /// <summary>
        /// Get the name of the specified parameter as it was defined in the xml description.
        /// <para>
        /// If the parameter does not exist the returned value is undefined. Use
        /// #GetIndex to get a valid ParameterIndex, which is guaranteed to exist.
        /// </para>
        /// </summary>
        /// <param name="typeId">TypeId of class.</param>
        /// <param name="parameter">Index of parameter.</param>
        /// <returns>The name of the parameter.</returns>
        public static string GetName(System.Int64 typeId,
                                              int parameter)
        {
            MemberType memberType;
            IntPtr parameterName;
            Int64 complexTypeId;
            CollectionType collectionType;
            Int32 numberOfValues;
            Kernel.DotsC_GetParameterInfo (typeId, parameter, out memberType, out parameterName, out complexTypeId, out collectionType, out numberOfValues);

            return InternalOperations.StringOf (parameterName);
        }

        /// <summary>
        /// Gets a string representation of the type of a parameter.
        /// <para>
        /// If the parameter is not an object or enumeration the result is undefined.
        /// If the parameter does not exist the returned value is undefined. Use
        /// #GetIndex to get a valid ParameterIndex, which is guaranteed to exist.
        /// </para>
        /// </summary>
        /// <param name="typeId">TypeId of class.</param>
        /// <param name="parameter">Index of parameter.</param>
        /// <returns>Name of the parameter type.</returns>
        public static string GetTypeName(System.Int64 typeId,
                                         int parameter)
        {
            MemberType memberType;
            IntPtr parameterName;
            Int64 complexTypeId;
            CollectionType collectionType;
            Int32 numberOfValues;
            Kernel.DotsC_GetParameterInfo (typeId, parameter, out memberType, out parameterName, out complexTypeId, out collectionType, out numberOfValues);

            if (memberType==MemberType.ObjectMemberType || memberType==MemberType.EnumerationMemberType)
            {
                IntPtr typeName=Kernel.DotsC_GetTypeName (complexTypeId);
                return InternalOperations.StringOf (typeName);
            }
            else
            {
                IntPtr typeName = Kernel.DotsC_MemberTypeName (memberType);
                return InternalOperations.StringOf (typeName);
            }
        }

        /// <summary>
        /// Get the type of a parameter.
        /// <para>
        /// If the parameter does not exist the returned value is undefined. Use
        /// #GetIndex to get a valid ParameterIndex, which is guaranteed to exist.
        /// </para>
        /// </summary>
        /// <param name="typeId">TypeId of class.</param>
        /// <param name="parameter">Index of parameter.</param>
        /// <returns>The type of the parameter.</returns>
        public static MemberType GetType(System.Int64 typeId,
                                                             int parameter)
        {
            MemberType memberType;
            IntPtr parameterName;
            Int64 complexTypeId;
            CollectionType collectionType;
            Int32 numberOfValues;
            Kernel.DotsC_GetParameterInfo (typeId, parameter, out memberType, out parameterName, out complexTypeId, out collectionType, out numberOfValues);

            return memberType;
        }



        /// <summary>
        /// Get the array size of a parameter.
        /// <para>
        /// If the parameter does not exist the returned value is undefined. Use
        /// #GetIndex to get a valid ParameterIndex, which is guaranteed to exist.
        /// </para>
        /// </summary>
        /// <param name="typeId">TypeId of class.</param>
        /// <param name="parameter">Index of parameter.</param>
        /// <returns>The array size of the parameter, or 1 if it is not an array.</returns>
        public static int GetArraySize(System.Int64 typeId,
                                                int parameter)
        {
            MemberType memberType;
            IntPtr parameterName;
            Int64 complexTypeId;
            CollectionType collectionType;
            Int32 numberOfValues;
            Kernel.DotsC_GetParameterInfo (typeId, parameter, out memberType, out parameterName, out complexTypeId, out collectionType, out numberOfValues);

            return numberOfValues;
        }

        #endregion

        #region GetParameterValues
        //************************************************************************************
        //* Functions for retrieval of parameters
        //************************************************************************************

        /// <summary>
        /// Get a boolean parameter value.
        /// </summary>
        /// <param name="typeId">TypeId of class.</param>
        /// <param name="parameter">Index of parameter.</param>
        /// <param name="index">Array index. If parameter is not an array this shall be 0.</param>
        /// <returns>Parameter value.</returns>
        public static bool GetBoolean(System.Int64 typeId,
                                      int parameter,
                                      int index)
        {
            byte val;
            Kernel.DotsC_GetBooleanParameter(typeId, parameter, index, out val);
            return Internal.InternalOperations.BoolOf(val);
        }

        /// <summary>
        /// Get an enumeration parameter value.
        /// </summary>
        /// <param name="typeId">TypeId of class.</param>
        /// <param name="parameter">Index of parameter.</param>
        /// <param name="index">Array index. If parameter is not an array this shall be 0.</param>
        /// <returns>Parameter value.</returns>
        public static int GetEnumeration(System.Int64 typeId,
                                         int parameter,
                                         int index)
        {
            System.Int32 val;
            Kernel.DotsC_GetEnumerationParameter (typeId, parameter, index, KeyValMode.ValueMode, out val);

            return val;
        }

        /// <summary>
        /// Get an Int32 parameter value.
        /// </summary>
        /// <param name="typeId">TypeId of class.</param>
        /// <param name="parameter">Index of parameter.</param>
        /// <param name="index">Array index. If parameter is not an array this shall be 0.</param>
        /// <returns>Parameter value.</returns>
        public static int GetInt32(System.Int64 typeId,
                                   int parameter,
                                   int index)
        {
            System.Int32 val;
            Kernel.DotsC_GetInt32Parameter(typeId, parameter, index, KeyValMode.ValueMode, out val);
            return val;
        }

        /// <summary>
        /// Get an Int64 parameter value.
        /// </summary>
        /// <param name="typeId">TypeId of class.</param>
        /// <param name="parameter">Index of parameter.</param>
        /// <param name="index">Array index. If parameter is not an array this shall be 0.</param>
        /// <returns>Parameter value.</returns>
        public static long GetInt64(System.Int64 typeId,
                                             int parameter,
                                             int index)
        {
            System.Int64 val;
            Kernel.DotsC_GetInt64Parameter(typeId, parameter, index, KeyValMode.ValueMode, out val);
            return val;
        }

        /// <summary>
        /// Get a Float32 parameter value.
        /// </summary>
        /// <param name="typeId">TypeId of class.</param>
        /// <param name="parameter">Index of parameter.</param>
        /// <param name="index">Array index. If parameter is not an array this shall be 0.</param>
        /// <returns>Parameter value.</returns>
        public static float GetFloat32(System.Int64 typeId,
                                                int parameter,
                                                int index)
        {
            float val;
            Kernel.DotsC_GetFloat32Parameter(typeId, parameter, index, out val);
            return val;
        }


        /// <summary>
        /// Get a Float64 parameter value.
        /// </summary>
        /// <param name="typeId">TypeId of class.</param>
        /// <param name="parameter">Index of parameter.</param>
        /// <param name="index">Array index. If parameter is not an array this shall be 0.</param>
        /// <returns>Parameter value.</returns>
        public static double GetFloat64(System.Int64 typeId,
                                                 int parameter,
                                                 int index)
        {
            double val;
            Kernel.DotsC_GetFloat64Parameter(typeId, parameter, index, out val);
            return val;
        }

        /// <summary>
        /// Get a string parameter value.
        /// </summary>
        /// <param name="typeId">TypeId of class.</param>
        /// <param name="parameter">Index of parameter.</param>
        /// <param name="index">Array index. If parameter is not an array this shall be 0.</param>
        /// <returns>Parameter value.</returns>
        public static string GetString(System.Int64 typeId,
                                                int parameter,
                                                int index)
        {
            System.IntPtr sp;
            Kernel.DotsC_GetStringParameter(typeId, parameter, index, KeyValMode.ValueMode, out sp);
            return Internal.InternalOperations.StringOf(sp);
        }

        /// <summary>
        /// Get a TypeId parameter value.
        /// </summary>
        /// <param name="typeId">TypeId of class.</param>
        /// <param name="parameter">Index of parameter.</param>
        /// <param name="index">Array index. If parameter is not an array this shall be 0.</param>
        /// <returns>Parameter value.</returns>
        public static System.Int64 GetTypeId(System.Int64 typeId,
                                                      int parameter,
                                                      int index)
        {
            System.Int64 val;
            Kernel.DotsC_GetTypeIdParameter(typeId, parameter, index, KeyValMode.ValueMode, out val);
            return val;
        }

        /// <summary>
        /// Get an InstanceId parameter value.
        /// </summary>
        /// <param name="typeId">TypeId of class.</param>
        /// <param name="parameter">Index of parameter.</param>
        /// <param name="index">Array index. If parameter is not an array this shall be 0.</param>
        /// <returns>Parameter value.</returns>
        public static InstanceId GetInstanceId(System.Int64 typeId,
                                                 int parameter,
                                                 int index)
        {
            System.Int64 hashVal;
            System.IntPtr strVal;
            Kernel.DotsC_GetHashedIdParameter(typeId, parameter, index, KeyValMode.ValueMode, out hashVal, out strVal);
            if (strVal == System.IntPtr.Zero)
            {
                return new InstanceId(hashVal);
            }
            else
            {
                return new InstanceId(hashVal, Internal.InternalOperations.StringOf(strVal));
            }
        }

        /// <summary>
        /// Get an EntityId parameter value.
        /// </summary>
        /// <param name="typeId">TypeId of class.</param>
        /// <param name="parameter">Index of parameter.</param>
        /// <param name="index">Array index. If parameter is not an array this shall be 0.</param>
        /// <returns>Parameter value.</returns>
        public static Dob.Typesystem.EntityId GetEntityId(System.Int64 typeId,
                                                          int parameter,
                                                          int index)
        {
            Dob.Typesystem.Internal.DotsC_EntityId eid;
            System.IntPtr strVal;
            Kernel.DotsC_GetEntityIdParameter(typeId, parameter, index, KeyValMode.ValueMode, out eid, out strVal);
            if (strVal == System.IntPtr.Zero)
            {
                return new EntityId(eid.TypeId, new Dob.Typesystem.InstanceId(eid.InstanceId));
            }
            else
            {
                return new EntityId(eid.TypeId, new Dob.Typesystem.InstanceId
                    (eid.InstanceId, Internal.InternalOperations.StringOf(strVal)));
            }
        }

        /// <summary>
        /// Get a ChannelId parameter value.
        /// </summary>
        /// <param name="typeId">TypeId of class.</param>
        /// <param name="parameter">Index of parameter.</param>
        /// <param name="index">Array index. If parameter is not an array this shall be 0.</param>
        /// <returns>Parameter value.</returns>
        public static ChannelId GetChannelId(System.Int64 typeId,
                                                 int parameter,
                                                 int index)
        {
            System.Int64 hashVal;
            System.IntPtr strVal;
            Kernel.DotsC_GetHashedIdParameter(typeId, parameter, index, KeyValMode.ValueMode, out hashVal, out strVal);
            if (strVal == System.IntPtr.Zero)
            {
                return new ChannelId(hashVal);
            }
            else
            {
                return new ChannelId(hashVal, Internal.InternalOperations.StringOf(strVal));
            }
        }

        /// <summary>
        /// Get a handlerId parameter value.
        /// </summary>
        /// <param name="typeId">TypeId of class.</param>
        /// <param name="parameter">Index of parameter.</param>
        /// <param name="index">Array index. If parameter is not an array this shall be 0.</param>
        /// <returns>Parameter value.</returns>
        public static HandlerId GetHandlerId(System.Int64 typeId,
                                                 int parameter,
                                                 int index)
        {
            System.Int64 hashVal;
            System.IntPtr strVal;
            Kernel.DotsC_GetHashedIdParameter(typeId, parameter, index, KeyValMode.ValueMode, out hashVal, out strVal);
            if (strVal == System.IntPtr.Zero)
            {
                return new HandlerId(hashVal);
            }
            else
            {
                return new HandlerId(hashVal, Internal.InternalOperations.StringOf(strVal));
            }
        }

        /// <summary>
        /// Get an Object parameter value.
        /// </summary>
        /// <param name="typeId">TypeId of class.</param>
        /// <param name="parameter">Index of parameter.</param>
        /// <param name="index">Array index. If parameter is not an array this shall be 0.</param>
        /// <returns>Parameter value.</returns>
        public static Dob.Typesystem.Object GetObject(System.Int64 typeId,
                                                      int parameter,
                                                      int index)
        {
            IntPtr blob;
            Kernel.DotsC_GetObjectParameter(typeId, parameter, index, out blob);
            Object obj = ObjectFactory.Instance.CreateObject(blob);
            obj.SetChanged(false);
            return obj;
        }

        /// <summary>
        /// Get a Binary parameter value.
        /// </summary>
        /// <param name="typeId">TypeId of class.</param>
        /// <param name="parameter">Index of parameter.</param>
        /// <param name="index">Array index. If parameter is not an array this shall be 0.</param>
        /// <returns>Parameter value.</returns>
        public static byte[] GetBinary( System.Int64 typeId,
                                        int parameter,
                                        int index)
        {
            IntPtr binPtr;
            System.Int32 size;
            Kernel.DotsC_GetBinaryParameter(typeId, parameter, index, out binPtr, out size);
            byte[] binary = new byte[size];
            Marshal.Copy(binPtr, binary, 0, size);
            return binary;
        }

        /// <summary>
        /// Dictionaries the index of the key to.
        /// </summary>
        /// <returns>The key to index.</returns>
        /// <param name="typeId">Type identifier.</param>
        /// <param name="parameter">Parameter.</param>
        /// <param name="key">Key.</param>
        public static Int32 DictionaryKeyToIndex(Int64 typeId, Int32 parameter, Int32 key)
        {
            return Kernel.DotsC_DictionaryInt32KeyToIndex (typeId, parameter, key);
        }

        /// <summary>
        /// Dictionaries the index of the key to.
        /// </summary>
        /// <returns>The key to index.</returns>
        /// <param name="typeId">Type identifier.</param>
        /// <param name="parameter">Parameter.</param>
        /// <param name="key">Key.</param>
        public static Int32 DictionaryKeyToIndex(Int64 typeId, Int32 parameter, Int64 key)
        {
            return Kernel.DotsC_DictionaryInt64KeyToIndex (typeId, parameter, key);
        }

        /// <summary>
        /// Dictionaries the index of the key to.
        /// </summary>
        /// <returns>The key to index.</returns>
        /// <param name="typeId">Type identifier.</param>
        /// <param name="parameter">Parameter.</param>
        /// <param name="key">Key.</param>
        public static Int32 DictionaryKeyToIndex(Int64 typeId, Int32 parameter, string key)
        {
            System.IntPtr sp = Internal.InternalOperations.CStringOf(key);
            int index = Kernel.DotsC_DictionaryStringKeyToIndex (typeId, parameter, sp);
            Marshal.FreeHGlobal(sp);
            return index;
        }

        /// <summary>
        /// Dictionaries the index of the key to.
        /// </summary>
        /// <returns>The key to index.</returns>
        /// <param name="typeId">Type identifier.</param>
        /// <param name="parameter">Parameter.</param>
        /// <param name="key">Key.</param>
        public static Int32 DictionaryKeyToIndex(Int64 typeId, Int32 parameter, EntityId key)
        {
            Internal.DotsC_EntityId eid;
            eid.TypeId = key.TypeId;
            eid.InstanceId = key.InstanceId.RawValue;
            return Kernel.DotsC_DictionaryEntityIdKeyToIndex (typeId, parameter, ref eid);
        }

        /// <summary>
        /// Dictionaries the index of the key to.
        /// </summary>
        /// <returns>The key to index.</returns>
        /// <param name="typeId">Type identifier.</param>
        /// <param name="parameter">Parameter.</param>
        /// <param name="key">Key.</param>
        public static Int32 DictionaryKeyToIndex(Int64 typeId, Int32 parameter, InstanceId key)
        {
            return Kernel.DotsC_DictionaryInt64KeyToIndex (typeId, parameter, key.RawValue);
        }

        /// <summary>
        /// Dictionaries the index of the key to.
        /// </summary>
        /// <returns>The key to index.</returns>
        /// <param name="typeId">Type identifier.</param>
        /// <param name="parameter">Parameter.</param>
        /// <param name="key">Key.</param>
        public static Int32 DictionaryKeyToIndex(Int64 typeId, Int32 parameter, HandlerId key)
        {
            return Kernel.DotsC_DictionaryInt64KeyToIndex (typeId, parameter, key.RawValue);
        }

        /// <summary>
        /// Dictionaries the index of the key to.
        /// </summary>
        /// <returns>The key to index.</returns>
        /// <param name="typeId">Type identifier.</param>
        /// <param name="parameter">Parameter.</param>
        /// <param name="key">Key.</param>
        public static Int32 DictionaryKeyToIndex(Int64 typeId, Int32 parameter, ChannelId key)
        {
            return Kernel.DotsC_DictionaryInt64KeyToIndex (typeId, parameter, key.RawValue);
        }

        #endregion
    }
}
