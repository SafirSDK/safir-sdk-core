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

using System;
using Safir.Dob.Typesystem.Internal;
using System.Runtime.InteropServices;

namespace Safir.Dob.Typesystem
{
    public class Parameters
    {
        #region ParameterInfo
        //********************************************************
        //* Functions handling parameters
        //********************************************************
        public static int GetNumberOfParameters(System.Int64 typeId)
        {
            return Kernel.DotsC_GetNumberOfParameters(typeId);
        }

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

        public static string GetName(System.Int64 typeId,
                                              int parameter)
        {
            return Internal.InternalOperations.StringOf(Kernel.DotsC_GetParameterName(typeId, parameter));
        }

        public static MemberType GetType(System.Int64 typeId,
                                                             int parameter)
        {
            return Kernel.DotsC_GetParameterType(typeId, parameter);
        }

        public static string GetTypeName(System.Int64 typeId,
                                                  int parameter)
        {
            return Internal.InternalOperations.StringOf(Kernel.DotsC_GetParameterTypeName(typeId, parameter));
        }

        public static int GetArraySize(System.Int64 typeId,
                                                int parameter)
        {
            return Kernel.DotsC_GetParameterArraySize(typeId, parameter);
        }

        #endregion

        #region GetParameterValues
        //************************************************************************************
        //* Functions for retrieval of parameters
        //************************************************************************************
        public static bool GetBoolean(System.Int64 typeId,
                                      int parameter,
                                      int index)
        {
            byte val;
            Kernel.DotsC_GetBooleanParameter(typeId, parameter, index, out val);
            return Internal.InternalOperations.BoolOf(val);
        }

        public static int GetEnumeration(System.Int64 enumId,
                                                  System.Int64 typeId,
                                                  int parameter,
                                                  int index)
        {
            System.Int32 val;
            Kernel.DotsC_GetEnumerationParameter(typeId, parameter, index, out val);

            return val;
        }

        public static int GetInt32(System.Int64 typeId,
                                   int parameter,
                                   int index)
        {
            System.Int32 val;
            Kernel.DotsC_GetInt32Parameter(typeId, parameter, index, out val);
            return val;
        }

        public static long GetInt64(System.Int64 typeId,
                                             int parameter,
                                             int index)
        {
            System.Int64 val;
            Kernel.DotsC_GetInt64Parameter(typeId, parameter, index, out val);
            return val;
        }

        public static float GetFloat32(System.Int64 typeId,
                                                int parameter,
                                                int index)
        {
            float val;
            Kernel.DotsC_GetFloat32Parameter(typeId, parameter, index, out val);
            return val;
        }

        public static double GetFloat64(System.Int64 typeId,
                                                 int parameter,
                                                 int index)
        {
            double val;
            Kernel.DotsC_GetFloat64Parameter(typeId, parameter, index, out val);
            return val;
        }

        public static string GetString(System.Int64 typeId,
                                                int parameter,
                                                int index)
        {
            System.IntPtr sp;
            Kernel.DotsC_GetStringParameter(typeId, parameter, index, out sp);
            return Internal.InternalOperations.StringOf(sp);
        }

        public static System.Int64 GetTypeId(System.Int64 typeId,
                                                      int parameter,
                                                      int index)
        {
            System.Int64 val;
            Kernel.DotsC_GetTypeIdParameter(typeId, parameter, index, out val);
            return val;
        }

        public static InstanceId GetInstanceId(System.Int64 typeId,
                                                 int parameter,
                                                 int index)
        {
            System.Int64 hashVal;
            System.IntPtr strVal;
            Kernel.DotsC_GetHashedIdParameter(typeId, parameter, index, out hashVal, out strVal);
            if (strVal == System.IntPtr.Zero)
            {
                return new InstanceId(hashVal);
            }
            else
            {
                return new InstanceId(hashVal, Internal.InternalOperations.StringOf(strVal));
            }
        }

        public static Dob.Typesystem.EntityId GetEntityId(System.Int64 typeId,
                                                          int parameter,
                                                          int index)
        {
            Dob.Typesystem.Internal.DotsC_EntityId eid;
            System.IntPtr strVal;
            Kernel.DotsC_GetEntityIdParameter(typeId, parameter, index, out eid, out strVal);
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

        public static ChannelId GetChannelId(System.Int64 typeId,
                                                 int parameter,
                                                 int index)
        {
            System.Int64 hashVal;
            System.IntPtr strVal;
            Kernel.DotsC_GetHashedIdParameter(typeId, parameter, index, out hashVal, out strVal);
            if (strVal == System.IntPtr.Zero)
            {
                return new ChannelId(hashVal);
            }
            else
            {
                return new ChannelId(hashVal, Internal.InternalOperations.StringOf(strVal));
            }
        }

        public static HandlerId GetHandlerId(System.Int64 typeId,
                                                 int parameter,
                                                 int index)
        {
            System.Int64 hashVal;
            System.IntPtr strVal;
            Kernel.DotsC_GetHashedIdParameter(typeId, parameter, index, out hashVal, out strVal);
            if (strVal == System.IntPtr.Zero)
            {
                return new HandlerId(hashVal);
            }
            else
            {
                return new HandlerId(hashVal, Internal.InternalOperations.StringOf(strVal));
            }
        }

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

        #endregion
    }
}
