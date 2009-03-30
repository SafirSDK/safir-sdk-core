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
using System.Runtime.InteropServices;
using Safir.Dob.Typesystem.Internal;

namespace Safir.Dob.Typesystem
{
    /// <summary>
    /// Summary description for Operations.
    /// </summary>
    public class Operations
    {
        #region TypeInfo
        //********************************************************
        //* Type information operations
        //********************************************************
        public static int GetNumberOfTypeIds()
        {
            return Kernel.DotsC_NumberOfTypeIds();
        }

        public static int GetNumberOfClasses()
        {
            return Kernel.DotsC_NumberOfClasses();
        }

        public static int GetNumberOfProperties()
        {
            return Kernel.DotsC_NumberOfProperties();
        }

        public static int GetNumberOfEnumerations()
        {
            return Kernel.DotsC_NumberOfEnumerations();
        }

        public static System.Int64[] GetAllTypeIds()
        {
            int size=0;
            int bufSize=GetNumberOfTypeIds();
            System.Int64[] buf=new System.Int64[bufSize];
            System.IntPtr arrPtr=Marshal.AllocHGlobal(8*bufSize);
            Kernel.DotsC_GetAllTypeIds(arrPtr, bufSize, out size);
            Marshal.Copy(arrPtr, buf, 0, size);
            Marshal.FreeHGlobal(arrPtr);
            return buf;
        }

        public static bool Exists(System.Int64 typeId)
        {
            return Internal.InternalOperations.BoolOf(Kernel.DotsC_TypeExists(typeId));
        }

        public static bool IsClass(System.Int64 typeId)
        {
            return Internal.InternalOperations.BoolOf(Kernel.DotsC_IsClass(typeId));
        }

        public static bool IsProperty(System.Int64 typeId)
        {
            return Internal.InternalOperations.BoolOf(Kernel.DotsC_IsProperty(typeId));
        }

        public static bool IsEnumeration(System.Int64 typeId)
        {
            return Internal.InternalOperations.BoolOf(Kernel.DotsC_IsEnumeration(typeId));
        }

        public static System.Int64 GetTypeId(string typeName)
        {
            System.IntPtr sp=Internal.InternalOperations.CStringOf(typeName);
            System.Int64 id=Kernel.DotsC_TypeIdFromName(sp);
            Marshal.FreeHGlobal(sp);
            return id;
        }

        public static string GetName(System.Int64 typeId)
        {
            IntPtr sp=Kernel.DotsC_GetTypeName(typeId);
            if (sp == IntPtr.Zero)
            {
                throw new IllegalValueException("There is no such type defined");
            }
            else
            {
                return Internal.InternalOperations.StringOf(sp);
            }
        }

        public static int GetNumberOfEnumerationValues(System.Int64 enumId)
        {
            int result = Kernel.DotsC_GetNumberOfEnumerationValues(enumId);
            if (result == -1)
            {
                throw new IllegalValueException("No such enumeration exists");
            }
            else
            {
                return result;
            }
        }

        public static string GetEnumerationValueName(System.Int64 enumId,
                                                     System.Int32 enumVal)
        {
            IntPtr result = Kernel.DotsC_GetEnumerationValueName(enumId, enumVal);
            if (result == IntPtr.Zero)
            {
                throw new IllegalValueException("There is no such enumeration or value defined");
            }
            else
            {
                return Internal.InternalOperations.StringOf(result);
            }

        }

        public static int GetEnumerationValue(System.Int64 enumId,
                                              string enumValueName)
        {
            System.IntPtr p=Internal.InternalOperations.CStringOf(enumValueName);
            int val=Kernel.DotsC_EnumerationValueFromName(enumId, p);
            Marshal.FreeHGlobal(p);

            if (val == -1)
            {
                throw new IllegalValueException("There is no such enumeration or value defined");
            }
            else
            {
                return val;
            }
        }
        #endregion

        #region TypeCompatibility
        //************************************************************************************
        //* Type compatibility
        //************************************************************************************
        public static bool  IsOfType(System.Int64 theType,
                                     System.Int64 ofType)
        {
            return Internal.InternalOperations.BoolOf(Kernel.DotsC_IsOfType(theType, ofType));
        }

        public static System.Int64[] GetClassTree(System.Int64 theType)
        {
            int noOfClassesInTree;
            int totNbrOfClasses = GetNumberOfClasses();// +GetNumberOfProperties();
            System.IntPtr arrPtr = Marshal.AllocHGlobal(8 * totNbrOfClasses);
            Kernel.DotsC_GetCompleteType(theType, arrPtr, totNbrOfClasses, out noOfClassesInTree);
            System.Int64[] buf = new System.Int64[noOfClassesInTree];
            Marshal.Copy(arrPtr, buf, 0, noOfClassesInTree);
            Marshal.FreeHGlobal(arrPtr);
            return buf;
        }

        public static System.Int64 GetParentType(System.Int64 typeId)
        {
            return Kernel.DotsC_GetParentType(typeId);
        }

        public static void HasProperty(System.Int64 classId,
                                       System.Int64 propertyId,
                                       ref bool hasProperty,
                                       ref bool isInherited)
        {
            byte hasPropertyVal;
            byte isInheritedVal;
            Kernel.DotsC_HasProperty(classId, propertyId, out hasPropertyVal, out isInheritedVal);
            hasProperty = Internal.InternalOperations.BoolOf(hasPropertyVal);
            isInherited = Internal.InternalOperations.BoolOf(isInheritedVal);
        }

        public static bool HasProperty(System.Int64 classId,
                                       System.Int64 propertyId)
        {
            byte hasPropertyVal;
            byte isInheritedVal;
            Kernel.DotsC_HasProperty(classId, propertyId, out hasPropertyVal, out isInheritedVal);
            return Internal.InternalOperations.BoolOf(hasPropertyVal);
        }

#endregion




    }
}
