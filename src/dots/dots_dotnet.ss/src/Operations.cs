/* ****************************************************************************
*
* Copyright Consoden AB, 2005-2015 (http://safir.sourceforge.net)
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
    /// Type information operations.
    /// </summary>
    public class Operations
    {
        #region Type information operations

        /// <summary>
        /// Get the number of type id's defined in the system.
        /// <para>
        /// This is equal to GetNumberOfClasses() + GetNumberOfProperties() + GetNumberOfEnumerations().
        /// </para>
        /// </summary>
        /// <returns>Number of existing types.</returns>
        public static int GetNumberOfTypeIds()
        {
            return Kernel.DotsC_NumberOfTypeIds();
        }

        /// <summary>
        /// Get the number of classes defined in the system.
        /// </summary>
        /// <returns>Number of existing classes.</returns>
        public static int GetNumberOfClasses()
        {
            return Kernel.DotsC_NumberOfClasses();
        }

        /// <summary>
        /// Get the number of properties defined in the system.
        /// </summary>
        /// <returns>Number of existing properties.</returns>
        public static int GetNumberOfProperties()
        {
            return Kernel.DotsC_NumberOfProperties();
        }

        /// <summary>
        /// Get the number of enumeration types defined in the system.
        /// </summary>
        /// <returns>Number of existing enumeration types.</returns>
        public static int GetNumberOfEnumerations()
        {
            return Kernel.DotsC_NumberOfEnumerations();
        }

        /// <summary>
        /// Get all type id's that exists in the system.
        /// </summary>
        /// <returns>A vector containing all the type ids in the system.</returns>
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

        /// <summary>
        ///  Check if class with specified type id exist.
        /// </summary>
        /// <param name="typeId">Type id of class.</param>
        /// <returns>True if the type exists.</returns>
        public static bool Exists(System.Int64 typeId)
        {
            return Internal.InternalOperations.BoolOf(Kernel.DotsC_TypeExists(typeId));
        }

        /// <summary>
        /// Check if a type represented by a type id is a class.
        /// <para>
        /// If the typeid does not exist at all in the system, false will be returned.
        /// </para>
        /// </summary>
        /// <param name="typeId">Type id to check.</param>
        /// <returns>True if the type exists as a class.</returns>
        public static bool IsClass(System.Int64 typeId)
        {
            return Internal.InternalOperations.BoolOf(Kernel.DotsC_IsClass(typeId));
        }

        /// <summary>
        /// Check if a type represented by a type id is a property.
        /// <para>
        /// If the typeid does not exist at all in the system, false will be returned.
        /// </para>
        /// </summary>
        /// <param name="typeId">Type id to check.</param>
        /// <returns>True if the type exists as a property.</returns>
        public static bool IsProperty(System.Int64 typeId)
        {
            return Internal.InternalOperations.BoolOf(Kernel.DotsC_IsProperty(typeId));
        }

        /// <summary>
        /// Check if a type represented by a type id is an enumeration.
        /// <para>
        /// If the typeid does not exist at all in the system, false will be returned.
        /// </para>
        /// </summary>
        /// <param name="typeId">Type id to check.</param>
        /// <returns>True if the type exists as an enumeration.</returns>
        public static bool IsEnumeration(System.Int64 typeId)
        {
            return Internal.InternalOperations.BoolOf(Kernel.DotsC_IsEnumeration(typeId));
        }

        /// <summary>
        /// Check if a type represented by a type id is an enumeration.
        /// <para>
        /// If the typeid does not exist at all in the system, false will be returned.
        /// </para>
        /// </summary>
        /// <param name="typeId">Type id to check.</param>
        /// <returns>True if the type exists as an enumeration.</returns>
        public static bool IsException(System.Int64 typeId)
        {
            return Internal.InternalOperations.BoolOf(Kernel.DotsC_IsException(typeId));
        }

        /// <summary>
        /// Calculates the type id for the given name.
        /// <para>
        /// Note that this is a pure mathematical method and will always return the correct
        /// typeId for a class with the specified name. This method does not give any information
        /// about wheter a class with the specified name actually exist in the system. Use Exists
        /// to check if a class with the typeId returned from this method exists.
        /// TypeIds for classes and properties are based on namespaces and class name like this:
        /// typeName="MyNamespace1.MyNamespace2.MyClass"
        /// However for enumeration types the type id also contains the enum values, like this:
        /// typeName="MyNamespace1.MyNamespace2.MyEnumType.EnumValue1.EnumValue2.EnumValue3"
        /// </para>
        /// </summary>
        /// <param name="typeName">The name shall contain namespaces and class name with '.' as separator,
        /// example "MyNamespace1.MyNamespace2.MyClass"</param>
        /// <returns>Generated type id.</returns>
        public static System.Int64 GetTypeId(string typeName)
        {
            System.IntPtr sp=Internal.InternalOperations.CStringOf(typeName);
            System.Int64 id=Kernel.DotsC_TypeIdFromName(sp);
            Marshal.FreeHGlobal(sp);
            return id;
        }

        /// <summary>
        /// Gets the name associated with the specified type id.
        /// </summary>
        /// <param name="typeId">The type id to get the real name of.</param>
        /// <returns>Name of the type.</returns>
        /// <exception cref="Safir.Dob.Typesystem.IllegalValueException">There is no such type defined.</exception>
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

        /// <summary>
        /// Get the number of enumeration values the specified enumeration type has.
        /// </summary>
        /// <param name="enumId">Type id of enum type.</param>
        /// <returns>Number of enumeration values.</returns>
        /// <exception cref="Safir.Dob.Typesystem.IllegalValueException">There is no such type defined or it is not an enumeration.</exception>
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

        /// <summary>
        /// Get the string representation of the specified value for a enumeration type.
        /// </summary>
        /// <param name="enumId">Type id of enum type.</param>
        /// <param name="enumVal">The enumeration value.</param>
        /// <returns>String representation of the enumeration value.</returns>
        /// <exception cref="Safir.Dob.Typesystem.IllegalValueException">There is no such type defined or it is not an enumeration.</exception>
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

        /// <summary>
        /// Get integer value associated with the enumeration value for the specified enumeration type.
        /// </summary>
        /// <param name="enumId">Type id of enum type.</param>
        /// <param name="enumValueName">String representation of the desired value.</param>
        /// <returns>Integer value for the enumeration value name.</returns>
        /// <exception cref="Safir.Dob.Typesystem.IllegalValueException">There is no such type defined or it is not an enumeration.</exception>
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

        /// <summary>
        /// Get the Checksum over all enumeration members for an enumeration type.
        /// </summary>
        /// <param name="enumId">Type id of enum type.</param>
        /// <returns>Checksum of the enumeration type.</returns>
        /// <exception cref="Safir.Dob.Typesystem.IllegalValueException">There is no such type defined or it is not an enumeration.</exception>
        public static System.Int64 GetEnumerationChecksum(long enumId)
        {
            long result;
            Kernel.DotsC_GetEnumerationChecksum(enumId, out result);
            if (result == 0)
            {
                throw new IllegalValueException("There is no such enumeration or value defined");
            }
            else
            {
                return result;
            }
        }
        #endregion

        #region Type compatibility

        /// <summary>
        /// Checks if type is ofType or a subclass of ofType.
        /// </summary>
        /// <param name="theType">The type to check if it is of another type.</param>
        /// <param name="ofType">The type to compare to.</param>
        /// <returns>True of type is equal to ofType or if type inherits from ofType.
        /// False if one of the types are not a class type id. (i.e. a random number or a
        /// property or enumeration id).</returns>
        public static bool IsOfType(System.Int64 theType,
                                    System.Int64 ofType)
        {
            return Internal.InternalOperations.BoolOf(Kernel.DotsC_IsOfType(theType, ofType));
        }

        /// <summary>
        /// Returns all type id's that is of the given type by inheritance.
        /// <para>
        /// The type 'rootClass' will also be included. If for example type A
        /// is the base class for type B and type C, GetCompleteType(A.ClassTypeId) will
        /// return a vector with A, B and C.
        /// </para>
        /// </summary>
        /// <param name="theType">The base type.</param>
        /// <returns>A vector of type ids.</returns>
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

        /// <summary>
        /// Returns the typeId of the base class to the argument type. If type represents
        /// Object, then the typeId for Object is retured again.
        /// </summary>
        /// <param name="typeId">The type for which the parent type is requested.</param>
        /// <returns>A typeId.</returns>
        public static System.Int64 GetParentType(System.Int64 typeId)
        {
            return Kernel.DotsC_GetParentType(typeId);
        }

        /// <summary>
        /// Checks if the a class has a property and if it is inherited.
        /// </summary>
        /// <param name="classId">The type id of the class to check if it has a specific property.</param>
        /// <param name="propertyId">The type id of the property.</param>
        /// <param name="hasProperty">True if the class has the property.</param>
        /// <param name="isInherited">Indicates whether the property is set on the class itself or
        /// whether it was inherited from a parent class.</param>
        public static void HasProperty(System.Int64 classId,
                                       System.Int64 propertyId,
                                       out bool hasProperty,
                                       out bool isInherited)
        {
            byte hasPropertyVal;
            byte isInheritedVal;
            Kernel.DotsC_HasProperty(classId, propertyId, out hasPropertyVal, out isInheritedVal);
            hasProperty = Internal.InternalOperations.BoolOf(hasPropertyVal);
            isInherited = Internal.InternalOperations.BoolOf(isInheritedVal);
        }

        /// <summary>
        /// Checks if the a class has a property.
        /// </summary>
        /// <param name="classId">The type id of the class to check if it has a specific property.</param>
        /// <param name="propertyId">The type id of the property.</param>
        /// <returns>True if the class has the property.</returns>
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
