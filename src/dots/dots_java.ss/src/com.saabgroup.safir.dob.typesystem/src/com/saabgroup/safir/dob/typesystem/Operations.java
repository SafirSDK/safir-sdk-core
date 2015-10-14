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

/**
 * Operations on DOB types.
 */
public class Operations {

    /**
     * Get the number of type id's defined in the system.
     * This is equal to GetNumberOfClasses() + GetNumberOfProperties() + GetNumberOfEnumerations().
     *
     * @return Number of existing types.
     */
    public static int getNumberOfTypeIds()
    {
        return Kernel.NumberOfTypeIds();
    }

    /**
     * Get the number of classes defined in the system.
     *
     * @return Number of existing classes.
    */
    public static int getNumberOfClasses()
    {
        return Kernel.NumberOfClasses();
    }

    /**
     * Get the number of properties defined in the system.
     *
     * @return Number of existing properties
     */
    public static int getNumberOfProperties()
    {
        return Kernel.NumberOfProperties();
    }

    /**
     * Get the number of enum types defined in the system.
     *
     * @return Number of existing enumeration types.
     */
    public static int getNumberOfEnumerations()
    {
        return Kernel.NumberOfEnumerations();
    }

    /**
     * Get all type id's that exists in the system.
     *
     * @return An array containing all the type ids in the system.
     */
    public static long[] getAllTypeIds()
    {
        int size [] = new int[1];
        int bufSize = getNumberOfTypeIds();
        long buf [] = new long[bufSize];
        Kernel.GetAllTypeIds(buf,bufSize,size);
        return buf;
    }

    /**
     * Check if class with specified type id exist.
     *
     * @param typeId Type id of class.
     * @return True if the type exists.
     */
    public static boolean exists(long typeId)
    {
        return Kernel.TypeExists(typeId);
    }

    /**
     * Check if a type represented by a type id is a class.
     *
     * Using this function on a TypeId that does not exist at all in the system will give false as return value.
     *
     * @param typeId Type id to check.
     * @return True if the type exists as a class.
     */
    public static boolean isClass(long typeId)
    {
        return Kernel.IsClass(typeId);
    }

    /**
     * Check if a type represented by a type id is a property.
     *
     * Using this function on a TypeId that does not exist at all in the system will give false as return value.
     *
     * @param typeId Type id to check.
     * @return True if the type exists as a property.
     */
    public static boolean isProperty(long typeId)
    {
        return Kernel.IsProperty(typeId);
    }

    /**
     * Check if a type represented by a type id is an enumeration.
     *
     * Using this function on a TypeId that does not exist at all in the system will give false as return value.
     *
     * @param typeId Type id to check.
     * @return True if the type exists as an enumeration.
     */
    public static boolean isEnumeration(long typeId)
    {
        return Kernel.IsEnumeration(typeId);
    }

    /**
     * Check if a type represented by a type id is an exception.
     *
     * Using this function on a TypeId that does not exist at all in the system will give false as return value.
     *
     * @param typeId Type id to check.
     * @return True if the type exists as an exception.
     */
    public static boolean isException(long typeId)
    {
        return Kernel.IsException(typeId);
    }

    /**
     * Calculates the type id for the given name.
     *
     * Note that this is a pure mathematical
     * function and will always return the correct typeId for a class with the specified
     * name. This function does not give any information about whether a class with the
     * specified name actually exist in the system. Use Exists to check if a class with
     * the typeId returned from this function exists.
     * TypeIds for classes and properties are based on namespaces and class name like this:
     * typeName="MyNamespace1.MyNamespace2.MyClass"
     * However for enumeration types the type id also contains the enum values, like this:
     * typeName="MyNamespace1.MyNamespace2.MyEnumType.EnumValue1.EnumValue2.EnumValue3"
     *
     * @param typeName The name shall contain namespaces and class name
     *                   with '.' as separator, example "MyNamespace1.MyNamespace2.MyClass"
     * @return Generated type id.
     */
    public static long getTypeId(String typeName)
    {
        return Kernel.TypeIdFromName(typeName);
    }

    /**
     * Gets the name associated with the specified type id.
     *
     * @param typeId The type id to get the real name of.
     * @return Name of the type.
     * @throws IllegalValueException There is no such type defined.
     */
    public static String getName(long typeId)
    {
        String result = Kernel.GetTypeName(typeId);
        if (result == null)
        {
            throw new IllegalValueException("There is no such type defined");
        }
        else
        {
            return result;
        }
    }

    /**
     * Get the number of enumeration values the specified enumeration type has.
     *
     * @param enumId Type id of enum type.
     * @return Number of enumeration values.
     * @throws IllegalValueException There is no such type defined or it is not an enumeration.
     */
    public static int getNumberOfEnumerationValues(long enumId)
    {
        int result = Kernel.GetNumberOfEnumerationValues(enumId);
        if (result == -1)
        {
            throw new IllegalValueException("No such enumeration exists");
        }
        else
        {
            return result;
        }
    }

    /**
     * Get the string representation of the specified value for a enumeration type.
     *
     * @param enumId Type id of enum type.
     * @param enumVal The enumeration value.
     * @return String representation of the enumeration value.
     * @throws IllegalValueException There is no such type defined or it is not an enumeration.
     */
    public static String getEnumerationValueName(long enumId, int enumVal)
    {
        String result = Kernel.GetEnumerationValueName(enumId, enumVal);
        if (result == null)
        {
            throw new IllegalValueException("There is no such enumeration or value defined");
        }
        else
        {
            return result;
        }
    }

    /**
     * Get integer value associated with the enumeration value for the specified enumeration type.
     *
     * @param enumId Type id of enum type.
     * @param enumValueName String representation of the desired value.
     * @return Integer value for the enumeration value name.
     * @throws IllegalValueException There is no such type defined or it is not an enumeration.
     */
    public static int getEnumerationValue(long enumId, String enumValueName)
    {
        int result = Kernel.EnumerationValueFromName(enumId, enumValueName);
        if (result == -1)
        {
            throw new IllegalValueException("There is no such enumeration or value defined");
        }
        else
        {
            return result;
        }
    }

    /**
     * Get the Checksum over all enumeration members for an enumeration type.
     *
     * @param enumId [in] - Type id of enum type.
     * @return Checksum of the enumeration type.
     * @throws IllegalValueException There is no such type defined or it is not an enumeration.
     */
    public static long getEnumerationChecksum(long enumId){
        long result = Kernel.GetEnumerationChecksum(enumId);
        if (result == 0)
        {
            throw new IllegalValueException("There is no such enumeration or value defined");
        }
        else
        {
            return result;
        }
    }


    //************************************************************************************
    //* Type compatibility
    //************************************************************************************
    /**
     * Checks if type is ofType or a subclass of ofType.
     *
     * @param type The type to check if it is of another type.
     * @param ofType The type to compare to.
     * @return True of type is equal to ofType or if type inherits from ofType
     *          false if one of the types are not a class type id. (i.e. a random number or a
     *          property or enumeration id).
     */
    public static boolean isOfType(long type, long ofType)
    {
        return Kernel.IsOfType(type, ofType);
    }

    /**
     * Returns a list of all type id's that is of the given type by inheritance.
     * The type 'rootClass' will also be inserted in the list. If for example type A
     * is the base class for type B and type C, GetCompleteType(A::ClassTypeId) will
     * return a list with A, B and C.
     *
     * @param rootClass The base type.
     * @return An array of type ids.
     */
    public static long[] getClassTree(long rootClass)
    {
        return Kernel.GetCompleteType(rootClass);
    }

    /**
     * Returns the typeId of the base class to the argument type. If type represents
     * Object, then the typeId for Object is returned again.
     *
     * @param type The type for which the parent type is requested.
     * @return A typeId.
     */
    public static long getParentType(long type)
    {
        return Kernel.GetParentType(type);
    }

    /**
     * Checks if the a class has a property.
     *
     * @param classType [in] - The type id of the class to check if it has a specific property.
     * @param propertyType [in] - The type id of the property.
     * @return True if the class has the property.
     */
    public static boolean hasProperty(long classType, long propertyType)
    {
        boolean hasProperty[] = new boolean[1];
        boolean isInherited[] = new boolean[1];
        Kernel.HasProperty(classType, propertyType, hasProperty, isInherited);
        return hasProperty[0];
    }

    /**
     * Check if a property is inherited.
     *
     * @param classType [in] - The type id of the class to check if it has a specific property.
     * @param propertyType [in] - The type id of the property.
     * @return true if the class has the property and it is inherited (rather than defined on this class)
     */
    public static boolean propertyIsInherited(long classType, long propertyType)
    {
        boolean hasProperty[] = new boolean[1];
        boolean isInherited[] = new boolean[1];
        Kernel.HasProperty(classType, propertyType, hasProperty, isInherited);
        return isInherited[0];
    }
}
