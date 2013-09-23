/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safir.sourceforge.net)
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

#ifndef __DOTS_TYPE_OPERATIONS_H__
#define __DOTS_TYPE_OPERATIONS_H__

#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Typesystem/Exceptions.h>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
/**
 * Operations on DOB types.
 */
namespace Operations
{
    /**
     * @name Type information operations
     */
    /** @{ */

    /**
     * Get the number of type id's defined in the system.
     * This is equal to GetNumberOfClasses() + GetNumberOfProperties() + GetNumberOfEnumerations().
     *
     * @return Number of existing types.
     */
    DOTS_API Dob::Typesystem::Int32 GetNumberOfTypeIds();

    /**
     * Get the number of classes defined in the system.
     *
     * @return Number of existing classes.
    */
    DOTS_API Dob::Typesystem::Int32 GetNumberOfClasses();

    /**
     * Get the number of properties defined in the system.
     *
     * @return Number of existing properties
     */
    DOTS_API Dob::Typesystem::Int32 GetNumberOfProperties();

    /**
     * Get the number of enum types defined in the system.
     *
     * @return Number of existing enumeration types.
     */
    DOTS_API Dob::Typesystem::Int32 GetNumberOfEnumerations();

    /**
     * Get all type id's that exists in the system.
     *
     * @return A vector containing all the type ids in the system.
     */
    DOTS_API TypeIdVector GetAllTypeIds();

    /**
     * Check if class with specified type id exist.
     *
     * @param typeId [in] - Type id of class.
     * @return True if the type exists.
     */
    DOTS_API bool Exists(const Dob::Typesystem::TypeId typeId);

    /**
     * Check if a type represented by a type id is a class.
     *
     * Using this function on a typeid that does not exist at all in the system will give false as return value.
     *
     * @param typeId [in] - Type id to check.
     * @return True if the type exists as a class.
     */
    DOTS_API bool IsClass(const Dob::Typesystem::TypeId typeId);

    /**
     * Check if a type represented by a type id is a property.
     *
     * Using this function on a typeid that does not exist at all in the system will give false as return value.
     *
     * @param typeId [in] - Type id to check.
     * @return True if the type exists as a property.
     */
    DOTS_API bool IsProperty(const Dob::Typesystem::TypeId typeId);

    /**
     * Check if a type represented by a type id is an enumeration.
     *
     * Using this function on a typeid that does not exist at all in the system will give false as return value.
     *
     * @param typeId [in] - Type id to check.
     * @return True if the type exists as an enumeration.
     */
    DOTS_API bool IsEnumeration(const Dob::Typesystem::TypeId typeId);

    /**
     * Check if a type represented by a type id is an exception.
     *
     * Using this function on a typeid that does not exist at all in the system will give false as return value.
     *
     * @param typeId [in] - Type id to check.
     * @return True if the type exists as an exception.
     */
    DOTS_API bool IsException(const Dob::Typesystem::TypeId typeId);

    

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
     * @param typeName [in] - The name shall contain namespaces and class name
     *                        with '.' as separator, example "MyNamespace1.MyNamespace2.MyClass"
     * @return Generated type id.
     */
    DOTS_API Dob::Typesystem::TypeId GetTypeId(const std::wstring & typeName);

    /**
     * Gets the name associated with the specified type id.
     *
     * @param typeId [in] - The type id to get the real name of.
     * @return Name of the type.
     * @throws IllegalValueException There is no such type defined.
     */
    DOTS_API std::wstring GetName(const Dob::Typesystem::TypeId typeId);

    /**
     * Get the number of enumeration values the specified enumeration type has.
     *
     * @param enumId [in] - Type id of enum type.
     * @return Number of enumeration values.
     * @throws IllegalValueException There is no such type defined or it is not an enumeration.
     */
    DOTS_API Dob::Typesystem::Int32 GetNumberOfEnumerationValues(const Dob::Typesystem::TypeId enumId);

    /**
     * Get the string representation of the specified value for a enumeration type.
     *
     * @param enumId [in] - Type id of enum type.
     * @param enumVal [in] - The enumeration value.
     * @return String representation of the enumeration value.
     * @throws IllegalValueException There is no such type defined or it is not an enumeration.
     */
    DOTS_API std::wstring GetEnumerationValueName(const Dob::Typesystem::TypeId enumId,
                                                  const Dob::Typesystem::EnumerationValue enumVal);

    /**
     * Get integer value associated with the enumeration value for the specified enumeration type.
     *
     * @param enumId [in] - Type id of enum type.
     * @param enumValueName [in] - String representation of the desired value.
     * @return Integer value for the enumeration value name.
     * @throws IllegalValueException There is no such type defined or it is not an enumeration.
     */
    DOTS_API EnumerationValue GetEnumerationValue(const Dob::Typesystem::TypeId enumId,
                                                  const std::wstring & enumValueName);
    /**
     * Get the Checksum over all enumeration members for an enumeration type.
     *
     * @param enumId [in] - Type id of enum type.
     * @return Checksum of the enumeration type.
     * @throws IllegalValueException There is no such type defined or it is not an enumeration.
     */
    DOTS_API Dob::Typesystem::TypeId GetEnumerationChecksum(const Dob::Typesystem::TypeId enumId);

    /** @} */

    /**
     * @name Type compatibility
     */
    /** @{ */

    /**
     * Checks if type is ofType or a subclass of ofType.
     *
     * @param type [in] - The type to check if it is of another type.
     * @param ofType [in] - The type to compare to.
     * @return True of type is equal to ofType or if type inherits from ofType
     *          false if one of the types are not a class type id. (i.e. a random number or a
     *          property or enumeration id).
     */
    DOTS_API bool IsOfType(const Dob::Typesystem::TypeId type,
                           const Dob::Typesystem::TypeId ofType);

    /**
     * Returns a list of all type id's that is of the given type by inheritance.
     * The type 'rootClass' will also be inserted in the list. If for example type A
     * is the base class for type B and type C, GetCompleteType(A::ClassTypeId) will
     * return a list with A, B and C.
     *
     * @param rootClass [in] - The base type.
     * @return A vector of type ids.
     */
    DOTS_API TypeIdVector GetClassTree(const Dob::Typesystem::TypeId rootClass);

    /**
     * Returns the typeId of the base class to the argument type. If type represents
     * Object, then the typeId for Object is returned again.
     *
     * @param type [in] - The type for which the parent type is requested.
     * @return A typeId.
     */
    DOTS_API Dob::Typesystem::TypeId GetParentType(const Dob::Typesystem::TypeId type);

    /**
     * Checks if the a class has a property.
     *
     * @param classType [in] - The type id of the class to check if it has a specific property.
     * @param propertyType [in] - The type id of the property.
     * @return True if the class has the property.
     */
    DOTS_API bool HasProperty(const Dob::Typesystem::TypeId classType,
                              const Dob::Typesystem::TypeId propertyType);

    /**
     * Checks if the a class has a property and if it is inherited.
     *
     * @param classType [in] - The type id of the class to check if it has a specific property.
     * @param propertyType [in] - The type id of the property.
     * @param hasProperty [out] - True if the class has the property.
     * @param isInherited [out] - Indicates whether the property is set on the class itself or
     *                            whether it was inherited from a parent class.
     */
    DOTS_API void HasProperty(const Dob::Typesystem::TypeId classType,
                              const Dob::Typesystem::TypeId propertyType,
                              bool & hasProperty,
                              bool & isInherited);
    /** @} */

}
}
}
}

#endif
