/******************************************************************************
*
* Copyright Saab AB, 2004-2013 (http://safir.sourceforge.net)
*
* Created by: Joel Ottosson / joot
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
* GNU General Public License for more Internals.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/
#ifndef __DOTS_INTERNAL_TYPE_REPOSITORY_H__
#define __DOTS_INTERNAL_TYPE_REPOSITORY_H__

#include <set>
#include <string>
#include <Safir/Dob/Typesystem/LanguageInterfaceDefs.h>
#include <Safir/Utilities/Internal/Id.h>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace ToolSupport
{
    //forward declarations
    template <class T> struct TypeRepositoryTraits;
    class ClassDescription;
    class MemberDescription;

    /**
     * @brief Description of a property type.
     */
    class PropertyDescription
    {
    public:
        /**
         * @brief Get dou file path as a string.
         * @return Path to file.
         */
        virtual const char* FileName() const=0;

        /**
         * @brief Get a summary about this property.
         * @return Summary.
         */
        virtual const char* Summary() const=0;

        /**
         * @brief Get the typeId of this property.
         * @return TypeId.
         */
        virtual DotsC_TypeId GetTypeId() const=0;

        /**
         * @brief Get qualified name of the property type.
         * @return Type name.
         */
        virtual const char* GetName() const=0;

        /**
         * @brief Get the number of members contained by this property.
         * @return Number of members.
         */
        virtual int GetNumberOfMembers() const=0;

        /**
         * @brief Get index of a named member.
         * @param memberName [in] - Name of the member.
         * @return Index of the member with the given name. Returns -1 if no such member exists.
         */
        virtual DotsC_MemberIndex GetMemberIndex(const std::string& memberName) const=0;

        /**
         * @brief Get a member description.
         * @param index [in] - Index of the requested member. Valid values are 0 to GetNumberOfMembers()-1
         * @return Member description.
         */
        virtual const MemberDescription* GetMember(DotsC_MemberIndex index) const=0;
    };

    /**
     * @brief Description of an exception type.
     */
    class ExceptionDescription
    {
    public:
        /**
         * @brief Get dou file path as a string.
         * @return Path to file.
         */
        virtual const char* FileName() const=0;

        /**
         * @brief Get a summary about this exception.
         * @return Summary.
         */
        virtual const char* Summary() const=0;

        /**
         * @brief Get the typeId of this exception.
         * @return TypeId.
         */
        virtual DotsC_TypeId GetTypeId() const=0;

        /**
         * @brief Get qualified name of the exception type.
         * @return Type name.
         */
        virtual const char* GetName() const=0;

        /**
         * @brief Get the base class description of this exception.
         * @return Base class description.
         */
        virtual const ExceptionDescription* GetBaseClass() const=0;
    };

    /**
     * @brief Description of a parameter.
     */
    class ParameterDescription
    {
    public:
        /**
         * @brief Get a summary about this parameter.
         * @return Summary.
         */
        virtual const char* Summary() const=0;

        /**
         * @brief Get the name of the parameter. Not including namespaces and class name.
         * @return Parameter name.
         */
        virtual const char* GetName() const=0; //get the parameter name, no namespace and class name: MyParameter

        /**
         * @brief Get fully qualified parameter name. Example: MyNamespace.MyClass.MyParameter
         * @return Qualified parameter name.
         */
        virtual const char* GetQualifiedName() const=0;

        /**
         * @brief Get member type of this parameter value. If member type is Enumeration or Object, use GetTypeId() to determine specific type.
         * @return Parameter value member type.
         */
        virtual DotsC_MemberType GetMemberType() const=0;

        /**
         * @brief If this parameter has MemberType Enumeration or Object, the specific typeId of that type can be retrieved by this method.
         * If MemberType is not Enum or Object the result of this method is undefined. Hence always use this in conjunction with GetMemberType().
         * @return TypeId of this parameter. Must always be a class or enumeration typeId.
         */
        virtual DotsC_TypeId GetTypeId() const=0;

        /**
         * @brief Get the collection type of this parameter.
         * @return Collection type.
         */
        virtual DotsC_CollectionType GetCollectionType() const=0;

        /**
         * @brief Get key type of this parameter. Only valid if CollectionType=HashtableCollectionType, use GetCollectionType() method.
         * @return Key member type.
         */
        virtual DotsC_MemberType GetKeyType() const=0; //only valid if collectionType is Hashtable

        /**
         * @brief Get the array size of this parameter.
         * @return Array size. If parameter is not an array, 1 is returned.
         */
        virtual int GetArraySize() const=0;

        /**
         * @brief Check if this is a normal parameter defined explicitly in a dou-file or if it is a special hidden parameter that has been
         * automatically generated.
         * @return False if parameter is a normal parameter. True if parameter is a hidden autmatically generated parameter.
         */
        virtual bool IsHidden() const=0;

        //Get parameter values - depending on actual type of the parameter
        //For entityId use GetInt64Value for typeId and GetHashedValue for instanceId

        /**
         * @brief Get int32 parameter value. Only valid if MemberType is Int32.
         * @param index [in] - Parameter index.
         * @return Parameter value.
         */
        virtual boost::int32_t GetInt32Value(int index) const=0; //int32, enum

        /**
         * @brief Get int64 parameter value. Only valid if MemberType is Int64 or TypeId
         * @param index [in] - Parameter index.
         * @return Parameter value.
         */
        virtual boost::int64_t GetInt64Value(int index) const=0; //int64, typeId

        /**
         * @brief Get float32 parameter value. Only valid if MemberType is Float32 or any Si32 type.
         * @param index [in] - Parameter index.
         * @return Parameter value.
         */
        virtual float GetFloat32Value(int index) const=0; //float32, si32

        /**
         * @brief Get float64 parameter value. Only valid if MemberType is Float64 or any Si64 type.
         * @param index [in] - Parameter index.
         * @return Parameter value.
         */
        virtual double GetFloat64Value(int index) const=0; //float64, si64

        /**
         * @brief Get bool parameter value. Only valid if MemberType is Boolean.
         * @param index [in] - Parameter index.
         * @return Parameter value.
         */
        virtual bool GetBoolValue(int index) const=0;

        /**
         * @brief Get string parameter value. Only valid if MemberType is String.
         * @param index [in] - Parameter index.
         * @return Parameter value.
         */
        virtual const char* GetStringValue(int index) const=0;

        /**
         * @brief Get object parameter value. Only valid if MemberType is Object.
         * @param index [in] - Parameter index.
         * @return Parameter value as a pair containing a blob and a blob size.
         */
        virtual std::pair<const char*, size_t> GetObjectValue(int index) const=0;

        /**
         * @brief Get binary parameter value. Only valid if MemberType is Binary.
         * @param index [in] - Parameter index.
         * @return Parameter value as a pair containing a binary and a binary size.
         */
        virtual std::pair<const char*, size_t> GetBinaryValue(int index) const=0;

        /**
         * @brief Get hashed parameter value. Only valid if MemberType is InstanceId, ChannelId, HandlerId
         * @param index [in] - Parameter index.
         * @return Parameter value as a pair containing a hash value and optionally a string that can be NULL.
         */
        virtual std::pair<boost::int64_t, const char*> GetHashedValue(int index) const=0; //instanceId, channelId, handlerId
    };

    /**
     * @brief Description of an enumeration type.
     */
    class EnumDescription
    {
    public:
        /**
         * @brief Get dou file path as a string.
         * @return Path to file.
         */
        virtual const char* FileName() const=0;

        /**
         * @brief Get a summary about this enumeration.
         * @return Summary.
         */
        virtual const char* Summary() const=0;

        /**
         * @brief Get the typeId of this enumeration.
         * @return TypeId.
         */
        virtual DotsC_TypeId GetTypeId() const=0;

        /**
         * @brief Get qualified name of the enumeration type.
         * @return Type name.
         */
        virtual const char* GetName() const=0;

        /**
         * @brief Get checksum of this enum type.
         * @return Checksum.
         */
        virtual DotsC_TypeId GetCheckSum() const=0;

        /**
         * @brief Get number of enumeration values.
         * @return Number of enumeration values.
         */
        virtual int GetNumberOfValues() const=0;

        /**
         * @brief Get name of a enumeration value.
         * @param val [in] - The ordinal of the requested value. Valid is 0 to GetNumberOfValues()-1.
         * @return Name of enumeration value.
         */
        virtual const char* GetValueName(DotsC_EnumerationValue val) const=0;

        /**
         * @brief Get index (ordinal) for a named enumeration value. Supports short name and fully qualified name. Ex: 'Monday' and 'MyEnumType.Monday'
         * @param valueName [in] - An enumeration value name.
         * @return Index (ordinal).
         */
        virtual int GetIndexOfValue(const std::string& valueName) const=0;
    };

    /**
     * @brief Description of a member.
     */
    class MemberDescription
    {
    public:
        /**
         * @brief Get a summary about this member.
         * @return Summary.
         */
        virtual const char* Summary() const=0;

        /**
         * @brief If this member has MemberType Enumeration or Object, the specific typeId of that type can be retrieved by this method.
         * If MemberType is not Enum or Object the result of this method is undefined. Hence always use this in conjunction with GetMemberType().
         * @return TypeId of this member. Must always be a class or enumeration typeId.
         */
        virtual DotsC_TypeId GetTypeId() const=0;

        /**
         * @brief Get member name.
         * @return Member name.
         */
        virtual const char* GetName() const=0;

        /**
         * @brief Get type of this member. If member type is Enumeration or Object, use GetTypeId() to determine specific type.
         * @return Parameter value member type.
         */
        virtual DotsC_MemberType GetMemberType() const=0;

        /**
         * @brief Get the collection type of this member.
         * @return Collection type.
         */
        virtual DotsC_CollectionType GetCollectionType() const=0;

        /**
         * @brief Get key type of this member. Only valid if CollectionType=HashtableCollectionType, use GetCollectionType() method.
         * @return Key member type.
         */
        virtual DotsC_MemberType GetKeyType() const=0; //only valid if collectionType is Hashtable

        /**
         * @brief Get the array size of this member.
         * @return Array size. If member is not an array, 1 is returned. Use GetCollectionType() method to determine if member is an array.
         */
        virtual int GetArraySize() const=0;

        /**
         * @brief If member type is String this method returns the maximum allowed string length.
         * If MemberType is not String the result of this method is undefined. Hence always use this in conjunction with GetMemberType().
         * @return Max string length.
         */
        virtual int GetMaxLength() const=0;
    };

    /**
     * @brief Description of a property member mapping.
     */
    class MemberMappingDescription
    {
    public:
        /**
         * @brief Get type of property member mapping. A member can be mapped to NULL, to a class member or to a parameter.
         * @return Type of mapping.
         */
        virtual DotsC_PropertyMappingKind GetMappingKind() const=0;

        /**
         * @brief If this property mapping is of type MappedToParameter, use GetMappingKind() to check that, this method returns a pair containing
         * parameter description and parameter index. If member is mapped to a whole array, parameterIndex is set to -1.
         * @return
         */
        virtual std::pair<const ParameterDescription*, int /*paramIndex*/> GetParameter() const=0;

        //if mapped to member
        /**
         * @brief If this property mapping is of type MappedToMember, use GetMappingKind() to check that, this method returns a member depth level.
         * I.e how many indirection into contained objects that is used. For example if property member M1 is mapped into Class1.ObjectMember1.Int32Member
         * this method will return 2.
         * @return Reference depth for a member mapping of type MappedToMember.
         */
        virtual int MemberReferenceDepth() const=0;

        /**
         * @brief If this property mapping is of type MappedToMember, use GetMappingKind() to check that, this method will get a pair containing a class member index
         * and an array index for the member mapping of specified depth.
         * @param depth [in] - The depth of the member mappping that is requested.
         * @return Pair containing member index and array index.
         */
        virtual std::pair<DotsC_MemberIndex, DotsC_ArrayIndex> GetMemberReference(int depth) const=0;
    };

    /**
     * @brief Description of a property mapping.
     */
    class PropertyMappingDescription
    {
    public:
        /**
         * @brief Get dou file path as a string.
         * @return Path to file.
         */
        virtual const char* FileName() const=0;

        /**
         * @brief Get a summary about this property mapping.
         * @return Summary.
         */
        virtual const char* Summary() const=0;

        /**
         * @brief Get the property this property mapping refers to.
         * @return Property description.
         */
        virtual const PropertyDescription* GetProperty() const=0;

        /**
         * @brief Get the class this property mapping refers to.
         * @return Class description.
         */
        virtual const ClassDescription* GetClass() const=0;

        /**
         * @brief Get member mapping for a specific property member.
         * @param propertyMemberIndex [in] - The member index of the property member.
         * @return Member mapping description.
         */
        virtual const MemberMappingDescription* GetMemberMapping(int propertyMemberIndex) const=0;
    };

    /**
     * @brief Description of a create routine.
     */
    class CreateRoutineDescription
    {
    public:
        /**
         * @brief Get a summary about this create routine.
         * @return Summary.
         */
        virtual const char* Summary() const=0;

        /**
         * @brief Get create routine name.
         * @return Create routine name.
         */
        virtual const char* GetName() const=0;

        /**
         * @brief Get the number of in parameters for this create routine.
         * @return Number of in parameters.
         */
        virtual int GetNumberOfInParameters() const=0;

        /**
         * @brief Get member description for an in parameter.
         * @param index [in] - Index of in parameter. Valid 0 to GetNumberOfInParameters()-1.
         * @return Member description.
         */
        virtual const MemberDescription* GetInParameterMember(int index) const=0;

        /**
         * @brief Get number of default values that this create routine will set.
         * @return Number of default values.
         */
        virtual int GetNumberOfDefaultValues() const=0;

        /**
         * @brief Get member description for a default value.
         * @param index [in] - Index of default value. Valid 0 to GetNumberOfDefaultValues()-1.
         * @return Member description.
         */
        virtual const MemberDescription* GetDefaultValueMember(int index) const=0;

        /**
         * @brief Get parameter description for a default value.
         * @param index [in] - Index of default value. Valid 0 to GetNumberOfDefaultValues()-1.
         * @return Pair containing a parameter description and an array index.
         */
        virtual std::pair<const ParameterDescription*, int /*paramIndex*/> GetDefaultValue(int index) const=0;
    };

    /**
     * @brief Description of a class type.
     */
    class ClassDescription
    {
    public:
        /**
         * @brief Get dou file path as a string.
         * @return Path to file.
         */
        virtual const char* FileName() const=0;

        /**
         * @brief Get a summary about this class.
         * @return Summary.
         */
        virtual const char* Summary() const=0;

        /**
         * @brief Get the typeId of this class.
         * @return TypeId.
         */
        virtual DotsC_TypeId GetTypeId() const=0;

        /**
         * @brief Get qualified name of the class type.
         * @return Type name.
         */
        virtual const char* GetName() const=0;

        /**
         * @brief Get the base class description of this class.
         * @return Base class description.
         */
        virtual const ClassDescription* GetBaseClass() const=0;
        virtual int GetNumberOfDescendants() const=0;
        virtual const ClassDescription* GetDescendant(int index) const=0;

        /**
         * @brief Get the total number of members contained by this class including inherited members.
         * @return Number of members.
         */
        virtual int GetNumberOfMembers() const=0;

        /**
         * @brief Get the number of members defined by this class. Inherited members are not included.
         * @return Number non inherited members.
         */
        virtual int GetNumberOfOwnMembers() const=0;

        /**
         * @brief Get the number of inherited members. Members defined by this class itself are not included.
         * @return Number inherited members.
         */
        virtual int GetNumberOfInheritedMembers() const=0;

        /**
         * @brief Get index of a named member.
         * @param memberName [in] - Name of the member.
         * @return Index of the member with the given name. Returns -1 if no such member exists.
         */
        virtual DotsC_MemberIndex GetMemberIndex(const std::string& memberName) const=0;

        /**
         * @brief Get a member description.
         * @param index [in] - Index of the requested member. Valid values are 0 to GetNumberOfMembers()-1
         * @return Member description.
         */
        virtual const MemberDescription* GetMember(DotsC_MemberIndex index) const=0;

        /**
         * @brief Get the total number of parameters contained by this class including inherited parameters.
         * @return Number of parameters.
         */
        virtual int GetNumberOfParameters() const=0;

        /**
         * @brief Get the number of parameters defined by this class. Inherited parameters are not included.
         * @return Number non inherited parameters.
         */
        virtual int GetNumberOfOwnParameters() const=0;

        /**
         * @brief Get the number of inherited parameters. Parameters defined by this class itself are not included.
         * @return Number inherited parameters.
         */
        virtual int GetNumberOfInheritedParameters() const=0;

        /**
         * @brief Get parameter description.
         * @param index [in] - Index of the requested parameter. Valid values are 0 to GetNumberOfParameters()-1
         * @return
         */
        virtual const ParameterDescription* GetParameter(DotsC_ParameterIndex index) const=0;

        /**
         * @brief Get a set of all properties in this class.
         * @param propertyIds [out] - Set of typeIds
         */
        virtual void GetPropertyIds(std::set<DotsC_TypeId>& propertyIds) const=0;

        /**
         * @brief Get property mapping description for a specific property that describes how it is mapped for this class.
         * @param propertyTypeId [in] - TypeId of the property.
         * @param isInherited [out] - Boolean value indicating if the property is inherited or not.
         * @return Property mapping description.
         */
        virtual const PropertyMappingDescription* GetPropertyMapping(DotsC_TypeId propertyTypeId, bool& isInherited) const=0;

        /**
         * @brief Get the number of create routines in this class.
         * @return Number of create routines.
         */
        virtual int GetNumberOfCreateRoutines() const=0;

        /**
         * @brief Get a create routine description.
         * @param index [in] - Index of the requested create routine. Valid values are 0 to GetNumberOfCreateRoutines()-1
         * @return
         */
        virtual const CreateRoutineDescription* GetCreateRoutine(int index) const=0;

        /**
         * @brief Get the minimum size that a blob of this class can be. This method will most likely be removed in later versions.
         * @return Initial blob size.
         */
        virtual int InitialSize() const=0;

        /**
         * @brief Get size needed by members defined in this class, not including inherited members. This method will most likely be removed in later versions.
         * @return Size.
         */
        virtual int OwnSize() const=0;
    };

    /**
     * @brief The TypeRepository class is the baseclass of any TypeRepository implementation.
     */
    class TypeRepository
    {
    public:

        //-----------------
        // Enumerations
        //-----------------
        /**
         * @brief Get enumeration description by typeId.
         * @param typeId [in] - TypeId of the enumeration
         * @return EnumDescription or NULL if typeId does not exist or is an enum type.
         */
        virtual const EnumDescription* GetEnum(DotsC_TypeId typeId) const=0;

        /**
         * @brief Get number of enumeration types.
         * @return Number of types.
         */
        virtual int GetNumberOfEnums() const=0;

        /**
         * @brief Get a set of all typeIds that represent enumeration types.
         * @param typeIds [out] - Set of typeIds.
         */
        virtual void GetAllEnumTypeIds(std::set<DotsC_TypeId>& typeIds) const=0;

        //-----------------
        // Properties
        //-----------------
        /**
         * @brief Get property description by typeId.
         * @param typeId [in] - TypeId of the property.
         * @return PropertyDescription or NULL if typeId does not exist or is a property type.
         */
        virtual const PropertyDescription* GetProperty(DotsC_TypeId typeId) const=0;

        /**
         * @brief Get number of property types.
         * @return Number of types.
         */
        virtual int GetNumberOfProperties() const=0;

        /**
         * @brief Get a set of all typeIds that represent property types.
         * @param typeIds [out] - Set of typeIds.
         */
        virtual void GetAllPropertyTypeIds(std::set<DotsC_TypeId>& typeIds) const=0;

        //-----------------
        // Classes
        //-----------------
        /**
         * @brief Get class description by typeId.
         * @param typeId [in] - TypeId of the class.
         * @return ClassDescription or NULL if typeId does not exist or is a class type.
         */
        virtual const ClassDescription* GetClass(DotsC_TypeId typeId) const=0;

        /**
         * @brief Get number of class types.
         * @return Number of types.
         */
        virtual int GetNumberOfClasses() const=0;

        /**
         * @brief Get a set of all typeIds that represent class types.
         * @param typeIds [out] - Set of typeIds.
         */
        virtual void GetAllClassTypeIds(std::set<DotsC_TypeId>& typeIds) const=0;

        //-----------------
        // Exceptions
        //-----------------
        /**
         * @brief Get exception description by typeId.
         * @param typeId [in] - TypeId of the class.
         * @return ExceptionDescription or NULL if typeId does not exist or is a class type.
         */
        virtual const ExceptionDescription* GetException(DotsC_TypeId typeId) const=0;

        /**
         * @brief Get number of exception types.
         * @return Number of types.
         */
        virtual int GetNumberOfExceptions() const=0;

        /**
         * @brief Get a set of all typeIds that represent exception types.
         * @param typeIds [out] - Set of typeIds.
         */
        virtual void GetAllExceptionTypeIds(std::set<DotsC_TypeId>& typeIds) const=0;
    };

    /**
     * TypeTraits used to deduce specific types when TypeRepository is used in template code.
     */
    template<> struct TypeRepositoryTraits<TypeRepository>
    {
        typedef TypeRepository RepositoryType;
        typedef ClassDescription ClassDescriptionType;
        typedef MemberDescription MemberDescriptionType;
        typedef PropertyDescription PropertyDescriptionType;
        typedef ExceptionDescription ExceptionDescriptionType;
        typedef ParameterDescription ParameterDescriptionType;
        typedef EnumDescription EnumDescriptionType;
        typedef MemberMappingDescription MemberMappingDescriptionType;
        typedef PropertyMappingDescription PropertyMappingDescriptionType;
        typedef CreateRoutineDescription CreateRoutineDescriptionType;
    };
}
}
}
} //end namespace Safir::Dob::Typesystem::Internal

#endif
