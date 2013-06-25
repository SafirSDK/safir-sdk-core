/******************************************************************************
*
* Copyright Saab AB, 2004-2013 (http://www.safirsdk.com)
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
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/
#ifndef __DOTS_INTERNAL_TYPEREPOSITORY_H__
#define __DOTS_INTERNAL_TYPEREPOSITORY_H__

#include <vector>
#include <string>
#include <Safir/Dob/Typesystem/Internal/KernelDefs2.h>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    //forward declarations
    class ClassDescription;
    class MemberDescription;

    class PropertyDescription
    {
    public:
        virtual const char* FileName() const=0;
        virtual const char* Summary() const=0;
        virtual DotsC_TypeId GetTypeId() const=0;
        virtual const char* GetName() const=0;
        virtual int GetNumberOfMembers() const=0;
        virtual DotsC_MemberIndex GetMemberIndex(const std::string& memberName) const=0;
        virtual const MemberDescription* GetMember(DotsC_MemberIndex index) const=0;
    };

    class ExceptionDescription
    {
    public:
        virtual const char* FileName() const=0;
        virtual const char* Summary() const=0;
        virtual DotsC_TypeId GetTypeId() const=0;
        virtual const char* GetName() const=0;
        virtual const ExceptionDescription* GetBaseClass() const=0;
    };

    class ParameterDescription
    {
    public:
        virtual const char* Summary() const=0;
        virtual const char* GetName() const=0;
        virtual DotsC_MemberType GetMemberType() const=0;
        virtual DotsC_TypeId GetTypeId() const=0; //only valid if MemberType is object or enum
        virtual bool IsArray() const=0;
        virtual int GetArraySize() const=0;
        virtual bool IsHidden() const=0;

        //Get parameter values - depending on actual type of the parameter
        //For entityId use GetInt64Value for typeId and GetHashedValue for instanceId
        virtual boost::int32_t GetInt32Value(int index) const=0; //int32, enum
        virtual boost::int64_t GetInt64Value(int index) const=0; //int64, typeId
        virtual float GetFloat32Value(int index) const=0; //float32, si32
        virtual double GetFloat64Value(int index) const=0; //float64, si64
        virtual bool GetBoolValue(int index) const=0;
        virtual const char* GetStringValue(int index) const=0;
        virtual const char* GetObjectValue(int index) const=0;
        virtual std::pair<const char*, size_t> GetBinaryValue(int index) const=0;
        virtual std::pair<boost::int64_t, const char*> GetHashedValue(int index) const=0; //instanceId, channelId, handlerId
    };

    class EnumDescription
    {
    public:
        virtual const char* FileName() const=0;
        virtual const char* Summary() const=0;
        virtual DotsC_TypeId GetTypeId() const=0;
        virtual const char* GetName() const=0;
        virtual DotsC_TypeId GetCheckSum() const=0;
        virtual int GetNumberOfValues() const=0;
        virtual const char* GetValueName(DotsC_EnumerationValue val) const=0;
        virtual int GetIndexOfValue(const std::string& valueName) const=0;
    };

    class MemberDescription
    {
    public:
        virtual const char* Summary() const=0;
        virtual DotsC_TypeId GetTypeId() const=0; //only valid if MemberType is object or enum
        virtual const char* GetName() const=0;
        virtual DotsC_MemberType GetMemberType() const=0;
        virtual const bool IsArray() const=0;
        virtual int GetArraySize() const=0;
        virtual int GetMaxLength() const=0; //only valid if memberType is String
    };

    class MemberMappingDescription
    {
    public:
        virtual DotsC_PropertyMappingKind GetMappingKind() const=0;
        virtual std::pair<const ParameterDescription*, int /*paramIndex*/> GetParameter() const=0; //if mapped to parameter. If paramIndex<0, whole array is.

        //if mapped to member
        virtual int MemberReferenceDepth() const=0;
        virtual std::pair<DotsC_MemberIndex, DotsC_ArrayIndex> GetMemberReference(int depth) const=0;
    };

    class PropertyMappingDescription
    {
    public:
        virtual const char* FileName() const=0;
        virtual const char* Summary() const=0;
        virtual const PropertyDescription* GetProperty() const=0;
        virtual const ClassDescription* GetClass() const=0;
        virtual const MemberMappingDescription* GetMemberMapping(int propertyMemberIndex) const=0;
    };

    class CreateRoutineDescription
    {
    public:
        virtual const char* Summary() const=0;
        virtual const char* GetName() const=0;

        virtual int GetNumberOfInParameters() const=0;
        virtual const MemberDescription* GetInParameterMember(int index) const=0;

        virtual int GetNumberOfDefaultValues() const=0;
        virtual const MemberDescription* GetDefaultValueMember(int index) const=0;
        virtual std::pair<const ParameterDescription*, int /*paramIndex*/> GetDefaultValue(int index) const=0;
    };

    class ClassDescription
    {
    public:
        virtual const char* FileName() const=0;
        virtual const char* Summary() const=0;
        virtual DotsC_TypeId GetTypeId() const=0;
        virtual const char* GetName() const=0;
        virtual const ClassDescription* GetBaseClass() const=0;
        virtual int GetNumberOfDescendants() const=0;
        virtual const ClassDescription* GetDescendant(int index) const=0;
        virtual int GetNumberOfMembers() const=0;
        virtual int GetNumberOfOwnMembers() const=0;
        virtual int GetNumberOfInheritedMembers() const=0;
        virtual DotsC_MemberIndex GetMemberIndex(const std::string& memberName) const=0;
        virtual const MemberDescription* GetMember(DotsC_MemberIndex index) const=0;
        virtual int GetNumberOfParameters() const=0;
        virtual int GetNumberOfOwnParameters() const=0;
        virtual int GetNumberOfInheritedParameters() const=0;
        virtual const ParameterDescription* GetParameter(DotsC_ParameterIndex index) const=0;
        virtual void GetPropertyIds(std::vector<DotsC_TypeId>& propertyIds) const=0;
        virtual const PropertyMappingDescription* GetPropertyMapping(DotsC_TypeId propertyTypeId, bool & isInherited) const=0;
        virtual int GetNumberOfCreateRoutines() const=0;
        virtual const CreateRoutineDescription* GetCreateRoutine(int index) const=0;
        virtual int InitialSize() const=0;
        virtual int OwnSize() const=0;
    };

    /**
     * @brief The TypeRepository class is the baseclass of any TypeRepository implementation.
     */
    class TypeRepository
    {
    public:        
        //Enmerations
        virtual const EnumDescription* GetEnum(DotsC_TypeId typeId) const=0;
        virtual size_t GetNumberOfEnums() const=0;
        virtual void GetAllEnumTypeIds(std::vector<DotsC_TypeId>& typeIds) const=0;

        //properties
        virtual const PropertyDescription* GetProperty(DotsC_TypeId typeId) const=0;
        virtual size_t GetNumberOfProperties() const=0;
        virtual void GetAllPropertyTypeIds(std::vector<DotsC_TypeId>& typeIds) const=0;

        //classes
        virtual const ClassDescription* GetClass(DotsC_TypeId typeId) const=0;
        virtual size_t GetNumberOfClasses() const=0;
        virtual void GetAllClassTypeIds(std::vector<DotsC_TypeId>& typeIds) const=0;

        //exceptions
        virtual const ExceptionDescription* GetException(DotsC_TypeId typeId) const=0;
        virtual size_t GetNumberOfExceptions() const=0;
        virtual void GetAllExceptionTypeIds(std::vector<DotsC_TypeId>& typeIds) const=0;
    };
}
}
}
} //end namespace Safir::Dob::Typesystem::Internal

#endif
