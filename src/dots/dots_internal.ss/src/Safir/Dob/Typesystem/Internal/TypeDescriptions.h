/******************************************************************************
*
* Copyright Saab AB, 2004-2012 (http://www.safirsdk.com)
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
#ifndef __DOTS_TYPE_DESCRIPTIONS_H__
#define __DOTS_TYPE_DESCRIPTIONS_H__

#include <string>
#include <Safir/Dob/Typesystem/Internal/KernelDefs.h>

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
        virtual const std::string& FileName() const = 0;
        virtual const std::string& Summary() const = 0;
        virtual DotsC_TypeId GetTypeId() const = 0;
        virtual const std::string& GetName() const = 0;
        virtual int GetNumberOfMembers() const = 0;
        virtual DotsC_MemberIndex GetMemberIndex(const std::string& memberName) const = 0;
        virtual const MemberDescription* GetMember(DotsC_MemberIndex index) const = 0;
    };
    
    class ExceptionDescription
    {
    public:
        virtual const std::string& FileName() const = 0;
        virtual const std::string& Summary() const = 0;
        virtual DotsC_TypeId GetTypeId() const = 0;
        virtual const std::string& GetName() const = 0;
        virtual const ExceptionDescription* GetbaseClass() const = 0;
    };

    class ParameterDescription
    {
    public:
        virtual const std::string& Summary() const = 0;
        virtual const std::string& GetName() const = 0;
        virtual DotsC_MemberType GetMemberType() const = 0;
        virtual DotsC_TypeId GetTypeId() const = 0; //only valid if MemberType is object or enum
        virtual bool IsArray() const = 0;
        virtual int GetarraySize() const = 0;
        //GetValue<T>
    };

    class EnumDescription
    {
    public:
        virtual const std::string& FileName() const = 0;
        virtual const std::string& Summary() const = 0;
        virtual DotsC_TypeId GetTypeId() const = 0;
        virtual const std::string& GetName() const = 0;
        virtual DotsC_TypeId GetCheckSum() const = 0;
        virtual int GetNumberOfValues() const = 0;
        virtual const std::string& GetValueName(DotsC_EnumerationValue val) const = 0;
        virtual int GetIndexOfValue(const std::string& valueName) const = 0;
    };

    class MemberDescription
    {
    public:
        virtual const std::string& Summary() const = 0;
        virtual DotsC_TypeId GetTypeId() const = 0;
        virtual const std::string& GetName() const = 0;
        virtual DotsC_MemberType GetMemberType() const = 0;
        virtual const ClassDescription* GetClass() const = 0;
        virtual const EnumDescription* GetEnum() const = 0;
        virtual const bool IsArray() const = 0;
        virtual int GetarraySize() const = 0;
        //GetDataLength
    };

    class MemberMappingDescription
    {
    public:
        virtual DotsC_PropertyMappingKind GetMappingKind() const = 0;
        virtual const ParameterDescription * GetParameter() const = 0; //if mapped to parameter

        //if mapped to member
        virtual int MemberReferenceDepth() const = 0;
        virtual std::pair<DotsC_MemberIndex, DotsC_ArrayIndex> GetMemberReference(int depth) const = 0;
    };

    class PropertyMappingDescription
    {
    public:
        virtual const std::string& FileName() const = 0;
        virtual const std::string& Summary() const = 0;
        virtual const PropertyDescription* GetProperty() const = 0;
        virtual const ClassDescription* GetClass() const = 0;
        virtual int GetNumberOfMappings() const = 0;
        virtual const MemberMappingDescription* GetMapping(int index) const = 0;
    };

    class CreateRoutineDescription
    {
    public:
        virtual const std::string& Summary() const = 0;
        virtual const std::string& GetName() const = 0;
        virtual const ClassDescription* GetClass() const = 0;

        virtual int GetNumberOfInParameters() const = 0;
        virtual const MemberDescription* GetInParameterMember(int index) const = 0;

        virtual int GetNumberOfDefaultValues() const = 0;
        virtual const MemberDescription* GetDefaultValueMember(int index) const = 0;        
        virtual std::pair<const ParameterDescription*, int /*paramIndex*/> GetDefaultValue(int index) const = 0;
    };

    class ClassDescription
    {
    public:
        virtual const std::string& FileName() const = 0;
        virtual const std::string& Summary() const = 0;
        virtual DotsC_TypeId GetTypeId() const = 0;
        virtual const std::string& GetName() const = 0;
        virtual const ClassDescription* GetbaseClass() const = 0;
        virtual int GetNumberOfDescendants() const = 0;
        virtual const ClassDescription* GetDescendant(int index) const = 0;
        virtual int GetNumberOfMembers() const = 0;
        virtual int GetNumberOfOwnMembers() const = 0;
        virtual int GetNumberOfInheritedMembers() const = 0;
        virtual DotsC_MemberIndex GetMemberIndex(const std::string& memberName) const = 0;
        virtual const MemberDescription* GetMember(DotsC_MemberIndex index) const = 0;
        virtual int GetNumberOfParameters() const = 0;
        virtual int GetNumberOfOwnParameters() const = 0;
        virtual int GetNumberOfInheritedParameters() const = 0;
        virtual const ParameterDescription* GetParameter(DotsC_ParameterIndex index) const = 0;
        virtual void GetPropertyIds(std::vector<DotsC_TypeId>& propertyIds) const = 0;
        virtual const PropertyMappingDescription* GetPropertyMapping(DotsC_TypeId propertyType, bool & isInherited) const = 0;
        virtual int GetNumberOfCreateRoutines() const = 0;
        virtual const CreateRoutineDescription* GetCreateRoutine(int index) const = 0;
        virtual int InitialSize() const = 0;
        virtual int OwnSize() const = 0;
    };
}
}
}
}

#endif
