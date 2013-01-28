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
#include <sstream>
#include <algorithm>
#include "RepositoryWrapper.h"
#include "InternalDefs.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    RepositoryWrapper::RepositoryWrapper(const ParseState& state) : m_rawResult(state.result),
                                                                    m_exceptionDef(),
                                                                    m_fundamentalExceptionDef(),
                                                                    m_enums(),
                                                                    m_classes(),
                                                                    m_properties(),
                                                                    m_exceptions(),
                                                                    m_parameters()
    {
        //Add predefined types
        m_exceptionDef.name=BasicTypes::ExceptionName;
        DotsC_TypeId typeId = DotsId_Generate64(m_exceptionDef.name.c_str());
        m_exceptions.insert(std::make_pair(typeId, ExceptionDescriptionWrapper(&m_exceptionDef, typeId)));

        m_fundamentalExceptionDef.name=BasicTypes::FundamentalExceptionName;
        typeId = DotsId_Generate64(m_fundamentalExceptionDef.name.c_str());
        m_exceptions.insert(std::make_pair(typeId, ExceptionDescriptionWrapper(&m_fundamentalExceptionDef, typeId)));

        //Insert all types from raw result
        SetupWithTypeId(m_rawResult->enumerations, m_enums);
        SetupWithTypeId(m_rawResult->exceptions, m_exceptions);
        SetupWithTypeId(m_rawResult->properties, m_properties);
        SetupWithTypeId(m_rawResult->classes, m_classes);
        
        //Setup Exception baseclass ptr
        for (boost::unordered_map<DotsC_TypeId, ExceptionDescriptionWrapper>::iterator it=m_exceptions.begin(); it!=m_exceptions.end(); ++it)
        {
            DotsC_TypeId baseTypeId = DotsId_Generate64(it->second.m_def->baseClass.c_str());
            it->second.m_base=GetPtr(m_exceptions, baseTypeId); //Will return NULL if not found wich is ok, and will happen when exception inherits from preset classes. The validation has already been done.
        }

        //Property members
        for (boost::unordered_map<DotsC_TypeId, PropertyDescriptionWrapper>::iterator it=m_properties.begin(); it!=m_properties.end(); ++it)
        {
            CreateMembers(it->second);
        }

        //classes
        for (boost::unordered_map<DotsC_TypeId, ClassDescriptionWrapper>::iterator it=m_classes.begin(); it!=m_classes.end(); ++it)
        {            
            //Set base class
            DotsC_TypeId baseTypeId = DotsId_Generate64(it->second.m_def->baseClass.c_str());
            ClassDescriptionWrapper* bc=GetPtr(m_classes, baseTypeId);
            if (bc)
            {
                bc->m_descendants.push_back(&(it->second));
                it->second.m_base=bc;
            }

            //Create members
            CreateMembers(it->second);

            //CreateRoutines
            std::for_each(it->second.m_def->createRoutines.cbegin(), it->second.m_def->createRoutines.cend(), [&](const CreateRoutineDefinition& crd)
            {
                it->second.m_createRoutines.push_back(CreateRoutineDescriptionWrapper(&crd, &(it->second)));

            });

            //Parameters            
            it->second.m_allParameters=&m_parameters;
            for (ParameterDefinitions::const_iterator paramIt=it->second.m_def->parameters.begin(); paramIt!=it->second.m_def->parameters.end(); ++paramIt)
            {
                std::string fullParameterName=it->second.m_def->name+"."+paramIt->name;
                m_parameters.insert( boost::unordered_map<std::string, ParameterDescriptionWrapper>::value_type(fullParameterName, ParameterDescriptionWrapper(&(*paramIt), fullParameterName)) );
                it->second.m_ownParameters.push_back(fullParameterName);
            }
        }

        //propertyMappings
        for (PropertyMappingDefinitions::const_iterator it=m_rawResult->propertyMappings.begin(); it!=m_rawResult->propertyMappings.end(); ++it)
        {
            ClassDescriptionWrapper* cls=GetPtr(m_classes, DotsId_Generate64(it->className.c_str()));
            PropertyDescriptionWrapper* prop=GetPtr(m_properties, DotsId_Generate64(it->propertyName.c_str()));
            PropertyMappingDescriptionWrapper propMapping(&(*it), prop, cls);

            for (MappedMemberDefinitions::const_iterator memberIt=it->mappedMembers.begin(); memberIt!=it->mappedMembers.end(); ++memberIt)
            {
                MemberMappingWrapper member(&(*memberIt));
                switch (member.GetMappingKind())
                {
                case MappedToParameter:
                {
                    member.m_paramIndex=member.m_def->memberReferences[0].second;
                    boost::unordered_map<std::string, ParameterDescriptionWrapper>::iterator paramIt = m_parameters.find(member.m_def->memberReferences[0].first);
                    ENSURE(paramIt!=m_parameters.end(), <<"Cant find parameter "<<member.m_def->memberReferences[0].first);
                    member.m_paramRef=&(paramIt->second);
                }
                    break;
                case MappedToMember:
                {
                    const ClassDescription* mappedClass=cls;
                    for (MemberReferenceVector::const_iterator memberRefIt=memberIt->memberReferences.begin(); memberRefIt!=memberIt->memberReferences.end(); ++memberRefIt)
                    {
                        ENSURE(mappedClass!=NULL, <<"Invalid memberMapping for member "<<memberIt->name<<". Class: "<<cls->GetName()<<". Property: "<<prop->GetName());

                        const std::string& classMemberName = memberRefIt->first;
                        const int& arrayIndex = memberRefIt->second;

                        int memberIndex = mappedClass->GetMemberIndex(classMemberName);
                        ENSURE(memberIndex>=0, <<"Cant find mapped member "<<classMemberName<<" in class "<<cls->GetName());

                        const MemberDescription* mappedMember = mappedClass->GetMember(memberIndex);

                        member.m_memberRef.push_back( std::make_pair(memberIndex, arrayIndex) );

                        if (mappedMember->GetMemberType()==ObjectMemberType)
                        {
                            mappedClass= GetPtr(m_classes, mappedMember->GetTypeId()); //Get class description for the type of the mapped member and continue
                        }
                    }
                }
                    break;
                default: //Mapped to NULL
                    break;
                }

                propMapping.m_memberMappings.push_back(member);
            }

            cls->m_properties.push_back(propMapping);
        }
    }

    //---------------------------------------------------
    // Description wrapper implementation
    //---------------------------------------------------
    
    //Class
    //---------
    const ParameterDescription* RepositoryWrapper::ClassDescriptionWrapper::GetParameter(DotsC_ParameterIndex index) const
    {
        int numInherited=GetNumberOfInheritedParameters();
        if (index<numInherited)
        {
            return m_base->GetParameter(index);
        }

        return GetPtr<std::string, ParameterDescriptionWrapper>(*m_allParameters, m_ownParameters[index-numInherited]);
    }

    void RepositoryWrapper::ClassDescriptionWrapper::GetPropertyIds(std::vector<DotsC_TypeId>& propertyIds) const
    {
        if (m_base)
        {
            m_base->GetPropertyIds(propertyIds);
        }

        for (std::vector<PropertyMappingDescriptionWrapper>::const_iterator it=m_properties.begin(); it!=m_properties.end(); ++it)
        {
            propertyIds.push_back(it->m_property->GetTypeId());
        }
    }

    const PropertyMappingDescription* RepositoryWrapper::ClassDescriptionWrapper::GetPropertyMapping(DotsC_TypeId propertyType, bool & isInherited) const
    {
        for (std::vector<PropertyMappingDescriptionWrapper>::const_iterator it=m_properties.begin(); it!=m_properties.end(); ++it)
        {
            if (it->GetProperty()->GetTypeId()==propertyType)
            {
                isInherited=false;
                return &(*it);
            }
        }        
        
        if (m_base)
        {            
            const PropertyMappingDescription* tmp=m_base->GetPropertyMapping(propertyType, isInherited);
            isInherited=true;
            return tmp;
        }

        return NULL;        
    }

    DotsC_MemberIndex RepositoryWrapper::ClassDescriptionWrapper::GetMemberIndex(const std::string& memberName) const
    {
        for (std::vector<MemberDescriptionWrapper>::const_iterator it=m_members.begin(); it!=m_members.end(); ++it)
        {
            if (it->GetName()==memberName)
            {
                return std::distance(m_members.begin(), it) + GetNumberOfInheritedMembers();
            }
        }

        if (m_base)
        {
            return m_base->GetMemberIndex(memberName);
        }

        return -1;
    }

    const MemberDescription* RepositoryWrapper::ClassDescriptionWrapper::GetMember(DotsC_MemberIndex index) const 
    {
        int numInherited=GetNumberOfInheritedMembers();
        if (index<numInherited)
        {
            return m_base->GetMember(index);
        }
        return &m_members[index-numInherited];
    }


    //properties
    //-----------
    DotsC_MemberIndex RepositoryWrapper::PropertyDescriptionWrapper::GetMemberIndex(const std::string& memberName) const
    {
        for (std::vector<MemberDescriptionWrapper>::const_iterator it=m_members.begin(); it!=m_members.end(); ++it)
        {
            if (it->GetName()==memberName)
            {
                return std::distance(m_members.begin(), it);
            }
        }
        return -1;
    }

    //enumerations
    //-------------
    RepositoryWrapper::EnumDescriptionWrapper::EnumDescriptionWrapper(const EnumerationDefinition* def, DotsC_TypeId typeId) : m_def(def), m_typeId(typeId)
    {
        std::ostringstream ss;
        for (StringVector::const_iterator it=m_def->enumerationValues.begin(); it!=m_def->enumerationValues.end(); ++it)
        {
            ss<<*it;
        }
        m_checksum=DotsId_Generate64(ss.str().c_str());
    }

    int RepositoryWrapper::EnumDescriptionWrapper::GetIndexOfValue(const std::string& valueName) const
    {
        for (int i=0; i<static_cast<int>(m_def->enumerationValues.size()); ++i)
        {
            if (m_def->enumerationValues[i]==valueName)
                return i;
        }
        return -1;
    }

    //Member
    //---------
    DotsC_TypeId RepositoryWrapper::MemberDescriptionWrapper::GetTypeId() const
    {
        switch (m_def->memberType)
        {
        case ObjectMemberType:
            return m_class->GetTypeId();
        case EnumerationMemberType:
            return m_enum->GetTypeId();
        default:
            ENSURE(false, << "Only Object or Enum member descriptions have TypeIds! Type = " << BasicTypes::Instance().StringOf(m_def->memberType));
            return -1;
        }
    }

    //Parameter
    //------------
    RepositoryWrapper::ParameterDescriptionWrapper::ParameterDescriptionWrapper(const ParameterDefinition* def, const std::string& fullName) : m_def(def), m_fullParameterName(fullName)
    {
        if (m_def->memberType==ObjectMemberType || m_def->memberType==EnumerationMemberType)
        {
            m_typeId=DotsId_Generate64(m_def->typeName.c_str());
        }
    }

    //CreateRoutines
    //----------------
    const MemberDescription* RepositoryWrapper::CreateRoutineDescriptionWrapper::GetInParameterMember(int index) const
    {
        return m_class->GetMember(m_class->GetMemberIndex(m_def->parameters[index]));
    }

    const MemberDescription* RepositoryWrapper::CreateRoutineDescriptionWrapper::GetDefaultValueMember(int index) const
    {
        const std::pair<std::string, std::pair<std::string, int> >& ref=m_def->memberValues[index];
        return m_class->GetMember(m_class->GetMemberIndex(ref.first));
    }

    std::pair<const ParameterDescription*, int /*paramIndex*/> RepositoryWrapper::CreateRoutineDescriptionWrapper::GetDefaultValue(int index) const
    {
        const std::pair<std::string, std::pair<std::string, int> >& ref=m_def->memberValues[index];
        const ParameterDescription* par= GetPtr<std::string, ParameterDescriptionWrapper>(*(m_class->m_allParameters), ref.second.first);
        return std::make_pair(par, ref.second.second);
    }
}
}
}
}
