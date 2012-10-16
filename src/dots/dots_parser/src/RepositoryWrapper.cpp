#include <sstream>
#include <algorithm>
#include <boost/bind.hpp>
#include <Safir/Dob/Typesystem/Internal/Id.h>
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
    template <class Key, class Val>
    const Val* GetPtr(const std::map<Key, Val> & m, Key key)
    {
        std::map<Key, Val>::const_iterator it = m.find(key);
        if (it!=m.end())
        {
            return &(it->second);
        }
        return NULL;
    }

    template <class Key, class Val>
    Val* GetPtr(std::map<Key, Val> & m, Key key)
    {
        std::map<Key, Val>::iterator it = m.find(key);
        if (it!=m.end())
        {
            return &(it->second);
        }
        return NULL;
    }

    template <class Key, class Val>
    void GetKeys(const std::map<Key, Val>& m, std::vector<Key>& keys)
    {        
        for (std::map<Key, Val>::const_iterator it=m.begin(); it!=m.end(); ++it)
        {
            keys.push_back(it->first);
        }
    }

    template <class Def, class Descr>
    void SetupWithTypeId(const std::vector<Def>& src, std::map<DotsC_TypeId, Descr>& dest)
    {
        for (std::vector<Def>::const_iterator it=src.begin(); it!=src.end(); ++it)
        {
            DotsC_TypeId typeId = DotsId_Generate64(it->Name.c_str());
            dest.insert(std::map<DotsC_TypeId, Descr>::value_type(typeId, Descr(*it, typeId)));
        }
    }        

    RepositoryWrapper::RepositoryWrapper(RawParseResultConstPtr rawResult) : m_rawResult(rawResult), m_parameters()
    {
        SetupWithTypeId(m_rawResult->Enumerations, m_enums);
        SetupWithTypeId(m_rawResult->Exceptions, m_exceptions);
        SetupWithTypeId(m_rawResult->Properties, m_properties);
        SetupWithTypeId(m_rawResult->Classes, m_classes);
        
        //Setup Exception baseclass ptr
        for (std::map<DotsC_TypeId, ExceptionDescriptionWrapper>::iterator it=m_exceptions.begin(); it!=m_exceptions.end(); ++it)
        {
            DotsC_TypeId baseTypeId = DotsId_Generate64(it->second.m_def.BaseClass.c_str());
            it->second.m_base=GetPtr(m_exceptions, baseTypeId); //Will return NULL if not found wich is ok, and will happen when exception inherits from preset classes. The validation has already been done.
        }

        //Property members
        for (std::map<DotsC_TypeId, PropertyDescriptionWrapper>::iterator it=m_properties.begin(); it!=m_properties.end(); ++it)
        {
            CreateMembers(it->second);
        }

        //Classes
        for (std::map<DotsC_TypeId, ClassDescriptionWrapper>::iterator it=m_classes.begin(); it!=m_classes.end(); ++it)
        {            
            //Set base class
            DotsC_TypeId baseTypeId = DotsId_Generate64(it->second.m_def.BaseClass.c_str());
            ClassDescriptionWrapper* bc=GetPtr(m_classes, baseTypeId);
            if (bc)
            {
                bc->m_descendants.push_back(&(it->second));
                it->second.m_base=bc;
            }

            //Create members
            CreateMembers(it->second);

            //Parameters            
            it->second.m_allParameters=&m_parameters;
            for (ParameterDefinitions::const_iterator paramIt=it->second.m_def.Parameters.begin(); paramIt!=it->second.m_def.Parameters.end(); ++paramIt)
            {
                m_parameters.insert( std::map<std::string, ParameterDescriptionWrapper>::value_type(paramIt->Name, ParameterDescriptionWrapper(*paramIt)) );
                it->second.m_ownParameters.push_back(paramIt->Name);
            }
        }

        //PropertyMappings        
        for (PropertyMappingDefinitions::const_iterator it=m_rawResult->PropertyMappings.begin(); it!=m_rawResult->PropertyMappings.end(); ++it)
        {
            ClassDescriptionWrapper* cls=GetPtr(m_classes, DotsId_Generate64(it->ClassName.c_str()));
            PropertyDescriptionWrapper* prop=GetPtr(m_properties, DotsId_Generate64(it->PropertyName.c_str()));
            PropertyMappingDescriptionWrapper propMapping(*it, prop);

            for (MappedMemberDefinitions::const_iterator memberIt=it->MappedMembers.begin(); memberIt!=it->MappedMembers.end(); ++memberIt)
            {
                MemberMappingWrapper member(*memberIt);
                switch (member.GetMappingKind())
                {
                case MappedToParameter:
                    {
                        member.m_valueParameter.m_name=it->PropertyName+std::string("@")+memberIt->Name+std::string("-")+it->ClassName;
                        member.m_valueParameter.m_value=memberIt->Value;                        

                        //Find out memberType
                        for (std::vector<MemberDescriptionWrapper>::const_iterator propertyMemberIt=prop->m_members.begin(); propertyMemberIt!=prop->m_members.end(); ++propertyMemberIt)
                        {
                            if (propertyMemberIt->GetName()==memberIt->Name)
                            {
                                member.m_valueParameter.m_memberType=propertyMemberIt->GetMemberType();
                                if (member.m_valueParameter.m_memberType==ObjectMemberType)
                                {
                                    member.m_valueParameter.m_typeId=propertyMemberIt->GetClass()->GetTypeId();
                                }
                                else if (member.m_valueParameter.m_memberType==EnumerationMemberType)
                                {
                                    member.m_valueParameter.m_typeId=propertyMemberIt->GetEnum()->GetTypeId();
                                }
                                break;
                            }
                        }
                    }
                    break;
                case MappedToMember:
                    {
                        const ClassDescription* mappedClass=cls;
                        for (MemberReferenceVector::const_iterator memberRefIt=memberIt->MemberReferences.begin(); memberRefIt!=memberIt->MemberReferences.end(); ++memberRefIt)
                        {
                            ENSURE(mappedClass!=NULL, <<"Invalid memberMapping for member "<<memberIt->Name<<". Class: "<<cls->GetName()<<". Property: "<<prop->GetName());

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

    RepositoryWrapper::~RepositoryWrapper(void)
    {
    }

    //Enmerations
    const EnumDescription* RepositoryWrapper::GetEnum(DotsC_TypeId typeId) const
    {        
        return GetPtr(m_enums, typeId);
    }

    size_t RepositoryWrapper::GetNumberOfEnums() const
    {
        return m_enums.size();
    }

    std::vector<DotsC_TypeId> RepositoryWrapper::GetAllEnumTypeIds() const
    {
        std::vector<DotsC_TypeId> res;
        GetKeys(m_enums, res);
        return res;
    }

    //Properties
    const PropertyDescription* RepositoryWrapper::GetProperty(DotsC_TypeId typeId) const
    {
        return GetPtr(m_properties, typeId);
    }

    size_t RepositoryWrapper::GetNumberOfProperties() const
    {
        return m_properties.size();
    }

    std::vector<DotsC_TypeId> RepositoryWrapper::GetAllPropertyTypeIds() const
    {
        std::vector<DotsC_TypeId> res;
        GetKeys(m_properties, res);
        return res;
    }

    //Classes
    const ClassDescription* RepositoryWrapper::GetClass(DotsC_TypeId typeId) const
    {
        return GetPtr(m_classes, typeId);
    }

    size_t RepositoryWrapper::GetNumberOfClasses() const
    {
        return m_classes.size();
    }

    std::vector<DotsC_TypeId> RepositoryWrapper::GetAllClassTypeIds() const
    {
        std::vector<DotsC_TypeId> res;
        GetKeys(m_classes, res);
        return res;
    }

    //Exceptions
    const ExceptionDescription* RepositoryWrapper::GetException(DotsC_TypeId typeId) const
    {
        return GetPtr(m_exceptions, typeId);
    }

    size_t RepositoryWrapper::GetNumberOfExceptions() const
    {
        return m_exceptions.size();
    }

    std::vector<DotsC_TypeId> RepositoryWrapper::GetAllExceptionTypeIds() const
    {
        std::vector<DotsC_TypeId> res;
        GetKeys(m_exceptions, res);
        return res;
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

    int RepositoryWrapper::ClassDescriptionWrapper::GetNumberOfProperties() const
    {
        int size=static_cast<int>(m_properties.size());
        if (m_base)
        {
            size+=m_base->GetNumberOfProperties();
        }
        return size;
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


    //Properties
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

    //Enumerations
    //-------------
    RepositoryWrapper::EnumDescriptionWrapper::EnumDescriptionWrapper(const EnumerationDefinition& def, DotsC_TypeId typeId) : m_def(def), m_typeId(typeId)
    {
        std::ostringstream ss;
        for (StringVector::const_iterator it=m_def.EnumerationValues.begin(); it!=m_def.EnumerationValues.end(); ++it)
        {
            ss<<*it;
        }
        m_checksum=DotsId_Generate64(ss.str().c_str());
    }

    int RepositoryWrapper::EnumDescriptionWrapper::GetIndexOfValue(const std::string& valueName) const
    {
        for (int i=0; i<m_def.EnumerationValues.size(); ++i)
        {
            if (m_def.EnumerationValues[i]==valueName)
                return i;
        }
        return -1;
    }

    //Member
    //---------
    DotsC_TypeId RepositoryWrapper::MemberDescriptionWrapper::GetTypeId() const
    {
        switch (m_def.MemberType)
        {
        case ObjectMemberType:
            return m_class->GetTypeId();
        case EnumerationMemberType:
            return m_enum->GetTypeId();
        default:
            ENSURE(false, << "Only Object or Enum member descriptions have TypeIds! Type = " << BasicTypes::Instance().StringOf(m_def.MemberType));
            return -1;
        }
    }

    //Parameter
    //------------
    RepositoryWrapper::ParameterDescriptionWrapper::ParameterDescriptionWrapper(const ParameterDefinition& def) : m_def(def) 
    {
        if (m_def.MemberType==ObjectMemberType || m_def.MemberType==EnumerationMemberType)
        {
            m_typeId=DotsId_Generate64(m_def.TypeName.c_str());
        }
    }

}
}
}
}
