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
#ifndef __DOTS_REPOSITORY_WRAPPER_H__
#define __DOTS_REPOSITORY_WRAPPER_H__

#include <map>
#include <Safir/Dob/Typesystem/Internal/Id.h>
#include <Safir/Dob/Typesystem/Internal/TypeRepository.h>
#include <Safir/Dob/Typesystem/Internal/ParseResult.h>
#include "BasicTypes.h"

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
        typename std::map<Key, Val>::const_iterator it = m.find(key);
        if (it!=m.end())
        {
            return &(it->second);
        }
        return NULL;
    }

    template <class Key, class Val>
    Val* GetPtr(std::map<Key, Val> & m, Key key)
    {
        typename std::map<Key, Val>::iterator it = m.find(key);
        if (it!=m.end())
        {
            return &(it->second);
        }
        return NULL;
    }

    template <class Key, class Val>
    void GetKeys(const std::map<Key, Val>& m, std::vector<Key>& keys)
    {
        for (typename std::map<Key, Val>::const_iterator it=m.begin(); it!=m.end(); ++it)
        {
            keys.push_back(it->first);
        }
    }

    template <class Def, class Descr>
    void SetupWithTypeId(const std::vector<Def>& src, std::map<DotsC_TypeId, Descr>& dest)
    {
        for (typename std::vector<Def>::const_iterator it=src.begin(); it!=src.end(); ++it)
        {
            DotsC_TypeId typeId = DotsId_Generate64(it->Name.c_str());
            dest.insert(typename std::map<DotsC_TypeId, Descr>::value_type(typeId, Descr(&(*it), typeId)));
        }
    }

    class RepositoryWrapper : public TypeRepository
    {
    public:
        RepositoryWrapper(RawParseResultConstPtr rawResult);
        virtual ~RepositoryWrapper(void);

        //Enmerations
        virtual const EnumDescription* GetEnum(DotsC_TypeId typeId) const;
        virtual size_t GetNumberOfEnums() const;
        virtual std::vector<DotsC_TypeId> GetAllEnumTypeIds() const;

        //Properties
        virtual const PropertyDescription* GetProperty(DotsC_TypeId typeId) const;
        virtual size_t GetNumberOfProperties() const;
        virtual std::vector<DotsC_TypeId> GetAllPropertyTypeIds() const;

        //Classes
        virtual const ClassDescription* GetClass(DotsC_TypeId typeId) const;
        virtual size_t GetNumberOfClasses() const;
        virtual std::vector<DotsC_TypeId> GetAllClassTypeIds() const;

        //Exceptions
        virtual const ExceptionDescription* GetException(DotsC_TypeId typeId) const;
        virtual size_t GetNumberOfExceptions() const;
        virtual std::vector<DotsC_TypeId> GetAllExceptionTypeIds() const;

    private:
        RawParseResultConstPtr m_rawResult;

        //Klar, sätt enum och cls
        struct MemberDescriptionWrapper : public MemberDescription
        {
            MemberDescriptionWrapper(const MemberDefinition* def) : m_def(def), m_class(NULL), m_enum(NULL) {}

            //Visible interface
            virtual DotsC_TypeId GetTypeId() const;
            virtual const std::string& GetName() const { return m_def->Name; }
            virtual DotsC_MemberType GetMemberType() const {return m_def->MemberType;}
            virtual const ClassDescription* GetClass() const {return m_class;}
            virtual const EnumDescription* GetEnum() const {return m_enum;}
            virtual const bool IsArray() const {return m_def->IsArray;}
            virtual int GetArraySize() const {return m_def->ArraySize;}
            //GetDataLength

            //Fields            
            const MemberDefinition* m_def;
            const ClassDescription* m_class;
            const EnumDescription* m_enum;
        };

        struct PropertyDescriptionWrapper : public PropertyDescription
        {
            PropertyDescriptionWrapper(const PropertyDefinition* def, DotsC_TypeId typeId) : m_def(def), m_typeId(typeId), m_members() {}
            
            //Visible interface
            virtual DotsC_TypeId GetTypeId() const {return m_typeId;}
            virtual const std::string& GetName() const {return m_def->Name;}
            virtual int GetNumberOfMembers() const {return m_def->Members.size();}
            virtual DotsC_MemberIndex GetMemberIndex(const std::string& memberName) const;
            virtual const MemberDescription* GetMember(DotsC_MemberIndex index) const {return &m_members[index];}

            //Fields
            const PropertyDefinition* m_def;
            DotsC_TypeId m_typeId;
            std::vector<MemberDescriptionWrapper> m_members;
        };
    
        //Klar, sätt basklass
        struct ExceptionDescriptionWrapper : public ExceptionDescription
        {
            ExceptionDescriptionWrapper(const ExceptionDefinition* def, DotsC_TypeId typeId) : m_def(def), m_typeId(typeId), m_base(NULL) {}

            //Visible interface
            virtual DotsC_TypeId GetTypeId() const {return m_typeId;}
            virtual const std::string& GetName() const {return m_def->Name;}
            virtual const ExceptionDescription* GetBaseClass() const {return m_base;}
            
            //Fields
            const ExceptionDefinition* m_def;
            DotsC_TypeId m_typeId;
            const ExceptionDescriptionWrapper* m_base;
        };

        //Klar förutom Get<T>
        struct ParameterDescriptionWrapper : public ParameterDescription
        {
            ParameterDescriptionWrapper(const ParameterDefinition* def);

            //Visible interface
            virtual const std::string& GetName() const {return m_def->Name;}
            virtual DotsC_MemberType GetMemberType() const {return m_def->MemberType;}
            virtual DotsC_TypeId GetTypeId() const {return m_typeId;} //only valid if MemberType is object or enum
            virtual int GetArraySize() const {return m_def->Values.size();}
            //GetValue<T>

            //Fields
            const ParameterDefinition* m_def;
            DotsC_TypeId m_typeId;
        };

        //Klar
        struct EnumDescriptionWrapper : public EnumDescription
        {
            EnumDescriptionWrapper(const EnumerationDefinition* def, DotsC_TypeId typeId);

            //Visible interface
            virtual DotsC_TypeId GetTypeId() const {return m_typeId;}
            virtual const std::string& GetName() const {return m_def->Name;}
            virtual DotsC_TypeId GetCheckSum() const {return m_checksum;}
            virtual int GetNumberOfValues() const {return m_def->EnumerationValues.size();}
            virtual const std::string& GetValueName(DotsC_EnumerationValue val) const {return m_def->EnumerationValues[val];}
            virtual int GetIndexOfValue(const std::string& valueName) const;

            //Fields
            const EnumerationDefinition* m_def;
            DotsC_TypeId m_typeId;
            DotsC_TypeId m_checksum;            
        };

        struct ValueMappingWrapper : public ParameterDescription
        {
            ValueMappingWrapper() {}            

            //Visible interface
            virtual const std::string& GetName() const {return m_name;}
            virtual DotsC_MemberType GetMemberType() const {return m_memberType;}
            virtual DotsC_TypeId GetTypeId() const {return m_typeId;} //only valid if MemberType is object or enum
            virtual int GetArraySize() const {return 1;}
            //GetValue<T>

            //Fields
            std::string m_name;
            std::string m_value;
            DotsC_MemberType m_memberType;
            DotsC_TypeId m_typeId;
        };

        //Klar, sätt param och memberRef
        struct MemberMappingWrapper : public MemberMappingDescription
        {
            MemberMappingWrapper(const MappedMemberDefinition* def) : m_def(def), m_valueParameter(), m_memberRef() {}

            //Visible interface
            virtual DotsC_PropertyMappingKind GetMappingKind() const {return m_def->Kind;}
            virtual const ParameterDescription * GetParameter() const {return &m_valueParameter;} //if mapped to parameter
            virtual int MemberReferenceDepth() const {return m_memberRef.size();} //if mapped to member
            virtual std::pair<DotsC_MemberIndex, DotsC_ArrayIndex> GetMemberReference(int depth) const {return m_memberRef[depth];} //if mapped to member

            //Fields
            DotsC_TypeId m_typeId;
            const MappedMemberDefinition* m_def;
            ValueMappingWrapper m_valueParameter;
            std::vector< std::pair<DotsC_MemberIndex, DotsC_ArrayIndex> > m_memberRef;
        };

        struct PropertyMappingDescriptionWrapper : public PropertyMappingDescription
        {
            PropertyMappingDescriptionWrapper(const PropertyMappingDefinition* def, const PropertyDescription* p) : m_def(def), m_property(p), m_memberMappings() {}

            //Visible interface
            virtual const PropertyDescription* GetProperty() const {return m_property;}
            virtual int GetNumberOfMappings() const {return m_memberMappings.size();}
            virtual const MemberMappingDescription* GetMapping(int index) const {return &m_memberMappings[index];}

            //Fields
            const PropertyMappingDefinition* m_def;
            const PropertyDescription* m_property;
            std::vector<MemberMappingWrapper> m_memberMappings;

        };

        struct ClassDescriptionWrapper : public ClassDescription
        {
            ClassDescriptionWrapper(const ClassDefinition* def,
                                    DotsC_TypeId typeId) :  m_def(def),
                                                            m_typeId(typeId),
                                                            m_base(NULL),
                                                            m_descendants(),
                                                            m_members(),
                                                            m_properties(),
                                                            m_ownParameters(),
                                                            m_allParameters(NULL)
            {
            }

            //Visible interface
            virtual DotsC_TypeId GetTypeId() const {return m_typeId;}
            virtual const std::string& GetName() const {return m_def->Name;}
            virtual const ClassDescription* GetBaseClass() const {return m_base;}
            virtual int GetNumberOfDescendants() const {return m_descendants.size();}
            virtual const ClassDescription* GetDescendant(int index) const {return m_descendants[index];}
            virtual int GetNumberOfOwnMembers() const {return m_members.size();}
            virtual int GetNumberOfInheritedMembers() const {return m_base ? m_base->GetNumberOfMembers() : 0;}
            virtual int GetNumberOfMembers() const {return GetNumberOfOwnMembers()+GetNumberOfInheritedMembers();}
            virtual DotsC_MemberIndex GetMemberIndex(const std::string& memberName) const;
            virtual const MemberDescription* GetMember(DotsC_MemberIndex index) const;
            virtual int GetNumberOfOwnParameters() const {return m_ownParameters.size();}
            virtual int GetNumberOfInheritedParameters() const {return m_base ? m_base->GetNumberOfParameters() : 0;}
            virtual int GetNumberOfParameters() const {return GetNumberOfOwnParameters()+GetNumberOfInheritedParameters();}
            virtual const ParameterDescription* GetParameter(DotsC_ParameterIndex index) const;
            virtual int GetNumberOfProperties() const;
            virtual void GetPropertyIds(std::vector<DotsC_TypeId>& propertyIds) const;
            virtual const PropertyMappingDescription* GetPropertyMapping(DotsC_TypeId propertyType, bool & isInherited) const;
            virtual int InitialSize() const {return 0;}
            virtual int OwnSize() const {return 0;}

            //Fields
            const ClassDefinition* m_def;
            DotsC_TypeId m_typeId;
            const ClassDescription* m_base;
            std::vector<const ClassDescription*> m_descendants;
            std::vector<MemberDescriptionWrapper> m_members;
            std::vector<PropertyMappingDescriptionWrapper> m_properties;
            std::vector<std::string> m_ownParameters; //name used to look-up ParameterDescription in m_parameters
            std::map<std::string, ParameterDescriptionWrapper>* m_allParameters;
        };

        std::map<DotsC_TypeId, EnumDescriptionWrapper> m_enums;
        std::map<DotsC_TypeId, ClassDescriptionWrapper> m_classes;
        std::map<DotsC_TypeId, PropertyDescriptionWrapper> m_properties;
        std::map<DotsC_TypeId, ExceptionDescriptionWrapper> m_exceptions;
        std::map<std::string, ParameterDescriptionWrapper> m_parameters;        

        template <class Description>
        void CreateMembers(Description& d)
        {
            for (MemberDefinitions::const_iterator memberIt=d.m_def->Members.begin(); memberIt!=d.m_def->Members.end(); ++memberIt)
            {
                MemberDescriptionWrapper member(&(*memberIt));
                
                switch (memberIt->MemberType)
                {
                case EnumerationMemberType:
                    member.m_enum=GetPtr(m_enums, DotsId_Generate64(member.m_def->TypeName.c_str()));
                    break;
                case ObjectMemberType:
                    member.m_class=GetPtr(m_classes, DotsId_Generate64(member.m_def->TypeName.c_str()));
                    break;
                default:
                    break;
                }
                d.m_members.push_back(member);
            }
        }
        
    };
}
}
}
}

#endif
