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

#include <Safir/Dob/Typesystem/Internal/TypeRepository.h>
#include "ParseState.h"
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
    const Val* GetPtr(const boost::unordered_map<Key, Val> & m, Key key)
    {
        typename boost::unordered_map<Key, Val>::const_iterator it = m.find(key);
        if (it!=m.end())
        {
            return &(it->second);
        }
        return NULL;
    }

    template <class Key, class Val>
    Val* GetPtr(boost::unordered_map<Key, Val> & m, Key key)
    {
        typename boost::unordered_map<Key, Val>::iterator it = m.find(key);
        if (it!=m.end())
        {
            return &(it->second);
        }
        return NULL;
    }

    template <class Key, class Val>
    void GetKeys(const boost::unordered_map<Key, Val>& m, std::vector<Key>& keys)
    {
        for (typename boost::unordered_map<Key, Val>::const_iterator it=m.begin(); it!=m.end(); ++it)
        {
            keys.push_back(it->first);
        }
    }

    template <class Def, class Descr>
    void SetupWithTypeId(const std::vector<Def>& src, boost::unordered_map<DotsC_TypeId, Descr>& dest)
    {
        for (typename std::vector<Def>::const_iterator it=src.begin(); it!=src.end(); ++it)
        {
            DotsC_TypeId typeId = DotsId_Generate64(it->name.c_str());
            dest.insert(typename boost::unordered_map<DotsC_TypeId, Descr>::value_type(typeId, Descr(&(*it), typeId)));
        }
    }

    class RepositoryWrapper : public TypeRepository
    {
    public:
        RepositoryWrapper(const ParseState& state);
        virtual ~RepositoryWrapper(void) {}

        //Enmerations
        virtual const EnumDescription* GetEnum(DotsC_TypeId typeId) const {return GetPtr(m_enums, typeId);}
        virtual size_t GetNumberOfEnums() const {return m_enums.size();}
        virtual void GetAllEnumTypeIds(std::vector<DotsC_TypeId>& typeIds) const {GetKeys(m_enums, typeIds);}

        //properties
        virtual const PropertyDescription* GetProperty(DotsC_TypeId typeId) const {return GetPtr(m_properties, typeId);}
        virtual size_t GetNumberOfproperties() const {return m_properties.size();}
        virtual void GetAllPropertyTypeIds(std::vector<DotsC_TypeId>& typeIds) const {GetKeys(m_properties, typeIds);}

        //classes
        virtual const ClassDescription* GetClass(DotsC_TypeId typeId) const {return GetPtr(m_classes, typeId);}
        virtual size_t GetNumberOfclasses() const {return m_classes.size();}
        virtual void GetAllClassTypeIds(std::vector<DotsC_TypeId>& typeIds) const {GetKeys(m_classes, typeIds);}

        //exceptions
        virtual const ExceptionDescription* GetException(DotsC_TypeId typeId) const {return GetPtr(m_exceptions, typeId);}
        virtual size_t GetNumberOfexceptions() const {return m_exceptions.size();}
        virtual void GetAllExceptionTypeIds(std::vector<DotsC_TypeId>& typeIds) const {GetKeys(m_exceptions, typeIds);}

    private:

        struct MemberDescriptionWrapper : public MemberDescription
        {
            MemberDescriptionWrapper(const MemberDefinition* def) : m_def(def), m_class(NULL), m_enum(NULL) {}

            //Visible interface            
            virtual const std::string& Summary() const {return m_def->summary;}
            virtual DotsC_TypeId GetTypeId() const;
            virtual const std::string& GetName() const { return m_def->name; }
            virtual DotsC_MemberType GetMemberType() const {return m_def->memberType;}
            virtual const ClassDescription* GetClass() const {return m_class;}
            virtual const EnumDescription* GetEnum() const {return m_enum;}
            virtual const bool IsArray() const {return m_def->isArray;}
            virtual int GetarraySize() const {return m_def->arraySize;}
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
            virtual const std::string& FileName() const {return m_def->fileName;}
            virtual const std::string& Summary() const {return m_def->summary;}
            virtual DotsC_TypeId GetTypeId() const {return m_typeId;}
            virtual const std::string& GetName() const {return m_def->name;}
            virtual int GetNumberOfMembers() const {return m_def->members.size();}
            virtual DotsC_MemberIndex GetMemberIndex(const std::string& memberName) const;
            virtual const MemberDescription* GetMember(DotsC_MemberIndex index) const {return &m_members[index];}

            //Fields
            const PropertyDefinition* m_def;
            DotsC_TypeId m_typeId;
            std::vector<MemberDescriptionWrapper> m_members;
        };

        struct ExceptionDescriptionWrapper : public ExceptionDescription
        {
            ExceptionDescriptionWrapper(const ExceptionDefinition* def, DotsC_TypeId typeId) : m_def(def), m_typeId(typeId), m_base(NULL) {}

            //Visible interface
            virtual const std::string& FileName() const {return m_def->fileName;}
            virtual const std::string& Summary() const {return m_def->summary;}
            virtual DotsC_TypeId GetTypeId() const {return m_typeId;}
            virtual const std::string& GetName() const {return m_def->name;}
            virtual const ExceptionDescription* GetbaseClass() const {return m_base;}
            
            //Fields
            const ExceptionDefinition* m_def;
            DotsC_TypeId m_typeId;
            const ExceptionDescriptionWrapper* m_base;
        };

        struct ParameterDescriptionWrapper : public ParameterDescription
        {
            ParameterDescriptionWrapper(const ParameterDefinition* def, const std::string& fullName);

            //Visible interface            
            virtual const std::string& Summary() const {return m_def->summary;}
            virtual const std::string& GetName() const {return m_fullParameterName;}
            virtual DotsC_MemberType GetMemberType() const {return m_def->memberType;}
            virtual DotsC_TypeId GetTypeId() const {return m_typeId;} //only valid if MemberType is object or enum
            virtual bool IsArray() const {return m_def->isArray;}
            virtual int GetarraySize() const {return m_def->values.size();}
            //GetValue<T>

            //Fields
            const ParameterDefinition* m_def;
            std::string m_fullParameterName; //Full parameter name including namespace and class.
            DotsC_TypeId m_typeId; //TypeId belonging to the value of this parameter. Only valid if parameter is object or enum.
        };

        struct EnumDescriptionWrapper : public EnumDescription
        {
            EnumDescriptionWrapper(const EnumerationDefinition* def, DotsC_TypeId typeId);

            //Visible interface
            virtual const std::string& FileName() const {return m_def->fileName;}
            virtual const std::string& Summary() const {return m_def->summary;}
            virtual DotsC_TypeId GetTypeId() const {return m_typeId;}
            virtual const std::string& GetName() const {return m_def->name;}
            virtual DotsC_TypeId GetCheckSum() const {return m_checksum;}
            virtual int GetNumberOfValues() const {return m_def->enumerationValues.size();}
            virtual const std::string& GetValueName(DotsC_EnumerationValue val) const {return m_def->enumerationValues[val];}
            virtual int GetIndexOfValue(const std::string& valueName) const;

            //Fields
            const EnumerationDefinition* m_def;
            DotsC_TypeId m_typeId;
            DotsC_TypeId m_checksum;            
        };

        struct MemberMappingWrapper : public MemberMappingDescription
        {
            MemberMappingWrapper(const MappedMemberDefinition* def) : m_def(def), m_paramRef(NULL), m_paramIndex(-1), m_memberRef() {}

            //Visible interface
            virtual DotsC_PropertyMappingKind GetMappingKind() const {return m_def->kind;}
            virtual const ParameterDescription * GetParameter() const {return m_paramRef;} //if mapped to parameter
            virtual int MemberReferenceDepth() const {return m_memberRef.size();} //if mapped to member
            virtual std::pair<DotsC_MemberIndex, DotsC_ArrayIndex> GetMemberReference(int depth) const {return m_memberRef[depth];} //if mapped to member

            //Fields
            DotsC_TypeId m_typeId;
            const MappedMemberDefinition* m_def;
            ParameterDescriptionWrapper* m_paramRef;
            int m_paramIndex;
            std::vector< std::pair<DotsC_MemberIndex, DotsC_ArrayIndex> > m_memberRef;
        };

        struct PropertyMappingDescriptionWrapper : public PropertyMappingDescription
        {
            PropertyMappingDescriptionWrapper(const PropertyMappingDefinition* def, const PropertyDescription* p, const ClassDescription* c) : m_def(def), m_property(p), m_class(c), m_memberMappings() {}

            //Visible interface            
            virtual const std::string& FileName() const {return m_def->fileName;}
            virtual const std::string& Summary() const {return m_def->summary;}
            virtual const PropertyDescription* GetProperty() const {return m_property;}
            virtual const ClassDescription* GetClass() const {return m_class;}
            virtual int GetNumberOfMappings() const {return m_memberMappings.size();}
            virtual const MemberMappingDescription* GetMapping(int index) const {return &m_memberMappings[index];}

            //Fields
            const PropertyMappingDefinition* m_def;
            const PropertyDescription* m_property;
            const ClassDescription* m_class;
            std::vector<MemberMappingWrapper> m_memberMappings;

        };

        class ClassDescriptionWrapper; //forward declaration
        struct CreateRoutineDescriptionWrapper : public CreateRoutineDescription
        {
            CreateRoutineDescriptionWrapper(const CreateRoutineDefinition* def, const ClassDescriptionWrapper* c)
                : m_def(def)
                ,m_class(c)
            {
            }

            virtual const std::string& Summary() const {return m_def->summary;}
            virtual const std::string& GetName() const {return m_def->name;}
            virtual const ClassDescription* GetClass() const {return m_class;}

            virtual int GetNumberOfInParameters() const {return m_def->parameters.size();}
            virtual const MemberDescription* GetInParameterMember(int index) const;

            virtual int GetNumberOfDefaultValues() const {return m_def->memberValues.size();}
            virtual const MemberDescription* GetDefaultValueMember(int index) const;

            virtual std::pair<const ParameterDescription*, int /*paramIndex*/> GetDefaultValue(int index) const;

            const CreateRoutineDefinition* m_def;
            const ClassDescriptionWrapper* m_class; //ptr to class this createRoutine belongs to
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
            virtual const std::string& FileName() const {return m_def->fileName;}
            virtual const std::string& Summary() const {return m_def->summary;}
            virtual DotsC_TypeId GetTypeId() const {return m_typeId;}
            virtual const std::string& GetName() const {return m_def->name;}
            virtual const ClassDescription* GetbaseClass() const {return m_base;}
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

            virtual void GetPropertyIds(std::vector<DotsC_TypeId>& propertyIds) const;
            virtual const PropertyMappingDescription* GetPropertyMapping(DotsC_TypeId propertyType, bool & isInherited) const;

            virtual int GetNumberOfCreateRoutines() const {return m_createRoutines.size();}
            virtual const CreateRoutineDescription* GetCreateRoutine(int index) const {return &m_createRoutines[index];}

            virtual int InitialSize() const {return 0;}
            virtual int OwnSize() const {return 0;}

            //Fields
            const ClassDefinition* m_def;
            DotsC_TypeId m_typeId;
            const ClassDescription* m_base;
            std::vector<const ClassDescription*> m_descendants;
            std::vector<MemberDescriptionWrapper> m_members;
            std::vector<CreateRoutineDescriptionWrapper> m_createRoutines;
            std::vector<PropertyMappingDescriptionWrapper> m_properties;
            std::vector<std::string> m_ownParameters; //name used to look-up ParameterDescription in m_parameters
            boost::unordered_map<std::string, ParameterDescriptionWrapper>* m_allParameters; //ptr to the global m_parameters
        };

        //Raw parse result
        RawParseResultConstPtr m_rawResult;

        //Pre-defined types
        ExceptionDefinition m_exceptionDef;
        ExceptionDefinition m_fundamentalExceptionDef;

        //Type containers
        boost::unordered_map<DotsC_TypeId, EnumDescriptionWrapper> m_enums;
        boost::unordered_map<DotsC_TypeId, ClassDescriptionWrapper> m_classes;
        boost::unordered_map<DotsC_TypeId, PropertyDescriptionWrapper> m_properties;
        boost::unordered_map<DotsC_TypeId, ExceptionDescriptionWrapper> m_exceptions;
        boost::unordered_map<std::string, ParameterDescriptionWrapper> m_parameters;

        template <class Description>
        void CreateMembers(Description& d)
        {
            for (MemberDefinitions::const_iterator memberIt=d.m_def->members.begin(); memberIt!=d.m_def->members.end(); ++memberIt)
            {
                MemberDescriptionWrapper member(&(*memberIt));
                
                switch (memberIt->memberType)
                {
                case EnumerationMemberType:
                    member.m_enum=GetPtr(m_enums, DotsId_Generate64(member.m_def->typeName.c_str()));
                    break;
                case ObjectMemberType:
                    member.m_class=GetPtr(m_classes, DotsId_Generate64(member.m_def->typeName.c_str()));
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
