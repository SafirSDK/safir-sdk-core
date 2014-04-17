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
#ifndef __DOTS_INTERNAL_REPOSITORY_BASIC_H__
#define __DOTS_INTERNAL_REPOSITORY_BASIC_H__

#include <set>
#include <boost/noncopyable.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/unordered_map.hpp>
#include <Safir/Dob/Typesystem/ToolSupport/TypeUtilities.h>
#include <Safir/Dob/Typesystem/ToolSupport/Internal/BasicTypeOperations.h>

using namespace Safir::Dob::Typesystem::ToolSupport::Internal;

/**
 * A local memory implementation of the TypeRespository interface
 * The constructor takes all the ParseState from each worker in a ParseJob and performes
 * necessary finalization work that could not be done during the xml-parsing stage by the
 * workers. On errors ParseError is thrown, else the Repository is constructed and the
 * dou/dom-parsing is completed.
 */
namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace ToolSupport
{
    /**
     * Definition of a value. Used in parameters, createRoutines and propertyMappings.
     */
    enum ValueDefinitionKind {ValueKind, NullKind, RefKind};
    struct ValueDefinition
    {
        ValueDefinitionKind kind;
        std::string stringVal;
        std::vector<char> binaryVal; //object and binary
        DotsC_Int64 hashedVal;
        union
        {
            ValueDefinition* referenced; //if value is a reference to another value
            DotsC_Int32 int32Val;
            DotsC_Int64 int64Val;
            DotsC_Float32 float32Val;
            DotsC_Float64 float64Val;
            bool boolVal;
        };

        ValueDefinition()
            :kind(ValueKind)
            ,hashedVal(0)
            ,referenced(NULL)
        {
        }

        ValueDefinition(ValueDefinitionKind k)
            :kind(k)
            ,hashedVal(0)
            ,referenced(NULL)
        {
        }
    };

    typedef std::vector<ValueDefinition> ParameterValues;
    typedef std::pair<std::string, int> MemberReference; //pair<Member, Index> or pair<Parameter, Index>
    typedef std::vector<MemberReference> MemberReferenceVector;
    typedef std::pair<std::string, MemberReference> MemberValue; //pair <memberName, pair<Parameter,Index> >
    typedef std::vector<MemberValue> MemberValueVector;

    class ClassDescriptionBasic;
    typedef boost::shared_ptr<ClassDescriptionBasic> ClassDescriptionBasicPtr;

    class MemberDescriptionBasic : public MemberDescription
    {
    public:
        MemberDescriptionBasic()
            :collectionType(NoCollectionType)
            ,arraySize(1)
            ,maxLength(0)
        {
        }

        //Visible interface
        virtual const char* Summary() const {return summary.c_str();}
        virtual DotsC_TypeId GetTypeId() const {return typeId;}
        virtual const char* GetName() const { return name.c_str(); }
        virtual DotsC_MemberType GetMemberType() const {return memberType;}
        virtual DotsC_CollectionType GetCollectionType() const {return collectionType;}
        virtual DotsC_MemberType GetKeyType() const {return dictionaryKeyType;}
        virtual int GetArraySize() const {return arraySize;}
        virtual int GetMaxLength() const {return maxLength;}

        //Fields
        std::string summary;
        std::string name;
        std::string typeName;
        DotsC_MemberType memberType;
        DotsC_CollectionType collectionType;
        DotsC_MemberType dictionaryKeyType;
        int arraySize; //If isArray
        int maxLength; //Max string length. Only applicable if typeName is 'String'.

        DotsC_TypeId typeId; //TypeId belonging to the type of this member. Only valid if memberType is object or enum.
    };
    typedef boost::shared_ptr<MemberDescriptionBasic> MemberDescriptionBasicPtr;

    class PropertyDescriptionBasic : public PropertyDescription
    {
    public:
        PropertyDescriptionBasic()
        {
        }

        //Visible interface
        virtual const char* FileName() const {return fileName.c_str();}
        virtual const char* Summary() const {return summary.c_str();}
        virtual DotsC_TypeId GetTypeId() const {return typeId;}
        virtual const char* GetName() const {return name.c_str();}
        virtual int GetNumberOfMembers() const {return static_cast<int>(members.size());}
        virtual DotsC_MemberIndex GetMemberIndex(const std::string& memberName) const;
        virtual const MemberDescription* GetMember(DotsC_MemberIndex index) const {return members[index].get();}

        //Fields
        DotsC_TypeId typeId;
        std::string summary;
        std::string fileName;
        std::string name;
        std::vector<MemberDescriptionBasicPtr> members;
    };
    typedef boost::shared_ptr<PropertyDescriptionBasic> PropertyDescriptionBasicPtr;

    class ExceptionDescriptionBasic : public ExceptionDescription
    {
    public:
        ExceptionDescriptionBasic()
            :base(NULL)
        {
        }

        //Visible interface
        virtual const char* FileName() const {return fileName.c_str();}
        virtual const char* Summary() const {return summary.c_str();}
        virtual DotsC_TypeId GetTypeId() const {return typeId;}
        virtual const char* GetName() const {return name.c_str();}
        virtual const ExceptionDescription* GetBaseClass() const {return base;}

        //Fields
        DotsC_TypeId typeId;
        std::string summary;
        std::string fileName;
        std::string name;
        std::string baseClass;
        const ExceptionDescriptionBasic* base;
    };
    typedef boost::shared_ptr<ExceptionDescriptionBasic> ExceptionDescriptionBasicPtr;

    class ParameterDescriptionBasic : public ParameterDescription
    {
    public:
        ParameterDescriptionBasic()
            :collectionType(NoCollectionType)
            ,hidden(false)
        {
        }

        //Visible interface
        virtual const char* Summary() const {return summary.c_str();}
        virtual const char* GetName() const {return name.c_str();}
        virtual const char* GetQualifiedName() const {return qualifiedName.c_str();}
        virtual DotsC_MemberType GetMemberType() const {return memberType;}
        virtual DotsC_TypeId GetTypeId() const {return typeId;} //only valid if MemberType is object or enum
        virtual DotsC_CollectionType GetCollectionType() const {return collectionType;}
        virtual DotsC_MemberType GetKeyType() const {return dictionaryKeyType;} //only valid if collectionType is Dictionary
        virtual int GetArraySize() const {return static_cast<int>(values.size());}
        virtual bool IsHidden() const {return hidden;}

        //GetValues
        virtual boost::int32_t GetInt32Value(int index) const {return Value(static_cast<size_t>(index)).int32Val;}
        virtual boost::int64_t GetInt64Value(int index) const {return Value(static_cast<size_t>(index)).int64Val;}
        virtual float GetFloat32Value(int index) const {return Value(static_cast<size_t>(index)).float32Val;}
        virtual double GetFloat64Value(int index) const {return Value(static_cast<size_t>(index)).float64Val;}
        virtual bool GetBoolValue(int index) const {return Value(static_cast<size_t>(index)).boolVal;}
        virtual const char* GetStringValue(int index) const {return Value(static_cast<size_t>(index)).stringVal.c_str();}
        virtual std::pair<const char*, size_t> GetObjectValue(int index) const
        {
            const ValueDefinition& v=Value(static_cast<size_t>(index));
            return std::make_pair(&v.binaryVal[0], v.binaryVal.size());
        }
        virtual std::pair<const char*, size_t> GetBinaryValue(int index) const
        {
            const ValueDefinition& v=Value(static_cast<size_t>(index));
            return std::make_pair(v.stringVal.c_str(), v.stringVal.size());
        }
        virtual std::pair<boost::int64_t, const char*> GetHashedValue(int index) const
        {
            const ValueDefinition& val=Value(static_cast<size_t>(index));
            if (!val.stringVal.empty() && val.hashedVal==0)
            {
                //This is most likely a reference to a plain string, and if it's not this won't break anything anyway
                boost::int64_t hash=LlufId_Generate64(val.stringVal.c_str());
                return std::make_pair(hash, val.stringVal.c_str());
            }
            return std::make_pair(val.hashedVal, val.stringVal.empty() ? NULL : val.stringVal.c_str());
        }

        const ValueDefinition& Value(size_t index) const
        {
            const ValueDefinition* val=&values[index];
            while (val->kind==RefKind)
            {
                val=val->referenced;
            }
            return *val;
        }

        ValueDefinition& MutableValue(size_t index)
        {
            const ValueDefinition& val=Value(index);
            return *const_cast<ValueDefinition*>(&val);
        }

        //Fields
        std::string summary;
        std::string name;
        std::string qualifiedName;
        std::string typeName;
        DotsC_MemberType memberType;
        DotsC_CollectionType collectionType;
        DotsC_MemberType dictionaryKeyType;
        bool hidden;   //Some parameters are derived from propertyMapping values. The parser will automatically generate a
                        //hidden parameter for those values. All explicitly declared parameters will have hidden=false.
        ParameterValues values;

        DotsC_TypeId typeId; //TypeId belonging to the value of this parameter. Only valid if parameter is object or enum.
    };
    typedef boost::shared_ptr<ParameterDescriptionBasic> ParameterDescriptionBasicPtr;

    class EnumDescriptionBasic : public EnumDescription
    {
    public:
        EnumDescriptionBasic()
        {
        }

        //Visible interface
        virtual const char* FileName() const {return fileName.c_str();}
        virtual const char* Summary() const {return summary.c_str();}
        virtual DotsC_TypeId GetTypeId() const {return typeId;}
        virtual const char* GetName() const {return name.c_str();}
        virtual DotsC_TypeId GetCheckSum() const {return checksum;}
        virtual int GetNumberOfValues() const {return static_cast<int>(enumerationValues.size());}
        virtual const char* GetValueName(DotsC_EnumerationValue val) const {return enumerationValues[val].c_str();}
        virtual int GetIndexOfValue(const std::string& valueName) const;

        //Fields
        DotsC_TypeId typeId;
        std::string summary;
        std::string fileName;
        std::string name;
        StringVector enumerationValues;

        DotsC_TypeId checksum;
    };
    typedef boost::shared_ptr<EnumDescriptionBasic> EnumDescriptionBasicPtr;

    class MemberMappingBasic : public MemberMappingDescription
    {
    public:
        MemberMappingBasic()
        {
        }

        //Visible interface
        virtual DotsC_PropertyMappingKind GetMappingKind() const {return kind;}
        virtual std::pair<const ParameterDescription*, int /*paramIndex*/> GetParameter() const {return std::make_pair(paramRef, paramIndex);} //if mapped to parameter
        virtual int MemberReferenceDepth() const {return static_cast<int>(memberRef.size());} //if mapped to member
        virtual std::pair<DotsC_MemberIndex, DotsC_ArrayIndex> GetMemberReference(int depth) const {return memberRef[depth];} //if mapped to member

        //Fields
        DotsC_PropertyMappingKind kind;
        int propertyMemberIndex;
        ParameterDescriptionBasic* paramRef;
        int paramIndex;
        std::vector< std::pair<DotsC_MemberIndex, DotsC_ArrayIndex> > memberRef;
    };
    typedef boost::shared_ptr<MemberMappingBasic> MemberMappingBasicPtr;

    class PropertyMappingDescriptionBasic : public PropertyMappingDescription
    {
    public:
        PropertyMappingDescriptionBasic()
        {
        }

        //Visible interface
        virtual const char* FileName() const {return fileName.c_str();}
        virtual const char* Summary() const {return summary.c_str();}
        virtual const PropertyDescription* GetProperty() const;
        virtual const ClassDescription* GetClass() const;
        virtual const MemberMappingDescription* GetMemberMapping(int propertyMemberIndex) const {return memberMappings[propertyMemberIndex].get();}

        //Fields
        std::string summary;
        std::string fileName;
        PropertyDescriptionBasic* property;
        ClassDescriptionBasic* class_;
        std::vector<MemberMappingBasicPtr> memberMappings;
    };
    typedef boost::shared_ptr<PropertyMappingDescriptionBasic> PropertyMappingDescriptionBasicPtr;

    class CreateRoutineDescriptionBasic : public CreateRoutineDescription
    {
    public:
        explicit CreateRoutineDescriptionBasic(ClassDescriptionBasic* parent_)
            :parent(parent_)
            ,signature()
        {
        }

        virtual const char* Summary() const {return summary.c_str();}
        virtual const char* GetName() const {return name.c_str();}

        virtual int GetNumberOfInParameters() const {return static_cast<int>(parameters.size());}
        virtual const MemberDescription* GetInParameterMember(int index) const;

        virtual int GetNumberOfDefaultValues() const {return static_cast<int>(memberValues.size());}
        virtual const MemberDescription* GetDefaultValueMember(int index) const;

        virtual std::pair<const ParameterDescription*, int /*paramIndex*/> GetDefaultValue(int index) const;

        std::string summary;
        std::string name;
        StringVector parameters;
        MemberValueVector memberValues;
        std::vector< std::pair<const ParameterDescriptionBasic*, int> > memberValuesParams;

        ClassDescriptionBasic* parent;
        std::string signature;
    };
    typedef boost::shared_ptr<CreateRoutineDescriptionBasic> CreateRoutineDescriptionBasicPtr;

    class ClassDescriptionBasic : public ClassDescription
    {
    public:
        ClassDescriptionBasic()
        {
        }

        //Visible interface
        virtual const char* FileName() const {return fileName.c_str();}
        virtual const char* Summary() const {return summary.c_str();}
        virtual DotsC_TypeId GetTypeId() const {return typeId;}
        virtual const char* GetName() const {return name.c_str();}
        virtual const ClassDescription* GetBaseClass() const {return base;}
        virtual int GetNumberOfDescendants() const {return static_cast<int>(descendants.size());}
        virtual const ClassDescription* GetDescendant(int index) const {return descendants[index];}
        virtual int GetNumberOfOwnMembers() const {return static_cast<int>(members.size());}
        virtual int GetNumberOfInheritedMembers() const {return base ? base->GetNumberOfMembers() : 0;}
        virtual int GetNumberOfMembers() const {return GetNumberOfOwnMembers()+GetNumberOfInheritedMembers();}
        virtual DotsC_MemberIndex GetMemberIndex(const std::string& memberName) const;
        virtual const MemberDescription* GetMember(DotsC_MemberIndex index) const;

        virtual int GetNumberOfOwnParameters() const {return static_cast<int>(ownParameters.size());}
        virtual int GetNumberOfInheritedParameters() const {return base ? base->GetNumberOfParameters() : 0;}
        virtual int GetNumberOfParameters() const {return GetNumberOfOwnParameters()+GetNumberOfInheritedParameters();}
        virtual const ParameterDescription* GetParameter(DotsC_ParameterIndex index) const;

        virtual void GetPropertyIds(std::set<DotsC_TypeId>& propertyIds) const;
        virtual const PropertyMappingDescription* GetPropertyMapping(DotsC_TypeId propertyTypeId, bool & isInherited) const;

        virtual int GetNumberOfCreateRoutines() const {return static_cast<int>(createRoutines.size());}
        virtual const CreateRoutineDescription* GetCreateRoutine(int index) const {return createRoutines[index].get();}

        //Fields
        DotsC_TypeId typeId;
        std::string summary;
        std::string fileName;
        std::string name;
        std::string baseClass;

        ClassDescriptionBasic* base;
        std::vector<ClassDescriptionBasic*> descendants;
        std::vector<MemberDescriptionBasicPtr> members;
        std::vector<CreateRoutineDescriptionBasicPtr> createRoutines;
        std::vector<PropertyMappingDescriptionBasicPtr> properties;
        std::vector<ParameterDescriptionBasicPtr> ownParameters;
    };


    class RepositoryBasic : public TypeRepository, boost::noncopyable
    {
    public:
        //Enmerations
        virtual const EnumDescription* GetEnum(DotsC_TypeId typeId) const {return GetPtr(m_enums, typeId);}
        virtual int GetNumberOfEnums() const {return static_cast<int>(m_enums.size());}
        virtual void GetAllEnumTypeIds(std::set<DotsC_TypeId>& typeIds) const {GetKeys(m_enums, typeIds);}

        //properties
        virtual const PropertyDescription* GetProperty(DotsC_TypeId typeId) const {return GetPtr(m_properties, typeId);}
        virtual int GetNumberOfProperties() const {return static_cast<int>(m_properties.size());}
        virtual void GetAllPropertyTypeIds(std::set<DotsC_TypeId>& typeIds) const {GetKeys(m_properties, typeIds);}

        //classes
        virtual const ClassDescription* GetClass(DotsC_TypeId typeId) const {return GetPtr(m_classes, typeId);}
        virtual int GetNumberOfClasses() const {return static_cast<int>(m_classes.size());}
        virtual void GetAllClassTypeIds(std::set<DotsC_TypeId>& typeIds) const {GetKeys(m_classes, typeIds);}

        //exceptions
        virtual const ExceptionDescription* GetException(DotsC_TypeId typeId) const {return GetPtr(m_exceptions, typeId);}
        virtual int GetNumberOfExceptions() const {return static_cast<int>(m_exceptions.size());}
        virtual void GetAllExceptionTypeIds(std::set<DotsC_TypeId>& typeIds) const {GetKeys(m_exceptions, typeIds);}

        //Extra methods not from TypeRepository interface
        bool InsertEnum(const EnumDescriptionBasicPtr& val) {return m_enums.insert(std::make_pair(val->typeId, val)).second;}
        bool InsertClass(const ClassDescriptionBasicPtr& val) {return m_classes.insert(std::make_pair(val->typeId, val)).second;}
        bool InsertProperty(const PropertyDescriptionBasicPtr& val) {return m_properties.insert(std::make_pair(val->typeId, val)).second;}
        bool InsertException(const ExceptionDescriptionBasicPtr& val) {return m_exceptions.insert(std::make_pair(val->typeId, val)).second;}
        bool InsertParameter(const ParameterDescriptionBasicPtr& val) {return m_parameters.insert(std::make_pair(val->GetQualifiedName(), val.get())).second;}
        ParameterDescriptionBasic* GetParameterBasic(const std::string& qualifiedName);
        PropertyDescriptionBasic* GetPropertyBasic(DotsC_TypeId typeId) const {return GetPtr(m_properties, typeId);}
        ClassDescriptionBasic* GetClassBasic(DotsC_TypeId typeId) const {return GetPtr(m_classes, typeId);}
        ExceptionDescriptionBasic* GetExceptionBasic(DotsC_TypeId typeId) const {return GetPtr(m_exceptions, typeId);}

    private:
        friend class RepositoryCompletionAlgorithms;

        //Type containers
        boost::unordered_map<DotsC_TypeId, EnumDescriptionBasicPtr> m_enums;
        boost::unordered_map<DotsC_TypeId, ClassDescriptionBasicPtr> m_classes;
        boost::unordered_map<DotsC_TypeId, PropertyDescriptionBasicPtr> m_properties;
        boost::unordered_map<DotsC_TypeId, ExceptionDescriptionBasicPtr> m_exceptions;
        boost::unordered_map<std::string, ParameterDescriptionBasic*> m_parameters;

        template <class Key, class Val>
        static void GetKeys(const boost::unordered_map<Key, Val>& m, std::set<Key>& keys)
        {
            for (typename boost::unordered_map<Key, Val>::const_iterator it=m.begin(); it!=m.end(); ++it)
            {
                keys.insert(it->first);
            }
        }

        template <class Key, class Val>
        static Val* GetPtr(const boost::unordered_map< Key, boost::shared_ptr<Val> >& m, Key key)
        {
            typename boost::unordered_map< Key, boost::shared_ptr<Val> >::const_iterator it=m.find(key);
            if (it!=m.end())
            {
                return it->second.get();
            }
            return NULL;
        }
    };
}
}
}
}

#endif
