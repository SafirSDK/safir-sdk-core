/******************************************************************************
*
* Copyright Saab AB, 2004-2013 (http://safir.sourceforge.net)
*
* Created by: Joel Ottosson / joot
*
*******************************************************************************/
#ifndef __DOTS_KERNEL_REPOSITORY_H__
#define __DOTS_KERNEL_REPOSITORY_H__

#include <boost/noncopyable.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/interprocess/containers/vector.hpp>
#include <boost/interprocess/containers/string.hpp>
#include <boost/interprocess/managed_shared_memory.hpp>
#include <boost/interprocess/containers/map.hpp>
#include <boost/interprocess/smart_ptr/shared_ptr.hpp>
#include <boost/interprocess/offset_ptr.hpp>
#include <boost/circular_buffer.hpp>
#include <Safir/Utilities/StartupSynchronizer.h>
#include <Safir/Dob/Typesystem/Internal/TypeParser.h>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    /**
      *Define types shared memory equivalents to the TypeRepository and its subtypes.
      */

    //-------------------------------------------
    //VectorShm
    //-------------------------------------------
    template <class T>
    struct VectorShm
    {
        typedef boost::interprocess::allocator<T, boost::interprocess::managed_shared_memory::segment_manager> Allocator;
        typedef boost::interprocess::vector<T, Allocator> Type;
    };

    //-------------------------------------------
    //MapShm
    //-------------------------------------------
    template <class Val, class Key=DotsC_TypeId>
    struct MapShm
    {
        typedef std::pair<const Key, Val> ValueType;
        typedef boost::interprocess::allocator<ValueType, boost::interprocess::managed_shared_memory::segment_manager> Allocator;
        typedef boost::interprocess::map<Key, Val, std::less<const Key>, Allocator> Type;
    };

    //-------------------------------------------
    //StringShm
    //-------------------------------------------
    //typedef boost::interprocess::allocator<char, boost::interprocess::managed_shared_memory::segment_manager> CharAllocatorShm;
    typedef boost::interprocess::basic_string<char, std::char_traits<char>, boost::interprocess::allocator<char, boost::interprocess::managed_shared_memory::segment_manager> > StringShm;
    typedef VectorShm<StringShm>::Type StringVectorShm;

    //-------------------------------------------
    //MemberDescriptionShm
    //-------------------------------------------
    class MemberDescriptionShm
    {
    public:
        MemberDescriptionShm(const MemberDescription* md, boost::interprocess::managed_shared_memory* shm)
            :m_name(md->GetName(), shm->get_segment_manager())
            ,m_memberType(md->GetMemberType())
            ,m_isArray(md->IsArray())
            ,m_arraySize(md->GetArraySize())
            ,m_maxLength(md->GetMaxLength())
            ,m_typeId(md->GetTypeId())
        {
        }

        MemberDescriptionShm(const MemberDescriptionShm& other)
            :m_name(other.m_name)
            ,m_memberType(other.m_memberType)
            ,m_isArray(other.m_isArray)
            ,m_arraySize(other.m_arraySize)
            ,m_maxLength(other.m_maxLength)
            ,m_typeId(other.m_typeId)
        {
        }

        MemberDescriptionShm& operator=(const MemberDescriptionShm& other)
        {
            m_name=other.m_name;
            m_memberType=other.m_memberType;
            m_isArray=other.m_isArray;
            m_arraySize=other.m_arraySize;
            m_maxLength=other.m_maxLength;
            m_typeId=other.m_typeId;
            return *this;
        }

        const char* Summary() const {return NULL;}
        DotsC_TypeId GetTypeId() const {return m_typeId;} //only valid if MemberType is object or enum
        const char* GetName() const {return m_name.c_str();}
        DotsC_MemberType GetMemberType() const {return m_memberType;}
        const bool IsArray() const {return m_isArray;}
        int GetArraySize() const {return m_arraySize;}
        int GetMaxLength() const {return m_maxLength;} //only valid if memberType is String

    private:
        //Fields
        StringShm m_name;
        DotsC_MemberType m_memberType;
        bool m_isArray;
        int m_arraySize; //If isArray
        int m_maxLength; //Max string length. Only applicable if typeName is 'String'.

        DotsC_TypeId m_typeId; //TypeId belonging to the type of this member. Only valid if memberType is object or enum.
    };
    typedef VectorShm<MemberDescriptionShm>::Type MemberDescriptionVectorShm;

    //-------------------------------------------
    //EnumDescriptionShm
    //-------------------------------------------
    class EnumDescriptionShm
    {
    public:
        EnumDescriptionShm(const EnumDescription* ed, boost::interprocess::managed_shared_memory* shm)
            :m_typeId(ed->GetTypeId())
            ,m_file(ed->FileName(), shm->get_segment_manager())
            ,m_name(ed->GetName(), shm->get_segment_manager())
            ,m_enumerationValues(shm->get_segment_manager())
            ,m_checkSum(ed->GetCheckSum())
        {
            for (int i=0; i<ed->GetNumberOfValues(); ++i)
            {
                StringShm val(ed->GetValueName(i), shm->get_segment_manager());
                m_enumerationValues.push_back(val);
            }
        }

        const char* FileName() const {return m_file.c_str();}
        const char* Summary() const {return NULL;}
        DotsC_TypeId GetTypeId() const {return m_typeId;}
        const char* GetName() const {return m_name.c_str();}
        DotsC_TypeId GetCheckSum() const {return m_checkSum;}
        int GetNumberOfValues() const {return static_cast<int>(m_enumerationValues.size());}
        const char* GetValueName(DotsC_EnumerationValue val) const {return m_enumerationValues[static_cast<size_t>(val)].c_str();}
        int GetIndexOfValue(const std::string& valueName) const {return TypeRepositoryHelpers::GetIndexOfEnumValue(this, valueName);}

    private:
        DotsC_TypeId m_typeId;
        StringShm m_file;
        StringShm m_name;
        StringVectorShm m_enumerationValues;
        DotsC_TypeId m_checkSum;
    };
    typedef MapShm<EnumDescriptionShm>::Type EnumMapShm;

    //-------------------------------------------
    //ExceptionDescriptionShm
    //-------------------------------------------
    class ExceptionDescriptionShm
    {
    public:
        ExceptionDescriptionShm(const ExceptionDescription* ed, boost::interprocess::managed_shared_memory* shm)
            :m_typeId(ed->GetTypeId())
            ,m_file(ed->FileName(), shm->get_segment_manager())
            ,m_name(ed->GetName(), shm->get_segment_manager())
        {
        }

        const char* FileName() const {return m_file.c_str();}
        const char* Summary() const {return NULL;}
        DotsC_TypeId GetTypeId() const {return m_typeId;}
        const char* GetName() const {return m_name.c_str();}
        const ExceptionDescriptionShm* GetBaseClass() const {return m_base.get();}

        const void SetBaseClass(const ExceptionDescriptionShm* base) {m_base=base;}

    private:
        DotsC_TypeId m_typeId;
        StringShm m_file;
        StringShm m_name;
        boost::interprocess::offset_ptr<const ExceptionDescriptionShm> m_base;
    };
    typedef MapShm<ExceptionDescriptionShm>::Type ExceptionMapShm;

    //-------------------------------------------
    //PropertyDescriptionShm
    //-------------------------------------------
    class PropertyDescriptionShm
    {
    public:
        PropertyDescriptionShm(const PropertyDescription* pd, boost::interprocess::managed_shared_memory* shm)
            :m_typeId(pd->GetTypeId())
            ,m_file(pd->FileName(), shm->get_segment_manager())
            ,m_name(pd->GetName(), shm->get_segment_manager())
            ,m_members(shm->get_segment_manager())
        {
            for (int i=0; i<pd->GetNumberOfMembers(); ++i)
            {
                MemberDescriptionShm member(pd->GetMember(i), shm);
                m_members.push_back(member);
            }
        }

        const char* FileName() const {return m_file.c_str();}
        const char* Summary() const {return NULL;}
        DotsC_TypeId GetTypeId() const {return m_typeId;}
        const char* GetName() const {return m_name.c_str();}
        int GetNumberOfMembers() const {return static_cast<int>(m_members.size());}
        DotsC_MemberIndex GetMemberIndex(const std::string& memberName) const {return TypeRepositoryHelpers::GetPropertyMemberIndex<PropertyDescriptionShm, MemberDescriptionShm>(this, memberName);}
        const MemberDescriptionShm* GetMember(DotsC_MemberIndex index) const {return &m_members[static_cast<size_t>(index)];}

    private:
        DotsC_TypeId m_typeId;
        StringShm m_file;
        StringShm m_name;
        MemberDescriptionVectorShm m_members;
    };
    typedef boost::interprocess::offset_ptr<const PropertyDescriptionShm> PropertyDescriptionShmPtr;
    typedef MapShm<PropertyDescriptionShm>::Type PropertyMapShm;

    //-------------------------------------------
    //ParameterDescriptionShm
    //-------------------------------------------
    struct ValueDefinitionShm
    {
        StringShm stringVal;
        DotsC_Int64 hashedVal;
        union
        {
            DotsC_Int32 int32Val;
            DotsC_Int64 int64Val;
            DotsC_Float32 float32Val;
            DotsC_Float64 float64Val;
            bool boolVal;
        };

        ValueDefinitionShm(boost::interprocess::managed_shared_memory* shm)
            :stringVal(shm->get_segment_manager())
        {
        }

        ValueDefinitionShm(const char* strVal, boost::interprocess::managed_shared_memory* shm)
            :stringVal(strVal, shm->get_segment_manager())
        {
        }

        ValueDefinitionShm(const char* strVal, size_t size, boost::interprocess::managed_shared_memory* shm)
            :stringVal(strVal, size, shm->get_segment_manager())
        {
        }

        ValueDefinitionShm(const ValueDefinitionShm& other)
            :stringVal(other.stringVal)
            ,hashedVal(other.hashedVal)
            ,int64Val(other.int64Val)
        {
        }

        ValueDefinitionShm& operator=(const ValueDefinitionShm& other)
        {
            stringVal=other.stringVal;
            hashedVal=other.hashedVal;
            int64Val=other.int64Val;
            return *this;
        }
    };
    typedef VectorShm<ValueDefinitionShm>::Type ParameterValuesShm;

    class ParameterDescriptionShm
    {
    public:
        ParameterDescriptionShm(const ParameterDescription* pd, boost::interprocess::managed_shared_memory* shm);

        const char* Summary() const {return NULL;}
        const char* GetName() const {return m_name.c_str();}
        DotsC_MemberType GetMemberType() const {return m_memberType;}
        DotsC_TypeId GetTypeId() const {return m_typeId;}
        bool IsArray() const {return m_isArray;}
        int GetArraySize() const {return static_cast<int>(m_values.size());}
        bool IsHidden() const {return m_hidden;}

        //Get parameter values - depending on actual type of the parameter
        //For entityId use GetInt64Value for typeId and GetHashedValue for instanceId
        boost::int32_t GetInt32Value(int index) const {return m_values[static_cast<size_t>(index)].int32Val;}
        boost::int64_t GetInt64Value(int index) const {return m_values[static_cast<size_t>(index)].int64Val;}
        float GetFloat32Value(int index) const {return m_values[static_cast<size_t>(index)].float32Val;}
        double GetFloat64Value(int index) const {return m_values[static_cast<size_t>(index)].float64Val;}
        bool GetBoolValue(int index) const {return m_values[static_cast<size_t>(index)].boolVal;}
        const char* GetStringValue(int index) const {return m_values[static_cast<size_t>(index)].stringVal.c_str();}
        std::pair<const char*, size_t> GetObjectValue(int index) const
        {
            const ValueDefinitionShm& v=m_values[static_cast<size_t>(index)];
            return std::make_pair(v.stringVal.c_str(), v.stringVal.size());
        }
        std::pair<const char*, size_t> GetBinaryValue(int index) const
        {
            const ValueDefinitionShm& v=m_values[static_cast<size_t>(index)];
            return std::make_pair(v.stringVal.c_str(), v.stringVal.size());
        }
        std::pair<boost::int64_t, const char*> GetHashedValue(int index) const
        {
            const ValueDefinitionShm& v=m_values[static_cast<size_t>(index)];
            return std::make_pair(v.hashedVal, v.stringVal.empty() ? NULL : v.stringVal.c_str());
        }

    private:
        StringShm m_name;
        DotsC_MemberType m_memberType;
        bool m_isArray;
        bool m_hidden;
        DotsC_TypeId m_typeId; //enum or objects type
        ParameterValuesShm m_values;
    };
    typedef MapShm<ParameterDescriptionShm, StringShm>::Type ParameterMapShm;
    typedef boost::interprocess::offset_ptr<const ParameterDescriptionShm> ParameterDescriptionShmPtr;
    typedef VectorShm<ParameterDescriptionShmPtr>::Type ParameterPtrVectorShm;
    typedef MapShm<ParameterDescriptionShmPtr>::Type ParameterPtrMapShm;

    //-------------------------------------------
    //MemberMappingDescriptionShm
    //-------------------------------------------
    typedef std::pair<DotsC_MemberIndex, DotsC_ArrayIndex> MemberRef;
    typedef VectorShm<MemberRef>::Type MemberRefVectorShm;
    class MemberMappingDescriptionShm
    {
    public:
        MemberMappingDescriptionShm(const MemberMappingDescription* md, boost::interprocess::managed_shared_memory* shm)
            :m_kind(md->GetMappingKind())
            ,m_memberRefs(shm->get_segment_manager())
        {
            for (int i=0; i<md->MemberReferenceDepth(); ++i)
            {
                m_memberRefs.push_back(md->GetMemberReference(i));
            }
        }

        MemberMappingDescriptionShm(const MemberMappingDescriptionShm& other)
            :m_kind(other.m_kind)
            ,m_paramIndex(other.m_paramIndex)
            ,m_memberRefs(other.m_memberRefs)
            ,m_paramRef(other.m_paramRef)
        {
        }

        MemberMappingDescriptionShm& operator=(const MemberMappingDescriptionShm& other)
        {
            m_kind=other.m_kind;
            m_paramIndex=other.m_paramIndex;
            m_memberRefs=other.m_memberRefs;
            m_paramRef=other.m_paramRef;
            return *this;
        }

        int MemberReferenceDepth() const {return static_cast<int>(m_memberRefs.size());}
        std::pair<DotsC_MemberIndex, DotsC_ArrayIndex> GetMemberReference(int depth) const {return m_memberRefs[static_cast<size_t>(depth)];}
        DotsC_PropertyMappingKind GetMappingKind() const {return m_kind;}
        std::pair<const ParameterDescriptionShm*, int /*paramIndex*/> GetParameter() const {return std::make_pair(m_paramRef.get(), m_paramIndex);}

        void SetParamRef(const ParameterDescriptionShm* param, int paramIndex)
        {
            m_paramRef=param;
            m_paramIndex=paramIndex;
        }

    private:
        DotsC_PropertyMappingKind m_kind;
        int m_paramIndex;
        MemberRefVectorShm m_memberRefs;
        ParameterDescriptionShmPtr m_paramRef;
    };
    typedef VectorShm<MemberMappingDescriptionShm>::Type MemberMappingVectorShm;

    //-------------------------------------------
    //PropertyMappingDescriptionShm
    //-------------------------------------------
    struct ClassDescriptionShm; //forward declaration
    typedef boost::interprocess::offset_ptr<const ClassDescriptionShm> ClassDescriptionShmPtr;
    typedef VectorShm<ClassDescriptionShmPtr>::Type ClassPtrVectorShm;
    class PropertyMappingDescriptionShm
    {
    public:
        PropertyMappingDescriptionShm(const PropertyMappingDescription* pm,
                                      const ClassDescriptionShm* class_,
                                      const PropertyDescriptionShm* property,
                                      boost::interprocess::managed_shared_memory* shm)
            :m_file(pm->FileName(), shm->get_segment_manager())
            ,m_class(class_)
            ,m_property(property)
            ,m_memberMappings(shm->get_segment_manager())
        {
        }

        PropertyMappingDescriptionShm(const PropertyMappingDescriptionShm& other)
            :m_file(other.m_file)
            ,m_class(other.m_class)
            ,m_property(other.m_property)
            ,m_memberMappings(other.m_memberMappings)
        {
        }

        PropertyMappingDescriptionShm& operator=(const PropertyMappingDescriptionShm& other)
        {
            m_file=other.m_file;
            m_class=other.m_class;
            m_property=other.m_property;
            m_memberMappings=other.m_memberMappings;
            return *this;
        }

        const char* FileName() const {return m_file.c_str();}
        const char* Summary() const {return NULL;}
        const PropertyDescriptionShm* GetProperty() const {return m_property.get();}
        const ClassDescriptionShm* GetClass() const {return m_class.get();}
        const MemberMappingDescriptionShm* GetMemberMapping(int propertyMemberIndex) const {return &m_memberMappings[static_cast<size_t>(propertyMemberIndex)];}

        void AddMemberMapping(const MemberMappingDescriptionShm& mmShm) {m_memberMappings.push_back(mmShm);}

    private:
        StringShm m_file;
        ClassDescriptionShmPtr m_class;
        PropertyDescriptionShmPtr m_property;
        MemberMappingVectorShm m_memberMappings;
    };
    typedef VectorShm<PropertyMappingDescriptionShm>::Type PropertyMappingVectorShm;

    //-----------------------------------------------------------------------------------
    //CreateRoutineDescriptionShm - just a dummy impl. not needed in shared memory
    //-----------------------------------------------------------------------------------
    class CreateRoutineDescriptionShm
    {
    public:
        CreateRoutineDescriptionShm() {}
        const char* Summary() const {return NULL;}
        const char* GetName() const {return NULL;}
        int GetNumberOfInParameters() const {return 0;}
        const MemberDescriptionShm* GetInParameterMember(int /*index*/) const {return NULL;}
        int GetNumberOfDefaultValues() const {return 0;}
        const MemberDescriptionShm* GetDefaultValueMember(int /*index*/) const {return NULL;}
        std::pair<const ParameterDescriptionShm*, int> GetDefaultValue(int /*index*/) const {const ParameterDescriptionShm* pd=NULL; return std::make_pair(pd, 0);}
    };

    //-------------------------------------------
    //ClassDescriptionShm
    //-------------------------------------------
    struct ClassDescriptionShm
    {
        ClassDescriptionShm(const ClassDescription* cd, boost::interprocess::managed_shared_memory* shm)
            :m_typeId(cd->GetTypeId())
            ,m_file(cd->FileName(), shm->get_segment_manager())
            ,m_name(cd->GetName(), shm->get_segment_manager())
            ,m_initialSize(cd->InitialSize())
            ,m_ownSize(cd->OwnSize())
            ,m_descendants(shm->get_segment_manager())
            ,m_members(shm->get_segment_manager())
            ,m_properties(shm->get_segment_manager())
            ,m_ownParameters(shm->get_segment_manager())
        {
            int totalNumMembers=cd->GetNumberOfMembers();
            int startOwnMembers=totalNumMembers-cd->GetNumberOfOwnMembers();
            for (int i=startOwnMembers; i<totalNumMembers; ++i)
            {
                MemberDescriptionShm member(cd->GetMember(i), shm);
                m_members.push_back(member);
            }
        }

        const char* FileName() const {return m_file.c_str();}
        const char* Summary() const {return NULL;}
        DotsC_TypeId GetTypeId() const {return m_typeId;}
        const char* GetName() const {return m_name.c_str();}
        const ClassDescriptionShm* GetBaseClass() const {return m_base.get();}
        int GetNumberOfDescendants() const {return static_cast<int>(m_descendants.size());}
        const ClassDescriptionShm* GetDescendant(int index) const {return m_descendants[static_cast<size_t>(index)].get();}
        int GetNumberOfOwnMembers() const {return static_cast<int>(m_members.size());}
        int GetNumberOfInheritedMembers() const {return m_base ? m_base->GetNumberOfMembers() : 0;}
        int GetNumberOfMembers() const {return GetNumberOfOwnMembers()+GetNumberOfInheritedMembers();}


        DotsC_MemberIndex GetMemberIndex(const std::string& memberName) const
        {
            for (MemberDescriptionVectorShm::const_iterator it=m_members.begin(); it!=m_members.end(); ++it)
            {
                if (it->GetName()==memberName)
                {
                    return static_cast<DotsC_MemberIndex>(std::distance(m_members.begin(), it) + GetNumberOfInheritedMembers());
                }
            }

            if (m_base)
            {
                return m_base->GetMemberIndex(memberName);
            }

            return -1;
        }


        const MemberDescriptionShm* GetMember(DotsC_MemberIndex index) const
        {
            int numInherited=GetNumberOfInheritedMembers();
            if (index<numInherited)
            {
                return m_base->GetMember(index);
            }
            return &m_members[index-numInherited];
        }

        int GetNumberOfOwnParameters() const {return static_cast<int>(m_ownParameters.size());}
        int GetNumberOfInheritedParameters() const {return m_base ? m_base->GetNumberOfParameters() : 0;}
        int GetNumberOfParameters() const {return GetNumberOfOwnParameters()+GetNumberOfInheritedParameters();}

        const ParameterDescriptionShm* GetParameter(DotsC_ParameterIndex index) const
        {
            int numInherited=GetNumberOfInheritedParameters();
            if (index<numInherited)
            {
                return m_base->GetParameter(index);
            }

            return m_ownParameters[index-numInherited].get();
        }

        void GetPropertyIds(std::set<DotsC_TypeId>& propertyIds) const
        {
            if (m_base)
            {
                m_base->GetPropertyIds(propertyIds);
            }

            for (PropertyMappingVectorShm::const_iterator it=m_properties.begin(); it!=m_properties.end(); ++it)
            {
                propertyIds.insert(it->GetProperty()->GetTypeId());
            }
        }

        const PropertyMappingDescriptionShm* GetPropertyMapping(DotsC_TypeId propertyTypeId, bool & isInherited) const
        {
            for (PropertyMappingVectorShm::const_iterator it=m_properties.begin(); it!=m_properties.end(); ++it)
            {
                if (it->GetProperty()->GetTypeId()==propertyTypeId)
                {
                    isInherited=false;
                    return &(*it);
                }
            }

            if (m_base)
            {
                const PropertyMappingDescriptionShm* tmp=m_base->GetPropertyMapping(propertyTypeId, isInherited);
                isInherited=true;
                return tmp;
            }

            return NULL;
        }

        int GetNumberOfCreateRoutines() const {return 0;}
        const CreateRoutineDescriptionShm* GetCreateRoutine(int /*index*/) const {return NULL;}
        int InitialSize() const {return m_initialSize;}
        int OwnSize() const {return m_ownSize;}

        void SetBaseClass(const ClassDescriptionShm* base) {m_base=base;}
        void AddDescendant(const ClassDescriptionShm* descendant) {m_descendants.push_back(ClassDescriptionShmPtr(descendant));}
        void AddOwnParameter(const ParameterDescriptionShmPtr& paramPtr) {m_ownParameters.push_back(paramPtr);}
        void AddPropertyMapping(const PropertyMappingDescriptionShm& property) {m_properties.push_back(property);}

    private:
        DotsC_TypeId m_typeId;
        StringShm m_file;
        StringShm m_name;
        int m_initialSize;
        int m_ownSize;

        ClassDescriptionShmPtr m_base;
        ClassPtrVectorShm m_descendants;
        MemberDescriptionVectorShm m_members;
        PropertyMappingVectorShm m_properties;
        ParameterPtrVectorShm m_ownParameters;
    };
    typedef MapShm<ClassDescriptionShm>::Type ClassMapShm;

    class RepositoryShm : private boost::noncopyable
    {
    public:
        RepositoryShm(boost::interprocess::managed_shared_memory* shm)
            :m_enums(std::less<const DotsC_TypeId>(), shm->get_segment_manager())
            ,m_classes(std::less<const DotsC_TypeId>(), shm->get_segment_manager())
            ,m_properties(std::less<const DotsC_TypeId>(), shm->get_segment_manager())
            ,m_exceptions(std::less<const DotsC_TypeId>(), shm->get_segment_manager())
            ,m_params(std::less<const StringShm>(), shm->get_segment_manager())
        {
        }

        //Enmerations
        const EnumDescriptionShm* GetEnum(DotsC_TypeId typeId) const {return GetPtr<EnumDescriptionShm>(m_enums, typeId);}
        size_t GetNumberOfEnums() const {return m_enums.size();}
        void GetAllEnumTypeIds(std::set<DotsC_TypeId>& typeIds) const {GetKeys<EnumDescriptionShm>(m_enums, typeIds);}

        //properties
        const PropertyDescriptionShm* GetProperty(DotsC_TypeId typeId) const {return GetPtr<PropertyDescriptionShm>(m_properties, typeId);}
        size_t GetNumberOfProperties() const {return m_properties.size();}
        void GetAllPropertyTypeIds(std::set<DotsC_TypeId>& typeIds) const {GetKeys<PropertyDescriptionShm>(m_properties, typeIds);}

        //classes
        const ClassDescriptionShm* GetClass(DotsC_TypeId typeId) const {return GetPtr<ClassDescriptionShm>(m_classes, typeId);}
        size_t GetNumberOfClasses() const {return m_classes.size();}
        void GetAllClassTypeIds(std::set<DotsC_TypeId>& typeIds) const {GetKeys<ClassDescriptionShm>(m_classes, typeIds);}

        //exceptionsm_properties
        const ExceptionDescriptionShm* GetException(DotsC_TypeId typeId) const {return GetPtr<ExceptionDescriptionShm>(m_exceptions, typeId);}
        size_t GetNumberOfExceptions() const {return m_exceptions.size();}
        void GetAllExceptionTypeIds(std::set<DotsC_TypeId>& typeIds) const {GetKeys<ExceptionDescriptionShm>(m_exceptions, typeIds);}

    private:
        template <class Val>
        static const Val* GetPtr(const typename MapShm<Val>::Type& m, DotsC_TypeId key)
        {
            typename MapShm<Val>::Type::const_iterator it=m.find(key);
            if (it!=m.end())
            {
                return &(it->second);
            }
            return NULL;
        }

        template <class Val>
        static void GetKeys(const typename MapShm<Val>::Type& m, std::set<DotsC_TypeId>& keys)
        {
            for (typename MapShm<Val>::Type::const_iterator it=m.begin(); it!=m.end(); ++it)
            {
                keys.insert(it->first);
            }
        }

        EnumMapShm m_enums;
        ClassMapShm m_classes;
        PropertyMapShm m_properties;
        ExceptionMapShm m_exceptions;
        ParameterMapShm m_params;

        friend class RepositoryKeeper;
    };

    /**This class creates the shared memory. It initiates the dou-parsing and then
     * copies the data into the shared memory.
     */
    class RepositoryKeeper :
            public Safir::Utilities::Synchronized,
            private boost::noncopyable
    {
    public:
        static void Initialize(const std::string& path_);
        static const RepositoryShm* GetRepository();

    private:
        std::string m_path;
        Safir::Utilities::StartupSynchronizer m_startupSynchronizer;
        boost::scoped_ptr<boost::interprocess::managed_shared_memory> m_sharedMemory;
        RepositoryShm* m_repository;

        static RepositoryKeeper& Instance();

        RepositoryKeeper();
        ~RepositoryKeeper();

        void LoadData();

        //StartupSynchronizer stuff
        virtual void Create();
        virtual void Use();
        virtual void Destroy();
    };

    //type traits for shared memory repository. Needed to be able to use the
    //generic stuff in dots_internal.
    template<> struct TypeRepositoryTraits<RepositoryShm>
    {
        typedef RepositoryShm RepositoryType;
        typedef ClassDescriptionShm ClassDescriptionType;
        typedef MemberDescriptionShm MemberDescriptionType;
        typedef PropertyDescriptionShm PropertyDescriptionType;
        typedef ExceptionDescriptionShm ExceptionDescriptionType;
        typedef ParameterDescriptionShm ParameterDescriptionType;
        typedef EnumDescriptionShm EnumDescriptionType;
        typedef MemberMappingDescriptionShm MemberMappingDescriptionType;
        typedef PropertyMappingDescriptionShm PropertyMappingDescriptionType;
        typedef CreateRoutineDescriptionShm CreateRoutineDescriptionType;
    };
}
}
}
}

#endif
