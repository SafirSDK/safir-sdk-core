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
#ifndef __DOTS_INTERNAL_TOSTRING_HELPERS__
#define __DOTS_INTERNAL_TOSTRING_HELPERS__

#include <iostream>
#include <Safir/Dob/Typesystem/ToolSupport/ParseError.h>
#include <Safir/Dob/Typesystem/ToolSupport/Internal/BlobToJsonSerializer.h>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace ToolSupport
{
namespace Internal
{
    //Helper class for dumping a complete repository to a stream like stringstream och cout.
    template <class RepT, class Traits=Safir::Dob::Typesystem::ToolSupport::TypeRepositoryTraits<RepT> >
    class ToStringHelper
    {
    public:
        typedef typename Traits::RepositoryType RepositoryType;
        typedef typename Traits::ClassDescriptionType ClassDescriptionType;
        typedef typename Traits::MemberDescriptionType MemberDescriptionType;
        typedef typename Traits::PropertyDescriptionType PropertyDescriptionType;
        typedef typename Traits::ExceptionDescriptionType ExceptionDescriptionType;
        typedef typename Traits::ParameterDescriptionType ParameterDescriptionType;
        typedef typename Traits::EnumDescriptionType EnumDescriptionType;
        typedef typename Traits::MemberMappingDescriptionType MemberMappingDescriptionType;
        typedef typename Traits::PropertyMappingDescriptionType PropertyMappingDescriptionType;
        typedef typename Traits::CreateRoutineDescriptionType CreateRoutineDescriptionType;

        ToStringHelper(const RepositoryType* rep, bool includeCreateRoutines=false) //since createRoutines are not stored in shm, this enables possibility to compare output from local and shm repos.
            :m_rep(rep)
            ,m_includeCreateRoutines(includeCreateRoutines)
        {
        }

        void RepositoryToString(std::ostream& os) const {DumpRepository(os);}
        void TypeInfoToString(DotsC_TypeId typeId, std::ostream& os) const;
        void ParameterKeyToString(const ParameterDescriptionType* c, int index, std::ostream& os) const;
        void ParameterValueToString(const ParameterDescriptionType* c, int index, std::ostream& os) const;

    private:
        const RepositoryType* m_rep;
        const bool m_includeCreateRoutines;
        void DumpRepository(std::ostream& os) const;
        void DumpClassDescription(const ClassDescriptionType* c, std::ostream& os) const;
        void DumpMemberDescription(const MemberDescriptionType* c, std::ostream& os) const;
        void DumpParameterDescription(const ParameterDescriptionType* c, std::ostream& os) const;
        void DumpEnumerationDescription(const EnumDescriptionType* c, std::ostream& os) const;
        void DumpExceptionDescription(const ExceptionDescriptionType* c, std::ostream& os) const;
        void DumpPropertyDescription(const PropertyDescriptionType* c, std::ostream& os) const;
        void DumpMappingDescription(const PropertyMappingDescriptionType* c, bool inherited, std::ostream& os) const;
        void DumpCreateRoutineDescription(const CreateRoutineDescriptionType* c, std::ostream& os) const;

        void TypeIdToString(boost::int64_t tid, std::ostream& os) const;
        void HashedValToString(const std::pair<boost::int64_t, const char*>& hv, std::ostream& os) const;
    };

    template <class RepT, class Traits>
    void ToStringHelper<RepT, Traits>::TypeInfoToString(DotsC_TypeId typeId, std::ostream& os) const
    {
        const ClassDescriptionType* cd=m_rep->GetClass(typeId);
        if (cd)
        {
            DumpClassDescription(cd, os);
            return;
        }
        const EnumDescriptionType* ed=m_rep->GetEnum(typeId);
        if (ed)
        {
            DumpEnumerationDescription(ed, os);
            return;
        }
        const PropertyDescriptionType* pd=m_rep->GetProperty(typeId);
        if (pd)
        {
            DumpPropertyDescription(pd, os);
            return;
        }
        const ExceptionDescriptionType* ex=m_rep->GetException(typeId);
        if (ex)
        {
            DumpExceptionDescription(ex, os);
            return;
        }

        os<<"No such type!"<<std::endl;
    }

    template <class RepT, class Traits>
    void ToStringHelper<RepT, Traits>::TypeIdToString(boost::int64_t tid, std::ostream& os) const
    {
        const char* name=BasicTypeOperations::TypeIdToTypeName(m_rep, tid);
        if (name)
        {
            os<<name;
        }
        else
        {
            os<<"<UnknownType>";
        }
    }

    template <class RepT, class Traits>
    void ToStringHelper<RepT, Traits>::HashedValToString(const std::pair<boost::int64_t, const char*>& hv, std::ostream& os) const
    {
        os<<hv.first;
        if (hv.second)
        {
            os<<" ("<<hv.second<<")";
        }
    }

    template <class RepT, class Traits>
    void ToStringHelper<RepT, Traits>::ParameterKeyToString(const ParameterDescriptionType* c, int index, std::ostream& os) const
    {
        switch(c->GetKeyType())
        {
        case Int32MemberType:
        {
            os<<c->GetInt32Key(index);
        }
            break;
        case Int64MemberType:
        {
            os<<c->GetInt64Key(index);
        }
            break;

        case EntityIdMemberType:
        {
            TypeIdToString(c->GetInt64Key(index), os);
            os<<" : ";
            HashedValToString(c->GetHashedKey(index), os);
        }
            break;
        case TypeIdMemberType:
        {
            TypeIdToString(c->GetInt64Key(index), os);
        }
            break;
        case InstanceIdMemberType:
        case ChannelIdMemberType:
        case HandlerIdMemberType:
        {
            HashedValToString(c->GetHashedKey(index), os);
        }
            break;

        case StringMemberType:
        {
            os<<c->GetStringKey(index);
        }
            break;

        case EnumerationMemberType:
        {
            os<<m_rep->GetEnum(c->GetKeyTypeId())->GetValueName(c->GetInt32Key(index));
        }
            break;

        default:
            break;
        }
    }

    template <class RepT, class Traits>
    void ToStringHelper<RepT, Traits>::ParameterValueToString(const ParameterDescriptionType* c, int i, std::ostream& os) const
    {
        switch(c->GetMemberType())
        {
        case BooleanMemberType:
        {
            os<<std::boolalpha<<c->GetBoolValue(i)<<std::dec;

        }
            break;

        case Int32MemberType:
        {
            os<<c->GetInt32Value(i);
        }
            break;
        case Int64MemberType:
        {
            os<<c->GetInt64Value(i);
        }
            break;

        case EntityIdMemberType:
        {
            TypeIdToString(c->GetInt64Value(i), os);
            os<<" : ";
            HashedValToString(c->GetHashedValue(i), os);
        }
            break;
        case TypeIdMemberType:
        {
            TypeIdToString(c->GetInt64Value(i), os);
        }
            break;
        case InstanceIdMemberType:
        case ChannelIdMemberType:
        case HandlerIdMemberType:
        {
            HashedValToString(c->GetHashedValue(i), os);
        }
            break;

        case StringMemberType:
        {
            os<<c->GetStringValue(i)<<std::endl;
        }
            break;

        case ObjectMemberType:
        {
            const char* objParam=c->GetObjectValue(i).first;
            std::ostringstream json;
            (BlobToJsonSerializer<RepositoryType>(m_rep))(objParam, json);
            os<<std::endl<<json.str();
        }
            break;

        case EnumerationMemberType:
        {
            os<<m_rep->GetEnum(c->GetTypeId())->GetValueName(c->GetInt32Value(i));
        }
            break;

        case BinaryMemberType:
        {
            std::pair<const char*, size_t> bin=c->GetBinaryValue(i);
            if (bin.second>0)
            {
                std::string tmp(bin.first, bin.first+bin.second);
                os<<Safir::Dob::Typesystem::ToolSupport::Internal::SerializationUtils::ToBase64(tmp);
            }
        }
            break;

        case Float32MemberType:
        case Ampere32MemberType:
        case CubicMeter32MemberType:
        case Hertz32MemberType:
        case Joule32MemberType:
        case Kelvin32MemberType:
        case Kilogram32MemberType:
        case Meter32MemberType:
        case MeterPerSecond32MemberType:
        case MeterPerSecondSquared32MemberType:
        case Newton32MemberType:
        case Pascal32MemberType:
        case Radian32MemberType:
        case RadianPerSecond32MemberType:
        case RadianPerSecondSquared32MemberType:
        case Second32MemberType:
        case SquareMeter32MemberType:
        case Steradian32MemberType:
        case Volt32MemberType:
        case Watt32MemberType:
        {
            os<<c->GetFloat32Value(i);
        }
            break;

        case Float64MemberType:
        case Ampere64MemberType:
        case CubicMeter64MemberType:
        case Hertz64MemberType:
        case Joule64MemberType:
        case Kelvin64MemberType:
        case Kilogram64MemberType:
        case Meter64MemberType:
        case MeterPerSecond64MemberType:
        case MeterPerSecondSquared64MemberType:
        case Newton64MemberType:
        case Pascal64MemberType:
        case Radian64MemberType:
        case RadianPerSecond64MemberType:
        case RadianPerSecondSquared64MemberType:
        case Second64MemberType:
        case SquareMeter64MemberType:
        case Steradian64MemberType:
        case Volt64MemberType:
        case Watt64MemberType:
        {
            os<<c->GetFloat64Value(i);
        }
            break;
        }

    }

    template <class RepT, class Traits>
    void ToStringHelper<RepT, Traits>::DumpRepository(std::ostream& os) const
    {
        std::set<DotsC_TypeId> types;
        m_rep->GetAllClassTypeIds(types);
        for (std::set<DotsC_TypeId>::const_iterator it=types.begin(); it!=types.end(); ++it)
        {
            const ClassDescriptionType* tmp=m_rep->GetClass(*it);
            DumpClassDescription(tmp, os);
        }

        types.clear();
        m_rep->GetAllEnumTypeIds(types);
        for (std::set<DotsC_TypeId>::const_iterator it=types.begin(); it!=types.end(); ++it)
        {
            const EnumDescriptionType* tmp=m_rep->GetEnum(*it);
            DumpEnumerationDescription(tmp, os);
        }

        types.clear();
        m_rep->GetAllExceptionTypeIds(types);
        for (std::set<DotsC_TypeId>::const_iterator it=types.begin(); it!=types.end(); ++it)
        {
            const ExceptionDescriptionType* tmp=m_rep->GetException(*it);
            DumpExceptionDescription(tmp, os);
        }

        types.clear();
        m_rep->GetAllPropertyTypeIds(types);
        for (std::set<DotsC_TypeId>::const_iterator it=types.begin(); it!=types.end(); ++it)
        {
            const PropertyDescriptionType* tmp=m_rep->GetProperty(*it);
            DumpPropertyDescription(tmp, os);
        }
    }

    template <class RepT, class Traits>
    void ToStringHelper<RepT, Traits>::DumpClassDescription(const ClassDescriptionType* c, std::ostream& os) const
    {
        os<<"=========================================================="<<std::endl;
        os<<"Class: "<<c->GetName()<<std::endl;
        os<<"TypeId: "<<c->GetTypeId()<<std::endl;
        os<<"File: "<<c->FileName()<<std::endl;
        if (c->GetBaseClass()!=NULL)
            os<<"baseClass: "<<c->GetBaseClass()->GetName()<<std::endl;

        for (int i=0; i<c->GetNumberOfMembers(); ++i)
        {
            DumpMemberDescription(c->GetMember(i), os);
        }

        for (int i=0; i<c->GetNumberOfCreateRoutines(); ++i)
        {
            DumpCreateRoutineDescription(c->GetCreateRoutine(i), os);
        }

        for (int i=0; i<c->GetNumberOfParameters(); ++i)
        {
            DumpParameterDescription(c->GetParameter(i), os);
        }

        std::set<DotsC_TypeId> properties;
        c->GetPropertyIds(properties);
        for (std::set<DotsC_TypeId>::const_iterator it=properties.begin(); it!=properties.end(); ++it)
        {
            bool inherited;
            const PropertyMappingDescriptionType* tmp=c->GetPropertyMapping(*it, inherited);
            DumpMappingDescription(tmp, inherited, os);
        }
    }

    template <class RepT, class Traits>
    void ToStringHelper<RepT, Traits>::DumpEnumerationDescription(const EnumDescriptionType* c, std::ostream& os) const
    {
        os<<"=========================================================="<<std::endl;
        os<<"Enumeration: "<<c->GetName()<<std::endl;
        os<<"TypeId: "<<c->GetTypeId()<<std::endl;
        os<<"File: "<<c->FileName()<<std::endl;

        for (int i=0; i<c->GetArraySize(); ++i)
        {
            os<<"    Value ("<<i<<"): "<<c->GetValueName(i)<<std::endl;
        }
    }

    template <class RepT, class Traits>
    void ToStringHelper<RepT, Traits>::DumpExceptionDescription(const ExceptionDescriptionType* c, std::ostream& os) const
    {
        os<<"=========================================================="<<std::endl;
        os<<"Exception: "<<c->GetName()<<std::endl;
        os<<"TypeId: "<<c->GetTypeId()<<std::endl;
        os<<"File: "<<c->FileName()<<std::endl;
        if (c->GetBaseClass()!=NULL)
            os<<"baseClass: "<<c->GetBaseClass()->GetName()<<std::endl;
    }

    template <class RepT, class Traits>
    void ToStringHelper<RepT, Traits>::DumpPropertyDescription(const PropertyDescriptionType* c, std::ostream& os) const
    {
        os<<"=========================================================="<<std::endl;
        os<<"Property: "<<c->GetName()<<std::endl;
        os<<"TypeId: "<<c->GetTypeId()<<std::endl;
        os<<"File: "<<c->FileName()<<std::endl;

        for (int i=0; i<c->GetNumberOfMembers(); ++i)
        {
            DumpMemberDescription(c->GetMember(i), os);
        }
    }

    template <class RepT, class Traits>
    void ToStringHelper<RepT, Traits>::DumpMemberDescription(const MemberDescriptionType* c, std::ostream& os) const
    {
        os<<"    Member: "<<c->GetName()<<", type=";
        switch (c->GetMemberType())
        {
        case EnumerationMemberType:
            os<<m_rep->GetEnum(c->GetTypeId())->GetName();
            break;
        case ObjectMemberType:
            os<<m_rep->GetClass(c->GetTypeId())->GetName();
            break;
        case StringMemberType:            
            os<<BasicTypeOperations::MemberTypeToString(c->GetMemberType())<<", maxLen="<<c->GetMaxLength();
            break;
        default:
            os<<BasicTypeOperations::MemberTypeToString(c->GetMemberType());
            break;
        }

        switch (c->GetCollectionType())
        {
        case ArrayCollectionType:
            os<<", Array, arraySize="<<c->GetArraySize()<<std::endl;
            break;
        case SequenceCollectionType:
            os<<", Sequence"<<std::endl;
            break;
        case DictionaryCollectionType:
            os<<", Dictionary"<<std::endl;
            break;
        default:
            os<<std::endl;
            break;
        }
    }

    template <class RepT, class Traits>
    void ToStringHelper<RepT, Traits>::DumpParameterDescription(const ParameterDescriptionType* c, std::ostream& os) const
    {
        os<<"    Parameter: "<<c->GetName();

        if (c->IsHidden())
        {
            os<<" (HIDDEN)";
        }

        switch (c->GetMemberType())
        {
        case EnumerationMemberType:
            os<<", type="<<m_rep->GetEnum(c->GetTypeId())->GetName();
            break;
        case ObjectMemberType:
            os<<", type="<<m_rep->GetClass(c->GetTypeId())->GetName();
            break;
        default:
            os<<", type="<<BasicTypeOperations::MemberTypeToString(c->GetMemberType());
            break;
        }

        switch (c->GetCollectionType())
        {
        case SingleValueCollectionType:
        {
            os<<std::endl;
            for (int i=0; i<c->GetNumberOfValues(); ++i)
            {
                os<<"      value=";
                ParameterValueToString(c, i, os);
                os<<std::endl;
            }
        }
            break;
        case ArrayCollectionType:
        {
            os<<", Array, size="<<c->GetNumberOfValues()<<std::endl;
            for (int i=0; i<c->GetNumberOfValues(); ++i)
            {
                os<<"      value["<<i<<"]=";
                ParameterValueToString(c, i, os);
                os<<std::endl;
            }
        }
            break;
        case SequenceCollectionType:
        {
            os<<", Sequence, size="<<c->GetNumberOfValues()<<std::endl;
            for (int i=0; i<c->GetNumberOfValues(); ++i)
            {
                os<<"      value["<<i<<"]=";
                ParameterValueToString(c, i, os);
                os<<std::endl;
            }
        }
            break;
        case DictionaryCollectionType:
        {
            os<<", Dictionary<"<<(c->GetKeyType()==EnumerationMemberType ? m_rep->GetEnum(c->GetKeyTypeId())->GetName() : BasicTypeOperations::MemberTypeToString(c->GetKeyType()))<<">, size="<<c->GetNumberOfValues()<<std::endl;
            for (int i=0; i<c->GetNumberOfValues(); ++i)
            {
                os<<"      value[";
                ParameterKeyToString(c, i, os);
                os<<"]=";
                ParameterValueToString(c, i, os);
                os<<std::endl;
            }
        }
            break;
        }
    }

    template <class RepT, class Traits>
    void ToStringHelper<RepT, Traits>::DumpMappingDescription(const PropertyMappingDescriptionType* pmd, bool inherited, std::ostream& os) const
    {
        const PropertyDescriptionType* p=pmd->GetProperty();
        const ClassDescriptionType* c=pmd->GetClass();
        os<<"    Property: "<<p->GetName()<<", inherited="<<std::boolalpha<<inherited<<std::dec<<std::endl;
        os<<"      File: "<<pmd->FileName()<<std::endl;

        for (int i=0; i<p->GetNumberOfMembers(); ++i)
        {
            const MemberMappingDescriptionType* md=pmd->GetMemberMapping(i);
            const MemberDescriptionType* propertyMember=p->GetMember(i);

            os<<"      PropertyMember:  "<<propertyMember->GetName()<<std::endl;
            switch(md->GetMappingKind())
            {
            case MappedToParameter:
            {
                std::pair<const ParameterDescriptionType*, int> par=md->GetParameter();
                os<<"        MappingKind:     ValueMapping"<<std::endl;
                os<<"        MappedParam:       "<<par.first->GetName();
                if (propertyMember->GetCollectionType()==SingleValueCollectionType)
                {
                    if (par.first->GetCollectionType()==ArrayCollectionType)
                    {
                        os<<"["<<par.second<<"]";
                    }
                    else if (par.first->GetCollectionType()==DictionaryCollectionType)
                    {
                        os<<"[";
                        ParameterKeyToString(par.first, par.second, os);
                        os<<"]";
                    }
                }
                os<<std::endl;
            }
                break;
            case MappedToMember:
            {
                os<<"        MappingKind:     MemberMapping"<<std::endl;
                os<<"        MemberRef:       ";
                const ClassDescriptionType* currentClass=c;
                for (int memRef=0; memRef<md->MemberReferenceDepth(); ++memRef)
                {
                    std::pair<DotsC_MemberIndex, DotsC_ArrayIndex> ref=md->GetMemberReference(memRef);
                    const MemberDescriptionType* member=currentClass->GetMember(ref.first);
                    os<<"->"<<member->GetName();
                    if (member->GetCollectionType()==ArrayCollectionType && propertyMember->GetCollectionType()!=ArrayCollectionType)
                    {
                        os<<"["<<ref.second<<"]";
                    }

                    if (member->GetMemberType()==ObjectMemberType)
                    {
                        currentClass=m_rep->GetClass(member->GetTypeId());
                    }
                }
                os<<std::endl;
            }
                break;

            case MappedToNull:
            {
                os<<"        MappingKind:     NullMapping"<<std::endl;
            }
                break;
            }
        }
    }

    template <class RepT, class Traits>
    void ToStringHelper<RepT, Traits>::DumpCreateRoutineDescription(const CreateRoutineDescriptionType* c, std::ostream& os) const
    {
        if (!m_includeCreateRoutines)
        {
            return;
        }

        os<<"  - CreateRoutine: "<<c->GetName()<<std::endl;
        os<<"    summary:    "<<c->Summary()<<std::endl;
        for (int i=0; i<c->GetNumberOfInParameters(); ++i)
        {
            os<<"      Argument: "<<c->GetInParameterMember(i)->GetName()<<std::endl;
        }
        for (int i=0; i<c->GetNumberOfDefaultValues(); ++i)
        {
            std::pair<const ParameterDescriptionType*, int> val=c->GetDefaultValue(i);
            os<<"      DefaultVal: "<<c->GetDefaultValueMember(i)->GetName()<<"="<<val.first->GetName()<<"["<<val.second<<"]"<<std::endl;
        }
    }
}
}
}
}
} //end namespace Safir::Dob::Typesystem::ToolSupport::Internal

#endif
