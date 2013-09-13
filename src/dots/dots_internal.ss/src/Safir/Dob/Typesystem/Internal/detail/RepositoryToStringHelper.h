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
#ifndef __DOTS_INTERNAL_DETAIL_TOSTRING_HELPERS__
#define __DOTS_INTERNAL_DETAIL_TOSTRING_HELPERS__

#include <iostream>
#include <Safir/Dob/Typesystem/Internal/ParseError.h>
#include <Safir/Dob/Typesystem/Internal/detail/JsonToBlobSerializer.h>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
namespace detail
{

    //Helper class for dumping a complete repository to a stream like stringstream och cout.
    template <class RepT, class Traits=Safir::Dob::Typesystem::Internal::TypeRepositoryTraits<RepT> >
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

        ToStringHelper(const RepositoryType* rep)
            :m_rep(rep)
        {
        }

        void operator()(std::ostream& os) {DumpRepository(os);}

    private:
        const RepositoryType* m_rep;
        void DumpRepository(std::ostream& os);
        void DumpClassDescription(const ClassDescriptionType* c, std::ostream& os);
        void DumpMemberDescription(const MemberDescriptionType* c, std::ostream& os);
        void DumpParameterDescription(const ParameterDescriptionType* c, std::ostream& os);
        void DumpEnumerationDescription(const EnumDescriptionType* c, std::ostream& os);
        void DumpExceptionDescription(const ExceptionDescriptionType* c, std::ostream& os);
        void DumpPropertyDescription(const PropertyDescriptionType* c, std::ostream& os);
        void DumpMappingDescription(const PropertyMappingDescriptionType* c, std::ostream& os);
        void DumpCreateRoutineDescription(const CreateRoutineDescriptionType* c, std::ostream& os);

        void TypeIdToString(boost::int64_t tid, std::ostream& os) const;
        void HashedValToString(const std::pair<boost::int64_t, const char*>& hv, std::ostream& os) const;
    };

    template <class RepT, class Traits>
    void ToStringHelper<RepT, Traits>::TypeIdToString(boost::int64_t tid, std::ostream& os) const
    {
        const ClassDescriptionType* cd=m_rep->GetClass(tid);
        if (cd)
        {
            os<<cd->GetName();
            return;
        }
        const EnumDescriptionType* ed=m_rep->GetEnum(tid);
        if (ed)
        {
            os<<ed->GetName();
            return;
        }
        const PropertyDescriptionType* pd=m_rep->GetProperty(tid);
        if (pd)
        {
            os<<pd->GetName();
            return;
        }
        os<<"<UnknownType>";
    }

    template <class RepT, class Traits>
    void ToStringHelper<RepT, Traits>::HashedValToString(const std::pair<boost::int64_t, const char*>& hv, std::ostream& os) const
    {
        os<<hv.first;
        if (hv.second)
        {
            os<<" ("<<hv.second<<")"<<std::endl;
        }
    }

    template <class RepT, class Traits>
    void ToStringHelper<RepT, Traits>::DumpRepository(std::ostream& os)
    {
        std::vector<DotsC_TypeId> types;
        m_rep->GetAllClassTypeIds(types);
        for (std::vector<DotsC_TypeId>::const_iterator it=types.begin(); it!=types.end(); ++it)
        {
            const ClassDescriptionType* tmp=m_rep->GetClass(*it);
            DumpClassDescription(tmp, os);
        }

        types.clear();
        m_rep->GetAllEnumTypeIds(types);
        for (std::vector<DotsC_TypeId>::const_iterator it=types.begin(); it!=types.end(); ++it)
        {
            const EnumDescriptionType* tmp=m_rep->GetEnum(*it);
            DumpEnumerationDescription(tmp, os);
        }

        types.clear();
        m_rep->GetAllExceptionTypeIds(types);
        for (std::vector<DotsC_TypeId>::const_iterator it=types.begin(); it!=types.end(); ++it)
        {
            const ExceptionDescriptionType* tmp=m_rep->GetException(*it);
            DumpExceptionDescription(tmp, os);
        }

        types.clear();
        m_rep->GetAllPropertyTypeIds(types);
        for (std::vector<DotsC_TypeId>::const_iterator it=types.begin(); it!=types.end(); ++it)
        {
            const PropertyDescriptionType* tmp=m_rep->GetProperty(*it);
            DumpPropertyDescription(tmp, os);
        }
    }

    template <class RepT, class Traits>
    void ToStringHelper<RepT, Traits>::DumpClassDescription(const ClassDescriptionType* c, std::ostream& os)
    {
        os<<"=========================================================="<<std::endl;
        os<<"Class: "<<c->GetName()<<std::endl;
        os<<"TypeId: "<<c->GetTypeId()<<std::endl;
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

        std::vector<DotsC_TypeId> properties;
        c->GetPropertyIds(properties);
        for (std::vector<DotsC_TypeId>::const_iterator it=properties.begin(); it!=properties.end(); ++it)
        {
            bool inherited;
            const PropertyMappingDescriptionType* tmp=c->GetPropertyMapping(*it, inherited);
            DumpMappingDescription(tmp, os);
        }
    }

    template <class RepT, class Traits>
    void ToStringHelper<RepT, Traits>::DumpEnumerationDescription(const EnumDescriptionType* c, std::ostream& os)
    {
        os<<"=========================================================="<<std::endl;
        os<<"Enumeration: "<<c->GetName()<<std::endl;
        os<<"TypeId: "<<c->GetTypeId()<<std::endl;

        for (int i=0; i<c->GetNumberOfValues(); ++i)
        {
            os<<"    Value: "<<c->GetValueName(i)<<std::endl;
        }
    }

    template <class RepT, class Traits>
    void ToStringHelper<RepT, Traits>::DumpExceptionDescription(const ExceptionDescriptionType* c, std::ostream& os)
    {
        os<<"=========================================================="<<std::endl;
        os<<"Exception: "<<c->GetName()<<std::endl;
        os<<"TypeId: "<<c->GetTypeId()<<std::endl;
        if (c->GetBaseClass()!=NULL)
            os<<"baseClass: "<<c->GetBaseClass()->GetName()<<std::endl;
    }

    template <class RepT, class Traits>
    void ToStringHelper<RepT, Traits>::DumpPropertyDescription(const PropertyDescriptionType* c, std::ostream& os)
    {
        os<<"=========================================================="<<std::endl;
        os<<"Property: "<<c->GetName()<<std::endl;
        os<<"TypeId: "<<c->GetTypeId()<<std::endl;

        for (int i=0; i<c->GetNumberOfMembers(); ++i)
        {
            DumpMemberDescription(c->GetMember(i), os);
        }
    }

    template <class RepT, class Traits>
    void ToStringHelper<RepT, Traits>::DumpMemberDescription(const MemberDescriptionType* c, std::ostream& os)
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
            os<<BasicTypes::Instance().StringOf(c->GetMemberType())<<", maxLen="<<c->GetMaxLength();
            break;
        default:
            os<<BasicTypes::Instance().StringOf(c->GetMemberType());
            break;
        }

        if (c->IsArray())
        {
            os<<", isArray=true, arraySize="<<c->GetArraySize()<<std::endl;
        }
        else
        {
            os<<", isArray=false, arraySize="<<c->GetArraySize()<<std::endl;
            if (c->GetArraySize()<1)
                throw ParseError("Wrong ARrray size", c->GetName(), "", 0);
        }
    }

    template <class RepT, class Traits>
    void ToStringHelper<RepT, Traits>::DumpParameterDescription(const ParameterDescriptionType* c, std::ostream& os)
    {
        os<<"    Parameter: "<<c->GetName()<<", type=";

        switch (c->GetMemberType())
        {
        case EnumerationMemberType:
            os<<m_rep->GetEnum(c->GetTypeId())->GetName();
            break;
        case ObjectMemberType:
            os<<m_rep->GetClass(c->GetTypeId())->GetName();
            break;
        default:
            os<<BasicTypes::Instance().StringOf(c->GetMemberType());
            break;
        }

        if (c->IsArray())
        {
            os<<", isArray=true, arraySize="<<c->GetArraySize()<<std::endl;
        }
        else
        {
            os<<", isArray=false"<<std::endl;
        }

        //Values
        switch(c->GetMemberType())
        {
        case BooleanMemberType:
        {
            for (int i=0; i<c->GetArraySize(); ++i)
            {
                os<<"      value["<<i<<"]=";
                os<<std::boolalpha<<c->GetBoolValue(i)<<std::dec<<std::endl;
            }
        }
            break;

        case Int32MemberType:
        {
            for (int i=0; i<c->GetArraySize(); ++i)
            {
                os<<"      value["<<i<<"]="<<c->GetInt32Value(i)<<std::endl;
            }
        }
            break;
        case Int64MemberType:
        {
            for (int i=0; i<c->GetArraySize(); ++i)
            {
                os<<"      value["<<i<<"]="<<c->GetInt64Value(i)<<std::endl;
            }
        }
            break;

        case EntityIdMemberType:
        {
            for (int i=0; i<c->GetArraySize(); ++i)
            {
                os<<"      value["<<i<<"]=";
                TypeIdToString(c->GetInt64Value(i), os);
                os<<" : ";
                HashedValToString(c->GetHashedValue(i), os);
                os<<std::endl;
            }
        }
            break;
        case TypeIdMemberType:
        {
            for (int i=0; i<c->GetArraySize(); ++i)
            {
                os<<"      value["<<i<<"]=";
                TypeIdToString(c->GetInt64Value(i), os);
                os<<std::endl;
            }
        }
            break;
        case InstanceIdMemberType:
        case ChannelIdMemberType:
        case HandlerIdMemberType:
        {
            for (int i=0; i<c->GetArraySize(); ++i)
            {
                os<<"      value["<<i<<"]=";
                HashedValToString(c->GetHashedValue(i), os);
                os<<std::endl;
            }
        }
            break;

        case StringMemberType:
        {
            for (int i=0; i<c->GetArraySize(); ++i)
            {
                os<<"      value["<<i<<"]="<<c->GetStringValue(i)<<std::endl;
            }
        }
            break;

        case ObjectMemberType:
        {
            for (int i=0; i<c->GetArraySize(); ++i)
            {
                const char* objParam=c->GetObjectValue(i).first;
                std::ostringstream json;
                (BlobToJsonSerializer<RepositoryType>(m_rep))(objParam, json);
                os<<"      value["<<i<<"]="<<json.str()<<std::endl;
            }
        }
            break;

        case EnumerationMemberType:
        {
            for (int i=0; i<c->GetArraySize(); ++i)
            {
                os<<"      value["<<i<<"]="<<m_rep->GetEnum(c->GetTypeId())->GetValueName(c->GetInt32Value(i))<<std::endl;
            }
        }
            break;

        case BinaryMemberType:
        {
            for (int i=0; i<c->GetArraySize(); ++i)
            {
                //TODO: change to hex or base64 output, this assumes binary is an ascii string
                os<<"      value["<<i<<"]="<<c->GetStringValue(i)<<std::endl;
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
            for (int i=0; i<c->GetArraySize(); ++i)
            {
                os<<"      value["<<i<<"]="<<c->GetFloat32Value(i)<<std::endl;
            }
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
            for (int i=0; i<c->GetArraySize(); ++i)
            {
                os<<"      value["<<i<<"]="<<c->GetFloat64Value(i)<<std::endl;
            }
        }
            break;
        }
    }

    template <class RepT, class Traits>
    void ToStringHelper<RepT, Traits>::DumpMappingDescription(const PropertyMappingDescriptionType* pmd, std::ostream& os)
    {
        const PropertyDescriptionType* p=pmd->GetProperty();
        const ClassDescriptionType* c=pmd->GetClass();
        os<<"    Property: "<<p->GetName()<<std::endl;

        for (int i=0; i<p->GetNumberOfMembers(); ++i)
        {
            const MemberMappingDescriptionType* md=pmd->GetMemberMapping(i);

            os<<"  - PropertyMember:  "<<p->GetMember(i)->GetName()<<std::endl;
            switch(md->GetMappingKind())
            {
            case MappedToParameter:
            {
                std::pair<const ParameterDescriptionType*, int> par=md->GetParameter();
                os<<"    MappingKind:     ValueMapping"<<std::endl;
                os<<"    Parameter:       "<<par.first->GetName();
                if (par.second>=0)
                {
                    os<<"["<<par.second<<"]";
                }
                os<<std::endl;
                os<<"    Value:           <see parameter>"<<std::endl;
            }
                break;
            case MappedToMember:
            {
                os<<"    MappingKind:     MemberMapping"<<std::endl;
                os<<"    MemberRef:       ";
                for (int memRef=0; memRef<md->MemberReferenceDepth(); ++memRef)
                {
                    std::pair<DotsC_MemberIndex, DotsC_ArrayIndex> ref=md->GetMemberReference(memRef);
                    const MemberDescriptionType* member=c->GetMember(ref.first);
                    os<<"->"<<member->GetName();
                    if (member->IsArray())
                    {
                        os<<"["<<ref.second<<"]";
                    }
                }
                os<<std::endl;
            }
                break;

            case MappedToNull:
            {
                os<<"    MappingKind:     NullMapping"<<std::endl;
            }
                break;
            }
        }
    }

    template <class RepT, class Traits>
    void ToStringHelper<RepT, Traits>::DumpCreateRoutineDescription(const CreateRoutineDescriptionType* c, std::ostream& os)
    {
        os<<"  - CreateRoutine: "<<c->GetName()<<std::endl;
        os<<"    summary:    "<<c->Summary()<<std::endl;
        for (int i=0; i<c->GetNumberOfInParameters(); ++i)
        {
            os<<"      Parameter: "<<c->GetInParameterMember(i)->GetName()<<std::endl;
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
} //end namespace Safir::Dob::Typesystem::Internal::detail

#endif
