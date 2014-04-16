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
#ifndef __DOTS_INTERNAL_BLOB_TO_JSON_H__
#define __DOTS_INTERNAL_BLOB_TO_JSON_H__

#ifdef _MSC_VER
#pragma warning( push )
#pragma warning( disable : 4100 )
#endif

#include <string>
#include <vector>
#include <sstream>
#include <stdexcept>
#include <boost/noncopyable.hpp>
#include <boost/algorithm/string/replace.hpp>
#include <Safir/Dob/Typesystem/ToolSupport/TypeRepository.h>
#include <Safir/Dob/Typesystem/ToolSupport/BlobReader.h>
#include <Safir/Dob/Typesystem/ToolSupport/Internal/SerializationUtils.h>

#define SAFIR_JSON_QUOTE(x) "\""<<x<<"\""

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
    template <class RepT, class Traits=Safir::Dob::Typesystem::ToolSupport::TypeRepositoryTraits<RepT> >
    class BlobToJsonSerializer : private boost::noncopyable
    {
    public:
        typedef typename Traits::RepositoryType RepositoryType;
        typedef typename Traits::ClassDescriptionType ClassDescriptionType;
        typedef typename Traits::PropertyDescriptionType PropertyDescriptionType;
        typedef typename Traits::MemberDescriptionType MemberDescriptionType;
        typedef typename Traits::EnumDescriptionType EnumDescriptionType;

        BlobToJsonSerializer(const RepositoryType* repository)
            :m_repository(repository)
        {
        }

        void operator()(const char* blob, std::ostream& os) const
        {
            const ClassDescriptionType* cd=GetClass(blob);
            os<<"{";
            WriteMemberName(cd->GetName(), os);
            SerializeMembers(blob, os);
            os<<"}";
        }

    private:
        const RepositoryType* m_repository;

        inline void WriteMemberName(const char* name, std::ostream& os) const
        {
            os<<"\""<<name<<"\": ";
        }

        void SerializeMembers(const char* blob, std::ostream& os) const
        {
            static const std::string nullString="null";

            os<<"{";

            const ClassDescriptionType* cd=GetClass(blob);
            BlobReader<RepositoryType> reader(m_repository, blob);

            WriteMemberName("_DouType", os);
            os<<SAFIR_JSON_QUOTE(cd->GetName());

            for (DotsC_MemberIndex memberIx=0; memberIx<cd->GetNumberOfMembers(); ++memberIx)
            {
                const MemberDescriptionType* md=cd->GetMember(memberIx);

                if (md->GetCollectionType()!=ArrayCollectionType) //normal member
                {
                    if (!reader.IsNull(memberIx, 0))
                    {
                        os<<", ";
                        WriteMemberName(md->GetName(), os);
                        SerializeMember(reader, md, memberIx, 0, os);
                    }
                }
                else //array member
                {
                    std::ostringstream arrayValues;
                    arrayValues<<", ";
                    WriteMemberName(md->GetName(), arrayValues);
                    arrayValues<<"[";
                    bool nonNullValueInserted=false;
                    int accumulatedNulls=0;
                    bool hasInsertedValues=false;
                    for (DotsC_ArrayIndex arrIx=0; arrIx<md->GetArraySize(); ++arrIx)
                    {
                        if (reader.IsNull(memberIx, arrIx))
                        {
                            //we wait to insert null until we know we have to because an value exists after.
                            //This way we avoid lots of null at the end of an array
                            ++accumulatedNulls;
                        }
                        else
                        {
                            if (hasInsertedValues)
                            {
                                arrayValues<<", ";
                            }

                            for (int nullCount=0; nullCount<accumulatedNulls; ++nullCount)
                            {
                                arrayValues<<"null, ";
                            }
                            accumulatedNulls=0;
                            nonNullValueInserted=true;

                            SerializeMember(reader, md, memberIx, arrIx, arrayValues);
                            hasInsertedValues=true;
                        }
                    }
                    arrayValues<<"]";

                    if (nonNullValueInserted) //only add array element if there are non-null values
                    {
                        os<<arrayValues.str();
                    }
                }
            }

            os<<"}";
        }

        bool SerializeMember(const BlobReader<RepositoryType>& reader,
                             const MemberDescriptionType* md,
                             DotsC_MemberIndex memberIndex,
                             DotsC_ArrayIndex arrayIndex,
                             std::ostream& os) const
        {
            bool isNull=true;
            bool isChanged=false;

            switch(md->GetMemberType())
            {
            case BooleanMemberType:
            {
                bool val=true;
                reader.ReadValue(memberIndex, arrayIndex, val, isNull, isChanged);
                if (!isNull)
                {
                    os<<(val ? "true" : "false");
                    return true;
                }
            }
                break;

            case EnumerationMemberType:
            {
                DotsC_Int32 val=0;
                reader.ReadValue(memberIndex, arrayIndex, val, isNull, isChanged);
                if (!isNull)
                {
                    const char* enumVal=m_repository->GetEnum(md->GetTypeId())->GetValueName(val);
                    os<<SAFIR_JSON_QUOTE(enumVal);
                    return true;
                }
            }
                break;

            case Int32MemberType:
            {
                DotsC_Int32 val=0;
                reader.ReadValue(memberIndex, arrayIndex, val, isNull, isChanged);
                if (!isNull)
                {
                    os<<val;
                    return true;
                }
            }
                break;

            case Int64MemberType:
            {
                DotsC_Int64 val=0;
                reader.ReadValue(memberIndex, arrayIndex, val, isNull, isChanged);
                if (!isNull)
                {
                    os<<val;
                    return true;
                }
            }
                break;

            case TypeIdMemberType:
            {
                DotsC_Int64 val=0;
                reader.ReadValue(memberIndex, arrayIndex, val, isNull, isChanged);
                if (!isNull)
                {
                    const char* typeName=TypeIdToString(val);
                    if (typeName)
                    {
                        os<<SAFIR_JSON_QUOTE(typeName);
                    }
                    else
                    {
                        os<<val;
                    }
                    return true;
                }
            }
                break;

            case InstanceIdMemberType:
            case ChannelIdMemberType:
            case HandlerIdMemberType:
            {
                std::pair<DotsC_Int64, const char*> val;
                reader.ReadValue(memberIndex, arrayIndex, val, isNull, isChanged);
                if (!isNull)
                {
                    if (val.second)
                    {
                        os<<SAFIR_JSON_QUOTE(val.second);
                    }
                    else
                    {
                        os<<val.first;
                    }
                    return true;
                }
            }
                break;

            case EntityIdMemberType:
            {
                std::pair<DotsC_EntityId, const char*> val;
                reader.ReadValue(memberIndex, arrayIndex, val, isNull, isChanged);
                if (!isNull)
                {
                    os<<"{";
                    WriteMemberName("name", os);
                    const char* typeName=TypeIdToString(val.first.typeId);
                    if (typeName)
                    {
                        os<<SAFIR_JSON_QUOTE(typeName);
                    }
                    else
                    {
                        os<<val.first.typeId;
                    }

                    os<<", ";
                    WriteMemberName("instanceId", os);
                    if (val.second)
                    {
                        os<<SAFIR_JSON_QUOTE(val.second);
                    }
                    else
                    {
                        os<<val.first.instanceId;
                    }
                    os<<"}";
                    return true;
                }
            }
                break;

            case StringMemberType:
            {
                const char* val=NULL;
                reader.ReadValue(memberIndex, arrayIndex, val, isNull, isChanged);
                if (!isNull)
                {
                    std::string str=val;
                    std::string repl=std::string("\\")+std::string("\"");
                    boost::replace_all(str, "\"", repl);
                    os<<SAFIR_JSON_QUOTE(str);
                    return true;
                }
            }
                break;

            case ObjectMemberType:
            {
                std::pair<const char*, DotsC_Int32> val; //blob and size
                reader.ReadValue(memberIndex, arrayIndex, val, isNull, isChanged);
                if (!isNull)
                {
                    SerializeMembers(val.first, os);
                    return true;
                }
            }
                break;

            case BinaryMemberType:
            {
                std::pair<const char*, DotsC_Int32> val; //blob and size
                reader.ReadValue(memberIndex, arrayIndex, val, isNull, isChanged);
                if (!isNull)
                {
                    std::string bin(val.first, static_cast<size_t>(val.second));
                    os<<SAFIR_JSON_QUOTE(SerializationUtils::ToBase64(bin));
                    return true;
                }
            }
                break;

                //  32 bit floats
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
                DotsC_Float32 val=0;
                reader.ReadValue(memberIndex, arrayIndex, val, isNull, isChanged);
                if (!isNull)
                {
                    os<<classic_string_cast<std::string>(val);
                    return true;
                }
            }
                break;

                //  64 bit floats
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
                DotsC_Float64 val=0;
                reader.ReadValue(memberIndex, arrayIndex, val, isNull, isChanged);
                if (!isNull)
                {
                    os<<classic_string_cast<std::string>(val);
                    return true;
                }
            }
                break;
            }

            return false;
        }

        const char* TypeIdToString(DotsC_TypeId tid) const
        {
            const ClassDescriptionType* cd=m_repository->GetClass(tid);
            if (cd)
            {
                return cd->GetName();
            }

            const EnumDescriptionType* ed=m_repository->GetEnum(tid);
            if (ed)
            {
                return ed->GetName();
            }

            const PropertyDescriptionType* pd=m_repository->GetProperty(tid);
            if (pd)
            {
                return pd->GetName();
            }

            return NULL;
        }

        const ClassDescriptionType* GetClass(const char* blob) const
        {
            DotsC_TypeId typeId=BlobReader<RepositoryType>::GetTypeId(blob);
            const ClassDescriptionType* cd=m_repository->GetClass(typeId);
            if (cd==NULL)
            {
                std::ostringstream os;
                os<<"Corrupt blob. Can't find type descriptor for blob with typeId="<<typeId;
                throw ParseError("Binary to JSON error", os.str(), "", 166);
            }
            return cd;
        }
    };

}
}
}
}
} //end namespace Safir::Dob::Typesystem::ToolSupport::Internal

#ifdef _MSC_VER
#pragma warning( pop )
#endif

#endif
