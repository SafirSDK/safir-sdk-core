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
#include <Safir/Dob/Typesystem/ToolSupport/Internal/BlobLayoutImpl.h>
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
            ,m_blobLayout(repository)
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
        const BlobLayoutImpl<RepositoryType> m_blobLayout;

        inline void WriteMemberName(const char* name, std::ostream& os) const
        {
            os<<"\""<<name<<"\": ";
        }

        void SerializeMembers(const char* blob, std::ostream& os) const
        {
            static const std::string nullString="null";

            os<<"{";

            const ClassDescriptionType* cd=GetClass(blob);
            WriteMemberName("_DouType", os);
            os<<SAFIR_JSON_QUOTE(cd->GetName());

            for (DotsC_MemberIndex memberIx=0; memberIx<cd->GetNumberOfMembers(); ++memberIx)
            {
                const MemberDescriptionType* md=cd->GetMember(memberIx);

                if (!md->IsArray()) //normal member
                {
                    if (!m_blobLayout.GetStatus(blob, memberIx, 0).IsNull())
                    {
                        os<<", ";
                        WriteMemberName(md->GetName(), os);
                        SerializeMember(blob, md, memberIx, 0, os);
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
                        if (m_blobLayout.GetStatus(blob, memberIx, arrIx).IsNull())
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

                            SerializeMember(blob, md, memberIx, arrIx, arrayValues);
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

        bool SerializeMember(const char* blob,
                             const MemberDescriptionType* md,
                             DotsC_MemberIndex memberIndex,
                             DotsC_ArrayIndex arrayIndex,
                             std::ostream& os) const
        {
            switch(md->GetMemberType())
            {
            case BooleanMemberType:
            {
                bool val=true;
                MemberStatus status=m_blobLayout.template GetMember<bool>(blob, memberIndex, arrayIndex, val);
                if (!status.IsNull())
                {
                    os<<(val ? "true" : "false");
                    return true;
                }
            }
                break;

            case EnumerationMemberType:
            {
                DotsC_Int32 val=0;
                MemberStatus status=m_blobLayout.template GetMember<DotsC_Int32>(blob, memberIndex, arrayIndex, val);
                if (!status.IsNull())
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
                MemberStatus status=m_blobLayout.template GetMember<DotsC_Int32>(blob, memberIndex, arrayIndex, val);
                if (!status.IsNull())
                {
                    os<<val;
                    return true;
                }
            }
                break;

            case Int64MemberType:
            {
                DotsC_Int64 val=0;
                MemberStatus status=m_blobLayout.template GetMember<DotsC_Int64>(blob, memberIndex, arrayIndex, val);
                if (!status.IsNull())
                {
                    os<<val;
                    return true;
                }
            }
                break;

            case TypeIdMemberType:
            {
                DotsC_Int64 val=0;
                MemberStatus status=m_blobLayout.template GetMember<DotsC_Int64>(blob, memberIndex, arrayIndex, val);
                if (!status.IsNull())
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
                DotsC_Int64 val=0;
                const char* hashStr=NULL;
                MemberStatus status=m_blobLayout.GetMemberWithOptionalString(blob, memberIndex, arrayIndex, val, hashStr);
                if (!status.IsNull())
                {
                    if (hashStr)
                    {
                        os<<SAFIR_JSON_QUOTE(hashStr);
                    }
                    else
                    {
                        os<<val;
                    }
                    return true;
                }
            }
                break;

            case EntityIdMemberType:
            {
                DotsC_EntityId entId;
                const char* hashStr=0;
                MemberStatus status=m_blobLayout.GetMemberWithOptionalString(blob, memberIndex, arrayIndex, entId, hashStr);
                if (!status.IsNull())
                {
                    os<<"{";
                    WriteMemberName("name", os);
                    const char* typeName=TypeIdToString(entId.typeId);
                    if (typeName)
                    {
                        os<<SAFIR_JSON_QUOTE(typeName);
                    }
                    else
                    {
                        os<<entId.typeId;
                    }

                    os<<", ";
                    WriteMemberName("instanceId", os);
                    if (hashStr)
                    {
                        os<<SAFIR_JSON_QUOTE(hashStr);
                    }
                    else
                    {
                        os<<entId.instanceId;
                    }
                    os<<"}";
                    return true;
                }
            }
                break;

            case StringMemberType:
            {
                const char* strVal=NULL;
                DotsC_Int32 size=0;
                MemberStatus status=m_blobLayout.GetDynamicMember(blob, memberIndex, arrayIndex, strVal, size);
                if (!status.IsNull())
                {
                    std::string str=strVal;
                    std::string repl=std::string("\\")+std::string("\"");
                    boost::replace_all(str, "\"", repl);
                    os<<SAFIR_JSON_QUOTE(str);
                    return true;
                }
            }
                break;

            case ObjectMemberType:
            {
                const char* obj=NULL;
                DotsC_Int32 size=0;
                MemberStatus status=m_blobLayout.GetDynamicMember(blob, memberIndex, arrayIndex, obj, size);
                if (!status.IsNull())
                {
                    SerializeMembers(obj, os);
                    return true;
                }
            }
                break;

            case BinaryMemberType:
            {
                const char* binary=NULL;
                DotsC_Int32 size=0;
                MemberStatus status=m_blobLayout.GetDynamicMember(blob, memberIndex, arrayIndex, binary, size);
                if (!status.IsNull())
                {
                    std::vector<char> bin(binary, binary + size);
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
                MemberStatus status=m_blobLayout.template GetMember<DotsC_Float32>(blob, memberIndex, arrayIndex, val);
                if (!status.IsNull())
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
                MemberStatus status=m_blobLayout.template GetMember<DotsC_Float64>(blob, memberIndex, arrayIndex, val);
                if (!status.IsNull())
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
            DotsC_TypeId typeId=m_blobLayout.GetTypeId(blob);
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
} //end namespace Safir::Dob::Typesystem::Internal::Internal

#ifdef _MSC_VER
#pragma warning( pop )
#endif

#endif
