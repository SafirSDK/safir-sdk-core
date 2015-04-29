/******************************************************************************
*
* Copyright Consoden AB, 2004-2015 (http://safir.sourceforge.net)
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

                switch (md->GetCollectionType())
                {
                case SingleValueCollectionType:
                {
                    bool isNull=true, isChanged=true;
                    reader.ReadStatus(memberIx, 0, isNull, isChanged);
                    if (!isNull)
                    {
                        os<<", ";
                        WriteMemberName(md->GetName(), os);
                        SerializeMember(reader, md, memberIx, 0, os);
                    }
                }
                    break;
                case ArrayCollectionType:
                {
                    std::ostringstream arrayValues;
                    arrayValues<<", ";
                    WriteMemberName(md->GetName(), arrayValues);
                    arrayValues<<"[";
                    bool nonNullValueInserted=false;
                    int accumulatedNulls=0;
                    bool hasInsertedValues=false;
                    for (DotsC_Int32 arrIx=0; arrIx<md->GetArraySize(); ++arrIx)
                    {
                        bool isNull=true, isChanged=true;
                        reader.ReadStatus(memberIx, arrIx, isNull, isChanged);
                        if (isNull)
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
                    break;
                case SequenceCollectionType:
                {
                    int numberOfValues=reader.NumberOfValues(memberIx);
                    if (numberOfValues>0)
                    {
                        os<<", ";
                        WriteMemberName(md->GetName(), os);
                        os<<"[";

                        for (DotsC_Int32 valueIndex=0; valueIndex<numberOfValues; ++valueIndex)
                        {
                            if (valueIndex>0)
                            {
                                os<<", ";
                            }
                            SerializeMember(reader, md, memberIx, valueIndex, os);
                        }

                        os<<"]";
                    }
                }
                    break;
                case DictionaryCollectionType:
                {
                    int numberOfValues=reader.NumberOfValues(memberIx);
                    if (numberOfValues>0)
                    {
                        os<<", ";
                        WriteMemberName(md->GetName(), os);
                        os<<"[";

                        for (DotsC_Int32 valueIndex=0; valueIndex<numberOfValues; ++valueIndex)
                        {
                            if (valueIndex>0)
                            {
                                os<<", ";
                            }
                            os<<"{";
                            WriteMemberName("key", os);
                            SerializeKey(reader, md, memberIx, valueIndex, os);
                            os<<", ";
                            WriteMemberName("value", os);
                            if (!SerializeMember(reader, md, memberIx, valueIndex, os))
                                os<<"null";
                            os<<"}";
                        }

                        os<<"]";
                    }
                }
                    break;
                }
            }

            os<<"}";
        }

        void WriteString(const std::string& val, std::ostream& os) const
        {
            std::string str=val;
            std::string repl=std::string("\\")+std::string("\"");
            boost::replace_all(str, "\"", repl);
            os<<SAFIR_JSON_QUOTE(str);
        }

        void WriteHash(const std::pair<DotsC_Int64, const char*>& val, std::ostream& os) const
        {
            if (val.second)
            {
                os<<SAFIR_JSON_QUOTE(val.second);
            }
            else
            {
                os<<val.first;
            }
        }

        void WriteEntityId(const std::pair<DotsC_EntityId, const char*>& val, std::ostream& os) const
        {
            os<<"{";
            WriteMemberName("name", os);
            os<<SAFIR_JSON_QUOTE(TypeUtilities::GetTypeName(m_repository, val.first.typeId));

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
        }

        void SerializeKey(const BlobReader<RepositoryType>& reader,
                          const MemberDescriptionType* md,
                          DotsC_MemberIndex memberIndex,
                          DotsC_Int32 valueIndex,
                          std::ostream& os) const
        {
            switch(md->GetKeyType())
            {
            case Int32MemberType:
            {
                os<<reader.template ReadKey<DotsC_Int32>(memberIndex, valueIndex);
            }
                break;
            case Int64MemberType:
            {
                os<<reader.template ReadKey<DotsC_Int64>(memberIndex, valueIndex);
            }
                break;
            case EnumerationMemberType:
            {
                const char* enumVal=m_repository->GetEnum(md->GetKeyTypeId())->GetValueName(reader.template ReadKey<DotsC_EnumerationValue>(memberIndex, valueIndex));
                os<<SAFIR_JSON_QUOTE(enumVal);
            }
                break;
            case EntityIdMemberType:
            {
                std::pair<DotsC_EntityId, const char*> eid=reader.template ReadKey< std::pair<DotsC_EntityId, const char*> >(memberIndex, valueIndex);
                WriteEntityId(eid, os);
            }
                break;
            case TypeIdMemberType:
            {
                os<<SAFIR_JSON_QUOTE(TypeUtilities::GetTypeName(m_repository, reader.template ReadKey<DotsC_TypeId>(memberIndex, valueIndex)));
            }
                break;
            case InstanceIdMemberType:
            case ChannelIdMemberType:
            case HandlerIdMemberType:
            {
                std::pair<DotsC_Int64, const char*> hash=reader.template ReadKey< std::pair<DotsC_Int64, const char*> >(memberIndex, valueIndex);
                WriteHash(hash, os);
            }
                break;

            case StringMemberType:
            {
                WriteString(reader.template ReadKey<const char*>(memberIndex, valueIndex), os);
            }
                break;

            default:
                throw std::logic_error(std::string("Unexpected dictionary key type: ")+TypeUtilities::GetTypeName(md->GetKeyType()));
                break;
            }
        }

        bool SerializeMember(const BlobReader<RepositoryType>& reader,
                             const MemberDescriptionType* md,
                             DotsC_MemberIndex memberIndex,
                             DotsC_Int32 arrayIndex,
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
                DotsC_EnumerationValue val=0;
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
                    os<<SAFIR_JSON_QUOTE(TypeUtilities::GetTypeName(m_repository, val));
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
                    WriteHash(val, os);
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
                    WriteEntityId(val, os);
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
                    WriteString(val, os);
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
