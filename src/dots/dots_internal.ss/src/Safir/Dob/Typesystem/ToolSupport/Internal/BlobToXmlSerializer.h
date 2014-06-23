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
#ifndef __DOTS_INTERNAL_BLOB_TO_XML_H__
#define __DOTS_INTERNAL_BLOB_TO_XML_H__

#include <string>
#include <vector>
#include <sstream>
#include <stdexcept>
#include <boost/noncopyable.hpp>
#include <Safir/Dob/Typesystem/ToolSupport/TypeRepository.h>
#include <Safir/Dob/Typesystem/ToolSupport/BlobReader.h>
#include <Safir/Dob/Typesystem/ToolSupport/Internal/SerializationUtils.h>

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
    class BlobToXmlSerializer : private boost::noncopyable
    {
    public:
        typedef typename Traits::RepositoryType RepositoryType;
        typedef typename Traits::ClassDescriptionType ClassDescriptionType;
        typedef typename Traits::PropertyDescriptionType PropertyDescriptionType;
        typedef typename Traits::MemberDescriptionType MemberDescriptionType;
        typedef typename Traits::EnumDescriptionType EnumDescriptionType;

        BlobToXmlSerializer(const RepositoryType* repository)
            :m_repository(repository)
        {
        }

        void operator()(const char* blob, std::ostream& os) const
        {
            os<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>";
            const ClassDescriptionType* cd=GetClass(blob);
            os<<"<"<<cd->GetName()<<">";
            SerializeMembers(blob, os);
            os<<"</"<<cd->GetName()<<">";
        }

    private:
        const RepositoryType* m_repository;

        void SerializeMembers(const char* blob, std::ostream& os) const
        {
            const ClassDescriptionType* cd=GetClass(blob);
            BlobReader<RepositoryType> reader(m_repository, blob);

            for (DotsC_MemberIndex memberIx=0; memberIx<cd->GetNumberOfMembers(); ++memberIx)
            {
                const MemberDescriptionType* md=cd->GetMember(memberIx);
                switch (md->GetCollectionType())
                {
                case SingleValueCollectionType:
                {
                    SerializeMember(reader, md, memberIx, 0, md->GetName(), os);
                }
                    break;

                case ArrayCollectionType:
                {
                    const char* typeName=Safir::Dob::Typesystem::ToolSupport::TypeUtilities::GetTypeName(m_repository, md);

                    //find first non-null value in array
                    DotsC_ArrayIndex arrIx=0;
                    while (arrIx<md->GetArraySize())
                    {
                        if (!reader.IsNull(memberIx, arrIx))
                        {
                            break;
                        }
                        ++arrIx;
                    }

                    if (arrIx<md->GetArraySize()) //only add array if it contains non-null values
                    {
                        os<<"<"<<md->GetName()<<">";
                        for (DotsC_ArrayIndex arrIx=0; arrIx<md->GetArraySize(); ++arrIx)
                        {
                            SerializeMember(reader, md, memberIx, arrIx, typeName, os);
                        }
                        os<<"</"<<md->GetName()<<">";
                    }
                }
                    break;

                case SequenceCollectionType:
                {
                    const char* typeName=Safir::Dob::Typesystem::ToolSupport::TypeUtilities::GetTypeName(m_repository, md);
                    int numberOfValues=reader.NumberOfValues(memberIx);
                    if (numberOfValues>0)
                    {
                        os<<"<"<<md->GetName()<<">";
                        for (DotsC_ArrayIndex valueIndex=0; valueIndex<numberOfValues; ++valueIndex)
                        {
                            SerializeMember(reader, md, memberIx, valueIndex, typeName, os);
                        }
                        os<<"</"<<md->GetName()<<">";
                    }
                }
                    break;

                case DictionaryCollectionType:
                {
                    const char* typeName=Safir::Dob::Typesystem::ToolSupport::TypeUtilities::GetTypeName(m_repository, md);
                    int numberOfValues=reader.NumberOfValues(memberIx);
                    if (numberOfValues>0)
                    {
                        os<<"<"<<md->GetName()<<">";
                        for (DotsC_ArrayIndex valueIndex=0; valueIndex<numberOfValues; ++valueIndex)
                        {
                            os<<"<entry>";
                            SerializeKey(reader, md, memberIx, valueIndex, os);
                            SerializeMember(reader, md, memberIx, valueIndex, typeName, os);
                            os<<"</entry>";
                        }
                        os<<"</"<<md->GetName()<<">";
                    }

                }
                    break;
                }
            }
        }

        void WriteStartElement(const char* elementName,
                               DotsC_ArrayIndex arrayIndex,
                               bool isArray,
                               std::ostream& os) const
        {
            if (!isArray)
            {
                os<<"<"<<elementName<<">";
            }
            else
            {
                os<<"<"<<elementName<<" index=\""<<arrayIndex<<"\">";
            }
        }

        void WriteString(const char* str, std::ostream& os) const
        {
            size_t i=0;
            while (str[i]!='\0')
            {
                switch (str[i])
                {
                case '<':
                    os<<"&lt;";
                    break;

                case '>':
                    os<<"&gt;";
                    break;

                case '&':
                    os<<"&amp;";
                    break;

                case '"':
                    os<<"&quot;";
                    break;

                default:
                    os<<str[i];
                    break;
                }
                ++i;
            }
        }

        void SerializeKey(const BlobReader<RepositoryType>& reader,
                             const MemberDescriptionType* md,
                             DotsC_MemberIndex memberIndex,
                             DotsC_ArrayIndex valueIndex,
                             std::ostream& os) const
        {
            os<<"<key>";

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
            case EntityIdMemberType:
            {
                std::pair<DotsC_EntityId, const char*> eid=reader.template ReadKey< std::pair<DotsC_EntityId, const char*> >(memberIndex, valueIndex);
                os<<"<name>"<<TypeUtilities::GetTypeName(m_repository, eid.first.typeId)<<"</name>";
                if (eid.second)
                    os<<"<instanceId>"<<eid.second<<"</instanceId>";
                else
                    os<<"<instanceId>"<<eid.first.instanceId<<"</instanceId>";
            }
                break;
            case TypeIdMemberType:
            {
                os<<TypeUtilities::GetTypeName(m_repository, reader.template ReadKey<DotsC_TypeId>(memberIndex, valueIndex));
            }
                break;
            case InstanceIdMemberType:
            case ChannelIdMemberType:
            case HandlerIdMemberType:
            {
                std::pair<DotsC_Int64, const char*> hash=reader.template ReadKey< std::pair<DotsC_Int64, const char*> >(memberIndex, valueIndex);
                if (hash.second)
                    os<<hash.second;
                else
                    os<<hash.first;
            }
                break;

            case StringMemberType:
            {
                os<<reader.template ReadKey<const char*>(memberIndex, valueIndex);
            }
                break;

            default:
                throw std::logic_error(std::string("Unexpected dictionary key type: ")+TypeUtilities::GetTypeName(md->GetKeyType()));
                break;
            }

            os<<"</key>";
        }

        void SerializeMember(const BlobReader<RepositoryType>& reader,
                             const MemberDescriptionType* md,
                             DotsC_MemberIndex memberIndex,
                             DotsC_ArrayIndex arrayIndex,
                             const char* elementName,
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
                    WriteStartElement(elementName, arrayIndex, md->GetCollectionType()==ArrayCollectionType, os);
                    os<<(val ? "true" : "false");
                    os<<"</"<<elementName<<">";
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
                    WriteStartElement(elementName, arrayIndex, md->GetCollectionType()==ArrayCollectionType, os);
                    os<<enumVal;
                    os<<"</"<<elementName<<">";
                }
            }
                break;

            case Int32MemberType:
            {
                DotsC_Int32 val=0;
                reader.ReadValue(memberIndex, arrayIndex, val, isNull, isChanged);
                if (!isNull)
                {
                    WriteStartElement(elementName, arrayIndex, md->GetCollectionType()==ArrayCollectionType, os);
                    os<<val;
                    os<<"</"<<elementName<<">";
                }
            }
                break;

            case Int64MemberType:
            {
                DotsC_Int64 val=0;
                reader.ReadValue(memberIndex, arrayIndex, val, isNull, isChanged);
                if (!isNull)
                {
                    WriteStartElement(elementName, arrayIndex, md->GetCollectionType()==ArrayCollectionType, os);
                    os<<val;
                    os<<"</"<<elementName<<">";
                }
            }
                break;

            case TypeIdMemberType:
            {
                DotsC_Int64 val=0;
                reader.ReadValue(memberIndex, arrayIndex, val, isNull, isChanged);
                if (!isNull)
                {
                    WriteStartElement(elementName, arrayIndex, md->GetCollectionType()==ArrayCollectionType, os);

                    const char* typeName=TypeUtilities::GetTypeName(m_repository, val);
                    if (typeName)
                    {
                        os<<typeName;
                    }
                    else
                    {
                        os<<val;
                    }

                    os<<"</"<<elementName<<">";
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
                    WriteStartElement(elementName, arrayIndex, md->GetCollectionType()==ArrayCollectionType, os);
                    if (val.second)
                    {
                        os<<val.second;
                    }
                    else
                    {
                        os<<val.first;
                    }
                    os<<"</"<<elementName<<">";
                }
            }
                break;

            case EntityIdMemberType:
            {
                std::pair<DotsC_EntityId, const char*> val;
                reader.ReadValue(memberIndex, arrayIndex, val, isNull, isChanged);
                if (!isNull)
                {
                    WriteStartElement(elementName, arrayIndex, md->GetCollectionType()==ArrayCollectionType, os);

                    const char* typeName=TypeUtilities::GetTypeName(m_repository, val.first.typeId);
                    if (typeName)
                    {
                        os<<"<name>"<<typeName<<"</name>";
                    }
                    else
                    {
                        os<<"<name>"<<val.first.typeId<<"</name>";
                    }

                    if (val.second)
                    {
                        os<<"<instanceId>"<<val.second<<"</instanceId>";
                    }
                    else
                    {
                        os<<"<instanceId>"<<val.first.instanceId<<"</instanceId>";
                    }

                    os<<"</"<<elementName<<">";
                }
            }
                break;

            case StringMemberType:
            {
                const char* val=NULL;
                reader.ReadValue(memberIndex, arrayIndex, val, isNull, isChanged);
                if (!isNull)
                {
                    if (md->GetCollectionType()!=ArrayCollectionType)
                    {
                        os<<"<"<<elementName<<" xml:space=\"preserve\">";
                    }
                    else
                    {
                        os<<"<"<<elementName<<" index=\""<<arrayIndex<<"\" xml:space=\"preserve\">";
                    }
                    WriteString(val, os);
                    os<<"</"<<elementName<<">";
                }
            }
                break;

            case ObjectMemberType:
            {
                std::pair<const char*, DotsC_Int32> val; //blob and size
                reader.ReadValue(memberIndex, arrayIndex, val, isNull, isChanged);
                if (!isNull)
                {
                    bool typesDiffer=BlobReader<RepositoryType>::GetTypeId(val.first)!=md->GetTypeId();

                    if (typesDiffer) //we only need to add typeAttribute if types are not same as declared in dou (differ by inheritance)
                    {
                        os<<"<"<<elementName<<" type=\""<<GetClass(val.first)->GetName()<<"\"";
                    }
                    else
                    {
                        os<<"<"<<elementName;
                    }

                    if (md->GetCollectionType()!=ArrayCollectionType)
                    {
                        os<<">";
                    }
                    else
                    {
                        os<<" index=\""<<arrayIndex<<"\">";
                    }

                    SerializeMembers(val.first, os);
                    os<<"</"<<elementName<<">";
                }
            }
                break;

            case BinaryMemberType:
            {
                std::pair<const char*, DotsC_Int32> val; //blob and size
                reader.ReadValue(memberIndex, arrayIndex, val, isNull, isChanged);
                if (!isNull)
                {
                    WriteStartElement(elementName, arrayIndex, md->GetCollectionType()==ArrayCollectionType, os);
                    std::string bin(val.first, static_cast<size_t>(val.second));
                    os<<SerializationUtils::ToBase64(bin);
                    os<<"</"<<elementName<<">";
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
                    WriteStartElement(elementName, arrayIndex, md->GetCollectionType()==ArrayCollectionType, os);
                    os<<classic_string_cast<std::string>(val);
                    os<<"</"<<elementName<<">";
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
                    WriteStartElement(elementName, arrayIndex, md->GetCollectionType()==ArrayCollectionType, os);
                    os<<classic_string_cast<std::string>(val);
                    os<<"</"<<elementName<<">";
                }
            }
                break;
            }
        }

        const ClassDescriptionType* GetClass(const char* blob) const
        {
            DotsC_TypeId typeId=BlobReader<RepositoryType>::GetTypeId(blob);
            const ClassDescriptionType* cd=m_repository->GetClass(typeId);
            if (cd==NULL)
            {
                std::ostringstream os;
                os<<"Corrupt blob. Can't find type descriptor for blob with typeId="<<typeId;
                throw ParseError("Binary to XML error", os.str(), "", 167);
            }
            return cd;
        }
    };

}
}
}
}
} //end namespace Safir::Dob::Typesystem::ToolSupport::Internal

#endif
