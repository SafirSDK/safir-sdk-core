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
#ifndef __DOTS_INTERNAL_JSON_TO_BLOB_H__
#define __DOTS_INTERNAL_JSON_TO_BLOB_H__

#ifdef _MSC_VER
#pragma warning( push )
#pragma warning( disable : 4100 )
#endif

#include <string>
#include <vector>
#include <sstream>
#include <boost/noncopyable.hpp>
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>
#include <boost/iostreams/stream.hpp>
#include <boost/iostreams/device/array.hpp>
#include <Safir/Utilities/Internal/Id.h>
#include <Safir/Dob/Typesystem/ToolSupport/TypeRepository.h>
#include <Safir/Dob/Typesystem/ToolSupport/BlobWriter.h>
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
    class JsonToBlobSerializer : private boost::noncopyable
    {
    public:
        typedef typename Traits::RepositoryType RepositoryType;
        typedef typename Traits::ClassDescriptionType ClassDescriptionType;
        typedef typename Traits::MemberDescriptionType MemberDescriptionType;
        typedef typename Traits::EnumDescriptionType EnumDescriptionType;

        JsonToBlobSerializer(const RepositoryType* repository)
            :m_repository(repository)
        {
        }

        void operator()(const char* json, std::vector<char>& blob) const
        {
            boost::property_tree::ptree pt;
            boost::iostreams::array_source src(json, strlen(json));
            boost::iostreams::stream<boost::iostreams::array_source> stream(src);
            boost::property_tree::json_parser::read_json(stream, pt);
            this->operator ()(pt, blob);
        }

        void operator()(const boost::property_tree::ptree& json, std::vector<char>& blob) const
        {
            const boost::property_tree::ptree& members=json.front().second;
            boost::optional<std::string> typeName=members.get_optional<std::string>("_DouType");

            if (!typeName)
            {
                throw ParseError("JsonToBinary serialization error", "Json object does not have the _DouType field", "", 143);
            }

            SerializeObjectContent(*typeName, blob, members);
        }

        void SerializeObjectContent(const std::string& typeName, std::vector<char>& blob, const boost::property_tree::ptree& members) const
        {
            const DotsC_TypeId typeId=LlufId_Generate64(typeName.c_str());
            const ClassDescriptionType* cd=m_repository->GetClass(typeId);
            if (!cd)
            {
                throw ParseError("JsonToBinary serialization error", "Json does not contain a known type. Typename: "+typeName, "", 144);
            }

            BlobWriter<RepositoryType> writer(m_repository, typeId);

            for (boost::property_tree::ptree::const_iterator memIt=members.begin(); memIt!=members.end(); ++memIt)
            {
                const std::string& elementName=memIt->first;
                int memIx=cd->GetMemberIndex(elementName);
                if (memIx<0)
                {
                    if (elementName=="_DouType")
                    {
                        continue;
                    }

                    std::ostringstream os;
                    os<<"Failed to serialize Json to binary. The class '"<<cd->GetName()<<"' does not contain a member named '"<<elementName<<"'";
                    throw ParseError("JsonToBinary serialization error", os.str(), "", 145);
                }

                const MemberDescriptionType* md=cd->GetMember(memIx);

                switch (md->GetCollectionType())
                {
                case SingleValueCollectionType:
                    {
                        //non-array, then the inner propertyTree contains the content, i.e <myInt>123</myInt>
                        try
                        {
                            SetMember(md, memIx, 0, memIt->second, 0, writer);
                        }
                        catch (const boost::property_tree::ptree_error&)
                        {
                            std::ostringstream os;
                            os<<"Failed to serialize member '"<<cd->GetName()<<"."<<md->GetName()<<"' from Json to binary. Type is incorrect.";
                            throw ParseError("JsonToBinary serialization error", os.str(), "", 146);
                        }
                    }
                    break;

                case ArrayCollectionType:
                    {
                        DotsC_Int32 arrayIndex=0;
                        for (boost::property_tree::ptree::const_iterator arrIt=memIt->second.begin(); arrIt!=memIt->second.end(); ++arrIt)
                        {
                            if (md->GetArraySize()<=arrayIndex)
                            {
                                std::ostringstream os;
                                os<<"Failed to serialize array member '"<<cd->GetName()<<"."<<md->GetName()<<"' with index="<<arrayIndex<<" from json to binary. Index out of range. ArraySize is "<<md->GetArraySize();
                                throw ParseError("JsonToBinary serialization error", os.str(), "", 147);
                            }

                            try
                            {
                                SetMember(md, memIx, arrayIndex++, arrIt->second, 0, writer);
                            }
                            catch (const boost::property_tree::ptree_error&)
                            {
                                std::ostringstream os;
                                os<<"Failed to serialize array member '"<<cd->GetName()<<"."<<md->GetName()<<"' with index="<<arrayIndex<<" from Json to binary. Type is incorrect.";
                                throw ParseError("JsonToBinary serialization error", os.str(), "", 148);
                            }
                        }
                    }
                    break;

                case SequenceCollectionType:
                    {
                        DotsC_Int32 valueIndex=0;
                        for (boost::property_tree::ptree::const_iterator seqIt=memIt->second.begin(); seqIt!=memIt->second.end(); ++seqIt)
                        {
                            try
                            {
                                SetMember(md, memIx, 0, seqIt->second, 0, writer);
                            }
                            catch (const boost::property_tree::ptree_error&)
                            {
                                std::ostringstream os;
                                os<<"Failed to serialize sequence member '"<<cd->GetName()<<"."<<md->GetName()<<"' with index="<<valueIndex<<" from Json to binary. Type is incorrect.";
                                throw ParseError("JsonToBinary serialization error", os.str(), "", 195);
                            }
                            ++valueIndex;
                        }
                        writer.SetChangedTopLevel(memIx,true);
                    }
                    break;

                case DictionaryCollectionType:
                    {
                        DotsC_Int32 valueIndex=0;
                        for (boost::property_tree::ptree::const_iterator entryIt=memIt->second.begin(); entryIt!=memIt->second.end(); ++entryIt)
                        {
                            try
                            {
                                const boost::property_tree::ptree& keyTree=entryIt->second.get_child("key");
                                const boost::property_tree::ptree& valTree=entryIt->second.get_child("value");
                                SetMember(md, memIx, 0, valTree, keyTree, writer);
                            }
                            catch (const boost::property_tree::ptree_error&)
                            {
                                std::ostringstream os;
                                os<<"Failed to serialize dictionary member '"<<cd->GetName()<<"."<<md->GetName()<<"' with index="<<valueIndex<<" from Json to binary. Type is incorrect.";
                                throw ParseError("JsonToBinary serialization error", os.str(), "", 209);
                            }
                            ++valueIndex;
                        }
                        writer.SetChangedTopLevel(memIx,true);
                    }
                    break;
                }
            }

            DotsC_Int32 blobSize=writer.CalculateBlobSize();
            blob.resize(static_cast<size_t>(blobSize));
            writer.CopyRawBlob(&blob[0]);
        }

    private:
        const RepositoryType* m_repository;

        void SetMember(const MemberDescriptionType* md,
                       DotsC_MemberIndex memIx,
                       DotsC_Int32 arrIx,
                       const boost::property_tree::ptree& memberContent,
                       const boost::property_tree::ptree& keyContent,
                       BlobWriter<RepositoryType>& writer) const
        {
            switch(md->GetKeyType())
            {
            case Int32MemberType:
                {
                    SetMember(md, memIx, arrIx, memberContent, boost::lexical_cast<DotsC_Int32>(keyContent.data()), writer);
                }
                break;

            case Int64MemberType:
                {
                    SetMember(md, memIx, arrIx, memberContent, boost::lexical_cast<DotsC_Int64>(keyContent.data()), writer);
                }
                break;

            case TypeIdMemberType:
                {
                    DotsC_TypeId tid=SerializationUtils::StringToTypeId(keyContent.data());
                    SetMember(md, memIx, arrIx, memberContent, tid, writer);
                }
                break;

            case StringMemberType:
                {
                    SetMember(md, memIx, arrIx, memberContent, keyContent.data().c_str(), writer);
                }
                break;

            case EntityIdMemberType:
                {
                    const std::string inst = keyContent.get<std::string>("instanceId");
                    std::pair<DotsC_EntityId, const char*> eid=SerializationUtils::StringToEntityId(keyContent.get<std::string>("name"), inst);
                    SetMember(md, memIx, arrIx, memberContent, eid, writer);
                }
                break;

            case InstanceIdMemberType:
            case HandlerIdMemberType:
            case ChannelIdMemberType:
                {
                    std::pair<DotsC_Int64, const char*> hash=SerializationUtils::StringToHash(keyContent.data());
                    SetMember(md, memIx, arrIx, memberContent, hash, writer);
                }
                break;

            case EnumerationMemberType:
                {
                    const EnumDescriptionType* ed=m_repository->GetEnum(md->GetKeyTypeId());
                    DotsC_EnumerationValue enumVal=TypeUtilities::GetIndexOfEnumValue(ed, keyContent.data());
                    SetMember(md, memIx, arrIx, memberContent, enumVal, writer);
                }
                break;

            default:
                break;
            }
        }

        template <class KeyT>
        void SetMember(const MemberDescriptionType* md,
                       DotsC_MemberIndex memIx,
                       DotsC_Int32 arrIx,
                       const boost::property_tree::ptree& memberContent,
                       const KeyT& key,
                       BlobWriter<RepositoryType>& writer) const
        {
            //if dictionary first write the key
            if (md->GetCollectionType()==DictionaryCollectionType)
            {
                writer.WriteKey(memIx, key);
            }

            //first check if value is set to null in json, in that case just set status to null and return.
            if (memberContent.data()=="null")
            {
                writer.WriteValue(memIx, arrIx, 0, true, false);
                return;
            }

            switch(md->GetMemberType())
            {
            case BooleanMemberType:
                {
                    bool val=memberContent.get_value<bool>();
                    writer.WriteValue(memIx, arrIx, val, false, true);
                }
                break;

            case EnumerationMemberType:
                {
                    const EnumDescriptionType* ed=m_repository->GetEnum(md->GetTypeId());
                    int enumOrdinal=ed->GetIndexOfValue(memberContent.data());
                    if (enumOrdinal<0)
                    {
                        std::ostringstream os;
                        os<<"Enumeration member '"<<md->GetName()<<"' contains an invalid value. Value="<<memberContent.data()<<" is not a value of enum type "<<ed->GetName();
                        throw ParseError("JsonToBinary serialization error", os.str(), "", 158);
                    }
                    writer.WriteValue(memIx, arrIx, enumOrdinal, false, true);
                }
                break;

            case Int32MemberType:
                {
                    DotsC_Int32 val=memberContent.get_value<DotsC_Int32>();
                    writer.WriteValue(memIx, arrIx, val, false, true);
                }
                break;

            case Int64MemberType:
                {
                    DotsC_Int64 val=memberContent.get_value<DotsC_Int64>();
                    writer.WriteValue(memIx, arrIx, val, false, true);
                }
                break;

            case TypeIdMemberType:
                {
                    DotsC_TypeId tid=SerializationUtils::StringToTypeId(memberContent.data());
                    writer.WriteValue(memIx, arrIx, tid, false, true);
                }
                break;

            case InstanceIdMemberType:
            case ChannelIdMemberType:
            case HandlerIdMemberType:
                {
                    std::pair<DotsC_TypeId, const char*> hash=SerializationUtils::StringToHash(memberContent.data());
                    writer.WriteValue(memIx, arrIx, hash, false, true);
                }
                break;

            case EntityIdMemberType:
                {
                    boost::optional<std::string> typeIdString=memberContent.get_optional<std::string>("name");
                    boost::optional<std::string> instanceIdString=memberContent.get_optional<std::string>("instanceId");
                    if (!typeIdString)
                    {
                        std::ostringstream os;
                        os<<"EntityId member '"<<md->GetName()<<"' is missing the name-element that specifies the type.";
                        throw ParseError("JsonToBinary serialization error", os.str(), "", 159);
                    }
                    if (!instanceIdString)
                    {
                        std::ostringstream os;
                        os<<"EntityId member '"<<md->GetName()<<"' is missing the instanceId-element that specifies the instance.";
                        throw ParseError("JsonToBinary serialization error", os.str(), "", 160);
                    }
                    std::pair<DotsC_EntityId, const char*> entId;
                    entId.first.typeId=SerializationUtils::StringToTypeId(*typeIdString);
                    std::pair<DotsC_TypeId, const char*> instanceId=SerializationUtils::StringToHash(*instanceIdString);
                    entId.first.instanceId=instanceId.first;
                    entId.second=instanceId.second;
                    writer.WriteValue(memIx, arrIx, entId, false, true);
                }
                break;

            case StringMemberType:
                {
                    writer.WriteValue(memIx, arrIx, memberContent.data().c_str(), false, true);
                }
                break;

            case ObjectMemberType:
                {
                    boost::optional<std::string> xsiType=memberContent.get_optional<std::string>("_DouType");
                    if (!xsiType)
                    {
                        std::ostringstream os;
                        os<<"Json object does not have the attribute type for object member "<<md->GetName();
                        throw ParseError("JsonToBinary serialization error", os.str(), "", 161);
                    }

                    const ClassDescriptionType* cd=m_repository->GetClass(LlufId_Generate64(xsiType->c_str()));
                    if (!cd)
                    {
                        std::ostringstream os;
                        os<<"Attribute 'type' on member "<<md->GetName()<<" does not specifying a known class. type="<<(*xsiType);
                        throw ParseError("JsonToBinary serialization error", os.str(), "", 162);
                    }
                    else if (!BasicTypeOperations::IsOfType(m_repository, ObjectMemberType, cd->GetTypeId(), ObjectMemberType, md->GetTypeId()))
                    {
                        std::ostringstream os;
                        os<<"Attribute 'type' on member "<<md->GetName()<<" is specitying an invalid type. "<<cd->GetName()<<" is not a subtype of "<<m_repository->GetClass(md->GetTypeId())->GetName();
                        throw ParseError("JsonToBinary serialization error", os.str(), "", 163);
                    }

                    std::vector<char> insideBlob;
                    SerializeObjectContent(*xsiType, insideBlob, memberContent);
                    writer.WriteValue(memIx, arrIx, std::make_pair(static_cast<const char*>(&insideBlob[0]), static_cast<DotsC_Int32>(insideBlob.size())), false, true);
                }
                break;

            case BinaryMemberType:
                {
                    std::string bin;
                    if (!SerializationUtils::FromBase64(memberContent.data(), bin))
                    {
                        std::ostringstream os;
                        os<<"Member "<<md->GetName()<<" of type binary containes invalid base64 data";
                        throw ParseError("JsonToBinary serialization error", os.str(), "", 164);
                    }
                    writer.WriteValue(memIx, arrIx, std::make_pair(static_cast<const char*>(&bin[0]), static_cast<DotsC_Int32>(bin.size())), false, true);
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
                    try
                    {
                        DotsC_Float32 val=classic_string_cast<DotsC_Float32>(memberContent.data());
                        writer.WriteValue(memIx, arrIx, val, false, true);
                    }
                    catch (const boost::bad_lexical_cast&)
                    {
                        std::ostringstream os;
                        os<<"Member "<<md->GetName()<<" of type Float32 contains invalid value. Value="<<memberContent.data();
                        throw ParseError("JsonToBinary serialization error", os.str(), "", 165);
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
                    try
                    {
                        DotsC_Float64 val=classic_string_cast<DotsC_Float64>(memberContent.data());
                        writer.WriteValue(memIx, arrIx, val, false, true);
                    }
                    catch (const boost::bad_lexical_cast&)
                    {
                        std::ostringstream os;
                        os<<"Member "<<md->GetName()<<" of type Float32 contains invalid value. Value="<<memberContent.data();
                        throw ParseError("JsonToBinary serialization error", os.str(), "", 149);
                    }
                }
                break;
            }
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
