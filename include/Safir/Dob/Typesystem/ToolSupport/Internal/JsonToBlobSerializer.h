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
#include <Safir/Dob/Typesystem/ToolSupport/Internal/BlobLayoutImpl.h>
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
            ,m_blobLayout(repository)
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

            char* beginningOfUnused=NULL;

            size_t blobInitSize=std::max(size_t(1000), static_cast<size_t>(2*cd->InitialSize()));
            blob.reserve(blobInitSize); //Note: maybe JsonSize/2 would be enogh in almost all cases
            blob.resize(cd->InitialSize(), 0);
            m_blobLayout.FormatBlob(&blob[0], static_cast<Size>(blob.size()), typeId, beginningOfUnused);

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
                if (!md->IsArray())
                {
                    //non-array, then the inner propertyTree contains the content, i.e <myInt>123</myInt>
                    try
                    {
                        SetMember(md, memIx, 0, memIt->second, blob, beginningOfUnused);
                    }
                    catch (const boost::property_tree::ptree_error&)
                    {
                        std::ostringstream os;
                        os<<"Failed to serialize member '"<<cd->GetName()<<"."<<md->GetName()<<"' from Json to binary. Type is incorrect.";
                        throw ParseError("JsonToBinary serialization error", os.str(), "", 146);
                    }
                }
                else
                {
                    DotsC_ArrayIndex arrayIndex=0;
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
                            SetMember(md, memIx, arrayIndex++, arrIt->second, blob, beginningOfUnused);
                        }
                        catch (const boost::property_tree::ptree_error&)
                        {
                            std::ostringstream os;
                            os<<"Failed to serialize array member '"<<cd->GetName()<<"."<<md->GetName()<<"' with index="<<arrayIndex<<" from Json to binary. Type is incorrect.";
                            throw ParseError("JsonToBinary serialization error", os.str(), "", 148);
                        }
                    }
                }
            }
        }

    private:
        const RepositoryType* m_repository;
        const BlobLayoutImpl<RepositoryType> m_blobLayout;

        void SetMember(const MemberDescriptionType* md,
                       DotsC_MemberIndex memIx,
                       DotsC_ArrayIndex arrIx,
                       const boost::property_tree::ptree& memberContent,
                       std::vector<char>& blob,
                       char* &beginningOfUnused) const
        {
            //first check if value is set to null in json, in that case just set status to null and return.
            if (memberContent.data()=="null")
            {
                m_blobLayout.SetStatus(true, false, &blob[0], memIx, arrIx);
                return;
            }

            switch(md->GetMemberType())
            {
            case BooleanMemberType:
            {
                bool val=memberContent.get_value<bool>();
                m_blobLayout.template SetMember<bool>(val, &blob[0], memIx, arrIx);
                m_blobLayout.SetStatus(false, true, &blob[0], memIx, arrIx);
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
                m_blobLayout.template SetMember<DotsC_Int32>(enumOrdinal, &blob[0], memIx, arrIx);
                m_blobLayout.SetStatus(false, true, &blob[0], memIx, arrIx);
            }
                break;

            case Int32MemberType:
            {
                DotsC_Int32 val=memberContent.get_value<DotsC_Int32>();
                m_blobLayout.template SetMember<DotsC_Int32>(val, &blob[0], memIx, arrIx);
                m_blobLayout.SetStatus(false, true, &blob[0], memIx, arrIx);
            }
                break;

            case Int64MemberType:
            {
                DotsC_Int64 val=memberContent.get_value<DotsC_Int64>();
                m_blobLayout.template SetMember<DotsC_Int64>(val, &blob[0], memIx, arrIx);
                m_blobLayout.SetStatus(false, true, &blob[0], memIx, arrIx);
            }
                break;

            case TypeIdMemberType:
            {
                DotsC_TypeId tid=StringToTypeId(memberContent.data());
                m_blobLayout.template SetMember<DotsC_TypeId>(tid, &blob[0], memIx, arrIx);
                m_blobLayout.SetStatus(false, true, &blob[0], memIx, arrIx);
            }
                break;

            case InstanceIdMemberType:
            case ChannelIdMemberType:
            case HandlerIdMemberType:
            {
                std::pair<DotsC_TypeId, const char*> hash=StringToHash(memberContent.data());
                if (hash.second!=NULL)
                {
                    size_t numBytesNeeded=memberContent.data().size()+1+sizeof(DotsC_Int64)+sizeof(DotsC_Int32); //hash+stringLength+string
                    SerializationUtils::CreateSpaceForDynamicMember(m_blobLayout, blob, beginningOfUnused, numBytesNeeded);
                }
                m_blobLayout.CreateAndSetMemberWithOptionalString(&blob[0], hash.first, hash.second, static_cast<Size>(memberContent.data().size())+1, memIx, arrIx, false, beginningOfUnused);
                m_blobLayout.SetStatus(false, true, &blob[0], memIx, arrIx);
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
                DotsC_TypeId tid=StringToTypeId(*typeIdString);
                std::pair<DotsC_TypeId, const char*> instanceId=StringToHash(*instanceIdString);
                if (instanceId.second!=NULL)
                {
                    size_t numBytesNeeded=instanceIdString->size()+1+sizeof(DotsC_EntityId)+sizeof(DotsC_Int32); //(typeId+hash)+stringLength+string
                    SerializationUtils::CreateSpaceForDynamicMember(m_blobLayout, blob, beginningOfUnused, numBytesNeeded);
                }
                DotsC_EntityId eid={tid, instanceId.first};
                m_blobLayout.CreateAndSetMemberWithOptionalString(&blob[0], eid, instanceId.second, static_cast<Size>(instanceIdString->size())+1, memIx, arrIx, false, beginningOfUnused);
                m_blobLayout.SetStatus(false, true, &blob[0], memIx, arrIx);
            }
                break;

            case StringMemberType:
            {
                size_t numBytesNeeded=std::min(memberContent.data().size(), static_cast<size_t>(md->GetMaxLength()))+1; //add one for '\0'
                SerializationUtils::CreateSpaceForDynamicMember(m_blobLayout, blob, beginningOfUnused, numBytesNeeded);
                char* writeString=beginningOfUnused;
                m_blobLayout.CreateStringMember(&blob[0], static_cast<Size>(numBytesNeeded), memIx, arrIx, false, beginningOfUnused);
                strncpy(writeString, memberContent.data().c_str(), numBytesNeeded);
                m_blobLayout.SetStatus(false, true, &blob[0], memIx, arrIx);
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
                SerializationUtils::CreateSpaceForDynamicMember(m_blobLayout, blob, beginningOfUnused, insideBlob.size());
                char* writeObj=beginningOfUnused;
                m_blobLayout.CreateObjectMember(&blob[0], static_cast<Size>(insideBlob.size()), LlufId_Generate64(xsiType->c_str()), memIx, arrIx, false, beginningOfUnused);
                beginningOfUnused=writeObj+insideBlob.size(); //This is a hack. BlobLayout is not moving beginningOfUnused by the blobSize but instead only by the initialSize. Has to do with genated code.
                memcpy(writeObj, &insideBlob[0], insideBlob.size());
                m_blobLayout.SetStatus(false, true, &blob[0], memIx, arrIx);
            }
                break;

            case BinaryMemberType:
            {
                std::vector<char> bin;
                if (!SerializationUtils::FromBase64(memberContent.data(), bin))
                {
                    std::ostringstream os;
                    os<<"Member "<<md->GetName()<<" of type binary containes invalid base64 data";
                    throw ParseError("JsonToBinary serialization error", os.str(), "", 164);
                }
                SerializationUtils::CreateSpaceForDynamicMember(m_blobLayout, blob, beginningOfUnused, bin.size());
                char* writeBinary=beginningOfUnused;
                m_blobLayout.CreateBinaryMember(&blob[0], static_cast<Size>(bin.size()), memIx, arrIx, false, beginningOfUnused);
                if (!bin.empty())
                {
                    memcpy(writeBinary, &bin[0], bin.size());
                }
                m_blobLayout.SetStatus(false, true, &blob[0], memIx, arrIx);
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
                    m_blobLayout.template SetMember<DotsC_Float32>(val, &blob[0], memIx, arrIx);
                    m_blobLayout.SetStatus(false, true, &blob[0], memIx, arrIx);
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
                    m_blobLayout.template SetMember<DotsC_Float64>(val, &blob[0], memIx, arrIx);
                    m_blobLayout.SetStatus(false, true, &blob[0], memIx, arrIx);
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

        DotsC_TypeId StringToTypeId(const std::string& str) const
        {
            DotsC_TypeId tid=0;
            try
            {
                tid=boost::lexical_cast<DotsC_TypeId>(str);
            }
            catch (const boost::bad_lexical_cast&)
            {
                tid=LlufId_Generate64(str.c_str());
            }
            return tid;
        }

        std::pair<DotsC_TypeId, const char*> StringToHash(const std::string& str) const
        {
            std::pair<DotsC_TypeId, const char*> result(0, static_cast<const char*>(NULL));
            try
            {
                result.first=boost::lexical_cast<boost::int64_t>(str);
            }
            catch (const boost::bad_lexical_cast&)
            {
                result.first=LlufId_Generate64(str.c_str());
                result.second=str.c_str();
            }
            return result;
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
