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
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/
#include <boost/property_tree/xml_parser.hpp>
#include <boost/iostreams/stream.hpp>
#include <boost/iostreams/device/array.hpp>
#include <Safir/Dob/Typesystem/Internal/Id.h>
#include "XmlToBlobSerializer.h"
#include "ParseAlgorithms.h"
#include "BasicTypes.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    XmlToBlobSerializer::XmlToBlobSerializer(const TypeRepository* repository)
        :m_repository(repository)
        ,m_blobLayout(repository)
    {
    }

    void XmlToBlobSerializer::operator()(const char* xml, std::vector<char>& blob) const
    {
        boost::property_tree::ptree pt;
        boost::iostreams::array_source src(xml, strlen(xml));
        boost::iostreams::stream<boost::iostreams::array_source> stream(src);
        boost::property_tree::xml_parser::read_xml(stream, pt, boost::property_tree::xml_parser::trim_whitespace);
        this->operator ()(pt, blob);
    }

    void XmlToBlobSerializer::operator()(const boost::property_tree::ptree& xml, std::vector<char>& blob) const
    {
        const boost::property_tree::ptree& members=xml.front().second;
        boost::optional<std::string> xsiType=members.get_optional<std::string>("<xmlattr>.type");
        std::string typeName;
        if (xsiType)
        {
            typeName=*xsiType;
        }
        else
        {
            typeName=xml.front().first;
        }

        SerializeObjectContent(typeName, blob, members);

    }

    void XmlToBlobSerializer::SerializeObjectContent(const std::string& typeName,
                                                     std::vector<char>& blob,
                                                     const boost::property_tree::ptree& members) const
    {
        TypeId typeId=DotsId_Generate64(typeName.c_str());
        const ClassDescription* cd=m_repository->GetClass(typeId);
        if (!cd)
        {
            throw ParseError("XmlToBinary serialization error", "Xml does not contain a known type. Typename: "+typeName, "", 201);
        }

        char* beginningOfUnused=NULL;

        size_t blobInitSize=std::max(size_t(1000), static_cast<size_t>(2*cd->InitialSize()));
        blob.reserve(blobInitSize); //Note: maybe xmlSize/2 would be enogh in almost all cases
        blob.resize(cd->InitialSize(), 0);
        m_blobLayout.FormatBlob(&blob[0], blob.size(), typeId, beginningOfUnused);

        for (boost::property_tree::ptree::const_iterator memIt=members.begin(); memIt!=members.end(); ++memIt)
        {
            const std::string& elementName=memIt->first;
            int memIx=cd->GetMemberIndex(elementName);
            if (memIx<0)
            {
                if (elementName=="<xmlcomment>")
                {
                    continue;
                }
                if (elementName=="<xmlattr>")
                {
                    continue; //we ignore attributes. Improvement is to check that only allowed attributes are present. I.e type and xsi:nil
                }

                std::ostringstream os;
                os<<"Failed to serialize xml to binary. The class '"<<cd->GetName()<<"' does not contain a member named '"<<elementName<<"'";
                throw ParseError("XmlToBinary serialization error", os.str(), "", 203);
            }

            const MemberDescription* md=cd->GetMember(memIx);

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
                    os<<"Failed to serialize member '"<<cd->GetName()<<"."<<md->GetName()<<"' from xml to binary. Type is incorrect.";
                    throw ParseError("XmlToBinary serialization error", os.str(), "", 210);
                }
            }
            else
            {
                //array, then the inner propertyTree contains array element and the array elements contains the content
                //i.e <myIntArray><Int32 index=0>1</Int32><Int32 index=5>2</Int32></myIntArray>
                for (boost::property_tree::ptree::const_iterator arrIt=memIt->second.begin(); arrIt!=memIt->second.end(); ++arrIt)
                {
                    boost::optional<int> index=arrIt->second.get_optional<int>("<xmlattr>.index");
                    if (index)
                    {
                        if (md->GetArraySize()<=*index)
                        {
                            std::ostringstream os;
                            os<<"Failed to serialize array member '"<<cd->GetName()<<"."<<md->GetName()<<"' with index="<<*index<<" from xml to binary. Index out of range. ArraySize is "<<md->GetArraySize();
                            throw ParseError("XmlToBinary serialization error", os.str(), "", 216);
                        }

                        try
                        {
                            SetMember(md, memIx, *index, arrIt->second, blob, beginningOfUnused);
                        }
                        catch (const boost::property_tree::ptree_error&)
                        {
                            std::ostringstream os;
                            os<<"Failed to serialize array member '"<<cd->GetName()<<"."<<md->GetName()<<"' with index="<<*index<<" from xml to binary. Type is incorrect.";
                            throw ParseError("XmlToBinary serialization error", os.str(), "", 211);
                        }
                    }
                    else
                    {
                        std::ostringstream os;
                        os<<"Serialization from xml to binary failed because the xml of array member '"<<md->GetName()<<"' is missing index-attribute";
                        throw ParseError("XmlToBinary serialization error", os.str(), "", 204);
                    }
                }
            }
        }
    }

    void XmlToBlobSerializer::SetMember(const MemberDescription* md,
                                        DotsC_MemberIndex memIx,
                                        DotsC_ArrayIndex arrIx,
                                        const boost::property_tree::ptree& memberContent,
                                        std::vector<char>& blob,
                                        char* &beginningOfUnused) const
    {
        switch(md->GetMemberType())
        {
        case BooleanMemberType:
        {
            bool val=memberContent.get_value<bool>();
            m_blobLayout.SetMember<bool>(val, &blob[0], memIx, arrIx);
            m_blobLayout.SetStatus(false, false, &blob[0], memIx, arrIx);
        }
            break;

        case EnumerationMemberType:
        {
            const EnumDescription* ed=m_repository->GetEnum(md->GetTypeId());
            int enumOrdinal=ed->GetIndexOfValue(memberContent.data());
            if (enumOrdinal<0)
            {
                std::ostringstream os;
                os<<"Enumeration member '"<<md->GetName()<<"' contains an invalid value. Value="<<memberContent.data()<<" is not a value of enum type "<<ed->GetName();
                throw ParseError("XmlToBinary serialization error", os.str(), "", 214);
            }
            m_blobLayout.SetMember<DotsC_Int32>(enumOrdinal, &blob[0], memIx, arrIx);
            m_blobLayout.SetStatus(false, false, &blob[0], memIx, arrIx);
        }
            break;

        case Int32MemberType:
        {
            DotsC_Int32 val=memberContent.get_value<DotsC_Int32>();
            m_blobLayout.SetMember<DotsC_Int32>(val, &blob[0], memIx, arrIx);
            m_blobLayout.SetStatus(false, false, &blob[0], memIx, arrIx);
        }
            break;

        case Int64MemberType:
        {
            DotsC_Int64 val=memberContent.get_value<DotsC_Int64>();
            m_blobLayout.SetMember<DotsC_Int64>(val, &blob[0], memIx, arrIx);
            m_blobLayout.SetStatus(false, false, &blob[0], memIx, arrIx);
        }
            break;

        case TypeIdMemberType:
        {
            DotsC_TypeId tid=StringToTypeId(memberContent.data());
            m_blobLayout.SetMember<DotsC_TypeId>(tid, &blob[0], memIx, arrIx);
            m_blobLayout.SetStatus(false, false, &blob[0], memIx, arrIx);
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
                CreateSpaceForDynamicMember(blob, beginningOfUnused, numBytesNeeded);
            }
            m_blobLayout.CreateAndSetMemberWithOptionalString(&blob[0], hash.first, hash.second, memberContent.data().size()+1, memIx, arrIx, false, beginningOfUnused);
            m_blobLayout.SetStatus(false, false, &blob[0], memIx, arrIx);
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
                throw ParseError("XmlToBinary serialization error", os.str(), "", 212);
            }
            if (!instanceIdString)
            {
                std::ostringstream os;
                os<<"EntityId member '"<<md->GetName()<<"' is missing the instanceId-element that specifies the instance.";
                throw ParseError("XmlToBinary serialization error", os.str(), "", 213);
            }
            DotsC_TypeId tid=StringToTypeId(*typeIdString);
            std::pair<DotsC_TypeId, const char*> instanceId=StringToHash(*instanceIdString);
            if (instanceId.second!=NULL)
            {
                size_t numBytesNeeded=instanceIdString->size()+1+sizeof(DotsC_EntityId)+sizeof(DotsC_Int32); //(typeId+hash)+stringLength+string
                CreateSpaceForDynamicMember(blob, beginningOfUnused, numBytesNeeded);
            }
            DotsC_EntityId eid={tid, instanceId.first};
            m_blobLayout.CreateAndSetMemberWithOptionalString(&blob[0], eid, instanceId.second, instanceIdString->size()+1, memIx, arrIx, false, beginningOfUnused);
            m_blobLayout.SetStatus(false, false, &blob[0], memIx, arrIx);
        }
            break;

        case StringMemberType:
        {
            size_t numBytesNeeded=std::min(memberContent.data().size(), static_cast<size_t>(md->GetMaxLength()))+1; //add one for '\0'
            CreateSpaceForDynamicMember(blob, beginningOfUnused, numBytesNeeded);
            char* writeString=beginningOfUnused;
            m_blobLayout.CreateStringMember(&blob[0], numBytesNeeded, memIx, arrIx, false, beginningOfUnused);
            strncpy(writeString, memberContent.data().c_str(), numBytesNeeded);
            m_blobLayout.SetStatus(false, false, &blob[0], memIx, arrIx);
        }
            break;

        case ObjectMemberType:
        {
            //If object we must find the exact type. Inheritance possible.
            const ClassDescription* cd=NULL;
            boost::optional<std::string> xsiType=memberContent.get_optional<std::string>("<xmlattr>.type");
            if (xsiType)
            {
                cd=m_repository->GetClass(DotsId_Generate64(xsiType->c_str()));
                if (!cd)
                {
                    std::ostringstream os;
                    os<<"Attribute 'type' on member "<<md->GetName()<<" does not specifying a known class. type="<<(*xsiType);
                    throw ParseError("XmlToBinary serialization error", os.str(), "", 202);
                }
                else if (!IsOfType(m_repository, ObjectMemberType, cd->GetTypeId(), ObjectMemberType, md->GetTypeId()))
                {
                    std::ostringstream os;
                    os<<"Attribute 'type' on member "<<md->GetName()<<" is specitying an invalid type. "<<cd->GetName()<<" is not a subtype of "<<m_repository->GetClass(md->GetTypeId())->GetName();
                    throw ParseError("XmlToBinary serialization error", os.str(), "", 218);
                }
            }
            else
            {
                cd=m_repository->GetClass(md->GetTypeId());
            }

            std::vector<char> insideBlob;
            SerializeObjectContent(cd->GetName(), insideBlob, memberContent);
            CreateSpaceForDynamicMember(blob, beginningOfUnused, insideBlob.size());
            char* writeObj=beginningOfUnused;
            m_blobLayout.CreateObjectMember(&blob[0], insideBlob.size(), cd->GetTypeId(), memIx, arrIx, false, beginningOfUnused);
            beginningOfUnused=writeObj+insideBlob.size(); //This is a hack. BlobLayout is not moving beginningOfUnused by the blobSize but instead only by the initialSize. Has to do with genated code.
            memcpy(writeObj, &insideBlob[0], insideBlob.size());
            m_blobLayout.SetStatus(false, false, &blob[0], memIx, arrIx);
        }
            break;

        case BinaryMemberType:
        {
            std::string bin;
            if (!BasicTypes::FromBase64(memberContent.data(), bin))
            {
                std::ostringstream os;
                os<<"Member "<<md->GetName()<<" of type binary containes invalid base64 data";
                throw ParseError("XmlToBinary serialization error", os.str(), "",  205);
            }
            CreateSpaceForDynamicMember(blob, beginningOfUnused, bin.size());
            char* writeBinary=beginningOfUnused;
            m_blobLayout.CreateBinaryMember(&blob[0], bin.size(), memIx, arrIx, false, beginningOfUnused);
            memcpy(writeBinary, &bin[0], bin.size());
            m_blobLayout.SetStatus(false, false, &blob[0], memIx, arrIx);
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
                m_blobLayout.SetMember<DotsC_Float32>(val, &blob[0], memIx, arrIx);
                m_blobLayout.SetStatus(false, false, &blob[0], memIx, arrIx);
            }
            catch (const boost::bad_lexical_cast&)
            {
                std::ostringstream os;
                os<<"Member "<<md->GetName()<<" of type Float32 contains invalid value. Value="<<memberContent.data();
                throw ParseError("XmlToBinary serialization error", os.str(), "",  215);
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
                m_blobLayout.SetMember<DotsC_Float64>(val, &blob[0], memIx, arrIx);
                m_blobLayout.SetStatus(false, false, &blob[0], memIx, arrIx);
            }
            catch (const boost::bad_lexical_cast&)
            {
                std::ostringstream os;
                os<<"Member "<<md->GetName()<<" of type Float32 contains invalid value. Value="<<memberContent.data();
                throw ParseError("XmlToBinary serialization error", os.str(), "",  215);
            }
        }
            break;
        }

    }

    DotsC_TypeId XmlToBlobSerializer::StringToTypeId(const std::string& str) const
    {
        DotsC_TypeId tid=0;
        try
        {
            tid=boost::lexical_cast<DotsC_TypeId>(str);
        }
        catch (const boost::bad_lexical_cast&)
        {
            tid=DotsId_Generate64(str.c_str());
        }
        return tid;
    }

    std::pair<DotsC_TypeId, const char*> XmlToBlobSerializer::StringToHash(const std::string& str) const
    {        
        std::pair<DotsC_TypeId, const char*> result(0, static_cast<const char*>(NULL));
        try
        {
            result.first=boost::lexical_cast<boost::int64_t>(str);
        }
        catch (const boost::bad_lexical_cast&)
        {
            result.first=DotsId_Generate64(str.c_str());
            result.second=str.c_str();
        }
        return result;
    }

    void XmlToBlobSerializer::CreateSpaceForDynamicMember(std::vector<char>& blob,
                                                          char* & beginningOfUnused,
                                                          size_t dynamicMemberSizeNeeded) const
    {
        size_t usedSize=beginningOfUnused-&blob[0];

        //size_t blobSize=blob.size();
        //size_t blobCapacity=blob.capacity();

        ENSURE(usedSize<=blob.size(), <<"XmlToBlobSerializer has written outside blob");
        size_t unusedSize=blob.size()-usedSize;

        if (unusedSize>=dynamicMemberSizeNeeded)
        {
            return; //there is enough free bytes in blob
        }

        if (blob.capacity()-blob.size()<dynamicMemberSizeNeeded)
        {
            //Blob is too small. A bigger blob must be allocated and the content of the old one must be copied.
            std::vector<char> tmp;
            tmp.swap(blob);
            blob.clear(); //unneccessary?
            blob.reserve((tmp.capacity()+dynamicMemberSizeNeeded)*2);
            blob.insert(blob.begin(), tmp.begin(), tmp.end());
            beginningOfUnused=&blob[0]+usedSize;
        }

        //When we get here, the blob is guaranteed to have capacity for the needed extra space. Just resize.
        blob.resize(blob.size()+dynamicMemberSizeNeeded);

//        size_t usedSize2=beginningOfUnused-&blob[0];
//        size_t blobSize2=blob.size();
//        size_t blobCapacity2=blob.capacity();
//        std::cout<<usedSize<<" "<<blobSize2<<" "<<blobCapacity2<<std::endl;
    }
}
}
}
} //end namespace Safir::Dob::Typesystem::Internal
