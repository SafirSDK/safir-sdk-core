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
#ifndef __DOTS_INTERNAL_Internal_UGLYXML_TO_BLOB_H__
#define __DOTS_INTERNAL_Internal_UGLYXML_TO_BLOB_H__

#include <string>
#include <vector>
#include <sstream>
#include <boost/noncopyable.hpp>
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/xml_parser.hpp>
#include <boost/iostreams/stream.hpp>
#include <boost/iostreams/device/array.hpp>
#include <Safir/Dob/Typesystem/Internal/Id.h>
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
    class UglyXmlToBlobSerializer : private boost::noncopyable
    {
    public:
        typedef typename Traits::RepositoryType RepositoryType;
        typedef typename Traits::ClassDescriptionType ClassDescriptionType;
        typedef typename Traits::MemberDescriptionType MemberDescriptionType;
        typedef typename Traits::EnumDescriptionType EnumDescriptionType;
        typedef typename Traits::ParameterDescriptionType ParameterDescriptionType;

        UglyXmlToBlobSerializer(const RepositoryType* repository)
            :m_repository(repository)
            ,m_blobLayout(repository)
        {
        }

        void operator()(const char* xml, std::vector<char>& blob) const
        {
            boost::property_tree::ptree pt;
            boost::iostreams::array_source src(xml, strlen(xml));
            boost::iostreams::stream<boost::iostreams::array_source> stream(src);
            boost::property_tree::xml_parser::read_xml(stream, pt, boost::property_tree::xml_parser::trim_whitespace | boost::property_tree::xml_parser::no_comments);
            this->operator ()(pt, blob);
        }

        void operator()(boost::property_tree::ptree& xml, std::vector<char>& blob) const
        {
            if (xml.front().first!="object")
            {
                std::ostringstream os;
                os<<"The Xml does not contain member <object> wich must be the root element of the old xml format. You should consider using the new xml format instead!";
                throw ParseError("UglyXmlToBinary serialization error", os.str(), "", 124);
            }

            SerializeObjectContent(blob, xml.front().second);
        }

        DotsC_TypeId SerializeObjectContent(std::vector<char>& blob, boost::property_tree::ptree& content) const
        {
            boost::optional<std::string> typeName=content.get_optional<std::string>("name");
            if (!typeName)
            {
                std::ostringstream os;
                os<<"The Xml does not contain member <name> that specifies the type.";
                throw ParseError("UglyXmlToBinary serialization error", os.str(), "", 125);
            }

            SerializationUtils::Trim(*typeName);
            DotsC_TypeId typeId=DotsId_Generate64(typeName->c_str());
            const ClassDescriptionType* cd=m_repository->GetClass(typeId);
            if (!cd)
            {
                throw ParseError("UglyXmlToBlobSerializer serialization error", "Xml does not contain a known type. Typename: "+*typeName, "", 126);
            }

            char* beginningOfUnused=NULL;
            size_t blobInitSize=std::max(size_t(1000), static_cast<size_t>(2*cd->InitialSize()));
            blob.reserve(blobInitSize); //Note: maybe xmlSize/2 would be enogh in almost all cases
            blob.resize(cd->InitialSize(), 0);
            m_blobLayout.FormatBlob(&blob[0], static_cast<Size>(blob.size()), typeId, beginningOfUnused);

            boost::optional<boost::property_tree::ptree&> members=content.get_child_optional("members");
            if (!members)
            {
                return typeId; //this is ok, a class with no members
            }

            for (boost::property_tree::ptree::iterator memIt=members->begin(); memIt!=members->end(); ++memIt)
            {
                if (memIt->first!="member")
                {
                    std::ostringstream os;
                    os<<"Unexpected element '"<<memIt->first<<"'. Only <member> is valid at this level.";
                    throw ParseError("UglyXmlToBlobSerializer serialization error", os.str(), "", 127);
                }

                boost::optional<std::string> memberName=memIt->second.get_optional<std::string>("name");
                if (!memberName)
                {
                    throw ParseError("UglyXmlToBlobSerializer serialization error", "Missing member name. Expecting element <name>.", "", 128);
                }

                SerializationUtils::Trim(*memberName);
                int memIx=cd->GetMemberIndex(*memberName);
                if (memIx<0)
                {
                    std::ostringstream os;
                    os<<"Failed to serialize xml to binary. The class '"<<cd->GetName()<<"' does not contain a member named '"<<*memberName<<"'";
                    throw ParseError("UglyXmlToBlobSerializer serialization error", os.str(), "", 129);
                }

                const MemberDescriptionType* md=cd->GetMember(memIx);

                if (!md->IsArray())
                {
                    try
                    {
                        SetMember(md, memIx, 0, memIt->second, blob, beginningOfUnused);
                    }
                    catch (const boost::property_tree::ptree_error&)
                    {
                        std::ostringstream os;
                        os<<"Failed to serialize member '"<<cd->GetName()<<"."<<md->GetName()<<"' from xml to binary. Type is incorrect.";
                        throw ParseError("UglyXmlToBlobSerializer serialization error", os.str(), "", 130);
                    }
                }
                else
                {
                    boost::optional<boost::property_tree::ptree&> arrayElements=memIt->second.get_child_optional("arrayElements");
                    if (!arrayElements)
                    {
                        continue; //this is ok, no elements in array
                    }

                    int arrayIndex=0;
                    for (boost::property_tree::ptree::iterator arrIt=arrayElements->begin(); arrIt!=arrayElements->end(); ++arrIt)
                    {
                        if (arrIt->first!="arrayElement")
                        {
                            std::ostringstream os;
                            os<<"Failed to serialize array member '"<<cd->GetName()<<"."<<md->GetName()<<"' index="<<arrayIndex<<" from xml to binary. Expecting element <arrayElement>, not "<<arrIt->first;
                            throw ParseError("UglyXmlToBlobSerializer serialization error", os.str(), "", 131);
                        }

                        boost::optional<std::string> index=arrIt->second.get_optional<std::string>("index");
                        if (index) //arrayIndex explicit specified
                        {
                            SerializationUtils::Trim(*index);
                            arrayIndex=boost::lexical_cast<int>(*index);
                        }

                        if (md->GetArraySize()<=arrayIndex)
                        {
                            std::ostringstream os;
                            os<<"Failed to serialize array member '"<<cd->GetName()<<"."<<md->GetName()<<"' with index="<<arrayIndex<<" from xml to binary. Index out of range. ArraySize is "<<md->GetArraySize();
                            throw ParseError("UglyXmlToBlobSerializer serialization error", os.str(), "", 132);
                        }

                        try
                        {
                            SetMember(md, memIx, arrayIndex, arrIt->second, blob, beginningOfUnused);
                        }
                        catch (const boost::property_tree::ptree_error&)
                        {
                            std::ostringstream os;
                            os<<"Failed to serialize array member '"<<cd->GetName()<<"."<<md->GetName()<<"' with index="<<arrayIndex<<" from xml to binary. Type is incorrect.";
                            throw ParseError("UglyXmlToBlobSerializer serialization error", os.str(), "", 133);
                        }

                        ++arrayIndex;
                    }
                }
            }

            return typeId;
        }

    private:
        const RepositoryType* m_repository;
        const BlobLayoutImpl<RepositoryType> m_blobLayout;

        void SetMember(const MemberDescriptionType* md,
                       DotsC_MemberIndex memIx,
                       DotsC_ArrayIndex arrIx,
                       boost::property_tree::ptree& memberContent,
                       std::vector<char>& blob,
                       char* &beginningOfUnused) const
        {
            boost::optional<boost::property_tree::ptree&> val;
            if (md->GetMemberType()==ObjectMemberType)
            {
                val=memberContent.get_child_optional("object");
                if (!val)
                {
                    std::ostringstream os;
                    os<<"Element <object> is missing on member '"<<md->GetName()<<"'.";
                    throw ParseError("UglyXmlToBlobSerializer serialization error", os.str(), "", 134);
                }

                std::vector<char> insideBlob;
                DotsC_TypeId insideBlobTypeId=SerializeObjectContent(insideBlob, *val);

                if (!BasicTypeOperations::IsOfType(m_repository, ObjectMemberType, insideBlobTypeId, ObjectMemberType, md->GetTypeId()))
                {
                    std::ostringstream os;
                    os<<"Member "<<md->GetName()<<" has object of incompatible type. "<<m_repository->GetClass(insideBlobTypeId)->GetName()<<" is not a subtype of "<<m_repository->GetClass(md->GetTypeId())->GetName();
                    throw ParseError("UglyXmlToBlobSerializer serialization error", os.str(), "", 135);
                }

                SerializationUtils::CreateSpaceForDynamicMember(blob, beginningOfUnused, insideBlob.size());
                char* writeObj=beginningOfUnused;
                m_blobLayout.CreateObjectMember(&blob[0], static_cast<Size>(insideBlob.size()), insideBlobTypeId, memIx, arrIx, false, beginningOfUnused);
                beginningOfUnused=writeObj+insideBlob.size(); //This is a hack. BlobLayout is not moving beginningOfUnused by the blobSize but instead only by the initialSize. Has to do with genated code.
                assert(static_cast<size_t>(beginningOfUnused-&blob[0])<=blob.size()); //size_t usedSize=static_cast<size_t>(beginningOfUnused-&blob[0]);
                memcpy(writeObj, &insideBlob[0], insideBlob.size());
                m_blobLayout.SetStatus(false, false, &blob[0], memIx, arrIx);
                return;
            }
            else if (md->GetMemberType()==EntityIdMemberType)
            {
                val=memberContent.get_child_optional("entityId");
            }
            else
            {
                val=memberContent.get_child_optional("value");
            }

            if (val)
            {
                SerializationUtils::SetMemberValue(m_repository, m_blobLayout, md, memIx, arrIx, *val, blob, beginningOfUnused);
            }
            else
            {
                //not an object and not a value, then valueRef is the last valid option
                try
                {
                    boost::property_tree::ptree& refPt=memberContent.get_child("valueRef");
                    std::pair<std::string, int> paramInfo=GetParameterNameAndIndex(md, arrIx, refPt);
                    SerializationUtils::SetMemberFromParameter(m_repository, m_blobLayout, md, memIx, arrIx, paramInfo.first, paramInfo.second, blob, beginningOfUnused);
                }
                catch (const boost::property_tree::ptree_error&)
                {
                    std::ostringstream os;
                    os<<"Error while parsing member "<<md->GetName();
                    throw ParseError("UglyXmlToBinary serialization error", os.str(), "", 136);
                }
            }
        }

        std::pair<std::string, int> GetParameterNameAndIndex(const MemberDescriptionType* md, DotsC_ArrayIndex arrIx, boost::property_tree::ptree& memberContent) const
        {
            try
            {
                std::pair<std::string, int> param=std::make_pair(memberContent.get<std::string>("name"), 0);
                SerializationUtils::Trim(param.first);
                boost::optional<std::string> index=memberContent.get_optional<std::string>("index");
                if (index)
                {
                    SerializationUtils::Trim(*index);
                    param.second=boost::lexical_cast<int>(*index);
                }
                return param;
            }
            catch (...)
            {
                std::ostringstream os;
                os<<"Missing <name> element in valueRef inside a serialized object. Happened in member "<<md->GetName()<<" index="<<arrIx;
                throw ParseError("UglyXmlToBinary serialization error", os.str(), "", 137);
            }
        }
    };
}
}
}
}
} //end namespace Safir::Dob::Typesystem::Internal:Internal

#endif
