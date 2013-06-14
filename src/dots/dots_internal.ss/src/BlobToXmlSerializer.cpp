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
#include <stdexcept>
#include <boost/property_tree/xml_parser.hpp>
#include "BlobToXmlSerializer.h"
#include "BasicTypes.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    BlobToXmlSerializer::BlobToXmlSerializer(const TypeRepository* repository, const char* blob)
        :m_repository(repository)
        ,m_blob(blob)
        ,m_blobLayout(repository)
    {
    }

    void BlobToXmlSerializer::operator()(std::ostream& os) const
    {
        boost::property_tree::ptree content;
        SerializeMembers(content, false);
        boost::property_tree::ptree root;
        root.push_back(std::make_pair(GetClass()->GetName(), content));
        boost::property_tree::xml_parser::write_xml(os, root);
    }

    void BlobToXmlSerializer::SerializeMembers(boost::property_tree::ptree& content, bool addTypeAttr) const
    {
        const ClassDescription* cd=GetClass();

        if (addTypeAttr)
        {
            content.add("<xmlattr>.type", cd->GetName());
        }

        for (DotsC_MemberIndex memberIx=0; memberIx<cd->GetNumberOfMembers(); ++memberIx)
        {
            const MemberDescription* md=cd->GetMember(memberIx);
            if (!md->IsArray()) //normal member
            {
                SerializeMember(md, memberIx, 0, md->GetName(), content);
            }
            else //array member
            {
                DotsC_MemberType mt=md->GetMemberType();
                const char* typeName=NULL;
                switch (mt)
                {
                case ObjectMemberType:
                    typeName=m_repository->GetClass(md->GetTypeId())->GetName();
                    break;
                case EnumerationMemberType:
                    typeName=m_repository->GetEnum(md->GetTypeId())->GetName();
                    break;
                default:
                    typeName=BasicTypes::Instance().StringOf(mt).c_str();
                    break;
                }

                boost::property_tree::ptree arrayValues;
                for (DotsC_ArrayIndex arrIx=0; arrIx<md->GetArraySize(); ++arrIx)
                {
                    SerializeMember(md, memberIx, arrIx, typeName, arrayValues);
                }
                if (arrayValues.size()>0) //only add array element if there are non-null values
                {
                    content.add_child(md->GetName(), arrayValues);
                }
            }
        }
    }

    void BlobToXmlSerializer::SerializeMember(const MemberDescription* md,
                                              DotsC_MemberIndex memberIndex,
                                              DotsC_ArrayIndex arrayIndex,
                                              const char* elementName,
                                              boost::property_tree::ptree& pt) const
    {
        boost::property_tree::ptree* arrayElementPt=NULL;

        switch(md->GetMemberType())
        {
        case BooleanMemberType:
        {
            bool val=true;
            DotsC_MemberStatus status=m_blobLayout.GetMember<bool>(m_blob, memberIndex, arrayIndex, val);
            if (!status.IsNull())
            {
                arrayElementPt=&pt.add(elementName, val ? "true" : "false");
            }
        }
            break;

        case EnumerationMemberType:
        {
            DotsC_Int32 val=0;
            DotsC_MemberStatus status=m_blobLayout.GetMember<DotsC_Int32>(m_blob, memberIndex, arrayIndex, val);
            if (!status.IsNull())
            {
                const char* enumVal=m_repository->GetEnum(md->GetTypeId())->GetValueName(val);
                arrayElementPt=&pt.add(elementName, enumVal);
            }
        }
            break;

        case Int32MemberType:
        {
            DotsC_Int32 val=0;
            DotsC_MemberStatus status=m_blobLayout.GetMember<DotsC_Int32>(m_blob, memberIndex, arrayIndex, val);
            if (!status.IsNull())
            {
                arrayElementPt=&pt.add(elementName, val);
            }
        }
            break;

        case Int64MemberType:
        {
            DotsC_Int64 val=0;
            DotsC_MemberStatus status=m_blobLayout.GetMember<DotsC_Int64>(m_blob, memberIndex, arrayIndex, val);
            if (!status.IsNull())
            {
                arrayElementPt=&pt.add(elementName, val);
            }
        }
            break;

        case TypeIdMemberType:
        {
            DotsC_Int64 val=0;
            DotsC_MemberStatus status=m_blobLayout.GetMember<DotsC_Int64>(m_blob, memberIndex, arrayIndex, val);
            if (!status.IsNull())
            {
                const char* typeName=TypeIdToString(val);
                if (typeName)
                {
                    arrayElementPt=&pt.add(elementName, typeName);
                }
                else
                {
                    arrayElementPt=&pt.add(elementName, val);
                }
            }
        }
            break;

        case InstanceIdMemberType:
        case ChannelIdMemberType:
        case HandlerIdMemberType:
        {
            DotsC_Int64 val=0;
            const char* hashStr=NULL;
            DotsC_MemberStatus status=m_blobLayout.GetMemberWithOptionalString(m_blob, memberIndex, arrayIndex, val, hashStr);
            if (!status.IsNull())
            {
                if (hashStr)
                {
                    arrayElementPt=&pt.add(elementName, hashStr);
                }
                else
                {
                    arrayElementPt=&pt.add(elementName, val);
                }
            }
        }
            break;

        case EntityIdMemberType:
        {
            DotsC_EntityId entId;
            const char* hashStr=0;
            DotsC_MemberStatus status=m_blobLayout.GetMemberWithOptionalString(m_blob, memberIndex, arrayIndex, entId, hashStr);
            if (!status.IsNull())
            {
                boost::property_tree::ptree eid;
                const char* typeName=TypeIdToString(entId.typeId);
                if (typeName)
                {
                    eid.add("name", typeName);
                }
                else
                {
                    eid.add("name", entId.typeId);
                }

                if (hashStr)
                {
                    eid.add("instanceId", hashStr);
                }
                else
                {
                    eid.add("instanceId", entId.instanceId);
                }

                arrayElementPt=&pt.add_child(elementName, eid);
            }
        }
            break;

        case StringMemberType:
        {
            const char* strVal=NULL;
            DotsC_Int32 size=0;
            DotsC_MemberStatus status=m_blobLayout.GetDynamicMember(m_blob, memberIndex, arrayIndex, strVal, size);            
            if (!status.IsNull())
            {

                arrayElementPt=&pt.add(elementName, strVal);
                arrayElementPt->add("<xmlattr>.xml:space", "preserve");
            }
        }
            break;

        case ObjectMemberType:
        {
            const char* obj=NULL;
            DotsC_Int32 size=0;
            DotsC_MemberStatus status=m_blobLayout.GetDynamicMember(m_blob, memberIndex, arrayIndex, obj, size);
            if (!status.IsNull())
            {
                boost::property_tree::ptree members; //Serialize without the root-element, only members
                BlobToXmlSerializer objParser(m_repository, obj);
                bool typesDiffer=m_blobLayout.GetTypeId(obj)!=md->GetTypeId();
                objParser.SerializeMembers(members, typesDiffer); //we only need to add typeAttribute if types are not same as declared in dou (differ by inheritance)
                pt.push_back(std::make_pair(elementName, members));
                arrayElementPt=&pt.back().second;
            }
        }
            break;

        case BinaryMemberType:
        {
            const char* binary=NULL;
            DotsC_Int32 size=0;
            DotsC_MemberStatus status=m_blobLayout.GetDynamicMember(m_blob, memberIndex, arrayIndex, binary, size);
            if (!status.IsNull())
            {
                std::string bin(binary, size);
                arrayElementPt=&pt.add(elementName, BasicTypes::ToBase64(bin));
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
            DotsC_MemberStatus status=m_blobLayout.GetMember<DotsC_Float32>(m_blob, memberIndex, arrayIndex, val);
            if (!status.IsNull())
            {
                arrayElementPt=&pt.add(elementName, val);
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
            DotsC_MemberStatus status=m_blobLayout.GetMember<DotsC_Float64>(m_blob, memberIndex, arrayIndex, val);
            if (!status.IsNull())
            {
                arrayElementPt=&pt.add(elementName, val);
            }
        }
            break;
        }

        if (arrayElementPt!=NULL && md->IsArray())
        {
            arrayElementPt->add("<xmlattr>.index", arrayIndex);
        }
    }

    const char* BlobToXmlSerializer::TypeIdToString(DotsC_TypeId tid) const
    {
        const ClassDescription* cd=m_repository->GetClass(tid);
        if (cd)
        {
            return cd->GetName();
        }

        const EnumDescription* ed=m_repository->GetEnum(tid);
        if (ed)
        {
            return ed->GetName();
        }

        const PropertyDescription* pd=m_repository->GetProperty(tid);
        if (pd)
        {
            return pd->GetName();
        }

        return NULL;
    }

    const ClassDescription* BlobToXmlSerializer::GetClass() const
    {
        TypeId typeId=m_blobLayout.GetTypeId(m_blob);
        const ClassDescription* cd=m_repository->GetClass(typeId);
        if (cd==NULL)
        {
            std::ostringstream os;
            os<<"Corrupt blob. Can't find type descriptor for blob with typeId="<<typeId;
            throw ParseError("Binary to XML error", os.str(), "", 300);
        }        
        return cd;
    }
}
}
}
} //end namespace Safir::Dob::Typesystem::Internal
