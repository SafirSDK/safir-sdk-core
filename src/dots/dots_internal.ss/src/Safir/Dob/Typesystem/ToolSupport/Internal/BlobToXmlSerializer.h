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
            ,m_blobLayout(repository)
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
        const BlobLayoutImpl<RepositoryType> m_blobLayout;

        void SerializeMembers(const char* blob, std::ostream& os) const
        {
            const ClassDescriptionType* cd=GetClass(blob);

            for (DotsC_MemberIndex memberIx=0; memberIx<cd->GetNumberOfMembers(); ++memberIx)
            {
                const MemberDescriptionType* md=cd->GetMember(memberIx);
                if (md->GetCollectionType()!=ArrayCollectionType) //normal member
                {
                    SerializeMember(blob, md, memberIx, 0, md->GetName(), os);
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
                        typeName=BasicTypeOperations::MemberTypeToString(mt).c_str();
                        break;
                    }

                    //find first non-null value in array
                    DotsC_ArrayIndex arrIx=0;
                    while (arrIx<md->GetArraySize())
                    {
                        MemberStatus status=m_blobLayout.GetStatus(blob, memberIx, arrIx);
                        if (!status.IsNull())
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
                            SerializeMember(blob, md, memberIx, arrIx, typeName, os);
                        }
                        os<<"</"<<md->GetName()<<">";
                    }
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

        void SerializeMember(const char* blob,
                             const MemberDescriptionType* md,
                             DotsC_MemberIndex memberIndex,
                             DotsC_ArrayIndex arrayIndex,
                             const char* elementName,
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
                    WriteStartElement(elementName, arrayIndex, md->GetCollectionType()==ArrayCollectionType, os);
                    os<<(val ? "true" : "false");
                    os<<"</"<<elementName<<">";
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
                    WriteStartElement(elementName, arrayIndex, md->GetCollectionType()==ArrayCollectionType, os);
                    os<<enumVal;
                    os<<"</"<<elementName<<">";
                }
            }
                break;

            case Int32MemberType:
            {
                DotsC_Int32 val=0;
                MemberStatus status=m_blobLayout.template GetMember<DotsC_Int32>(blob, memberIndex, arrayIndex, val);
                if (!status.IsNull())
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
                MemberStatus status=m_blobLayout.template GetMember<DotsC_Int64>(blob, memberIndex, arrayIndex, val);
                if (!status.IsNull())
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
                MemberStatus status=m_blobLayout.template GetMember<DotsC_Int64>(blob, memberIndex, arrayIndex, val);
                if (!status.IsNull())
                {
                    WriteStartElement(elementName, arrayIndex, md->GetCollectionType()==ArrayCollectionType, os);

                    const char* typeName=TypeIdToString(val);
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
                DotsC_Int64 val=0;
                const char* hashStr=NULL;
                MemberStatus status=m_blobLayout.GetMemberWithOptionalString(blob, memberIndex, arrayIndex, val, hashStr);
                if (!status.IsNull())
                {
                    WriteStartElement(elementName, arrayIndex, md->GetCollectionType()==ArrayCollectionType, os);
                    if (hashStr)
                    {
                        os<<hashStr;
                    }
                    else
                    {
                        os<<val;
                    }
                    os<<"</"<<elementName<<">";
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
                    WriteStartElement(elementName, arrayIndex, md->GetCollectionType()==ArrayCollectionType, os);

                    const char* typeName=TypeIdToString(entId.typeId);
                    if (typeName)
                    {
                        os<<"<name>"<<typeName<<"</name>";
                    }
                    else
                    {
                        os<<"<name>"<<entId.typeId<<"</name>";
                    }

                    if (hashStr)
                    {
                        os<<"<instanceId>"<<hashStr<<"</instanceId>";
                    }
                    else
                    {
                        os<<"<instanceId>"<<entId.instanceId<<"</instanceId>";
                    }

                    os<<"</"<<elementName<<">";
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
                    if (md->GetCollectionType()!=ArrayCollectionType)
                    {
                        os<<"<"<<elementName<<" xml:space=\"preserve\">";
                    }
                    else
                    {
                        os<<"<"<<elementName<<" index=\""<<arrayIndex<<"\" xml:space=\"preserve\">";
                    }
                    WriteString(strVal, os);
                    os<<"</"<<elementName<<">";
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
                    bool typesDiffer=m_blobLayout.GetTypeId(obj)!=md->GetTypeId();

                    if (typesDiffer) //we only need to add typeAttribute if types are not same as declared in dou (differ by inheritance)
                    {
                        os<<"<"<<elementName<<" type=\""<<GetClass(obj)->GetName()<<"\"";
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

                    SerializeMembers(obj, os);
                    os<<"</"<<elementName<<">";
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
                    WriteStartElement(elementName, arrayIndex, md->GetCollectionType()==ArrayCollectionType, os);
                    std::string bin(binary, size);
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
                MemberStatus status=m_blobLayout.template GetMember<DotsC_Float32>(blob, memberIndex, arrayIndex, val);
                if (!status.IsNull())
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
                MemberStatus status=m_blobLayout.template GetMember<DotsC_Float64>(blob, memberIndex, arrayIndex, val);
                if (!status.IsNull())
                {
                    WriteStartElement(elementName, arrayIndex, md->GetCollectionType()==ArrayCollectionType, os);
                    os<<classic_string_cast<std::string>(val);
                    os<<"</"<<elementName<<">";
                }
            }
                break;
            }
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
