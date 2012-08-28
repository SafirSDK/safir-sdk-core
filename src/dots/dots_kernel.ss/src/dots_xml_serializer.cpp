/******************************************************************************
*
* Copyright Saab AB, 2004-2008 (http://www.safirsdk.com)
*
* Created by: Joel Ottosson / stjoot
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

#include "dots_xml_serializer.h"
#include "dots_blob_layout.h"
#include "dots_repository.h"
#include "dots_basic_types.h"
#include "dots_xml_elements.h"
#include "dots_base64_conversions.h"
#include <Safir/Utilities/Internal/LowLevelLogger.h>

//we need windows.h for the IsBadReadPtr call
#if defined _MSC_VER
#include <windows.h>
#endif
//#define LOG_CALLSTACK

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    //-----------------------------------
    // blob to xml serialization
    //-----------------------------------
    std::string BlobToXmlSerializer::Serialize(const char * const blobSource)
    {
#ifdef LOG_CALLSTACK
        lllout << __FUNCTION__ << std::endl;
#endif
        m_blobEnd = blobSource + BlobLayout::GetSize(blobSource);
        xml.clear();
        WriteObject(blobSource,true);
        return xml;
    }

    void BlobToXmlSerializer::WriteObject(const char * const blob, const bool includeNamespace)
    {
#ifdef LOG_CALLSTACK
        lllout << __FUNCTION__ << " inc = " << includeNamespace << std::endl;
#endif
        if (blob >= m_blobEnd)
        {
            WriteError("Passed end of blob!");
            lllout << "Passed end of blob by " << static_cast<long>(blob - m_blobEnd) << " bytes!!!!"<<std::endl;
            return;
        }

        if (includeNamespace)
        {
            xml+="<";
            xml+=XmlElements::OBJ;
            xml+=" xmlns=\"urn:safir-dots-unit\">";
        }
        else
        {
            WriteStartElement(XmlElements::OBJ);
        }

        if (blob == NULL)
        {
            WriteError("NULL blob!");
            WriteEndElement(XmlElements::OBJ);
            lllout
                << "dots_xml_serializer: Someone tried to serialize a null blob!" << std::endl;

            return;
        }

#if defined _MSC_VER
        //on windows we can perform this extra little check.
        if (IsBadReadPtr(blob,16))
        {
            WriteError("Bad blob ptr!");
            WriteEndElement(XmlElements::OBJ);
            lllout
                << "dots_xml_serializer: Someone tried to serialize a bad ptr blob!" << std::endl;

            return;
        }
#endif

        if (blob + BlobLayout::GetSize(blob) > m_blobEnd)
        {
            WriteError("Passed end of blob!");
            lllout << "Passed end of blob by " << static_cast<long>(blob + BlobLayout::GetSize(blob) - m_blobEnd) << " bytes (end of nested blob)!!!"<<std::endl;
            return;
        }

        const ClassDescription * cde=Repository::Classes().FindClass(BlobLayout::GetTypeId(blob));
        if (cde==NULL)
        {
            WriteError("Unknown class");
            WriteEndElement(XmlElements::OBJ);
            lllout
                << "dots_xml_serializer: Someone tried to serialize a strange blob:" << std::endl
                << "                     The blob pointer value = " << static_cast<const void*>(blob) << std::endl
                << "                     TypeId = " << BlobLayout::GetTypeId(blob) << std::endl;

            return;
        }
        WriteElement(XmlElements::NAME, cde->Name());

        bool anyUsedMember=false;
        size_t beforeMembersElement=xml.length();
        WriteStartElement(XmlElements::MEMBERS);

        const size_t noMembers=cde->NumberOfMembers();

        for (size_t i=0; i<noMembers; i++)
        {
            MemberIndex member=static_cast<MemberIndex>(i);
            const MemberDescription * memberDesc = cde->GetMember(member);

            //Check if there is anything to insert in this member
            bool anythingToInsert=false;
            for (Size i=0; i<memberDesc->ArrayLength(); i++)
            {
                if (MemberStatusHandler::ChangedOrNotNull(BlobLayout::GetStatus(blob, member, i)))
                {
                    anythingToInsert=true;
                    break;
                }
            }
            if (!anythingToInsert) //member is null, continue with next member
                continue;
            else
                anyUsedMember=true;

            bool indexElementNeeded=false;
            WriteStartElement(XmlElements::MEMBER);
            WriteElement(XmlElements::NAME, memberDesc->Name());
            bool isArr=false;
            if (memberDesc->ArrayLength()>1)
                isArr=true;
            if (isArr)
                WriteStartElement(XmlElements::ARRAY_ELEMENTS);
            for (Size i=0; i<memberDesc->ArrayLength(); i++)
            {
                bool used = false;
                size_t beforeArrayElement=xml.length();

                if (isArr)
                {
                    WriteStartElement(XmlElements::ARRAY_ELEMENT);
                    if (indexElementNeeded)
                    {
                        WriteStartElement(XmlElements::INDEX);
                        InsertInt(i);
                        WriteEndElement(XmlElements::INDEX);
                        indexElementNeeded=false;
                    }
                }
                switch(memberDesc->GetMemberType())
                {
                case BooleanMemberType:
                    used=WriteBooleanMember(memberDesc, blob, member, i);
                    break;
                case EnumerationMemberType:
                    used=WriteEnumMember(memberDesc, blob, member, i);
                    break;
                case Int32MemberType:
                    used=WriteInt32Member(memberDesc, blob, member, i);
                    break;
                case Int64MemberType:
                    used=WriteInt64Member(memberDesc, blob, member, i);
                    break;
                case Float32MemberType:
                    used=WriteFloat32Member(memberDesc, blob, member, i);
                    break;
                case Float64MemberType:
                    used=WriteFloat64Member(memberDesc, blob, member, i);
                    break;
                case TypeIdMemberType:
                    used=WriteTypeIdMember(memberDesc, blob, member, i);
                    break;
                case InstanceIdMemberType:
                case HandlerIdMemberType:
                case ChannelIdMemberType:
                    used = WriteHashedIdMember(memberDesc,blob,member,i);
                    break;
                case EntityIdMemberType:
                    used=WriteEntityIdMember(memberDesc, blob, member, i);
                    break;
                case StringMemberType:
                    used=WriteStringMember(memberDesc, blob, member, i);
                    break;
                case ObjectMemberType:
                    used=WriteObjectMember(memberDesc, blob, member, i);
                    break;
                case BinaryMemberType:
                    used=WriteBinaryMember(memberDesc, blob, member, i);
                    break;

                //SI Types
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
                    used=WriteFloat32Member(memberDesc, blob, member, i);
                    break;
                //SI Long Types
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
                    used=WriteFloat64Member(memberDesc, blob, member, i);
                    break;
                }
                if (isArr)
                {
                    if (used)
                    {
                        WriteEndElement(XmlElements::ARRAY_ELEMENT);
                    }
                    else //this element contained NULL-value. Roll back.
                    {
                        xml.erase(beforeArrayElement);
                        indexElementNeeded=true;
                    }
                }
            }
            if (isArr)
                WriteEndElement(XmlElements::ARRAY_ELEMENTS);
            WriteEndElement(XmlElements::MEMBER);
        }

        //end of object
        if (anyUsedMember)
        {
            WriteEndElement(XmlElements::MEMBERS);
        }
        else //no member written, roll-back
        {
             xml.erase(beforeMembersElement);
        }

        //End element of object
        WriteEndElement(XmlElements::OBJ);
    }

    void BlobToXmlSerializer::WriteTypeId(const TypeId tid)
    {
#ifdef LOG_CALLSTACK
        lllout << __FUNCTION__ << " arg = " << tid << std::endl;
#endif
        //class type
        const ClassDescription * cde=Repository::Classes().FindClass(tid);
        if (cde!=NULL)
        {
            xml+=cde->Name();
            return;
        }

        //property type
        const PropertyDescription * pde=Repository::Properties().FindProperty(tid);
        if (pde!=NULL)
        {
            xml+=pde->Name();
            return;
        }

        //enumeration type
        const EnumDescription* ede=Repository::Enums().FindEnum(tid);
        if (ede!=NULL)
        {
            xml+=ede->Name();
            return;
        }

        InsertI64(tid);

    }
/*
    void BlobToXmlSerializer::WriteEntityId(const DotsC_EntityId& oid)
    {
#ifdef LOG_CALLSTACK
        lllout << __FUNCTION__ << " arg = (" << oid.typeId << ", " << oid.instanceId << ")" << std::endl;
#endif
        const ClassDescription* cde=Repository::Classes().FindClass(oid.typeId);
        if (cde!=NULL)
        {
            WriteStartElement(XmlElements::OBJ_ID);
            WriteElement(XmlElements::NAME, cde->Name());
            WriteStartElement(XmlElements::INSTANCE);
            InsertInt(oid.instance);
            WriteEndElement(XmlElements::INSTANCE);
            WriteEndElement(XmlElements::OBJ_ID);
        }
        else
        {
            xml+="Unknown_Object_Id";
        }
    }
*/
    void BlobToXmlSerializer::WriteError(const char* msg)
    {
#ifdef LOG_CALLSTACK
        lllout << __FUNCTION__ << " arg = " << (msg!=NULL?msg:"NULL") << std::endl;
#endif
        WriteStartElement(XmlElements::ERR);
        xml+=msg;
        WriteEndElement(XmlElements::ERR);
    }

    //general
    void BlobToXmlSerializer::WriteStartElement(const char* element)
    {
#ifdef LOG_CALLSTACK
        lllout << __FUNCTION__ << " arg = " << (element!=NULL?element:"NULL") << std::endl;
#endif
        xml+="<";
        xml+=element;
        xml+=">";
    }

    void BlobToXmlSerializer::WriteEndElement(const char* element)
    {
#ifdef LOG_CALLSTACK
        lllout << __FUNCTION__ << " arg = " << (element!=NULL?element:"NULL") << std::endl;
#endif
        xml+="</";
        xml+=element;
        xml+=">";
    }

    void BlobToXmlSerializer::WriteElement(const char* element, const char* content)
    {
#ifdef LOG_CALLSTACK
        lllout << __FUNCTION__ << " arg = " << (element!=NULL?element:"NULL") << " , arg2 = " <<(content!=NULL?content:"NULL") << std::endl;
#endif
        WriteStartElement(element);
        xml+=content;
        WriteEndElement(element);
    }

    //conversion
    void BlobToXmlSerializer::InsertDouble(double t)
    {
#ifdef LOG_CALLSTACK
        lllout << __FUNCTION__;
#endif
        std::ostringstream str;
        str.precision(20);
        str << t;
        xml += str.str();

#ifdef LOG_CALLSTACK
        lllout << " arg = " << str.str().c_str() << std::endl;
#endif

        /*
        char buf[50];
        sprintf(buf, "%0.15f", t);
        xml+=buf;*/
    }

    void BlobToXmlSerializer::InsertFloat(float t)
    {
#ifdef LOG_CALLSTACK
        lllout << __FUNCTION__ << " arg = " << t << std::endl;
#endif
        std::ostringstream str;
        str.precision(10);
        str << t;
        xml += str.str();
        /*
        char buf[50];
        sprintf(buf, "%0.6f", t);
        xml+=buf;*/
    }

    void BlobToXmlSerializer::InsertInt(const Int32 t)
    {
#ifdef LOG_CALLSTACK
        lllout << __FUNCTION__ << " arg = " << t << std::endl;
#endif
/*        char buf[50];
        sprintf(buf, "%i", t);
        xml+=buf;*/
        std::ostringstream str;
        str << t;
        xml += str.str();
        //xml += boost::lexical_cast<std::string>(t);
    }

    void BlobToXmlSerializer::InsertI64(const Int64 t)
    {
#ifdef LOG_CALLSTACK
        lllout << __FUNCTION__ << " arg = " << t << std::endl;
#endif
        /*char buf[50];
        sprintf(buf, "%I64d", t);
        xml+=buf;*/
        std::ostringstream str;
        str << t;
        xml += str.str();
        //xml += boost::lexical_cast<std::string>(t);
    }

    void BlobToXmlSerializer::InsertUI64(const UInt64 t)
    {
#ifdef LOG_CALLSTACK
        lllout << __FUNCTION__ << " arg = " << t << std::endl;
#endif
        /*char buf[50];
        sprintf(buf, "%I64u", t);
        xml+=buf;*/
        std::ostringstream str;
        str << t;
        xml += str.str();
        //xml += boost::lexical_cast<std::string>(t);
    }

    void BlobToXmlSerializer::InsertBool(const bool b)
    {
#ifdef LOG_CALLSTACK
        lllout << __FUNCTION__ << " arg = " << b << std::endl;
#endif
        if (b) xml+="true";
        else  xml+="false";
    }

    bool BlobToXmlSerializer::WriteBooleanMember(const MemberDescription * /*memberDesc*/, const char * const blob, MemberIndex member, int ix)
    {
#ifdef LOG_CALLSTACK
        lllout << __FUNCTION__ << " arg = " << member << ", " << ix << std::endl;
#endif
        bool val;
        InternalMemberStatus status=BlobLayout::GetMember<bool>(blob, member, ix, val);
        if (!MemberStatusHandler::IsNull(status))
        {
            WriteStartElement(XmlElements::VALUE);
            InsertBool(val);
            WriteEndElement(XmlElements::VALUE);
        }
        return MemberStatusHandler::ChangedOrNotNull(status);
    }

    bool BlobToXmlSerializer::WriteInt32Member(const MemberDescription * /*memberDesc*/, const char * const blob, MemberIndex member, int ix)
    {
#ifdef LOG_CALLSTACK
        lllout << __FUNCTION__ << " arg = " << member << ", " << ix << std::endl;
#endif
        Int32 val;
        InternalMemberStatus status=BlobLayout::GetMember<Int32>(blob, member, ix, val);
        if (!MemberStatusHandler::IsNull(status))
        {
            WriteStartElement(XmlElements::VALUE);
            InsertInt(val);
            WriteEndElement(XmlElements::VALUE);
        }
        return MemberStatusHandler::ChangedOrNotNull(status);
    }

    bool BlobToXmlSerializer::WriteInt64Member(const MemberDescription * /*memberDesc*/, const char * const blob, MemberIndex member, int ix)
    {
#ifdef LOG_CALLSTACK
        lllout << __FUNCTION__ << " arg = " << member << ", " << ix << std::endl;
#endif
        Int64 val;
        InternalMemberStatus status=BlobLayout::GetMember<Int64>(blob, member, ix, val);
        if (!MemberStatusHandler::IsNull(status))
        {
            WriteStartElement(XmlElements::VALUE);
            InsertI64(val);
            WriteEndElement(XmlElements::VALUE);
        }
        return MemberStatusHandler::ChangedOrNotNull(status);
    }

    bool BlobToXmlSerializer::WriteFloat32Member(const MemberDescription * /*memberDesc*/, const char * const blob, MemberIndex member, int ix)
    {
#ifdef LOG_CALLSTACK
        lllout << __FUNCTION__ << " arg = " << member << ", " << ix << std::endl;
#endif
        Float32 val;
        InternalMemberStatus status=BlobLayout::GetMember<Float32>(blob, member, ix, val);
        if (!MemberStatusHandler::IsNull(status))
        {
            WriteStartElement(XmlElements::VALUE);
            InsertFloat(val);
            WriteEndElement(XmlElements::VALUE);
        }
        return MemberStatusHandler::ChangedOrNotNull(status);
    }

    bool BlobToXmlSerializer::WriteFloat64Member(const MemberDescription * /*memberDesc*/, const char * const blob, MemberIndex member, int ix)
    {
#ifdef LOG_CALLSTACK
        lllout << __FUNCTION__ << " arg = " << member << ", " << ix << std::endl;
#endif
        Float64 val;
        InternalMemberStatus status=BlobLayout::GetMember<Float64>(blob, member, ix, val);
        if (!MemberStatusHandler::IsNull(status))
        {
            WriteStartElement(XmlElements::VALUE);
            InsertDouble(val);
            WriteEndElement(XmlElements::VALUE);
        }
        return MemberStatusHandler::ChangedOrNotNull(status);
    }

    bool BlobToXmlSerializer::WriteTypeIdMember(const MemberDescription * /*memberDesc*/, const char * const blob, MemberIndex member, int ix)
    {
#ifdef LOG_CALLSTACK
        lllout << __FUNCTION__ << " arg = " << member << ", " << ix << std::endl;
#endif
        TypeId val;
        InternalMemberStatus status=BlobLayout::GetMember<TypeId>(blob, member, ix, val);
        if (!MemberStatusHandler::IsNull(status))
        {
            WriteStartElement(XmlElements::VALUE);
            WriteTypeId(val);
            WriteEndElement(XmlElements::VALUE);
        }
        return MemberStatusHandler::ChangedOrNotNull(status);
    }

    bool BlobToXmlSerializer::WriteHashedIdMember(const MemberDescription * /*memberDesc*/, const char * const blob, MemberIndex member, int ix)
    {
#ifdef LOG_CALLSTACK
        lllout << __FUNCTION__ << " arg = " << member << ", " << ix << std::endl;
#endif
        Int64 hashVal;
        const char * strVal = NULL;
        InternalMemberStatus status=BlobLayout::GetMemberWithOptionalString(blob, member, ix, hashVal, strVal);
        if (!MemberStatusHandler::IsNull(status))
        {
            if (strVal != NULL)
            {
                WriteElement(XmlElements::VALUE, strVal);
            }
            else
            {
                WriteElement(XmlElements::VALUE, boost::lexical_cast<std::string>(hashVal).c_str());
            }
        }
        return MemberStatusHandler::ChangedOrNotNull(status);
    }

    bool BlobToXmlSerializer::WriteEntityIdMember(const MemberDescription * /*memberDesc*/, const char * const blob, MemberIndex member, int ix)
    {
#ifdef LOG_CALLSTACK
        lllout << __FUNCTION__ << " arg = " << member << ", " << ix << std::endl;
#endif
        DotsC_EntityId val;
        const char * instanceIdStr;
        InternalMemberStatus status=BlobLayout::GetMemberWithOptionalString(blob, member, ix, val, instanceIdStr);
        if (!MemberStatusHandler::IsNull(status))
        {
            const ClassDescription* cde=Repository::Classes().FindClass(val.typeId);

            WriteStartElement(XmlElements::ENTITY_ID);
            if (cde!=NULL)
            {
                WriteElement(XmlElements::NAME, cde->Name());
            }
            else
            {
                WriteElement(XmlElements::NAME, boost::lexical_cast<std::string>(val.typeId).c_str());
            }
            if (instanceIdStr != NULL)
            {
                WriteElement(XmlElements::INSTANCE_ID, instanceIdStr);
            }
            else
            {
                WriteElement(XmlElements::INSTANCE_ID, boost::lexical_cast<std::string>(val.instanceId).c_str());
            }
            WriteEndElement(XmlElements::ENTITY_ID);
        }
        return MemberStatusHandler::ChangedOrNotNull(status);
    }

    bool BlobToXmlSerializer::WriteStringMember(const MemberDescription * /*memberDesc*/, const char * const blob, MemberIndex member, int ix)
    {
#ifdef LOG_CALLSTACK
        lllout << __FUNCTION__ << " arg = " << member << ", " << ix << std::endl;
#endif
        Int32 dummy=0;
        const char* str;
        InternalMemberStatus status=BlobLayout::GetDynamicMember(blob, member, ix, str, dummy);
        if (!MemberStatusHandler::IsNull(status))
        {
            if (strlen(str) == 0)
            {
                xml.append("<");
                xml.append(XmlElements::VALUE);
                xml.append("/>");
            }
            else
            {
                xml.append("<");
                xml.append(XmlElements::VALUE);
                xml.append(" xml:space=\"preserve\">");
                //                WriteStartElement(XmlElements::VALUE);
                if (str != NULL)
                {
                    while (*str != 0) //keep going until we hit the null termination
                    {
                        //Perform replacement of characters that can't be in xml.
                        switch (*str)
                        {
                        case '&':
                            xml.append("&amp;");
                            break;
                        case '<':
                            xml.append("&lt;");
                            break;
                        case '>':
                            xml.append("&gt;");
                            break;
                        case '"':
                            xml.append("&quot;");
                            break;
                        case '\'':
                            xml.append("&apos;");
                            break;
                        default:
                            xml += *str;
                            break;
                        }

                        ++str;
                    }
                }
                else
                {
                    WriteError("NULL string!");
                }
                WriteEndElement(XmlElements::VALUE);
            }
        }
        return MemberStatusHandler::ChangedOrNotNull(status);
    }

    bool BlobToXmlSerializer::WriteObjectMember(const MemberDescription * /*memberDesc*/, const char * const blob, MemberIndex member, int ix)
    {
#ifdef LOG_CALLSTACK
        lllout << __FUNCTION__ << " arg = " << member << ", " << ix << std::endl;
#endif

        Int32 dummy=0;
        const char * obj;
        InternalMemberStatus status=BlobLayout::GetDynamicMember(blob, member, ix, const_cast<const char*&>(obj), dummy);
        if (!MemberStatusHandler::IsNull(status))
        {
            WriteObject(obj);
        }
        return MemberStatusHandler::ChangedOrNotNull(status);
    }

    bool BlobToXmlSerializer::WriteBinaryMember(const MemberDescription * /*memberDesc*/, const char * const blob, MemberIndex member, int ix)
    {
#ifdef LOG_CALLSTACK
        lllout << __FUNCTION__ << " arg = " << member << ", " << ix << std::endl;
#endif

        Int32 size;
        char* binary;
        InternalMemberStatus status=BlobLayout::GetDynamicMember(blob, member, ix, const_cast<const char*&>(binary), size);
        if (!MemberStatusHandler::IsNull(status))
        {
            if (size == 0)
            {
                xml.append("<");
                xml.append(XmlElements::VALUE);
                xml.append("/>");
            }
            else
            {
                //get required buffer size
                std::vector<char> b64Dest(Base64Conversions::CalculateBase64Size(size));
                Int32 resultSize;
                Base64Conversions::ToBase64(&b64Dest[0], static_cast<Int32>(b64Dest.size()), binary, size, resultSize);
                if(static_cast<size_t>(resultSize) != b64Dest.size())
                {
                    throw InternalException("Invalid lengths from Base64Conversions::ToBase64, probably a programming error in dots_kernel",__FILE__,__LINE__);
                }
                WriteStartElement(XmlElements::VALUE);
                xml.append("\n");
                xml.append(b64Dest.begin(),b64Dest.end());
                xml.append("\n");
                WriteEndElement(XmlElements::VALUE);
            }
        }
        return MemberStatusHandler::ChangedOrNotNull(status);
    }

    bool BlobToXmlSerializer::WriteEnumMember(const MemberDescription * memberDesc, const char * const blob, MemberIndex member, int ix)
    {
#ifdef LOG_CALLSTACK
        lllout << __FUNCTION__ << " arg = " << member << ", " << ix << std::endl;
#endif
        Safir::Dob::Typesystem::Internal::EnumInternal val;
        InternalMemberStatus status=BlobLayout::GetMember<Safir::Dob::Typesystem::Internal::EnumInternal>(blob, member, ix, val);
        if (!MemberStatusHandler::IsNull(status))
        {
            WriteStartElement(XmlElements::VALUE);
            if (val < 0 || static_cast<Size>(val) >= Repository::Enums().FindEnum(memberDesc->GetTypeId())->NumberOfValues())
            {
                WriteError((std::string("Illegal enum value ") + boost::lexical_cast<std::string>(val)).c_str());
            }
            else if (memberDesc != NULL)
            {
                xml+=memberDesc->Enum()->Name();
                xml+=".";
                xml+=memberDesc->Enum()->ValueName(val);
            }
            else
            {
                WriteError("NULL MemberDescription!");
            }
            WriteEndElement(XmlElements::VALUE);
        }
        return MemberStatusHandler::ChangedOrNotNull(status);
    }
}
}
}
}
