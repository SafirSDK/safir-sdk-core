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

#include "dots_blob_serializer.h"
#include <stack>
#include <string>
#include <Safir/Dob/Typesystem/Internal/Id.h>
#include <expat.h>
#include "dots_blob_layout.h"
#include "dots_basic_types.h"
#include "dots_repository.h"
#include "dots_class_description.h"
#include "dots_error_handler.h"
#include "dots_xml_elements.h"
#include "dots_base64_conversions.h"

#include <iostream>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    //fwd decl
    static void HandleCharacters(const std::string & str);

    XmlToBlobSerializer::XmlToBlobSerializer()
    {
    }

    XmlToBlobSerializer::~XmlToBlobSerializer()
    {
    }

    const ParameterDescription * GetParameterByName(const std::string & name)
    {
        const size_t ix=name.find_last_of(".");
        const std::string className=name.substr(0, ix);
        const std::string paramName=name.substr(ix+1);
        const TypeId classId=DotsId_Generate64(className.c_str());
        const ClassDescription * const cd = Repository::Classes().FindClass(classId);
        if (cd == NULL)
        {
            std::string desc="Could not resolve parameter ";
            desc+=name;
            desc += ".\n Class ";
            desc+=className;
            desc += " was not found";
            ErrorHandler::Error("File parser error", desc, "dots_file_parser");
            return NULL;
        }
        /*        const int noParams=cd->NumberOfParameters();

                  for (int i=noParams-1; i>=0; i--) //we probably find it faster if we search backwards
                  {
                  const ParameterDescription * const pde = cd->GetParameter(i);
                  if (paramName == pde->Name()) //eq(pde->Name(), paramName.c_str()))
                  {
                  return pde;
                  }
                  }*/
        return cd->GetParameter(cd->GetParameterIndexFromName(paramName));
    }

    int ParameterIndexInternal(const std::string & name)
    {
        if (IsInt(name.c_str()))
        {
            return atoi(name.c_str());
        }

        const size_t arrStart=name.find_first_of('[');
        const size_t arrEnd=name.find_last_of(']');
        std::string param;
        int index=0;

        if (arrStart != std::string::npos && arrEnd != std::string::npos) //has index
        {
            param = name.substr(0, arrStart);
            index = ParameterIndexInternal(name.substr(arrStart+1, arrEnd-arrStart-1));
        }
        else
        {
            param=name;
        }

        return *GetParameterByName(param)->Value<Int32>(index);
    }

    void ResolveParameter(const std::string & text,
                          const ParameterDescription * & pde,
                          int & index)
    {
        size_t arrStart=text.find_first_of('[');
        size_t arrEnd=text.find_last_of(']');
        std::string name=text;
        index=0;
        if (arrStart!=std::string::npos && arrEnd!=std::string::npos)
        {
            try
            {
                std::string arrIx=text.substr(arrStart+1, arrEnd-arrStart-1);
                name=name.substr(0, arrStart);
                index=ParameterIndexInternal(arrIx);
            }
            catch(...)
            {
                std::string desc="Invalid array index for parameter. ";
                desc+=text;
                ErrorHandler::Error("File parser error", desc, "dots_file_parser");
                throw desc.c_str();
            }
        }
        pde=GetParameterByName(name);
    }


    class ParseInfo
    {
    public:
        const char* expectedElement;
        std::stack <const char*> elem;
        std::string ref;
        TypeId typeId;
        char * blob;
        bool haveDescription;
        const ClassDescription * cde;
        const MemberDescription* mde;
        MemberIndex member;
        int index;
        bool done;
        DotsC_EntityId entityId;
        std::string instanceIdStr;
        std::string base64Binary;
        std::string characterData;
        int insideRef;
        bool insideMember;
        bool insideArray;
        bool insideEntityId;

        ParseInfo():
            expectedElement(XmlElements::OBJ),
            elem(),
            ref(),
            blob(NULL),
            haveDescription(false),
            cde(NULL),
            mde(NULL),
            member(0),
            index(0),
            done(false),
            base64Binary(),
            insideRef(0),
            insideMember(false),
            insideArray(false),
            insideEntityId(false) {}
    };

    //*******************************************************
    //  This bit is stupid leftovers from stupid implementation
    //  of the expat interfaces. It shouldnt be that difficult
    //  to make a c++-like wrapper for expat.
    //  all the members of the struct below used to be statics
    //  but since initiation order of statics are not guaranteed
    //  they have been moved into a struct-singleton.
    //  The namespace is so that the class definitions dont collide
    //  with the one in dots_file_parser.cpp
    // I curse whoever wrote this code originally :-)
    //*******************************************************

    namespace StateHolder
    {
        struct ParsingState
        {
            static ParsingState & Instance();

            XML_Parser parser;
            bool parseError;
            std::stack <ParseInfo> objects;
            std::stack<bool> preserveSpace;
            char * theBlob;

            void Reset()
            {
                while (!objects.empty())
                {
                    objects.pop();
                }

                ParsingState::Instance().parseError=false;
                objects.push(ParseInfo());
            }

        };
    }
    using namespace StateHolder;

    ParsingState & ParsingState::Instance()
    {
        static ParsingState inst;
        return inst;
    }

    static const char* ValidElement(const char* name)
    {
        if (eq(name, XmlElements::OBJ)) return XmlElements::OBJ;
        if (eq(name, XmlElements::NAME)) return XmlElements::NAME;
        if (eq(name, XmlElements::INSTANCE_ID)) return XmlElements::INSTANCE_ID;
        if (eq(name, XmlElements::INDEX)) return XmlElements::INDEX;
        if (eq(name, XmlElements::INDEX_REF)) return XmlElements::INDEX_REF;
        if (eq(name, XmlElements::MEMBERS)) return XmlElements::MEMBERS;
        if (eq(name, XmlElements::MEMBER)) return XmlElements::MEMBER;
        if (eq(name, XmlElements::VALUE)) return XmlElements::VALUE;
        if (eq(name, XmlElements::VALUE_REF)) return XmlElements::VALUE_REF;
        if (eq(name, XmlElements::ENTITY_ID)) return XmlElements::ENTITY_ID;
        if (eq(name, XmlElements::ARRAY_ELEMENTS)) return XmlElements::ARRAY_ELEMENTS;
        if (eq(name, XmlElements::ARRAY_ELEMENT)) return XmlElements::ARRAY_ELEMENT;
        return NULL;
    }




    static void HandleValue(const char* val)
    {
        Int32 dummy=0;

        BlobLayout::SetStatus(false, true,
                              ParsingState::Instance().objects.top().blob,
                              ParsingState::Instance().objects.top().member,
                              ParsingState::Instance().objects.top().index);

        switch(ParsingState::Instance().objects.top().mde->GetMemberType())
        {
        case BooleanMemberType:
            {
                bool b;
                if (eq(val, "true") || eq(val, "TRUE") || eq(val, "True"))
                    b=true;
                else if (eq(val, "false") || eq(val, "FALSE") || eq(val, "False"))
                    b=false;
                else
                {
                    XML_StopParser(ParsingState::Instance().parser, false);
                    ParsingState::Instance().parseError=true;
                    std::string descr="Invalid bool value, should be 'true' or 'false', but found '";
                    descr+=val;
                    descr+="'.";
                    ErrorHandler::Error("Serialization error", descr, "dots_blob_serializer");
                    return;
                }
                BlobLayout::SetMember<bool>(b,
                                            ParsingState::Instance().objects.top().blob,
                                            ParsingState::Instance().objects.top().member,
                                            ParsingState::Instance().objects.top().index);
                return;
            }

        case EnumerationMemberType:
            {
                std::string en=val;
                size_t ix=en.find_last_of('.');
                std::string enumName=en.substr(0, ix);
                std::string enumVal=en.substr(ix+1);

                TypeId tid=DotsId_Generate64(enumName.c_str());
                const EnumDescription * ed = Repository::Enums().FindEnum(tid);
                if (ed!=NULL)
                {
                    Int32 enumIndex = ed->IndexOf(enumVal.c_str());
                    if (enumIndex == -1)
                    {
                        XML_StopParser(ParsingState::Instance().parser, false);
                        ParsingState::Instance().parseError=true;
                        std::string descr="Could not find enumeration value '";
                        descr+=enumVal;
                        descr+="'.";
                        ErrorHandler::Error("Serialization error", descr, "dots_blob_serializer");
                    }
                    EnumInternal eint=static_cast<EnumInternal>(enumIndex);
                    BlobLayout::SetMember<EnumInternal>(eint,
                                                        ParsingState::Instance().objects.top().blob,
                                                        ParsingState::Instance().objects.top().member,
                                                        ParsingState::Instance().objects.top().index);
                }
                else
                {
                    XML_StopParser(ParsingState::Instance().parser, false);
                    ParsingState::Instance().parseError=true;
                    std::string descr="Could not find enumeration type '";
                    descr+=enumName;
                    descr+="'.";
                    ErrorHandler::Error("Serialization error", descr, "dots_blob_serializer");
                }

                return;
            }

        case Int32MemberType:
            {
                if (IsInt(val))
                {
                    BlobLayout::SetMember<Int32>(atol(val),
                                                 ParsingState::Instance().objects.top().blob,
                                                 ParsingState::Instance().objects.top().member,
                                                 ParsingState::Instance().objects.top().index);
                }
                else
                {
                    XML_StopParser(ParsingState::Instance().parser, false);
                    ParsingState::Instance().parseError=true;
                    std::string descr="Invalid numeric value '";
                    descr+=val;
                    descr+="'.";
                    ErrorHandler::Error("Serialization error", descr, "dots_blob_serializer");
                }
                return;
            }

        case Int64MemberType:
            {
                if (IsInt(val))
                {
                    BlobLayout::SetMember<Int64>(boost::lexical_cast<Int64>(val),
                                                 ParsingState::Instance().objects.top().blob,
                                                 ParsingState::Instance().objects.top().member,
                                                 ParsingState::Instance().objects.top().index);
                }
                else
                {
                    XML_StopParser(ParsingState::Instance().parser, false);
                    ParsingState::Instance().parseError=true;
                    std::string descr="Invalid numeric value '";
                    descr+=val;
                    descr+="'.";
                    ErrorHandler::Error("Serialization error", descr, "dots_blob_serializer");
                }
                return;
            }

        case Float32MemberType:
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
            {
                if (IsFloat(val))
                {
                    BlobLayout::SetMember<Float32>(static_cast<Float32>(atof(val)),
                                                   ParsingState::Instance().objects.top().blob,
                                                   ParsingState::Instance().objects.top().member,
                                                   ParsingState::Instance().objects.top().index);
                }
                else
                {
                    XML_StopParser(ParsingState::Instance().parser, false);
                    ParsingState::Instance().parseError=true;
                    std::string descr="Invalid numeric value '";
                    descr+=val;
                    descr+="'.";
                    ErrorHandler::Error("Serialization error", descr, "dots_blob_serializer");
                }
                return;
            }

        case Float64MemberType:
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
            {
                if (IsFloat(val))
                {
                    BlobLayout::SetMember<Float64>(atof(val),
                                                   ParsingState::Instance().objects.top().blob,
                                                   ParsingState::Instance().objects.top().member,
                                                   ParsingState::Instance().objects.top().index);
                }
                else
                {
                    XML_StopParser(ParsingState::Instance().parser, false);
                    ParsingState::Instance().parseError=true;
                    std::string descr="Invalid numeric value '";
                    descr+=val;
                    descr+="'.";
                    ErrorHandler::Error("Serialization error", descr, "dots_blob_serializer");
                }
                return;
            }

        case TypeIdMemberType:
            {
                TypeId i;
                if (XmlToBlobSerializer::ToTypeId(val, i))
                {
                    BlobLayout::SetMember<TypeId>(i,
                                                  ParsingState::Instance().objects.top().blob,
                                                  ParsingState::Instance().objects.top().member,
                                                  ParsingState::Instance().objects.top().index);
                }
                else
                {
                    XML_StopParser(ParsingState::Instance().parser, false);
                    ParsingState::Instance().parseError=true;
                    std::string descr="Invalid TypeId value '";
                    descr+=val;
                    descr+="'.";
                    ErrorHandler::Error("Serialization error", descr, "dots_blob_serializer");
                }
                return;
            }
        case StringMemberType:
            {
                BlobLayout::SetDynamicMember(val, dummy,
                                             ParsingState::Instance().objects.top().blob,
                                             ParsingState::Instance().objects.top().member,
                                             ParsingState::Instance().objects.top().index);
                return;
            }
        case BinaryMemberType:
            {
                //store the entire base64 string. Converted in EndElement-handler, element=value
                ParsingState::Instance().objects.top().base64Binary+=val;
                return;
            }

        case InstanceIdMemberType:
        case ChannelIdMemberType:
        case HandlerIdMemberType:
            {
                if (IsInt(val))
                {
                    BlobLayout::SetMemberWithOptionalString(boost::lexical_cast<Int64>(val),
                                                            NULL,
                                                            ParsingState::Instance().objects.top().blob,
                                                            ParsingState::Instance().objects.top().member,
                                                            ParsingState::Instance().objects.top().index);
                }
                else
                {
                    BlobLayout::SetMemberWithOptionalString(DotsId_Generate64(val),
                                                            val,
                                                            ParsingState::Instance().objects.top().blob,
                                                            ParsingState::Instance().objects.top().member,
                                                            ParsingState::Instance().objects.top().index);
                }
                return;
            }
            break;

        case ObjectMemberType:
        case EntityIdMemberType:
            XML_StopParser(ParsingState::Instance().parser, false);
            ParsingState::Instance().parseError=true;
            std::string descr="Internal error. Got to HandleValue with unexpected memberType";
            ErrorHandler::Error("Serialization error", descr, "dots_blob_serializer");//, "Dots_Fatal_Error");
            return;
        }
    }

    static void HandleRefValue(const ParameterDescription * pde, int index)
    {
        BlobLayout::SetStatus(false, true,
                              ParsingState::Instance().objects.top().blob,
                              ParsingState::Instance().objects.top().member,
                              ParsingState::Instance().objects.top().index);

        switch(ParsingState::Instance().objects.top().mde->GetMemberType())
        {
        case BooleanMemberType:
            {
                BlobLayout::SetMember<bool>(*(pde->Value<bool>(index)),
                                            ParsingState::Instance().objects.top().blob,
                                            ParsingState::Instance().objects.top().member,
                                            ParsingState::Instance().objects.top().index);
                return;
            }

        case EnumerationMemberType:
            {
                BlobLayout::SetMember<EnumInternal>(*(pde->Value<EnumInternal>(index)),
                                                    ParsingState::Instance().objects.top().blob,
                                                    ParsingState::Instance().objects.top().member,
                                                    ParsingState::Instance().objects.top().index);
                return;
            }

        case Int32MemberType:
            {
                BlobLayout::SetMember<Int32>(*(pde->Value<Int32>(index)),
                                             ParsingState::Instance().objects.top().blob,
                                             ParsingState::Instance().objects.top().member,
                                             ParsingState::Instance().objects.top().index);
                return;
            }

        case Int64MemberType:
            {
                BlobLayout::SetMember<Int64>(*(pde->Value<Int64>(index)),
                                             ParsingState::Instance().objects.top().blob,
                                             ParsingState::Instance().objects.top().member,
                                             ParsingState::Instance().objects.top().index);
                return;
            }

        case Float32MemberType:
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
            {
                BlobLayout::SetMember<Float32>(*(pde->Value<Float32>(index)),
                                               ParsingState::Instance().objects.top().blob,
                                               ParsingState::Instance().objects.top().member,
                                               ParsingState::Instance().objects.top().index);
                return;
            }

        case Float64MemberType:
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
            {
                BlobLayout::SetMember<Float64>(*(pde->Value<Float64>(index)),
                                               ParsingState::Instance().objects.top().blob,
                                               ParsingState::Instance().objects.top().member,
                                               ParsingState::Instance().objects.top().index);
                return;
            }

        case TypeIdMemberType:
            {
                BlobLayout::SetMember<TypeId>(*(pde->Value<TypeId>(index)),
                                              ParsingState::Instance().objects.top().blob,
                                              ParsingState::Instance().objects.top().member,
                                              ParsingState::Instance().objects.top().index);
                return;
            }

        case InstanceIdMemberType:
        case ChannelIdMemberType:
        case HandlerIdMemberType:
            {
                Int64 hashVal;
                const char * strVal;
                pde->ValueWithOptionalString(index,hashVal,strVal);
                BlobLayout::SetMemberWithOptionalString(hashVal,strVal,
                                                        ParsingState::Instance().objects.top().blob,
                                                        ParsingState::Instance().objects.top().member,
                                                        ParsingState::Instance().objects.top().index);
                return;
            }

        case EntityIdMemberType:
            {
                DotsC_EntityId val;
                const char * instanceIdStr;
                pde->ValueWithOptionalString(index,val,instanceIdStr);
                BlobLayout::SetMemberWithOptionalString(val,instanceIdStr,
                                                        ParsingState::Instance().objects.top().blob,
                                                        ParsingState::Instance().objects.top().member,
                                                        ParsingState::Instance().objects.top().index);
                return;
            }

        case StringMemberType:
        case ObjectMemberType:
            {
                BlobLayout::SetDynamicMember(pde->Value<char>(index).get(), 0,
                                             ParsingState::Instance().objects.top().blob,
                                             ParsingState::Instance().objects.top().member,
                                             ParsingState::Instance().objects.top().index);
                return;
            }
        case BinaryMemberType:
            {
                const ParameterDescription::BinaryParameterValue value = pde->BinaryValue(index);
                BlobLayout::SetDynamicMember(value.first.get(), value.second,
                                             ParsingState::Instance().objects.top().blob,
                                             ParsingState::Instance().objects.top().member,
                                             ParsingState::Instance().objects.top().index);
                return;
            }
        }
    }

    //Error
    void UnexpectedElement(const char* name, const char * expected)
    {
        XML_StopParser(ParsingState::Instance().parser, false);
        ParsingState::Instance().parseError=true;
        std::ostringstream description;
        description << "UnexpectedElement element: " <<name << ", expected '" << expected << "'";
        ErrorHandler::Error("Serialization error", description.str(), "dots_blob_serializer");
    }

    //---------------------------------------
    // Start Element
    //---------------------------------------
    static void XMLCALL XmlObjStartElement(void * /*userData*/, const char *name, const char ** atts)
    {
        ParsingState::Instance().objects.top().characterData.clear();
        if (ParsingState::Instance().preserveSpace.empty())
        {
            ParsingState::Instance().preserveSpace.push(false);
        }
        else
        {
            ParsingState::Instance().preserveSpace.push(ParsingState::Instance().preserveSpace.top());
        }

        while(*atts != NULL)
        {
            const std::string attribute = *atts;
            ++atts;
            const std::string value = *atts;
            ++atts;

            if (attribute == "xml:space")
            {
                if (value == "default")
                {
                    ParsingState::Instance().preserveSpace.top() = false;
                }
                else if (value == "preserve")
                {
                    ParsingState::Instance().preserveSpace.top() = true;
                }
                else
                {
                    XML_StopParser(ParsingState::Instance().parser, false);
                    ParsingState::Instance().parseError=true;
                    std::string description="Unknown value for attribute xml:space: ";
                    description+=value;
                    ErrorHandler::Error("Serialization error", description, "dots_blob_serializer");
                    return;
                }

            }
        }

        const char* e=ValidElement(name);

        if (e==NULL)
        {
            XML_StopParser(ParsingState::Instance().parser, false);
            ParsingState::Instance().parseError=true;
            std::string description="Invalid start element: ";
            description+=name;
            ErrorHandler::Error("Serialization error", description, "dots_blob_serializer");
            return;
        }
        ParsingState::Instance().objects.top().elem.push(e);

        if (e==XmlElements::OBJ && ParsingState::Instance().objects.top().expectedElement==XmlElements::OBJ)
        {
            if (ParsingState::Instance().objects.top().insideMember)
            {
                ParsingState::Instance().objects.top().elem.pop(); //Remove <object> again from parent
                ParseInfo pi;
                ParsingState::Instance().objects.push(pi);
                ParsingState::Instance().objects.top().elem.push(e);
            }
            ParsingState::Instance().objects.top().expectedElement=XmlElements::NAME;
            return;
        }
        else if (e==XmlElements::NAME && ParsingState::Instance().objects.top().expectedElement==XmlElements::NAME)
        {
            return;
        }
        else if (e==XmlElements::INSTANCE_ID && ParsingState::Instance().objects.top().expectedElement==XmlElements::INSTANCE_ID)
        {
            return;
        }
        else if (e==XmlElements::INSTANCE_ID_REF && ParsingState::Instance().objects.top().expectedElement==XmlElements::INSTANCE_ID)
        {
            ParsingState::Instance().objects.top().expectedElement=XmlElements::NAME;
            ParsingState::Instance().objects.top().insideRef++;
            return;
        }
        else if (e==XmlElements::INDEX_REF &&
                 ((ParsingState::Instance().objects.top().expectedElement==XmlElements::INDEX) || ParsingState::Instance().objects.top().insideArray))
        {
            ParsingState::Instance().objects.top().expectedElement=XmlElements::NAME;
            ParsingState::Instance().objects.top().insideRef++;
            return;
        }
        else if (e==XmlElements::VALUE_REF && ParsingState::Instance().objects.top().expectedElement==XmlElements::VALUE)
        {
            ParsingState::Instance().objects.top().expectedElement=XmlElements::NAME;
            ParsingState::Instance().objects.top().insideRef++;
            return;
        }
        else if (e==XmlElements::MEMBERS &&
                 (ParsingState::Instance().objects.top().expectedElement==XmlElements::MEMBERS || ParsingState::Instance().objects.top().expectedElement==XmlElements::INSTANCE_ID))
        {
            ParsingState::Instance().objects.top().expectedElement=XmlElements::MEMBER;
            if (!ParsingState::Instance().objects.top().haveDescription)
            {
                XML_StopParser(ParsingState::Instance().parser, false);
                ParsingState::Instance().parseError=true;
                ErrorHandler::Error("Serialization error", "Unknown DOB-Class", "dots_blob_serializer");
            }
            return;
        }
        else if (e==XmlElements::MEMBER && ParsingState::Instance().objects.top().expectedElement==XmlElements::MEMBER)
        {
            if (ParsingState::Instance().objects.top().done)
            {
                XML_StopParser(ParsingState::Instance().parser, false);
                ParsingState::Instance().parseError=true;
                ErrorHandler::Error("Serialization error", "Too many members specified", "dots_blob_serializer");
            }
            ParsingState::Instance().objects.top().insideMember=true;
            ParsingState::Instance().objects.top().index=0;
            ParsingState::Instance().objects.top().expectedElement=XmlElements::NAME;
            return;
        }
        else if (e==XmlElements::ARRAY_ELEMENTS && ParsingState::Instance().objects.top().expectedElement==XmlElements::ARRAY_ELEMENTS)
        {
            ParsingState::Instance().objects.top().index=0;
            ParsingState::Instance().objects.top().insideArray=true;
            ParsingState::Instance().objects.top().expectedElement=XmlElements::ARRAY_ELEMENT;
            return;
        }
        else if (e==XmlElements::ARRAY_ELEMENT && ParsingState::Instance().objects.top().expectedElement==XmlElements::ARRAY_ELEMENT)
        {
            if (ParsingState::Instance().objects.top().mde->GetMemberType()==EntityIdMemberType)
                ParsingState::Instance().objects.top().expectedElement=XmlElements::ENTITY_ID;
            else if (ParsingState::Instance().objects.top().mde->GetMemberType()==ObjectMemberType)
                ParsingState::Instance().objects.top().expectedElement=XmlElements::OBJ;
            else
                ParsingState::Instance().objects.top().expectedElement=XmlElements::VALUE;
            return;
        }
        else if (e==XmlElements::INDEX &&
                 ((ParsingState::Instance().objects.top().expectedElement==XmlElements::INDEX) || ParsingState::Instance().objects.top().insideArray))
        {
            return;
        }
        else if (e==XmlElements::VALUE && ParsingState::Instance().objects.top().expectedElement==XmlElements::VALUE)
        {
            return;
        }
        else if (e==XmlElements::ENTITY_ID && ParsingState::Instance().objects.top().expectedElement==XmlElements::ENTITY_ID)
        {
            ParsingState::Instance().objects.top().insideEntityId=true;
            ParsingState::Instance().objects.top().expectedElement=XmlElements::NAME;
            ParsingState::Instance().objects.top().entityId.instanceId = 0;
            ParsingState::Instance().objects.top().entityId.typeId = 0;
            ParsingState::Instance().objects.top().instanceIdStr.clear();

            return;
        }
        UnexpectedElement(e,ParsingState::Instance().objects.top().expectedElement);
    }

    //---------------------------------------
    // End Element
    //---------------------------------------
    static void XMLCALL XmlObjEndElement(void * /*userData*/, const char *name)
    {
        //a scope so we can get some locals.
        {
            std::string & charData = ParsingState::Instance().objects.top().characterData;
            if (!charData.empty())
            {
                if (!ParsingState::Instance().preserveSpace.top())
                {
                    const size_t startpos = charData.find_first_not_of(" \t\n");
                    const size_t endpos = charData.find_last_not_of(" \t\n");

                    if ((std::string::npos != startpos) && (std::string::npos != endpos))
                    {
                        HandleCharacters(charData.substr(startpos,endpos-startpos+1));
                    }
                }
                else
                {
                    HandleCharacters(charData);
                }
                charData.clear();
            }
        }

        ParsingState::Instance().preserveSpace.pop();

        Size dummy=0;
        const char* e=ValidElement(name);
        if (e==NULL)
        {
            UnexpectedElement(name,ParsingState::Instance().objects.top().expectedElement);
            return;
        }
        else if (e!=ParsingState::Instance().objects.top().elem.top())
        {
            XML_StopParser(ParsingState::Instance().parser, false);
            ParsingState::Instance().parseError=true;
            std::string descr="XML element mismatch. expectedElement end element '";
            descr+=ParsingState::Instance().objects.top().elem.top();
            descr+=", but found '";
            descr+=name;
            descr+="'.";
            ErrorHandler::Error("Serialization error", descr, "dots_blob_serializer");
            return;
        }

        ParsingState::Instance().objects.top().elem.pop();

        if (e==XmlElements::OBJ)
        {
            ParsingState::Instance().theBlob=ParsingState::Instance().objects.top().blob;
            ParsingState::Instance().objects.pop();
            if (ParsingState::Instance().objects.size()>0)
            {
                BlobLayout::SetStatus(false, true, ParsingState::Instance().objects.top().blob, ParsingState::Instance().objects.top().member, ParsingState::Instance().objects.top().index);
                BlobLayout::SetDynamicMember(ParsingState::Instance().theBlob, dummy, ParsingState::Instance().objects.top().blob, ParsingState::Instance().objects.top().member, ParsingState::Instance().objects.top().index);
                BlobLayout::DeleteBlob(ParsingState::Instance().theBlob);
            }
        }
        else if (e==XmlElements::NAME)
        {
            if (ParsingState::Instance().objects.top().insideRef>0)
            {
                ParsingState::Instance().objects.top().expectedElement=XmlElements::INDEX;
            }
            else if (ParsingState::Instance().objects.top().insideEntityId)
            {
                ParsingState::Instance().objects.top().expectedElement=XmlElements::INSTANCE_ID;
            }
            else if (ParsingState::Instance().objects.top().insideMember)
            {
                if (ParsingState::Instance().objects.top().mde->ArrayLength()>1)
                {
                    ParsingState::Instance().objects.top().expectedElement=XmlElements::ARRAY_ELEMENTS;
                }
                else if (ParsingState::Instance().objects.top().mde->GetMemberType()==EntityIdMemberType)
                {
                    ParsingState::Instance().objects.top().expectedElement=XmlElements::ENTITY_ID;
                }
                else if (ParsingState::Instance().objects.top().mde->GetMemberType()==ObjectMemberType)
                {
                    ParsingState::Instance().objects.top().expectedElement=XmlElements::OBJ;
                }
                else
                {
                    ParsingState::Instance().objects.top().expectedElement=XmlElements::VALUE;
                }
            }
            else //class name
            {
                ParsingState::Instance().objects.top().expectedElement=XmlElements::MEMBERS;
            }

            return;
        }
        else if (e==XmlElements::INSTANCE_ID)
        {
            if (ParsingState::Instance().objects.top().insideEntityId)
            {
                if (ParsingState::Instance().objects.top().insideArray)
                {
                    ParsingState::Instance().objects.top().expectedElement=XmlElements::ARRAY_ELEMENT;
                }
                else
                {
                    ParsingState::Instance().objects.top().expectedElement=XmlElements::MEMBER;
                }
            }
            return;
        }
        else if (e==XmlElements::INSTANCE_ID_REF)
        {
            ParsingState::Instance().objects.top().insideRef--;
            int index;
            const ParameterDescription * pde;
            ResolveParameter(ParsingState::Instance().objects.top().ref, pde, index);
            Int64 hashVal;
            const char * strVal;
            pde->ValueWithOptionalString(index,hashVal,strVal);

            if (ParsingState::Instance().objects.top().insideEntityId)
            {
                ParsingState::Instance().objects.top().entityId.instanceId=hashVal;
                ParsingState::Instance().objects.top().instanceIdStr = strVal;
                if (ParsingState::Instance().objects.top().insideArray)
                {
                    ParsingState::Instance().objects.top().expectedElement=XmlElements::ARRAY_ELEMENT;
                }
                else
                {
                    ParsingState::Instance().objects.top().expectedElement=XmlElements::MEMBER;
                }
            }
            return;
        }
        else if (e==XmlElements::INDEX_REF)
        {
            ParsingState::Instance().objects.top().insideRef--;
            if (ParsingState::Instance().objects.top().insideRef==0)
            {
                int index;
                const ParameterDescription * pde;
                ResolveParameter(ParsingState::Instance().objects.top().ref, pde, index);
                ParsingState::Instance().objects.top().index=static_cast<int>(*(pde->Value<Int32>(index)));

                if (ParsingState::Instance().objects.top().mde->GetMemberType()==EntityIdMemberType)
                    ParsingState::Instance().objects.top().expectedElement=XmlElements::ENTITY_ID;
                else if (ParsingState::Instance().objects.top().mde->GetMemberType()==ObjectMemberType)
                    ParsingState::Instance().objects.top().expectedElement=XmlElements::OBJ;
                else
                    ParsingState::Instance().objects.top().expectedElement=XmlElements::VALUE;
            }
            return;
        }
        else if (e==XmlElements::VALUE_REF)
        {
            ParsingState::Instance().objects.top().insideRef--;
            int index;
            const ParameterDescription * pde;
            ResolveParameter(ParsingState::Instance().objects.top().ref, pde, index);
            if (pde == NULL)
            {
                throw DeserializationFailure();
            }
            HandleRefValue(pde, index);
            return;
        }
        else if (e==XmlElements::INDEX)
        {
            if (ParsingState::Instance().objects.top().mde->GetMemberType()==EntityIdMemberType)
                ParsingState::Instance().objects.top().expectedElement=XmlElements::ENTITY_ID;
            else if (ParsingState::Instance().objects.top().mde->GetMemberType()==ObjectMemberType)
                ParsingState::Instance().objects.top().expectedElement=XmlElements::OBJ;
            else
                ParsingState::Instance().objects.top().expectedElement=XmlElements::VALUE;
            return;
        }
        else if (e==XmlElements::MEMBERS)
        {
            return;
        }
        else if (e==XmlElements::MEMBER)
        {
            ParsingState::Instance().objects.top().expectedElement=XmlElements::MEMBER;
            return;
        }
        else if (e==XmlElements::VALUE)
        {
            if (ParsingState::Instance().objects.top().mde->GetMemberType()==StringMemberType)
            {
                //we want to set the status regardless of whether there was any actual content or not
                //so we set it here just in case HandleValue was never called
                BlobLayout::SetStatus(false, true,
                                      ParsingState::Instance().objects.top().blob,
                                      ParsingState::Instance().objects.top().member,
                                      ParsingState::Instance().objects.top().index);
            }
            else if (ParsingState::Instance().objects.top().mde->GetMemberType()==BinaryMemberType)
            {
                //we want to set the status regardless of whether there was any actual content or not
                //so we set it here just in case HandleValue was never called
                BlobLayout::SetStatus(false, true,
                                      ParsingState::Instance().objects.top().blob,
                                      ParsingState::Instance().objects.top().member,
                                      ParsingState::Instance().objects.top().index);

                //base 64 convert
                int base64Size=(int)ParsingState::Instance().objects.top().base64Binary.size();
                Int32 binarySize = Base64Conversions::CalculateBinarySize(base64Size);
                std::vector<char> binary (binarySize);
                const char * dataStart = NULL;
                if (binarySize != 0) //for zero-size binaries we need not do the actual conversion...
                {
                    dataStart = &binary[0];
                    Int32 resultSize;
                    if (!Base64Conversions::ToBinary(&binary[0],
                                                     static_cast<Int32>(binary.size()),
                                                     ParsingState::Instance().objects.top().base64Binary.c_str(),
                                                     base64Size,
                                                     resultSize))
                    {
                        XML_StopParser(ParsingState::Instance().parser, false);
                        ParsingState::Instance().parseError=true;
                        std::string descr="Failed to deserialize binary member from base64 to binary. Element: ";
                        descr+=ParsingState::Instance().objects.top().elem.top();
                        ErrorHandler::Error("Serialization error", descr, "dots_blob_serializer");
                        return;
                    }
                    binary.resize(resultSize);
                }
                BlobLayout::SetDynamicMember(dataStart,
                                             static_cast<Int32>(binary.size()),
                                             ParsingState::Instance().objects.top().blob,
                                             ParsingState::Instance().objects.top().member,
                                             ParsingState::Instance().objects.top().index);
                ParsingState::Instance().objects.top().base64Binary.clear();
            }

            if (ParsingState::Instance().objects.top().insideArray)
            {
                ParsingState::Instance().objects.top().expectedElement=XmlElements::ARRAY_ELEMENT;
            }
            else
            {
                ParsingState::Instance().objects.top().expectedElement=XmlElements::MEMBER;
            }
            return;
        }
        else if (e==XmlElements::ENTITY_ID)
        {
            ParsingState::Instance().objects.top().insideEntityId=false;
            BlobLayout::SetStatus(false, true, ParsingState::Instance().objects.top().blob, ParsingState::Instance().objects.top().member, ParsingState::Instance().objects.top().index);
            BlobLayout::SetMemberWithOptionalString
                (ParsingState::Instance().objects.top().entityId,
                 ParsingState::Instance().objects.top().instanceIdStr.empty()?NULL:ParsingState::Instance().objects.top().instanceIdStr.c_str(),
                 ParsingState::Instance().objects.top().blob,
                 ParsingState::Instance().objects.top().member,
                 ParsingState::Instance().objects.top().index);
            if (ParsingState::Instance().objects.top().insideArray)
            {
                ParsingState::Instance().objects.top().expectedElement=XmlElements::ARRAY_ELEMENT;
            }
            else
            {
                ParsingState::Instance().objects.top().expectedElement=XmlElements::MEMBER;
            }
            return;

        }
        else if (e==XmlElements::ARRAY_ELEMENTS)
        {
            ParsingState::Instance().objects.top().insideArray=false;
            return;
        }
        else if (e==XmlElements::ARRAY_ELEMENT)
        {
            ParsingState::Instance().objects.top().index++;
            ParsingState::Instance().objects.top().expectedElement=XmlElements::ARRAY_ELEMENT;
            return;
        }
    }

    //---------------------------------------
    // Content
    //---------------------------------------
    static void XMLCALL XmlObjCharacters(void * /*userData*/, const XML_Char *s, int len)
    {
        std::string charData(s,s+len);
        if (!ParsingState::Instance().preserveSpace.top())
        {
            size_t startpos = charData.find_first_not_of(" \t\n");
            size_t endpos = charData.find_last_not_of(" \t\n");

            if ((std::string::npos == startpos) || (std::string::npos == endpos))
            {
                if (!ParsingState::Instance().objects.top().characterData.empty() &&
                    *(ParsingState::Instance().objects.top().characterData.end()-1) != ' ')
                {
                    ParsingState::Instance().objects.top().characterData += ' ';
                }
                return;
            }

            if (0 != startpos &&
                !ParsingState::Instance().objects.top().characterData.empty() &&
                *(ParsingState::Instance().objects.top().characterData.end()-1) != ' ')
            {
                --startpos;
                charData[startpos] = ' ';
            }
            if (charData.size() - 1 != endpos)
            {
                ++endpos;
                charData[endpos] = ' ';
            }

            ParsingState::Instance().objects.top().characterData +=
                charData.substr(startpos,endpos-startpos+1);
        }
        else
        {
            ParsingState::Instance().objects.top().characterData += charData;
        }
    }

    static void HandleCharacters(const std::string & str)
    {
        if (ParsingState::Instance().objects.top().elem.top()==XmlElements::NAME)
        {
            if (ParsingState::Instance().objects.top().insideRef>0)
            {
                if (ParsingState::Instance().objects.top().insideRef==1)
                    ParsingState::Instance().objects.top().ref.clear();
                ParsingState::Instance().objects.top().ref+=str;
                return;
            }
            else if (ParsingState::Instance().objects.top().insideEntityId)
            {
                ParsingState::Instance().objects.top().entityId.typeId=DotsId_Generate64(str.c_str());
                return;
            }
            else if (ParsingState::Instance().objects.top().insideMember)
            {
                const ClassDescription * cde = ParsingState::Instance().objects.top().cde;
                while (cde != NULL)
                {
                    for (Size i=0; i < cde->NumberOfOwnMembers(); i++)
                    {
                        if (str == cde->GetOwnMember(i)->Name())
                        {
                            ParsingState::Instance().objects.top().member = cde->NumberOfInheritedMembers() + i;
                            ParsingState::Instance().objects.top().mde = cde->GetMember(ParsingState::Instance().objects.top().member);
                            return;
                        }
                    }
                    //go to parent
                    cde = cde->BaseClass().get();
                }

                //If we get here, member was not found
                XML_StopParser(ParsingState::Instance().parser, false);
                ParsingState::Instance().parseError=true;
                std::string descr="Undefined member '";
                descr+=str+"' for class '";
                descr += ParsingState::Instance().objects.top().cde->Name();
                ErrorHandler::Error("Serialization error", descr, "dots_blob_serializer");
                return;
            }
            else //class name of object
            {
                TypeId tid=DotsId_Generate64(str.c_str());
                ParsingState::Instance().objects.top().cde=Repository::Classes().FindClass(tid);

                if (ParsingState::Instance().objects.top().cde==NULL)
                {
                    XML_StopParser(ParsingState::Instance().parser, false);
                    ParsingState::Instance().parseError=true;
                    std::string descr="Unknown class '";
                    descr+=str+"'";
                    ErrorHandler::Error("Serialization error", descr, "dots_blob_serializer");
                    return;
                }
                else
                {
                    BlobLayout::CreateBlob(tid, ParsingState::Instance().objects.top().blob);
                    ParsingState::Instance().objects.top().haveDescription=true;
                    return;
                }
            }
        }
        else if (ParsingState::Instance().objects.top().elem.top()==XmlElements::INSTANCE_ID)
        {
            Int64 instNumber;
            const char * strVal = NULL;

            if (IsInt(str.c_str()))
            {
                instNumber=boost::lexical_cast<Int64>(str);
            }
            else
            {
                instNumber = DotsId_Generate64(str.c_str());
                strVal = str.c_str();
            }

            if (ParsingState::Instance().objects.top().insideEntityId)
            {
                ParsingState::Instance().objects.top().entityId.instanceId=instNumber;
                ParsingState::Instance().objects.top().instanceIdStr = strVal==NULL?"":strVal;
            }
            return;
        }
        else if (ParsingState::Instance().objects.top().elem.top()==XmlElements::INDEX)
        {
            int index;
            if (IsInt(str.c_str(), true))
            {
                index=atoi(str.c_str());
            }
            else
            {
                XML_StopParser(ParsingState::Instance().parser, false);
                ParsingState::Instance().parseError=true;
                std::string descr="Index invalid or out of bounds, found index=";
                descr+=str;
                ErrorHandler::Error("Serialization error", descr, "dots_blob_serializer");
                return;
            }

            if (ParsingState::Instance().objects.top().insideRef>0)
            {
                ParsingState::Instance().objects.top().ref+="[";
                ParsingState::Instance().objects.top().ref+=str+"]";
            }
            else //array index
            {
                ParsingState::Instance().objects.top().index=index;
            }
            return;

        }
        else if (eq(ParsingState::Instance().objects.top().elem.top(), XmlElements::VALUE))
        {
            HandleValue(str.c_str());
            return;
        }
    }

    char * XmlToBlobSerializer::Serialize(const char* xmlSource)
    {
        ParsingState::Instance().Reset();
        ParsingState::Instance().parser = XML_ParserCreate("UTF-8");
        bool done = false;
        int depth = 0;
        XML_SetUserData(ParsingState::Instance().parser, &depth);
        XML_SetElementHandler(ParsingState::Instance().parser, XmlObjStartElement, XmlObjEndElement);
        XML_SetCharacterDataHandler(ParsingState::Instance().parser, XmlObjCharacters);

        if (XML_Parse(ParsingState::Instance().parser, xmlSource, static_cast<int>(strlen(xmlSource)), done) == XML_STATUS_ERROR)
        {
            ParsingState::Instance().parseError=true;
        }

        XML_ParserFree(ParsingState::Instance().parser);

        //clear the ParsingState::Instance().objects
        while (!ParsingState::Instance().objects.empty())
        {
            ParsingState::Instance().objects.pop();
        }

        if (!ParsingState::Instance().parseError)
        {
            return ParsingState::Instance().theBlob;
        }
        else
        {
            BlobLayout::DeleteBlob(ParsingState::Instance().theBlob);
            return NULL;
        }
    }

    bool XmlToBlobSerializer::ToTypeId(const std::string & str, TypeId& tid)
    {
        tid=DotsId_Generate64(str.c_str());
        return  Repository::Classes().FindClass(tid) != NULL       ||
            Repository::Properties().FindProperty(tid) != NULL ||
            Repository::Enums().FindEnum(tid) != NULL;
    }

    bool XmlToBlobSerializer::ToEntityId(const std::string & str, DotsC_EntityId& entityId, std::string & instanceIdStr)
    {
        static const std::string startName= std::string("<") + XmlElements::NAME + ">";
        static const std::string stopName=std::string("</") + XmlElements::NAME + ">";
        static const std::string startInstance=std::string("<") + XmlElements::INSTANCE_ID + ">";
        static const std::string stopInstance=std::string("</") + XmlElements::INSTANCE_ID + ">";

        //
        // Read type id
        size_t startNameIndex=str.find(startName);
        const size_t stopNameIndex=str.find(stopName);

        if (startNameIndex == std::string::npos || stopNameIndex == std::string::npos)
        {
            return false;
        }
        startNameIndex += startName.length();

        const std::string name=str.substr(startNameIndex, stopNameIndex - startNameIndex);
        const bool success = ToTypeId(name.c_str(), entityId.typeId);
        if (!success)
        {
            return false;
        }

        //
        // Read instance
        size_t startInstanceIndex=str.find(startInstance);
        const size_t stopInstanceIndex=str.find(stopInstance);

        //if there was no insntace
        if (startInstanceIndex == std::string::npos || stopInstanceIndex == std::string::npos)
        {
            return false;
        }
        startInstanceIndex += startInstance.length();

        const std::string instance=str.substr(startInstanceIndex, stopInstanceIndex-startInstanceIndex);

        if (IsInt(instance.c_str()))
        {
            entityId.instanceId = boost::lexical_cast<Int64>(instance.c_str());
            instanceIdStr.clear();
        }
        else
        {
            entityId.instanceId = DotsId_Generate64(instance.c_str());
            instanceIdStr = instance;
        }

        return true;
    }
}
}
}
}
