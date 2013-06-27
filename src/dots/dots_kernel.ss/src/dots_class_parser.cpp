/******************************************************************************
*
* Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
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

#include "dots_class_parser.h"
#include <Safir/Dob/Typesystem/Internal/Id.h>
#include <Safir/Utilities/Internal/ConfigReader.h>
#include "dots_blob_layout.h"
#include "dots_basic_types.h"
#include "dots_blob_serializer.h"
#include "dots_file_parser.h"
#include <iostream>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{

    /**
     * Parses dou-files containing class definitions.
     */
    ClassParser::ClassParser()
    {
        m_unit="dots_class_parser";
        m_allParameters.clear();
        m_parameterBlobSize=0;

        //Insert Object as preset class
        m_tmpClass.m_name=OBJECT_CLASS;
        m_tmpClass.m_typeId=DotsId_Generate64(m_tmpClass.m_name.c_str());
        m_tmpClass.m_baseClassName=NULL_CLASS;
        m_tmpClass.m_baseClassTypeId=DotsId_Generate64(m_tmpClass.m_baseClassName.c_str());
        m_tmpClass.m_initialSize=0;
        m_tmpClass.m_sizeCalculated=false;
        m_result.push_back(m_tmpClass);
    }

    void ClassParser::Reset()
    {
        m_expected=CLASS_CEL;

        m_uClass=false;
        m_uParameters=false;
        m_uParameter=false;
        m_uParameterArray=false;
        m_uParameterArrayElement=false;
        m_uMembers=false;
        m_uMember=false;
        m_uParameterValue=false;
        m_uSummary=false;
        m_uCreateRoutines=false;
        m_uEntityId=false;
        m_uObject=0;
        m_uRef=0;

        //Reset tmpClass
        m_tmpClass.m_name.clear();
        m_tmpClass.m_baseClassName.clear();
        m_tmpClass.m_baseClassTypeId=0;
        m_tmpClass.m_typeId=0;
        m_tmpClass.m_initialSize=0;
        m_tmpClass.m_sizeCalculated=false;
        m_tmpClass.m_noInheritedMembers=0;
        m_tmpClass.m_noInheritedParameters=0;
        m_tmpClass.m_members.clear();
        m_tmpClass.m_parameters.clear();
        m_tmpClass.m_complex=false;

        //Reset tmpMember
        m_tmpMember.m_name.clear();
        m_tmpMember.m_arrayLength=1;
        m_tmpMember.m_dataLength=0;
        m_tmpMember.m_objType=0;
        m_tmpMember.m_arrSizeFromParameter=false;
        m_tmpMember.m_strLenFromParameter=false;

        //Reset tmpParameter
        m_tmpParameter.m_name.clear();
        m_tmpParameter.m_values.clear();

        //Reset tmpRef
        m_tmpRef.clear();
    }

    const char* ClassParser::ValidElement(const char* e)
    {
        if (eq(e, XmlElements::CLASS)) return XmlElements::CLASS;
        if (eq(e, XmlElements::NAME)) return XmlElements::NAME;
        if (eq(e, XmlElements::BASE_CLASS)) return XmlElements::BASE_CLASS;
        if (eq(e, XmlElements::PARAMETERS)) return XmlElements::PARAMETERS;
        if (eq(e, XmlElements::MEMBERS)) return XmlElements::MEMBERS;

        if (eq(e, XmlElements::PARAMETER)) return XmlElements::PARAMETER;
        if (eq(e, XmlElements::TYPE)) return XmlElements::TYPE;
        if (eq(e, XmlElements::VALUE)) return XmlElements::VALUE;
        if (eq(e, XmlElements::VALUE_REF)) return XmlElements::VALUE_REF;
        if (eq(e, XmlElements::ARRAY_ELEMENTS)) return XmlElements::ARRAY_ELEMENTS;
        if (eq(e, XmlElements::ARRAY_ELEMENT)) return XmlElements::ARRAY_ELEMENT;
        if (eq(e, XmlElements::ARRAY_SIZE)) return XmlElements::ARRAY_SIZE;
        if (eq(e, XmlElements::ARRAY_SIZE_REF)) return XmlElements::ARRAY_SIZE_REF;
        if (eq(e, XmlElements::LENGTH)) return XmlElements::LENGTH;
        if (eq(e, XmlElements::LENGTH_REF)) return XmlElements::LENGTH_REF;
        if (eq(e, XmlElements::OBJ)) return XmlElements::OBJ;
        if (eq(e, XmlElements::ENTITY_ID)) return XmlElements::ENTITY_ID;
        if (eq(e, XmlElements::INDEX)) return XmlElements::INDEX;
        if (eq(e, XmlElements::INDEX_REF)) return XmlElements::INDEX_REF;
        if (eq(e, XmlElements::INSTANCE_ID)) return XmlElements::INSTANCE_ID;
        if (eq(e, XmlElements::INSTANCE_ID_REF)) return XmlElements::INSTANCE_ID_REF;

        if (eq(e, XmlElements::MEMBER)) return XmlElements::MEMBER;

        if (eq(e, XmlElements::SUMMARY)) return XmlElements::SUMMARY;
        if (eq(e, XmlElements::CREATE_ROUTINE)) return XmlElements::CREATE_ROUTINE;
        if (eq(e, XmlElements::CREATE_ROUTINES)) return XmlElements::CREATE_ROUTINES;
        if (eq(e, XmlElements::VALUES)) return XmlElements::VALUES;

        return NULL;
    }

    std::string ClassParser::ExpandEnvironmentVariables(const std::string& str)
    {
        std::string result;
        try
        {
            result = Safir::Utilities::Internal::Expansion::ExpandSpecial(str);
        }
        catch (const std::logic_error& e)
        {
            ErrorHandler::Error("Special variable expansion error", e.what(), "dots_class_parser");
            exit(1);
        }

        try
        {
            result = Safir::Utilities::Internal::Expansion::ExpandEnvironment(result);
        }
        catch (const std::logic_error& e)
        {
            ErrorHandler::Error("Environment variable expansion error", e.what(), "dots_class_parser");
            exit(1);
        }
        
        return result;
    }

    void ClassParser::PushBackParameterValue()
    {
        m_tmpParameterValue.m_value=ExpandEnvironmentVariables(m_tmpParameterValue.m_value);
        switch (m_tmpParameter.m_type)
        {
        case StringMemberType:
            {
                m_parameterBlobSize+=m_tmpParameterValue.m_value.size()+1 + sizeof(void*);
            }
            break;
        case InstanceIdMemberType:
        case ChannelIdMemberType:
        case HandlerIdMemberType:
        case EntityIdMemberType:
            {
                m_parameterBlobSize+=BasicTypes::SizeOfType(m_tmpParameter.m_type);
                if (!IsInt(m_tmpParameterValue.m_value.c_str()))
                {
                    m_parameterBlobSize += BasicTypes::SizeOfType(m_tmpParameter.m_type) + sizeof(void*) +
                        m_tmpParameterValue.m_value.size() +1;
                }
            }
            break;
        default:
            m_parameterBlobSize+=BasicTypes::SizeOfType(m_tmpParameter.m_type);
        }
        //Note that the blob size does not include binaries or objects, since they are allocated
        //outside this parameter blob!

        m_tmpParameter.m_values.push_back(m_tmpParameterValue);
    }

    void ClassParser::ParameterStartElement(const char* e)
    {
        m_tmpParameterValue.m_value+="<";
        m_tmpParameterValue.m_value+=e;
        m_tmpParameterValue.m_value+=">";
    }

    void ClassParser::ParameterEndElement(const char* e)
    {
        m_tmpParameterValue.m_value+="</";
        m_tmpParameterValue.m_value+=e;
        m_tmpParameterValue.m_value+=">";
    }

    void ClassParser::ResetTempParameterValue()
    {
        m_tmpParameterValue.m_isNull=true;
        m_tmpParameterValue.m_indexFromParameter=false;
        m_tmpParameterValue.m_valueFromParameter=false;
        m_tmpParameterValue.m_index=0;
        m_tmpParameterValue.m_value.clear();

        boost::filesystem::path dummy;
        FileParser::GetFileNameAndLineNumber(dummy, m_tmpParameterValue.m_lineNumber);

    }

    int ClassParser::GetIndex(TypeId t)
    {
        for (size_t i=0; i<m_result.size(); i++)
        {
            if (m_result[i].m_typeId==t)
                return static_cast<int>(i);
        }
        return -1;
    }

    //---------------------------------------------------------
    // Element handlers
    //---------------------------------------------------------
    bool ClassParser::StartElement(const std::string& element)
    {
        const char* e=ValidElement(element.c_str());
        if (e==NULL)
        {
            IllegalElement(element.c_str());
            return false;
        }
        m_elem.push(e);

        if (m_uSummary || m_uCreateRoutines)
        {
            return true;
        }
        else if (e==XmlElements::SUMMARY)
        {
            m_uSummary=true;
            return true;
        }
        else if (e==XmlElements::CREATE_ROUTINES)
        {
            m_uCreateRoutines=true;
            return true;
        }
        else if (m_uClass || e==XmlElements::CLASS)
        {
            return StartElement_Class(e);
        }

        IllegalOrder(e);
        return false;
    }

    bool ClassParser::EndElement(const std::string& element)
    {
        const char* e=ValidElement(element.c_str());
        if (e==NULL)
        {
            IllegalElement(element.c_str(), false);
            return false;
        }
        else if (e!=m_elem.top())
        {
            ElementMismatch(e);
            return false;
        }

        m_elem.pop();

        if (e==XmlElements::SUMMARY)
        {
            m_uSummary=false;
            return true;
        }
        else if (e==XmlElements::CREATE_ROUTINES)
        {
            m_uCreateRoutines=false;
            return true;
        }
        if (m_uSummary || m_uCreateRoutines)
        {
            return true;
        }
        else if (m_uClass)
        {
            return EndElement_Class(e);
        }

        IllegalElement(e, false);
        return false;
    }

    bool ClassParser::Content(const std::string& str)
    {
        if (m_uSummary || m_uCreateRoutines)
        {
            return true;
        }
        else if (m_uClass)
        {
            return Content_Class(str);
        }

        return true;
    }

    //-----------------------------------
    //Class level
    //-----------------------------------
    bool ClassParser::StartElement_Class(const char* e)
    {
        if (e==XmlElements::CLASS)
        {
            m_uClass=true;
            m_expected=CLASS_NAME_CEL;
            return true;
        }
        else if (m_uParameters ||
            (m_expected==PARAMETERS_CEL && e==XmlElements::PARAMETERS))
        {
            return StartElement_Parameters(e);
        }
        else if (m_uMembers ||
            ((m_expected==PARAMETERS_CEL || m_expected==MEMBERS_CEL) &&
            e==XmlElements::MEMBERS))
        {
            return StartElement_Members(e);
        }
        else if (m_expected==CLASS_NAME_CEL && e==XmlElements::NAME)
        {
            return true;
        }
        else if (m_expected==BASE_CLASS_CEL && e==XmlElements::BASE_CLASS)
        {
            return true;
        }

        IllegalOrder(e);
        return false;;
    }

    bool ClassParser::EndElement_Class(const char* e)
    {
        if (e==XmlElements::CLASS)
        {
            m_result.push_back(m_tmpClass);
            m_uClass=false;
            return true;
        }
        else if (m_uParameters || e==XmlElements::PARAMETERS)
        {
            return EndElement_Parameters(e);
        }
        else if (m_uMembers || e==XmlElements::MEMBERS)
        {
            return EndElement_Members(e);
        }
        else if (e==XmlElements::NAME)
        {
            m_expected=BASE_CLASS_CEL;
            return true;
        }
        else if (e==XmlElements::BASE_CLASS)
        {
            m_expected=PARAMETERS_CEL;
            return true;
        }

        IllegalElement(e, false);
        return false;
    }

    bool ClassParser::Content_Class(const std::string& str)
    {

        if (m_uParameters)
        {
            return Content_Parameters(str);
        }
        else if (m_uMembers)
        {
            return Content_Members(str);
        }
        if (m_uRef>0)
        {
            return Content_Ref(str);
        }
        else if (m_elem.top()==XmlElements::NAME)
        {
            m_tmpClass.m_name=str;
            m_tmpClass.m_typeId=DotsId_Generate64(m_tmpClass.m_name.c_str());
            return true;
        }
        else if (m_elem.top()==XmlElements::BASE_CLASS)
        {
            m_tmpClass.m_baseClassName=str;
            m_tmpClass.m_baseClassTypeId=DotsId_Generate64(m_tmpClass.m_baseClassName.c_str());
            return true;
        }

        BadContent(m_elem.top(), str.c_str());
        return false;
    }

    //-----------------------------------
    //Parameters level
    //-----------------------------------
    bool ClassParser::StartElement_Parameters(const char* e)
    {
        if (e==XmlElements::PARAMETERS)
        {
            m_uParameters=true;
            m_expected=PARAMETER_CEL;
            return true;
        }
        else if (m_uParameterArray)
        {
            return StartElement_Parameters_Array(e);
        }
        else if (m_uEntityId)
        {
            return StartElement_Parameters_EntityId(e);
        }
        else if (m_uRef>0)
        {
            return StartElement_Ref(e);
        }
        else if (m_uObject>0)
        {
            return StartElement_Parameters_Object(e);
        }

        if (m_expected==PARAMETER_CEL && e==XmlElements::PARAMETER)
        {
            m_expected=PARAMETER_NAME_CEL;
            m_uParameter=true;
            m_tmpParameter.m_values.clear();
            int line;
            boost::filesystem::path file;
            FileParser::GetFileNameAndLineNumber(file, line);
            m_tmpParameter.m_file=file;
            ResetTempParameterValue();
            return true;
        }
        else if (m_expected==PARAMETER_NAME_CEL && e==XmlElements::NAME)
        {
            return true;
        }
        else if (m_expected==PARAMETER_TYPE_CEL && e==XmlElements::TYPE)
        {
            return true;
        }
        else if (m_expected==PARAMETER_VALUE_CEL)
        {
            if (e==XmlElements::OBJ)
            {
                return StartElement_Parameters_Object(e);
            }
            else if (e==XmlElements::ENTITY_ID)
            {
                return StartElement_Parameters_EntityId(e);
            }
            else if (e==XmlElements::ARRAY_ELEMENTS)
            {
                return StartElement_Parameters_Array(e);
            }
            else if (e==XmlElements::VALUE)
            {
                return true;
            }
            else if (e==XmlElements::VALUE_REF)
            {
                return StartElement_Ref(e);
            }
        }

        IllegalOrder(e);
        return false;
    }

    bool ClassParser::EndElement_Parameters(const char* e)
    {
        if (e==XmlElements::PARAMETERS)
        {
            m_uParameters=false;
            m_expected=MEMBERS_CEL;
            return true;
        }
        else if (m_uParameterArray || e==XmlElements::ARRAY_ELEMENTS)
        {
            return EndElement_Parameters_Array(e);
        }
        else if (m_uObject>0)
        {
            return EndElement_Parameters_Object(e);
        }
        else if (m_uEntityId || e==XmlElements::ENTITY_ID)
        {
            return EndElement_Parameters_EntityId(e);
        }
        else if (m_uRef>0 || e==XmlElements::VALUE_REF)
        {
            if (m_uRef==1 && e==XmlElements::VALUE_REF)
            {
                m_tmpParameterValue.m_isNull=false;
                m_tmpParameterValue.m_valueFromParameter=true;
                m_tmpParameterValue.m_value=m_tmpRef;
                PushBackParameterValue();
            }
            return EndElement_Ref(e);
        }
        else if (e==XmlElements::NAME)
        {
            m_expected=PARAMETER_TYPE_CEL;
            return true;
        }
        else if (e==XmlElements::TYPE)
        {
            m_expected=PARAMETER_VALUE_CEL;
            return true;
        }
        else if (e==XmlElements::VALUE)
        {
            PushBackParameterValue();
            return true;
        }
        else if (e==XmlElements::PARAMETER)
        {
            m_tmpClass.m_parameters.push_back(m_tmpParameter);
            m_allParameters[m_tmpClass.m_name+"."+m_tmpParameter.m_name]=m_tmpParameter;
            m_uParameter=false;
            m_expected=PARAMETER_CEL;
            return true;
        }

        IllegalElement(e, false);
        return false;
    }

    bool ClassParser::Content_Parameters(const std::string& str)
    {
        if (m_uParameterArray)
        {
            return Content_Parameters_Array(str);
        }
        else if (m_uEntityId)
        {
            return Content_Parameters_EntityId(str);
        }
        if (m_uRef>0)
        {
            return Content_Ref(str);
        }
        else if (m_uObject>0)
        {
            return Content_Parameters_Object(str);
        }

        else if (m_elem.top()==XmlElements::NAME)
        {
            m_tmpParameter.m_name=str;
            return true;
        }
        else if (m_elem.top()==XmlElements::TYPE)
        {
            m_tmpParameter.m_type=Safir::Dob::Typesystem::Internal::BasicTypes::MemberTypeOf(str.c_str());
            if (m_tmpParameter.m_type==ObjectMemberType)
            {
                m_tmpParameter.m_objType=DotsId_Generate64(str.c_str());
                m_tmpClass.m_complex=true;
            }
            return true;
        }
        else if (m_elem.top()==XmlElements::VALUE)
        {
            m_tmpParameterValue.m_valueFromParameter=false;
            m_tmpParameterValue.m_isNull=false;
            m_tmpParameterValue.m_value=str;
            return true;
        }
        return true;
    }

    //--- parameter array ---
    bool ClassParser::StartElement_Parameters_Array(const char* e)
    {
        if (m_uObject>0)
        {
            return StartElement_Parameters_Object(e);
        }
        else if (e==XmlElements::ARRAY_ELEMENTS)
        {
            m_expected=PARAMETER_ARRAY_ELEMENT_CEL;
            m_uParameterArray=true;
            m_tmpParameterValueIndex=0;
            return true;
        }
        else if (m_uEntityId)
        {
            return StartElement_Parameters_EntityId(e);
        }
        else if (m_uRef>0)
        {
            return StartElement_Ref(e);
        }
        else if (m_expected==PARAMETER_ARRAY_ELEMENT_CEL && e==XmlElements::ARRAY_ELEMENT)
        {
            m_expected=PARAMETER_ARRAY_INDEX_CEL;
            m_uParameterArrayElement=true;
            ResetTempParameterValue();
            m_tmpParameterValue.m_index=m_tmpParameterValueIndex++;
            return true;
        }
        else if (m_expected==PARAMETER_ARRAY_INDEX_CEL && e==XmlElements::INDEX)
        {
            m_expected=PARAMETER_VALUE_CEL;
            return true;
        }
        else if (m_expected==PARAMETER_ARRAY_INDEX_CEL && e==XmlElements::INDEX_REF)
        {
            m_expected=PARAMETER_VALUE_CEL;
            //m_uRef++;
            return StartElement_Ref(e);
        }
        else if (m_expected==PARAMETER_ARRAY_INDEX_CEL || m_expected==PARAMETER_VALUE_CEL)
        {
            if (e==XmlElements::OBJ)
            {
                return StartElement_Parameters_Object(e);
            }
            else if (e==XmlElements::ENTITY_ID)
            {
                return StartElement_Parameters_EntityId(e);
            }
            else if (e==XmlElements::VALUE)
            {
                return true;
            }
            else if (e==XmlElements::VALUE_REF)
            {
                return StartElement_Ref(e);
            }
        }

        IllegalOrder(e);
        return false;
    }

    bool ClassParser::EndElement_Parameters_Array(const char* e)
    {
        if (m_uObject>0)
        {
            return EndElement_Parameters_Object(e);
        }
        else if (e==XmlElements::ARRAY_ELEMENTS)
        {
            m_uParameterArray=false;
            return true;
        }
        else if (e==XmlElements::ARRAY_ELEMENT)
        {
            m_expected=PARAMETER_ARRAY_ELEMENT_CEL;
            m_uParameterArrayElement=false;
            return true;
        }
        else if (m_uEntityId)
        {
            return EndElement_Parameters_EntityId(e);
        }
        else if (m_uRef>0)
        {
            if (m_uRef==1)
            {
                if (e==XmlElements::INDEX_REF)
                {
                    m_expected=PARAMETER_VALUE_CEL;
                    m_tmpParameterValue.m_indexFromParameter=true;
                    m_tmpParameterValue.m_indexParameter=m_tmpRef;
                }
                else if (e==XmlElements::VALUE_REF)
                {
                    m_tmpParameterValue.m_isNull=false;
                    m_tmpParameterValue.m_valueFromParameter=true;
                    m_tmpParameterValue.m_value=m_tmpRef;
                    PushBackParameterValue();
                }
            }

            return EndElement_Ref(e);
        }
        else if (e==XmlElements::INDEX)
        {
            m_expected=PARAMETER_VALUE_CEL;
            return true;
        }
        else if (e==XmlElements::VALUE)
        {
            PushBackParameterValue();
            return true;
        }

        IllegalElement(e, false);
        return false;
    }

    bool ClassParser::Content_Parameters_Array(const std::string& str)
    {
        if (m_uObject>0)
        {
            return Content_Parameters_Object(str);
        }
        else if (m_uEntityId)
        {
            return Content_Parameters_EntityId(str);
        }
        if (m_uRef)
        {
            return Content_Ref(str);
        }
        else if (m_elem.top()==XmlElements::INDEX)
        {
            m_tmpParameterValue.m_indexFromParameter=false;
            if (IsInt(str.c_str(), true))
            {
                m_tmpParameterValue.m_index=atoi(str.c_str());
                m_tmpParameterValueIndex=m_tmpParameterValue.m_index+1;
                return true;
            }
            else
            {
                BadContent(m_elem.top(), str.c_str());
                return false;
            }
        }
        else if (m_elem.top()==XmlElements::VALUE)
        {
            m_tmpParameterValue.m_valueFromParameter=false;
            m_tmpParameterValue.m_isNull=false;
            m_tmpParameterValue.m_value=str;
            return true;
        }

        BadContent(m_elem.top(), str.c_str());
        return false;
    }

    //--- parameter entityId ---
    bool ClassParser::StartElement_Parameters_EntityId(const char* e)
    {
        if (e==XmlElements::ENTITY_ID)
        {
            m_expected=PARAMETER_ENTITY_ID_NAME_CEL;
            m_uEntityId=true;
            m_tmpParameterValue.m_isNull=false;
            m_tmpParameterValue.m_valueFromParameter=false;
            m_tmpParameterValue.m_value.clear();
            ParameterStartElement(e);
            return true;
        }
        else if (m_uRef>0)
        {
            return StartElement_Ref(e);
        }
        else if (m_expected==PARAMETER_ENTITY_ID_NAME_CEL && e==XmlElements::NAME)
        {
            m_expected=PARAMETER_ENTITY_ID_INSTANCE_ID_CEL;
            ParameterStartElement(e);
            return true;
        }
        else if (m_expected==PARAMETER_ENTITY_ID_INSTANCE_ID_CEL)
        {
            ParameterStartElement(e);
            if (e==XmlElements::INSTANCE_ID)
            {
                return true;
            }
            else if (e==XmlElements::INSTANCE_ID_REF)
            {
                return StartElement_Ref(e);
            }
        }

        IllegalOrder(e);
        return false;
    }

    bool ClassParser::EndElement_Parameters_EntityId(const char* e)
    {
        ParameterEndElement(e);

        if (e==XmlElements::ENTITY_ID)
        {
            PushBackParameterValue();
            m_uEntityId=false;
            return true;
        }
        else if (m_uRef>0)
        {
            return EndElement_Ref(e);
        }
        else if (e==XmlElements::NAME)
        {
            m_expected=PARAMETER_ENTITY_ID_INSTANCE_ID_CEL;
            return true;
        }
        else if (e==XmlElements::INSTANCE_ID)
        {
            m_expected=PARAMETER_CEL;
            return true;
        }
        else if (e==XmlElements::INSTANCE_ID_REF)
        {
            m_expected=PARAMETER_CEL;
            return EndElement_Ref(e);
        }

        IllegalElement(e, false);
        return false;
    }

    bool ClassParser::Content_Parameters_EntityId(const std::string& str)
    {
        //TODO - better handling of entityId
        m_tmpParameterValue.m_value+=str;
        return true;
    }

    //--- parameter object ---
    bool ClassParser::StartElement_Parameters_Object(const char* e)
    {
        if (e==XmlElements::OBJ)
        {
            m_uObject++;
            if (m_uObject==1) //start of new object
            {
                m_tmpParameterValue.m_isNull=false;
                m_tmpParameterValue.m_valueFromParameter=false;
                m_tmpParameterValue.m_value.clear();
            }
        }

        ParameterStartElement(e);
        return true;
    }

    bool ClassParser::EndElement_Parameters_Object(const char* e)
    {
        ParameterEndElement(e);

        if (e==XmlElements::OBJ)
        {
            m_uObject--;

            if (m_uObject==0) //complete object
            {
                PushBackParameterValue();
            }
        }
        return true;
    }

    bool ClassParser::Content_Parameters_Object(const std::string& str)
    {
        m_tmpParameterValue.m_value+=str;
        return true;
    }

    //-----------------------------------
    //Members level
    //-----------------------------------
    bool ClassParser::StartElement_Members(const char* e)
    {
        if (e==XmlElements::MEMBERS)
        {
            m_uMembers=true;
            m_expected=MEMBER_CEL;
            return true;
        }
        else if (m_expected==MEMBER_CEL && e==XmlElements::MEMBER)
        {
            m_expected=MEMBER_NAME_CEL;
            m_uMember=true;

            m_tmpMember.m_arrayLength=1;
            m_tmpMember.m_dataLength=0;
            m_tmpMember.m_arrSizeFromParameter=false;
            m_tmpMember.m_strLenFromParameter=false;

            return true;
        }
        else if (m_uRef>0)
        {
            return StartElement_Ref(e);
        }
        else if (m_expected==MEMBER_NAME_CEL && e==XmlElements::NAME)
        {
            return true;
        }
        else if (m_expected==MEMBER_ARRAY_SIZE_CEL && e==XmlElements::ARRAY_SIZE)
        {
            return true;
        }
        else if (m_expected==MEMBER_ARRAY_SIZE_CEL && e==XmlElements::ARRAY_SIZE_REF)
        {
            return StartElement_Ref(e);
        }
        else if ((m_expected==MEMBER_ARRAY_SIZE_CEL || m_expected==MEMBER_TYPE_CEL) && e==XmlElements::TYPE)
        {
            return true;
        }
        else if (m_expected==MEMBER_STRING_LENGTH_CEL && e==XmlElements::LENGTH)
        {
            return true;
        }
        else if (m_expected==MEMBER_STRING_LENGTH_CEL && e==XmlElements::LENGTH_REF)
        {
            return StartElement_Ref(e);
        }

        IllegalOrder(e);
        return false;
    }

    bool ClassParser::EndElement_Members(const char* e)
    {
        if (e==XmlElements::MEMBERS)
        {
            m_uMembers=false;
            return true;
        }
        else if (e==XmlElements::MEMBER)
        {
            m_tmpClass.m_members.push_back(m_tmpMember);
            m_expected=MEMBER_CEL;
            m_uMember=false;
            return true;
        }
        else if (m_uRef>0)
        {
            if (m_uRef==1)
            {
                if (e==XmlElements::LENGTH_REF)
                {
                    m_tmpMember.m_strLenFromParameter=true;
                    m_tmpMember.m_strLenParameter=m_tmpRef;
                }
                else if (e==XmlElements::ARRAY_SIZE_REF)
                {
                    m_expected=MEMBER_TYPE_CEL;
                    m_tmpMember.m_arrSizeFromParameter=true;
                    m_tmpMember.m_arrSizeParameter=m_tmpRef;
                }
            }
            return EndElement_Ref(e);
        }
        else if (e==XmlElements::NAME)
        {
            m_expected=MEMBER_ARRAY_SIZE_CEL;
            return true;
        }
        else if (e==XmlElements::ARRAY_SIZE)
        {
            m_expected=MEMBER_TYPE_CEL;
            return true;
        }
        else if (e==XmlElements::TYPE)
        {
            m_expected=MEMBER_STRING_LENGTH_CEL;
            return true;
        }
        else if (e==XmlElements::LENGTH)
        {
            return true;
        }

        IllegalElement(e, false);
        return false;
    }

    bool ClassParser::Content_Members(const std::string& str)
    {
        if (m_uRef>0)
        {
            return Content_Ref(str);
        }
        else if (m_elem.top()==XmlElements::NAME)
        {
            m_tmpMember.m_name=str;
            return true;
        }
        else if (m_elem.top()==XmlElements::ARRAY_SIZE)
        {
            if (IsInt(str.c_str(), true))
            {
                m_tmpMember.m_arrSizeFromParameter=false;
                m_tmpMember.m_arrayLength=boost::lexical_cast<Int32>(str);
                return true;
            }
            else
            {
                BadContent(m_elem.top(), str.c_str());
                return false;
            }
        }
        else if (m_elem.top()==XmlElements::TYPE)
        {
            m_tmpMember.m_type=Safir::Dob::Typesystem::Internal::BasicTypes::MemberTypeOf(str.c_str());
            if (m_tmpMember.m_type==ObjectMemberType)
            {
                m_tmpMember.m_objType=DotsId_Generate64(str.c_str());
                m_tmpClass.m_complex=true;
            }
            return true;
        }
        else if (m_elem.top()==XmlElements::LENGTH)
        {
            if (IsInt(str.c_str(), true))
            {
                m_tmpMember.m_strLenFromParameter=false;
                m_tmpMember.m_dataLength=boost::lexical_cast<Int32>(str);
                return true;
            }
            else
            {
                BadContent(m_elem.top(), str.c_str());
                return false;
            }
        }

        BadContent(m_elem.top(), str.c_str());
        return false;
    }

    //-----------------------------------
    //Reference level
    //-----------------------------------
    bool ClassParser::StartElement_Ref(const char* e)
    {
        if (e==XmlElements::ARRAY_SIZE_REF || e==XmlElements::INDEX_REF ||
            e==XmlElements::INSTANCE_ID_REF ||
            e==XmlElements::LENGTH_REF || e==XmlElements::VALUE_REF )
        {
            m_uRef++;
            if (m_uRef==1) //start of new ref
            {
                m_tmpRef.clear();
            }
            else //nested ref
            {
                m_tmpRef+="[";
            }
            return true;
        }
        else if (e==XmlElements::NAME)
        {
            return true;
        }
        else if (e==XmlElements::INDEX)
        {
            m_tmpRef+="[";
            return true;
        }

        IllegalOrder(e);
        return false;
    }

    bool ClassParser::EndElement_Ref(const char* e)
    {
        if (e==XmlElements::ARRAY_SIZE_REF || e==XmlElements::INDEX_REF ||
            e==XmlElements::INSTANCE_ID_REF ||
            e==XmlElements::LENGTH_REF || e==XmlElements::VALUE_REF )
        {
            m_uRef--;
            if (m_uRef>0) //end of nested ref
            {
                m_tmpRef+="]";
            }
            return true;
        }
        else if (e==XmlElements::NAME)
        {
            return true;
        }
        else if (e==XmlElements::INDEX)
        {
            m_tmpRef+="]";
            return true;
        }

        IllegalOrder(e);
        return false;
    }

    bool ClassParser::Content_Ref(const std::string& str)
    {
        if (m_elem.top()==XmlElements::NAME)
        {
            m_tmpRef+=str;
            return true;
        }
        else if (m_elem.top()==XmlElements::INDEX)
        {
            m_tmpRef+=str;
            return true;
        }
        else if (m_elem.top()==XmlElements::INDEX_REF)
        {
            m_tmpRef+=str;
            return true;
        }

        BadContent(m_elem.top(), str.c_str());
        return false;
    }

}
}
}
}
