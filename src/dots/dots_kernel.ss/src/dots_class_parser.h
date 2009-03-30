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

#ifndef _dots_class_parser_h
#define _dots_class_parser_h

#include "dots_xml_parser_base.h"
#include "dots_temporary_descriptions.h"

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
    class ClassParser : public XmlParserBase
    {
    public:
        ClassParser();

        //Inherited methods
        void Reset();
        bool StartElement(std::string s);
        bool EndElement(std::string s);
        bool Content(std::string str);

        //special ClassParser functions
        int GetIndex(TypeId t);
        DobClasses & Result() {return m_result;}
        size_t GetParameterBlobSize() {return m_parameterBlobSize;}

        typedef unordered_map<std::string, DobParameter> ParameterHashTable;
        ParameterHashTable m_allParameters;


    private:
        DobClasses m_result;
        DobClass m_tmpClass;
        DobMember m_tmpMember;
        DobParameter m_tmpParameter;
        DobParameter::ParameterValue m_tmpParameterValue;
        int m_tmpParameterValueIndex;
        size_t m_parameterBlobSize;
        std::string m_tmpRef;

        std::string ExpandEnvironmentVariables(const std::string& str);
        void PushBackParameterValue();
        void ParameterStartElement(const char* e);
        void ParameterEndElement(const char* e);
        void ResetTempParameterValue();
        const char* ValidElement(const char* e);

        //-----------------------------------
        //Class level
        //-----------------------------------
        bool StartElement_Class(const char* e);
        bool EndElement_Class(const char* e);
        bool Content_Class(std::string& str);

        //-----------------------------------
        //Parameters level
        //-----------------------------------
        bool StartElement_Parameters(const char* e);
        bool EndElement_Parameters(const char* e);
        bool Content_Parameters(std::string& str);
        //--- parameter array ---
        bool StartElement_Parameters_Array(const char* e);
        bool EndElement_Parameters_Array(const char* e);
        bool Content_Parameters_Array(std::string& str);
        //--- parameter entityId ---
        bool StartElement_Parameters_EntityId(const char* e);
        bool EndElement_Parameters_EntityId(const char* e);
        bool Content_Parameters_EntityId(std::string& str);
        //--- parameter object ---
        bool StartElement_Parameters_Object(const char* e);
        bool EndElement_Parameters_Object(const char* e);
        bool Content_Parameters_Object(std::string& str);

        //-----------------------------------
        //Reference level
        //-----------------------------------
        bool StartElement_Ref(const char* e);
        bool EndElement_Ref(const char* e);
        bool Content_Ref(std::string& str);

        //-----------------------------------
        //Members level
        //-----------------------------------
        bool StartElement_Members(const char* e);
        bool EndElement_Members(const char* e);
        bool Content_Members(std::string& str);

        //Flags telling us where in the element hierarchy we are
        bool m_uClass;

        bool m_uParameters;
        bool m_uParameter;
        bool m_uParameterArray;
        bool m_uParameterArrayElement;
        bool m_uParameterValue;

        bool m_uMembers;
        bool m_uMember;

        bool m_uSummary;
        bool m_uCreateRoutines;

        bool m_uEntityId;
        int m_uObject; //greater than 0 menas inside object
        int m_uRef; //greater than 0 means inside ref

        //Valid elements
        typedef enum
        {
            //Class schema
            CLASS_CEL,
            CLASS_NAME_CEL,
            BASE_CLASS_CEL,
            PARAMETERS_CEL,
            MEMBERS_CEL,

            //Parameters schema
            PARAMETER_CEL,
            PARAMETER_NAME_CEL,
            PARAMETER_TYPE_CEL,
            PARAMETER_VALUE_CEL,
            PARAMETER_OBJECT_CEL,
            PARAMETER_ENTITY_ID_CEL,
            PARAMETER_ENTITY_ID_NAME_CEL,
            PARAMETER_ENTITY_ID_INSTANCE_ID_CEL,
            PARAMETER_ARRAY_ELEMENT_CEL,
            PARAMETER_ARRAY_INDEX_CEL,

            //Members schema
            MEMBER_CEL,
            MEMBER_NAME_CEL,
            MEMBER_ARRAY_SIZE_CEL,
            MEMBER_TYPE_CEL,
            MEMBER_STRING_LENGTH_CEL,

        } ExpectedElement;

        //Expected xml-element
        ExpectedElement m_expected;
    };
}
}
}
}
#endif
