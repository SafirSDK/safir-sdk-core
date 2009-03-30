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

#include "dots_property_mapping_parser.h"
#include "dots_file_parser.h"
#include <Safir/Dob/Typesystem/Internal/Id.h>
#include <iostream>
#include <assert.h>
#include <sstream>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    PropertyMappingParser::PropertyMappingParser()
    {
        m_unit="dots_property_mapping_parser";
    }

    PropertyMappingParser::~PropertyMappingParser()
    {

    }


    void PropertyMappingParser::Reset()
    {

    }

    bool PropertyMappingParser::ValidElement(const std::string & element)
    {
        if (m_ParseStack.empty())
        {
            return element == XmlElements::PROPERTY_MAPPING;
        }
        else if (m_ParseStack.back() == XmlElements::PROPERTY_MAPPING)
        {
            return element == XmlElements::SUMMARY ||
                element == XmlElements::PROPERTY ||
                element == XmlElements::CLASS ||
                element == XmlElements::MEMBER_MAPPING;
        }
        else if (m_ParseStack.back() == XmlElements::MEMBER_MAPPING)
        {
            return element == XmlElements::MEMBER;
        }
        else if (m_ParseStack.back() == XmlElements::MEMBER)
        {
            return element == XmlElements::PROPERTY_MEMBER ||
                element == XmlElements::CLASS_MEMBER_REFERENCE ||
                element == XmlElements::VALUE ||
                element == XmlElements::VALUE_REF ||
                element == XmlElements::OBJ ||
                element == XmlElements::ENTITY_ID ||
                element == XmlElements::ARRAY_ELEMENTS ||
                element == XmlElements::NAME;
        }
        else if (m_ParseStack.back() == XmlElements::CLASS_MEMBER_REFERENCE)
        {
            return element == XmlElements::CLASS_MEMBER ||
                element == XmlElements::INDEX ||
                element == XmlElements::INDEX_REF ||
                element == XmlElements::CLASS_MEMBER_REFERENCE;
        }
        else if (m_ParseStack.back() == XmlElements::INDEX_REF ||
                 m_ParseStack.back() == XmlElements::VALUE_REF ||
                 m_ParseStack.back() == XmlElements::INSTANCE_ID_REF)
        {
            return element == XmlElements::NAME ||
                element == XmlElements::INDEX ||
                element == XmlElements::INDEX_REF;
        }

        else if (m_ParseStack.back() == XmlElements::OBJ)
        {
            return element == XmlElements::NAME ||
                element == XmlElements::MEMBERS;
        }
        else if (m_ParseStack.back() == XmlElements::MEMBERS)
        {
            return element == XmlElements::MEMBER;
        }
        else if (m_ParseStack.back() == XmlElements::ENTITY_ID)
        {
            return element == XmlElements::NAME ||
                element == XmlElements::INSTANCE_ID ||
                element == XmlElements::INSTANCE_ID_REF;
        }
        else if (m_ParseStack.back() == XmlElements::ARRAY_ELEMENTS)
        {
            return element == XmlElements::ARRAY_ELEMENT;
        }
        else if (m_ParseStack.back() == XmlElements::ARRAY_ELEMENT)
        {
            return element == XmlElements::INDEX ||
                element == XmlElements::INDEX_REF ||
                element == XmlElements::VALUE ||
                element == XmlElements::VALUE_REF ||
                element == XmlElements::OBJ ||
                element == XmlElements::ENTITY_ID;
        }
        else
        {
            //TODO: report error with file name and line number of the xml doco
            return false;
        }

    }

    bool PropertyMappingParser::StartElement(std::string element)
    {
        if (!ValidElement(element))
        {
            IllegalElement(element.c_str());
            return false;
        }

        boost::filesystem::path filename;
        int line;
        FileParser::GetFileNameAndLineNumber(filename, line);

        //root element
        if (element == XmlElements::PROPERTY_MAPPING)
        {
            if (m_CurrentElement != NULL)
            {
                throw "Internal Error";
            }
            m_PropertyMappings.push_back(DobPropertyMapping(filename));
        }
        else if (element == XmlElements::MEMBER && m_ParseStack.back() == XmlElements::MEMBER_MAPPING)
        {
            m_PropertyMappings.back().m_memberMappingsDOM.push_back(SimpleDOM::Element(element, line));
            m_CurrentElement = &(m_PropertyMappings.back().m_memberMappingsDOM.back());
        }
        else if (m_CurrentElement != NULL)
        {
            m_CurrentElement->m_Children.push_back(SimpleDOM::Element(element, line));
            m_CurrentElement->m_Children.back().m_pParent = m_CurrentElement;
            m_CurrentElement = &m_CurrentElement->m_Children.back();
        }
        //other elements (property and class) are read in Contents(...)

        m_ParseStack.push_back(element);
        return true;
    }

    bool PropertyMappingParser::EndElement(std::string element)
    {
        if (m_ParseStack.back() != element)
        {
            ElementMismatch(element.c_str());
            return false;
        }
        m_ParseStack.pop_back();

        //Check that the filename is what we expect
        if (element == XmlElements::CLASS)
        {
            std::string generatedFileName = m_PropertyMappings.back().m_className
                + "-"
                + m_PropertyMappings.back().m_propertyName
                + ".dom";

            if (generatedFileName != m_currentFileName)
            {
                //TODO: generate error
                std::wcout << "Filename is BAD!" << std::endl;
                std::wcout << m_currentFileName.c_str()
                           << ":0: Unexpected filename. Expected this file to be named '"
                           << generatedFileName.c_str()
                           << "'." << std::endl;
                return false;
            }
        }

        //are we inside a member?
        if (m_CurrentElement != NULL)
        {
            //check that we have a name and a value or child
            if (m_CurrentElement->m_Name.empty() ||
                (!m_CurrentElement->m_bHaveValue && m_CurrentElement->m_Children.empty()))
            {
                IllegalElement(element.c_str(),true);
            }

            //go up one in the hierarchy
            m_CurrentElement = m_CurrentElement->m_pParent;
        }
        return true;
    }

    bool PropertyMappingParser::Content(std::string str)
    {
        if (m_CurrentElement != NULL)
        {
            m_CurrentElement->m_Value = str;
            m_CurrentElement->m_bHaveValue = true;
        }
        else
        {
            //ignore comments
            if (m_ParseStack.back() == XmlElements::SUMMARY)
            {
                return true;
            }
            else if (m_ParseStack.back() == XmlElements::PROPERTY)
            {
                m_PropertyMappings.back().m_propertyName = str;
                m_PropertyMappings.back().m_propertyTypeId=DotsId_Generate64(str.c_str());
            }
            else if (m_ParseStack.back() == XmlElements::CLASS)
            {
                m_PropertyMappings.back().m_className = str;
                m_PropertyMappings.back().m_classTypeId=DotsId_Generate64(str.c_str());
            }
        }

        return true;
    }

    //recursively add the reference parts to the reference
    void GetTemporaryReference(const SimpleDOM::Element & domElement, std::string & reference)
    {
        assert(domElement.m_Children.at(0).m_bHaveValue);
        reference.append(domElement.m_Children.at(0).m_Value);

        //do we have more elements?
        if (domElement.m_Children.size() > 1)
        {
            reference.append("[");
            //do we have an index
            if (domElement.m_Children.at(1).m_Name == XmlElements::INDEX)
            {
                reference.append(domElement.m_Children.at(1).m_Value);
            }
            else if (domElement.m_Children.at(1).m_Name == XmlElements::INDEX_REF) //or a ref
            {
                GetTemporaryReference(domElement.m_Children.at(1),reference);
            }
            reference.append("]");
        }
    }


    //recursively add the references to the classmemberreference
    void GetTemporaryClassMemberReference(const SimpleDOM::Element & domElement, Temporary::ClassMemberReference & reference)
    {
        reference.push_back(Temporary::MemberReferenceElement()); //add somewhere to put the data

        Temporary::MemberReferenceElement & theRefPart = reference.back(); //make it easier to access.

        assert(domElement.m_Name == XmlElements::CLASS_MEMBER_REFERENCE);

        assert(domElement.m_Children.at(0).m_bHaveValue);
        theRefPart.m_classMember = domElement.m_Children.at(0).m_Value; //get the member name

        //do we have more elements?
        if (domElement.m_Children.size() > 1)
        {
            //do we have an index
            if (domElement.m_Children.at(1).m_Name == XmlElements::INDEX)
            {
                theRefPart.m_index = domElement.m_Children.at(1).m_Value;
            }
            else if(domElement.m_Children.at(1).m_Name == XmlElements::INDEX_REF)
            { //build the index string
                GetTemporaryReference(domElement.m_Children.at(1),theRefPart.m_index);
            }

            //do we have another classmemberref in child 1
            if (domElement.m_Children.at(1).m_Name == XmlElements::CLASS_MEMBER_REFERENCE)
            {
                //recurse
                GetTemporaryClassMemberReference(domElement.m_Children[1], reference);
            }
            else if (domElement.m_Children.size() > 2 && domElement.m_Children.at(2).m_Name == XmlElements::CLASS_MEMBER_REFERENCE)
            {//or maybe in child 2
                //recurse
                GetTemporaryClassMemberReference(domElement.m_Children[2], reference);
            }
        }
    }

    void SerializeDOM(const SimpleDOM::Element & element, std::string & destination)
    {
        destination += "<" + element.m_Name + ">";
        if (element.m_bHaveValue)
        {
            destination += element.m_Value;
        }
        else
        {
            for (SimpleDOM::Tree::const_iterator it = element.m_Children.begin();
                 it != element.m_Children.end(); ++it)
            {
                SerializeDOM(*it,destination);
            }
        }
        destination += "</" + element.m_Name + ">";
    }


    void ProcessAndSerializeParameterArrayElementDOM(const SimpleDOM::Element & element, DobParameter & parameter, int & nextIndex)
    {
        parameter.m_values.push_back(DobParameter::ParameterValue());
        parameter.m_values.back().m_isNull = false;
        parameter.m_values.back().m_valueFromParameter = false;

        bool gotIndex = false;
        for (SimpleDOM::Tree::const_iterator it = element.m_Children.begin();
             it != element.m_Children.end(); ++it)
        {
            if (it->m_Name == XmlElements::INDEX)
            {
                parameter.m_values.back().m_indexFromParameter = false;
                if (IsInt(it->m_Value.c_str()))
                {
                    parameter.m_values.back().m_index = atoi(it->m_Value.c_str());
                    nextIndex = parameter.m_values.back().m_index + 1;
                }
                else
                {
                    throw "not an integer index"; //TODO: error handling
                }

                gotIndex = true;
            }
            else if (it->m_Name == XmlElements::INDEX_REF)
            {
                parameter.m_values.back().m_indexFromParameter = true;
                SerializeDOM(*it,parameter.m_values.back().m_indexParameter);

                //we step up the index counter by one, and hope that there will be no conflict
                //maybe what we really should do is to get the parameter value and keep going
                //from there, but that is not how the class_parser does it.
                //TODO: fix the array indexref handling.
                gotIndex = false;
            }
            else if (it->m_Name == XmlElements::VALUE)
            {
                parameter.m_values.back().m_value = it->m_Value;
            }
            else if (it->m_Name == XmlElements::VALUE_REF)
            {
                parameter.m_values.back().m_valueFromParameter = true;
                SerializeDOM(*it,parameter.m_values.back().m_value);
            }
            else if (it->m_Name == XmlElements::OBJ)
            {
                SerializeDOM(*it,parameter.m_values.back().m_value);
            }
            else if (it->m_Name == XmlElements::ENTITY_ID)
            {
                parameter.m_values.back().m_valueFromParameter = false;
                SerializeDOM(*it,parameter.m_values.back().m_value);
            }
            else
            {
                throw "Error!!!";
            }
        }
        if (!gotIndex)
        {
            parameter.m_values.back().m_indexFromParameter = false;
            parameter.m_values.back().m_index = nextIndex;
            ++nextIndex;
            //TODO
        }
    }


    void ProcessAndSerializeParameterDOM(const SimpleDOM::Element & element,
                                         DobParameter & parameter)
    {
        //TODO: who fills in m_objType, and what should it be?!
        //probably typeid of object or enum, and can be filled out by finalize
        if (element.m_Name == XmlElements::VALUE)
        {
            parameter.m_values.push_back(DobParameter::ParameterValue());
            parameter.m_values.back().m_isNull = false;
            parameter.m_values.back().m_indexFromParameter = false;
            parameter.m_values.back().m_index = 0;
            parameter.m_values.back().m_valueFromParameter = false;
            parameter.m_values.back().m_value = element.m_Value;
            parameter.m_values.back().m_lineNumber=element.m_lineNumber;
        }
        else if (element.m_Name == XmlElements::VALUE_REF)
        {
            parameter.m_values.push_back(DobParameter::ParameterValue());
            parameter.m_values.back().m_isNull = false;
            parameter.m_values.back().m_indexFromParameter = false;
            parameter.m_values.back().m_index = 0;
            parameter.m_values.back().m_valueFromParameter = true;
            parameter.m_values.back().m_lineNumber=element.m_lineNumber;
            SerializeDOM(element,parameter.m_values.back().m_value);
        }
        else if (element.m_Name == XmlElements::OBJ)
        {
            parameter.m_values.push_back(DobParameter::ParameterValue());
            parameter.m_values.back().m_isNull = false;
            parameter.m_values.back().m_indexFromParameter = false;
            parameter.m_values.back().m_index = 0;
            parameter.m_values.back().m_valueFromParameter = false;
            parameter.m_values.back().m_lineNumber=element.m_lineNumber;
            SerializeDOM(element,parameter.m_values.back().m_value);
        }
        else if (element.m_Name == XmlElements::ENTITY_ID)
        {
            parameter.m_values.push_back(DobParameter::ParameterValue());
            parameter.m_values.back().m_isNull = false;
            parameter.m_values.back().m_indexFromParameter = false;
            parameter.m_values.back().m_index = 0;
            parameter.m_values.back().m_valueFromParameter = false;
            parameter.m_values.back().m_lineNumber=element.m_lineNumber;
            SerializeDOM(element,parameter.m_values.back().m_value);
        }
        else if (element.m_Name == XmlElements::ARRAY_ELEMENTS)
        {
            int nextIndex = 0;
            for (SimpleDOM::Tree::const_iterator it = element.m_Children.begin();
                 it != element.m_Children.end(); ++it)
            {
                ProcessAndSerializeParameterArrayElementDOM(*it,parameter,nextIndex);
            }
        }
        /*
        //recurse into the parameter
        parameter.m_values.back().m_value += "<" + element.m_Name + ">";

        for (SimpleDOM::Tree::const_iterator it = element.m_Children.begin();
        it != element.m_Children.end(); ++it)
        {
        ProcessAndSerializeParameterDOM(*it,parameter,false);
        }

        parameter.m_values.back().m_value += "</" + element.m_Name + ">";
        }
        else
        {
        parameter.m_values.back().m_value += "<" + element.m_Name + ">";
        if (element.m_bHaveValue)
        {
        parameter.m_values.back().m_value += element.m_Value;
        }
        else
        {
        for (SimpleDOM::Tree::const_iterator it = element.m_Children.begin();
        it != element.m_Children.end(); ++it)
        {
        ProcessAndSerializeParameterDOM(*it,parameter);
        }
        }
        parameter.m_values.back().m_value += "</" + element.m_Name + ">";
        }*/
    }

    void PropertyMappingParser::ProcessDOM()
    {
        for (DobPropertyMappings::iterator mappIt = Result().begin();
             mappIt != Result().end(); ++mappIt)
        {
            DobPropertyMapping & theMapping = *mappIt;

            /* std::wcout << "Property "
                      << theMapping.m_propertyName << "-" << theMapping.m_className
                      << " contains "
                      << static_cast<unsigned int>(theMapping.m_memberMappingsDOM.size())
                      << " member mappings" << std::endl;*/

            for (SimpleDOM::Tree::const_iterator memIt = theMapping.m_memberMappingsDOM.begin();
                 memIt != theMapping.m_memberMappingsDOM.end(); ++memIt)
            {
                std::string paddedMemberName =  memIt->m_Children.at(0).m_Value;
                paddedMemberName.resize(20);
                /*                std::wcout << " "
                          << paddedMemberName
                          << " is mapped to ";*/

                theMapping.m_mappings.push_back(Temporary::MappingMember(memIt->m_Children.at(0).m_Value));

                Temporary::MappingMember & theMappingMember = theMapping.m_mappings.back();

                if (memIt->m_Children.size() == 1)
                {
                    //  std::wcout << "NULL" << std::endl;
                    continue;
                }

                //                std::wcout << memIt->m_Children.at(1).m_Name << ": ";

                if (memIt->m_Children.at(1).m_Name == XmlElements::CLASS_MEMBER_REFERENCE)
                {
                    theMappingMember.m_kind = Temporary::ClassMemberReferenceMapping;
                    GetTemporaryClassMemberReference(memIt->m_Children.at(1),theMappingMember.m_classMemberReference);
                    //                    std::wcout << theMappingMember.m_classMemberReference << std::endl;
                }
                else if (memIt->m_Children.at(1).m_Name == XmlElements::VALUE ||
                         memIt->m_Children.at(1).m_Name == XmlElements::VALUE_REF ||
                         memIt->m_Children.at(1).m_Name == XmlElements::OBJ ||
                         memIt->m_Children.at(1).m_Name == XmlElements::ENTITY_ID ||
                         memIt->m_Children.at(1).m_Name == XmlElements::ARRAY_ELEMENTS)
                {
                    theMappingMember.m_kind = Temporary::ParameterMapping;
                    theMappingMember.m_parameter.m_name =
                        theMappingMember.m_propertyMemberName
                        + "@"
                        + theMapping.m_propertyName
                        + "-"
                        + theMapping.m_className;

                    //Type must be filled out by finalize

                    ProcessAndSerializeParameterDOM(memIt->m_Children.at(1),theMappingMember.m_parameter);
                    /*
                    std::wcout << theMappingMember.m_parameter.m_name << " = " << std::endl;
                    for (std::vector<DobParameter::ParameterValue>::iterator parIt= theMappingMember.m_parameter.m_values.begin();
                         parIt != theMappingMember.m_parameter.m_values.end(); ++parIt)
                    {
                        std::ostringstream index;
                        if (parIt->m_indexFromParameter)
                        {
                            index << parIt->m_indexParameter;
                        }
                        else
                        {
                            index << parIt->m_index;
                        }
                        std::string indexStr = index.str();
                        indexStr.resize(30,' ');
                        std::wcout << "        " << indexStr << "  " << parIt->m_value << std::endl;
                        }*/
                }
            }
        }
        //Result().clear();
    }

}
}
}
}
