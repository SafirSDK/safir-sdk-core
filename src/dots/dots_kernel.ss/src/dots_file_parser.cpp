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

#include "dots_file_parser.h"
#include <Safir/Dob/Typesystem/Internal/Id.h>
#include "dots_basic_types.h"
#include "dots_blob_layout.h"
#include "dots_error_handler.h"
#include "dots_xml_elements.h"
#include "dots_class_parser.h"
#include "dots_exception_parser.h"
#include "dots_property_parser.h"
#include "dots_property_mapping_parser.h"
#include "dots_enum_parser.h"
#include <boost/filesystem.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/convenience.hpp>

#include <iostream>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    enum DefinitionType
    {
        UnknownDefinition = 0,
        //        IllegalDefinition,
        ClassDefinition ,
        PropertyDefinition,
        MappingDefinition,
        EnumDefinition,
        ExceptionDefinition
    };

    //*******************************************************
    // XML-parser part
    //  This bit is stupid leftovers from stupid implementation
    //  of the expat interfaces. It shouldnt be that difficult
    //  to make a c++-like wrapper for expat.
    //  all the members of the struct below used to be statics
    //  but since initiation order of statics are not guaranteed
    //  they have been moved into a struct-singleton.
    // I curse whoever wrote this code originally :-)
    //*******************************************************

    struct ParsingState
    {
        static ParsingState & Instance();

        void Reset()
        {
            xmlFileName = "";
            parseError = false;
            classParser.Reset();
            ParsingState::Instance().propertyParser.Reset();
            mappingParser.Reset();
            exceptionParser.Reset();
            ParsingState::Instance().enumParser.Reset();
            definitionType = UnknownDefinition;
        }

        void Destroy()
        {
            xmlFileName = "";
            classParser = ClassParser();
            propertyParser = PropertyParser();
            mappingParser = PropertyMappingParser();
            enumParser = EnumParser();
            exceptionParser = ExceptionParser();
        }

        boost::filesystem::path xmlFileName;
        XML_Parser parser;
        bool parseError;
        ClassParser classParser;
        PropertyParser propertyParser;
        PropertyMappingParser mappingParser;
        EnumParser enumParser;
        ExceptionParser exceptionParser;
        DefinitionType definitionType;
        std::stack<bool> preserveSpace;
        std::string characterData;
    };

    //fwd decl
    static void HandleCharacters(const std::string & str);

    ParsingState & ParsingState::Instance()
    {
        static ParsingState obj;
        return obj;
    }

    void FileParser::GetFileNameAndLineNumber(boost::filesystem::path & filename, int& line)
    {
        filename = ParsingState::Instance().xmlFileName;
        if (ParsingState::Instance().parser!=NULL)
            line=XML_GetCurrentLineNumber(ParsingState::Instance().parser);
        else
            line=-1;
    }

    static void XMLCALL startElement(void * /*userData*/, const char *name, const char ** atts)
    {
        ParsingState::Instance().characterData.clear();
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


        switch (ParsingState::Instance().definitionType)
        {
        case ClassDefinition:
            if (!ParsingState::Instance().classParser.StartElement(name))
            {
                XML_StopParser(ParsingState::Instance().parser, false);
                ParsingState::Instance().parseError = true;
                return;
            }
            break;

        case PropertyDefinition:
            if (!ParsingState::Instance().propertyParser.StartElement(name))
            {
                XML_StopParser(ParsingState::Instance().parser, false);
                ParsingState::Instance().parseError = true;
                return;
            }
            break;

        case MappingDefinition:
            if (!ParsingState::Instance().mappingParser.StartElement(name))
            {
                XML_StopParser(ParsingState::Instance().parser, false);
                ParsingState::Instance().parseError = true;
                return;
            }
            break;

        case EnumDefinition:
            if (!ParsingState::Instance().enumParser.StartElement(name))
            {
                XML_StopParser(ParsingState::Instance().parser, false);
                ParsingState::Instance().parseError = true;
                return;
            }
            break;

        case ExceptionDefinition:
            if (!ParsingState::Instance().exceptionParser.StartElement(name))
            {
                XML_StopParser(ParsingState::Instance().parser, false);
                ParsingState::Instance().parseError = true;
                return;
            }
            break;

        case UnknownDefinition:
            if (eq(name, XmlElements::CLASS)) {
                ParsingState::Instance().definitionType = ClassDefinition;
                ParsingState::Instance().classParser.StartElement(name);
            }
            else if (eq(name, XmlElements::PROPERTY)) {
                ParsingState::Instance().definitionType = PropertyDefinition;
                ParsingState::Instance().propertyParser.StartElement(name);
            }
            else if (eq(name, XmlElements::PROPERTY_MAPPING)) {
                ParsingState::Instance().definitionType = MappingDefinition;
                ParsingState::Instance().mappingParser.StartElement(name);
            }
            else if (eq(name, XmlElements::ENUM)) {
                ParsingState::Instance().definitionType = EnumDefinition;
                ParsingState::Instance().enumParser.StartElement(name);
            }
            else if (eq(name, XmlElements::EXCEPTION)) {
                ParsingState::Instance().definitionType = ExceptionDefinition;
                ParsingState::Instance().exceptionParser.StartElement(name);
            }
            break;
        }
    }

    static void XMLCALL endElement(void * /*userData*/, const char *name)
    {
        //a scope so we can get some locals.
        {
            std::string & charData = ParsingState::Instance().characterData;
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


        switch (ParsingState::Instance().definitionType)
        {
        case ClassDefinition:
            if (!ParsingState::Instance().classParser.EndElement(name))
            {
                XML_StopParser(ParsingState::Instance().parser, false);
                ParsingState::Instance().parseError = true;
                return;
            }
            break;

        case PropertyDefinition:
            if (!ParsingState::Instance().propertyParser.EndElement(name))
            {
                XML_StopParser(ParsingState::Instance().parser, false);
                ParsingState::Instance().parseError = true;
                return;
            }
            break;

        case MappingDefinition:
            if (!ParsingState::Instance().mappingParser.EndElement(name))
            {
                XML_StopParser(ParsingState::Instance().parser, false);
                ParsingState::Instance().parseError = true;
                return;
            }
            break;

        case EnumDefinition:
            if (!ParsingState::Instance().enumParser.EndElement(name))
            {
                XML_StopParser(ParsingState::Instance().parser, false);
                ParsingState::Instance().parseError = true;
                return;
            }
            break;

        case ExceptionDefinition:
            if (!ParsingState::Instance().exceptionParser.EndElement(name))
            {
                XML_StopParser(ParsingState::Instance().parser, false);
                ParsingState::Instance().parseError = true;
                return;
            }
            break;

        default:
            XML_StopParser(ParsingState::Instance().parser, false);
            ParsingState::Instance().parseError = true;
            std::string descr = "Illegal unit type '";
            descr += name;
            descr += "'.";
            ErrorHandler::Error("File parser error", descr, "dots_file_parser");
            return;
        }
    }

    static void XMLCALL characters(void * /*userData*/, const XML_Char *s, int len)
    {
        std::string charData(s,s+len);
        if (!ParsingState::Instance().preserveSpace.top())
        {
            size_t startpos = charData.find_first_not_of(" \t\n");
            size_t endpos = charData.find_last_not_of(" \t\n");

            if ((std::string::npos == startpos) || (std::string::npos == endpos))
            {
                if (!ParsingState::Instance().characterData.empty() &&
                    *(ParsingState::Instance().characterData.end()-1) != ' ')
                {
                    ParsingState::Instance().characterData += ' ';
                }
                return;
            }

            if (0 != startpos &&
                !ParsingState::Instance().characterData.empty() &&
                *(ParsingState::Instance().characterData.end()-1) != ' ')
            {
                --startpos;
                charData[startpos] = ' ';
            }
            if (charData.size() - 1 != endpos)
            {
                ++endpos;
                charData[endpos] = ' ';
            }

            ParsingState::Instance().characterData +=
                charData.substr(startpos,endpos-startpos+1);
        }
        else
        {
            ParsingState::Instance().characterData += charData;
        }
    }

    static void HandleCharacters(const std::string & str)
    {
        /*int startpos = 0;
        int endpos = len-1;
        while((unsigned)s[startpos]<33 && startpos<len) startpos++;
        while((unsigned)s[endpos]<33 && endpos >= 0) endpos--;
        if (startpos>endpos)
            return;
        std::string str = "";
        for (int i = startpos; i <= endpos; i++)
        str += s[i];*/

        switch (ParsingState::Instance().definitionType)
        {
        case ClassDefinition:
            if (!ParsingState::Instance().classParser.Content(str))
            {
                XML_StopParser(ParsingState::Instance().parser, false);
                ParsingState::Instance().parseError = true;
                return;
            }
            break;

        case PropertyDefinition:
            if (!ParsingState::Instance().propertyParser.Content(str))
            {
                XML_StopParser(ParsingState::Instance().parser, false);
                ParsingState::Instance().parseError = true;
                return;
            }
            break;

        case MappingDefinition:
            if (!ParsingState::Instance().mappingParser.Content(str))
            {
                XML_StopParser(ParsingState::Instance().parser, false);
                ParsingState::Instance().parseError = true;
                return;
            }
            break;

        case EnumDefinition:
            if (!ParsingState::Instance().enumParser.Content(str))
            {
                XML_StopParser(ParsingState::Instance().parser, false);
                ParsingState::Instance().parseError = true;
                return;
            }
            break;

        case ExceptionDefinition:
            if (!ParsingState::Instance().exceptionParser.Content(str))
            {
                XML_StopParser(ParsingState::Instance().parser, false);
                ParsingState::Instance().parseError = true;
                return;
            }
            break;

        default:
            XML_StopParser(ParsingState::Instance().parser, false);
            ParsingState::Instance().parseError = true;
            std::string descr = "Illegal content '";
            descr += str;
            descr += "'.";
            ErrorHandler::Error("File parser error", descr, "dots_file_parser");
            return;
        }
    }

    //*******************************************************
    // Class FileParser
    //*******************************************************
    FileParser::FileParser():
        m_propertyParametersSize(0)
    {

    }

    FileParser::~FileParser()
    {
        ParsingState::Instance().Destroy();
    }

    bool FileParser::ParseFiles()
    {
        if (!ParseDouFiles())
            return false;
        if (!ParseDomFiles())
            return false;
        return true;
    }

    boost::filesystem::path GetDobFileDirectory()
    {
        char * env = getenv(RUNTIME_ENV);
        if (env == NULL)
        {
            ErrorHandler::Error("Dou/Dom file parsing","Failed to get environment variable SAFIR_RUNTIME","dots_file_parser");
            exit(1);
        }
        boost::filesystem::path filename(env,boost::filesystem::native);

        filename /= DOB_CLASSES_DIR;
        ENSURE(boost::filesystem::exists(filename) && boost::filesystem::is_directory(filename),
               << "The directory for dou and dom files could not be found. Using $(SAFIR_RUNTIME)/" << DOB_CLASSES_DIR << " it evaluates to " << filename.string());


        return filename;

    }

    bool FileParser::ParseDouFiles()
    {
        for (boost::filesystem::directory_iterator path = boost::filesystem::directory_iterator(GetDobFileDirectory());
             path != boost::filesystem::directory_iterator(); ++path)
        {
            const std::string extension = boost::filesystem::extension(*path);
            if (extension != UNIT_FILE_EXT)
            {
                continue;
            }

            lllout << "ParseDouFiles: Parsing file " << path->string().c_str() << std::endl;

            if (!ParseFile(*path))
            {
                return false;
            }
        }

        if (!FinalizeClasses()) return false;
        if (!FinalizeProperties()) return false;

        ParsingState::Instance().Reset();
        return true;
    }

    bool FileParser::ParseDomFiles()
    {
        for (boost::filesystem::directory_iterator path = boost::filesystem::directory_iterator(GetDobFileDirectory());
             path != boost::filesystem::directory_iterator(); ++path)
        {
            const std::string extension = boost::filesystem::extension(*path);
            if (extension != PROPERTY_MAPPING_FILE_EXT)
            {
                continue;
            }

            lllout << "ParseDomFiles: Parsing file " << path->string().c_str() << std::endl;

            ParsingState::Instance().mappingParser.SetFileName(*path);
            if (!ParseFile(*path))
            {
                return false;
            }
        }

        if (!FinalizePropertyMappings()) return false;
        ParsingState::Instance().Reset();
        return true;
    }

    bool FileParser::ParseFile(const boost::filesystem::path & filename)
    {
        ParsingState::Instance().Reset();
        FILE* stream = fopen(filename.string().c_str(), "r");
        if (stream == NULL)
        {
            std::string descr = "Failed to open file: ";
            descr += filename.string();
            ErrorHandler::Error("File parser error", descr, "dots_file_parser");
            return false;
        }
        char buf[100000]; //TODO - better buffer handling. Might crash if xml-file is more than 100000 chars.
        ParsingState::Instance().parser = XML_ParserCreate(NULL);
        ParsingState::Instance().xmlFileName=filename;
        bool done = false;
        int depth = 0;
        XML_SetUserData(ParsingState::Instance().parser, &depth);
        XML_SetElementHandler(ParsingState::Instance().parser, startElement, endElement);
        XML_SetCharacterDataHandler(ParsingState::Instance().parser, characters);
        while (!done)
        {
            size_t len = fread(buf, 1, sizeof(buf), stream);
            done = len < sizeof(buf);
            if (XML_Parse(ParsingState::Instance().parser, buf, static_cast<int>(len), done) == XML_STATUS_ERROR)
            {
                ErrorHandler::Error("File Syntax Error", "Syntax error in dou-file.", filename);
                std::wcout << "Line number: " << XML_GetCurrentLineNumber(ParsingState::Instance().parser) << std::endl;
                return !ParsingState::Instance().parseError;
            }
        }
        fclose(stream);
        XML_ParserFree(ParsingState::Instance().parser);
        ParsingState::Instance().parser=NULL;
        return !ParsingState::Instance().parseError;
    }

    //--------------------------------------
    // Finalize Properties
    //--------------------------------------
    bool FileParser::FinalizeProperties()
    {
        for (size_t i = 0; i<ParsingState::Instance().propertyParser.Result().size(); i++)
        {
            if (ParsingState::Instance().propertyParser.Result()[i].m_complex)
            {
                for (size_t j = 0; j<ParsingState::Instance().propertyParser.Result()[i].m_members.size(); j++)
                {
                    if (ParsingState::Instance().propertyParser.Result()[i].m_members[j].m_type == ObjectMemberType)
                    {

                        if (!GetIndexOfType(ParsingState::Instance().propertyParser.Result()[i].m_members[j].m_objType,
                                            ParsingState::Instance().propertyParser.Result()[i].m_members[j].m_type,
                                            ParsingState::Instance().propertyParser.Result()[i].m_members[j].m_class))
                        {
                            std::string descr = "Unknown type for property member '";
                            descr += ParsingState::Instance().propertyParser.Result()[i].m_name;
                            descr += ".";
                            descr += ParsingState::Instance().propertyParser.Result()[i].m_members[j].m_name;
                            descr += "'";
                            ErrorHandler::Error("File parser error", descr, "dots_file_parser");
                            return false;
                        }
                    }
                }
            }
        }
        return true;
    }


    //--------------------------------------
    // Utility functions for Finalize Property Mappings
    //--------------------------------------
    bool FindPropertyMember(const int propertyIndex,
                            const std::string & propertyMemberName,
                            int & propertyMemberIndex,
                            MemberType & propertyMemberType,
                            bool & propertyMemberIsArray)
    {
        for (size_t ix = 0; ix < ParsingState::Instance().propertyParser.Result()[propertyIndex].m_members.size(); ix++)
        {
            if (ParsingState::Instance().propertyParser.Result()[propertyIndex].m_members[ix].m_name == propertyMemberName)
            {
                propertyMemberIndex   = static_cast<int>(ix);
                propertyMemberType    = ParsingState::Instance().propertyParser.Result()[propertyIndex].m_members[ix].m_type;
                propertyMemberIsArray = ParsingState::Instance().propertyParser.Result()[propertyIndex].m_members[ix].m_propertyIsArray;
                return true;
            }
        }

        //not found
        std::string descr = "Could not find member '";
        descr += propertyMemberName + "' in property '";
        descr += ParsingState::Instance().propertyParser.Result()[propertyIndex].m_name + "'.";
        ErrorHandler::Error("Property Mapping Error", descr, "dots_file_parser");
        return false;
    }

    const DobMember * const FindClassMember(const int classIndex,const Temporary::MemberReferenceElement refElement, int & classMember)
    {
        classMember = -1;
        DobClass & dc = ParsingState::Instance().classParser.Result()[classIndex];
        for (size_t ix = 0; ix < dc.m_members.size(); ++ix)
        {
            if (dc.m_members[ix].m_name == refElement.m_classMember)
            {
                classMember = static_cast<int>(ix) + dc.m_noInheritedMembers;

                return &dc.m_members[ix];
            }
        }

        if (dc.m_baseClassIndex != -1)
        {
            return FindClassMember(dc.m_baseClassIndex,refElement,classMember);
        }
        //didn't find the class member;
        return NULL;
    }

    bool IsOfType(const std::string & value, const MemberType type)
    {
        switch (type)
        {
        case BooleanMemberType:
            return value == "true" || value == "True" || value == "TRUE" || value == "false" || value == "False" || value == "FALSE";

        case EnumerationMemberType:
            throw InternalException("This method cannot handle enumerations!",__FILE__,__LINE__);

        case Int32MemberType:
        case Int64MemberType:
        case TypeIdMemberType:
            return IsInt(value.c_str());

        case StringMemberType:
            return true;

        case EntityIdMemberType:
        case ObjectMemberType:
            return false;

        case InstanceIdMemberType:
        case ChannelIdMemberType:
        case HandlerIdMemberType:
            throw InternalException("This method cannot handle hashed types!",__FILE__,__LINE__);
            //TODO: is this needed for ids?

        default: //everything else is float!
            return IsFloat(value.c_str());
        }
    }
    //--------------------------------------
    // Finalize Property Mappings
    //--------------------------------------
    bool FileParser::FinalizePropertyMappings()
    {
        /*
        //a little piece of code to write out all parameters from the classes
        for (stdext::hash_map<std::string, DobParameter>::iterator it = ParsingState::Instance().classParser.m_allParameters.begin();
             it != ParsingState::Instance().classParser.m_allParameters.end(); ++it)
        {
           // if (it->first[0] == 'M')
            {
                std::wcout << it->first << ": " << std::endl;
                for (std::vector<DobParameter::ParameterValue>::iterator it2 = it->second.m_values.begin();
                    it2 != it->second.m_values.end(); ++it2)
                {
                    if (it2->m_indexFromParameter)
                    {
                        std::wcout << "  "<< it2->m_indexParameter;
                    }
                    else
                    {
                        std::wcout << "  "<< it2->m_index;
                    }
                    std::wcout <<"   " << it2->m_value <<std::endl;
                }
            }
        }
        */
        //        std::wcout << "PROCESSING DOM" << std::endl;
        ParsingState::Instance().mappingParser.ProcessDOM();

        //        std::wcout << "FINALIZING MAPPINGS" << std::endl;

        for (DobPropertyMappings::iterator mappIt = ParsingState::Instance().mappingParser.Result().begin();
             mappIt != ParsingState::Instance().mappingParser.Result().end(); ++mappIt)
        {
            DobPropertyMapping & theMapping = *mappIt;

            //Check that the class exists and fill in the classIndex field in the mapping
            theMapping.m_classIndex = ParsingState::Instance().classParser.GetIndex(theMapping.m_classTypeId);
            if (theMapping.m_classIndex<0)
            {
                std::string descr = "Unknown class specified in property mapping. Class: '";
                descr += theMapping.m_className+"' Property '";
                descr += theMapping.m_propertyName+"'.";
                ErrorHandler::Error("File Parser Error", descr, "dots_file_parser");
                return false;
            }
            //Check that the property exists and fill in the propertyIndex field in the mapping
            theMapping.m_propertyIndex = ParsingState::Instance().propertyParser.GetIndex(theMapping.m_propertyTypeId);
            if (theMapping.m_propertyIndex<0)
            {
                std::string descr = "Unknown property specified in property mapping. Class: '";
                descr += theMapping.m_className+"' Property '";
                descr += theMapping.m_propertyName+"'.";
                ErrorHandler::Error("File Parser Error", descr, "dots_file_parser");
                return false;
            }

            for (Temporary::MappingMembers::iterator memIt = theMapping.m_mappings.begin();
                 memIt != theMapping.m_mappings.end(); ++memIt)
            {
                Temporary::MappingMember & theMappingMember = *memIt;

                MemberType propertyMemberType;
                bool propertyMemberIsArray;

                if (!FindPropertyMember(theMapping.m_propertyIndex,
                                        theMappingMember.m_propertyMemberName,
                                        theMappingMember.m_propertyMemberIndex,
                                        propertyMemberType,
                                        propertyMemberIsArray))
                {
                    //TODO: print error message
                    return false;
                }

                switch (theMappingMember.m_kind)
                {
                case Temporary::NullMapping:
                    break;

                case Temporary::ClassMemberReferenceMapping:
                    {
                        int classIndex = theMapping.m_classIndex;
                        for (Temporary::ClassMemberReference::const_iterator refElIt = theMappingMember.m_classMemberReference.begin();
                             refElIt != theMappingMember.m_classMemberReference.end(); ++refElIt)
                        {
                            theMappingMember.m_binaryClassMemberReference.push_back(Temporary::BinaryMemberReferenceElement());
                            Temporary::BinaryMemberReferenceElement & theElement = theMappingMember.m_binaryClassMemberReference.back();
                            theElement.m_index = ParameterIndexInternal(refElIt->m_index);

                            int classMember = 0;
                            const DobMember * const member = FindClassMember(classIndex,*refElIt,classMember);
                            if (member != NULL)
                            {
                                theElement.m_classMember = classMember;

                                if (theElement.m_index >= member->m_arrayLength)
                                {
                                    std::string descr = "Invalid array index for element '";
                                    descr += member->m_name;
                                    descr += "' in property '";
                                    descr += theMapping.m_className + "-" + theMapping.m_propertyName + "'.";
                                    ErrorHandler::Error("Property Mapping Error", descr, "dots_file_parser");
                                    return false;
                                }

                                //if we're still working through the reference
                                if (refElIt + 1 != theMappingMember.m_classMemberReference.end())
                                {
                                    if (member->m_type != ObjectMemberType)
                                    {
                                        std::string descr = "Element '";
                                        descr += member->m_name;
                                        descr += "' is not an object, in property '";
                                        descr += theMapping.m_className + "-" + theMapping.m_propertyName + "'.";
                                        ErrorHandler::Error("Property Mapping Error", descr, "dots_file_parser");
                                        return false;
                                    }

                                    classIndex = member->m_class;
                                }
                                else //if we're at the last element of the reference
                                {
                                    if (member->m_type != propertyMemberType)
                                    {
                                        std::string descr = "Invalid type for element '";
                                        descr += member->m_name;
                                        descr += "' in property '";
                                        descr += theMapping.m_className + "-" + theMapping.m_propertyName + "'.";
                                        ErrorHandler::Error("Property Mapping Error", descr, "dots_file_parser");
                                        return false;
                                    }
                                    if (propertyMemberIsArray && theElement.m_index != 0)
                                    {
                                        std::string descr = "Element '";
                                        descr += member->m_name;
                                        descr += "' is not an array, in property '";
                                        descr += theMapping.m_className + "-" + theMapping.m_propertyName + "'.";
                                        ErrorHandler::Error("Property Mapping Error", descr, "dots_file_parser");
                                        return false;
                                    }
                                    if (propertyMemberIsArray)
                                    {
                                        theElement.m_index = -1;
                                    }
                                }

                            }
                            else
                            { //no such member
                                std::string descr = "Member '";
                                descr += refElIt->m_classMember;//member->m_name;
                                descr += "' not found in class, in property '";
                                descr += theMapping.m_className + "-" + theMapping.m_propertyName + "'.";
                                ErrorHandler::Error("Property Mapping Error", descr, "dots_file_parser");
                                return false;
                            }
                        }
                    }
                    break;

                case Temporary::ParameterMapping:
                    {
                        //set the type
                        theMappingMember.m_parameter.m_type = propertyMemberType;

                        if (propertyMemberType == EnumerationMemberType ||
                            propertyMemberType == ObjectMemberType)
                        {
                            theMappingMember.m_parameter.m_objType =
                                ParsingState::Instance().propertyParser.Result()[theMapping.m_propertyIndex].
                                m_members[theMappingMember.m_propertyMemberIndex].m_objType;
                        }
                        else
                        {
                            theMappingMember.m_parameter.m_objType = 0;
                        }

                        //calculate the needed blob memory for mapping parameters
                        if (propertyMemberType == StringMemberType)
                        {
                            for (std::vector<DobParameter::ParameterValue>::iterator parIt =
                                     theMappingMember.m_parameter.m_values.begin();
                                 parIt != theMappingMember.m_parameter.m_values.end(); ++parIt)
                            {
                                if (!parIt->m_valueFromParameter)
                                {
                                    m_propertyParametersSize += parIt->m_value.size() + 1;
                                }
                            }
                        }
                        if (propertyMemberType == InstanceIdMemberType ||
                            propertyMemberType == ChannelIdMemberType ||
                            propertyMemberType == HandlerIdMemberType||
                            propertyMemberType == EntityIdMemberType)
                        {
                            for (std::vector<DobParameter::ParameterValue>::iterator parIt =
                                     theMappingMember.m_parameter.m_values.begin();
                                 parIt != theMappingMember.m_parameter.m_values.end(); ++parIt)
                            {
                                if (!parIt->m_valueFromParameter)
                                {
                                    if (!IsInt(parIt->m_value.c_str()))
                                    {
                                        m_propertyParametersSize += BasicTypes::SizeOfType(propertyMemberType) + sizeof(Int32) +
                                            parIt->m_value.size() +1;
                                    }
                                }
                            }
                        }

                        m_propertyParametersSize += BasicTypes::SizeOfType(propertyMemberType) *
                            theMappingMember.m_parameter.m_values.size();
                    }
                    break;
                }

            }

            //Check that all property members have been mapped once and only once. Also sort them after propertyMemberIndex
            for (size_t prop = 0; prop < ParsingState::Instance().propertyParser.Result()[theMapping.m_propertyIndex].m_members.size(); ++prop)
            {
                int found = 0;
                for (size_t ii = prop; ii < theMapping.m_mappings.size(); ii++)
                {
                    if (theMapping.m_mappings[ii].m_propertyMemberIndex == static_cast<long>(prop))
                    {
                        ++found;
                        if (found == 1) //only swap the first one, if there are more it is an error anyway.
                        {
                            std::swap(theMapping.m_mappings[prop],theMapping.m_mappings[ii]);
                        }
                    }
                }
                if (found == 0) //not found
                {
                    std::string descr = "Could not find mapping for property member '";
                    descr += ParsingState::Instance().propertyParser.Result()[theMapping.m_propertyIndex].m_members[prop].m_name;
                    descr += "'\n    in property '";
                    descr += theMapping.m_className + "-" + theMapping.m_propertyName + "'.";
                    ErrorHandler::Error("Property Mapping Error", descr, "dots_file_parser");
                    return false;
                }
                else if (found > 1) //more than one found
                {
                    std::string descr = "Found several mappings for property member '";
                    descr += ParsingState::Instance().propertyParser.Result()[theMapping.m_propertyIndex].m_members[prop].m_name;
                    descr += "' in property '";
                    descr += theMapping.m_propertyName + "-" + theMapping.m_className + "'.";
                    //descr += ParsingState::Instance().propertyParser.Result()[theMapping.m_propertyIndex].m_name+"'.";
                    ErrorHandler::Error("Property Mapping Error", descr, "dots_file_parser");
                    return false;
                }
            }
            ParsingState::Instance().classParser.Result()[theMapping.m_classIndex].m_propertyMappings.push_back( theMapping );
        }

        //        std::wcout << "MAPPINGS FINALIZED" << std::endl;
        return true;
    }

    //--------------------------------------
    // Finalize Classes
    //--------------------------------------
    bool FileParser::FinalizeClasses()
    {
        if (!SetBaseClasses())
            return false;

        if (!SetClassSizes())
            return false;

        if (!ResolveParameterReferences())
            return false;

        return true;
    }

    bool FileParser::ResolveParameterReferences()
    {
        bool success = true;

        for (size_t i = 0; i<ParsingState::Instance().classParser.Result().size(); i++)
        {
            //resolve other parameter references while we're at it...
            for (DobParameters::iterator par = ParsingState::Instance().classParser.Result()[i].m_parameters.begin();
                par != ParsingState::Instance().classParser.Result()[i].m_parameters.end(); ++par)
            {
                for (std::vector<DobParameter::ParameterValue>::iterator it = par->m_values.begin();
                    it != par->m_values.end(); ++it)
                {
                    if (it->m_indexFromParameter)
                    {
                        //std::wcout << "Parameter " << ParsingState::Instance().classParser.Result()[i].m_name << "." << par->m_name << "[" << std::distance(par->m_values.begin(),it) << "]"
                        //    << " is indexRef to " << it->m_value << std::endl;
                        std::string parameterName;
                        size_t arrIndex = 0;
                        ParseParameter(it->m_indexParameter, parameterName, arrIndex);

                        ClassParser::ParameterHashTable::iterator findIt =
                            ParsingState::Instance().classParser.m_allParameters.find(parameterName);
                        if (findIt == ParsingState::Instance().classParser.m_allParameters.end())
                        {
                            std::string desc = "Invalid value or unknown parameter. Class '";
                            desc += ParsingState::Instance().classParser.Result()[i].m_name;
                            desc += ".";
                            desc += par->m_name;
                            desc += "' contains invalid ref for value. valueRef = ";
                            desc += it->m_indexParameter;
                            ErrorHandler::Error("File parser error", desc, "dots_file_parser");
                            success = false;
                            continue;
                        }
                        else if (arrIndex<0 || arrIndex >= findIt->second.m_values.size())
                        {
                            std::string desc = "Parameter array index for array size out of bounds. Parameter '";
                            desc += ParsingState::Instance().classParser.Result()[i].m_name;
                            desc += "'";
                            ErrorHandler::Error("File parser error", desc, "dots_file_parser");
                            success = false;
                            continue;
                        }
                        if (!IsInt(findIt->second.m_values[arrIndex].m_value.c_str(),true))
                        {
                            std::string desc = "Type mismatch. Parameter '";
                            desc += ParsingState::Instance().classParser.Result()[i].m_name;
                            desc += ".";
                            desc += par->m_name;
                            desc += "' contains invalid ref for value. valueRef = ";
                            desc += it->m_indexParameter;
                            ErrorHandler::Error("File parser error", desc, "dots_file_parser");
                            success = false;
                            continue;
                        }
                        if (findIt->second.m_values[arrIndex].m_valueFromParameter)
                        {
                            std::string desc = "Reference to reference that will not be resolved. Parameter '";
                            desc += ParsingState::Instance().classParser.Result()[i].m_name;
                            desc += ".";
                            desc += par->m_name;
                            desc += "' valueRef = ";
                            desc += it->m_indexParameter;
                            ErrorHandler::Error("File parser error", desc, "dots_file_parser");
                            success = false;
                            continue;
                        }

                        it->m_indexParameter = findIt->second.m_values[arrIndex].m_value;
                        it->m_indexFromParameter = false;
                    }

                    if (it->m_valueFromParameter)
                    {
                        //std::wcout << "Parameter " << ParsingState::Instance().classParser.Result()[i].m_name << "." << par->m_name << "[" << std::distance(par->m_values.begin(),it) << "]"
                        //    << " is valueRef to " << it->m_value << std::endl;

                        std::string parameterName;
                        size_t arrIndex = 0;
                        ParseParameter(it->m_value, parameterName, arrIndex);

                        ClassParser::ParameterHashTable::iterator findIt =
                            ParsingState::Instance().classParser.m_allParameters.find(parameterName);
                        if (findIt == ParsingState::Instance().classParser.m_allParameters.end())
                        {
                            std::string desc = "Invalid value or unknown parameter. Class '";
                            desc += ParsingState::Instance().classParser.Result()[i].m_name;
                            desc += ".";
                            desc += par->m_name;
                            desc += "' contains invalid ref for value. valueRef = ";
                            desc += it->m_value;
                            ErrorHandler::Error("File parser error", desc, "dots_file_parser");
                            success = false;
                            continue;
                        }
                        else if (arrIndex<0 || arrIndex >= findIt->second.m_values.size())
                        {
                            std::string desc = "Parameter array index for array size out of bounds. Parameter '";
                            desc += ParsingState::Instance().classParser.Result()[i].m_name;
                            desc += "'";
                            ErrorHandler::Error("File parser error", desc, "dots_file_parser");
                            success = false;
                            continue;
                        }
                        if (par->m_type != findIt->second.m_type)
                        {
                            if (findIt->second.m_type == StringMemberType &&
                                (par->m_type == InstanceIdMemberType ||
                                 par->m_type == ChannelIdMemberType ||
                                 par->m_type == HandlerIdMemberType))
                            {
                                lllout << "We have an ok hash to string reference" << std::endl;
                            }
                            else if (par->m_type == EnumerationMemberType &&
                                findIt->second.m_type == ObjectMemberType &&
                                par->m_objType == findIt->second.m_objType)
                            {
                                lllout << "We have an ok enumeration reference" << std::endl;
                            }
                            else
                            {
                                std::string desc = "Type mismatch. Parameter '";
                                desc += ParsingState::Instance().classParser.Result()[i].m_name;
                                desc += ".";
                                desc += par->m_name;
                                desc += "' is not of the same type as '";
                                desc += it->m_value;
                                desc += "'";
                                ErrorHandler::Error("File parser error", desc, "dots_file_parser");
                                success = false;
                                continue;
                            }
                        }
                        if (findIt->second.m_values[arrIndex].m_valueFromParameter)
                        {
                            std::string desc = "Reference to reference that will not be resolved. Parameter '";
                            desc += ParsingState::Instance().classParser.Result()[i].m_name;
                            desc += ".";
                            desc += par->m_name;
                            desc += "' valueRef = ";
                            desc += it->m_indexParameter;
                            ErrorHandler::Error("File parser error", desc, "dots_file_parser");
                            success = false;
                            continue;
                        }
                        it->m_value = findIt->second.m_values[arrIndex].m_value;
                        it->m_valueFromParameter = false;
                        /*
                        if (IsInt(it->second.m_values[arrIndex].m_value.c_str(), true))
                        {
                            ParsingState::Instance().classParser.Result()[i].m_members[mi].m_arrayLength = static_cast<unsigned short>(atoi(it->second.m_values[arrIndex].m_value.c_str()));
                        }
                        else
                        {
                            std::string desc = "Type mismatch. Class '";
                            desc += ParsingState::Instance().classParser.Result()[i].m_name;
                            desc += ".";
                            desc += ParsingState::Instance().classParser.Result()[i].m_members[mi].m_name;
                            desc += "' contains invalid value for array size. ArraySize = ";
                            desc += it->second.m_values[arrIndex].m_value.c_str();
                            ErrorHandler::Error("File parser error", desc, "dots_file_parser");
                            throw desc.c_str();
                        }
*/
                    }
                }
            }
        }
        return success;
    }

    bool FileParser::SetBaseClasses()
    {
        const TypeId noBaseClass = DotsId_Generate64(NULL_CLASS);
        const TypeId objectTypeId = DotsId_Generate64(OBJECT_CLASS);
        for (size_t i = 0; i<ParsingState::Instance().classParser.Result().size(); i++)
        {
            //Handle complex parameters
            for (size_t cix = 0; cix<ParsingState::Instance().classParser.Result()[i].m_parameters.size(); cix++)
            {
                if (ParsingState::Instance().classParser.Result()[i].m_parameters[cix].m_type == ObjectMemberType)
                {
                    int dummy=0;
                    if (!GetIndexOfType(ParsingState::Instance().classParser.Result()[i].m_parameters[cix].m_objType,
                                        ParsingState::Instance().classParser.Result()[i].m_parameters[cix].m_type,
                                        dummy))
                    {
                        std::string descr = "Unknown type of parameter '";
                        descr += ParsingState::Instance().classParser.Result()[i].m_parameters[cix].m_name;
                        descr += "'";
                        ErrorHandler::Error("File parser error", descr, "dots_file_parser");
                        return false;
                    }
                }
            }

            bool found = false;
            for (size_t j = 0; j<ParsingState::Instance().classParser.Result().size(); j++)
            {
                if (i != j && ParsingState::Instance().classParser.Result()[i].m_baseClassTypeId == ParsingState::Instance().classParser.Result()[j].m_typeId)
                {
                    found = true;
                    ParsingState::Instance().classParser.Result()[i].m_baseClassIndex = static_cast<int>(j);
                    ParsingState::Instance().classParser.Result()[j].m_desc.push_back(static_cast<int>(i));
                    break;
                }
            }
            if (ParsingState::Instance().classParser.Result()[i].m_baseClassTypeId == noBaseClass && ParsingState::Instance().classParser.Result()[i].m_typeId == objectTypeId)
            {
                ParsingState::Instance().classParser.Result()[i].m_baseClassIndex = -1;
                found = true;
            }
            if (!found)
            {
                std::string descr = "Could not find base class '";
                descr += ParsingState::Instance().classParser.Result()[i].m_baseClassName;
                descr += "' for class '";
                descr += ParsingState::Instance().classParser.Result()[i].m_name;
                ErrorHandler::Error("File parser error", descr, "dots_file_parser");
                return false;
            }
        }
        for (size_t i = 0; i<ParsingState::Instance().classParser.Result().size(); i++)
        {
            DobClass dc = ParsingState::Instance().classParser.Result()[i];
            bool reachedObject = false;
            for (size_t j = 0; j<ParsingState::Instance().classParser.Result().size(); j++)
            {
                if (dc.m_typeId == objectTypeId)
                {
                    reachedObject = true;
                    break;
                }
                else
                {
                    dc = ParsingState::Instance().classParser.Result()[dc.m_baseClassIndex];
                }
            }
            if (!reachedObject)
            {
                std::string descr = "Circular inheritance '";
                descr += ParsingState::Instance().classParser.Result()[i].m_name;
                descr += "'";
                ErrorHandler::Error("File parser error", descr, "dots_file_parser");
                return false;
            }
        }
        return true;
    }

    int FileParser::ParameterIndexInternal(const std::string& name)
    {
        if (name.empty())
        {
            return 0;
        }

        if (IsInt(name.c_str()))
        {
            return atoi(name.c_str());
        }


        size_t arrStart = name.find_first_of('[');
        size_t arrEnd = name.find_last_of(']');
        std::string param;
        int index = 0;

        if (arrStart != std::string::npos && arrEnd != std::string::npos) //has index
        {
            param = name.substr(0, arrStart);
            index = ParameterIndexInternal(name.substr(arrStart+1, arrEnd-arrStart-1));
        }
        else
        {
            param = name;
        }

        ClassParser::ParameterHashTable::iterator it = ParsingState::Instance().classParser.m_allParameters.find(param);

        if (it == ParsingState::Instance().classParser.m_allParameters.end())
        {
            std::string desc = "Invalid value or unknown parameter. Class '";
            desc += name;
            desc += "' ";
            ErrorHandler::Error("File parser error", desc, "dots_file_parser");
            throw desc.c_str();
        }
        else if (index<0 || index >= static_cast<int>(it->second.m_values.size()))
        {
            std::string desc = "Parameter array index out of bounds. Parameter '";
            desc += name+"'";
            ErrorHandler::Error("File parser error", desc, "dots_file_parser");
            throw desc.c_str();
        }
        if (it->second.m_values[index].m_valueFromParameter)
        {
            std::string desc = "Reference to reference that will not be resolved. Parameter '";
            desc += name;
            ErrorHandler::Error("File parser error", desc, "dots_file_parser");
            throw desc.c_str();
        }
        if (IsInt(it->second.m_values[index].m_value.c_str(), true))
        {
            return atoi(it->second.m_values[index].m_value.c_str());
        }
        else
        {
            std::string desc = "Type mismatch for index parameter '";
            desc += name+"'";
            ErrorHandler::Error("File parser error", desc, "dots_file_parser");
            throw desc.c_str();
        }
    }

    void FileParser::ParseParameter(const std::string text, std::string& name, size_t& index)
    {
        size_t arrStart = text.find_first_of('[');
        size_t arrEnd = text.find_last_of(']');
        name = text;
        index = 0;
        if (arrStart != std::string::npos && arrEnd != std::string::npos)
        {
            try
            {
                std::string arrIx = text.substr(arrStart+1, arrEnd-arrStart-1);
                index = ParameterIndexInternal(arrIx);
                name = name.substr(0, arrStart);
            }
            catch(...)
            {
                std::string desc = "Invalid array index for parameter. ";
                desc += text;
                ErrorHandler::Error("File parser error", desc, "dots_file_parser");
                throw desc.c_str();
            }
        }
    }

    unsigned int FileParser::SizeOfClass(int i)
    {
        if (ParsingState::Instance().classParser.Result()[i].m_sizeCalculated)
        {
            return ParsingState::Instance().classParser.Result()[i].m_initialSize;
        }

        if (ParsingState::Instance().classParser.Result()[i].m_baseClassIndex >= 0)
        {
            ParsingState::Instance().classParser.Result()[i].m_initialSize = SizeOfClass(ParsingState::Instance().classParser.Result()[i].m_baseClassIndex);
            ParsingState::Instance().classParser.Result()[i].m_thisClassSize = 0;
            ParsingState::Instance().classParser.Result()[i].m_noInheritedMembers = ParsingState::Instance().classParser.Result()[ParsingState::Instance().classParser.Result()[i].m_baseClassIndex].m_noInheritedMembers+
                ParsingState::Instance().classParser.Result()[ParsingState::Instance().classParser.Result()[i].m_baseClassIndex].m_members.size();
            ParsingState::Instance().classParser.Result()[i].m_noInheritedParameters = ParsingState::Instance().classParser.Result()[ParsingState::Instance().classParser.Result()[i].m_baseClassIndex].m_noInheritedParameters+
                ParsingState::Instance().classParser.Result()[ParsingState::Instance().classParser.Result()[i].m_baseClassIndex].m_parameters.size();
        }
        else //only Safir.Dots.Object goes here
        {
            ParsingState::Instance().classParser.Result()[i].m_initialSize = BlobLayout::OFFSET_HEADER_LENGTH;
            ParsingState::Instance().classParser.Result()[i].m_thisClassSize = BlobLayout::OFFSET_HEADER_LENGTH;
            ParsingState::Instance().classParser.Result()[i].m_noInheritedMembers = 0;
            ParsingState::Instance().classParser.Result()[i].m_noInheritedParameters = 0;
        }
        unsigned int size = 0;
        for (size_t mi = 0; mi<ParsingState::Instance().classParser.Result()[i].m_members.size(); mi++)
        {
            Int32 tmpSize = BlobLayout::MEMBER_STATUS_LENGTH;
            if (ParsingState::Instance().classParser.Result()[i].m_members[mi].m_type == StringMemberType)
            {
                if (ParsingState::Instance().classParser.Result()[i].m_members[mi].m_strLenFromParameter)
                {
                    std::string parameterName;
                    size_t arrIndex = 0;
                    ParseParameter(ParsingState::Instance().classParser.Result()[i].m_members[mi].m_strLenParameter, parameterName, arrIndex);

                    ClassParser::ParameterHashTable::iterator it;
                    it = ParsingState::Instance().classParser.m_allParameters.find(parameterName);
                    if (it == ParsingState::Instance().classParser.m_allParameters.end())
                    {
                        std::string desc = "Invalid value or unknown parameter. Class '";
                        desc += ParsingState::Instance().classParser.Result()[i].m_name;
                        desc += ".";
                        desc += ParsingState::Instance().classParser.Result()[i].m_members[mi].m_name;
                        desc += "' contains invalid value for string length. StringLength = ";
                        desc += ParsingState::Instance().classParser.Result()[i].m_members[mi].m_strLenParameter;
                        ErrorHandler::Error("File parser error", desc, "dots_file_parser");
                        throw desc.c_str();
                    }
                    else if (arrIndex<0 || arrIndex >= it->second.m_values.size())
                    {
                        std::string desc = "Parameter array index for string length out of bounds. Parameter '";
                        desc += ParsingState::Instance().classParser.Result()[i].m_members[mi].m_strLenParameter;
                        desc += "'";
                        ErrorHandler::Error("File parser error", desc, "dots_file_parser");
                        throw desc.c_str();
                    }
                    if (IsInt(it->second.m_values[arrIndex].m_value.c_str(), true))
                    {
                        ParsingState::Instance().classParser.Result()[i].m_members[mi].m_dataLength = boost::lexical_cast<Int32>(it->second.m_values[arrIndex].m_value);
                    }
                    else
                    {
                        std::string desc = "Type mismatch. Class '";
                        desc += ParsingState::Instance().classParser.Result()[i].m_name;
                        desc += ".";
                        desc += ParsingState::Instance().classParser.Result()[i].m_members[mi].m_name;
                        desc += "' contains invalid value for string length. StringLength = ";
                        desc += it->second.m_values[arrIndex].m_value.c_str();
                        ErrorHandler::Error("File parser error", desc, "dots_file_parser");
                        throw desc.c_str();
                    }
                }
            }

            //Warning!!!! The GetIndexOfType below actually modifies the m_type field if it is an enumeration!
            if (ParsingState::Instance().classParser.Result()[i].m_members[mi].m_type == ObjectMemberType)
            {
                if (!GetIndexOfType(    ParsingState::Instance().classParser.Result()[i].m_members[mi].m_objType,
                                        ParsingState::Instance().classParser.Result()[i].m_members[mi].m_type,
                                        ParsingState::Instance().classParser.Result()[i].m_members[mi].m_class))
                {
                    std::string descr = "Unknown type for class member '";
                    descr += ParsingState::Instance().classParser.Result()[i].m_name;
                    descr += ".";
                    descr += ParsingState::Instance().classParser.Result()[i].m_members[mi].m_name;
                    descr += "'";
                    ErrorHandler::Error("File parser error", descr, "dots_file_parser");
                    throw descr.c_str();
                }
            }
            tmpSize += Safir::Dob::Typesystem::Internal::BasicTypes::SizeOfType(ParsingState::Instance().classParser.Result()[i].m_members[mi].m_type);

            if (ParsingState::Instance().classParser.Result()[i].m_members[mi].m_arrSizeFromParameter)
            {
                std::string parameterName;
                size_t arrIndex = 0;
                ParseParameter(ParsingState::Instance().classParser.Result()[i].m_members[mi].m_arrSizeParameter, parameterName, arrIndex);

                ClassParser::ParameterHashTable::iterator it;
                it = ParsingState::Instance().classParser.m_allParameters.find(parameterName);
                if (it == ParsingState::Instance().classParser.m_allParameters.end())
                {
                    std::string desc = "Invalid value or unknown parameter. Class '";
                    desc += ParsingState::Instance().classParser.Result()[i].m_name;
                    desc += ".";
                    desc += ParsingState::Instance().classParser.Result()[i].m_members[mi].m_name;
                    desc += "' contains invalid value for array size. ArraySize = ";
                    desc += ParsingState::Instance().classParser.Result()[i].m_members[mi].m_arrSizeParameter;
                    ErrorHandler::Error("File parser error", desc, "dots_file_parser");
                    throw desc.c_str();
                }
                else if (arrIndex<0 || arrIndex >= it->second.m_values.size())
                {
                    std::string desc = "Parameter array index for array size out of bounds. Parameter '";
                    desc += ParsingState::Instance().classParser.Result()[i].m_members[mi].m_arrSizeParameter;
                    desc += "'";
                    ErrorHandler::Error("File parser error", desc, "dots_file_parser");
                    throw desc.c_str();
                }
                if (IsInt(it->second.m_values[arrIndex].m_value.c_str(), true))
                {
                    ParsingState::Instance().classParser.Result()[i].m_members[mi].m_arrayLength = boost::lexical_cast<Int32>(it->second.m_values[arrIndex].m_value);
                }
                else
                {
                    std::string desc = "Type mismatch. Class '";
                    desc += ParsingState::Instance().classParser.Result()[i].m_name;
                    desc += ".";
                    desc += ParsingState::Instance().classParser.Result()[i].m_members[mi].m_name;
                    desc += "' contains invalid value for array size. ArraySize = ";
                    desc += it->second.m_values[arrIndex].m_value.c_str();
                    ErrorHandler::Error("File parser error", desc, "dots_file_parser");
                    throw desc.c_str();
                }
            }

            tmpSize *= ParsingState::Instance().classParser.Result()[i].m_members[mi].m_arrayLength;
            size += (tmpSize+BlobLayout::OFFSET_MEMBER_LENGTH);
        }
        ParsingState::Instance().classParser.Result()[i].m_initialSize += size;
        if (ParsingState::Instance().classParser.Result()[i].m_baseClassIndex >= 0) //not Safir.Dob.Typesystem.Object
        {
            ParsingState::Instance().classParser.Result()[i].m_thisClassSize = size;
        }
        ParsingState::Instance().classParser.Result()[i].m_sizeCalculated = true;
        return ParsingState::Instance().classParser.Result()[i].m_initialSize;
    }

    bool FileParser::SetClassSizes()
    {
        try
        {
            for (size_t i = 0; i<ParsingState::Instance().classParser.Result().size(); i++)
            {
                SizeOfClass(static_cast<int>(i));
            }
            return true;
        }
        catch(...)
        {
            return false;
        }
    }

    //Index of a type
    bool FileParser::GetIndexOfType(TypeId tid, MemberType& mt, int& index)
    {
        index = ParsingState::Instance().classParser.GetIndex(tid);
        if (index >= 0)
        {
            mt = ObjectMemberType;
            return true;
        }

        index = ParsingState::Instance().enumParser.GetIndex(tid);
        if (index >= 0)
        {
            mt = EnumerationMemberType;
            return true;
        }

        return false;
    }

    //-----------------------------------------
    // Results
    //-----------------------------------------
    const DobClasses& FileParser::ResultClasses() const
    {
        return ParsingState::Instance().classParser.Result();
    }

    const DobProperties& FileParser::ResultProperties() const
    {
        return ParsingState::Instance().propertyParser.Result();
    }

    const DobEnumerations& FileParser::ResultEnums() const
    {
        return ParsingState::Instance().enumParser.Result();
    }

    const DobExceptions& FileParser::ResultExceptions() const
    {
        return ParsingState::Instance().exceptionParser.Result();
    }

    size_t FileParser::ParameterSize()
    {
        return ParsingState::Instance().classParser.GetParameterBlobSize() + m_propertyParametersSize;
    }

    void FileParser::DumpClass(DobClass& c)
    {
        std::wcout<<std::endl;
        std::wcout<<"class "<<c.m_name.c_str()<<" extends "<<c.m_baseClassName.c_str()<<std::endl;
        std::wcout<<"   typeId: "<<c.m_typeId<<""<<std::endl;
        std::wcout<<"   initialSize: "<<c.m_initialSize<<std::endl;
        std::wcout<<"   inheritedEl: "<<c.m_noInheritedMembers<<std::endl;
        std::wcout<<"   members:"<<std::endl;
        for (size_t j = 0; j<c.m_members.size(); j++)
        {
            std::wcout<<"\t"<<Safir::Dob::Typesystem::Internal::BasicTypes::StringOf(c.m_members[j].m_type);
            std::wcout<<" "<<c.m_members[j].m_name.c_str();
            if (c.m_members[j].m_type == StringMemberType)
                std::wcout<<"  length = "<<c.m_members[j].m_dataLength;
            if (c.m_members[j].m_arrayLength>1)
                std::wcout<<"  arraySize = "<<c.m_members[j].m_arrayLength;
            std::wcout<<std::endl;
        }
        std::wcout<<"   inheritedParameters: "<<c.m_noInheritedParameters<<std::endl;
        std::wcout<<"   parameters:"<<std::endl;
        for (size_t j = 0; j<c.m_parameters.size(); j++)
        {
            std::wcout<<"\t"<<Safir::Dob::Typesystem::Internal::BasicTypes::StringOf(c.m_parameters[j].m_type);
            std::wcout<<" "<<c.m_parameters[j].m_name.c_str();
            for (size_t cix = 0; cix<c.m_parameters[j].m_values.size(); cix++)
                std::wcout<<" = "<<c.m_parameters[j].m_values[cix].m_value.c_str()<<std::endl;
        }

        std::wcout<<"   propertyMappings:"<<std::endl;
        for (size_t j = 0; j<c.m_propertyMappings.size(); j++)
        {
            std::wcout<<"\t"<<c.m_propertyMappings[j].m_propertyName.c_str()<<std::endl;
            for (size_t m = 0; m<c.m_propertyMappings[j].m_mappings.size(); m++)
            {
                std::wcout<<"\t   "<<c.m_propertyMappings[j].m_mappings[m].m_propertyMemberName.c_str()<<" maps_to <TODO>"<<std::endl;//<<c.m_propertyMappings[j].m_mappings[m].m_classMemberName<<std::endl;
                //TODO: dump the propertymapping
            }
        }
    }

    void FileParser::DumpClasses()
    {
        DobClasses m_classes = ParsingState::Instance().classParser.Result();
        for (size_t i = 0; i<m_classes.size(); i++)
        {
            DumpClass(m_classes[i]);
        }
    }

    void FileParser::DumpProperties()
    {
        DobProperties m_properties = ParsingState::Instance().propertyParser.Result();

        for (size_t i = 0; i<m_properties.size(); i++)
        {
            std::wcout<<std::endl;
            std::wcout<<"property "<<m_properties[i].m_name.c_str()<<std::endl;
            std::wcout<<"   typeId: "<<m_properties[i].m_typeId<<""<<std::endl;
            std::wcout<<"   members:"<<std::endl;
            for (size_t j = 0; j<m_properties[i].m_members.size(); j++)
            {
                std::wcout<<"\t"<<Safir::Dob::Typesystem::Internal::BasicTypes::StringOf(m_properties[i].m_members[j].m_type);
                std::wcout<<" "<<m_properties[i].m_members[j].m_name.c_str();
                if (m_properties[i].m_members[j].m_type == StringMemberType)
                    std::wcout<<"  length = "<<m_properties[i].m_members[j].m_dataLength;
                if (m_properties[i].m_members[j].m_arrayLength>1)
                    std::wcout<<"  arraySize = "<<m_properties[i].m_members[j].m_arrayLength;
                std::wcout<<std::endl;
            }
        }
    }
}
}
}
}
