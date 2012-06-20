/******************************************************************************
*
* Copyright Saab AB, 2004-2012 (http://www.safirsdk.com)
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
#include <iostream>
#include <boost/property_tree/xml_parser.hpp>
#include <Safir/Dob/Typesystem/TypeDefinitionParser.h>
#include "ElementParserDefs.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Parser
{
    //Funcions for debug
    void DumpResult(const ParseResult& result);
    void DumpClass(const ClassDefinition& c);
    void DumpMember(const ClassMemberDefinition& c);
    void DumpParameter(const ParameterDefinition& c);
    void DumpCreateRoutine(const CreateRoutineDefinition& c);
    void DumpEnumeration(const EnumerationDefinition& c);
    void DumpException(const ExceptionDefinition& c);
    void DumpProperty(const PropertyDefinition& c);
    void DumpMapping(const PropertyMappingDefinition& c);

    TypeDefinitionParser::TypeDefinitionParser(void) :  m_result()
    {
    }

    void TypeDefinitionParser::Parse(const boost::filesystem::path& definitions)
    {   
        if (!boost::filesystem::is_directory(definitions))
        {
            throw ParseError("Invalid directory path", "The specified root directory does not exist.", definitions.string());
        }

        ParseState state(m_result);
        boost::filesystem::recursive_directory_iterator it(definitions), end;
         for (it; it!=end; ++it)
         {
             if (it->path().extension() == ".dou" || it->path().extension() == ".dom" )
             {    
#if BOOST_FILESYSTEM_VERSION<3
                     std::string path = it->string();
                     std::string leaf = it->leaf();
#else
                     std::string path = it->path().string();
                     std::string leaf = it->path().leaf().string();
#endif
                 try
                 {
                     boost::property_tree::ptree pt;
                     boost::property_tree::read_xml(path, pt, boost::property_tree::xml_parser::trim_whitespace);
                     
                     state.CurrentPath=path;

                     DobUnitParser parser;
                     boost::property_tree::ptree::iterator ptIt=pt.begin();
                     if (parser.Match(ptIt->first, state))
                     {
                         parser.Parse(ptIt->second, state);
                     }
                     
                 }
                 catch (const ParseError&)
                 {
                     throw;
                 }
                 catch (const boost::property_tree::xml_parser_error& err)
                 {
                     std::ostringstream ss;
                     ss<<err.message()<<". Line: "<<err.line();
                     throw ParseError("Invalid XML", ss.str(), state.CurrentPath);
                 }
                 catch (const std::exception& err)
                 {
                     throw ParseError("Unexpected Error", err.what(), state.CurrentPath);
                 }
                 catch(...)
                 {
                     throw ParseError("Programming Error", "You have found a bug in DotsParser. Please save the this dou-file and attach it to your bug report.", state.CurrentPath);
                 }
             }
         }

         ParseResultFinalizer finalizer(state);
         finalizer.ProcessResult();
    }

    void TypeDefinitionParser::Dump(const ParseResult& result)
    {
        DumpResult(result);
    }

    //--------------------------------------------------
    // Debug trace functions
    //--------------------------------------------------
    void DumpResult(const ParseResult& result)
    {
        std::cout<<"========== Result =========="<<std::endl;
        std::for_each(result.Classes.begin(), result.Classes.end(), DumpClass);
        std::for_each(result.Enumerations.begin(), result.Enumerations.end(), DumpEnumeration);
        std::for_each(result.Exceptions.begin(), result.Exceptions.end(), DumpException);
        std::for_each(result.Properties.begin(), result.Properties.end(), DumpProperty);
        std::for_each(result.PropertyMappings.begin(), result.PropertyMappings.end(), DumpMapping);
        std::cout<<"============================"<<std::endl;
    }

    void DumpClass(const ClassDefinition& c)
    {
        std::cout<<"=========================================================="<<std::endl;
        std::cout<<"ClassName: "<<c.Name<<std::endl;        
        std::cout<<"BaseClass: "<<c.BaseClass<<std::endl;
        std::cout<<"FileName: "<<c.FileName<<std::endl;
        std::cout<<"Summary: "<<c.Summary<<std::endl;
        std::for_each(c.Members.begin(), c.Members.end(), DumpMember);
        std::for_each(c.Parameters.begin(), c.Parameters.end(), DumpParameter);
        std::for_each(c.CreateRoutines.begin(), c.CreateRoutines.end(), DumpCreateRoutine);
    }
    
    void DumpMember(const ClassMemberDefinition& m)
    {
        std::cout<<"  - MemberName: "<<m.Name<<std::endl;
        std::cout<<"    Type: "<<m.TypeName<<std::endl;
        std::cout<<"    IsArray: "<<std::boolalpha<<m.IsArray<<std::endl;
        std::cout<<"    ArraySize: "<<m.ArraySize<<std::endl;
        std::cout<<"    MaxLength: "<<m.MaxLength<<std::endl;
        std::cout<<"    Summary: "<<m.Summary<<std::endl;

    }

    void DumpParameter(const ParameterDefinition& p)
    {
        std::cout<<"  - ParameterName: "<<p.Name<<std::endl;
        std::cout<<"    Type:    "<<p.TypeName<<std::endl;
        std::cout<<"    IsArray: "<<std::boolalpha<<p.IsArray<<std::endl;
        std::cout<<"    Summary: "<<p.Summary<<std::endl;
        for (StringVector::const_iterator it = p.Values.begin(); it!=p.Values.end(); ++it)
        {
            std::cout<<"      Value: "<<(*it)<<std::endl;
        }
    }

    void DumpCreateRoutine(const CreateRoutineDefinition& c)
    {
        std::cout<<"  - CreateRoutine: "<<c.Name<<std::endl;
        std::cout<<"    Summary:    "<<c.Summary<<std::endl;
        for (StringVector::const_iterator it = c.Parameters.begin(); it!=c.Parameters.end(); ++it)
        {
            std::cout<<"      Parameter: "<<(*it)<<std::endl;
        }
        for (MemberValueVector::const_iterator it = c.MemberValues.begin(); it!=c.MemberValues.end(); ++it)
        {
            std::cout<<"      MemberVal: "<<it->first<<" = "<<it->second<<std::endl;
        }
    }

    void DumpEnumeration(const EnumerationDefinition& e)
    {
        std::cout<<"=========================================================="<<std::endl;
        std::cout<<"Enumeration: "<<e.Name<<std::endl;
        std::cout<<"File:    "<<e.FileName<<std::endl;
        std::cout<<"Summary: "<<e.Summary<<std::endl;
        for (StringVector::const_iterator it = e.EnumerationValues.begin(); it!=e.EnumerationValues.end(); ++it)
        {
            std::cout<<"      Value: "<<(*it)<<std::endl;
        }
    }

    void DumpException(const ExceptionDefinition& e)
    {
        std::cout<<"=========================================================="<<std::endl;
        std::cout<<"Exception: "<<e.Name<<std::endl;
        std::cout<<"BaseClass: "<<e.BaseClass<<std::endl;
        std::cout<<"File:    "<<e.FileName<<std::endl;
        std::cout<<"Summary: "<<e.Summary<<std::endl;
    }

    void DumpProperty(const PropertyDefinition& p)
    {
        std::cout<<"=========================================================="<<std::endl;
        std::cout<<"Property: "<<p.Name<<std::endl;
        std::cout<<"File:    "<<p.FileName<<std::endl;
        std::cout<<"Summary: "<<p.Summary<<std::endl;
        for (PropertyMemberDefinitions::const_iterator it = p.Members.begin(); it!=p.Members.end(); ++it)
        {
            std::cout<<"      Member:  "<<it->Name<<std::endl;
            std::cout<<"      Type:    "<<it->TypeName<<std::endl;
            std::cout<<"      Summary: "<<it->Summary<<std::endl;
        }
    }

    void DumpMapping(const PropertyMappingDefinition& p)
    {
        std::cout<<"=========================================================="<<std::endl;
        std::cout<<"PropertyMapping"<<std::endl;
        std::cout<<"ClassName:    "<<p.ClassName<<std::endl;
        std::cout<<"PropertyName:    "<<p.PropertyName<<std::endl;
        std::cout<<"File:    "<<p.FileName<<std::endl;
        std::cout<<"Summary: "<<p.Summary<<std::endl;
    }
}
}
}
} //end namespace Safir::Dob::Typesystem::Parser
