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
#include <Safir/Dob/Typesystem/Internal/TypeDefinitionParser.h>
#include "ElementParserDefs.h"
#include "RepositoryWrapper.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    //Funcions for debug
    void DumpResult(boost::shared_ptr<const RawParseResult> result, std::ostringstream& os);
    void DumpClassDefinition(const ClassDefinition& c, std::ostringstream& os);
    void DumpMemberDefinition(const MemberDefinition& c, std::ostringstream& os);
    void DumpParameterDefinition(const ParameterDefinition& c, std::ostringstream& os);
    void DumpCreateRoutineDefinition(const CreateRoutineDefinition& c, std::ostringstream& os);
    void DumpEnumerationDefinition(const EnumerationDefinition& c, std::ostringstream& os);
    void DumpExceptionDefinition(const ExceptionDefinition& c, std::ostringstream& os);
    void DumpPropertyDefinition(const PropertyDefinition& c, std::ostringstream& os);
    void DumpMappingDefinition(const PropertyMappingDefinition& c, std::ostringstream& os);

    void DumpRepository(boost::shared_ptr<const TypeRepository> rep, std::ostringstream& os);
    void DumpClassDescription(const ClassDescription* c, std::ostringstream& os);
    void DumpMemberDescription(const MemberDescription* c, std::ostringstream& os);
    void DumpParameterDescription(const ParameterDescription* c, std::ostringstream& os);    
    void DumpEnumerationDescription(const EnumDescription* c, std::ostringstream& os);
    void DumpExceptionDescription(const ExceptionDescription* c, std::ostringstream& os);
    void DumpPropertyDescription(const PropertyDescription* c, std::ostringstream& os);
    void DumpMappingDescription(const PropertyMappingDescription* c, std::ostringstream& os);

    TypeDefinitionParser::TypeDefinitionParser(const boost::filesystem::path& definitions) : m_path(definitions), m_result()
    {
        Parse();
    }

    void TypeDefinitionParser::Parse()
    {   
        if (!boost::filesystem::is_directory(m_path))
        {
            throw ParseError("Invalid directory path", "The specified root directory does not exist.", m_path.string());
        }

        m_result.reset(new RawParseResult());        
        ParseState state(m_result);

        boost::filesystem::recursive_directory_iterator it(m_path), end;
         for (; it!=end; ++it)
         {
             if (it->path().extension() == ".dou" || it->path().extension() == ".dom" )
             {    
                 std::string path = it->path().string();

                 try
                 {
                     state.CurrentPath=path;
                     boost::property_tree::ptree pt;
                     boost::property_tree::read_xml(path, pt, boost::property_tree::xml_parser::trim_whitespace);
                     DobUnitParser parser;
                     boost::property_tree::ptree::iterator ptIt=pt.begin();
                     if (parser.Match(ptIt->first, state))
                     {
                         parser.Parse(ptIt->second, state);
                         parser.Reset(state);
                     }
                     
                 }
                 catch (const ParseError&)
                 {
                     throw;
                 }
                 catch (boost::property_tree::xml_parser_error& err) //cant catch as const-ref due to bug in early boost versions.
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

    boost::shared_ptr<const TypeRepository> TypeDefinitionParser::GetRepository() const
    {
        boost::shared_ptr<const TypeRepository> repository(new RepositoryWrapper(m_result));        
        return repository;
    }

    std::string TypeDefinitionParser::ToString(boost::shared_ptr<const RawParseResult> rawResult)
    {
        std::ostringstream os;
        DumpResult(rawResult, os);
        return os.str();
    }

    std::string TypeDefinitionParser::ToString(boost::shared_ptr<const TypeRepository> repository)
    {
        std::ostringstream os;
        DumpRepository(repository, os);
        return os.str();
    }   

    //--------------------------------------------------
    // Debug trace functions
    //--------------------------------------------------
    void DumpResult(boost::shared_ptr<const RawParseResult> result, std::ostringstream& os)
    {
        os<<"========== Result =========="<<std::endl;
        std::for_each(result->Classes.begin(), result->Classes.end(), boost::bind(DumpClassDefinition, _1, boost::ref(os)));
        std::for_each(result->Enumerations.begin(), result->Enumerations.end(), boost::bind(DumpEnumerationDefinition, _1, boost::ref(os)));
        std::for_each(result->Exceptions.begin(), result->Exceptions.end(), boost::bind(DumpExceptionDefinition, _1, boost::ref(os)));
        std::for_each(result->Properties.begin(), result->Properties.end(), boost::bind(DumpPropertyDefinition, _1, boost::ref(os)));
        std::for_each(result->PropertyMappings.begin(), result->PropertyMappings.end(), boost::bind(DumpMappingDefinition, _1, boost::ref(os)));
        os<<"============================"<<std::endl;
    }

    void DumpClassDefinition(const ClassDefinition& c, std::ostringstream& os)
    {
        os<<"=========================================================="<<std::endl;
        os<<"ClassName: "<<c.Name<<std::endl;        
        os<<"BaseClass: "<<c.BaseClass<<std::endl;
        os<<"FileName: "<<c.FileName<<std::endl;
        os<<"Summary: "<<c.Summary<<std::endl;
        std::for_each(c.Members.begin(), c.Members.end(), boost::bind(DumpMemberDefinition, _1, boost::ref(os)));
        std::for_each(c.Parameters.begin(), c.Parameters.end(), boost::bind(DumpParameterDefinition, _1, boost::ref(os)));
        std::for_each(c.CreateRoutines.begin(), c.CreateRoutines.end(), boost::bind(DumpCreateRoutineDefinition, _1, boost::ref(os)));
    }
    
    void DumpMemberDefinition(const MemberDefinition& m, std::ostringstream& os)
    {
        os<<"  - MemberName: "<<m.Name<<std::endl;
        os<<"    Type: "<<m.TypeName<<std::endl;
        os<<"    IsArray: "<<std::boolalpha<<m.IsArray<<std::endl;
        os<<"    ArraySize: "<<m.ArraySize<<std::endl;
        os<<"    MaxLength: "<<m.MaxLength<<std::endl;
        os<<"    Summary: "<<m.Summary<<std::endl;

    }

    void DumpParameterDefinition(const ParameterDefinition& p, std::ostringstream& os)
    {
        os<<"  - ParameterName: "<<p.Name<<std::endl;
        os<<"    Type:    "<<p.TypeName<<std::endl;
        os<<"    IsArray: "<<std::boolalpha<<p.IsArray<<std::endl;
        os<<"    Summary: "<<p.Summary<<std::endl;
        for (StringVector::const_iterator it = p.Values.begin(); it!=p.Values.end(); ++it)
        {
            os<<"      Value: "<<(*it)<<std::endl;
        }
    }

    void DumpCreateRoutineDefinition(const CreateRoutineDefinition& c, std::ostringstream& os)
    {
        os<<"  - CreateRoutine: "<<c.Name<<std::endl;
        os<<"    Summary:    "<<c.Summary<<std::endl;
        for (StringVector::const_iterator it = c.Parameters.begin(); it!=c.Parameters.end(); ++it)
        {
            os<<"      Parameter: "<<(*it)<<std::endl;
        }
        for (MemberValueVector::const_iterator it = c.MemberValues.begin(); it!=c.MemberValues.end(); ++it)
        {
            os<<"      MemberVal: "<<it->first<<" = "<<it->second<<std::endl;
        }
    }

    void DumpEnumerationDefinition(const EnumerationDefinition& e, std::ostringstream& os)
    {
        os<<"=========================================================="<<std::endl;
        os<<"Enumeration: "<<e.Name<<std::endl;
        os<<"File:    "<<e.FileName<<std::endl;
        os<<"Summary: "<<e.Summary<<std::endl;
        for (StringVector::const_iterator it = e.EnumerationValues.begin(); it!=e.EnumerationValues.end(); ++it)
        {
            os<<"      Value: "<<(*it)<<std::endl;
        }
    }

    void DumpExceptionDefinition(const ExceptionDefinition& e, std::ostringstream& os)
    {
        os<<"=========================================================="<<std::endl;
        os<<"Exception: "<<e.Name<<std::endl;
        os<<"BaseClass: "<<e.BaseClass<<std::endl;
        os<<"File:    "<<e.FileName<<std::endl;
        os<<"Summary: "<<e.Summary<<std::endl;
    }

    void DumpPropertyDefinition(const PropertyDefinition& p, std::ostringstream& os)
    {
        os<<"=========================================================="<<std::endl;
        os<<"Property: "<<p.Name<<std::endl;
        os<<"File:    "<<p.FileName<<std::endl;
        os<<"Summary: "<<p.Summary<<std::endl;
        for (MemberDefinitions::const_iterator it = p.Members.begin(); it!=p.Members.end(); ++it)
        {
            os<<"  - Member:  "<<it->Name<<std::endl;
            os<<"    Type:    "<<it->TypeName<<std::endl;
            os<<"    Summary: "<<it->Summary<<std::endl;
            if (it->IsArray)
                os<<"    IsArray: true"<<std::endl;
        }
    }

    void DumpMappingDefinition(const PropertyMappingDefinition& p, std::ostringstream& os)
    {
        os<<"=========================================================="<<std::endl;
        os<<"PropertyMapping"<<std::endl;
        os<<"ClassName:    "<<p.ClassName<<std::endl;
        os<<"PropertyName:    "<<p.PropertyName<<std::endl;
        os<<"File:    "<<p.FileName<<std::endl;
        os<<"Summary: "<<p.Summary<<std::endl;
        for (MappedMemberDefinitions::const_iterator it=p.MappedMembers.begin(); it!=p.MappedMembers.end(); ++it)
        {
            os<<"  - PropertyMember:  "<<it->Name<<std::endl;            
            if (it->Kind==MappedToParameter)
            {
                os<<"    MappingKind:     ValueMapping"<<std::endl;
                os<<"    Value:           "<<it->Value<<std::endl;
            }
            else if (it->Kind==MappedToMember)
            {
                os<<"    MappingKind:     MemberMapping"<<std::endl;
                os<<"    MemberRef:       ";
                for (MemberReferenceVector::const_iterator memIt=it->MemberReferences.begin(); memIt!=it->MemberReferences.end(); ++memIt)
                {
                    os<<"->"<<memIt->first<<"["<<memIt->second<<"]";
                }
                os<<std::endl;
            }
            else
            {
                os<<"    MappingKind:     NullMapping"<<std::endl;
            }
        }
    }

    void DumpRepository(boost::shared_ptr<const TypeRepository> rep, std::ostringstream& os)
    {
        std::vector<DotsC_TypeId> types=rep->GetAllClassTypeIds();
        for (std::vector<DotsC_TypeId>::const_iterator it=types.begin(); it!=types.end(); ++it)
        {
            const ClassDescription* tmp = rep->GetClass(*it);
            DumpClassDescription(tmp, os);
        }

        types=rep->GetAllEnumTypeIds();
        for (std::vector<DotsC_TypeId>::const_iterator it=types.begin(); it!=types.end(); ++it)
        {
            const EnumDescription* tmp = rep->GetEnum(*it);
            DumpEnumerationDescription(tmp, os);
        }

        types=rep->GetAllExceptionTypeIds();
        for (std::vector<DotsC_TypeId>::const_iterator it=types.begin(); it!=types.end(); ++it)
        {
            const ExceptionDescription* tmp = rep->GetException(*it);
            DumpExceptionDescription(tmp, os);
        }

        types=rep->GetAllPropertyTypeIds();
        for (std::vector<DotsC_TypeId>::const_iterator it=types.begin(); it!=types.end(); ++it)
        {
            const PropertyDescription* tmp = rep->GetProperty(*it);
            DumpPropertyDescription(tmp, os);
        }
    }

    void DumpClassDescription(const ClassDescription* c, std::ostringstream& os)
    {
        if (c==NULL) return;

        os<<"=========================================================="<<std::endl;
        os<<"Class: "<<c->GetName()<<std::endl;
        os<<"TypeId: "<<c->GetTypeId()<<std::endl;
        if (c->GetBaseClass()!=NULL)
            os<<"BaseClass: "<<c->GetBaseClass()->GetName()<<std::endl;

        for (int i=0; i<c->GetNumberOfMembers(); ++i)
        {
            DumpMemberDescription(c->GetMember(i), os);
        }

        for (int i=0; i<c->GetNumberOfParameters(); ++i)
        {
            DumpParameterDescription(c->GetParameter(i), os);
        }

        /*for (int i=0; i<c->GetNumberOfProperties(); ++i)
        {
            DumpParameterDescription(c->FindPropertyMapping((i), os);
        }*/

    }

    void DumpEnumerationDescription(const EnumDescription* c, std::ostringstream& os)
    {
        if (c==NULL) return;

        os<<"=========================================================="<<std::endl;
        os<<"Enumeration: "<<c->GetName()<<std::endl;
        os<<"TypeId: "<<c->GetTypeId()<<std::endl;

        for (int i=0; i<c->GetNumberOfValues(); ++i)
        {
            os<<"    Value: "<<c->GetValueName(i)<<std::endl;    
        }
    }

    void DumpExceptionDescription(const ExceptionDescription* c, std::ostringstream& os)
    {
        if (c==NULL) return;

        os<<"=========================================================="<<std::endl;
        os<<"Exception: "<<c->GetName()<<std::endl;
        os<<"TypeId: "<<c->GetTypeId()<<std::endl;
        if (c->GetBaseClass()!=NULL)
            os<<"BaseClass: "<<c->GetBaseClass()->GetName()<<std::endl;
    }

    void DumpPropertyDescription(const PropertyDescription* c, std::ostringstream& os)
    {
        if (c==NULL) return;

        os<<"=========================================================="<<std::endl;
        os<<"Property: "<<c->GetName()<<std::endl;
        os<<"TypeId: "<<c->GetTypeId()<<std::endl;
        
        for (int i=0; i<c->GetNumberOfMembers(); ++i)
        {
            DumpMemberDescription(c->GetMember(i), os);
        }
    }

    void DumpMemberDescription(const MemberDescription* c, std::ostringstream& os)
    {
        if (c==NULL) return;

        os<<"    Member: "<<c->GetName()<<"  :  Type";
        
        switch (c->GetMemberType())
        {
        case EnumerationMemberType:
            os<<c->GetEnum()->GetName();
            break;
        case ObjectMemberType:
            os<<c->GetClass()->GetName();
            break;
        default:
            os<<BasicTypes::Instance().StringOf(c->GetMemberType());
            break;
        }
        if (c->IsArray())
        {
            os<<", isArray=true, ArraySize: "<<c->GetArraySize()<<std::endl;
        }
        else
        {
            os<<", isArray=false"<<std::endl;
        }
    }

    void DumpParameterDescription(const ParameterDescription* c, std::ostringstream& os)
    {
        if (c==NULL) return;       

        os<<"    Parameter: "<<c->GetName()<<"  :  Type";
        
        /*switch (c->GetMemberType())
        {
        case EnumerationMemberType:
            os<<c->GetEnum()->GetName();
            break;
        case ObjectMemberType:
            os<<c->GetClass()->GetName();
            break;
        default:*/
            os<<BasicTypes::Instance().StringOf(c->GetMemberType());
            /*break;
        }*/

        if (c->IsArray())
        {
            os<<", isArray=true, ArraySize: "<<c->GetArraySize()<<std::endl;
        }
        else
        {
            os<<", isArray=false"<<std::endl;
        }

    }

    void DumpMappingDescription(const PropertyMappingDescription* c, std::ostringstream& os)
    {
        if (c==NULL) return;
        os<<"    Property: "<<c->GetProperty()->GetName()<<std::endl;
    }
}
}
}
} //end namespace Safir::Dob::Typesystem::Parser
