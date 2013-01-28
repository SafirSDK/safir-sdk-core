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
#include <algorithm>
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
    void DumpRepository(boost::shared_ptr<const TypeRepository> rep, std::ostringstream& os);
    void DumpClassDescription(const ClassDescription* c, std::ostringstream& os);
    void DumpMemberDescription(const MemberDescription* c, std::ostringstream& os);
    void DumpParameterDescription(const ParameterDescription* c, std::ostringstream& os);    
    void DumpEnumerationDescription(const EnumDescription* c, std::ostringstream& os);
    void DumpExceptionDescription(const ExceptionDescription* c, std::ostringstream& os);
    void DumpPropertyDescription(const PropertyDescription* c, std::ostringstream& os);
    void DumpMappingDescription(const PropertyMappingDescription* c, std::ostringstream& os);
    void DumpCreateRoutineDescription(const CreateRoutineDescription* c, std::ostringstream& os);

    TypeDefinitionParser::TypeDefinitionParser(const boost::filesystem::path& definitions) : m_path(definitions)
    {
        if (!boost::filesystem::is_directory(m_path))
        {
            throw ParseError("Invalid directory path", "The specified root directory does not exist.", m_path.string());
        }

        boost::shared_ptr<RawParseResult> result(new RawParseResult());
        ParseState state(result);
        ParseResultFinalizer finalizer(state);

        StringVector domFiles;
        boost::filesystem::recursive_directory_iterator it(m_path), end;
        for (; it!=end; ++it)
        {
            if (it->path().extension() == ".dou")
            {
                ParseFile(it->path().string(), state);
            }
            else if (it->path().extension()==".dom")
            {
                //Dom files have to wait until all dou-files have been parsed
                domFiles.push_back(it->path().string());
            }
        }
        finalizer.ProcessDouResults();

        //now parse the dom-files
        std::for_each(domFiles.begin(), domFiles.end(), boost::bind(&TypeDefinitionParser::ParseFile, this, _1, boost::ref(state)));
        finalizer.ProcessDomResults();

        //Wrap result in a repository interface
        m_repository.reset(new RepositoryWrapper(state));
    }

    void TypeDefinitionParser::ParseFile(const std::string& path, ParseState& state)
    {
        try
        {
            state.currentPath=path;
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
            throw ParseError("Invalid XML", ss.str(), state.currentPath);
        }
        catch (const std::exception& err)
        {
            throw ParseError("Unexpected Error", err.what(), state.currentPath);
        }
        catch(...)
        {
            throw ParseError("Programming Error", "You have found a bug in DotsParser. Please save the this dou-file and attach it to your bug report.", state.currentPath);
        }
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
    void DumpRepository(boost::shared_ptr<const TypeRepository> rep, std::ostringstream& os)
    {
        std::vector<DotsC_TypeId> types;
        rep->GetAllClassTypeIds(types);
        for (std::vector<DotsC_TypeId>::const_iterator it=types.begin(); it!=types.end(); ++it)
        {
            const ClassDescription* tmp = rep->GetClass(*it);
            DumpClassDescription(tmp, os);
        }

        types.clear();
        rep->GetAllEnumTypeIds(types);
        for (std::vector<DotsC_TypeId>::const_iterator it=types.begin(); it!=types.end(); ++it)
        {
            const EnumDescription* tmp = rep->GetEnum(*it);
            DumpEnumerationDescription(tmp, os);
        }

        types.clear();
        rep->GetAllExceptionTypeIds(types);
        for (std::vector<DotsC_TypeId>::const_iterator it=types.begin(); it!=types.end(); ++it)
        {
            const ExceptionDescription* tmp = rep->GetException(*it);
            DumpExceptionDescription(tmp, os);
        }

        types.clear();
        rep->GetAllPropertyTypeIds(types);
        for (std::vector<DotsC_TypeId>::const_iterator it=types.begin(); it!=types.end(); ++it)
        {
            const PropertyDescription* tmp = rep->GetProperty(*it);
            DumpPropertyDescription(tmp, os);
        }
    }

    void DumpClassDescription(const ClassDescription* c, std::ostringstream& os)
    {
        os<<"=========================================================="<<std::endl;
        os<<"Class: "<<c->GetName()<<std::endl;
        os<<"TypeId: "<<c->GetTypeId()<<std::endl;
        if (c->GetbaseClass()!=NULL)
            os<<"baseClass: "<<c->GetbaseClass()->GetName()<<std::endl;

        for (int i=0; i<c->GetNumberOfMembers(); ++i)
        {
            DumpMemberDescription(c->GetMember(i), os);
        }

        for (int i=0; i<c->GetNumberOfCreateRoutines(); ++i)
        {
            DumpCreateRoutineDescription(c->GetCreateRoutine(i), os);
        }

        for (int i=0; i<c->GetNumberOfParameters(); ++i)
        {
            DumpParameterDescription(c->GetParameter(i), os);
        }

        std::vector<DotsC_TypeId> properties;
        c->GetPropertyIds(properties);
        for (std::vector<DotsC_TypeId>::const_iterator it=properties.begin(); it!=properties.end(); ++it)
        {
            bool inherited;
            const PropertyMappingDescription* tmp = c->GetPropertyMapping(*it, inherited);
            DumpMappingDescription(tmp, os);
        }
    }

    void DumpEnumerationDescription(const EnumDescription* c, std::ostringstream& os)
    {
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
        os<<"=========================================================="<<std::endl;
        os<<"Exception: "<<c->GetName()<<std::endl;
        os<<"TypeId: "<<c->GetTypeId()<<std::endl;
        if (c->GetbaseClass()!=NULL)
            os<<"baseClass: "<<c->GetbaseClass()->GetName()<<std::endl;
    }

    void DumpPropertyDescription(const PropertyDescription* c, std::ostringstream& os)
    {
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
            os<<", isArray=true, arraySize: "<<c->GetarraySize()<<std::endl;
        }
        else
        {
            os<<", isArray=false"<<std::endl;
        }
    }

    void DumpParameterDescription(const ParameterDescription* c, std::ostringstream& os)
    {
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
            os<<", isArray=true, arraySize: "<<c->GetarraySize()<<std::endl;
        }
        else
        {
            os<<", isArray=false"<<std::endl;
        }

    }

    void DumpMappingDescription(const PropertyMappingDescription* pmd, std::ostringstream& os)
    {
        const PropertyDescription* p = pmd->GetProperty();
        const ClassDescription* c = pmd->GetClass();
        os<<"    Property: "<<p->GetName()<<std::endl;

        for (int i=0; i<pmd->GetNumberOfMappings(); ++i)
        {
            const MemberMappingDescription* md=pmd->GetMapping(i);

            os<<"  - PropertyMember:  "<<p->GetMember(i)->GetName()<<std::endl;
            switch(md->GetMappingKind())
            {
            case MappedToParameter:
            {
                os<<"    MappingKind:     ValueMapping"<<std::endl;
                os<<"    Parameter:       "<<md->GetParameter()->GetName()<<std::endl;
                os<<"    Value:           <Not implemented>"<<std::endl;
            }
                break;
            case MappedToMember:
            {
                os<<"    MappingKind:     MemberMapping"<<std::endl;
                os<<"    MemberRef:       ";
                for (int memRef=0; memRef<md->MemberReferenceDepth(); ++memRef)
                {
                    std::pair<DotsC_MemberIndex, DotsC_ArrayIndex> ref = md->GetMemberReference(memRef);
                    const MemberDescription* member = c->GetMember(ref.first);
                    os<<"->"<<member->GetName();
                    if (member->IsArray())
                    {
                        os<<"["<<ref.second<<"]";
                    }
                }
                os<<std::endl;
            }
                break;

            case MappedToNull:
            {
                os<<"    MappingKind:     NullMapping"<<std::endl;
            }
                break;
            }
        }
    }

    void DumpCreateRoutineDescription(const CreateRoutineDescription* c, std::ostringstream& os)
    {
        os<<"  - CreateRoutine: "<<c->GetName()<<std::endl;
        os<<"    summary:    "<<c->Summary()<<std::endl;
        for (int i=0; i<c->GetNumberOfInParameters(); ++i)
        {
            os<<"      Parameter: "<<c->GetInParameterMember(i)->GetName()<<std::endl;
        }
        for (int i=0; i<c->GetNumberOfDefaultValues(); ++i)
        {
            auto val=c->GetDefaultValue(i);
            os<<"      DefaultVal: "<<c->GetDefaultValueMember(i)->GetName()<<" = "<<val.first->GetName()<<"["<<val.second<<"]"<<std::endl;
        }

    }
}
}
}
} //end namespace Safir::Dob::Typesystem::Parser
