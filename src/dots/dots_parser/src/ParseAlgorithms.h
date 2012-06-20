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
#ifndef __DOTS_PARSER_ALGORITHMS_H__
#define __DOTS_PARSER_ALGORITHMS_H__

#include <assert.h>
#include <iostream>
#include <set>
#include <boost/function.hpp>
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/xml_parser.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/noncopyable.hpp>
#include <Safir/Dob/Typesystem/ParseResult.h>
#include "ElementNames.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Parser
{
    struct ParseState : public boost::noncopyable
    {        
        struct ParameterReference
        {
            std::string FileName;
            size_t ClassIndex, MemberIndex, ValueIndex;
            std::string ParameterName;
            int ParameterIndex;

            ParameterReference(const std::string& parName, const std::string file, size_t classIx) :
                ParameterName(parName), ParameterIndex(0), FileName(file), ClassIndex(classIx), MemberIndex(0), ValueIndex(0) {}
            ParameterReference(const std::string& parName, const std::string file, size_t classIx, size_t memIx) :
                ParameterName(parName), ParameterIndex(0), FileName(file), ClassIndex(classIx), MemberIndex(memIx), ValueIndex(0) {}
            ParameterReference(const std::string& parName, const std::string file, size_t classIx, size_t memIx, size_t valIx) :
                ParameterName(parName), ParameterIndex(0), FileName(file), ClassIndex(classIx), MemberIndex(memIx), ValueIndex(valIx) {}
        };
        typedef std::vector<ParameterReference> ParameterReferenceVector;
                
        std::string CurrentPath;
        ParseResult& Result;
        ParameterReferenceVector ArraySizeReferences;
        ParameterReferenceVector MaxLengthReferences;
        ParameterReferenceVector CreateRoutineValueReferences;

        ParseState(ParseResult& r) : Result(r) 
        {
            Result.Classes.clear();
            Result.Enumerations.clear();
            Result.Exceptions.clear();
            Result.Properties.clear();
            Result.PropertyMappings.clear();
        }
    };
    
    template <int ElemName> struct ParseAlgorithm
    {
        void operator()(boost::property_tree::ptree& /*pt*/, ParseState& /*state*/){}
    };

    //Template specializations
    template<> struct ParseAlgorithm<ElementNames::Class>
    {
        void operator()(boost::property_tree::ptree& /*pt*/, ParseState& state)
        {
            ClassDefinition cd;
            cd.FileName=state.CurrentPath;
            state.Result.Classes.push_back(cd);
        }
    };

    template<> struct ParseAlgorithm<ElementNames::Member>
    {
        void operator()(boost::property_tree::ptree& /*pt*/, ParseState& state){state.Result.Classes.back().Members.push_back(ClassMemberDefinition());}
    };

    template<> struct ParseAlgorithm<ElementNames::CreateRoutine>
    {
        void operator()(boost::property_tree::ptree& /*pt*/, ParseState& state){state.Result.Classes.back().CreateRoutines.push_back(CreateRoutineDefinition());}
    };

    template<> struct ParseAlgorithm<ElementNames::CreateRoutineValue>
    {
        void operator()(boost::property_tree::ptree& /*pt*/, ParseState& state){state.Result.Classes.back().CreateRoutines.back().MemberValues.push_back(MemberValue("", ""));}
    };

    template<> struct ParseAlgorithm<ElementNames::CreateRoutineValueMember>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state){state.Result.Classes.back().CreateRoutines.back().MemberValues.back().first=pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::CreateRoutineValueParameter>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state)
        {
            size_t clsIx=state.Result.Classes.size()-1;
            size_t crIx=state.Result.Classes.back().CreateRoutines.size()-1;
            size_t valIx=state.Result.Classes.back().CreateRoutines.back().MemberValues.size()-1;
            state.CreateRoutineValueReferences.push_back(ParseState::ParameterReference(pt.data(), state.CurrentPath, clsIx, crIx, valIx));
        }
    };

    struct ObjectParameterHandler
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state)
        {
            boost::optional<boost::property_tree::ptree&> objSubTree = pt.get_child_optional(ElementNames::Instance().String(ElementNames::Object));
            if (objSubTree)
            {
                //Haven't found a better way to get rid of the xml version than brute force.
                std::ostringstream ss, val;
                boost::property_tree::write_xml(ss, objSubTree.get());
                val << "<" 
                    << ElementNames::Instance().String(ElementNames::Object) //<object>
                    << ">" 
                    << ss.str().substr(ss.str().find_first_of('>')+1)  //remove <? xml version... ?> 
                    << "</"
                    << ElementNames::Instance().String(ElementNames::Object)<<">"; //</object>
                state.Result.Classes.back().Parameters.back().Values.push_back(val.str());
            }
        }        
    }; 

    template<> struct ParseAlgorithm<ElementNames::Parameter>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state)
        {
            state.Result.Classes.back().Parameters.push_back(ParameterDefinition());
            ObjectParameterHandler objHandler;
            objHandler(pt, state);
            //If we cant find object we just continue parsing and expects a <value> or <arrayElements>
        }
    };

    template<> struct ParseAlgorithm<ElementNames::Enumeration>
    {
        void operator()(boost::property_tree::ptree& /*pt*/, ParseState& state)
        {
            EnumerationDefinition e;
            e.FileName=state.CurrentPath;
            state.Result.Enumerations.push_back(e);
        }
    };
    
    template<> struct ParseAlgorithm<ElementNames::Exception>
    {
        void operator()(boost::property_tree::ptree& /*pt*/, ParseState& state)
        {
            ExceptionDefinition e;
            e.FileName=state.CurrentPath;
            state.Result.Exceptions.push_back(e);
        }
    };

    template<> struct ParseAlgorithm<ElementNames::BaseClass>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) {state.Result.Classes.back().BaseClass=pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::ExceptionBase>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) {state.Result.Exceptions.back().BaseClass=pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::EnumerationValue>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) {state.Result.Enumerations.back().EnumerationValues.push_back(pt.data());}
    };

    template<> struct ParseAlgorithm<ElementNames::MemberType>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) {state.Result.Classes.back().Members.back().TypeName=pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::ParameterType>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) {state.Result.Classes.back().Parameters.back().TypeName=pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::ClassName>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state){state.Result.Classes.back().Name = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::ExceptionName>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state){state.Result.Exceptions.back().Name = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::EnumerationName>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state){state.Result.Enumerations.back().Name = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::PropertyName>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state){state.Result.Properties.back().Name = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::MemberName>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state){state.Result.Classes.back().Members.back().Name = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::ParameterName>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state){state.Result.Classes.back().Parameters.back().Name = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::CreateRoutineName>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state){state.Result.Classes.back().CreateRoutines.back().Name = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::ClassSummary>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state){state.Result.Classes.back().Summary = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::ExceptionSummary>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state){state.Result.Exceptions.back().Summary = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::EnumerationSummary>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state){state.Result.Enumerations.back().Summary = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::PropertySummary>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state){state.Result.Properties.back().Summary = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::PropertyMappingSummary>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state){state.Result.PropertyMappings.back().Summary = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::MemberSummary>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state){state.Result.Classes.back().Members.back().Summary = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::ParameterSummary>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state){state.Result.Classes.back().Parameters.back().Summary = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::CreateRoutineSummary>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state){state.Result.Classes.back().CreateRoutines.back().Name = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::MaxLength>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state){state.Result.Classes.back().Members.back().MaxLength = boost::lexical_cast<int, std::string>(pt.data());}
    };

    template<> struct ParseAlgorithm<ElementNames::MaxLengthRefName>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state)
        {
            size_t clsIx=state.Result.Classes.size()-1;
            size_t memIx=state.Result.Classes.back().Members.size()-1;
            state.MaxLengthReferences.push_back(ParseState::ParameterReference(pt.data(), state.CurrentPath, clsIx, memIx));
        }
    };

    template<> struct ParseAlgorithm<ElementNames::ArraySize>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state)
        {
            if (pt.data()=="dynamic")
            {
                state.Result.Classes.back().Members.back().ArraySize = -1;

            }
            else
            {
                state.Result.Classes.back().Members.back().ArraySize = boost::lexical_cast<int, std::string>(pt.data());
            }
        }
    };

    template<> struct ParseAlgorithm<ElementNames::ArraySizeRefName>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state)
        {
            size_t clsIx=state.Result.Classes.size()-1;
            size_t memIx=state.Result.Classes.back().Members.size()-1;
            state.ArraySizeReferences.push_back(ParseState::ParameterReference(pt.data(), state.CurrentPath, clsIx, memIx));
        }
    };

    template<> struct ParseAlgorithm<ElementNames::ParameterArrayElements>
    {
        void operator()(boost::property_tree::ptree& /*pt*/, ParseState& state){state.Result.Classes.back().Parameters.back().IsArray=true;}
    };    

    template<> struct ParseAlgorithm<ElementNames::ParameterValue>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state)
        {            
            state.Result.Classes.back().Parameters.back().Values.push_back(pt.data());
        }
    };

    template<> struct ParseAlgorithm<ElementNames::EntityId>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state)
        {                 
            boost::optional<boost::property_tree::ptree&> typeName = pt.get_child_optional("name");
            boost::optional<boost::property_tree::ptree&> instance = pt.get_child_optional("instanceId");
            if (!typeName || !instance)
            {
                std::ostringstream ss;
                ss<<"The entityId parameter '"<<state.Result.Classes.back().Parameters.back().Name<<"' in class '"<<state.Result.Classes.back().Name<<"' is missing element <name> or <instanceId>.";
                throw ParseError("Invalid parameter", ss.str(), state.CurrentPath);
            }            
            state.Result.Classes.back().Parameters.back().Values.push_back(typeName.get().data()+std::string(" : ")+instance.get().data());
        }
    };

    template<> struct ParseAlgorithm<ElementNames::CreateRoutineMemberName>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state){state.Result.Classes.back().CreateRoutines.back().Parameters.push_back(pt.data());}
    };


    //------------------------------------
    // Post xml-parsing algorithms
    //------------------------------------
    class ParseResultFinalizer : public boost::noncopyable
    {
    public:
        explicit ParseResultFinalizer(ParseState& state) : m_state(state) {}
        void ProcessResult(); //Will throw ParseError if something's not ok.
    private:
        ParseState& m_state;

        void ProcessEnum(EnumerationDefinition& e);
        void ProcessException(ExceptionDefinition& e);
        void ProcessProperty(PropertyDefinition& p);
        void ProcessClass(ClassDefinition& c);
        void ProcessPropertyMapping(PropertyMappingDefinition& p);

        //Sub process-helpers for ProcessClass
        void ProcessCreateRoutine(ClassDefinition& host, CreateRoutineDefinition& c);
        void ProcessClassMember(ClassDefinition& host, ClassMemberDefinition& m);
        void ProcessParameter(ClassDefinition& host, ParameterDefinition& p);

        //Separates a name after the last dot. I.e Safir.Dob.Entity into part1=Safir.Dob and part2=Entity.
        //If no dot exists, part2 will contain fullname and part1 will be empty.
        void SplitFullName(const std::string& fullName, std::string& part1, std::string& part2) const;
        bool ValidName(const std::string& name, bool allowDot) const;
        bool ValidType(const std::string& typeName) const;
        bool IsOfType(const std::string& type, const std::string& ofType) const;
        void CheckNameAndFilenameConsistency(const std::string& filename, const std::string name) const;

        std::string ExpandEnvironmentVariables(const std::string& str) const;
        const ParameterDefinition* GetParameter(const std::string& name) const;
        void ResolveReferences(ParseState::ParameterReferenceVector& vec, const std::string& refName, boost::function<void(ClassMemberDefinition&, int)> setVal);
        void ResolveCreateRoutineValues();

        static std::set<std::string> CreateBasicTypeSet();
    };
}
}
}
}

#endif
