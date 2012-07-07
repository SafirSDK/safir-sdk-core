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
            size_t TopIndex, SubIndex1, SubIndex2, SubIndex3;
            std::string ParameterName;
            int ParameterIndex;

            ParameterReference(const std::string file) : FileName(file) {}                                        
        };
        typedef std::vector<ParameterReference> ParameterReferenceVector;
                
        std::string CurrentPath;
        ParseResult& Result;
        ParameterReferenceVector ParamToParamReferences;
        ParameterReferenceVector ArraySizeReferences;
        ParameterReferenceVector MaxLengthReferences;
        ParameterReferenceVector CreateRoutineValueReferences;
        ParameterReferenceVector MappedValueReferences;

        ParseState(ParseResult& r) : Result(r) 
        {
            Result.Classes.clear();
            Result.Enumerations.clear();
            Result.Exceptions.clear();
            Result.Properties.clear();
            Result.PropertyMappings.clear();
        }
    };
    
    struct ReferenceHandler
    {
        void GetReferencedParameter(boost::property_tree::ptree& pt, ParseState::ParameterReference& ref) const
        {
            ref.ParameterName = pt.get<std::string>(ElementNames::Instance().String(ElementNames::ReferenceName));
            ref.ParameterIndex = pt.get(ElementNames::Instance().String(ElementNames::ReferenceIndex), 0);            
        }    

    };

    struct EntityIdParameterHandler
    {
        std::string GetVal(boost::property_tree::ptree& pt) const
        {
            std::string name = pt.get<std::string>(ElementNames::Instance().String(ElementNames::ClassName));
            std::string inst = pt.get<std::string>(ElementNames::Instance().String(ElementNames::InstanceId));
            return name+std::string(", ")+inst;
        }        
    };

    struct ObjectParameterHandler
    {        
        std::string GetVal(boost::property_tree::ptree& pt) const
        {
            //Haven't found a better way to get rid of the xml version than brute force.
            std::ostringstream ss, val;
            boost::property_tree::write_xml(ss, pt);
            val << "<" << ElementNames::Instance().String(ElementNames::ParameterObject)<< ">"  //add start element <object>
                << ss.str().substr(ss.str().find_first_of('>')+1)  //remove <? xml version... ?> 
                << "</" << ElementNames::Instance().String(ElementNames::ParameterObject)<< ">"; //add end element </object>
            return val.str();
        }
    };

    template <int ElemName> struct ParseAlgorithm
    {
        void operator()(boost::property_tree::ptree& /*pt*/, ParseState& /*state*/) const {}
    };

    template<> struct ParseAlgorithm<ElementNames::MapObject> : private ObjectParameterHandler
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const 
        {
            MappedMemberDefinition& md = state.Result.PropertyMappings.back().MappedMembers.back();
            md.Kind=MappedMemberDefinition::ValueMapping;
            md.Value=GetVal(pt);
        }
    };

    template<> struct ParseAlgorithm<ElementNames::ParameterObject> : private ObjectParameterHandler
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.Result.Classes.back().Parameters.back().Values.push_back(GetVal(pt));}
    };

    template<> struct ParseAlgorithm<ElementNames::MapEntityId> : private EntityIdParameterHandler
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const 
        {
            MappedMemberDefinition& md = state.Result.PropertyMappings.back().MappedMembers.back();
            md.Kind=MappedMemberDefinition::ValueMapping;
            try 
            {
                md.Value=GetVal(pt);
            }
            catch(const boost::property_tree::ptree_error&)
            {
                throw ParseError("Incomplete EntityId XML", "Missing 'name' and/or 'instanceId' element in EntityId property mapping value", state.CurrentPath);
            }
        }
    };

    template<> struct ParseAlgorithm<ElementNames::ParameterEntityId> : private EntityIdParameterHandler
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const 
        {
            try 
            {
                state.Result.Classes.back().Parameters.back().Values.push_back(GetVal(pt));
            }
            catch(const boost::property_tree::ptree_error&)
            {
                throw ParseError("Incomplete EntityId XML", "Missing 'name' and/or 'instanceId' element in EntityId parameter", state.CurrentPath);
            }
            
        }
    };
    
    template<> struct ParseAlgorithm<ElementNames::MapValueRef> : private ReferenceHandler
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const 
        {
            ParseState::ParameterReference ref(state.CurrentPath);
            GetReferencedParameter(pt, ref);
            ref.TopIndex=state.Result.PropertyMappings.size()-1;
            ref.SubIndex1=state.Result.PropertyMappings.back().MappedMembers.size()-1;
            state.MappedValueReferences.push_back(ref);            
        }
    };

    template<> struct ParseAlgorithm<ElementNames::ParameterValueRef> : private ReferenceHandler
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const 
        {
            ParseState::ParameterReference ref(state.CurrentPath);
            GetReferencedParameter(pt, ref);
            ref.TopIndex=state.Result.Classes.size()-1;
            ref.SubIndex1=state.Result.Classes.back().Parameters.size()-1;
            ref.SubIndex2=state.Result.Classes.back().Parameters.back().Values.size(); //index where to put value after its been resolved
            state.Result.Classes.back().Parameters.back().Values.push_back(""); //add placeholder for the value when it's resolved later
            state.ParamToParamReferences.push_back(ref);
        }
    };

    template<> struct ParseAlgorithm<ElementNames::MaxLengthRef> : private ReferenceHandler
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const 
        {
            ParseState::ParameterReference ref(state.CurrentPath);            
            GetReferencedParameter(pt, ref);
            ref.TopIndex=state.Result.Classes.size()-1;
            ref.SubIndex1=state.Result.Classes.back().Members.size()-1;
            state.MaxLengthReferences.push_back(ref);
        }
    };

    template<> struct ParseAlgorithm<ElementNames::ArraySizeRef> : private ReferenceHandler
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const 
        {
            state.Result.Classes.back().Members.back().IsArray=true;
            ParseState::ParameterReference ref(state.CurrentPath);
            GetReferencedParameter(pt, ref);
            ref.TopIndex=state.Result.Classes.size()-1;
            ref.SubIndex1=state.Result.Classes.back().Members.size()-1;
            state.ArraySizeReferences.push_back(ref);
        }
    };

    //Template specializations
    template<> struct ParseAlgorithm<ElementNames::Class>
    {
        void operator()(boost::property_tree::ptree& /*pt*/, ParseState& state) const
        {
            ClassDefinition cd;
            cd.FileName=state.CurrentPath;
            state.Result.Classes.push_back(cd);
        }
    };

    template<> struct ParseAlgorithm<ElementNames::Enumeration>
    {
        void operator()(boost::property_tree::ptree& /*pt*/, ParseState& state) const 
        {
            EnumerationDefinition e;
            e.FileName=state.CurrentPath;
            state.Result.Enumerations.push_back(e);
        }
    };
    
    template<> struct ParseAlgorithm<ElementNames::Exception>
    {
        void operator()(boost::property_tree::ptree& /*pt*/, ParseState& state) const 
        {
            ExceptionDefinition e;
            e.FileName=state.CurrentPath;
            state.Result.Exceptions.push_back(e);
        }
    };

    template<> struct ParseAlgorithm<ElementNames::Property>
    {
        void operator()(boost::property_tree::ptree& /*pt*/, ParseState& state) const 
        {
            PropertyDefinition p;
            p.FileName=state.CurrentPath;
            state.Result.Properties.push_back(p);
        }
    };

    template<> struct ParseAlgorithm<ElementNames::PropertyMapping>
    {
        void operator()(boost::property_tree::ptree& /*pt*/, ParseState& state) const 
        {
            PropertyMappingDefinition p;
            p.FileName=state.CurrentPath;
            state.Result.PropertyMappings.push_back(p);
        }
    };

    template<> struct ParseAlgorithm<ElementNames::Member>
    {
        void operator()(boost::property_tree::ptree& /*pt*/, ParseState& state) const {state.Result.Classes.back().Members.push_back(ClassMemberDefinition());}
    };

    template<> struct ParseAlgorithm<ElementNames::CreateRoutine>
    {
        void operator()(boost::property_tree::ptree& /*pt*/, ParseState& state) const {state.Result.Classes.back().CreateRoutines.push_back(CreateRoutineDefinition());}
    };

    template<> struct ParseAlgorithm<ElementNames::CreateRoutineValue>
    {
        void operator()(boost::property_tree::ptree& /*pt*/, ParseState& state) const {state.Result.Classes.back().CreateRoutines.back().MemberValues.push_back(MemberValue("", ""));}
    };

    template<> struct ParseAlgorithm<ElementNames::CreateRoutineValueMember>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.Result.Classes.back().CreateRoutines.back().MemberValues.back().first=pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::CreateRoutineValueParameter>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const 
        {
            ParseState::ParameterReference ref(state.CurrentPath);
            ref.ParameterIndex=0;
            ref.ParameterName=pt.data();
            ref.TopIndex=state.Result.Classes.size()-1;
            ref.SubIndex1=state.Result.Classes.back().CreateRoutines.size()-1;
            ref.SubIndex2=state.Result.Classes.back().CreateRoutines.back().MemberValues.size()-1;
            state.CreateRoutineValueReferences.push_back(ref);
        }
    };

    template<> struct ParseAlgorithm<ElementNames::Parameter>
    {
        void operator()(boost::property_tree::ptree& /*pt*/, ParseState& state) const {state.Result.Classes.back().Parameters.push_back(ParameterDefinition());}
    };

    template<> struct ParseAlgorithm<ElementNames::BaseClass>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.Result.Classes.back().BaseClass=pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::ExceptionBase>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.Result.Exceptions.back().BaseClass=pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::EnumerationValue>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.Result.Enumerations.back().EnumerationValues.push_back(pt.data());}
    };

    template<> struct ParseAlgorithm<ElementNames::MemberType>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.Result.Classes.back().Members.back().TypeName=pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::ParameterType>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.Result.Classes.back().Parameters.back().TypeName=pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::ClassName>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.Result.Classes.back().Name = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::ExceptionName>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.Result.Exceptions.back().Name = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::EnumerationName>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.Result.Enumerations.back().Name = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::PropertyName>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.Result.Properties.back().Name = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::MemberName>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.Result.Classes.back().Members.back().Name = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::ParameterName>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.Result.Classes.back().Parameters.back().Name = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::CreateRoutineName>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.Result.Classes.back().CreateRoutines.back().Name = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::ClassSummary>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.Result.Classes.back().Summary = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::ExceptionSummary>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.Result.Exceptions.back().Summary = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::EnumerationSummary>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.Result.Enumerations.back().Summary = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::PropertySummary>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.Result.Properties.back().Summary = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::PropertyMember>
    {
        void operator()(boost::property_tree::ptree& /*pt*/, ParseState& state) const {state.Result.Properties.back().Members.push_back(PropertyMemberDefinition());}
    };

    template<> struct ParseAlgorithm<ElementNames::PropertyMemberSummary>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.Result.Properties.back().Members.back().Summary=pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::PropertyMemberName>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.Result.Properties.back().Members.back().Name=pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::PropertyMemberType>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.Result.Properties.back().Members.back().TypeName=pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::PropertyMemberIsArray>
    {
        void operator()(boost::property_tree::ptree& /*pt*/, ParseState& state) const {state.Result.Properties.back().Members.back().IsArray=true;}
    };

    template<> struct ParseAlgorithm<ElementNames::PropertyMappingSummary>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.Result.PropertyMappings.back().Summary = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::MappedProperty>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.Result.PropertyMappings.back().PropertyName = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::MappedClass>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.Result.PropertyMappings.back().ClassName = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::MemberMapping>
    {
        void operator()(boost::property_tree::ptree& /*pt*/, ParseState& state) const 
        {
            MappedMemberDefinition md;
            md.Kind=MappedMemberDefinition::NullMapping;
            state.Result.PropertyMappings.back().MappedMembers.push_back(md);
        }
    };

    template<> struct ParseAlgorithm<ElementNames::MapPropertyMember>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.Result.PropertyMappings.back().MappedMembers.back().Name=pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::MapValue>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const 
        {
            MappedMemberDefinition& md = state.Result.PropertyMappings.back().MappedMembers.back();
            md.Kind=MappedMemberDefinition::ValueMapping;
            md.Value=pt.data();
        }
    };

    template<> struct ParseAlgorithm<ElementNames::ClassMemberReference>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const 
        {            
            std::string member = pt.get<std::string>(ElementNames::Instance().String(ElementNames::ClassMemberReferenceName));
            int index = pt.get(ElementNames::Instance().String(ElementNames::ClassMemberReferenceIndex), 0); //default index=0 if not present
            MappedMemberDefinition& md = state.Result.PropertyMappings.back().MappedMembers.back();
            md.Kind=MappedMemberDefinition::MemberMapping;
            md.MemberReferences.push_back(MemberReference(member, index));

            //Handle nested class memeber references by recursion.
            boost::optional<boost::property_tree::ptree&> nestedClassMemberRef = pt.get_child_optional("classMemberReference");
            if (nestedClassMemberRef)
            {
                this->operator()(nestedClassMemberRef.get(), state);
            }            
        }
    };

    template<> struct ParseAlgorithm<ElementNames::MemberSummary>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.Result.Classes.back().Members.back().Summary = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::ParameterSummary>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.Result.Classes.back().Parameters.back().Summary = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::CreateRoutineSummary>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.Result.Classes.back().CreateRoutines.back().Name = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::MaxLength>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const 
        {
            try
            {
                state.Result.Classes.back().Members.back().MaxLength = boost::lexical_cast<int, std::string>(pt.data());
            }
            catch (const boost::bad_lexical_cast&)
            {
                std::ostringstream ss;
                ss<<"The maxLength value specified in member '"<<state.Result.Classes.back().Members.back().Name<<"' can't be converted to a number.";
                throw ParseError("Invalid maxLength value", ss.str(), state.CurrentPath);
            }
        }
    };

    template<> struct ParseAlgorithm<ElementNames::ArraySize>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const 
        {
            state.Result.Classes.back().Members.back().IsArray=true;
            if (pt.data()=="dynamic")
            {
                state.Result.Classes.back().Members.back().ArraySize = -1;

            }
            else
            {
                try
                {
                    state.Result.Classes.back().Members.back().ArraySize = boost::lexical_cast<int, std::string>(pt.data());
                }
                catch (const boost::bad_lexical_cast&)
                {
                    std::ostringstream ss;
                    ss<<"The arraySize value specified in member '"<<state.Result.Classes.back().Members.back().Name<<"' can't be converted to a number.";
                    throw ParseError("Invalid arraySize value", ss.str(), state.CurrentPath);
                }
            }
        }
    };

    template<> struct ParseAlgorithm<ElementNames::ParameterArrayElements>
    {
        void operator()(boost::property_tree::ptree& /*pt*/, ParseState& state) const {state.Result.Classes.back().Parameters.back().IsArray=true;}
    };    

    template<> struct ParseAlgorithm<ElementNames::ParameterValue>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const 
        {            
            state.Result.Classes.back().Parameters.back().Values.push_back(pt.data());
        }
    };

    template<> struct ParseAlgorithm<ElementNames::CreateRoutineMemberName>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.Result.Classes.back().CreateRoutines.back().Parameters.push_back(pt.data());}
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
        bool IsOfType(const std::string& type, const std::string& ofType) const;
        void CheckNameAndFilenameConsistency(const std::string& filename, const std::string name) const;

        std::string ExpandEnvironmentVariables(const std::string& str) const;
        const ParameterDefinition* GetParameter(const std::string& name) const;
        const ClassMemberDefinition* GetMember(ClassDefinition& cls, const std::string& name) const;
        void ResolveReferences(ParseState::ParameterReferenceVector& vec, const std::string& refName, boost::function<void(ClassMemberDefinition&, int)> setVal);
        void ResolveCreateRoutineValues();
        void ResolvePropertyMappingValueRefs();
        void ResolveParameterToParameterRefs();
    };
}
}
}
}

#endif
