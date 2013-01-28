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
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/xml_parser.hpp>
#include <boost/lexical_cast.hpp>
#include "BasicTypes.h"
#include "ParseState.h"
#include "ElementNames.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{   
    struct ReferenceHandler
    {
        void GetReferencedParameter(boost::property_tree::ptree& pt, ParseState::ParameterReference& ref) const
        {
            ref.parameterName = pt.get<std::string>(ElementNames::Instance().String(ElementNames::ReferenceName));
            ref.parameterIndex = pt.get(ElementNames::Instance().String(ElementNames::ReferenceIndex), 0);
        }
    };

    struct MappedValueParameterHandler
    {
        void Insert(ParseState& state, const std::string& value) const
        {            
            PropertyMappingDefinition& propMapping = state.result->propertyMappings.back();
            MappedMemberDefinition& memberMapping = propMapping.mappedMembers.back();
            memberMapping.kind=MappedToParameter;
            std::string paramName="#"+memberMapping.name+"@"+propMapping.propertyName;
            memberMapping.memberReferences.push_back(MemberReference(propMapping.className+"."+paramName, -1));
            //Verify that class exists
            ClassDefinition* cls = state.GetClass(propMapping.className);
            if (cls)
            {
                ParameterDefinition param;
                param.values.push_back(ValueDefinition(value));
                //param.values.push_back(value);
                param.hidden=true;
                param.name=paramName;
                cls->parameters.push_back(param);
            }
            //If the class was not found its a parseError that will be detected after all dom-files have been parsed.
            //Since we have to check noraml parameterRef-mappings anyway, we check these types of pseudo-parameters at the same place.
        }
    };

    struct EntityIdParameterHandler
    {
        std::string GetVal(boost::property_tree::ptree& pt) const
        {
            std::string name = pt.get<std::string>(ElementNames::Instance().String(ElementNames::className));
            std::string inst = pt.get<std::string>(ElementNames::Instance().String(ElementNames::InstanceId));
            return name+std::string(", ")+inst;
        }        
    };

    struct ObjectParameterHandler
    {        
        std::string GetVal(boost::property_tree::ptree& pt) const
        {
            //static int count = 0;
            //std::cout<<"Object "<<(++count)<<std::endl;
            //TODO:
            //Here we can save quite much time by not calling write_xml, but instead copy or even better store a ref to pt for later parsing.
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

    template<> struct ParseAlgorithm<ElementNames::MapObject> : private ObjectParameterHandler, private MappedValueParameterHandler
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const { Insert(state, GetVal(pt)); }
    };

    template<> struct ParseAlgorithm<ElementNames::ParameterObject> : private ObjectParameterHandler
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.result->classes.back().parameters.back().values.push_back(ValueDefinition(GetVal(pt)));}
    };

    template<> struct ParseAlgorithm<ElementNames::MapEntityId> : private EntityIdParameterHandler, private MappedValueParameterHandler
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const 
        {
            try 
            {
                Insert(state, GetVal(pt));
            }
            catch(const boost::property_tree::ptree_error&)
            {
                throw ParseError("Incomplete EntityId XML", "Missing 'name' and/or 'instanceId' element in EntityId property mapping value", state.currentPath);
            }
        }
    };

    template<> struct ParseAlgorithm<ElementNames::ParameterEntityId> : private EntityIdParameterHandler
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const 
        {
            try 
            {
                state.result->classes.back().parameters.back().values.push_back(ValueDefinition(GetVal(pt)));
            }
            catch(const boost::property_tree::ptree_error&)
            {
                throw ParseError("Incomplete EntityId XML", "Missing 'name' and/or 'instanceId' element in EntityId parameter", state.currentPath);
            }
            
        }
    };
    
    template<> struct ParseAlgorithm<ElementNames::MapValueRef> : private ReferenceHandler
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const 
        {
            ParseState::ParameterReference ref(state.currentPath);
            GetReferencedParameter(pt, ref);
            PropertyMappingDefinition& propMapping = state.result->propertyMappings.back();
            propMapping.mappedMembers.back().memberReferences.push_back(MemberReference(ref.parameterName, ref.parameterIndex));
        }
    };

    template<> struct ParseAlgorithm<ElementNames::ParameterValueRef> : private ReferenceHandler
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const 
        {
            ParseState::ParameterReference ref(state.currentPath);
            GetReferencedParameter(pt, ref);
            ref.topIndex=state.result->classes.size()-1;
            ref.subIndex1=state.result->classes.back().parameters.size()-1;
            ref.subIndex2=state.result->classes.back().parameters.back().values.size(); //index where to put value after its been resolved
            state.result->classes.back().parameters.back().values.push_back(ValueDefinition()); //add placeholder for the value when it's resolved later
            state.paramToParamReferences.push_back(ref);
        }
    };

    template<> struct ParseAlgorithm<ElementNames::maxLengthRef> : private ReferenceHandler
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const 
        {
            ParseState::ParameterReference ref(state.currentPath);
            GetReferencedParameter(pt, ref);
            ref.topIndex=state.result->classes.size()-1;
            ref.subIndex1=state.result->classes.back().members.size()-1;
            state.maxLengthReferences.push_back(ref);
        }
    };

    template<> struct ParseAlgorithm<ElementNames::arraySizeRef> : private ReferenceHandler
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const 
        {
            state.result->classes.back().members.back().isArray=true;
            ParseState::ParameterReference ref(state.currentPath);
            GetReferencedParameter(pt, ref);
            ref.topIndex=state.result->classes.size()-1;
            ref.subIndex1=state.result->classes.back().members.size()-1;
            state.arraySizeReferences.push_back(ref);
        }
    };

    //Template specializations
    template<> struct ParseAlgorithm<ElementNames::Class>
    {
        void operator()(boost::property_tree::ptree& /*pt*/, ParseState& state) const
        {
            ClassDefinition cd;
            cd.fileName=state.currentPath;
            state.result->classes.push_back(cd);
        }
    };

    template<> struct ParseAlgorithm<ElementNames::Enumeration>
    {
        void operator()(boost::property_tree::ptree& /*pt*/, ParseState& state) const 
        {
            EnumerationDefinition e;
            e.fileName=state.currentPath;
            state.result->enumerations.push_back(e);
        }
    };
    
    template<> struct ParseAlgorithm<ElementNames::Exception>
    {
        void operator()(boost::property_tree::ptree& /*pt*/, ParseState& state) const 
        {
            ExceptionDefinition e;
            e.fileName=state.currentPath;
            state.result->exceptions.push_back(e);
        }
    };

    template<> struct ParseAlgorithm<ElementNames::Property>
    {
        void operator()(boost::property_tree::ptree& /*pt*/, ParseState& state) const 
        {
            PropertyDefinition p;
            p.fileName=state.currentPath;
            state.result->properties.push_back(p);
        }
    };

    template<> struct ParseAlgorithm<ElementNames::PropertyMapping>
    {
        void operator()(boost::property_tree::ptree& /*pt*/, ParseState& state) const 
        {
            PropertyMappingDefinition p;
            p.fileName=state.currentPath;
            state.result->propertyMappings.push_back(p);
        }
    };

    template<> struct ParseAlgorithm<ElementNames::Member>
    {
        void operator()(boost::property_tree::ptree& /*pt*/, ParseState& state) const {state.result->classes.back().members.push_back(MemberDefinition());}
    };

    template<> struct ParseAlgorithm<ElementNames::CreateRoutine>
    {
        void operator()(boost::property_tree::ptree& /*pt*/, ParseState& state) const {state.result->classes.back().createRoutines.push_back(CreateRoutineDefinition());}
    };

    template<> struct ParseAlgorithm<ElementNames::CreateRoutineValue>
    {
        void operator()(boost::property_tree::ptree& /*pt*/, ParseState& state) const {state.result->classes.back().createRoutines.back().memberValues.push_back(MemberValue("", MemberReference("",0)));}
    };

    template<> struct ParseAlgorithm<ElementNames::CreateRoutineValueMember>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.result->classes.back().createRoutines.back().memberValues.back().first=pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::CreateRoutineValueParameter>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const 
        {
            MemberReference& paramRef=state.result->classes.back().createRoutines.back().memberValues.back().second;
            paramRef.first=pt.data();
        }
    };

    template<> struct ParseAlgorithm<ElementNames::Parameter>
    {
        void operator()(boost::property_tree::ptree& /*pt*/, ParseState& state) const {state.result->classes.back().parameters.push_back(ParameterDefinition());}
    };

    template<> struct ParseAlgorithm<ElementNames::baseClass>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.result->classes.back().baseClass=pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::ExceptionBase>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.result->exceptions.back().baseClass=pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::EnumerationValue>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.result->enumerations.back().enumerationValues.push_back(pt.data());}
    };

    template<> struct ParseAlgorithm<ElementNames::MemberType>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.result->classes.back().members.back().typeName=pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::ParameterType>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.result->classes.back().parameters.back().typeName=pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::className>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const
        {
            state.result->classes.back().name = pt.data();
            DotsC_TypeId tid = DotsId_Generate64(pt.data().c_str());
            if (!state.index.insert(std::make_pair(tid, state.result->classes.size()-1)).second)
            {
                std::ostringstream ss;
                ss<<"The type '"<<state.result->classes.back().name<<"' is already defined. ";
                throw ParseError("Duplicated type definition", ss.str(), state.currentPath);
            }
        }
    };

    template<> struct ParseAlgorithm<ElementNames::ExceptionName>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const
        {
            state.result->exceptions.back().name = pt.data();
            DotsC_TypeId tid = DotsId_Generate64(pt.data().c_str());
            if (!state.index.insert(std::make_pair(tid, state.result->exceptions.size()-1)).second)
            {
                std::ostringstream ss;
                ss<<"The type '"<<state.result->exceptions.back().name<<"' is already defined. ";
                throw ParseError("Duplicated type definition", ss.str(), state.currentPath);
            }
        }
    };

    template<> struct ParseAlgorithm<ElementNames::EnumerationName>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const
        {
            state.result->enumerations.back().name = pt.data();
            DotsC_TypeId tid = DotsId_Generate64(pt.data().c_str());
            if (!state.index.insert(std::make_pair(tid, state.result->enumerations.size()-1)).second)
            {
                std::ostringstream ss;
                ss<<"The type '"<<state.result->enumerations.back().name<<"' is already defined. ";
                throw ParseError("Duplicated type definition", ss.str(), state.currentPath);
            }
        }
    };

    template<> struct ParseAlgorithm<ElementNames::propertyName>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const
        {
            state.result->properties.back().name = pt.data();
            DotsC_TypeId tid = DotsId_Generate64(pt.data().c_str());
            if (!state.index.insert(std::make_pair(tid, state.result->properties.size()-1)).second)
            {
                std::ostringstream ss;
                ss<<"The type '"<<state.result->properties.back().name<<"' is already defined. ";
                throw ParseError("Duplicated type definition", ss.str(), state.currentPath);
            }
        }
    };

    template<> struct ParseAlgorithm<ElementNames::MemberName>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.result->classes.back().members.back().name = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::ParameterName>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.result->classes.back().parameters.back().name = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::CreateRoutineName>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.result->classes.back().createRoutines.back().name = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::Classsummary>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.result->classes.back().summary = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::Exceptionsummary>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.result->exceptions.back().summary = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::Enumerationsummary>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.result->enumerations.back().summary = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::Propertysummary>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.result->properties.back().summary = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::PropertyMember>
    {
        void operator()(boost::property_tree::ptree& /*pt*/, ParseState& state) const {state.result->properties.back().members.push_back(MemberDefinition());}
    };

    template<> struct ParseAlgorithm<ElementNames::PropertyMembersummary>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.result->properties.back().members.back().summary=pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::PropertyMemberName>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.result->properties.back().members.back().name=pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::PropertyMemberType>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.result->properties.back().members.back().typeName=pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::PropertyMemberisArray>
    {
        void operator()(boost::property_tree::ptree& /*pt*/, ParseState& state) const {state.result->properties.back().members.back().isArray=true;}
    };

    template<> struct ParseAlgorithm<ElementNames::PropertyMappingsummary>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.result->propertyMappings.back().summary = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::MappedProperty>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.result->propertyMappings.back().propertyName = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::MappedClass>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.result->propertyMappings.back().className = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::MemberMapping>
    {
        void operator()(boost::property_tree::ptree& /*pt*/, ParseState& state) const 
        {
            MappedMemberDefinition md;
            md.kind=MappedToNull;
            state.result->propertyMappings.back().mappedMembers.push_back(md);
        }
    };

    template<> struct ParseAlgorithm<ElementNames::MapPropertyMember>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.result->propertyMappings.back().mappedMembers.back().name=pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::MapValue> : private MappedValueParameterHandler
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {Insert(state, pt.data());}
    };

    template<> struct ParseAlgorithm<ElementNames::ClassMemberReference>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const 
        {            
            std::string member = pt.get<std::string>(ElementNames::Instance().String(ElementNames::ClassMemberReferenceName));
            int index = pt.get(ElementNames::Instance().String(ElementNames::ClassMemberReferenceIndex), -1); //default index=-1 if not present
            MappedMemberDefinition& md = state.result->propertyMappings.back().mappedMembers.back();
            md.kind=MappedToMember;
            md.memberReferences.push_back(MemberReference(member, index));

            //Handle nested class memeber references by recursion.
            boost::optional<boost::property_tree::ptree&> nestedClassMemberRef = pt.get_child_optional("classMemberReference");
            if (nestedClassMemberRef)
            {
                this->operator()(nestedClassMemberRef.get(), state);
            }
        }
    };

    template<> struct ParseAlgorithm<ElementNames::Membersummary>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.result->classes.back().members.back().summary = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::Parametersummary>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.result->classes.back().parameters.back().summary = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::CreateRoutinesummary>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.result->classes.back().createRoutines.back().name = pt.data();}
    };

    template<> struct ParseAlgorithm<ElementNames::maxLength>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const 
        {
            try
            {
                state.result->classes.back().members.back().maxLength = boost::lexical_cast<int, std::string>(pt.data());
            }
            catch (const boost::bad_lexical_cast&)
            {
                std::ostringstream ss;
                ss<<"The maxLength value specified in member '"<<state.result->classes.back().members.back().name<<"' can't be converted to a number.";
                throw ParseError("Invalid maxLength value", ss.str(), state.currentPath);
            }
        }
    };

    template<> struct ParseAlgorithm<ElementNames::arraySize>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const 
        {
            state.result->classes.back().members.back().isArray=true;
            if (pt.data()=="dynamic")
            {
                state.result->classes.back().members.back().arraySize = -1;

            }
            else
            {
                try
                {
                    state.result->classes.back().members.back().arraySize = boost::lexical_cast<int, std::string>(pt.data());
                }
                catch (const boost::bad_lexical_cast&)
                {
                    std::ostringstream ss;
                    ss<<"The arraySize value specified in member '"<<state.result->classes.back().members.back().name<<"' can't be converted to a number.";
                    throw ParseError("Invalid arraySize value", ss.str(), state.currentPath);
                }
            }
        }
    };

    template<> struct ParseAlgorithm<ElementNames::ParameterArrayElements>
    {
        void operator()(boost::property_tree::ptree& /*pt*/, ParseState& state) const {state.result->classes.back().parameters.back().isArray=true;}
    };    

    template<> struct ParseAlgorithm<ElementNames::ParameterValue>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const 
        {            
            state.result->classes.back().parameters.back().values.push_back(ValueDefinition(pt.data()));
        }
    };

    template<> struct ParseAlgorithm<ElementNames::CreateRoutineMemberName>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.result->classes.back().createRoutines.back().parameters.push_back(pt.data());}
    };


    //------------------------------------
    // Post xml-parsing algorithms
    //------------------------------------
    class ParseResultFinalizer : public boost::noncopyable
    {
    public:
        explicit ParseResultFinalizer(ParseState& state) : m_state(state) {}
        void ProcessDouResults();
        void ProcessDomResults();
    private:
        ParseState& m_state;

        void ProcessEnum(EnumerationDefinition& e);
        void ProcessException(ExceptionDefinition& e);
        void ProcessProperty(PropertyDefinition& p);
        void ProcessClass(ClassDefinition& c);
        void ProcessPropertyMapping(PropertyMappingDefinition& p);
        void ProcessPropertyMappedToParameter(PropertyMappingDefinition& mapping, PropertyDefinition& property, ClassDefinition& cls, MappedMemberDefinition& member);
        void ProcessPropertyMappedToClassMember(PropertyMappingDefinition& mapping, PropertyDefinition& property, ClassDefinition& cls, MappedMemberDefinition& member);

        //Sub process-helpers for ProcessClass
        void ProcessCreateRoutine(ClassDefinition& host, CreateRoutineDefinition& c);
        void ProcessClassMember(ClassDefinition& host, MemberDefinition& m);
        void ProcessParameter(ClassDefinition& host, ParameterDefinition& p);

        bool ValidName(const std::string& name, bool allowDot) const;
        bool IsOfType(const std::string& type, const std::string& ofType) const;
        void CheckNameAndFilenameConsistency(const std::string& filename, const std::string name) const;

        std::string ExpandEnvironmentVariables(const std::string& str) const;
        void ResolveReferences(ParseState::ParameterReferenceVector& vec, const std::string& refName, boost::function<void(MemberDefinition&, int)> setVal);
        void ResolveParameterToParameterRefs();
    };
}
}
}
}

#endif
