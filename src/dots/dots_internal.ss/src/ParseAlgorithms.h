/******************************************************************************
*
* Copyright Saab AB, 2004-2013 (http://safir.sourceforge.net)
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
* GNU General Public License for more Internals.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/
#ifndef __DOTS_INTERNAL_PARSE_ALGORITHMS_H__
#define __DOTS_INTERNAL_PARSE_ALGORITHMS_H__

#include <iostream>
#include <set>
#include <boost/algorithm/string.hpp>
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/xml_parser.hpp>
#include <boost/lexical_cast.hpp>
#include <Safir/Dob/Typesystem/ToolSupport/Internal/XmlToBlobSerializer.h>
#include "ParseState.h"
#include "ElementNames.h"

//--------------------------------------------------------------------------------------------------------------
//This file contains all algoritms that is performed during the xml-parsing. Work done here must be fully
//parallelizable since parsing may be done in many threads simultanously. The work that cannot be done before
//all types are known or cant be done in parallel is done in the RepositoryCompletionAlgorithms.
//--------------------------------------------------------------------------------------------------------------
namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace ToolSupport
{
    //------------------------------------------
    // Helper functions
    //------------------------------------------
    bool ValidName(const std::string& name);
    void CheckNameAndFilenameConsistency(const std::string& filename, const std::string name);
    //Resolves references on the form <...><name>param</name>123<index></index></...>
    void GetReferencedParameter(boost::property_tree::ptree& pt, std::string& paramName, int& paramIndex);
    int GetReferencedIndex(boost::property_tree::ptree& pt, ParseState& state);
    std::string GetEntityIdParameterAsString(boost::property_tree::ptree& pt);
    bool ParseValue(DotsC_MemberType memberType, const std::string& val, ValueDefinition& result);

    template <class Ptr>
    bool NameComparerPtr(const Ptr& obj, const std::string& name) {return obj->name==name;}

    template <class Descr>
    void CreateTopLevelDefinition(boost::property_tree::ptree& pt,
                                  const std::string& currentPath,
                                  const std::string& elementName,
                                  const boost::function< bool(const boost::shared_ptr<Descr>&) >& insert,
                                  boost::shared_ptr<Descr>& lastInserted)
    {
        try
        {
            boost::shared_ptr<Descr> val(new Descr);
            val->fileName=currentPath;
            val->name=pt.get<std::string>(elementName);
            val->typeId=LlufId_Generate64(val->name.c_str());
            if (!ValidName(val->name))
            {
                throw ParseError("Invalid name", elementName + std::string(" name '")+val->name+std::string(" is invalid. Must start with an alphabetic char and then only contain alpha-numeric chars"), val->fileName, 1);
            }
            CheckNameAndFilenameConsistency(val->fileName, val->name);

            if (insert(val))
            {
                lastInserted=val;
            }
            else //already exists
            {
                std::ostringstream ss;
                ss<<"The type '"<<val->name<<"' is already defined. ";
                throw ParseError("Duplicated type definition", ss.str(), currentPath, 10);
            }
        }
        catch (const boost::property_tree::ptree_error&)
        {
            throw ParseError("Missing element", elementName + " is missing in definition. '", currentPath, 3);
        }
    }

    //-----------------------------------------------
    // DOU file algorithms
    //-----------------------------------------------
    //Default for non specialized elements, still needed since occurrance checks will be done by calling this for empty elements
    template <class ElementT> struct ParseAlgorithm
    {
        void operator()(boost::property_tree::ptree& /*pt*/, ParseState& /*state*/) const {}
    };

    template<> struct ParseAlgorithm<Elements::ParameterObjectDeprecated>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const
        {
            ParameterDescriptionBasicPtr& def=state.lastInsertedClass->ownParameters.back();
            if (def->memberType!=ObjectMemberType)
            {
                std::ostringstream os;
                os<<"The parameter "<<def->name<<" in class "<<state.lastInsertedClass->name<<" is declared as type '"<<def->typeName<<"' which is not an object";
                throw ParseError("Invalid parameter value", os.str(), state.currentPath, 35);
            }

            state.objectParameters.push_back(ParseState::ObjectParameter(state.lastInsertedClass, def, def->values.size(), &pt, state.propertyTree));
            state.objectParameters.back().deprecatedXmlFormat=true;
            def->values.push_back(ValueDefinition()); //placeholder
        }
    };

    template<> struct ParseAlgorithm<Elements::ParameterObject>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const
        {
            ParameterDescriptionBasicPtr& def=state.lastInsertedClass->ownParameters.back();
            if (def->memberType!=ObjectMemberType)
            {
                std::ostringstream os;
                os<<"The parameter "<<def->name<<" in class "<<state.lastInsertedClass->name<<" is declared as type '"<<def->typeName<<"' which is not an object";
                throw ParseError("Invalid parameter value", os.str(), state.currentPath, 38);
            }

            state.objectParameters.push_back(ParseState::ObjectParameter(state.lastInsertedClass, def, def->values.size(), &pt, state.propertyTree));
            def->values.push_back(ValueDefinition()); //placeholder
        }
    };

    template<> struct ParseAlgorithm<Elements::ParameterEntityId>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const
        {
            ParameterDescriptionBasicPtr& def=state.lastInsertedClass->ownParameters.back();
            if (def->memberType!=EntityIdMemberType)
            {
                std::ostringstream os;
                os<<"The parameter "<<def->name<<" in class "<<state.lastInsertedClass->name<<" is declared as type '"<<def->typeName<<"' whch is not an entityId";
                throw ParseError("Invalid parameter value", os.str(), state.currentPath, 36);
            }

            try
            {
                ValueDefinition val;
                if (ParseValue(def->memberType, GetEntityIdParameterAsString(pt), val))
                {
                    def->values.push_back(val);
                }
                else
                {
                    std::ostringstream os;
                    os<<"The value '"<<val.stringVal<<"' doesn't match the type "<<def->typeName<<" for parameter "<<def->name<<" in class "<<state.lastInsertedClass->name;
                    throw ParseError("Invalid parameter value", os.str(), state.currentPath, 34);
                }
            }
            catch(const boost::property_tree::ptree_error&)
            {
                throw ParseError("Incomplete EntityId XML", "Missing 'name' and/or 'instanceId' element in EntityId parameter", state.currentPath, 5);
            }
            catch(const std::string& var)
            {
                throw ParseError("Incomplete EntityId XML", "Failed to expand environment variable '"+var+"' in EntityId parameter", state.currentPath, 139);
            }
        }
    };

    template<> struct ParseAlgorithm<Elements::ParameterValueRef>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const
        {
            try
            {
                std::string paramName;
                int paramIx;
                GetReferencedParameter(pt, paramName, paramIx);
                ParseState::ParameterReference<ParameterDescriptionBasic> ref(state.lastInsertedClass,
                                                                              state.lastInsertedClass->ownParameters.back(),
                                                                              state.lastInsertedClass->ownParameters.back()->values.size(),
                                                                              paramName, paramIx);
                ref.referee.referencingItem->values.push_back(ValueDefinition(RefKind)); //add placeholder for the value when it's resolved later
                state.paramToParamReferences.push_back(ref);
            }
            catch (...)
            {
                std::ostringstream os;
                os<<"Missing <name> element in parameter reference. In parameter "<<state.lastInsertedClass->ownParameters.back()->name<<
                    " in class "<<state.lastInsertedClass->name;
                throw ParseError("Invalid parameter reference syntax", os.str(), state.currentPath, 31);
            }
        }
    };

    template<> struct ParseAlgorithm<Elements::MaxLengthRef>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const
        {
            try
            {
                std::string paramName;
                int paramIx;
                GetReferencedParameter(pt, paramName, paramIx);
                ParseState::ParameterReference<MemberDescriptionBasic> ref(state.lastInsertedClass,
                                                                           state.lastInsertedClass->members.back(),
                                                                           0,
                                                                           paramName, paramIx);
                state.maxLengthReferences.push_back(ref);
            }
            catch (...)
            {
                std::ostringstream os;
                os<<"Missing <name> element in maxLengthRef. In member "<<state.lastInsertedClass->members.back()->name<<
                    " in class "<<state.lastInsertedClass->name;
                throw ParseError("Invalid parameter reference syntax", os.str(), state.currentPath, 32);
            }
        }
    };

    template<> struct ParseAlgorithm<Elements::ArraySizeRef>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const
        {
            state.lastInsertedClass->members.back()->isArray=true;
            try
            {
                std::string paramName;
                int paramIx;
                GetReferencedParameter(pt, paramName, paramIx);
                ParseState::ParameterReference<MemberDescriptionBasic> ref(state.lastInsertedClass,
                                                                           state.lastInsertedClass->members.back(),
                                                                           0,
                                                                           paramName, paramIx);
                state.arraySizeReferences.push_back(ref);
            }
            catch (...)
            {
                std::ostringstream os;
                os<<"Missing <name> element in arraySizeRef. In member "<<state.lastInsertedClass->members.back()->name<<
                    " in class "<<state.lastInsertedClass->name;
                throw ParseError("Invalid parameter reference syntax", os.str(), state.currentPath, 33);
            }
        }
    };

    //Template specializations
    template<> struct ParseAlgorithm<Elements::Class>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const
        {
            boost::function< bool(const ClassDescriptionBasicPtr&) > insert=boost::bind(&RepositoryBasic::InsertClass, state.repository, _1);
            CreateTopLevelDefinition(pt,
                                     state.currentPath,
                                     Elements::ClassName::Name(),
                                     insert,
                                     state.lastInsertedClass);
        }
    };


    template<> struct ParseAlgorithm<Elements::Enumeration>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const
        {
            boost::function< bool(const EnumDescriptionBasicPtr&) > insert=boost::bind(&RepositoryBasic::InsertEnum, state.repository, _1);
            CreateTopLevelDefinition(pt,
                                     state.currentPath,
                                     Elements::EnumerationName::Name(),
                                     insert,
                                     state.lastInsertedEnum);
        }
    };
    
    template<> struct ParseAlgorithm<Elements::Exception>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const
        {
            boost::function< bool(const ExceptionDescriptionBasicPtr&) > insert=boost::bind(&RepositoryBasic::InsertException, state.repository, _1);
            CreateTopLevelDefinition(pt,
                                     state.currentPath,
                                     Elements::ExceptionName::Name(),
                                     insert,
                                     state.lastInsertedException);
        }
    };

    template<> struct ParseAlgorithm<Elements::Property>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const
        {
            boost::function< bool(const PropertyDescriptionBasicPtr&) > insert=boost::bind(&RepositoryBasic::InsertProperty, state.repository, _1);
            CreateTopLevelDefinition(pt,
                                     state.currentPath,
                                     Elements::PropertyName::Name(),
                                     insert,
                                     state.lastInsertedProperty);
        }
    };

    template<> struct ParseAlgorithm<Elements::Member>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const
        {
            MemberDescriptionBasicPtr def(new MemberDescriptionBasic);
            try
            {
                 def->name=pt.get<std::string>(Elements::MemberName::Name());
                 SerializationUtils::Trim(def->name);
            }
            catch (const boost::property_tree::ptree_error&)
            {
                throw ParseError("Missing element", "Element 'name' is missing for a member in class '"+state.lastInsertedClass->name+"'.", state.currentPath, 17);
            }

            if (!ValidName(def->name))
            {
                throw ParseError("Invalid name", "Class name '"+def->name+" is invalid. Must start with an alphabetic char and then only contain alpha-numeric chars", state.currentPath, 18);
            }

            //check for duplicates
            if (std::find_if(state.lastInsertedClass->members.begin(), state.lastInsertedClass->members.end(),
                             boost::bind(NameComparerPtr<MemberDescriptionBasicPtr>, _1, def->name))!=state.lastInsertedClass->members.end())
            {
                std::ostringstream os;
                os<<"A member with name '"<<def->name<<"' is defined more than one time in class "<<state.lastInsertedClass->name;
                throw ParseError("Duplicated member", os.str(), state.currentPath, 175);
            }

            state.lastInsertedClass->members.push_back(def);
        }
    };

    template<> struct ParseAlgorithm<Elements::MemberType>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const
        {
            SerializationUtils::Trim(pt.data());
            state.lastInsertedClass->members.back()->typeName=pt.data();
            if (!BasicTypeOperations::IsBasicTypeName(pt.data(), state.lastInsertedClass->members.back()->memberType))
            {
                //not a basic type, we have to check later if its an enum or class type, for now we assume class
                state.lastInsertedClass->members.back()->memberType=ObjectMemberType;
            }
        }
    };

    template<> struct ParseAlgorithm<Elements::CreateRoutine>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const
        {
            CreateRoutineDescriptionBasicPtr def(new CreateRoutineDescriptionBasic(state.lastInsertedClass.get()));
            try
            {
                def->name=pt.get<std::string>(Elements::CreateRoutineName::Name());
                SerializationUtils::Trim(def->name);
            }
            catch(const boost::property_tree::ptree_error&)
            {
                throw ParseError("Missing Element", "CreateRoutine is missing <name> element", state.currentPath, 23);
            }

            if (!ValidName(def->name))
            {
                throw ParseError("Invalid name", "CreateRoutine with name '"+def->name+"' in class "+state.lastInsertedClass->name+" is not valid.", state.currentPath, 24);
            }

            //Extract the parameters and create the signature for createRoutine 'MyNamespace.MyClass.MyCreateRoutine#param1#...#paramN
            std::ostringstream signature;
            signature<<state.lastInsertedClass->name<<"."<<def->name;
            boost::optional<boost::property_tree::ptree&> parameters=pt.get_child_optional(Elements::CreateRoutineParameterList::Name());
            if (parameters)
            {
                for (boost::property_tree::ptree::iterator it=parameters->begin(); it!=parameters->end(); ++it)
                {
                    if (it->first==Elements::CreateRoutineMemberName::Name())
                    {
                        SerializationUtils::Trim(it->second.data());
                        signature<<"#"<<it->second.data();

                        //Check for duplicates
                        StringVector::const_iterator foundIt=std::find(def->parameters.begin(), def->parameters.end(), it->second.data());
                        if (foundIt!=def->parameters.end())
                        {
                            std::ostringstream os;
                            os<<"Parameter '"<<it->second.data()<<"' exists more than one time in createRoutine "<<def->name<<" in class "<<state.lastInsertedClass->name;
                            throw ParseError("Duplicated CreateRoutine parameter", os.str(), state.currentPath, 25);
                        }

                        def->parameters.push_back(it->second.data());
                    }
                }
            }
            def->signature=signature.str();

            //Check for createRoutines with same signature
            for (std::vector<CreateRoutineDescriptionBasicPtr>::const_iterator crIt=state.lastInsertedClass->createRoutines.begin(); crIt!=state.lastInsertedClass->createRoutines.end(); ++crIt)
            {
                if ((*crIt)->signature==def->signature)
                {
                    //Same signature is not allowed
                    std::ostringstream os;
                    os<<"The class '"<<state.lastInsertedClass->name<<"' contains CreatRoutines with the same name and identical parameters. Create routine name '"<<def->name<<"' signature("<<def->signature<<")";
                    throw ParseError("Create routines with identical signature", os.str(), state.currentPath, 26);
                }
            }

            state.lastInsertedClass->createRoutines.push_back(def);
        }
    };

    template<> struct ParseAlgorithm<Elements::CreateRoutineValue>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const
        {
            CreateRoutineDescriptionBasicPtr& def=state.lastInsertedClass->createRoutines.back();
            //We get the member here to since it is mandatory and it simplifies the parsing of CreateRoutineValueXXX if we are sure we alredy know the member name.
            try
            {
                std::string memberName=pt.get<std::string>(Elements::CreateRoutineValueMember::Name());
                SerializationUtils::Trim(memberName);

                //Check duplicates
                StringVector::const_iterator foundIt=std::find(def->parameters.begin(), def->parameters.end(), memberName);
                if (foundIt!=def->parameters.end())
                {
                    std::ostringstream os;
                    os<<"Member '"<<memberName<<"' in createRoutine "<<def->name<<" in class "<<state.lastInsertedClass->name<<
                        " is already definded as a createRoutineParameter. It is not allowed to declare the same classMember as createRoutineMember and createRoutineParameter.";

                    throw ParseError("Duplicated CreateRoutine member", os.str(), state.currentPath, 27);
                }
                for (MemberValueVector::const_iterator it=def->memberValues.begin(); it!=def->memberValues.end(); ++it)
                {
                    if (it->first==memberName)
                    {
                        std::ostringstream os;
                        os<<"Member '"<<memberName<<"' already has a specified value in createRoutine "<<def->name<<" in class "<<state.lastInsertedClass->name;
                        throw ParseError("Duplicated CreateRoutine member", os.str(), state.currentPath, 28);
                    }
                }

                def->memberValues.push_back(MemberValue(memberName, MemberReference("",0)));
            }
            catch(const boost::property_tree::ptree_error&)
            {
                std::ostringstream os;
                os<<"A createRoutine value in CreateRoutine '"<<def->GetName()<<"' is missing the <member>-element";
                throw ParseError("Incomplete CreateRoutine XML", os.str(), state.currentPath, 39);
            }
        }
    };

    template<> struct ParseAlgorithm<Elements::CreateRoutineValueParameter>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const
        {
            try
            {
                MemberValue& memVal=state.lastInsertedClass->createRoutines.back()->memberValues.back();
                GetReferencedParameter(pt, memVal.second.first, memVal.second.second);
            }
            catch (...)
            {
                std::ostringstream os;
                os<<"Missing <name> element in parameter reference. In createRoutine "<<state.lastInsertedClass->createRoutines.back()->name<<
                    " in class "<<state.lastInsertedClass->name;
                throw ParseError("Invalid parameter reference syntax", os.str(), state.currentPath, 29);

                //This is for backward compatibility. If it ok to change syntax throw exception above instead.
                //Reason for change is that CreateRoutine.Values only allow to specify a parameter name and no index
                //Instead we should use the same sytax as for maxLenghtRef and arraySizeRef: <...><name>MyParam</name><index>123</index></...>
                //and remove the code here:
                // --- BEGIN to be removed ----
//                MemberValue& memVal=state.lastInsertedClass->createRoutines.back()->memberValues.back();
//                memVal.second.first=pt.data();
//                memVal.second.second=0;
                // --- END to be removed ----
            }
        }
    };

    template<> struct ParseAlgorithm<Elements::CreateRoutineValueValue>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const
        {
            SerializationUtils::Trim(pt.data());

            //Add hidden parameter
            //member@signature -> MyClassMember@MyNamespace.MyClass.MyCreateRoutine#param1#...#paramN
            CreateRoutineDescriptionBasicPtr& def=state.lastInsertedClass->createRoutines.back();
            MemberValue& memVal=def->memberValues.back();
            std::ostringstream paramName;
            paramName<<memVal.first<<"@"<<def->signature;
            memVal.second.first=paramName.str();
            memVal.second.second=0;

            ParameterDescriptionBasicPtr par(new ParameterDescriptionBasic);
            par->qualifiedName=paramName.str();
            par->name=par->qualifiedName;
            par->hidden=true;
            par->isArray=false;
            par->memberType=Int32MemberType; //just to indicate that type is not object or entityId, it is a basicType or enum
            ValueDefinition val;
            val.kind=ValueKind;
            val.stringVal=pt.data();
            par->values.push_back(val);
            state.lastInsertedClass->ownParameters.push_back(par);
            state.repository->InsertParameter(par);

            //type is still missing, add for later processing
            ParseState::ParameterReference<CreateRoutineDescriptionBasic> ref(state.lastInsertedClass,
                                                                              def,
                                                                              def->memberValues.size()-1,
                                                                              par->GetName(), 0);
            state.createRoutineIncompleteHiddenParameters.push_back(ref);
        }
    };

    template<> struct ParseAlgorithm<Elements::CreateRoutineValueEntityId>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const
        {
            //Add hidden parameter
            //member@signature -> MyClassMember@MyNamespace.MyClass.MyCreateRoutine#param1#...#paramN
            CreateRoutineDescriptionBasicPtr& def=state.lastInsertedClass->createRoutines.back();
            MemberValue& memVal=def->memberValues.back();
            std::ostringstream paramName;
            paramName<<memVal.first<<"@"<<def->signature;
            memVal.second.first=paramName.str();
            memVal.second.second=0;

            ValueDefinition val;
            val.kind=ValueKind;
            try
            {
                if (!ParseValue(EntityIdMemberType,
                                GetEntityIdParameterAsString(pt),
                                val))
                {
                    std::ostringstream os;
                    os<<"The value '"<<val.stringVal<<"' doesn't match the type entityId for createRoutine "<<def->name<<" in class "<<state.lastInsertedClass->name;
                    throw ParseError("Invalid create routine value", os.str(), state.currentPath, 62);
                }
            }
            catch(const boost::property_tree::ptree_error&)
            {
                throw ParseError("Incomplete EntityId XML", "Missing 'name' and/or 'instanceId' element in EntityId value in createRoutine "+def->name, state.currentPath, 4);
            }
            catch(const std::string& envVar)
            {
                throw ParseError("Incomplete EntityId XML", "Failed to expand environment variable '"+envVar+"' in CreateRoutine "+def->GetName()+" member "+memVal.first, state.currentPath, 140);
            }

            ParameterDescriptionBasicPtr par(new ParameterDescriptionBasic);
            par->qualifiedName=paramName.str();
            par->name=par->qualifiedName;
            par->hidden=true;
            par->isArray=false;
            par->memberType=EntityIdMemberType;
            par->typeName=BasicTypeOperations::MemberTypeToString(EntityIdMemberType);
            par->values.push_back(val);
            state.lastInsertedClass->ownParameters.push_back(par);
            state.repository->InsertParameter(par);
        }
    };

    template<> struct ParseAlgorithm<Elements::CreateRoutineValueObject>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const
        {
            //Add hidden parameter
            //member@signature -> MyClassMember@MyNamespace.MyClass.MyCreateRoutine#param1#...#paramN
            CreateRoutineDescriptionBasicPtr& def=state.lastInsertedClass->createRoutines.back();
            MemberValue& memVal=def->memberValues.back();
            std::ostringstream paramName;
            paramName<<memVal.first<<"@"<<def->signature;
            memVal.second.first=paramName.str();
            memVal.second.second=0;

            ParameterDescriptionBasicPtr par(new ParameterDescriptionBasic);
            par->qualifiedName=paramName.str();
            par->name=par->qualifiedName;
            par->hidden=true;
            par->isArray=false;
            par->memberType=ObjectMemberType;
            ValueDefinition val;
            val.kind=ValueKind;
            val.stringVal=pt.data();
            par->values.push_back(val);
            state.lastInsertedClass->ownParameters.push_back(par);
            state.objectParameters.push_back(ParseState::ObjectParameter(state.lastInsertedClass,
                                                                         par,
                                                                         par->values.size()-1,
                                                                         &pt, state.propertyTree));
            state.repository->InsertParameter(par);

            //type is still missing, add for later processing
            ParseState::ParameterReference<CreateRoutineDescriptionBasic> ref(state.lastInsertedClass,
                                                                              def,
                                                                              def->memberValues.size()-1,
                                                                              par->GetName(), 0);
            state.createRoutineIncompleteHiddenParameters.push_back(ref);
        }
    };

    template<> struct ParseAlgorithm<Elements::CreateRoutineValueObjectDeprecated>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const
        {
            //Add hidden parameter
            //member@signature -> MyClassMember@MyNamespace.MyClass.MyCreateRoutine#param1#...#paramN
            CreateRoutineDescriptionBasicPtr& def=state.lastInsertedClass->createRoutines.back();
            MemberValue& memVal=def->memberValues.back();
            std::ostringstream paramName;
            paramName<<memVal.first<<"@"<<def->signature;
            memVal.second.first=paramName.str();
            memVal.second.second=0;

            ParameterDescriptionBasicPtr par(new ParameterDescriptionBasic);
            par->qualifiedName=paramName.str();
            par->name=par->qualifiedName;
            par->hidden=true;
            par->isArray=false;
            par->memberType=ObjectMemberType;
            ValueDefinition val;
            val.kind=ValueKind;
            val.stringVal=pt.data();
            par->values.push_back(val);
            state.lastInsertedClass->ownParameters.push_back(par);
            state.objectParameters.push_back(ParseState::ObjectParameter(state.lastInsertedClass,
                                                                         par,
                                                                         par->values.size()-1,
                                                                         &pt, state.propertyTree));
            state.objectParameters.back().deprecatedXmlFormat=true;
            state.repository->InsertParameter(par);

            //type is still missing, add for later processing
            ParseState::ParameterReference<CreateRoutineDescriptionBasic> ref(state.lastInsertedClass,
                                                                              def,
                                                                              def->memberValues.size()-1,
                                                                              par->GetName(), 0);
            state.createRoutineIncompleteHiddenParameters.push_back(ref);
        }
    };

    template<> struct ParseAlgorithm<Elements::Parameter>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const
        {
            ParameterDescriptionBasicPtr def(new ParameterDescriptionBasic);

            //Check parameter name
            try
            {
                def->name=pt.get<std::string>(Elements::ParameterName::Name());
                SerializationUtils::Trim(def->name);
                std::vector<ParameterDescriptionBasicPtr>::const_iterator found=std::find_if(state.lastInsertedClass->ownParameters.begin(),
                                                                                             state.lastInsertedClass->ownParameters.end(),
                                                                                             boost::bind(NameComparerPtr<ParameterDescriptionBasicPtr>, _1, def->name));
                if (found!=state.lastInsertedClass->ownParameters.end())
                {
                    std::ostringstream os;
                    os<<"The parameter '"<<def->name<<"' is defined more than one time in class "<<state.lastInsertedClass->name;
                    throw ParseError("Duplicated parameter", os.str(), state.currentPath, 37);
                }
            }
            catch (const boost::property_tree::ptree_error&)
            {
                throw ParseError("Missing element", "Element 'name' is missing for a parameter in class '"+state.lastInsertedClass->name+"'.", state.currentPath, 19);
            }

            if (!ValidName(def->name))
            {
                throw ParseError("Invalid name", "Parameter name '"+def->name+"'' is invalid. Must start with an alphabetic char and then only contain alpha-numeric chars", state.currentPath, 20);
            }

            def->qualifiedName=state.lastInsertedClass->name+"."+def->name;

            //Check parameter type
            try
            {
                def->typeName=pt.get<std::string>(Elements::ParameterType::Name());
                SerializationUtils::Trim(def->typeName);
                if (!BasicTypeOperations::IsBasicTypeName(def->typeName, def->memberType))
                {
                    //not a basic type, we have to check later if its an enum or class type, for now we assume class
                    def->memberType=ObjectMemberType;
                    def->typeId=LlufId_Generate64(def->typeName.c_str());
                }
            }
            catch (const boost::property_tree::ptree_error&)
            {
                throw ParseError("Missing element", "Element 'type' is missing for parameter '"+def->name+"'' in class '"+state.lastInsertedClass->name+"'.", state.currentPath, 102);
            }

            //Check for duplicates later after baseClass is known too.
            state.lastInsertedClass->ownParameters.push_back(def);
            state.repository->InsertParameter(def);
        }
    };

    template<> struct ParseAlgorithm<Elements::Parametersummary>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.lastInsertedClass->ownParameters.back()->summary=pt.data();}
    };

    template<> struct ParseAlgorithm<Elements::ParameterArrayElements>
    {
        void operator()(boost::property_tree::ptree& /*pt*/, ParseState& state) const {state.lastInsertedClass->ownParameters.back()->isArray=true;}
    };

    template<> struct ParseAlgorithm<Elements::ParameterArray>
    {
        void operator()(boost::property_tree::ptree& /*pt*/, ParseState& state) const {state.lastInsertedClass->ownParameters.back()->isArray=true;}
    };

    template<> struct ParseAlgorithm<Elements::ParameterValue>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const
        {
            SerializationUtils::Trim(pt.data());

            ParameterDescriptionBasicPtr& def=state.lastInsertedClass->ownParameters.back();
            ValueDefinition val;
            try
            {
                if (def->memberType==ObjectMemberType)
                {
                    def->memberType=EnumerationMemberType; //Handled later, should be an enum otherwise <value>-element is not valid
                }

                if (def->memberType==EnumerationMemberType)
                {
                    val.stringVal=SerializationUtils::ExpandEnvironmentVariables(pt.data());
                    def->values.push_back(val);
                }
                else if (ParseValue(def->memberType, SerializationUtils::ExpandEnvironmentVariables(pt.data()), val))
                {
                    def->values.push_back(val);
                }
                else
                {
                    std::ostringstream os;
                    os<<"The value '"<<pt.data()<<"' doesn't match the type "<<def->typeName<<" for parameter "<<def->name<<" in class "<<state.lastInsertedClass->name;
                    throw ParseError("Invalid parameter value", os.str(), state.currentPath, 30);
                }
            }
            catch(const std::string& envVar)
            {
                std::ostringstream os;
                os<<"Failed to expand environment variable '"<<envVar<<"' in parameter "<<def->name<<" in class "<<state.lastInsertedClass->name;
                throw ParseError("Invalid parameter value", os.str(), state.currentPath, 142);
            }
        }
    };

    template<> struct ParseAlgorithm<Elements::BaseClass>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const
        {
            SerializationUtils::Trim(pt.data());
            state.lastInsertedClass->baseClass=pt.data();
        }
    };

    template<> struct ParseAlgorithm<Elements::ExceptionBase>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const
        {
            SerializationUtils::Trim(pt.data());
            state.lastInsertedException->baseClass=pt.data();
        }
    };

    template<> struct ParseAlgorithm<Elements::EnumerationValue>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const
        {
            SerializationUtils::Trim(pt.data());
            if (!ValidName(pt.data()))
            {
                throw ParseError("Invalid enumeration value", std::string("Enumeration value '")+pt.data()+" is invalid. Must start with an alphabetic char and then only contain alpha-numeric chars", state.currentPath, 66);
            }
            StringVector::const_iterator foundIt=std::find(state.lastInsertedEnum->enumerationValues.begin(), state.lastInsertedEnum->enumerationValues.end(), pt.data());
            if (foundIt!=state.lastInsertedEnum->enumerationValues.end())
            {
                throw ParseError("Duplicated enumeration value", "Enum value '"+pt.data()+"' exists more than one time in enum type "+state.lastInsertedEnum->name, state.currentPath, 16);
            }
            state.lastInsertedEnum->enumerationValues.push_back(pt.data());
        }
    };

    template<> struct ParseAlgorithm<Elements::Classsummary>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.lastInsertedClass->summary=pt.data();}
    };

    template<> struct ParseAlgorithm<Elements::Exceptionsummary>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.lastInsertedException->summary=pt.data();}
    };

    template<> struct ParseAlgorithm<Elements::Enumerationsummary>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.lastInsertedEnum->summary=pt.data();}
    };

    template<> struct ParseAlgorithm<Elements::Propertysummary>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.lastInsertedProperty->summary=pt.data();}
    };

    template<> struct ParseAlgorithm<Elements::PropertyMember>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const
        {
            MemberDescriptionBasicPtr def(new MemberDescriptionBasic);
            try
            {
                 def->name=pt.get<std::string>(Elements::PropertyMemberName::Name());
                 SerializationUtils::Trim(def->name);
            }
            catch (const boost::property_tree::ptree_error&)
            {
                throw ParseError("Missing element", "Element 'name' is missing for a member in property '"+state.lastInsertedProperty->name+"'.", state.currentPath, 67);
            }

            if (!ValidName(def->name))
            {
                std::ostringstream os;
                os<<"Property "<<state.lastInsertedProperty->name<<" contains a member with invalid name. Member name '"<<def->name+"' is invalid. Must start with an alphabetic char and then only contain alpha-numeric chars";
                throw ParseError("Invalid name", os.str(), state.currentPath, 68);
            }

            if (std::find_if(state.lastInsertedProperty->members.begin(), state.lastInsertedProperty->members.end(), boost::bind(NameComparerPtr<MemberDescriptionBasicPtr>, _1, def->name))!=
                state.lastInsertedProperty->members.end())
            {
                throw ParseError("Duplicated property member", def->name+" is defined more than one time in property "+state.lastInsertedProperty->name, state.currentPath, 69);
            }

            state.lastInsertedProperty->members.push_back(def);
        }
    };

    template<> struct ParseAlgorithm<Elements::PropertyMembersummary>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.lastInsertedProperty->members.back()->summary=pt.data();}
    };

    template<> struct ParseAlgorithm<Elements::PropertyMemberType>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const
        {
            SerializationUtils::Trim(pt.data());
            state.lastInsertedProperty->members.back()->typeName=pt.data();
            if (!BasicTypeOperations::IsBasicTypeName(pt.data(), state.lastInsertedProperty->members.back()->memberType))
            {
                //not a basic type, we have to check later if its an enum or class type, for now we assume class
                state.lastInsertedProperty->members.back()->memberType=ObjectMemberType;
            }
        }
    };

    template<> struct ParseAlgorithm<Elements::PropertyMemberisArray>
    {
        void operator()(boost::property_tree::ptree& /*pt*/, ParseState& state) const {state.lastInsertedProperty->members.back()->isArray=true;}
    };

    template<> struct ParseAlgorithm<Elements::Membersummary>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.lastInsertedClass->members.back()->summary=pt.data();}
    };

    template<> struct ParseAlgorithm<Elements::CreateRoutinesummary>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.lastInsertedClass->createRoutines.back()->summary=pt.data();}
    };

    template<> struct ParseAlgorithm<Elements::MaxLength>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const
        {
            try
            {
                SerializationUtils::Trim(pt.data());
                int size=boost::lexical_cast<int, std::string>(pt.data());
                if (size<=0)
                {
                    std::ostringstream ss;
                    ss<<"Max length must be greater than 0. The maxLength value specified for member '"<<state.lastInsertedClass->members.back()->name<<"' is "<<size;
                    throw ParseError("Invalid maxLength value", ss.str(), state.currentPath, 54);

                }
                state.lastInsertedClass->members.back()->maxLength=size;
            }
            catch (const boost::bad_lexical_cast&)
            {
                std::ostringstream ss;
                ss<<"The maxLength value specified in member '"<<state.lastInsertedClass->members.back()->name<<"' can't be converted to a number.";
                throw ParseError("Invalid maxLength value", ss.str(), state.currentPath, 55);
            }
        }
    };

    template<> struct ParseAlgorithm<Elements::ArraySize>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const
        {
            state.lastInsertedClass->members.back()->isArray=true;
            try
            {
                SerializationUtils::Trim(pt.data());
                int size=boost::lexical_cast<int, std::string>(pt.data());
                if (size<=0)
                {
                    std::ostringstream ss;
                    ss<<"Array size must be greater than 0. The arraySize value specified for member '"<<state.lastInsertedClass->members.back()->name<<"' is "<<size;
                    throw ParseError("Invalid arraySize value", ss.str(), state.currentPath, 64);

                }
                state.lastInsertedClass->members.back()->arraySize=size;
            }
            catch (const boost::bad_lexical_cast&)
            {
                std::ostringstream ss;
                ss<<"The arraySize value specified in member '"<<state.lastInsertedClass->members.back()->name<<"' can't be converted to a number.";
                throw ParseError("Invalid arraySize value", ss.str(), state.currentPath, 65);
            }
        }
    };

    //-----------------------------------------------
    // DOM file algorithms
    //-----------------------------------------------
    inline void InsertInlineParameter(ParseState& state, bool isArray)
    {
        const PropertyDescriptionBasic* pd=state.lastInsertedPropertyMapping->property;
        const MemberDescriptionBasic* propMem=pd->members[state.lastInsertedMemberMapping->propertyMemberIndex].get();

        ParameterDescriptionBasicPtr param(new ParameterDescriptionBasic);
        std::ostringstream paramName;
        //MyPropNamespace.MyProperty.PropMember@MyClassNamespace.MyClass#pm
        paramName<<pd->GetName()<<"."<<propMem->GetName()<<"@"<<state.lastInsertedPropertyMapping->class_->GetName();
        param->qualifiedName=paramName.str();
        param->name=param->qualifiedName;
        param->hidden=true;
        param->isArray=isArray;
        param->memberType=propMem->memberType;
        param->typeId=propMem->typeId;
        param->typeName=propMem->typeName;

        state.lastInsertedMemberMapping->paramRef=param.get();
        state.lastInsertedMemberMapping->paramIndex=0;
        state.notInsertedParameters.push_back(std::make_pair(state.lastInsertedPropertyMapping->class_, param));
    }

    template<> struct ParseAlgorithm<Elements::MapObject>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const
        {
            state.lastInsertedMemberMapping->kind=MappedToParameter;
            const PropertyDescriptionBasic* pd=state.lastInsertedPropertyMapping->property;
            const MemberDescriptionBasic* propMem=pd->members[state.lastInsertedMemberMapping->propertyMemberIndex].get();

            if (propMem->memberType!=ObjectMemberType)
            {
                std::ostringstream os;
                os<<"Can't map object to property member '"<<propMem->GetName()<<"' of type "<<propMem->typeName;
                throw ParseError("Type missmatch", os.str(), state.currentPath, 98);
            }

            ParameterDescriptionBasic* param=state.lastInsertedMemberMapping->paramRef;

            //Get the correct type name of the serialized object
            boost::optional<std::string> typeAttr=pt.get_optional<std::string>("<xmlattr>.type");
            std::string typeName;
            if (typeAttr)
            {
                SerializationUtils::Trim(*typeAttr);
                //if type has an explicit type-attribute, check type compliance
                typeName=*typeAttr;
                DotsC_TypeId tid=LlufId_Generate64(typeName.c_str());
                if (!BasicTypeOperations::IsOfType<TypeRepository>(state.repository.get(), ObjectMemberType, tid, ObjectMemberType, param->GetTypeId()))
                {
                    std::ostringstream os;
                    os<<"PropertyMapping with object of incorrect type. The object specified for propertyMember '"<<pd->GetName()<<"."<<propMem->GetName()<<"' in class '"<<state.lastInsertedPropertyMapping->class_->GetName()
                     <<"' is not compatible with the expected type "<<propMem->typeName;
                    throw ParseError("Type missmatch", os.str(), state.currentPath, 138);
                }
            }
            else
            {
                //type defaults to dou declaration
                typeName=param->typeName;
            }

            //do the serialization to the expected type
            ValueDefinition vd;
            vd.kind=ValueKind;
            try
            {
                XmlToBlobSerializer<TypeRepository> serializer(state.repository.get());
                serializer.SerializeObjectContent(typeName, vd.binaryVal, pt); //since pt does not include the root element we have to use method SerializeObjectContent
            }
            catch (const ParseError& err)
            {
                std::ostringstream os;
                os<<"Failed to deserialize object in propertyMapping. The object specified for propertyMember '"<<pd->GetName()<<"."<<propMem->GetName()<<"' in class '"<<state.lastInsertedPropertyMapping->class_->GetName()
                 <<"' cant be deserialized. "<<err.Description();
                throw ParseError("Invalid Object", os.str(), state.currentPath, err.ErrorId());
            }
            param->values.push_back(vd);
        }
    };



    template<> struct ParseAlgorithm<Elements::MapObjectDeprecated>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const
        {
            state.lastInsertedMemberMapping->kind=MappedToParameter;
            const PropertyDescriptionBasic* pd=state.lastInsertedPropertyMapping->property;
            const MemberDescriptionBasic* propMem=pd->members[state.lastInsertedMemberMapping->propertyMemberIndex].get();

            if (propMem->memberType!=ObjectMemberType)
            {
                std::ostringstream os;
                os<<"Can't map object to property member '"<<propMem->GetName()<<"' of type "<<propMem->typeName;
                throw ParseError("Type missmatch", os.str(), state.currentPath, 99);
            }

            ParameterDescriptionBasic* param=state.lastInsertedMemberMapping->paramRef;

            //do the serialization to the expected type
            ValueDefinition vd;
            vd.kind=ValueKind;
            DotsC_TypeId tid;
            try
            {
                UglyXmlToBlobSerializer<TypeRepository> serializer(state.repository.get());
                tid=serializer.SerializeObjectContent(vd.binaryVal, pt); //since pt does not include the root element we have to use method SerializeObjectContent
            }
            catch (const ParseError& err)
            {
                std::ostringstream os;
                os<<"Failed to deserialize object in propertyMapping. The object specified for propertyMember '"<<pd->GetName()<<"."<<propMem->GetName()<<"' in class '"<<state.lastInsertedPropertyMapping->class_->GetName()
                 <<"' cant be deserialized. "<<err.Description();
                throw ParseError("Invalid Object", os.str(), state.currentPath, err.ErrorId());
            }

            if (!BasicTypeOperations::IsOfType<TypeRepository>(state.repository.get(), ObjectMemberType, tid, ObjectMemberType, param->GetTypeId()))
            {
                std::ostringstream os;
                os<<"PropertyMapping with object of incorrect type. The object specified for propertyMember '"<<pd->GetName()<<"."<<propMem->GetName()<<"' in class '"<<state.lastInsertedPropertyMapping->class_->GetName()
                 <<"' is not compatible with the expected type "<<propMem->typeName;
                throw ParseError("Type missmatch", os.str(), state.currentPath, 113);
            }

            param->values.push_back(vd);
        }
    };

    template<> struct ParseAlgorithm<Elements::MapEntityId>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const
        {
            state.lastInsertedMemberMapping->kind=MappedToParameter;
            const PropertyDescriptionBasic* pd=state.lastInsertedPropertyMapping->property;
            const MemberDescriptionBasic* propMem=pd->members[state.lastInsertedMemberMapping->propertyMemberIndex].get();

            if (propMem->memberType!=EntityIdMemberType)
            {
                std::ostringstream os;
                os<<"Can't map entityId to property member '"<<propMem->GetName()<<"'' of type "<<propMem->typeName;
                throw ParseError("Type missmatch", os.str(), state.currentPath, 103);
            }

            ParameterDescriptionBasic* param=state.lastInsertedMemberMapping->paramRef;

            try
            {
                ValueDefinition vd;
                vd.kind=ValueKind;
                if (ParseValue(param->memberType, GetEntityIdParameterAsString(pt), vd))
                {
                    param->values.push_back(vd);
                }
                else
                {
                    std::ostringstream os;
                    os<<"The value '"<<vd.stringVal<<"' doesn't match the type "<<param->typeName<<" for property mapping of member "<<propMem->name;
                    throw ParseError("Invalid value", os.str(), state.currentPath, 97);
                }
            }
            catch(const boost::property_tree::ptree_error&)
            {
                throw ParseError("Incomplete EntityId XML", "Missing 'name' and/or 'instanceId' element in EntityId parameter", state.currentPath, 40);
            }
            catch(const std::string& envVar)
            {
                throw ParseError("Incomplete EntityId XML", "Failed to expand environment variable '"+envVar+"' in propertyMapping for member "+propMem->GetName(), state.currentPath, 141);
            }
        }
    };

    template<> struct ParseAlgorithm<Elements::MapValue>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const
        {
            SerializationUtils::Trim(pt.data());

            state.lastInsertedMemberMapping->kind=MappedToParameter;
            const PropertyDescriptionBasic* pd=state.lastInsertedPropertyMapping->property;
            const MemberDescriptionBasic* propMem=pd->members[state.lastInsertedMemberMapping->propertyMemberIndex].get();

            ParameterDescriptionBasic* param=state.lastInsertedMemberMapping->paramRef;

            ValueDefinition vd;
            vd.kind=ValueKind;
            if (param->memberType==EnumerationMemberType)
            {
                const EnumDescription* ed=state.repository->GetEnum(param->typeId);
                vd.int32Val=ed->GetIndexOfValue(pt.data());
                if (vd.int32Val<0)
                {
                    std::ostringstream os;
                    os<<"The value '"<<pt.data()<<"' doesn't match the type "<<param->typeName<<" for property mapping of member "<<propMem->name;
                    throw ParseError("Invalid value", os.str(), state.currentPath, 111);
                }
                vd.stringVal=pt.data();
                param->values.push_back(vd);
            }
            else
            {
                if (ParseValue(param->memberType, pt.data(), vd))
                {
                    param->values.push_back(vd);
                }
                else
                {
                    std::ostringstream os;
                    os<<"The value '"<<pt.data()<<"' doesn't match the type "<<param->typeName<<" for property mapping of member "<<propMem->name;
                    throw ParseError("Invalid value", os.str(), state.currentPath, 112);
                }
            }
        }
    };

    template<> struct ParseAlgorithm<Elements::MapValueRef>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const
        {
            state.lastInsertedMemberMapping->kind=MappedToParameter;
            const PropertyDescriptionBasic* pd=state.lastInsertedPropertyMapping->property;
            const MemberDescriptionBasic* propMem=pd->members[state.lastInsertedMemberMapping->propertyMemberIndex].get();
            std::string paramName;
            int paramIndex=-1;

            //Get parameterName
            try
            {
                paramName=pt.get<std::string>(Elements::ReferenceName::Name());
                SerializationUtils::Trim(paramName);
            }
            catch (const boost::property_tree::ptree_error&)
            {
                std::ostringstream os;
                os<<"Missing <name> element in parameter reference. In propertyMapping for member "<<state.lastInsertedPropertyMapping->property->name<<"."<<
                    propMem->GetName()<<" in class "<<state.lastInsertedPropertyMapping->class_->GetName();
                throw ParseError("Invalid parameter reference syntax", os.str(), state.currentPath, 89);
            }

            //Get Parameter Index
            boost::optional<boost::property_tree::ptree&> indexRef=pt.get_child_optional(Elements::IndexRef::Name());
            if (indexRef)
            {
                paramIndex=GetReferencedIndex(*indexRef, state);
            }
            else
            {
                paramIndex=pt.get(Elements::ReferenceIndex::Name(), -1);
            }

            ParameterDescriptionBasic* param=state.repository->GetParameterBasic(paramName);
            if (!param)
            {
                //parameter does not exist
                std::ostringstream os;
                os<<"Referenced parameter '"<<paramName<<"' does not exist. In propertyMapping for member "<<state.lastInsertedPropertyMapping->property->name<<"."<<propMem->GetName()<<".";
                throw ParseError("Invalid parameter reference", os.str(), state.currentPath, 90);
            }

            if (propMem->IsArray())
            {
                if (paramIndex>=0)
                {
                    //index not expected
                    std::ostringstream os;
                    os<<"Referenced parameter '"<<paramName<<"' is an array, index is not expected. In propertyMapping for member "<<state.lastInsertedPropertyMapping->property->name<<"."<<propMem->GetName()<<".";
                    throw ParseError("Index not expected", os.str(), state.currentPath, 91);
                }
                if (!param->IsArray())
                {
                    std::ostringstream os;
                    os<<"Referenced parameter '"<<paramName<<"' is not an array. Property member "<<state.lastInsertedPropertyMapping->property->name<<"."<<propMem->GetName()
                     <<" is an array and must be mapped to an array";
                    throw ParseError("Mapping array property to non-array parameter", os.str(), state.currentPath, 92);
                }
            }
            else //property member not array
            {
                if (param->IsArray())
                {
                    if (paramIndex<0)
                    {
                        //must specify index
                        std::ostringstream os;
                        os<<"Referenced parameter '"<<paramName<<"' is an array and an index must be specified when mapping property member "<<state.lastInsertedPropertyMapping->property->name<<"."<<propMem->GetName();
                        throw ParseError("Index expected", os.str(), state.currentPath, 93);
                    }
                    if (paramIndex>=param->GetArraySize())
                    {
                        //index out of range
                        std::ostringstream os;
                        os<<"Referenced parameter '"<<paramName<<"' has arraySize="<<param->GetArraySize()<<". Index out of range in mapping of property member "<<state.lastInsertedPropertyMapping->property->name<<"."<<propMem->GetName();
                        throw ParseError("Index out of range", os.str(), state.currentPath, 94);
                    }
                }
            }

            if (!BasicTypeOperations::IsOfType<TypeRepository>(state.repository.get(), param->GetMemberType(), param->GetTypeId(), propMem->GetMemberType(), propMem->GetTypeId()))
            {
                //Types does not match
                std::ostringstream os;
                os<<"PropertyMapping is mapping property member "<<propMem->GetName()<<" that has type "<<propMem->typeName
                    <<" to parameter '"<<param->GetName()<<"' that has type "<<param->typeName<<". The types are not compatible.";
                throw ParseError("Type missmatch", os.str(), state.currentPath, 95);
            }

            state.lastInsertedMemberMapping->paramRef=param;
            state.lastInsertedMemberMapping->paramIndex=paramIndex<0 ? 0 : paramIndex;
        }
    };

    template<> struct ParseAlgorithm<Elements::ClassMemberReference>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const
        {
            state.lastInsertedMemberMapping->kind=MappedToMember;
            const PropertyDescriptionBasic* pd=state.lastInsertedPropertyMapping->property;
            const MemberDescriptionBasic* propMem=pd->members[state.lastInsertedMemberMapping->propertyMemberIndex].get();
            const ClassDescriptionBasic* cd=state.lastInsertedPropertyMapping->class_;
            const MemberDescriptionBasic* classMem=NULL;
            int memberArrayIndex=-1;

            //Step into nested member refs if any
            for (std::vector< std::pair<DotsC_MemberIndex, DotsC_ArrayIndex> >::const_iterator it=state.lastInsertedMemberMapping->memberRef.begin();
                 it!=state.lastInsertedMemberMapping->memberRef.end(); ++it)
            {
                DotsC_TypeId tid=cd->GetMember(it->first)->GetTypeId();
                cd=state.repository->GetClassBasic(tid);
            }

            //Get class description
            try
            {
                std::string memberName=pt.get<std::string>(Elements::ClassMemberReferenceName::Name());
                SerializationUtils::Trim(memberName);

                //Get Parameter Index
                boost::optional<boost::property_tree::ptree&> indexRef=pt.get_child_optional(Elements::IndexRef::Name());
                if (indexRef)
                {
                    memberArrayIndex=GetReferencedIndex(*indexRef, state);
                }
                else
                {
                    memberArrayIndex=pt.get(Elements::ClassMemberReferenceIndex::Name(), -1); //default index=-1 if not present
                }

                int classMemberIx=cd->GetMemberIndex(memberName);
                if (classMemberIx<0)
                {
                    std::ostringstream os;
                    os<<"PropertyMapping is mapping the property member '"<<propMem->GetName()<<"' to class member '"<<memberName<<"'. But the class member does not exist in class "<<cd->GetName();
                    throw ParseError("Invalid class member", os.str(), state.currentPath, 77);
                }
                classMem=static_cast<const MemberDescriptionBasic*>(cd->GetMember(classMemberIx));
                int arrIx=memberArrayIndex<0 ? 0 : memberArrayIndex;
                state.lastInsertedMemberMapping->memberRef.push_back(std::make_pair(classMemberIx, arrIx));
            }
            catch (const boost::property_tree::ptree_error&)
            {
                std::ostringstream os;
                os<<"PropertyMapping is missing the classMember-element in classMemberReference for property member '"<<propMem->GetName()<<"'.";
                throw ParseError("Missing element", os.str(), state.currentPath, 78);
            }

            //Handle nested class memeber references by recursion.
            boost::optional<boost::property_tree::ptree&> nestedClassMemberRef=pt.get_child_optional("classMemberReference");
            if (nestedClassMemberRef)
            {
                //since this is not the leaf, we just care that a valid member is pointed out. And that it is an object so it's possible to step into
                //Types does not have to be compatible.
                if (classMem->IsArray())
                {
                    if (memberArrayIndex<0)
                    {
                        //index missing
                        std::ostringstream os;
                        os<<"PropertyMapping is mapping property member '"<<propMem->GetName()<<"' to referencing class member '"<<classMem->GetName()<<"' ";
                        os<<" but <index>-element is missing. Index is mandatory since the class member is an array.";
                        throw ParseError("Index element missing", os.str(), state.currentPath, 79);
                    }
                    else if (memberArrayIndex>=classMem->GetArraySize())
                    {
                        //index out of range
                        std::ostringstream os;
                        os<<"PropertyMapping is mapping property member '"<<propMem->GetName()<<"' to class member '"<<classMem->GetName()<<"' ";
                        os<<" with index="<<memberArrayIndex<<". Index is out of range, the arraySize="<<classMem->GetArraySize();
                        throw ParseError("Index out of range", os.str(), state.currentPath, 80);
                    }
                }
                else if (memberArrayIndex!=-1)
                {
                    //Index specified on non-array member
                    std::ostringstream os;
                    os<<"PropertyMapping is mapping property member '"<<propMem->GetName()<<"' to class member '"<<classMem->GetName()<<"' ";
                    os<<" with index="<<memberArrayIndex<<". Array index is not allowed since the member is not an array.";
                    throw ParseError("Index specified for non-array member", os.str(), state.currentPath, 81);
                }

                if (classMem->GetMemberType()!=ObjectMemberType)
                {
                    std::ostringstream os;
                    os<<"PropertyMapping can only use nested references into object members. PropertyMember "<<propMem->GetName()<<
                        "' is using nested references into class member '"<<classMem->GetName()<<"' of type "<<BasicTypeOperations::MemberTypeToString(classMem->GetMemberType());
                    throw ParseError("Nested propertyMapping into non-object member", os.str(), state.currentPath, 82);
                }

                //All seems to be ok so far, call recursivly to resolve next memberRef
                this->operator()(nestedClassMemberRef.get(), state);
            }
            else //This is the leaf, then the types must be compliant
            {
                if (propMem->IsArray())
                {
                    if (!classMem->IsArray())
                    {
                        //propery is array but not classMember
                        std::ostringstream os;
                        os<<"PropertyMapping is mapping array property member "<<propMem->GetName()<<
                            "' to non-array class member '"<<classMem->GetName()<<"'. ";
                        throw ParseError("Invalid classMemberReference", os.str(), state.currentPath, 83);
                    }
                    else if (memberArrayIndex!=-1)
                    {
                        //property is array but is mapped to a singel member array index
                        std::ostringstream os;
                        os<<"PropertyMapping is mapping array property member "<<propMem->GetName()<<
                            "' to a single array index in class member '"<<classMem->GetName()<<"'. Index-element is not allowed when mapping whole array.";
                        throw ParseError("Invalid classMemberReference", os.str(), state.currentPath, 84);
                    }
                    else
                    {
                        //this is the leaf memberRef, and the propertyMember is an array. Then there is an agreement that arrayIndex shall be -1
                        state.lastInsertedMemberMapping->memberRef.back().second=-1;
                    }
                }
                else //propertyMember not array
                {
                    if (classMem->IsArray())
                    {
                        if (memberArrayIndex<0)
                        {
                            //propMem not array but has been mapped to a whole array classMember
                            std::ostringstream os;
                            os<<"PropertyMapping is mapping non-array property member "<<propMem->GetName()<<
                                "' a whole array class member '"<<classMem->GetName()<<"'. Use Index-element to map to a single array index.";
                            throw ParseError("Invalid classMemberReference", os.str(), state.currentPath, 85);
                        }
                        else if (memberArrayIndex>=classMem->GetArraySize())
                        {
                            //index out of range
                            std::ostringstream os;
                            os<<"PropertyMapping is mapping property member '"<<propMem->GetName()<<"' to class member '"<<classMem->GetName()<<"' ";
                            os<<" with index="<<memberArrayIndex<<". Index is out of range, the arraySize="<<classMem->GetArraySize();
                            throw ParseError("Index out of range", os.str(), state.currentPath, 86);
                        }
                    }
                    else //propMember not array and classMember not array
                    {
                        if (memberArrayIndex>=0)
                        {
                            //Index specified on non-array member
                            std::ostringstream os;
                            os<<"PropertyMapping is mapping property member '"<<propMem->GetName()<<"' to class member '"<<classMem->GetName()<<"' ";
                            os<<" with index="<<memberArrayIndex<<". Array index is not allowed since the member is not an array.";
                            throw ParseError("Index specified for non-array member", os.str(), state.currentPath, 87);
                        }
                    }
                }

                //When we get here array/non-array issues are handled, now compare types
                if (!BasicTypeOperations::IsOfType<TypeRepository>(state.repository.get(), classMem->GetMemberType(), classMem->GetTypeId(), propMem->GetMemberType(), propMem->GetTypeId()))
                {
                    //Types does not match
                    std::ostringstream os;
                    os<<"PropertyMapping is mapping property member "<<propMem->GetName()<<" that has type "<<propMem->typeName
                        <<" to class member '"<<classMem->GetName()<<"' that has type "<<classMem->typeName<<". The types are not compatible.";
                    throw ParseError("Type missmatch", os.str(), state.currentPath, 88);
                }
            }
        }
    };

    template<> struct ParseAlgorithm<Elements::PropertyMappingsummary>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const {state.lastInsertedPropertyMapping->summary=pt.data();}
    };

    template<> struct ParseAlgorithm<Elements::MemberMapping>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const
        {
            const PropertyDescriptionBasic* pd=state.lastInsertedPropertyMapping->property;
            MemberMappingBasicPtr md(new MemberMappingBasic);
            md->kind=MappedToNull;
            bool inlineParam=false;
            bool isArray=false;

            //get property member
            try
            {
                std::string propMemberName=pt.get<std::string>(Elements::MapPropertyMember::Name());
                SerializationUtils::Trim(propMemberName);

                md->propertyMemberIndex=pd->GetMemberIndex(propMemberName);
                if (md->propertyMemberIndex<0)
                {
                    std::ostringstream os;
                    os<<"The property member '"<<propMemberName<<"' has been mapped in a propertyMapping, but the member does not exist in property "<<pd->GetName();
                    throw ParseError("Invalid propertyMapping", os.str(), state.currentPath, 74);
                }

                //check for duplicates
                if (state.lastInsertedPropertyMapping->memberMappings[md->propertyMemberIndex]!=NULL)
                {
                    //Already been mapped, i.e duplicated memberMapping
                    std::ostringstream ss;
                    ss<<"The property member '"<<propMemberName<<"' in property '"<<pd->GetName()<<"' is defined more than one time in property mapping.";
                    throw ParseError("Duplicated property member mapping", ss.str(), state.currentPath, 75);
                }
            }
            catch (const boost::property_tree::ptree_error&)
            {
                throw ParseError("Missing element", "PropertyMapping is missing the propertyMember-element in a memberMapping'", state.currentPath, 76);
            }

            for (boost::property_tree::ptree::iterator it=pt.begin(); it!=pt.end(); ++it)
            {
                if (it->first==Elements::MapPropertyMember::Name())
                {
                    continue;
                }
                else if (it->first==Elements::ClassMemberReference::Name())
                {
                    //mapped to a class member
                    md->kind=MappedToMember;
                    break;
                }
                else if (it->first==Elements::MapValueRef::Name())
                {
                    //mapped to real parameter (not a hidden inline param)
                    md->kind=MappedToParameter;
                    break;
                }
                else if (it->first==Elements::MapArray::Name())
                {
                    //inline array value, new format
                    md->kind=MappedToParameter;
                    inlineParam=true;
                    isArray=true;
                    break;

                }
                else if (it->first==Elements::MapArrayElements::Name())
                {
                    //inline array value, old format
                    md->kind=MappedToParameter;
                    inlineParam=true;
                    isArray=true;
                    break;
                }
                else
                {
                    //value, entityId, object, i.e inline non-array value
                    md->kind=MappedToParameter;
                    inlineParam=true;
                    break;
                }
            }

            state.lastInsertedPropertyMapping->memberMappings[md->propertyMemberIndex]=md;
            state.lastInsertedMemberMapping=md;

            if (inlineParam)
            {
                const MemberDescriptionBasic* propMem=pd->members[md->propertyMemberIndex].get();
                if (isArray && !propMem->IsArray())
                {
                    std::ostringstream os;
                    os<<"PropertyMember '"<<pd->GetName()<<"."<<propMem->GetName()<<"' is not an array. Element <arrayElements> is not allowed.";
                    throw ParseError("Property member not array", os.str(), state.currentPath, 96);
                }
                else if (!isArray && propMem->IsArray())
                {
                    std::ostringstream os;
                    os<<"PropertyMember '"<<pd->GetName()<<"."<<propMem->GetName()<<"' is an array but the specified value is not an array.";
                    throw ParseError("Property member is array", os.str(), state.currentPath, 108);
                }

                InsertInlineParameter(state, isArray);
            }
        }
    };

    template<> struct ParseAlgorithm<Elements::PropertyMapping>
    {
        void operator()(boost::property_tree::ptree& pt, ParseState& state) const
        {
            PropertyMappingDescriptionBasicPtr def(new PropertyMappingDescriptionBasic);
            state.notInsertedPropertyMappings.push_back(def);
            state.lastInsertedPropertyMapping=def;
            def->fileName=state.currentPath;

            //Get class
            try
            {
                std::string className=pt.get<std::string>(Elements::MappedClass::Name());
                SerializationUtils::Trim(className);
                DotsC_TypeId classTypeId=LlufId_Generate64(className.c_str());
                def->class_=state.repository->GetClassBasic(classTypeId);
                if (!def->class_)
                {
                    throw ParseError("Class does not exist", "PropertyMapping is specifying a class that does not exist '"+className+"'.", state.currentPath, 70);
                }
            }
            catch (const boost::property_tree::ptree_error&)
            {
                throw ParseError("Missing element", "PropertyMapping is missing the 'class' element.", state.currentPath, 71);
            }

            //Get property
            try
            {
                std::string propName=pt.get<std::string>(Elements::MappedProperty::Name());
                SerializationUtils::Trim(propName);
                DotsC_TypeId propTypeId=LlufId_Generate64(propName.c_str());
                def->property=state.repository->GetPropertyBasic(propTypeId);
                if (!def->property)
                {
                    throw ParseError("Property does not exist", "PropertyMapping is specifying a property that does not exist '"+propName+"'.", state.currentPath, 72);
                }
            }
            catch (const boost::property_tree::ptree_error&)
            {
                throw ParseError("Missing element", "PropertyMapping is missing the 'property' element.", state.currentPath, 73);
            }

            //When finished there should one mapping for each property member, set correct size here.
            def->memberMappings.resize(def->property->members.size()); //will default each index to null pointer

            //Verify file name
            CheckNameAndFilenameConsistency(def->FileName(), def->class_->name+"-"+def->property->name);
        }
    };

    //-----------------------------------------------
    // Post parsing algorithms
    //-----------------------------------------------
    class DouCompletionAlgorithm
    {
    public:
        void operator()(const ParseState& state);

    private:
        void DeserializeObjects(const ParseState& state);
        void ResolveReferences(const ParseState& state);
        void ResolveParamToParamRefs(const ParseState& state);
        bool ResolveParamToParamRef(const ParseState& state, const ParseState::ParameterReference<ParameterDescriptionBasic>& ref);
        void ResolveArraySizeRef(const ParseState& state, const ParseState::ParameterReference<MemberDescriptionBasic>& ref);
        void ResolveMaxLengthRef(const ParseState& state, const ParseState::ParameterReference<MemberDescriptionBasic>& ref);
        void ResolveHiddenCreateRoutineParams(const ParseState& state, const ParseState::ParameterReference<CreateRoutineDescriptionBasic>& ref);
        void HandleCreateRoutines(const ParseState& state, ClassDescriptionBasic* cd);
        void CalculateEnumChecksums(const ParseState& state);
        void VerifyParameterTypes(const ParseState& state);
        void CalculateClassSize(const ParseState& state, ClassDescriptionBasic* cd);
    };

    class DomCompletionAlgorithm
    {
    public:
        void operator()(const ParseState& state);

    private:
        void InsertPropertyMapping(const PropertyMappingDescriptionBasicPtr& pm);
    };
}
}
}
}

#endif
