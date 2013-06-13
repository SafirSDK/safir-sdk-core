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
#include <cctype>
#include <map>
#include <boost/timer.hpp>
#include "ParseAlgorithms.h"

#ifndef PROFILE_ENABLED
//--- Uncomment next line to enable Profiling to std:out ---
//#define PROFILE_ENABLED
#endif
#ifdef PROFILE_ENABLED
std::map<std::string, double> g_time;
struct prof
{ 
    boost::timer m_timer;
    const char* m_fun;
    prof(const char* fun) : m_fun(fun) {}
    ~prof(){g_time[m_fun]+=m_timer.elapsed();}
};
struct timeCmp{ bool operator()(const std::pair<std::string, double>& l, const std::pair<std::string, double>& r){return l.second>r.second;}};
#define PROFILE prof _pr(__FUNCTION__);
#else
#define PROFILE
#endif

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    template <class T>
    void CheckbaseClass(std::vector<T>& vec, T& val, const std::string& rootbaseClass, const std::string& secondRootbaseClass)
    {
        PROFILE
        static const int MaxInheritanceLevel = 100; //Here we introduce a max inheritance level. Should be enough.
        
        if (val.name==rootbaseClass || (val.name==secondRootbaseClass && !secondRootbaseClass.empty()))
        {
            //This is the root base class.
            return;
        }

        T* current = &val;

        for (int inheritanceLevel=0; inheritanceLevel<MaxInheritanceLevel; ++inheritanceLevel)
        {
            if ((current->baseClass==rootbaseClass) || (current->baseClass==secondRootbaseClass && !secondRootbaseClass.empty()))
            {
                //We reached root.
                return;
            }

            T* baseClass=GetByFieldValue<T, std::string>(vec, current->baseClass, boost::bind(&T::name,_1));
            if (!baseClass)
            {
                std::ostringstream ss;
                ss<<"The type '"<<current->name<<"' specifies an invalid or non-existing base class. Base class: '"<<current->baseClass<<"'.";
                throw ParseError("Invalid base class", ss.str(), current->fileName);
            }
            current=baseClass;
        }

        throw ParseError("Circular inheritance", std::string("The inheritance tree starting from '")+val.name+std::string("' will never end."), val.fileName);
    }

    template <class T>
    bool NameComparer(const T& obj, const std::string& name){ return FieldCmp<T, std::string>(obj,name, boost::bind(&T::name, _1)); }

    void SetarraySize(MemberDefinition& m, int val){m.arraySize=val;}
    void SetmaxLength(MemberDefinition& m, int val){m.maxLength=val;}
    
    void ParseResultFinalizer::ProcessDouResults()
    {
        PROFILE

        ResolveParameterToParameterRefs();
        ResolveReferences(m_state.arraySizeReferences, "arraySizeRef", SetarraySize);
        ResolveReferences(m_state.maxLengthReferences, "maxLengthRef", SetmaxLength);

        std::for_each(m_state.result->enumerations.begin(), m_state.result->enumerations.end(), boost::bind(&ParseResultFinalizer::ProcessEnum, this, _1)); //ProcessEnums
        std::for_each(m_state.result->exceptions.begin(), m_state.result->exceptions.end(), boost::bind(&ParseResultFinalizer::ProcessException, this, _1)); //Processexceptions
        std::for_each(m_state.result->properties.begin(), m_state.result->properties.end(), boost::bind(&ParseResultFinalizer::ProcessProperty, this, _1)); //Processproperties
        std::for_each(m_state.result->classes.begin(), m_state.result->classes.end(), boost::bind(&ParseResultFinalizer::ProcessClass, this, _1)); //Processclasses

        //Finally handle all parameters. We do this part last so we are sure all type definitions are in place.
        for (ClassDefinitions::iterator it=m_state.result->classes.begin(); it!=m_state.result->classes.end(); ++it)
        {
            std::for_each(it->parameters.begin(), it->parameters.end(), boost::bind(&ParseResultFinalizer::ProcessParameter, this, *it, _1));
        }
    }

    void ParseResultFinalizer::ProcessDomResults()
    {
        PROFILE

        std::for_each(m_state.result->propertyMappings.begin(), m_state.result->propertyMappings.end(), boost::bind(&ParseResultFinalizer::ProcessPropertyMapping, this, _1)); //ProcessPropertyMapping

#ifdef PROFILE_ENABLED
        g_time[_pr.m_fun]+=_pr.m_timer.elapsed();

        std::vector<std::pair<std::string, double>> myvec(g_time.begin(), g_time.end());
        std::sort(myvec.begin(), myvec.end(), timeCmp());

        std::cout<<std::endl<<"------ Profiling ----------"<<std::endl;
        for (std::vector<std::pair<std::string, double>>::const_iterator it=myvec.begin(); it!=myvec.end(); ++it)
        {
            std::cout<<it->first<<": "<<it->second<<std::endl;
        }
        std::cout<<std::endl<<"----------------------------"<<std::endl;
#endif

    }

    void ParseResultFinalizer::ProcessEnum(EnumerationDefinition& e)
    {
        PROFILE

        if (!ValidName(e.name, true))
        {
            throw ParseError("Invalid name", std::string("Enumeration name '")+e.name+std::string(" is invalid. Must start with an alphabetic char and then only contain alpha-numeric chars"), e.fileName);
        }

        CheckNameAndFilenameConsistency(e.fileName, e.name);

        for (StringVector::const_iterator it=e.enumerationValues.begin(); it!=e.enumerationValues.end(); ++it)
        {
            if (!ValidName(*it, false))
            {
                throw ParseError("Invalid enumeration value", 
                std::string("Enumeration value '")+(*it)+std::string(" is invalid. Must start with an alphabetic char and then only contain alpha-numeric chars"), e.fileName);
            }

            if (std::count(e.enumerationValues.begin(), e.enumerationValues.end(), *it)>1)
            {
                throw ParseError("Duplicated enumeration value", (*it)+std::string(" exists more than one time in enum type ")+e.name, e.fileName);
            }
        }
    }

    void ParseResultFinalizer::ProcessException(ExceptionDefinition& e)
    {
        PROFILE

        if (!ValidName(e.name, true))
        {
            throw ParseError("Invalid name", std::string("Exception name '")+e.name+std::string(" is invalid. Must start with an alphabetic char and then only contain alpha-numeric chars"), e.fileName);
        }

        CheckNameAndFilenameConsistency(e.fileName, e.name);

        CheckbaseClass<ExceptionDefinition>(m_state.result->exceptions, e, BasicTypes::ExceptionName, BasicTypes::FundamentalExceptionName);
    }

    void ParseResultFinalizer::ProcessProperty(PropertyDefinition& p)
    {
        PROFILE

        if (!ValidName(p.name, true))
        {
            throw ParseError("Invalid name", std::string("Property name '")+p.name+std::string(" is invalid. Must start with an alphabetic char and then only contain alpha-numeric chars"), p.fileName);
        }

        CheckNameAndFilenameConsistency(p.fileName, p.name);

        //Handle property members
        for (MemberDefinitions::iterator it=p.members.begin(); it!=p.members.end(); ++it)
        {
            if (std::count_if(p.members.begin(), p.members.end(), boost::bind(NameComparer<MemberDefinition>, _1, it->name))>1)
            {
                throw ParseError("Duplicated property member", it->name+std::string(" is defined more than one time int property '")+p.name, p.fileName);
            }
            
            if (!BasicTypes::Instance().MemberTypeOf(it->typeName, m_state.result, it->memberType))
            {
                std::ostringstream ss;
                ss<<"The member '"<<it->name<<"' in property '"<<p.name<<"' has an invalid type specified. Type: "<<it->typeName;
                throw ParseError("Invalid type", ss.str(), p.fileName);
            }
        }
    }

    void ParseResultFinalizer::ProcessClass(ClassDefinition& c)
    {
        PROFILE

        if (!ValidName(c.name, true))
        {
            throw ParseError("Invalid name", std::string("Class name '")+c.name+std::string(" is invalid. Must start with an alphabetic char and then only contain alpha-numeric chars"), c.fileName);
        }

        CheckNameAndFilenameConsistency(c.fileName, c.name);


        CheckbaseClass<ClassDefinition>(m_state.result->classes, c, BasicTypes::ObjectName, "");

        //Handle createRoutines
        std::for_each(c.createRoutines.begin(), c.createRoutines.end(), boost::bind(&ParseResultFinalizer::ProcessCreateRoutine, this, c, _1));

        //Handle Members
        std::for_each(c.members.begin(), c.members.end(), boost::bind(&ParseResultFinalizer::ProcessClassMember, this, c, _1));

        //Handle Parameters
        //std::for_each(c.parameters.begin(), c.parameters.end(), boost::bind(&ParseResultFinalizer::ProcessParameter, this, c, _1));
    }

    void ParseResultFinalizer::ProcessCreateRoutine(ClassDefinition& host, CreateRoutineDefinition& c)
    {
        PROFILE

        if (!ValidName(c.name, false))
        {
            std::ostringstream ss;
            ss<<"The create routine name '"<<c.name<<"' in class '"<<host.name<<"' is illegal.";
            throw ParseError("Illegal name", ss.str(), host.fileName);
        }

        //Check that all create routine parameters are existing members in the class
        for (StringVector::const_iterator member=c.parameters.begin(); member!=c.parameters.end(); ++member)
        {
            const MemberDefinition* found = m_state.GetMember(&host, *member);
            if (found==NULL)
            {
                //the create routine parameter was not found in class members
                std::ostringstream ss;
                ss<<"The create routine '"<<c.name<<"' in class '"<<host.name<<"' has specified an invalid member: "<<(*member);
                throw ParseError("Illegal create routine member", ss.str(), host.fileName);
            }

            //Check for create routine parameter duplicates
            if (std::count(c.parameters.begin(), c.parameters.end(), *member)>1)
            {                
                std::ostringstream ss;
                ss<<"The create routine '"<<c.name<<"' in class '"<<host.name<<"' has specified member '"<<(*member)<<"' more than one time.";
                throw ParseError("Duplicated create routine member", ss.str(), host.fileName);
            }
        }

        //Check that all member values are ok
        for (MemberValueVector::const_iterator memberValue=c.memberValues.begin(); memberValue!=c.memberValues.end(); ++memberValue)
        {
            const MemberDefinition* classMember = m_state.GetMember(&host, memberValue->first);
            if (classMember==NULL)
            {
                //the create routine parameter was not found in class members
                std::ostringstream ss;
                ss<<"The create routine '"<<c.name<<"' in class '"<<host.name<<"' has specified default value for an invalid member: "<<memberValue->first;
                throw ParseError("Illegal create routine member", ss.str(), host.fileName);
            }

            const std::string& paramName = memberValue->second.first;
            int paramIndex = memberValue->second.second;
            ParameterDefinition* param = m_state.GetParameter(paramName);

            //Check that parameter exists
            if (!param)
            {
                std::ostringstream ss;
                ss<<"Could not resolve CreateRoutine value. Referenced parameter "<<paramName<<" not found. Referenced from createRoutine  "<<c.name<<" in class "<<host.name;
                throw ParseError("Parameter reference error", ss.str(), host.fileName);
            }

            if (paramIndex>=static_cast<int>(param->values.size()))
            {
                std::ostringstream ss;
                ss<<"CreateRoutine value is referencing out of bounds. Referenced parameter "<<paramName<<" index="<<paramIndex<<". Referenced from createRoutine  "<<c.name<<" in class "<<host.name;
                throw ParseError("Array index out of bounds", ss.str(), host.fileName);
            }

            if (!IsOfType(param->typeName, classMember->typeName))
            {
                std::ostringstream ss;
                ss<<"Referenced parameter '"<<paramName<<"' is not of the expected type. Type is '"<<param->typeName<<"' and expected type is '"<<classMember->typeName<<"'. Referenced from createRoutine  "<<c.name<<" in class "<<host.name;
                throw ParseError("Type missmatch in createRoutine value", ss.str(), host.fileName);
            }
            
           //TODO: check for duplicated membervalues. Check for membervalues that already are present as parameters. Add indexer to init array values.
        }
    }

    void ParseResultFinalizer::ProcessClassMember(ClassDefinition& host, MemberDefinition& m)
    {
        PROFILE

        if (!ValidName(m.name, false))
        {
            std::ostringstream ss;
            ss<<"The member name '"<<m.name<<"' in class '"<<host.name<<"' is illegal.";
            throw ParseError("Illegal name", ss.str(), host.fileName);
        }

        if (!BasicTypes::Instance().MemberTypeOf(m.typeName, m_state.result, m.memberType))
        {
            std::ostringstream ss;
            ss<<"The member '"<<m.name<<"' in class '"<<host.name<<"' has an invalid type specified. Type: "<<m.typeName;
            throw ParseError("Invalid type", ss.str(), host.fileName);
        }

        //Check for member duplicates
        if (std::count_if(host.members.begin(), host.members.end(), boost::bind(NameComparer<MemberDefinition>, _1, m.name))>1)
        {
            std::ostringstream ss;
            ss<<"The member '"<<m.name<<"' in class '"<<host.name<<"' is defined more than one time.";
            throw ParseError("Duplicated class member", ss.str(), host.fileName);
        }
    }

    void ParseResultFinalizer::ProcessParameter(ClassDefinition& host, ParameterDefinition& p)
    {
        PROFILE

        if (!p.hidden)
        {
            if (!ValidName(p.name, false))
            {
                std::ostringstream ss;
                ss<<"The parameter name '"<<p.name<<"' in class '"<<host.name<<"' is illegal.";
                throw ParseError("Illegal name", ss.str(), host.fileName);
            }
        }

        if (!BasicTypes::Instance().MemberTypeOf(p.typeName, m_state.result, p.memberType))
        {
            std::ostringstream ss;
            ss<<"The parameter '"<<p.name<<"' in class '"<<host.name<<"' has an invalid type specified. Type: "<<p.typeName;
            throw ParseError("Invalid type", ss.str(), host.fileName);
        }

        for (ParameterValues::iterator valIt=p.values.begin(); valIt!=p.values.end(); ++valIt)
        {
            try
            {
                valIt->stringVal = ExpandEnvironmentVariables(valIt->stringVal);
            }
            catch (const ParseError& err)
            {
                std::ostringstream ss;
                ss<<"Failed to expand the environment variable '"<<err.Label()<<"'.";
                throw ParseError("Environment variable expansion error", ss.str(), host.fileName);
            }

            if (!BasicTypes::Instance().CanParseValue(p.typeName, valIt->stringVal, m_state.result))
            {
                std::ostringstream ss;
                ss<<"The parameter '"<<p.name<<"' with value '"<<valIt->stringVal<<"' cannot be converted to the expected type "<<p.typeName;
                throw ParseError("Failed to interpret parameter value", ss.str(), host.fileName);
            }
        }
    }

    void ParseResultFinalizer::ProcessPropertyMapping(PropertyMappingDefinition& p)
    {
        PROFILE

        CheckNameAndFilenameConsistency(p.fileName, p.className+std::string("-")+p.propertyName);

        //Check for duplicated property mappings
        int count=0;
        for (PropertyMappingDefinitions::const_iterator it=m_state.result->propertyMappings.begin(); it!=m_state.result->propertyMappings.end(); ++it)
        {
            if (it->className==p.className && it->propertyName==p.propertyName)
                ++count;
        }
        if (count>1)
        {
            throw ParseError("Duplicated property mapping definition", "The PropertyMapping is defined more than one time.", p.fileName);
        }
        
        //Verify that property exists
        PropertyDefinition* prop = m_state.GetProperty(p.propertyName);
        if (!prop)
        {
            std::ostringstream ss;
            ss<<"The property '"<<p.propertyName<<"' does not exist. Referenced from property mapping dom-file.";
            throw ParseError("Property does not exist", ss.str(), p.fileName);
        }

        //Verify that class exists
        ClassDefinition* cls = m_state.GetClass(p.className);
        if (!cls)
        {
            std::ostringstream ss;
            ss<<"The class '"<<p.className<<"' does not exist. Referenced from property mapping dom-file.";
            throw ParseError("Class does not exist", ss.str(), p.fileName);
        }

        //Check that all propertyMembers have been mapped exactly one time
        for (MemberDefinitions::const_iterator it=prop->members.begin(); it!=prop->members.end(); ++it)
        {
            size_t count = std::count_if(p.mappedMembers.begin(), p.mappedMembers.end(), boost::bind(NameComparer<MappedMemberDefinition>, _1, it->name));
            if (count>1)
            {
                std::ostringstream ss;
                ss<<"The property member '"<<it->name<<"' in property '"<<prop->name<<"' is defined more than one time in property mapping.";
                throw ParseError("Duplicated property member mapping", ss.str(), p.fileName);
            }
            else if (count==0)
            {
                std::ostringstream ss;
                ss<<"The property member '"<<it->name<<"' in property '"<<prop->name<<"' is not mapped in property mapping.";
                throw ParseError("Missing property member in property mapping", ss.str(), p.fileName);
            }

        }
        
        //Handle all mapped members
        for (MappedMemberDefinitions::iterator mappedMemberIt=p.mappedMembers.begin(); mappedMemberIt!=p.mappedMembers.end(); ++mappedMemberIt)
        {
            if (mappedMemberIt->kind==MappedToParameter) //If the propertyMember is mapped to a value, check that the value can be parsed as the expected type.
            {
                ProcessPropertyMappedToParameter(p, *prop, *cls, *mappedMemberIt);
            }
            else if (mappedMemberIt->kind==MappedToMember) //Mapped to a class member. Verify that member exists, arrayIndices not out of bounds, type compliance.
            {
                ProcessPropertyMappedToClassMember(p, *prop, *cls, *mappedMemberIt);
            }
        }
    }

    void ParseResultFinalizer::ProcessPropertyMappedToParameter(PropertyMappingDefinition& mapping,
                                                                PropertyDefinition& property,
                                                                ClassDefinition& cls,
                                                                MappedMemberDefinition& member)
    {
        MemberDefinitions::const_iterator propMemberIt = std::find_if(property.members.begin(), property.members.end(), boost::bind(NameComparer<MemberDefinition>, _1, member.name));
        if (propMemberIt==property.members.end())
        {
            std::ostringstream ss;
            ss<<"The property '"<<property.name<<"' does not define the member '"<<member.name<<"' that has been mapped in property mapping.";
            throw ParseError("Property member does not exist", ss.str(), mapping.fileName);
        }

        const std::string& paramName = member.memberReferences[0].first;
        int paramIndex = member.memberReferences[0].second;
        ParameterDefinition* param = m_state.GetParameter(paramName);

        //Check that parameter exists
        if (!param)
        {
            std::ostringstream ss;
            ss<<"Could not resolve PropertyMapping valueRef "<<paramName<<". Referenced from property mapping member:  "<<member.name;
            throw ParseError("Parameter reference error", ss.str(), mapping.fileName);
        }

        //If this is a hidden parameter, then the type information is still missing
        if (param->hidden)
        {
            param->memberType=propMemberIt->memberType;
            param->typeName=propMemberIt->typeName;
            try
            {
                ProcessParameter(cls, *param);
            }
            catch(const ParseError& err)
            {
                std::ostringstream ss;
                ss<<"Error occured while parsing PropertyMapping value for member"<<member.name<<". Accitional info: "<<err.Description();
                throw ParseError("PropertyMapping value error", ss.str(), mapping.fileName);
            }
        }

        if (propMemberIt->isArray!=param->isArray)
        {
            std::ostringstream ss;
            ss<<"The member '"<<member.name<<"' has been mapped to a value that does not match the corresponding property member in terms of isArray. A single value must be mapped to a single value, and an array to an array.";
            throw ParseError("PropertyMapping array missmatch", ss.str(), mapping.fileName);
        }

        if (!propMemberIt->isArray && paramIndex>=0)
        {
            std::ostringstream ss;
            ss<<"The property member '"<<member.name<<"' is not an array but an array index is specified";
            throw ParseError("Illegal index element", ss.str(), mapping.fileName);
        }

        if (propMemberIt->isArray && paramIndex>=static_cast<int>(param->values.size()))
        {
            std::ostringstream ss;
            ss<<"The member '"<<member.name<<"' indexed out of bounds.";
            throw ParseError("Array index out of bounds", ss.str(), mapping.fileName);
        }

        if (!IsOfType(param->typeName, propMemberIt->typeName))
        {
            std::ostringstream ss;
            ss<<"The mapped member '"<<member.name<<"' is not of the expected type. Type is '"<<param->typeName<<"' and expected type is '"<<propMemberIt->typeName<<"'";
            throw ParseError("Type missmatch in propertyMapping", ss.str(), mapping.fileName);
        }
    }

    void ParseResultFinalizer::ProcessPropertyMappedToClassMember(PropertyMappingDefinition& mapping,
                                                                  PropertyDefinition& property,
                                                                  ClassDefinition& cls,
                                                                  MappedMemberDefinition& member)
    {
        MemberDefinitions::const_iterator propMemberIt = std::find_if(property.members.begin(), property.members.end(), boost::bind(NameComparer<MemberDefinition>, _1, member.name));
        if (propMemberIt==property.members.end())
        {
            std::ostringstream ss;
            ss<<"The property '"<<mapping.propertyName<<"' does not define the member '"<<member.name<<"' that has been mapped in property mapping.";
            throw ParseError("Property member does not exist", ss.str(), mapping.fileName);
        }

        ClassDefinition* referencedClass = &cls;
        MemberDefinition* referencedMember = NULL;
        for (size_t memRefIndex=0; memRefIndex<member.memberReferences.size()-1; ++memRefIndex)
        {
            const MemberReference& memberRef = member.memberReferences[memRefIndex];
            const std::string& memberName=memberRef.first;
            const int& arrayIndex=memberRef.second;
            referencedMember = m_state.GetMember(referencedClass, memberName);
            if (referencedMember==NULL)
            {
                std::ostringstream ss;
                ss<<"The mapping for property member '"<<member.name<<"' specifies illegal classMemberReference. Member '"<<memberName<<"' does not exist in class '"<<referencedClass->name;
                throw ParseError("Member does not exist", ss.str(), mapping.fileName);
            }
            else if (referencedMember->isArray && arrayIndex<0)
            {
                std::ostringstream ss;
                ss<<"The property member '"<<member.name<<"' specifies illegal classMemberReference. Member '"<<memberName<<"' is an array but no array index is specified";
                throw ParseError("Array index missing", ss.str(), mapping.fileName);

            }
            else if (referencedMember->isArray==false && arrayIndex>=0)
            {
                std::ostringstream ss;
                ss<<"The mapping of property member '"<<member.name<<"' specifies illegal classMemberReference. Member '"<<memberName<<"' is not an array but an array index is specified";
                throw ParseError("Illegal index element", ss.str(), mapping.fileName);
            }

            referencedClass = m_state.GetClass(referencedMember->typeName);
        }

        const MemberReference& memberRef = member.memberReferences[member.memberReferences.size()-1];
        referencedMember = m_state.GetMember(referencedClass, memberRef.first);
        if (referencedMember==NULL)
        {
            std::ostringstream ss;
            ss<<"The mapping for property member '"<<member.name<<"' specifies illegal classMemberReference. Member '"<<memberRef.first<<"' does not exist in class '"<<referencedClass->name;
            throw ParseError("Member does not exist", ss.str(), mapping.fileName);
        }
        else if (!IsOfType(referencedMember->typeName, propMemberIt->typeName))
        {
            std::ostringstream ss;
            ss<<"The member '"<<referencedMember->name<<"' mapped to the property member '"<<member.name<<"' is not of the expected type. Type is '"<<referencedMember->typeName<<"' and expected type is '"<<propMemberIt->typeName<<"'";
            throw ParseError("Type missmatch in propertyMapping", ss.str(), mapping.fileName);
        }
        else if (referencedMember->isArray && !propMemberIt->isArray)
        {
            std::ostringstream ss;
            ss<<"The member '"<<referencedMember->name<<"' is an array but propertyMember '"<<member.name<<"' is not";
            throw ParseError("Property member is not an array", ss.str(), mapping.fileName);
        }
        else if (!referencedMember->isArray && propMemberIt->isArray)
        {
            std::ostringstream ss;
            ss<<"The member '"<<referencedMember->name<<"' is not an array but propertyMember '"<<member.name<<"' is";
            throw ParseError("Property member is an array", ss.str(), mapping.fileName);
        }
        else if (propMemberIt->isArray && referencedMember->arraySize<=memberRef.second)
        {
            std::ostringstream ss;
            ss<<"The member '"<<referencedMember->name<<"' indexed out of bounds.";
            throw ParseError("Array index out of bounds", ss.str(), mapping.fileName);
        }
        else if (!referencedMember->isArray && memberRef.second>=0)
        {
            std::ostringstream ss;
            ss<<"The mapping of property member '"<<member.name<<"' specifies illegal classMemberReference. Member '"<<referencedMember->name<<"' is not an array but an array index is specified";
            throw ParseError("Illegal index element", ss.str(), mapping.fileName);
        }
    }

    bool ParseResultFinalizer::ValidName(const std::string& name, bool allowDot) const
    {
        PROFILE

        //Maybe better to use regexp?
        //Valid names must start with a alpha char and the rest must be alphanumeric chars. We also allow underscores.
        std::string::const_iterator it = name.begin();
        if (name.empty() || !std::isalpha(*it))
        {
            return false;
        }
      
        ++it;
        for (; it!=name.end(); ++it)
        {            
            if (!std::isalnum(*it) && (*it)!='_' &&  !(allowDot && (*it)=='.'))
            {
                //not alphaNum, not underscore, and not an allowed dot. 
                return false;
            }
        }

        return true;
    }



    void ParseResultFinalizer::CheckNameAndFilenameConsistency(const std::string& filename, const std::string name) const
    {
        PROFILE

        size_t ix = filename.rfind(name);
        if (ix!=filename.length()-name.length()-4)
        {
            throw ParseError("File name missmatch", std::string("The file name does not match the DOB unit name: ") + name + std::string(" - Filename and Dob unit name must match!"), filename);
        }
    }  

    std::string ParseResultFinalizer::ExpandEnvironmentVariables(const std::string& str) const
    {
        PROFILE

        //Copied from dots_kernel::dots_class_parser
        const size_t start=str.find("$(");
        const size_t stop=str.find(')', start);

        if (start==std::string::npos || stop==std::string::npos)
            return str;

        const std::string var=str.substr(start+2, stop-start-2);

        //Get rid of Microsof warning
#ifdef _MSC_VER
#pragma warning(disable:4996)
#endif
        const char * const env = getenv(var.c_str());
#ifdef _MSC_VER
#pragma warning(default:4996)
#endif

        if (env == NULL)
        {
            throw ParseError(var, "", "");
        }
        const std::string res=str.substr(0, start) + env + str.substr(stop+1, str.size()-stop-1);
        return ExpandEnvironmentVariables(res); //search for next environment variable
    }

    bool ParseResultFinalizer::IsOfType(const std::string& type, const std::string& ofType) const
    {
        PROFILE

        if (type==ofType) //Will handle the case of basic types and enums
            return true;

        //Check object types and handle inheritance.
        ClassDefinition* tmpClass = m_state.GetClass(type);
        while (tmpClass)
        {
            if (tmpClass->name==ofType)
                return true;

            tmpClass=m_state.GetClass(tmpClass->baseClass);
        }

        return false;
    }

    void ParseResultFinalizer::ResolveReferences(ParseState::ParameterReferenceVector& vec, 
                                                const std::string& refName, 
                                                boost::function<void(MemberDefinition&, int)> setVal)
    {
        PROFILE

        //Resolve index refs
        for (ParseState::ParameterReferenceVector::const_iterator it=vec.begin(); it!=vec.end(); ++it)
        {
            ClassDefinition& cls = m_state.result->classes[it->topIndex];
            MemberDefinition& mem = cls.members[it->subIndex1];
            const ParameterDefinition* par = m_state.GetParameter(it->parameterName);
            if (!par)
            {
                std::ostringstream ss;
                ss<<"Could not resolve "<<refName<<" parameter "<<it->parameterName<<". Referenced from: "<<cls.name<<"."<<mem.name;
                throw ParseError("Parameter reference error", std::string("Could not resolve "+refName+" parameter ")+it->parameterName, it->fileName);
            }
            
            if (par->values.size()<=static_cast<size_t>(it->parameterIndex))
            {
                std::ostringstream ss;
                ss<<"Array index out of range for "<<refName<<" parameter '"<<it->parameterName<<"' and index="<<it->parameterIndex<<". Referenced from: "<<cls.name<<"."<<mem.name;
                throw ParseError("Parameter reference error", ss.str(), it->fileName);
            }
            
            if (!IsOfType(par->typeName, "Int32"))
            {
                std::ostringstream ss;
                ss<<"The type of the referenced parameter '"<<it->parameterName<<"' is not of the expected type. Referenced from: "<<cls.name<<"."<<mem.name;
                throw ParseError("Type missmatch", ss.str(), it->fileName);
            }

            setVal(mem, boost::lexical_cast<int, std::string>(par->values[static_cast<size_t>(it->parameterIndex)].stringVal));
        }
    }

    void ParseResultFinalizer::ResolveParameterToParameterRefs()
    {
        PROFILE

        for (ParseState::ParameterReferenceVector::const_iterator it=m_state.paramToParamReferences.begin(); it!=m_state.paramToParamReferences.end(); ++it)
        {       
            ParameterDefinition& referencingPar = m_state.result->classes[it->topIndex].parameters[it->subIndex1];
            const ParameterDefinition* par = m_state.GetParameter(it->parameterName);

            if (!par)
            {
                std::ostringstream ss;
                ss<<"Could not resolve Parameter valueRef "<<it->parameterName<<". Referenced from parameter:  "<<referencingPar.name;
                throw ParseError("Parameter reference error", ss.str(), it->fileName);
            }

            if (par->values.size()<=static_cast<size_t>(it->parameterIndex))
            {
                std::ostringstream ss;
                ss<<"Array index out of range for Parameter valueRef '"<<it->parameterName<<"' and index="<<it->parameterIndex<<". Referenced from parameter:  "<<referencingPar.name;
                throw ParseError("Parameter reference error", ss.str(), it->fileName);
            }
            
            if (!IsOfType(par->typeName, referencingPar.typeName))
            {
                std::ostringstream ss;
                ss<<"The type of the referenced parameter '"<<it->parameterName<<"' is not of the expected type. Referenced from parameter:  "<<referencingPar.name;
                throw ParseError("Type missmatch", ss.str(), it->fileName);
            }

            //TODO: Find circular references.

            referencingPar.values[it->subIndex2]=par->values[it->parameterIndex];
        }
    }
}
}
}
}
