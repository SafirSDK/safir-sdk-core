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
#include "ParseAlgorithms.h"
#include "BasicTypes.h"

#include <boost/timer.hpp>
#include <map>

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
    bool NameComparer(const T& obj, const std::string& name)
    {
        PROFILE
        return obj.Name==name;
    }

    template <class T>
    bool ContainsDuplicates(const std::vector<T>& myVector, const T** duplicated)
    {
        PROFILE
        for (typename std::vector<T>::const_iterator it = myVector.begin(); it!=myVector.end(); ++it)
        {
            size_t c = std::count_if(myVector.begin(), myVector.end(), boost::bind(NameComparer<T>, _1, it->Name));
            if (c>1)
            {
                (*duplicated)=&(*it);
                return true;
            }
        }
        return false;
    }

    template <class T>
    T* GetByFieldValue(std::vector<T>& v, const std::string& name, boost::function<const std::string&(const T&)> fieldToCmp)
    {
        PROFILE
        for (typename std::vector<T>::iterator it=v.begin(); it!=v.end(); ++it)
        {
            if (name==fieldToCmp(*it))
            {
                return &(*it);
            }
        }
        return NULL;
    }

    template <class T>
    void CheckBaseClass(std::vector<T>& vec, T& val, const std::string& rootBaseClass, const std::string& secondRootBaseClass)
    {
        PROFILE
        static const int MaxInheritanceLevel = 100; //Here we introduce a max inheritance level. Should be enough.
        
        if (val.Name==rootBaseClass || (val.Name==secondRootBaseClass && !secondRootBaseClass.empty()))
        {
            //This is the root base class.
            return;
        }

        T* current = &val;

        for (int inheritanceLevel=0; inheritanceLevel<MaxInheritanceLevel; ++inheritanceLevel)
        {
            if ((current->BaseClass==rootBaseClass) || (current->BaseClass==secondRootBaseClass && !secondRootBaseClass.empty()))
            {
                //We reached root.
                return;
            }

            T* baseClass=GetByFieldValue<T>(vec, current->BaseClass, boost::bind(&T::Name,_1));
            if (!baseClass)
            {
                std::ostringstream ss;
                ss<<"The type '"<<current->Name<<"' specifies an invalid or non-existing base class. Base class: '"<<current->BaseClass<<"'.";
                throw ParseError("Invalid base class", ss.str(), current->FileName);
            }
            current=baseClass;
        }

        throw ParseError("Circular inheritance", std::string("The inheritance tree starting from '")+val.Name+std::string("' will never end."), val.FileName);
    }

    void SetArraySize(MemberDefinition& m, int val){m.ArraySize=val;}
    void SetMaxLength(MemberDefinition& m, int val){m.MaxLength=val;}
    
    void ParseResultFinalizer::ProcessResult()
    {
        PROFILE
        //Add object to result to avoid lots of special handling in the code.
        ClassDefinition objectDef;
        objectDef.Name=BasicTypes::ObjectName;        
        objectDef.FileName=BasicTypes::ObjectName+".dou";
        m_state.Result->Classes.push_back(objectDef);

        ResolveParameterToParameterRefs();
        ResolveCreateRoutineValues();
        ResolveReferences(m_state.ArraySizeReferences, "arraySizeRef", SetArraySize);
        ResolveReferences(m_state.MaxLengthReferences, "maxLengthRef", SetMaxLength);

        std::for_each(m_state.Result->Enumerations.begin(), m_state.Result->Enumerations.end(), boost::bind(&ParseResultFinalizer::ProcessEnum, this, _1)); //ProcessEnums
        std::for_each(m_state.Result->Exceptions.begin(), m_state.Result->Exceptions.end(), boost::bind(&ParseResultFinalizer::ProcessException, this, _1)); //ProcessExceptions
        std::for_each(m_state.Result->Properties.begin(), m_state.Result->Properties.end(), boost::bind(&ParseResultFinalizer::ProcessProperty, this, _1)); //ProcessProperties
        std::for_each(m_state.Result->Classes.begin(), m_state.Result->Classes.end(), boost::bind(&ParseResultFinalizer::ProcessClass, this, _1)); //ProcessClasses
        
        ResolvePropertyMappingValueRefs();

        std::for_each(m_state.Result->PropertyMappings.begin(), m_state.Result->PropertyMappings.end(), boost::bind(&ParseResultFinalizer::ProcessPropertyMapping, this, _1)); //ProcessPropertyMapping

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

        if (!ValidName(e.Name, true))
        {
            throw ParseError("Invalid name", std::string("Enumeration name '")+e.Name+std::string(" is invalid. Must start with an alphabetic char and then only contain alpha-numeric chars"), e.FileName);
        }

        CheckNameAndFilenameConsistency(e.FileName, e.Name);
        if (std::count_if(m_state.Result->Enumerations.begin(), m_state.Result->Enumerations.end(), boost::bind(NameComparer<EnumerationDefinition>, _1, e.Name))>1)
        {
            throw ParseError("Duplicated enumeration definition", e.Name+std::string(" is defined more than one time."), e.FileName);
        }

        for (StringVector::const_iterator it=e.EnumerationValues.begin(); it!=e.EnumerationValues.end(); ++it)
        {
            if (!ValidName(*it, false))
            {
                throw ParseError("Invalid enumeration value", 
                std::string("Enumeration value '")+(*it)+std::string(" is invalid. Must start with an alphabetic char and then only contain alpha-numeric chars"), e.FileName);
            }

            if (std::count(e.EnumerationValues.begin(), e.EnumerationValues.end(), *it)>1)
            {
                throw ParseError("Duplicated enumeration value", (*it)+std::string(" exists more than one time in enum type ")+e.Name, e.FileName);
            }
        }
    }

    void ParseResultFinalizer::ProcessException(ExceptionDefinition& e)
    {
        PROFILE

        if (!ValidName(e.Name, true))
        {
            throw ParseError("Invalid name", std::string("Exception name '")+e.Name+std::string(" is invalid. Must start with an alphabetic char and then only contain alpha-numeric chars"), e.FileName);
        }

        CheckNameAndFilenameConsistency(e.FileName, e.Name);
        if (std::count_if(m_state.Result->Exceptions.begin(), m_state.Result->Exceptions.end(), boost::bind(NameComparer<ExceptionDefinition>, _1, e.Name))>1)
        {
            throw ParseError("Duplicated exception definition", e.Name+std::string(" is defined more than one time."), e.FileName);
        }

        CheckBaseClass<ExceptionDefinition>(m_state.Result->Exceptions, e, BasicTypes::ExceptionName, BasicTypes::FundamentalExceptionName);
    }

    void ParseResultFinalizer::ProcessProperty(PropertyDefinition& p)
    {
        PROFILE

        if (!ValidName(p.Name, true))
        {
            throw ParseError("Invalid name", std::string("Property name '")+p.Name+std::string(" is invalid. Must start with an alphabetic char and then only contain alpha-numeric chars"), p.FileName);
        }

        CheckNameAndFilenameConsistency(p.FileName, p.Name);
        if (std::count_if(m_state.Result->Properties.begin(), m_state.Result->Properties.end(), boost::bind(NameComparer<PropertyDefinition>, _1, p.Name))>1)
        {
            throw ParseError("Duplicated property definition", p.Name+std::string(" is defined more than one time."), p.FileName);
        }
        

        //Handle property members
        for (MemberDefinitions::iterator it=p.Members.begin(); it!=p.Members.end(); ++it)
        {
            if (std::count_if(p.Members.begin(), p.Members.end(), boost::bind(NameComparer<MemberDefinition>, _1, it->Name))>1)
            {
                throw ParseError("Duplicated property member", it->Name+std::string(" is defined more than one time int property '")+p.Name, p.FileName);
            }
            
            if (!BasicTypes::Instance().MemberTypeOf(it->TypeName, m_state.Result, it->MemberType))
            {
                std::ostringstream ss;
                ss<<"The member '"<<it->Name<<"' in property '"<<p.Name<<"' has an invalid type specified. Type: "<<it->TypeName;
                throw ParseError("Invalid type", ss.str(), p.FileName);
            }
        }
    }

    void ParseResultFinalizer::ProcessClass(ClassDefinition& c)
    {
        PROFILE

        if (!ValidName(c.Name, true))
        {
            throw ParseError("Invalid name", std::string("Class name '")+c.Name+std::string(" is invalid. Must start with an alphabetic char and then only contain alpha-numeric chars"), c.FileName);
        }

        CheckNameAndFilenameConsistency(c.FileName, c.Name);
        if (std::count_if(m_state.Result->Classes.begin(), m_state.Result->Classes.end(), boost::bind(NameComparer<ClassDefinition>, _1, c.Name))>1)
        {
            throw ParseError("Duplicated class definition", c.Name+std::string(" is defined more than one time."), c.FileName);
        }

        CheckBaseClass<ClassDefinition>(m_state.Result->Classes, c, BasicTypes::ObjectName, "");

        //Handle CreateRoutines
        std::for_each(c.CreateRoutines.begin(), c.CreateRoutines.end(), boost::bind(&ParseResultFinalizer::ProcessCreateRoutine, this, c, _1));

        //Handle Members
        std::for_each(c.Members.begin(), c.Members.end(), boost::bind(&ParseResultFinalizer::ProcessClassMember, this, c, _1));

        //Handle Parameters
        std::for_each(c.Parameters.begin(), c.Parameters.end(), boost::bind(&ParseResultFinalizer::ProcessParameter, this, c, _1));
    }

    void ParseResultFinalizer::ProcessCreateRoutine(ClassDefinition& host, CreateRoutineDefinition& c)
    {
        PROFILE

        if (!ValidName(c.Name, false))
        {
            std::ostringstream ss;
            ss<<"The create routine name '"<<c.Name<<"' in class '"<<host.Name<<"' is illegal.";
            throw ParseError("Illegal name", ss.str(), host.FileName);
        }

        //Check that all create routine parameters are existing members inte the class
        for (StringVector::const_iterator member=c.Parameters.begin(); member!=c.Parameters.end(); ++member)
        {
            const MemberDefinition* found = GetMember(host, *member);
            if (found==NULL)
            {
                //the create routine parameter was not found in class members
                std::ostringstream ss;
                ss<<"The create routine '"<<c.Name<<"' in class '"<<host.Name<<"' has specified an invalid member: "<<(*member);
                throw ParseError("Illegal create routine member", ss.str(), host.FileName);
            }

            //Check for create routine parameter duplicates
            if (std::count(c.Parameters.begin(), c.Parameters.end(), *member)>1)
            {                
                std::ostringstream ss;
                ss<<"The create routine '"<<c.Name<<"' in class '"<<host.Name<<"' has specified member '"<<(*member)<<"' more than one time.";
                throw ParseError("Duplicated create routine member", ss.str(), host.FileName);
            }
        }

        //Check that all member values are ok
        for (MemberValueVector::const_iterator memberValue=c.MemberValues.begin(); memberValue!=c.MemberValues.end(); ++memberValue)
        {
            const MemberDefinition* found = GetMember(host, memberValue->first);            
            if (found==NULL)
            {
                //the create routine parameter was not found in class members
                std::ostringstream ss;
                ss<<"The create routine '"<<c.Name<<"' in class '"<<host.Name<<"' has specified default value for an invalid member: "<<memberValue->first;
                throw ParseError("Illegal create routine member", ss.str(), host.FileName);
            }
            else if (!BasicTypes::Instance().CanParseValue(found->TypeName, memberValue->second, m_state.Result))
            {
                std::ostringstream ss;
                ss<<"The create routine '"<<c.Name<<"' in class '"<<host.Name<<"' has specified a default value '"<<memberValue->second<<"' for member '"<<found->Name<<"' that can not be converted to the expected type: "<<found->TypeName;
                throw ParseError("Illegal create routine default value", ss.str(), host.FileName);
            }
            
           //TODO: check for duplicated membervalues. Check for membervalues that already are present as parameters. Add indexer to init array values.
        }
    }

    void ParseResultFinalizer::ProcessClassMember(ClassDefinition& host, MemberDefinition& m)
    {
        PROFILE

        if (!ValidName(m.Name, false))
        {
            std::ostringstream ss;
            ss<<"The member name '"<<m.Name<<"' in class '"<<host.Name<<"' is illegal.";
            throw ParseError("Illegal name", ss.str(), host.FileName);
        }

        if (!BasicTypes::Instance().MemberTypeOf(m.TypeName, m_state.Result, m.MemberType))
        {
            std::ostringstream ss;
            ss<<"The member '"<<m.Name<<"' in class '"<<host.Name<<"' has an invalid type specified. Type: "<<m.TypeName;
            throw ParseError("Invalid type", ss.str(), host.FileName);
        }

        //Check for member duplicates
        if (std::count_if(host.Members.begin(), host.Members.end(), boost::bind(NameComparer<MemberDefinition>, _1, m.Name))>1)
        {
            std::ostringstream ss;
            ss<<"The member '"<<m.Name<<"' in class '"<<host.Name<<"' is defined more than one time.";
            throw ParseError("Duplicated class member", ss.str(), host.FileName);
        }
    }

    void ParseResultFinalizer::ProcessParameter(ClassDefinition& host, ParameterDefinition& p)
    {
        PROFILE

        if (!ValidName(p.Name, false))
        {
            std::ostringstream ss;
            ss<<"The parameter name '"<<p.Name<<"' in class '"<<host.Name<<"' is illegal.";
            throw ParseError("Illegal name", ss.str(), host.FileName);
        }

        if (!BasicTypes::Instance().MemberTypeOf(p.TypeName, m_state.Result, p.MemberType))
        {
            std::ostringstream ss;
            ss<<"The parameter '"<<p.Name<<"' in class '"<<host.Name<<"' has an invalid type specified. Type: "<<p.TypeName;
            throw ParseError("Invalid type", ss.str(), host.FileName);
        }

        for (StringVector::iterator valIt=p.Values.begin(); valIt!=p.Values.end(); ++valIt)
        {
            try
            {
                (*valIt) = ExpandEnvironmentVariables(*valIt);
            }
            catch (const ParseError& err)
            {
                std::ostringstream ss;
                ss<<"Failed to expand the environment variable '"<<err.Label()<<"'. Remember to restart the system after you add/change environment variables.";
                throw ParseError("Environment variable expansion error", ss.str(), host.FileName);
            }

            if (!BasicTypes::Instance().CanParseValue(p.TypeName, *valIt, m_state.Result))
            {
                std::ostringstream ss;
                ss<<"The parameter '"<<p.Name<<"' with value '"<<*valIt<<"' cannot be converted to the expected type "<<p.TypeName;
                throw ParseError("Failed to interpret parameter value", ss.str(), host.FileName);
            }
        }
    }

    void ParseResultFinalizer::ProcessPropertyMapping(PropertyMappingDefinition& p)
    {
        PROFILE

        //std::string classAndPropName=p.ClassName+std::string("-")+p.PropertyName;
        CheckNameAndFilenameConsistency(p.FileName, p.ClassName+std::string("-")+p.PropertyName);

        //Check for duplicated property mappings
        int count=0;
        for (PropertyMappingDefinitions::const_iterator it=m_state.Result->PropertyMappings.begin(); it!=m_state.Result->PropertyMappings.end(); ++it)
        {
            if (it->ClassName==p.ClassName && it->PropertyName==p.PropertyName)
                ++count;
        }
        if (count>1)
        {
            throw ParseError("Duplicated property mapping definition", "The PropertyMapping is defined more than one time.", p.FileName);
        }
        
        //Verify that property exists
        PropertyDefinition* prop = GetByFieldValue<PropertyDefinition>(m_state.Result->Properties, p.PropertyName, boost::bind(&PropertyDefinition::Name,_1));        
        if (!prop)
        {
            std::ostringstream ss;
            ss<<"The property '"<<p.PropertyName<<"' does not exist. Referenced from property mapping dom-file.";
            throw ParseError("Property does not exist", ss.str(), p.FileName);
        }

        //Verify that class exists
        ClassDefinition* cls = GetByFieldValue<ClassDefinition>(m_state.Result->Classes, p.ClassName, boost::bind(&ClassDefinition::Name,_1));
        if (!cls)
        {
            std::ostringstream ss;
            ss<<"The class '"<<p.ClassName<<"' does not exist. Referenced from property mapping dom-file.";
            throw ParseError("Class does not exist", ss.str(), p.FileName);
        }

        //Check that all propertyMembers have been mapped exactly one time
        for (MemberDefinitions::const_iterator it=prop->Members.begin(); it!=prop->Members.end(); ++it)
        {
            size_t count = std::count_if(p.MappedMembers.begin(), p.MappedMembers.end(), boost::bind(NameComparer<MappedMemberDefinition>, _1, it->Name));
            if (count>1)
            {
                std::ostringstream ss;
                ss<<"The property member '"<<it->Name<<"' in property '"<<prop->Name<<"' is defined more than one time in property mapping.";
                throw ParseError("Duplicated property member mapping", ss.str(), p.FileName);
            }
            else if (count==0)
            {
                std::ostringstream ss;
                ss<<"The property member '"<<it->Name<<"' in property '"<<prop->Name<<"' is not mapped in property mapping.";
                throw ParseError("Missing property member in property mapping", ss.str(), p.FileName);
            }

        }
        
        //Check that there's no mapped members thats not defined by the property.
        for (MappedMemberDefinitions::const_iterator it=p.MappedMembers.begin(); it!=p.MappedMembers.end(); ++it)
        {       
            MemberDefinitions::const_iterator propMemberIt = std::find_if(prop->Members.begin(), prop->Members.end(), boost::bind(NameComparer<MemberDefinition>, _1, it->Name));

            if (propMemberIt==prop->Members.end())
            {
                std::ostringstream ss;
                ss<<"The property '"<<p.PropertyName<<"' does not define the member '"<<it->Name<<"' that has been mapped in property mapping.";
                throw ParseError("Property member does not exist", ss.str(), p.FileName);
            }
            
            //If the propertyMember is mapped to a value, check that the value can be parsed as the expected type.
            if (it->Kind==MappedToParameter)
            {
                //member is mapped to a value, now check if the value looks correct according to the type specified by the property
                if (!BasicTypes::Instance().CanParseValue(propMemberIt->TypeName, it->Value, m_state.Result))
                {
                    std::ostringstream ss;
                    ss<<"The mapped property member '"<<it->Name<<"' with value '"<<it->Value<<"' cannot be converted to the expected type "<<propMemberIt->TypeName;
                    throw ParseError("Failed to interpret mapped value", ss.str(), p.FileName);
                }
            }
            else if (it->Kind==MappedToMember) //Mapped to a class member. Verify that member exists, arrayIndices not out of bounds, type compliance.
            {
                //TODO:
                for (MemberReferenceVector::const_iterator memberRefIt=it->MemberReferences.begin(); memberRefIt!=it->MemberReferences.end(); ++memberRefIt)
                {
                    //const std::string& memberName=memberRefIt->first;
                    //const int& arrayIndex=memberRefIt->second;
                }
            }
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

    void ParseResultFinalizer::SplitFullName(const std::string& fullName, std::string& part1, std::string& part2) const
    {
        PROFILE

        size_t dotIx = fullName.find_last_of('.');
        if (dotIx>0)
        {
            part1=fullName.substr(0, dotIx);
            part2=fullName.substr(dotIx+1);
        }
        else
        {
            part1.clear();
            part2=fullName;
        }
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

    const ParameterDefinition* ParseResultFinalizer::GetParameter(const std::string& name) const
    {
        PROFILE

        std::string cls, par;
        SplitFullName(name, cls, par);
        ClassDefinition* cd = GetByFieldValue<ClassDefinition>(m_state.Result->Classes, cls, boost::bind(&ClassDefinition::Name,_1));
        if (cd)
        {
            ParameterDefinition* pd = GetByFieldValue<ParameterDefinition>(cd->Parameters, par, boost::bind(&ParameterDefinition::Name,_1));
            return pd;
        }

        return NULL;
    }

    bool ParseResultFinalizer::IsOfType(const std::string& type, const std::string& ofType) const
    {
        PROFILE

        if (type==ofType) //Will handle the case of basic types and enums
            return true;

        //Check object types and handle inheritance.
        ClassDefinition* tmpClass = GetByFieldValue<ClassDefinition>(m_state.Result->Classes, type, boost::bind(&ClassDefinition::Name,_1));
        while (tmpClass)
        {
            if (tmpClass->Name==ofType)
                return true;

            tmpClass=GetByFieldValue<ClassDefinition>(m_state.Result->Classes, tmpClass->BaseClass, boost::bind(&ClassDefinition::Name,_1));
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
            ClassDefinition& cls = m_state.Result->Classes[it->TopIndex];
            MemberDefinition& mem = cls.Members[it->SubIndex1];
            const ParameterDefinition* par = GetParameter(it->ParameterName);
            if (!par)
            {
                std::ostringstream ss;
                ss<<"Could not resolve "<<refName<<" parameter "<<it->ParameterName<<". Referenced from: "<<cls.Name<<"."<<mem.Name;
                throw ParseError("Parameter reference error", std::string("Could not resolve arraySizeRef parameter ")+it->ParameterName, it->FileName);
            }
            
            if (par->Values.size()<=static_cast<size_t>(it->ParameterIndex))
            {
                std::ostringstream ss;
                ss<<"Array index out of range for "<<refName<<" parameter '"<<it->ParameterName<<"' and index="<<it->ParameterIndex<<". Referenced from: "<<cls.Name<<"."<<mem.Name;
                throw ParseError("Parameter reference error", ss.str(), it->FileName);
            }
            
            if (!IsOfType(par->TypeName, "Int32"))
            {
                std::ostringstream ss;
                ss<<"The type of the referenced parameter '"<<it->ParameterName<<"' is not of the expected type. Referenced from: "<<cls.Name<<"."<<mem.Name;
                throw ParseError("Type missmatch", ss.str(), it->FileName);
            }

            setVal(mem, boost::lexical_cast<int, std::string>(par->Values[static_cast<size_t>(it->ParameterIndex)]));
        }
    }

    const MemberDefinition* ParseResultFinalizer::GetMember(ClassDefinition& cls, const std::string& name) const
    {
        PROFILE

        const MemberDefinition* member=GetByFieldValue<MemberDefinition>(cls.Members, name, boost::bind(&MemberDefinition::Name,_1));
        if (member==NULL)
        {
            ClassDefinition* baseClass = GetByFieldValue<ClassDefinition>(m_state.Result->Classes, cls.BaseClass, boost::bind(&ClassDefinition::Name,_1));
            if (baseClass)
            {
                return GetMember(*baseClass, name);
            }
        }

        return member;
    }

    void ParseResultFinalizer::ResolveCreateRoutineValues()
    {
        PROFILE

        for (ParseState::ParameterReferenceVector::const_iterator it=m_state.CreateRoutineValueReferences.begin(); it!=m_state.CreateRoutineValueReferences.end(); ++it)
        {
            ClassDefinition& cls = m_state.Result->Classes[it->TopIndex];
            CreateRoutineDefinition& cr = cls.CreateRoutines[it->SubIndex1];
            MemberValue& mv = cr.MemberValues[it->SubIndex2];
            const ParameterDefinition* par = GetParameter(it->ParameterName);
            const MemberDefinition* member=GetMember(cls, mv.first);
            if (!par)
            {
                std::ostringstream ss;
                ss<<"Could not resolve CreateRoutine member value parameter "<<it->ParameterName<<". Referenced from create routine member:  "<<cls.Name<<"."<<cr.Name<<"."<<mv.first;
                throw ParseError("Parameter reference error", ss.str(), it->FileName);
            }
            if (!member)
            {
                std::ostringstream ss;
                ss<<"Create routine defines initial value for member that does not exist. Member '"<<mv.first<<"' does not exist in class '"<<cls.Name<<". Referenced from create routine member:  "<<cls.Name<<"."<<cr.Name<<"."<<mv.first;
                throw ParseError("Illegal create routine member initialization", ss.str(), it->FileName);
            }

            if (par->Values.size()<=static_cast<size_t>(it->ParameterIndex))
            {
                std::ostringstream ss;
                ss<<"Array index out of range for CreateRoutine member value parameter '"<<it->ParameterName<<"' and index="<<it->ParameterIndex<<". Referenced from create routine member: "<<cls.Name<<"."<<cr.Name<<"."<<mv.first;
                throw ParseError("Parameter reference error", ss.str(), it->FileName);
            }
            
            if (!IsOfType(par->TypeName, member->TypeName))
            {
                std::ostringstream ss;
                ss<<"The type of the referenced parameter '"<<it->ParameterName<<"' is not of the expected type. Referenced from create routine member: "<<cls.Name<<"."<<cr.Name<<"."<<mv.first;
                throw ParseError("Type missmatch", ss.str(), it->FileName);
            }

            mv.second=par->Values[it->ParameterIndex];
        }
    }

    void ParseResultFinalizer::ResolvePropertyMappingValueRefs()
    {
        PROFILE

        for (ParseState::ParameterReferenceVector::const_iterator it=m_state.MappedValueReferences.begin(); it!=m_state.MappedValueReferences.end(); ++it)
        {
            PropertyMappingDefinition& propertyMapping = m_state.Result->PropertyMappings[it->TopIndex];
            MappedMemberDefinition& mappedMember = propertyMapping.MappedMembers[it->SubIndex1];            
            const ParameterDefinition* par = GetParameter(it->ParameterName);

            if (!par)
            {
                std::ostringstream ss;
                ss<<"Could not resolve PropertyMapping valueRef "<<it->ParameterName<<". Referenced from property mapping member:  "<<mappedMember.Name;
                throw ParseError("Parameter reference error", ss.str(), it->FileName);
            }

            if (par->Values.size()<=static_cast<size_t>(it->ParameterIndex))
            {
                std::ostringstream ss;
                ss<<"Array index out of range for PropertyMapping member valueRef parameter '"<<it->ParameterName<<"' and index="<<it->ParameterIndex<<". Referenced from property mapping member:  "<<mappedMember.Name;
                throw ParseError("Parameter reference error", ss.str(), it->FileName);
            }

            PropertyDefinition* prop = GetByFieldValue<PropertyDefinition>(m_state.Result->Properties, propertyMapping.PropertyName, boost::bind(&PropertyDefinition::Name,_1));            
            MemberDefinition* propMember = GetByFieldValue<MemberDefinition>(prop->Members, mappedMember.Name, boost::bind(&MemberDefinition::Name,_1));                        
            
            if (!IsOfType(par->TypeName, propMember->TypeName))
            {
                std::ostringstream ss;
                ss<<"The type of the referenced parameter '"<<it->ParameterName<<"' is not of the expected type. Referenced from property mapping member:  "<<mappedMember.Name;
                throw ParseError("Type missmatch", ss.str(), it->FileName);
            }

            mappedMember.Value=par->Values[it->ParameterIndex];
            mappedMember.Kind=MappedToParameter;
        }
    }

    void ParseResultFinalizer::ResolveParameterToParameterRefs()
    {
        PROFILE

        for (ParseState::ParameterReferenceVector::const_iterator it=m_state.ParamToParamReferences.begin(); it!=m_state.ParamToParamReferences.end(); ++it)
        {       
            ParameterDefinition& referencingPar = m_state.Result->Classes[it->TopIndex].Parameters[it->SubIndex1];
            const ParameterDefinition* par = GetParameter(it->ParameterName);

            if (!par)
            {
                std::ostringstream ss;
                ss<<"Could not resolve Parameter valueRef "<<it->ParameterName<<". Referenced from parameter:  "<<referencingPar.Name;
                throw ParseError("Parameter reference error", ss.str(), it->FileName);
            }

            if (par->Values.size()<=static_cast<size_t>(it->ParameterIndex))
            {
                std::ostringstream ss;
                ss<<"Array index out of range for Parameter valueRef '"<<it->ParameterName<<"' and index="<<it->ParameterIndex<<". Referenced from parameter:  "<<referencingPar.Name;
                throw ParseError("Parameter reference error", ss.str(), it->FileName);
            }
            
            if (!IsOfType(par->TypeName, referencingPar.TypeName))
            {
                std::ostringstream ss;
                ss<<"The type of the referenced parameter '"<<it->ParameterName<<"' is not of the expected type. Referenced from parameter:  "<<referencingPar.Name;
                throw ParseError("Type missmatch", ss.str(), it->FileName);
            }

            //TODO: Find circular references.

            referencingPar.Values[it->SubIndex2]=par->Values[it->ParameterIndex];
        }
    }
}
}
}
}
