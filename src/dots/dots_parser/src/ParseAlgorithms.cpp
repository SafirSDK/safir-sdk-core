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

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Parser
{
    template <class T>
    bool NameComparer(const T& obj, const std::string& name)
    {
        return obj.Name==name;
    }

    template <class T>
    bool ContainsDuplicates(const std::vector<T>& myVector, const T** duplicated)
    {
        for (std::vector<T>::const_iterator it = myVector.begin(); it!=myVector.end(); ++it)
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
    T* GetByName(std::vector<T>& v, const std::string& name)
    {
        std::vector<T>::iterator it = std::find_if(v.begin(), v.end(), boost::bind(NameComparer<T>, _1, name) );
        if (it!=v.end())
        {
            return &(*it);
        }
        
        return NULL;
    }    

    template <class T>
    void CheckBaseClass(std::vector<T>& vec, T& val, const std::string& rootBaseClass, const std::string& secondRootBaseClass)
    {
        static const int MaxInheritanceLevel = 100; //Here we introduce a max inheritance level. Should be enough.
                
        T* current = &val;

        for (int inheritanceLevel=0; inheritanceLevel<MaxInheritanceLevel; ++inheritanceLevel)
        {
            if ((current->BaseClass==rootBaseClass) || (current->BaseClass==secondRootBaseClass && !secondRootBaseClass.empty()))
            {
                //We reached root.
                return;
            }

            T* baseClass=GetByName<T>(vec, current->BaseClass);
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

    void SetArraySize(ClassMemberDefinition& m, int val){m.ArraySize=val;}
    void SetMaxLength(ClassMemberDefinition& m, int val){m.MaxLength=val;}
    
    void ParseResultFinalizer::ProcessResult()
    {
        std::for_each(m_state.Result.Enumerations.begin(), m_state.Result.Enumerations.end(), boost::bind(&ParseResultFinalizer::ProcessEnum, this, _1)); //ProcessEnums
        std::for_each(m_state.Result.Exceptions.begin(), m_state.Result.Exceptions.end(), boost::bind(&ParseResultFinalizer::ProcessException, this, _1)); //ProcessExceptions
        std::for_each(m_state.Result.Properties.begin(), m_state.Result.Properties.end(), boost::bind(&ParseResultFinalizer::ProcessProperty, this, _1)); //ProcessProperties
        std::for_each(m_state.Result.Classes.begin(), m_state.Result.Classes.end(), boost::bind(&ParseResultFinalizer::ProcessClass, this, _1)); //ProcessClasses

        ResolveReferences(m_state.ArraySizeReferences, "arraySizeRef", SetArraySize);
        ResolveReferences(m_state.MaxLengthReferences, "maxLengthRef", SetMaxLength);
        ResolveCreateRoutineValues();

        std::for_each(m_state.Result.PropertyMappings.begin(), m_state.Result.PropertyMappings.end(), boost::bind(&ParseResultFinalizer::ProcessPropertyMapping, this, _1)); //ProcessPropertyMappings
    }

    void ParseResultFinalizer::ProcessEnum(EnumerationDefinition& e)
    {
        if (!ValidName(e.Name, true))
        {
            throw ParseError("Invalid name", std::string("Enumeration name '")+e.Name+std::string(" is invalid. Must start with an alphabetic char and then only contain alpha-numeric chars"), e.FileName);
        }

        CheckNameAndFilenameConsistency(e.FileName, e.Name);
        if (std::count_if(m_state.Result.Enumerations.begin(), m_state.Result.Enumerations.end(), boost::bind(NameComparer<EnumerationDefinition>, _1, e.Name))>1)
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
        if (!ValidName(e.Name, true))
        {
            throw ParseError("Invalid name", std::string("Exception name '")+e.Name+std::string(" is invalid. Must start with an alphabetic char and then only contain alpha-numeric chars"), e.FileName);
        }

        CheckNameAndFilenameConsistency(e.FileName, e.Name);
        if (std::count_if(m_state.Result.Exceptions.begin(), m_state.Result.Exceptions.end(), boost::bind(NameComparer<ExceptionDefinition>, _1, e.Name))>1)
        {
            throw ParseError("Duplicated exception definition", e.Name+std::string(" is defined more than one time."), e.FileName);
        }

        CheckBaseClass<ExceptionDefinition>(m_state.Result.Exceptions, e, "Exception", "FundamentalException");
    }

    void ParseResultFinalizer::ProcessProperty(PropertyDefinition& p)
    {
        if (!ValidName(p.Name, true))
        {
            throw ParseError("Invalid name", std::string("Property name '")+p.Name+std::string(" is invalid. Must start with an alphabetic char and then only contain alpha-numeric chars"), p.FileName);
        }

        CheckNameAndFilenameConsistency(p.FileName, p.Name);
        if (std::count_if(m_state.Result.Properties.begin(), m_state.Result.Properties.end(), boost::bind(NameComparer<PropertyDefinition>, _1, p.Name))>1)
        {
            throw ParseError("Duplicated property definition", p.Name+std::string(" is defined more than one time."), p.FileName);
        }
        

        //Handle property members
        for (PropertyMemberDefinitions::const_iterator it=p.Members.begin(); it!=p.Members.end(); ++it)
        {
            if (std::count_if(p.Members.begin(), p.Members.end(), boost::bind(NameComparer<PropertyMemberDefinition>, _1, it->Name))>1)
            {
                throw ParseError("Duplicated property member", it->Name+std::string(" is defined more than one time int property '")+p.Name, p.FileName);
            }

            if (!ValidType(it->TypeName))
            {
                std::ostringstream ss;
                ss<<"The member '"<<it->Name<<"' in property '"<<p.Name<<"' has an invalid type specified. Type: "<<it->TypeName;
                throw ParseError("Invalid type", ss.str(), p.FileName);
            }
        }
    }

    void ParseResultFinalizer::ProcessClass(ClassDefinition& c)
    {
        if (!ValidName(c.Name, true))
        {
            throw ParseError("Invalid name", std::string("Class name '")+c.Name+std::string(" is invalid. Must start with an alphabetic char and then only contain alpha-numeric chars"), c.FileName);
        }

        CheckNameAndFilenameConsistency(c.FileName, c.Name);
        if (std::count_if(m_state.Result.Classes.begin(), m_state.Result.Classes.end(), boost::bind(NameComparer<ClassDefinition>, _1, c.Name))>1)
        {
            throw ParseError("Duplicated class definition", c.Name+std::string(" is defined more than one time."), c.FileName);
        }

        CheckBaseClass<ClassDefinition>(m_state.Result.Classes, c, "Object", "");

        //Handle CreateRoutines
        std::for_each(c.CreateRoutines.begin(), c.CreateRoutines.end(), boost::bind(&ParseResultFinalizer::ProcessCreateRoutine, this, c, _1));

        //Handle Members
        std::for_each(c.Members.begin(), c.Members.end(), boost::bind(&ParseResultFinalizer::ProcessClassMember, this, c, _1));

        //Handle Parameters
        std::for_each(c.Parameters.begin(), c.Parameters.end(), boost::bind(&ParseResultFinalizer::ProcessParameter, this, c, _1));
    }

    void ParseResultFinalizer::ProcessCreateRoutine(ClassDefinition& host, CreateRoutineDefinition& c)
    {
        if (!ValidName(c.Name, false))
        {
            std::ostringstream ss;
            ss<<"The create routine name '"<<c.Name<<"' in class '"<<host.Name<<"' is illegal.";
            throw ParseError("Illegal name", ss.str(), host.FileName);
        }

        //Check that all create routine parameters are existing members inte the class
        for (StringVector::const_iterator member=c.Parameters.begin(); member!=c.Parameters.end(); ++member)
        {
            ClassMemberDefinitions::const_iterator found=std::find_if(host.Members.begin(), host.Members.end(), boost::bind(NameComparer<ClassMemberDefinition>, _1, *member));
            if (found==host.Members.end())
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
    }

    void ParseResultFinalizer::ProcessClassMember(ClassDefinition& host, ClassMemberDefinition& m)
    {
        if (!ValidName(m.Name, false))
        {
            std::ostringstream ss;
            ss<<"The member name '"<<m.Name<<"' in class '"<<host.Name<<"' is illegal.";
            throw ParseError("Illegal name", ss.str(), host.FileName);
        }

        if (!ValidType(m.TypeName))
        {
            std::ostringstream ss;
            ss<<"The member '"<<m.Name<<"' in class '"<<host.Name<<"' has an invalid type specified. Type: "<<m.TypeName;
            throw ParseError("Invalid type", ss.str(), host.FileName);
        }

        //Check for member duplicates
        if (std::count_if(host.Members.begin(), host.Members.end(), boost::bind(NameComparer<ClassMemberDefinition>, _1, m.Name))>1)
        {
            std::ostringstream ss;
            ss<<"The member '"<<m.Name<<"' in class '"<<host.Name<<"' is defined more than one time.";
            throw ParseError("Duplicated class member", ss.str(), host.FileName);
        }
    }

    void ParseResultFinalizer::ProcessParameter(ClassDefinition& host, ParameterDefinition& p)
    {
        if (!ValidName(p.Name, false))
        {
            std::ostringstream ss;
            ss<<"The parameter name '"<<p.Name<<"' in class '"<<host.Name<<"' is illegal.";
            throw ParseError("Illegal name", ss.str(), host.FileName);
        }

        if (!ValidType(p.TypeName))
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
        }
    }

    void ParseResultFinalizer::ProcessPropertyMapping(PropertyMappingDefinition& p)
    {
        p;
        //TODO
    }


    bool ParseResultFinalizer::ValidName(const std::string& name, bool allowDot) const
    {
        //Maybe better to use regexp?
        //Valid names must start with a alpha char and the rest must be alphanumeric chars. We also allow underscores. 
        std::string::const_iterator it = name.begin();
        if (name.empty() || !std::isalpha(*it))
        {
            return false;
        }
      
        ++it;
        for (it; it!=name.end(); ++it)
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
        size_t ix = filename.rfind(name);
        if (ix!=filename.length()-name.length()-4)
        {
            throw ParseError("File name missmatch", std::string("The file name does not match the DOB unit name: ") + name + std::string(" - Filename and Dob unit name must match!"), filename);
        }
    }

    bool ParseResultFinalizer::ValidType(const std::string& typeName) const
    {
        //Check if we have a basic type, i.e Int32, Float64, Boolean, String etc.
        static std::set<std::string> basicTypes = CreateBasicTypeSet();
        if (basicTypes.find(typeName)!=basicTypes.end())
        {
            return true; //A basic type.
        }

        //Check if it is a defined enum type
        EnumerationDefinitions::const_iterator enumIt=std::find_if( m_state.Result.Enumerations.begin(), m_state.Result.Enumerations.end(), 
                                                                    boost::bind(NameComparer<EnumerationDefinition>, _1, typeName));
        if (enumIt!=m_state.Result.Enumerations.end())
        {
            return true;
        }

        //Check if complex type, i.e defined class.
        ClassDefinitions::const_iterator classIt=std::find_if( m_state.Result.Classes.begin(), m_state.Result.Classes.end(), 
                                                                    boost::bind(NameComparer<ClassDefinition>, _1, typeName));
        if (classIt!=m_state.Result.Classes.end())
        {
            return true;
        }

        //Not a valid type
        return false;
    }

    std::set<std::string> ParseResultFinalizer::CreateBasicTypeSet()
    {
        std::set<std::string> types;
        types.insert("Boolean");
        types.insert("Enumeration");
        types.insert("Int32");
        types.insert("Int64");
        types.insert("Float32");
        types.insert("Float64");
        types.insert("TypeId");
        types.insert("InstanceId");
        types.insert("EntityId");
        types.insert("ChannelId");
        types.insert("HandlerId");
        types.insert("String");
        types.insert("Object");
        types.insert("Binary");
        types.insert("Ampere32");
        types.insert("CubicMeter32");
        types.insert("Hertz32");
        types.insert("Joule32");
        types.insert("Kelvin32");
        types.insert("Kilogram32");
        types.insert("Meter32");
        types.insert("MeterPerSecond32");
        types.insert("MeterPerSecondSquared32");
        types.insert("Newton32");
        types.insert("Pascal32");
        types.insert("Radian32");
        types.insert("RadianPerSecond32");
        types.insert("RadianPerSecondSquared32");
        types.insert("Second32");
        types.insert("SquareMeter32");
        types.insert("Steradian32");
        types.insert("Volt32");
        types.insert("Watt32");
        types.insert("Ampere64");
        types.insert("CubicMeter64");
        types.insert("Hertz64");
        types.insert("Joule64");
        types.insert("Kelvin64");
        types.insert("Kilogram64");
        types.insert("Meter64");
        types.insert("MeterPerSecond64");
        types.insert("MeterPerSecondSquared64");
        types.insert("Newton64");
        types.insert("Pascal64");
        types.insert("Radian64");
        types.insert("RadianPerSecond64");
        types.insert("RadianPerSecondSquared64");
        types.insert("Second64");
        types.insert("SquareMeter64");
        types.insert("Steradian64");
        types.insert("Volt64");
        types.insert("Watt64");
        return types;
    }


    std::string ParseResultFinalizer::ExpandEnvironmentVariables(const std::string& str) const
    {
        //Copied from dots_kernel::dots_class_parser
        const size_t start=str.find("$(");
        const size_t stop=str.find(')', start);

        if (start==std::string::npos || stop==std::string::npos)
            return str;

        const std::string var=str.substr(start+2, stop-start-2);

        //Get rid of Microsof warning
#pragma warning(disable:4996)
        const char * const env = getenv(var.c_str());
#pragma warning(default:4996)

        if (env == NULL)
        {
            throw ParseError(var, "", "");
        }
        const std::string res=str.substr(0, start) + env + str.substr(stop+1, str.size()-stop-1);
        return ExpandEnvironmentVariables(res); //search for next environment variable
    }

    const ParameterDefinition* ParseResultFinalizer::GetParameter(const std::string& name) const
    {
        std::string cls, par;
        SplitFullName(name, cls, par);
        ClassDefinition* cd = GetByName<ClassDefinition>(m_state.Result.Classes, cls);
        if (cd)
        {
            ParameterDefinition* pd = GetByName<ParameterDefinition>(cd->Parameters, par);
            return pd;
        }

        return NULL;
    }

    bool ParseResultFinalizer::IsOfType(const std::string& type, const std::string& ofType) const
    {
        if (type==ofType) //Will handle the case of basic types and enums
            return true;

        //Check object types and handle inheritance.
        ClassDefinition* tmpClass = GetByName<ClassDefinition>(m_state.Result.Classes, type);
        while (tmpClass)
        {
            if (tmpClass->Name==ofType)
                return true;

            tmpClass=GetByName<ClassDefinition>(m_state.Result.Classes, tmpClass->BaseClass);
        }

        return false;
    }

    void ParseResultFinalizer::ResolveReferences(ParseState::ParameterReferenceVector& vec, 
                                                const std::string& refName, 
                                                boost::function<void(ClassMemberDefinition&, int)> setVal)
    {
        //Resolve index refs
        for (ParseState::ParameterReferenceVector::const_iterator it=vec.begin(); it!=vec.end(); ++it)
        {
            ClassDefinition& cls = m_state.Result.Classes[it->ClassIndex];
            ClassMemberDefinition& mem = cls.Members[it->MemberIndex];
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

    void ParseResultFinalizer::ResolveCreateRoutineValues()
    {
        for (ParseState::ParameterReferenceVector::const_iterator it=m_state.CreateRoutineValueReferences.begin(); it!=m_state.CreateRoutineValueReferences.end(); ++it)
        {
            ClassDefinition& cls = m_state.Result.Classes[it->ClassIndex];
            CreateRoutineDefinition& cr = cls.CreateRoutines[it->MemberIndex];
            MemberValue& mv = cr.MemberValues[it->ValueIndex];
            const ParameterDefinition* par = GetParameter(it->ParameterName);
            const ClassMemberDefinition* member=GetByName<ClassMemberDefinition>(cls.Members, mv.first);
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
}
}
}
}
