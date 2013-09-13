/******************************************************************************
*
* Copyright Saab AB, 2004-2013 (http://www.safirsdk.com)
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
#include "ParseAlgorithms.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    //----------------------------------------------
    // Free helper functions
    //----------------------------------------------
    bool ValidName(const std::string& name)
    {
        //Maybe better to use regexp?
        //Valid names must start with a alpha char and the rest must be alphanumeric chars. We also allow underscores.
        std::string::const_iterator it=name.begin();
        if (name.empty() || !std::isalpha(*it, std::locale::classic()))
        {
            return false;
        }

        ++it;
        for (; it!=name.end(); ++it)
        {
            if (!std::isalnum(*it, std::locale::classic()) && (*it)!='_' &&  (*it)!='.')
            {
                //not alphaNum, not underscore, and not an allowed dot.
                return false;
            }
        }

        return true;
    }

    void CheckNameAndFilenameConsistency(const std::string& filename, const std::string name)
    {
        size_t ix=filename.rfind(name);
        if (ix!=filename.length()-name.length()-4)
        {
            throw ParseError("File name missmatch", std::string("The file name does not match the DOB unit name: ") + name + std::string(" - Filename and Dob unit name must match!"), filename, 9);
        }
    }

    //Resolves references on the form <...><name>param</name>123<index></index></...>
    void GetReferencedParameter(boost::property_tree::ptree& pt, std::string& paramName, int& paramIndex)
    {
        paramName=pt.get<std::string>(ElementNames::Instance().String(ElementNames::ReferenceName));
        paramIndex=pt.get(ElementNames::Instance().String(ElementNames::ReferenceIndex), 0);
    }

    std::string GetEntityIdParameterAsString(boost::property_tree::ptree& pt)
    {
        std::string name=pt.get<std::string>(ElementNames::Instance().String(ElementNames::ClassName));
        std::string inst=pt.get<std::string>(ElementNames::Instance().String(ElementNames::InstanceId));
        return  ExpandEnvironmentVariables(name)+std::string(", ")+ExpandEnvironmentVariables(inst);
    }

    std::string ExpandEnvironmentVariables(const std::string& str)
    {
        //Copied from dots_kernel::dots_class_parser
        const size_t start=str.find("$(");
        const size_t stop=str.find(')', start);

        if (start==std::string::npos || stop==std::string::npos)
            return str;

        const std::string var=str.substr(start+2, stop-start-2);

        //Get rid of Microsoft warning
#ifdef _MSC_VER
#pragma warning(disable:4996)
#endif
        const char * const env=getenv(var.c_str());
#ifdef _MSC_VER
#pragma warning(default:4996)
#endif

        if (env == NULL)
        {
            throw ParseError(var, "", "", -1);
        }
        const std::string res=str.substr(0, start) + env + str.substr(stop+1, str.size()-stop-1);
        return ExpandEnvironmentVariables(res); //search for next environment variable
    }

    bool ParseValue(DotsC_MemberType memberType, const std::string& val, ValueDefinition& result)
    {
        try
        {
            switch(memberType)
            {
            case BooleanMemberType:
            {
                if (val=="True" || val=="true")
                    result.boolVal=true;
                else if (val=="False" || val=="false")
                    result.boolVal=false;
                else
                    return false;
            }
                break;

            case Int32MemberType:
            {
                result.int32Val=boost::lexical_cast<DotsC_Int32>(val);
            }
                break;
            case Int64MemberType:
            {
                result.int64Val=boost::lexical_cast<DotsC_Int64>(val);
            }
                break;
            case Float32MemberType:
            {
                result.float32Val=classic_string_cast<DotsC_Float32>(val);
            }
                break;
            case Float64MemberType:
            {
                result.float64Val=classic_string_cast<DotsC_Float64>(val);
            }
                break;
            case EntityIdMemberType:
            {
                result.stringVal=val;
                size_t sep=val.find(", ");
                result.int64Val=DotsId_Generate64(val.substr(0, sep).c_str());
                result.stringVal=val.substr(sep+2);
                try
                {
                    result.hashedVal=boost::lexical_cast<boost::int64_t>(result.stringVal);
                    result.stringVal.clear();
                }
                catch(const boost::bad_lexical_cast&)
                {
                    result.hashedVal=DotsId_Generate64(result.stringVal.c_str());
                }
            }
                break;
            case TypeIdMemberType:
            {
                result.int64Val=DotsId_Generate64(val.c_str());
                result.stringVal=val;
            }
                break;
            case InstanceIdMemberType:
            case ChannelIdMemberType:
            case HandlerIdMemberType:
            {
                try
                {
                    result.hashedVal=boost::lexical_cast<boost::int64_t>(val);
                    result.stringVal.clear();
                }
                catch(const boost::bad_lexical_cast&)
                {
                    result.hashedVal=DotsId_Generate64(val.c_str());
                    result.stringVal=val;
                }
            }
                break;

            case StringMemberType:
            {
                result.stringVal=val;
            }
                break;

            case ObjectMemberType:
                return false; //dont know about object types here
            case EnumerationMemberType:
                return false; //dont know about enum types here
            case BinaryMemberType:
            {
                if (!BasicTypes::Instance().FromBase64(val, result.stringVal))
                {
                    return false;
                }
            }
                break;

            case Ampere32MemberType:
            case CubicMeter32MemberType:
            case Hertz32MemberType:
            case Joule32MemberType:
            case Kelvin32MemberType:
            case Kilogram32MemberType:
            case Meter32MemberType:
            case MeterPerSecond32MemberType:
            case MeterPerSecondSquared32MemberType:
            case Newton32MemberType:
            case Pascal32MemberType:
            case Radian32MemberType:
            case RadianPerSecond32MemberType:
            case RadianPerSecondSquared32MemberType:
            case Second32MemberType:
            case SquareMeter32MemberType:
            case Steradian32MemberType:
            case Volt32MemberType:
            case Watt32MemberType:
            {
                result.float32Val=classic_string_cast<DotsC_Float32>(val);
            }
                break;

            case Ampere64MemberType:
            case CubicMeter64MemberType:
            case Hertz64MemberType:
            case Joule64MemberType:
            case Kelvin64MemberType:
            case Kilogram64MemberType:
            case Meter64MemberType:
            case MeterPerSecond64MemberType:
            case MeterPerSecondSquared64MemberType:
            case Newton64MemberType:
            case Pascal64MemberType:
            case Radian64MemberType:
            case RadianPerSecond64MemberType:
            case RadianPerSecondSquared64MemberType:
            case Second64MemberType:
            case SquareMeter64MemberType:
            case Steradian64MemberType:
            case Volt64MemberType:
            case Watt64MemberType:
            {
                result.float64Val=classic_string_cast<DotsC_Float64>(val);
            }
                break;
            }
        }
        catch (const boost::bad_lexical_cast&)
        {
            return false;
        }

        return true;
    }

    template <class Key, class Val>
    Val* GetPtr(const boost::unordered_map< Key, boost::shared_ptr<Val> >& m, Key key)
    {
        typename boost::unordered_map< Key, boost::shared_ptr<Val> >::const_iterator it=m.find(key);
        if (it!=m.end())
        {
            return it->second.get();
        }
        return NULL;
    }

    template <class T>
    void MergeMaps(const boost::unordered_map<DotsC_TypeId, T>& src, boost::unordered_map<DotsC_TypeId, T>& dest)
    {
        for (typename boost::unordered_map<DotsC_TypeId, T>::const_iterator it=src.begin(); it!=src.end(); ++it)
        {
            if (!dest.insert(*it).second)
            {
                std::ostringstream ss;
                ss<<"The type '"<<it->second->GetName()<<"' is already defined. ";
                throw ParseError("Duplicated type definition", ss.str(), it->second->FileName(), 2);
            }
        }
    }

    template <class K, class V>
    void MergeMapsNoCheck(const boost::unordered_map<K, V>& src, boost::unordered_map<K, V>& dest)
    {
        for (typename boost::unordered_map<K, V>::const_iterator it=src.begin(); it!=src.end(); ++it)
        {
            dest.insert(*it);
        }
    }

    static void SetExceptionBase(ExceptionDescriptionBasic* me, ExceptionDescriptionBasic* base) {me->base=base;}
    static void SetClassBase(ClassDescriptionBasic* me, ClassDescriptionBasic* base)
    {
        me->base=base;
        if (base)
        {
            base->descendants.push_back(me);
        }
    }

    template <class Descr>
    void SetupBaseClass(boost::unordered_map< DotsC_TypeId, boost::shared_ptr<Descr> >& m,
                        std::set<DotsC_TypeId>& endConditions,
                        Descr* d,
                        const boost::function<void(Descr*, Descr*)>& setBase,
                        int& recursionLevel)
    {
        static const int MaxInheritanceLevel=100; //Simple and sufficient way to find circular inheritance. More than 100 levels of inheritance is disallowed.

        if (endConditions.find(d->GetTypeId())!=endConditions.end())
        {
            //this class is an end condition, then we are done
            return;
        }
        else
        {
            DotsC_TypeId baseId=DotsId_Generate64(d->baseClass.c_str());
            Descr* basePtr=GetPtr(m, baseId);
            if (!basePtr)
            {
                throw ParseError("Type doesn't exist", std::string("The class '")+d->baseClass+std::string("' specified as base class, does not exist."), d->FileName(), 21);
            }

            if (++recursionLevel>=MaxInheritanceLevel)
            {
                throw ParseError("Circular inheritance", std::string("The inheritance tree starting from '")+d->GetName()+std::string("' will never end."), d->FileName(), 22);
            }

            SetupBaseClass(m, endConditions, basePtr, setBase, recursionLevel);

            endConditions.insert(d->GetTypeId());
            Descr* base=GetPtr(m, baseId);
            setBase(d, base);
        }
    }

    template <class Descr>
    void SetupMemberTypes(boost::shared_ptr<Descr>& d, const boost::shared_ptr<RepositoryBasic>& repository)
    {
        for (std::vector<MemberDescriptionBasicPtr>::iterator memberIt=d->members.begin(); memberIt!=d->members.end(); ++memberIt)
        {
            //If memberType is set to ObjectType we must check that the type exists. It is also possible
            //that the type actually is an enum, in that case we also have to change the memberType to EnumerationMemberType
            if ((*memberIt)->memberType==ObjectMemberType)
            {
                boost::int64_t id=DotsId_Generate64((*memberIt)->typeName.c_str());
                const ClassDescription* cd=repository->GetClass(id);
                if (cd)
                {
                    (*memberIt)->typeId=id;
                }
                else
                {
                    const EnumDescription* ed=repository->GetEnum(id);
                    if (ed)
                    {
                        (*memberIt)->typeId=id;
                        (*memberIt)->memberType=EnumerationMemberType;
                    }
                    else
                    {
                        std::ostringstream ss;
                        ss<<"The member '"<<(*memberIt)->name<<"' in class/property '"<<d->GetName()<<"' has an invalid type specified. Type: "<<(*memberIt)->typeName;
                        throw ParseError("Invalid type", ss.str(), d->FileName(), 20);
                    }
                }
            }
        }
    }

    //----------------------------------------------
    // RepositoryCompletionAlgorithms
    //----------------------------------------------
    RepositoryCompletionAlgorithms::RepositoryCompletionAlgorithms(boost::shared_ptr<RepositoryBasic>& emptyRepository)
        :m_result(emptyRepository)
    {
    }

    // DOU file completion algorithm
    void RepositoryCompletionAlgorithms::DouParsingCompletion(const std::vector<ParseStatePtr>& states)
    {
        m_result=states.front()->repository;

        //Add predefined types
        ClassDescriptionBasicPtr obj(new ClassDescriptionBasic);
        obj->name=BasicTypes::ObjectName();
        obj->typeId=DotsId_Generate64(obj->name.c_str());
        obj->base=NULL;
        obj->ownSize=OFFSET_HEADER_LENGTH;
        obj->initialSize=OFFSET_HEADER_LENGTH;
        m_result->InsertClass(obj);

        ExceptionDescriptionBasicPtr exc(new ExceptionDescriptionBasic);
        exc->name=BasicTypes::ExceptionName();
        exc->typeId=DotsId_Generate64(exc->name.c_str());
        exc->base=NULL;
        m_result->InsertException(exc);

        ExceptionDescriptionBasicPtr fxc(new ExceptionDescriptionBasic);
        fxc->name=BasicTypes::FundamentalExceptionName();
        fxc->typeId=DotsId_Generate64(fxc->name.c_str());
        fxc->base=NULL;
        m_result->InsertException(fxc);

        //Merge the other repositiories into result
        for (size_t i=1; i<states.size(); ++i)
        {
            MergeMaps(states[i]->repository->m_enums, m_result->m_enums);
            MergeMaps(states[i]->repository->m_exceptions, m_result->m_exceptions);
            MergeMaps(states[i]->repository->m_properties, m_result->m_properties);
            MergeMaps(states[i]->repository->m_classes, m_result->m_classes);
            MergeMapsNoCheck(states[i]->repository->m_parameters, m_result->m_parameters);
        }

        //Enum checksum calculation
        CalculateEnumChecksums();

        //Setup Exception baseclass
        std::set<DotsC_TypeId> endConditions;
        endConditions.insert(DotsId_Generate64(BasicTypes::ExceptionName().c_str()));
        endConditions.insert(DotsId_Generate64(BasicTypes::FundamentalExceptionName().c_str()));
        boost::function<void(ExceptionDescriptionBasic*, ExceptionDescriptionBasic*)> setExeptBaseFun(SetExceptionBase);
        for (boost::unordered_map<DotsC_TypeId, ExceptionDescriptionBasicPtr>::iterator it=m_result->m_exceptions.begin(); it!=m_result->m_exceptions.end(); ++it)
        {
            int recLevel=0;
            SetupBaseClass(m_result->m_exceptions, endConditions, it->second.get(), setExeptBaseFun, recLevel);
        }

        //Property members
        for (boost::unordered_map<DotsC_TypeId, PropertyDescriptionBasicPtr>::iterator it=m_result->m_properties.begin(); it!=m_result->m_properties.end(); ++it)
        {
            SetupMemberTypes(it->second, m_result);
        }

        //Classes - set baseClass, create members, parameters, createRoutines.
        std::vector<ClassDescriptionBasic*> classesWithCreateRoutines;
        classesWithCreateRoutines.reserve(100);
        boost::function<void(ClassDescriptionBasic*, ClassDescriptionBasic*)> setClassBaseFun(SetClassBase);
        endConditions.clear();
        endConditions.insert(DotsId_Generate64(BasicTypes::ObjectName().c_str()));
        for (boost::unordered_map<DotsC_TypeId, ClassDescriptionBasicPtr>::iterator it=m_result->m_classes.begin(); it!=m_result->m_classes.end(); ++it)
        {
            //Set base class
            int recLevel=0;
            SetupBaseClass(m_result->m_classes, endConditions, it->second.get(), setClassBaseFun, recLevel);

            //Create members
            SetupMemberTypes(it->second, m_result);

            if (it->second->createRoutines.size()>0)
            {
                classesWithCreateRoutines.push_back(it->second.get());
            }
        }

        //Resolve parameter to parameter references
        ResolveParamToParamRefs(states);

        //Resolve arraySizeRef and maxLenghtRef. Also handle implicit createRoutine parameters that need some post processing.
        ResolveReferences(states);

        //Class sizes
        for (boost::unordered_map<DotsC_TypeId, ClassDescriptionBasicPtr>::iterator it=m_result->m_classes.begin(); it!=m_result->m_classes.end(); ++it)
        {
            CalculateClassSize(it->second.get());
        }

        //Deserialize xml objects
        DeserializeObjects(states);

        //Verify that parameter with memberType object or enum is referencing valid types.
        VerifyParameterTypes();

        //Create routines - verify types, check duplicates etc.
        std::for_each(classesWithCreateRoutines.begin(), classesWithCreateRoutines.end(), boost::bind(&RepositoryCompletionAlgorithms::HandleCreateRoutines, this, _1));
    }

    void RepositoryCompletionAlgorithms::DeserializeObjects(const std::vector<ParseStatePtr>& states)
    {
        XmlToBlobSerializer<TypeRepository> serializer(m_result.get());

        for (std::vector<ParseStatePtr>::const_iterator stateIt=states.begin(); stateIt!=states.end(); ++stateIt)
        {
            //check object parameters
            for (std::vector<ParseState::ObjectParameter>::const_iterator parIt=(*stateIt)->objectParameters.begin();
                 parIt!=(*stateIt)->objectParameters.end(); ++parIt)
            {
                ParameterDescriptionBasic* param=parIt->referee.referencingItem;
                size_t paramIndex=parIt->referee.referencingIndex;
                ValueDefinition& val=param->MutableValue(paramIndex);

                //Get the correct type name of the serialized object
                const boost::property_tree::ptree& pt=*(parIt->obj);
                boost::optional<std::string> typeAttr=pt.get_optional<std::string>("<xmlattr>.type");
                std::string typeName;
                if (typeAttr)
                {
                    //if type has an explicit type-attribute, check type compliance
                    typeName=*typeAttr;
                    TypeId tid=DotsId_Generate64(typeName.c_str());
                    if (!BasicTypes::Instance().IsOfType(m_result.get(), ObjectMemberType, tid, ObjectMemberType, param->GetTypeId()))
                    {
                        std::ostringstream os;
                        os<<param->GetName()<<" index="<<paramIndex<<" in class "<<parIt->referee.referencingClass->GetName()<<" contains a value of type '"
                         <<typeName<<"' that is not a subtype of the declared type "<<param->typeName;
                        throw ParseError("Type missmatch", os.str(), parIt->referee.referencingClass->FileName(), 105);
                    }
                }
                else
                {
                    //type defaults to dou declaration
                    typeName=param->typeName;
                }

                //do the serialization to the expected type
                try
                {
                    serializer.SerializeObjectContent(typeName, val.binaryVal, pt); //since pt does not include the root element we have to use method SerializeObjectContent
                }
                catch (const ParseError& err)
                {
                    std::ostringstream os;
                    os<<"Failed to deserialize object parameter "<<param->GetName()<<" index="<<paramIndex<<" in class "<<parIt->referee.referencingClass->GetName()
                     <<". "<<err.Description();
                    throw ParseError("Invalid Object", os.str(), parIt->referee.referencingClass->FileName(), err.ErrorId());
                }
            }
        }

    }

    void RepositoryCompletionAlgorithms::ResolveParamToParamRefs(const std::vector<ParseStatePtr>& states)
    {
        typedef ParseState::ParameterReference<ParameterDescriptionBasic> ParamToParamRef;
        typedef std::vector<ParamToParamRef> ParamRefVec;

        const ParamToParamRef* failure=NULL;
        for (int level=0; level<100; ++level) //no more than 100 levels of indirection is permitted
        {
            failure=NULL;

            for (std::vector<ParseStatePtr>::const_iterator stateIt=states.begin(); stateIt!=states.end(); ++stateIt)
            {
                for (ParamRefVec::const_iterator parIt=(*stateIt)->paramToParamReferences.begin();
                     parIt!=(*stateIt)->paramToParamReferences.end(); ++parIt)
                {
                    if (!ResolveParamToParamRef(*parIt))
                    {
                        failure=&(*parIt);
                    }
                }
            }

            if (failure==NULL)
            {
                break;
            }
        }

        if (failure)
        {
            //Error could not resolve reference. Circular reference is probably the reason.
            std::ostringstream os;
            os<<"The parameter "<<failure->referee.referencingItem->GetName()<<" index "<<failure->referee.referencingIndex<<" is part of a circular referencing error.";
            throw ParseError("Parameter reference error", os.str(), failure->referee.referencingClass->FileName(), 50);
        }
    }

    bool RepositoryCompletionAlgorithms::ResolveParamToParamRef(const ParseState::ParameterReference<ParameterDescriptionBasic>& ref)
    {
        ClassDescriptionBasic* cd=ref.referee.referencingClass;
        ParameterDescriptionBasic* referencing=ref.referee.referencingItem;

        if (referencing->values[ref.referee.referencingIndex].referenced!=NULL)
        {
            //this parameter has already been resolved.
            return true;
        }


        ParameterDescriptionBasic* referenced=m_result->GetParameterBasic(ref.parameterName);

        if (!referenced)
        {
            std::ostringstream ss;
            ss<<"Could not resolve Parameter valueRef "<<ref.parameterName<<". Referenced from parameter:  "<<referencing->GetName();
            if (referencing->IsArray())
                ss<<" (index="<<ref.referee.referencingIndex<<") ";
            throw ParseError("Parameter reference error", ss.str(), cd->FileName(), 42);
        }

        if (referenced->GetArraySize()<=static_cast<int>(ref.parameterIndex))
        {
            std::ostringstream ss;
            ss<<"Array index out of range for Parameter valueRef '"<<ref.parameterName<<"' and index="<<ref.parameterIndex<<". Referenced from parameter: "<<referencing->GetName();
            throw ParseError("Parameter index out of bounds", ss.str(), cd->FileName(), 43);
        }

        //Check that the referenced parameter contains a value. If referenced param is also referencing another param, we have to resolve that one first.
        if (referenced->values[ref.parameterIndex].kind==RefKind && referenced->values[ref.parameterIndex].referenced==NULL)
        {
            //unresolved paramToParamRef
            return false;
        }

        if (referenced->memberType==ObjectMemberType || referenced->memberType==EnumerationMemberType)
        {
            //referencing to non-basic types are always set to ObjectMemberType before we know the type
            referencing->memberType=referenced->memberType;
        }

        if (!BasicTypes::Instance().IsOfType(m_result.get(), referenced->memberType, referenced->GetTypeId(),
                      referencing->memberType, referencing->GetTypeId()))
        {
            //referenced parameter cant be derived as the same type as referencing parameter
            std::ostringstream ss;
            ss<<"The parameter '"<<referencing->GetName()<<"' ";
            if (referencing->IsArray())
                ss<<"(index="<<ref.referee.referencingIndex<<") ";
            ss<<"of type "<<referencing->typeName<<" is referencing parameter '"<<referenced->GetName()<<
                " of type "<<referenced->typeName<<". The types are not compatilbe and "<<
                referenced->typeName<<" is not derived from "<<referencing->typeName;
            throw ParseError("Parameter reference error", ss.str(), cd->FileName(), 44);
        }

        //point to referenced
        referencing->values[ref.referee.referencingIndex].referenced=&referenced->values[ref.parameterIndex];

        return true;
    }    

    void RepositoryCompletionAlgorithms::ResolveArraySizeRef(const ParseState::ParameterReference<MemberDescriptionBasic>& ref)
    {
        ClassDescriptionBasic* cd=ref.referee.referencingClass;
        MemberDescriptionBasic* md=ref.referee.referencingItem;
        ParameterDescriptionBasic* referenced=m_result->GetParameterBasic(ref.parameterName);
        if (!referenced)
        {
            std::ostringstream ss;
            ss<<"Could not resolve parameter for arraySizeRef '"<<ref.parameterName<<"'. Referenced from member '"<<md->GetName()<<"' in class "<<cd->GetName();
            throw ParseError("Parameter reference error", ss.str(), cd->FileName(), 49);
        }
        if (referenced->GetArraySize()<=static_cast<int>(ref.parameterIndex))
        {
            std::ostringstream ss;
            ss<<"Array index out of range for Parameter arraySizeRef '"<<ref.parameterName<<"' and index="<<ref.parameterIndex<<". Referenced from memeber '"<<md->GetName()<<"' in class "<<cd->GetName();
            throw ParseError("Parameter index out of bounds", ss.str(), cd->FileName(), 43);
        }
        if (referenced->GetMemberType()!=Int32MemberType)
        {
            std::ostringstream ss;
            ss<<"The parameter referenced for arraySize '"<<ref.parameterName<<"' has type "<<referenced->typeName<<" and not the expected type Int32. Referenced from memeber '"<<md->GetName()<<"' in class "<<cd->GetName();
            throw ParseError("Type missmatch in arraySizeRef", ss.str(), cd->FileName(), 43);
        }

        int size=referenced->GetInt32Value(ref.parameterIndex);
        if (size<=0)
        {
            std::ostringstream ss;
            ss<<"Array size must be greater than 0. The arraySize value specified for member '"<<md->GetName()<<"' is "<<size;
            throw ParseError("Invalid arraySize value", ss.str(), cd->FileName(), 7);

        }

        //Seems to be an ok reference. Copy value for arraySize
        md->arraySize=size;
    }

    void RepositoryCompletionAlgorithms::ResolveMaxLengthRef(const ParseState::ParameterReference<MemberDescriptionBasic>& ref)
    {
        ClassDescriptionBasic* cd=ref.referee.referencingClass;
        MemberDescriptionBasic* md=ref.referee.referencingItem;
        ParameterDescriptionBasic* referenced=m_result->GetParameterBasic(ref.parameterName);
        if (!referenced)
        {
            std::ostringstream ss;
            ss<<"Could not resolve parameter for maxLengthRef '"<<ref.parameterName<<"'. Referenced from memeber '"<<md->GetName()<<"' in class "<<cd->GetName();
            throw ParseError("Parameter reference error", ss.str(), cd->FileName(), 49);
        }
        if (referenced->GetArraySize()<=static_cast<int>(ref.parameterIndex))
        {
            std::ostringstream ss;
            ss<<"Array index out of range for Parameter maxLengthRef '"<<ref.parameterName<<"' and index="<<ref.parameterIndex<<". Referenced from memeber '"<<md->GetName()<<"' in class "<<cd->GetName();
            throw ParseError("Parameter index out of bounds", ss.str(), cd->FileName(), 43);
        }
        if (referenced->GetMemberType()!=Int32MemberType)
        {
            std::ostringstream ss;
            ss<<"The parameter referenced for maxLength '"<<ref.parameterName<<"' has type "<<referenced->typeName<<" and not the expected type Int32. Referenced from memeber '"<<md->GetName()<<"' in class "<<cd->GetName();
            throw ParseError("Type missmatch in maxLengthRef", ss.str(), cd->FileName(), 43);
        }

        int size=referenced->GetInt32Value(ref.parameterIndex);
        if (size<=0)
        {
            std::ostringstream ss;
            ss<<"Max length must be greater than 0. The maxLength value specified for member '"<<md->GetName()<<"' is "<<size;
            throw ParseError("Invalid maxLength value", ss.str(), cd->FileName(), 6);

        }

        //Seems to be an ok reference. Copy value for maxLength
        md->maxLength=size;
    }

    void RepositoryCompletionAlgorithms::ResolveHiddenCreateRoutineParams(const ParseState::ParameterReference<CreateRoutineDescriptionBasic>& ref)
    {
        ClassDescriptionBasic* cd=ref.referee.referencingClass;
        CreateRoutineDescriptionBasic* cr=ref.referee.referencingItem;
        const std::string& memberName=cr->memberValues[ref.referee.referencingIndex].first;
        int memberIndex=cd->GetMemberIndex(memberName);
        if (memberIndex<0)
        {
            //Error member does not exist, will be detected and reported in HandleCreateRoutines
            return;
        }
        const MemberDescriptionBasic* md=static_cast<const MemberDescriptionBasic*>(cd->GetMember(memberIndex));
        //ParameterDescription is a hidden parameter that can't be null, if so its a programming error in dots.
        ParameterDescriptionBasic* pdef=m_result->GetParameterBasic(ref.parameterName);

        pdef->memberType=md->memberType;
        pdef->typeName=md->typeName;
        pdef->typeId=md->typeId;

        if (pdef->memberType!=EnumerationMemberType && pdef->memberType!=ObjectMemberType)
        {
            //Basic types is just stored as strings and must be converted
            std::string paramRawVal=pdef->Value(0).stringVal;
            if (!ParseValue(pdef->memberType, paramRawVal, pdef->MutableValue(0)))
            {
                throw ParseError("Invalid create routine value", "Cant parse value", cd->FileName(), 63);
            }
        }
    }

    void RepositoryCompletionAlgorithms::ResolveReferences(const std::vector<ParseStatePtr>& states)
    {
        for (std::vector<ParseStatePtr>::const_iterator stateIt=states.begin(); stateIt!=states.end(); ++stateIt)
        {
            //array size refs
            std::for_each((*stateIt)->arraySizeReferences.begin(),
                          (*stateIt)->arraySizeReferences.end(),
                          boost::bind(&RepositoryCompletionAlgorithms::ResolveArraySizeRef, this, _1));

            //max length refs
            std::for_each((*stateIt)->maxLengthReferences.begin(),
                          (*stateIt)->maxLengthReferences.end(),
                          boost::bind(&RepositoryCompletionAlgorithms::ResolveMaxLengthRef, this, _1));

            //hidden create routine basic type parameters (not enums or objects)
            std::for_each((*stateIt)->createRoutineIncompleteHiddenParameters.begin(),
                          (*stateIt)->createRoutineIncompleteHiddenParameters.end(),
                          boost::bind(&RepositoryCompletionAlgorithms::ResolveHiddenCreateRoutineParams, this, _1));
        }
    }

    void RepositoryCompletionAlgorithms::HandleCreateRoutines(ClassDescriptionBasic* cd)
    {
        //Verify all createRoutine in-parameters and default values
        for (std::vector<CreateRoutineDescriptionBasicPtr>::iterator crit=cd->createRoutines.begin();
             crit!=cd->createRoutines.end(); ++crit)
        {
            //Check that every parameter has corresponding member
            CreateRoutineDescriptionBasic& cr=*(crit->get());
            for (StringVector::const_iterator inParamIt=cr.parameters.begin(); inParamIt!=cr.parameters.end(); ++inParamIt)
            {
                if (cd->GetMemberIndex(*inParamIt)<0)
                {
                    //Member not found in class
                    std::ostringstream os;
                    os<<"The createRoutine '"<<cr.name<<"' in class "<<cd->GetName()<<"' specifies an invalid parameter '"<<*inParamIt<<"'. The parameter is not a member of the class.";
                    throw ParseError("Invalid CreateRoutine parameter", os.str(), cd->FileName(), 56);
                }
            }

            cr.memberValuesParams.reserve(cr.memberValues.size());

            //check that default values are correct
            for (MemberValueVector::const_iterator mit=cr.memberValues.begin(); mit!=cr.memberValues.end(); ++mit)
            {
                //memberName: mit->first
                //paramName:  mit->second.first;
                //paramIndex: mit->second.second;

                int memberIndex=cd->GetMemberIndex(mit->first);
                if (memberIndex<0)
                {
                    //Member not found in class
                    std::ostringstream os;
                    os<<"The createRoutine '"<<cr.name<<"' in class "<<cd->GetName()<<"' specifies an invalid member '"<<mit->first<<"'. The member does not exist in the class.";
                    throw ParseError("Invalid CreateRoutine value", os.str(), cd->FileName(), 57);
                }
                const ParameterDescriptionBasic* pd=m_result->GetParameterBasic(mit->second.first);
                if (!pd)
                {
                    //Parameter not found
                    std::ostringstream os;
                    os<<"The createRoutine '"<<cr.name<<"' in class "<<cd->GetName()<<"' specifies an invalid parameter as default value for member '"<<
                        mit->first<<"'. The parameter '"<<mit->second.first<<"' does not exist.";
                    throw ParseError("Invalid CreateRoutine value", os.str(), cd->FileName(), 58);
                }
                if (pd->GetArraySize()<=mit->second.second)
                {
                    //Array index out of range
                    std::ostringstream os;
                    os<<"The createRoutine '"<<cr.name<<"' in class "<<cd->GetName()<<"' specifies an parameter index that is out of range for member '"<<
                        mit->first<<"'. Parameter'"<<mit->second.first<<"'' with index="<<mit->second.second;
                    if (!pd->IsArray())
                    {
                        os<<" is not an array.";
                    }
                    else
                    {
                        os<<" - actual array size is "<<pd->GetArraySize();
                    }
                    throw ParseError("Invalid CreateRoutine value", os.str(), cd->FileName(), 59);
                }
                const MemberDescriptionBasic* md=static_cast<const MemberDescriptionBasic*>(cd->GetMember(memberIndex));
                if (!BasicTypes::Instance().IsOfType(m_result.get(), pd->GetMemberType(), pd->GetTypeId(), md->GetMemberType(), md->GetTypeId()))
                {
                    //Type missmatch
                    std::ostringstream os;
                    os<<"The createRoutine '"<<cr.name<<"' in class '"<<cd->GetName()<<"' specifies a value of incorrect type for member '"<<mit->first<<"'. Expected type "<<
                        md->typeName<<" but specified value has type "<<pd->typeName;
                    throw ParseError("Invalid CreateRoutine value", os.str(), cd->FileName(), 60);
                }

                if (md->IsArray())
                {
                    std::ostringstream os;
                    os<<"The createRoutine '"<<cr.name<<"' in class '"<<cd->GetName()<<"' a default value for member '"<<mit->first<<"' wich is an array. Array members can't have default values.";
                    os<<" ..."<<md->GetName()<<" arrSize="<<md->GetArraySize()<<" isArr="<<md->IsArray();
                    throw ParseError("CreateRoutine array values not supported", os.str(), cd->FileName(), 61);
                }

                cr.memberValuesParams.push_back(std::make_pair(pd, mit->second.second));
            }
        }
    }

    void RepositoryCompletionAlgorithms::CalculateEnumChecksums()
    {
        for (boost::unordered_map<DotsC_TypeId, EnumDescriptionBasicPtr>::iterator it=m_result->m_enums.begin();
             it!=m_result->m_enums.end(); ++it)
        {
            EnumDescriptionBasicPtr& ed=it->second;
            std::ostringstream ss;
            ss<<ed->name;
            for (StringVector::const_iterator valIt=ed->enumerationValues.begin(); valIt!=ed->enumerationValues.end(); ++valIt)
            {
                ss<<"."<<*valIt;
            }
            ed->checksum=DotsId_Generate64(ss.str().c_str());
        }
    }

    void RepositoryCompletionAlgorithms::VerifyParameterTypes()
    {
        //loop through all parameters and verify all TypeId, EntityId, and Enum
        for (boost::unordered_map<std::string, ParameterDescriptionBasic*>::iterator parIt=m_result->m_parameters.begin();
             parIt!=m_result->m_parameters.end(); ++parIt)
        {
            ParameterDescriptionBasic* pd=parIt->second;

            if (pd->GetMemberType()==TypeIdMemberType)
            {
                //Verify that typeId parameters contains values that are existing typeIds.
                for (int index=0; index<pd->GetArraySize(); ++index)
                {
                    const ValueDefinition& val=pd->Value(static_cast<size_t>(index));
                    if (!BasicTypes::Instance().ValidTypeId(m_result.get(), val.int64Val))
                    {
                        std::string file=parIt->first.substr(0, parIt->first.rfind(".")+1)+"dou";
                        std::ostringstream os;
                        os<<"The parameter "<<pd->GetName()<<" has an invalid value. '"<<val.stringVal<<"' is not a TypeId of an existing type.";
                        throw ParseError("Invalid TypeId parameter", os.str(), file, 45);
                    }
                }
            }
            else if (pd->GetMemberType()==EntityIdMemberType)
            {
                //Verify that EntityId parameters contains values that are existing classe types.
                for (int index=0; index<pd->GetArraySize(); ++index)
                {
                    const ValueDefinition& val=pd->Value(static_cast<size_t>(index));
                    if (!m_result->GetClass(val.int64Val))
                    {
                        std::string file=parIt->first.substr(0, parIt->first.rfind(".")+1)+"dou";
                        std::ostringstream os;
                        os<<"The parameter "<<pd->GetName()<<" has an invalid value. '"<<val.stringVal<<"' is not a valid EntityId, the typeId does not exist.";
                        throw ParseError("Invalid EntityId parameter", os.str(), file, 46);
                    }
                }
            }
            else if (pd->GetMemberType()==EnumerationMemberType)
            {
                //Verify that enum parameter values are valid according to the specified enum type.
                pd->typeId=DotsId_Generate64(pd->typeName.c_str());
                const EnumDescription* ed=m_result->GetEnum(pd->typeId);
                if (!ed)
                {
                    //Enum type does not exist
                    std::string file=parIt->first.substr(0, parIt->first.rfind(".")+1)+"dou";
                    std::ostringstream os;
                    os<<"The parameter type '"<<pd->typeName<<"' does not exist. Expected to be a basic type or enum type. Specified for parameter "<<pd->GetName();
                    throw ParseError("Invalid type", os.str(), file, 47);
                }

                //Check value
                for (int index=0; index<pd->GetArraySize(); ++index)
                {
                    ValueDefinition& val=pd->MutableValue(static_cast<size_t>(index));
                    val.int32Val=ed->GetIndexOfValue(val.stringVal);
                    if (val.int32Val<0)
                    {
                        std::string file=parIt->first.substr(0, parIt->first.rfind(".")+1)+"dou";
                        std::ostringstream os;
                        os<<"The parameter "<<pd->GetName()<<" has an invalid value '"<<val.stringVal<<"'. Expected to be an enum value of type "<<ed->GetName();
                        throw ParseError("Invalid enum value", os.str(), file, 48);
                    }
                }
            }
        }
    }

    void RepositoryCompletionAlgorithms::CalculateClassSize(ClassDescriptionBasic* cd)
    {
        if (cd->initialSize>0)
        {
            //Already calculated
            return;
        }

        CalculateClassSize(cd->base);

        for (std::vector<MemberDescriptionBasicPtr>::const_iterator memIt=cd->members.begin(); memIt!=cd->members.end(); ++memIt)
        {
            int repeat=(*memIt)->isArray ? (*memIt)->arraySize : 1;
            cd->ownSize+=OFFSET_MEMBER_LENGTH+(MEMBER_STATUS_LENGTH+BasicTypes::Instance().SizeOfType((*memIt)->memberType))*repeat;
        }

        cd->initialSize=cd->ownSize+cd->base->InitialSize();
    }

    // DOM file completion algorithm
    void RepositoryCompletionAlgorithms::DomParsingCompletion(const std::vector<ParseStatePtr>& states)
    {
        for (std::vector<ParseStatePtr>::const_iterator stateIt=states.begin(); stateIt!=states.end(); ++stateIt)
        {
            //Insert propertyMappings and check for duplicates and missing memberMappings
            std::for_each((*stateIt)->notInsertedPropertyMappings.begin(), (*stateIt)->notInsertedPropertyMappings.end(),
                          boost::bind(&RepositoryCompletionAlgorithms::InsertPropertyMapping, this, _1));

            //Insert hidden parameters
            for (std::vector< std::pair<ClassDescriptionBasic*, ParameterDescriptionBasicPtr> >::const_iterator parIt=(*stateIt)->notInsertedParameters.begin();
                 parIt!=(*stateIt)->notInsertedParameters.end(); ++parIt)
            {
                parIt->first->ownParameters.push_back(parIt->second);
                m_result->InsertParameter(parIt->second);
            }
        }


    }

    void RepositoryCompletionAlgorithms::InsertPropertyMapping(const PropertyMappingDescriptionBasicPtr& pm)
    {
        //Check duplicated propertyMappings
        for (std::vector<PropertyMappingDescriptionBasicPtr>::const_iterator it=pm->class_->properties.begin();
             it!=pm->class_->properties.end(); ++it)
        {
            if (pm->property->typeId==(*it)->property->typeId)
            {
                //duplicated mapping
                std::ostringstream ss;
                ss<<"The property '"<<pm->property->GetName()<<"' is mapped more than one time to class '"<<pm->class_->GetName()<<"'";
                throw ParseError("Duplicated propertyMapping", ss.str(), pm->FileName(), 100);
            }
        }

        //Check that all members have been mapped
        size_t numMembers=pm->GetProperty()->GetNumberOfMembers();
        //std::cout<<"PropMembers "<<numMembers<<", mappings="<<pm->memberMappings.size()<<std::endl;
        for (size_t memIx=0; memIx<numMembers; ++memIx)
        {
            if (pm->memberMappings[memIx]==NULL)
            {
                //Member has not been mapped
                std::ostringstream ss;
                ss<<"In propertyMapping between property '"<<pm->property->GetName()<<"' and class '"<<pm->class_->GetName()<<"' there is no mapping for property member '"<< pm->property->GetMember(memIx)->GetName()<<"'.";
                throw ParseError("Member not mapped", ss.str(), pm->FileName(), 101);
            }
        }

        pm->class_->properties.push_back(pm);
    }
}
}
}
}
