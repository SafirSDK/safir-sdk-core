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

    bool IsOfType(const TypeRepository* repository, MemberType mt, TypeId tid, MemberType ofMt, TypeId ofTid)
    {
        if (mt!=ofMt)
        {
            return false;
        }
        else if (mt==EnumerationMemberType)
        {
            return tid==ofTid;
        }
        else if (mt==ObjectMemberType)
        {
            //Check object types and handle inheritance.
            const ClassDescription* tmpClass=repository->GetClass(tid);
            while (tmpClass)
            {
                if (tmpClass->GetTypeId()==ofTid)
                    return true;

                tmpClass=tmpClass->GetBaseClass();
            }
            return false;
        }
        else //Basic types
        {
            return true;
        }
    }

    bool ValidTypeId(const TypeRepository* repository, TypeId tid)
    {
        return repository->GetClass(tid)!=NULL ||
                repository->GetEnum(tid)!=NULL ||
                repository->GetProperty(tid)!=NULL ||
                repository->GetException(tid)!=NULL;
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
        obj->name=BasicTypes::ObjectName;
        obj->typeId=DotsId_Generate64(obj->name.c_str());
        obj->base=NULL;
        obj->ownSize=OFFSET_HEADER_LENGTH;
        obj->initialSize=OFFSET_HEADER_LENGTH;
        m_result->InsertClass(obj);

        ExceptionDescriptionBasicPtr exc(new ExceptionDescriptionBasic);
        exc->name=BasicTypes::ExceptionName;
        exc->typeId=DotsId_Generate64(exc->name.c_str());
        exc->base=NULL;
        m_result->InsertException(exc);

        ExceptionDescriptionBasicPtr fxc(new ExceptionDescriptionBasic);
        fxc->name=BasicTypes::FundamentalExceptionName;
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
        endConditions.insert(DotsId_Generate64(BasicTypes::ExceptionName.c_str()));
        endConditions.insert(DotsId_Generate64(BasicTypes::FundamentalExceptionName.c_str()));
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
        endConditions.insert(DotsId_Generate64(BasicTypes::ObjectName.c_str()));
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

        //Verify that parameter with memberType object or enum is referencing valid types.
        VerifyParameterTypes();

        //Create routines
        std::for_each(classesWithCreateRoutines.begin(), classesWithCreateRoutines.end(), boost::bind(&RepositoryCompletionAlgorithms::HandleCreateRoutines, this, _1));

        //Class sizes
        for (boost::unordered_map<DotsC_TypeId, ClassDescriptionBasicPtr>::iterator it=m_result->m_classes.begin(); it!=m_result->m_classes.end(); ++it)
        {
            CalculateClassSize(it->second.get());
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

        if (referencing->values[ref.referee.referencingIndex].kind!=RefKind)
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
        if (referenced->values[ref.parameterIndex].kind==RefKind)
        {
            //unresolved paramToParamRef
            return false;
        }

        if (referenced->memberType==ObjectMemberType || referenced->memberType==EnumerationMemberType)
        {
            //referencing to non-basic types are always set to ObjectMemberType before we know the type
            referencing->memberType=referenced->memberType;
        }

        if (!IsOfType(m_result.get(), referenced->memberType, referenced->GetTypeId(),
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

        //Seems to be an ok reference. To make it simple we just copy the value instead of dealing with even more pointers.
        referencing->values[ref.referee.referencingIndex]=referenced->values[ref.parameterIndex];

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
            ss<<"Could not resolve parameter for arraySizeRef '"<<ref.parameterName<<"'. Referenced from memeber '"<<md->GetName()<<"' in class "<<cd->GetName();
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

        int size=referenced->values[ref.parameterIndex].int32Val;
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

        int size=referenced->values[ref.parameterIndex].int32Val;
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

        ENSURE(pdef->memberType==Int32MemberType, <<"Hidden parameter is corrupt: "<<pdef->name)

        pdef->memberType=md->memberType;
        pdef->typeName=md->typeName;
        pdef->typeId=md->typeId;
        std::string paramRawVal=pdef->values[0].stringVal;
        if (!BasicTypes::Instance().ParseValue(pdef->memberType, paramRawVal, pdef->values[0]))
        {
            throw ParseError("Invalid create routine value", "Cant parse value", cd->FileName(), 63);
        }
    }

    void RepositoryCompletionAlgorithms::ResolveReferences(const std::vector<ParseStatePtr>& states)
    {
        for (std::vector<ParseStatePtr>::const_iterator stateIt=states.begin(); stateIt!=states.end(); ++stateIt)
        {
            //check object parameters
//            for (std::vector<ParseState::ObjectParameterTypeCheck>::const_iterator parIt=(*stateIt)->objectParameterTypeChecks.begin();
//                 parIt!=(*stateIt)->objectParameterTypeChecks.end(); ++parIt)
//            {

//                //std::ostringstream ss, val;
//                //boost::property_tree::write_xml(ss, *parIt->obj);
//                //                val << "<object>"  //add start element <object>
//                //                    << ss.str().substr(ss.str().find_first_of('>')+1)  //remove <? xml version... ?>
//                //                    << "</" << ElementNames::Instance().String(ElementNames::ParameterObject)<< ">"; //add end element </object>
//                //std::cout<<"<object>"<<val.str()<<"</object>" <<std::endl;


//            }

            //array size refs
            std::for_each((*stateIt)->arraySizeReferences.begin(),
                          (*stateIt)->arraySizeReferences.end(),
                          boost::bind(&RepositoryCompletionAlgorithms::ResolveArraySizeRef, this, _1));

            //max length refs
            std::for_each((*stateIt)->maxLengthReferences.begin(),
                          (*stateIt)->maxLengthReferences.end(),
                          boost::bind(&RepositoryCompletionAlgorithms::ResolveMaxLengthRef, this, _1));

            //create routine hidden parameters
            std::for_each((*stateIt)->createRoutineIncompleteHiddenParameters.begin(),
                          (*stateIt)->createRoutineIncompleteHiddenParameters.end(),
                          boost::bind(&RepositoryCompletionAlgorithms::ResolveHiddenCreateRoutineParams, this, _1));
        }
    }

    void RepositoryCompletionAlgorithms::HandleCreateRoutines(ClassDescriptionBasic* cd)
    {
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
                if (!IsOfType(m_result.get(), pd->GetMemberType(), pd->GetTypeId(), md->GetMemberType(), md->GetTypeId()))
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
                for (ParameterValues::const_iterator valIt=pd->values.begin(); valIt!=pd->values.end(); ++valIt)
                {
                    if (!ValidTypeId(m_result.get(), valIt->int64Val))
                    {
                        std::string file=parIt->first.substr(0, parIt->first.rfind(".")+1)+"dou";
                        std::ostringstream os;
                        os<<"The parameter "<<pd->GetName()<<" has an invalid value. '"<<valIt->stringVal<<"' is not a TypeId of an existing type.";
                        throw ParseError("Invalid TypeId parameter", os.str(), file, 45);
                    }
                }
            }
            else if (pd->GetMemberType()==EntityIdMemberType)
            {
                //Verify that EntityId parameters contains values that are existing classe types.
                for (ParameterValues::const_iterator valIt=pd->values.begin(); valIt!=pd->values.end(); ++valIt)
                {
                    if (!m_result->GetClass(valIt->int64Val))
                    {
                        std::string file=parIt->first.substr(0, parIt->first.rfind(".")+1)+"dou";
                        std::ostringstream os;
                        os<<"The parameter "<<pd->GetName()<<" has an invalid value. '"<<valIt->stringVal<<"' is not a valid EntityId, the typeId does not exist.";
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
                for (ParameterValues::iterator valIt=pd->values.begin(); valIt!=pd->values.end(); ++valIt)
                {
                    valIt->int32Val=ed->GetIndexOfValue(valIt->stringVal);
                    if (valIt->int32Val<0)
                    {
                        std::string file=parIt->first.substr(0, parIt->first.rfind(".")+1)+"dou";
                        std::ostringstream os;
                        os<<"The parameter "<<pd->GetName()<<" has an invalid value '"<<valIt->stringVal<<"'. Expected to be an enum value of type "<<ed->GetName();
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


//#ifndef PROFILE_ENABLED
////--- Uncomment next line to enable Profiling to std:out ---
////#define PROFILE_ENABLED
//#endif
//#ifdef PROFILE_ENABLED
//std::map<std::string, double> g_time;
//struct prof
//{
//    boost::timer m_timer;
//    const char* m_fun;
//    prof(const char* fun) : m_fun(fun) {}
//    ~prof(){g_time[m_fun]+=m_timer.elapsed();}
//};
//struct timeCmp{ bool operator()(const std::pair<std::string, double>& l, const std::pair<std::string, double>& r){return l.second>r.second;}};
//#define PROFILE prof _pr(__FUNCTION__);
//#else
//#define PROFILE
//#endif

//namespace Safir
//{
//namespace Dob
//{
//namespace Typesystem
//{
//namespace Internal
//{
////    void ParseResultFinalizer::ProcessDouResults()
////    {
////        PROFILE

////        ResolveParameterToParameterRefs();
////        ResolveReferences(m_state.arraySizeReferences, "arraySizeRef", SetarraySize);
////        ResolveReferences(m_state.maxLengthReferences, "maxLengthRef", SetmaxLength);

////        std::for_each(m_state.result->enumerations.begin(), m_state.result->enumerations.end(), boost::bind(&ParseResultFinalizer::ProcessEnum, this, _1)); //ProcessEnums
////        std::for_each(m_state.result->exceptions.begin(), m_state.result->exceptions.end(), boost::bind(&ParseResultFinalizer::ProcessException, this, _1)); //Processexceptions
////        std::for_each(m_state.result->properties.begin(), m_state.result->properties.end(), boost::bind(&ParseResultFinalizer::ProcessProperty, this, _1)); //Processproperties
////        std::for_each(m_state.result->classes.begin(), m_state.result->classes.end(), boost::bind(&ParseResultFinalizer::ProcessClass, this, _1)); //Processclasses

////        //Finally handle all parameters. We do this part last so we are sure all type definitions are in place.
////        for (ClassDefinitions::iterator it=m_state.result->classes.begin(); it!=m_state.result->classes.end(); ++it)
////        {
////            std::for_each(it->parameters.begin(), it->parameters.end(), boost::bind(&ParseResultFinalizer::ProcessParameter, this, *it, _1));
////        }
////    }

////    void ParseResultFinalizer::ProcessDomResults()
////    {
////        PROFILE

////        std::for_each(m_state.result->propertyMappings.begin(), m_state.result->propertyMappings.end(), boost::bind(&ParseResultFinalizer::ProcessPropertyMapping, this, _1)); //ProcessPropertyMapping

////#ifdef PROFILE_ENABLED
////        g_time[_pr.m_fun]+=_pr.m_timer.elapsed();

////        std::vector<std::pair<std::string, double>> myvec(g_time.begin(), g_time.end());
////        std::sort(myvec.begin(), myvec.end(), timeCmp());

////        std::cout<<std::endl<<"------ Profiling ----------"<<std::endl;
////        for (std::vector<std::pair<std::string, double>>::const_iterator it=myvec.begin(); it!=myvec.end(); ++it)
////        {
////            std::cout<<it->first<<": "<<it->second<<std::endl;
////        }
////        std::cout<<std::endl<<"----------------------------"<<std::endl;
////#endif

////    }


////    void ParseResultFinalizer::ProcessClass(ClassDefinition& c)
////    {

////        //Handle createRoutines
////        std::for_each(c.createRoutines.begin(), c.createRoutines.end(), boost::bind(&ParseResultFinalizer::ProcessCreateRoutine, this, c, _1));


////        //Handle Parameters
////        //std::for_each(c.parameters.begin(), c.parameters.end(), boost::bind(&ParseResultFinalizer::ProcessParameter, this, c, _1));
////    }

////    void ParseResultFinalizer::ProcessCreateRoutine(ClassDefinition& host, CreateRoutineDefinition& c)
////    {
////        PROFILE

////        if (!ValidName(c.name, false))
////        {
////            std::ostringstream ss;
////            ss<<"The create routine name '"<<c.name<<"' in class '"<<host.name<<"' is illegal.";
////            throw ParseError("Illegal name", ss.str(), host.fileName);
////        }

////        //Check that all create routine parameters are existing members in the class
////        for (StringVector::const_iterator member=c.parameters.begin(); member!=c.parameters.end(); ++member)
////        {
////            const MemberDefinition* found=m_state.GetMember(&host, *member);
////            if (found==NULL)
////            {
////                //the create routine parameter was not found in class members
////                std::ostringstream ss;
////                ss<<"The create routine '"<<c.name<<"' in class '"<<host.name<<"' has specified an invalid member: "<<(*member);
////                throw ParseError("Illegal create routine member", ss.str(), host.fileName);
////            }

////            //Check for create routine parameter duplicates
////            if (std::count(c.parameters.begin(), c.parameters.end(), *member)>1)
////            {
////                std::ostringstream ss;
////                ss<<"The create routine '"<<c.name<<"' in class '"<<host.name<<"' has specified member '"<<(*member)<<"' more than one time.";
////                throw ParseError("Duplicated create routine member", ss.str(), host.fileName);
////            }
////        }

////        //Check that all member values are ok
////        for (MemberValueVector::const_iterator memberValue=c.memberValues.begin(); memberValue!=c.memberValues.end(); ++memberValue)
////        {
////            const MemberDefinition* classMember=m_state.GetMember(&host, memberValue->first);
////            if (classMember==NULL)
////            {
////                //the create routine parameter was not found in class members
////                std::ostringstream ss;
////                ss<<"The create routine '"<<c.name<<"' in class '"<<host.name<<"' has specified default value for an invalid member: "<<memberValue->first;
////                throw ParseError("Illegal create routine member", ss.str(), host.fileName);
////            }

////            const std::string& paramName=memberValue->second.first;
////            int paramIndex=memberValue->second.second;
////            ParameterDefinition* param=m_state.GetParameter(paramName);

////            //Check that parameter exists
////            if (!param)
////            {
////                std::ostringstream ss;
////                ss<<"Could not resolve CreateRoutine value. Referenced parameter "<<paramName<<" not found. Referenced from createRoutine  "<<c.name<<" in class "<<host.name;
////                throw ParseError("Parameter reference error", ss.str(), host.fileName);
////            }

////            if (paramIndex>=static_cast<int>(param->values.size()))
////            {
////                std::ostringstream ss;
////                ss<<"CreateRoutine value is referencing out of bounds. Referenced parameter "<<paramName<<" index="<<paramIndex<<". Referenced from createRoutine  "<<c.name<<" in class "<<host.name;
////                throw ParseError("Array index out of bounds", ss.str(), host.fileName);
////            }

////            if (!IsOfType(param->typeName, classMember->typeName))
////            {
////                std::ostringstream ss;
////                ss<<"Referenced parameter '"<<paramName<<"' is not of the expected type. Type is '"<<param->typeName<<"' and expected type is '"<<classMember->typeName<<"'. Referenced from createRoutine  "<<c.name<<" in class "<<host.name;
////                throw ParseError("Type missmatch in createRoutine value", ss.str(), host.fileName);
////            }

////           //TODO: check for duplicated membervalues. Check for membervalues that already are present as parameters. Add indexer to init array values.
////        }
////    }


////    void ParseResultFinalizer::ProcessParameter(ClassDefinition& host, ParameterDefinition& p)
////    {
////        PROFILE

////        if (!p.hidden)
////        {
////            if (!ValidName(p.name, false))
////            {
////                std::ostringstream ss;
////                ss<<"The parameter name '"<<p.name<<"' in class '"<<host.name<<"' is illegal.";
////                throw ParseError("Illegal name", ss.str(), host.fileName);
////            }
////        }

////        if (!BasicTypes::Instance().MemberTypeOf(p.typeName, m_state.result, p.memberType))
////        {
////            std::ostringstream ss;
////            ss<<"The parameter '"<<p.name<<"' in class '"<<host.name<<"' has an invalid type specified. Type: "<<p.typeName;
////            throw ParseError("Invalid type", ss.str(), host.fileName);
////        }

////        for (ParameterValues::iterator valIt=p.values.begin(); valIt!=p.values.end(); ++valIt)
////        {
////            try
////            {
////                valIt->stringVal=ExpandEnvironmentVariables(valIt->stringVal);
////            }
////            catch (const ParseError& err)
////            {
////                std::ostringstream ss;
////                ss<<"Failed to expand the environment variable '"<<err.Label()<<"'.";
////                throw ParseError("Environment variable expansion error", ss.str(), host.fileName);
////            }

////            if (!BasicTypes::Instance().CanParseValue(p.typeName, valIt->stringVal, m_state.result))
////            {
////                std::ostringstream ss;
////                ss<<"The parameter '"<<p.name<<"' with value '"<<valIt->stringVal<<"' cannot be converted to the expected type "<<p.typeName;
////                throw ParseError("Failed to interpret parameter value", ss.str(), host.fileName);
////            }
////        }
////    }

////    void ParseResultFinalizer::ProcessPropertyMapping(PropertyMappingDefinition& p)
////    {
////        PROFILE

////        CheckNameAndFilenameConsistency(p.fileName, p.className+std::string("-")+p.propertyName);

////        //Check for duplicated property mappings
////        int count=0;
////        for (PropertyMappingDefinitions::const_iterator it=m_state.result->propertyMappings.begin(); it!=m_state.result->propertyMappings.end(); ++it)
////        {
////            if (it->className==p.className && it->propertyName==p.propertyName)
////                ++count;
////        }
////        if (count>1)
////        {
////            throw ParseError("Duplicated property mapping definition", "The PropertyMapping is defined more than one time.", p.fileName);
////        }

////        //Verify that property exists
////        PropertyDefinition* prop=m_state.GetProperty(p.propertyName);
////        if (!prop)
////        {
////            std::ostringstream ss;
////            ss<<"The property '"<<p.propertyName<<"' does not exist. Referenced from property mapping dom-file.";
////            throw ParseError("Property does not exist", ss.str(), p.fileName);
////        }

////        //Verify that class exists
////        ClassDefinition* cls=m_state.GetClass(p.className);
////        if (!cls)
////        {
////            std::ostringstream ss;
////            ss<<"The class '"<<p.className<<"' does not exist. Referenced from property mapping dom-file.";
////            throw ParseError("Class does not exist", ss.str(), p.fileName);
////        }

////        //Check that all propertyMembers have been mapped exactly one time
////        for (MemberDefinitions::const_iterator it=prop->members.begin(); it!=prop->members.end(); ++it)
////        {
////            size_t count=std::count_if(p.mappedMembers.begin(), p.mappedMembers.end(), boost::bind(NameComparer<MappedMemberDefinition>, _1, it->name));
////            if (count>1)
////            {
////                std::ostringstream ss;
////                ss<<"The property member '"<<it->name<<"' in property '"<<prop->name<<"' is defined more than one time in property mapping.";
////                throw ParseError("Duplicated property member mapping", ss.str(), p.fileName);
////            }
////            else if (count==0)
////            {
////                std::ostringstream ss;
////                ss<<"The property member '"<<it->name<<"' in property '"<<prop->name<<"' is not mapped in property mapping.";
////                throw ParseError("Missing property member in property mapping", ss.str(), p.fileName);
////            }

////        }

////        //Handle all mapped members
////        for (MappedMemberDefinitions::iterator mappedMemberIt=p.mappedMembers.begin(); mappedMemberIt!=p.mappedMembers.end(); ++mappedMemberIt)
////        {
////            if (mappedMemberIt->kind==MappedToParameter) //If the propertyMember is mapped to a value, check that the value can be parsed as the expected type.
////            {
////                ProcessPropertyMappedToParameter(p, *prop, *cls, *mappedMemberIt);
////            }
////            else if (mappedMemberIt->kind==MappedToMember) //Mapped to a class member. Verify that member exists, arrayIndices not out of bounds, type compliance.
////            {
////                ProcessPropertyMappedToClassMember(p, *prop, *cls, *mappedMemberIt);
////            }
////        }
////    }

////    void ParseResultFinalizer::ProcessPropertyMappedToParameter(PropertyMappingDefinition& mapping,
////                                                                PropertyDefinition& property,
////                                                                ClassDefinition& cls,
////                                                                MappedMemberDefinition& member)
////    {
////        MemberDefinitions::const_iterator propMemberIt=std::find_if(property.members.begin(), property.members.end(), boost::bind(NameComparer<MemberDefinition>, _1, member.name));
////        if (propMemberIt==property.members.end())
////        {
////            std::ostringstream ss;
////            ss<<"The property '"<<property.name<<"' does not define the member '"<<member.name<<"' that has been mapped in property mapping.";
////            throw ParseError("Property member does not exist", ss.str(), mapping.fileName);
////        }

////        const std::string& paramName=member.memberReferences[0].first;
////        int paramIndex=member.memberReferences[0].second;
////        ParameterDefinition* param=m_state.GetParameter(paramName);

////        //Check that parameter exists
////        if (!param)
////        {
////            std::ostringstream ss;
////            ss<<"Could not resolve PropertyMapping valueRef "<<paramName<<". Referenced from property mapping member:  "<<member.name;
////            throw ParseError("Parameter reference error", ss.str(), mapping.fileName);
////        }

////        //If this is a hidden parameter, then the type information is still missing
////        if (param->hidden)
////        {
////            param->memberType=propMemberIt->memberType;
////            param->typeName=propMemberIt->typeName;
////            try
////            {
////                ProcessParameter(cls, *param);
////            }
////            catch(const ParseError& err)
////            {
////                std::ostringstream ss;
////                ss<<"Error occured while parsing PropertyMapping value for member"<<member.name<<". Accitional info: "<<err.Description();
////                throw ParseError("PropertyMapping value error", ss.str(), mapping.fileName);
////            }
////        }

////        if (propMemberIt->isArray!=param->isArray)
////        {
////            std::ostringstream ss;
////            ss<<"The member '"<<member.name<<"' has been mapped to a value that does not match the corresponding property member in terms of isArray. A single value must be mapped to a single value, and an array to an array.";
////            throw ParseError("PropertyMapping array missmatch", ss.str(), mapping.fileName);
////        }

////        if (!propMemberIt->isArray && paramIndex>=0)
////        {
////            std::ostringstream ss;
////            ss<<"The property member '"<<member.name<<"' is not an array but an array index is specified";
////            throw ParseError("Illegal index element", ss.str(), mapping.fileName);
////        }

////        if (propMemberIt->isArray && paramIndex>=static_cast<int>(param->values.size()))
////        {
////            std::ostringstream ss;
////            ss<<"The member '"<<member.name<<"' indexed out of bounds.";
////            throw ParseError("Array index out of bounds", ss.str(), mapping.fileName);
////        }

////        if (!IsOfType(param->typeName, propMemberIt->typeName))
////        {
////            std::ostringstream ss;
////            ss<<"The mapped member '"<<member.name<<"' is not of the expected type. Type is '"<<param->typeName<<"' and expected type is '"<<propMemberIt->typeName<<"'";
////            throw ParseError("Type missmatch in propertyMapping", ss.str(), mapping.fileName);
////        }
////    }

////    void ParseResultFinalizer::ProcessPropertyMappedToClassMember(PropertyMappingDefinition& mapping,
////                                                                  PropertyDefinition& property,
////                                                                  ClassDefinition& cls,
////                                                                  MappedMemberDefinition& member)
////    {
////        MemberDefinitions::const_iterator propMemberIt=std::find_if(property.members.begin(), property.members.end(), boost::bind(NameComparer<MemberDefinition>, _1, member.name));
////        if (propMemberIt==property.members.end())
////        {
////            std::ostringstream ss;
////            ss<<"The property '"<<mapping.propertyName<<"' does not define the member '"<<member.name<<"' that has been mapped in property mapping.";
////            throw ParseError("Property member does not exist", ss.str(), mapping.fileName);
////        }

////        ClassDefinition* referencedClass=&cls;
////        MemberDefinition* referencedMember=NULL;
////        for (size_t memRefIndex=0; memRefIndex<member.memberReferences.size()-1; ++memRefIndex)
////        {
////            const MemberReference& memberRef=member.memberReferences[memRefIndex];
////            const std::string& memberName=memberRef.first;
////            const int& arrayIndex=memberRef.second;
////            referencedMember=m_state.GetMember(referencedClass, memberName);
////            if (referencedMember==NULL)
////            {
////                std::ostringstream ss;
////                ss<<"The mapping for property member '"<<member.name<<"' specifies illegal classMemberReference. Member '"<<memberName<<"' does not exist in class '"<<referencedClass->name;
////                throw ParseError("Member does not exist", ss.str(), mapping.fileName);
////            }
////            else if (referencedMember->isArray && arrayIndex<0)
////            {
////                std::ostringstream ss;
////                ss<<"The property member '"<<member.name<<"' specifies illegal classMemberReference. Member '"<<memberName<<"' is an array but no array index is specified";
////                throw ParseError("Array index missing", ss.str(), mapping.fileName);

////            }
////            else if (referencedMember->isArray==false && arrayIndex>=0)
////            {
////                std::ostringstream ss;
////                ss<<"The mapping of property member '"<<member.name<<"' specifies illegal classMemberReference. Member '"<<memberName<<"' is not an array but an array index is specified";
////                throw ParseError("Illegal index element", ss.str(), mapping.fileName);
////            }

////            referencedClass=m_state.GetClass(referencedMember->typeName);
////        }

////        const MemberReference& memberRef=member.memberReferences[member.memberReferences.size()-1];
////        referencedMember=m_state.GetMember(referencedClass, memberRef.first);
////        if (referencedMember==NULL)
////        {
////            std::ostringstream ss;
////            ss<<"The mapping for property member '"<<member.name<<"' specifies illegal classMemberReference. Member '"<<memberRef.first<<"' does not exist in class '"<<referencedClass->name;
////            throw ParseError("Member does not exist", ss.str(), mapping.fileName);
////        }
////        else if (!IsOfType(referencedMember->typeName, propMemberIt->typeName))
////        {
////            std::ostringstream ss;
////            ss<<"The member '"<<referencedMember->name<<"' mapped to the property member '"<<member.name<<"' is not of the expected type. Type is '"<<referencedMember->typeName<<"' and expected type is '"<<propMemberIt->typeName<<"'";
////            throw ParseError("Type missmatch in propertyMapping", ss.str(), mapping.fileName);
////        }
////        else if (referencedMember->isArray && !propMemberIt->isArray)
////        {
////            std::ostringstream ss;
////            ss<<"The member '"<<referencedMember->name<<"' is an array but propertyMember '"<<member.name<<"' is not";
////            throw ParseError("Property member is not an array", ss.str(), mapping.fileName);
////        }
////        else if (!referencedMember->isArray && propMemberIt->isArray)
////        {
////            std::ostringstream ss;
////            ss<<"The member '"<<referencedMember->name<<"' is not an array but propertyMember '"<<member.name<<"' is";
////            throw ParseError("Property member is an array", ss.str(), mapping.fileName);
////        }
////        else if (propMemberIt->isArray && referencedMember->arraySize<=memberRef.second)
////        {
////            std::ostringstream ss;
////            ss<<"The member '"<<referencedMember->name<<"' indexed out of bounds.";
////            throw ParseError("Array index out of bounds", ss.str(), mapping.fileName);
////        }
////        else if (!referencedMember->isArray && memberRef.second>=0)
////        {
////            std::ostringstream ss;
////            ss<<"The mapping of property member '"<<member.name<<"' specifies illegal classMemberReference. Member '"<<referencedMember->name<<"' is not an array but an array index is specified";
////            throw ParseError("Illegal index element", ss.str(), mapping.fileName);
////        }
////    }

////    bool ParseResultFinalizer::IsOfType(const std::string& type, const std::string& ofType) const
////    {
////        PROFILE

////        if (type==ofType) //Will handle the case of basic types and enums
////            return true;

////        //Check object types and handle inheritance.
////        ClassDefinition* tmpClass=m_state.GetClass(type);
////        while (tmpClass)
////        {
////            if (tmpClass->name==ofType)
////                return true;

////            tmpClass=m_state.GetClass(tmpClass->baseClass);
////        }

////        return false;
////    }

////    void ParseResultFinalizer::ResolveReferences(ParseState::ParameterReferenceVector& vec,
////                                                const std::string& refName,
////                                                boost::function<void(MemberDefinition&, int)> setVal)
////    {
////        PROFILE

////        //Resolve index refs
////        for (ParseState::ParameterReferenceVector::const_iterator it=vec.begin(); it!=vec.end(); ++it)
////        {
////            ClassDefinition& cls=m_state.result->classes[it->topIndex];
////            MemberDefinition& mem=cls.members[it->subIndex1];
////            const ParameterDefinition* par=m_state.GetParameter(it->parameterName);
////            if (!par)
////            {
////                std::ostringstream ss;
////                ss<<"Could not resolve "<<refName<<" parameter "<<it->parameterName<<". Referenced from: "<<cls.name<<"."<<mem.name;
////                throw ParseError("Parameter reference error", std::string("Could not resolve "+refName+" parameter ")+it->parameterName, it->fileName);
////            }

////            if (par->values.size()<=static_cast<size_t>(it->parameterIndex))
////            {
////                std::ostringstream ss;
////                ss<<"Array index out of range for "<<refName<<" parameter '"<<it->parameterName<<"' and index="<<it->parameterIndex<<". Referenced from: "<<cls.name<<"."<<mem.name;
////                throw ParseError("Parameter reference error", ss.str(), it->fileName);
////            }

////            if (!IsOfType(par->typeName, "Int32"))
////            {
////                std::ostringstream ss;
////                ss<<"The type of the referenced parameter '"<<it->parameterName<<"' is not of the expected type. Referenced from: "<<cls.name<<"."<<mem.name;
////                throw ParseError("Type missmatch", ss.str(), it->fileName);
////            }

////            setVal(mem, boost::lexical_cast<int, std::string>(par->values[static_cast<size_t>(it->parameterIndex)].stringVal));
////        }
////    }

////    void ParseResultFinalizer::ResolveParameterToParameterRefs()
////    {
////        PROFILE

////        for (ParseState::ParameterReferenceVector::const_iterator it=m_state.paramToParamReferences.begin(); it!=m_state.paramToParamReferences.end(); ++it)
////        {
////            ParameterDefinition& referencingPar=m_state.result->classes[it->topIndex].parameters[it->subIndex1];
////            const ParameterDefinition* par=m_state.GetParameter(it->parameterName);

////            if (!par)
////            {
////                std::ostringstream ss;
////                ss<<"Could not resolve Parameter valueRef "<<it->parameterName<<". Referenced from parameter:  "<<referencingPar.name;
////                throw ParseError("Parameter reference error", ss.str(), it->fileName);
////            }

////            if (par->values.size()<=static_cast<size_t>(it->parameterIndex))
////            {
////                std::ostringstream ss;
////                ss<<"Array index out of range for Parameter valueRef '"<<it->parameterName<<"' and index="<<it->parameterIndex<<". Referenced from parameter:  "<<referencingPar.name;
////                throw ParseError("Parameter reference error", ss.str(), it->fileName);
////            }

////            if (!IsOfType(par->typeName, referencingPar.typeName))
////            {
////                std::ostringstream ss;
////                ss<<"The type of the referenced parameter '"<<it->parameterName<<"' is not of the expected type. Referenced from parameter:  "<<referencingPar.name;
////                throw ParseError("Type missmatch", ss.str(), it->fileName);
////            }

////            //TODO: Find circular references.

////            referencingPar.values[it->subIndex2]=par->values[it->parameterIndex];
////        }
////    }
//}
//}
//}
//}
