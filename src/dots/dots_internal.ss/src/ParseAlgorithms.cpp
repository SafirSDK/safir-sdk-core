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
#include "ParseAlgorithms.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace ToolSupport
{
    //----------------------------------------------
    // Free helper functions
    //----------------------------------------------
    bool ValidName(const std::string& name)
    {
        if (name.empty())
        {
            return false;
        }

        bool firstLetter=true;
        for (std::string::const_iterator it=name.begin(); it!=name.end(); ++it)
        {
            if (firstLetter)
            {
                if (!std::isalpha(*it, std::locale::classic()))
                {
                    return false;
                }
            }
            else if (!std::isalnum(*it, std::locale::classic()) && (*it)!='_' &&  (*it)!='.')
            {
                return false;
            }

            firstLetter=(*it)=='.';
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

    //Resolves references on the form <...><name>foo</name><index>10</index></...> or <...><name>foo</name><key>bar</key></...>
    void GetReferencedParameter(boost::property_tree::ptree& pt, std::string& paramName, std::string& paramKey)
    {
        paramName=pt.get<std::string>(Elements::ReferenceName::Name());
        SerializationUtils::Trim(paramName);
        boost::optional<std::string> key=pt.get_optional<std::string>(Elements::ReferenceKey::Name());
        boost::optional<std::string> index=pt.get_optional<std::string>(Elements::ReferenceIndex::Name());

        //both key and index can't exist
        if (key && index) throw std::string("Cant have both key and index element in reference");

        if (key)
        {
            if (key->empty())
            {
                key=GetEntityIdParameterAsString(pt.get_child(Elements::ReferenceKey::Name()));
            }
            else
            {
                paramKey=*key;
            }
        }
        else if (index)
        {
            paramKey=*index;
        }
        else
        {
            paramKey.clear();
        }
    }

    int GetReferencedIndex(boost::property_tree::ptree& pt, ParseState& state)
    {
        boost::optional<std::string> paramName=pt.get_optional<std::string>(Elements::ReferenceName::Name());
        if (!paramName)
        {
            //name missing in indexRef
            const PropertyDescriptionBasic* pd=state.lastInsertedPropertyMapping->property;
            const MemberDescriptionBasic* propMem=pd->members[state.lastInsertedMemberMapping->propertyMemberIndex].get();
            std::ostringstream os;
            os<<"The <name> element is missing in indexRef for propertyMember "<<state.lastInsertedPropertyMapping->property->name<<"."<<propMem->GetName();
            throw ParseError("Bad indexRef", os.str(), state.currentPath, 168);
        }

        SerializationUtils::Trim(*paramName);
        int paramIndex=0;
        boost::optional<std::string> index=pt.get_optional<std::string>(Elements::ReferenceIndex::Name());
        if (index)
        {
            try
            {
                SerializationUtils::Trim(*index);
                paramIndex=boost::lexical_cast<int>(*index);
            }
            catch (const boost::bad_lexical_cast&)
            {
                std::ostringstream os;
                os<<"The specified index "<<*index<<" can not be parsed as an Int32 value. Referenced parameter is "<<*paramName;
                throw ParseError("Bad indexRef", os.str(), state.currentPath, 177);
            }
        }

        const ParameterDescriptionBasic* param=state.repository->GetParameterBasic(*paramName);
        if (!param)
        {
            //Error referenced param not exist
            const PropertyDescriptionBasic* pd=state.lastInsertedPropertyMapping->property;
            const MemberDescriptionBasic* propMem=pd->members[state.lastInsertedMemberMapping->propertyMemberIndex].get();
            std::ostringstream os;
            os<<"The parameter '"<<*paramName<<"' used in indexRef for propertyMember "<<state.lastInsertedPropertyMapping->property->name<<"."<<propMem->GetName()<<
                " does not exist";
            throw ParseError("Bad indexRef", os.str(), state.currentPath, 169);
        }
        if (param->GetMemberType()!=Int32MemberType)
        {
            //wrong type
            std::ostringstream os;
            os<<"The referenced parameter '"<<*paramName<<"' has type "<<param->typeName<<". Only Int32 parameters are valid as indexRef.";
            throw ParseError("Bad indexRef", os.str(), state.currentPath, 170);
        }
        if (paramIndex>=param->GetNumberOfValues())
        {
            //index out of bounds
            std::ostringstream os;
            os<<"The specified index "<<paramIndex<<" is out of bounds. Referenced parameter "<<*paramName;
            if (param->GetCollectionType()==ArrayCollectionType)
            {
                os<<" has arraySize="<<param->GetNumberOfValues();
            }
            else
            {
                os<<" is not an array.";
            }
            throw ParseError("Bad indexRef", os.str(), state.currentPath, 171);
        }

        return param->GetInt32Value(paramIndex);
    }

    int ReferencedKeyToIndex(const ParameterDescriptionBasic* pd, const std::string& key)
    {
        if (pd->GetCollectionType()==DictionaryCollectionType)
        {
            ValueDefinition vd;
            if (ParseKey(pd->GetKeyType(), key, vd))
            {
                switch(pd->GetKeyType())
                {
                case Int32MemberType:
                    return TypeUtilities::GetDictionaryIndexFromKey(pd, vd.key.int32);
                    break;

                case Int64MemberType:
                case TypeIdMemberType:
                    return TypeUtilities::GetDictionaryIndexFromKey(pd, vd.key.int64);
                    break;

                case StringMemberType:
                    return TypeUtilities::GetDictionaryIndexFromKey(pd, vd.key.str);
                    break;

                case EntityIdMemberType:
                    return TypeUtilities::GetDictionaryIndexFromKey(pd, std::make_pair(vd.key.int64, vd.key.hash));
                    break;

                case InstanceIdMemberType:
                case HandlerIdMemberType:
                case ChannelIdMemberType:
                    return TypeUtilities::GetDictionaryIndexFromKey(pd, std::make_pair(vd.key.hash, static_cast<const char*>(NULL)));
                    break;

                default:
                    break;
                }
            }
        }
        else
        {
            if (key.empty()) //the case if index is not present, then we implicit mean 0
            {
                return 0;
            }

            try
            {
                return boost::lexical_cast<int>(key);
            }
            catch (const boost::bad_lexical_cast&) {}
        }

        return -1;
    }

    std::string GetEntityIdParameterAsString(boost::property_tree::ptree& pt)
    {
        std::string name=pt.get<std::string>(Elements::ClassName::Name());
        std::string inst=pt.get<std::string>(Elements::InstanceId::Name());
        SerializationUtils::Trim(name);
        SerializationUtils::Trim(inst);
        return  SerializationUtils::ExpandEnvironmentVariables(name)+std::string(", ")+SerializationUtils::ExpandEnvironmentVariables(inst);
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
                    result.val.boolean=true;
                else if (val=="False" || val=="false")
                    result.val.boolean=false;
                else
                    return false;
            }
                break;

            case Int32MemberType:
            {
                result.val.int32=boost::lexical_cast<DotsC_Int32>(val);
            }
                break;
            case Int64MemberType:
            {
                result.val.int64=boost::lexical_cast<DotsC_Int64>(val);
            }
                break;
            case Float32MemberType:
            {
                result.val.float32=classic_string_cast<DotsC_Float32>(val);
            }
                break;
            case Float64MemberType:
            {
                result.val.float64=classic_string_cast<DotsC_Float64>(val);
            }
                break;
            case EntityIdMemberType:
            {
                result.val.str=val;
                size_t sep=val.find(", ");
                result.val.int64=LlufId_Generate64(val.substr(0, sep).c_str());
                result.val.str=val.substr(sep+2);
                try
                {
                    result.val.hash=boost::lexical_cast<boost::int64_t>(result.val.str);
                    result.val.str.clear();
                }
                catch(const boost::bad_lexical_cast&)
                {
                    result.val.hash=LlufId_Generate64(result.val.str.c_str());
                }
            }
                break;
            case TypeIdMemberType:
            {
                result.val.int64=LlufId_Generate64(val.c_str());
                result.val.str=val;
            }
                break;
            case InstanceIdMemberType:
            case ChannelIdMemberType:
            case HandlerIdMemberType:
            {
                try
                {
                    result.val.hash=boost::lexical_cast<boost::int64_t>(val);
                    result.val.str.clear();
                }
                catch(const boost::bad_lexical_cast&)
                {
                    result.val.hash=LlufId_Generate64(val.c_str());
                    result.val.str=val;
                }
            }
                break;

            case StringMemberType:
            {
                result.val.str=val;
            }
                break;

            case ObjectMemberType:
                return false; //dont know about object types here
            case EnumerationMemberType:
                return false; //dont know about enum types here
            case BinaryMemberType:
            {
                if (!SerializationUtils::FromBase64(val, result.val.str))
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
                result.val.float32=classic_string_cast<DotsC_Float32>(val);
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
                result.val.float64=classic_string_cast<DotsC_Float64>(val);
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

    bool ParseKey(DotsC_MemberType memberType, const std::string& val, ValueDefinition& result)
    {
        try
        {
            switch(memberType)
            {
            case Int32MemberType:
            {
                result.key.int32=boost::lexical_cast<DotsC_Int32>(val);
            }
                break;
            case Int64MemberType:
            {
                result.key.int64=boost::lexical_cast<DotsC_Int64>(val);
            }
                break;
            case EntityIdMemberType:
            {
                size_t sep=val.find(", ");
                result.key.int64=LlufId_Generate64(val.substr(0, sep).c_str());
                result.key.str=val.substr(sep+2);
                try
                {
                    result.key.hash=boost::lexical_cast<boost::int64_t>(result.key.str);
                    result.key.str.clear();
                }
                catch(const boost::bad_lexical_cast&)
                {
                    result.key.hash=LlufId_Generate64(result.key.str.c_str());
                }
            }
                break;
            case TypeIdMemberType:
            {
                result.key.int64=LlufId_Generate64(val.c_str());
            }
                break;
            case InstanceIdMemberType:
            case ChannelIdMemberType:
            case HandlerIdMemberType:
            {
                try
                {
                    result.key.hash=boost::lexical_cast<boost::int64_t>(val);
                    result.key.str.clear();
                }
                catch(const boost::bad_lexical_cast&)
                {
                    result.key.hash=LlufId_Generate64(val.c_str());
                    result.key.str=val;
                }
            }
                break;

            case StringMemberType:
            {
                result.key.str=val;
            }
                break;

            default: //not valid key type
                return false;
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
            DotsC_TypeId baseId=LlufId_Generate64(d->baseClass.c_str());
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
                boost::int64_t id=LlufId_Generate64((*memberIt)->typeName.c_str());
                const ClassDescription* cd=repository->GetClass(id);
                if (cd)
                {
                    (*memberIt)->typeId=id; //type is a known class, ok
                }
                else
                {
                    const EnumDescription* ed=repository->GetEnum(id);
                    if (ed)
                    {
                        (*memberIt)->typeId=id;
                        (*memberIt)->memberType=EnumerationMemberType; //type is a known enum, ok
                    }
                    else //not a known type
                    {
                        std::ostringstream ss;
                        ss<<"The member '"<<(*memberIt)->name<<"' in class/property '"<<d->GetName()<<"' has an invalid type specified. Type: "<<(*memberIt)->typeName;
                        throw ParseError("Invalid type", ss.str(), d->FileName(), 104);
                    }
                }
            }

            //if member is a dictionary we also have to check that the key type exists if it is enum
            if ((*memberIt)->collectionType==DictionaryCollectionType && (*memberIt)->keyType==EnumerationMemberType)
            {
                const EnumDescription* ed=repository->GetEnum((*memberIt)->keyTypeId);
                if (!ed) //not an enum, error
                {
                    std::ostringstream ss;
                    ss<<"The member '"<<(*memberIt)->name<<"' in class/property '"<<d->GetName()<<" is a dictionary collection and has an invalid key type specified. ";
                    const ClassDescription* cd=repository->GetClass((*memberIt)->keyTypeId);
                    if (cd)
                        ss<<cd->GetName()<<" is a class type and is not allowed as key type.";
                    else
                        ss<<"The specified key type does not exist.";

                    throw ParseError("Invalid key type", ss.str(), d->FileName(), 183);
                }
            }
        }
    }

    //----------------------------------------------
    // CompletionAlgorithms
    //----------------------------------------------

    // DOU file completion algorithm
    void DouCompletionAlgorithm::operator()(const ParseState& state)
    {
        //Add predefined types
        ClassDescriptionBasicPtr obj=boost::make_shared<ClassDescriptionBasic>();
        obj->name=BasicTypeOperations::PredefindedClassNames::ObjectName();
        obj->typeId=LlufId_Generate64(obj->name.c_str());
        obj->base=NULL;
        state.repository->InsertClass(obj);

        ExceptionDescriptionBasicPtr exceptionBase=boost::make_shared<ExceptionDescriptionBasic>();
        exceptionBase->name=BasicTypeOperations::PredefindedClassNames::ExceptionName();
        exceptionBase->typeId=LlufId_Generate64(exceptionBase->name.c_str());
        exceptionBase->base=NULL;
        state.repository->InsertException(exceptionBase);

        ExceptionDescriptionBasicPtr fundamentalException=boost::make_shared<ExceptionDescriptionBasic>();
        fundamentalException->name=BasicTypeOperations::PredefindedClassNames::FundamentalExceptionName();
        fundamentalException->typeId=LlufId_Generate64(fundamentalException->name.c_str());
        fundamentalException->base=NULL;
        state.repository->InsertException(fundamentalException);

        ExceptionDescriptionBasicPtr nullException=boost::make_shared<ExceptionDescriptionBasic>();
        nullException->name=BasicTypeOperations::PredefindedClassNames::NullExceptionName();
        nullException->typeId=LlufId_Generate64(nullException->name.c_str());
        nullException->baseClass=BasicTypeOperations::PredefindedClassNames::FundamentalExceptionName();
        nullException->base=fundamentalException.get();
        state.repository->InsertException(nullException);

        ExceptionDescriptionBasicPtr incompatibleTypesException=boost::make_shared<ExceptionDescriptionBasic>();
        incompatibleTypesException->name=BasicTypeOperations::PredefindedClassNames::IncompatibleTypesExceptionName();
        incompatibleTypesException->typeId=LlufId_Generate64(incompatibleTypesException->name.c_str());
        incompatibleTypesException->baseClass=BasicTypeOperations::PredefindedClassNames::FundamentalExceptionName();
        incompatibleTypesException->base=fundamentalException.get();
        state.repository->InsertException(incompatibleTypesException);

        ExceptionDescriptionBasicPtr readOnlyException=boost::make_shared<ExceptionDescriptionBasic>();
        readOnlyException->name=BasicTypeOperations::PredefindedClassNames::ReadOnlyExceptionName();
        readOnlyException->typeId=LlufId_Generate64(readOnlyException->name.c_str());
        readOnlyException->baseClass=BasicTypeOperations::PredefindedClassNames::FundamentalExceptionName();
        readOnlyException->base=fundamentalException.get();
        state.repository->InsertException(readOnlyException);

        ExceptionDescriptionBasicPtr illegalValueException=boost::make_shared<ExceptionDescriptionBasic>();
        illegalValueException->name=BasicTypeOperations::PredefindedClassNames::IllegalValueExceptionName();
        illegalValueException->typeId=LlufId_Generate64(illegalValueException->name.c_str());
        illegalValueException->baseClass=BasicTypeOperations::PredefindedClassNames::FundamentalExceptionName();
        illegalValueException->base=fundamentalException.get();
        state.repository->InsertException(illegalValueException);

        ExceptionDescriptionBasicPtr softwareViolationException=boost::make_shared<ExceptionDescriptionBasic>();
        softwareViolationException->name=BasicTypeOperations::PredefindedClassNames::SoftwareViolationExceptionName();
        softwareViolationException->typeId=LlufId_Generate64(softwareViolationException->name.c_str());
        softwareViolationException->baseClass=BasicTypeOperations::PredefindedClassNames::FundamentalExceptionName();
        softwareViolationException->base=fundamentalException.get();
        state.repository->InsertException(softwareViolationException);

        ExceptionDescriptionBasicPtr configurationErrorException=boost::make_shared<ExceptionDescriptionBasic>();
        configurationErrorException->name=BasicTypeOperations::PredefindedClassNames::ConfigurationErrorExceptionName();
        configurationErrorException->typeId=LlufId_Generate64(configurationErrorException->name.c_str());
        configurationErrorException->baseClass=BasicTypeOperations::PredefindedClassNames::FundamentalExceptionName();
        configurationErrorException->base=fundamentalException.get();
        state.repository->InsertException(configurationErrorException);

        //Enum checksum calculation
        CalculateEnumChecksums(state);

        //Setup Exception baseclass
        std::set<DotsC_TypeId> endConditions;
        endConditions.insert(exceptionBase->typeId);
        endConditions.insert(fundamentalException->typeId);
        endConditions.insert(nullException->typeId);
        endConditions.insert(incompatibleTypesException->typeId);
        endConditions.insert(readOnlyException->typeId);
        endConditions.insert(illegalValueException->typeId);
        endConditions.insert(softwareViolationException->typeId);
        endConditions.insert(configurationErrorException->typeId);
        boost::function<void(ExceptionDescriptionBasic*, ExceptionDescriptionBasic*)> setExeptBaseFun(SetExceptionBase);
        for (boost::unordered_map<DotsC_TypeId, ExceptionDescriptionBasicPtr>::iterator it=state.repository->m_exceptions.begin(); it!=state.repository->m_exceptions.end(); ++it)
        {
            int recLevel=0;
            SetupBaseClass(state.repository->m_exceptions, endConditions, it->second.get(), setExeptBaseFun, recLevel);
        }

        //Property members
        for (boost::unordered_map<DotsC_TypeId, PropertyDescriptionBasicPtr>::iterator it=state.repository->m_properties.begin(); it!=state.repository->m_properties.end(); ++it)
        {
            SetupMemberTypes(it->second, state.repository);
        }

        //Classes - set baseClass, create members, parameters, createRoutines.
        std::vector<ClassDescriptionBasic*> classesWithCreateRoutines;
        classesWithCreateRoutines.reserve(100);
        boost::function<void(ClassDescriptionBasic*, ClassDescriptionBasic*)> setClassBaseFun(SetClassBase);
        endConditions.clear();
        endConditions.insert(obj->typeId);
        for (boost::unordered_map<DotsC_TypeId, ClassDescriptionBasicPtr>::iterator it=state.repository->m_classes.begin(); it!=state.repository->m_classes.end(); ++it)
        {
            //Set base class
            int recLevel=0;
            SetupBaseClass(state.repository->m_classes, endConditions, it->second.get(), setClassBaseFun, recLevel);

            //Create members
            SetupMemberTypes(it->second, state.repository);

            if (it->second->createRoutines.size()>0)
            {
                classesWithCreateRoutines.push_back(it->second.get());
            }
        }

        //Resolve parameter to parameter references
        ResolveParamToParamRefs(state);

        //Resolve arraySizeRef and maxLenghtRef. Also handle implicit createRoutine parameters that need some post processing.
        ResolveReferences(state);

        //Verify that parameter with memberType typeId, entityId or enum is referencing valid types.
        VerifyParameterTypes(state);

        //Deserialize xml objects
        DeserializeObjects(state);

        //Create routines - verify types, check duplicates etc.
        std::for_each(classesWithCreateRoutines.begin(), classesWithCreateRoutines.end(), boost::bind(&DouCompletionAlgorithm::HandleCreateRoutines, this, boost::ref(state), _1));
    }

    void DouCompletionAlgorithm::DeserializeObjects(const ParseState& state)
    {
        XmlToBlobSerializer<TypeRepository> niceSerializer(state.repository.get());
        UglyXmlToBlobSerializer<TypeRepository> deprecatedSerializer(state.repository.get());

        //check object parameters
        for (std::vector<ParseState::ObjectParameter>::const_iterator parIt=state.objectParameters.begin();
             parIt!=state.objectParameters.end(); ++parIt)
        {
            ParameterDescriptionBasic* param=parIt->referee.referencingItem;
            size_t paramIndex=parIt->referee.referencingIndex;
            ValueDefinition& vd=param->MutableValue(paramIndex);
            boost::property_tree::ptree& pt=*(parIt->obj);

            if (!parIt->deprecatedXmlFormat)
            {
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
                    niceSerializer.SerializeObjectContent(typeName, vd.val.bin, pt); //since pt does not include the root element we have to use method SerializeObjectContent
                }
                catch (const ParseError& err)
                {
                    std::ostringstream os;
                    os<<"Failed to deserialize object parameter "<<param->GetName()<<" index="<<paramIndex<<" in class "<<parIt->referee.referencingClass->GetName()
                     <<". "<<err.Description();
                    throw ParseError("Invalid Object", os.str(), parIt->referee.referencingClass->FileName(), err.ErrorId());
                }
            }
            else //This is when using the old xml format
            {
                DotsC_TypeId tid;
                try
                {
                    tid=deprecatedSerializer.SerializeObjectContent(vd.val.bin, pt);
                }
                catch (const ParseError& err)
                {
                    std::ostringstream os;
                    os<<"Failed to deserialize object parameter "<<param->GetName()<<" index="<<paramIndex<<" in class "<<parIt->referee.referencingClass->GetName()
                     <<". "<<err.Description();
                    throw ParseError("Invalid Object", os.str(), parIt->referee.referencingClass->FileName(), err.ErrorId());
                }

                if (!BasicTypeOperations::IsOfType<TypeRepository>(state.repository.get(), ObjectMemberType, tid, ObjectMemberType, param->GetTypeId()))
                {
                    std::ostringstream os;
                    os<<param->GetName()<<" index="<<paramIndex<<" in class "<<parIt->referee.referencingClass->GetName()<<" contains a value of type '"
                     <<state.repository->GetClass(tid)->GetName()<<"' that is not a subtype of the declared type "<<param->typeName;
                    throw ParseError("Type missmatch", os.str(), parIt->referee.referencingClass->FileName(), 106);
                }
            }
        }
    }

    void DouCompletionAlgorithm::ResolveParamToParamRefs(const ParseState& state)
    {
        typedef ParseState::ParameterReference<ParameterDescriptionBasic> ParamToParamRef;
        typedef std::vector<ParamToParamRef> ParamRefVec;

        const ParamToParamRef* failure=NULL;
        for (int level=0; level<100; ++level) //no more than 100 levels of indirection is permitted
        {
            failure=NULL;

            for (ParamRefVec::const_iterator parIt=state.paramToParamReferences.begin();
                 parIt!=state.paramToParamReferences.end(); ++parIt)
            {
                if (!ResolveParamToParamRef(state, *parIt))
                {
                    failure=&(*parIt);
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

    bool DouCompletionAlgorithm::ResolveParamToParamRef(const ParseState& state, const ParseState::ParameterReference<ParameterDescriptionBasic>& ref)
    {
        ClassDescriptionBasic* cd=ref.referee.referencingClass;
        ParameterDescriptionBasic* referencing=ref.referee.referencingItem;

        if (referencing->values[ref.referee.referencingIndex].val.referenced!=NULL)
        {
            //this parameter has already been resolved.
            return true;
        }


        ParameterDescriptionBasic* referenced=state.repository->GetParameterBasic(ref.parameterName);

        if (!referenced)
        {
            std::ostringstream ss;
            ss<<"Could not resolve Parameter valueRef "<<ref.parameterName<<". Referenced from parameter:  "<<referencing->GetName();
            if (referencing->GetCollectionType()==ArrayCollectionType)
                ss<<" (index="<<ref.referee.referencingIndex<<") ";
            throw ParseError("Parameter reference error", ss.str(), cd->FileName(), 42);
        }

        int parameterIndex=ReferencedKeyToIndex(referenced, ref.parameterKey);
        if (parameterIndex<0)
        {
            std::ostringstream ss;
            ss<<"The specified key in parameter valueRef does not exist or is out of bounds.'"<<ref.parameterName<<"' and key='"<<ref.parameterKey<<"'. Referenced from parameter: "<<referencing->GetName();
            throw ParseError("Invalid parameter reference", ss.str(), cd->FileName(), 203);
        }

        if (referenced->GetNumberOfValues()<=parameterIndex)
        {
            std::ostringstream ss;
            ss<<"Array index out of range for Parameter valueRef '"<<ref.parameterName<<"' and index="<<parameterIndex<<". Referenced from parameter: "<<referencing->GetName();
            throw ParseError("Parameter index out of bounds", ss.str(), cd->FileName(), 43);
        }

        //Check that the referenced parameter contains a value. If referenced param is also referencing another param, we have to resolve that one first.
        if (referenced->values[static_cast<size_t>(parameterIndex)].kind==RefKind && referenced->values[static_cast<size_t>(parameterIndex)].val.referenced==NULL)
        {
            //unresolved paramToParamRef
            return false;
        }

        if (referenced->memberType==ObjectMemberType || referenced->memberType==EnumerationMemberType)
        {
            //referencing to non-basic types are always set to ObjectMemberType before we know the type
            referencing->memberType=referenced->memberType;
        }

        if (!BasicTypeOperations::IsOfType<TypeRepository>(state.repository.get(), referenced->memberType, referenced->GetTypeId(),
                      referencing->memberType, referencing->GetTypeId()))
        {
            if ((referencing->memberType==InstanceIdMemberType ||
                 referencing->memberType==ChannelIdMemberType ||
                 referencing->memberType==HandlerIdMemberType) &&
                    referenced->memberType==StringMemberType)
            {
                //A special case, hashed types can reference a plain string
            }
            else
            {
                //referenced parameter cant be derived as the same type as referencing parameter
                std::ostringstream ss;
                ss<<"The parameter '"<<referencing->GetName()<<"' ";
                if (referencing->GetCollectionType()==ArrayCollectionType)
                    ss<<"(index="<<ref.referee.referencingIndex<<") ";
                ss<<"of type "<<referencing->typeName<<" is referencing parameter '"<<referenced->GetName()<<
                    " of type "<<referenced->typeName<<". The types are not compatilbe and "<<
                    referenced->typeName<<" is not derived from "<<referencing->typeName;
                throw ParseError("Parameter reference error", ss.str(), cd->FileName(), 44);
            }
        }

        //point to referenced
        referencing->values[ref.referee.referencingIndex].val.referenced=&referenced->values[static_cast<size_t>(parameterIndex)];

        return true;
    }    

    void DouCompletionAlgorithm::ResolveArraySizeRef(const ParseState& state, const ParseState::ParameterReference<MemberDescriptionBasic>& ref)
    {
        ClassDescriptionBasic* cd=ref.referee.referencingClass;
        MemberDescriptionBasic* md=ref.referee.referencingItem;
        ParameterDescriptionBasic* referenced=state.repository->GetParameterBasic(ref.parameterName);
        if (!referenced)
        {
            std::ostringstream ss;
            ss<<"Could not resolve parameter for arraySizeRef '"<<ref.parameterName<<"'. Referenced from member '"<<md->GetName()<<"' in class "<<cd->GetName();
            throw ParseError("Parameter reference error", ss.str(), cd->FileName(), 49);
        }

        int parameterIndex=ReferencedKeyToIndex(referenced, ref.parameterKey);
        if (parameterIndex<0)
        {
            std::ostringstream os;
            os<<"Can't resolve arraySizeRef "<<ref.parameterName<<"['"<<ref.parameterKey<<"]. Referenced from memeber '"<<md->GetName()<<"' in class "<<cd->GetName();
            throw ParseError("Invalid arraySizeRef", os.str(), cd->FileName(), 204);
        }

        if (referenced->GetNumberOfValues()<=parameterIndex)
        {
            std::ostringstream ss;
            ss<<"Array index out of range for Parameter arraySizeRef '"<<ref.parameterName<<"' and index="<<parameterIndex<<". Referenced from memeber '"<<md->GetName()<<"' in class "<<cd->GetName();
            throw ParseError("Parameter index out of bounds", ss.str(), cd->FileName(), 51);
        }
        if (referenced->GetMemberType()!=Int32MemberType)
        {
            std::ostringstream ss;
            ss<<"The parameter referenced for arraySize '"<<ref.parameterName<<"' has type "<<referenced->typeName<<" and not the expected type Int32. Referenced from memeber '"<<md->GetName()<<"' in class "<<cd->GetName();
            throw ParseError("Type missmatch in arraySizeRef", ss.str(), cd->FileName(), 52);
        }

        int size=referenced->GetInt32Value(parameterIndex);
        if (size<=0)
        {
            std::ostringstream ss;
            ss<<"Array size must be greater than 0. The arraySize value specified for member '"<<md->GetName()<<"' is "<<size;
            throw ParseError("Invalid arraySize value", ss.str(), cd->FileName(), 7);

        }

        //Seems to be an ok reference. Copy value for arraySize
        md->arraySize=size;
    }

    void DouCompletionAlgorithm::ResolveMaxLengthRef(const ParseState& state, const ParseState::ParameterReference<MemberDescriptionBasic>& ref)
    {
        ClassDescriptionBasic* cd=ref.referee.referencingClass;
        MemberDescriptionBasic* md=ref.referee.referencingItem;
        ParameterDescriptionBasic* referenced=state.repository->GetParameterBasic(ref.parameterName);
        if (!referenced)
        {
            std::ostringstream ss;
            ss<<"Could not resolve parameter for maxLengthRef '"<<ref.parameterName<<"'. Referenced from memeber '"<<md->GetName()<<"' in class "<<cd->GetName();
            throw ParseError("Parameter reference error", ss.str(), cd->FileName(), 107);
        }

        int parameterIndex=ReferencedKeyToIndex(referenced, ref.parameterKey);
        if (parameterIndex<0)
        {
            std::ostringstream os;
            os<<"Can't resolve maxLengthRef "<<ref.parameterName<<"['"<<ref.parameterKey<<"]. Referenced from memeber '"<<md->GetName()<<"' in class "<<cd->GetName();
            throw ParseError("Invalid maxLengthRef", os.str(), cd->FileName(), 178);
        }

        if (referenced->GetNumberOfValues()<=parameterIndex)
        {
            std::ostringstream ss;
            ss<<"Array index out of range for Parameter maxLengthRef '"<<ref.parameterName<<"' and index="<<parameterIndex<<". Referenced from memeber '"<<md->GetName()<<"' in class "<<cd->GetName();
            throw ParseError("Parameter index out of bounds", ss.str(), cd->FileName(), 53);
        }
        if (referenced->GetMemberType()!=Int32MemberType)
        {
            std::ostringstream ss;
            ss<<"The parameter referenced for maxLength '"<<ref.parameterName<<"' has type "<<referenced->typeName<<" and not the expected type Int32. Referenced from memeber '"<<md->GetName()<<"' in class "<<cd->GetName();
            throw ParseError("Type missmatch in maxLengthRef", ss.str(), cd->FileName(), 41);
        }

        int size=referenced->GetInt32Value(parameterIndex);
        if (size<=0)
        {
            std::ostringstream ss;
            ss<<"Max length must be greater than 0. The maxLength value specified for member '"<<md->GetName()<<"' is "<<size;
            throw ParseError("Invalid maxLength value", ss.str(), cd->FileName(), 6);

        }

        //Seems to be an ok reference. Copy value for maxLength
        md->maxLength=size;
    }

    void DouCompletionAlgorithm::ResolveCreateRoutineParams(const ParseState& state, const ParseState::ParameterReference<CreateRoutineDescriptionBasic>& ref)
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
        ParameterDescriptionBasic* pdef=state.repository->GetParameterBasic(ref.parameterName);

        if (!pdef)
        {
            //Parameter not found
            std::ostringstream os;
            os<<"The createRoutine '"<<cr->name<<"' in class "<<cd->GetName()<<"' specifies an invalid parameter as default value for member '"<<
                md->name<<"'. The parameter '"<<ref.parameterName<<"' does not exist.";
            throw ParseError("Invalid CreateRoutine value", os.str(), cd->FileName(), 58);
        }

        if (pdef->hidden) //inline parameter
        {
            pdef->memberType=md->memberType;
            pdef->typeName=md->typeName;
            pdef->typeId=md->typeId;

            if (pdef->memberType!=EnumerationMemberType && pdef->memberType!=ObjectMemberType)
            {
                //Basic types is just stored as strings and must be converted
                std::string paramRawVal=pdef->Value(0).val.str;
                if (!ParseValue(pdef->memberType, paramRawVal, pdef->MutableValue(0)))
                {
                    std::ostringstream os;
                    os<<"Cant parse createRoutine value '"<<paramRawVal<<"' for member "<<memberName<<" in createRoutine "<<cr->GetName()<<" in class "<<cd->GetName()<<" as a value of the expected type";
                    throw ParseError("Invalid create routine value", os.str(), cd->FileName(), 63);
                }
            }
        }
        else //explicit parameter
        {
            if (!BasicTypeOperations::IsOfType<TypeRepository>(state.repository.get(), pdef->GetMemberType(), pdef->GetTypeId(), md->GetMemberType(), md->GetTypeId()))
            {
                //Type missmatch
                std::ostringstream os;
                os<<"The createRoutine '"<<cr->name<<"' in class '"<<cd->GetName()<<"' specifies a value of incorrect type for member '"<<md->name<<"'. Expected type "<<
                    md->typeName<<" but specified value has type "<<pdef->typeName;
                throw ParseError("Invalid CreateRoutine value", os.str(), cd->FileName(), 60);
            }

            if (md->GetCollectionType()!=SingleValueCollectionType)
            {
                std::ostringstream os;
                os<<"The createRoutine '"<<cr->name<<"' in class '"<<cd->name<<"' specifies a default value for member '"<<md->name<<"' wich has collectionType="<<BasicTypeOperations::CollectionTypeToString(md->GetCollectionType())<<". Collections can't have default values.";
                throw ParseError("CreateRoutine collection values not supported", os.str(), cd->FileName(), 61);
            }

            int paramIndex=ReferencedKeyToIndex(pdef, ref.parameterKey);
            if (paramIndex<0)
            {
                std::ostringstream os;
                os<<"Can't resolve createRoutine value reference "<<ref.parameterName<<"["<<ref.parameterKey<<"] for member "<<memberName<<" in createRoutine "<<cr->GetName()<<" in class "<<cd->GetName();
                throw ParseError("Invalid create routine value", os.str(), cd->FileName(), 179);
            }

            cr->memberValues[ref.referee.referencingIndex].second.first=ref.parameterName;
            cr->memberValues[ref.referee.referencingIndex].second.second=paramIndex;
        }
    }

    void DouCompletionAlgorithm::ResolveReferences(const ParseState& state)
    {
        //array size refs
        std::for_each(state.arraySizeReferences.begin(),
                      state.arraySizeReferences.end(),
                      boost::bind(&DouCompletionAlgorithm::ResolveArraySizeRef, this, boost::ref(state), _1));

        //max length refs
        std::for_each(state.maxLengthReferences.begin(),
                      state.maxLengthReferences.end(),
                      boost::bind(&DouCompletionAlgorithm::ResolveMaxLengthRef, this, boost::ref(state), _1));

        //hidden create routine basic type parameters (not enums or objects)
        std::for_each(state.createRoutineIncompleteHiddenParameters.begin(),
                      state.createRoutineIncompleteHiddenParameters.end(),
                      boost::bind(&DouCompletionAlgorithm::ResolveCreateRoutineParams, this, boost::ref(state), _1));
    }

    void DouCompletionAlgorithm::HandleCreateRoutines(const ParseState& state, ClassDescriptionBasic* cd)
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
                //paramName:  mit->second.first
                //paramIndex: mit->second.second

                int memberIndex=cd->GetMemberIndex(mit->first);
                if (memberIndex<0)
                {
                    //Member not found in class
                    std::ostringstream os;
                    os<<"The createRoutine '"<<cr.name<<"' in class "<<cd->GetName()<<"' specifies an invalid member '"<<mit->first<<"'. The member does not exist in the class.";
                    throw ParseError("Invalid CreateRoutine value", os.str(), cd->FileName(), 57);
                }
                const ParameterDescriptionBasic* pd=state.repository->GetParameterBasic(mit->second.first);

                if (pd->GetNumberOfValues()<=mit->second.second)
                {
                    //Array index out of range
                    std::ostringstream os;
                    os<<"The createRoutine '"<<cr.name<<"' in class "<<cd->GetName()<<"' specifies an parameter index that is out of range for member '"<<
                        mit->first<<"'. Parameter'"<<mit->second.first<<"'' with index="<<mit->second.second;
                    if (pd->GetCollectionType()!=ArrayCollectionType)
                    {
                        os<<" is not an array.";
                    }
                    else
                    {
                        os<<" - actual array size is "<<pd->GetNumberOfValues();
                    }
                    throw ParseError("Invalid CreateRoutine value", os.str(), cd->FileName(), 59);
                }

                cr.memberValuesParams.push_back(std::make_pair(pd, mit->second.second));
            }
        }
    }

    void DouCompletionAlgorithm::CalculateEnumChecksums(const ParseState& state)
    {
        for (boost::unordered_map<DotsC_TypeId, EnumDescriptionBasicPtr>::iterator it=state.repository->m_enums.begin();
             it!=state.repository->m_enums.end(); ++it)
        {
            EnumDescriptionBasicPtr& ed=it->second;
            std::ostringstream ss;
            ss<<ed->name;
            for (StringVector::const_iterator valIt=ed->enumerationValues.begin(); valIt!=ed->enumerationValues.end(); ++valIt)
            {
                ss<<"."<<*valIt;
            }
            ed->checksum=LlufId_Generate64(ss.str().c_str());
        }
    }

    void DouCompletionAlgorithm::VerifyParameterTypes(const ParseState& state)
    {
        static const DotsC_TypeId EntityTypeId=LlufId_Generate64("Safir.Dob.Entity");

        //loop through all parameters and verify all TypeId, EntityId, and Enum
        for (boost::unordered_map<std::string, ParameterDescriptionBasic*>::iterator parIt=state.repository->m_parameters.begin();
             parIt!=state.repository->m_parameters.end(); ++parIt)
        {
            ParameterDescriptionBasic* pd=parIt->second;

            if (pd->GetMemberType()==TypeIdMemberType)
            {
                //Verify that typeId parameters contains values that are existing typeIds.
                for (int index=0; index<pd->GetNumberOfValues(); ++index)
                {
                    const ValueDefinition& v=pd->Value(static_cast<size_t>(index));
                    if (!BasicTypeOperations::ValidTypeId(state.repository.get(), v.val.int64))
                    {
                        std::string file=parIt->first.substr(0, parIt->first.rfind(".")+1)+"dou";
                        std::ostringstream os;
                        os<<"The parameter "<<pd->GetName()<<" has an invalid value. '"<<v.val.str<<"' is not a TypeId of an existing type.";
                        throw ParseError("Invalid TypeId parameter", os.str(), file, 45);
                    }
                }
            }
            else if (pd->GetMemberType()==EntityIdMemberType)
            {
                //Verify that EntityId parameters contains values that are existing class type.
                for (int index=0; index<pd->GetNumberOfValues(); ++index)
                {
                    const ValueDefinition& v=pd->Value(static_cast<size_t>(index));
                    const ClassDescription* tmpCd=state.repository->GetClass(v.val.int64);
                    if (!tmpCd)
                    {
                        std::string file=parIt->first.substr(0, parIt->first.rfind(".")+1)+"dou";
                        std::ostringstream os;
                        os<<"The parameter "<<pd->GetName()<<" has an invalid value. The typeId does not exist or is not a class type.";
                        throw ParseError("Invalid EntityId parameter", os.str(), file, 46);
                    }

                    if (!BasicTypeOperations::IsOfType<TypeRepository>(state.repository.get(), ObjectMemberType, v.val.int64, ObjectMemberType, EntityTypeId))
                    {
                        std::string file=parIt->first.substr(0, parIt->first.rfind(".")+1)+"dou";
                        std::ostringstream os;
                        os<<"The parameter "<<pd->GetName()<<" has an invalid typeId. The class '"<<tmpCd->GetName()<<"' is not a subtype of Safir.Dob.Entity";
                        throw ParseError("Invalid EntityId parameter", os.str(), file, 172);
                    }
                }
            }
            else if (pd->GetMemberType()==EnumerationMemberType)
            {
                //Verify that enum parameter values are valid according to the specified enum type.
                pd->typeId=LlufId_Generate64(pd->typeName.c_str());
                const EnumDescription* ed=state.repository->GetEnum(pd->typeId);
                if (!ed)
                {
                    //Enum type does not exist
                    std::string file=parIt->first.substr(0, parIt->first.rfind(".")+1)+"dou";
                    std::ostringstream os;
                    os<<"The parameter type '"<<pd->typeName<<"' does not exist. Expected to be a basic type or enum type. Specified for parameter "<<pd->GetName();
                    throw ParseError("Invalid type", os.str(), file, 47);
                }

                //Check value
                for (int index=0; index<pd->GetNumberOfValues(); ++index)
                {
                    ValueDefinition& v=pd->MutableValue(static_cast<size_t>(index));
                    v.val.int32=ed->GetIndexOfValue(v.val.str);
                    if (v.val.int32<0)
                    {
                        std::string file=parIt->first.substr(0, parIt->first.rfind(".")+1)+"dou";
                        std::ostringstream os;
                        os<<"The parameter "<<pd->GetName()<<" has an invalid value '"<<v.val.str<<"'. Expected to be an enum value of type "<<ed->GetName();
                        throw ParseError("Invalid enum value", os.str(), file, 48);
                    }
                }
            }

            //if dictionary and key is enum we must check the enum value too
            if (pd->GetCollectionType()==DictionaryCollectionType && pd->GetKeyType()==EnumerationMemberType)
            {
                //Verify that enum parameter key is valid according to the specified enum type.
                const EnumDescription* ed=state.repository->GetEnum(pd->GetKeyTypeId());
                if (!ed)
                {
                    //Enum type does not exist
                    std::string file=parIt->first.substr(0, parIt->first.rfind(".")+1)+"dou";
                    std::ostringstream os;
                    os<<"The keyType specified for the dictionary paramaeter '"<<pd->typeName<<"' does not exist";
                    throw ParseError("Key type does not exist", os.str(), file, 194);
                }

                //Check value
                for (int index=0; index<pd->GetNumberOfValues(); ++index)
                {
                    ValueDefinition& val=pd->MutableValue(static_cast<size_t>(index));
                    val.key.int32=ed->GetIndexOfValue(val.key.str);
                    if (val.key.int32<0)
                    {
                        std::string file=parIt->first.substr(0, parIt->first.rfind(".")+1)+"dou";
                        std::ostringstream os;
                        os<<"The parameter "<<pd->GetName()<<" has an invalid key '"<<val.key.str<<"'. Expected to be an enum value of type "<<ed->GetName();
                        throw ParseError("Invalid key", os.str(), file, 196);
                    }
                }
            }
        }
    }

    // DOM file completion algorithm
    void DomCompletionAlgorithm::operator()(const ParseState& state)
    {
        //Insert propertyMappings and check for duplicates and missing memberMappings
        std::for_each(state.notInsertedPropertyMappings.begin(), state.notInsertedPropertyMappings.end(),
                      boost::bind(&DomCompletionAlgorithm::InsertPropertyMapping, this, _1));

        //Insert hidden parameters
        for (std::vector< std::pair<ClassDescriptionBasic*, ParameterDescriptionBasicPtr> >::const_iterator parIt=state.notInsertedParameters.begin();
             parIt!=state.notInsertedParameters.end(); ++parIt)
        {
            parIt->first->ownParameters.push_back(parIt->second);
            state.repository->InsertParameter(parIt->second);
        }
    }

    void DomCompletionAlgorithm::InsertPropertyMapping(const PropertyMappingDescriptionBasicPtr& pm)
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
        for (size_t memIx=0; memIx<numMembers; ++memIx)
        {
            if (pm->memberMappings[memIx]==NULL)
            {
                //Member has not been mapped
                std::ostringstream ss;
                ss<<"In propertyMapping between property '"<<pm->property->GetName()<<"' and class '"<<pm->class_->GetName()<<"' there is no mapping for property member '"<< pm->property->GetMember(static_cast<int>(memIx))->GetName()<<"'.";
                throw ParseError("Member not mapped", ss.str(), pm->FileName(), 101);
            }
        }

        pm->class_->properties.push_back(pm);
    }
}
}
}
}
