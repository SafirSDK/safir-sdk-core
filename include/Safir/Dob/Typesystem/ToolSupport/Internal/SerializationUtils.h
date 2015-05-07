/******************************************************************************
*
* Copyright Consoden AB, 2004-2015 (http://safir.sourceforge.net)
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
#ifndef __DOTS_INTERNAL_SERIALIZATION_UTILS_H__
#define __DOTS_INTERNAL_SERIALIZATION_UTILS_H__

#ifdef _MSC_VER
#pragma warning(disable:4127)
#endif

#include <string>
#include <vector>
#include <boost/property_tree/ptree.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/archive/iterators/base64_from_binary.hpp>
#include <boost/archive/iterators/binary_from_base64.hpp>
#include <boost/archive/iterators/transform_width.hpp>
#include <boost/archive/iterators/insert_linebreaks.hpp>
#include <boost/archive/iterators/remove_whitespace.hpp>
#include <Safir/Utilities/Internal/ConfigReader.h>
#include <Safir/Dob/Typesystem/ToolSupport/TypeUtilities.h>
#include <Safir/Dob/Typesystem/ToolSupport/ParseError.h>
#include <Safir/Dob/Typesystem/ToolSupport/Internal/classic_string_cast.h>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace ToolSupport
{
namespace Internal
{
namespace SerializationUtils
{
    inline std::string ToBase64(const std::string& bin)
    {
        typedef boost::archive::iterators::insert_linebreaks< boost::archive::iterators::base64_from_binary< boost::archive::iterators::transform_width<std::string::const_iterator,6,8> >, 72 > it_base64_t;
        unsigned int writePaddChars=(3-bin.size()%3)%3;
        std::string base64(it_base64_t(bin.begin()),it_base64_t(bin.end()));
        base64.append(writePaddChars,'=');
        return base64;
    }

    inline bool FromBase64(std::string base64, std::string& bin)
    {
        try
        {
            typedef boost::archive::iterators::transform_width< boost::archive::iterators::binary_from_base64< boost::archive::iterators::remove_whitespace< std::string::const_iterator> >, 8, 6 > it_binary_t;
            const size_t paddChars=std::count(base64.begin(), base64.end(), '=');
            std::replace(base64.begin(),base64.end(),'=','A'); // replace '=' by base64 encoding of '\0'
            bin.insert(bin.begin(), it_binary_t(base64.begin()), it_binary_t(base64.end()));
            bin.erase(bin.end()-paddChars,bin.end());  // erase pad
            return true;
        }
        catch (const std::exception&)
        {
            return false;
        }
    }

    inline void Trim(std::string& s)
    {
        boost::trim_if(s, boost::is_any_of("\r\n\t "));
    }

    inline std::string TrimCopy(const std::string& s)
    {
        return boost::trim_copy_if(s, boost::is_any_of("\r\n\t "));
    }

    inline std::string ExpandEnvironmentVariables(const std::string& str)
    {
        std::string result = Safir::Utilities::Internal::Expansion::ExpandSpecial(str);
        result = Safir::Utilities::Internal::Expansion::ExpandEnvironment(result);
        return result;
    }

    inline DotsC_TypeId StringToTypeId(const std::string& str)
    {
        DotsC_TypeId tid=0;
        try
        {
            tid=boost::lexical_cast<DotsC_TypeId>(str);
        }
        catch (const boost::bad_lexical_cast&)
        {
            tid=LlufId_Generate64(str.c_str());
        }
        return tid;
    }

    inline std::pair<DotsC_Int64, const char*> StringToHash(const std::string& str)
    {
        std::pair<DotsC_Int64, const char*> result(0, static_cast<const char*>(NULL));
        try
        {
            result.first=boost::lexical_cast<DotsC_Int64>(str);
        }
        catch (const boost::bad_lexical_cast&)
        {
            result.first=LlufId_Generate64(str.c_str());
            result.second=str.c_str();
        }
        return result;
    }

    inline std::pair<DotsC_EntityId, const char*> StringToEntityId(const std::string& type, const std::string& inst)
    {
        std::pair<DotsC_EntityId, const char*> entityId;
        entityId.first.typeId=SerializationUtils::StringToTypeId(type);
        std::pair<DotsC_Int64, const char*> instanceId=SerializationUtils::StringToHash(inst);
        entityId.first.instanceId=instanceId.first;
        entityId.second=instanceId.second;
        return entityId;
    }

    inline bool StringToBoolean(const std::string& val)
    {
        if (val=="true" || val=="True")
        {
            return true;
        }
        else if (val=="false" || val=="False")
        {
            return false;
        }

        throw std::invalid_argument("Failed to convert '"+val+"' to boolean");
    }

    template <class WriterT, class KeyT>
    void SetKeyWithNullValue(DotsC_MemberIndex memIx,
                             const KeyT& key,
                             WriterT& writer)
    {
        writer.WriteKey(memIx, key);
        writer.WriteValue(memIx, 0, 0, true, true);
    }

    template <class WriterT, class KeyT>
    void SetMemberValue(const typename WriterT::RepositoryType* repository,
                        const typename WriterT::MemberDescriptionType* md,
                        DotsC_MemberIndex memIx,
                        DotsC_Int32 arrIx,
                        boost::property_tree::ptree& memberContent,
                        const KeyT& key,
                        WriterT& writer)
    {
        if (md->GetCollectionType()==DictionaryCollectionType)
        {
            //if dictionary first write the key
            writer.WriteKey(memIx, key);
        }

        //write the value
        switch(md->GetMemberType())
        {
        case BooleanMemberType:
            {
                Trim(memberContent.data());
                const std::string& val=memberContent.data();
                bool boolVal=false;
                try
                {
                    boolVal=StringToBoolean(val);
                }
                catch (const std::exception& err)
                {
                    throw ParseError("Serialization error", err.what(), "", 208);
                }

                writer.WriteValue(memIx, arrIx, boolVal, false, true);
            }
            break;

        case EnumerationMemberType:
            {
                Trim(memberContent.data());
                const typename WriterT::EnumDescriptionType* ed=repository->GetEnum(md->GetTypeId());
                DotsC_Int32 enumOrdinal=ed->GetIndexOfValue(memberContent.data());
                if (enumOrdinal<0)
                {
                    std::ostringstream os;
                    os<<"Enumeration member '"<<md->GetName()<<"' contains an invalid value. Value="<<memberContent.data()<<" is not a value of enum type "<<ed->GetName();
                    throw ParseError("Serialization error", os.str(), "", 114);
                }

                writer.WriteValue(memIx, arrIx, enumOrdinal, false, true);
            }
            break;

        case Int32MemberType:
            {
                Trim(memberContent.data());
                DotsC_Int32 val=memberContent.get_value<DotsC_Int32>();
                writer.WriteValue(memIx, arrIx, val, false, true);
            }
            break;

        case Int64MemberType:
            {
                Trim(memberContent.data());
                DotsC_Int64 val=memberContent.get_value<DotsC_Int64>();
                writer.WriteValue(memIx, arrIx, val, false, true);
            }
            break;

        case TypeIdMemberType:
            {
                Trim(memberContent.data());
                DotsC_TypeId tid=SerializationUtils::StringToTypeId(memberContent.data());

                if (!BasicTypeOperations::TypeIdToTypeName(repository, tid))
                {
                    std::ostringstream os;
                    os<<"TypeId member "<<md->GetName()<<" does not refer to an existing type. Specified type name: "<<memberContent.data();
                    throw ParseError("Serialization error", os.str(), "", 174);
                }

                writer.WriteValue(memIx, arrIx, tid, false, true);
            }
            break;

        case InstanceIdMemberType:
        case ChannelIdMemberType:
        case HandlerIdMemberType:
            {
                Trim(memberContent.data());
                std::pair<DotsC_Int64, const char*> hash=SerializationUtils::StringToHash(memberContent.data());
                writer.WriteValue(memIx, arrIx, hash, false, true);
            }
            break;

        case EntityIdMemberType:
            {
                static const DotsC_TypeId EntityTypeId=LlufId_Generate64("Safir.Dob.Entity");

                boost::optional<std::string> typeIdString=memberContent.get_optional<std::string>("name");
                boost::optional<std::string> instanceIdString=memberContent.get_optional<std::string>("instanceId");
                if (!typeIdString)
                {
                    std::ostringstream os;
                    os<<"EntityId member '"<<md->GetName()<<"' is missing the name-element that specifies the type.";
                    throw ParseError("Serialization error", os.str(), "", 115);
                }
                if (!instanceIdString)
                {
                    std::ostringstream os;
                    os<<"EntityId member '"<<md->GetName()<<"' is missing the instanceId-element that specifies the instance.";
                    throw ParseError("Serialization error", os.str(), "", 116);
                }

                Trim(*typeIdString);
                Trim(*instanceIdString);
                std::pair<DotsC_EntityId, const char*> entityId=StringToEntityId(*typeIdString, *instanceIdString);

                if (!BasicTypeOperations::IsOfType(repository, ObjectMemberType, entityId.first.typeId, ObjectMemberType, EntityTypeId))
                {
                    std::ostringstream os;
                    os<<"EntityId member "<<md->GetName()<<" contains a typeId that does not refer to a subtype of Safir.Dob.Entity. Specified type name: "<<*typeIdString;
                    if (!BasicTypeOperations::TypeIdToTypeName(repository, entityId.first.typeId))
                    {
                        os<<". By the way, the type '"<<*typeIdString<<"'' does not exist at all!";
                    }
                    throw ParseError("Serialization error", os.str(), "", 173);
                }

                writer.WriteValue(memIx, arrIx, entityId, false, true);
            }
            break;

        case StringMemberType:
            {
                boost::optional<std::string> preserve=memberContent.get_optional<std::string>("<xmlattr>.xml:space");
                if (!preserve || *preserve!="preserve")
                {
                    Trim(memberContent.data());
                }
                //The only time we dont trim content
                writer.WriteValue(memIx, arrIx, memberContent.data().c_str(), false, true);
            }
            break;

        case ObjectMemberType:
            {
                //handled separately
            }
            break;

        case BinaryMemberType:
            {
                Trim(memberContent.data());
                std::string bin;
                if (!FromBase64(memberContent.data(), bin))
                {
                    std::ostringstream os;
                    os<<"Member "<<md->GetName()<<" of type binary containes invalid base64 data";
                    throw ParseError("Serialization error", os.str(), "",  117);
                }

                writer.WriteValue(memIx, arrIx, std::make_pair(static_cast<const char*>(&bin[0]), static_cast<DotsC_Int32>(bin.size())), false, true);
            }
            break;

            //  32 bit floats
        case Float32MemberType:
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
                Trim(memberContent.data());
                try
                {
                    DotsC_Float32 val=classic_string_cast<DotsC_Float32>(memberContent.data());
                    writer.WriteValue(memIx, arrIx, val, false, true);
                }
                catch (const boost::bad_lexical_cast&)
                {
                    std::ostringstream os;
                    os<<"Member "<<md->GetName()<<" of type Float32 contains invalid value. Value="<<memberContent.data();
                    throw ParseError("Serialization error", os.str(), "",  118);
                }
            }
            break;

            //  64 bit floats
        case Float64MemberType:
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
                Trim(memberContent.data());
                try
                {
                    DotsC_Float64 val=classic_string_cast<DotsC_Float64>(memberContent.data());
                    writer.WriteValue(memIx, arrIx, val, false, true);
                }
                catch (const boost::bad_lexical_cast&)
                {
                    std::ostringstream os;
                    os<<"Member "<<md->GetName()<<" of type Float64 contains invalid value. Value="<<memberContent.data();
                    throw ParseError("Serialization error", os.str(), "",  119);
                }
            }
            break;
        }
    }

    template <class WriterT, class KeyT>
    void SetMemberFromParameter(const typename WriterT::RepositoryType* repository,
                                const typename WriterT::MemberDescriptionType* md,
                                DotsC_MemberIndex memIx,
                                DotsC_Int32 arrIx,
                                const std::string& parameterName,
                                int parameterIndex,
                                const KeyT& key,
                                WriterT& writer)
    {
        //get the referenced parameter an make all the error checking
        Safir::Dob::Typesystem::ToolSupport::TypeUtilities::GetParameterByFullName<typename WriterT::RepositoryType> tmp;
        const typename WriterT::ParameterDescriptionType* param=tmp(repository, parameterName);

        if (!param)
        {
            std::ostringstream os;
            os<<"The parameter '"<<parameterName<<"' does not exist. Specified as valueRef in member "<<md->GetName();
            throw ParseError("Serialization error", os.str(), "", 120);
        }

        if (parameterIndex>=param->GetNumberOfValues())
        {
            std::ostringstream os;
            os<<"Parameter index out of range in valueRef. Member '"<<md->GetName()<<"'' is referencing parameter '"<<param->GetName()<<
                "' with index="<<parameterIndex<<" but the parameter ";
            if (param->GetCollectionType()==ArrayCollectionType)
                os<<"has arraySize="<<param->GetNumberOfValues();
            else
                os<<" is not an array.";

            throw ParseError("Serialization error", os.str(), "", 121);
        }
        if (param->GetMemberType()!=md->GetMemberType())
        {
            std::ostringstream os;
            os<<"Member "<<md->GetName()<<" is referencing a parameter of another type. Expected type is "<<BasicTypeOperations::MemberTypeToString(md->GetMemberType())<<
                " but parameter "<<param->GetName()<<" has type "<<BasicTypeOperations::MemberTypeToString(param->GetMemberType());
            throw ParseError("Serialization error", os.str(), "", 122);
        }
        if (param->GetMemberType()==ObjectMemberType)
        {
            std::ostringstream os;
            os<<"ValueRef is not supported for complex types (objects). Occurred in member "<<md->GetName();
            throw ParseError("Serialization error", os.str(), "", 123);
        }

        //when we get here we have found the referenced parameter and it seems to be valid for usage here

        if (md->GetCollectionType()==DictionaryCollectionType)
        {
            //if we're dealing with a dictionary, we must first set the key
            writer.WriteKey(memIx, key);
        }

        switch(md->GetMemberType())
        {
        case BooleanMemberType:
            {
                writer.WriteValue(memIx, arrIx, param->GetBoolValue(parameterIndex), false, true);
            }
            break;

        case EnumerationMemberType:
            {
                writer.WriteValue(memIx, arrIx, param->GetInt32Value(parameterIndex), false, true);
            }
            break;

        case Int32MemberType:
            {
                writer.WriteValue(memIx, arrIx, param->GetInt32Value(parameterIndex), false, true);
            }
            break;

        case Int64MemberType:
            {
                writer.WriteValue(memIx, arrIx, param->GetInt64Value(parameterIndex), false, true);
            }
            break;

        case TypeIdMemberType:
            {
                writer.WriteValue(memIx, arrIx, param->GetInt64Value(parameterIndex), false, true);
            }
            break;

        case InstanceIdMemberType:
        case ChannelIdMemberType:
        case HandlerIdMemberType:
            {            
                writer.WriteValue(memIx, arrIx, param->GetHashedValue(parameterIndex), false, true);
            }
            break;

        case EntityIdMemberType:
            {
                DotsC_EntityId entId;
                entId.typeId=param->GetInt64Value(parameterIndex);
                std::pair<DotsC_TypeId, const char*> instanceId=param->GetHashedValue(parameterIndex);
                entId.instanceId=instanceId.first;
                writer.WriteValue(memIx, arrIx, std::make_pair(entId, instanceId.second), false, true);
            }
            break;

        case StringMemberType:
            {
                writer.WriteValue(memIx, arrIx, param->GetStringValue(parameterIndex), false, true);
            }
            break;

        case ObjectMemberType:
            {
                //not supported
            }
            break;

        case BinaryMemberType:
            {
                writer.WriteValue(memIx, arrIx, param->GetBinaryValue(parameterIndex), false, true);
            }
            break;

            //  32 bit floats
        case Float32MemberType:
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
                writer.WriteValue(memIx, arrIx, param->GetFloat32Value(parameterIndex), false, true);
            }
            break;

            //  64 bit floats
        case Float64MemberType:
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
                writer.WriteValue(memIx, arrIx, param->GetFloat64Value(parameterIndex), false, true);
            }
            break;
        }
    }
}
}
}
}
}
} //end namespace Safir::Dob::Typesystem::ToolSupport::Internal

#ifdef _MSC_VER
#pragma warning(default:4127) //Get rid of warning that this if-expression is constant (comparing two constants)
#endif

#endif
