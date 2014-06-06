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
        std::string result;
        try
        {
            result = Safir::Utilities::Internal::Expansion::ExpandSpecial(str);
        }
        catch (const std::logic_error& e)
        {
            throw ParseError("Special variable expansion error", e.what(), "", 178);
        }

        try
        {
            result = Safir::Utilities::Internal::Expansion::ExpandEnvironment(result);
        }
        catch (const std::logic_error& e)
        {
            throw ParseError("Environment variable expansion error", e.what(), "", 179);
        }
        
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

    inline std::pair<DotsC_TypeId, const char*> StringToHash(const std::string& str)
    {
        std::pair<DotsC_TypeId, const char*> result(0, static_cast<const char*>(NULL));
        try
        {
            result.first=boost::lexical_cast<boost::int64_t>(str);
        }
        catch (const boost::bad_lexical_cast&)
        {
            result.first=LlufId_Generate64(str.c_str());
            result.second=str.c_str();
        }
        return result;
    }

    template <class BlobLayoutT>
    inline void CreateSpaceForDynamicMember(const BlobLayoutT& blobLayout,
                                            std::vector<char>& blob,
                                            char* & beginningOfUnused,
                                            size_t dynamicMemberSizeNeeded)
    {
        size_t usedSize=beginningOfUnused-&blob[0];
        assert(usedSize<=blob.size());        
        size_t unusedSize=blob.size()-usedSize;

        if (unusedSize>=dynamicMemberSizeNeeded)
        {
            return; //there is enough free bytes in blob
        }

        if (blob.capacity()-blob.size()<dynamicMemberSizeNeeded)
        {
            //std::cout<<"  Current blob: size="<<blob.size()<<", cap="<<blob.capacity()<<", dynNeed="<<dynamicMemberSizeNeeded<<", need="<<blob.size()+dynamicMemberSizeNeeded<<std::endl;
            //Blob is too small. A bigger blob must be allocated and the content of the old one must be copied.
            std::vector<char> tmp;
            tmp.swap(blob);
            blob.clear(); //unneccessary?
            blob.reserve((tmp.capacity()+dynamicMemberSizeNeeded)*2);
            blob.insert(blob.begin(), tmp.begin(), tmp.end());
            beginningOfUnused=&blob[0]+usedSize;
            //std::cout<<"New blob capacity: "<<blob.capacity()<<std::endl;
        }

        //When we get here, the blob is guaranteed to have capacity for the needed extra space. Just resize.
        blob.resize(blob.size()+dynamicMemberSizeNeeded);
        blobLayout.SetSize(&blob[0], static_cast<DotsC_Int32>(blob.size()));
        //std::cout<<"Blob grew, new size: "<<blob.size()<<std::endl;
    }

    template <class BlobLayoutT>
    void SetMemberValue(const typename BlobLayoutT::RepositoryType* repository,
                        const BlobLayoutT& blobLayout,
                        const typename BlobLayoutT::MemberDescriptionType* md,
                        DotsC_MemberIndex memIx,
                        DotsC_ArrayIndex arrIx,
                        boost::property_tree::ptree& memberContent,
                        std::vector<char>& blob,
                        char* &beginningOfUnused)
    {
        switch(md->GetMemberType())
        {
        case BooleanMemberType:
        {
            Trim(memberContent.data());
            bool boolVal=true;
            const std::string& val=memberContent.data();
            if (val=="True" || val=="true")
            {
                boolVal=true;
            }
            else if (val=="False" || val=="false")
            {
                boolVal=false;
            }
            else
            {
                boolVal=memberContent.get_value<bool>();
            }
            blobLayout.template SetMember<bool>(boolVal, &blob[0], memIx, arrIx);
            blobLayout.SetStatus(false, true, &blob[0], memIx, arrIx);
        }
            break;

        case EnumerationMemberType:
        {
            Trim(memberContent.data());
            const typename BlobLayoutT::EnumDescriptionType* ed=repository->GetEnum(md->GetTypeId());
            int enumOrdinal=ed->GetIndexOfValue(memberContent.data());
            if (enumOrdinal<0)
            {
                std::ostringstream os;
                os<<"Enumeration member '"<<md->GetName()<<"' contains an invalid value. Value="<<memberContent.data()<<" is not a value of enum type "<<ed->GetName();
                throw ParseError("Serialization error", os.str(), "", 114);
            }

            blobLayout.template SetMember<DotsC_Int32>(enumOrdinal, &blob[0], memIx, arrIx);
            blobLayout.SetStatus(false, true, &blob[0], memIx, arrIx);
        }
            break;

        case Int32MemberType:
        {
            Trim(memberContent.data());
            DotsC_Int32 val=memberContent.get_value<DotsC_Int32>();
            blobLayout.template SetMember<DotsC_Int32>(val, &blob[0], memIx, arrIx);
            blobLayout.SetStatus(false, true, &blob[0], memIx, arrIx);
        }
            break;

        case Int64MemberType:
        {
            Trim(memberContent.data());
            DotsC_Int64 val=memberContent.get_value<DotsC_Int64>();
            blobLayout.template SetMember<DotsC_Int64>(val, &blob[0], memIx, arrIx);
            blobLayout.SetStatus(false, true, &blob[0], memIx, arrIx);
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

            blobLayout.template SetMember<DotsC_TypeId>(tid, &blob[0], memIx, arrIx);
            blobLayout.SetStatus(false, true, &blob[0], memIx, arrIx);
        }
            break;

        case InstanceIdMemberType:
        case ChannelIdMemberType:
        case HandlerIdMemberType:
        {
            Trim(memberContent.data());
            std::pair<DotsC_TypeId, const char*> hash=SerializationUtils::StringToHash(memberContent.data());
            if (hash.second!=NULL)
            {
                size_t numBytesNeeded=memberContent.data().size()+1+sizeof(DotsC_Int64)+sizeof(DotsC_Int32); //hash+stringLength+string
                SerializationUtils::CreateSpaceForDynamicMember(blobLayout, blob, beginningOfUnused, numBytesNeeded);
            }
            blobLayout.CreateAndSetMemberWithOptionalString(&blob[0], hash.first, hash.second, static_cast<Size>(memberContent.data().size()+1), memIx, arrIx, false, beginningOfUnused);
            blobLayout.SetStatus(false, true, &blob[0], memIx, arrIx);
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
            DotsC_TypeId tid=SerializationUtils::StringToTypeId(*typeIdString);

            if (!BasicTypeOperations::IsOfType(repository, ObjectMemberType, tid, ObjectMemberType, EntityTypeId))
            {
                std::ostringstream os;
                os<<"EntityId member "<<md->GetName()<<" contains a typeId that does not refer to a subtype of Safir.Dob.Entity. Specified type name: "<<*typeIdString;
                if (!BasicTypeOperations::TypeIdToTypeName(repository, tid))
                {
                    os<<". By the way, the type '"<<*typeIdString<<"'' does not exist at all!";
                }
                throw ParseError("Serialization error", os.str(), "", 173);
            }

            Trim(*instanceIdString);
            std::pair<DotsC_TypeId, const char*> instanceId=SerializationUtils::StringToHash(*instanceIdString);
            if (instanceId.second!=NULL)
            {
                size_t numBytesNeeded=instanceIdString->size()+1+sizeof(DotsC_EntityId)+sizeof(DotsC_Int32); //(typeId+hash)+stringLength+string
                SerializationUtils::CreateSpaceForDynamicMember(blobLayout, blob, beginningOfUnused, numBytesNeeded);
            }
            DotsC_EntityId eid={tid, instanceId.first};
            blobLayout.CreateAndSetMemberWithOptionalString(&blob[0], eid, instanceId.second, static_cast<Size>(instanceIdString->size()+1), memIx, arrIx, false, beginningOfUnused);
            blobLayout.SetStatus(false, true, &blob[0], memIx, arrIx);
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
            size_t numBytesNeeded=std::min(memberContent.data().size(), static_cast<size_t>(md->GetMaxLength()))+1; //add one for '\0'
            SerializationUtils::CreateSpaceForDynamicMember(blobLayout, blob, beginningOfUnused, numBytesNeeded);
            char* writeString=beginningOfUnused;
            blobLayout.CreateStringMember(&blob[0], static_cast<Size>(numBytesNeeded), memIx, arrIx, false, beginningOfUnused);
            strncpy(writeString, memberContent.data().c_str(), numBytesNeeded-1);
            blobLayout.SetStatus(false, true, &blob[0], memIx, arrIx);
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
            SerializationUtils::CreateSpaceForDynamicMember(blobLayout, blob, beginningOfUnused, bin.size());
            char* writeBinary=beginningOfUnused;
            blobLayout.CreateBinaryMember(&blob[0], static_cast<Size>(bin.size()), memIx, arrIx, false, beginningOfUnused);
            memcpy(writeBinary, &bin[0], bin.size());
            blobLayout.SetStatus(false, true, &blob[0], memIx, arrIx);
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
                blobLayout.template SetMember<DotsC_Float32>(val, &blob[0], memIx, arrIx);
                blobLayout.SetStatus(false, true, &blob[0], memIx, arrIx);
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
                blobLayout.template SetMember<DotsC_Float64>(val, &blob[0], memIx, arrIx);
                blobLayout.SetStatus(false, true, &blob[0], memIx, arrIx);
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

    template <class BlobLayoutT>
    void SetMemberFromParameter(const typename BlobLayoutT::RepositoryType* repository,
                                const BlobLayoutT& blobLayout,
                                const typename BlobLayoutT::MemberDescriptionType* md,
                                DotsC_MemberIndex memIx,
                                DotsC_ArrayIndex arrIx,
                                const std::string& parameterName,
                                int parameterIndex,
                                std::vector<char>& blob,
                                char* &beginningOfUnused)
    {
        //get the referenced parameter an make all the error checking
        Safir::Dob::Typesystem::ToolSupport::TypeUtilities::GetParameterByFullName<typename BlobLayoutT::RepositoryType> tmp;
        const typename BlobLayoutT::ParameterDescriptionType* param=tmp(repository, parameterName);

        if (!param)
        {
            std::ostringstream os;
            os<<"The parameter '"<<parameterName<<"' does not exist. Specified as valueRef in member "<<md->GetName();
            throw ParseError("Serialization error", os.str(), "", 120);
        }

        if (parameterIndex>=param->GetArraySize())
        {
            std::ostringstream os;
            os<<"Parameter index out of range in valueRef. Member '"<<md->GetName()<<"'' is referencing parameter '"<<param->GetName()<<
                "' with index="<<parameterIndex<<" but the parameter ";
            if (param->IsArray())
                os<<"has arraySize="<<param->GetArraySize();
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
        switch(md->GetMemberType())
        {
        case BooleanMemberType:
        {
            blobLayout.template SetMember<bool>(param->GetBoolValue(parameterIndex), &blob[0], memIx, arrIx);
            blobLayout.SetStatus(false, true, &blob[0], memIx, arrIx);
        }
            break;

        case EnumerationMemberType:
        {
            blobLayout.template SetMember<DotsC_Int32>(param->GetInt32Value(parameterIndex), &blob[0], memIx, arrIx);
            blobLayout.SetStatus(false, true, &blob[0], memIx, arrIx);
        }
            break;

        case Int32MemberType:
        {
            blobLayout.template SetMember<DotsC_Int32>(param->GetInt32Value(parameterIndex), &blob[0], memIx, arrIx);
            blobLayout.SetStatus(false, true, &blob[0], memIx, arrIx);
        }
            break;

        case Int64MemberType:
        {
            blobLayout.template SetMember<DotsC_Int64>(param->GetInt64Value(parameterIndex), &blob[0], memIx, arrIx);
            blobLayout.SetStatus(false, true, &blob[0], memIx, arrIx);
        }
            break;

        case TypeIdMemberType:
        {
            blobLayout.template SetMember<DotsC_TypeId>(param->GetInt64Value(parameterIndex), &blob[0], memIx, arrIx);
            blobLayout.SetStatus(false, true, &blob[0], memIx, arrIx);
        }
            break;

        case InstanceIdMemberType:
        case ChannelIdMemberType:
        case HandlerIdMemberType:
        {
            std::pair<DotsC_TypeId, const char*> hash=param->GetHashedValue(parameterIndex);
            size_t strLen=1; //null char
            if (hash.second!=NULL)
            {
                strLen+=strlen(hash.second);
                size_t numBytesNeeded=strLen+sizeof(DotsC_Int64)+sizeof(DotsC_Int32); //hash+stringLength+string
                SerializationUtils::CreateSpaceForDynamicMember(blobLayout, blob, beginningOfUnused, numBytesNeeded);
            }
            blobLayout.CreateAndSetMemberWithOptionalString(&blob[0], hash.first, hash.second, static_cast<Size>(strLen), memIx, arrIx, false, beginningOfUnused);
            blobLayout.SetStatus(false, true, &blob[0], memIx, arrIx);
        }
            break;

        case EntityIdMemberType:
        {
            DotsC_TypeId tid=param->GetInt64Value(parameterIndex);
            std::pair<DotsC_TypeId, const char*> instanceId=param->GetHashedValue(parameterIndex);
            size_t strLen=1; //null char
            if (instanceId.second!=NULL)
            {
                strLen+=strlen(instanceId.second);
                size_t numBytesNeeded=strLen+sizeof(DotsC_EntityId)+sizeof(DotsC_Int32); //(typeId+hash)+stringLength+string
                SerializationUtils::CreateSpaceForDynamicMember(blobLayout, blob, beginningOfUnused, numBytesNeeded);
            }
            DotsC_EntityId eid={tid, instanceId.first};
            blobLayout.CreateAndSetMemberWithOptionalString(&blob[0], eid, instanceId.second, static_cast<Size>(strLen), memIx, arrIx, false, beginningOfUnused);
            blobLayout.SetStatus(false, true, &blob[0], memIx, arrIx);
        }
            break;

        case StringMemberType:
        {
            const char* str=param->GetStringValue(parameterIndex);
            size_t strLen=strlen(str);
            size_t numBytesNeeded=std::min(strLen, static_cast<size_t>(md->GetMaxLength()))+1; //add one for '\0'
            SerializationUtils::CreateSpaceForDynamicMember(blobLayout, blob, beginningOfUnused, numBytesNeeded);
            char* writeString=beginningOfUnused;
            blobLayout.CreateStringMember(&blob[0], static_cast<Size>(numBytesNeeded), memIx, arrIx, false, beginningOfUnused);
            strncpy(writeString, str, numBytesNeeded-1);
            blobLayout.SetStatus(false, true, &blob[0], memIx, arrIx);
        }
            break;

        case ObjectMemberType:
        {
            //not supported
        }
            break;

        case BinaryMemberType:
        {
            std::pair<const char*, size_t> bin=param->GetBinaryValue(parameterIndex);
            SerializationUtils::CreateSpaceForDynamicMember(blobLayout, blob, beginningOfUnused, bin.second);
            char* writeBinary=beginningOfUnused;
            blobLayout.CreateBinaryMember(&blob[0], static_cast<Size>(bin.second), memIx, arrIx, false, beginningOfUnused);
            memcpy(writeBinary, bin.first, bin.second);
            blobLayout.SetStatus(false, true, &blob[0], memIx, arrIx);
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
            blobLayout.template SetMember<DotsC_Float32>(param->GetFloat32Value(parameterIndex), &blob[0], memIx, arrIx);
            blobLayout.SetStatus(false, true, &blob[0], memIx, arrIx);
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
            blobLayout.template SetMember<DotsC_Float64>(param->GetFloat64Value(parameterIndex), &blob[0], memIx, arrIx);
            blobLayout.SetStatus(false, true, &blob[0], memIx, arrIx);
        }
            break;
        }
    }
}
}
}
}
}
} //end namespace Safir::Dob::Typesystem::Internal::Internal

#ifdef _MSC_VER
#pragma warning(default:4127) //Get rid of warning that this if-expression is constant (comparing two constants)
#endif

#endif
