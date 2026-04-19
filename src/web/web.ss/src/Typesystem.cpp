/******************************************************************************
*
* Copyright Saab AB, 2026 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson
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
#include <vector>
#include <set>
#include <sstream>
#include <boost/filesystem.hpp>
#include <Safir/Utilities/Internal/ConfigReader.h>
#include <Safir/Dob/Typesystem/ToolSupport/TypeParser.h>
#include <Safir/Dob/Typesystem/ToolSupport/TypeUtilities.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Dob/Typesystem/Parameters.h>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Web/Typesystem/TypeHierarchy.h>

#include "Typesystem.h"

namespace sd = Safir::Dob;
namespace ts = Safir::Dob::Typesystem;
namespace sup = Safir::Dob::Typesystem::ToolSupport;

using namespace Safir::Web::Typesystem;

typedef std::shared_ptr<const Safir::Dob::Typesystem::ToolSupport::TypeRepository> Repository;

namespace Typesystem
{
    namespace
    {
        std::wstring Wstr(const char* str)
        {
            if (str)
                return ts::Utilities::ToWstring(str);
            else
                return L"";
        }

        Repository CreateRepository()
        {
            std::vector<boost::filesystem::path> directories;
            Safir::Utilities::Internal::ConfigReader reader;

            //get all dou directory strings
            std::vector<std::pair<std::string,std::string> > dirs = Safir::Utilities::Internal::ConfigHelper::GetDouDirectories(reader);

            for (std::vector<std::pair<std::string,std::string> >::const_iterator it = dirs.begin();
                 it != dirs.end(); ++it)
            {
                boost::filesystem::path douDirectory(it->second);

                if (!boost::filesystem::exists(douDirectory) || !boost::filesystem::is_directory(douDirectory))
                {
                    std::cout<<"dou_directory '"+douDirectory.string()+"' in typesystem.ini does not appear to be a directory"<<std::endl;
                    exit(22);
                }

                directories.push_back(douDirectory);
            }

            try
            {
                //localRepository=Safir::Dob::Typesystem::ToolSupport::ParseTypeDefinitions(directories);
                auto localRepository=Safir::Dob::Typesystem::ToolSupport::ParseTypeDefinitions(directories);
                return localRepository;
            }
            catch(const Safir::Dob::Typesystem::ToolSupport::ParseError& err)
            {
                std::cout<<"********** Parse Error **********************************************"<<std::endl;
                std::cout<<"* Label: "<<err.Label()<<std::endl;
                std::cout<<"* Descr: "<<err.Description()<<std::endl;
                std::cout<<"* File:  "<<err.File()<<std::endl;
                std::cout<<"* ErrId: "<<err.ErrorId()<<std::endl;
                std::cout<<"*********************************************************************"<<std::endl;
                throw err;
            }
        }

        void InsertEnums(const Repository& rep, TypeHierarchyPtr& h)
        {
            std::set<ts::TypeId> typeIds;
            rep->GetAllEnumTypeIds(typeIds);

            for (auto it = typeIds.begin(); it != typeIds.end(); ++it)
            {
                EnumTypePtr et=EnumType::Create();
                const sup::EnumDescription* ed=rep->GetEnum(*it);
                //et->Summary()=Wstr(ed->Summary());
                et->Name()=Wstr(ed->GetName());
                et->SourceFile()=Wstr(ed->FileName());
                for (int i=0; i<ed->GetNumberOfValues(); ++i)
                {
                    et->Values().push_back(Wstr(ed->GetValueName(i)));
                }

                h->Enums().push_back(et);
            }
        }

        ClassTypePtr ToClassType(const Repository& rep, const sup::ClassDescription* cd)
        {
            ClassTypePtr ct=ClassType::Create();

            //ct->Summary()=Wstr(cd->Summary());
            ct->Name()=Wstr(cd->GetName());
            ct->SourceFile()=Wstr(cd->FileName());

            for (int i=0; i<cd->GetNumberOfMembers(); ++i)
            {
                //create member
                const sup::MemberDescription* md=cd->GetMember(i);
                MemberPtr member=Member::Create();
                //member->Summary()=Wstr(md->Summary());
                member->Name()=Wstr(md->GetName());
                member->Type()=Wstr(sup::TypeUtilities::GetTypeName(rep.get(), md));
                switch(md->GetCollectionType())
                {
                case SingleValueCollectionType:
                    break;
                case ArrayCollectionType:
                    member->ArraySize()=md->GetArraySize();
                    break;
                case SequenceCollectionType:
                    member->ArraySize()=-1;
                    break;
                case DictionaryCollectionType:
                {
                    if (md->GetKeyType()==EnumerationMemberType)
                        member->DictionaryKeyType()=Wstr(sup::TypeUtilities::GetTypeName(rep.get(), md->GetKeyTypeId()));
                    else
                        member->DictionaryKeyType()=Wstr(sup::TypeUtilities::GetTypeName(md->GetKeyType()));
                }
                    break;
                }
                ct->Members().push_back(member);
            }

            for (int i=0; i<cd->GetNumberOfDescendants(); i++)
            {
                const sup::ClassDescription* des=cd->GetDescendant(i);
                auto desClassType=ToClassType(rep, des);
                ct->Descendants().push_back(desClassType);
            }

            return ct;
        }

        void InsertClasses(const Repository& rep, TypeHierarchyPtr& h)
        {
            const sup::ClassDescription* cd=rep->GetClass(ts::Object::ClassTypeId);
            h->RootClass()=ToClassType(rep, cd);
        }

        std::string TypeHierarchyAsJson()
        {
            auto rep=CreateRepository();

            TypeHierarchyPtr h=TypeHierarchy::Create();
            InsertEnums(rep, h);
            InsertClasses(rep, h);
            std::string json=ts::Utilities::ToUtf8(ts::Serialization::ToJson(h));
            return json;
        }
    }

    std::string GetTypeHierarchy()
    {
        static std::string jsonStr=TypeHierarchyAsJson();
        return jsonStr;
    }

    // ---------------------------------------------------------------------------
    // GetParameter helpers
    // ---------------------------------------------------------------------------
    namespace
    {
        // Serialize a single scalar value (JSON fragment, no surrounding quotes added for numbers/bools)
        std::string SerializeParameterValue(ts::TypeId typeId, int paramIdx, int arrIdx,
                                            ts::MemberType memberType, ts::TypeId memberTypeId)
        {
            std::ostringstream os;
            switch (memberType)
            {
            case BooleanMemberType:
                os << (ts::Parameters::GetBoolean(typeId, paramIdx, arrIdx) ? "true" : "false");
                break;
            case EnumerationMemberType:
            {
                auto ordinal = ts::Parameters::GetEnumeration(typeId, paramIdx, arrIdx);
                auto name = ts::Operations::GetEnumerationValueName(memberTypeId, ordinal);
                os << '"' << ts::Utilities::ToUtf8(name) << '"';
                break;
            }
            case Int32MemberType:
                os << ts::Parameters::GetInt32(typeId, paramIdx, arrIdx);
                break;
            case Int64MemberType:
                os << ts::Parameters::GetInt64(typeId, paramIdx, arrIdx);
                break;
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
                os << ts::Parameters::GetFloat32(typeId, paramIdx, arrIdx);
                break;
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
                os << ts::Parameters::GetFloat64(typeId, paramIdx, arrIdx);
                break;
            case TypeIdMemberType:
            {
                auto val = ts::Parameters::GetTypeId(typeId, paramIdx, arrIdx);
                os << '"' << ts::Utilities::ToUtf8(ts::Operations::GetName(val)) << '"';
                break;
            }
            case InstanceIdMemberType:
            {
                auto val = ts::Parameters::GetInstanceId(typeId, paramIdx, arrIdx);
                os << '"' << ts::Utilities::ToUtf8(val.ToString()) << '"';
                break;
            }
            case EntityIdMemberType:
            {
                auto val = ts::Parameters::GetEntityId(typeId, paramIdx, arrIdx);
                os << "{\"typeId\":\"" << ts::Utilities::ToUtf8(ts::Operations::GetName(val.GetTypeId()))
                   << "\",\"instanceId\":\"" << ts::Utilities::ToUtf8(val.GetInstanceId().ToString()) << "\"}";
                break;
            }
            case ChannelIdMemberType:
            {
                auto val = ts::Parameters::GetChannelId(typeId, paramIdx, arrIdx);
                os << '"' << ts::Utilities::ToUtf8(val.ToString()) << '"';
                break;
            }
            case HandlerIdMemberType:
            {
                auto val = ts::Parameters::GetHandlerId(typeId, paramIdx, arrIdx);
                os << '"' << ts::Utilities::ToUtf8(val.ToString()) << '"';
                break;
            }
            case StringMemberType:
            {
                auto val = ts::Utilities::ToUtf8(ts::Parameters::GetString(typeId, paramIdx, arrIdx));
                // Escape the string for JSON
                std::ostringstream escaped;
                for (const unsigned char c : val)
                {
                    switch (c)
                    {
                    case '"':  escaped << "\\\""; break;
                    case '\\': escaped << "\\\\"; break;
                    case '\n': escaped << "\\n"; break;
                    case '\r': escaped << "\\r"; break;
                    case '\t': escaped << "\\t"; break;
                    default:   escaped << static_cast<char>(c); break;
                    }
                }
                os << '"' << escaped.str() << '"';
                break;
            }
            case BinaryMemberType:
            {
                auto val = ts::Parameters::GetBinary(typeId, paramIdx, arrIdx);
                os << '"' << ts::Utilities::BinaryToBase64(val) << '"';
                break;
            }
            case ObjectMemberType:
            {
                auto obj = ts::Parameters::GetObject(typeId, paramIdx, arrIdx);
                os << ts::Utilities::ToUtf8(ts::Serialization::ToJson(obj));
                break;
            }
            default:
                os << "null";
                break;
            }
            return os.str();
        }

        // Serialize a dictionary key to JSON fragment
        std::string SerializeDictionaryKey(ts::TypeId typeId, int paramIdx, int arrIdx,
                                           ts::MemberType keyType, ts::TypeId keyTypeId)
        {
            std::ostringstream os;
            switch (keyType)
            {
            case EnumerationMemberType:
            {
                auto ordinal = ts::Parameters::GetEnumerationDictionaryKey(typeId, paramIdx, arrIdx);
                auto name = ts::Operations::GetEnumerationValueName(keyTypeId, ordinal);
                os << '"' << ts::Utilities::ToUtf8(name) << '"';
                break;
            }
            case Int32MemberType:
                os << ts::Parameters::GetInt32DictionaryKey(typeId, paramIdx, arrIdx);
                break;
            case Int64MemberType:
                os << ts::Parameters::GetInt64DictionaryKey(typeId, paramIdx, arrIdx);
                break;
            case TypeIdMemberType:
            {
                auto val = ts::Parameters::GetTypeIdDictionaryKey(typeId, paramIdx, arrIdx);
                os << '"' << ts::Utilities::ToUtf8(ts::Operations::GetName(val)) << '"';
                break;
            }
            case InstanceIdMemberType:
            {
                auto val = ts::Parameters::GetInstanceIdDictionaryKey(typeId, paramIdx, arrIdx);
                os << '"' << ts::Utilities::ToUtf8(val.ToString()) << '"';
                break;
            }
            case EntityIdMemberType:
            {
                auto val = ts::Parameters::GetEntityIdDictionaryKey(typeId, paramIdx, arrIdx);
                os << "{\"typeId\":\"" << ts::Utilities::ToUtf8(ts::Operations::GetName(val.GetTypeId()))
                   << "\",\"instanceId\":\"" << ts::Utilities::ToUtf8(val.GetInstanceId().ToString()) << "\"}";
                break;
            }
            case ChannelIdMemberType:
            {
                auto val = ts::Parameters::GetChannelIdDictionaryKey(typeId, paramIdx, arrIdx);
                os << '"' << ts::Utilities::ToUtf8(val.ToString()) << '"';
                break;
            }
            case HandlerIdMemberType:
            {
                auto val = ts::Parameters::GetHandlerIdDictionaryKey(typeId, paramIdx, arrIdx);
                os << '"' << ts::Utilities::ToUtf8(val.ToString()) << '"';
                break;
            }
            case StringMemberType:
            {
                auto val = ts::Utilities::ToUtf8(ts::Parameters::GetStringDictionaryKey(typeId, paramIdx, arrIdx));
                std::ostringstream escaped;
                for (const unsigned char c : val)
                {
                    switch (c)
                    {
                    case '"':  escaped << "\\\""; break;
                    case '\\': escaped << "\\\\"; break;
                    case '\n': escaped << "\\n"; break;
                    case '\r': escaped << "\\r"; break;
                    case '\t': escaped << "\\t"; break;
                    default:   escaped << static_cast<char>(c); break;
                    }
                }
                os << '"' << escaped.str() << '"';
                break;
            }
            default:
                os << "null";
                break;
            }
            return os.str();
        }

    } // anonymous namespace

    std::string GetParameter(const std::string& fullyQualifiedName)
    {
        // Split "TypeName.ParameterName" at the last dot
        const auto dot = fullyQualifiedName.rfind('.');
        if (dot == std::string::npos)
            throw ts::IllegalValueException(L"'parameter' must be a fully qualified name like 'MyNamespace.MyClass.MyParameter'", __WFILE__, __LINE__);

        const auto typeName  = fullyQualifiedName.substr(0, dot);
        const auto paramName = fullyQualifiedName.substr(dot + 1);

        // Resolve type and parameter — both throw IllegalValueException on unknown names
        const auto typeId   = ts::Operations::GetTypeId(ts::Utilities::ToWstring(typeName));
        const auto paramIdx = ts::Parameters::GetIndex(typeId, ts::Utilities::ToWstring(paramName));

        ts::MemberType memberType;
        ts::MemberType keyType;
        std::wstring   wParamName;
        ts::TypeId     memberTypeId;
        ts::TypeId     keyTypeId;
        ts::CollectionType collectionType;
        ts::Int32      numberOfValues;
        ts::Parameters::GetInfo(typeId, paramIdx, memberType, keyType, wParamName, memberTypeId, keyTypeId, collectionType, numberOfValues);

        std::ostringstream os;
        const bool isSingle = (collectionType == SingleValueCollectionType);
        if (!isSingle) os << "[";
        for (ts::Int32 i = 0; i < numberOfValues; ++i)
        {
            if (i > 0) os << ",";
            if (collectionType == DictionaryCollectionType)
            {
                os << "{\"key\":" << SerializeDictionaryKey(typeId, paramIdx, i, keyType, keyTypeId)
                   << ",\"value\":" << SerializeParameterValue(typeId, paramIdx, i, memberType, memberTypeId) << "}";
            }
            else
            {
                os << SerializeParameterValue(typeId, paramIdx, i, memberType, memberTypeId);
            }
        }
        if (!isSingle) os << "]";
        return os.str();
    }

}
