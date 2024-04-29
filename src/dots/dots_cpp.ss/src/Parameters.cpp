/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / stlrha
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

#include <Safir/Dob/Typesystem/Parameters.h>
#include <Safir/Dob/Typesystem/Exceptions.h>
#include <Safir/Dob/Typesystem/Utilities.h>
#include <Safir/Dob/Typesystem/Object.h>
#include <Safir/Dob/Typesystem/ObjectFactory.h>
#include <Safir/Dob/Typesystem/Internal/Kernel.h>
#include <Safir/Dob/Typesystem/Internal/InternalOperations.h>
#include <sstream>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Parameters
{
    //Parameter handling
    Int32
    GetNumberOfParameters(const TypeId typeId)
    {
        const Int32 parameters = DotsC_GetNumberOfParameters(typeId);
        if (parameters == -1)
        {
            throw IllegalValueException(L"There is no such type defined", __WFILE__, __LINE__);
        }
        else
        {
            return parameters;
        }
    }

    ParameterIndex
    GetIndex(const TypeId typeId,
             const std::wstring& parameterName)
    {
        const ParameterIndex result = DotsC_GetParameterId(typeId, Utilities::ToUtf8(parameterName).c_str());
        if (result == -1)
        {
            const char* typeName = DotsC_GetTypeName(typeId);
            std::wostringstream ostr;

            if (typeName != nullptr)
            {
                ostr << "The class '"<< Safir::Dob::Typesystem::Utilities::ToWstring(typeName) <<
                        "' does not have a parameter called '" << parameterName << "'!";
            }
            else
            {
                ostr << L"Trying to read parameter '" << parameterName << "' from a class that does not exist. TypeId=" << typeId;
            }

            throw IllegalValueException(ostr.str(), __WFILE__, __LINE__);
        }
        else
        {
            return result;
        }
    }

    const std::wstring
    GetName(const TypeId typeId,
            const ParameterIndex parameter)
    {
        DotsC_MemberType parameterType;
        DotsC_MemberType keyType;
        const char* parameterName;
        DotsC_TypeId complexTypeId;
        DotsC_TypeId keyTypeId;
        DotsC_CollectionType collectionType;
        DotsC_Int32 numberOfValues;
        DotsC_GetParameterInfo(typeId, parameter, parameterType, keyType, parameterName, complexTypeId, keyTypeId, collectionType, numberOfValues);

        return Utilities::ToWstring(parameterName);
    }

    std::wstring GetTypeName(const Dob::Typesystem::TypeId typeId, const Dob::Typesystem::ParameterIndex parameter)
    {
        DotsC_MemberType parameterType;
        DotsC_MemberType keyType;
        const char* parameterName;
        DotsC_TypeId complexTypeId;
        DotsC_TypeId keyTypeId;
        DotsC_CollectionType collectionType;
        DotsC_Int32 numberOfValues;
        DotsC_GetParameterInfo(typeId, parameter, parameterType, keyType, parameterName, complexTypeId, keyTypeId, collectionType, numberOfValues);

        if (parameterType==ObjectMemberType || parameterType==EnumerationMemberType)
        {
            return Safir::Dob::Typesystem::Utilities::ToWstring(DotsC_GetTypeName(complexTypeId));
        }
        else
        {
            return Safir::Dob::Typesystem::Utilities::ToWstring(DotsC_MemberTypeName(parameterType));
        }
    }

    MemberType
    GetType(const TypeId typeId,
            const ParameterIndex parameter)
    {
        DotsC_MemberType parameterType;
        DotsC_MemberType keyType;
        const char* parameterName;
        DotsC_TypeId complexTypeId;
        DotsC_TypeId keyTypeId;
        DotsC_CollectionType collectionType;
        DotsC_Int32 numberOfValues;
        DotsC_GetParameterInfo(typeId, parameter, parameterType, keyType, parameterName, complexTypeId, keyTypeId, collectionType, numberOfValues);

        return parameterType;
    }

    Int32
    GetArraySize(const TypeId typeId,
                 const ParameterIndex parameter)
    {
        return GetCollectionSize(typeId, parameter);
    }

    Int32
    GetCollectionSize(const TypeId typeId,
                      const ParameterIndex parameter)
    {
        DotsC_MemberType parameterType;
        DotsC_MemberType keyType;
        const char* parameterName;
        DotsC_TypeId complexTypeId;
        DotsC_TypeId keyTypeId;
        DotsC_CollectionType collectionType;
        DotsC_Int32 numberOfValues;
        DotsC_GetParameterInfo(typeId, parameter, parameterType, keyType, parameterName, complexTypeId, keyTypeId, collectionType, numberOfValues);

        return numberOfValues;
    }

    //Get parameters
    bool
    GetBoolean(const TypeId typeId,
               const ParameterIndex parameter,
               const Dob::Typesystem::ArrayIndex index)
    {
        bool b;
        DotsC_GetBooleanParameter(typeId, parameter, index, b);
        return b;
    }

    Int32
    GetEnumeration(const TypeId typeId,
                   const ParameterIndex parameter,
                   const Dob::Typesystem::ArrayIndex index)
    {
        Int32 i;
        DotsC_GetEnumerationParameter(typeId, parameter, index, DotsC_ValueMode, i);
        return i;
    }

    Int32
    GetInt32(const TypeId typeId,
             const ParameterIndex parameter,
             const Dob::Typesystem::ArrayIndex index)
    {
        Int32 i;
        DotsC_GetInt32Parameter(typeId, parameter, index, DotsC_ValueMode, i);
        return i;
    }

    Int64
    GetInt64(const TypeId typeId,
             const ParameterIndex parameter,
             const Dob::Typesystem::ArrayIndex index)
    {
        Int64 i;
        DotsC_GetInt64Parameter(typeId, parameter, index, DotsC_ValueMode, i);
        return i;
    }

    Float32
    GetFloat32(const TypeId typeId,
               const ParameterIndex parameter,
               const Dob::Typesystem::ArrayIndex index)
    {
        Float32 i;
        DotsC_GetFloat32Parameter(typeId, parameter, index, i);
        return i;
    }

    Float64
    GetFloat64(const TypeId typeId,
               const ParameterIndex parameter,
               const Dob::Typesystem::ArrayIndex index)
    {
        Float64 i;
        DotsC_GetFloat64Parameter(typeId, parameter, index, i);
        return i;
    }

    TypeId
    GetTypeId(const TypeId typeId,
              const ParameterIndex parameter,
              const Dob::Typesystem::ArrayIndex index)
    {
        TypeId t;
        DotsC_GetTypeIdParameter(typeId, parameter, index, DotsC_ValueMode, t);
        return t;
    }

    const InstanceId
    GetInstanceId(const TypeId typeId,
                  const ParameterIndex parameter,
                  const Dob::Typesystem::ArrayIndex index)
    {
        Int64 hashVal;
        const char * strVal;
        DotsC_GetHashedIdParameter(typeId, parameter, index, DotsC_ValueMode, hashVal, strVal);
        if (strVal == NULL)
        {
            return InstanceId(hashVal);
        }
        else
        {
            return InstanceId(hashVal, Utilities::ToWstring(strVal));
        }
    }

    const EntityId
    GetEntityId(const TypeId typeId,
                const ParameterIndex parameter,
                const Dob::Typesystem::ArrayIndex index)
    {
        DotsC_EntityId eid;
        const char * instanceIdStr;
        DotsC_GetEntityIdParameter(typeId, parameter, index, DotsC_ValueMode, eid, instanceIdStr);
        if(instanceIdStr==NULL)
        {
            return EntityId(eid.typeId,InstanceId(eid.instanceId));
        }
        else
        {
            return EntityId(eid.typeId,InstanceId(eid.instanceId,Utilities::ToWstring(instanceIdStr)));
        }
    }


    const ChannelId
    GetChannelId(const TypeId typeId,
                  const ParameterIndex parameter,
                  const Dob::Typesystem::ArrayIndex index)
    {
        Int64 hashVal;
        const char * strVal;
        DotsC_GetHashedIdParameter(typeId, parameter, index, DotsC_ValueMode, hashVal, strVal);
        if (strVal == NULL)
        {
            return ChannelId(hashVal);
        }
        else
        {
            return ChannelId(hashVal,Utilities::ToWstring(strVal));
        }
    }


    const HandlerId
    GetHandlerId(const TypeId typeId,
                  const ParameterIndex parameter,
                  const Dob::Typesystem::ArrayIndex index)
    {
        Int64 hashVal;
        const char * strVal;
        DotsC_GetHashedIdParameter(typeId, parameter, index, DotsC_ValueMode, hashVal, strVal);
        if (strVal == NULL)
        {
            return HandlerId(hashVal);
        }
        else
        {
            return HandlerId(hashVal,Utilities::ToWstring(strVal));
        }
    }


    const std::wstring
    GetString(const TypeId typeId,
              const ParameterIndex parameter,
              const Dob::Typesystem::ArrayIndex index)
    {
        const char* tmp;
        DotsC_GetStringParameter(typeId, parameter, index, DotsC_ValueMode, tmp);
        return Utilities::ToWstring(tmp);
    }

    const ObjectPtr
    GetObject(const TypeId typeId,
              const ParameterIndex parameter,
              const Dob::Typesystem::ArrayIndex index)
    {
        const char * tmp;
        DotsC_GetObjectParameter(typeId, parameter, index, tmp);

        ObjectPtr obj = ObjectFactory::Instance().CreateObject(tmp);
        obj->SetChanged(false);
        return obj;
    }

    const Dob::Typesystem::Binary
    GetBinary(  const Dob::Typesystem::TypeId typeId,
                const Dob::Typesystem::ParameterIndex parameter,
                const Dob::Typesystem::ArrayIndex index)
    {
        DotsC_Int32 size;
        const char * tmp;
        DotsC_GetBinaryParameter(typeId, parameter, index, tmp, size);
        Safir::Dob::Typesystem::Binary binary(tmp, tmp+size);
        return binary;
    }

    Int32 DictionaryKeyToIndex(const TypeId typeId,
                               const ParameterIndex parameter,
                               const Int32 key)
    {
        return DotsC_DictionaryInt32KeyToIndex(typeId, parameter, key);
    }

    Int32 DictionaryKeyToIndex(const TypeId typeId,
                               const ParameterIndex parameter,
                               const Int64 key)
    {
        return DotsC_DictionaryInt64KeyToIndex(typeId, parameter, key);
    }


    Int32 DictionaryKeyToIndex(const TypeId typeId,
                               const ParameterIndex parameter,
                               const std::wstring& key)
    {
        return DotsC_DictionaryStringKeyToIndex(typeId, parameter, Utilities::ToUtf8(key).c_str());
    }


    Int32 DictionaryKeyToIndex(const TypeId typeId,
                               const ParameterIndex parameter,
                               const EntityId& key)
    {
        DotsC_EntityId eid={key.GetTypeId(), key.GetInstanceId().GetRawValue()};
        return DotsC_DictionaryEntityIdKeyToIndex(typeId, parameter, eid);
    }

    Int32 DictionaryKeyToIndex(const TypeId typeId,
                               const ParameterIndex parameter,
                               const InstanceId& key)
    {
        return DotsC_DictionaryInt64KeyToIndex(typeId, parameter, key.GetRawValue());
    }

    Int32 DictionaryKeyToIndex(const TypeId typeId,
                               const ParameterIndex parameter,
                               const HandlerId& key)
    {
        return DotsC_DictionaryInt64KeyToIndex(typeId, parameter, key.GetRawValue());
    }

    Int32 DictionaryKeyToIndex(const TypeId typeId,
                               const ParameterIndex parameter,
                               const ChannelId& key)
    {
        return DotsC_DictionaryInt64KeyToIndex(typeId, parameter, key.GetRawValue());
    }


    void GetInfo(const Dob::Typesystem::TypeId typeId,
                 const Dob::Typesystem::ParameterIndex parameter,
                 Dob::Typesystem::MemberType& parameterType,
                 Dob::Typesystem::MemberType& keyType,
                 std::wstring& parameterName,
                 Dob::Typesystem::TypeId& parameterTypeId,
                 Dob::Typesystem::TypeId& keyTypeId,
                 Dob::Typesystem::CollectionType & collectionType,
                 Dob::Typesystem::Int32& numberOfValues)
    {
        const char* parameterNameUtf8 = nullptr;

        DotsC_GetParameterInfo(typeId,
                               parameter,
                               parameterType,
                               keyType,
                               parameterNameUtf8,
                               parameterTypeId,
                               keyTypeId,
                               collectionType,
                               numberOfValues);

        if (parameterNameUtf8 != nullptr)
        {
            parameterName = Utilities::ToWstring(parameterNameUtf8);
        }
    }

    Int32
    GetEnumerationDictionaryKey(const TypeId typeId,
                                const ParameterIndex parameter,
                                const Dob::Typesystem::ArrayIndex index)
    {
        Int32 i;
        DotsC_GetEnumerationParameter(typeId, parameter, index, DotsC_KeyMode, i);
        return i;
    }

    Int32
    GetInt32DictionaryKey(const TypeId typeId,
                          const ParameterIndex parameter,
                          const Dob::Typesystem::ArrayIndex index)
    {
        Int32 i;
        DotsC_GetInt32Parameter(typeId, parameter, index, DotsC_KeyMode, i);
        return i;
    }

    Int64
    GetInt64DictionaryKey(const TypeId typeId,
                          const ParameterIndex parameter,
                          const Dob::Typesystem::ArrayIndex index)
    {
        Int64 i;
        DotsC_GetInt64Parameter(typeId, parameter, index, DotsC_KeyMode, i);
        return i;
    }


    TypeId
    GetTypeIdDictionaryKey(const TypeId typeId,
                           const ParameterIndex parameter,
                           const Dob::Typesystem::ArrayIndex index)
    {
        TypeId t;
        DotsC_GetTypeIdParameter(typeId, parameter, index, DotsC_KeyMode, t);
        return t;
    }

    const InstanceId
    GetInstanceIdDictionaryKey(const TypeId typeId,
                               const ParameterIndex parameter,
                               const Dob::Typesystem::ArrayIndex index)
    {
        Int64 hashVal;
        const char * strVal;
        DotsC_GetHashedIdParameter(typeId, parameter, index, DotsC_KeyMode, hashVal, strVal);
        if (strVal == NULL)
        {
            return InstanceId(hashVal);
        }
        else
        {
            return InstanceId(hashVal, Utilities::ToWstring(strVal));
        }
    }

    const EntityId
    GetEntityIdDictionaryKey(const TypeId typeId,
                             const ParameterIndex parameter,
                             const Dob::Typesystem::ArrayIndex index)
    {
        DotsC_EntityId eid;
        const char * instanceIdStr;
        DotsC_GetEntityIdParameter(typeId, parameter, index, DotsC_KeyMode, eid, instanceIdStr);
        if(instanceIdStr==NULL)
        {
            return EntityId(eid.typeId,InstanceId(eid.instanceId));
        }
        else
        {
            return EntityId(eid.typeId,InstanceId(eid.instanceId,Utilities::ToWstring(instanceIdStr)));
        }
    }


    const ChannelId
    GetChannelIdDictionaryKey(const TypeId typeId,
                              const ParameterIndex parameter,
                              const Dob::Typesystem::ArrayIndex index)
    {
        Int64 hashVal;
        const char * strVal;
        DotsC_GetHashedIdParameter(typeId, parameter, index, DotsC_KeyMode, hashVal, strVal);
        if (strVal == NULL)
        {
            return ChannelId(hashVal);
        }
        else
        {
            return ChannelId(hashVal,Utilities::ToWstring(strVal));
        }
    }


    const HandlerId
    GetHandlerIdDictionaryKey(const TypeId typeId,
                              const ParameterIndex parameter,
                              const Dob::Typesystem::ArrayIndex index)
    {
        Int64 hashVal;
        const char * strVal;
        DotsC_GetHashedIdParameter(typeId, parameter, index, DotsC_KeyMode, hashVal, strVal);
        if (strVal == NULL)
        {
            return HandlerId(hashVal);
        }
        else
        {
            return HandlerId(hashVal,Utilities::ToWstring(strVal));
        }
    }

    const std::wstring
    GetStringDictionaryKey(const TypeId typeId,
                           const ParameterIndex parameter,
                           const Dob::Typesystem::ArrayIndex index)
    {
        const char* tmp;
        DotsC_GetStringParameter(typeId, parameter, index, DotsC_KeyMode, tmp);
        return Utilities::ToWstring(tmp);
    }
}
}
}
}
