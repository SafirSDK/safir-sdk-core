/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
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
            std::wostringstream ostr;
            ostr << "There is no such type or parameter defined: ("
                 << typeId << ", " 
                 << parameterName << ")";
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
        return Utilities::ToWstring(DotsC_GetParameterName(typeId, parameter));
    }

    MemberType
    GetType(const TypeId typeId,
            const ParameterIndex parameter)
    {
        return static_cast<MemberType>(DotsC_GetParameterType(typeId, parameter));
    }

    const std::wstring
    GetTypeName(const TypeId typeId,
                const ParameterIndex parameter)
    {
        return Utilities::ToWstring(DotsC_GetParameterTypeName(typeId, parameter));
    }

    Int32
    GetArraySize(const TypeId typeId,
                 const ParameterIndex parameter)
    {
        return DotsC_GetParameterArraySize(typeId, parameter);
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
        DotsC_GetEnumerationParameter(typeId, parameter, index, i);
        return i;
    }

    Int32
    GetInt32(const TypeId typeId,
             const ParameterIndex parameter,
             const Dob::Typesystem::ArrayIndex index)
    {
        Int32 i;
        DotsC_GetInt32Parameter(typeId, parameter, index, i);
        return i;
    }

    Int64
    GetInt64(const TypeId typeId,
             const ParameterIndex parameter,
             const Dob::Typesystem::ArrayIndex index)
    {
        Int64 i;
        DotsC_GetInt64Parameter(typeId, parameter, index, i);
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
        DotsC_GetTypeIdParameter(typeId, parameter, index, t);
        return t;
    }

    const InstanceId
    GetInstanceId(const TypeId typeId,
                  const ParameterIndex parameter,
                  const Dob::Typesystem::ArrayIndex index)
    {
        Int64 hashVal;
        const char * strVal;
        DotsC_GetHashedIdParameter(typeId, parameter, index, hashVal, strVal);
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
        DotsC_GetEntityIdParameter(typeId, parameter, index, eid, instanceIdStr);
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
        DotsC_GetHashedIdParameter(typeId, parameter, index, hashVal, strVal);
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
        DotsC_GetHashedIdParameter(typeId, parameter, index, hashVal, strVal);
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
        DotsC_GetStringParameter(typeId, parameter, index, tmp);
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
}
}
}
}
