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

#include <Safir/Dob/Typesystem/Members.h>
#include <Safir/Dob/Typesystem/Exceptions.h>
#include <Safir/Dob/Typesystem/Utilities.h>
#include <Safir/Dob/Typesystem/Object.h>
#include <Safir/Dob/Typesystem/Internal/Kernel.h>


namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Members
{
    //Functions for retrieving member info about object types
    Int32
    GetNumberOfMembers(const TypeId typeId)
    {
        const Int32 result = DotsC_GetNumberOfMembers(typeId);
        if (result == -1)
        {
            throw IllegalValueException(L"No such type",__WFILE__,__LINE__);
        }
        else
        {
            return result;
        }
    }

    MemberIndex 
    GetIndex(const TypeId typeId, const std::wstring & memberName)
    {
        const MemberIndex result = DotsC_GetMemberId(typeId, Utilities::ToUtf8(memberName).c_str());
        if (result == -1)
        {
            throw IllegalValueException(L"There is no such type or member defined", __WFILE__, __LINE__);
        }
        else
        {
            return result;
        }
    }

    std::wstring 
    GetName(const TypeId typeId, 
            const MemberIndex member)
    {
        const char * result = DotsC_GetMemberName(typeId, member);
        if (result == NULL)
        {
            throw IllegalValueException(L"There is no such type or member defined", __WFILE__, __LINE__);
        }
        else
        {
            return Utilities::ToWstring(result);
        }
    }

    TypeId  
    GetTypeId(const TypeId typeId, 
                      const MemberIndex member)
    {
        TypeId result = DotsC_GetComplexMemberTypeId(typeId, member);
        if (result == -1)
        {
            throw IllegalValueException(L"There is no such type or member defined", __WFILE__, __LINE__);
        }
        else
        {
            return result;
        }
    }

    void 
    GetInfo(const TypeId typeId,                //in 
            const MemberIndex member,           //in
            MemberType& memberType,             //out
            const char* & memberName,           //out
            TypeId& memberTypeId,                //out
            Int32& stringLength,                //out
            bool& isArray,                      //out
            Int32& arrayLength)                 //out
    {
        DotsC_GetMemberInfo(typeId, member, memberType, memberName, memberTypeId, stringLength, isArray, arrayLength);
        if (memberName == NULL)
        {
            throw IllegalValueException(L"There is no such type or member defined", __WFILE__, __LINE__);
        }
    }


    Int32 
    GetArraySize(const TypeId typeId, 
                 const MemberIndex member)
    {
        const Int32 result = DotsC_GetMemberArraySize(typeId, member);
        if (result == -1)
        {
            throw IllegalValueException(L"No such type or array defined", __WFILE__,__LINE__);
        }
        else
        {
            return result;
        }
    }

    Int32 
    GetMaxStringLength(const TypeId typeId, 
                       const MemberIndex member)
    {
        const Int32 result = DotsC_GetStringMemberMaxLength(typeId, member);
        if (result == -1)
        {
            throw IllegalValueException(L"No such type or member defined", __WFILE__,__LINE__);
        }
        else
        {
            return result;
        }
    }


    std::wstring 
    GetTypeName(const TypeId typeId, 
                const MemberIndex member)
    {
        const char * result = DotsC_GetMemberTypeName(typeId, member);
        if (result == NULL)
        {
            throw IllegalValueException(L"There is no such type or member defined", __WFILE__, __LINE__);
        }
        else
        {
            return Utilities::ToWstring(result);
        }
    }
}
}
}
}
