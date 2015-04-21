/******************************************************************************
*
* Copyright Consoden AB, 2006-2013 (http://safir.sourceforge.net)
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
        DotsC_MemberType memberType;
        const char* memberName;
        DotsC_TypeId complexType;
        DotsC_Int32 stringLength;
        DotsC_CollectionType collectionType;
        DotsC_Int32 arraySize;
        DotsC_GetMemberInfo(typeId, member, memberType, memberName, complexType, stringLength, collectionType, arraySize);

        if (memberName == NULL)
        {
            throw IllegalValueException(L"There is no such type or member defined", __WFILE__, __LINE__);
        }
        else
        {
            return Utilities::ToWstring(memberName);
        }
    }

    std::wstring GetTypeName(const Dob::Typesystem::TypeId typeId, const Dob::Typesystem::MemberIndex member)
    {
        DotsC_MemberType memberType;
        const char* memberName;
        DotsC_TypeId complexType;
        DotsC_Int32 stringLength;
        DotsC_CollectionType collectionType;
        DotsC_Int32 arraySize;
        DotsC_GetMemberInfo(typeId, member, memberType, memberName, complexType, stringLength, collectionType, arraySize);

        if (memberName == NULL)
        {
            throw IllegalValueException(L"There is no such type or member defined", __WFILE__, __LINE__);
        }

        if (memberType==ObjectMemberType || memberType==EnumerationMemberType)
        {
            return Safir::Dob::Typesystem::Utilities::ToWstring(DotsC_GetTypeName(complexType));
        }
        else
        {
            return Safir::Dob::Typesystem::Utilities::ToWstring(DotsC_MemberTypeName(memberType));
        }
    }

    TypeId  
    GetTypeId(const TypeId typeId, 
                      const MemberIndex member)
    {
        DotsC_MemberType memberType;
        const char* memberName;
        DotsC_TypeId complexType;
        DotsC_Int32 stringLength;
        DotsC_CollectionType collectionType;
        DotsC_Int32 arraySize;
        DotsC_GetMemberInfo(typeId, member, memberType, memberName, complexType, stringLength, collectionType, arraySize);

        if (complexType == -1)
        {
            throw IllegalValueException(L"There is no such type or member defined", __WFILE__, __LINE__);
        }
        else
        {
            return complexType;
        }
    }

    void 
    GetInfo(const TypeId typeId,                //in 
            const MemberIndex member,           //in
            MemberType& memberType,             //out
            const char* & memberName,           //out
            TypeId& memberTypeId,               //out
            Int32& stringLength,                //out
            CollectionType& collectionType,     //out
            Int32& arrayLength)                 //out
    {
        DotsC_GetMemberInfo(typeId, member, memberType, memberName, memberTypeId, stringLength, collectionType, arrayLength);
        if (memberName == NULL)
        {
            throw IllegalValueException(L"There is no such type or member defined", __WFILE__, __LINE__);
        }
    }


    Int32 
    GetArraySize(const TypeId typeId, 
                 const MemberIndex member)
    {
        DotsC_MemberType memberType;
        const char* memberName;
        DotsC_TypeId complexType;
        DotsC_Int32 stringLength;
        DotsC_CollectionType collectionType;
        DotsC_Int32 arraySize;
        DotsC_GetMemberInfo(typeId, member, memberType, memberName, complexType, stringLength, collectionType, arraySize);

        if (arraySize == -1)
        {
            throw IllegalValueException(L"No such type or array defined", __WFILE__,__LINE__);
        }
        else
        {
            return arraySize;
        }
    }

    Int32 
    GetMaxStringLength(const TypeId typeId, 
                       const MemberIndex member)
    {
        DotsC_MemberType memberType;
        const char* memberName;
        DotsC_TypeId complexType;
        DotsC_Int32 stringLength;
        DotsC_CollectionType collectionType;
        DotsC_Int32 arraySize;
        DotsC_GetMemberInfo(typeId, member, memberType, memberName, complexType, stringLength, collectionType, arraySize);

        if (stringLength == -1)
        {
            throw IllegalValueException(L"No such type or member defined", __WFILE__,__LINE__);
        }
        else
        {
            return stringLength;
        }
    }
}
}
}
}
