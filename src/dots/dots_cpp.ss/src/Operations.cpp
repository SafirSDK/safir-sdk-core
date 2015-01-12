/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safir.sourceforge.net)
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

#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Typesystem/Utilities.h>
#include <Safir/Dob/Typesystem/Internal/Kernel.h>
#include <Safir/Dob/Typesystem/Exceptions.h>
#include <boost/lexical_cast.hpp>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Operations
{

    Int32
    GetNumberOfTypeIds()
    {
        return DotsC_NumberOfTypeIds();
    }

    Int32
    GetNumberOfClasses()
    {
        return DotsC_NumberOfClasses();
    }

    Int32
    GetNumberOfProperties()
    {
        return DotsC_NumberOfProperties();
    }

    Int32
    GetNumberOfEnumerations()
    {
        return DotsC_NumberOfEnumerations();
    }

    TypeIdVector
    GetAllTypeIds()
    {
        //see item 16 of Effective STL for explanation on why we do the things below

        TypeIdVector typeIds(GetNumberOfTypeIds());
        Int32 resultSize;
        DotsC_GetAllTypeIds(&typeIds[0], static_cast<Int32>(typeIds.size()), resultSize);
        if (typeIds.size() != static_cast<size_t>(resultSize))
        {
            throw SoftwareViolationException(L"Incorrect number of types returned by GetNumberOfTypeIds()",__WFILE__,__LINE__);
        }
        return typeIds;
    }

    bool
    Exists(const TypeId typeId)
    {
        return DotsC_TypeExists(typeId);
    }

    bool
    IsClass(const TypeId typeId)
    {
        return DotsC_IsClass(typeId);
    }

    bool
    IsProperty(const TypeId typeId)
    {
        return DotsC_IsProperty(typeId);
    }

    bool
    IsEnumeration(const TypeId typeId)
    {
        return DotsC_IsEnumeration(typeId);
    }

    bool
    IsException(const TypeId typeId)
    {
        return DotsC_IsException(typeId);
    }

    //Interpretation between type name and typeId
    TypeId
    GetTypeId(const std::wstring& typeName)
    {
        return DotsC_TypeIdFromName(Safir::Dob::Typesystem::Utilities::ToUtf8(typeName).c_str());
    }

    std::wstring
    GetName(const TypeId typeId)
    {
        const char * result = DotsC_GetTypeName(typeId);
        if (result == NULL)
        {
            throw IllegalValueException(L"There is no such type defined. typeId = "
                                        + boost::lexical_cast<std::wstring>(typeId),
                                        __WFILE__, __LINE__);
        }
        else
        {
            return Utilities::ToWstring(result);
        }
    }

    Int32
    GetNumberOfEnumerationValues(const TypeId enumId)
    {
        const Int32 result = DotsC_GetNumberOfEnumerationValues(enumId);
        if (result == -1)
        {
            throw IllegalValueException(L"No such enumeration exists. enumId = " 
                                        + boost::lexical_cast<std::wstring>(enumId),
                                        __WFILE__,__LINE__);
        }
        else
        {
            return result;
        }
    }

    std::wstring
    GetEnumerationValueName(const TypeId enumId,
                            const EnumerationValue enumVal)
    {
        const char * result = DotsC_GetEnumerationValueName(enumId, enumVal);

        if (result == NULL)
        {
            throw IllegalValueException(L"There is no such enumeration or value defined. enumId = "
                                        + boost::lexical_cast<std::wstring>(enumId)
                                        + L", enumVal = "
                                        + boost::lexical_cast<std::wstring>(enumVal),
                                        __WFILE__, __LINE__);
        }
        else
        {
            return Utilities::ToWstring(result);
        }
    }

    EnumerationValue
    GetEnumerationValue(const TypeId enumId,
                        const std::wstring& enumValueName)
    {
        const EnumerationValue result = DotsC_EnumerationValueFromName(enumId, Utilities::ToUtf8(enumValueName).c_str());
        if (result == -1)
        {
            throw IllegalValueException(L"There is no such enumeration or value defined. enumId = "
                                        + boost::lexical_cast<std::wstring>(enumId)
                                        + L", enumVal = " + enumValueName,
                                        __WFILE__, __LINE__);
        }
        else
        {
            return result;
        }
    }

    Dob::Typesystem::TypeId
    GetEnumerationChecksum(const Dob::Typesystem::TypeId enumId)
    {
        TypeId checksum;
        DotsC_GetEnumerationChecksum(enumId,checksum);
        return checksum;
    }

    //Checks if type is an instance of the ofType, direct or by inheritance
    bool
    IsOfType(const TypeId type,
             const TypeId ofType)
    {
        return DotsC_IsOfType(type, ofType);
    }

    TypeIdVector
    GetClassTree(const TypeId type)
    {
        //see item 16 and 17 of Effective STL for explanation on why we do the things below

        TypeIdVector typeIds(GetNumberOfTypeIds());
        Int32 resultSize;
        DotsC_GetCompleteType(type, &typeIds[0], static_cast<Int32>(typeIds.size()), resultSize);
        if (typeIds.size() < static_cast<size_t>(resultSize))
        {
            throw SoftwareViolationException(L"Incorrect number of types returned by GetNumberOfTypeIds()",__WFILE__,__LINE__);
        }
        typeIds.resize(resultSize);

        TypeIdVector(typeIds).swap(typeIds); //minimize vector (swap trick)
        return typeIds;
    }

    Dob::Typesystem::TypeId
    GetParentType(const Dob::Typesystem::TypeId type)
    {
        return DotsC_GetParentType(type);
    }

    bool
    HasProperty(const TypeId classType,
                const TypeId propertyType)
    {
        bool isInherited, hasProperty;
        DotsC_HasProperty(classType, propertyType, hasProperty, isInherited);
        return hasProperty;
    }

    void
    HasProperty(const TypeId classType,
                const TypeId propertyType,
                bool & hasProperty,
                bool & isInherited)
    {
        return DotsC_HasProperty(classType, propertyType, hasProperty, isInherited);
    }

}
}
}
}
