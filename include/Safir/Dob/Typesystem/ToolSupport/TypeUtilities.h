/******************************************************************************
*
* Copyright Saab AB, 2004-2023 (http://safirsdkcore.com)
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
#ifndef __DOTS_INTERNAL_TYPE_UTILITIES_H__
#define __DOTS_INTERNAL_TYPE_UTILITIES_H__

#include <Safir/Dob/Typesystem/ToolSupport/Internal/BasicTypeOperations.h>

/**
  * Useful helper functions that operates on type repositories and type descriptions.
  */
namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace ToolSupport
{
namespace TypeUtilities
{
    /**
     * Calculates a typeId from a string. Does not check that the type exists.
     * @param name [in] - Name of the type
     * @return The typeId
     */
    inline DotsC_TypeId CalculateTypeId(const std::string& name)
    {
        return LlufId_Generate64(name.c_str());
    }

    /**
     * Finds corresponding type name to a memberType.
     *
     * @param memberType [in] - MemberType to convert to string
     * @return The type name or NULL if type doesn't exist
     */
    inline const char* GetTypeName(DotsC_MemberType memberType)
    {
        return Safir::Dob::Typesystem::ToolSupport::Internal::BasicTypeOperations::MemberTypeToString(memberType).c_str();
    }

    /**
     * Finds corresponding type name to a typeId. If no type exists with given typeId NULL is returned.
     *
     * @param repository [in] - Type repository containing all type information
     * @param typeId [in] - TypeId to lookup and find name for.
     * @return The type name or NULL if type doesn't exist in the repository.
     */
    template <class RepositoryT>
    const char* GetTypeName(const RepositoryT* repository, DotsC_TypeId typeId)
    {
        return Safir::Dob::Typesystem::ToolSupport::Internal::BasicTypeOperations::TypeIdToTypeName(repository, typeId);
    }

    /**
     * Finds corresponding type name to a member or parameter description.
     *
     * @param repository [in] - Type repository containing all type information
     * @param member [in] - Member description or Parameter description.
     * @return The type name of the type.
     */
    template <class RepositoryT, class DescriptionT>
    const char* GetTypeName(const RepositoryT* repository, const DescriptionT* member)
    {
        DotsC_MemberType mt=member->GetMemberType();
        if (mt==EnumerationMemberType || mt==ObjectMemberType)
        {
            return GetTypeName(repository, member->GetTypeId());
        }
        return GetTypeName(mt);
    }

    /**
     * Check if a type is the same or a subtype of another type. Only usable for Enumerations and ObjectTypes.
     *
     * @param repository [in] - Type repository containing all type information
     * @param tid [in] - TypeId to check if it is the same or a subtype of the ofTid
     * @param ofTid [in] - The typeId to check tid against.
     * @return True if tid is the same or a subtype of ofTid, else false.
     */
    template <class RepositoryT>
    bool IsOfType(const RepositoryT* repository, DotsC_TypeId tid, DotsC_TypeId ofTid)
    {
        if (tid==ofTid)
        {
            return true;
        }
        return Safir::Dob::Typesystem::ToolSupport::Internal::BasicTypeOperations::IsOfType(repository, ObjectMemberType, tid, ObjectMemberType, ofTid);
    }

    /**
     * Get the index (ordinal) of an enumeration value.
     *
     * @param description [in] - EnumerationDescription
     * @param valueName [in] - Enumeration value name, can be short form or fully qualified. Ex: 'Monday' and 'MyNamespace.MyEnumType.Monday'
     * @return Index of the value or -1 if not found.
     */
    template <class EnumDescriptionT>
    int GetIndexOfEnumValue(const EnumDescriptionT* description, const std::string& valueName) //Supports short name and fully qualified name. Ex: 'Monday' and 'MyEnumType.Monday'
    {
        size_t pos=valueName.rfind('.');
        if (pos==std::string::npos)
        {
            for (int i=0; i<description->GetNumberOfValues(); ++i)
            {
                if (valueName==description->GetValueName(i))
                {
                    return i;
                }
            }
        }
        else
        {
            std::string strippedValueName=valueName.substr(pos+1);
            for (int i=0; i<description->GetNumberOfValues(); ++i)
            {
                if (strippedValueName==description->GetValueName(i))
                {
                    return i;
                }
            }
        }
        return -1;
    }

    /**
     * Get index of a property member.
     *
     * @param pd [in] - Property description.
     * @param memberName [in] - member name
     * @return Index of member or -1 if not found.
     */
    template <class PropertyDescriptionT, class MemberDescriptionT>
    DotsC_MemberIndex GetPropertyMemberIndex(const PropertyDescriptionT* pd, const std::string& memberName)
    {
        for (int i=0; i<pd->GetNumberOfMembers(); ++i)
        {
            const MemberDescriptionT* md=pd->GetMember(i);
            if (memberName==md->GetName())
            {
                return static_cast<DotsC_MemberIndex>(i);
            }
        }
        return -1;
    }

    /**
     * Get parameter by name when the classDescription is already retrieved. To get parameter from a
     * fully qualified name, use GetParameterByFullName below.
     *
     * @param cd [in] - The class description that contains the parameter.
     * @param paramName [in] - Parameter name, can be short form or fully qualified name.
     * @return ParameterDescription or NULL if not found.
     */
    template <class ClassDescriptionT, class ParameterDescriptionT>
    const ParameterDescriptionT* GetParameterByName(const ClassDescriptionT* cd, const std::string& paramName)
    {
        size_t dot=paramName.rfind('.');
        if (dot==std::string::npos)
        {
            for (int i=0; i<cd->GetNumberOfParameters(); ++i)
            {
                const ParameterDescriptionT* pd=cd->GetParameter(i);
                if (paramName==pd->GetName())
                {
                    return pd;
                }
            }
        }
        else
        {
            std::string shortName=paramName.substr(dot+1);
            for (int i=0; i<cd->GetNumberOfParameters(); ++i)
            {
                const ParameterDescriptionT* pd=cd->GetParameter(i);
                if (shortName==pd->GetName())
                {
                    return pd;
                }
            }
        }
        return NULL;
    }

    /**
     * Helper class to get ParameterDescription from a fully qualified name without having the ClassDescription.
     */
    template <class RepT, class Traits=Safir::Dob::Typesystem::ToolSupport::TypeRepositoryTraits<RepT> >
    struct GetParameterByFullName
    {
        typedef typename Traits::RepositoryType RepositoryType;
        typedef typename Traits::ClassDescriptionType ClassDescriptionType;
        typedef typename Traits::ParameterDescriptionType ParameterDescriptionType;

        /**
         * Get ParameterDescription from a fully qualified name.
         * @param rep [in] - TypeRepository containing all type information needed.
         * @param parameterName [in] - Fully qualified name including namespace and class name, Ex: MyNamespace.MyClass.MyParameter
         * @return ParameterDescription or NULL if not found.
         */
        const ParameterDescriptionType* operator()(const RepositoryType* rep, const std::string& parameterName) const
        {
            size_t pos=parameterName.rfind('.');
            if (pos==std::string::npos)
            {
                return NULL;
            }

            std::string className=parameterName.substr(0, pos);
            const ClassDescriptionType* cd=rep->GetClass(CalculateTypeId(className));
            if (!cd)
            {
                return NULL;
            }

            return GetParameterByName<ClassDescriptionType, ParameterDescriptionType>(cd, parameterName.substr(pos+1));
        }
    };

    /**
     * @brief ToUnifiedDictionaryKey - Convert all keys to an int64 that is the internal key format.
     * @param key
     * @return Int64 representation of a dictionary key
     */
    inline DotsC_Int64 ToUnifiedDictionaryKey(DotsC_Int64 key) {return key;}
    inline DotsC_Int64 ToUnifiedDictionaryKey(DotsC_Int32 key) {return static_cast<DotsC_Int64>(key);}
    inline DotsC_Int64 ToUnifiedDictionaryKey(const std::string& key) {return LlufId_Generate64(key.c_str());}
    inline DotsC_Int64 ToUnifiedDictionaryKey(const DotsC_EntityId& key)
    {
        std::ostringstream os;
        os<<key.typeId<<key.instanceId;
        return LlufId_Generate64(os.str().c_str());
    }

    template <class ParameterDescriptionT, class KeyT>
    int GetDictionaryIndexFromKey(const ParameterDescriptionT* pd, const KeyT& key)
    {
        assert(pd->GetCollectionType()==DictionaryCollectionType);
        return pd->GetIndexByUnifiedKey(ToUnifiedDictionaryKey(key));
    }

}
}
}
}
} //end namespace Safir::Dob::Typesystem::ToolSupport::TypeUtilities

#endif
