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
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/
#ifndef __DOTS_INTERNAL_TYPE_UTILITIES_H__
#define __DOTS_INTERNAL_TYPE_UTILITIES_H__

#include <Safir/Dob/Typesystem/Internal/Detail/BasicTypeOperations.h>

/**
  * Useful helper functions that operates on type repositories and type descriptions.
  */
namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
namespace TypeUtilities
{
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
        return Safir::Dob::Typesystem::Internal::Detail::BasicTypeOperations::TypeIdToTypeName(repository, typeId);
    }

//    /**
//     * Finds corresponding type name to a typeId. If no type exists with given typeId NULL is returned.
//     *
//     * @param repository [in] - Type repository containing all type information
//     * @param typeId [in] - TypeId to lookup and find name for.
//     * @return The type name or NULL if type doesn't exist in the repository.
//     */
//    template <class RepositoryT>
//    const char* GetMemberTypeName(const RepositoryT* repository, DotsC_MemberType memberType, DotsC_TypeId typeId)
//    {
//        if (Safir::Dob::Typesystem::Internal::Detail::BasicTypeOperations::IsBasicMemberType(memberType))
//        {
//            return Safir::Dob::Typesystem::Internal::Detail::BasicTypeOperations::ty
//        }
//        return (Safir::Dob::Typesystem::Internal::Detail::ToStringHelper<RepositoryT>(repository, false)).GetTypeName(typeId);
//    }

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

    template <class ClassDescriptionT, class ParameterDescriptionT>
    const ParameterDescriptionT* GetParameterByName(const ClassDescriptionT* cd, const std::string& paramName)
    {
        size_t dot=paramName.find('.');
        if (dot!=std::string::npos)
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
            std::string fullName=std::string(cd->GetName())+"."+paramName;
            for (int i=0; i<cd->GetNumberOfParameters(); ++i)
            {
                const ParameterDescriptionT* pd=cd->GetParameter(i);
                if (fullName==pd->GetName())
                {
                    return pd;
                }
            }
        }
        return NULL;
    }

    template <class RepT, class Traits=Safir::Dob::Typesystem::Internal::TypeRepositoryTraits<RepT> >
    struct GetParameterByFullName
    {
        typedef typename Traits::RepositoryType RepositoryType;
        typedef typename Traits::ClassDescriptionType ClassDescriptionType;
        typedef typename Traits::ParameterDescriptionType ParameterDescriptionType;

        const ParameterDescriptionType* operator()(const RepositoryType* rep, const std::string& parameterName) const
        {
            size_t pos=parameterName.rfind('.');
            if (pos==std::string::npos)
            {
                return NULL;
            }

            std::string className=parameterName.substr(0, pos);
            const ClassDescriptionType* cd=rep->GetClass(DotsId_Generate64(className.c_str()));
            if (!cd)
            {

                return NULL;
            }

            return GetParameterByName<ClassDescriptionType, ParameterDescriptionType>(cd, parameterName);
        }
    };
}
}
}
}
} //end namespace Safir::Dob::Typesystem::Internal::TypeUtilities

#endif
