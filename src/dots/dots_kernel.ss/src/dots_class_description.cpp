/******************************************************************************
*
* Copyright Saab AB, 2004-2008 (http://www.safirsdk.com)
* 
* Created by: Joel Ottosson / stjoot
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

#include "dots_class_description.h"
#include "dots_repository.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{

    ClassDescription::ClassDescription(const std::string & name,
                                       const TypeId typeId,
                                       const TypeId /*baseClassTypeId*/,
                                       const Size initialSize,
                                       const unsigned int thisClassSize,
                                       const Size noInheritedMembers,
                                       const Size noInheritedParameters,
                                       const Size noMembers,
                                       const Size noDescendants,
                                       const Size noParameters,
                                       const Size noProperties,
                                       AllocationHelper & allocHelper):
        m_name(name.begin(),name.end(),allocHelper.GetAllocator<char>()),
        m_baseClass(),
        m_typeId(typeId),
        m_initialSize(initialSize),
        m_thisClassSize(thisClassSize),
        m_noInheritedMembers(noInheritedMembers),
        m_members(allocHelper.GetAllocator<MemberDescription>()),
        m_descendants(allocHelper.GetAllocator<ClassDescriptionConstPtr>()),
        m_noInheritedParameters(noInheritedParameters),
        m_parameters(allocHelper.GetAllocator<ParameterDescription>()),
        m_propertyMappings(allocHelper.GetAllocator<PropertyMappingDescription>())
    {
        m_descendants.reserve(noDescendants);
        m_members.reserve(noMembers);
        m_parameters.reserve(noParameters);
        m_propertyMappings.reserve(noProperties);
    }

    ClassDescription::~ClassDescription()
    {

    }

    const MemberDescription *
    ClassDescription::GetOwnMember(const MemberIndex member) const
    {
        if (member < 0 || member >= static_cast<MemberIndex>(m_members.size()))
        {
            return NULL;
        }
        return &m_members[member];
    }


    const MemberDescription *
    ClassDescription::GetMember(const MemberIndex member) const
    {
        //        std::wcout << "GetMember " << member << " in class " << Name() << std::endl;
        //        std::wcout << " NumberOfInheritedMembers() = " << NumberOfInheritedMembers() << std::endl;

        if (member >= static_cast<MemberIndex>(NumberOfInheritedMembers()))
        {
            return GetOwnMember(member - NumberOfInheritedMembers());
        }
        else
        {
            if (BaseClass() != NULL)
            {
                return BaseClass()->GetMember(member);
            }
            else
            {
                return NULL;
            }
        }
    }


    MemberIndex
    ClassDescription::GetMemberIndexFromName(const std::string & name) const
    {
        for (MemberVector::const_iterator it = m_members.begin();
             it != m_members.end(); ++it)
        {
            if (name == it->Name())
            {
                return static_cast<MemberIndex>(NumberOfInheritedMembers() + std::distance(m_members.begin(),it));
            }
        }


        if (BaseClass() == NULL)
        {
            return -1;
        }
        else
        {
            return BaseClass()->GetMemberIndexFromName(name);
        }
    }

    const ParameterDescription *
    ClassDescription::GetOwnParameter(const ParameterIndex parameter) const
    {
        if (parameter < 0 || parameter >= static_cast<ParameterIndex>(m_parameters.size()))
        {
            return NULL;
        }
        return &m_parameters[parameter];
    }


    const ParameterDescription *
    ClassDescription::GetParameter(const ParameterIndex parameter) const
    {
        if (parameter >= static_cast<ParameterIndex>(NumberOfInheritedParameters()))
        {
            return GetOwnParameter(parameter - NumberOfInheritedParameters());
        }
        else
        {
            if (BaseClass() != NULL)
            {
                return BaseClass()->GetParameter(parameter);
            }
            else
            {
                return NULL;
            }
        }
    }

    ParameterIndex
    ClassDescription::GetParameterIndexFromName(const std::string & name) const
    {
        for (ParameterVector::const_iterator it = m_parameters.begin();
             it != m_parameters.end(); ++it)
        {
            if (name == it->Name())
            {
                return static_cast<ParameterIndex>(NumberOfInheritedParameters() + std::distance(m_parameters.begin(),it));
            }
        }

        if (BaseClass() == NULL)
        {
            return -1;
        }
        else
        {
            return BaseClass()->GetParameterIndexFromName(name);
        }
    }

    //returns NULL if property does not exist in class tree.
    const PropertyMappingDescription *
    ClassDescription::FindPropertyMapping(const TypeId propertyType, bool & isInherited) const
    {
        for (PropertyMappingVector::const_iterator it = m_propertyMappings.begin();
             it != m_propertyMappings.end(); ++it)
        {
            if (it->Property()->GetTypeId() == propertyType)
            {
                isInherited = false;
                return &(*it);
            }
        }

        if (BaseClass() != NULL)
        {
            const PropertyMappingDescription * const parentResult = BaseClass()->FindPropertyMapping(propertyType,isInherited);
            if (parentResult != NULL)
            {
                isInherited = true;
            }
            return parentResult;
        }

        isInherited = false;
        return NULL;
    }

}
}
}
}
