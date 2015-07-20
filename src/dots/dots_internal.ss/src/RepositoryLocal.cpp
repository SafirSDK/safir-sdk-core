/******************************************************************************
*
* Copyright Saab AB, 2004-2015 (http://safir.sourceforge.net)
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
#include <sstream>
#include <algorithm>
#include <set>
#include <Safir/Dob/Typesystem/ToolSupport/Internal/InternalDefs.h>
#include "RepositoryLocal.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace ToolSupport
{
    ParameterDescriptionLocal* RepositoryLocal::GetParameterLocal(const std::string& qualifiedName)
    {
        boost::unordered_map<std::string, ParameterDescriptionLocal*>::const_iterator it=m_parameters.find(qualifiedName);
        if (it!=m_parameters.end())
        {
            return it->second;
        }
        return NULL;
    }

    //---------------------------------------------------
    // Description wrapper implementation
    //---------------------------------------------------
    
    //Class
    //---------
    const ParameterDescription* ClassDescriptionLocal::GetParameter(DotsC_ParameterIndex index) const
    {
        int numInherited=GetNumberOfInheritedParameters();
        if (index<numInherited)
        {
            return base->GetParameter(index);
        }

        return ownParameters[index-numInherited].get();
    }

    void ClassDescriptionLocal::GetPropertyIds(std::set<DotsC_TypeId>& propertyIds) const
    {
        if (base)
        {
            base->GetPropertyIds(propertyIds);
        }

        for (std::vector<PropertyMappingDescriptionLocalPtr>::const_iterator it=properties.begin(); it!=properties.end(); ++it)
        {
            propertyIds.insert((*it)->property->GetTypeId());
        }
    }

    const PropertyMappingDescription* ClassDescriptionLocal::GetPropertyMapping(DotsC_TypeId propertyTypeId, bool & isInherited) const
    {
        for (std::vector<PropertyMappingDescriptionLocalPtr>::const_iterator it=properties.begin(); it!=properties.end(); ++it)
        {
            if ((*it)->property->GetTypeId()==propertyTypeId)
            {
                isInherited=false;
                return it->get();
            }
        }        
        
        if (base)
        {            
            const PropertyMappingDescription* tmp=base->GetPropertyMapping(propertyTypeId, isInherited);
            isInherited=true;
            return tmp;
        }

        return NULL;        
    }

    DotsC_MemberIndex ClassDescriptionLocal::GetMemberIndex(const std::string& memberName) const
    {
        for (std::vector<MemberDescriptionLocalPtr>::const_iterator it=members.begin(); it!=members.end(); ++it)
        {
            if ((*it)->GetName()==memberName)
            {
                return static_cast<DotsC_MemberIndex>(std::distance(members.begin(), it) + GetNumberOfInheritedMembers());
            }
        }

        if (base)
        {
            return base->GetMemberIndex(memberName);
        }

        return -1;
    }

    const MemberDescription* ClassDescriptionLocal::GetMember(DotsC_MemberIndex index) const
    {
        int numInherited=GetNumberOfInheritedMembers();
        if (index<numInherited)
        {
            return base->GetMember(index);
        }
        return members[index-numInherited].get();
    }


    //properties
    //-----------
    DotsC_MemberIndex PropertyDescriptionLocal::GetMemberIndex(const std::string& memberName) const
    {
        return TypeUtilities::GetPropertyMemberIndex<PropertyDescription, MemberDescription>(this, memberName);
    }

    //enumerations
    //-------------
    int EnumDescriptionLocal::GetIndexOfValue(const std::string& valueName) const
    {
        return TypeUtilities::GetIndexOfEnumValue(this, valueName);
    }

    //CreateRoutines
    //----------------
    const MemberDescription* CreateRoutineDescriptionLocal::GetInParameterMember(int index) const
    {
        return parent->GetMember(parent->GetMemberIndex(parameters[index]));
    }

    const MemberDescription* CreateRoutineDescriptionLocal::GetDefaultValueMember(int index) const
    {
        const std::pair<std::string, std::pair<std::string, int> >& ref=memberValues[index];
        return parent->GetMember(parent->GetMemberIndex(ref.first));
    }

    std::pair<const ParameterDescription*, int /*paramIndex*/> CreateRoutineDescriptionLocal::GetDefaultValue(int index) const
    {
        return memberValuesParams[index];
    }

    //PropertyMappings
    //------------------
    const PropertyDescription* PropertyMappingDescriptionLocal::GetProperty() const
    {
        return property;
    }

    const ClassDescription* PropertyMappingDescriptionLocal::GetClass() const
    {
        return class_;
    }
}
}
}
}
