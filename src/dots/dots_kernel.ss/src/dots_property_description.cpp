/******************************************************************************
*
* Copyright Saab AB, 2004-2013 (http://safir.sourceforge.net)
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

#include "dots_property_description.h"
#include "dots_repository.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    PropertyDescription::PropertyDescription(const std::string & name,
                                             const TypeId typeId,
                                             const Size noMembers,
                                             AllocationHelper & allocHelper):
        m_name(name.begin(),name.end(),allocHelper.GetAllocator<char>()),
        m_typeId(typeId),
        m_members(allocHelper.GetAllocator<MemberDescription>())
    {
        m_members.reserve(noMembers);
    }


    PropertyDescription::~PropertyDescription()
    {

    }


    MemberIndex PropertyDescription::GetMemberIndexFromName(const std::string & name) const
    {
        for (MemberVector::const_iterator it = m_members.begin();
             it != m_members.end(); ++it)
        {
            if (name == it->Name())
            {
                return static_cast<MemberIndex>(std::distance(m_members.begin(),it));
            }
        }
        return -1;
    }

}
}
}
}
