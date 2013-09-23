/******************************************************************************
*
* Copyright Saab AB, 2005-2013 (http://safir.sourceforge.net)
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

#include "dots_enum_description.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    EnumDescription::EnumDescription(const std::string & name,
                                     const TypeId typeId,
                                     const TypeId checksum,
                                     const std::vector<std::string> & values,
                                     AllocationHelper & allocHelper):
        m_name(name.begin(),name.end(),allocHelper.GetAllocator<char>()),
        m_typeId(typeId),
        m_checkSum(checksum),
        m_values(/*1,ShmString("",allocHelper.GetAllocator<char>()),*/allocHelper.GetAllocator<ShmString>())
    {
        m_values.reserve(values.size());
        for (std::vector<std::string>::const_iterator it = values.begin();
             it != values.end(); ++it)
        {
            m_values.push_back(ShmString(it->begin(),it->end(),allocHelper.GetAllocator<char>()));
        }
    }

    EnumDescription::~EnumDescription()
    {

    }

    Int32 EnumDescription::IndexOf(const std::string & enumValue) const
    {
        for (EnumValueVector::const_iterator it = m_values.begin();
             it != m_values.end(); ++it)
        {
            if (enumValue == it->c_str())
            {
                return static_cast<Int32>(std::distance(m_values.begin(),it));
            }
        }
        return -1;
    }

    const char *
    EnumDescription::ValueName(const EnumerationValue val) const
    {
        if (val < 0 || val >= static_cast<EnumerationValue>(m_values.size()))
        {
            return NULL;
        }
        return m_values[val].c_str();
    }

}
}
}
}

