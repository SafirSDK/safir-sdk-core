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

#include "dots_member_description.h"
#include "dots_class_description.h"
#include "dots_enum_description.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{

    MemberDescription::MemberDescription(const std::string & name,
                                         const MemberType type,
                                         const Size arrayLength,// m_arrayLength=1 -> normal memer, m_arrayLength>1 -> array
                                         const ClassDescriptionConstPtr classDesc,
                                         const EnumDescriptionConstPtr enumDesc,
                                         const Size dataLength,
                                         AllocationHelper & allocHelper):
        m_name(name.begin(),name.end(),allocHelper.GetAllocator<char>()),
        m_class(classDesc),
        m_enum(enumDesc),
        m_type(type),
        m_dataLength(dataLength),
        m_arrayLength(arrayLength)
    {
        switch (type)
        {
        case ObjectMemberType:
            ENSURE(classDesc != NULL && enumDesc == NULL && dataLength == 0, << "Illegal input to MemberDescription constructor. type = "
                   << type << ", classDesc = " << std::boolalpha << (classDesc != NULL) << " enumDesc = " << (enumDesc != NULL) << " dataLength = " << (dataLength != 0));
            break;

        case EnumerationMemberType:
            ENSURE(classDesc == NULL && enumDesc != NULL && dataLength == 0, << "Illegal input to MemberDescription constructor. type = "
                   << type << ", classDesc = " << std::boolalpha << (classDesc != NULL) << " enumDesc = " << (enumDesc != NULL) << " dataLength = " << (dataLength != 0));
            break;

        case StringMemberType:
            ENSURE(classDesc == NULL && enumDesc == NULL, << "Illegal input to MemberDescription constructor. type = "
                   << type << ", classDesc = " << std::boolalpha << (classDesc != NULL) << " enumDesc = " << (enumDesc != NULL) << " dataLength = " << dataLength);
            break;

        default:
            ENSURE(classDesc == NULL && enumDesc == NULL && dataLength == 0, << "Illegal input to MemberDescription constructor. type = "
                   << type << ", classDesc = " << std::boolalpha << (classDesc != NULL) << " enumDesc = " << (enumDesc != NULL) << " dataLength = " << (dataLength != 0));
            break;
        }
    }


    MemberDescription::~MemberDescription()
    {

    }

    MemberDescription::MemberDescription(const MemberDescription& other):
        m_name(other.m_name),
        m_class(other.m_class),
        m_enum(other.m_enum),
        m_type(other.m_type),
        m_dataLength(other.m_dataLength),
        m_arrayLength(other.m_arrayLength)
    {

    }

    MemberDescription& MemberDescription::operator=(const MemberDescription& other)
    {
        m_name = other.m_name;
        m_class = other.m_class;
        m_enum = other.m_enum;
        m_type = other.m_type;
        m_dataLength = other.m_dataLength;
        m_arrayLength = other.m_arrayLength;
        return *this;
    }


    const ClassDescriptionConstPtr
    MemberDescription::Class() const
    {
        ENSURE(GetMemberType() == ObjectMemberType, << "Illegal to call Class() on MemberDescription that has type: " << GetMemberType());
        return m_class;
    }


    const EnumDescriptionConstPtr
    MemberDescription::Enum() const
    {
        ENSURE(GetMemberType() == EnumerationMemberType, << "Illegal to call Enum() on MemberDescription that has type: " << GetMemberType());
        return m_enum;
    }


    TypeId MemberDescription::GetTypeId() const
    {
        switch (m_type)
        {
        case ObjectMemberType:
            return m_class->GetTypeId();
        case EnumerationMemberType:
            return m_enum->GetTypeId();
        default:
            ENSURE(false, << "Only Object or Enum member descriptions have TypeIds! Type = " << m_type);
            return -1;
        }
    }
}
}
}
}
