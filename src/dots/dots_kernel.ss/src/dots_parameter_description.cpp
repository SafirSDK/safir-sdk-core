/******************************************************************************
*
* Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
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

#include "dots_parameter_description.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{

    ParameterDescription::ParameterDescription(const std::string & name,
                                               const MemberType memberType,
                                               const Size arrayLength,
                                               //                             const TypeId objType,
                                               const ParameterOffsetConst & offset,
                                               AllocationHelper & allocHelper):
        m_name(name.begin(),name.end(),allocHelper.GetAllocator<char>()),
        m_memberType(memberType),
        m_arrayLength(arrayLength),
        m_offset(offset)
    {

    }

    ParameterDescription::~ParameterDescription()
    {
    }


    ParameterDescription::ParameterDescription(const ParameterDescription& other):
        m_name(other.m_name),
        m_memberType(other.m_memberType),
        m_arrayLength(other.m_arrayLength),
        m_offset(other.m_offset)
    {
    }

    ParameterDescription& ParameterDescription::operator=(const ParameterDescription& other) {
        m_name = other.m_name;
        m_memberType = other.m_memberType;
        m_arrayLength = other.m_arrayLength;
        m_offset = other.m_offset;
        return *this;
    }


    const ParameterDescription::BinaryParameterValue
    ParameterDescription::BinaryValue(const ArrayIndex index) const
    {
        //        std::wcout << "ABAQ Reading binary parameter (index = " << index << ") from address " << (const void *)((m_offset + index * sizeof(ParameterOffset)).get()) << std::endl;
        const ParameterOffsetConst paramPtrLocation = m_offset + index * sizeof(ParameterOffset);
        //        std::wcout << "ABAQ   paramPtrLocation = " << (const void *)paramPtrLocation.get() << std::endl;
        const ParameterOffsetConst paramLocation = *ParameterOffsetCast<const ParameterOffsetConst>(paramPtrLocation);
        //        std::wcout << "ABAQ   paramLocation = " << (const void *)paramLocation.get() << std::endl;
        const Size size = *ParameterOffsetCast<const Size>(paramLocation);
        //        std::wcout << "ABAQ   size = " << size << std::endl;
        const ParameterOffsetConst dataLocation = paramLocation + sizeof(Size);
        //        std::wcout << "ABAQ   dataLocation = " << (const void *)dataLocation.get() << std::endl;
        return std::make_pair(dataLocation,size);
        /*        return std::make_pair(*ParameterOffsetCast<const ParameterOffsetConst>(m_offset + index * sizeof(ParameterOffset) + sizeof(Size*)),
         *ParameterOffsetCast<const Size>(m_offset + index * sizeof(ParameterOffset)));*/
        /*
        char* binary=*reinterpret_cast<char**>(Repository::Parameters().m_parameters+m_offset+i*sizeof(char*));
        binary = Repository::m_pool.RebasePointer<char>(binary);
        size=*reinterpret_cast<Size*>(binary);
        return binary+sizeof(Size);*/
    }


    
    template <>
    const boost::interprocess::offset_ptr<const char>
    ParameterDescription::ValuePtr<char>(const ArrayIndex index) const
    {
        switch (m_memberType)
        {
        case ObjectMemberType:
        case StringMemberType:
            return *ParameterOffsetCast<const ParameterOffsetConst>(m_offset + index * sizeof(ParameterOffset));

        default:
            ENSURE(false, << "Someone tried to call the char specialization of ParameterDescription::Value on parameter of type " << m_memberType);
            return NULL; //keep compiler happy
        }
    }

}
}
}
}

