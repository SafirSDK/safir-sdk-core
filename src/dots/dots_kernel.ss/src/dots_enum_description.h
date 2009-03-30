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

#ifndef _dots_enum_description_h
#define _dots_enum_description_h

#include "dots_internal_defs.h"
#include "dots_allocation_helper.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    typedef AllocationHelper::Containers<ShmString>::vector EnumValueVector;

    /**
     * Contains information about a Dob enum type specified in xml-files.
     */
    class EnumDescription
    {
    public:
        EnumDescription(const std::string & name,
                        const TypeId typeId,
                        const TypeId checksum,
                        const std::vector<std::string> & values,
                        AllocationHelper & allocHelper);

        ~EnumDescription();

        const char * Name() const {return m_name.c_str();}

        Int32 IndexOf(const std::string & enumValue) const;

        const TypeId GetTypeId() const {return m_typeId;}
        const TypeId Checksum() const {return m_checkSum;}

        Size NumberOfValues() const {return static_cast<Size>(m_values.size());}
        const char * ValueName(const EnumerationValue val) const; //returns NULL if no such value found

        //        void Dump() const;
    private:
        ShmString m_name;
        TypeId m_typeId;
        TypeId m_checkSum;
        EnumValueVector m_values;
    };
}
}
}
}
#endif
