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

#ifndef _dots_property_description_h
#define _dots_property_description_h

#include "dots_allocation_helper.h"
#include "dots_internal_defs.h"
#include "dots_member_description.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    /**
     * Contains information about a Dob-property specified in xml-files.
     */
    class PropertyDescription
    {
    public:
        PropertyDescription(const std::string & name,
                            const TypeId typeId,
                            const Size noMembers,
                            AllocationHelper & allocHelper);

        ~PropertyDescription();


        const char * Name() const {return m_name.c_str();}
        TypeId GetTypeId() const {return m_typeId;}

        void AddMember(const MemberDescription & member) {m_members.push_back(member);}
        const MemberDescription * GetMember(const MemberIndex member) const {return &m_members.at(member);}

        Size NumberOfMembers() const {return static_cast<Size>(m_members.size());}

        //returns -1 if not found
        MemberIndex GetMemberIndexFromName(const std::string & name) const;

    private:
        ShmString m_name; //including namespace ex: MyNamespace1.MyNamespace2.MyClass
        TypeId m_typeId;
        MemberVector m_members;
    };
}
}
}
}
#endif

