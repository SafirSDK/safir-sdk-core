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

#ifndef _dots_member_description_h
#define _dots_member_description_h

#include "dots_internal_defs.h"
#include "dots_blob_layout.h"
#include "dots_allocation_helper.h"
#include "dots_fwd.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{

    /**
     * Contains information about a member in a Dob-class.
     */
    class MemberDescription
    {
    public:

        /*
         * This table describes which parameters are used (M, for mandatory) depending on
         * the MemberType of the member.
         * The other parameters are mandatory for all types
         *
         *  type   | classDesc | enumDesc | dataLength |
         * --------+-----------+----------+------------+-
         *  class  |    M      |    -     |     -      |
         * --------+-----------+----------+------------+-
         *  enum   |    -      |    M     |     -      |
         * -------------------+----------+------------+-
         *  string |    -      |    -     |     M      |
         * --------+-----------+----------+------------+-
         *  others |    -      |    -     |     -      |
         * --------+-----------+----------+------------+-
         *
        */
        MemberDescription(const std::string & name,
                          const MemberType type,
                          const Size arrayLength,// m_arrayLength=1 -> normal memer, m_arrayLength>1 -> array
                          const ClassDescriptionConstPtr classDesc,
                          const EnumDescriptionConstPtr enumDesc,
                          const Size dataLength,
                          AllocationHelper & allocHelper);

        MemberDescription(const MemberDescription& other);
        MemberDescription& operator=(const MemberDescription& other);

        ~MemberDescription();

        const char* Name() const {return m_name.c_str();}

        const ClassDescriptionConstPtr Class() const;
        const EnumDescriptionConstPtr Enum() const;

        inline MemberType GetMemberType() const {return m_type;}
        inline Size DataLength() const {return m_dataLength;}
        inline Size ArrayLength() const {return m_arrayLength;}
        TypeId GetTypeId() const;

    private:
        ShmString m_name;   //element name
        ClassDescriptionConstPtr m_class;
        EnumDescriptionConstPtr m_enum;
        MemberType m_type;          //element type
        Size m_dataLength;              //valid if m_type is string
        Size m_arrayLength;         //m_arrayLength=1 -> normal memer, m_arrayLength>1 -> array
    };

    //Collection type for Members
    typedef AllocationHelper::Containers<MemberDescription>::vector MemberVector;

}
}
}
}
#endif
