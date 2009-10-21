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

#ifndef _dots_property_parser_h
#define _dots_property_parser_h

#include "dots_xml_parser_base.h"
#include "dots_temporary_descriptions.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    /**
     * Parses dou-files containing property definitions.
     */
    class PropertyParser  : public XmlParserBase
    {
    public:

        //Constructor
        PropertyParser();

        //Inherited methods
        void Reset();
        bool StartElement(const std::string& s);
        bool EndElement(const std::string& s);
        bool Content(const std::string& str);

        //special PropertyParser methods
        int GetIndex(TypeId t);
        DobProperties & Result() {return m_result;}

    private:
        DobProperties m_result;
        DobProperty m_tmpProperty;
        DobMember m_tmpMember;

        const char* ValidElement(const char* e);

        bool m_uProperty;
        bool m_uMembers;
        bool m_uMember;
        bool m_uSummary;

        typedef enum
        {
            PROPERTY_PEL,
            PROPERTY_NAME_PEL,
            MEMBERS_PEL,
            MEMBER_PEL,
            MEMBER_NAME_PEL,
            MEMBER_TYPE_PEL,
            ARRAY,
        } ExpectedElement;

        ExpectedElement m_expected;

    };
}
}
}
}
#endif
