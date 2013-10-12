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

#ifndef _dots_enum_parser_h
#define _dots_enum_parser_h

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
     * Parses dou-files containing enum type definitions.
     */
    class EnumParser : public XmlParserBase
    {
    public:
        EnumParser();

        //Inherited methods
        void Reset();
        bool StartElement(const std::string& s);
        bool EndElement(const std::string& s);
        bool Content(const std::string& str);

        //special PropertyParser methods
        DobEnumerations & Result() {return m_result;}

        int GetIndex(TypeId t);

    private:
        DobEnumerations m_result;
        DobEnumeration m_tmpEnumeration;
        std::string m_tmpEnumVal;

        bool m_uSummary;

        const char* ValidElement(const char* e);

        typedef enum
        {
            ENUM_EEL = 0,
            ENUM_NAME_EEL,
            VALUES_EEL,
            VALUE_EEL
        } ExpectedElement;

        ExpectedElement m_expected;
    };
}
}
}
}
#endif
