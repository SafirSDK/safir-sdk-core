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

#ifndef _dots_xml_parser_base_h
#define _dots_xml_parser_base_h

#include <stack>
#include "dots_internal_defs.h"
#include "dots_error_handler.h"
#include "dots_xml_elements.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    /**
     * Base class for xml-parser classes.
     */
    class XmlParserBase
    {
    public:
        XmlParserBase() {};
        virtual ~XmlParserBase() {};

        virtual void Reset() = 0;
        virtual bool StartElement(const std::string& s) = 0;
        virtual bool EndElement(const std::string& s) = 0;
        virtual bool Content(const std::string& s) = 0;


    protected:
        std::stack< const char* > m_elem;

        std::string m_unit;
        void IllegalElement(const char* el, bool startElement = true);
        void IllegalOrder(const char* el, bool startElement = true);
        void ElementMismatch(const char* el);
        void BadContent(const char* el, const char* content);

    private:
        void ReportError(const std::string & description);
    };
}
}
}
}
#endif
