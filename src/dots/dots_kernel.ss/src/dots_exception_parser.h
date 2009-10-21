/******************************************************************************
*
* Copyright Saab AB, 2009 (http://www.safirsdk.com)
* 
* Created by: Lars Hagström / stlrha
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

#ifndef __DOTS_EXCEPTION_PARSER_H__
#define __DOTS_EXCEPTION_PARSER_H__

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
     * A parser for exceptions
     */
    class ExceptionParser:
        public XmlParserBase
    {
    public:
        //Constructor and Destructor
        ExceptionParser();
        //~ExceptionParser();

        //Inherited methods
        void Reset();
        bool StartElement(const std::string& element);
        bool EndElement(const std::string& element);
        bool Content(const std::string& str);

        const DobExceptions& Result() const {return m_exceptions;}
    private:
        bool ValidElement(const std::string& element);

        std::vector<std::string> m_parseStack;

        DobExceptions m_exceptions;

        std::string m_currentName;
        std::string m_currentBaseClass;
    };
}
}
}
}

#endif

