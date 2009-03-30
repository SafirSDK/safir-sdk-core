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

#include "dots_xml_parser_base.h"
#include "dots_file_parser.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    void XmlParserBase::ReportError(const std::string & description)
    {
        boost::filesystem::path f;
        int l;
        FileParser::GetFileNameAndLineNumber(f, l);
        ErrorHandler::Error("File parser error", description, f, l, m_unit);
    }

    void XmlParserBase::IllegalElement(const char* el, bool startElement)
    {

        std::string descr="Invalid";
        if (startElement)
            descr+=" Start-Element '";
        else
            descr+=" End-Element '";
        descr+=el;
        descr+="'.";
        ReportError(descr);
    }

    void XmlParserBase::IllegalOrder(const char* el, bool startElement)
    {
        std::string descr="Invalid Xml ";
        if (startElement)
            descr+=" Start ";
        else
            descr+=" End ";
        descr+="Element order. Did not expect '";
        descr+=el;
        descr+="'.";
        ReportError(descr);
    }

    void XmlParserBase::ElementMismatch(const char* el)
    {
        std::string descr="Start and end element mismatch Xml. Did not expect end element '";
        descr+=el;
        descr+="'.";
        ReportError(descr);
    }

    void XmlParserBase::BadContent(const char* el, const char* content)
    {
        std::string descr="Bad content for element '";
        descr+=el;
        descr+="'. Content: '";
        descr+=content;
        descr+="'.";
        ReportError(descr);
    }
}
}
}
}
