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

#ifndef _dots_property_mapping_parser_h
#define _dots_property_mapping_parser_h

#include "dots_xml_parser_base.h"
#include "dots_temporary_descriptions.h"
#include <boost/filesystem.hpp>
namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    /**
     * Parses dom-files containing property mappings.
     */
    class PropertyMappingParser  : public XmlParserBase
    {
    public:
        //Constructor and Destructor
        PropertyMappingParser();
        ~PropertyMappingParser();

        //Inherited methods
        void Reset();
        bool StartElement(const std::string& element);
        bool EndElement(const std::string& elsement);
        bool Content(const std::string& str);

        //special PropertyParser methods
        DobPropertyMappings & Result() {return m_PropertyMappings;}
        void ProcessDOM();

        //a rather roundabout way of setting the string, but we do it to support boost.filesystem v2 and v3
        void SetFileName(const boost::filesystem::path & filename) {m_currentFileName = std::string(filename.filename().c_str());}
    private:

        bool ValidElement(const std::string & element);

        std::vector<std::string> m_ParseStack;


        DobPropertyMappings m_PropertyMappings;

        SimpleDOM::Element * m_CurrentElement;
        std::string m_currentFileName;

    };
}
}
}
}
#endif

