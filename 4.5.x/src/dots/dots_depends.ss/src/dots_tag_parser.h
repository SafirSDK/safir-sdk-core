/******************************************************************************
*
* Copyright Saab AB, 2005-2013 (http://safir.sourceforge.net)
* 
* Created by: Mikael Wennerberg / stmiwn
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
#ifndef _dots_tag_parser_h
#define _dots_tag_parser_h

#include "dots_depends_defs.h"

namespace DotsDepends
{

    class DotsTagParser
    {

    public:
        DotsTagParser(boost::filesystem::path fileDirectory, bool verbose);
        ~DotsTagParser();

        int ResolveDependencies();


    private:
        bool Valid(Defs::Str_Pair pair);
        bool NamespaceExists(std::string ns);
        void CheckFile(boost::filesystem::path filename);

        boost::filesystem::path m_fileDirectory;
        Defs::ns_mapping m_map;
        Defs::ns_list m_namespaces;
        bool m_verbose;

    };
}

#endif // _dots_tag_parser_h
