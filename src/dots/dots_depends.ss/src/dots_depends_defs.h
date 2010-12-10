/******************************************************************************
*
* Copyright Saab AB, 2005-2010 (http://www.safirsdk.com)
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
#ifndef _dots_depend_defs_h
#define _dots_depend_defs_h

#include <boost/filesystem.hpp>
#include <vector>

namespace DotsDepends
{
    namespace Defs
    {

        const char * const SDK_ENV               = "SAFIR_SDK";
        const char * const TAG_DIR               = "/dots/dots_generated/tags";
        const char * const DEPEND_STR            = "DEPENDENCYBASE:";
        const char * const CPP_DEPEND_FILE       = "dll_imports.cpp";
        const char * const TXT_DEPEND_FILE       = "dll_imports.cpp_template";
        const char * const CMAKE_DEPEND_FILE     = "cmake_depend.txt";
        const char * const TAG_FILE_EXT          = ".txt";

        typedef std::pair<std::string, std::string> Str_Pair;
        typedef std::vector<Str_Pair> ns_mapping;
        typedef std::vector<std::string> ns_list;
    }
}

#endif // _dots_depend_defs_h

