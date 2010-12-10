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
#ifndef _dots_writer_h
#define _dots_writer_h

#include "dots_depends_defs.h"

namespace DotsDepends
{
    class DotsWriter
    {

    public:
        DotsWriter(boost::filesystem::path fileDirectory);
        ~DotsWriter();

        void MakeImportFile(const Defs::ns_list& list);
        void MakeCmakeFile(const Defs::ns_list& list, const Defs::ns_mapping& map);

    private:
        void WriteImports(FILE* stream, const Defs::ns_list& list, std::string part1, std::string part2);

        boost::filesystem::path m_fileDirectory;



    };

}

#endif // _dots_writer_h

