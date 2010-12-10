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
#include <iostream>
#include <stdio.h>
#include <string.h>

#include "dots_depends_defs.h"
#include "dots_writer.h"

namespace DotsDepends
{

    DotsWriter::DotsWriter(boost::filesystem::path fileDirectory):
    m_fileDirectory(fileDirectory)
    {
    }

    DotsWriter::~DotsWriter()
    {
    }

    void DotsWriter::MakeImportFile(const Defs::ns_list & list)
    {
        boost::filesystem::path path = m_fileDirectory;
        path/="..";

        boost::filesystem::path txtPath = path;
        txtPath/=Defs::TXT_DEPEND_FILE;

        boost::filesystem::path cppPath = path;
        cppPath/=Defs::CPP_DEPEND_FILE;


        FILE* cppStream = fopen(cppPath.string().c_str(), "w");
        FILE* txtStream = fopen(txtPath.string().c_str(), "r");
        
        if (txtStream == NULL)
        {
            std::wcout << "Failed to open file: " << path.string().c_str() << std::endl;
            return;
        }

        const char * const part1 = "@LoadLibraryD";
        const char * const part2 = "@LoadLibrary";
        const char * const part3 = "@dlopen"; 
        const int buff_size = 128;
        char line[128];

        // find @
        while( fgets( line, buff_size, txtStream ) != NULL)
        {
            if (strncmp(part1, line, strlen(part1)) == 0)
            {
                WriteImports(cppStream, list, "                LoadLibrary(L\"dots_generated-", "-cppd.dll\");\n");
            }
            else if (strncmp(part2, line, strlen(part2)) == 0)
            {
                WriteImports(cppStream, list, "                LoadLibrary(L\"dots_generated-", "-cpp.dll\");\n");
            }
            else if (strncmp(part3, line, strlen(part3)) == 0)
            {
                WriteImports(cppStream, list, "                lib_handle = dlopen(\"../lib/libdots_generated-", "-cpp.so\", RTLD_LAZY);\n");
            }
            else
            {
                fwrite(line, sizeof( char ), strlen(line), cppStream);
            }
        }

        fclose(txtStream);
        fclose(cppStream);
    }

    void DotsWriter::WriteImports(FILE* stream, const Defs::ns_list& list, std::string part1, std::string part2)
    {
        Defs::ns_list::const_iterator iter = list.begin();

        while (iter != list.end())
        {
            //std::wcout << (*iter).c_str() << std::endl;
            fputs(part1.c_str(),stream);
            fputs((*iter).c_str(),stream);
            fputs(part2.c_str(),stream);
            iter++;
        }    
    }

   void DotsWriter::MakeCmakeFile(const Defs::ns_list& list, const Defs::ns_mapping& map)
   {
        boost::filesystem::path path = m_fileDirectory;
        path/="..";

        boost::filesystem::path txtPath = path;
        txtPath/=Defs::CMAKE_DEPEND_FILE;

        FILE* stream = fopen(txtPath.string().c_str(), "w");        

        // Namespaces
        std::string line = "SET(DOTS_NS";
        Defs::ns_list::const_iterator nsIter = list.begin();

        while (nsIter != list.end())
        {
            line.append(" ");
            line.append(*nsIter);
            nsIter++;
        }    

        line.append(")\n");
        fputs(line.c_str(),stream);

        // Dependencies
        Defs::ns_mapping::const_iterator iter = map.begin();

        while (iter != map.end())
        {
            line = "SET(DOTS_NS_";
            line.append(iter->first.c_str());
            line.append(" \"");
            line.append(iter->second.c_str());
            line.append("\")\n");
            fputs(line.c_str(),stream);
            iter++;
        }    

        fclose(stream);
   }


}
