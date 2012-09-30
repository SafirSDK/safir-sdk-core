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
#include <cstdio>
#include <cstring>
#include <map>

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

        const char * const loadDll = "@LoadDll";
        const int buff_size = 128;
        char line[128];

        // find @
        while( fgets( line, buff_size, txtStream ) != NULL)
        {
            if (strncmp(loadDll, line, strlen(loadDll)) == 0)
            {
                WriteImports(cppStream, list, "                LoadDll(\"dots_generated-", "-cpp\");\n");
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

        // rebuild map
        typedef std::map<std::string, Defs::ns_list> depend_map;
        depend_map tmp_map;

        for (Defs::ns_mapping::const_iterator iter = map.begin();iter != map.end(); ++iter)
        {
            depend_map::iterator map_iter = tmp_map.find(iter->first);
            if (map_iter == tmp_map.end())
            {
                // key not found, create a new entry
                tmp_map[iter->first] = Defs::ns_list();
            }
            // add dependecy to vector
            tmp_map[iter->first].push_back(iter->second);
        }

        // write dependencies
        for (depend_map::const_iterator iter = tmp_map.begin();iter != tmp_map.end(); ++iter)
        {
            line = "SET(DOTS_NS_";
            line.append(iter->first.c_str());

            // loop through the dependencies for this namespace.
            for (Defs::ns_list::const_iterator vect_iter = iter->second.begin();vect_iter != iter->second.end(); ++vect_iter)
            {
                line.append(" \"");
                line.append(vect_iter->c_str());
                line.append("\"");
            }

            line.append(")\n");
            fputs(line.c_str(),stream);
        }


        fclose(stream);
   }


}
