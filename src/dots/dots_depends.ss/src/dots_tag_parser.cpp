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
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/convenience.hpp>

#include <iostream>
#include <vector>
#include <cstring>
#include <cstdio>

#include "dots_depends_defs.h"
#include "dots_tag_parser.h"
#include "dots_writer.h"

namespace DotsDepends
{
    DotsTagParser::DotsTagParser(boost::filesystem::path fileDirectory, bool verbose):
    m_fileDirectory(fileDirectory),
    m_verbose(verbose)
    {
    }

    DotsTagParser::~DotsTagParser()
    {
    }

    int DotsTagParser::ResolveDependencies()
    {
        for (boost::filesystem::directory_iterator path = boost::filesystem::directory_iterator(m_fileDirectory);
            path != boost::filesystem::directory_iterator(); ++path)
        {
            const std::string extension = boost::filesystem::extension(*path);

            if ( boost::filesystem::is_directory(*path) )
            {
                continue;
            }

            if (extension != Defs::TAG_FILE_EXT)
            {
                if (m_verbose)
                {
                    std::wcout << "Skipped file: " << (*path).string().c_str() << std::endl;
                }
                continue;
            }            

            CheckFile(*path);

        }

        DotsWriter writer(m_fileDirectory);
        writer.MakeImportFile(m_namespaces);
        writer.MakeCmakeFile(m_namespaces, m_map);


        if (m_verbose)
        {
            Defs::ns_list::iterator nsIter = m_namespaces.begin();

            std::wcout << std::endl << "Found namespaces:" << std::endl;
            std::wcout << "-----------------" << std::endl;
            while (nsIter != m_namespaces.end())
            {
                std::wcout << (*nsIter).c_str() << std::endl;
                nsIter++;
            }

            std::wcout << "-----------------" << std::endl << std::endl;
            std::wcout << "Dependencies:" << std::endl;
            std::wcout << "-----------------" << std::endl;
            Defs::ns_mapping::iterator iter = m_map.begin();

            while (iter != m_map.end())
            {
                std::wcout << iter->first.c_str()<< " dependens on " << iter->second.c_str() << std::endl;
                iter++;
            }    

        }

        return 0;
    }

    bool DotsTagParser::Valid(Defs::Str_Pair pair)
    {
        if(pair.first.compare(pair.second) == 0)
            return false; // depends on itself

        if(pair.second.compare("Dots") == 0)
            return false; // depends on dots - all do...

        // check if already exist
        Defs::ns_mapping::iterator iter = m_map.begin();

        while (iter != m_map.end())
        {
            if (iter->first.compare(pair.first) == 0 && iter->second.compare(pair.second) == 0)
                return false;

            iter++;
        }

        return true;
    }

    bool DotsTagParser::NamespaceExists(std::string ns)
    {
        Defs::ns_list::iterator iter = m_namespaces.begin();

        while (iter != m_namespaces.end())
        {
            if ((*iter).compare(ns) == 0)
                return true;

            iter++;
        }

        return false;
    }


    void DotsTagParser::CheckFile(boost::filesystem::path filename)
    {
        FILE* stream = fopen(filename.string().c_str(), "r");
        if (stream == NULL)
        {
            std::wcout << "Failed to open file: " << filename.string().c_str() << std::endl;
            return;
        }

        if (m_verbose)
        {
            std::wcout << "Processing file: " << filename.string().c_str() << std::endl;
        }

        const int buff_size = 128;
        char line[buff_size];
        const char delimiter[] = ".\t\n";
        char* token;

        //find namspace token
        while( fgets( line, buff_size, stream ) != NULL)
        {
            if (strncmp(Defs::NAMESPACE_STR, line, strlen(Defs::NAMESPACE_STR)) == 0)
            {
                break;
            }
        }

        if (!fgets( line, buff_size, stream ))
        {
            std::wcout << "Parse error: " << filename.string().c_str() << std::endl;
            return;            
        }
        if (line[0] == ' ')
        {
            token = strtok(line, delimiter);
            while (*token == ' ') token++;  // trim
            if (!NamespaceExists(token))
            {
                m_namespaces.push_back(token);            
            }
        }

        

        // find dependency token
        while( fgets( line, buff_size, stream ) != NULL)
        {
            if (strncmp(Defs::DEPEND_STR, line, strlen(Defs::DEPEND_STR)) == 0)
            {
                break;
            }
        }

        // check dependencies
        bool found = false;
        const char fileDelimiter[] = "-";
        char baseName[256];
        strcpy(baseName, boost::filesystem::basename(filename).c_str());
        const char* const fileToken = strtok(baseName, fileDelimiter);;

        while( fgets( line, buff_size, stream ) != NULL)
        {
            if (line[0] != ' ')
            {
                break;
            }

            token = strtok(line, delimiter);

            if (token != NULL)
            {
                //trim
                while (*token == ' ') token++;

                if (!found)
                {
                    found = true;
                }


                if (strlen(token) > 0 && strlen(fileToken) > 0)
                {
                   std::string strToken(token);
                   std::string strFileToken(fileToken);

                   Defs::Str_Pair pair = Defs::Str_Pair(strFileToken, strToken);
                   if (Valid(pair))
                       m_map.push_back(pair);

                   if (!NamespaceExists(strFileToken))
                       m_namespaces.push_back(strFileToken);
                }
            }

        }

        fclose(stream);
    }

}
