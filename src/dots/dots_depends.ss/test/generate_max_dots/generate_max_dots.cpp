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
#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable : 4702)
#endif

#include <boost/filesystem.hpp>
#include <iostream>
#include <string.h>

int main(int argc, char* argv[])
{
    const char * const SDK_ENV               = "SAFIR_SDK";
    const char * const MAX_FILE              = "/dots/dots_generated/DotsMax.Entity.dou";
    const char * const NEW_FILE              = "/dots/dots_generated/DotsMax/DotsMax.Entity";
    const char * const MAX_DIR               = "/dots/dots_generated/DotsMax";
    const char * const FIND_TOKEN            = "    <name>DotsMax.Entity</name>";

    // Read the files from the standard location 
    char* env = getenv(SDK_ENV);

    if (env == NULL)
    {
        std::wcout<< "Tag file parsing failed to get environment variable SAFIR_SDK" << std::endl;
        return 1;
    }

    int count = 0;
    bool valid = false;
    if (argc == 2)
    {
        count = atoi(argv[1]);
        if (count > 0)
        {
            valid = true;
            std::wcout << "Creating " << count << " dou files..." << std::endl;
        }
    }

    if (!valid)
    {
        std::wcout << "Help:" << std::endl;
        std::wcout << "--------------------" << std::endl;
        std::wcout << "specify how many dou files to create." << std::endl;
        return 1;
    }



    boost::filesystem::path maxDir(env,boost::filesystem::native);

    maxDir /= MAX_DIR;
    if (!boost::filesystem::exists(maxDir))
    {
        boost::filesystem::create_directory(maxDir);
    }
    else
    {
        boost::filesystem::remove_all(maxDir);
        boost::filesystem::create_directory(maxDir);
    }

    boost::filesystem::path baseFile(env,boost::filesystem::native);
    baseFile /= MAX_FILE;
    if (!boost::filesystem::exists(baseFile))
    {
        std::wcout << baseFile.string().c_str() << " could not be found." << std::endl;;
        return 1;
    }


    char newBuf[128];
    char tmpBuf[64];
    const int buff_size = 128;
    char line[buff_size];

    for (int i=0; i<count; i++)
    {
        sprintf(newBuf, "%s%i%s", NEW_FILE, i, ".dou");
        boost::filesystem::path newFile(env,boost::filesystem::native);
        newFile /= newBuf;
        
        boost::filesystem::copy_file(baseFile, newFile);        

        FILE* stream = fopen(newFile.string().c_str(), "r+");
        fpos_t pos;

        while( fgets( line, buff_size, stream ) != NULL)
        {
            if (strncmp(FIND_TOKEN, line, strlen(FIND_TOKEN)) == 0)
            {
                fsetpos(stream, &pos);
                sprintf(tmpBuf, "    <name>DotsMax.Entity%i</name>\n", i);
                fputs(tmpBuf, stream);
                break;
            }
            fgetpos(stream, &pos);
        }

        fclose(stream);
    }

    return 0;
}

