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

#include <iostream>
#include <cstring>
#include "dots_tag_parser.h"

int main(int argc, char* argv[])
{

    // Read the files from the standard location 
    char* env = getenv(DotsDepends::Defs::SDK_ENV);

    if (env == NULL)
    {
        std::wcout<< "Tag file parsing failed to get environment variable SAFIR_SDK" << std::endl;
        return 1;
    }

    boost::filesystem::path filename(env,boost::filesystem::native);

    filename /= DotsDepends::Defs::TAG_DIR;
    if (!(boost::filesystem::exists(filename) && boost::filesystem::is_directory(filename)))
    {
        std::wcout << "The directory for tag files could not be found. Using $(SAFIR_SDK)/" << DotsDepends::Defs::TAG_DIR << " it evaluates to " << filename.string().c_str() << std::endl;;
        return 1;
    }



    bool verbose = false;
    if (argc == 2)
    {
        if (strcmp(argv[1],"-v") == 0)
        {
            verbose = true;
            std::wcout << "Verbose mode ON." << std::endl;
        }
        else
        {
            std::wcout << "Help:" << std::endl;
            std::wcout << "--------------------" << std::endl;
            std::wcout << "-v for verbose mode" << std::endl;
            return 1;
        }
    }

    DotsDepends::DotsTagParser parser(filename, verbose);

    return parser.ResolveDependencies();
}

