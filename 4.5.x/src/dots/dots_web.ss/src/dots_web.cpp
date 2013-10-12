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
#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable : 4702)
#endif

#include <iostream>
#include <string.h>
#include <boost/filesystem.hpp>
#include "reader.h"

int main(int argc, char* argv[])
{

    // Read the files from the standard location 
    char* env = getenv("SAFIR_RUNTIME");

    if (env == NULL)
    {
        std::wcout<< "Tag file parsing failed to get environment variable SAFIR_RUNTIME" << std::endl;
        return 1;
    }

    //boost::filesystem::path dotsDir(env,boost::filesystem::native);

    //dotsDir /= "/dots/dots_generated";
    //if (!(boost::filesystem::exists(dotsDir) && boost::filesystem::is_directory(dotsDir)))
    //{
    //    std::wcout << "The directory for dots files could not be found. Using $(SAFIR_RUNTIME)/dots/dots_generated" << " it evaluates to " << dotsDir.string().c_str() << std::endl;;
    //    return 1;
    //}


    boost::filesystem::path filename(env,boost::filesystem::native);

    filename /= "/data/text/web/index.html.template";
    if (!boost::filesystem::exists(filename))
    {
        std::wcout << "$(SAFIR_RUNTIME)/data/text/web/index.html.template could not be found," << " it evaluates to " << filename.string().c_str() << std::endl;;
        return 1;
    }


    DotsWeb::Reader reader(filename);
    

    return reader.Run();
}

