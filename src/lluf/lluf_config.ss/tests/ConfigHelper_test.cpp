/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safir.sourceforge.net)
*
* Created by: Anders Widén / anders.widen@consoden.se
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
#include <Safir/Utilities/Internal/ConfigReader.h>
#include <boost/filesystem/path.hpp>
#include <iostream>

int main()
{
    try
    {
        using namespace Safir::Utilities::Internal;
        ConfigReader reader;

        std::vector<std::string> douFilePaths = ConfigHelper::GetDouDirectories(reader);

        if (douFilePaths.size() != 3)
        {
            std::wcout << "Expected 3 file paths, got " << douFilePaths.size() << std::endl;
            return 1;
        }

        if (douFilePaths[0] != "/path/to/default/directory" ||
            douFilePaths[1] != "/path/to/some/other/directory" ||
            douFilePaths[2] != "/path/to/default/AnotherOverride")
        {
            std::wcout << "Unexpected path!\n" 
                       << " " << douFilePaths[0].c_str() << "\n"
                       << " " << douFilePaths[1].c_str() << "\n"
                       << " " << douFilePaths[2].c_str() << std::endl;

            return 1;
        }

    }
    catch (const std::exception& e)
    {
        std::wcout << "exception:" << e.what() <<  std::endl;
        return 1;
    }
    std::wcout << "success" << std::endl;
    return 0;
}


