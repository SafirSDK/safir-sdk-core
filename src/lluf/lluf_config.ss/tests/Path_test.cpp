/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
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
#include "../src/Path.h"
#include <iostream>

int main(const int argc, const char* argv[])
{
    using namespace Safir::Utilities::Internal;

    if (argc != 3)
    {
        std::wcout << "Expect two arguments, one directory and one file" << std::endl;
        return 1;
    }

    try
    {
        std::wcout << "test path append" << std::endl;
        {
            Path p("asdf");
            if (p.str() != "asdf")
            {
                return 1;
            }
            
            p /= "foo";
            if (p.str() != "asdf/foo" && p.str() != "asdf\\foo")
            {
                return 1;
            }

            const Path p2 = p / "bar";
            if (p2.str() != "asdf/foo/bar" && p2.str() != "asdf\\foo\\bar")
            {
                return 1;
            }
        }

        std::wcout << "test path append with trailing slashes" << std::endl;
        {
            Path p("asdf/");
            if (p.str() != "asdf/")
            {
                return 1;
            }
            
            p /= "foo/";
            if (p.str() != "asdf/foo/" && p.str() != "asdf\\foo\\")
            {
                return 1;
            }
            
            const Path p2 = p / "bar/";
            if (p2.str() != "asdf/foo/bar/" && p2.str() != "asdf\\foo\\bar\\")
            {
                return 1;
            }
        }

        std::wcout << "test Exists, IsFile and IsDirectory on existing directory and file" << std::endl;
        {
            const Path dir(argv[1]);
            const Path file(argv[2]);
            
            if (!dir.Exists())
            {
                return 1;
            }

            if (!file.Exists())
            {
                return 1;
            }

            if (dir.IsFile())
            {
                return 1;
            }

            if (!file.IsFile())
            {
                return 1;
            }

            if (!dir.IsDirectory())
            {
                return 1;
            }

            if (file.IsDirectory())
            {
                return 1;
            }
        }

        std::wcout << "test Exists, IsFile and IsDirectory on NON-existing directory and file" << std::endl;
        {
            const Path dir = Path(argv[1]) / "asdfasdf/";
            const Path file = Path(argv[2]) / "foofoo";
            
            if (dir.Exists())
            {
                return 1;
            }

            if (file.Exists())
            {
                return 1;
            }

            if (dir.IsFile())
            {
                return 1;
            }

            if (file.IsFile())
            {
                return 1;
            }

            if (dir.IsDirectory())
            {
                return 1;
            }

            if (file.IsDirectory())
            {
                return 1;
            }
        }

        std::wcout << "test empty" << std::endl;
        {
            const Path p1("");
            const Path p2("not empty");
            if (!p1.empty())
            {
                return 1;
            }

            if (p2.empty())
            {
                return 1;
            }

        }
        

    }
    catch (...)
    {
        std::wcout << "caught exception" << std::endl;
        return 1;
    }
    std::wcout << "success" << std::endl;
    return 0;
}


