/******************************************************************************
*
* Copyright Saab AB, 2013 (http://www.safirsdk.com)
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
#include "../src/Util.h"
#include <iostream>

namespace
{
    void SetEnv(const std::string& name, const std::string& value)
    {
#ifdef _WIN32
        _putenv((name + "=" + value).c_str());

#else
        setenv(name.c_str(),value.c_str(),1);
#endif
    }
}

int main()
{
    using namespace Safir::Utilities::Internal;

    try
    {
        std::wcout << "test GetEnv" << std::endl;
        {
            if (GetEnv("PATH").empty())
            {
                return 1;
            } 

            try
            {
                GetEnv("PATHLKJSADF");
                return 1;
            }
            catch (const std::logic_error&)
            {

            }
        }

#ifdef LLUF_CONFIG_READER_USE_WINDOWS
        std::wcout << "test GetFolderPathFromCSIDL" << std::endl;
        {
            if (GetFolderPathFromCSIDL(CSIDL_LOCAL_APPDATA).str().empty())
            {
                return 1;
            } 

            try
            {
                //try an id that probably does not exist...
                GetFolderPathFromCSIDL(1003438789);
                return 1;
            }
            catch (const std::logic_error&)
            {

            }
        }
#endif


        std::wcout << "test ExpandSpecial" << std::endl;
        {
            //expand unknown
            try
            {
                ExpandSpecial("asdfasdf@{HELLOWORLD}");
                return 1;
            }
            catch (const std::logic_error&)
            {

            }

#ifdef LLUF_CONFIG_READER_USE_WINDOWS
            const std::string expected = "asdf" + GetFolderPathFromCSIDL(CSIDL_APPDATA).str() + "foobar";
            todo!
                add an environment variable in there as well
#endif

        }

        std::wcout << "test ExpandEnvironment" << std::endl;
        {
            //expand unknown
            try
            {
                ExpandEnvironment("asdfasdf$(HELLOWORLD)");
                return 1;
            }
            catch (const std::logic_error&)
            {

            }

            if (ExpandEnvironment("asdfasdf$(PATH)dfff") != "asdfasdf" + GetEnv("PATH") + "dfff")
            {
                return 1;
            }
            
            SetEnv("SOME_NUMBER", "0");
            SetEnv("FOO_BAR_01222", "blahonga");
            
            if (ExpandEnvironment("uu$(PATH)iiii$(FOO_BAR_$(SOME_NUMBER)1222)ooo") != 
                "uu" + GetEnv("PATH") + "iiiiblahongaooo")
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


