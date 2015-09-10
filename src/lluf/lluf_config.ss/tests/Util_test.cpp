/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safir.sourceforge.net)
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


#ifdef LLUF_CONFIG_READER_USE_WINDOWS
    std::string GetCSIDL(const int csidl)
    {
        char path[MAX_PATH];

        if(SUCCEEDED(SHGetFolderPathA(NULL, 
                                     csidl|CSIDL_FLAG_CREATE, 
                                     NULL, 
                                     0, 
                                     path))) 
        {
            return path;
        }
        else
        {
            throw std::logic_error("Call to SHGetFolderPath failed!");
        }
    }

    void TestCSIDLExpand(const int csidl, const std::string& variableName)
    {
        using namespace Safir::Utilities::Internal;
        const std::string expected = "asdf" + GetCSIDL(csidl) + "foobar";
        if (ExpandSpecial("asdf@{" + variableName + "}foobar") != expected)
        {
            throw std::logic_error("TestCSIDLExpand failed");
        }
    }
#endif

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

        std::wcout << "test nothrow GetEnv" << std::endl;
        {
            if (GetEnv("PATH", std::nothrow).empty())
            {
                return 1;
            } 

            try
            {
                GetEnv("PATHLKJSADF",std::nothrow);
            }
            catch (const std::logic_error&)
            {
                return 1;
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
            TestCSIDLExpand(CSIDL_APPDATA,"CSIDL_APPDATA");
            TestCSIDLExpand(CSIDL_APPDATA,"FOLDERID_RoamingAppData");
            TestCSIDLExpand(CSIDL_LOCAL_APPDATA,"CSIDL_LOCAL_APPDATA");
            TestCSIDLExpand(CSIDL_LOCAL_APPDATA,"FOLDERID_LocalAppData");
            TestCSIDLExpand(CSIDL_COMMON_APPDATA,"CSIDL_COMMON_APPDATA");
            TestCSIDLExpand(CSIDL_COMMON_APPDATA,"FOLDERID_ProgramData");
            TestCSIDLExpand(CSIDL_MYDOCUMENTS,"CSIDL_MYDOCUMENTS");
            TestCSIDLExpand(CSIDL_MYDOCUMENTS,"FOLDERID_Documents");
            TestCSIDLExpand(CSIDL_COMMON_DOCUMENTS,"CSIDL_COMMON_DOCUMENTS");
            TestCSIDLExpand(CSIDL_COMMON_DOCUMENTS,"FOLDERID_PublicDocuments");
#endif

            std::wcout << "expand TEMP" << std::endl;
            //expand TEMP
            std::string expected;
#ifdef LLUF_CONFIG_READER_USE_WINDOWS
            expected = GetEnv("TEMP",std::nothrow);
            if (expected.empty())
            {
                expected = GetEnv("TMP",std::nothrow);
            }
#else
            expected = "/tmp";
#endif
            if (ExpandSpecial("asdf@{TEMP}foobar") != "asdf" + expected + "foobar")
            {
                std::wcout << expected.c_str() << std::endl;
                return 1;
            }
            
            std::wcout << "expand SAFIR_INSTANCE" << std::endl;

            SetEnv("SAFIR_INSTANCE", "");

            if (ExpandSpecial("widde@{SAFIR_INSTANCE}eddiw") != "widde0eddiw")
            {
                return 1;
            }

            SetEnv("SAFIR_INSTANCE", "5");

            if (ExpandSpecial("widde@{SAFIR_INSTANCE}eddiw") != "widde5eddiw")
            {
                return 1;
            }

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


        std::wcout << "test GetSafirInstanceSuffix" << std::endl;
        {
            SetEnv("SAFIR_INSTANCE", "");
            if (GetSafirInstanceSuffix() != "_0")
            {
                return 1;
            }
            
            SetEnv("SAFIR_INSTANCE", "0");
            if (GetSafirInstanceSuffix() != "_0")
            {
                return 1;
            }

            SetEnv("SAFIR_INSTANCE", "1000");
            if (GetSafirInstanceSuffix() != "_1000")
            {
                return 1;
            }

            SetEnv("SAFIR_INSTANCE", "tardis");
            try
            {
                GetSafirInstanceSuffix();
                return 1;
            }
            catch (const std::logic_error&)
            {

            }
        }
    }
    catch (const std::exception& e)
    {
        std::wcout << "caught exception: " << e.what() << std::endl;
        return 1;
    }
    std::wcout << "success" << std::endl;
    return 0;
}


