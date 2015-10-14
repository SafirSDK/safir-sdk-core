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
#include "../src/PathFinders.h"
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


    void UnsetEnv(const std::string& name)
    {
#ifdef _WIN32
        _putenv((name + "=").c_str());
#else
        unsetenv(name.c_str());
#endif
    }

#ifdef _WIN32
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
#endif
}

int main()
{
    using namespace Safir::Utilities::Internal;

#ifdef LLUF_CONFIG_READER_USE_WINDOWS
    typedef WindowsPathFinder PathFinder;
#else
    typedef LinuxPathFinder PathFinder;
#endif

    try
    {
        std::wcout << "test override config" << std::endl;
        {
            const Path p = Path("some") / "other" / "path";
            SetEnv("SAFIR_TEST_CONFIG_OVERRIDE", p.str());
            if (PathFinder::SafirTestConfigOverrideDirectory().str() != p.str())
            {
                return 1;
            }
        }

        std::wcout << "test system config" << std::endl;
        {
#ifdef LLUF_CONFIG_READER_USE_LINUX
            if (PathFinder::SystemConfigDirectory().str() != "/etc/safir-sdk-core")
            {
                return 1;
            }
#else
            if (PathFinder::SystemConfigDirectory().str() != GetCSIDL(CSIDL_COMMON_APPDATA) + "\\safir-sdk-core\\config")
            {
                return 1;
            }
#endif
        }

        std::wcout << "test user config" << std::endl;
        {
#ifdef LLUF_CONFIG_READER_USE_LINUX
            UnsetEnv("HOME");
            UnsetEnv("XDG_CONFIG_HOME");

            try
            {
                PathFinder::UserConfigDirectory();
                return 1;
            }
            catch (const std::logic_error&)
            {

            }

            SetEnv("HOME","some/path");
            if (PathFinder::UserConfigDirectory().str() != "some/path/.config/safir-sdk-core")
            {
                return 1;
            }

            SetEnv("XDG_CONFIG_HOME","some/other/path");
            if (PathFinder::UserConfigDirectory().str() != "some/other/path/safir-sdk-core")
            {
                return 1;
            }

            UnsetEnv("HOME");
            SetEnv("XDG_CONFIG_HOME","some/other/path");
            if (PathFinder::UserConfigDirectory().str() != "some/other/path/safir-sdk-core")
            {
                return 1;
            }

#else
            if (PathFinder::UserConfigDirectory().str() != GetCSIDL(CSIDL_LOCAL_APPDATA) + "\\safir-sdk-core\\config")
            {
                return 1;
            }
#endif
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
