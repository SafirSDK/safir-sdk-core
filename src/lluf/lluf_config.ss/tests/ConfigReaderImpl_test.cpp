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
#include "../src/ConfigReaderImpl.h"
#include <iostream>

namespace
{
    using namespace Safir::Utilities::Internal;
    
    static Path runtime_config;
    static Path system_config;
    static Path user_config;

    struct TestDirs
    {
        static Path SystemConfigDirectory()
        {
            return system_config;
        }
        
        static Path UserConfigDirectory()
        {
            return user_config;
        }

        static Path SafirRuntimeConfigDirectory()
        {
            return runtime_config;
        }
    };

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
#endif
}
int main(const int argc, const char* argv[])
{
    using namespace Safir::Utilities::Internal;

    if (argc != 2)
    {
        std::wcout << "Expect one directory as arg" << std::endl;
        return 1;
    }
    const Path dir(argv[1]);

    try
    {

        std::wcout << "fail to find" << std::endl;
        {
            try
            {
                ConfigReaderImpl impl;
                impl.Read<TestDirs>();
                impl.ExpandEnvironmentVariables();
                return 1;
            }
            catch(const std::logic_error&)
            {
                
            }
        }

        std::wcout << "find safir" << std::endl;
        {
            ::runtime_config = dir / "runtime";
            
            ConfigReaderImpl impl;
            impl.Read<TestDirs>();
            impl.ExpandEnvironmentVariables();

            if (impl.m_locations.get<std::string>("question") != "blahonga")
            {
                return 1;
            }

            if (impl.m_logging.get<std::string>("answer") != "rymdbörje")
            {
                return 1;
            }

            if (impl.m_typesystem.get<std::string>("what") != "runtime_config")
            {
                return 1;
            }

        }

        std::wcout << "find user" << std::endl;
        {
            ::user_config = dir / "user";
            SetEnv("SOME_ENVIRONMENT_VARIABLE_THAT_I_NEED","myvalue");
            ConfigReaderImpl impl;
            impl.Read<TestDirs>();
            impl.ExpandEnvironmentVariables();

            if (impl.m_locations.get<std::string>("question") != "9*5")
            {
                return 1;
            }
            
            if (impl.m_locations.get<std::string>("env") != "myvalue:blahonga")
            {
                std::wcout << impl.m_locations.get<std::string>("env").c_str() << std::endl;
                return 1;
            }

            if (impl.m_logging.get<std::string>("answer") != "42")
            {
                return 1;
            }

            if (impl.m_typesystem.get<std::string>("what") != "user")
            {
                return 1;
            }
        }

        std::wcout << "missing env" << std::endl;
        {
            ::user_config = dir / "user";
            UnsetEnv("SOME_ENVIRONMENT_VARIABLE_THAT_I_NEED");

            try
            {
                ConfigReaderImpl impl;
                impl.Read<TestDirs>();
                impl.ExpandEnvironmentVariables();
        
                return 1;
            }
            catch (const std::logic_error&)
            {

            }
        }

        std::wcout << "find system" << std::endl;
        {
            ::system_config = dir / "system";
            
            ConfigReaderImpl impl;
            impl.Read<TestDirs>();
            impl.ExpandEnvironmentVariables();

            if (impl.m_locations.get<std::string>("question") != "bryna nuppa fjässa sponken")
            {
                return 1;
            }

            if (impl.m_logging.get<std::string>("answer") != "razor")
            {
                return 1;
            }

            if (impl.m_typesystem.get<std::string>("what") != "system")
            {
                return 1;
            }
        }


        std::wcout << "broken" << std::endl;
        {
            try
            {
                ::system_config = dir / "broken";
                ConfigReaderImpl impl;
                impl.Read<TestDirs>();
                impl.ExpandEnvironmentVariables();
                return 1;
            }
            catch(const std::logic_error&)
            {
                
            }
        }

#ifdef LLUF_CONFIG_READER_USE_WINDOWS
        std::wcout << "test windows special folders" << std::endl;
        {
            ::system_config = dir / "windows";
            
            ConfigReaderImpl impl;
            impl.Read<TestDirs>();
            impl.ExpandEnvironmentVariables();

            if (impl.m_locations.get<std::string>("special") != GetCSIDL(CSIDL_COMMON_APPDATA) + "/asdf/asdf")
            {
                return 1;
            }
        }
#endif

    }
    catch (const std::exception& e)
    {
        std::wcout << "caught exception: " << e.what() << std::endl;
        return 1;
    }
    catch (...)
    {
        std::wcout << "caught exception" << std::endl;
        return 1;
    }
    std::wcout << "success" << std::endl;
    return 0;
}


