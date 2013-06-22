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
#include <Safir/Utilities/Internal/ConfigReader.h>
#include <boost/property_tree/ini_parser.hpp>
#include <iostream>

#if defined(linux) || defined(__linux) || defined(__linux__)
#define LLUF_CONFIG_READER_USE_LINUX
#  include <sys/types.h>
#  include <sys/stat.h>
#  include <unistd.h>
#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
#define LLUF_CONFIG_READER_USE_WINDOWS
#  include <windows.h>
#  include <Shlobj.h>
#else
#  error You need to implement ConfigReader for this platform!
#endif


namespace
{
#ifdef LLUF_CONFIG_READER_USE_LINUX
    const char PATH_SEPARATOR = '/';
#else
    const char PATH_SEPARATOR = '\\';
    const char ALTERNATE_PATH_SEPARATOR = '/';
#endif


    /** 
     * A simple path class that is okay to use during elaboration
     * (which boost::filesystem::path is not...
     */
    class Path
    {
    public:
        Path() {}
        explicit Path(const std::string& path):m_path(path) {}

        /** 
         * True if it is possible to call lstat or GetFileAttributesW on the path.
         * Basically this means that there is something there that could be a file
         * or a directory that we can at least look at.
         */
        bool Exists() const
        {
#ifdef LLUF_CONFIG_READER_USE_LINUX
            struct stat pathstat;
            const int res = ::lstat(m_path.c_str(), &pathstat);
            if (res != 0)
            {
                return false;
            }
            return 
                S_ISREG(pathstat.st_mode) ||
                S_ISDIR(pathstat.st_mode) ||
                S_ISCHR(pathstat.st_mode) ||
                S_ISBLK(pathstat.st_mode) ||
                S_ISFIFO(pathstat.st_mode) ||
                S_ISLNK(pathstat.st_mode) ||
                S_ISSOCK(pathstat.st_mode);                
#else
            DWORD attr(::GetFileAttributesA(m_path.c_str()));
            return attr != INVALID_FILE_ATTRIBUTES;
#endif
        }
        
        bool IsFile() const
        {
#ifdef LLUF_CONFIG_READER_USE_LINUX
            struct stat pathstat;
            const int res = ::lstat(m_path.c_str(), &pathstat);
            if (res != 0)
            {
                return false;
            }
            return S_ISREG(pathstat.st_mode);
#else
            DWORD attr(::GetFileAttributesA(m_path.c_str()));
            if (attr == INVALID_FILE_ATTRIBUTES)
            {
                return false;
            }
            return (attr & FILE_ATTRIBUTE_DIRECTORY) == 0;
#endif
        }

        bool IsDirectory() const
        {
#ifdef LLUF_CONFIG_READER_USE_LINUX
            struct stat pathstat;
            const int res = ::lstat(m_path.c_str(), &pathstat);
            if (res != 0)
            {
                return false;
            }
            return S_ISDIR(pathstat.st_mode);
#else
            DWORD attr(::GetFileAttributesA(m_path.c_str()));
            if (attr == INVALID_FILE_ATTRIBUTES)
            {
                return false;
            }
            return (attr & FILE_ATTRIBUTE_DIRECTORY) != 0;
#endif
        }
        
        void operator/=(const std::string& p)
        {
            if (!IsSeparator(m_path[m_path.size() - 1]))
            {
                m_path += PATH_SEPARATOR;
            }
            m_path += p;
        }
        Path operator/(const std::string& p) const {Path tmp = *this; tmp /= p; return tmp;}

        const std::string& str() const {return m_path;}
    private:
        static bool IsSeparator(const char c)
        {
            return c == PATH_SEPARATOR
#ifdef LLUF_CONFIG_READER_USE_WINDOWS
                || c == ALTERNATE_PATH_SEPARATOR
#endif
                ;
        }
        

        std::string m_path;
    };

    std::string GetEnv(const std::string& name)
    {
        char* env = getenv(name.c_str());
        if (env == NULL)
        {
            throw std::logic_error("Environment variable " + name + " does not appear to be set");
        }
        return std::string(env);
    }
    
    /** Throws logic_error if environment variable is not set */
    Path SafirRuntimeConfigDirectory()
    {
        const Path path(GetEnv("SAFIR_RUNTIME"));
        return path / "data" / "core_config";
    }
    
#ifdef LLUF_CONFIG_READER_USE_WINDOWS
    Path GetFolderPathFromCSIDL(const int csidl)
    {
        char path[MAX_PATH];

        if(SUCCEEDED(SHGetFolderPathA(NULL, 
                                     csidl|CSIDL_FLAG_CREATE, 
                                     NULL, 
                                     0, 
                                     path))) 
        {
            return Path(path);
        }
        else
        {
            throw std::logic_error("Call to SHGetFolderPath failed!");
        }
    }
#endif

    Path SystemConfigDirectory()
    {
#ifdef LLUF_CONFIG_READER_USE_LINUX
        return Path("/etc/safir_sdk_core/");
#else
        //this should be c:/ProgramData most of the time
        return GetFolderPathFromCSIDL(CSIDL_COMMON_APPDATA) / "safir_sdk_core" / "config";
#endif
    }

    Path UserConfigDirectory()
    {
#ifdef LLUF_CONFIG_READER_USE_LINUX
        try
        {
            return Path(GetEnv("XDG_CONFIG_HOME")) / "safir_sdk_core";
        }
        catch (const std::logic_error&)
        {
        }

        try
        {
            return Path(GetEnv("HOME")) / ".config" / "safir_sdk_core";
        }
        catch (const std::logic_error&)
        {
        }

        throw std::logic_error("Could not find either HOME or XDG_CONFIG_HOME environment variables.");

#else
        //try %USERPROFILE%/AppData/Local/
        return GetFolderPathFromCSIDL(CSIDL_LOCAL_APPDATA) / "safir_sdk_core" / "config";
#endif

    }

    //expandera @@CSIDL_LOCAL_APPDATA@@
    //gör i dots_kernel också...
    //dokumentera alltihopa wp och sug




    std::string ExpandSpecial(const std::string& str)
    {
#ifdef LLUF_CONFIG_READER_USE_WINDOWS
        const size_t start=str.rfind("@{");
        const size_t stop=str.find('}', start);

        if (start==std::string::npos || stop==std::string::npos)
            return str;

        const std::string var=str.substr(start+2, stop-start-2);
        std::string value;
        if (var == "CSIDL_APPDATA" || var == "FOLDERID_RoamingAppData")
        {
            value = GetFolderPathFromCSIDL(CSIDL_APPDATA);
        }
        else if (var == "CSIDL_LOCAL_APPDATA" || var == "FOLDERID_LocalAppData")
        {
            value = GetFolderPathFromCSIDL(CSIDL_LOCAL_APPDATA);
        }
        else if (var == "CSIDL_COMMON_APPDATA" || var == "FOLDERID_ProgramData")
        {
            value = GetFolderPathFromCSIDL(CSIDL_COMMON_APPDATA);
        }
        else if (var == "CSIDL_MYDOCUMENTS" || var == "FOLDERID_Documents")
        {
            value = GetFolderPathFromCSIDL(CSIDL_MYDOCUMENTS);
        }
        else if (var == "CSIDL_COMMON_DOCUMENTS" || var == "FOLDERID_PublicDocuments")
        {
            value = GetFolderPathFromCSIDL(CSIDL_COMMON_DOCUMENTS);
        }

        const std::string res=str.substr(0, start) + value + str.substr(stop+1, str.size()-stop-1);
        //search for next special variable 
        return ExpandSpecial(res); 
#else
        return str;
#endif

    }



    std::string ExpandEnvironment(const std::string& str)
    {
        const size_t start=str.rfind("$(");
        const size_t stop=str.find(')', start);

        if (start==std::string::npos || stop==std::string::npos)
            return str;

        const std::string var=str.substr(start+2, stop-start-2);
        const std::string env = GetEnv(var);

        const std::string res=str.substr(0, start) + env + str.substr(stop+1, str.size()-stop-1);
        //search for next environment variable or
        //recursively expand nested variable, e.g. $(NAME_$(NUMBER))
        return ExpandEnvironment(res); 
    }


}

namespace Safir
{
namespace Utilities
{
namespace Internal
{
    class ConfigReader::Impl
    {
    public:
        Impl()
        {

        }

        void ExpandEnvironmentVariables()
        {
            ExpandEnvironmentVariables(m_locations);
            ExpandEnvironmentVariables(m_logging);
            ExpandEnvironmentVariables(m_typesystem);
        }

        void Read()
        {
            if (TryLoad(SystemConfigDirectory()))
            {
                return;
            }
            
            if (TryLoad(UserConfigDirectory()))
            {
                return;
            }

            if (TryLoad(SafirRuntimeConfigDirectory()))
            {
                return;
            }
            
            throw std::logic_error("Failed to load configuration");
        }

        boost::property_tree::ptree m_locations;
        boost::property_tree::ptree m_logging;
        boost::property_tree::ptree m_typesystem;

    private:
        /**
         * Returns false if it fails to find a configuration in the directory.
         * throws logic_error if the configuration cannot be loaded.
         */
        bool TryLoad(const Path& directory)
        {
            if (!directory.IsDirectory())
            {
                return false;
            }

            const Path locations = directory / "locations.ini";
            const Path logging = directory / "logging.ini";
            const Path typesystem = directory / "typesystem.ini";

            if (!locations.Exists() && !logging.Exists() && !typesystem.Exists())
            {
                return false;
            }
            else if (locations.IsFile() && logging.IsFile() && typesystem.IsFile())
            {
                boost::property_tree::read_ini(locations.str(), m_locations);
                boost::property_tree::read_ini(logging.str(), m_logging);
                boost::property_tree::read_ini(typesystem.str(), m_typesystem);
                return true;
            }
            else
            {
                throw std::logic_error("Failed to find all three ini files");
            }
        }

        static void ExpandEnvironmentVariables(boost::property_tree::ptree& ptree)
        {
            for (boost::property_tree::ptree::iterator it = ptree.begin();
                 it != ptree.end(); ++it)
            {
                const bool isSection = !it->second.empty();

                if (isSection)
                {
                    ExpandEnvironmentVariables(it->second);
                }
                else
                {
                    const std::string value = it->second.get_value<std::string>();
                    it->second.put_value(ExpandSpecial(value));
                    it->second.put_value(ExpandEnvironment(value));
                }
                

            }
        }


    };

    ConfigReader::ConfigReader()
        : m_impl(new Impl())
    {
        m_impl->Read();
        m_impl->ExpandEnvironmentVariables();
    }

    const boost::property_tree::ptree& ConfigReader::Locations() const
    {
        return m_impl->m_locations;
    }

    const boost::property_tree::ptree& ConfigReader::Logging() const
    {
        return m_impl->m_logging;
    }

    const boost::property_tree::ptree& ConfigReader::Typesystem() const
    {
        return m_impl->m_typesystem;
    }

    std::string ConfigReader::ExpandEnvironment(const std::string& str)
    {
        return ::ExpandEnvironment(str);
    }
   
    std::string ConfigReader::ExpandSpecial(const std::string& str)
    {
        return ::ExpandSpecial(str);
    }

}
}
}
