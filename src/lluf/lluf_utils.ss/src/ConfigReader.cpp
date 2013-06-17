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
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/ini_parser.hpp>

#if defined(linux) || defined(__linux) || defined(__linux__)
#  include <sys/types.h>
#  include <sys/stat.h>
#  include <unistd.h>
#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
//#  include <windows.h>
#else
#  error You need to implement ConfigReader for this platform!
#endif


namespace
{
#if defined(linux) || defined(__linux) || defined(__linux__)
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
         * True if IsFile and IsDirectory returns true.
         * Note that a symlink or block device will return false!
         */
        bool Exists() const
        {
            struct stat pathstat;
            const int res = ::lstat(m_path.c_str(), &pathstat);
            if (res == 0)
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
        }
        
        bool IsFile() const
        {
            struct stat pathstat;
            const int res = ::lstat(m_path.c_str(), &pathstat);
            if (res == 0)
            {
                return false;
            }
            return S_ISREG(pathstat.st_mode);
        }

        bool IsDirectory() const
        {
            struct stat pathstat;
            const int res = ::lstat(m_path.c_str(), &pathstat);
            if (res == 0)
            {
                return false;
            }
            return S_ISDIR(pathstat.st_mode);
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
#     ifdef BOOST_WINDOWS_API
                || c == ALTERNATE_PATH_SEPARATOR
#     endif
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
        return path / "data" / "config";
    }
    
    Path SystemConfigDirectory()
    {
#if defined(linux) || defined(__linux) || defined(__linux__)
        return Path("/etc/safir/");
#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
//#  include <windows.h>
#endif

    }

    Path UserConfigDirectory()
    {
#if defined(linux) || defined(__linux) || defined(__linux__)
        //const std::string tail("/safir/");
        try
        {
            return Path(GetEnv("XDG_CONFIG_HOME")) / "safir";
        }
        catch (const std::logic_error&)
        {
        }

        try
        {
            return Path(GetEnv("HOME")) / ".config" / "safir";
        }
        catch (const std::logic_error&)
        {
        }

        throw std::logic_error("Could not find either HOME or XDG_CONFIG_HOME environment variables.");

#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
//#  include <windows.h>
#endif

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

        void Read()
        {
            try
            {
                const Path safirDir = SafirRuntimeConfigDirectory();
                if (TryLoad(safirDir))
                {
                    return;
                }

                const Path systemDir = SystemConfigDirectory();
                if (TryLoad(systemDir))
                {
                    return;
                }

                const Path userDir = UserConfigDirectory();
                if (TryLoad(safirDir))
                {
                    return;
                }

                throw std::logic_error("Failed to load configuration");
                // boost::property_tree::read_ini(JoinPath(systemDir,"locations.ini"), m_locations);
                //boost::property_tree::read_ini(JoinPath(systemDir,"locations.ini"), m_locations);

                /*nej, kolla om filerna finns och går att öppna först.
                    om inte katalogen finns --> försök med user
                    om ingen av filerna finns --> försök med user
                    om inte alla tre filerna finns --> exception
                    om inte alla tre filerna går att läsa korrekt --> exception

                    Vi vill nog ha "load_user_config" som en option i alla (eller några av) filerna.
                    Den gör så att vi går vidare och läser user config efter att vi läst system
                    vid något fel --> exception
                    
                    vi kanske vill använda SAFIR_RUNTIME i en övergångsperiod här?!*/
            }
            catch (const std::exception& e)
            {
                //TODO
            }
        }
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
            }
            else
            {
                throw std::logic_error("Failed to find all three ini files");
            }

            return false;
        }

        boost::property_tree::ptree m_locations;
        boost::property_tree::ptree m_logging;
        boost::property_tree::ptree m_typesystem;
    };

    ConfigReader::ConfigReader()
        : m_impl(new Impl())
    {
        m_impl->Read();
    }
}
}
}
