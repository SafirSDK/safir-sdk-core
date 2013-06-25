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
#ifndef __PATH_H__
#define __PATH_H__

#include <string>

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



}

namespace Safir
{
namespace Utilities
{
namespace Internal
{
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

}
}
}

#endif

