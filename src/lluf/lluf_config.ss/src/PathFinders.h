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
#ifndef __LLUF_CONFIG_PATH_FINDERS_H__
#define __LLUF_CONFIG_PATH_FINDERS_H__

#include "Path.h"
#include "Util.h"

namespace Safir
{
namespace Utilities
{
namespace Internal
{
    struct CommonPathFinder
    {
        /** Returns empty path if environment variable is not set*/
        static Path SafirTestConfigOverrideDirectory()
        {
            return Path(GetEnv("SAFIR_TEST_CONFIG_OVERRIDE", std::nothrow));
        }

    };


#ifdef LLUF_CONFIG_READER_USE_WINDOWS
    struct WindowsPathFinder
        : public CommonPathFinder
    {
        static Path SystemConfigDirectory()
        {
            //this should be c:/ProgramData most of the time
            return GetFolderPathFromCSIDL(CSIDL_COMMON_APPDATA) / "safir-sdk-core" / "config";
        }

        static Path UserConfigDirectory()
        {
            //try %USERPROFILE%/AppData/Local/
            return GetFolderPathFromCSIDL(CSIDL_LOCAL_APPDATA) / "safir-sdk-core" / "config";
        }
    };
#endif

#ifdef LLUF_CONFIG_READER_USE_LINUX
    struct LinuxPathFinder
        : public CommonPathFinder
    {
        static Path SystemConfigDirectory()
        {
            return Path("/etc/safir-sdk-core");
        }

        static Path UserConfigDirectory()
        {
            const auto xdgPath = GetEnv("XDG_CONFIG_HOME", std::nothrow);
            if (!xdgPath.empty())
            {
                return Path(xdgPath) / "safir-sdk-core";
            }

            const auto homePath = GetEnv("HOME", std::nothrow);
            if (!homePath.empty())
            {
                return Path(homePath) / ".config" / "safir-sdk-core";
            }

            throw std::logic_error("Could not find either HOME or XDG_CONFIG_HOME environment variables.");
        }
    };
#endif
}
}
}

#endif
