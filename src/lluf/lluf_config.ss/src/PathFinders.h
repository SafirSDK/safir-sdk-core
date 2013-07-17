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
        /** Throws logic_error if environment variable is not set */
        static Path SafirRuntimeConfigDirectory()
        {
            const Path path(GetEnv("SAFIR_RUNTIME"));
            return path / "data" / "core_config";
        }

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
            return GetFolderPathFromCSIDL(CSIDL_COMMON_APPDATA) / "safir_sdk_core" / "config";
        }
        
        static Path UserConfigDirectory()
        {
            //try %USERPROFILE%/AppData/Local/
            return GetFolderPathFromCSIDL(CSIDL_LOCAL_APPDATA) / "safir_sdk_core" / "config";
        }
    };
#endif

#ifdef LLUF_CONFIG_READER_USE_LINUX
    struct LinuxPathFinder
        : public CommonPathFinder
    {
        static Path SystemConfigDirectory()
        {
            return Path("/etc/safir_sdk_core");
        }

        static Path UserConfigDirectory()
        {
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
        }
    };
#endif
}
}
}

#endif

