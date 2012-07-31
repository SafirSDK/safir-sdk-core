/******************************************************************************
*
* Copyright Saab AB, 2012 (http://www.safirsdk.com)
*
* Created by: Anders Widén / stawi
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
#ifndef __BOOST_FILESYSTEM_WRAPPER_H__
#define __BOOST_FILESYSTEM_WRAPPER_H__

#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/version.hpp>

// Header only wrapper that handles incompatible differences between boost filesystem version 2 and 3

#define BOOST_MAJOR_VERSION BOOST_VERSION / 100000
#define BOOST_MINOR_VERSION BOOST_VERSION / 100 % 1000

#if (BOOST_MAJOR_VERSION >= 1 && BOOST_MINOR_VERSION >= 48)   
    // Boost filesystem version 3 is the only version that is included
    #define USE_BOOST_FILESYSTEM_V3
#elif (BOOST_MAJOR_VERSION >= 1 && BOOST_MINOR_VERSION >= 46)
    // Version 3 is the default but the user can force the use of version 2
    #if (!defined(BOOST_FILESYSTEM_VERSION) || BOOST_FILESYSTEM_VERSION == 3)
        #define USE_BOOST_FILESYSTEM_V3
    #endif
#elif (BOOST_MAJOR_VERSION >= 1 && BOOST_MINOR_VERSION >= 44)
    // Version 2 is the default but the user can force the use of version 3
    #if (defined(BOOST_FILESYSTEM_VERSION) && BOOST_FILESYSTEM_VERSION == 3)
        #define USE_BOOST_FILESYSTEM_V3
    #endif
#endif
    
namespace Safir
{
namespace Utilities
{
namespace Internal
{

    inline std::string GetFilenameFromDirectoryIterator(const boost::filesystem::directory_iterator& iter)
    {
    #if defined(USE_BOOST_FILESYSTEM_V3)
        return iter->path().filename().string();
    #else
        return iter->path().filename();
    #endif
    }

    inline std::string GetFilenameFromPath(const boost::filesystem::path& path)
    {
    #if defined(USE_BOOST_FILESYSTEM_V3)
        return path.filename().string();
    #else
        return path.filename();
    #endif
    }
}
}
}

#endif
