/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safir.sourceforge.net)
*
* Created by: Anders Widén/ anders.widen@consoden.se
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
#ifndef __LLUF_IPC_NAME_H__
#define __LLUF_IPC_NAME_H__

#include <Safir/Utilities/Internal/ConfigReader.h>
#include <boost/filesystem.hpp>
#include <iostream>

namespace Safir
{
namespace Utilities
{
namespace Internal
{
    /**
     * Compose an id that is a valid identification for an Ipc endpoint.
     *
     */

#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
    std::string GetIpcStreamId(const std::string& name)
    {
        return std::string("\\\\.\\pipe\\") + name +
                Safir::Utilities::Internal::Expansion::GetSafirInstanceSuffix();
    }

#elif defined(linux) || defined(__linux) || defined(__linux__)

    const boost::filesystem::path GetIpcDirectory()
    {
        using namespace boost::filesystem;

        std::wcout << "GetIpcDirectory() called" << std::endl;

        Safir::Utilities::Internal::ConfigReader config;

        std::wcout << "config object is constructed!" << std::endl;

        const path dir(config.Locations().get<std::string>("ipc_endpoints_directory"));

        std::wcout << "dir object is constructed! string length is" << dir.string().length() << std::endl;

        if (!exists(dir))
        {
            try
            {
                create_directories(dir);
            }
            catch (...)
            {
                std::ostringstream ostr;
                ostr << "Failed to create directory '" << dir.string() << "'" << std::endl;
                throw std::logic_error(ostr.str());
            }
        }
        else if (!is_directory(dir))
        {
            std::ostringstream ostr;
            ostr << "The IPC endpoint directory does not appear to be a directory. dir = '" << dir.string() << "'" << std::endl;
            throw std::logic_error(ostr.str());
        }
        return dir;
    }

    const std::string GetIpcStreamId(const std::string& name)
    {
        return (GetIpcDirectory() / (name + Safir::Utilities::Internal::Expansion::GetSafirInstanceSuffix())).string();
    }

#endif

}
}
}

#endif

