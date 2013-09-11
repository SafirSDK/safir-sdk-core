/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
*
* Created by: Lars Hagström / stlrha
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
#include <Safir/Utilities/ProcessInfo.h>
#include <vector>

#include <boost/lexical_cast.hpp>
#include <boost/filesystem/path.hpp>

#if defined(linux) || defined(__linux) || defined(__linux__)

#include <boost/filesystem/fstream.hpp>
#include <boost/algorithm/string/join.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include <iostream>
#include <iterator>

#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
#ifndef _WIN32_WINNT
  #define _WIN32_WINNT 0x0501
#endif
#include <windows.h>
#include <psapi.h>
#include <process.h>
#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/split.hpp>
#endif

namespace
{
#if defined(linux) || defined(__linux) || defined(__linux__)
    const std::vector<std::string> GetCommandLine(const pid_t pid)
    {
        const std::string pidString = boost::lexical_cast<std::string>(pid);
        const boost::filesystem::path filename = boost::filesystem::path("/proc")
            / pidString / "cmdline";

        boost::filesystem::ifstream cmdline(filename);
        if (!cmdline.good())
        {
            return std::vector<std::string>(1,pidString);
        }

        //Note: cmdline is null character separated. See man proc(5).
        
        std::vector<std::string> result(1);
        for (std::istreambuf_iterator<char> it = std::istreambuf_iterator<char>(cmdline);
             it != std::istreambuf_iterator<char>(); ++it)
        {
            if (*it == '\0')
            {
                result.push_back(std::string());
            }
            else
            {
                result.back().push_back(*it);
            }
        }
        return result;
    }
#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
    const std::vector<std::string> GetCmdLine()
    {
        std::vector<std::string> res;
        std::string cmd = GetCommandLineA();


        boost::split(res, cmd, boost::is_any_of(" \r\n\t"), boost::token_compress_on);

        return res;
    }
#else
#  error You need to implement GetCmdLine for this platform!
#endif
}


namespace Safir
{
namespace Utilities
{
    ProcessInfo::ProcessInfo(const pid_t pid):
        m_pid(pid)
    {

    }

    ProcessInfo::~ProcessInfo()
    {

    }

    const std::string ProcessInfo::GetProcessDescription()
    {
#if defined(linux) || defined(__linux) || defined(__linux__)
        return boost::algorithm::join(GetCommandLine(GetPid())," ");
#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
        return GetCommandLineA();
#else
#  error You need to implement GetProcessDescription for this platform!
#endif
    }

    const std::string ProcessInfo::GetProcessName() const
    {
#if defined(linux) || defined(__linux) || defined(__linux__)
        const std::vector<std::string> cmdline = GetCommandLine(m_pid);
#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
        const std::vector<std::string> cmdline = GetCmdLine();
#else
#  error You need to implement GetCommandLine for this platform!
#endif

        if (cmdline.size() == 0)
        {
            return boost::lexical_cast<std::string>(m_pid);
        }

        const boost::filesystem::path filename = boost::filesystem::path(cmdline[0]).filename();
        
        //try to find the name of the jar file
        if (filename == "java" || filename == "java.exe")
        {
            for (std::vector<std::string>::const_iterator it = cmdline.begin();
                 it != cmdline.end(); ++it)
            {
                if (*it == "-jar" && it+1 != cmdline.end())
                {
                    return boost::filesystem::path(*(it+1)).filename().string();
                }
            }
        }

        //try to find the assembly name when running under mono
        if (filename == "mono" || filename == "mono.exe" || filename == "cli" || filename == "cli.exe")
        {
            for (std::vector<std::string>::const_iterator it = cmdline.begin();
                 it != cmdline.end(); ++it)
            {
                if (boost::algorithm::ends_with(*it,".exe") || boost::algorithm::ends_with(*it,".csexe"))
                {
                    return boost::filesystem::path(*it).filename().string();
                }
            }
        }

        return boost::filesystem::path(*cmdline.begin()).filename().string();
    }

    pid_t ProcessInfo::GetPid()
    {
#if defined(linux) || defined(__linux) || defined(__linux__)
        return getpid();
#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
        return _getpid();
#else
#  error You need to implement GetPid for this platform!
#endif
    }
}
}
