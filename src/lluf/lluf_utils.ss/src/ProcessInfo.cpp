/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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
#include <iostream>
#include <boost/lexical_cast.hpp>
#include <boost/filesystem/path.hpp>

#if defined(linux) || defined(__linux) || defined(__linux__)

#include <boost/filesystem/fstream.hpp>
#include <boost/algorithm/string/join.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include <iterator>

#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
#ifndef _WIN32_WINNT
  #define _WIN32_WINNT 0x0501
#endif
#include <windows.h>
#include <shellapi.h>
#include <psapi.h>
#include <process.h>
#include <boost/algorithm/string.hpp>
#include <boost/thread.hpp>
#include <boost/algorithm/string/split.hpp>
#include <Safir/Utilities/Internal/StringEncoding.h>
#endif

namespace bfs = boost::filesystem;

#if defined(linux) || defined(__linux) || defined(__linux__)
namespace
{

    const std::vector<std::string> GetCommandLine(const pid_t pid)
    {
        const std::string pidString = boost::lexical_cast<std::string>(pid);
        const bfs::path filename = bfs::path("/proc")
            / pidString / "cmdline";

        bfs::ifstream cmdline(filename);
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
}
#endif

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

        if (cmdline.size() == 0)
        {
            return boost::lexical_cast<std::string>(m_pid);
        }

        const bfs::path filename = bfs::path(cmdline[0]).filename();

        //try to find the name of the jar file
        if (filename == "java" || filename == "java.exe")
        {
            for (std::vector<std::string>::const_iterator it = cmdline.begin();
                 it != cmdline.end(); ++it)
            {
                if (*it == "-jar" && it+1 != cmdline.end())
                {
                    return bfs::path(bfs::path(*(it+1)).filename()).string();
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
                    return bfs::path(bfs::path(*it).filename()).string();
                }
            }
        }

        return bfs::path(bfs::path(*cmdline.begin()).filename()).string();

#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
        HANDLE hProcess = ::OpenProcess(PROCESS_QUERY_INFORMATION | PROCESS_VM_READ,
                                        FALSE,
                                        m_pid);
        if (hProcess == NULL)
        {
            return boost::lexical_cast<std::string>(m_pid);
        }

        // Get the process name.
        char szProcessName[MAX_PATH];
        strncpy(szProcessName,boost::lexical_cast<std::string>(m_pid).c_str(), MAX_PATH);

        std::unique_ptr<HMODULE[]> hMod;
        DWORD cbNeededMBN = sizeof(HMODULE);

        for (int retry = 0; retry < 20; ++retry)
        {
            hMod.reset(new HMODULE[cbNeededMBN/sizeof(HMODULE)]);
            const BOOL result = EnumProcessModules(hProcess,
                                                   hMod.get(),
                                                   cbNeededMBN,
                                                   &cbNeededMBN);
            if (!result)
            {
                std::wcerr << "EnumProcessModules attempt " << retry << " failed: "
                           << ::GetLastError() << " cbNeededMBN = " << cbNeededMBN
                           << std::endl;
                boost::this_thread::yield();
            }
            else
            {
                if (retry > 0) //TODO clean up all this debug output!
                {
                    std::wcerr << "EnumProcessModules attempt " << retry << " succeeded!" << std::endl;

                    if (cbNeededMBN/sizeof(HMODULE) > 1)
                    {
                        for (unsigned int i = 0; i < cbNeededMBN/sizeof(HMODULE); ++i)
                        {
                            char pn[MAX_PATH];
                            BOOL ret3 = GetModuleBaseNameA( hProcess, hMod[i], pn, MAX_PATH );
                            if (!ret3)
                            {
                                std::wcout << "GetModuleBaseNameA failed " << i << " " << GetLastError() << std::endl;
                            }
                            else
                            {
                                std::wcout << "GetModuleBaseNameA " << i << ": " << pn << std::endl;
                            }
                        }
                    }
                    break; //we break out here so that we return an incorrect process name to make the test case fail.
                }

                GetModuleBaseNameA( hProcess, hMod[0], szProcessName, MAX_PATH );
                break;
            }
        }

        CloseHandle(hProcess);

        return szProcessName;

#else
#  error You need to implement GetCommandLine for this platform!
#endif
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
