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
#include <boost/lexical_cast.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/filesystem/fstream.hpp>
#include <fstream>
#include <iostream>
#include <ace/config.h>

#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
#include <windows.h>
#include <psapi.h>
#endif

namespace Safir
{
namespace Utilities
{
    ProcessInfo::ProcessInfo(const int pid):
        m_pid(pid)
    {

    }

    ProcessInfo::~ProcessInfo()
    {

    }

    const std::string ProcessInfo::GetProcessDescription()
    {
#if defined(linux) || defined(__linux) || defined(__linux__)
        const boost::filesystem::path filename = std::string("/proc/")
            + boost::lexical_cast<std::string>(m_pid) + "/cmdline";

        boost::filesystem::ifstream cmdline(filename);
        if (!cmdline.good())
        {
            return boost::lexical_cast<std::string>(m_pid);
        }

        std::string argv;
        std::string line;
        while (std::getline(cmdline,line))
        {
            argv += line;
        }
        return argv;
#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
        return GetCommandLineA();
#else
#  error You need to implement GetProcessDescription for this platform!
#endif
    }

    const std::string ProcessInfo::GetProcessName()
    {
#if defined(linux) || defined(__linux) || defined(__linux__)
        const boost::filesystem::path filename = std::string("/proc/")
            + boost::lexical_cast<std::string>(m_pid) + "/cmdline";

        boost::filesystem::ifstream cmdline(filename);
        if (!cmdline.good())
        {
            return boost::lexical_cast<std::string>(m_pid);
        }

        std::string arg0;
        cmdline >> arg0;
        boost::filesystem::path arg0path(arg0);

        return arg0path.leaf();
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
        
        HMODULE hMod;
        DWORD cbNeededMBN;

        if ( EnumProcessModules( hProcess, &hMod, sizeof(hMod), 
            &cbNeededMBN) )
        {
            GetModuleBaseNameA( hProcess, hMod, szProcessName, MAX_PATH );
        }

        CloseHandle(hProcess);
        
        return szProcessName;
#else
#  error You need to implement GetProcessName for this platform!
#endif

    }
}
}
