/******************************************************************************
*
* Copyright Saab AB, 2013 (http://www.safirsdk.com)
*
* Created by: Anders Widén
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
#ifdef _MSC_VER

#include "LogWin32.h"

#include <windows.h>
#include <psapi.h>
#include <vector>
#include <string>

#include <boost/shared_ptr.hpp>

namespace Safir
{
namespace Utilities
{
namespace Internal
{

namespace
{

// Some constants
const DWORD maxStringSize = 64u * 1024u;
const std::string regKey =
        "SYSTEM\\CurrentControlSet\\Services\\EventLog\\Application\\Safir";
const std::string eventMessageFile = "C:\\Windows\\Microsoft.NET\\Framework64\\v2.0.50727\\EventLogMessages.dll";
const char* eventMessageFileParamName = "EventMessageFile";
const char* typesSupportedParamName = "TypesSupported";
const DWORD typesSupported = (EVENTLOG_SUCCESS |
                              EVENTLOG_INFORMATION_TYPE |
                              EVENTLOG_WARNING_TYPE |
                              EVENTLOG_ERROR_TYPE);
const char* logSourceName ="Safir";

//------------------------------------------
void GetOwnModuleHandle(HMODULE& handle)
{
    // Acquire all modules of the current process
    HANDLE hProcess = GetCurrentProcess();
    std::vector<HMODULE> modules;
    DWORD moduleCount = 1024;
    do
    {
        modules.resize(moduleCount, HMODULE(0));
        BOOL res = EnumProcessModules(
            hProcess,
            &modules[0],
            static_cast<DWORD>(modules.size() * sizeof(HMODULE)),
            &moduleCount);
        moduleCount /= sizeof(HMODULE);

        if (!res)
        {
            throw std::logic_error("Could not enumerate process modules");
        }
    }
    while (moduleCount > modules.size());
    modules.resize(moduleCount, HMODULE(0));

    // Now find the current module among them
    void* p = (void*)&GetOwnModuleHandle;
    for (std::size_t i = 0, n = modules.size(); i < n; ++i)
    {
        MODULEINFO info;
        if (!GetModuleInformation(hProcess, modules[i], &info, sizeof(info)))
        {
            throw std::logic_error("Could not acquire module information");
        }

        if (info.lpBaseOfDll <= p && (static_cast<unsigned char*>(info.lpBaseOfDll) + info.SizeOfImage) > p)
        {
            // Found it
            handle = modules[i];
            break;
        }
    }

    if (!handle)
    {
        throw std::logic_error("Could not find own module");
    }
}

//------------------------------------------
// Retrieves the full name of the current module (be that dll or exe)
//
std::string GetOwnModuleName()
{
    HMODULE hSelfModule = 0;

    GetOwnModuleHandle(hSelfModule);

    // Get the module file name
    char buf[MAX_PATH];
    DWORD size = GetModuleFileNameA(hSelfModule, buf, sizeof(buf) / sizeof(*buf));
    if (size == 0)
    {
        throw std::logic_error("Could not get module file name");
    }

    return std::string(buf, buf + size);
}

//------------------------------------------
LSTATUS GetRegistryValue(HKEY hKey, const char* lpValueName, std::string& value)
{
    DWORD type = REG_NONE;
    DWORD size = 0;
    LSTATUS res = RegQueryValueExA(hKey, lpValueName, NULL, &type, NULL, &size);
    if (res == ERROR_SUCCESS && ((type != REG_EXPAND_SZ && type != REG_SZ) || size > maxStringSize))
    {
        return ERROR_INVALID_DATA;
    }
    if (size == 0)
    {
        return res;
    }

    value.resize(size);
    res = RegQueryValueExA(hKey, lpValueName, NULL, &type, reinterpret_cast<LPBYTE>(&value[0]), &size);
    value.resize(std::strlen(value.c_str())); // remove extra terminating zero

    return res;
}

//------------------------------------------
LSTATUS GetRegistryValue(HKEY hKey, const char* lpValueName, DWORD& value)
{
    DWORD type = REG_NONE;
    DWORD size = sizeof(value);
    LSTATUS res = RegQueryValueExA(hKey, lpValueName, NULL, &type, reinterpret_cast<LPBYTE>(&value), &size);
    if (res == ERROR_SUCCESS && type != REG_DWORD && type != REG_BINARY)
    {
        res = ERROR_INVALID_DATA;
    }
    return res;
}

//------------------------------------------
bool RegistryIsInitialized(const std::string& regKey,
                           const std::string& eventMessageFile,
                           DWORD              typesSupported)
{
    // Open the key
    HKEY hKey = 0;
    LSTATUS res = RegOpenKeyExA(HKEY_LOCAL_MACHINE,
                                regKey.c_str(),
                                REG_OPTION_NON_VOLATILE,
                                KEY_READ,
                                &hKey);
    if (res != ERROR_SUCCESS)
    {
        return false;
    }

    // Shared pointer used as guard to close registry key when we leave scope
    boost::shared_ptr<void> hKeyGuard(hKey, RegCloseKey);

    std::string msgFile;
    res = GetRegistryValue(hKey, eventMessageFileParamName, msgFile);
    if (res != ERROR_SUCCESS || msgFile != eventMessageFile)
    {
        return false;
    }

    DWORD eventTypes = 0;
    res = GetRegistryValue(hKey, typesSupportedParamName, eventTypes);
    if (res != ERROR_SUCCESS || eventTypes != typesSupported)
    {
        return false;
    }

    return true;
}

//------------------------------------------
void AddRegistryEntries()
{
    std::string eventMessageFile;
    if (sizeof(void*) > 4)
    {
        eventMessageFile = eventMessageFile64;
    }
    else
    {
        eventMessageFile = eventMessageFile64;
    }
    //TODO ta bort
    //const std::string eventMessageFile = "C:\\Windows\\Microsoft.NET\\Framework64\\v4.0.30319\\EventLogMessages.dll";
    //const std::string eventMessageFile = GetOwnModuleName();

    // First check the registry keys and values in read-only mode.
    // This allow us to avoid to elevate permissions to modify the registry
    // when we dont need to.
    if (RegistryIsInitialized(regKey, eventMessageFile, typesSupported))
    {
        return;
    }

    // Create or open the key
    HKEY hKey = 0;
    DWORD disposition = 0;
    LSTATUS res = RegCreateKeyExA(HKEY_LOCAL_MACHINE,
                                  regKey.c_str(),
                                  0,
                                  NULL,
                                  REG_OPTION_NON_VOLATILE,
                                  KEY_WRITE,
                                  NULL,
                                  &hKey,
                                  &disposition);
    if (res != ERROR_SUCCESS)
    {
        throw std::logic_error("Could not create registry key for the event log");
    }

    // Shared pointer used as guard to close registry key when we leave scope
    boost::shared_ptr<void> hKeyGuard(hKey, RegCloseKey);

    // Set the module file name that contains event resources
    res = RegSetValueExA(hKey,
                         eventMessageFileParamName,
                         0,
                         REG_EXPAND_SZ,
                         reinterpret_cast<LPBYTE>(const_cast<char*>(eventMessageFile.c_str())),
                         static_cast<DWORD>(eventMessageFile.size() + 1));
    if (res != ERROR_SUCCESS)
    {
        throw std::logic_error(std::string("Could not create registry value ") + eventMessageFileParamName);
    }

    // Set the supported event types
    DWORD eventTypes = typesSupported;
    res = RegSetValueExA(hKey,
                         typesSupportedParamName,
                         0,
                         REG_DWORD,
                         reinterpret_cast<LPBYTE>(&eventTypes),
                         static_cast<DWORD>(sizeof(eventTypes)));
    if (res != ERROR_SUCCESS)
    {
        throw std::logic_error(std::string("Could not create registry value ") + typesSupportedParamName);
    }
}

} // anonymous namespace

void InitWindowsLogging()
{
    AddRegistryEntries();
}

WindowsLogger::WindowsLogger(const std::string& processName)
    : m_sourceHandle(0)
{
    //TODO
    m_sourceHandle = RegisterEventSourceA(NULL, "Safir");
    //m_sourceHandle = RegisterEventSourceA(NULL, processName.c_str());
    if (!m_sourceHandle)
    {
        throw std::logic_error("Could not register event source");
    }
}

WindowsLogger::~WindowsLogger()
{
    DeregisterEventSource(m_sourceHandle);
}

void WindowsLogger::Send(const std::string& log)
{
    const char* message = log.c_str();

    // TODO Bara vanlig enkel log?
    ReportEventA(m_sourceHandle,
                 EVENTLOG_INFORMATION_TYPE,
                 0,
                 0,
                 NULL,
                 1,
                 0,
                 &message,
                 NULL);
}

}
}
}

#endif
