/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safir.sourceforge.net)
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

WindowsLogger::WindowsLogger(const std::wstring& processName)
    : m_startupSynchronizer("LLUF_WINDOWS_LOGGING_INITIALIZATION"),
      m_sourceHandle(0),
      m_processName(processName)
{
    m_sourceHandle = RegisterEventSourceW(NULL, L"Safir");
    if (!m_sourceHandle)
    {
        throw std::logic_error("Could not register event source");
    }
}

WindowsLogger::~WindowsLogger()
{
    DeregisterEventSource(m_sourceHandle);
}

void WindowsLogger::Send(const WORD eventType, const std::wstring& log) const
{
    const wchar_t* logPtr = log.c_str();

    ReportEventW(m_sourceHandle,
                 eventType,
                 0,
                 0,
                 NULL,
                 1,
                 0,
                 &logPtr,
                 NULL);
}

}
}
}

#endif
