/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
*
* Created by: Jonas Thor / stjth
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
#include "ProcessMonitorImpl.h"
#include <Safir/Utilities/ProcessMonitor.h>
#if defined _MSC_VER
#include "ProcessMonitorWin32.h"
#elif defined __GNUC__
#include "ProcessMonitorLinux.h"
#endif

namespace Safir
{
namespace Utilities
{
    
    ProcessMonitor::ProcessMonitor()
        : m_impl(NULL)
    {
    }
    
    
    ProcessMonitor::~ProcessMonitor()
    {
        if (m_impl)
        {
            m_impl->StopThread();
            delete m_impl;
        }
    }
    
    void 
    ProcessMonitor::Init(const OnTerminateCb& callback)
    {
#if defined _MSC_VER
        m_impl = new ProcessMonitorWin32(callback);
#elif defined __GNUC__
        m_impl = new ProcessMonitorLinux(callback);
#endif

        if (m_impl)
            m_impl->StartThread();
    }
    

    void
    ProcessMonitor::StartMonitorPid(const pid_t pid)
    {
        if (m_impl)
            m_impl->StartMonitorPid(pid);
    }
    
    
    void 
    ProcessMonitor::StopMonitorPid(const pid_t pid)
    {
        if (m_impl)
            m_impl->StopMonitorPid(pid);
    }
}
}


