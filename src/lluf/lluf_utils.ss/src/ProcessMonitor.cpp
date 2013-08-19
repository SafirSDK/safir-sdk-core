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
#include <Safir/Utilities/ProcessMonitor.h>
#include "ProcessMonitorImpl.h"
#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
#  include "ProcessMonitorWin32.h"
#elif defined(linux) || defined(__linux) || defined(__linux__)
#  include "ProcessMonitorLinux.h"
#endif

namespace Safir
{
namespace Utilities
{
    
    ProcessMonitor::ProcessMonitor()
    {
    }
    
    
    ProcessMonitor::~ProcessMonitor()
    {
        m_impl->StopThread();
        m_impl.reset();
    }
    
    void 
    ProcessMonitor::Init(const OnTerminateCb& callback)
    {
        if (m_impl != NULL)
        {
            throw std::logic_error("ProcessMonitor already initialized!");
        }
#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
        m_impl.reset(new ProcessMonitorWin32(callback));
#elif defined(linux) || defined(__linux) || defined(__linux__)
        m_impl.reset(new ProcessMonitorLinux(callback));
#endif

        m_impl->StartThread();
    }
    

    void
    ProcessMonitor::StartMonitorPid(const pid_t pid)
    {
        if (m_impl == NULL)
        {
            throw std::logic_error("ProcessMonitor not initialized!");
        }
        
        m_impl->StartMonitorPid(pid);
    }
    
    
    void 
    ProcessMonitor::StopMonitorPid(const pid_t pid)
    {
        if (m_impl == NULL)
        {
            throw std::logic_error("ProcessMonitor not initialized!");
        }

        m_impl->StopMonitorPid(pid);
    }
}
}


