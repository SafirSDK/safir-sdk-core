/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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
#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
#  include "ProcessMonitorWin32.h"
#elif defined(linux) || defined(__linux) || defined(__linux__)
#  include "ProcessMonitorLinux.h"
#endif

namespace Safir
{
namespace Utilities
{

    ProcessMonitor::ProcessMonitor(boost::asio::io_service& ioService,
                                   const boost::function<void(const pid_t pid)>& callback,
                                   const boost::chrono::steady_clock::duration& pollPeriod)
        : m_impl(new ProcessMonitorImpl(ioService,callback, pollPeriod))
    {

    }

    void ProcessMonitor::Stop()
    {
        m_impl->Stop();
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
