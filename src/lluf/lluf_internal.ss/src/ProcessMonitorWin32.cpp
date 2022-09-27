/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safirsdkcore.com)
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
#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)

#include "ProcessMonitorWin32.h"
#include <Safir/Utilities/Internal/AsioStrandWrap.h>

namespace Safir
{
namespace Utilities
{
    ProcessMonitorImpl::ProcessMonitorImpl(boost::asio::io_context& io,
                                           const std::function<void(const pid_t pid)>& callback,
                                           const boost::chrono::steady_clock::duration& /*pollPeriod*/)
        : m_callback(callback)
        , m_io(io)
        , m_stopped(false)
        , m_strand(io)
    {

    }

    void ProcessMonitorImpl::Stop()
    {
        const bool was_stopped = m_stopped.exchange(true);
        if (!was_stopped)
        {
            boost::asio::dispatch(m_strand, [this]{StopInternal();});
        }
    }

    void ProcessMonitorImpl::StopInternal()
    {
        for(ProcessTable::iterator it = m_processes.begin();
            it != m_processes.end(); ++it)
        {
            it->second->handle.cancel();
        }
    }


    void
    ProcessMonitorImpl::StartMonitorPidInternal(const pid_t pid)
    {
        if (m_processes.find(pid) != m_processes.end())
        {
            //we're already watching this pid.
            return;
        }

        HANDLE process = OpenProcess(PROCESS_ALL_ACCESS,//PROCESS_QUERY_INFORMATION | PROCESS_VM_READ | SYNCHRONIZE,
                                     FALSE,
                                     (DWORD)pid);

        //if we can't get a handle to the process we assume that it is dead.
        if (process == NULL)
        {
            boost::asio::post(m_io, [this, pid]{m_callback(pid);});
            return;
        }

        std::shared_ptr<Process> proc(new Process(m_io, process, pid));
        m_processes.insert(std::make_pair(pid, proc));
        proc->handle.async_wait(Safir::Utilities::Internal::WrapInStrand(m_strand, [this, proc](const boost::system::error_code& error){HandleEvent(proc, error);}));

    }


    void
    ProcessMonitorImpl::StopMonitorPidInternal(const pid_t pid)
    {
        ProcessTable::iterator findIt = m_processes.find(pid);
        if (findIt == m_processes.end())
        {
            //not monitoring this pid!
            return;
        }

        findIt->second->handle.cancel();
        m_processes.erase(findIt);
    }


    void ProcessMonitorImpl::HandleEvent(const std::shared_ptr<Process>& process,
                                         const boost::system::error_code& error)
    {
        if (error)
        {
            return;
        }

        m_processes.erase(process->pid);
        boost::asio::post(m_io, [this, pid = process->pid]{m_callback(pid);});
    }
}
}

#endif
