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
#pragma once

#include <Safir/Utilities/ProcessMonitor.h>
#include <functional>
#include <map>
#include <atomic>
#include <utility> 

#ifdef _MSC_VER
#  pragma warning (push)
#  pragma warning (disable: 4267)
#  pragma warning (disable: 4100)
#endif

#include <boost/asio.hpp>

#ifdef _MSC_VER
#  pragma warning (pop)
#endif

namespace Safir
{
namespace Utilities
{
    class ProcessMonitorImpl
    {
    public:
        explicit ProcessMonitorImpl(boost::asio::io_context& io,
                                    const std::function<void(const pid_t pid)>& callback,
                                    const boost::chrono::steady_clock::duration& pollPeriod);

        void Stop();

        void StartMonitorPid(const pid_t pid)
        {
            boost::asio::dispatch(m_strand, [this,pid]{StartMonitorPidInternal(pid);});
        }

        void StopMonitorPid(const pid_t pid)
        {
            boost::asio::dispatch(m_strand, [this,pid]{StopMonitorPidInternal(pid);});
        }
    private:
        void StopInternal();

        void StartMonitorPidInternal(const pid_t pid);
        void StopMonitorPidInternal(const pid_t pid);

        struct Process; //forward decl

        void HandleEvent(const std::shared_ptr<Process>& process, const boost::system::error_code& error);        

        // Client callback
        std::function<void(const pid_t pid)> m_callback;

        boost::asio::io_context& m_io;
        std::atomic<bool> m_stopped;
        boost::asio::io_context::strand m_strand;

        // In some cases async supervision is not possible due to application access level. In that
        // case the fallback solution is polling.
        const boost::chrono::steady_clock::duration m_pollPeriod;
        void Poll(std::shared_ptr<Process> proc, const boost::system::error_code& error);

        HANDLE GetProcessHandle(const pid_t pid) const;
        bool ProcessExists(const pid_t pid) const;

        struct Process
        {
            Process(boost::asio::io_context& io, HANDLE process, const pid_t pid_)
                : handle(io, process)
                , pid(pid_)
            {}
            boost::asio::windows::object_handle handle;
            const pid_t pid;
            std::unique_ptr<boost::asio::steady_timer> pollTimer;

        };
        typedef std::map<pid_t, std::shared_ptr<Process> > ProcessTable;

        ProcessTable m_processes;
    };
}
}
