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
#pragma once

#include <Safir/Utilities/ProcessMonitor.h>
#include <boost/bind.hpp>

#ifdef _MSC_VER
#  pragma warning (push)
#  pragma warning (disable: 4267)
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
        explicit ProcessMonitorImpl(boost::asio::io_service& ioService,
                                    const boost::function<void(const pid_t pid)>& callback,
                                    const boost::chrono::steady_clock::duration& pollPeriod);

        void Stop();

        void StartMonitorPid(const pid_t pid)
        {
            m_strand.dispatch(boost::bind(&ProcessMonitorImpl::StartMonitorPidInternal,this,pid));
        }

        void StopMonitorPid(const pid_t pid)
        {
            m_strand.dispatch(boost::bind(&ProcessMonitorImpl::StopMonitorPidInternal,this,pid));
        }
    private:
        void StartMonitorPidInternal(const pid_t pid);
        void StopMonitorPidInternal(const pid_t pid);

        struct Process; //forward decl

        void HandleEvent(const boost::shared_ptr<Process>& process, const boost::system::error_code& error);

        // Client callback
        boost::function<void(const pid_t pid)> m_callback;

        boost::asio::io_service& m_ioService;

        boost::asio::io_service::strand m_strand;

        struct Process
        {
            Process(boost::asio::io_service& ioService, HANDLE process, const pid_t pid_)
                : handle(ioService, process)
                , pid(pid_)
            {}
            boost::asio::windows::object_handle handle;
            const pid_t pid;
        };
        typedef std::map<pid_t, boost::shared_ptr<Process> > ProcessTable;

        ProcessTable m_processes;
    };
}
}
