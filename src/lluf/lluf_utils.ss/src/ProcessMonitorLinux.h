/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
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

#include <boost/asio.hpp>
#include <boost/asio/steady_timer.hpp>
#include <boost/bind.hpp>
#include <boost/function.hpp>
#include <set>
#include <boost/atomic.hpp>

namespace Safir
{
namespace Utilities
{
    class ProcessMonitorImpl
    {
    public:
        ProcessMonitorImpl(boost::asio::io_service& ioService,
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

        void Poll(const boost::system::error_code& error);

        // Client callback
        boost::function<void(const pid_t pid)> m_callback;

        boost::asio::io_service& m_ioService;
        boost::asio::io_service::strand m_strand;
        boost::atomic<bool> m_stopped;

        const boost::chrono::steady_clock::duration m_pollPeriod;
        boost::asio::steady_timer m_pollTimer;

        std::set<pid_t> m_monitoredPids;
    };
}
}

