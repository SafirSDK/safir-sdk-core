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
#if defined(linux) || defined(__linux) || defined(__linux__)

#include "ProcessMonitorLinux.h"
#include <Safir/Utilities/Internal/SystemLog.h>
#include <boost/lexical_cast.hpp>
#include <boost/filesystem.hpp>


namespace Safir
{
namespace Utilities
{
    ProcessMonitorImpl::ProcessMonitorImpl(boost::asio::io_service& ioService,
                                           const boost::function<void(const pid_t pid)>& callback,
                                           const boost::chrono::steady_clock::duration& pollPeriod)
        : m_callback(callback)
        , m_ioService(ioService)
        , m_strand(ioService)
        , m_pollPeriod(pollPeriod)
        , m_pollTimer(ioService)
    {
        m_pollTimer.expires_from_now(m_pollPeriod);
        m_pollTimer.async_wait(m_strand.wrap(boost::bind(&ProcessMonitorImpl::Poll,this,_1)));
    }


    void ProcessMonitorImpl::Stop()
    {
        m_strand.dispatch(boost::bind(&boost::asio::steady_timer::cancel,&m_pollTimer));
    }

    void ProcessMonitorImpl::Poll(const boost::system::error_code& error)
    {
        if (error)
        {
            return;
        }

        m_pollTimer.expires_from_now(m_pollPeriod);
        m_pollTimer.async_wait(m_strand.wrap(boost::bind(&ProcessMonitorImpl::Poll,this,_1)));

        using namespace boost::filesystem;

        std::set<pid_t> missingPids = m_monitoredPids;

        for (directory_iterator dit = directory_iterator("/proc");
             dit != directory_iterator(); ++dit)
        {
            const std::string name = dit->path().filename().string();
            if (name.find_first_not_of("0123456789") == std::string::npos)
            {
                missingPids.erase(boost::lexical_cast<pid_t>(name));
            }
        }

        for(std::set<pid_t>::const_iterator it = missingPids.begin();
            it != missingPids.end(); ++it)
        {
            m_monitoredPids.erase(*it);
            m_ioService.post(boost::bind(m_callback,*it));
        }

    }

    void
    ProcessMonitorImpl::StartMonitorPidInternal(const pid_t pid)
    {
        m_monitoredPids.insert(pid);
    }

    void
    ProcessMonitorImpl::StopMonitorPidInternal(const pid_t pid)
    {
        m_monitoredPids.erase(pid);
    }
}
}

#endif
