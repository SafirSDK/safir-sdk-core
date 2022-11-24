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
#ifndef __PROCESS_MONITOR_H__
#define __PROCESS_MONITOR_H__

#include <Safir/Utilities/Internal/UtilsExportDefs.h>
#include <Safir/Utilities/ProcessInfo.h>
#include <boost/asio.hpp>
#include <functional>
#include <memory>

namespace Safir
{
namespace Utilities
{
    class ProcessMonitorImpl;

    class LLUF_INTERNAL_API ProcessMonitor
    {
    public:
        /**
         * Constructor.
         *
         * @param [in] io The io_context that will be used for monitoring processes.
         * @param [in] callback Callback that will be invoked when a process terminates.
         * @param [in] pollPeriod On some platforms polling may be used for process detection,
         *                        instead of events. This should be set to something like 1 second.
         */
        ProcessMonitor(boost::asio::io_context& io,
                       const std::function<void(const pid_t pid)>& callback,
                       const boost::chrono::steady_clock::duration& pollPeriod);

        /**
         * Stop monitoring all processes and stop the process monitor.
         *
         * This member function is thread safe.
         */
        void Stop();

        /**
         * Start monitor the given PID.
         * Multiple calls with the same pid will give only one callback
         *
         * This member function is thread safe.
         *
         * @param [in] pid  The PID which we want to monitor.
         */
        void StartMonitorPid(const pid_t pid);

        /**
         * Stop monitor the given PID.
         * One call to StopMonitorPid will stop monitoring the pid even if StartMonitorPid was
         * called multiple times for that pid.
         *
         * This member function is thread safe.
         *
         * @param [in] pid  The PID to stop monitoring.
         */
        void StopMonitorPid(const pid_t pid);

    private:

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4251)
#endif

        std::shared_ptr<ProcessMonitorImpl> m_impl;

#ifdef _MSC_VER
#pragma warning (pop)
#endif
    };

}
}

#endif
