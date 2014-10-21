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
#ifndef __PROCESS_MONITOR_H__
#define __PROCESS_MONITOR_H__

#include <Safir/Utilities/Internal/UtilsExportDefs.h>
#include <Safir/Utilities/ProcessInfo.h>
#include <boost/function.hpp>
#include <boost/asio.hpp>
#include <boost/shared_ptr.hpp>

namespace Safir
{
namespace Utilities
{
    class ProcessMonitorImpl;

    class LLUF_UTILS_API ProcessMonitor
    {
    public:
        /**
         * Constructor.
         *
         * @param [in] ioService The io_service that will be used for monitoring processes.
         * @param [in] callback Callback that will be invoked when a process terminates.
         */
        ProcessMonitor(boost::asio::io_service& ioService,
                       const boost::function<void(const pid_t pid)>& callback);

        /**
         * Start monitor the given PID.
         *
         * @param [in] pid  The PID which we want to monitor.
         */
        void StartMonitorPid(const pid_t pid);

        /**
         * Stop monitor the given PID.
         *
         * @param [in] pid  The PID to stop monitoring.
         */
        void StopMonitorPid(const pid_t pid);

    private:

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4251)
#endif

        boost::shared_ptr<ProcessMonitorImpl> m_impl;

#ifdef _MSC_VER
#pragma warning (pop)
#endif
    };

}
}

#endif
