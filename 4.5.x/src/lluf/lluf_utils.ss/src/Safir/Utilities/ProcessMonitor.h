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
#include <boost/shared_ptr.hpp>

namespace Safir
{
namespace Utilities
{
    class ProcessMonitorImpl;

    class LLUF_UTILS_API ProcessMonitor 
    {
    public:
        typedef boost::function<void(const pid_t pid)> OnTerminateCb;

        ProcessMonitor();
        ~ProcessMonitor();

        /**
         * Init the ProcessMonitor.
         *
         * This method must be called first thing and before any call to StartMonitorPid or StopMonitorPid.
         *
         * @param [in] callback The function to be called when a monitored process exists. Note that this function
         *                      is executed in ProcessMonitor's own thread.
         */
        void Init(const OnTerminateCb& callback);
        

        /**
         * Start monitor the given PID.
         *
         * @param [in] pid  The PID which we want to monitor.
         */
        void StartMonitorPid(const pid_t pid);

        /**
         * Stop monitor the given PID.
         *
         * @param [in] pid  The PID which we want to stop monitor.
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

