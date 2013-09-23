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
#ifndef __PROCESS_MONITOR_WIN32_H__
#define __PROCESS_MONITOR_WIN32_H__

#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)

#include "ProcessMonitorImpl.h"
#include <Safir/Utilities/ProcessMonitor.h>

#include <windows.h>
#include <boost/shared_ptr.hpp>
#include <boost/thread.hpp>

#include <map>
#include <vector>
#include <set>

namespace Safir
{
namespace Utilities
{
    class ProcessMonitorWin32Thread
    {
    public:
        ProcessMonitorWin32Thread(const HANDLE& event);
        ~ProcessMonitorWin32Thread();

        void StartThread();
        void StopThread();
        
        void StartMonitorPid(const pid_t pid);
        void StopMonitorPid(const pid_t pid);

        void GetTerminatedPids(std::vector<pid_t>& pids);

    private:
        void Run(); // Thread loop        

        boost::thread m_thread;
        bool m_stop;
        
        // Supervised pids
        typedef std::map<pid_t, HANDLE, std::less<pid_t> > PidMap;
        PidMap m_pids;
        
        std::vector<pid_t> m_deadPids;
        
        boost::mutex m_mutex;
        
        HANDLE m_prevThreadEvent;

        HANDLE m_signalEvent;
        boost::shared_ptr<ProcessMonitorWin32Thread> m_nextThread;
    };


    class ProcessMonitorWin32 : public ProcessMonitorImpl
    {
    public:
        explicit ProcessMonitorWin32(const ProcessMonitor::OnTerminateCb& callback);
        ~ProcessMonitorWin32();

        void StartThread();
        void StopThread();
        
        void StartMonitorPid(const pid_t pid);
        void StopMonitorPid(const pid_t pid);
    private:
        void Run(); // Thread loop

        // Client callback
        ProcessMonitor::OnTerminateCb m_callback;

        boost::thread m_thread;
        bool m_stop;
        
        // Supervised pids
        typedef std::set<pid_t> PidSet;
        PidSet m_pids;
        
        boost::mutex m_mutex;
        
        HANDLE m_signalEvent;

        HANDLE m_nextThreadEvent;
        boost::shared_ptr<ProcessMonitorWin32Thread> m_nextThread;
    };

    
}
}

#endif
#endif
