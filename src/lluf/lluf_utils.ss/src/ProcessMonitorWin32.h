/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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

#ifdef _MSC_VER

#include "ProcessMonitorImpl.h"
#include "Safir/Utilities/ProcessMonitor.h"

#include <ace/Auto_Event.h>
#include <ace/Thread.h>
#include <ace/Synch.h>
#include <ace/OS.h>

#include <boost/shared_ptr.hpp>

#include <map>
#include <vector>
#include <set>

namespace Safir
{
namespace Utilities
{
    enum ThreadState
    {
        NotStarted,  
        Running,     
        Stopped, 
    };

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

        void Run(); // Thread loop
    private:
        static ACE_THR_FUNC_RETURN ThreadFun(void* param);

        
        // Thread stuff
        ACE_Auto_Event m_startEvent;
        ACE_Auto_Event m_stopEvent;
        
        volatile bool m_stop;
        
        ThreadState m_threadState;
        
        // Supervised pids
        typedef std::map<pid_t, HANDLE, std::less<pid_t> > PidMap;
        PidMap m_pids;
        
        std::vector<pid_t> m_deadPids;
        
        ACE_Thread_Mutex m_mutex;
        
        HANDLE m_prevThreadEvent;

        HANDLE m_signalEvent;
        boost::shared_ptr<ProcessMonitorWin32Thread> m_nextThread;
    };


    class ProcessMonitorWin32 : public ProcessMonitorImpl
    {
    public:
        ProcessMonitorWin32(const ProcessMonitor::OnTerminateCb& callback);
        ~ProcessMonitorWin32();

        void StartThread();
        void StopThread();
        
        void StartMonitorPid(const pid_t pid);
        void StopMonitorPid(const pid_t pid);

        void Run(); // Thread loop
    private:
        static ACE_THR_FUNC_RETURN ThreadFun(void* param);

        // Client callback
        ProcessMonitor::OnTerminateCb m_callback;
        
        // Thread stuff
        ACE_Auto_Event m_startEvent;
        ACE_Auto_Event m_stopEvent;
        
        volatile bool m_stop;
        
        ThreadState m_threadState;
        
        // Supervised pids
        typedef std::set<pid_t> PidSet;
        PidSet m_pids;
        
        ACE_Thread_Mutex m_mutex;
        
        HANDLE m_signalEvent;

        HANDLE m_nextThreadEvent;
        boost::shared_ptr<ProcessMonitorWin32Thread> m_nextThread;
    };

    
}
}

#endif
#endif
