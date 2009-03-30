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
#ifndef __PROCESS_MONITOR_LINUX_H__
#define __PROCESS_MONITOR_LINUX_H__

#ifdef __GNUC__

#include <ace/Auto_Event.h>
#include <ace/Event_Handler.h>
#include <ace/Reactor.h>
#include <ace/Thread.h>
#include <ace/Mutex.h>

#include <boost/date_time/posix_time/posix_time.hpp>

#include "ProcessMonitorImpl.h"
#include "Safir/Utilities/ProcessMonitor.h"

#include <sys/types.h>
#include <sys/inotify.h>

#include <list>
#include <map>
#include <set>
#include <string>

namespace Safir
{
namespace Utilities
{
    // Event struct size
    const int INOTIFY_EVENT_SIZE = sizeof(struct inotify_event);

    // Event buffer length
    const int INOTIFY_BUFLEN  = 1024 * (INOTIFY_EVENT_SIZE + 16);

    class ProcessMonitorLinux 
        : public ProcessMonitorImpl
        , public ACE_Event_Handler
    {
    public:
        ProcessMonitorLinux(const ProcessMonitor::OnTerminateCb& callback);
        ~ProcessMonitorLinux();

        // Overrides ACE_Event_Handler
        int handle_input (ACE_HANDLE fd);
        int handle_exception (ACE_HANDLE fd=ACE_INVALID_HANDLE);
        int handle_timeout (const ACE_Time_Value &current_time, const void *act=0);
        
        void StartThread();
        void StopThread();
        
        
        void StartMonitorPid(const pid_t pid);
        void StopMonitorPid(const pid_t pid);

        void Run(); // Thread loop
    private:

        static ACE_THR_FUNC_RETURN ThreadFun(void* param);

        // Client callback
        ProcessMonitor::OnTerminateCb m_callback;

        // The reactor and the FD which it supervise
        ACE_Reactor m_reactor;
        int m_fd;
        
        // Buffer for incoming events
        unsigned char m_buf[INOTIFY_BUFLEN];  

        // Thread stuff
        ACE_Auto_Event m_startEvent;
        ACE_Auto_Event m_stopEvent;

        
        enum ThreadState
        {
            NotStarted,  
            Running,     
            Stopped, 
        };

        ThreadState m_threadState;

        // Queue of pids to add / remove
        std::list<pid_t> m_startWatchPids;
        std::list<pid_t> m_stopWatchPids;
        ACE_Thread_Mutex m_mutex;
        
        typedef std::list<pid_t> PidList;
        class WdInfo
        {
        public:
            PidList m_pidList;
            std::string m_binPath;
        };
        
        typedef std::map<pid_t /*pid*/           , int /*wd*/, std::less<pid_t> > PidMap;
        typedef std::map<int   /*wd*/            , WdInfo    , std::less<int> >   WdMap;
        typedef std::map<std::string /*bin path*/, int /*wd*/, std::less<std::string> > BinPathMap;
        
        PidMap m_pidMap;
        WdMap  m_wdMap;
        BinPathMap m_binpathMap;

        typedef std::set<int /*wd*/> WdSet;
        
        WdSet m_wdQueue;
        boost::posix_time::ptime m_checkUntil;
        bool m_timerStarted;
    };
}
}

#endif
#endif
