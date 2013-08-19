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

#if defined(linux) || defined(__linux) || defined(__linux__)

#include "ProcessMonitorImpl.h"
#include "Safir/Utilities/ProcessMonitor.h"
#include <boost/asio.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/thread.hpp>
#include <boost/thread/mutex.hpp>
#include <list>
#include <map>
#include <set>
#include <string>
#include <sys/inotify.h>
#include <sys/types.h>

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
    {
    public:
        explicit ProcessMonitorLinux(const ProcessMonitor::OnTerminateCb& callback);
        ~ProcessMonitorLinux();

        //StartMonitorPid and StopMonitorPid induces the io_service to call this
        //to change the monitored pids on the io_service thread.
        void ChangeMonitoredPids();

        void HandleInotifyEvent (const boost::system::error_code ec);
        void HandleTimeout();

        void StartThread();
        void StopThread();
        
        
        void StartMonitorPid(const pid_t pid);
        void StopMonitorPid(const pid_t pid);

        void Run(); // Thread loop
    private:

        // Client callback
        ProcessMonitor::OnTerminateCb m_callback;

        //The io_service we use to listen to inotify events and to schedule timers
        boost::asio::io_service m_ioService;

        //asio object that wraps the inotify file descriptor
        boost::asio::posix::stream_descriptor m_inotifyStream;

        //Timer used for polling a process after we have received an inotify event
        // on its /proc/xxxx directory
        boost::shared_ptr<boost::asio::deadline_timer> m_timer;
        
        // Buffer for incoming events
        unsigned char m_buf[INOTIFY_BUFLEN];  

        boost::thread m_thread;

        // Queue of pids to add / remove
        std::list<pid_t> m_startWatchPids;
        std::list<pid_t> m_stopWatchPids;
        boost::mutex m_mutex;
        
        typedef std::list<pid_t> PidList;
        class WdInfo
        {
        public:
            PidList m_pidList;
            std::string m_binPath;
        };
        
        typedef std::map<pid_t       /*pid*/           , int /*wd*/, std::less<pid_t> > PidMap;
        typedef std::map<int         /*wd*/            , WdInfo    , std::less<int> >   WdMap;
        typedef std::map<std::string /*bin path*/      , int /*wd*/, std::less<std::string> > BinPathMap;
        
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
