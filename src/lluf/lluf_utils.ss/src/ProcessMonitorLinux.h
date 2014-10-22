/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safir.sourceforge.net)
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

#if defined(linux) || defined(__linux) || defined(__linux__)

#include <boost/asio.hpp>
#include <boost/asio/steady_timer.hpp>
#include <boost/bind.hpp>
#include <boost/function.hpp>
#include <set>



namespace Safir
{
namespace Utilities
{
#if 0
    // Event struct size
    const int INOTIFY_EVENT_SIZE = sizeof(struct inotify_event);

    // Event buffer length
    const int INOTIFY_BUFLEN  = 1024 * (INOTIFY_EVENT_SIZE + 16);
#endif
    class ProcessMonitorImpl
    {
    public:
        ProcessMonitorImpl(boost::asio::io_service& ioService,
                           const boost::function<void(const pid_t pid)>& callback,
                           const boost::chrono::steady_clock::duration& pollPeriod);

        void Stop();
#if 0
        ~ProcessMonitorImpl();

        //StartMonitorPid and StopMonitorPid induces the io_service to call this
        //to change the monitored pids on the io_service thread.
        void ChangeMonitoredPids();

        void HandleInotifyEvent (const boost::system::error_code ec);
        void HandleTimeout();

        void StartThread();
        void StopThread();

#endif
        void StartMonitorPid(const pid_t pid)
        {
            m_strand.dispatch(boost::bind(&ProcessMonitorImpl::StartMonitorPidInternal,this,pid));
        }

        void StopMonitorPid(const pid_t pid)
        {
            m_strand.dispatch(boost::bind(&ProcessMonitorImpl::StopMonitorPidInternal,this,pid));
        }
#if 0
        void Run(); // Thread loop
#endif
    private:
        void StartMonitorPidInternal(const pid_t pid);
        void StopMonitorPidInternal(const pid_t pid);

        void Poll(const boost::system::error_code& error);

        // Client callback
        boost::function<void(const pid_t pid)> m_callback;

        //The io_service we use to listen to inotify events and to schedule timers
        boost::asio::io_service& m_ioService;

        boost::asio::io_service::strand m_strand;

        const boost::chrono::steady_clock::duration m_pollPeriod;
        boost::asio::steady_timer m_pollTimer;
#if 0

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
#endif
        std::set<pid_t> m_monitoredPids;
    };
}
}

#endif
