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
#if 0
#if defined(linux) || defined(__linux) || defined(__linux__)

#include <boost/lexical_cast.hpp>
#include <boost/filesystem.hpp>

#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>

#include "ProcessMonitorLinux.h"

#include <sys/inotify.h>
#include <unistd.h>
#include <sys/types.h>
#include <signal.h>

#include <sstream>

namespace Safir
{
namespace Utilities
{
    bool GetPathForPid(const pid_t pid, std::string& path)
    {
        std::ostringstream os;

        os << "/proc/" << pid << "/exe";

        boost::filesystem::path bp(os.str());

        lllout << "GetPathForPid() - is_symlink: " <<  boost::filesystem::is_symlink(bp) << std::endl;

        if(boost::filesystem::is_symlink(bp))
        {
            char buf[1025];
            int len = readlink(os.str().c_str(),buf,1024);

            if (len != -1)
            {
                buf[len] = 0;

                lllout << "GetPathForPid() - buf: " << buf << std::endl;

                path = std::string(buf);
                return true;
            }
        }

        return false;
    }



    ProcessMonitorLinux::ProcessMonitorLinux(const ProcessMonitor::OnTerminateCb& callback)
        : m_callback(callback)
        , m_ioService()
        , m_inotifyStream(m_ioService)
        , m_timerStarted(false)
    {
        lllout << "ProcessMonitorLinux::ProcessMonitorLinux() - called..." << std::endl;
    }

    ProcessMonitorLinux::~ProcessMonitorLinux() {}

    void ProcessMonitorLinux::HandleInotifyEvent(const boost::system::error_code ec)
    {
        lllout << "ProcessMonitorLinux::HandleInotifyEvent() - called..." << std::endl;

        if (ec)
        {
            SEND_SYSTEM_LOG(Critical,
                            << "ProcessMonitorLinux::HandleInotifyEvent() - Error from boost::asio " << ec);
            return;
        }

        const ssize_t len = read(m_inotifyStream.native(), m_buf, INOTIFY_BUFLEN);

        ssize_t i = 0;
        while (i < len)
        {
            struct inotify_event* pEvt = (struct inotify_event*) &m_buf[i];


            lllout << "ProcessMonitorLinux::HandleInotifyEvent() - wd: " << pEvt->wd
                   << ". mask: " << pEvt->mask << std::endl;

            if (pEvt->mask == IN_CLOSE_NOWRITE)
            {
                m_wdQueue.insert(pEvt->wd);
            }


            i += INOTIFY_EVENT_SIZE + (ssize_t) pEvt->len;
        }

        if (m_wdQueue.size() > 0)
        {
            m_checkUntil = boost::posix_time::microsec_clock::universal_time() + boost::posix_time::seconds(1);

            if (!m_timerStarted)
            {
                m_timer.reset(new boost::asio::deadline_timer(m_ioService,boost::posix_time::seconds(0)));
                m_timer->async_wait(boost::bind(&ProcessMonitorLinux::HandleTimeout,this));
                m_timerStarted = true;
            }
        }

        m_inotifyStream.async_read_some(boost::asio::null_buffers(), boost::bind(&ProcessMonitorLinux::HandleInotifyEvent,this,_1));

        lllout << "ProcessMonitorLinux::HandleInotifyEvent() - done...  " << std::endl;
    }

    void ProcessMonitorLinux::ChangeMonitoredPids()
    {
        lllout << "ProcessMonitorLinux::ChangeMonitoredPids() - called... " << std::endl;

        //  Add lock
        boost::lock_guard<boost::mutex> lck(m_mutex);

        // Handle added pids.
        for(std::list<pid_t>::iterator it = m_startWatchPids.begin();
            it != m_startWatchPids.end();
            ++it)
        {

            pid_t pid = (*it);
            std::string path;

            if(GetPathForPid(pid, path))
            {
                BinPathMap::iterator binIt = m_binpathMap.find(path);

                if (binIt != m_binpathMap.end())
                {
                    // Already watching this path. Only update out maps

                    int wd = (*binIt).second;

                    lllout << "ProcessMonitorLinux::ChangeMonitoredPids() - found wd: " << wd << std::endl;

                    WdMap::iterator wdIt = m_wdMap.find(wd);

                    (*wdIt).second.m_pidList.push_back(pid);

                    m_pidMap.insert(std::make_pair(pid,wd));
                }
                else
                {
                    int wd = inotify_add_watch(m_inotifyStream.native(), path.c_str(), IN_CLOSE_NOWRITE);

                    lllout << "ProcessMonitorLinux::ChangeMonitoredPids - inotify_add_watch(). fd: " << m_inotifyStream.native()
                           << ". path: " << path.c_str()
                           << ". wd: " << wd << std::endl;

                    if (wd != - 1)
                    {
                        m_binpathMap.insert(std::make_pair(path,wd));

                        WdInfo wInfo;
                        wInfo.m_pidList.push_back(pid);
                        wInfo.m_binPath = path;
                        m_wdMap.insert(std::make_pair(wd,wInfo));

                        m_pidMap.insert(std::make_pair(pid,wd));
                    }
                    else
                    {
                        SEND_SYSTEM_LOG(Critical,
                                        << "ProcessMonitorLinux::ChangeMonitoredPids() - problem calling inotify_add_watch(). path: "
                                        << path.c_str()
                                        << ". errno: " << errno);
                    }
                }
            }
            else
            {
                SEND_SYSTEM_LOG(Critical,
                                << "ProcessMonitorLinux::ChangeMonitoredPids() - problem finding bin path for pid: " << pid);
            }
        }

        m_startWatchPids.clear();

        // Handle remove pids.
        for(std::list<pid_t>::iterator it = m_stopWatchPids.begin();
            it != m_stopWatchPids.end();
            ++it)
        {
            pid_t pid = (*it);

            PidMap::iterator pidIt = m_pidMap.find(pid);

            if  (pidIt != m_pidMap.end())
            {
                int wd = (*pidIt).second;

                WdMap::iterator wdIt = m_wdMap.find(wd);

                if (wdIt != m_wdMap.end())
                {
                    (*wdIt).second.m_pidList.remove(pid);

                    // No pids left for this wd. Remove it and remove inotify watch.
                    if((*wdIt).second.m_pidList.empty())
                    {
                        int result = inotify_rm_watch(m_inotifyStream.native(), (*wdIt).first);

                        lllout << "ProcessMonitorLinux::ChangeMonitoredPids - inotify_rm_watch(). fd: " << m_inotifyStream.native()
                               << ". wd: " << (*wdIt).first
                               << ". result: " << result << std::endl;

                        if (result == -1)
                        {
                            SEND_SYSTEM_LOG(Critical,
                                            << "ProcessMonitorLinux::ChangeMonitoredPids - problem with inotify_rm_watch() wd: "
                                            << (*wdIt).first << ". errno: " << errno);
                        }

                        // Clean m_binpathMap
                        m_binpathMap.erase((*wdIt).second.m_binPath);

                        // Clean m_wdMap
                        m_wdMap.erase(wdIt);
                    }
                }

                // Remove pid
                m_pidMap.erase(pidIt);
            }
        }

        m_stopWatchPids.clear();
    }

    void ProcessMonitorLinux::HandleTimeout()
    {
        WdSet removeWd;

        for(WdSet::iterator it = m_wdQueue.begin();
            it != m_wdQueue.end();
            ++it)
        {
            WdMap::iterator wdIt = m_wdMap.find((*it));

            if (wdIt != m_wdMap.end())
            {
                PidList deadPids;

                // Check for dead pids
                for(PidList::iterator plIt = (*wdIt).second.m_pidList.begin();
                    plIt != (*wdIt).second.m_pidList.end();
                    ++plIt)
                {
                    if (kill((*plIt), 0 /* No signal */) != 0)
                    {
                        // Process dosn't exist anymore.

                        lllout << "ProcessMonitorLinux::HandleTimeout() - Dont exists pid: " << (*plIt) << std::endl;
                        deadPids.push_back((*plIt));
                    }
                }


                // Handle dead pids.
                for(PidList::iterator deadIt = deadPids.begin();
                    deadIt != deadPids.end();
                    ++deadIt)
                {
                    // Run callback fnc
                    m_callback((*deadIt));

                    (*wdIt).second.m_pidList.remove((*deadIt));

                    m_pidMap.erase((*deadIt));
                }

                // No pids left for this wd. Remove it and remove inotify watch.
                if((*wdIt).second.m_pidList.empty())
                {
                    int result = inotify_rm_watch(m_inotifyStream.native(), (*wdIt).first);

                    lllout << "ProcessMonitorLinux::HandleTimeout - inotify_rm_watch(). fd: " << m_inotifyStream.native()
                           << ". wd: " << (*wdIt).first
                           << ". result: " << result << std::endl;

                    if (result == -1)
                    {
                        SEND_SYSTEM_LOG(Critical,
                                        << "ProcessMonitorLinux::HandleTimeout - problem with inotify_rm_watch() wd: "
                                        << (*wdIt).first
                                        << ". errno: " << errno);
                    }

                    // Store it
                    removeWd.insert((*wdIt).first);

                    // Clean m_binpathMap
                    m_binpathMap.erase((*wdIt).second.m_binPath);

                    // Clean m_wdMap
                    m_wdMap.erase(wdIt);

                }
            }
        }

        // These wd's are removed. Erase them from our check queue.
        for(WdSet::iterator it = removeWd.begin();
            it != removeWd.end();
            ++it)
        {
            m_wdQueue.erase((*it));
        }

        // Check if we passed the 'until' time.
        if (m_checkUntil < boost::posix_time::microsec_clock::universal_time())
        {
            lllout << "ProcessMonitorLinux::HandleTimeout() - m_checkUntil passed: " << m_checkUntil << std::endl;
            m_wdQueue.clear();
        }


        // Check if there is more to handle
        if (m_wdQueue.size() > 0)
        {
            m_timer.reset(new boost::asio::deadline_timer(m_ioService,boost::posix_time::milliseconds(10)));
            m_timer->async_wait(boost::bind(&ProcessMonitorLinux::HandleTimeout,this));
        }
        else
        {
            m_timerStarted = false;
        }
    }


    void
    ProcessMonitorLinux::StartThread()
    {
        lllout << "ProcessMonitorLinux::StartThread() - called... tid: " << boost::this_thread::get_id() << std::endl;

        if (m_thread.get_id() != boost::thread::id())
        {
            return;
        }

        m_thread = boost::thread(boost::bind(&ProcessMonitorLinux::Run,this));

        lllout << "ProcessMonitorLinux::StartThread() - done..." << std::endl;
    }


    void
    ProcessMonitorLinux::StopThread()
    {
        lllout << "ProcessMonitorLinux::StopThread() - called... tid: " << boost::this_thread::get_id() << std::endl;

        if (m_thread.get_id() != boost::thread::id())
        {
            m_ioService.stop();

            m_thread.join();
            m_thread = boost::thread();
        }

        lllout << "ProcessMonitorLinux::StopThread() - done..." << std::endl;
    }



    void
    ProcessMonitorLinux::StartMonitorPid(const pid_t pid)
    {
        lllout << "ProcessMonitorLinux::StartMonitorPid() - pid: " << pid << std::endl;

        // Add lock
        boost::lock_guard<boost::mutex> lck(m_mutex);

        m_startWatchPids.push_back(pid);

        m_ioService.post(boost::bind(&ProcessMonitorLinux::ChangeMonitoredPids,this));
    }

    void
    ProcessMonitorLinux::StopMonitorPid(const pid_t pid)
    {
        lllout << "ProcessMonitorLinux::StopMonitorPid() - pid: " << pid << std::endl;

        // Add lock
        boost::lock_guard<boost::mutex> lck(m_mutex);

        m_stopWatchPids.push_back(pid);

        m_ioService.post(boost::bind(&ProcessMonitorLinux::ChangeMonitoredPids,this));
    }


    void ProcessMonitorLinux::Run()
    {
        lllout << "ProcessMonitorLinux::Run() - called...  tid: " << boost::this_thread::get_id() << std::endl;
        const int fd = inotify_init();

        if (fd == -1)
        {
                SEND_SYSTEM_LOG(Critical,
                                << "ProcessMonitorLinux::Run() - Problem with inotify_init()....");

            return;
        }

        lllout << "ProcessMonitorLinux::Run() - fd: " << fd << std::endl;

        m_inotifyStream.assign(fd);
        m_inotifyStream.async_read_some(boost::asio::null_buffers(), boost::bind(&ProcessMonitorLinux::HandleInotifyEvent,this,_1));
        lllout << "ProcessMonitorLinux::Run() - about to start io_service loop... " << std::endl;

        //The fact that HandleInotifyEvent will reschedule an async_read_some means that the
        //io_service has work to do, and hence run() will not exit
        m_ioService.run();

        lllout << "ProcessMonitorLinux::Run() - io_service loop done... " << std::endl;

        for(WdMap::iterator it = m_wdMap.begin();
            it != m_wdMap.end();
            ++it)
        {
            const int result = inotify_rm_watch(m_inotifyStream.native(), (*it).first);

            lllout << "ProcessMonitorLinux::Run() - inotify_rm_watch(). fd: " << m_inotifyStream.native()
                   << ". wd: " << (*it).first
                   << ". result: " << result << std::endl;

            if (result == -1)
            {
                SEND_SYSTEM_LOG(Critical,
                                << "ProcessMonitorLinux::Run() - problem with inotify_rm_watch() wd: " << (*it).first
                                << ". errno: " << errno);
            }
        }
    }
}
}

#endif
#endif
