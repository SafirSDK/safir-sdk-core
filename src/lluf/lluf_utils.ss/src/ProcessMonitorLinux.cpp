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

#if 0

#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>


#include <sys/inotify.h>
#include <unistd.h>
#include <sys/types.h>
#include <signal.h>

#include <sstream>
#endif

namespace Safir
{
namespace Utilities
{
    // Will throw std::runtime_error if pid was not found
    boost::filesystem::path GetProcPathForPid(const pid_t pid)
    {
        boost::filesystem::path path = "/proc";
        path /= boost::lexical_cast<std::string>(pid);
        path /= "exe";
        if (!boost::filesystem::exists(path))
        {
            throw std::runtime_error("Process with pid "
                                     + boost::lexical_cast<std::string>(pid)
                                     + " does not appear to exist.");
        }
        return path;
    }

    // Will throw std::runtime_error if path cannot be resolved,
    // e.g. if the executable has been deleted.
    // Note that this is a common occurrence during development, when the compiler
    // overwrites binaries that may still be running.
    boost::filesystem::path ResolveProcPath(const boost::filesystem::path& path)
    {
        if(!boost::filesystem::is_symlink(path))
        {
            throw std::runtime_error("Proc path does not appear to be a symlink: " + path.string());
        }

        return boost::filesystem::canonical(path);
    }


    ProcessMonitorImpl::ProcessMonitorImpl(boost::asio::io_service& ioService,
                                           const boost::function<void(const pid_t pid)>& callback)
        : m_callback(callback)
        , m_ioService(ioService)
        , m_strand(ioService)
        , m_pollPeriod(boost::chrono::seconds(1))
        , m_pollTimer(ioService)
          //, m_inotifyStream(m_ioService)
          //        , m_timerStarted(false)
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
#if 0


    ProcessMonitorImpl::~ProcessMonitorImpl() {}

    void ProcessMonitorImpl::HandleInotifyEvent(const boost::system::error_code ec)
    {
        lllout << "ProcessMonitorImpl::HandleInotifyEvent() - called..." << std::endl;

        if (ec)
        {
            SEND_SYSTEM_LOG(Critical,
                            << "ProcessMonitorImpl::HandleInotifyEvent() - Error from boost::asio " << ec);
            return;
        }

        const ssize_t len = read(m_inotifyStream.native(), m_buf, INOTIFY_BUFLEN);

        ssize_t i = 0;
        while (i < len)
        {
            struct inotify_event* pEvt = (struct inotify_event*) &m_buf[i];


            lllout << "ProcessMonitorImpl::HandleInotifyEvent() - wd: " << pEvt->wd
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
                m_timer->async_wait(boost::bind(&ProcessMonitorImpl::HandleTimeout,this));
                m_timerStarted = true;
            }
        }

        m_inotifyStream.async_read_some(boost::asio::null_buffers(), boost::bind(&ProcessMonitorImpl::HandleInotifyEvent,this,_1));

        lllout << "ProcessMonitorImpl::HandleInotifyEvent() - done...  " << std::endl;
    }

    void ProcessMonitorImpl::ChangeMonitoredPids()
    {
        lllout << "ProcessMonitorImpl::ChangeMonitoredPids() - called... " << std::endl;

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

                    lllout << "ProcessMonitorImpl::ChangeMonitoredPids() - found wd: " << wd << std::endl;

                    WdMap::iterator wdIt = m_wdMap.find(wd);

                    (*wdIt).second.m_pidList.push_back(pid);

                    m_pidMap.insert(std::make_pair(pid,wd));
                }
                else
                {
                    int wd = inotify_add_watch(m_inotifyStream.native(), path.c_str(), IN_CLOSE_NOWRITE);

                    lllout << "ProcessMonitorImpl::ChangeMonitoredPids - inotify_add_watch(). fd: " << m_inotifyStream.native()
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
                                        << "ProcessMonitorImpl::ChangeMonitoredPids() - problem calling inotify_add_watch(). path: "
                                        << path.c_str()
                                        << ". errno: " << errno);
                    }
                }
            }
            else
            {
                SEND_SYSTEM_LOG(Critical,
                                << "ProcessMonitorImpl::ChangeMonitoredPids() - problem finding bin path for pid: " << pid);
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

                        lllout << "ProcessMonitorImpl::ChangeMonitoredPids - inotify_rm_watch(). fd: " << m_inotifyStream.native()
                               << ". wd: " << (*wdIt).first
                               << ". result: " << result << std::endl;

                        if (result == -1)
                        {
                            SEND_SYSTEM_LOG(Critical,
                                            << "ProcessMonitorImpl::ChangeMonitoredPids - problem with inotify_rm_watch() wd: "
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

    void ProcessMonitorImpl::HandleTimeout()
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

                        lllout << "ProcessMonitorImpl::HandleTimeout() - Dont exists pid: " << (*plIt) << std::endl;
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

                    lllout << "ProcessMonitorImpl::HandleTimeout - inotify_rm_watch(). fd: " << m_inotifyStream.native()
                           << ". wd: " << (*wdIt).first
                           << ". result: " << result << std::endl;

                    if (result == -1)
                    {
                        SEND_SYSTEM_LOG(Critical,
                                        << "ProcessMonitorImpl::HandleTimeout - problem with inotify_rm_watch() wd: "
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
            lllout << "ProcessMonitorImpl::HandleTimeout() - m_checkUntil passed: " << m_checkUntil << std::endl;
            m_wdQueue.clear();
        }


        // Check if there is more to handle
        if (m_wdQueue.size() > 0)
        {
            m_timer.reset(new boost::asio::deadline_timer(m_ioService,boost::posix_time::milliseconds(10)));
            m_timer->async_wait(boost::bind(&ProcessMonitorImpl::HandleTimeout,this));
        }
        else
        {
            m_timerStarted = false;
        }
    }




#endif
    void
    ProcessMonitorImpl::StartMonitorPidInternal(const pid_t pid)
    {
        m_monitoredPids.insert(pid);
        /*        boost::filesystem::path procPath;
        try
        {
            procPath = GetProcPathForPid(pid);
        }
        catch (const std::runtime_error& e)
        {
            //std::wcout << e.what() << std::endl;
            m_callback(pid);
            return;
        }
        std::wcout << "StartMonitorPidInternal: pid = " << pid
        << ", path = " << ResolveProcPath(procPath).string().c_str() << std::endl;*/

        //m_callback(pid);
        // Add lock
        // boost::lock_guard<boost::mutex> lck(m_mutex);

        //m_startWatchPids.push_back(pid);

        //m_ioService.post(boost::bind(&ProcessMonitorImpl::ChangeMonitoredPids,this));
    }

    void
    ProcessMonitorImpl::StopMonitorPidInternal(const pid_t pid)
    {
        m_monitoredPids.erase(pid);
        // Add lock
        //boost::lock_guard<boost::mutex> lck(m_mutex);

        //m_stopWatchPids.push_back(pid);

        //m_ioService.post(boost::bind(&ProcessMonitorImpl::ChangeMonitoredPids,this));
    }

#if 0
    void ProcessMonitorImpl::Run()
    {
        lllout << "ProcessMonitorImpl::Run() - called...  tid: " << boost::this_thread::get_id() << std::endl;
        const int fd = inotify_init();

        if (fd == -1)
        {
                SEND_SYSTEM_LOG(Critical,
                                << "ProcessMonitorImpl::Run() - Problem with inotify_init()....");

            return;
        }

        lllout << "ProcessMonitorImpl::Run() - fd: " << fd << std::endl;

        m_inotifyStream.assign(fd);
        m_inotifyStream.async_read_some(boost::asio::null_buffers(), boost::bind(&ProcessMonitorImpl::HandleInotifyEvent,this,_1));
        lllout << "ProcessMonitorImpl::Run() - about to start io_service loop... " << std::endl;

        //The fact that HandleInotifyEvent will reschedule an async_read_some means that the
        //io_service has work to do, and hence run() will not exit
        m_ioService.run();

        lllout << "ProcessMonitorImpl::Run() - io_service loop done... " << std::endl;

        for(WdMap::iterator it = m_wdMap.begin();
            it != m_wdMap.end();
            ++it)
        {
            const int result = inotify_rm_watch(m_inotifyStream.native(), (*it).first);

            lllout << "ProcessMonitorImpl::Run() - inotify_rm_watch(). fd: " << m_inotifyStream.native()
                   << ". wd: " << (*it).first
                   << ". result: " << result << std::endl;

            if (result == -1)
            {
                SEND_SYSTEM_LOG(Critical,
                                << "ProcessMonitorImpl::Run() - problem with inotify_rm_watch() wd: " << (*it).first
                                << ". errno: " << errno);
            }
        }
    }
#endif

}
}

#endif
