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
#ifdef __GNUC__

#include <ace/Thread.h>

#include <boost/lexical_cast.hpp>
#include <boost/filesystem.hpp>

#include <Safir/Utilities/Internal/LowLevelLogger.h>

#include "ProcessMonitorLinux.h"

#include <sys/inotify.h>
#include <unistd.h>

#include <sstream>

namespace Safir
{
namespace Utilities
{

    const std::wstring ToWstring(const std::string & str)
    {
        int left = 0;
        wchar_t *pwszBuf = new wchar_t[str.length() + 1];
        wchar_t *pwsz;
        unsigned long pos;

        pwsz = pwszBuf;

        std::string::const_iterator it;
        for( it = str.begin(); it != str.end(); ++it)
        {
            pos = (unsigned char) *it;
            if ((left == 0) ^ ((pos & 0xC0) != 0x80)) // Continuation byte mismatch
            {
                left = 0;
                *pwsz++ = L'#';
            }

            if (pos < 0x80) // 7-bit ASCII
            {
                *pwsz++ = (wchar_t) pos;
            }
            else if ((pos & 0xC0) == (0x80)) // Correct continuation
            {
                left--;
                *pwsz = (*pwsz << 6) + (wchar_t) (pos & 0x3F);
                if (left == 0)
                    pwsz++;
            }
            else if ((pos & 0xE0) == (0xC0)) // First of 2
            {
                *pwsz = (wchar_t) (pos & 0x1F);
                left = 1;
            }
            else if ((pos & 0xF0) == (0xE0)) // First of 3
            {
                *pwsz = (wchar_t) (pos & 0x0F);
                left = 2;
            }
            else // Only the BMP is supported.
            {
                left = 0;
                *pwsz++ = L'#';
            }

        }

        std::wstring wstr( pwszBuf, pwsz - pwszBuf );

        delete [] pwszBuf;
        return wstr;
    }



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
            
                lllout << "GetPathForPid() - buf: " << ToWstring(buf) << std::endl;

                path = std::string(buf);
                return true;
            }
        }
        
        return false;
    }
    

    
    ProcessMonitorLinux::ProcessMonitorLinux(const ProcessMonitor::OnTerminateCb& callback)
        : m_callback(callback)
        , m_threadState(NotStarted)
        , m_timerStarted(false)
    {
        lllout << "ProcessMonitorLinux::ProcessMonitorLinux() - called..." << std::endl;

    }
    
    ProcessMonitorLinux::~ProcessMonitorLinux() {}


    int
    ProcessMonitorLinux::handle_input (ACE_HANDLE fd)
    {
        lllout << "ProcessMonitorLinux::handle_input() - called..." << std::endl;
        
        ssize_t len = read(fd, m_buf, INOTIFY_BUFLEN);
        
        ssize_t i = 0;
        while (i < len) 
        {
            struct inotify_event* pEvt = (struct inotify_event*) &m_buf[i];
            
            
            lllout << "ProcessMonitorLinux::handle_input() - wd: " << pEvt->wd 
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
                m_reactor.schedule_timer(this, NULL, ACE_Time_Value(0));
                m_timerStarted = true;
            }
        }
        
        lllout << "ProcessMonitorLinux::handle_input() - done...  " << std::endl;
        return 0;
    }

    int
    ProcessMonitorLinux::handle_exception (ACE_HANDLE /*fd*/)
    {
        lllout << "ProcessMonitorLinux::handle_exception() - called... " << std::endl;

        //  Add lock
        ACE_Guard<ACE_Thread_Mutex> lck(m_mutex);

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

                    lllout << "ProcessMonitorLinux::handle_exception() - found wd: " << wd << std::endl;
                    
                    WdMap::iterator wdIt = m_wdMap.find(wd);
                    
                    (*wdIt).second.m_pidList.push_back(pid);
                    
                    m_pidMap.insert(std::make_pair(pid,wd));
                }
                else
                {
                    int wd = inotify_add_watch(m_fd, path.c_str(), IN_CLOSE_NOWRITE);
            
                    lllout << "ProcessMonitorLinux::handle_exception - inotify_add_watch(). fd: " << m_fd 
                           << ". path: " << ToWstring(path)
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
                        lllerr << "ProcessMonitorLinux::handle_exception() - problem calling inotify_add_watch(). path: " 
                               << ToWstring(path)
                               << ". errno: " << errno << std::endl;
                    }
                }
            }
            else
            {
                lllerr << "ProcessMonitorLinux::handle_exception() - problem finding bin path for pid: " << pid << std::endl;
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
                        int result = inotify_rm_watch(m_fd, (*wdIt).first);
                    
                        lllout << "ProcessMonitorLinux::handle_exception - inotify_rm_watch(). fd: " << m_fd 
                               << ". wd: " << (*wdIt).first 
                               << ". result: " << result << std::endl;

                        if (result == -1)
                        {
                            lllerr << "ProcessMonitorLinux::handle_exception - problem with inotify_rm_watch() wd: " 
                                   << (*wdIt).first << ". errno: " << errno << std::endl;
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
        

        return 0;
    }
    
    int
    ProcessMonitorLinux::handle_timeout (const ACE_Time_Value& /*current_time*/, const void */*act*/)
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
                        
                        lllout << "ProcessMonitorLinux::handle_timeout() - Dont exists pid: " << (*plIt) << std::endl;
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
                    int result = inotify_rm_watch(m_fd, (*wdIt).first);
                    
                    lllout << "ProcessMonitorLinux::handle_timeout - inotify_rm_watch(). fd: " << m_fd 
                           << ". wd: " << (*wdIt).first 
                           << ". result: " << result << std::endl;
                    
                    if (result == -1)
                    {
                        lllerr << "ProcessMonitorLinux::handle_timeout - problem with inotify_rm_watch() wd: " << (*wdIt).first 
                               << ". errno: " << errno << std::endl;
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
            lllout << "ProcessMonitorLinux::handle_timeout() - m_checkUntil passed: " << m_checkUntil << std::endl;
            m_wdQueue.clear();
        }


        // Check if there is more to handle 
        if (m_wdQueue.size() > 0)
        {
            m_reactor.schedule_timer(this, NULL, ACE_Time_Value(0,10000));
        }
        else
        {
            m_timerStarted = false;
        }

        return 0;
    }
    

    
    void
    ProcessMonitorLinux::StartThread()
    {
        lllout << "ProcessMonitorLinux::StartThread() - called... tid: " << ACE_Thread::self() << std::endl;
        
        if (m_threadState != NotStarted)
            return;
        
        ACE_thread_t threadId;
        ACE_hthread_t threadHandle;

        const int result = ACE_Thread::spawn(ThreadFun,
                                             this,
                                             THR_NEW_LWP | THR_JOINABLE ,
                                             &threadId,
                                             &threadHandle);
        if (result != 0)
        {
            lllerr << "ProcessMonitorLinux::StartThread() - problem creating thread." << std::endl;
            return;
        }
        
        m_startEvent.wait();
        m_threadState = Running;

        lllout << "ProcessMonitorLinux::StartThread() - done..." << std::endl;
    }

    
    void
    ProcessMonitorLinux::StopThread()
    {
        lllout << "ProcessMonitorLinux::StopThread() - called... tid: " << ACE_Thread::self() << std::endl;

        if (m_threadState == Running)
        {
            m_reactor.end_reactor_event_loop();

            m_stopEvent.wait();
            m_threadState = Stopped;
        }

        lllout << "ProcessMonitorLinux::StopThread() - done..." << std::endl;
    }
    
        
        
    void
    ProcessMonitorLinux::StartMonitorPid(const pid_t pid)
    {
        lllout << "ProcessMonitorLinux::StartMonitorPid() - pid: " << pid << std::endl;
        
        // Add lock
        ACE_Guard<ACE_Thread_Mutex> lck(m_mutex);
        
        m_startWatchPids.push_back(pid);

        m_reactor.notify(this);
    }
    
    void
    ProcessMonitorLinux::StopMonitorPid(const pid_t pid)
    {
        lllout << "ProcessMonitorLinux::StopMonitorPid() - pid: " << pid << std::endl;
        
        // Add lock
        ACE_Guard<ACE_Thread_Mutex> lck(m_mutex);
        
        m_stopWatchPids.push_back(pid);

        m_reactor.notify(this);
    }


    void ProcessMonitorLinux::Run()
    {
        lllout << "ProcessMonitorLinux::Run() - called...  tid: " << ACE_Thread::self() << std::endl;
        
        m_startEvent.signal();

        m_fd = inotify_init();

        if (m_fd == -1) 
        {
            lllerr << "ProcessMonitorLinux::Run() - Problem with inotify_init()...." << std::endl;
            
            m_threadState = Stopped;
            m_stopEvent.signal();

            return;
        }

        lllout << "ProcessMonitorLinux::Run() - m_fd: " << m_fd << std::endl;

        m_reactor.owner(ACE_Thread::self());

        m_reactor.register_handler(m_fd, this,ACE_Event_Handler::READ_MASK);

        lllout << "ProcessMonitorLinux::Run() - about to call reactor... " << std::endl;

        m_reactor.run_reactor_event_loop();

        lllout << "ProcessMonitorLinux::Run() - reactor event-loop done... " << std::endl;
        
        
        for(WdMap::iterator it = m_wdMap.begin();
            it != m_wdMap.end();
            ++it)
        {
            int result = inotify_rm_watch(m_fd, (*it).first);
                    
            lllout << "ProcessMonitorLinux::Run() - inotify_rm_watch(). fd: " << m_fd 
                   << ". wd: " << (*it).first 
                   << ". result: " << result << std::endl;
                    
            if (result == -1)
            {
                lllerr << "ProcessMonitorLinux::Run() - problem with inotify_rm_watch() wd: " << (*it).first 
                       << ". errno: " << errno << std::endl;
            }
        }
        

        m_threadState = Stopped;
        m_stopEvent.signal();
    }
    
    

    //
    // Private 
    //

    //initiates the dispatch thread
    ACE_THR_FUNC_RETURN ProcessMonitorLinux::ThreadFun(void* param)
    {
        static_cast<ProcessMonitorLinux*>(param)->Run();
        return 0;
    }




}
}

#endif
