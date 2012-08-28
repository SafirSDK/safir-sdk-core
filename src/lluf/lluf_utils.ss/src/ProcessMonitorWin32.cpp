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
#ifdef _MSC_VER


#include "ProcessMonitorWin32.h"

#include <Safir/Utilities/Internal/LowLevelLogger.h>

#include <vector>

namespace Safir
{
namespace Utilities
{
    //======================================================================
    //
    // ProcessMonitorWin32Thread
    // 
    //======================================================================

    const int MAX_PID_PER_THREAD = 63;
    const int MAX_EVENTS = 64;

    
    ProcessMonitorWin32Thread::ProcessMonitorWin32Thread(const HANDLE& event)
        : m_stop(false)
        , m_prevThreadEvent(event)
    {
        m_signalEvent = CreateEvent(NULL,false,false,NULL);
    }
    
    ProcessMonitorWin32Thread::~ProcessMonitorWin32Thread() 
    {
        CloseHandle(m_signalEvent);
    }


    void
    ProcessMonitorWin32Thread::StartThread()
    {
        lllout << "ProcessMonitorWin32Thread::StartThread() - called... tid: " << boost::this_thread::get_id() << std::endl;

        if (m_thread != boost::thread())
        {
            return;
        }
        
        m_thread = boost::thread(boost::bind(&ProcessMonitorWin32Thread::Run,this));

        lllout << "ProcessMonitorWin32Thread::StartThread() - done..." << std::endl;
    }

    
    void
    ProcessMonitorWin32Thread::StopThread()
    {
        lllout << "ProcessMonitorWin32Thread::StopThread() - called... tid: " << boost::this_thread::get_id() << std::endl;

        if (m_thread != boost::thread())
        {
            m_stop = true;
            SetEvent(m_signalEvent);

            m_thread.join();
            m_thread = boost::thread();

            if(m_nextThread)
            {
                m_nextThread->StopThread();
            }
        }

        lllout << "ProcessMonitorWin32Thread::StopThread() - done..." << std::endl;
    }

    void
    ProcessMonitorWin32Thread::StartMonitorPid(const pid_t pid)
    {
        lllout << "ProcessMonitorWin32Thread::StartMonitorPid() - called... tid: " << boost::this_thread::get_id() 
               << ". pid: " << std::endl;
        boost::lock_guard<boost::mutex> lck(m_mutex);
        

        if (m_pids.size() < MAX_PID_PER_THREAD)
        {
            HANDLE hProcess = OpenProcess(PROCESS_ALL_ACCESS,//PROCESS_QUERY_INFORMATION | PROCESS_VM_READ | SYNCHRONIZE,
                                          FALSE, 
                                          (DWORD)pid );
            
            if (hProcess != NULL)
            {
                lllout << "ProcessMonitorWin32Thread::StartMonitorPid() - hProcess: " << hProcess << std::endl;
                
                m_pids.insert(std::make_pair(pid,hProcess));
                
                SetEvent(m_signalEvent);
            }
            else
            {
                lllerr << "ProcessMonitorWin32Thread::StartMonitorPid() - call to OpenProcess failed." << std::endl;
            }
        }
        else
        {
            if (m_nextThread == NULL)
            {
                lllout << "ProcessMonitorWin32Thread::StartMonitorPid() - creating new ProcessMonitorWin32Thread. tid: " 
                       << boost::this_thread::get_id() << std::endl;
                
                m_nextThread.reset(new ProcessMonitorWin32Thread(m_signalEvent));
                m_nextThread->StartThread();
            }

            lllout << "ProcessMonitorWin32Thread::StartMonitorPid() - Sending to next thread. tid: " 
                   << boost::this_thread::get_id() 
                   << ". pid: " << pid << std::endl;

            m_nextThread->StartMonitorPid(pid);
        }
    }
    
    void
    ProcessMonitorWin32Thread::StopMonitorPid(const pid_t pid)
    {
        lllout << "ProcessMonitorWin32Thread::StopMonitorPid() - called... tid: " << boost::this_thread::get_id() 
               << ". pid: " << pid << std::endl;
        boost::lock_guard<boost::mutex> lck(m_mutex);

        PidMap::iterator it = m_pids.find(pid);
        
        if (it != m_pids.end())
        {
            CloseHandle((*it).second);
            
            m_pids.erase(it);
            SetEvent(m_signalEvent);
        }
        else if(m_nextThread)
        {
            m_nextThread->StopMonitorPid(pid);
        }
    }


    void 
    ProcessMonitorWin32Thread::GetTerminatedPids(std::vector<pid_t>& pids)
    {
        boost::lock_guard<boost::mutex> lck(m_mutex);

        std::swap(m_deadPids,pids);
        
        lllout << "ProcessMonitorWin32Thread::GetTerminatedPids() - m_deadPids.size(): " << m_deadPids.size() 
               << ". pids.size(): " << pids.size() 
               << ". tid: " << boost::this_thread::get_id() << std::endl;
    }

    void ProcessMonitorWin32Thread::Run()
    {
        lllout << "ProcessMonitorWin32Thread::Run() - called...  tid: " << boost::this_thread::get_id() << std::endl;
        
        HANDLE  hEvents[MAX_EVENTS]; // array of handles for enabled events

        while(!m_stop)
        {
            hEvents[0] = m_signalEvent;
            int nrOfPids = 0;
            
            std::vector<pid_t> pids;

            {
                // Setup hEvents with all pid events....
                boost::lock_guard<boost::mutex> lck(m_mutex);

                for(PidMap::iterator it = m_pids.begin();
                    it != m_pids.end();
                    ++it)
                {
                    ++nrOfPids;
                    
                    hEvents[nrOfPids] = (*it).second;
                    pids.push_back((*it).first);
                    
                    lllout << "ProcessMonitorWin32Thread::Run() - adding pid: " << (*it).first 
                           << ". tid: " << boost::this_thread::get_id() 
                           << std::endl;
                }
            }
            
            DWORD dwRet = WaitForMultipleObjects(nrOfPids + 1,hEvents,false,INFINITE);

            lllout << "ProcessMonitorWin32Thread::Run() - dwRet: " << dwRet 
                   << ". nrOfPids: " << nrOfPids
                   << ". tid: " << boost::this_thread::get_id() 
                   << std::endl;

            if (dwRet == WAIT_FAILED)
            {
                const DWORD error = GetLastError();
                lllerr << "ProcessMonitorWin32Thread::Run() - WaitForMultipleObjects returned WAIT_FAILED!" << std::endl
                    << "   Error code = " << error << std::endl;

                LPVOID lpMsgBuf;
                DWORD dw = GetLastError(); 

                FormatMessage(
                    FORMAT_MESSAGE_ALLOCATE_BUFFER | 
                    FORMAT_MESSAGE_FROM_SYSTEM,
                    NULL,
                    dw,
                    MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                    (LPTSTR) &lpMsgBuf,
                    0, NULL );

                lllerr << "   Error message: " << (LPTSTR)lpMsgBuf << std::endl;                    

                LocalFree(lpMsgBuf);
            }
            else if (dwRet > 0)
            {
                const pid_t pid  = pids[dwRet - 1];
                
                lllout << "ProcessMonitorWin32Thread::Run() - died: " << pid 
                       << ". tid: " << boost::this_thread::get_id() 
                       << std::endl;

                boost::lock_guard<boost::mutex> lck(m_mutex);

                m_deadPids.push_back(pid);
                

                PidMap::iterator it = m_pids.find(pid);
        
                if (it != m_pids.end())
                {
                    CloseHandle((*it).second);

                    m_pids.erase(pid);
                }
                
                SetEvent(m_prevThreadEvent);
            }
            else if (dwRet == 0)
            {
                if (m_nextThread)
                {
                    m_nextThread->GetTerminatedPids(m_deadPids);
                }

                lllout << "ProcessMonitorWin32Thread::Run() - m_deadPids.size(): " << m_deadPids.size() 
                       << ". tid: " << boost::this_thread::get_id() 
                       << std::endl;
                
                if (m_deadPids.size() > 0)
                {
                    SetEvent(m_prevThreadEvent);
                }
            }
        }
        

        lllout << "ProcessMonitorWin32Thread::Run() - about to stop...  tid: " << boost::this_thread::get_id() << std::endl;
    }

    
    //======================================================================
    //
    // ProcessMonitorWin32
    // 
    //======================================================================
    
    ProcessMonitorWin32::ProcessMonitorWin32(const ProcessMonitor::OnTerminateCb& callback)
        : m_callback(callback)
        , m_stop(false)
    {
        m_signalEvent = CreateEvent(NULL,false,false,NULL);
    }
    
    ProcessMonitorWin32::~ProcessMonitorWin32() 
    {
        CloseHandle(m_signalEvent);
    }

    void
    ProcessMonitorWin32::StartThread()
    {
        lllout << "ProcessMonitorWin32::StartThread() - called... tid: " << boost::this_thread::get_id() << std::endl;

        if (m_thread != boost::thread())
        {
            return;
        }
        
        m_thread = boost::thread(boost::bind(&ProcessMonitorWin32::Run,this));

        lllout << "ProcessMonitorWin32::StartThread() - done..." << std::endl;
    }
    
    void
    ProcessMonitorWin32::StopThread()
    {
        lllout << "ProcessMonitorWin32::StopThread() - called... tid: " << boost::this_thread::get_id() << std::endl;

        if (m_thread != boost::thread())
        {
            m_stop = true;
            SetEvent(m_signalEvent);

            m_thread.join();
            m_thread = boost::thread();

            if(m_nextThread)
            {
                m_nextThread->StopThread();
            }
        }

        lllout << "ProcessMonitorWin32::StopThread() - done..." << std::endl;
    }
    
        
    void
    ProcessMonitorWin32::StartMonitorPid(const pid_t pid)
    {
        lllout << "ProcessMonitorWin32::StartMonitorPid() - called... tid: " << boost::this_thread::get_id() 
               << ". pid: " << std::endl;
        boost::lock_guard<boost::mutex> lck(m_mutex);
        
        PidSet::iterator it = m_pids.find(pid);
        
        if (it == m_pids.end())
        {
            m_pids.insert(pid);

            if (m_nextThread == NULL)
            {
                // Create a "child" thread which will handle the monitor of the pid.
                m_nextThread.reset(new ProcessMonitorWin32Thread(m_signalEvent));
                m_nextThread->StartThread();
            }
            
            m_nextThread->StartMonitorPid(pid);
        }
    }


    void
    ProcessMonitorWin32::StopMonitorPid(const pid_t pid)
    {
        lllout << "ProcessMonitorWin32::StopMonitorPid() - called... tid: " << boost::this_thread::get_id() 
               << ". pid: " << pid << std::endl;
        boost::lock_guard<boost::mutex> lck(m_mutex);

        PidSet::iterator it = m_pids.find(pid);
        
        if (it != m_pids.end())
        {
            m_pids.erase(it);

            m_nextThread->StopMonitorPid(pid);
        }
    }

    void ProcessMonitorWin32::Run()
    {
        lllout << "ProcessMonitorWin32::Run() - called...  tid: " << boost::this_thread::get_id() << std::endl;
        
        HANDLE  hEvents[MAX_EVENTS]; // array of handles for enabled events
        
        while(!m_stop)
        {
            hEvents[0] = m_signalEvent;
            
            DWORD dwRet = WaitForMultipleObjects(1,hEvents,false,INFINITE);

            lllout << "ProcessMonitorWin32::Run() - dwRet: " << dwRet << std::endl;

            if (dwRet == 0)
            {
                std::vector<pid_t> deadPids;
                
                if (m_nextThread)
                {
                    m_nextThread->GetTerminatedPids(deadPids);
                }
                
                for(std::vector<pid_t>::iterator it = deadPids.begin();
                    it != deadPids.end();
                    ++it)
                {
                    boost::lock_guard<boost::mutex> lck(m_mutex);
                    m_pids.erase((*it));

                    lllout << "ProcessMonitorWin32::Run() - died: " << (*it) << std::endl;
                    m_callback((*it));
                }
            }
        }
        
        lllout << "ProcessMonitorWin32::Run() - about to stop...  tid: " << boost::this_thread::get_id() << std::endl;
    }
}
}

#endif
