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
        , m_threadState(NotStarted)
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
        lllout << "ProcessMonitorWin32Thread::StartThread() - called... tid: " << ACE_Thread::self() << std::endl;
        
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
            lllerr << "ProcessMonitorWin32Thread::StartThread() - problem creating thread." << std::endl;
            return;
        }
        
        m_startEvent.wait();
        m_threadState = Running;

        lllout << "ProcessMonitorWin32Thread::StartThread() - done..." << std::endl;
    }

    
    void
    ProcessMonitorWin32Thread::StopThread()
    {
        lllout << "ProcessMonitorWin32Thread::StopThread() - called... tid: " << ACE_Thread::self() << std::endl;

        if (m_threadState == Running)
        {
            m_stop = true;

            SetEvent(m_signalEvent);

            m_stopEvent.wait();
            m_threadState = Stopped;

            if (m_nextThread)
            {
                m_nextThread->StopThread();
            }
        }

        lllout << "ProcessMonitorWin32Thread::StopThread() - done..." << std::endl;
    }

        
    void
    ProcessMonitorWin32Thread::StartMonitorPid(const pid_t pid)
    {
        lllout << "ProcessMonitorWin32Thread::StartMonitorPid() - called... tid: " << ACE_Thread::self() 
               << ". pid: " << std::endl;
        ACE_Guard<ACE_Thread_Mutex> lck(m_mutex);
        

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
                       << ACE_Thread::self() << std::endl;
                
                m_nextThread.reset(new ProcessMonitorWin32Thread(m_signalEvent));
                m_nextThread->StartThread();
            }

            lllout << "ProcessMonitorWin32Thread::StartMonitorPid() - Sending to next thread. tid: " 
                   << ACE_Thread::self() 
                   << ". pid: " << pid << std::endl;

            m_nextThread->StartMonitorPid(pid);
        }
    }
    
    void
    ProcessMonitorWin32Thread::StopMonitorPid(const pid_t pid)
    {
        lllout << "ProcessMonitorWin32Thread::StopMonitorPid() - called... tid: " << ACE_Thread::self() 
               << ". pid: " << pid << std::endl;
        ACE_Guard<ACE_Thread_Mutex> lck(m_mutex);

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
        ACE_Guard<ACE_Thread_Mutex> lck(m_mutex);

        std::copy(m_deadPids.begin(),
                  m_deadPids.end(),
                  std::back_inserter(pids));
        
        lllout << "ProcessMonitorWin32Thread::GetTerminatedPids() - m_deadPids.size(): " << m_deadPids.size() 
               << ". pids.size(): " << pids.size() 
               << ". tid: " << ACE_Thread::self() << std::endl;

        m_deadPids.clear();
    }


    void ProcessMonitorWin32Thread::Run()
    {
        lllout << "ProcessMonitorWin32Thread::Run() - called...  tid: " << ACE_Thread::self() << std::endl;
        
        m_startEvent.signal();

        HANDLE  hEvents[MAX_EVENTS]; // array of handles for enabled events
        

        while(!m_stop)
        {
            hEvents[0] = m_signalEvent;
            int nrOfPids = 0;
            
            std::vector<pid_t> pids;

            {
                // Setup hEvents with all pid events....
                ACE_Guard<ACE_Thread_Mutex> lck(m_mutex);

                for(PidMap::iterator it = m_pids.begin();
                    it != m_pids.end();
                    ++it)
                {
                    ++nrOfPids;
                    
                    hEvents[nrOfPids] = (*it).second;
                    pids.push_back((*it).first);
                    
                    lllout << "ProcessMonitorWin32Thread::Run() - adding pid: " << (*it).first 
                           << ". tid: " << ACE_Thread::self() 
                           << std::endl;
                }
            }
            
            DWORD dwRet = WaitForMultipleObjects(nrOfPids + 1,hEvents,false,INFINITE);

            lllout << "ProcessMonitorWin32Thread::Run() - dwRet: " << dwRet 
                   << ". nrOfPids: " << nrOfPids
                   << ". tid: " << ACE_Thread::self() 
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
                pid_t pid  = pids[dwRet - 1];
                
                lllout << "ProcessMonitorWin32Thread::Run() - died: " << pid 
                       << ". tid: " << ACE_Thread::self() 
                       << std::endl;

                ACE_Guard<ACE_Thread_Mutex> lck(m_mutex);

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
                       << ". tid: " << ACE_Thread::self() 
                       << std::endl;
                
                if (m_deadPids.size() > 0)
                {
                    SetEvent(m_prevThreadEvent);
                }
            }
        }
        

        lllout << "ProcessMonitorWin32Thread::Run() - about to stop...  tid: " << ACE_Thread::self() << std::endl;

        m_threadState = Stopped;
        m_stopEvent.signal();
    }


    //
    // Private 
    //

    //initiates the dispatch thread
    ACE_THR_FUNC_RETURN ProcessMonitorWin32Thread::ThreadFun(void* param)
    {
        static_cast<ProcessMonitorWin32Thread*>(param)->Run();
        return 0;
    }


    
    //======================================================================
    //
    // ProcessMonitorWin32
    // 
    //======================================================================
    
    ProcessMonitorWin32::ProcessMonitorWin32(const ProcessMonitor::OnTerminateCb& callback)
        : m_callback(callback)
        , m_stop(false)
        , m_threadState(NotStarted)
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
        lllout << "ProcessMonitorWin32::StartThread() - called... tid: " << ACE_Thread::self() << std::endl;
        
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
            lllerr << "ProcessMonitorWin32::StartThread() - problem creating thread." << std::endl;
            return;
        }
        
        m_startEvent.wait();
        m_threadState = Running;

        lllout << "ProcessMonitorWin32::StartThread() - done..." << std::endl;
    }
    
    void
    ProcessMonitorWin32::StopThread()
    {
        lllout << "ProcessMonitorWin32::StopThread() - called... tid: " << ACE_Thread::self() << std::endl;

        if (m_threadState == Running)
        {
            m_stop = true;

            SetEvent(m_signalEvent);

            m_stopEvent.wait();
            m_threadState = Stopped;


            // Stop child.
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
        lllout << "ProcessMonitorWin32::StartMonitorPid() - called... tid: " << ACE_Thread::self() 
               << ". pid: " << std::endl;
        ACE_Guard<ACE_Thread_Mutex> lck(m_mutex);
        
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
        lllout << "ProcessMonitorWin32::StopMonitorPid() - called... tid: " << ACE_Thread::self() 
               << ". pid: " << pid << std::endl;
        ACE_Guard<ACE_Thread_Mutex> lck(m_mutex);

        PidSet::iterator it = m_pids.find(pid);
        
        if (it != m_pids.end())
        {
            m_pids.erase(it);

            m_nextThread->StopMonitorPid(pid);
        }
    }

    void ProcessMonitorWin32::Run()
    {
        lllout << "ProcessMonitorWin32::Run() - called...  tid: " << ACE_Thread::self() << std::endl;
        
        m_startEvent.signal();

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
                    ACE_Guard<ACE_Thread_Mutex> lck(m_mutex);
                    m_pids.erase((*it));

                    lllout << "ProcessMonitorWin32::Run() - died: " << (*it) << std::endl;
                    m_callback((*it));
                }
            }
        }
        
        lllout << "ProcessMonitorWin32::Run() - about to stop...  tid: " << ACE_Thread::self() << std::endl;

        m_threadState = Stopped;
        m_stopEvent.signal();
    }

    //
    // Private 
    //

    //initiates the dispatch thread
    ACE_THR_FUNC_RETURN ProcessMonitorWin32::ThreadFun(void* param)
    {
        static_cast<ProcessMonitorWin32*>(param)->Run();
        return 0;
    }
}
}

#endif
