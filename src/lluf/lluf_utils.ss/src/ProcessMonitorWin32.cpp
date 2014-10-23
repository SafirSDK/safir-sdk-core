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
#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)

#include "ProcessMonitorWin32.h"
#if 0
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>

#include <vector>
#endif
namespace Safir
{
namespace Utilities
{
#if 0
    //======================================================================
    //
    // ProcessMonitorImplThread
    //
    //======================================================================

    const int MAX_PID_PER_THREAD = 63;
    const int MAX_EVENTS = 64;


    ProcessMonitorImplThread::ProcessMonitorImplThread(const HANDLE& event)
        : m_stop(false)
        , m_prevThreadEvent(event)
    {
        m_signalEvent = CreateEvent(NULL,false,false,NULL);
    }

    ProcessMonitorImplThread::~ProcessMonitorImplThread()
    {
        CloseHandle(m_signalEvent);
    }


    void
    ProcessMonitorImplThread::StartThread()
    {
        lllout << "ProcessMonitorImplThread::StartThread() - called... tid: " << boost::this_thread::get_id() << std::endl;

        if (m_thread != boost::thread())
        {
            return;
        }

        m_thread = boost::thread(boost::bind(&ProcessMonitorImplThread::Run,this));

        lllout << "ProcessMonitorImplThread::StartThread() - done..." << std::endl;
    }


    void
    ProcessMonitorImplThread::StopThread()
    {
        lllout << "ProcessMonitorImplThread::StopThread() - called... tid: " << boost::this_thread::get_id() << std::endl;

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

        lllout << "ProcessMonitorImplThread::StopThread() - done..." << std::endl;
    }

    void
    ProcessMonitorImplThread::StartMonitorPid(const pid_t pid)
    {
        lllout << "ProcessMonitorImplThread::StartMonitorPid() - called... tid: " << boost::this_thread::get_id()
               << ". pid: " << std::endl;
        boost::lock_guard<boost::mutex> lck(m_mutex);


        if (m_pids.size() < MAX_PID_PER_THREAD)
        {
            HANDLE hProcess = OpenProcess(PROCESS_ALL_ACCESS,//PROCESS_QUERY_INFORMATION | PROCESS_VM_READ | SYNCHRONIZE,
                                          FALSE,
                                          (DWORD)pid );

            if (hProcess != NULL)
            {
                lllout << "ProcessMonitorImplThread::StartMonitorPid() - hProcess: " << hProcess << std::endl;

                m_pids.insert(std::make_pair(pid,hProcess));

                SetEvent(m_signalEvent);
            }
            else
            {
                SEND_SYSTEM_LOG(Critical,
                                << "ProcessMonitorImplThread::StartMonitorPid() - call to OpenProcess failed.");
            }
        }
        else
        {
            if (m_nextThread == NULL)
            {
                lllout << "ProcessMonitorImplThread::StartMonitorPid() - creating new ProcessMonitorImplThread. tid: "
                       << boost::this_thread::get_id() << std::endl;

                m_nextThread.reset(new ProcessMonitorImplThread(m_signalEvent));
                m_nextThread->StartThread();
            }

            lllout << "ProcessMonitorImplThread::StartMonitorPid() - Sending to next thread. tid: "
                   << boost::this_thread::get_id()
                   << ". pid: " << pid << std::endl;

            m_nextThread->StartMonitorPid(pid);
        }
    }

    void
    ProcessMonitorImplThread::StopMonitorPid(const pid_t pid)
    {
        lllout << "ProcessMonitorImplThread::StopMonitorPid() - called... tid: " << boost::this_thread::get_id()
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
    ProcessMonitorImplThread::GetTerminatedPids(std::vector<pid_t>& pids)
    {
        boost::lock_guard<boost::mutex> lck(m_mutex);

        std::swap(m_deadPids,pids);

        lllout << "ProcessMonitorImplThread::GetTerminatedPids() - m_deadPids.size(): " << m_deadPids.size()
               << ". pids.size(): " << pids.size()
               << ". tid: " << boost::this_thread::get_id() << std::endl;
    }

    void ProcessMonitorImplThread::Run()
    {
        lllout << "ProcessMonitorImplThread::Run() - called...  tid: " << boost::this_thread::get_id() << std::endl;

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

                    lllout << "ProcessMonitorImplThread::Run() - adding pid: " << (*it).first
                           << ". tid: " << boost::this_thread::get_id()
                           << std::endl;
                }
            }

            DWORD dwRet = WaitForMultipleObjects(nrOfPids + 1,hEvents,false,INFINITE);

            lllout << "ProcessMonitorImplThread::Run() - dwRet: " << dwRet
                   << ". nrOfPids: " << nrOfPids
                   << ". tid: " << boost::this_thread::get_id()
                   << std::endl;

            if (dwRet == WAIT_FAILED)
            {
                const DWORD error = GetLastError();
                SEND_SYSTEM_LOG(Critical,
                                << "ProcessMonitorImplThread::Run() - WaitForMultipleObjects returned WAIT_FAILED!"
                                << " Error code = " << error);

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

                SEND_SYSTEM_LOG(Critical,
                                << " - Error message: " << (LPTSTR)lpMsgBuf);

                LocalFree(lpMsgBuf);
            }
            else if (dwRet > 0)
            {
                const pid_t pid  = pids[dwRet - 1];

                lllout << "ProcessMonitorImplThread::Run() - died: " << pid
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

                lllout << "ProcessMonitorImplThread::Run() - m_deadPids.size(): " << m_deadPids.size()
                       << ". tid: " << boost::this_thread::get_id()
                       << std::endl;

                if (m_deadPids.size() > 0)
                {
                    SetEvent(m_prevThreadEvent);
                }
            }
        }


        lllout << "ProcessMonitorImplThread::Run() - about to stop...  tid: " << boost::this_thread::get_id() << std::endl;
    }

#endif

    //======================================================================
    //
    // ProcessMonitorImpl
    //
    //======================================================================

    ProcessMonitorImpl::ProcessMonitorImpl(boost::asio::io_service& ioService,
                                             const boost::function<void(const pid_t pid)>& callback,
                                             const boost::chrono::steady_clock::duration& /*pollPeriod*/)
        : m_callback(callback)
        , m_ioService(ioService)
        , m_strand(ioService)
    {
        //m_signalEvent = CreateEvent(NULL,false,false,NULL);
    }

    void ProcessMonitorImpl::Stop()
    {
        for(ProcessTable::iterator it = m_processes.begin();
            it != m_processes.end(); ++it)
        {
            it->second->handle.cancel();
        }
    }

#if 0
    ProcessMonitorImpl::~ProcessMonitorImpl()
    {
        CloseHandle(m_signalEvent);
    }

    void
    ProcessMonitorImpl::StartThread()
    {
        lllout << "ProcessMonitorImpl::StartThread() - called... tid: " << boost::this_thread::get_id() << std::endl;

        if (m_thread != boost::thread())
        {
            return;
        }

        m_thread = boost::thread(boost::bind(&ProcessMonitorImpl::Run,this));

        lllout << "ProcessMonitorImpl::StartThread() - done..." << std::endl;
    }

    void
    ProcessMonitorImpl::StopThread()
    {
        lllout << "ProcessMonitorImpl::StopThread() - called... tid: " << boost::this_thread::get_id() << std::endl;

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

        lllout << "ProcessMonitorImpl::StopThread() - done..." << std::endl;
    }
#endif

    void
    ProcessMonitorImpl::StartMonitorPidInternal(const pid_t pid)
    {
        if (m_processes.find(pid) != m_processes.end())
        {
            //we're already watching this pid.
            return;
        }

        HANDLE process = OpenProcess(PROCESS_ALL_ACCESS,//PROCESS_QUERY_INFORMATION | PROCESS_VM_READ | SYNCHRONIZE,
                                     FALSE,
                                     (DWORD)pid);

        //if we can't get a handle to the process we assume that it is dead.
        if (process == NULL)
        {
            m_ioService.post(boost::bind(m_callback,pid));
            return;
        }

        boost::shared_ptr<Process> proc(new Process(m_ioService, process, pid));
        m_processes.insert(std::make_pair(pid, proc));
        proc->handle.async_wait(m_strand.wrap(boost::bind(&ProcessMonitorImpl::HandleEvent, this, proc, _1)));

    }


    void
    ProcessMonitorImpl::StopMonitorPidInternal(const pid_t pid)
    {
        ProcessTable::iterator findIt = m_processes.find(pid);
        if (findIt == m_processes.end())
        {
            //not monitoring this pid!
            return;
        }

        findIt->second->handle.cancel();
        m_processes.erase(findIt);
    }


    void ProcessMonitorImpl::HandleEvent(const boost::shared_ptr<Process>& process,
                                         const boost::system::error_code& error)
    {
        if (error)
        {
            return;
        }

        m_processes.erase(process->pid);
        m_ioService.post(boost::bind(m_callback,process->pid));
    }

#if 0
    void ProcessMonitorImpl::Run()
    {
        lllout << "ProcessMonitorImpl::Run() - called...  tid: " << boost::this_thread::get_id() << std::endl;

        HANDLE  hEvents[MAX_EVENTS]; // array of handles for enabled events

        while(!m_stop)
        {
            hEvents[0] = m_signalEvent;

            DWORD dwRet = WaitForMultipleObjects(1,hEvents,false,INFINITE);

            lllout << "ProcessMonitorImpl::Run() - dwRet: " << dwRet << std::endl;

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

                    lllout << "ProcessMonitorImpl::Run() - died: " << (*it) << std::endl;
                    m_callback((*it));
                }
            }
        }

        lllout << "ProcessMonitorImpl::Run() - about to stop...  tid: " << boost::this_thread::get_id() << std::endl;
    }
#endif
}
}

#endif
