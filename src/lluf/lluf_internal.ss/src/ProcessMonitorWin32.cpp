/******************************************************************************
 *
 * Copyright Saab AB, 2007-2013 (http://safirsdkcore.com)
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
#include <Safir/Utilities/Internal/AsioStrandWrap.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>

#include <tlhelp32.h>

namespace Safir
{
    namespace Utilities
    {
        ProcessMonitorImpl::ProcessMonitorImpl(boost::asio::io_context &io,
                                               const std::function<void(const pid_t pid)> &callback,
                                               const std::chrono::steady_clock::duration & pollPeriod)
            : m_callback(callback)
            , m_io(io)
            , m_stopped(false)
            , m_strand(io)
            , m_pollPeriod(pollPeriod)
        {
        }

        void ProcessMonitorImpl::Stop()
        {
            const bool was_stopped = m_stopped.exchange(true);
            if (!was_stopped)
            {
                boost::asio::dispatch(m_strand, [this]
                                      { StopInternal(); });
            }
        }

        void ProcessMonitorImpl::StopInternal()
        {
            for (ProcessTable::iterator it = m_processes.begin();
                 it != m_processes.end(); ++it)
            {
                if (it->second->pollTimer)
                {
                    it->second->pollTimer->cancel();
                }
                else
                {
                    it->second->handle.cancel();
                }
            }
        }

        void ProcessMonitorImpl::StartMonitorPidInternal(const pid_t pid)
        {
            if (m_processes.find(pid) != m_processes.end())
            {
                // we're already watching this pid.
                return;
            }

            HANDLE process = GetProcessHandle(pid);
            if (process != NULL)
            {
                // normal case
                std::shared_ptr<Process> proc(new Process(m_io, process, pid));
                m_processes.insert(std::make_pair(pid, proc));
                proc->handle.async_wait(Safir::Utilities::Internal::WrapInStrand(m_strand, [this, proc](const boost::system::error_code &error)
                                                                                 { HandleEvent(proc, error); }));
            }
            else if (ProcessExists((DWORD)pid))
            {
                // Very rare, for example when Sate is running as a different user and tries to connect.
                lllog(5) << L"ProcessMonitor - Could not get HANDLE for process with pid=" << pid << L". However process semms to be running. Will fallback on monitoring by polling." << std::endl;
                std::shared_ptr<Process> proc(new Process(m_io, process, pid));
                proc->pollTimer = std::make_unique<boost::asio::steady_timer>(m_io);
                m_processes.insert(std::make_pair(pid, proc));
                proc->pollTimer->expires_after(m_pollPeriod);
                proc->pollTimer->async_wait(
                    boost::asio::bind_executor(m_strand, [this, proc](const boost::system::error_code &error)
                                               { Poll(proc, error); }));
                return;
            }
            else
            {
                // if we can't get a handle to the process we assume that it is dead.
                DWORD error = ::GetLastError();
                std::string message = std::system_category().message(error);
                lllog(5) << L"ProcessMonitor - Could do OpenProcess with pid=" << pid << L", GetLastError=" << error << L" - " << message.c_str() << std::endl;
                boost::asio::post(m_io, [this, pid]
                                  { m_callback(pid); });
                return;
            }
        }

        HANDLE ProcessMonitorImpl::GetProcessHandle(const pid_t pid) const
        {
            // A quick an dirty way to be able to unit test the polling version.
#ifdef PROCMONTEST_NO_OPENPROCESS
            (void)pid; //unused
            lllog(9) << L"WARNING ProcessMonitorWin32 is running in test mode." << std::endl;
            return NULL;
#else
            return OpenProcess(PROCESS_ALL_ACCESS, FALSE, (DWORD)pid);
#endif

        }

        // This method searches for a pid among all running processes. Used as a fall back method if
        // OpenProcess is not allowed.
        bool ProcessMonitorImpl::ProcessExists(const pid_t pid) const
        {
            bool exists = false;
            PROCESSENTRY32 entry;
            entry.dwSize = sizeof(PROCESSENTRY32);

            HANDLE snapshot = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, NULL);

            if (Process32First(snapshot, &entry))
            {
                while (Process32Next(snapshot, &entry))
                {
                    if (entry.th32ProcessID == (DWORD)pid)
                    {
                        exists = true;
                        break;
                    }
                }
            }

            CloseHandle(snapshot);
            return exists;
        }

        void ProcessMonitorImpl::Poll(std::shared_ptr<Process> proc, const boost::system::error_code &error)
        {
            if (error || m_stopped)
            {
                StopMonitorPidInternal(proc->pid);
                return;
            }
            
            bool isAlive = ProcessExists(proc->pid);
            if (isAlive)
            {
                // Still alive, restart poll timer
                proc->pollTimer->expires_after(m_pollPeriod);
                proc->pollTimer->async_wait(
                    boost::asio::bind_executor(m_strand, [this, proc](const boost::system::error_code &error)
                                               { Poll(proc, error); }));

            }
            else
            {
                // Process is dead
                m_processes.erase(proc->pid);
                boost::asio::post(m_io, [this, pid = proc->pid]
                                  { m_callback(pid); });
            }
        }

        void ProcessMonitorImpl::StopMonitorPidInternal(const pid_t pid)
        {
            ProcessTable::iterator findIt = m_processes.find(pid);
            if (findIt == m_processes.end())
            {
                // not monitoring this pid!
                return;
            }

            if (findIt->second->pollTimer)
            {
                findIt->second->pollTimer->cancel();
            }
            else
            {
                findIt->second->handle.cancel();
            }

            m_processes.erase(findIt);
            lllog(5) << L"ProcessMonitor - Removed monitoring of pid: " << pid << std::endl;
        }

        void ProcessMonitorImpl::HandleEvent(const std::shared_ptr<Process> &process,
                                             const boost::system::error_code &error)
        {
            if (error || m_stopped)
            {
                lllog(5) << L"ProcessMonitor (" << process->pid << L") - HandleEvent got error: " << error.message().c_str() << std::endl;
                return;
            }

            lllog(5) << L"ProcessMonitor (" << process->pid << L") - HandleEvent process dead." << std::endl;
            m_processes.erase(process->pid);
            boost::asio::post(m_io, [this, pid = process->pid]
                              { m_callback(pid); });
        }
    }
}

#endif
