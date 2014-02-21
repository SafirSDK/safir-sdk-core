/******************************************************************************
*
* Copyright Saab AB, 2011-2013 (http://safir.sourceforge.net)
*
* Created by: Anders Widén / aiwi
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

#include "dose_main_thread_monitor.h"

#include <Safir/Dob/NodeParameters.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Utilities/Internal/StringEncoding.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    using Safir::Utilities::Internal::ToUtf16;

    ThreadMonitor::ThreadMonitor()
        : m_watchdogs(),
          m_lock(),
          m_checkerThread()
    {
        m_checkerThread = boost::thread(&ThreadMonitor::Check, this);
    }

    ThreadMonitor::~ThreadMonitor()
    {
        m_checkerThread.interrupt();
        m_checkerThread.join();
    }

    void ThreadMonitor::StartWatchdog(const boost::thread::id& threadId,
                                      const std::wstring& threadName)
    {
        boost::lock_guard<boost::mutex> lock(m_lock);

        m_watchdogs.insert(std::make_pair(threadId, WatchdogInfo(threadName)));

    }

    void ThreadMonitor::StopWatchdog(const boost::thread::id& threadId)
    {
        boost::lock_guard<boost::mutex> lock(m_lock);

        m_watchdogs.erase(threadId);
    }

    void ThreadMonitor::KickWatchdog(const boost::thread::id& threadId)
    {
        boost::lock_guard<boost::mutex> lock(m_lock);

        WatchdogMap::iterator it = m_watchdogs.find(threadId);

        ENSURE(it != m_watchdogs.end(), << "Thread id" << threadId <<
               " hasn't called StartWatchdog!");

        ++it->second.counter;
    }

    void ThreadMonitor::Check()
    {
        try
        {
            const boost::chrono::seconds watchdogTimeout(Safir::Dob::NodeParameters::DoseMainThreadWatchdogTimeout());

            for (;;)
            {
                boost::this_thread::sleep_for(watchdogTimeout/4);

                const boost::chrono::steady_clock::time_point now = boost::chrono::steady_clock::now();

                {
                    boost::lock_guard<boost::mutex> lock(m_lock);

                    for (WatchdogMap::iterator it = m_watchdogs.begin(); it != m_watchdogs.end(); ++it)
                    {
                        if (it->second.counter != it->second.lastCheckedCounterVal)
                        {
                            // The counter has been kicked
                            it->second.lastTimeAlive = now;
                            it->second.lastCheckedCounterVal = it->second.counter;
                            it->second.errorLogIsGenerated = false;
                        }
                        else
                        {
                            // The counter hasn't been kicked ...

                            const boost::chrono::steady_clock::duration timeSinceLastKick = now - it->second.lastTimeAlive;

                            // Check if the duration since last kick is "abnormal", thus indicating some sort of external
                            // manipulation of the clock. For instance, a very long duration could be caused by the cpu going
                            // into sleep/hibernate mode.
                            if (timeSinceLastKick < boost::chrono::seconds(0) || timeSinceLastKick > watchdogTimeout * 2)
                            {
                                it->second.lastTimeAlive = now;
                                continue;
                            }
                            
                            if (timeSinceLastKick > watchdogTimeout)
                            {
                                // ... and this thread has been hanging for so long time now
                                // that we actually will kill dose_main itself!!
                                if (Safir::Dob::NodeParameters::TerminateDoseMainWhenUnrecoverableError())
                                {
                                    SEND_SYSTEM_LOG(Alert,
                                                    << it->second.threadName << " (tid " << it->first
                                                    << ") has been hanging for at least "
                                                    << timeSinceLastKick
                                                    << ". Safir.Dob.NodeParameters.TerminateDoseMainWhenUnrecoverableError is true"
                                                    << " so dose_main will be terminated!!");

                                    boost::this_thread::sleep_for(boost::chrono::seconds(5));

                                    exit(1); // Terminate dose_main!!!!
                                }
                                else if (!it->second.errorLogIsGenerated)
                                {
                                    SEND_SYSTEM_LOG(Alert,
                                                    << it->second.threadName << " (tid " << it->first
                                                    << ") has been hanging for at least "
                                                    << timeSinceLastKick
                                                    << ". Safir.Dob.NodeParameters.TerminateDoseMainWhenUnrecoverableError is true"
                                                    << " so dose_main will not be terminated!!");

                                    it->second.errorLogIsGenerated = true;
                                }                           
                            }
                        }
                    }
                }  // lock released here
            }
        }
        catch (boost::thread_interrupted&)
        {
            // Thread was interrupted, which is expected behaviour. By catching this exception we make
            // sure that the whole program is not aborted, which could otherwise be the case on some platforms
            // where an unhandled exception in a thread brings down the whole program.
        }
    }
}
}
}
