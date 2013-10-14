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

#ifndef _dose_main_thread_monitor_h
#define _dose_main_thread_monitor_h

#include <Safir/Dob/Internal/Atomic.h>

#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable : 4244 4127)
#endif
#include <boost/thread.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#if defined _MSC_VER
  #pragma warning (pop)
#endif

namespace Safir
{
namespace Dob
{
namespace Internal
{
    // Monitors threads by checking that they kick their watchdog at regular intervals.
    //
    // This is an active object that contains its own thread. The thread will
    // be started by the constructor and it will be stopped by the destructor.
    class ThreadMonitor
    {
    public:

        // Constructor starts monitoring thread
        ThreadMonitor();

        // Destructor waits for the monitoring thread to be stopped
        // before returning.
        ~ThreadMonitor();

        void StartWatchdog(const boost::thread::id& threadId,
                           const std::wstring& threadName);

        void StopWatchdog(const boost::thread::id& threadId);

        void KickWatchdog(const boost::thread::id& threadId);

    private:

        void Check();

        struct WatchdogInfo
        {
            explicit WatchdogInfo(const std::wstring& _threadName) :
                        threadName(_threadName),
                        counter(0),
                        lastCheckedCounterVal(0),
                        lastTimeAlive(boost::posix_time::second_clock::universal_time()),
                        errorLogIsGenerated(false) {};

            std::wstring                threadName;
            unsigned int                counter;
            unsigned int                lastCheckedCounterVal;
            boost::posix_time::ptime    lastTimeAlive;
            bool                        errorLogIsGenerated;
        };

        typedef std::map<boost::thread::id, WatchdogInfo> WatchdogMap;

        WatchdogMap m_watchdogs;

        boost::mutex m_lock;

        boost::thread m_checkerThread;
    };
}
}
}

#endif
