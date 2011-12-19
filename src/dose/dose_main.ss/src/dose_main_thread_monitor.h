/******************************************************************************
*
* Copyright Saab AB, 2011 (http://www.safirsdk.com)
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
#include <boost/thread.hpp>

#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable : 4127)
#endif
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
    class ThreadMonitor
    {
    public:

        ThreadMonitor();
        ~ThreadMonitor();

        void StartWatchdog(const boost::thread::id& threadId,
                           const std::string& threadName);

        void StopWatchdog(const boost::thread::id& threadId);

        void KickWatchdog(const boost::thread::id& threadId);

    private:

        void Check();

        struct WatchdogInfo
        {
            explicit WatchdogInfo(const std::string& _threadName) :
                        threadName(_threadName),
                        counter(0),
                        lastCheckedCounterVal(0),
                        lastTimeAlive(boost::posix_time::second_clock::universal_time()) {};

            std::string                 threadName;
            unsigned int                counter;
            unsigned int                lastCheckedCounterVal;
            boost::posix_time::ptime    lastTimeAlive;
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
