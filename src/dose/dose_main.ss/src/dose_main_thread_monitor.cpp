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

#include "dose_main_thread_monitor.h"

#include <Safir/Dob/NodeParameters.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/PanicLogging.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
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
                                      const std::string& threadName)
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
            for (;;)
            {
                boost::this_thread::sleep(boost::posix_time::seconds(7));

                const boost::posix_time::ptime now = boost::posix_time::second_clock::universal_time();

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
                            if (now - it->second.lastTimeAlive >
                                boost::posix_time::seconds(Safir::Dob::NodeParameters::DoseMainThreadWatchdogTimeout()))
                            {
                                // ... and this thread has been hanging for so long time now
                                // that we actually will kill dose_main itself!!
                                std::ostringstream ostr;
                                ostr << it->second.threadName << " (tid " << it->first
                                    << ") seems to have been hanging for at least "
                                    << boost::posix_time::to_simple_string(now - it->second.lastTimeAlive) << '\n';
                                if (Safir::Dob::NodeParameters::TerminateDoseMainWhenUnrecoverableError())
                                {
                                    ostr << "Parameter TerminateDoseMainWhenUnrecoverableError is set to true"
                                        " which means that dose_main will now be terminated!!" << std::endl;
                                    lllerr << ostr.str().c_str();
                                    Safir::Utilities::Internal::PanicLogging::Log(ostr.str());

                                    boost::this_thread::sleep(boost::get_system_time() + boost::posix_time::seconds(5));

                                    exit(1); // Terminate dose_main!!!!
                                }
                                else if (!it->second.errorLogIsGenerated)
                                {
                                    ostr << "Parameter TerminateDoseMainWhenUnrecoverableError is set to false"
                                        " which means that dose_main will not be terminated!!" << std::endl;
                                    lllerr << ostr.str().c_str();
                                    Safir::Utilities::Internal::PanicLogging::Log(ostr.str());
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
