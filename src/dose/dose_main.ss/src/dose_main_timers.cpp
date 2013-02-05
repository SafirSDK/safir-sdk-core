/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
*
* Created by: Anders Widén / stawi
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

#include "dose_main_timers.h"
#include <cmath>
#include <iostream>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <boost/bind.hpp>
#include <boost/date_time/posix_time/posix_time_types.hpp>

#ifdef _MSC_VER

#  pragma comment (lib, "winmm.lib")
#  pragma warning (push)
#  pragma warning (disable: 4201)
#  include <MMSystem.h>
#  pragma warning (pop)

namespace
{
    // Windows stuff to enable better timer resolution on windows plattforms.
    void enableWindowsMultimediaTimers()
    {
        // Set best possible timer resolution on windows
        
        TIMECAPS    tc;
        UINT        wTimerRes;
        const UINT  wantedResolution = 1;  // ms

        if (timeGetDevCaps(&tc, sizeof(TIMECAPS)) != TIMERR_NOERROR)
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Dose_Main: Failed to obtain timer resolution!", __WFILE__, __LINE__);
        }

        wTimerRes = static_cast<UINT>(std::min(std::max(tc.wPeriodMin, wantedResolution), tc.wPeriodMax));
        timeBeginPeriod(wTimerRes);
    }
}
#endif


namespace Safir
{
namespace Dob
{
namespace Internal
{
    //static member initialization
    TimerHandler* TimerHandler::m_instance = NULL;

    static const boost::posix_time::ptime epoch(boost::gregorian::date(1970,1,1));
    static const double fraction_multiplicator = pow(10.0,-boost::posix_time::time_duration::num_fractional_digits());

    double GetUtcTime()
    {
        using namespace boost::posix_time;

        const ptime now = microsec_clock::universal_time();
        const time_duration diff = now - epoch;
        const double foo =  diff.total_seconds() + diff.fractional_seconds() * fraction_multiplicator;
        return foo;
    }


    TimerInfoBase::TimerInfoBase(const TimerId timerId):
        m_timerId(timerId)
    {

    }

    TimerInfoBase::~TimerInfoBase()
    {

    }

    EmptyTimerInfo::EmptyTimerInfo(const TimerId timerId):
        TimerInfoBase(timerId)
    {

    }

    bool
    EmptyTimerInfo::operator<(const TimerInfoBase & other) const
    {
#ifndef NDEBUG
        if (GetTimerId() == other.GetTimerId() && typeid(*this) != typeid(other))
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Two incompatible types are being used with the same TimerId",__WFILE__,__LINE__);
        }
#endif
        return GetTimerId() < other.GetTimerId();
    }




    //--------------------
    // Timers
    //--------------------

    TimerHandler::TimerHandler(boost::asio::io_service & ioService):
        m_ioService(ioService)
    {
#ifdef _MSC_VER
        enableWindowsMultimediaTimers();
#endif
    }

    TimerHandler::~TimerHandler()
    {

    }


    void TimerHandler::Instantiate(boost::asio::io_service & ioService)
    {
        ENSURE(m_instance == NULL, << L"Instantiate() was called twice!");
        m_instance = new TimerHandler(ioService);
    }

    TimerHandler & TimerHandler::Instance()
    {
        ENSURE(m_instance != NULL, << L"Instance() was called before Instantiate()");
        return *m_instance;
    }




    const TimerId
    TimerHandler::RegisterTimeoutHandler(const std::wstring & timerName, TimeoutHandler & timeoutHandler)
    {
        for (TimeoutHandlerTable::iterator it = m_timeoutHandlerTable.begin();
            it != m_timeoutHandlerTable.end(); ++it)
        {
            if (it->first == timerName && it->second == &timeoutHandler)
            {
                throw Safir::Dob::Typesystem::SoftwareViolationException(L"Multiple timers with the same name added!",__WFILE__,__LINE__);
            }
        }

        m_timeoutHandlerTable.push_back(std::make_pair(timerName,&timeoutHandler));
        return static_cast<TimerId>(m_timeoutHandlerTable.size() - 1);
    }


    const std::wstring &
    TimerHandler::GetTimerName(const TimerId timerId) const
    {
        return m_timeoutHandlerTable.at(timerId).first;
    }

    void TimerHandler::Set(const SetPolicy policy,
                           const TimerInfoPtr & timerInfo,
                           const double when)
    {
        switch (policy)
        {
        case Replace:
            {
                TimerTable::iterator findIt = m_timerTable.find(timerInfo);

                if (findIt == m_timerTable.end()) //not found
                {
                    TimerTable::iterator insertedAt = m_timerTable.insert(std::make_pair(timerInfo,when)).first;
                    m_timerQueue.insert(std::make_pair(when,insertedAt));
                    ScheduleTimer();
                }
                else
                {
                    bool removedSomething = false;
                    //remove old timer
                    TimerQueue::iterator findItInQueue = m_timerQueue.find(findIt->second);
                    for (TimerQueue::iterator it = findItInQueue; it != m_timerQueue.end(); ++it)
                    {
                        if (it->second == findIt)
                        {
                            removedSomething = true;
                            m_timerQueue.erase(it);
                            break;
                        }

                        if (it->first != findIt->second)
                        {
                            break;
                        }
                    }

                    ENSURE(removedSomething, << L"removedSomething was not set to true correctly in TimerHandler::Set");

                    //add new timer
                    findIt->second = when;
                    m_timerQueue.insert(std::make_pair(when,findIt));
                    ScheduleTimer();
                }
            }
            break;

        case Discard:
            {
                if (m_timerTable.find(timerInfo) == m_timerTable.end())
                {
                    TimerTable::iterator insertedAt = m_timerTable.insert(std::make_pair(timerInfo,when)).first;
                    m_timerQueue.insert(std::make_pair(when,insertedAt));
                    ScheduleTimer();
                }
            }
            break;
        }
    }

    void TimerHandler::Remove(const TimerInfoPtr & timerInfo) //O(1)
    {
        TimerTable::iterator findIt = m_timerTable.find(timerInfo);

        if (findIt != m_timerTable.end())
        {
            bool removedSomething = false;
            //remove old timer
            TimerQueue::iterator findItInQueue = m_timerQueue.find(findIt->second);
            for (TimerQueue::iterator it = findItInQueue; it != m_timerQueue.end(); ++it)
            {
                if (it->second == findIt)
                {
                    removedSomething = true;
                    m_timerQueue.erase(it);
                    m_timerTable.erase(findIt);
                    break;
                }

                if (it->first != findIt->second)
                {
                    break;
                }
            }

            ENSURE(removedSomething, << L"removedSomething was not set to true correctly in TimerHandler::Remove");

            ScheduleTimer();
        }
    }

    void DontDelete(const TimerInfoBase *) {}

    void TimerHandler::Remove(const TimerInfoBase & timerInfo)
    {
        TimerInfoPtr ptr = TimerInfoPtr(&const_cast<TimerInfoBase&>(timerInfo),DontDelete);

        TimerTable::iterator findIt = m_timerTable.find(ptr);

        if (findIt != m_timerTable.end())
        {
            bool removedSomething = false;
            //remove old timer
            TimerQueue::iterator findItInQueue = m_timerQueue.find(findIt->second);
            for (TimerQueue::iterator it = findItInQueue; it != m_timerQueue.end(); ++it)
            {
                if (it->second == findIt)
                {
                    removedSomething = true;
                    m_timerQueue.erase(it);
                    m_timerTable.erase(findIt);
                    break;
                }

                if (it->first != findIt->second)
                {
                    break;
                }
            }
            
            ENSURE(removedSomething, << L"removedSomething was not set to true correctly in TimerHandler::Remove");

            ScheduleTimer();
        }
    }


    void TimerHandler::Remove(const TimerId timerId) //O(n)
    {
        for (TimerTable::iterator it = m_timerTable.begin();
            it != m_timerTable.end(); ++it)
        {
            if (it->first->GetTimerId() == timerId)
            {
                bool removedSomething = false;
                //remove from queue
                TimerQueue::iterator findItInQueue = m_timerQueue.find(it->second);
                for (TimerQueue::iterator it2 = findItInQueue; it2 != m_timerQueue.end(); ++it2)
                {
                    if (it2->second == it)
                    {
                        removedSomething = true;
                        m_timerQueue.erase(it2);
                        break;
                    }

                    if (it2->first != it->second)
                    {
                        break;
                    }
                }
                
                ENSURE(removedSomething, << L"removedSomething was not set to true correctly in TimerHandler::Remove");

                if (it == m_timerTable.begin())
                {
                    m_timerTable.erase(it);
                    it = m_timerTable.begin();
                }
                else
                {
                    TimerTable::iterator removeIt = it;
                    --it;
                    m_timerTable.erase(removeIt);
                }
            }
        }
        ScheduleTimer();
    }

    bool TimerHandler::IsSet(const TimerId timerId) const
    {
        for (TimerTable::const_iterator it = m_timerTable.begin();
            it != m_timerTable.end(); ++it)
        {
            if (it->first->GetTimerId() == timerId)
            {
                return true;
            }
        }
        return false;
    }

    bool TimerHandler::IsSet(const TimerInfoPtr & timerInfo) const
    {
        return m_timerTable.find(timerInfo) != m_timerTable.end();
    }

    //returns the time to the next timeout in milliseconds
    const boost::posix_time::time_duration TimerHandler::NextTimeout() const
    {
        using namespace boost::posix_time;

        if(m_timerQueue.empty())
        {
            return seconds(100000); //one hundred thousand seconds...
        }
        else
        {
            const double delay = m_timerQueue.begin()->first - GetUtcTime();

            if (delay < 0.0)
            {
                return seconds(0);
            }
            else
            {
                return microseconds(static_cast<boost::int64_t>(delay*1e6));
            }

        }
    }

    void TimerHandler::ScheduleTimer()
    {
        if (m_deadlineTimer != NULL)
        {
            m_deadlineTimer->cancel();
        }
        m_deadlineTimer.reset(new boost::asio::deadline_timer(m_ioService,NextTimeout()));
        m_deadlineTimer->async_wait(boost::bind(&TimerHandler::HandleTimeout,this,_1));
    }

    void TimerHandler::HandleTimeout(const boost::system::error_code & error)
    {
        //check if timer was cancelled (happens in ScheduleTimer) and don't recurse...
        if (error == boost::asio::error::operation_aborted)
        {
            return;
        }

        ENSURE(!error, << "TimerHandler got an error from boost asio! " << error);

        if (!m_timerQueue.empty())
        {
            const double now = GetUtcTime();

            TimerQueue::iterator theTimer = m_timerQueue.begin();

            if (theTimer->first < now)
            {
                //lllerr << "Timer " << theTimer->second->first->GetTimerId() << " timed out"<<std::endl;

                TimerId timerId = theTimer->second->first->GetTimerId();
                TimerInfoPtr timerInfo = theTimer->second->first;

                m_timerTable.erase(theTimer->second);
                m_timerQueue.erase(theTimer);

                m_timeoutHandlerTable[timerId].second->HandleTimeout(timerInfo);
            }
        }
        ScheduleTimer();
    }
}
}
}
