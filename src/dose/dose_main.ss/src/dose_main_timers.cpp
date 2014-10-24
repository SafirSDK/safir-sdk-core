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
#include <iostream>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <boost/bind.hpp>
#include <ace/Reactor.h>

#if defined(linux) || defined(__linux) || defined(__linux__)
#include <time.h>
#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
#include <windows.h>
#else
#  error dose_main timers not supported on this plattform!
#endif

namespace Safir
{
namespace Dob
{
namespace Internal
{

    ACE_Time_Value GetMonotonicTime()
    {        
#if defined(linux) || defined(__linux) || defined(__linux__)

        timespec_t t;

        clock_gettime(CLOCK_MONOTONIC, &t);

        return ACE_Time_Value(t);

#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)

        ULONGLONG t = GetTickCount64();

        return ACE_Time_Value(t/1000, (t%1000) * 1000);

#endif

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

    TimerHandler::TimerHandler()
    {

    }

    TimerHandler::~TimerHandler()
    {

    }

    TimerHandler & TimerHandler::Instance()
    {
        static TimerHandler theInstance;
        return theInstance;
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
                           const ACE_Time_Value& when)
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
                    //remove old timer
                    TimerQueue::iterator findItInQueue = m_timerQueue.find(findIt->second);
                    for (TimerQueue::iterator it = findItInQueue; it != m_timerQueue.end(); ++it)
                    {
                        if (it->second == findIt)
                        {
                            m_timerQueue.erase(it);
                            break;
                        }

                        if (it->first != findIt->second)
                        {
                            break;
                        }
                    }

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
            //remove old timer
            TimerQueue::iterator findItInQueue = m_timerQueue.find(findIt->second);
            for (TimerQueue::iterator it = findItInQueue; it != m_timerQueue.end(); ++it)
            {
                if (it->second == findIt)
                {
                    m_timerQueue.erase(it);
                    m_timerTable.erase(findIt);
                    break;
                }

                if (it->first != findIt->second)
                {
                    break;
                }
            }
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
            //remove old timer
            TimerQueue::iterator findItInQueue = m_timerQueue.find(findIt->second);
            for (TimerQueue::iterator it = findItInQueue; it != m_timerQueue.end(); ++it)
            {
                if (it->second == findIt)
                {
                    m_timerQueue.erase(it);
                    m_timerTable.erase(findIt);
                    break;
                }

                if (it->first != findIt->second)
                {
                    break;
                }
            }
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
                //remove from queue
                TimerQueue::iterator findItInQueue = m_timerQueue.find(it->second);
                for (TimerQueue::iterator it2 = findItInQueue; it2 != m_timerQueue.end(); ++it2)
                {
                    if (it2->second == it)
                    {
                        m_timerQueue.erase(it2);
                        break;
                    }

                    if (it2->first != it->second)
                    {
                        break;
                    }
                }

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
    const ACE_Time_Value TimerHandler::NextTimeout() const
    {
        if(m_timerQueue.empty())
        {
            return ACE_Time_Value(100000); //one hundred thousand seconds...
        }
        else
        {
            const ACE_Time_Value delay = m_timerQueue.begin()->first - GetMonotonicTime();

            if (delay < ACE_Time_Value::zero)
            {
                return ACE_Time_Value::zero;
            }
            else
            {
                return delay;
            }

        }
    }

    void TimerHandler::ScheduleTimer()
    {
        ACE_Reactor::instance()->cancel_timer(this);
        ACE_Reactor::instance()->schedule_timer(this,0,NextTimeout());
    }

    int TimerHandler::handle_timeout(const ACE_Time_Value&, const void*)
    {
        if (!m_timerQueue.empty())
        {
            const ACE_Time_Value now = GetMonotonicTime();

            TimerQueue::iterator theTimer = m_timerQueue.begin();

            if (theTimer->first < now)
            {
                TimerId timerId = theTimer->second->first->GetTimerId();
                TimerInfoPtr timerInfo = theTimer->second->first;

                m_timerTable.erase(theTimer->second);
                m_timerQueue.erase(theTimer);

                m_timeoutHandlerTable[timerId].second->HandleTimeout(timerInfo);
            }
        }
        ScheduleTimer();
        return 0;
    }
}
}
}
