/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
*
* Created by: Joel Ottosson / joot
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
#include "SimpleReactor.h"
#include <vector>
#include <iostream>
#include <boost/bind.hpp>

SimpleReactor::SimpleReactor() :     
    m_running(false),
    m_timeToDispatch(false)
{
}


SimpleReactor::~SimpleReactor(void)
{
}

void SimpleReactor::OnDoDispatch()
{    
    boost::lock_guard<boost::mutex> lock(m_mutex);
    m_timeToDispatch=true;
    m_condition.notify_one();    
}

boost::system_time SimpleReactor::NextTimer()
{    
    boost::system_time next = boost::get_system_time()+boost::posix_time::minutes(1);
    for (TimerMap::iterator it=m_timers.begin(); it!=m_timers.end(); ++it)
    {
        if (it->second.Elapse<next)
        {
            next=it->second.Elapse;
        }
    }
    return next;
}

void SimpleReactor::RunReactor()
{
    m_running=true;

    while (m_running)
    {             
        boost::mutex::scoped_lock lock(m_mutex);
              
        m_condition.timed_wait(lock, NextTimer());       
        
        if (m_timeToDispatch)
        {                        
            DoDispatch();
            m_timeToDispatch=false;
        }
        
        //Check timers
        HandleTimers();        
    }
}

void SimpleReactor::HandleTimers()
{
    boost::system_time now = boost::get_system_time();
    std::vector<int> elapsedTimers;
    std::vector<int> removeTimers;
    for (TimerMap::iterator it=m_timers.begin(); it!=m_timers.end(); ++it)
    {
        if (it->second.Elapse<now)
        {
            elapsedTimers.push_back(it->first);
            if (it->second.Cyclic)
            {
                int interval=it->second.Interval;
                it->second.Elapse=boost::get_system_time()+boost::posix_time::millisec(interval);                    
            }
            else
            {
                removeTimers.push_back(it->first);
            }
        }           
    }
        
    std::for_each(removeTimers.begin(), removeTimers.end(), boost::bind(&SimpleReactor::RemoveTimer, this, boost::cref(_1).get()));
    std::for_each(elapsedTimers.begin(), elapsedTimers.end(), boost::bind(&SimpleReactor::OnTimeout, this, boost::cref(_1).get()));
}

void SimpleReactor::SetTimer(int timerId, int milliseconds, bool cyclic)
{    
    Timer timer;
    timer.Id=timerId;
    timer.Cyclic=cyclic;
    timer.Interval=milliseconds;
    timer.Elapse=boost::get_system_time()+boost::posix_time::millisec(milliseconds);    
    m_timers[timerId] = timer;
}

void SimpleReactor::RemoveTimer(int timerId)
{
    TimerMap::iterator it = m_timers.find(timerId);
    if (it!=m_timers.end())
    {
        m_timers.erase(it);
    }
}
