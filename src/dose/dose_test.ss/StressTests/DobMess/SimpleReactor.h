/******************************************************************************
*
* Copyright Saab AB, 2007-2011 (http://www.safirsdk.com)
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
#ifndef _Reactor_h_
#define _Reactor_h_

#include <Safir/Dob/Consumer.h>
#include <boost/noncopyable.hpp>
#include <boost/thread.hpp>
#include <boost/thread/condition_variable.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <map>

class SimpleReactor :   public Safir::Dob::Dispatcher,
                        private boost::noncopyable
{
public:
    SimpleReactor();
    virtual ~SimpleReactor(void);

    void RunReactor();
    void SetTimer(int timerId, int milliseconds, bool cyclic);
    void RemoveTimer(int timerId);

    virtual void DoDispatch() = 0;
    virtual void OnTimeout(int timerId) = 0;

private:    
    boost::condition_variable m_condition;
    boost::mutex m_mutex;
    bool m_running;
    bool m_timeToDispatch;
    
    struct Timer
    {
        int Id;
        int Interval;
        bool Cyclic;
        boost::system_time Elapse;        
    };        

    typedef std::map<int/*timerId*/, Timer> TimerMap;
    TimerMap m_timers;

    virtual void OnDoDispatch();
    void HandleTimers();
    boost::system_time NextTimer();
};

#endif
