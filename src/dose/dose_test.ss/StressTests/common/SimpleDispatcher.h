/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
*
* Created by: Lars Hagström / stlrha
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

#ifndef __SIMPLEDISPATCHER_H__
#define __SIMPLEDISPATCHER_H__

#include <Safir/Dob/Consumer.h>
#include <boost/noncopyable.hpp>
#include <stdlib.h>


//we include a bunch of extra stuff in here that most of the stress tests use anyway, to reduce 
//the number of places for the warning stuff.
#ifdef _MSC_VER
  #pragma warning(push)
  #pragma warning(disable: 4267)
  #pragma warning(disable: 4702)
  #pragma warning(disable: 4251 4275 4127)
#endif

#include <boost/program_options/cmdline.hpp>
#include <boost/program_options/parsers.hpp>
#include <boost/program_options/variables_map.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/thread/mutex.hpp>
#include <boost/thread/condition.hpp>
#include <boost/bind.hpp>

#ifdef _MSC_VER
  #pragma warning(pop)
#endif



class SimpleDispatcher:
    public Safir::Dob::StopHandler,
    public Safir::Dob::Dispatcher,
    private boost::noncopyable
{
public:
    explicit SimpleDispatcher(Safir::Dob::Connection & connection)
        : m_connection(connection)
        , m_dispatch(false)
    {
    
    }

    virtual void OnStopOrder() {exit(0);}
    virtual void OnDoDispatch() 
    {
        {
            boost::lock_guard<boost::mutex> lock(m_mutex);
            m_dispatch=true;
        }
        m_condition.notify_one();
    }
    
    
    //return true if dispatch is due
    bool Wait(const long milliseconds)
    {
        if (milliseconds <= 0)
        {
            return false;
        }

        const boost::posix_time::milliseconds delay(milliseconds);

        boost::unique_lock<boost::mutex> lock(m_mutex);
        const bool res = m_condition.timed_wait(lock,delay,boost::bind(&SimpleDispatcher::DispatchPending,this));
        m_dispatch = false;
        return res;
    }

private:
    bool DispatchPending() const {return m_dispatch;}

    Safir::Dob::Connection & m_connection;
    bool m_dispatch;
    boost::mutex m_mutex;
    boost::condition m_condition;
};

#endif

