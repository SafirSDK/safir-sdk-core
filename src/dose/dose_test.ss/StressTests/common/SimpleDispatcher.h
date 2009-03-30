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
#include <ace/Auto_Event.h>
#include <ace/Time_Value.h>
#include <stdlib.h>

class SimpleDispatcher:
    public Safir::Dob::StopHandler,
    public Safir::Dob::Dispatcher,
    private boost::noncopyable
{
public:
    explicit SimpleDispatcher(Safir::Dob::Connection & connection):m_connection(connection) {}

    virtual void OnStopOrder() {exit(0);}
    virtual void OnDoDispatch() {m_event.signal();}

    //return true if dispatch is due
    bool Wait(const long milliseconds)
    {
        if (milliseconds <= 0)
        {
            return false;
        }

        const std::ldiv_t divResult = std::ldiv(milliseconds,1000L);
        const ACE_Time_Value delay(divResult.quot,divResult.rem*1000);

        const int res = m_event.wait(&delay,0);

        if(res == 0)
        {
            return true;
        }
        else
        {
            return false;
        }
    }

private:
    ACE_Auto_Event m_event;
    Safir::Dob::Connection & m_connection;
};

#endif

