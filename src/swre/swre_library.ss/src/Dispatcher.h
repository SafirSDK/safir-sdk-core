/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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
#ifndef __SWRE_DISPATCHER_H__
#define __SWRE_DISPATCHER_H__

#include <ace/Reactor.h>
#include <Safir/Dob/Connection.h>
#include <iostream>

class Dispatcher:
    public Safir::Dob::Dispatcher,
    public ACE_Event_Handler
{
public:
    Dispatcher(ACE_Reactor & reactor,
               Safir::Dob::Connection & connection):
        ACE_Event_Handler(&reactor),
        m_connection(&connection),
        m_isNotified(0)
    {
    }

    virtual ~Dispatcher()
    {

    }

    virtual int handle_input (ACE_HANDLE)
    {
        m_isNotified = 0;
        m_connection->Dispatch();
        return 0; //means success
    }

    virtual void OnDoDispatch()
    {
        if (m_isNotified == 0)
        {
            m_isNotified = 1;
            reactor()->notify(this,ACE_Event_Handler::READ_MASK);
        }
    }
private:
    Safir::Dob::Connection * m_connection;
    volatile int m_isNotified;
};

#endif
