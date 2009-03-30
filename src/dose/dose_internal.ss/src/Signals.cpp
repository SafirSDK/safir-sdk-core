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

#include "Signals.h"
#include <ace/Guard_T.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <boost/lexical_cast.hpp>
#include <iostream>
namespace Safir
{
namespace Dob
{
namespace Internal
{
    const std::string Prefix = "dose";
    const char * ConnectOrOutSignalName = "DOSE_CONN_OR_OUT";

    Signals * volatile Signals::m_instance = NULL;
    ACE_Thread_Mutex Signals::m_instantiationLock;// = ACE_Thread_Mutex();


    Signals & Signals::Instance()
    {
        if (m_instance == NULL)
        {
            ACE_Guard<ACE_Thread_Mutex> lck(m_instantiationLock);

            if (m_instance == NULL)
            {
                m_instance = new Signals();
            }
        }
        return *m_instance;

    }

    Signals::Signals():
        m_connectOrOutSignal(ConnectOrOutSignalName)
    {

    }


    void Signals::SignalIn(const ConnectionId & connection)
    {
        m_signalSignals.GetSemaphore(connection)->post();
    }

    const Signals::SemaphorePtr
    Signals::SignalTable::GetSemaphore(const ConnectionId & connection)
    {
#ifndef NDEBUG
        // Reading dots parameters takes some time, and since this method is called frequently, we don't call it in release builds.
        ENSURE(connection.m_node == Safir::Dob::ThisNodeParameters::NodeNumber(),
               << "It is not possible to signal or wait for a connection on other node!!! connId = " << connection);
#endif
        {
            ACE_Read_Guard<ACE_RW_Thread_Mutex> guard(m_lock);

            Semaphores::iterator findIt = m_semaphores.find(connection.m_id);
            if (findIt != m_semaphores.end())
            {
                return findIt->second;
            }

            if (-1 != m_lock.tryacquire_write_upgrade())
            {
                return AddSemaphore(connection);
            }
        }
        //tryacqure failed, so we have to wait for the write lock
        ACE_Write_Guard<ACE_RW_Thread_Mutex> guard(m_lock);
        return AddSemaphore(connection);
    }

    const Signals::SemaphorePtr&
    Signals::SignalTable::AddSemaphore(const ConnectionId & connection)
    {
        const size_t semSize = m_semaphores.size();
        if (semSize >= 300)
        {
            lllout << "PERFORMING SIGNAL GARBAGE COLLECTION!" << std::endl;
            for (int i = 0; i < 50; ++i)
            {
                Semaphores::iterator it = m_semaphores.begin();
                std::advance(it,rand() % (semSize - i));
                m_semaphores.erase(it);
            }
        }
        const std::string name = Prefix + boost::lexical_cast<std::string>(connection.m_id);
        return m_semaphores.insert(std::make_pair(connection.m_id,
                                                  new Semaphore(name))).
            first->second;
    }


    void Signals::SignalConnectOrOut()
    {
        m_connectOrOutSignal.post();
    }

    void Signals::WaitForConnectionSignal(const ConnectionId& connection)
    {
        SemaphorePtr sem = m_waitSignals.GetSemaphore(connection);
        sem->wait();
        //remove any further signals from the semaphore
        while(sem->try_wait())
            ;
    }

    void Signals::WaitForConnectOrOut()
    {
        m_connectOrOutSignal.wait();

        //remove any further signals from the semaphore
        while(m_connectOrOutSignal.try_wait())
            ;
    }

    void Signals::Remove(const ConnectionId& connection)
    {
        m_waitSignals.GetSemaphore(connection)->remove();
    }
}
}
}
