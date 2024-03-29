/******************************************************************************
*
* Copyright Saab AB, 2007-2013,2015 (http://safirsdkcore.com)
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
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <iostream>

#ifdef _MSC_VER
  #pragma warning(push)
  #pragma warning(disable: 4702)
#endif

#include <boost/lexical_cast.hpp>

#ifdef _MSC_VER
  #pragma warning(pop)
#endif

namespace
{
    const std::string GetSemaphoreName(const Safir::Dob::Internal::ConnectionId& connection)
    {
        return std::string("Global\\SAFIR_CONNECTION_") + boost::lexical_cast<std::string>(connection.m_id);
    }
}

namespace Safir
{
namespace Dob
{
namespace Internal
{
    const char * ConnectOrOutSignalName = "Global\\SAFIR_DOSE_CONN_OR_OUT";


    std::once_flag Signals::SingletonHelper::m_onceFlag;

    Signals & Signals::SingletonHelper::Instance()
    {
        static Signals instance;
        return instance;
    }

    Signals & Signals::Instance()
    {
        std::call_once(SingletonHelper::m_onceFlag,[]{SingletonHelper::Instance();});
        return SingletonHelper::Instance();
    }

    Signals::Signals():
        m_connectOrOutSignal(ConnectOrOutSignalName)
    {

    }

    Signals::~Signals()
    {

    }

    void Signals::SignalIn(const ConnectionId & connection)
    {
        m_signalSignals.GetSemaphore(connection)->post();
    }

    Signals::SignalTable::SignalTable()
    {

    }

    const Signals::SemaphorePtr
    Signals::SignalTable::GetSemaphore(const ConnectionId & connection)
    {
        {
            boost::shared_lock<SignalsLock> guard(m_lock);

            Semaphores::iterator findIt = m_semaphores.find(connection.m_id);
            if (findIt != m_semaphores.end())
            {
                return findIt->second;
            }
        }

        //Did not find the semaphore, we need to get write access.

        //first get an upgrade lock, which is only a read lock, but stops new
        //readers from getting in, so we will never be starved.
        boost::upgrade_lock<SignalsLock> readGuard(m_lock);

        //someone else may have gotten in and added the semaphore, so we check.
        Semaphores::iterator findIt = m_semaphores.find(connection.m_id);
        if (findIt != m_semaphores.end())
        {
            return findIt->second;
        }

        //ok, now we upgrade and add.
        boost::upgrade_to_unique_lock<SignalsLock> writeGuard(readGuard);
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
        const std::string name = GetSemaphoreName(connection);
        SemaphorePtr sem(new NamedSemaphore(name));
        return m_semaphores.insert(std::make_pair(connection.m_id, sem)).
            first->second;
    }


    void Signals::SignalConnectOrOut()
    {
        m_connectOrOutSignal.post();
    }

    void wait(const Signals::SemaphorePtr & sem)
    {
        sem->wait();
        //remove any further signals from the semaphore
        while(sem->try_wait())
            ;
    }

    const std::function<void(void)> Signals::GetConnectionSignalWaiter(const ConnectionId& connection)
    {
        return [this,connection]{m_waitSignals.GetSemaphore(connection)->wait();};
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
        NamedSemaphore::remove(GetSemaphoreName(connection));
    }

    void Signals::RemoveConnectOrOut()
    {
        NamedSemaphore::remove(ConnectOrOutSignalName);
    }
}
}
}
