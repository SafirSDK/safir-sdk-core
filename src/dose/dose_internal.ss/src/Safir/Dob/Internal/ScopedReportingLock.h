/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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
#ifndef __SCOPED_REPORTING_LOCK_H__
#define __SCOPED_REPORTING_LOCK_H__

#include <boost/interprocess/managed_shared_memory.hpp>
#include <boost/interprocess/sync/named_mutex.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/thread/thread_time.hpp>
#include <Safir/Utilities/Internal/LowLevelLogger.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    template<typename LockType, int TimeOut>   
    class ScopedReportingLock
    {
    public:
        //TimeOut in seconds. If lock has not been aquired after timeOut the lock
        //is released 'hard', and the lock is taken.
        ScopedReportingLock(LockType& lock) : m_lock(lock), m_owns(false)
        {
            Aquire();
        }

        ~ScopedReportingLock()
        {
            if (m_owns)
            {
                m_lock.unlock();
            }
        }    

    private:
        bool m_owns;
        LockType& m_lock;

        void Aquire()
        {
            m_owns = m_lock.timed_lock(boost::get_system_time()+boost::posix_time::seconds(TimeOut));
            if (!m_owns)
            {                
                lllerr<<"Safir.Dob.Internal.ScopedReportingLock timed out when waiting for a lock."<<std::endl;
                Aquire();
            }
        }
    };


    /*
     *  UpgradableReportingLock
     */
    template<typename LockType, int TimeOut>   
    class UpgradableReportingLock
    {
    public:
        //TimeOut in seconds. If lock has not been aquired after timeOut the lock
        //is released 'hard', and the lock is taken.
        UpgradableReportingLock(LockType& lock) : m_lock(lock), m_owns(false)
        {
            Aquire();
        }

        ~UpgradableReportingLock()
        {
            if (m_owns)
            {
                m_lock.unlock_upgradable();
            }
        }    

    private:
        bool m_owns;
        LockType& m_lock;

        void Aquire()
        {
            m_owns = m_lock.timed_lock_upgradable(boost::get_system_time()+boost::posix_time::seconds(TimeOut));
            if (!m_owns)
            {               
                lllerr<<"Safir.Dob.Internal.ScopedReportingLock timed out when waiting for a lock."<<std::endl;
                Aquire();
            }
        }
    };

    /*
     *  SharableReportingLock
     */
    template<typename LockType, int TimeOut>   
    class SharableReportingLock
    {
    public:
        //TimeOut in seconds. If lock has not been aquired after timeOut the lock
        //is released 'hard', and the lock is taken.
        SharableReportingLock(LockType& lock) : m_lock(lock), m_owns(false)
        {
            Aquire();
        }

        ~SharableReportingLock()
        {
            Unlock();
        }   
 
    
        void Unlock()
        {
            if (m_owns)
            {
                m_lock.unlock_sharable();
                m_owns=false;
            }
        }
            

    private:
        bool m_owns;
        LockType& m_lock;

        void Aquire()
        {
            m_owns = m_lock.timed_lock_sharable(boost::get_system_time()+boost::posix_time::seconds(TimeOut));
            if (!m_owns)
            {          
                lllerr<<"Safir.Dob.Internal.ScopedReportingLock timed out when waiting for a lock."<<std::endl;
                Aquire();
            }
        }
    };
}
}
}

#endif
