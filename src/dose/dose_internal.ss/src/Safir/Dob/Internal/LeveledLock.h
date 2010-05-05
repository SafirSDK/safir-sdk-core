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
#ifndef __DOSE_LEVELED_LOCK_H__
#define __DOSE_LEVELED_LOCK_H__

#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Dob/Internal/LeveledLockHelper.h>
#include <boost/interprocess/sync/interprocess_upgradable_mutex.hpp>
#include <ace/RW_Thread_Mutex.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{

    // Template wrappers that add level awareness to locks.
    //
    // The idea is to assign a level to each type of lock and then - in runtime,
    // for each thread - check that while holding a lock at a certain level it is
    // only possible to acquire a lock at a lower level (or same level, see below).
    // This means that we can detect nasty deadlock situations without relying on
    // a real deadlock actually manifesting itself during the test runs.
    //
    // Since keeping track of the held levels imposes a significant overhead (~400x),
    // compared to "native" locks, the check is performed only for debug built code.
    //
    // Sometimes there is a need to acquire locks on the same level in different order
    // in different parts of the code. To avoid deadlock in this case, a lock at a higher level,
    // a "master lock", must be held. For example EntityState locks and RegistrationState
    // locks are taken in different order and must therefore be protected by a master lock
    // at type level. The LeveledLock will enforce this restriction.
    // (The observant reader may wonder why we need lower level locks when there is
    // a master lock. The reason is that we want to be able to access a single
    // EntityState or RegistrationState without holding a type level lock (typically
    // done by subscribers).)
    //
    // Thus, it is ok for a thread to acquire a lock if one of the following is true.
    //
    // 1. The thread doesn't hold any lock - In this case any lock can be acquired, even
    //    locks that are declared with a master lock.
    //
    // 2. The thread holds a higher level lock.
    //
    // 3. The thread holds a lock at the same level AND the master lock is held.


    // Base class that contains the AddLevel and RemoveLevel methods
    template<unsigned short level, unsigned short masterLevel>
    class LeveledLockBase
    {
    public:

        inline void AddLevel()
        {
#ifndef NDEBUG
            Check();
            LeveledLockHelper::Instance().AddLevel(level);
#endif
        }

        inline void RemoveLevel()
        {
#ifndef NDEBUG
            LeveledLockHelper::Instance().RemoveLevel(level);
#endif
        }

    private:
        inline void Check() const
        {
            const unsigned short lowest_held_level = LeveledLockHelper::Instance().GetLowestHeldLevel();

            // Taking a lock at the same level than the current held is allowed only if a "master lock" is taken.

            if (lowest_held_level != 0 && level > lowest_held_level)
            {
                ENSURE(false,
                       << "Locks locked in invalid order (deadlock). "
                       << "A thread is trying to lock a lock at level " << level
                       << " while holding a lock at level " << lowest_held_level << ".");
            }
            else if (level == lowest_held_level)
            {
                ENSURE(masterLevel > 0,
                       << "Trying to lock another lock at level " << level
                       << " but the lock isn't declared to have a master level."
                       << " Check that all locks at level " << level
                       << " have the same master level.");

                ENSURE(LeveledLockHelper::Instance().IsHeld(masterLevel),
                       << "Trying to lock another lock at level " << level
                       << " but the master level (" << masterLevel << ") isn't held");
            }
        }

    };


    /**
     * A template wrapper that add level awareness for any lock that exhibits
     * a lock() and an unlock() method.
     */
    template <typename Lock, unsigned short level, unsigned short masterLevel>
    class LeveledLock : LeveledLockBase<level, masterLevel>
    {
    public:

        inline void lock()
        {
            LeveledLockBase<level, masterLevel>::AddLevel();
            m_lock.lock();
        }

        inline void unlock()
        {
            LeveledLockBase<level, masterLevel>::RemoveLevel();
            m_lock.unlock();
        }

    private:
        Lock m_lock;
    };

    /**
     * A template specialization for boost::interprocess::interprocess_upgradable_mutex.
     * (This type requires some additional operations.)
     */
    template<unsigned short level, unsigned short masterLevel>
    class LeveledLock<boost::interprocess::interprocess_upgradable_mutex, level, masterLevel>
        : LeveledLockBase<level, masterLevel>
    {
    public:

        inline void lock()
        {
            LeveledLockBase<level, masterLevel>::AddLevel();
            m_lock.lock();
        }

        inline void unlock()
        {
            LeveledLockBase<level, masterLevel>::RemoveLevel();
            m_lock.unlock();
        }

        inline void lock_upgradable()
        {
            LeveledLockBase<level, masterLevel>::AddLevel();
            m_lock.lock_upgradable();
        }

        inline void unlock_upgradable()
        {
            LeveledLockBase<level, masterLevel>::RemoveLevel();
            m_lock.unlock_upgradable();
        }

        inline void lock_sharable()
        {
            LeveledLockBase<level, masterLevel>::AddLevel();
            m_lock.lock_sharable();
        }

        inline void unlock_sharable()
        {
            LeveledLockBase<level, masterLevel>::RemoveLevel();
            m_lock.unlock_sharable();
        }

        inline void unlock_upgradable_and_lock()
        {
            // Need no level check for lock promotion.
            m_lock.unlock_upgradable_and_lock();
        }

    private:
        boost::interprocess::interprocess_upgradable_mutex m_lock;
    };

    /**
     * A template specialization for ACE_RW_Thread_Mutex.
     * (This type requires some additional operations.)
     */
    template<unsigned short level, unsigned short masterLevel>
    class LeveledLock<ACE_RW_Thread_Mutex, level, masterLevel>
        : LeveledLockBase<level, masterLevel>
    {
    public:

        inline int acquire_read()
        {
            LeveledLockBase<level, masterLevel>::AddLevel();
            return m_lock.acquire_read();
        }

        inline int acquire_write()
        {
            LeveledLockBase<level, masterLevel>::AddLevel();
            return m_lock.acquire_write();
        }

        inline int release()
        {
            LeveledLockBase<level, masterLevel>::RemoveLevel();
            return m_lock.release();
        }

        inline int tryacquire_write_upgrade()
        {
            // Need no level check for lock promotion.
            return m_lock.tryacquire_write_upgrade();
        }

    private:
        ACE_RW_Thread_Mutex m_lock;
    };
}
}
}
#endif
