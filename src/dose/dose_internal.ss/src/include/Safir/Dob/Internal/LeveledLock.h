/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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
#include <boost/thread/shared_mutex.hpp>

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
    // compared to "native" locks, the check is performed only if the code is not built
    // with the preprocessor symbol NDEBUG. It is also possible to turn off in debug builds
    // by defining the DOSE_NO_LOCK_CHECKING symbol.
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
#if !defined(NDEBUG) && !defined(DOSE_NO_LOCK_CHECKING)
            Check();
            LeveledLockHelper::Instance().AddLevel(level);
#endif
        }

        inline void RemoveLevel()
        {
#if !defined(NDEBUG) && !defined(DOSE_NO_LOCK_CHECKING)
            LeveledLockHelper::Instance().RemoveLevel(level);
#endif
        }

    private:
        inline void Check() const
        {
            ENSURE(LeveledLockHelper::Instance().GetNumberOfHeldLocks() < 5,
                   << "The number of current held locks exceeds the expected limit. "
                   << "Please change the expected limit if you have made code changes that justify this.");

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
    class LeveledLock : private LeveledLockBase<level, masterLevel>
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

        inline bool try_lock()
        {
            if (m_lock.try_lock())
            {
                LeveledLockBase<level, masterLevel>::AddLevel();
                return true;
            }
            else
            {
                return false;
            }
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
        : private LeveledLockBase<level, masterLevel>
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

        inline bool try_lock()
        {
            if (m_lock.try_lock())
            {
                LeveledLockBase<level, masterLevel>::AddLevel();
                return true;
            }
            else
            {
                return false;
            }
        }

    private:
        boost::interprocess::interprocess_upgradable_mutex m_lock;
    };

    /**
     * A template specialization for boost::shared_mutex
     * (This type requires some additional operations.)
     */
    template<unsigned short level, unsigned short masterLevel>
    class LeveledLock<boost::shared_mutex, level, masterLevel>
        : private LeveledLockBase<level, masterLevel>
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

        inline void lock_upgrade()
        {
            LeveledLockBase<level, masterLevel>::AddLevel();
            m_lock.lock_upgrade();
        }

        inline void unlock_upgrade()
        {
            LeveledLockBase<level, masterLevel>::RemoveLevel();
            m_lock.unlock_upgrade();
        }

        inline void lock_shared()
        {
            LeveledLockBase<level, masterLevel>::AddLevel();
            m_lock.lock_shared();
        }

        inline void unlock_shared()
        {
            LeveledLockBase<level, masterLevel>::RemoveLevel();
            m_lock.unlock_shared();
        }

        inline void unlock_and_lock_upgrade()
        {
            // Need no level check for lock demotion.
            m_lock.unlock_and_lock_upgrade();
        }

        inline void unlock_upgrade_and_lock()
        {
            // Need no level check for lock promotion.
            m_lock.unlock_upgrade_and_lock();
        }

    private:
        boost::shared_mutex m_lock;
    };


    template <class lock_type>
    bool steady_try_lock_for(lock_type& lock, const boost::chrono::steady_clock::duration& rel_time)
    {
        const boost::chrono::steady_clock::time_point abs_time = boost::chrono::steady_clock::now() + rel_time;
        for(;;)
        {
            const bool locked = lock.try_lock();
            if (locked)
            {
                return true;
            }

            if (boost::chrono::steady_clock::now() > abs_time)
            {
                return false;
            }

            boost::this_thread::yield();
        }
    }
}
}
}
#endif
