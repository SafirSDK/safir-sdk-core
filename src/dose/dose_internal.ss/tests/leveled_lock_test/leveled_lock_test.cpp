/******************************************************************************
*
* Copyright Saab AB, 2026 (http://safirsdkcore.com)
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
#include <Safir/Dob/Internal/LeveledLock.h>
#include <Safir/Dob/Internal/LeveledLockHelper.h>
#include <Safir/Dob/Typesystem/Exceptions.h>

#if defined _MSC_VER
#  pragma warning (push)
#  pragma warning (disable : 4100)
#endif

#include <boost/thread.hpp>

#if defined _MSC_VER
#  pragma warning (pop)
#endif

#define BOOST_TEST_MODULE LeveledLockTest
#include <boost/test/unit_test.hpp>

// The level checking (deadlock detection) is only compiled in when the code is
// built without NDEBUG and without DOSE_NO_LOCK_CHECKING. The test cases that
// exercise the checking logic are guarded accordingly, while the plain locking
// mechanics are exercised in all build configurations.
#if !defined(NDEBUG) && !defined(DOSE_NO_LOCK_CHECKING)
#  define LEVELED_LOCK_CHECKING_ENABLED 1
#endif

using namespace Safir::Dob::Internal;
using Safir::Dob::Typesystem::SoftwareViolationException;

namespace
{
    [[maybe_unused]] unsigned int HeldLocks()
    {
        return LeveledLockHelper::Instance().GetNumberOfHeldLocks();
    }

#ifdef LEVELED_LOCK_CHECKING_ENABLED
    // A minimal lock that records whether its lock()/try_lock() was ever called.
    // Used to verify that the ordering check runs *before* the underlying lock is
    // acquired (see level_check_runs_before_acquiring_lock).
    struct RecordingMutex
    {
        static bool s_acquireCalled;

        void lock() { s_acquireCalled = true; }
        void unlock() {}
        bool try_lock() { s_acquireCalled = true; return true; }
    };

    bool RecordingMutex::s_acquireCalled = false;
#endif
}

// Basic lock/unlock mechanics for a plain mutex wrapper.
BOOST_AUTO_TEST_CASE(basic_lock_unlock)
{
    LeveledLock<boost::mutex, 1, 0> lock;

    lock.lock();
#ifdef LEVELED_LOCK_CHECKING_ENABLED
    BOOST_CHECK_EQUAL(HeldLocks(), 1u);
#endif
    lock.unlock();
#ifdef LEVELED_LOCK_CHECKING_ENABLED
    BOOST_CHECK_EQUAL(HeldLocks(), 0u);
#endif
}

// try_lock on a free lock succeeds and registers the level.
BOOST_AUTO_TEST_CASE(try_lock_when_free)
{
    LeveledLock<boost::mutex, 1, 0> lock;

    BOOST_CHECK(lock.try_lock());
#ifdef LEVELED_LOCK_CHECKING_ENABLED
    BOOST_CHECK_EQUAL(HeldLocks(), 1u);
#endif
    lock.unlock();
#ifdef LEVELED_LOCK_CHECKING_ENABLED
    BOOST_CHECK_EQUAL(HeldLocks(), 0u);
#endif
}

// try_lock fails (without registering a level) when the lock is held by
// another thread.
BOOST_AUTO_TEST_CASE(try_lock_when_held_by_other_thread)
{
    LeveledLock<boost::mutex, 1, 0> lock;
    lock.lock();

    bool acquired = true;
    boost::thread t([&lock, &acquired]
                    {
                        acquired = lock.try_lock();
                        if (acquired)
                        {
                            lock.unlock();
                        }
                    });
    t.join();

    BOOST_CHECK(!acquired);

    lock.unlock();
#ifdef LEVELED_LOCK_CHECKING_ENABLED
    BOOST_CHECK_EQUAL(HeldLocks(), 0u);
#endif
}

// Acquiring locks in decreasing level order is allowed.
BOOST_AUTO_TEST_CASE(decreasing_level_order_ok)
{
    LeveledLock<boost::mutex, 5, 0> high;
    LeveledLock<boost::mutex, 3, 0> low;

    BOOST_CHECK_NO_THROW(high.lock());
    BOOST_CHECK_NO_THROW(low.lock());
#ifdef LEVELED_LOCK_CHECKING_ENABLED
    BOOST_CHECK_EQUAL(HeldLocks(), 2u);
#endif
    low.unlock();
    high.unlock();
#ifdef LEVELED_LOCK_CHECKING_ENABLED
    BOOST_CHECK_EQUAL(HeldLocks(), 0u);
#endif
}

#ifdef LEVELED_LOCK_CHECKING_ENABLED

// Acquiring a higher level lock while holding a lower level one is a deadlock
// risk and must be detected.
BOOST_AUTO_TEST_CASE(increasing_level_order_throws)
{
    BOOST_CHECK_EQUAL(HeldLocks(), 0u);

    LeveledLock<boost::mutex, 3, 0> low;
    low.lock();

    LeveledLock<boost::mutex, 5, 0> high;
    BOOST_CHECK_THROW(high.lock(), SoftwareViolationException);

    // The offending acquisition registered nothing; only the lock we successfully
    // took remains held.
    BOOST_CHECK_EQUAL(HeldLocks(), 1u);

    low.unlock();
    BOOST_CHECK_EQUAL(HeldLocks(), 0u);
}

// Acquiring a second lock at the same level without a master lock is not
// allowed.
BOOST_AUTO_TEST_CASE(same_level_without_master_throws)
{
    BOOST_CHECK_EQUAL(HeldLocks(), 0u);

    LeveledLock<boost::mutex, 5, 0> first;
    first.lock();

    LeveledLock<boost::mutex, 5, 0> second;
    BOOST_CHECK_THROW(second.lock(), SoftwareViolationException);

    BOOST_CHECK_EQUAL(HeldLocks(), 1u);

    first.unlock();
    BOOST_CHECK_EQUAL(HeldLocks(), 0u);
}

// Acquiring a second lock at the same level is allowed when the declared master
// lock is held.
BOOST_AUTO_TEST_CASE(same_level_with_master_ok)
{
    BOOST_CHECK_EQUAL(HeldLocks(), 0u);

    LeveledLock<boost::mutex, 10, 0> master;
    LeveledLock<boost::mutex, 5, 10> first;
    LeveledLock<boost::mutex, 5, 10> second;

    master.lock();
    first.lock();
    BOOST_CHECK_NO_THROW(second.lock());
    BOOST_CHECK_EQUAL(HeldLocks(), 3u);

    second.unlock();
    first.unlock();
    master.unlock();
    BOOST_CHECK_EQUAL(HeldLocks(), 0u);
}

// Acquiring a same level lock fails if only the wrong (unheld) master is
// declared.
BOOST_AUTO_TEST_CASE(same_level_with_unheld_master_throws)
{
    BOOST_CHECK_EQUAL(HeldLocks(), 0u);

    LeveledLock<boost::mutex, 5, 10> first;
    first.lock();

    LeveledLock<boost::mutex, 5, 10> second;
    BOOST_CHECK_THROW(second.lock(), SoftwareViolationException);

    BOOST_CHECK_EQUAL(HeldLocks(), 1u);

    first.unlock();
    BOOST_CHECK_EQUAL(HeldLocks(), 0u);
}

// Holding more than the expected number of locks is detected.
BOOST_AUTO_TEST_CASE(too_many_held_locks_throws)
{
    BOOST_CHECK_EQUAL(HeldLocks(), 0u);

    LeveledLock<boost::mutex, 9, 0> l9;
    LeveledLock<boost::mutex, 8, 0> l8;
    LeveledLock<boost::mutex, 7, 0> l7;
    LeveledLock<boost::mutex, 6, 0> l6;
    LeveledLock<boost::mutex, 5, 0> l5;

    l9.lock();
    l8.lock();
    l7.lock();
    l6.lock();
    l5.lock();
    BOOST_CHECK_EQUAL(HeldLocks(), 5u);

    LeveledLock<boost::mutex, 4, 0> l4;
    BOOST_CHECK_THROW(l4.lock(), SoftwareViolationException);

    BOOST_CHECK_EQUAL(HeldLocks(), 5u);

    l5.unlock();
    l6.unlock();
    l7.unlock();
    l8.unlock();
    l9.unlock();
    BOOST_CHECK_EQUAL(HeldLocks(), 0u);
}

// Regression test: the ordering check must run *before* the underlying lock is
// acquired. The whole purpose of LeveledLock is to detect lock ordering
// violations pre-emptively, "without relying on a real deadlock actually
// manifesting itself". If the check ran after m_lock.lock(), then in a genuine
// cyclic deadlock the thread would block on the (contended) mutex and hang
// instead of throwing - defeating the detection. Using a mock mutex that records
// whether it was acquired lets us verify the ordering deterministically, without
// any threading or risk of hanging the test.
BOOST_AUTO_TEST_CASE(level_check_runs_before_acquiring_lock)
{
    BOOST_CHECK_EQUAL(HeldLocks(), 0u);

    LeveledLock<boost::mutex, 3, 0> held;
    held.lock();

    RecordingMutex::s_acquireCalled = false;

    // Acquiring level 5 while holding level 3 is an ordering violation.
    LeveledLock<RecordingMutex, 5, 0> offender;
    BOOST_CHECK_THROW(offender.lock(), SoftwareViolationException);

    // The crucial assertion: the underlying lock must never have been acquired,
    // because the check threw first.
    BOOST_CHECK(!RecordingMutex::s_acquireCalled);

    // Nothing was registered for the offending acquisition.
    BOOST_CHECK_EQUAL(HeldLocks(), 1u);

    held.unlock();
    BOOST_CHECK_EQUAL(HeldLocks(), 0u);
}

#endif

// Exercise the interprocess_upgradable_mutex specialization.
BOOST_AUTO_TEST_CASE(upgradable_mutex_operations)
{
    LeveledLock<boost::interprocess::interprocess_upgradable_mutex, 1, 0> lock;

    lock.lock();
    lock.unlock();

    lock.lock_sharable();
    lock.unlock_sharable();

    lock.lock_upgradable();
    lock.unlock_upgradable_and_lock();
    lock.unlock();

    BOOST_CHECK(lock.try_lock());
    lock.unlock();

#ifdef LEVELED_LOCK_CHECKING_ENABLED
    BOOST_CHECK_EQUAL(HeldLocks(), 0u);
#endif
}

// Exercise the boost::shared_mutex specialization.
BOOST_AUTO_TEST_CASE(shared_mutex_operations)
{
    LeveledLock<boost::shared_mutex, 1, 0> lock;

    lock.lock();
    lock.unlock_and_lock_upgrade();
    lock.unlock_upgrade_and_lock();
    lock.unlock();

    lock.lock_shared();
    lock.unlock_shared();

    lock.lock_upgrade();
    lock.unlock_upgrade();

#ifdef LEVELED_LOCK_CHECKING_ENABLED
    BOOST_CHECK_EQUAL(HeldLocks(), 0u);
#endif
}
