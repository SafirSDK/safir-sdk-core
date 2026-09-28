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
#include <Safir/Utilities/StartupSynchronizer.h>
#include <Safir/Utilities/Internal/ConfigReader.h>
#include <Safir/Utilities/Internal/Expansion.h>

#include <boost/filesystem/fstream.hpp>
#include <boost/filesystem/operations.hpp>

#include <atomic>
#include <functional>
#include <mutex>
#include <sstream>
#include <stdexcept>
#include <string>
#include <thread>
#include <vector>

#define BOOST_TEST_MODULE StartupSynchronizerTest
#include <boost/test/unit_test.hpp>

/**
 * Single process tests of the things that are hard to see from the process level
 * tests: the bookkeeping inside one process, the failure paths, and the state
 * that the protocol leaves on disk.
 *
 * The multi process behaviour is covered by the processes, threads,
 * processes_and_threads and lifecycle tests.
 */

using Safir::Utilities::StartupSynchronizer;

namespace
{
    /** These have to match the ones in StartupSynchronizer.cpp. Pinning them here
     * is deliberate, for two reasons. The lock file names are part of what is
     * deployed on customer systems, so changing them by accident is a real
     * problem. And which files exist after a resource has been destroyed is part
     * of the contract too: a lock file that gets deleted while someone still holds
     * a lock on it is invisible to the next process that creates the same file
     * name, which is exactly the bug these tests guard against. */
    const char* const UsersLockSuffix = "_FIRST";
    const char* const GenerationLockSuffix = "_SECOND";
    const char* const CreatedMarkerSuffix = "_CREATED";

    const boost::filesystem::path LockFileDirectory()
    {
        Safir::Utilities::Internal::ConfigReader config;
        return boost::filesystem::path(config.Locations().get<std::string>("lock_file_directory"));
    }

    /** The name that StartupSynchronizer will actually use for its files. */
    const std::string FullName(const std::string& name)
    {
        return name + Safir::Utilities::Internal::Expansion::GetSafirInstanceSuffix();
    }

    const boost::filesystem::path FilePath(const std::string& name, const char* const suffix)
    {
        return LockFileDirectory() / (FullName(name) + suffix);
    }

    bool Exists(const std::string& name, const char* const suffix)
    {
        return boost::filesystem::exists(FilePath(name, suffix));
    }

    /**
     * Tracks what the protocol has told us, and checks the invariants that hold
     * no matter what the individual test is doing. Every test asserts that no
     * violation was recorded, so a double create or a use after destroy fails
     * the test it happens in rather than going unnoticed.
     */
    class Resource
    {
    public:
        explicit Resource(const std::string& name)
            : m_name(name)
        {
            //A previous run that was killed at the wrong moment can leave a
            //marker behind. Start from a known state instead of inheriting it.
            boost::system::error_code ec;
            boost::filesystem::remove(FilePath(name, CreatedMarkerSuffix), ec);
        }

        const std::string& Name() const {return m_name;}

        void NoteCreate()
        {
            std::lock_guard<std::mutex> lck(m_lock);
            if (m_alive)
            {
                //Not a violation: a generation whose users all went away without
                //anyone getting to run Destroy is debris, and creating a new one
                //on top of it is exactly what the Create callback is documented to
                //have to cope with. It is worth counting though, since it should
                //only happen in the tests that arrange for it.
                ++m_abandoned;
            }
            m_alive = true;
            ++m_creates;
        }

        void NoteUse()
        {
            std::lock_guard<std::mutex> lck(m_lock);
            if (!m_alive)
            {
                Violation("Use was called while the resource was not alive");
            }
            ++m_uses;
        }

        void NoteDestroy(const bool instanceWasUsed)
        {
            std::lock_guard<std::mutex> lck(m_lock);
            if (!m_alive)
            {
                Violation("Destroy was called while the resource was not alive");
            }
            if (!instanceWasUsed)
            {
                Violation("Destroy was called on an instance that never got a Use callback");
            }
            m_alive = false;
            ++m_destroys;
        }

        int Creates() const {std::lock_guard<std::mutex> lck(m_lock); return m_creates;}
        /** Generations that were replaced without anyone running Destroy on them. */
        int Abandoned() const {std::lock_guard<std::mutex> lck(m_lock); return m_abandoned;}
        int Uses() const {std::lock_guard<std::mutex> lck(m_lock); return m_uses;}
        int Destroys() const {std::lock_guard<std::mutex> lck(m_lock); return m_destroys;}
        bool Alive() const {std::lock_guard<std::mutex> lck(m_lock); return m_alive;}

        /** Empty unless an invariant was broken. */
        const std::string Violations() const
        {
            std::lock_guard<std::mutex> lck(m_lock);
            return m_violations;
        }

    private:
        void Violation(const std::string& what)
        {
            m_violations += what;
            m_violations += "\n";
        }

        mutable std::mutex m_lock;
        const std::string m_name;
        bool m_alive = false;
        int m_creates = 0;
        int m_uses = 0;
        int m_destroys = 0;
        int m_abandoned = 0;
        std::string m_violations;
    };

    /** One user of a Resource. The hooks let a test make a callback misbehave. */
    class Instance : public Safir::Utilities::Synchronized
    {
    public:
        explicit Instance(Resource& resource)
            : m_resource(resource)
        {
        }

        /** Called at the start of Create(), before anything is recorded. */
        std::function<void()> inCreate;
        /** Called at the start of Use(), before anything is recorded. */
        std::function<void()> inUse;

        bool wasUsed = false;

    private:
        void Create() override
        {
            if (inCreate)
            {
                inCreate();
            }
            m_resource.NoteCreate();
        }

        void Use() override
        {
            if (inUse)
            {
                inUse();
            }
            m_resource.NoteUse();
            wasUsed = true;
        }

        void Destroy() override
        {
            m_resource.NoteDestroy(wasUsed);
        }

        Resource& m_resource;
    };
}


BOOST_AUTO_TEST_CASE(create_use_destroy)
{
    Resource resource("SS_UNIT_basic");
    Instance instance(resource);

    {
        StartupSynchronizer ss(resource.Name().c_str());
        ss.Start(&instance);

        BOOST_CHECK_EQUAL(resource.Creates(), 1);
        BOOST_CHECK_EQUAL(resource.Uses(), 1);
        BOOST_CHECK_EQUAL(resource.Destroys(), 0);
        BOOST_CHECK(Exists(resource.Name(), CreatedMarkerSuffix));
    }

    BOOST_CHECK_EQUAL(resource.Destroys(), 1);
    BOOST_CHECK(!resource.Alive());
    BOOST_CHECK_EQUAL(resource.Abandoned(), 0);
    BOOST_CHECK_EQUAL(resource.Violations(), "");
}

BOOST_AUTO_TEST_CASE(lock_files_outlive_the_resource_but_the_marker_does_not)
{
    //The lock files must never be deleted. Deleting a file that other processes
    //still hold locks on leaves those locks on an inode that nobody can find
    //again, so two processes can end up believing they are both alone.
    Resource resource("SS_UNIT_files");
    Instance instance(resource);

    {
        StartupSynchronizer ss(resource.Name().c_str());
        ss.Start(&instance);

        BOOST_CHECK(Exists(resource.Name(), UsersLockSuffix));
        BOOST_CHECK(Exists(resource.Name(), GenerationLockSuffix));
        BOOST_CHECK(Exists(resource.Name(), CreatedMarkerSuffix));
    }

    BOOST_CHECK(Exists(resource.Name(), UsersLockSuffix));
    BOOST_CHECK(Exists(resource.Name(), GenerationLockSuffix));
    BOOST_CHECK(!Exists(resource.Name(), CreatedMarkerSuffix));
    BOOST_CHECK_EQUAL(resource.Violations(), "");
}

BOOST_AUTO_TEST_CASE(a_second_instance_in_the_process_only_uses)
{
    Resource resource("SS_UNIT_two_instances");
    Instance first(resource);
    Instance second(resource);

    {
        StartupSynchronizer ssFirst(resource.Name().c_str());
        ssFirst.Start(&first);

        StartupSynchronizer ssSecond(resource.Name().c_str());
        ssSecond.Start(&second);

        BOOST_CHECK_EQUAL(resource.Creates(), 1);
        BOOST_CHECK_EQUAL(resource.Uses(), 2);
        BOOST_CHECK_EQUAL(resource.Destroys(), 0);
    }

    //Exactly one Destroy, from whichever instance happened to be last out.
    BOOST_CHECK_EQUAL(resource.Destroys(), 1);
    BOOST_CHECK_EQUAL(resource.Violations(), "");
}

BOOST_AUTO_TEST_CASE(destroy_waits_for_the_last_instance)
{
    Resource resource("SS_UNIT_last_out");
    Instance first(resource);
    Instance second(resource);

    StartupSynchronizer ssFirst(resource.Name().c_str());
    ssFirst.Start(&first);

    {
        StartupSynchronizer ssSecond(resource.Name().c_str());
        ssSecond.Start(&second);
    }

    //The second instance is gone, but the first one is still using it.
    BOOST_CHECK_EQUAL(resource.Destroys(), 0);
    BOOST_CHECK(resource.Alive());
    BOOST_CHECK(Exists(resource.Name(), CreatedMarkerSuffix));
    BOOST_CHECK_EQUAL(resource.Violations(), "");
}

BOOST_AUTO_TEST_CASE(each_generation_gets_its_own_create)
{
    //A long lived process that starts and stops using a resource repeatedly must
    //get a fresh Create every time, and must not accumulate anything.
    Resource resource("SS_UNIT_generations");

    const int generations = 20;
    for (int i = 0; i < generations; ++i)
    {
        Instance instance(resource);
        StartupSynchronizer ss(resource.Name().c_str());
        ss.Start(&instance);
        BOOST_CHECK(resource.Alive());
    }

    BOOST_CHECK_EQUAL(resource.Creates(), generations);
    BOOST_CHECK_EQUAL(resource.Uses(), generations);
    BOOST_CHECK_EQUAL(resource.Destroys(), generations);
    BOOST_CHECK_EQUAL(resource.Abandoned(), 0);
    BOOST_CHECK_EQUAL(resource.Violations(), "");
}

BOOST_AUTO_TEST_CASE(an_instance_that_outlives_another_can_restart_the_resource)
{
    //This used to be the worst failure mode there was: the inner instance
    //destroyed the resource and deleted the lock files, and the outer instance
    //then ran the whole protocol on file descriptors pointing at deleted files,
    //where it excluded nobody at all.
    Resource resource("SS_UNIT_outlive");
    Instance outer(resource);
    Instance inner(resource);

    StartupSynchronizer ssOuter(resource.Name().c_str());

    {
        StartupSynchronizer ssInner(resource.Name().c_str());
        ssInner.Start(&inner);
    }

    BOOST_CHECK_EQUAL(resource.Creates(), 1);
    BOOST_CHECK_EQUAL(resource.Destroys(), 1);
    BOOST_CHECK(!resource.Alive());

    //The outer instance was constructed before all that happened, and shares the
    //same internals. Starting it now has to produce a clean new generation.
    ssOuter.Start(&outer);

    BOOST_CHECK_EQUAL(resource.Creates(), 2);
    BOOST_CHECK_EQUAL(resource.Uses(), 2);
    BOOST_CHECK(resource.Alive());
    BOOST_CHECK(Exists(resource.Name(), CreatedMarkerSuffix));
    BOOST_CHECK_EQUAL(resource.Violations(), "");
}

BOOST_AUTO_TEST_CASE(a_stale_marker_from_a_killed_generation_is_replaced)
{
    //If every process using a resource is killed, nobody gets to run Destroy and
    //the marker file is left behind. The next process along is alone in the
    //system, so it has to create a fresh generation rather than start using
    //whatever the dead one left.
    Resource resource("SS_UNIT_stale_marker");

    {
        boost::filesystem::ofstream file(FilePath(resource.Name(), CreatedMarkerSuffix));
        BOOST_REQUIRE(file.good());
    }
    BOOST_REQUIRE(Exists(resource.Name(), CreatedMarkerSuffix));

    Instance instance(resource);
    StartupSynchronizer ss(resource.Name().c_str());
    ss.Start(&instance);

    BOOST_CHECK_EQUAL(resource.Creates(), 1);
    BOOST_CHECK_EQUAL(resource.Uses(), 1);
    BOOST_CHECK_EQUAL(resource.Violations(), "");
}

BOOST_AUTO_TEST_CASE(an_exception_from_create_propagates_and_leaves_nothing_behind)
{
    //This used to abort the process: the failed Start left an internal lock
    //object in a state where the destructor threw, from a destructor.
    Resource resource("SS_UNIT_create_throws");

    {
        Instance failing(resource);
        failing.inCreate = []{throw std::runtime_error("create failed on purpose");};

        StartupSynchronizer ss(resource.Name().c_str());
        BOOST_CHECK_THROW(ss.Start(&failing), std::runtime_error);

        //No Use, and in particular no Destroy: an instance that never got to use
        //the resource must not be asked to tear it down.
        BOOST_CHECK_EQUAL(resource.Creates(), 0);
        BOOST_CHECK_EQUAL(resource.Uses(), 0);
        BOOST_CHECK_EQUAL(resource.Destroys(), 0);
        BOOST_CHECK(!Exists(resource.Name(), CreatedMarkerSuffix));
    }

    //Nothing was left locked, so the next attempt has to work normally.
    Instance working(resource);
    StartupSynchronizer ss(resource.Name().c_str());
    ss.Start(&working);

    BOOST_CHECK_EQUAL(resource.Creates(), 1);
    BOOST_CHECK_EQUAL(resource.Uses(), 1);
    BOOST_CHECK_EQUAL(resource.Violations(), "");
}

BOOST_AUTO_TEST_CASE(an_exception_from_use_propagates_without_a_destroy_callback)
{
    Resource resource("SS_UNIT_use_throws");

    {
        Instance failing(resource);
        failing.inUse = []{throw std::runtime_error("use failed on purpose");};

        StartupSynchronizer ss(resource.Name().c_str());
        BOOST_CHECK_THROW(ss.Start(&failing), std::runtime_error);

        //The resource was created, but this instance never used it, so it must not
        //be asked to destroy it either.
        BOOST_CHECK_EQUAL(resource.Creates(), 1);
        BOOST_CHECK_EQUAL(resource.Uses(), 0);
        BOOST_CHECK_EQUAL(resource.Destroys(), 0);
    }

    //And we must not be left holding the resource: if we were, no process
    //anywhere could ever destroy this generation again. The next attempt has to
    //be able to take it over, which it can only do if we let go.
    Instance working(resource);
    StartupSynchronizer ss(resource.Name().c_str());
    ss.Start(&working);

    BOOST_CHECK_EQUAL(resource.Uses(), 1);

    //The generation the failing instance created was left behind for us to clean
    //up, which is the documented price of a Use callback that throws.
    BOOST_CHECK_EQUAL(resource.Abandoned(), 1);
    BOOST_CHECK_EQUAL(resource.Violations(), "");
}

BOOST_AUTO_TEST_CASE(misuse_is_reported_instead_of_corrupting_the_bookkeeping)
{
    Resource resource("SS_UNIT_misuse");
    Instance instance(resource);
    Instance other(resource);

    StartupSynchronizer ss(resource.Name().c_str());

    BOOST_CHECK_THROW(ss.Start(nullptr), std::logic_error);

    ss.Start(&instance);

    //A second Start on the same instance used to leave the first pointer
    //registered forever, which quietly prevented the Destroy callback.
    BOOST_CHECK_THROW(ss.Start(&other), std::logic_error);
}

BOOST_AUTO_TEST_CASE(unusable_names_are_rejected)
{
    //The name ends up in file names, so it has to be checked. It used to be
    //passed straight through to the file system and to the semaphore API.
    BOOST_CHECK_THROW(StartupSynchronizer(nullptr), std::logic_error);
    BOOST_CHECK_THROW(StartupSynchronizer(""), std::logic_error);
    BOOST_CHECK_THROW(StartupSynchronizer("with/separator"), std::logic_error);
    BOOST_CHECK_THROW(StartupSynchronizer("with\\separator"), std::logic_error);
    BOOST_CHECK_THROW(StartupSynchronizer(".hidden"), std::logic_error);
    BOOST_CHECK_THROW(StartupSynchronizer(std::string(500, 'x').c_str()), std::logic_error);

    //And a name that is merely long is fine.
    Resource resource("SS_UNIT_long_name_" + std::string(130, 'x'));
    Instance instance(resource);
    StartupSynchronizer ss(resource.Name().c_str());
    ss.Start(&instance);
    BOOST_CHECK_EQUAL(resource.Creates(), 1);
}

BOOST_AUTO_TEST_CASE(constructing_and_destroying_from_many_threads_is_safe)
{
    //The internals are shared between all instances for a name in a process, and
    //the table that hands them out used to be able to lose track of one, which
    //ended in a terminate() from a destructor. Several threads churning through
    //short lived instances is what it takes to hit that.
    const int threadCount = 4;
    const int iterations = 20000;

    std::vector<std::thread> threads;
    std::atomic<int> failures(0);
    for (int t = 0; t < threadCount; ++t)
    {
        threads.emplace_back([&failures]
        {
            try
            {
                for (int i = 0; i < iterations; ++i)
                {
                    StartupSynchronizer ss("SS_UNIT_churn");
                }
            }
            catch (...)
            {
                ++failures;
            }
        });
    }
    for (std::vector<std::thread>::iterator it = threads.begin(); it != threads.end(); ++it)
    {
        it->join();
    }

    BOOST_CHECK_EQUAL(failures.load(), 0);
}

BOOST_AUTO_TEST_CASE(many_threads_starting_and_stopping_the_same_resource)
{
    //Every thread takes the resource, holds it for a moment and lets go again, so
    //the resource is created and destroyed over and over while other threads are
    //joining and leaving. The Resource invariants are what is really being
    //tested: nobody may create a resource that is already alive, and nobody may
    //use one that is not.
    Resource resource("SS_UNIT_thread_churn");

    const int threadCount = 8;
    const int iterations = 200;

    std::vector<std::thread> threads;
    std::atomic<int> failures(0);
    for (int t = 0; t < threadCount; ++t)
    {
        threads.emplace_back([&resource,&failures]
        {
            try
            {
                for (int i = 0; i < iterations; ++i)
                {
                    Instance instance(resource);
                    StartupSynchronizer ss(resource.Name().c_str());
                    ss.Start(&instance);
                }
            }
            catch (...)
            {
                ++failures;
            }
        });
    }
    for (std::vector<std::thread>::iterator it = threads.begin(); it != threads.end(); ++it)
    {
        it->join();
    }

    BOOST_CHECK_EQUAL(failures.load(), 0);
    BOOST_CHECK_EQUAL(resource.Violations(), "");
    BOOST_CHECK_EQUAL(resource.Uses(), threadCount * iterations);
    BOOST_CHECK_EQUAL(resource.Creates(), resource.Destroys());
    BOOST_CHECK_EQUAL(resource.Abandoned(), 0);
    BOOST_CHECK(!resource.Alive());
}
