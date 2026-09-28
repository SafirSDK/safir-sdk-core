/******************************************************************************
*
* Copyright Saab AB, 2007-2013,2015,2026 (http://safirsdkcore.com)
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
#include <Safir/Utilities/StartupSynchronizer.h>
#include <Safir/Utilities/Internal/ConfigReader.h>
#include <Safir/Utilities/Internal/Expansion.h>
#include <boost/filesystem/fstream.hpp>
#include <boost/filesystem/operations.hpp>
#include <chrono>
#include <iostream>
#include <map>
#include <mutex>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>
#include <thread>

#ifdef _MSC_VER
#  pragma warning(push)
#  pragma warning (disable: 4189)
#endif

#include <boost/interprocess/sync/file_lock.hpp>

#ifdef _MSC_VER
#  pragma warning(pop)
#endif


/* A tip to anyone trying to understand this code:
 *
 * The job is to let any number of processes (and threads) agree on who creates a
 * shared resource, to make everyone else wait until it exists, and to let the
 * last user tear it down. It has to survive processes being killed at any point,
 * which is why it is built on file locks: the operating system releases those
 * when a process dies, no matter how it dies. No other cross platform primitive
 * has that property - in particular Boost's named mutexes and named upgradable
 * mutexes do *not*, so a process killed while holding one of those leaves it
 * locked forever.
 *
 * Read up on the Boost.Interprocess file locks, and understand their
 * limitations. Also understand the posix lifetime of locking primitives.
 * Some recommended reading:
 * http://en.wikipedia.org/wiki/File_locking
 * http://www.boost.org/doc/libs/ (select interprocess and read *all* you
 * can find about file locks, in particular the "Caution: synchronization
 * limitations" section.
 *
 * The parts of those semantics that this code actually depends on:
 *
 * - On posix, file locks belong to the *process* and are keyed on the inode, not
 *   on the file descriptor. Two things follow from that. Closing *any* file
 *   descriptor to the file releases *all* locks the process holds on it, which
 *   is why there is exactly one lock object per name per process and why it is
 *   never destroyed (see ImplKeeper). And re-locking a file that the process
 *   already holds a lock on silently *converts* the lock instead of blocking or
 *   failing, which is why we never ask for a lock we may already hold (see
 *   TrackedFileLock).
 * - On Windows, locks belong to the handle and re-locking an overlapping range
 *   fails instead of converting. So the code must work both ways: it always
 *   unlocks explicitly before taking a different kind of lock.
 * - Locks on a deleted file are useless: they are invisible to anyone who
 *   recreates the file name, since that is a different inode. So the lock files
 *   are created once and *never* deleted. The only file that gets deleted is the
 *   marker file, and nothing ever locks or holds that one open.
 * - There is no portable atomic way to downgrade an exclusive lock to a sharable
 *   one (posix can do it, Windows cannot), so there is always a window where the
 *   creator holds neither. Everything below is built to tolerate that window
 *   rather than to pretend it does not exist.
 *
 * The protocol uses three files per resource, all in the lock file directory. The
 * two lock files keep the names they have always had, since deployed systems may
 * have rules that refer to them; the code calls them the users lock and the
 * generation lock, which is what they are:
 *
 *   <name>_FIRST    The users lock. Sharable lock = "I am using the resource",
 *                   exclusive lock = "nobody is using it". This is the whole
 *                   reason the thing works: the operating system maintains the
 *                   user count for us, so a killed process cannot leak one.
 *   <name>_SECOND   The generation lock. Exclusive lock = "a live generation of
 *                   this resource exists, and I am the process that created it".
 *                   Only ever taken with try_lock, so it can never deadlock. It
 *                   is what stops a second process from creating the resource
 *                   while the creator is inside the downgrade window described
 *                   above, and what stops anyone from destroying a generation
 *                   whose creator is still alive.
 *   <name>_CREATED  Exists = "Create() completed successfully". Waiters need this
 *                   because getting the sharable lock only tells them that the
 *                   creator is no longer holding the exclusive lock, not whether
 *                   it succeeded or died halfway through. This is the only new
 *                   file: it replaces the named semaphore that older versions kept
 *                   outside the lock file directory (/dev/shm/sem.<name> on
 *                   Linux). It cannot live inside one of the lock files, because
 *                   on Windows reading or writing a byte range that we hold a lock
 *                   on fails when it is done through a second handle, and opening
 *                   a second handle at all would drop all our locks on posix.
 *
 * Acquiring is a retry loop, and the reason is worth stating: between the moment
 * a process concludes that the resource exists and the moment it holds the
 * sharable lock that keeps it alive, the last user of that generation may
 * destroy it. That window cannot be closed portably. So instead of failing (the
 * old behaviour, which turned a perfectly normal restart into a hard error) we
 * simply start over, and normally end up creating the next generation ourselves.
 */


namespace
{
    /** How many attempts the acquire protocol makes before giving up.
     *
     * Deliberately a count of attempts and not a deadline: waiting for another
     * process to finish its Create() happens inside a blocking lock call, and
     * that is allowed to take as long as it likes (parsing a whole dou repository
     * or allocating a large shared memory segment can take a while on a loaded
     * machine). Only attempts that got nowhere are counted, so a slow creator can
     * never use up the budget of the processes waiting for it. At RetryInterval
     * each, this is around ten seconds of actual retrying, which is far more than
     * any legitimate sequence of restarts needs. */
    const int MaxAcquireAttempts = 1000;

    /** How long to wait between attempts in the acquire loop. */
    const std::chrono::milliseconds RetryInterval(10);

    /** How long we wait for the users lock before saying out loud that we are
     * still waiting.
     *
     * The wait itself is legitimate and has no bound: the process holding the lock
     * exclusively is inside its Create or Destroy callback, and neither of those is
     * ours to put a limit on. What is not acceptable is waiting in silence, which
     * makes a stuck system indistinguishable from a slow one - and that is a much
     * harder thing to diagnose afterwards than it is to report as it happens. */
    const std::chrono::seconds LockWaitReportInterval(30);

    /** How often we re-test a lock we are waiting for. Frequent enough that nobody
     * notices the latency, rare enough that a long legitimate wait costs nothing. */
    const std::chrono::milliseconds LockPollInterval(20);

    /** Suffixes of the three files.
     *
     * The two lock file names are historical and deliberately left alone: they
     * have been on disk under these names for many years, and hardening rules in
     * deployed systems may well refer to them. The code below calls them the users
     * lock and the generation lock, which is what they actually are.
     *
     * None of these suffixes may be a suffix of another one, or two resources
     * whose names differ by a suffix could end up sharing a file. */
    const char* const UsersLockSuffix = "_FIRST";
    const char* const GenerationLockSuffix = "_SECOND";
    const char* const CreatedMarkerSuffix = "_CREATED";

    /** The longest resource name we accept, leaving room for the suffixes above
     * and the instance suffix inside the usual 255 character file name limit. */
    const std::size_t MaxNameLength = 180;

    /** Get the text of the exception that is currently being handled. Must only
     * be called from inside a catch block. */
    const std::string CurrentExceptionText()
    {
        try
        {
            throw;
        }
        catch (const std::exception& exc)
        {
            return exc.what();
        }
        catch (...)
        {
            return "an unknown exception";
        }
    }

    /**
     * Check that a name can be used to build file names, and return it.
     * Throws std::logic_error if it cannot.
     */
    const std::string CheckName(const char* const uniqueName)
    {
        if (uniqueName == nullptr)
        {
            throw std::logic_error("StartupSynchronizer: the resource name must not be null");
        }

        const std::string name(uniqueName);

        std::string problem;
        if (name.empty())
        {
            problem = "it is empty";
        }
        else if (name.size() > MaxNameLength)
        {
            std::ostringstream ostr;
            ostr << "it is longer than " << MaxNameLength << " characters";
            problem = ostr.str();
        }
        else if (name.find_first_of("/\\<>:\"|?*") != std::string::npos)
        {
            problem = "it contains one of the characters /\\<>:\"|?*";
        }
        else if (name[0] == '.')
        {
            problem = "it starts with a dot";
        }

        if (!problem.empty())
        {
            std::ostringstream ostr;
            ostr << "StartupSynchronizer: the resource name '" << name
                 << "' cannot be used, since " << problem
                 << ". The name is used to build file names." << std::endl;
            throw std::logic_error(ostr.str());
        }

        return name;
    }

    /** Get the directory where lock files are kept */
    const boost::filesystem::path GetLockfileDirectory()
    {
        using namespace boost::filesystem;

        Safir::Utilities::Internal::ConfigReader config;
        path dir(config.Locations().get<std::string>("lock_file_directory"));

        //Just try to create it. Checking first and creating afterwards would lose
        //a race against all the other processes that start at the same time as we
        //do, which is precisely when this code runs.
        boost::system::error_code ec;
        create_directories(dir, ec);

        if (!is_directory(dir))
        {
            std::ostringstream ostr;
            ostr << "The lluf lockfile directory '" << dir.string()
                 << "' is not a directory and could not be created: " << ec.message() << std::endl;
            throw std::logic_error(ostr.str());
        }
        return dir;
    }

    /** Make a file usable by every user on the machine, like the rest of Safir's
     * lock files. Failing to do so is not fatal: it only matters when several
     * users share a system, and in that case the directory permissions have to
     * allow it too. */
    void MakeFileShared(const boost::filesystem::path& path)
    {
        boost::system::error_code ec;
        boost::filesystem::permissions(path,
                                       boost::filesystem::owner_read | boost::filesystem::owner_write |
                                       boost::filesystem::group_read | boost::filesystem::group_write |
                                       boost::filesystem::others_read | boost::filesystem::others_write,
                                       ec);
    }

    /** Check that a path is usable for our purposes, and return it. */
    const boost::filesystem::path CheckFilePath(const boost::filesystem::path& path)
    {
        if (boost::filesystem::exists(path) && !boost::filesystem::is_regular_file(path))
        {
            std::ostringstream ostr;
            ostr << "The lockfile does not appear to be a regular file. filename = '"
                 << path.string() << "'" << std::endl;
            throw std::logic_error(ostr.str());
        }
        return path;
    }

    /**
     * Make sure one of the lock files exists, without disturbing it if it does.
     *
     * Note the append mode: these files are opened by every process that uses
     * the resource, including while other processes hold locks on them, so
     * truncating them would be a (currently harmless, but pointless) surprise.
     */
    const boost::filesystem::path CreateLockFile(const boost::filesystem::path& path)
    {
        CheckFilePath(path);

        {
            boost::filesystem::ofstream file(path, std::ios::out | std::ios::app);
            if (!file.good())
            {
                std::ostringstream ostr;
                ostr << "Failed to open the lockfile '" << path.string() << "'." << std::endl;
                throw std::logic_error(ostr.str());
            }
        }

        MakeFileShared(path);

        return path;
    }
}


namespace Safir
{
namespace Utilities
{
    /**
     * A file lock together with the state we believe it to be in.
     *
     * The state is not a convenience, it is the point: asking a file lock for a
     * lock that we may already hold means different things on different
     * platforms (posix converts it silently, Windows fails), so we always know
     * what we hold and unlock explicitly before asking for something else.
     *
     * Not thread safe. Callers hold StartupSynchronizerImpl::m_threadLock.
     */
    class TrackedFileLock
    {
    public:
        explicit TrackedFileLock(const boost::filesystem::path& path)
            : m_path(path)
            , m_lock(path.string().c_str())
        {
        }

        /** The file this lock lives on. Only used to say something useful when a
         * wait is taking a long time. */
        const boost::filesystem::path& Path() const {return m_path;}

        /**
         * Try to take the lock exclusively, without waiting.
         * Returns true if it is held exclusively when this returns, which
         * includes the case where we held it that way already.
         */
        bool TryLockExclusive()
        {
            if (m_state == State::Exclusive)
            {
                return true;
            }
            Unlock();
            if (!m_lock.try_lock())
            {
                return false;
            }
            m_state = State::Exclusive;
            return true;
        }

        /**
         * Try to take the lock sharably, giving up after `limit` so that the caller
         * can report the wait and come back for more. Returns true if we hold it
         * sharably when this returns.
         *
         * This polls rather than blocking in the kernel the way lock_sharable()
         * would, which is what buys us the chance to report. The cost is one extra
         * system call per LockPollInterval while waiting, and waiting at all is the
         * rare case.
         */
        bool TimedLockSharable(const std::chrono::steady_clock::duration& limit)
        {
            if (m_state == State::Sharable)
            {
                return true;
            }
            Unlock();

            const std::chrono::steady_clock::time_point deadline =
                std::chrono::steady_clock::now() + limit;
            for (;;)
            {
                if (m_lock.try_lock_sharable())
                {
                    m_state = State::Sharable;
                    return true;
                }
                if (std::chrono::steady_clock::now() >= deadline)
                {
                    return false;
                }
                std::this_thread::sleep_for(LockPollInterval);
            }
        }

        /** Release whatever we hold, if anything. Never throws. */
        void Unlock() noexcept
        {
            const State was = m_state;

            //Record the new state first: if the unlock fails there is nothing
            //useful we can do about it, and believing that we still hold the
            //lock would only make us skip the next unlock too.
            m_state = State::Unlocked;

            try
            {
                if (was == State::Exclusive)
                {
                    m_lock.unlock();
                }
                else if (was == State::Sharable)
                {
                    m_lock.unlock_sharable();
                }
            }
            catch (...)
            {
                //Cannot happen for a valid file handle, and if it does happen
                //the operating system still releases everything when we exit.
            }
        }

    private:
        TrackedFileLock(const TrackedFileLock&) = delete;
        TrackedFileLock& operator=(const TrackedFileLock&) = delete;

        enum class State {Unlocked, Sharable, Exclusive};

        const boost::filesystem::path m_path;
        boost::interprocess::file_lock m_lock;
        State m_state = State::Unlocked;
    };


    class StartupSynchronizerImpl
    {
    public:
        explicit StartupSynchronizerImpl(const std::string& uniqueName)
            : StartupSynchronizerImpl(uniqueName, GetLockfileDirectory())
        {
        }

        /**
         * Destructor. Never invoked: the impls are owned by ImplKeeper, which
         * deliberately keeps them (and their open file descriptors) for the
         * lifetime of the process. See ImplKeeper for why.
         */
        ~StartupSynchronizerImpl()
        {
        }

        /**
         * Start using the resource, creating it if we turn out to be the one who
         * has to. Throws if the resource could not be acquired, in which case
         * nothing has been registered and no callbacks are owed.
         */
        void Start(Synchronized* const synchronized)
        {
            std::lock_guard<std::mutex> lck(m_threadLock);

            if (!m_acquired)
            {
                Acquire(synchronized);
            }

            //Registered before the callback, so that the bookkeeping stays
            //correct if Use() throws.
            m_synchronized.insert(synchronized);
            try
            {
                synchronized->Use();
            }
            catch (...)
            {
                m_synchronized.erase(m_synchronized.find(synchronized));

                if (m_synchronized.empty())
                {
                    //Nobody in this process is using the resource, so let go of
                    //it again - otherwise our sharable lock would stop anyone,
                    //anywhere, from ever destroying this generation. Note that
                    //there is deliberately no Destroy callback: this instance
                    //never had a successful Use, so it must not be asked to tear
                    //the resource down. What we leave behind is exactly what a
                    //process killed at this point would leave behind, and the
                    //next process to come along recreates it.
                    m_acquired = false;
                    ReleaseLocks();
                }
                throw;
            }
        }

        /**
         * Stop using the resource, and destroy it if we were the last user
         * anywhere. Called from a destructor, so this never throws.
         */
        void Remove(Synchronized* const synchronized) noexcept
        {
            try
            {
                std::lock_guard<std::mutex> lck(m_threadLock);

                const std::multiset<Synchronized*>::iterator findIt = m_synchronized.find(synchronized);
                if (findIt == m_synchronized.end())
                {
                    //Cannot happen: StartupSynchronizer only calls us for
                    //instances whose Start succeeded.
                    return;
                }
                m_synchronized.erase(findIt);

                if (!m_synchronized.empty() || !m_acquired)
                {
                    return;
                }

                Release(synchronized);
            }
            catch (...)
            {
                //There is nobody to report to from inside a destructor, and
                //throwing would terminate the process.
            }
        }

    private:
        StartupSynchronizerImpl(const StartupSynchronizerImpl&) = delete;
        StartupSynchronizerImpl& operator=(const StartupSynchronizerImpl&) = delete;

        /** Delegated to so that the lock file directory is only looked up once. */
        StartupSynchronizerImpl(const std::string& uniqueName, const boost::filesystem::path& directory)
            : m_name(uniqueName)
            , m_markerFilePath(CheckFilePath(directory / (uniqueName + CreatedMarkerSuffix)))
            , m_usersLock(CreateLockFile(directory / (uniqueName + UsersLockSuffix)))
            , m_generationLock(CreateLockFile(directory / (uniqueName + GenerationLockSuffix)))
        {
        }

        /**
         * Run the acquire protocol until we hold the sharable users lock and know
         * that the resource exists. Calls Create() if we are the one who gets to
         * create this generation.
         */
        void Acquire(Synchronized* const synchronized)
        {
            std::string lastProblem;

            for (int attempt = 1; ; ++attempt)
            {
                //Anything that goes wrong once Create() has been called must not
                //be retried, since that would call it twice for one generation.
                bool createInvoked = false;
                try
                {
                    if (m_usersLock.TryLockExclusive() && m_generationLock.TryLockExclusive())
                    {
                        //Nobody is using the resource and no other process claims
                        //to have created it, so anything lying around is debris
                        //from a generation that died. Remove the marker before
                        //creating, so that a failure here cannot leave something
                        //behind that claims success.
                        RemoveMarker();

                        createInvoked = true;
                        synchronized->Create();

                        CreateMarker();
                    }
                    else
                    {
                        //Someone else owns this generation, so we must not keep
                        //claiming responsibility for it.
                        m_generationLock.Unlock();
                    }

                    //Take the lock that says "I am using this". Note that we
                    //always drop the exclusive lock first: the atomic downgrade
                    //that posix offers has no Windows equivalent, so we use the
                    //same code path on both platforms and check below whether we
                    //lost the resource in the window.
                    m_usersLock.Unlock();
                    WaitForUsersLock();

                    if (MarkerExists())
                    {
                        m_acquired = true;
                        return;
                    }

                    if (createInvoked)
                    {
                        //We created the resource, recorded that we had, and the
                        //record is gone again already. Nobody taking part in this
                        //protocol can have removed it, since that needs the
                        //generation lock and we are holding it - so something
                        //outside Safir is deleting files from the lock file
                        //directory. Retrying would call Create() a second time
                        //for one generation, which is exactly what this class
                        //promises never to do, so report it instead.
                        ReleaseLocks();

                        std::ostringstream ostr;
                        ostr << "Created the shared resource '" << m_name
                             << "', but the marker file '" << m_markerFilePath.string()
                             << "' disappeared immediately. Something outside Safir appears to be "
                             << "removing files from the lock file directory." << std::endl;
                        std::wcerr << ostr.str().c_str() << std::flush;
                        throw std::logic_error(ostr.str());
                    }

                    //Either the process that was creating the resource died
                    //halfway through, or the last user of the generation we were
                    //about to join destroyed it while we held no lock. Both are
                    //normal, and both are fixed by starting over: next time round
                    //we will most likely create it ourselves.
                    lastProblem = "the resource was destroyed, or its creator died, while we were acquiring it";
                    m_usersLock.Unlock();
                }
                catch (...)
                {
                    lastProblem = CurrentExceptionText();
                    ReleaseLocks();

                    if (createInvoked)
                    {
                        //Create() threw, or we could not record that it
                        //succeeded. Either way this is the caller's problem, and
                        //retrying would call Create() a second time.
                        throw;
                    }

                    //A lock operation failed. On some platforms and older Boost
                    //versions an ordinary signal is enough to do that, so treat
                    //it as temporary and try again.
                }

                if (attempt >= MaxAcquireAttempts)
                {
                    ReleaseLocks();

                    std::ostringstream ostr;
                    ostr << "Gave up trying to acquire the shared resource '" << m_name
                         << "' after " << attempt
                         << " attempts. The last problem was: " << lastProblem << std::endl;
                    std::wcerr << ostr.str().c_str() << std::flush;
                    throw std::logic_error(ostr.str());
                }

                std::this_thread::sleep_for(RetryInterval);
            }
        }

        /**
         * Take the sharable users lock, waiting for as long as it takes but saying
         * so while we do.
         *
         * Note that this deliberately does not give up, and does not consume the
         * acquire attempt budget: whoever holds the lock exclusively is inside a
         * Create or Destroy callback, which can legitimately take a long time (a
         * whole dou repository gets parsed, or a large shared memory segment gets
         * built), and turning that into a startup failure would be worse than
         * waiting. All we add is that the waiting is visible.
         */
        void WaitForUsersLock()
        {
            std::chrono::seconds waited(0);
            while (!m_usersLock.TimedLockSharable(LockWaitReportInterval))
            {
                waited += LockWaitReportInterval;

                std::ostringstream ostr;
                ostr << "Still waiting to start using the shared resource '" << m_name
                     << "' after " << waited.count() << " seconds. Another process holds '"
                     << m_usersLock.Path().string() << "' exclusively, which means it is "
                     << "inside its Create or Destroy callback for this resource."
                     << std::endl;
                std::wcerr << ostr.str().c_str() << std::flush;
            }
        }

        /**
         * Let go of the resource, destroying it if nobody else is using it.
         * Precondition: we hold the sharable users lock and no instance in this
         * process is using the resource any more.
         */
        void Release(Synchronized* const synchronized) noexcept
        {
            m_acquired = false;

            try
            {
                //If another process created this generation and is still around,
                //it is responsible for it, not us. This is also what keeps us
                //from destroying a generation whose creator is still inside the
                //window where it holds no users lock.
                if (m_generationLock.TryLockExclusive())
                {
                    //Give up our own claim on the resource before asking whether
                    //anyone else still has one, since ours would count.
                    m_usersLock.Unlock();

                    if (m_usersLock.TryLockExclusive() && MarkerExists())
                    {
                        //Nobody else holds the users lock, so there are no users
                        //left anywhere and we may tear the resource down. Losing
                        //that race instead is perfectly fine: it means somebody
                        //is using the resource, so it has to stay.

                        //Marker first: if we are killed inside Destroy(), the
                        //next process along has to create a fresh generation
                        //rather than start using a half destroyed one.
                        RemoveMarker();
                        synchronized->Destroy();
                    }
                }
            }
            catch (...)
            {
                //Destroy() threw, or a lock or file operation failed. Nothing to
                //report to (we are called from a destructor) and nothing to do:
                //releasing the locks below leaves the resource in a state that
                //the next process can recover from, which is the same state a
                //killed process would leave it in.
            }

            ReleaseLocks();
        }

        void ReleaseLocks() noexcept
        {
            m_usersLock.Unlock();
            m_generationLock.Unlock();
        }

        /** Does the resource exist, i.e. did some Create() complete? */
        bool MarkerExists() const
        {
            //Deliberately not using the error_code overload: if we cannot even
            //find out whether the marker is there we must not guess that it is
            //missing, since that would make us recreate a resource that may be
            //perfectly fine.
            return boost::filesystem::exists(m_markerFilePath);
        }

        void CreateMarker()
        {
            {
                boost::filesystem::ofstream file(m_markerFilePath, std::ios::out | std::ios::trunc);
                if (!file.good())
                {
                    std::ostringstream ostr;
                    ostr << "Failed to create the marker file '" << m_markerFilePath.string()
                         << "' for the shared resource '" << m_name << "'." << std::endl;
                    throw std::logic_error(ostr.str());
                }
            }
            MakeFileShared(m_markerFilePath);
        }

        void RemoveMarker() noexcept
        {
            boost::system::error_code ec;
            boost::filesystem::remove(m_markerFilePath, ec);
        }

        std::mutex m_threadLock;

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4251)
#endif

        std::multiset<Synchronized*> m_synchronized;
        const std::string m_name;
        const boost::filesystem::path m_markerFilePath;

        //The lock files are created once and never deleted, and these objects
        //keep them open for the lifetime of the process. See the comment at the
        //top of this file for why both of those matter.
        TrackedFileLock m_usersLock;
        TrackedFileLock m_generationLock;

#ifdef _MSC_VER
#pragma warning(pop)
#endif

        /** Are we currently using the resource, i.e. holding the sharable users
         * lock with the resource known to exist? */
        bool m_acquired = false;
    };


    /**
     * A singleton that holds all the impl:s
     * Note that all instances for the same name in the same process
     * shares the same impl! This is to make it possible to have
     * thread (as opposed to process) guarantees!
     *
     * Neither this singleton nor the impls in it are ever destroyed, and that is
     * not laziness:
     *
     * - On posix, closing any file descriptor to a file releases every lock the
     *   process holds on that file. Creating a second impl for a name that an
     *   existing impl still holds locks on would therefore silently unlock the
     *   first one, and the protocol would quietly stop working. Keeping exactly
     *   one impl per name for the lifetime of the process makes that impossible.
     * - It also means that a StartupSynchronizer can safely outlive every other
     *   user of its name, and that a StartupSynchronizer which is itself destroyed
     *   during static destruction cannot find a half destroyed table.
     *
     * The cost is one small object and two file descriptors per resource name
     * that the process has ever used, which is a handful. Note that leak checkers
     * report this as still reachable rather than lost, since the singleton is
     * reachable from a pointer that lives as long as the process does.
     */
    class ImplKeeper
    {
    public:
        static ImplKeeper& Instance()
        {
            std::call_once(SingletonHelper::m_onceFlag,[]{SingletonHelper::Instance();});
            return SingletonHelper::Instance();
        }

        StartupSynchronizerImpl& Get(const std::string& uniqueName)
        {
            std::lock_guard<std::mutex> lck(m_lock);
            Table::iterator findIt = m_table.find(uniqueName);
            if (findIt == m_table.end())
            {
                //Deliberately never deleted, see the class documentation.
                StartupSynchronizerImpl* const impl = new StartupSynchronizerImpl(uniqueName);
                findIt = m_table.insert(std::make_pair(uniqueName, impl)).first;
            }
            return *findIt->second;
        }

    private:
        /** Constructor*/
        ImplKeeper()
        {

        }

        /** Destructor. Never invoked, see the class documentation. */
        ~ImplKeeper()
        {

        }

        std::mutex m_lock;

        typedef std::map<std::string, StartupSynchronizerImpl*> Table;
        Table m_table;


    private:
        /**
         * This class is here to ensure that only the Instance method can get at the
         * instance, so as to be sure that call_once is used correctly.
         * Also makes it easier to grep for singletons in the code, if all
         * singletons use the same construction and helper-name.
         */
        struct SingletonHelper
        {
        private:
            friend ImplKeeper& ImplKeeper::Instance();

            static ImplKeeper& Instance()
            {
                //Deliberately never deleted, see the class documentation.
                static ImplKeeper* const instance = new ImplKeeper();
                return *instance;
            }
            static std::once_flag m_onceFlag;
        };

    };

    //mandatory static initialization
    std::once_flag ImplKeeper::SingletonHelper::m_onceFlag;




    StartupSynchronizer::StartupSynchronizer(const char* uniqueName)
        : m_impl(&ImplKeeper::Instance().Get(CheckName(uniqueName) +
                                             Safir::Utilities::Internal::Expansion::GetSafirInstanceSuffix()))
        , m_synchronized(nullptr)
    {

    }

    StartupSynchronizer::~StartupSynchronizer()
    {
        if (m_synchronized != nullptr)
        {
            m_impl->Remove(m_synchronized);
        }
    }


    void StartupSynchronizer::Start(Synchronized* const synchronized)
    {
        if (synchronized == nullptr)
        {
            throw std::logic_error("StartupSynchronizer::Start was called with a null pointer");
        }
        if (m_synchronized != nullptr)
        {
            throw std::logic_error("StartupSynchronizer::Start may only be called once per instance");
        }

        //Only remembered once we know it worked, so that a failed Start owes no
        //Destroy callback.
        m_impl->Start(synchronized);
        m_synchronized = synchronized;
    }

}
}
