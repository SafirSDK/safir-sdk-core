/******************************************************************************
*
* Copyright Saab AB, 2007-2013,2015 (http://safir.sourceforge.net)
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
#include <boost/thread/once.hpp>
#include <boost/thread/mutex.hpp>
#include <boost/thread/locks.hpp>
#include <boost/bind.hpp>
#include <boost/weak_ptr.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/convenience.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/interprocess/sync/named_semaphore.hpp>
#include <boost/interprocess/sync/file_lock.hpp>
#include <boost/interprocess/sync/scoped_lock.hpp>
#include <boost/interprocess/sync/sharable_lock.hpp>
#include <iostream>
#include <set>

/* A tip to anyone trying to understand this code:
 * Read up on the boost interprocess file locks, and understand their
 * limitations. Also understand the posix lifetime of locking primitives.
 * Some recommended reading:
 * http://en.wikipedia.org/wiki/File_locking
 * http://www.boost.org/doc/libs/ (select interprocess and read *all* you
 * can find about file locks, in particular the "Caution: synchronization
 * limitations" section.
 * A lot of the complexity stems from the lack of threading guarantees
 * and upgrade/downgrade functionality in the file locks.
 */


namespace
{
    /** Get the directory where lock files are kept */
    const boost::filesystem::path GetLockfileDirectory()
    {
        using namespace boost::filesystem;

        Safir::Utilities::Internal::ConfigReader config;
        const path dir(config.Locations().get<std::string>("lock_file_directory"));

        if (!exists(dir))
        {
            try
            {
                create_directories(dir);
            }
            catch (...)
            {
                std::ostringstream ostr;
                ostr << "Failed to create directory '" << dir.string() << "'" << std::endl;
                throw std::logic_error(ostr.str());
            }
        }
        else if (!is_directory(dir))
        {
            std::ostringstream ostr;
            ostr << "The lluf lockfile directory does not appear to be a directory. dir = '" << dir.string() << "'" << std::endl;
            throw std::logic_error(ostr.str());
        }
        return dir;
    }

    /**
     * Construct a full path to a lock file to use for a particular resource name
     * Also do some sanity checks that will check if it can be used.
     */
    const boost::filesystem::path GetLockFile(const std::string& name)
    {
        using namespace boost::filesystem;
        const path filename = GetLockfileDirectory() / name;

        if (exists(filename))
        {
            if (!is_regular_file(filename))
            {
                std::ostringstream ostr;
                ostr << "The lockfile does not appear to be a regular file. filename = '" << filename.string() << "'" << std::endl;
                throw std::logic_error(ostr.str());
            }
        }

        boost::filesystem::ofstream file(filename);
        if (!file.good())
        {
            std::ostringstream ostr;
            ostr << "Failed to open the lockfile '" << filename.string() << "'." << std::endl;
            throw std::logic_error(ostr.str());
        }

        return filename;
    }
}


namespace Safir
{
namespace Utilities
{

    class StartupSynchronizerImpl
    {
        friend class ImplKeeper;
    private: //constructor and destructor are private, to make sure only ImplKeeper can destroy them
        explicit StartupSynchronizerImpl(const std::string& uniqueName)
            : m_name(uniqueName)
            , m_firstLockFilePath(GetLockFile(m_name + "_FIRST"))
            , m_secondLockFilePath(GetLockFile(m_name + "_SECOND"))
            , m_firstLock(m_firstLockFilePath.string().c_str())
            , m_secondLock(m_secondLockFilePath.string().c_str())
        {
        }

        ~StartupSynchronizerImpl()
        {
            if (!m_synchronized.empty())
            {
                throw std::logic_error("Unexpectedly found some stuff in m_synchronized");
            }
        }
    public:
        const std::string& Name() const {return m_name;}

        void Start(Synchronized* const synchronized)
        {
            boost::lock_guard<boost::mutex> lck(m_threadLock);

            m_synchronized.insert(synchronized);

            if (m_synchronized.size() == 1)
            {
                FirstStart(synchronized);
                return;
            }

            //invoke user callback
            synchronized->Use();
        }

        void FirstStart(Synchronized* const synchronized)
        {
            //try to take the first lock exclusively, if we get it we may be allowed to call Create
            //                const bool gotLock = m_fileLock->try_lock();
            m_firstExclusiveLock = boost::interprocess::scoped_lock<boost::interprocess::file_lock>
                (m_firstLock,boost::interprocess::try_to_lock);
            if (m_firstExclusiveLock.owns())
            {
                //Ok, got the first lock. Now see if we can get the second lock
                m_secondExclusiveLock = boost::interprocess::scoped_lock<boost::interprocess::file_lock>
                    (m_secondLock,boost::interprocess::try_to_lock);
                if (m_secondExclusiveLock.owns())
                {
                    //We got it! We're allowed to start initializing everything

                    //remove any semaphore left over from previous instances
                    boost::interprocess::named_semaphore::remove(m_name.c_str());

                    //invoke the user callback
                    synchronized->Create();

                    //Create a new semaphore so that users know if we succeeded or not.
                    boost::interprocess::named_semaphore sem(boost::interprocess::create_only,m_name.c_str(),1);
                }

                m_firstExclusiveLock.unlock();
            }

            //Wait for the exclusive lock to be released (or, if we're the Creator, just get the shared lock)
            //this will cause everyone to wait for Create to have completed successfully.
            m_firstSharableLock = boost::interprocess::sharable_lock<boost::interprocess::file_lock>
                (m_firstLock);

            try
            {
                //Try to *open* the semaphore. If it has not been created this will cause an exception
                //to be thrown.
                boost::interprocess::named_semaphore sem(boost::interprocess::open_only,m_name.c_str());

                sem.wait();
                sem.post();
            }
            catch (const boost::interprocess::interprocess_exception& exc)
            {
                std::ostringstream ostr;
                ostr << "It appears that Create failed in some other process for '"
                     << m_name << "'." << std::endl;
                ostr << "The exception I got was " << exc.what()
                     << " and strerror(errno) returned this info: "
                     << strerror(errno) << std::endl;
                std::wcerr << ostr.str().c_str() << std::flush;
                throw std::logic_error(ostr.str());
            }

            //invoke user callback
            synchronized->Use();
        }

        void Remove(Synchronized* const synchronized)
        {
            boost::lock_guard<boost::mutex> lck(m_threadLock);
            std::multiset<Synchronized*>::iterator findIt = m_synchronized.find(synchronized);
            if (findIt == m_synchronized.end())
            {
                throw std::logic_error("Unexpectedly failed to find synchronized in m_synchronized");
            }

            m_synchronized.erase(findIt);

            if (!m_synchronized.empty())
            {
                return;
            }

            //synchronized is now a candidate for calling Destroy on (if we're the last process)


            //Try to get the inner lock, if we do not already have it
            if (!m_secondExclusiveLock.owns())
            {
                m_secondExclusiveLock = boost::interprocess::scoped_lock<boost::interprocess::file_lock>
                    (m_secondLock,boost::interprocess::try_to_lock);
            }
            if (!m_secondExclusiveLock.owns())
            {
                //someone else is holding the second lock, we're not going to be
                //the last instance out...
                return;
            }

            //Note: What happens after here is not analyzed properly. So if any bug occurs
            //it might pay off to look at this code.

            //try to upgrade the first lock (no explicit upgrade path, which is why we have the second lock)
            m_firstSharableLock.unlock();
            m_firstExclusiveLock = boost::interprocess::scoped_lock<boost::interprocess::file_lock>
                (m_firstLock,boost::interprocess::try_to_lock);

            if (m_firstExclusiveLock.owns())
            {
                // Got exclusive lock, noone else has it open, so I can remove everything!
                synchronized->Destroy();

                boost::interprocess::named_semaphore::remove(m_name.c_str());
                boost::filesystem::remove(m_firstLockFilePath);
                boost::filesystem::remove(m_secondLockFilePath);
            }
        }

    private:

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4251)
#endif

        std::multiset<Synchronized*> m_synchronized;
        const std::string m_name;

        const boost::filesystem::path m_firstLockFilePath;
        const boost::filesystem::path m_secondLockFilePath;

        //The idea is to take these locks in a specific fashion, and to never
        //release them. They get released when the process exits.
        boost::mutex m_threadLock;
        boost::interprocess::file_lock m_firstLock;
        boost::interprocess::file_lock m_secondLock;

        boost::interprocess::scoped_lock<boost::interprocess::file_lock> m_firstExclusiveLock;
        boost::interprocess::sharable_lock<boost::interprocess::file_lock> m_firstSharableLock;
        boost::interprocess::scoped_lock<boost::interprocess::file_lock> m_secondExclusiveLock;

#ifdef _MSC_VER
#pragma warning(pop)
#endif

    };


    /**
     * A singleton that holds all the impl:s
     * Note that all instances for the same name in the same process
     * shares the same impl! This is to make it possible to have
     * thread (as opposed to process) guarantees!
     * The kept references are weak pointers! So this singleton will
     * never keep an impl "alive" on its own.
     */
    class ImplKeeper
    {
    public:
        static ImplKeeper& Instance()
        {
            boost::call_once(SingletonHelper::m_onceFlag,boost::bind(SingletonHelper::Instance));
            return SingletonHelper::Instance();
        }

        const boost::shared_ptr<StartupSynchronizerImpl> Get(const std::string& uniqueName)
        {
            boost::lock_guard<boost::mutex> lck(m_lock);
            Table::iterator findIt = m_table.find(uniqueName);
            if (findIt != m_table.end())
            {
                boost::shared_ptr<StartupSynchronizerImpl> impl = findIt->second.lock();
                if (impl != NULL)
                {
                    return impl;
                }
                else
                {
                    m_table.erase(findIt);
                }
            }

            //Either not found or had already been deleted. Create a new one
            boost::shared_ptr<StartupSynchronizerImpl> newImpl(new StartupSynchronizerImpl(uniqueName),
                                                               boost::bind(&ImplKeeper::Deleter,this,_1));
            m_table.insert(std::make_pair(uniqueName,boost::weak_ptr<StartupSynchronizerImpl>(newImpl)));
            return newImpl;
        }

    private:
        /** Constructor*/
        ImplKeeper()
        {

        }

        /** Destructor */
        ~ImplKeeper()
        {

        }

        void Deleter(StartupSynchronizerImpl* impl)
        {
            boost::lock_guard<boost::mutex> lck(m_lock);
            Table::iterator findIt = m_table.find(impl->Name());

            if (findIt == m_table.end())
            {
                throw std::logic_error("Something very odd has happened!");
            }

            m_table.erase(findIt);
            delete impl;
        }


        boost::mutex m_lock;

        typedef std::map<std::string,boost::weak_ptr<StartupSynchronizerImpl> > Table;
        Table m_table;


    private:
        /**
         * This class is here to ensure that only the Instance method can get at the
         * instance, so as to be sure that boost call_once is used correctly.
         * Also makes it easier to grep for singletons in the code, if all
         * singletons use the same construction and helper-name.
         */
        struct SingletonHelper
        {
        private:
            friend ImplKeeper& ImplKeeper::Instance();

            static ImplKeeper& Instance()
            {
                static ImplKeeper instance;
                return instance;
            }
            static boost::once_flag m_onceFlag;
        };

    };

    //mandatory static initialization
    boost::once_flag ImplKeeper::SingletonHelper::m_onceFlag = BOOST_ONCE_INIT;




    StartupSynchronizer::StartupSynchronizer(const char* uniqueName)
        : m_impl(ImplKeeper::Instance().Get(uniqueName +
                                            Safir::Utilities::Internal::Expansion::GetSafirInstanceSuffix()))
        , m_synchronized(NULL)
    {

    }

    StartupSynchronizer::~StartupSynchronizer()
    {
        if (m_synchronized != NULL)
        {
            m_impl->Remove(m_synchronized);
        }
    }


    void StartupSynchronizer::Start(Synchronized* const synchronized)
    {
        m_synchronized = synchronized;
        m_impl->Start(synchronized);
    }


}
}
