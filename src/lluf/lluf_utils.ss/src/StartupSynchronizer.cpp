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
#include <Safir/Utilities/StartupSynchronizer.h>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/convenience.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/interprocess/sync/named_semaphore.hpp>
#include <boost/thread/once.hpp>
#include <boost/bind.hpp>
#include <cstdio>
#include <iostream>

namespace
{
    class FileLockKeeper
        : private boost::noncopyable
    {
    public:
        static FileLockKeeper& Instance();

        typedef boost::shared_ptr<boost::interprocess::file_lock> FileLockPtr;

        const FileLockPtr Get(const boost::filesystem::path& name);
        
    private:
        /** Constructor*/
        explicit FileLockKeeper();

        /** Destructor */
        ~FileLockKeeper();

        boost::mutex m_lock;

        typedef std::map<boost::filesystem::path,FileLockPtr> Table;
        Table m_table;

        /**
         * This class is here to ensure that only the Instance method can get at the 
         * instance, so as to be sure that boost call_once is used correctly.
         * Also makes it easier to grep for singletons in the code, if all 
         * singletons use the same construction and helper-name.
         */
        struct SingletonHelper
        {
        private:
            friend FileLockKeeper& FileLockKeeper::Instance();
            
            static FileLockKeeper& Instance();
            static boost::once_flag m_onceFlag;
        };        
    };

    boost::once_flag FileLockKeeper::SingletonHelper::m_onceFlag = BOOST_ONCE_INIT;

    FileLockKeeper & FileLockKeeper::SingletonHelper::Instance()
    {
        static FileLockKeeper instance;
        return instance;
    }

    FileLockKeeper & FileLockKeeper::Instance()
    {
        boost::call_once(SingletonHelper::m_onceFlag,boost::bind(SingletonHelper::Instance));
        return SingletonHelper::Instance();
    }

    FileLockKeeper::FileLockKeeper()
    {
        
    }

    FileLockKeeper::~FileLockKeeper()
    {

    }

    const FileLockKeeper::FileLockPtr FileLockKeeper::Get(const boost::filesystem::path& name)
    {
        boost::lock_guard<boost::mutex> lck(m_lock);
        Table::iterator findIt = m_table.find(name);
        if (findIt != m_table.end())
        {
            return findIt->second;
        }
        else
        {
            FileLockPtr newLock(new boost::interprocess::file_lock(name.string().c_str()));
            m_table.insert(std::make_pair(name,newLock));
            return newLock;
        }
    }


    const boost::filesystem::path GetLockfileDirectory()
    {
        using namespace boost::filesystem;

        const char * ENV_NAME = "SAFIR_RUNTIME";
        char * env = getenv(ENV_NAME);
        if (env == NULL)
        {
            throw std::logic_error("Environment variable SAFIR_RUNTIME does not appear to be set");
        }
        path dir(env);

        dir /= "data/text/lluf/";
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
    StartupSynchronizer::StartupSynchronizer(const std::string& uniqeName):
        m_synchronized(NULL),
        m_name(uniqeName),
        m_started(false),
        m_lockfile(GetLockFile(m_name)),
        m_lockfile2(GetLockFile(m_name + "2"))
    {

    }

    StartupSynchronizer::~StartupSynchronizer()
    {
        Stop();
    }

    void StartupSynchronizer::Stop()
    {
        if (m_started)
        {
            //Get the inner lock, so that noone can start initializing while we upgrade the outer lock.
            if (m_fileLock2 == NULL)
            {
                m_fileLock2 = FileLockKeeper::Instance().Get(m_lockfile2);
                const bool gotLock2 = m_fileLock2->try_lock();
                if (!gotLock2)
                {
                    m_fileLock2.reset();
                }
            }
            //TODO: what happens after here is not analyzed

            //this is implicit when a call to try_lock is made with an exclusive lock taken.
            m_fileLock->unlock_sharable();

            //race here!

            const bool gotLock = m_fileLock->try_lock();
            if (gotLock)
            {
                // Got exclusive lock, noone else has it open, so I can remove everything!
                m_synchronized->Destroy();

                boost::interprocess::named_semaphore::remove(m_name.c_str());
                boost::filesystem::remove(m_lockfile);
                m_fileLock->unlock();
            }

            m_started = false;
        }
    }


    void StartupSynchronizer::Start(Synchronized* const synchronized)
    {
        if (m_started)
        {
            return;
        }
        m_synchronized = synchronized;

        m_fileLock = FileLockKeeper::Instance().Get(m_lockfile);

        //try to take the first lock exclusively, if we get it we may be allowed to call Create
        const bool gotLock = m_fileLock->try_lock();
        if (gotLock)
        {
            m_fileLock2 = FileLockKeeper::Instance().Get(m_lockfile2);
            //Ok, got the first lock. Now see if we can get the second lock
            const bool gotLock2 = m_fileLock2->try_lock();
            if (gotLock2)
            {
                //We got it! We're allowed to start initializing everything

                //remove any semaphore left over from previous instances
                boost::interprocess::named_semaphore::remove(m_name.c_str());

                //invoke the user callback
                m_synchronized->Create();
                
                //Create a new semaphore so that users know if we succeeded or not.
                boost::interprocess::named_semaphore sem(boost::interprocess::create_only,m_name.c_str(),1);

            }
            else
            {
                //keep m_fileLock2 as null if we dont have it locked
                m_fileLock2.reset();
            }

            //open the floodgates!
            m_fileLock->unlock();
        }

        //Wait for the exclusive lock to be released (or, if we're the Creator, just get the shared lock)
        //this will cause everyone to wait for Create to have completed successfully.
        m_fileLock->lock_sharable();

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
            ostr << "It appears that Create failed in some other process for '" << m_name << "'." << std::endl;
            ostr << "The exception I got was " << exc.what() << " and strerror(errno) returned this info: " << strerror(errno) << std::endl;
            std::wcerr << ostr.str().c_str() << std::flush;
            throw std::logic_error(ostr.str());
        }

        //invoke user callback
        m_synchronized->Use();

        m_started = true;
    }
}
}


