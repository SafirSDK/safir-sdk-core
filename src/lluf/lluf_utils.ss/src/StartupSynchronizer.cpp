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
#include <cstdio>
#include <iostream>
namespace Safir
{
namespace Utilities
{
    StartupSynchronizer::StartupSynchronizer(const std::string& uniqeName):
        m_synchronized(NULL),
        m_name(uniqeName),
        m_started(false)
    {

    }

    StartupSynchronizer::~StartupSynchronizer()
    {
        Stop();
    }

    void StartupSynchronizer::Stop()
    {
        boost::lock_guard<boost::mutex> lck(m_threadLock);
        if (m_started)
        {
            const bool gotLock = m_fileLock->try_lock();
            if (gotLock)
            {
                //                std::wcerr << "Got exclusive lock, noone else has it open, so I can remove everything!" << std::endl;
                m_synchronized->Destroy();

                boost::interprocess::named_semaphore::remove(m_name.c_str());
                boost::filesystem::remove(m_lockfile);
                m_fileLock->unlock();
            }

            m_started = false;
        }
    }

    const boost::filesystem::path GetLockfileDirectory()
    {
        using namespace boost::filesystem;

        const char * ENV_NAME = "SAFIR_RUNTIME";
        char * env = getenv(ENV_NAME);
        if (env == NULL)
        {
            throw StartupSynchronizerException("Environment variable SAFIR_RUNTIME does not appear to be set");
        }
        path dir(env,native);

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
                throw StartupSynchronizerException(ostr.str());
            }
        }
        else if (!is_directory(dir))
        {
            std::ostringstream ostr;
            ostr << "The lluf lockfile directory does not appear to be a directory. dir = '" << dir.string() << "'" << std::endl;
            throw StartupSynchronizerException(ostr.str());
        }
        return dir;
    }

    const boost::filesystem::path GetLockFile(const std::string& name)
    {
        using namespace boost::filesystem;
        const path filename = GetLockfileDirectory() / name;

        if (exists(filename))
        {
            if (!is_regular(filename))
            {
                std::ostringstream ostr;
                ostr << "The lockfile does not appear to be a reglar file. filename = '" << filename.string() << "'" << std::endl;
                throw StartupSynchronizerException(ostr.str());
            }
        }

        boost::filesystem::ofstream file(filename);
        if (!file.good())
        {
            std::ostringstream ostr;
            ostr << "Failed to open the lockfile '" << filename.string() << "'." << std::endl;
            throw StartupSynchronizerException(ostr.str());
        }

        return filename;
    }

    void StartupSynchronizer::Start(Synchronized* const synchronized)
    {
        boost::lock_guard<boost::mutex> lck(m_threadLock);
        if (m_started)
        {
            return;
        }
        m_synchronized = synchronized;

        const boost::filesystem::path lockfile = GetLockFile(m_name);
        //        std::wcerr << "Using '" << lockfile.string().c_str() << "' as lockfile" << std::endl;

        m_fileLock.reset(new boost::interprocess::file_lock(lockfile.string().c_str()));

        const bool gotLock = m_fileLock->try_lock();
        if (gotLock)
        {
            boost::interprocess::named_semaphore::remove(m_name.c_str());

            //            std::wcerr << "Got exclusive lock, calling Create" << std::endl;
            m_synchronized->Create();

            //            std::wcerr << "Creating semaphore." << std::endl;
            boost::interprocess::named_semaphore sem(boost::interprocess::create_only,m_name.c_str(),1);
            //            std::wcerr << "Initialize complete" << std::endl;
            m_fileLock->unlock();
        }
        else
        {
            //            std::wcerr << "Failed to get exclusive lock, someone else is initializing" << std::endl;
        }

        //        std::wcerr << "Waiting for sharable lock" << std::endl;
        m_fileLock->lock_sharable();
        //        std::wcerr << "Got sharable lock" << std::endl;

        try
        {
            //            std::wcerr << "Opening semaphore" << std::endl;
            boost::interprocess::named_semaphore sem(boost::interprocess::open_only,m_name.c_str());
            //            std::wcerr << "Waiting on semaphore" << std::endl;
            sem.wait();

            //            std::wcerr << "Posting semaphore" << std::endl;
            sem.post();
        }
        catch (const boost::interprocess::interprocess_exception& exc)
        {
            perror("open_semaphore");
            std::ostringstream ostr;
            ostr << "It appears that Create failed in some other process for '" << m_name << "'." << std::endl;
            ostr << "The exception I got was " << exc.what() << std::endl;
            std::wcerr << ostr.str().c_str() << std::flush;
            throw StartupSynchronizerException(ostr.str());
        }

        //        std::wcerr << "Initialization seems to have been successful! Calling Use" << std::endl;
        m_synchronized->Use();
        //        std::wcerr << "StartupSynchronizer::Start is complete" << std::endl;

        m_started = true;
    }
}
}


