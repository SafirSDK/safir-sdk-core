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
#ifndef __LLUF_STARTUP_SYNCHRONIZER_H__
#define __LLUF_STARTUP_SYNCHRONIZER_H__

//disable warnings in boost and ace
#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable : 4267)
  #pragma warning (disable : 4512)
  #pragma warning (disable : 4244)
  #pragma warning (disable : 4805)
#endif

#include <Safir/Utilities/Internal/UtilsExportDefs.h>
#include <boost/noncopyable.hpp>
#include <boost/function.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/interprocess/sync/file_lock.hpp>
#include <ace/Thread_Mutex.h>

//and enable the warnings again
#if defined _MSC_VER
  #pragma warning (pop)
#endif

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4251) // warning C4251: 'Safir::Dob::Typesystem::LibraryExceptions::m_CallbackMap' : class 'stdext::hash_map<_Kty,_Ty>' needs to have dll-interface to be used by clients of class 'Safir::Dob::Typesystem::LibraryExceptions'
#pragma warning (disable: 4275) // warning C4275: non dll-interface class 'boost::noncopyable_::noncopyable' used as base for dll-interface class 'Safir::Dob::Typesystem::LibraryExceptions'
#endif

namespace Safir
{
namespace Utilities
{
    class Synchronized
    {
    public:
        virtual ~Synchronized() {}

        virtual void Create() = 0;
        virtual void Use() = 0;
        virtual void Destroy() = 0;
    };

    class StartupSynchronizerException:
        public std::exception
    {
    public:
        explicit StartupSynchronizerException(const std::string& str):
            m_str(str)
        {

        }

        virtual ~StartupSynchronizerException() throw() {}
        const char * what() const throw () {return m_str.c_str();}
    private:
        std::string m_str;
    };

    class LLUF_UTILS_API StartupSynchronizer:
        private boost::noncopyable
    {
    public:
        typedef boost::function<void(void)> Callback;

        StartupSynchronizer(const std::string& uniqeName,
                            Synchronized* const syncronized);

        void Start();

        void Stop();

        ~StartupSynchronizer();
    private:
        Synchronized* const m_synchronized;
        const std::string m_name;
        bool m_started;
        boost::filesystem::path m_lockfile;
        boost::shared_ptr<boost::interprocess::file_lock> m_fileLock;
        ACE_Thread_Mutex m_threadLock; //file_locks do not guarantee thread locking, so we need this.
    };

}
}

#ifdef _MSC_VER
#pragma warning (pop)
#endif

#endif

