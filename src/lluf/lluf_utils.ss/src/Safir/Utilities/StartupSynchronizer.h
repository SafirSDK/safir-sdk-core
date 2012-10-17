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

#include <Safir/Utilities/Internal/UtilsExportDefs.h>

//disable warnings in boost
#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable : 4267)
  #pragma warning (disable : 4512)
  #pragma warning (disable : 4805)
#endif

#include <boost/noncopyable.hpp>
#include <boost/function.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/interprocess/sync/file_lock.hpp>
#include <boost/thread/mutex.hpp>

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
    /** 
     * Interface for a synchronized object.
     */
    class Synchronized
    {
    public:
        virtual ~Synchronized() {}

        /** 
         * Guarantees are that only one call to Create in all processes will be made.
         * Even the process that gets a call to Create will get a call to Use after 
         * Create is finished.
         * Use this callback to create the shared resource.
         *
         * Note that this callback must not assume that a previous instance has 
         * been able to call Destroy (see below), but may have to perform cleanup
         * before creating the shared resource.
         */
        virtual void Create() = 0;

        /**
         * All processes will get a call to Use, even the one that got the Create callback.
         * Use this callback to open the shared resource for use.
         */
        virtual void Use() = 0;

        /**
         * This callback will be called when the last process destroys its StartupSynchronizer.
         *
         * Use it to perform any cleanup.
         *
         * Note that a call to Destroy is not guaranteed, only a best effort is made to call
         * this. The last instance could be killed by a signal anyway, so there is no
         * point in making this a strong guarantee.
         */
        virtual void Destroy() = 0;
    };

    /**
     * WARNING: Note that no thread guarantees are made, only process guarantees.
     * Also, make sure only one instance (per uniqueName) exists within each
     * process!
     */
    class LLUF_UTILS_API StartupSynchronizer:
        private boost::noncopyable
    {
    public:
        explicit StartupSynchronizer(const std::string& uniqueName);

        /**
         * Call this to start the synchronized startup.
         * The callbacks to synchronized will be called as described in the
         * Synchronized class documentation.
         */
        void Start(Synchronized* const synchronized);

        /**
         * Stop using the shared resource. If this instance is the last user
         * of the resource the Destroy callback will *probably* be called
         * (see Synchronized class documentation).
         * Calls to Start after Stop has been called (on same instance) 
         * results in undefined behaviour.
         */
        void Stop();

        /**
         * Destructor implicitly calls Stop.
         */
        ~StartupSynchronizer();
    private:
        Synchronized* m_synchronized;
        const std::string m_name;
        bool m_started;
        const boost::filesystem::path m_lockfile;
        const boost::filesystem::path m_lockfile2;

        //The idea is to take these locks in a specific fashion, and to never
        //release them. They get released when the process exits.
        boost::shared_ptr<boost::interprocess::file_lock> m_fileLock;
        boost::shared_ptr<boost::interprocess::file_lock> m_fileLock2; //should only be set if we have the lock
    };

}
}

#ifdef _MSC_VER
#pragma warning (pop)
#endif

#endif

