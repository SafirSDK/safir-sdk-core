/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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

#include <Safir/Utilities/Internal/VisibilityHelpers.h>

#ifdef lluf_startup_synchronizer_EXPORTS
#  define LLUF_STARTUP_SYNCHRONIZER_API SAFIR_HELPER_DLL_EXPORT
#else
#  define LLUF_STARTUP_SYNCHRONIZER_API SAFIR_HELPER_DLL_IMPORT
#  define SAFIR_LIBRARY_NAME "lluf_startup_synchronizer"
#  define SAFIR_NO_DEBUG_LIBRARY_SUFFIX
#  include <Safir/Utilities/Internal/AutoLink.h>
#endif
#define LLUF_STARTUP_SYNCHRONIZER_LOCAL SAFIR_HELPER_DLL_LOCAL

#include <boost/noncopyable.hpp>
#include <boost/shared_ptr.hpp>


#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4275)
#endif


namespace Safir
{
namespace Utilities
{
    /** Forward declaration for pimpl idiom */
    class StartupSynchronizerImpl;


    /** 
     * Interface for a synchronized object.
     * Note that NONE of these callbacks are exception safe! If you let an
     * exception propagate through the callback anything could happen!
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
     * This class can be used to synchronize creation and opening of a resource that
     * is shared between multiple processes and threads.
     * The resource has to have a unique name, and the basic idea is that:
     * One and only one call to Start will generate a Create() callback.
     * All instances (including the Creator) will get a Use() callback when the create
     * has completed.
     * The last instance of StartupSynchronizer to be destroyed will generate a 
     * Destroy callback.
     *
     * Note that multiple threads within a process may call these functions simultaneously
     * on *different* instances with different arguments to no ill effect.
     */
    class LLUF_STARTUP_SYNCHRONIZER_API StartupSynchronizer:
        private boost::noncopyable
    {
    public:
        /**
         * Construct a StartupSynchronizer for a resource with a given name.
         */
        explicit StartupSynchronizer(const char* uniqueName);

        /**
         * Destructor.
         *
         * Stop using the shared resource. If this instance is the last user
         * of the resource the Destroy callback will *probably* be called
         * (see Synchronized class documentation).
         */
        ~StartupSynchronizer();

        /**
         * Call this to start the synchronized startup.
         * The callbacks to synchronized will be called as described in the
         * Synchronized class documentation.
         *
         * Only one call to Start is allowed for a particular instance.
         * Multiple calls result in undefined behaviour.
         *
         * Multiple calls to different instances of this class with
         * the same pointer argument also results in undefined behaviour.
         */
        void Start(Synchronized* const synchronized);

    private:

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4251)
#endif

        boost::shared_ptr<StartupSynchronizerImpl> m_impl;

#ifdef _MSC_VER
#pragma warning (pop)
#endif
        
        Synchronized* m_synchronized;

    };

}
}


#ifdef _MSC_VER
#pragma warning (pop)
#endif


#endif

