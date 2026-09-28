/******************************************************************************
*
* Copyright Saab AB, 2007-2013,2022,2026 (http://safirsdkcore.com)
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
#pragma once

#include <Safir/Utilities/Internal/VisibilityHelpers.h>

#ifdef lluf_startup_synchronizer_EXPORTS
#  define LLUF_STARTUP_SYNCHRONIZER_API SAFIR_HELPER_DLL_EXPORT
#else
#  define LLUF_STARTUP_SYNCHRONIZER_API SAFIR_HELPER_DLL_IMPORT
#endif
#define LLUF_STARTUP_SYNCHRONIZER_LOCAL SAFIR_HELPER_DLL_LOCAL

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
     *
     * The callbacks are invoked with an internal lock held, which has two
     * consequences:
     *
     * - A callback must not construct, destroy or Start a StartupSynchronizer
     *   for the *same* name. Doing so deadlocks.
     * - A callback that uses a StartupSynchronizer for a *different* name makes
     *   the two names lock-ordered. All processes must then use the same order,
     *   or they can deadlock against each other.
     *
     * Note that Create() and Destroy() are not exception safe: an exception from
     * Create() will propagate out of Start() (leaving the resource uncreated and
     * unused), but an exception from Destroy() is swallowed, since Destroy() is
     * called from a destructor.
     */
    class Synchronized
    {
    public:
        virtual ~Synchronized() {}

        /**
         * Guarantees are that only one call to Create in all processes will be
         * made *per generation* of the resource. A generation starts when some
         * process creates the resource and ends when the last user of it goes
         * away (see Destroy below). Even the process that gets a call to Create
         * will get a call to Use after Create is finished.
         * Use this callback to create the shared resource.
         *
         * Note that this callback must not assume that a previous instance has
         * been able to call Destroy (see below), but may have to perform cleanup
         * before creating the shared resource. This happens whenever a previous
         * generation was killed rather than shut down, and it is also how a
         * half-finished Create from a process that died gets cleaned up.
         *
         * If this callback throws, the exception propagates out of Start(), the
         * resource is left untouched (no Use and no Destroy callback will be
         * made for this instance) and the next process to try will get a Create
         * callback of its own.
         */
        virtual void Create() = 0;

        /**
         * All processes will get a call to Use, even the one that got the Create
         * callback. Use this callback to open the shared resource for use.
         *
         * If this callback throws, the exception propagates out of Start() and
         * this instance is not registered as a user of the resource: it will get
         * no Destroy callback, and if it was the only user in this process then
         * this process stops using the resource altogether. The resource itself is
         * left alone, so the next process to create it gets a Create callback and
         * has to clean up after this generation, exactly as it would if we had
         * been killed here.
         */
        virtual void Use() = 0;

        /**
         * This callback will be called when the last user of the resource lets
         * go of it.
         *
         * Use it to perform any cleanup.
         *
         * It is guaranteed that Destroy is only ever called on an instance that
         * has had a successful Use callback.
         *
         * Note that a call to Destroy is not guaranteed, only a best effort is
         * made to call this. The last instance could be killed by a signal
         * anyway, so there is no point in making this a strong guarantee. In
         * particular, if the last two users of a resource go away at exactly the
         * same time, neither of them may end up calling Destroy.
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
     * The same resource may be created again after it has been destroyed, i.e.
     * the sequence Create-Use-Destroy-Create-Use-Destroy is perfectly normal for
     * a long lived process that starts and stops using a resource repeatedly.
     *
     * Note that multiple threads within a process may call these functions simultaneously
     * on *different* instances with different arguments to no ill effect.
     *
     * WARNING: Ensure that the instance if this class is destroyed *after* the variables
     * you use in Synchronized::Destroy(), or otherwise you will be using destroyed variables.
     */
    class LLUF_STARTUP_SYNCHRONIZER_API StartupSynchronizer
    {
    public:
        /**
         * Construct a StartupSynchronizer for a resource with a given name.
         *
         * The name is used to derive file names, so it must be a non-empty
         * string of at most 180 characters that contains no directory
         * separators, and none of the characters that Windows disallows in file
         * names. A std::logic_error is thrown if it is not.
         */
        explicit StartupSynchronizer(const char* uniqueName);

        /**
         * Destructor.
         *
         * Stop using the shared resource. If this instance is the last user
         * of the resource the Destroy callback will *probably* be called
         * (see Synchronized class documentation).
         *
         * This never throws, whatever happens.
         */
        ~StartupSynchronizer();

        /**
         * Call this to start the synchronized startup.
         * The callbacks to synchronized will be called as described in the
         * Synchronized class documentation.
         *
         * Only one call to Start is allowed for a particular instance. A second
         * call throws std::logic_error, as does a null argument.
         *
         * Multiple calls to different instances of this class with
         * the same pointer argument also results in undefined behaviour.
         *
         * If the resource could not be acquired, or if one of the callbacks
         * throws, this throws. No Destroy callback will ever be made for an
         * instance whose Start did not succeed, and no locks are left held, so
         * the next call to Start (in this or any other process) starts over from
         * a clean slate.
         */
        void Start(Synchronized* const synchronized);

    private:

        StartupSynchronizer(const StartupSynchronizer&) = delete;
        StartupSynchronizer& operator=(const StartupSynchronizer&) = delete;

        /**
         * The implementations are owned by an internal singleton and live for as
         * long as the process does, so this is a plain pointer that is always
         * valid. See ImplKeeper in the .cpp file for why they are never
         * destroyed.
         */
        StartupSynchronizerImpl* m_impl;

        Synchronized* m_synchronized;
    };

}
}


#ifdef _MSC_VER
#pragma warning (pop)
#endif
