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

#ifndef __DOSE_SIGNALS_H__
#define __DOSE_SIGNALS_H__


#include <Safir/Dob/Internal/Semaphore.h>
#include <boost/noncopyable.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/thread/mutex.hpp>
#include <boost/thread/shared_mutex.hpp>
#include <Safir/Dob/Internal/ConnectionId.h>
#include <Safir/Dob/Internal/LeveledLock.h>
#include <Safir/Dob/Internal/InternalDefs.h>
#include <Safir/Utilities/Internal/UnorderedMap.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    /**
     * A singleton for caching signals.
     * Note that this class does NOT reside in shared memory!
     */
    class Signals:
        private boost::noncopyable
    {
    public:
        typedef boost::shared_ptr<NamedSemaphore> SemaphorePtr;

        static Signals & Instance();

        /**
         * Kick an application.
         */
        void SignalIn(const ConnectionId& connection);



        /**
         * This is for applications waiting for "things" to happen to its connection.
         * This blocks forever until SignalIn for that connection has been called.
         * This should ONLY be used for waiting on your own connection. All other use will yield
         * undefined results...
         * Only meant to be called from inside Connections::WaitForConnectionSignal
         */
        const boost::function<void(void)> GetConnectionSignalWaiter(const ConnectionId& connectionId);

        /**
         * Signal that an application wants to connect or wants something else done by dose_main.
         * This must only be called from within Connections, since flags also have to be set
         * for this to mean anything.
         */
        void SignalConnectOrOut();

        /** Wait for the above signal (blocking).*/
        void WaitForConnectOrOut();

        /** This method causes the underlying semaphore to be deleted, so that new process may
         * not be able to get hold of the semaphore.
         * This should be called for connections when dose_main are removing them,
         * and when constructing a connection so that we don't reuse some old corrupt
         * semaphore data.
         */
        static void Remove(const ConnectionId& connectionId);

        /** As above, but for the special dose_main semaphore. */
        static void RemoveConnectOrOut();
    private:
        Signals();
        ~Signals();


        class SignalTable
        {
        public:
            SignalTable();

            //Get the semaphore for a connection (returns a new one if it wasnt in list)
            const SemaphorePtr GetSemaphore(const ConnectionId& connection);
        private:
            //return the newly created semaphore
            //you must have write lock to call this! And you must copy the ptr before releasing the lock!
            const SemaphorePtr& AddSemaphore(const ConnectionId& connection);

            //Note that this contains semaphores allocated with new!
            //remember to delete the semaphore if removing from map!
            typedef unordered_map<Identifier, SemaphorePtr> Semaphores;
            Semaphores m_semaphores;

            typedef Safir::Dob::Internal::LeveledLock<boost::shared_mutex,
                                                      SIGNALS_LOCK_LEVEL,
                                                      NO_MASTER_LEVEL_REQUIRED> SignalsLock;
            SignalsLock m_lock;

            const Safir::Dob::Typesystem::Int32 m_nodeNumber;
        };

        SignalTable m_waitSignals; //signals that we wait for
        SignalTable m_signalSignals; //signals that we signal.

        NamedSemaphore m_connectOrOutSignal;

        /**
         * This class is here to ensure that only the Instance method can get at the 
         * instance, so as to be sure that boost call_once is used correctly.
         * Also makes it easier to grep for singletons in the code, if all 
         * singletons use the same construction and helper-name.
         */
        struct SingletonHelper
        {
        private:
            friend Signals& Signals::Instance();

            static Signals& Instance();
            static boost::once_flag m_onceFlag;
        };
    };

}
}
}

#endif

