/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
*
* Created by: Joel Ottosson / stjoot
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

#ifndef _dose_dispatch_thread_h
#define _dose_dispatch_thread_h

#include <Safir/Dob/Internal/Interface.h>
#include <Safir/Dob/Internal/Connection.h>
#include <Safir/Dob/Internal/ConsumerId.h>

#include <ace/Thread.h>
#include <ace/Thread_Semaphore.h>
#include <ace/Auto_Event.h>
#include <boost/noncopyable.hpp>
#include <boost/enable_shared_from_this.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    /**
     * This class is best used with a shared_ptr, since the only way to stop the thread is to
     * destroy the object.
     */
    class DispatchThread:
        public boost::enable_shared_from_this<DispatchThread>,
        private boost::noncopyable
    {
    public:
        DispatchThread(const ConnectionId & id,
                       const ConsumerId & consumer,
                       OnDispatchCb * const onDispatchCb);

        /**
         * Thread must be in a stopped state to be possible to destroy!
         */
        ~DispatchThread();

        /**
         * Attempt to stop the thread. If it could not be stopped false is returned.
         * If waitAWhile is true this call will delay with up to 100 ms before returning false.
         */
        bool Stop(const bool waitAWhile);

        /**
         * If you're starting a new dispatch thread (i.e. C#, Ada, or C++)
         * Blocks until new thread is started.
         */
        void StartNewThread();

        /**
         * If you're starting the dispatcher loop inside an existing
         *  thread (i.e. Java), use the StartInThisThread method.
         */
        void StartInThisThread();

    private:
        static ACE_THR_FUNC_RETURN ThreadFun(void* param);

        void Run(); //dispatch loop

        OnDispatchCb * const m_onDispatchCallback;
        const ConsumerId m_consumer;

        ConnectionId m_id;

        ACE_thread_t m_threadId; //for debug output, remember the thread id.
        ACE_hthread_t m_threadHandle;

        ACE_Auto_Event m_startEvent;
        ACE_Auto_Event m_stopEvent;

        AtomicUint32 m_stop;

        enum ThreadState
        {
            NotStarted,  //the thread has not been started yet
            RunningInNewThread, //The thread has been started using StartNewThread
            RunningInExistingThread, //The thread has been started using StartInThisThread
            Stopped, //the thread has been stopped, and all resources cleaned up (i.e. the thread is joined)
        };

        ThreadState m_threadState;

        bool m_stopSignalled;

    };
}
}
}

#endif
