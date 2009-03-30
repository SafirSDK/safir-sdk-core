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

#include "dose_dispatch_thread.h"

#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Dob/Typesystem/Exceptions.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Dob/Typesystem/LibraryExceptions.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>

#include <iostream>
#include <ace/Time_Value.h>
#include <ace/OS_NS_sys_time.h>
#include <ace/Thread.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    DispatchThread::DispatchThread(const ConnectionId & id,
                                   const ConsumerId & consumer,
                                   OnDispatchCb * const onDispatchCb) :
        m_onDispatchCallback(onDispatchCb),
        m_consumer(consumer),
        m_id(id),
        m_threadId(0),
        m_threadHandle(0),
        m_stop(false),
        m_threadState(NotStarted)
    {
        lllout << "DispatchThread::DispatchThread() - called. tid: " << ACE_Thread::self() << std::endl;
    }

    DispatchThread::~DispatchThread()
    {
        ENSURE (m_threadState == NotStarted || m_threadState == Stopped, << "DispatchThread is being destroyed while the thread is still running!");
    }

    bool DispatchThread::Stop(const bool waitAWhile)
    {
        /**
         * Case 1: Thread has not been started at all.
         * Case 2: Thread has been started and this is the first call to Stop.
         * Case 3: Stop has been called before, successfully.
         * Case 4: Stop has been called before, unsuccessfully, and the thread is still running (i.e.
         waiting in user code to notify main thread)
         * Case 5: Stop has been called before, unsuccessfully, but the thread has stopped since then.
         * Case 5: Stop has been called before, unsuccessfully, but the thread is about to stop
         *          (m_isRunning is still true and the semaphore hasnt been signalled).
         */
        lllout <<"DispatchThread::Stop() - called... waitAWhile: " << std::boolalpha << waitAWhile << std::endl;

        m_stop = true;

        switch (m_threadState)
        {
        case NotStarted:
            m_threadState = Stopped;
            return true;

        case RunningInNewThread:
            {
                Connections::Instance().GetConnection(m_id)->SignalIn();

                ACE_Time_Value waitUntil = ACE_OS::gettimeofday();
                if (waitAWhile)
                {
                    waitUntil += ACE_Time_Value(0,100000); //0.1 second delay
                }
                if (0 == m_stopEvent.wait(&waitUntil))
                {   //we got it, the thread must have stopped
                    //reap exit code
                    ACE_THR_FUNC_RETURN exitValue;
                    const int ret = ACE_Thread::join(m_threadHandle,&exitValue);
                    ENSURE(ret == 0, << "ACE_Thread::join failed with return value " << ret << ", errno is " << errno << ", strerror = " << strerror(errno));
                    ENSURE(exitValue == 0, << "Got unexpected exit value when stopping DispatchThread: "<< exitValue);

                    lllout << "DispatchThread::Stop() - joined thread. ret: " << ret << ". exitValue: " << exitValue << std::endl;

                    m_threadState = Stopped;
                    return true;
                }
                else
                {//oh, no! The thread is probably waiting to do a thread switch in the user app.
                    //we return false and sort it out at a higher level
                    return false;
                }
            }
            break;

        case RunningInExistingThread:
            {
                Connections::Instance().GetConnection(m_id)->SignalIn();

                ACE_Time_Value waitUntil = ACE_OS::gettimeofday();
                if (waitAWhile)
                {
                    waitUntil += ACE_Time_Value(0,100000); //0.1 second delay
                }
                if (0 == m_stopEvent.wait(&waitUntil))
                {   //we got it, the thread must have stopped
                    m_threadState = Stopped;
                    return true;
                }
                else
                {//oh, no! The thread is probably waiting to do a thread switch in the user app.
                    //we return false and sort it out at a higher level
                    return false;
                }
            }
            break;

        case Stopped:
            return true;
        }
        ENSURE(false, << "Internal error in DispatchThread::Stop. State is " << m_threadState);
        return false;
    }

    void DispatchThread::StartNewThread()
    {
        ENSURE(m_threadState == NotStarted, << "Someone tried to start a DispatchThread that was already running!");
        const int result = ACE_Thread::spawn(ThreadFun,
                                             this,
                                             THR_NEW_LWP | THR_JOINABLE ,
                                             &m_threadId,
                                             &m_threadHandle);
        ENSURE(result == 0, << "Failed to spawn new thread! ACE_Thread::spawn return value was " << result);
        //m_startSemaphore.acquire();
        m_startEvent.wait();
        m_threadState = RunningInNewThread;
        lllout <<"Dose_dll - DispatchThread (" << m_threadId << "): Started thread"<<std::endl;
    }


    void DispatchThread::StartInThisThread()
    {
        //we cannot use the startEvent here, but we don't need to since we know that the
        //call will succeed, and succeed immediately: We're not starting a new thread!

        ENSURE(m_threadState == NotStarted, << "Someone tried to start a DispatchThread that was already running!");
        m_threadState = RunningInExistingThread;
        m_threadId = ACE_Thread::self();
        lllout<<"Dose_dll - DispatchThread (" << m_threadId << "): Started dispatching inside existing thread."<<std::endl;
        Run();
    }

    void DispatchThread::Run()
    {
        //hold on to a copy of ourselves to ensure that the class is not destroyed until
        //the thread has ended.
        boost::shared_ptr<DispatchThread> dummy = shared_from_this();

        lllout << "DispatchThread::Run() - called. tid: " << ACE_Thread::self() << std::endl;

        m_startEvent.signal();

        lllout << "DispatchThread::Run() - m_startEvent.signal() done. tid: " << ACE_Thread::self() << std::endl;

        try
        {
            for (;;)
            {
                Connections::Instance().WaitForConnectionSignal(m_id);

                // Check stop-flag
                if(m_stop)
                {
                    lllout << "DispatchThread::Run() - about to stop... tid: " << ACE_Thread::self() << std::endl;

                    m_threadState = Stopped;
                    m_stopEvent.signal();

                    return;
                }

                bool success;
                m_onDispatchCallback(m_consumer.consumer, success);
                if (!success)
                {
                    Typesystem::LibraryExceptions::Instance().Throw();
                }
            }
        }
        catch (const char * e)
        {
            lllerr<<"DispatchThread::Run: Caught 'char *' exception: "
                  << "  '" << e << "'." << std::endl;
        }
        catch (const Safir::Dob::Typesystem::Internal::CommonExceptionBase & e)
        {
            lllerr <<"DispatchThread::Run: Caught 'DOTS' exception: "
                      << "  '" << e.GetExceptionInfo() << "'." << std::endl;
        }
        catch (const std::exception & e)
        {
            lllerr<<"DispatchThread::Run: Caught 'std::exception' exception: "
                  << "  '" << e.what() << "'." << std::endl;
        }
        catch (...)
        {
            lllerr<<"DispatchThread::Run: Caught '...' exception!"<<std::endl;
        }

        m_threadState = Stopped;
        m_stopEvent.signal();

        // Got exception. Connection is broken. Exit process.
        exit(-1);
    }


    //initiates the dispatch thread
    ACE_THR_FUNC_RETURN DispatchThread::ThreadFun(void* param)
    {
        static_cast<DispatchThread*>(param)->Run();
        lllout << "DispatchThread::ThreadFun() - About to return..." << std::endl;
        return 0;
    }
}
}
}


