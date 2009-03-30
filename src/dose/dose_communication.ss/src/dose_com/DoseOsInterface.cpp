/******************************************************************************
*
* Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
*
* Created by: Lars Engdahl / stlsen
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

/*****************************************************************
* DoseOsInterface.cpp - a part of DoseComDll - For LINUX and WIN32
*
* Contains Lots of OS functions needed by DOSE
******************************************************************/
#ifndef _CRT_SECURE_NO_DEPRECATE
#define _CRT_SECURE_NO_DEPRECATE
#endif

#define IS_USING_SOCKETS
#define IS_USING_THREADS
#include "DosePlatform.h"

#include "DoseOsInterface.h"
#include "PrintError.h"

extern volatile int * volatile pDbg;

#ifdef _LINUX
#include <pthread.h>     // pthread functions and data structures ???
#endif

//#########################################
// Time
//#########################################

/********************************************
* As WIN32 GetTickCount()
*
* Returns millisec since system started
********************************************/
unsigned long DoseOs::Get_TickCount(void)
{
#ifdef _WIN32
    return GetTickCount();
#endif
//---------------------------------
#ifdef _LINUX
// API times():
// All times are measured in terms of the number of clock ticks used.
// Upon successful completion, times() shall return the elapsed real time,
// in clock ticks, since an arbitrary point in the past (for example,
// system start-up time). This point does not change from one invocation
// of times() within the process to another. The return value may overflow
// the possible range of type clock_t. If times() fails, (clock_t)-1 shall
// be returned and errno set to indicate the error.
//--------------------
// Alternative API: int clock_gettime(clockid_t clk_id, struct timespec *tp);
// Function clock_gettime() retrieve the time of the specified clock clk_id.
// CLOCK_MONOTONIC = Clock that cannot be set and represents monotonic
// time since some unspecified starting point.
// struct timespec {time_ttv_sec; long tv_nsec; }// sec/ nanoseconds
// -------------------

    static int MilliSecPerTick = 0;
    tms tm;

    if(!MilliSecPerTick)  // First time
    {
        // MilliSecPerTick = 10 = 1000/100 on my Suse 10.0 Linux
       MilliSecPerTick = 1000 / sysconf(_SC_CLK_TCK);
    }
    return  (MilliSecPerTick * times(&tm));

#endif
}
/************************************************
* Sleep
*************************************************/
void DoseOs::Sleep(unsigned long TimeoutMs)
{
#ifdef _WIN32
    ::Sleep(TimeoutMs);
#endif
//---------------------------------
#ifdef _LINUX
    usleep(1000 * TimeoutMs);
#endif
}

//#########################################
// Section Sockets
//#########################################

char *DoseOs::Inet_Ntoa(unsigned long IpAddr_nw)
{
    static char buff[20];

    sprintf(buff, "%lu.%lu.%lu.%lu",
            IpAddr_nw & 0xFF,   (IpAddr_nw>>8) & 0xFF,
            (IpAddr_nw>>16) & 0xFF, (IpAddr_nw>>24) & 0xFF);
    return(buff);
}

unsigned long DoseOs::Inet_Addr(const char *pAddStr)
{
 return(inet_addr(pAddStr));
}


//#########################################
// Section Events
//#########################################

/******************************************************
*
*******************************************************/
int DoseOs::CDoseComEvent::Set()
{
#ifdef _WIN32
    return SetEvent(m_EventId);
#endif
//------------------------
#ifdef _LINUX
    // Must use Mutex to prevent lost signal
    int rc;
    //PrintDbg("-----SetEvent() Entry\n");

    pthread_mutex_lock(&m_Mutex);
    m_SignalCounter++;
    //PrintDbg("-----SetEvent() Signal Event\n");

    // signal the condition variable
    rc = pthread_cond_signal(&m_Condition);

    //PrintDbg("-----SetEvent() Has Signal Event. rc=%d\n",rc);

    pthread_mutex_unlock(&m_Mutex);
#endif
}

/******************************************************
* Returns:
*
*******************************************************/

// Do this to use CLOCK_REALTIME fo test
// #undef CLOCK_MONOTONIC
// #define CLOCK_MONOTONIC CLOCK_REALTIME

int DoseOs::CDoseComEvent::Create()
{
#ifdef _WIN32
        m_EventId = CreateEvent(NULL,false,false,NULL);
        return(0);
#endif
//----------------------------------------
#ifdef _LINUX
    pthread_condattr_t cattr;
    int rc;
    clockid_t clock_id;
    pthread_mutex_t mut = PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP;
    pthread_cond_t cond = PTHREAD_COND_INITIALIZER;

    m_Mutex         = mut;
    m_Condition     = cond;

    m_SignalCounter = 0;

    // Create the default cond attributes object
    // ingen skillnad
    rc = pthread_condattr_init(&cattr);

    if(rc != 0) PrintDbg("*--- ERROR pthread_condattr_init ==> %d\n",rc);
    //else PrintDbg("First pthread_condattr_init() OK\n");
    //pthread_condattr_destroy (&cattr);

    // ingen skillnad - pthread_mutex_init(&m_Mutex, NULL);

    // The clock attribute is the clock ID of the clock that shall be used
    // to measure the timeout service of pthread_cond_timedwait(). The default
    // value of the clock attribute shall refer to the system clock.
    // Default is CLOCK_REALTIME which jumps if operator sets time
    // Works OK for _MONOTONIC and _REALTIME

    rc = pthread_condattr_setclock(&cattr, CLOCK_MONOTONIC);
    if(*pDbg) PrintDbg("=====Create_Event() pthread_condattr_setclock() ==> %d\n",rc);

    rc = pthread_cond_init(&m_Condition, &cattr);

    if(rc != 0) PrintDbg("***** ERROR pthread_condattr_init ==> %d\n",rc);

    rc = pthread_condattr_getclock(&cattr, &clock_id);

    //PrintDbg("=====Create_Event() rc = %d clock_id = %d (%d)\n",
    //      rc, clock_id, CLOCK_MONOTONIC);

    pthread_condattr_destroy (&cattr);

    return(0);
#endif
};

/******************************************************
* Returns:
*  0 - timeout
*  1 - wakedup by an event
* -1 - error
*
* Improvement: if no time out, use pthread_cond_wait(&m_Condition,&m_Mutex);
*******************************************************/
int DoseOs::CDoseComEvent::WaitFor(unsigned long TimeoutMs)
{
#ifdef _WIN32
    DWORD dwRet = WaitForSingleObject(m_EventId, TimeoutMs);
    switch (dwRet)
    {
        case WAIT_TIMEOUT: return(0);
        case WAIT_OBJECT_0: return(1);
        default:    return(-1);
    }
#endif
//------------------------
#ifdef _LINUX
    int rc;
    struct timespec timeout;
    struct timespec now;

    pthread_mutex_lock(&m_Mutex); // be sure DoseComEvent::Set() not is doing something

    // If no thread is waiting for the signal (doing pthread_cond_timedwait())
    // when the signal is sent (by pthread_cond_signal()), the signal is lost.
    // Because of this, m_SignalCounter is incremented when a signal is sent.
    // If so, return at once.

    if(m_SignalCounter != 0)
    {
        // Note 1: We could also do "m_SignalCounter--"
        // But since we are scanning all jobs  after a wakeup,
        // it does not matter if there is one or more than one
        // pending signals.
        m_SignalCounter = 0;
        pthread_mutex_unlock(&m_Mutex);
        return(1); // Waked by (pending) event
    }

    // If no pending ( = lost ) signals, we must wait

    clock_gettime(CLOCK_MONOTONIC, &now);

    timeout.tv_sec  = now.tv_sec + TimeoutMs / 1000;
    timeout.tv_nsec = now.tv_nsec + 1000 * 1000 * (TimeoutMs % 1000);

    if(timeout.tv_nsec >= 1000000000)
    {
        timeout.tv_sec  += 1;
        timeout.tv_nsec -= 1000000000;
    }

    // Note that pthread_cond_timedwait() releases the mutex before it
    // blocks, and then re-acquires it before it returns.
    //
    // Upon successful completion, a value of zero shall be returned
    // otherwise, an error number shall be returned to indicate the error.
    //
    // Note that signals when we not are in pthread_cond_timedwait() are lost.
    //
    // Note that we also could be waked up by other conditions, but that
    // doesn't matter. All that will be done is scanning for jobs to do.

    rc = pthread_cond_timedwait(&m_Condition, &m_Mutex, &timeout);

    m_SignalCounter = 0; // See Note 1. if(m_SignalCounter)  m_SignalCounter--;

    pthread_mutex_unlock(&m_Mutex); // allow DoseComEvent::Set() to run

    if(rc == 0)
    {
        return(1);
    }
    if(rc == ETIMEDOUT) // ETIMEDOUT = 110
    {
        return(0);
    }
    //PrintDbg("+++++WaitFor() - ret %d. - ? ? ? ?      (%u))\n",rc, TimeoutMs);
    return(-1);

#endif //_LINUX
}

//#########################################
// Section Processes and Threads
//#########################################

/******************************************************
*
*******************************************************/

typedef THREAD_API THREAD_FUNCTION(void* param); // ??? in an header file - how for _WIN32

unsigned long DoseOs::CreateThread(unsigned long& tid,
                THREAD_FUNCTION thread_function, void* param)
{
#ifdef _WIN32

    tid = _beginthreadex(NULL,0,thread_function, param,0,NULL);

    if (tid != 0x0L)
        return 0; // Ok
    else
        return((ulong) -1); //error
#endif
//---------------------------------
#ifdef _LINUX
    // int pthread_create(pthread_t *restrict thread,
    //              const pthread_attr_t *restrict attr,
    //              void *(*start_routine)(void*), void *restrict arg);
    // Return Value
    // If successful, the pthread_create() function shall return zero;
    // otherwise, an error number shall be returned to indicate the error.

    int        rc;
    pthread_t  p_thread1;      // first thread's structure

    rc = pthread_create(&p_thread1, NULL, thread_function, param);
    if(rc == 0) return((ulong) 0); //???? check error code
    return((ulong) -1); //error
#endif
}

/******************************************************
* Same for WINN32 and LINUX
*******************************************************/
void DoseOs::Exit(int ExitCode) {exit(ExitCode);}

//#########################################
// Section Misc
//#########################################

/******************************************************
*
*******************************************************/

int DoseOs::Get_LastError(void)
{
#ifdef _WIN32
    return GetLastError();
#endif
//---------------------------------
#ifdef _LINUX
    // When multithreading, errno is a macro to a function
    return(errno);
#endif
}

/*----------------- end DoseInterface.cpp -------------*/
