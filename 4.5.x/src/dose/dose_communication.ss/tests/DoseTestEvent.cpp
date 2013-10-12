/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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

/******************************************************************
* DoseTestEvent.cpp - A part of DoseTest.exe -for LINUX and WIN32
*****************************************************************/

#ifdef _WIN32
#include <windows.h>
#endif
//#include "DosePlatform.h"  //????

#include <stdio.h> //printf()
#include <errno.h>
#include "DoseTestEvent.h"

/******************************************************
*
*******************************************************/
int CDoseTestEvent::Set()
{
#ifdef _WIN32
    return SetEvent(m_EventId);
#endif
//------------------------
#ifdef _LINUX
    // Must use Mutex to prevent lost signal
    int rc;
    //printf("-----SetEvent() Entry\n"); fflush(stdout);

    pthread_mutex_lock(&m_Mutex);
    m_SignalCounter++;
    //printf("-----SetEvent() Signal Event\n");  fflush(stdout);

    // signal the condition variable
    rc = pthread_cond_signal(&m_Condition);

    //printf("-----SetEvent() Has Signal Event. rc=%d\n",rc);  fflush(stdout);

    pthread_mutex_unlock(&m_Mutex);
#endif
}

/******************************************************
* Returns: Allways 0
*
*******************************************************/

// Do this to use CLOCK_REALTIME fo test
//#undef CLOCK_MONOTONIC
//#define CLOCK_MONOTONIC 0  // this is CLOCK_REALTIME

int CDoseTestEvent::Create() // Could be constructor ????
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

    if(rc != 0) printf("*--- ERROR pthread_condattr_init ==> %d\n",rc);
    //else printf("First pthread_condattr_init() OK\n");
    //pthread_condattr_destroy (&cattr);

    // ingen skillnad - pthread_mutex_init(&m_Mutex, NULL);

    // The clock attribute is the clock ID of the clock that shall be used
    // to measure the timeout service of pthread_cond_timedwait(). The default
    // value of the clock attribute shall refer to the system clock.
    // Default is CLOCK_REALTIME which jumps if operator sets time
    // Works OK for _MONOTONIC and _REALTIME

    rc = pthread_condattr_setclock(&cattr, CLOCK_MONOTONIC);
    //printf("=====Create_Event() pthread_condattr_setclock() ==> %d\n",rc);
    //fflush(stdout);

    rc = pthread_cond_init(&m_Condition, &cattr);

    if(rc != 0) printf("***** ERROR pthread_condattr_init ==> %d\n",rc);
    //else printf("pthread_condattr_init() OK\n");

    //printf("=====Create_Event() pthread_cond_init() ==> %d\n",rc);

    rc = pthread_condattr_getclock(&cattr, &clock_id);

    //printf("=====Create_Event() rc = %d clock_id = %d (%d)\n",
    //      rc, clock_id, CLOCK_MONOTONIC);
    //fflush(stdout);

    pthread_condattr_destroy (&cattr);

#endif
    return(0);
};

/******************************************************
* Returns:
*  0 - timeout
*  1 - wakedup by an event
* -1 - error
*
* Improvement: if no time out, use pthread_cond_wait(&m_Condition,&m_Mutex);
*******************************************************/
int CDoseTestEvent::WaitFor(unsigned long TimeoutMs)
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
    int rc = 0; // 0 ==> ret 1 = by event = there is data
    struct timespec timeout;
    struct timespec now;

    //printf("+#+++WaitFor() Entry SigCount=%d                       Timeout=%d ms.\n",
    //    m_SignalCounter,  TimeoutMs);
    //fflush(stdout);

    // If no pending ( = lost ) signals, we must wait
    if(m_SignalCounter == 0)
    {
        clock_gettime(CLOCK_MONOTONIC, &now);

        timeout.tv_sec  = now.tv_sec + TimeoutMs / 1000;
        timeout.tv_nsec = now.tv_nsec + 1000 * 1000 * (TimeoutMs % 1000);

        if(timeout.tv_nsec >= 1000000000)
        {
            timeout.tv_sec  += 1;
            timeout.tv_nsec -= 1000000000;
        }

        //printf("       sec=%d.%09d --> sec=%d.%09d\n",
        //      now.tv_sec, now.tv_nsec,
        //      timeout.tv_sec, timeout.tv_nsec);

        pthread_mutex_lock(&m_Mutex);

        // pthread_cond...releases the mutex before it blocks, and then
        // re-acquires it before it returns.
        // Upon successful completion, a value of zero is returned
        // otherwise, an error number is returned.

        rc = pthread_cond_timedwait(&m_Condition, &m_Mutex, &timeout);

        pthread_mutex_unlock(&m_Mutex);
    }
    m_SignalCounter = 0;

    if(rc == 0)
    {
        //printf("+++++WaitFor() - ret %d. - An Event                     (%u)\n",rc, TimeoutMs);  fflush(stdout);
        return(1);
    }
    if(rc == ETIMEDOUT) //ETIMEDOUT = 110
    {
        //printf("+++++WaitFor() - ret %d. - Timeout                      (%u)\n",rc, TimeoutMs);
        //fflush(stdout);
        //usleep(1000 * 1000); // 1 sec
       return(0);
    }
    printf("+++++WaitFor() - ret %d. - ? ? ? ?                      (%u))\n",rc, TimeoutMs);
    fflush(stdout);
    return(-1);

#endif //_LINUX
}
