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

/***************************************************************
* DoseOsInterface.h - a part of DoseComDll - For LINUX and WIN32
*
* Contains OS functions needed by DOSE. Supports LINUX and WIN32
***************************************************************/

#ifndef __DOSE_OSINTEFACE
#define __DOSE_OSINTEFACE

//--------------------
// OS Include files - remove for WIN32 is OK ???
//---------------------
//#ifdef _WIN32
//#include <Winsock2.h>
//#endif

//#include <stdio.h>

#ifndef dcom_ulong32
typedef  unsigned int  dcom_ulong32;
typedef  unsigned short dcom_ushort16;
typedef  unsigned char  dcom_uchar8;
#endif

#ifdef _WIN32
#ifndef dcom_ulong64
typedef unsigned _int64  dcom_ulong64;
typedef _int64  long64;
#endif
#endif

#ifdef _LINUX
typedef unsigned long long  dcom_ulong64;
typedef long long  long64;
#endif

#ifndef FALSE
#define FALSE 0
#define TRUE 1
#endif

//--------------------------------------------
// Errorcodes from OS (that are used by DOSE)
//--------------------------------------------
//#include <WinError.h>

#ifdef _WIN32
#define DOSE_ECONNRESET     WSAECONNRESET
#define DOSE_ETIMEDOUT      WSAETIMEDOUT
#endif

#ifdef _LINUX
#define DOSE_ECONNRESET     ECONNRESET
#define DOSE_ETIMEDOUT      ETIMEDOUT
#endif

//--------------------------------------------
// Os routines
//--------------------------------------------

namespace DoseOs
{
    //=======================
    // Shared Memory
    //=======================

    void *Shared_Memory_Create(int Size);

    //=======================
    // Time
    //=======================

    // as WIN32 GetTickCount()
    unsigned long Get_TickCount(void);

    void Sleep(unsigned long TimeoutMs);

    //===============
    // Sockets
    //===============

    // as Winsock inet_ntoa(), inet_addr()
    char *Inet_Ntoa(unsigned long IpAddr_nw);
    unsigned long Inet_Addr(const char *pAddStr);

    //===============
    // Events
    //===============
    //* Example of usage:
    // CDoseComEvent *pMyEvent;
    //
    // Thread 1:
    // pMyEvent = new CDoseComEvent;
    // pMyEvent->Create();
    // pMyEvent->WaitFor(1000);
    //
    // Thread 2:
    // pMyEvent->Set();

#ifdef IS_USING_THREADS  // define this in files using events

    class CDoseComEvent
    {
    public:
        int Set();
        int Create();

        // WaitFor returns:
        //  0 - timeout / 1 - wakedup by an event / -1 - error
        int WaitFor(unsigned long TimeoutMs);

    private:

#ifdef _WIN32
        HANDLE m_EventId;
#endif
#ifdef _LINUX
        pthread_mutex_t m_Mutex;
        pthread_cond_t  m_Condition;
        int             m_SignalCounter;
#endif
    };
#endif  // IS_USING_THREADS

    //=======================
    // Processes and Threads
    //=======================

#ifdef _WIN32
    typedef unsigned __stdcall THREAD_FUNCTION(void* param);

    unsigned long CreateThread(unsigned long& tid,
            THREAD_FUNCTION thread_function, void* param);
#endif

#ifdef _LINUX
typedef void * THREAD_FUNCTION(void* param);

unsigned long CreateThread(unsigned long& tid,
                THREAD_FUNCTION thread_function, void* param);
#endif

    void Exit(int ExitCode);

    //==============
    // Misc
    //==============

    // as Win32 GetLastError()
    int Get_LastError(void);
};

#endif
