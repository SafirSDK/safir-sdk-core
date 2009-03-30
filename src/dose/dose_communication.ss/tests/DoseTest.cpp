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

/******************************************************************
* Main part of DoseTest.exe - for LINUX and WIN32
* Utility to test DoseComDll
*
* Function: See PrintHelp()
*******************************************************************/

#if defined _WIN32

#define _CRT_SECURE_NO_DEPRECATE
#include <windows.h>
#include <stdio.h>
#include <process.h>
#include <malloc.h>

#elif defined(linux) || defined(__linux) || defined(__linux__)

#ifndef _LINUX
#define _LINUX
#endif

#include <stdio.h>
#include <string.h>
#include <malloc.h>
// check what is needed
#include <stdlib.h>    // exit()
#include <sys/types.h>
#include <arpa/inet.h>

#include <errno.h>
#include <unistd.h> // for usleep(), exit()
#include <sys/times.h>
#include <pthread.h>     // pthread functions and data structures

#else
#  error "Unable to work out platform"
#endif

//---------------------------------
// Dose include files and typedefs
//---------------------------------

#ifdef _LINUX
typedef unsigned long long ulong64;
#endif

#ifdef _WIN32
typedef unsigned _int64 ulong64;
#endif

#include "DoseTestEvent.h"
#include "../../src/Defs/DoseUdpMsg.h"
#include "../../src/Defs/DoseNodeStatus.h"
#include "../../src/Defs/DoseCom_Interface.h"
#include "../Defs/DoseUdpMsg.h"
#include "../Defs/DoseNodeStatus.h"
#include "../DoseComDll/PrintError.h"
#include "../Defs/DoseCom_Interface.h"


#ifdef _WIN32

#define THREAD_API void __cdecl
// Not found by MsDev 6. So I decare it here
//BOOL TryEnterCriticalSection(LPCRITICAL_SECTION lpCriticalSection);
#endif

#ifdef _LINUX
#define THREAD_API void *
#define GetLastError() errno
#define TRUE    1
#define FALSE   0
#endif

//---------------------------------------
// Local variables
//---------------------------------------
static int Debug = 0;

static  int bIsInteractive = 0;

static  CDoseTestEvent  *g_pReadEventArr[6];
static  CDoseTestEvent  *g_pQueueNotFullEventArr[6];
static  CDoseTestEvent  *g_pDistributePoolEvent;

static ulong g_dwTimeRxEcho = 0;
static ulong g_dwTimeTxEcho = 0;

static int      g_Priority = 1;
static int      g_DestinationId = 65;
static int      g_PD_DestinationId = 64; // for PD - not allowed for other
static bool     g_bUseAck = true;
static unsigned long g_SizeToSend = 100;

static char BigBuff[100000]; // = {{0}};

//======================================================================
// Buffer handling
// ----------------
// DoseComDll sees this base class with virtual functions declared in
// DoseCom_Interface.h (which is included by DoseCom and DoseTest/DoseMain)
//
// class DoseComAllocator
// {
// public:
//  virtual char *Allocate(const size_t Size);
//  virtual void Deallocate(char *pBuff);
// };
//
// Application = DoseMain or DoseTest (this program) creates class
// CMyDoseComAllocator derived or inherited from DoseComAllocator
//
// Application creates an instance of CMyDoseComAllocator
// as g_pMyDoseBuff = new CMyDoseComAllocator();
//
// g_pMyDoseBuff is passed to DoseComDll via DoseCom_Init(g_pMyDoseBuff,...
// DoseComDll makes a copy of the pointer g_pMyDoseBuff and then uses
// p->Allocate() and  p->Deallocatee() which causes
// char *CMyDoseComAllocator::Allocate()
// and  CMyDoseBuff::Deallocate() to be called. (like callback routines)
//
// As a result of this design - malloc() and free() can be replaced by
// any functions without affecting DoseComDll
//=======================================================================

class CMyDoseComAllocator : public DoseComAllocator
{
public:
    char *Allocate(const size_t Size);
    void Deallocate(char *pBuff);
};

char *CMyDoseComAllocator::Allocate(const size_t Size)
{
    char *p = (char *) malloc(Size);
    //printf("CMyDoseComAllocator::Allocate(s=%u %X)\n",Size, p);
    return (p);
}

void CMyDoseComAllocator::Deallocate(char *pBuff)
{
    //printf("CMyDoseComAllocator::Deallocate(%X)\n", pBuff);
        try { free(pBuff); } catch (...) { printf("ERROR free() catched\n"); }
}

static CMyDoseComAllocator *g_pMyDoseBuff = NULL;

//============================================================================
// Notification Handlers
// Callback design similar to 'Buffer Handling' above.
//
// These routines are callbacks from dose_com
//============================================================================

class CMyDoseComNotificationHandler : public DoseComNotificationHandler
{
public:
    // these can be called from any thread, and must be implemented to be non-blocking.
    void NotifyIncomingData(const int priorityChannel); // = 0;
    void NotifyQueueNotFull(const int priorityChannel); // = 0;
    void NotifyNodeStatusChanged(); // = 0;
    void NotifyStartPoolDistribution(); // = 0;
};

/************************************
* Data aviable to Read
*************************************/
void CMyDoseComNotificationHandler::NotifyIncomingData(const int priorityChannel)
{
    if(Debug>3)
                printf("Callback_Read_Event(%d) - send event to Test_Read_Event_Thread()\n",
            priorityChannel);
    g_pReadEventArr[0]->Set();
}

/*************************************
* Queue  NOT full any more
*************************************/
void CMyDoseComNotificationHandler::NotifyQueueNotFull(const int priorityChannel)
{
    if(Debug>3)
                printf("Callback_QueueNotFull(%d)\n", priorityChannel);

    g_pQueueNotFullEventArr[priorityChannel]->Set();
}

/************************************
* NodeStatus changed
**************************************/
void CMyDoseComNotificationHandler::NotifyNodeStatusChanged()
{
    int             count = 0;
    unsigned long   IpAddr_nw;
    unsigned long   DoseId;
    unsigned long   Status;
    bool            bValidNode;


    printf(">>> Callback_NodeChange_Event() %d\n", count++);  fflush(stdout);

    for(;;)
    {
        bValidNode = DoseCom_GetNodeChange( DoseId, Status, IpAddr_nw);
        if(!bValidNode) break;
        printf(">>> Status=%c  DoseId=%2d IpAddr=%s\n",
                Status, DoseId,
                inet_ntoa( *(struct in_addr *)  &IpAddr_nw));
        fflush(stdout);
    }
}

/*********************************
* Start PD
*********************************/
void CMyDoseComNotificationHandler::NotifyStartPoolDistribution()
{
    printf(">>> Callback_DistributePool_Event()"
            " - send event to Test_DistributePool_Event_Thread()\n");
    g_pDistributePoolEvent->Set();
}

static CMyDoseComNotificationHandler *g_pMyDoseComNotificationHandler = NULL;

/**********************************************************
*
*
***********************************************************/
static void PrintHelp(void)
{
    printf("----------------------------------------DoseTest 08-01-21 ---\n");;
    printf("Threads for reception of events starts at startup\n");
    printf("\n");
    printf("-X<send data> Send\n");
    printf("-y<n>         Send 'n' messages\n");
    printf("-Y<n>         Send 'n' messages - no print\n");
    printf("-Z<n>         Send 'n' messages on all Priority channels\n");
    printf("-S            Send predefined data of length def by '-N' param.\n");
    printf("-M and -m     As -Y and -y, but each 32 is 50K\n");
    printf("-C<size>      Send Echo request\n");
    printf("-------------------------------------------------------------\n");
    printf("\n");
    printf("-ER           Test Set Read Event\n");
    printf("-EQ           Test Set QueueNotFull Event\n");
    printf("-ED           Test Set DistributePool Event\n");
    //printf("-EN           Test Set g_NodeChange Event\n");
    printf("-------------------------------------------------------------\n");
    printf("-D<n>         Set Debug level to <n>.  (0-9). (default=0).\n");
    printf("-P<n>         Set Prio Channel for send <n>. (0-4). (default=1).\n");
    printf("-T<nn>        Set DestId for send <n>. (64- ..). (default=64).\n");
    printf("-A<n>         Set Use Ack for send <n>. 1=Use, 0=NotUse"
                                    "  (default=UseAck).\n");
    printf("-N<nn>        Set Size to send.                   (default=100)\n");
    printf("-Q            Quit\n");
    printf("-------------------------------------------------------------\n");
}
/*********************************************
*
*********************************************/
#ifdef _LINUX
typedef void * THREAD_FUNCTION(void* param);
#endif

#ifdef _WIN32
typedef unsigned int __stdcall THREAD_FUNCTION(void* param);
#endif

unsigned long Create_Thread(unsigned long& tid,
                THREAD_FUNCTION thread_function,
                void* param)
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
    int        rc;
    pthread_t  p_thread1;      // first thread's structure

    rc = pthread_create(&p_thread1, NULL, thread_function, param);
    if(rc == 0) return((ulong) 0); //???? check error code
    return((ulong) -1); //error
#endif
}

//#########################################
// Time
//#########################################

// as WIN32 GetTickCount()
unsigned long Get_TickCount(void)
{
#ifdef _WIN32
    return GetTickCount();
#endif
//---------------------------------
#ifdef _LINUX
   static int MilliSecPerTick = 0;
    tms tm;

    if(!MilliSecPerTick)
    {
        // MilliSecPerTick = 10 = 1000/100 on my Suse 10.0 Linux
       MilliSecPerTick = 1000 / sysconf(_SC_CLK_TCK);
    }
    return  (MilliSecPerTick * times(&tm));
#endif
}
//---------------------------------
#ifdef _LINUX
void Sleep(unsigned long TimeoutMs)
{
    usleep(1000 * TimeoutMs);
}
#endif

/*******************************************************
*
*
********************************************************/
static void Test_Send(const char *pData, int Size,
                      bool PoolDistr, int Priority, int DestinationId, int bQuit)
{
    int     jj;
    int     result;
    char    *pSendBuf;
    unsigned long   ChkSum = 0;
    static unsigned long GetMemCount = 0;

    if(Size == 0) Size = strlen(pData) + 1;

    //pSendBuf = (char *) malloc(Size+4);
    pSendBuf = (char *) g_pMyDoseBuff->Allocate(Size+4);

    //printf(">>> GETMEM %08X - %u\n", pSendBuf, GetMemCount++);

    if(pSendBuf == NULL)
    {
        printf(">>> ERROR: Can not allocate a buffer\n");
        return;
    }

    if(!bQuit)
    printf(">>> Test_Send() Prio=%d DestinationId=%d UseAck=%d\n",
                    Priority,DestinationId, g_bUseAck);

    if(pData != NULL)  // in this case we don't add checksum to the msg
    {
        for(jj=0 ; jj<Size ; jj++)
        {
            pSendBuf[jj] = pData[jj];
            ChkSum += (pSendBuf[jj] & 0xFF);
        }
        pSendBuf[Size] = 0;
    }
    else // Create a msg and Add checksum
    {
        for(jj=0 ; jj<(Size-4) ; jj++)
        {
            pSendBuf[jj] = 'A' + jj % 25;
            ChkSum += (pSendBuf[jj] & 0xFF);
        }
        pSendBuf[jj++] = (uchar) (ChkSum & 0xFF);           // Size-4
        pSendBuf[jj++] = (uchar)((ChkSum>>8)  & 0xFF);
        pSendBuf[jj++] = (uchar)((ChkSum>>16) & 0xFF);
        pSendBuf[jj++] = (uchar)((ChkSum>>24) & 0xFF);  // Size-1
        pSendBuf[jj++] = 0;                     // Size
    }

    // int DoseCom_Send(const char *pBuf, unsigned long Size,
    //                  bool  PoolDistribution, bool    bUseAck,
    //                  int Priority, int   Destination);
Try_Again:
    result = DoseCom_Send(pSendBuf, Size, PoolDistr, g_bUseAck,
                            Priority, DestinationId);

    if((result != 0) && (result != 79))
        printf("    DoseCom_Send() failed %d\n", result);
    else
    if(!bQuit) printf(">>> Tx Size=%u ChkSum=%X\n", Size, ChkSum);

    if(result == 79) // overflow
    {
        ulong dwTime;
        dwTime = Get_TickCount();
        //printf("QFull %u\n");

        //printf(">>> Start Waiting for QueueNotFullEvent(%d)\n", Priority);
        int rc = g_pQueueNotFullEventArr[Priority]->WaitFor(1000);
        //printf(">>> Wait for QueueNotFull(%d) ret %d T=%d\n",
        //            Priority, rc, Get_TickCount() - dwTime);
        goto Try_Again;
    }
}

/*********************************************
*
*
***********************************************/
static void Test_Send_Echo(int Size)
{
    int     result,jj;
    char    *pSendBuf;

    //pSendBuf = (char *) malloc(Size+4);
    pSendBuf = (char *) g_pMyDoseBuff->Allocate(Size+4);

    //printf(">>> GETMEM %X\n", pSendBuf);

    if(pSendBuf == NULL)
    {
        printf(">>> ERROR: Can not allocate a buffer\n");
        return;
    }

    printf(">>> Test_Send_Echo(%d)\n", Size);

    for(jj=0;jj<Size;jj++) pSendBuf[jj] = jj & 0xFF;

    g_dwTimeTxEcho = Get_TickCount();


    result = DoseCom_Send(pSendBuf, Size, 0, 1, 1,64);

    if(Debug) printf("    Tx(%d): Result=%d (0 = OK)\n", Size, result);
}

/*************************************************
*
**************************************************/

static THREAD_API Test_DistributePool_Event_Thread(void *)
{
    int     jj;
    unsigned long   Size = 40;
    char    buff[1024];
    char    ComputerName[40];


    int Ret;

    printf(">>> Test_DistributePool_Event_Thread() starts\n");

    strcpy(ComputerName, "Testar"); // ???
    for(;;)
    {
        Sleep(1000);

        printf(">>> Start Wait for Test_DistributePoolEvent()\n");

        Ret = g_pDistributePoolEvent->WaitFor(1000 * 300 + 25);  //krash

        if(Ret == 1) // Event
        {
            printf(">>> Got a PoolDistributeEvent - Starting PoolDistribut\n");

            for(jj= 0;jj<10;jj++)
            {
                //if(jj==5) gets(buff);
                sprintf(buff,"Pool Distribution Data %d from %s\n", jj,
                        ComputerName);

                Test_Send(buff,0, TRUE, g_Priority,g_PD_DestinationId,0);
                Test_Send("Not PD",0, FALSE, g_Priority,g_DestinationId,0);
                // also to NOT PD
            }

            DoseCom_PoolDistributed(g_Priority, g_PD_DestinationId);
            Test_Send("NOT PD",0, FALSE, g_Priority,g_DestinationId,0);
            printf(">>> Pool Distribution has ended."
                    " Has called DoseCom_PoolDistributed()\n");
        }
        else if(Ret == 0) // Timeout
        {
            //if(Debug>2)
            printf(">>> Test_DistributePool_Event_Thread()"
                    " - WaitForEvent() - Timeout (%d)\n", Ret);
            Sleep(1000);
        }
        else // some error
        {
            //if(Debug>2)
            printf(">>> ERROR - Test_DistributePool_Event_Thread()"
                    " - WaitForEvent()  (%d)\n", Ret);
            Sleep(1000);
        }
    }
}

/*************************************************
*
**************************************************/

static THREAD_API Test_Receive_Thread(void *)
{
    int     jj;
    int     pos;
    unsigned long   Count = 0;
    unsigned long   RxCount = 0;
    int     Next_Seq = 0;
    unsigned long   ChkSumTx;
    unsigned long   ChkSumRx;
    ulong   Size;
    int     result;
    char    *pReadBuf;
    unsigned long dwFromBitMap;
    bool    bIsNative;


    printf(">>> Test_Receive_Thread() started\n");
    fflush(stdout);

    // We should start trigging a search in case we missed something at startup
    g_pReadEventArr[0]->Set();

    for(;;)
    {
        int Ret = g_pReadEventArr[0]->WaitFor(100027);

        if(1) // check queue for all events
        {
            if(Debug>2)
            printf(">>> WaitForEvent() ret = %X\n",Ret); fflush(stdout);

            for(;;)
            {
                result = DoseCom_Read(0xFFFF, &dwFromBitMap, &pReadBuf,
                                        &Size, &bIsNative);

                if(result == ERR_DOSECOM_OK)
                {
                    RxCount++;
                    pos = Size - 1;
                    if (pos > 80) pos = 80;

                    ChkSumRx = 0;
                    for(jj=0 ; jj< (int)(Size-4) ; jj++)
                        ChkSumRx += (pReadBuf[jj] & 0xFF);

                    if(pReadBuf[0] != 0)
                    {
                        // Compare this with how a msg i sent
                        if(    (pReadBuf[0] == 'A') && (pReadBuf[1] == 'B')
                            && (pReadBuf[2] == 'C') && (pReadBuf[3] == 'D')
                            && (pReadBuf[4] == 'E') && (pReadBuf[5] == 'F'))
                        {
                            ChkSumTx = (pReadBuf[Size-4] & 0xFF )
                                     | ((pReadBuf[Size-3] << 8)  & 0xFF00)
                                     | ((pReadBuf[Size-2] << 16) & 0xFF0000)
                                     | ((pReadBuf[Size-1] << 24) & 0xFF000000);

                            if(ChkSumTx == ChkSumRx)
                            {
                                // last char is overwritten with a 0
                                pReadBuf[pos] = 0;
                                printf(">>> Rx ChkSumOK(%02X): "
                                    "Size=%u Chk=%X %s\n",
                                    dwFromBitMap, Size, ChkSumRx, pReadBuf);
                            }
                            else
                            {
                                pReadBuf[pos] = 0;
                                printf(">>> Rx ChkSumErr(%02X):"
                                        " Size=%u ChkTx/Rx=%X/%X  %s\n",
                                        dwFromBitMap, Size, ChkSumTx,
                                        ChkSumRx, pReadBuf);
                            }
                        } // end "ABDEDEF"
                        //---------------------------------------------
                        // Sent by 'Ynn' test
                        // "nn+......"   -  'nn' is sequence number
                        // Check
                        //---------------------------------------------
/*---
                        else
                        if(pReadBuf[2] == '-')
                        {

                            for(jj=Size-4 ; jj<(int) Size ; jj++)
                                ChkSumRx += (pReadBuf[jj] & 0xFF);

                            printf(">>> Seq=%d\n", Seq);
                        }
----*/
                        else //In this case, compare it with what sender printed
                        {

                            for(jj=Size-4 ; jj<(int) Size ; jj++)
                                ChkSumRx += (pReadBuf[jj] & 0xFF);

                            // This is for the 'Ynn' test

                            if(pReadBuf[2] == '-')
                            {
                                int Seq =10 * (pReadBuf[0]-'0')+pReadBuf[1]-'0';

                                if (Seq != Next_Seq)
                                {
                                    // The first is "00-d...."
                                    if(!((Seq == 0) && (pReadBuf[3] == 'd')))
                                        printf("\nERROR Seq Got/Exp= %d/%d\n",
                                                Seq, Next_Seq);
                                }
                                Next_Seq = (Seq + 1) % 100;

                                if((Count % 1000) == 0)
                                    printf("%c", '0' + ((Count / 1000) % 10) );
                                Count++;
                            }

                            if(pReadBuf[2] != '-')
                            {
                                pReadBuf[20] = 0; // do not print too much
                                printf(">>> Rx(%02X)[%d]: Size=%u Chk=%X %s\n",
                                        dwFromBitMap, RxCount, Size,
                                        ChkSumRx, pReadBuf);
                            }
                        }
                    }
                    else
                    //---------------------------------------------
                    // Messages starting with 0 are special cases
                    // 0,1,2, ... shall be echoed back with 1 --> 2
                    // 0,2,2, ..  are echo responses
                    //--------------------------------------------
                    {
                        // Got an echo request
                        if((pReadBuf[1] == 1) && (pReadBuf[2] == 2))
                        {
                            // echo it back
                            pReadBuf[1] = 2;
                            result = DoseCom_Send(pReadBuf, Size, 0,1,1,64);
                            pReadBuf = NULL; //to prevent from beeing freed by me
                        }
                        else
                        // Got the echoed back msg
                        if((pReadBuf[1] == 2) && (pReadBuf[2] == 2))
                        {
                            g_dwTimeRxEcho = Get_TickCount();

                            printf(">>> Got the echo back msg."
                                    "Size = %d Time = %d ms\n",
                                    Size, g_dwTimeRxEcho - g_dwTimeTxEcho);
                            // Check data
                            for(unsigned long ii = 0 ; ii < Size ; ii++)
                                if((char)(pReadBuf[ii] & 0xFF)!=(char)(ii & 0xFF))
                                    printf(">>> Invalid data at %d = %X\n",
                                        ii, pReadBuf[ii]);
                        }
                        else
                            printf( ">>> Got an unexpected special msg\n");
                    }

                    // Free buffer - MUST be done after msg is processed
                    if(pReadBuf != NULL)
                    {
                        g_pMyDoseBuff->Deallocate(pReadBuf);
                        //free((void *) pReadBuf);
                        if(Debug) printf(">>> Free RecvBuff\n");
                    }
                }
                else
                {
                    if(Debug>1) printf(">>> No more messages to read\n");
                    break;
                }
            }
        }
        else
        {
            if(Debug)
                printf(">>> Test_Receive_Thread() - WaitForEvent()->timeout\n");
        }
        /////////Sleep(1000);
    }
}
/*------------- end Test_Receive_Thread() ------------------*/

/************************************************************
* Parse commands from command line or input file.
*
* Note: pLine = argv[0]
*
* Returns: 0 if a valid command, -1 if syntax error
*************************************************************/

int ParseCmd (const char *pLine, char **argv)
{
    int j1,j2,num;
    char buff[2000];

    switch(pLine[0])
    {
        case '?':
            PrintHelp();
            if(!bIsInteractive) exit(0);
            break;

        case 'q':
        case 'Q':
            exit(0);

        case 'c':
        case 'C':
            Test_Send_Echo(atoi(&pLine[1]));
            break;

        case 'e':
        case 'E':
            switch(pLine[1] & 0xDF)
            {
                case 'R':
                    printf(">>> Test: SetReadEvent\n");
                    g_pReadEventArr[0]->Set(); break;

                case 'Q':
                    printf(">>> Test: SetQueueNotFullEvent\n");
                    g_pQueueNotFullEventArr[0]->Set(); break;

                case 'D':
                    printf(">>> Test: DistributePoolEventEvent\n");
                    g_pDistributePoolEvent->Set(); break;
                //case 'N': g_pNodeChangeEvent->Set(); break;
            }
            break;

        case 'd':
        case 'D':
            Debug = pLine[1] - '0';
            printf(">>> Debug=%d\n",Debug);
            break;

        case 'p':
        case 'P':
            g_Priority = pLine[1] - '0';
            printf(">>> Priority=%d\n",g_Priority);
            break;

        case 't':
        case 'T':
            g_DestinationId = pLine[1] - '0';
            if((pLine[2]>='0') && (pLine[2] <= '9'))
                g_DestinationId = 10*g_DestinationId + pLine[2] - '0';

            printf(">>> DestinationId=%d\n",g_DestinationId);
            break;

        case 'n':
        case 'N':
            g_SizeToSend = atoi(&pLine[1]);
            printf(">>> g_SizeToSend = %u\n", g_SizeToSend);
            break;

        case 'a':
        case 'A':
            if( pLine[1] == '0')
            {
                g_bUseAck = false;
                printf(">>> UseAck = false\n");
            }
            else
            {
                g_bUseAck = true;
                printf(">>> UseAck = true\n");
            }
            break;

        //case 'r':
        //case 'R':
        //  Test_Read();
        //  break;

        case 'X':
        case 'x':
            Test_Send(pLine,strlen(pLine)+1,FALSE,g_Priority,g_DestinationId,0);
            break;

        case 's':
        case 'S':
            Test_Send(NULL, g_SizeToSend, FALSE, g_Priority,g_DestinationId, 0);
            break;

        case 'M':
        case 'm':
        case 'Y':
        case 'y':
            if(( pLine[1] < '0') || ( pLine[1] > '9'))
            {
                printf("Syntax error. Need digit after y\n");
                break;
            }
            int Size;
            Size = 25; //1400;
            ulong StartTime;
            num = atoi(&pLine[1]);

            StartTime = Get_TickCount();
            for(j1=0;j1<num;j1++)
            {
                for(j2=0 ; j2<Size ; j2++)
                    BigBuff[j2] = 'a'+ (j1 & 0xF) + (j2 & 7);
                BigBuff[j2-1] = 0;

                BigBuff[0] = '0' + ((j1/10) % 10);
                BigBuff[1] = '0' + (j1 % 10);

                if( pLine[0] - 'y') BigBuff[2] = '-';//this makes rec to be quit
                else BigBuff[2] = '+';

                if(((pLine[0] & 0xDF) == 'M') && ((j1 & 0x1F) == 0))
                    Test_Send(BigBuff, 50000,FALSE, g_Priority,
                                g_DestinationId, pLine[0] - 'y');
                else
                    Test_Send(BigBuff, 0,FALSE, g_Priority,
                                g_DestinationId, pLine[0] - 'y');

                if(j1==8)
                {
                    // send no target messages
                    int save = g_DestinationId;
                    g_DestinationId = 7;

                    for(int i=0 ; i<6 ;i++)
                        Test_Send(BigBuff, strlen(BigBuff) - 1 -i,FALSE,
                                g_Priority, g_DestinationId, pLine[0] - 'y');

                    g_DestinationId = save;
                }
                //printf(">>>>>>>>\n");
            }
            printf("It took %d ms (not counting the 14 last).\n",
                    Get_TickCount() - StartTime);
            break;

        case 'Z':
        case 'z':
            if(( pLine[1] < '0') || ( pLine[1] > '9'))
            {
                printf("Syntax error. Need digit after y\n");
                break;
            }
            num = atoi(&pLine[1]);
            for(j1=0;j1<num;j1++)
            {
                for(j2=0;j2<30;j2++) buff[j2] = 'P'+ j1 + (j2 & 7);
                buff[j2] = 0;
                Test_Send(buff, 0,FALSE,0,g_DestinationId, 0);
                Test_Send(&buff[2], 0,FALSE,1,g_DestinationId, 0);
                Test_Send(&buff[4], 0,FALSE,2,g_DestinationId, 0);
                Test_Send(&buff[6], 0,FALSE,3,g_DestinationId, 0);
            }
            break;

        default:
            printf("Use '?' for help\n");
            printf(">>> UseAck        = %s\n", g_bUseAck ? "true" : "false");
            printf(">>> DestinationId = %d\n", g_DestinationId);
            printf(">>> Priority      = %d\n", g_Priority);
            printf(">>> g_SizeToSend  = %u\n", g_SizeToSend);
            break;
    } // end switch
    return(0);
}
/*--------------------- end ParseCmd() --------------------*/

/************************************************************
*
* Returns: 0 if OK, != 0 if some Error
*************************************************************/

int main (int argc, char **argv)
{
    int     an;
    char    *a_p;
    int     result = 0;
    char    line[512];

    printf("DoseTest start\n");

    g_pMyDoseBuff = new CMyDoseComAllocator();

        g_pMyDoseComNotificationHandler = new CMyDoseComNotificationHandler();

#ifdef _WIN32
    DoseCom_Add_DestinationId(64, "225.1.1.100", (ulong64) 0x0FFFFFFFFFFFFFFF);
    DoseCom_Add_DestinationId(65, "225.1.1.101", (ulong64) 0xAAAAAAAAAAAAAAAA);
    DoseCom_Add_DestinationId(66, "225.1.1.102", (ulong64) 0x5555555555555555);
    DoseCom_Add_DestinationId(67, "225.1.1.103", (ulong64) 0x3333333333333333);
    DoseCom_Add_DestinationId(68, "225.1.1.104", (ulong64) 0xCCCCCCCCCCCCCCCC);
    DoseCom_Add_DestinationId(69, "225.1.1.105", (ulong64) 0x1111111100000000);
    DoseCom_Add_DestinationId(70, "224.1.1.105", (ulong64) 0x1111111122222222);
//  DoseCom_Add_DestinationId(99, "225.1.1.100", (ulong64) 0xFFFFFFFFFFFFFFFF);
    // for PD only
#endif
#ifdef _LINUX
    DoseCom_Add_DestinationId(64, "225.1.1.100", (ulong64) 0xFFFFFFFF);
    DoseCom_Add_DestinationId(65, "225.1.1.101", (ulong64) 0xAAAAAAAA);
    DoseCom_Add_DestinationId(66, "225.1.1.102", (ulong64) 0x55555555);
    DoseCom_Add_DestinationId(67, "225.1.1.103", (ulong64) 0x33333333);
    DoseCom_Add_DestinationId(68, "225.1.1.104", (ulong64) 0xCCCCCCCC);
    DoseCom_Add_DestinationId(69, "225.1.1.105", (ulong64) 0x00000000);
    DoseCom_Add_DestinationId(70, "224.1.1.105", (ulong64) 0x22222222);
    //  DoseCom_Add_DestinationId(99,
#endif

    // Create events
    g_pDistributePoolEvent = new CDoseTestEvent();
    g_pDistributePoolEvent->Create();

    for(int jj=0 ;jj<6 ; jj++)
    {
        g_pQueueNotFullEventArr[jj] = new CDoseTestEvent();
        g_pReadEventArr[jj]         = new CDoseTestEvent();

        g_pQueueNotFullEventArr[jj]->Create();
        g_pReadEventArr[jj]->Create();
    }

    DoseCom_Init(g_pMyDoseBuff,
                NULL,   // "225.1.1.100",
                NULL,   // "192.0.0.0", //NULL,   // netAddr
                5,      // DoseId
                g_pMyDoseComNotificationHandler);

#ifdef _WIN32
    _beginthread( &Test_Receive_Thread,0, NULL);
    _beginthread( &Test_DistributePool_Event_Thread,0, NULL);
#endif

#ifdef _LINUX
    unsigned long tid;
    int par = 5;
    Create_Thread(tid, &Test_Receive_Thread, NULL); //(void *) &par)
    Create_Thread(tid, &Test_DistributePool_Event_Thread, NULL);
#endif


    /* ----------- command line mode -------------------*/

    for (an=1 ; an < argc ; an++)
    {
        a_p = argv[an];
        if (a_p[0] == '-')
            result = ParseCmd(&a_p[1], &argv[an]);
        else
            result = ParseCmd(&a_p[0], &argv[an]);
    }

    if (argc >1 ) return(result);

    /* --------- interactive mode (if no args ) --------------*/

    bIsInteractive = 1;

    for(;;)
    {
        Sleep(1000);
        fgets(line, sizeof(line) - 4, stdin);
        switch (line[0] )
        {
            case 'q':
            case 'Q': break;
            default:
                if(line[0] == '-')
                    result = ParseCmd(&line[1], NULL);
                else
                    result = ParseCmd(line, NULL);
                break;
        }
        line[0] = 0;
    }
}
/*-------------- end DoseTest.Cpp ----------------------*/
