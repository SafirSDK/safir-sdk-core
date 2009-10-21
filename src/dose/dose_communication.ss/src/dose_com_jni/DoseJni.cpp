/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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

/************************************************************************
* DoseJni.cpp - A part of DoseJni.dll for LINUX and WIN32
* To be used by Java DoseMon.
*
* Implements a Java native interface to shared memory used by DoseComDll.
*
* To be linked with DoseOsSharedMem.cpp which implements Os dependent code.
*
* In Win32, it is compiled/linked to DoseJni.dll
* In Linux, it is compiled/linked to libDoseJni.so
**************************************************************************/
#define _CRT_SECURE_NO_DEPRECATE

#define IS_USING_SOCKETS

#include "../dose_com/DosePlatform.h"

#include <stdio.h>

extern void PrintDbg( const char *format, ... );
extern void PrintErr(int ErrorCode, const char *format, ... );

// "jni.h" defines Java binding interface
// In WIN32 jni.h and jnimd.h are fetched from from Java SDK
// In LINUX, they seem to be present on some valid include path

#include <jni.h>  // which includes - jni_md.h

#include "../dose_com/DoseOsInterface.h"
#include "../defs/DoseNodeStatus.h"

static int Debug = 0;

int volatile * volatile pDbg = &Debug;  // DoseOsSharedMem.cpp needs it

/******************************************************************
*
* The declaration in Java is:
* public native int GetInfo(int cmd, int param, byte[] buf);
*
* and it is used as:
*
* DoseJni doseJni = new DoseJni();
* int rc = doseJni.loadDoseLib();
*
* byte[] buff = new byte[100];
*
* System.out.println("*** call dose" );
* rc = doseJni.GetInfo(77,4, buff);
* System.out.println("*** ret Dose " + rc );
*
* for(byte ii = 0 ; ii< 10 ; ii++)
*   System.out.print(" " + buff[ii]);
* System.out.println();
*
* Java_DoseJni_GetInfo means:
* Java      prefix must allbe there
* DoseJni   is the class
* GetInfo   is the Java name
* -----------------------------------
* The Java code could be like:
*
* class DoseJni
* {
*   public native int GetInfo(int cmd, int param, byte[] buf);
*
*   public int loadDoseLib() //throws ??????
*   {
*       try
*       {
*           System.loadLibrary("dose_com"); // Note: no ".so" or ".dll"
*           return(0); // OK
*       }
*       catch (Throwable e)
*       {
*           e.printStackTrace();
*       }
*       return(-1); // Failed
*   }
* }
*******************************************************************/

/*****************************************************
*
******************************************************/

DOSE_SHARED_DATA_S * Get_NodeSharedData_Pointer(void)
{
    //printf("Get_NodeSharedData_Pointer()\n");

    return((DOSE_SHARED_DATA_S *)
            DoseOs::Shared_Memory_Create(sizeof(DOSE_SHARED_DATA_S)));
}

/*********************************************
*
**********************************************/
static void IpAddr_ToString(unsigned long IpAddr_nw, char *StrIpAddr)
{
    int pos = 0;

    for(int jj = 0 ; jj< 4 ; jj++)
    {
        sprintf(&StrIpAddr[pos], "%lu.", (IpAddr_nw>>(8*jj)) & 0xFF);
        pos = strlen(StrIpAddr);
    }
    StrIpAddr[pos-1] = 0; // remove last '.'
}

/*******************************************************
* Called by JavaDoseMon via Java_DoseJni_GetInfo()
* to get info about NodeStatus
*******************************************************/
static int Get_Status_Info(DOSE_SHARED_DATA_S *pShm, char *buff)
{
    int     pos;
    unsigned long   WillStartSoon;
    char    MyIpAddrBuff[48];
    char    IpMcAddrBuff[48];
    char    IpNetAddrBuff[48];


    IpAddr_ToString(pShm->MyIpAddr_nw, MyIpAddrBuff);
    IpAddr_ToString(pShm->IpMulticastAddr_nw, IpMcAddrBuff);
    IpAddr_ToString(pShm->NetAddr_nw, IpNetAddrBuff);


    // G_pShm->PoolDistributionWillStartSoon holds time from GetTickCount()
    // So I show expired time time instead of absolute time in ticks
    if(pShm->PoolDistributionWillStartSoon)
        WillStartSoon = 1;//GetTickCount()-pShm->PoolDistributionWillStartSoon;
    else
        WillStartSoon = 0;

    sprintf(buff,
            "My IpAddr         = %s\r\n"
            "IP MulticastAddr  = %s\r\n"
            "IP NetAddr        = %s\r\n"
            "New               = %08X  %08X\r\n"
            "Up                = %08X  %08X\r\n"
            "Down              = %08X  %08X\r\n"
            "ToBePoolDistr     = %08X  %08X\r\n"
            "BeingPoolDistr    = %08X  %08X\r\n"
            "LatestPoolDistr   = %08X  %08X\r\n\r\n"
            "PoolDistributionWillStartSoon = %lu\r\n"   // ms has expired\r\n"
            "PoolDistributionIsInProgress  = %d\r\n"
            "PoolDistributionWillEndSoon   = %d\r\n\r\n"
            ,
            MyIpAddrBuff, IpMcAddrBuff, IpNetAddrBuff,
            (dcom_ulong32)(pShm->BitMapNodesNew64>>32),
            (dcom_ulong32)(pShm->BitMapNodesNew64 & 0xFFFFFFFF),
            (dcom_ulong32)(pShm->BitMapNodesUp64>>32),
            (dcom_ulong32)(pShm->BitMapNodesUp64 & 0xFFFFFFFF),
            (dcom_ulong32)(pShm->BitMapNodesDown64>>32),
            (dcom_ulong32)(pShm->BitMapNodesDown64 & 0xFFFFFFFF),
            (dcom_ulong32)(pShm->BitMapToBePoolDistributed64>>32),
            (dcom_ulong32)(pShm->BitMapToBePoolDistributed64 & 0xFFFFFFFF),
            (dcom_ulong32)(pShm->BitMapBeingPoolDistributed64>>32),
            (dcom_ulong32)(pShm->BitMapBeingPoolDistributed64 & 0xFFFFFFFF),
            (dcom_ulong32)(pShm->BitMapLatestPoolDistributed64>>32),
            (dcom_ulong32)(pShm->BitMapLatestPoolDistributed64 & 0xFFFFFFFF),
            WillStartSoon,
            pShm->PoolDistributionIsInProgress,
            pShm->PoolDistributionWillEndSoon);

    pos = strlen(buff);
    sprintf(&buff[pos],
            "Statistics:\r\n"
            "Tot ReceiveCount      = %d\r\n"
            "Tot TransmitCount     = %d\r\n"
            "ReTransmitCount       = %d\r\n"
            "LostAckCount          = %d\r\n"
            //"BuffOverFlowCount     = %d\r\n"
            "ReceiveQueueFullCount = %d\r\n"
            "TransmitQueueFullCount= %d\r\n"
            ,
            pShm->Statistics.TotRxCount,
            pShm->Statistics.TotTxCount,
            pShm->Statistics.ReTxCount,
            pShm->Statistics.LostAckCount,
            pShm->Statistics.ReceiveQueueFullCount,
            pShm->Statistics.TransmitQueueFullCount);

    return(strlen(buff));
}
/************************************************************
* Called from Java - be sure parameters match.
*
* Cmds:
* 'D' - Set/Get Debug level
* 'S' - Get status of all nodes
*
*************************************************************/
extern "C" JNIEXPORT jint JNICALL
Java_DoseJni_GetInfo(JNIEnv *env, jclass, // myclass,
                     int Cmd, int Param,
                     jbyteArray arr)
{
    int count = 0;
    jbyte buf[1024];    // be sure this is big enough
    DOSE_SHARED_DATA_S *pShm = NULL;
    dcom_ulong64 BitMap64;


    //printf("DoseJni_GetInfo(%X)\n", Cmd);

    pShm = (DOSE_SHARED_DATA_S *) Get_NodeSharedData_Pointer();
    if(pShm == NULL) return(-1);

    //-----------------------------
    // Debug++/--
    //-----------------------------

    if(Cmd == 'D')
    {

        if(pShm->Debug + Param < 0)
        {
            pShm->Debug = 0;
            return(pShm->Debug);
        }

        if(Cmd != 0) pShm->Debug += Param;

        return(pShm->Debug);  // ret new value
    }

    //----------------------------------
    // Node status
    //----------------------------------

    if(Cmd == 'N') // Get NodeStatus
    {
        buf[2] = 17; buf[3] = 13;
        int jx;

        for (jx= 0 ; jx< 64 ; jx++)
        {
            if(pShm->NodeStatusTable[jx].Status == 0) continue;

            buf[8*count]   = (jbyte) jx; // DoseId
            buf[8*count+1] = pShm->NodeStatusTable[jx].Status;
            buf[8*count+2] = 0; // spare
            //buf[8*count+3] =  // see below
            buf[8*count+4]
                    = (dcom_uchar8)(pShm->NodeStatusTable[jx].IpAddr_nw & 0xFF);
            buf[8*count+5]
                    = (dcom_uchar8)((pShm->NodeStatusTable[jx].IpAddr_nw>>8) & 0xFF);
            buf[8*count+6]
                    = (dcom_uchar8)((pShm->NodeStatusTable[jx].IpAddr_nw>>16) & 0xFF);
            buf[8*count+7]
                    = (dcom_uchar8)((pShm->NodeStatusTable[jx].IpAddr_nw>>24) & 0xFF);

            BitMap64 = (dcom_ulong64) 1 << jx;

            if(pShm->NodeStatusTable[jx].IpAddr_nw == pShm->MyIpAddr_nw) // Me
            {
                buf[8*count+1] = 'M';  // This is Me

                // Color in 'xtra' field for node 'Me'.
                // Blue if sending, or shall send a PD to any node. Else yellow.

                if(pShm->BitMapToBePoolDistributed64 | pShm->BitMapBeingPoolDistributed64)
                    buf[8*count+3] = 'N';
                else
                    buf[8*count+3] = 'M';
            }

            else // Not Me
            // Indicates how I belive others see me.
            // Note myself is allways 'NEW'
            {
                //BitMap64 = (dcom_ulong64) 1 << jx;
                if(pShm->NodeStatusTable[jx].Status == 'U')
                {
                    if((pShm->BitMapToBePoolDistributed64
                        | pShm->BitMapBeingPoolDistributed64) & BitMap64)
                        buf[8*count+3] = 'N';
                    else
                        buf[8*count+3] = 'U';
                }
                else
                if(pShm->NodeStatusTable[jx].Status == 'N')
                {
                    if(((pShm->BitMapToBePoolDistributed64
                        | pShm->BitMapBeingPoolDistributed64) & BitMap64) == 0)
                        buf[8*count+3] = 'U';
                    else
                        buf[8*count+3] = 'N';
                }
            }

            //printf("DLL: buf[%d] = %X\n", 8*count, buf[8*count]);
            count++;
        }
        if(count != 0) // copy data from buff to arr
            env->SetByteArrayRegion(arr, 0, 8*count, buf);

        return(count);  // num nodes
    }

    if(Cmd == 'S')
    {
        count = Get_Status_Info(pShm, (char *) buf);

        env->SetByteArrayRegion(arr,0,count,buf); // copy data from buff to arr
        return(count);  // length of buf
    }
    //----------------------------------
    // Add more here


    //----------------------------------
    //for(i=0 ; i<8 ; i++) buf[i] = (jbyte) ('A' + i);
    // copy buff to arr
    //env->SetByteArrayRegion(arr, 0, 8, buf);  // copy data from buff to arr
    //----------------------------------

    return (0);
}

/*------------------- end DoseJni.cpp ---------------*/
