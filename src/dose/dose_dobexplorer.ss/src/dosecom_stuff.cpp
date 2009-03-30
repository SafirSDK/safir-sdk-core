/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
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
#include "dosecom_stuff.h"
#include <stdio.h>
#include <stdarg.h>

#if defined(linux) || defined(__linux) || defined(__linux__)
#define _vsnprintf vsnprintf
#endif


#define IS_USING_SOCKETS


extern void PrintDbg( const char *format, ... );
extern void PrintErr(int ErrorCode, const char *format, ... );

#include <Safir/Dob/Internal/DoseComAux/DoseOsInterface.h>


static int Debug = 0;

int volatile * volatile pDbg = &Debug;  // DoseOsSharedMem.cpp needs it

void PrintDbg( const char *format, ... )
{
    va_list marker;
    char    buffer[1024];

    va_start( marker, format); // Initialize variable arguments.
    _vsnprintf(buffer, sizeof(buffer), format, marker);
    va_end( marker ); // needed ? Reset variable arguments

    printf("%s", buffer); fflush(stdout);
}

/**********************************************************
* Args as printf
***********************************************************/

void PrintErr(int ErrorCode, const char *format, ... )
{
    va_list marker;
    char    Buf1[400];

    va_start( marker, format );     // Initialize variable arguments.
    _vsnprintf(Buf1, sizeof(Buf1), format, marker);
    va_end( marker ); // needed ? Reset variable arguments

    printf("ERROR [%d]: %s\n", ErrorCode, Buf1);
}


DOSE_SHARED_DATA_S * Get_NodeSharedData_Pointer(void)
{
    //printf("Get_NodeSharedData_Pointer()\n");
    static DOSE_SHARED_DATA_S * ptr = (DOSE_SHARED_DATA_S *)
        DoseOs::Shared_Memory_Create(sizeof(DOSE_SHARED_DATA_S));
    return ptr;
}

/*********************************************
*
**********************************************/
void IpAddr_ToString(unsigned long IpAddr_nw, char *StrIpAddr)
{
    int pos = 0;

    for(int jj = 0 ; jj< 4 ; jj++)
    {
        sprintf(&StrIpAddr[pos], "%lu.", (IpAddr_nw>>(8*jj)) & 0xFF);
        pos = strlen(StrIpAddr);
    }
    StrIpAddr[pos-1] = 0; // remove last '.'
}


int Get_Status_Info(DOSE_SHARED_DATA_S *pShm, char *buff)
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
            "New               = %08lX  %08lX\r\n"
            "Up                = %08lX  %08lX\r\n"
            "Down              = %08lX  %08lX\r\n"
            "ToBePoolDistr     = %08lX  %08lX\r\n"
            "BeingPoolDistr    = %08lX  %08lX\r\n"
            "LatestPoolDistr   = %08lX  %08lX\r\n\r\n"
            "PoolDistributionWillStartSoon = %lu\r\n"   // ms has expired\r\n"
            "PoolDistributionIsInProgress  = %ld\r\n"
            "PoolDistributionWillEndSoon   = %ld\r\n\r\n"
            ,
            MyIpAddrBuff, IpMcAddrBuff, IpNetAddrBuff,
            (ulong)(pShm->BitMapNodesNew64>>32),
            (ulong)(pShm->BitMapNodesNew64 & 0xFFFFFFFF),
            (ulong)(pShm->BitMapNodesUp64>>32),
            (ulong)(pShm->BitMapNodesUp64 & 0xFFFFFFFF),
            (ulong)(pShm->BitMapNodesDown64>>32),
            (ulong)(pShm->BitMapNodesDown64 & 0xFFFFFFFF),
            (ulong)(pShm->BitMapToBePoolDistributed64>>32),
            (ulong)(pShm->BitMapToBePoolDistributed64 & 0xFFFFFFFF),
            (ulong)(pShm->BitMapBeingPoolDistributed64>>32),
            (ulong)(pShm->BitMapBeingPoolDistributed64 & 0xFFFFFFFF),
            (ulong)(pShm->BitMapLatestPoolDistributed64>>32),
            (ulong)(pShm->BitMapLatestPoolDistributed64 & 0xFFFFFFFF),
            WillStartSoon,
            pShm->PoolDistributionIsInProgress,
            pShm->PoolDistributionWillEndSoon);

    pos = strlen(buff);
    sprintf(&buff[pos],
            "Statistics:\r\n"
            "Tot ReceiveCount      = %ld\r\n"
            "Tot TransmitCount     = %ld\r\n"
            "ReTransmitCount       = %ld\r\n"
            "LostAckCount          = %ld\r\n"
            //"BuffOverFlowCount     = %d\r\n"
            "ReceiveQueueFullCount = %ld\r\n"
            "TransmitQueueFullCount= %ld\r\n"
            ,
            pShm->Statistics.TotRxCount,
            pShm->Statistics.TotTxCount,
            pShm->Statistics.ReTxCount,
            pShm->Statistics.LostAckCount,
            pShm->Statistics.ReceiveQueueFullCount,
            pShm->Statistics.TransmitQueueFullCount);

    return(strlen(buff));
}
