/******************************************************************************
*
* Copyright Saab AB, 2003-2013 (http://safir.sourceforge.net)
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

/*************************************************************************
* DoseNodeStatus.cpp - a part of DoseComDll for LINUX and WIN32
*
* Contains a table with information of all nodes and
* routines to access it.
**************************************************************************/
#ifndef _CRT_SECURE_NO_DEPRECATE
#define _CRT_SECURE_NO_DEPRECATE
#endif

#include "DosePlatform.h"
#include "DoseOsInterface.h"
#include "DoseComConfig.h"
#include "DoseComReceive.h"
#include "../defs/DoseNodeStatus.h"
#include "PrintError.h"

static unsigned long g_TickPrev = 0;

//-----------------------
// externals
//-----------------------

extern void WakeUp_NodeChange(void);
extern void WakeUp_DistributePool(void);
extern void WakeUp_RequestPoolDistribution(int doseId);

//--------------------------------------------------------
// Local non-shared data
//--------------------------------------------------------

volatile static int Debug = 0; // Define initial value of *pDbg here

volatile int * volatile pDbg = &Debug;    // will be set to point to &pShm->Debug

static NODESTATUS_TABLE     *g_pNodeStatusTable = NULL;
static DOSE_SHARED_DATA_S   *g_pShm = NULL;

/********************************************************************
* Used By DoseCom and DoseMonitor to connect to shared memory
********************************************************************/
#define DOSE_SHAREDMEM_NAME "DoseComDll_Shared_Mem"

static DOSE_SHARED_DATA_S *Initialize_Shared_Memory(void)  //to learn - template
{
    DOSE_SHARED_DATA_S *pShm;

    if(*pDbg) PrintDbg("Initialize_Shared_Memory(start)\n");

    pShm = (DOSE_SHARED_DATA_S *) DoseOs::Shared_Memory_Create(sizeof(DOSE_SHARED_DATA_S));
    if(pShm != NULL)
    {
        if(*pDbg) PrintDbg("Open() shared memory OK: %s\n", DOSE_SHAREDMEM_NAME);
        pDbg = &pShm->Debug;
        return(pShm);
    }

    PrintErr(0,"Can not Create shared memory\n");
    return(NULL);
}

/******************************************************************
* Used by DoseComMain.cpp - DoseCom_Init at startup
*
* Set up local pointers to our shared data
*
* mode is not used
******************************************************************/

int CNodeStatus::InitNodeStatus(int) // mode)
{
    int Tmp;
    int DbgSetByCfg;

    // Initialize_Shared_Memory() will set *pDbg to point at shm
    // Because of this, save current value which might have been
    // set by cfg file.
    // After return from Initialize_Shared_Memory(), pDbg point at
    // Debug variable in shm. This might have been set by DoseMon.

    DbgSetByCfg = *pDbg;

    if(g_pShm != NULL) return(0); // already done

    g_pShm = Initialize_Shared_Memory();

    if(g_pShm == NULL)
    {
        PrintErr(0, "Initialize_Shared_Memory() failed\n");
        return(-1);
    }

    g_pNodeStatusTable   = g_pShm->NodeStatusTable;

    // Now we have two values on Debug - Which shall we use ?
    // DbgSetByCfg   - Set by config file
    // *pDbg         - set byDoseMon
    //

    if(DbgSetByCfg > *pDbg) *pDbg = DbgSetByCfg; // Use biggest

    //---------------------------------------------------------
    // Put data in shared memory so DoseMonitor can access it.
    //---------------------------------------------------------
    Tmp = g_pShm->Debug;  // save it in case DoseMon has set it

    memset((void*)g_pShm,0,sizeof(DOSE_SHARED_DATA_S));

    g_pShm->Debug               = Tmp;
    g_pShm->MyIpAddr_nw         = CConfig::m_MyIpAddr_nw;
    g_pShm->IpMulticastAddr_nw  = CConfig::m_BaseIpMultiCastAddr_nw;
    g_pShm->NetAddr_nw          = CConfig::m_NetAddr_nw;
    g_pShm->MyDoseId            = CConfig::m_MyDoseId;
    g_pShm->DosePort            = CConfig::m_Dose_KeepAlivePort;

    return(0);
}

/******************************************************************
*
******************************************************************/
CNodeStatus::CNodeStatus()
{
    ;
}

/******************************************************************
* Used by DoseComMain.cpp -
*    DoseCommunicationC_GetNodeUp, DoseCommunicationC_GetNodeDown
*
******************************************************************/
int CNodeStatus::GetNodeInfo(dcom_ushort16 DoseId, dcom_ulong32 *pIpAddr,
                             dcom_ulong32 *pNodeStatus)
{
    if(DoseId >= NODESTATUS_TABLE_SIZE) return(-1);

    if(pIpAddr != NULL)
        *pIpAddr = g_pNodeStatusTable[DoseId].IpAddr_nw;

    if(pNodeStatus != NULL)
        *pNodeStatus = g_pNodeStatusTable[DoseId].Status;

    return(0);
}

/***********************************************************************
* Used by DoseComNodeStatus.cpp
*       - CheckTimedOutNodes(), UpdateNode_Up()
* Context: KeepAlive Thread
*
* Everytime somethings changes, NodeStatusBitMap is recalculated
***********************************************************************/
void CNodeStatus::UpdateNodeStatusBitMap(void)
{
    int jj;
    dcom_ulong64 BitMapUp64   = (dcom_ulong64) 0;
    dcom_ulong64 BitMapDown64 = (dcom_ulong64) 0;
    dcom_ulong64 BitMapNew64  = (dcom_ulong64) 0;
    dcom_ulong64 BitMapToBePd64  = (dcom_ulong64) 0;

    for (jj=0 ; jj< 64 ; jj++)
    {
        if(g_pNodeStatusTable[jj].Status == NODESTATUS_UP)
            BitMapUp64 |= ((dcom_ulong64)1<<jj);
        else
        if(g_pNodeStatusTable[jj].Status == NODESTATUS_DOWN)
            BitMapDown64 |= ((dcom_ulong64)1<<jj);
        else
        if(g_pNodeStatusTable[jj].Status == NODESTATUS_NEW)
            BitMapNew64 |= ((dcom_ulong64)1<<jj);

        if(g_pNodeStatusTable[jj].ToBePoolDistributed)
            BitMapToBePd64 |= ((dcom_ulong64)1<<jj);
    }

    // New are Up or New but not Up or New before

    //06-02-16
    // Note: BitMapToBePoolDistributed64 will be set to 0 after it is
    // copied to BitMapBeingPoolDistributed64 in routine CheckTimedOutNodes()
    // which checks if it is time to start a PoolDistribution.
    // So here we 'or' new nodes.

    if(BitMapToBePd64 != (dcom_ulong64)0)
    {
        g_pShm->BitMapToBePoolDistributed64 |= BitMapToBePd64;

        // Clear my own bit
        g_pShm->BitMapToBePoolDistributed64
                     &= ~((dcom_ulong64)1<< (g_pShm->MyDoseId & 0x3F));
    }

    g_pShm->BitMapNodesUp64   = BitMapUp64;
    g_pShm->BitMapNodesNew64  = BitMapNew64;
    g_pShm->BitMapNodesDown64 = BitMapDown64;

    if(*pDbg>1)
        PrintDbg("    UpdateNodeStatusBitMap()."
                 " Setting BitMapToBePd=%X.%08X  N=%X.%08X U=%X.%08X\n",
                 (dcom_ulong32) (g_pShm->BitMapToBePoolDistributed64>>32),
                 (dcom_ulong32) (g_pShm->BitMapToBePoolDistributed64  & 0xFFFFFFFF),
                 (dcom_ulong32) (BitMapNew64>>32), (dcom_ulong32) (BitMapNew64 & 0xFFFFFFFF),
                 (dcom_ulong32) (BitMapUp64>>32), (dcom_ulong32) (BitMapNew64 & 0xFFFFFFFF));
}
/*----------------- end UpdateNodeStatusBitMap() -----------*/

/****************************************************************************
* private
* Called from CNodeStatus::UpdateNode_Up() when a node with status
* NODESTATUS_NEW or _UP and incorrect timestamp is detected. It shall be
* registered as down. Compare with code in CNodeStatus::CheckTimedOutNodes().
* It will be _NEW when next msg arrives.
******************************************************************************/
void CNodeStatus::SetNodeDownWhenInvalidTimeStamp(dcom_uchar8 DoseId)
{
    PrintErr(0, "*** A node with invalid TimeStamp. DoseId=%d", DoseId);
    PrintErr(0, "*** Check for multiple nodes with the same DoseId on the network!", DoseId);

    
    g_pNodeStatusTable[DoseId].Status = NODESTATUS_DOWN;
    CDoseComReceive::UpdateNodeUp((dcom_uchar8) DoseId);
    g_pNodeStatusTable[DoseId].ToBeGivenToAppl       = 2;
    g_pNodeStatusTable[DoseId].ToBePoolDistributed   = 0;
    g_pNodeStatusTable[DoseId].HasReceivedPdComplete = 0;
    g_pNodeStatusTable[DoseId].HasReceivedPdStart = 0;
    g_pNodeStatusTable[DoseId].ForcePoolDistribution = 0;

    UpdateNodeStatusBitMap();
    WakeUp_NodeChange();
}
/*------------------- end SetNodeDownWhenInvalidTimeStamp() ---------------*/

/****************************************************************
* Used by:
* DoseKeepAlive.cpp - DOSE_KeepAlive_Init, KeepAlive_Thread
*
* Called every time a KeepAlive msg is received.
* Check if a new node.
*
* Returns:
*  0 - if no change
*****************************************************************/
int CNodeStatus::UpdateNode_Up(unsigned char DoseId,
                               dcom_ulong32 IpAddr_nw, dcom_ulong32 TimeStamp)
{
    unsigned long TickNow;


    TickNow = DoseOs::Get_TickCount();
    //PrintDbg("%4d UpdateNode_Up(DoseId=%2d,IP=%s Status=%c)\n",
    //          TickNow - TickPrev, DoseId,
    //          DoseOsIn::Inet_Ntoa(IpAddr_nw), g_pNodeStatusTable[DoseId].Status);

    g_TickPrev = TickNow;

    if(DoseId >= NODESTATUS_TABLE_SIZE) return(0); // error - nothing to do

    // Already UP - this is the normal case
    if(g_pNodeStatusTable[DoseId].Status == NODESTATUS_UP) // already OK
    {
        // This might happen if a a remote DoseMain is terminated then
        // restarted before a timeout is detected.
        if(TimeStamp != g_pNodeStatusTable[DoseId].TimeStamp) //06-03-07
            SetNodeDownWhenInvalidTimeStamp(DoseId);
        else
            g_pNodeStatusTable[DoseId].LatestTime = DoseOs::Get_TickCount();
        return(0);
    }

    // Already NEW
    if(g_pNodeStatusTable[DoseId].Status == NODESTATUS_NEW)
    {
        if(TimeStamp != g_pNodeStatusTable[DoseId].TimeStamp) //06-03-07
        {
            SetNodeDownWhenInvalidTimeStamp(DoseId);
            return(0);
        }

        g_pNodeStatusTable[DoseId].LatestTime = DoseOs::Get_TickCount();

        // Check if RxThread has signaled 'got a PD msg'

        if(g_pNodeStatusTable[DoseId].HasReceivedPdComplete)
        {
            g_pNodeStatusTable[DoseId].HasReceivedPdComplete = 0;
            g_pNodeStatusTable[DoseId].HasReceivedPdStart = 0;
            g_pNodeStatusTable[DoseId].Status                = NODESTATUS_UP;
            g_pNodeStatusTable[DoseId].ToBeGivenToAppl       = 3;

            //---------------------???????
            // To be checked
            // The line g_pNodeStatusTable[DoseId].ToBePoolDistributed  = 0;
            // was used in UPS releases, but not in my code, which was like:.
            // ???? no // g_pNodeStatusTable[DoseId].ToBePoolDistributed=0; //070720
            // Why did I remove it 070720 without sending to UPS ??

            // PrintDbg("UpdateNode_Up() Already new. ToBePoolDistributed=%d Status --> UP\n",
            //      g_pNodeStatusTable[DoseId].ToBePoolDistributed);

            g_pNodeStatusTable[DoseId].ToBePoolDistributed  = 0;  // 070720

            UpdateNodeStatusBitMap();

            WakeUp_NodeChange();  // Send an event to application
        }
        return(0);
    }

    // Switched from DOWN to NEW (can't happen to myself)
    if(g_pNodeStatusTable[DoseId].Status == NODESTATUS_DOWN)
    {
        g_pNodeStatusTable[DoseId].LatestTime = DoseOs::Get_TickCount();
        g_pNodeStatusTable[DoseId].Status     = NODESTATUS_NEW;
        g_pNodeStatusTable[DoseId].TimeStamp  = TimeStamp; //06-03-07
        g_pNodeStatusTable[DoseId].NewTime  = DoseOs::Get_TickCount(); 
        g_pNodeStatusTable[DoseId].ToBeGivenToAppl     = 2;
        g_pNodeStatusTable[DoseId].ToBePoolDistributed = 2;

        if(IpAddr_nw != g_pNodeStatusTable[DoseId].IpAddr_nw)
        {
            // If restarted with new address from DHCP
            g_pNodeStatusTable[DoseId].IpAddr_nw  = IpAddr_nw; 
            CConfig::Add_UnicastIpAddr(DoseId, IpAddr_nw);
        }

    }
    // A new node
    else
    {
        if(IpAddr_nw != CConfig::m_MyIpAddr_nw)
        {
            g_pNodeStatusTable[DoseId].ToBeGivenToAppl     = 1;
            g_pNodeStatusTable[DoseId].ToBePoolDistributed = 1;
        }

        g_pNodeStatusTable[DoseId].LatestTime = DoseOs::Get_TickCount();
        g_pNodeStatusTable[DoseId].Status     = NODESTATUS_NEW;
        g_pNodeStatusTable[DoseId].IpAddr_nw  = IpAddr_nw;
        g_pNodeStatusTable[DoseId].TimeStamp  = TimeStamp; //06-03-07
        g_pNodeStatusTable[DoseId].NewTime  = DoseOs::Get_TickCount(); 

        CConfig::Add_UnicastIpAddr(DoseId, IpAddr_nw);
    }

    //---------------------------------------
    // We come here if a new node is _NEW
    //---------------------------------------

    if(DoseId > g_pShm->MaxUsedDoseId)
        g_pShm->MaxUsedDoseId = DoseId;

    UpdateNodeStatusBitMap();

    if(IpAddr_nw != CConfig::m_MyIpAddr_nw)
    {
        g_pShm->PoolDistributionWillStartSoon = DoseOs::Get_TickCount();

        if(g_pShm->PoolDistributionWillStartSoon == 0) // Must not be 0
            g_pShm->PoolDistributionWillStartSoon = 0xFFFFFFFF;

        WakeUp_NodeChange();  // Send an event to application
    }

    return(NODESTATUS_UP);
}
/*---------------- end UpdateNode_Up() ------------------*/

/****************************************************************
* Used by DoseComKeepAlive.cpp - KeepAlive_Thread
*
* Called at regular intervals to:
* 1) Search in NodeStatusTable[] for nodes that has timed out
* 2) Check if it is time for a PoolDistribution
*
* Returns: Number of changed nodes
*
*****************************************************************/
int CNodeStatus::CheckTimedOutNodes(void)
{
    int     jj;
    int     ChangeCount = 0;
    dcom_ulong32   dwCurrentTime;
    dcom_ulong32   TickNow;


    //if(*pDbg>3) PrintDbg("CheckTimedOutNodes()\n");

    dwCurrentTime = DoseOs::Get_TickCount();

    for (jj=0 ; jj<NODESTATUS_TABLE_SIZE ; jj++)
    {
        if(CConfig::m_MyIpAddr_nw == g_pNodeStatusTable[jj].IpAddr_nw) // this is me
            continue;

        if((g_pNodeStatusTable[jj].Status == NODESTATUS_UP)
         || (g_pNodeStatusTable[jj].Status == NODESTATUS_NEW) )
        {
            if((dwCurrentTime - g_pNodeStatusTable[jj].LatestTime)
                            > KEEP_ALIVE_TIMEOUT)
            {
                if(*pDbg>2)
                    PrintDbg("*** A node timed out. IP=%u.%u.%u.%u\n",
                            g_pNodeStatusTable[jj].IpAddr_nw & 0xFF,
                            (g_pNodeStatusTable[jj].IpAddr_nw>>8) & 0xFF,
                            (g_pNodeStatusTable[jj].IpAddr_nw>>16) & 0xFF,
                            (g_pNodeStatusTable[jj].IpAddr_nw>>24) & 0xFF);


                TickNow = DoseOs::Get_TickCount();

                if(*pDbg)
                PrintDbg("%4d *** A node timed out. IP=%u.%u.%u.%u\n",
                            TickNow - g_TickPrev,
                            g_pNodeStatusTable[jj].IpAddr_nw & 0xFF,
                            (g_pNodeStatusTable[jj].IpAddr_nw>>8) & 0xFF,
                            (g_pNodeStatusTable[jj].IpAddr_nw>>16) & 0xFF,
                            (g_pNodeStatusTable[jj].IpAddr_nw>>24) & 0xFF);
                g_TickPrev = TickNow;

                g_pNodeStatusTable[jj].Status = NODESTATUS_DOWN;
                CDoseComReceive::UpdateNodeUp((dcom_uchar8) jj);
                g_pNodeStatusTable[jj].ToBeGivenToAppl       = 2;
                g_pNodeStatusTable[jj].ToBePoolDistributed   = 0;
                g_pNodeStatusTable[jj].HasReceivedPdComplete = 0;
                g_pNodeStatusTable[jj].HasReceivedPdStart = 0;
                g_pNodeStatusTable[jj].ForcePoolDistribution = 0;
                ChangeCount++;
            }
            // 2010-06-14 - miwn: added check of timeout if stuck in status new and no pd start have been received.
            else if (g_pNodeStatusTable[jj].Status == NODESTATUS_NEW && 
                !g_pNodeStatusTable[jj].HasReceivedPdStart && 
                !g_pNodeStatusTable[jj].HasReceivedPdComplete) 
            {
                if((dwCurrentTime - g_pNodeStatusTable[jj].NewTime) > NODESTATUS_NEW_TIMEOUT)
                {
                    TickNow = DoseOs::Get_TickCount();
                    if(*pDbg)
                        PrintDbg("%4d *** Node %d timed out in status NEW. Request PD.\n", TickNow - g_TickPrev, jj);
                    // save time so we send request once and can retry later.
                    g_pNodeStatusTable[jj].NewTime = TickNow; 
                    //
                    // kick application to send request for pool distribution
                    WakeUp_RequestPoolDistribution(jj);
                    if(*pDbg>2)
                        PrintDbg("%4d *** A node is stuck in status NEW, request PD from: IP=%u.%u.%u.%u\n",
                                 TickNow - g_TickPrev,
                                 g_pNodeStatusTable[jj].IpAddr_nw & 0xFF,
                                (g_pNodeStatusTable[jj].IpAddr_nw>>8) & 0xFF,
                                (g_pNodeStatusTable[jj].IpAddr_nw>>16) & 0xFF,
                                (g_pNodeStatusTable[jj].IpAddr_nw>>24) & 0xFF);
                    g_TickPrev = TickNow;
                }
            }
            // 2010-11-17 - miwn: check if force pool distribution.
            if (g_pNodeStatusTable[jj].ForcePoolDistribution &&
                !g_pNodeStatusTable[jj].ToBePoolDistributed) 
            {
                g_pNodeStatusTable[jj].ToBePoolDistributed = 3; // 3 indicates force pool distribution
                g_pNodeStatusTable[jj].ForcePoolDistribution = 0; // job done, clear flag

                // activate pool distribution
                g_pShm->PoolDistributionWillStartSoon = DoseOs::Get_TickCount();

                if(g_pShm->PoolDistributionWillStartSoon == 0) // Must not be 0
                    g_pShm->PoolDistributionWillStartSoon = 0xFFFFFFFF;

                ChangeCount++;
            }
        }
    }

    if(ChangeCount)
    {
        UpdateNodeStatusBitMap();
        WakeUp_NodeChange();
    }

    //-----------------------------------------------
    // Check if it is time for a PoolDistribution
    //-----------------------------------------------

    if(    (g_pShm->PoolDistributionWillStartSoon)
        && (!g_pShm->PoolDistributionIsInProgress))
    {
        if((dwCurrentTime - g_pShm->PoolDistributionWillStartSoon)
            > POOLDISTRIBUTION_DELAYTIME )
        {
            g_pShm->BitMapBeingPoolDistributed64 =
                                g_pShm->BitMapToBePoolDistributed64;

            g_pShm->BitMapLatestPoolDistributed64 =
                                g_pShm->BitMapToBePoolDistributed64;

            g_pShm->BitMapToBePoolDistributed64 = (dcom_ulong64)0;

            for (jj=0 ; jj<NODESTATUS_TABLE_SIZE ; jj++)
            {
                if(g_pNodeStatusTable[jj].ToBePoolDistributed)
                {
                    g_pNodeStatusTable[jj].ToBePoolDistributed = 0;
                }
            }
            if(*pDbg>1)
                PrintDbg("CheckTimedOutNodes()."
                    " Setting BitMapBeingPd=%X.%08X (clear ToBePd)\n",
                    (dcom_ulong32)(g_pShm->BitMapBeingPoolDistributed64>>32),
                    (dcom_ulong32)(g_pShm->BitMapBeingPoolDistributed64 & 0xFFFFFFFF));

            g_pShm->PoolDistributionWillStartSoon = 0;
            g_pShm->PoolDistributionIsInProgress = 1;
            WakeUp_DistributePool(); // set event to appl
        }
    }
    return(ChangeCount);
}
/*------------------- end CheckTimedOutNodes() ---------------*/

/****************************************************************
* Used by DoseComMain.cpp - DoseCom_ForcePoolDistribution
*
* Trigger a pooldistribution to DoseId
*
*****************************************************************/
void CNodeStatus::ForcePoolDistribution(int DoseId)
{
    g_pNodeStatusTable[DoseId].ForcePoolDistribution = 1;
    CDoseComReceive::UpdateNodeUp((dcom_uchar8) DoseId);
}

/****************************************************************
* Used by DoseComMain.cpp - DoseCommunicationC_GetNodeChange
*
* Search in NodeStatusTable[] for nodes is marked with ''
*
* Returns: Current DoseId (index in table) -
*          to be used as next start search index
*
*****************************************************************/
dcom_ulong32 CNodeStatus::GetNextChangedNode(unsigned char *pNodeStatus,
                                             dcom_ulong32 *pIpAddr_nw)
{
    int jj;


    for (jj=0 ; jj<NODESTATUS_TABLE_SIZE ; jj++)
    {
        if(g_pNodeStatusTable[jj].ToBeGivenToAppl == 0) // not this
            continue;

        g_pNodeStatusTable[jj].ToBeGivenToAppl = 0; // clear flag

        if(jj == g_pShm->MyDoseId) // do not return own
            continue;

        *pNodeStatus = g_pNodeStatusTable[jj].Status;
        *pIpAddr_nw  = g_pNodeStatusTable[jj].IpAddr_nw;

        break;
    }
    return(jj);
}
/*********************************************************
* Called from RxThread when a PdIsComplete msg is received
* This will set NodeStatus to _UP
**********************************************************/

void CNodeStatus::Set_HasReceivedPdComplete(int DoseId)
{
    g_pNodeStatusTable[DoseId].HasReceivedPdComplete = 1;
    //PrintDbg("Set_HasReceivedPdComplete()\n");
}

/*********************************************************
* Called from RxThread when the first Pd msg is received
**********************************************************/

void CNodeStatus::Set_HasReceivedPdStart(int DoseId)
{
    g_pNodeStatusTable[DoseId].HasReceivedPdStart = 1;
    //PrintDbg("Set_HasReceivedPdStart()\n");
}

/******************************************************************
* Used by:
* DoseComKeepAlive.cpp - DOSE_KeepAlive_Init()
* DoseComReceive.cpp   - DOSE_Receive_Init()
* DoseComTxData.cpp    - DOSE_Xmit_Init()
******************************************************************/
DOSE_SHARED_DATA_S * CNodeStatus::GetNodeSharedDataPointer(void)
{
    if(g_pShm != NULL) return(g_pShm);
    g_pShm = Initialize_Shared_Memory();
    return(g_pShm);
}

/******************************************************************
*  This is for DoseCom Monitor only
******************************************************************/
DOSE_SHARED_DATA_S * Get_NodeSharedData_Pointer(void)
{
    //if(*pDbg>3) PrintDbg("Get_NodeSharedData_Pointer() g_pShm=%X\n", g_pShm);

    if(g_pShm != NULL) return(g_pShm);
    g_pShm = Initialize_Shared_Memory();

    return(g_pShm);
}

/**************************************************************************
* Called by KeepAliveThread to send a message as respnse to an UDP request
* Can consume 28 + MAX_NUM_DEST_CHANNELS * 21 chars = 28+32*21 = 700
*************************************************************************/
void CNodeStatus::Get_Info(char *pBuff)
{
    sprintf(pBuff,
        "NodeStatus Info - to be implemented DoseId %d\n", CConfig::m_MyDoseId);
#ifdef NOT_ANY_MORE //???????
    for(int ChNum = 0 ; ChNum < MAX_NUM_DEST_CHANNELS ; ChNum++)
    {
        if( (g_pShm->BitMapDestChannelMembers[1][ChNum]
           | g_pShm->BitMapDestChannelMembers[0][ChNum]) != 0)
        {
            pos = strlen(pBuff);
            sprintf(&pBuff[pos], "%2X %08X %08X\n",ChNum,
                    g_pShm->BitMapDestChannelMembers[1][ChNum],
                    g_pShm->BitMapDestChannelMembers[0][ChNum]);
        }
    }
#endif
}

/*---------------------------------end DoseComNodeStatus.cpp  -------*/
