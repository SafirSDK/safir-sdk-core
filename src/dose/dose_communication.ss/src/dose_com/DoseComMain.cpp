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

/**************************************************************************
* DoseComMain.cpp - Top level part of DoseComDll - For LINUX and WIN32
*
* All entries to the DLL are located here
*
* See DoseCom_Interface.h for more description
****************************************************************************/

#include "DosePlatform.h"

//---------------------------------
// Dose include files
//---------------------------------

#include "../defs/DoseCom_Interface.h"
#include "DoseComConfig.h"
#include "DoseComTransmit.h"
#include "DoseComReceive.h"
#include "../defs/DoseUdpMsg.h"
#include "../defs/DoseNodeStatus.h"
#include "DoseOsInterface.h"
#include "PrintError.h"

//---------------------------------
// external in other DoseCom files
//---------------------------------

extern int DOSE_KeepAlive_Init(dcom_uchar8 DoseId);
extern dcom_ulong32 Get_Own_IpAddress(void);

extern volatile int * volatile pDbg;

///############################
// Section Events
//###########################

//-------------------------------------------------------------------
// Define this to enable StandAlone support
// StandAlone mode will be used if Ipmulticast address = "127.0.0.1"
// In this mode DoseComDll is a dummy doing nothing.
//-------------------------------------------------------------------

#define SUPPORT_STANDALONE

//--------------------------------
// Local variables
//--------------------------------

static dcom_ushort16 g_MyDoseId;

static bool bIsAllowedToCallDoseComAPI = FALSE;
static bool g_bIsStandAlone = FALSE;

//-----------------------------------
// C++ classes
//-----------------------------------

CDoseComTransmit DoseTransmit;
CDoseComReceive  DoseReceive;

static DoseComAllocator *g_pDoseBuff = NULL;

static DoseComNotificationHandler *g_pNotificationHandler = NULL;

/*************************************************************
*
**************************************************************/
char *DoseCom_GetBuff(int Size)
{
    //PrintDbg("DoseCom GetBuff()\n");
    return (char *) g_pDoseBuff->Allocate(Size);
}

/*************************************************************
*
**************************************************************/
int DoseCom_FreeBuff(char *pBuf)
{
    //PrintDbg("DoseCom FreeBuff()\n");
    g_pDoseBuff->Deallocate(pBuf);
    return(0);
}

//################################################
// Section routines used by internally by DoseCom
//################################################

/************************************************************************
*
*************************************************************************/
void WakeUp_Reader(int PrioChannel)
{
    //PrintDbg("WakeUp_Reader(%d)\n", PrioChannel);
    if(*pDbg)
        PrintDbg("WakeUp_Reader(%d)\n", PrioChannel);
    g_pNotificationHandler->NotifyIncomingData(PrioChannel);
}
/************************************************************************
*
*************************************************************************/
void WakeUp_QueueNotFull(int PrioChannel)
{
    g_pNotificationHandler->NotifyQueueNotFull(PrioChannel);
}
/************************************************************************
*
************************************)*************************************/

void WakeUp_NodeUp(void)
{
    //PrintDbg("WakeUp_NodeUp()\n");

    g_pNotificationHandler->NotifyNodeStatusChanged();
}
/************************************************************************
*
*************************************************************************/
void WakeUp_NodeDown(void)
{
    g_pNotificationHandler->NotifyNodeStatusChanged();
}
/************************************************************************
*
*************************************************************************/
void WakeUp_NodeChange(void)
{
    //PrintDbg("WakeUp_NodeChange()\n");
    g_pNotificationHandler->NotifyNodeStatusChanged();
    //PrintDbg("WakeUp_NodeChange() Exit\n");
}

/************************************************************************
*
*************************************************************************/
void WakeUp_DistributePool(void)
{
    g_pNotificationHandler->NotifyStartPoolDistribution();
}

/************************************************************************
*
*************************************************************************/
void WakeUp_RequestPoolDistribution(int doseId)
{
    g_pNotificationHandler->NotifyRequestPoolDistribution(doseId);
}

//####################################
// Section DLL entry routines
//####################################

/********************************************************************
*
* Param multicastAddress:
*  "127.0.0.1" ==> run stand alone (DoseComDll is a dummy)
*  NULL ==> Use default IpMulticast address
* else use the defined IpMulticast address
*
* Param nodeId:
* 0-63 ==> used this as DoseId
* else ==> use last six buts in own IpAddress as DoseId
*)
* Returns:
*   ERR_DOSECOM_OK (=0)       if OK
*   ERR_DOSECOM_INVALID_PARAM if invalid DoseId
*********************************************************************/

int DoseCom_Init(DoseComAllocator    *pAllocator,  // the base class
                 const char*         multicastAddress,
                 int                 multicastTtl,
                 const char*         netAddress,
                 int                 DoseId,
                 DoseComNotificationHandler *pNotificationHandler)
{
    int result;

    if(DoseId > 63)
    {
        return(ERR_DOSECOM_INVALID_PARAM);
    }

    g_pNotificationHandler = pNotificationHandler;
    g_pDoseBuff = pAllocator;

    //-------------------------------------------------------------------
    // Get Configuration parameters (e.g Ip address and port
    //
    // It does:
    // 1) Set default values on Port and IpMulticast BaseAddr
    // 2) Tries to open and read config files and overwrite default values
    //--------------------------------------------------------------------

    CConfig::Dose_Config((dcom_uchar8) DoseId, multicastAddress, multicastTtl, netAddress);

    g_bIsStandAlone = FALSE;

/*----test
    if(*pDbg)
        PrintDbg("/// McIP=%s\n",
            inet_ntoa( *(struct in_addr *) &CConfig::m_BaseIpMultiCastAddr_nw));

    if(*pDbg)
        PrintDbg("/// MyIP=%s\n",
                inet_ntoa( *(struct in_addr *) &CConfig::m_MyIpAddr_nw));
-----*/
    //---------------------------------------------------------------
    // Start all threads in a correct order
    // DOSE_KeepAlive_Init() makes an automatic DoseId configuration
    //---------------------------------------------------------------

    CNodeStatus::InitNodeStatus(0);

#ifdef SUPPORT_STANDALONE
    if(g_bIsStandAlone)
    {
        bIsAllowedToCallDoseComAPI = TRUE;
        return(0);
    }
#endif

    g_MyDoseId = CConfig::m_MyDoseId;

    result = DOSE_KeepAlive_Init((dcom_uchar8) g_MyDoseId);

    if(*pDbg)
        PrintDbg("/// DoseId = %d\n", g_MyDoseId);

    DoseTransmit.Xmit_Init(g_MyDoseId);

    DoseReceive.Receive_Init(g_MyDoseId);

    //------------------------------------------------------------------
    // Wait here to check if there are any other nodes around.
    // The principle is to wait for KeepAlive messages from other nodes.
    // These are sent every 1000 millisecond.
    // We wait 2.3 seconds. In this time it should beat least 2 messages.
    // If a node is detected, return is made at once.
    //
    // Returns: 0 if no nodes found
    //          ERR_DOSECOM_OTHER_EXISTS if one or more nodes found
    //------------------------------------------------------------------

    DOSE_SHARED_DATA_S *pShm = CNodeStatus::GetNodeSharedDataPointer();
    dcom_ulong64 BitForMe64 = (dcom_ulong64)1 << (g_MyDoseId & 0x3F);

    for(int jj=0 ; jj < 23; jj++)
    {
        if(
                ((pShm->BitMapNodesUp64  & ~BitForMe64) != (dcom_ulong64) 0)
            ||
                ((pShm->BitMapNodesNew64 & ~BitForMe64) != (dcom_ulong64) 0)
          )
        {
            bIsAllowedToCallDoseComAPI = TRUE;

            return(ERR_DOSECOM_OTHER_EXISTS);
        }
        DoseOs::Sleep(100);
     }

    bIsAllowedToCallDoseComAPI = TRUE;

    return(0);
}

/**********************************************************
* Must be called first ( since _init starts everything )
**********************************************************/

int DoseCom_Add_DestinationId(  int         DestinationId,
                                const char  *IpMulticastAddr,
                                dcom_ulong64 BitMapDestChanMembers)
{
    int result;

    if((DestinationId < 64)
        || (DestinationId > (64+MAX_NUM_DEST_CHANNELS)))
    {
        return(ERR_DOSECOM_INVALID_PARAM);
    }

    result =
        CConfig::Add_DestinationId(DestinationId, IpMulticastAddr,
                                BitMapDestChanMembers);

    return(result);
}

/************************************************************************
* See DoseCom_Interface.h for description.
*************************************************************************/

int DoseCom_Send(const char *pMsg, dcom_ulong32 MsgLength,
                 bool PoolDistribution, bool bUseAck,
                 int  Priority, int Destination)
{
    int result;


    if(!bIsAllowedToCallDoseComAPI) return(ERR_DOSECOM_NOT_INIT);

    if((Destination < 0) || (Destination>(64+MAX_NUM_DEST_CHANNELS)))
    {
        return(ERR_DOSECOM_INVALID_PARAM);
    }
    if((Priority < 0) || (Priority >= MAX_NUM_PRIO_CHANNELS))
    {
        return(ERR_DOSECOM_INVALID_PARAM);
    }
    if(MsgLength > 40000000)
    {
        return(ERR_DOSECOM_INVALID_SIZE);
    }

#ifdef SUPPORT_STANDALONE
    if(g_bIsStandAlone) { DoseCom_FreeBuff((char *) pMsg); return(0); }
#endif

    result = DoseTransmit.Xmit_Msg(pMsg, MsgLength,
                                PoolDistribution ? 1 : 0,
                                bUseAck, Priority, Destination);
    return(result);
}

/************************************************************************
* See DoseCom_Interface.h for description.
*************************************************************************/

int DoseCom_Read(dcom_ulong32 RxUseBitMap,
                 dcom_ulong32 *pRxFromBitMap,
                 char **ppBuf, dcom_ulong32 *pSize, bool *pIsNative)
{
    int result;

    if(!bIsAllowedToCallDoseComAPI)
    {
        PrintDbg("Not bIsAllowedToCallDoseComAPI *****************************\n");
        return(ERR_DOSECOM_NOT_INIT);
    }
    result = DoseReceive.Read_Msg(RxUseBitMap, pRxFromBitMap,
                                    ppBuf, pSize, pIsNative);
    return(result);
}

/************************************************************************
* When DoseCom detects a NodeStatus change "Not present" --> UP <--> DOWN
* an event is sent to application. Application reads this routine until
* no more data
*
* Application contiouse calling until there is no more changed nodes
* Application must check if state is UP/DOWN
*
* Returns:
*   true  if data about a node is returned
*   false if there was no more nodes to report
*************************************************************************/

bool DoseCom_GetNodeChange( dcom_ulong32 & DoseId, dcom_ulong32 & Status,
                            dcom_ulong32 & IpAddr_nw)
{
    dcom_uchar8   NodeStatus;
    dcom_ulong32   TestedDoseId;
    dcom_ulong32   NodeIpAddr_nw;

    if(!bIsAllowedToCallDoseComAPI) return(false);

#ifdef SUPPORT_STANDALONE
    if(g_bIsStandAlone) return(false);
#endif

    TestedDoseId = CNodeStatus::GetNextChangedNode(&NodeStatus,&NodeIpAddr_nw);

    if(TestedDoseId >= NODESTATUS_TABLE_SIZE) return(false);

    DoseId    = TestedDoseId;
    Status    = NodeStatus;
    IpAddr_nw = NodeIpAddr_nw;

    return true;
}

/************************************************************************
*
*************************************************************************/

void DoseCom_PoolDistributed(int Priority, int DestinationId)
{
    if(!bIsAllowedToCallDoseComAPI) return;

    if((DestinationId < 0) || (DestinationId>(64+MAX_NUM_DEST_CHANNELS)))
    {
        return;
    }
    if((Priority < 0) || (Priority >= MAX_NUM_PRIO_CHANNELS))
    {
        return;
    }

#ifdef SUPPORT_STANDALONE
    if(g_bIsStandAlone) return;
#endif

    DoseTransmit.Set_PoolDistributionIsCompleted(Priority, DestinationId);
    return;
}

/************************************************************************
* If this is called too early, DoseIs is not valid.
*************************************************************************/

void DoseCom_GetDoseId(dcom_ulong32 & DoseId)
{
    DoseId = g_MyDoseId;
}

/************************************************************************
* If this is called too early, m_MyIpAddr is not valid.
* We should return an error indication, but currently do not.
*
*************************************************************************/

void DoseCom_GetOwnIpAddr( dcom_ulong32 & IpAddr_nw)
{
    IpAddr_nw = CConfig::m_MyIpAddr_nw;
}

/************************************************************************
* For internal tests
* 'd' - get Debug level  (used by DoseMonitor)
* 'D' - set Debug level  (used by DoseMonitor)
*************************************************************************/

void DoseCom_Test(dcom_ulong32 TestCode, dcom_ulong32 Param)
{
    //PrintDbg("DoseCom_Test() pDbg=%X\n", pDbg);

    if(TestCode == 'd') *(int *) Param = *pDbg; //Param is a ptr
    else
    if(TestCode == 'D') *pDbg = *(int *) Param; //Param is a ptr
    else
        PrintDbg("DoseCom_Test(): Invalid TestCode = %X\n", TestCode);
}
/*-------------- end DoseComMain.cpp ---------------------*/
