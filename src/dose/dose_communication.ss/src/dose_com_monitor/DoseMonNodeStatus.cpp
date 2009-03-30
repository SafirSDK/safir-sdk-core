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

#include <windows.h>
#include <stdio.h>

typedef unsigned _int64 ulong64;
#include "../Defs/DoseNodeStatus.h"
#include "../Defs/DoseUdpMsg.h"
#include "PrintError.h"

extern DOSE_SHARED_DATA_S *G_pShm;

int G_Mode_Local_Remote = 'L'; // 'L', 'R', 0

extern int Debug;

static struct  // DoseIndex is index in this array
{
    DWORD   IpAddr_nw;
    DWORD   KeepAliveTime;
    BYTE    UpDown;
    BYTE    Spare1;
    BYTE    Spare2;
    BYTE    Spare3;
} g_DoseNodeStatus[64] = {{0}};

DWORD g_BaseIpMultiCastAddr_nw = 0;
DWORD g_MyIpAddr_nw = 0;
WORD  g_Dose_KeepAlivePort = 0;

/**************************************************************
* Get NodeStatus from internal table built by KeepAliveRxThread
* or from DoseComDll.
*
* Returns:
* 1  if it exists
* 0  if it not exists
* -1 if invalid mode for this call
***************************************************************/

int Get_NodeStatus(BYTE DoseId, int *pUpDown, DWORD *pIpAddr_nw)
{
    if(G_Mode_Local_Remote == 'R') // Remote mode
    {
        if(g_DoseNodeStatus[DoseId].UpDown == 0) return(0);

        *pUpDown = g_DoseNodeStatus[DoseId].UpDown;
        *pIpAddr_nw = g_DoseNodeStatus[DoseId].IpAddr_nw;
        return(1);
    }
    else
    if(G_Mode_Local_Remote == 'L') // Local mode
    {
        if(G_pShm->NodeStatusTable[DoseId].Status == 0) return(0);

        *pIpAddr_nw = G_pShm->NodeStatusTable[DoseId].IpAddr_nw;
        *pUpDown = G_pShm->NodeStatusTable[DoseId].Status;
        return(1);
    }
    else
    {
        return(-1);
    }
}
/*****************************************************
*
******************************************************/
int Get_BitMapNodesUp(int,// ix,
                      ulong64 *pBitMap)
{
    if(G_Mode_Local_Remote == 'L') // Local mode
    {
        *pBitMap = G_pShm->BitMapNodesUp64;
        return(1);
    }
    return(0);
}
/*****************************************************
*
******************************************************/
int Get_BitMapNodesNew(int,// ix,
                      ulong64 *pBitMap)
{
    if(G_Mode_Local_Remote == 'L') // Local mode
    {
        *pBitMap = G_pShm->BitMapNodesNew64;
        return(1);
    }
    return(0);
}
/*****************************************************
*
******************************************************/
int Get_BitMapNodesDown(int, //ix,
                        ulong64 *pBitMap)
{
    if(G_Mode_Local_Remote == 'L') // Local mode
    {
        *pBitMap = G_pShm->BitMapNodesDown64;
        return(1);
    }
    return(0);
}
/*****************************************************
*
******************************************************/
DWORD Get_MyIpAddr(void)
{
    if(G_Mode_Local_Remote == 'L') // Local mode
    {
        return G_pShm->MyIpAddr_nw;
    }
    return(0);
}

/*******************************************************
*
********************************************************/
DWORD Get_RxCount(BYTE DoseId)
{
    //return(G_pShm->Statistics.TotRxCount);
    if(G_Mode_Local_Remote == 'L') // Local mode
        return G_pShm->NodeStatusTable[DoseId].RxCount;

    return(0);
}

/**************************************************************
* - Connect to DoseComDll   - if Local mode
* - Start KeepAliveRxThread - if Remote mode
*
***************************************************************/

#ifdef TO_BE_IMPLEMENTED
/******************************************************************
* This thread that receives alive messages and sends alive messages
*
*
*******************************************************************/
static unsigned int __stdcall KeepAlive_Thread(void *) //pArgVoid)
{
    int     result;
    int     StartUpSendCount = 0;
    ulong   dwPreviosSendTime = 0;
    ulong   dwCurrentTime;
    ulong   ErrCode;
    ulong   FromIpAddr_nw;
    char    RxBuff[32];  // Must be at least sizeof(DOSE_UDP_GETINFO_MSG)
    DOSE_UDP_KEEPALIVE_MSG *pRxMsg;
    CIpmSocket TxRxSock;
    bool bFoundAConflictingDoseId = FALSE;
    uchar DoseIdBuff[64] = {0};



    pRxMsg = (DOSE_UDP_KEEPALIVE_MSG *) RxBuff;

    if(Debug)
        PrintDbg("=== KeepAlive_Thread() starts. IP=%s Port=%d\n",
                inet_ntoa(*(struct in_addr *) &g_MyIpAddr_nw),
                g_Dose_KeepAlivePort);
    //--------------------------------------------------
    // Initializations
    //--------------------------------------------------

    //---------------------------------------------
    // Create socket for Receive and Send
    //---------------------------------------------

    result = TxRxSock.CreateIpMulticastSocket(
                        1,0,    //Rx, and Tx
                        g_BaseIpMultiCastAddr_nw,
                        g_Dose_KeepAlivePort,
                        0,      // Opt_so_rcvbuf_size,
                        800);   //  Opt_So_Rcvbuf_Timeout,

    if(result == -1)
    {
        PrintErr(0, "ERROR: KeepAlive() Can not create Rx/Tx socket\n");
        return(-1);
    }

    dwPreviosSendTime = GetTickCount() - 1500;

    //=========================================================
    // Loop here for ever - receive and send KeepAlive messages
    //=========================================================

    for(;;)
    {
        //------------------------------------------------
        // Wait for a msg or timeout
        //------------------------------------------------

        // note a 800 ms timeout on recvfrom

        result = TxRxSock.RecvFrom2(RxBuff,sizeof(RxBuff),NULL,0);

        if(result < 0) // Rx error
        {
            ErrCode = GetLastError();

            //PrintDbg("=   KeepAliveThread got a msg. r=%d E=%d\n",
            //          result, GetLastError());

            // On a UPD-datagram socket this error would indicate that a previous
            // send operation resulted in an ICMP "Port Unreachable" message.

            if(ErrCode == ECONNRESET)
            {
                if(Debug>=5)
                PrintDbg("=   KeepAliveThread got a DOSE_ECONNRESET\n");

                Sleep(500);
                goto Check_Timeout;
            }
            if (ErrCode == WSA_ETIMEDOUT)  // fall through
            {
                ; //PrintDbg("=   KeepAlive recvfrom() timed out\n");
                goto Check_Timeout;
            }
            else
            {
                PrintErr(ErrCode,"=   KeepAlive got an unexpected error.\n");

                Sleep(1000); // to prevent wild loop
                goto Check_Timeout;
            }
        }
        //PrintDbg("=   KeepAlive got a msg. 2\n");

        //--------------------------------------------------------
        // Got a KeepAlive msg from a node
        // Update Node_Status[]
        //
        // If a change (ret 'U' or 'D') we must notify subscribers)
        // Tghat is Appl and
        //--------------------------------------------------------
        if( pRxMsg->Magic != DOSE_MSG_MAGIC) goto Check_Timeout; //got junk

        if( pRxMsg->MsgType == MSG_TYPE_KEEPALIVE)
        {
            //if(*pDbg>3) PrintDbg("=   KeepAlive got a msg. 3\n");

            FromIpAddr_nw = pRxMsg->IpAddrFrom_nw;
            //PrintDbg("=   KeepAlive call UpdateNode_Up(%X,%X)\n",
            //          pRxMsg->DoseIdFrom, pRxMsg->IpAddrFrom_nw);

            result = CNodeStatus::UpdateNode_Up(pRxMsg->DoseIdFrom,
                                    pRxMsg->IpAddrFrom_nw);
                                    //pRxMsg->BitMapDestChannelMemberShip);

            if (result != 0) // If a change (a new node)
            {
                if(Debug>=2)
                    PrintDbg("=   KeepAlive Adding a node IP=%s\n",
                        inet_ntoa(*(struct in_addr *) &pRxMsg->IpAddrFrom_nw));
            }
        }

        //---------------------------------------------------------
        // Check if any missing nodes ( nodes that have timed out)
        //---------------------------------------------------------
Check_Timeout:

        //-----------------------------------
        // Time to send a KeepAlive msg
        //-----------------------------------
        dwCurrentTime = GetTickCount();

        if((dwCurrentTime - dwPreviosSendTime) > 1000)
        {
            // Check if any missing nodes (nodes that have timed out)
            //CNodeStatus::CheckTimedOutNodes();
            dwPreviosSendTime = dwCurrentTime;
        }
    } // end for(;;)
    return(0);
}
/*--------------------- end KeepAlive_Thread() --------------*/

#endif
