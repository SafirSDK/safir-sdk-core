/******************************************************************************
*
* Copyright Saab AB, 2005-2013 (http://safir.sourceforge.net)
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
* DoseComKeepAlive.cpp - a part of DoseComDll - For LINUX and WIN32
*
* Function:
* 1) Sends IpMulticast alive messages every time intervall.
* 2) Receives IpMulticast alive messages and stores in internal 'DataBase'.
*    This DataBase is accessable by other functions.
*************************************************************************/
#ifndef _CRT_SECURE_NO_DEPRECATE
#define _CRT_SECURE_NO_DEPRECATE
#endif

#define IS_USING_SOCKETS
#include "DosePlatform.h"

#include "DoseOsInterface.h"
#include "DoseComTransmit.h"
#include "DoseComReceive.h"
#include "DoseComConfig.h"
#include "../defs/DoseUdpMsg.h"
#include "PrintError.h"
#include "../defs/DoseNodeStatus.h"
#include "IpmSocket.h"
#include "DoseComStatistics.h"

extern volatile int * volatile pDbg;

//-----------------------------------------------------------
// Node_Status[] holds the status of all nodes in the system.
// It is updated by reception of KeepAlive messages (UP).
// or the absence of KeepAlive messages (DOWN).
//-----------------------------------------------------------

static  dcom_ushort16  g_MyDoseId = 123;
static  bool    g_bStartUpIsCompleted = FALSE;

static bool     g_bAutoConfigure;

static DOSE_SHARED_DATA_S *g_pShm;

static  Statistics::TX_STATISTICS_S *g_pTxStatistics;
static  Statistics::RX_STATISTICS_S *g_pRxStatistics;


/*******************************************************************
* For Monitoring and Debugging
* Build a response and send it.
*
* Use pRxMsg->ReqCode_1, _2 to define the contents.
*
* This goes to DoseWebMon
********************************************************************/
static void Handle_GetInfoRequest(CIpmSocket *pTxRxSock,
                                  DOSE_UDP_GETINFO_MSG *pRxMsg)
{
    int     jj;
    size_t  pos = 0;
    int     result;
    dcom_ulong32 IpMulticastAddr_nw;
    dcom_ulong32 IpAddr_nw;
    char    buff[3000];

    //PrintDbg("--- Handle_GetInfoRequest().\n");

    //-------------------------
    // [S0] Statistic Counters
    //-------------------------
    if(pRxMsg->ReqCode_1 == 'S')
    {
        sprintf(buff,
            "Counters DoseId %d\n"
#if (MAX_NUM_PRIO_CHANNELS == 6)
            "TxNoAckOk   =%u/%u/%u/%u/%u/%u\n"
            "TxWithAckOk =%u/%u/%u/%u/%u/%u\n"
            "TxRetransm  =%u/%u/%u/%u/%u/%u\n",
#else
            "TxNoAckOk   =%u/%u/%u/%u\n"
            "TxWithAckOk =%u/%u/%u/%u\n"
            "TxRetransm  =%u/%u/%u/%u\n",
#endif
            CConfig::m_MyDoseId,
            g_pTxStatistics[0].CountTxNoAckOk,
            g_pTxStatistics[1].CountTxNoAckOk,
            g_pTxStatistics[2].CountTxNoAckOk,
            g_pTxStatistics[3].CountTxNoAckOk,
#if (MAX_NUM_PRIO_CHANNELS == 6)
            g_pTxStatistics[4].CountTxNoAckOk,
            g_pTxStatistics[5].CountTxNoAckOk,
#endif
            g_pTxStatistics[0].CountTxWithAckOk,
            g_pTxStatistics[1].CountTxWithAckOk,
            g_pTxStatistics[2].CountTxWithAckOk,
            g_pTxStatistics[3].CountTxWithAckOk,
#if (MAX_NUM_PRIO_CHANNELS == 6)
            g_pTxStatistics[4].CountTxWithAckOk,
            g_pTxStatistics[5].CountTxWithAckOk,
#endif
            g_pTxStatistics[0].CountTxRetransmit,
            g_pTxStatistics[1].CountTxRetransmit,
            g_pTxStatistics[2].CountTxRetransmit,
            g_pTxStatistics[3].CountTxRetransmit
#if (MAX_NUM_PRIO_CHANNELS == 6)
            ,
            g_pTxStatistics[4].CountTxRetransmit,
            g_pTxStatistics[5].CountTxRetransmit
#endif
            );
        pos = strlen(buff);
        sprintf(&buff[pos],
#if (MAX_NUM_PRIO_CHANNELS == 6)
            "TxOverflow  =%u/%u/%u/%u/%u/%u\n"
            "TxNoTargets =%u/%u/%u/%u/%u/%u\n"
            "TxGiveup    =%u/%u/%u/%u/%u/%u\n",
#else
            "TxOverflow  =%u/%u/%u/%u\n"
            "TxNoTargets =%u/%u/%u/%u\n"
            "TxGiveup    =%u/%u/%u/%u\n",
#endif
            g_pTxStatistics[0].CountTxOverflow,
            g_pTxStatistics[1].CountTxOverflow,
            g_pTxStatistics[2].CountTxOverflow,
            g_pTxStatistics[3].CountTxOverflow,
#if (MAX_NUM_PRIO_CHANNELS == 6)
            g_pTxStatistics[4].CountTxOverflow,
            g_pTxStatistics[5].CountTxOverflow,
#endif
            g_pTxStatistics[0].CountTxNoTargets,
            g_pTxStatistics[1].CountTxNoTargets,
            g_pTxStatistics[2].CountTxNoTargets,
            g_pTxStatistics[3].CountTxNoTargets,
#if (MAX_NUM_PRIO_CHANNELS == 6)
            g_pTxStatistics[4].CountTxNoTargets,
            g_pTxStatistics[5].CountTxNoTargets,
#endif
            g_pTxStatistics[0].CountTxGiveUp,
            g_pTxStatistics[1].CountTxGiveUp,
            g_pTxStatistics[2].CountTxGiveUp,
            g_pTxStatistics[3].CountTxGiveUp
#if (MAX_NUM_PRIO_CHANNELS == 6)
            ,
            g_pTxStatistics[2].CountTxGiveUp,
            g_pTxStatistics[3].CountTxGiveUp
#endif
            );
        pos = strlen(buff);
        sprintf(&buff[pos],
#if (MAX_NUM_PRIO_CHANNELS == 6)
            "RxOverflow  =%u/%u/%u/%u/%u/%u\n"
            "RxNoAckOk   =%u/%u/%u/%u/%u/%u\n"
            "RxWithAckOk =%u/%u/%u/%u/%u/%u\n"
            "RxDuplicate =%u/%u/%u/%u/%u/%u\n",
#else
            "RxOverflow  =%u/%u/%u/%u\n"
            "RxNoAckOk   =%u/%u/%u/%u\n"
            "RxWithAckOk =%u/%u/%u/%u\n"
            "RxDuplicate =%u/%u/%u/%u\n",
#endif
            g_pRxStatistics[0].CountRxOverflow,
            g_pRxStatistics[1].CountRxOverflow,
            g_pRxStatistics[2].CountRxOverflow,
            g_pRxStatistics[3].CountRxOverflow,
#if (MAX_NUM_PRIO_CHANNELS == 6)
            g_pRxStatistics[4].CountRxOverflow,
            g_pRxStatistics[5].CountRxOverflow,
#endif
            g_pRxStatistics[0].CountRxNoAckOk,
            g_pRxStatistics[1].CountRxNoAckOk,
            g_pRxStatistics[2].CountRxNoAckOk,
            g_pRxStatistics[3].CountRxNoAckOk,
#if (MAX_NUM_PRIO_CHANNELS == 6)
            g_pRxStatistics[4].CountRxNoAckOk,
            g_pRxStatistics[5].CountRxNoAckOk,
#endif
            g_pRxStatistics[0].CountRxWithAckOk,
            g_pRxStatistics[1].CountRxWithAckOk,
            g_pRxStatistics[2].CountRxWithAckOk,
            g_pRxStatistics[3].CountRxWithAckOk,
#if (MAX_NUM_PRIO_CHANNELS == 6)
            g_pRxStatistics[4].CountRxWithAckOk,
            g_pRxStatistics[5].CountRxWithAckOk,
#endif
            g_pRxStatistics[0].CountRxDuplicate,
            g_pRxStatistics[1].CountRxDuplicate,
            g_pRxStatistics[2].CountRxDuplicate,
            g_pRxStatistics[3].CountRxDuplicate
#if (MAX_NUM_PRIO_CHANNELS == 6)
            ,
            g_pRxStatistics[2].CountRxDuplicate,
            g_pRxStatistics[3].CountRxDuplicate
#endif
            );

        pos = strlen(buff);
        sprintf(&buff[pos],
#if (MAX_NUM_PRIO_CHANNELS == 6)
            "RxInvalidMsg=%u/%u/%u/%u/%u/%u\n"
            "RxRxMustWait=%u/%u/%u/%u/%u/%u\n"
            "RxInvSeqNum =%u/%u/%u/%u/%u/%u\n",
#else
            "RxInvalidMsg=%u/%u/%u/%u\n"
            "RxRxMustWait=%u/%u/%u/%u\n"
            "RxInvSeqNum =%u/%u/%u/%u\n",
#endif
            g_pRxStatistics[0].CountRxInvalidMsg,
            g_pRxStatistics[1].CountRxInvalidMsg,
            g_pRxStatistics[2].CountRxInvalidMsg,
            g_pRxStatistics[3].CountRxInvalidMsg,
#if (MAX_NUM_PRIO_CHANNELS == 6)
            g_pRxStatistics[4].CountRxInvalidMsg,
            g_pRxStatistics[5].CountRxInvalidMsg,
#endif
            g_pRxStatistics[0].CountRxMustWait,
            g_pRxStatistics[1].CountRxMustWait,
            g_pRxStatistics[2].CountRxMustWait,
            g_pRxStatistics[3].CountRxMustWait,
#if (MAX_NUM_PRIO_CHANNELS == 6)
            g_pRxStatistics[4].CountRxMustWait,
            g_pRxStatistics[5].CountRxMustWait,
#endif
            g_pRxStatistics[0].CountRxInvSeqNum,
            g_pRxStatistics[1].CountRxInvSeqNum,
            g_pRxStatistics[2].CountRxInvSeqNum,
            g_pRxStatistics[3].CountRxInvSeqNum
#if (MAX_NUM_PRIO_CHANNELS == 6)
            ,
            g_pRxStatistics[2].CountRxInvSeqNum,
            g_pRxStatistics[3].CountRxInvSeqNum
#endif
            );

        pos = strlen(buff);
        sprintf(&buff[pos],
#if (MAX_NUM_PRIO_CHANNELS == 6)
            "RxInvFragNum=%u/%u/%u/%u/%u/%u\n"
            "RxOtherErr  =%u/%u/%u/%u/%u/%u\n",
#else
            "RxInvFragNum=%u/%u/%u/%u\n"
            "RxOtherErr  =%u/%u/%u/%u\n",
#endif
            g_pRxStatistics[0].CountRxInvFragmentNum,
            g_pRxStatistics[1].CountRxInvFragmentNum,
            g_pRxStatistics[2].CountRxInvFragmentNum,
            g_pRxStatistics[3].CountRxInvFragmentNum,
#if (MAX_NUM_PRIO_CHANNELS == 6)
            g_pRxStatistics[4].CountRxInvFragmentNum,
            g_pRxStatistics[5].CountRxInvFragmentNum,
#endif
            g_pRxStatistics[0].CountRxOtherErr,
            g_pRxStatistics[1].CountRxOtherErr,
            g_pRxStatistics[2].CountRxOtherErr,
            g_pRxStatistics[3].CountRxOtherErr
#if (MAX_NUM_PRIO_CHANNELS == 6)
            ,
            g_pRxStatistics[2].CountRxOtherErr,
            g_pRxStatistics[3].CountRxOtherErr
#endif
            );
    }
    //-------------------------------
    // [I1] Destination Address Table
    //-------------------------------
    else
    if((pRxMsg->ReqCode_1 == 'I') && (pRxMsg->ReqCode_2 == '1'))
    {
        char    IsUsedForReception;

        sprintf(buff,"Dest Table for DoseId %d\n", g_MyDoseId);

        for(jj=0 ; jj<MAX_NUM_DEST_CHANNELS ; jj++)
        {

            result = CConfig::GetDestinationItem(jj,
                                &IpMulticastAddr_nw,
                                &IsUsedForReception);
            if(result == -1) break;

            pos = strlen(buff);
            sprintf(&buff[pos], "%2d (%2d) %s %s\n", 64+jj, jj,
                    DoseOs::Inet_Ntoa(IpMulticastAddr_nw),
                    IsUsedForReception ? "(Rx)" : " ");

            //PrintDbg("%2d %08X (R=%d)\n",jj,
            //      IpMulticastAddr_nw,IsUsedForReception);
            //??? merge to buffer and send it
        }
    }
    //---------------------------------
    // [I2] Up/Down Nodes
    //---------------------------------
    else
    if((pRxMsg->ReqCode_1 == 'I') && (pRxMsg->ReqCode_2 == '2'))
    {
        DOSE_SHARED_DATA_S *pS;

        dcom_ulong32 NodeStatus;

        sprintf(buff,"Up/Down Nodes for DoseId %d\n", g_MyDoseId);

        for(jj=0 ; jj<NODESTATUS_TABLE_SIZE ; jj++)
        {
            result = CNodeStatus::GetNodeInfo((dcom_ushort16)jj, &IpAddr_nw, &NodeStatus);
            if(result == -1) break;

            if(NodeStatus == 0) continue; // never up
            if(IpAddr_nw == CConfig::m_MyIpAddr_nw) NodeStatus = '*';

            pos = strlen(buff);

            sprintf(&buff[pos], "%2d %c %s\n", jj,
                    (char)NodeStatus, // 'U' or 'D' or 'N'
                    DoseOs::Inet_Ntoa(IpAddr_nw));
        }
        pos = strlen(buff);

        pS = CNodeStatus::GetNodeSharedDataPointer();

        sprintf(&buff[pos],
            "\nNewMap= %08X %08X\nUpMap=  %08X %08X\nDownMap=%08X %08X\n",
             (dcom_ulong32)(pS->BitMapNodesNew64>>32),
             (dcom_ulong32)(pS->BitMapNodesNew64 & 0xFFFFFFFF),
             (dcom_ulong32)(pS->BitMapNodesUp64>>32),
             (dcom_ulong32)(pS->BitMapNodesUp64 & 0xFFFFFFFF),
             (dcom_ulong32)(pS->BitMapNodesDown64>>32),
             (dcom_ulong32)(pS->BitMapNodesDown64 & 0xFFFFFFFF));
    }
    //---------------------------------
    // [I3] Tx/Rx Queue Info
    //---------------------------------
    else
    if((pRxMsg->ReqCode_1 == 'I') && (pRxMsg->ReqCode_2 == '3'))
    {
        sprintf(buff,"TxQueueInfo DoseId %d\n", CConfig::m_MyDoseId);
        pos = strlen(buff);
        CDoseComTransmit::Get_Info(&buff[pos]);

        strcat(buff,"\nRxQueueInfo\n");
        pos = strlen(buff);
        CDoseComReceive::Get_Info(&buff[pos]);
    }
    //---------------------------------
    // [I4] NodeStatus
    //---------------------------------
    else
    if((pRxMsg->ReqCode_1 == 'I') && (pRxMsg->ReqCode_2 == '4'))
    {
         CNodeStatus::Get_Info(buff);
    }

    //---------------------------------
    // [C?] Configuration
    //---------------------------------
    else
    if(pRxMsg->ReqCode_1 == 'C')
    {
         CConfig::Get_Info(pRxMsg->ReqCode_2, buff);
    }
    //---------------------------------
    // Default
    //---------------------------------
    else
    {
        sprintf(buff,"DoseComDll got Invalid request: %c%c\n",
                pRxMsg->ReqCode_1,pRxMsg->ReqCode_2);
    }
    //-------------------
    // Send the message
    //-------------------

    result = pTxRxSock->SendTo2(pRxMsg->IpAddrFrom_nw,
                                htons(pRxMsg->RespPort_nw),
                                buff, static_cast<unsigned long>(strlen(buff)), NULL, 0);
}
/*------------- end Handle_GetInfoRequest() -----------------*/

/******************************************************************
* This thread that receives alive messages and sends alive messages
*
*
*******************************************************************/

static THREAD_API KeepAlive_Thread(void *)
{
    int     result;
    dcom_ulong32   dwPreviosSendTime = 0;
    dcom_ulong32   dwPreviosCheckTime = 0;  //NYTT 08-09-18
    dcom_ulong32   dwCurrentTime;
    dcom_ulong32   ErrCode;
    DOSE_UDP_KEEPALIVE_MSG TxMsg;
    char    RxBuff[32];  // Must be at least sizeof(DOSE_UDP_GETINFO_MSG)
    DOSE_UDP_KEEPALIVE_MSG *pRxMsg;
    CIpmSocket TxRxSock;


    DoseOs::Sleep(1000); // MUST sleep to be sure to get a new TimeStamp

    pRxMsg = (DOSE_UDP_KEEPALIVE_MSG *) RxBuff;

    if(*pDbg>=1)
        PrintDbg("=== KeepAlive_Thread() starts. IP=%s Port=%d\n",
                DoseOs::Inet_Ntoa(CConfig::m_MyIpAddr_nw),
                    CConfig::m_Dose_KeepAlivePort);
    //--------------------------------------------------
    // Initializations
    //--------------------------------------------------

    TxMsg.Magic         = DOSE_MSG_MAGIC;
    TxMsg.MsgType       = MSG_TYPE_KEEPALIVE;
    TxMsg.IpAddrFrom_nw = CConfig::m_MyIpAddr_nw;
    TxMsg.DoseIdFrom    = (dcom_uchar8) g_MyDoseId;
    TxMsg.TimeStamp     = (dcom_ulong32) time(NULL); // the time the prog is started
    //---------------------------------------------
    // Create socket for Receive and Send
    //---------------------------------------------

    result = TxRxSock.CreateIpMulticastSocket(
                        1,1,    //Rx, and Tx
                        CConfig::m_BaseIpMultiCastAddr_nw,
                        CConfig::m_MulticastTtl,
                        CConfig::m_Dose_KeepAlivePort,
                        0,      // Opt_so_rcvbuf_size,
                        800);   //  Opt_So_Rcvbuf_Timeout,

    if(result == -1)
    {
        PrintErr(0, "ERROR: KeepAlive() Can not create Rx/Tx socket\n");

        return(NULL);
    }

    dwPreviosSendTime = DoseOs::Get_TickCount() - 1500;

    g_bStartUpIsCompleted = TRUE;

    TxMsg.MsgType = MSG_TYPE_KEEPALIVE;

    //=========================================================
    // Loop here for ever - receive and send KeepAlive messages
    //=========================================================

    // Let RxThread start before we tell other nodes we are here

    DoseOs::Sleep(1000);

    dwPreviosSendTime = DoseOs::Get_TickCount();

    for(;;)
    {
        //------------------------------------------------
        // Wait for a msg or timeout
        //------------------------------------------------

        // note a 800 ms timeout on recvfrom

        result = TxRxSock.RecvFrom2(RxBuff,sizeof(RxBuff),NULL,0);

        // When timeout. RecvFrom2() returns:
        // Linux: result=0. Win32: result=-1 with GetLastError() = DOSE_ETIMEDOUT

        if(result == 0) // Linux Receive Timeout
            goto Check_Timeout;

        if(result < 0) // Rx error
        {
            ErrCode = DoseOs::Get_LastError();

            // On a UPD-datagram socket this error would indicate that a previous
            // send operation resulted in an ICMP "Port Unreachable" message.

            if(ErrCode == DOSE_ECONNRESET)
            {
                if(*pDbg>=5)
                PrintDbg("=   KeepAliveThread got a DOSE_ECONNRESET\n");

                DoseOs::Sleep(500);
                goto Check_Timeout;
            }
            if (ErrCode == DOSE_ETIMEDOUT)  // fall through
            {
                ; //PrintDbg("=   KeepAlive recvfrom() timed out\n");
                goto Check_Timeout;
            }
            else
            {
                PrintErr(ErrCode,"=   KeepAlive got an unexpected error.\n");

                DoseOs::Sleep(1000); // to prevent wild loop
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
            //PrintDbg("=   KeepAlive call UpdateNode_Up(%X,%X)\n",
            //          pRxMsg->DoseIdFrom, pRxMsg->IpAddrFrom_nw);

            result = CNodeStatus::UpdateNode_Up(pRxMsg->DoseIdFrom,
                pRxMsg->IpAddrFrom_nw, pRxMsg->TimeStamp);
            //pRxMsg->BitMapDestChannelMemberShip);

            if (result != 0) // If a change (a new node)
            {
                if(*pDbg>=2)
                    PrintDbg("=   KeepAlive Adding a node IP=%s\n",
                    DoseOs::Inet_Ntoa(pRxMsg->IpAddrFrom_nw));
            }
        }
        else
        if( pRxMsg->MsgType == MSG_TYPE_GETINFO_1)
        {
            Handle_GetInfoRequest(&TxRxSock, (DOSE_UDP_GETINFO_MSG *) pRxMsg);
        }

        //---------------------------------------------------------
        // Check if any missing nodes ( nodes that have timed out)
        //---------------------------------------------------------
Check_Timeout:

        //-----------------------------------
        // Time to send a KeepAlive msg
        //-----------------------------------
        dwCurrentTime = DoseOs::Get_TickCount();

        /*----------- OLD Style - Now removed //NYTT 08-09-18,

        if((dwCurrentTime - dwPreviosSendTime) > 1000)
        {
            // Check if any missing nodes (nodes that have timed out)

            CNodeStatus::CheckTimedOutNodes();

            //if(*pDbg>3) PrintDbg("=   KeepAlive Send KeepAlive\n");

            dwPreviosSendTime = dwCurrentTime;

            result = TxRxSock.SendTo2(CConfig::m_BaseIpMultiCastAddr_nw,
                                CConfig::m_Dose_KeepAlivePort,
                                (char *) &TxMsg, sizeof(DOSE_UDP_KEEPALIVE_MSG),
                                NULL, 0);

            //PrintDbg("KeepAlive Has sent 2 alive msg. result=%d\n", result);
        }---------------------- end old style ------*/

        // --- Start NYTT 08-09-18 ---

        if((dwCurrentTime - dwPreviosSendTime) > 900) // I changed this time
        {
            dwPreviosSendTime = dwCurrentTime;

            if (!g_pShm->InhibitOutgoingTraffic) // Used to stop outgoing traffic for test purposes
            {
                TxRxSock.SendTo2(CConfig::m_BaseIpMultiCastAddr_nw,
                    CConfig::m_Dose_KeepAlivePort,
                    (char *) &TxMsg, sizeof(DOSE_UDP_KEEPALIVE_MSG),
                    NULL, 0);
            }
        }

        if((dwCurrentTime - dwPreviosCheckTime) > 1000)
        {
            // Check if any missing nodes (nodes that have timed out)
            // First check if there are any pending messages on the
            // socket RxQueue. We must consume every messages before
            // checking timeout. This is to handle the case when
            // high CPU load so this thread can not run and messages
            // are queued on the socket.
            // (We do not want to report a node as down when there
            // are pending messages from the node on the socket RxQueue).

            if(TxRxSock.AreThereAnyPendingRxMessages())
                continue; // Get all msg before check timeout

            CNodeStatus::CheckTimedOutNodes(false);

            dwPreviosCheckTime = dwCurrentTime;
        }
        // --- End NYTT 08-09-18 ---

    } // end for(;;)
}
/*--------------------- end KeepAlive_Thread() --------------*/

/**********************************************************
*
***********************************************************/
void DOSE_KeepAlive_Init(dcom_uchar8 DoseId)
{
    if(*pDbg >= 1) PrintDbg("/// KeepAlive_Init().\n");

    // Add myself as the first node  ( ??? no effect)

    CNodeStatus::UpdateNode_Up((dcom_uchar8) g_MyDoseId,
                                CConfig::m_MyIpAddr_nw, (dcom_ulong32) time(NULL));

    g_pRxStatistics = Statistics::GetPtrToRxStatistics(0);
    g_pTxStatistics = Statistics::GetPtrToTxStatistics(0);

    g_pShm = CNodeStatus::GetNodeSharedDataPointer();

    //--------------------------------------------
    // A valid DoseId (0-63) means use this DoseId
    // An invalid means autoconfigure.
    //--------------------------------------------

    g_MyDoseId = DoseId;
    g_bAutoConfigure = FALSE;

    //--------------------------------------------
    // Start Keep Alive
    //--------------------------------------------

    // returns 0 if OK
    unsigned long tid;
    DoseOs::CreateThread(tid, &KeepAlive_Thread, NULL);

    CNodeStatus::UpdateNode_Up((dcom_uchar8) g_MyDoseId,
                                CConfig::m_MyIpAddr_nw, (dcom_ulong32) time(NULL));

    if(*pDbg>=1)
    PrintDbg("/   DOSE_KeepAlive_Init() done DoseId=%d\n", g_MyDoseId);
}
/*-------------------------- end DoseComKeepAlive.cpp ------------------*/
