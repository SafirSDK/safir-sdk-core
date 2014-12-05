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

/*************************************************************************
* DoseComReceive.cpp - A part of DoseComDll - For LINUX and WIN32
*
* Purpose:
*   Threads and routines to receive messages from other nodes and send ACK
*   Messages are placxed in a queue where application can get them
*
*
* Entries:
* --------
* void CDoseComReceive::Get_Info()
* int  CDoseComReceive::Receive_Init()
* int  CDoseComReceive::Read_Msg()
*
* Design
* ------   +-------------+
*          !             !-------------------> SetEvent to Appl
* <--Ack---!             !    +---------+
*          ! RxThread    !    !g_RxQ    !
* --RxMsg->!             !--->!         !----> Appl reads msg
*          !             !    !         !
*          +-------------+    +---------+
*
*          Figure Repeated for each PriorityChannel
*
* -------------------------------------------------------------------
* TODO:
* 1) Suppose a transmission of a fragmented msg is aborted.
*    If so we must free temporary Rxbuffer
*
*************************************************************************/
#ifndef _CRT_SECURE_NO_DEPRECATE
#define _CRT_SECURE_NO_DEPRECATE
#endif

// Since this shall be portable code and I dont want to include <windows.h>,
// I don't want to bring in an osin files that brings in everything possible,
// including window.h and winsock2.h.
// The method is to prevent inclusion by defining __OSIN_CONFIG_H#include

#define IS_USING_SOCKETS
#include "DosePlatform.h"

#define USE_STATISTICS

#include "DoseOsInterface.h"
#include "DoseComConfig.h"
#include "DoseComReceive.h"
#include "DoseComStatistics.h"
#include "../defs/DoseUdpMsg.h"
#include "../defs/DoseNodeStatus.h"
#include "PrintError.h"
#include "IpmSocket.h"

#include <string.h> // LINUX memcpy()

#include "../defs/DoseCom_Interface.h"

extern void WakeUp_RequestPoolDistribution(int doseId);

//---------------------
// This bit is to check the sizes of our data types,
// bad sizes will cause compilation errors
//---------------------
template <bool> struct STATIC_ASSERTION_FAILURE;
template <> struct STATIC_ASSERTION_FAILURE<true> { };

#define STATIC_ASSERT( B ) (STATIC_ASSERTION_FAILURE<(B) != 0> ())

namespace
{
    void dummy() {
        STATIC_ASSERT(sizeof(dcom_ushort16) == 2);
        STATIC_ASSERT(sizeof(dcom_uchar8) == 1);
        STATIC_ASSERT(sizeof(dcom_ulong32) == 4);

        STATIC_ASSERT(sizeof(DOSE_UDP_MSG_HDR) == 32);
        STATIC_ASSERT(sizeof(DOSE_UDP_ACK_MSG) == 16);
        STATIC_ASSERT(sizeof(DOSE_UDP_KEEPALIVE_MSG) == 12);
        STATIC_ASSERT(sizeof(DOSE_UDP_GETINFO_MSG) == 12);
    }
}
//---------------------


//----------------------
// externals
//----------------------

extern void     Show_Node_Status(int Ix, char *pIpAddrText, char *pText);
extern void     WakeUp_Reader(int PrioChannel);
extern char     *DoseCom_GetBuff(int Size);
extern int      DoseCom_FreeBuff(char *pBuf);

extern volatile int * volatile pDbg;

//------------------------------------------
// Swap bytes for little/big endian styles
//------------------------------------------

#define SWAP16(xxx) (((xxx>>8) & 0xFF) | (xxx<<8))

#define SWAP32(xxx)  ( ((xxx>>24) & 0xFF) | ((xxx>>8) & 0xFF00)\
                     | (xxx<<24) | ((xxx<<8) & 0xFF0000))

//---------------------------------------------------------------------
// When a message has been received it is placed here by the calling
// Receeiver_Thread. Rx_Put_ix is then updated.
// Application reader is waked up by an event.
//
// The Application calls ?????? which checks if Rx_Get_ix != Rx_Put_ix.
// If so, the msg on the Queue is fetched. Rx_Get_ix is then updated
// by ?????.
//---------------------------------------------------------------------

#define MAX_RX_QUEUE 32 // was 10 in rel 2.19

static struct
{
    volatile dcom_ulong32   Put_Ix;
    volatile dcom_ulong32   Get_Ix;
    volatile dcom_ushort16  MaxUsedQueueLength;
    dcom_ushort16  spare;

    struct
    {
        volatile dcom_ulong32   UsrMsgLength;
        volatile dcom_uchar8   DoseId;         // From this DoseId - ??? is this used
        volatile dcom_uchar8   bIsNative;
        dcom_uchar8   spare2;
        dcom_uchar8   spare3;
        char    * volatile pReadDataBuf;
    } RxQueue[MAX_RX_QUEUE];

    // We need to keep his information for each sender node and each PrioChan
    struct  //DoseId is index in this
    {
        // 0 is for UniCast, 'N+2' is for DestChannel 'N'
        volatile dcom_ushort16  SeqNumber[MAX_NUM_DEST_CHANNELS+2];
        char    * volatile pBuf;
        volatile dcom_ulong32   BufPos;
        volatile dcom_uchar8   bIsFragmented;
        dcom_uchar8   spare;
        volatile dcom_ushort16  FragmentNumber;
        volatile dcom_uchar8   SeqNumIsValid[MAX_NUM_DEST_CHANNELS+2]; // cleared when node comes up (restarts)
    } RxNodeStatus[NODESTATUS_TABLE_SIZE];  // one for each node
} g_RxQ[MAX_NUM_PRIO_CHANNELS] = {{0}};

//-----------------------------------------------------
//
//-----------------------------------------------------

static dcom_ulong32 g_MyDoseIdBitMap = 0;
static dcom_ulong32 g_MyDoseIdBitMap_Index = 0; // 0 or 1

static  DOSE_SHARED_DATA_S *g_pShm = NULL;

static dcom_ushort16   g_MyDoseId = 0xFFFF;

//########################################
// Section Local routines
//########################################

/********************************************************************
* This is called when a new node is up
* .SeqNumIsValid must be set to false in case the node is restarted.
*********************************************************************/
void CDoseComReceive::UpdateNodeUp(dcom_uchar8 DoseId)
{
    int PrioChan,ii;

    for(PrioChan=0 ; PrioChan<MAX_NUM_PRIO_CHANNELS ; PrioChan++)
    {
        for(ii=0 ; ii<(MAX_NUM_DEST_CHANNELS+2) ; ii++)
            g_RxQ[PrioChan].RxNodeStatus[DoseId].SeqNumIsValid[ii] = 0;
    }
}
/************************************************************************
*
*************************************************************************/

static int Send_AckMsg( CIpmSocket *pTxSock, DOSE_UDP_ACK_MSG *pTxAckMsg,
                       int MyIx, dcom_ulong32 IpAddrFrom_nw,
                       dcom_ushort16 SequenceNumber, dcom_uchar8 TxMsgArray_Ix,
                        dcom_ushort16 FragmentNumber, dcom_ushort16 Info)
{
    int result = 0;

    pTxAckMsg->FragmentNumber = FragmentNumber;
    pTxAckMsg->SequenceNumber = SequenceNumber;
    pTxAckMsg->TxMsgArray_Ix  = TxMsgArray_Ix;
    pTxAckMsg->Info           = Info;

    if(*pDbg>=5)
    {
        if(pTxAckMsg->MsgType == MSG_TYPE_ACK)
            PrintDbg("*-  RxThread[%d] Send ACK: DoseId=%d %s"
                     " Seq=%u GetIx=%u FragmNum=%X\n",
                        MyIx, pTxAckMsg->DoseIdFrom,
                        DoseOs::Inet_Ntoa(IpAddrFrom_nw),
                        SequenceNumber, TxMsgArray_Ix, pTxAckMsg->FragmentNumber);
        else //Nack
            PrintDbg("*-  RxThread[%d] Send NACK: DoseId=%d %s"
                     " Seq=%u GetIx=%u FragmNum=%X ExpFragm=%X\n",
                        MyIx, pTxAckMsg->DoseIdFrom,
                        DoseOs::Inet_Ntoa(IpAddrFrom_nw),
                        SequenceNumber, TxMsgArray_Ix,
                        pTxAckMsg->FragmentNumber, pTxAckMsg->Info);
    }

    result = pTxSock->SendTo2(IpAddrFrom_nw, CConfig::m_Dose_Port_Ack,
                            (char *) pTxAckMsg,sizeof(DOSE_UDP_ACK_MSG),NULL,0);
    if(result<0)
    {
        //if(*pDbg)
            PrintErr(DoseOs::Get_LastError(),
                    "ERROR: RxThread: SendTo2(ACK) failed\n");
    }
    return(result);
}
/*---------------- end Send_AckMsg() ----------------*/

/********************************************************************
* This thread receives messages and sends ACK messages.
* When a valid msg is received it is put on input queue,
* and an event is sent to application.
* There is one thread for each PriorityChannel.
* It is assumed that 'acked' and 'not acked' messages NOT are using
* the same priority channel. (not are received by the same RxThread().
*********************************************************************/

#define RXDATA_SIZE 1600
static THREAD_API RxThread(void *pChNum)
{
    int                 jj;
    int                 result;
    dcom_ulong32               Next_PutIx;
    CIpmSocket          RxSock;
    CIpmSocket          TxSock;
    DOSE_UDP_MSG_HDR    MsgHdr;
    DOSE_UDP_ACK_MSG    TxAckMsg;
    char                *pReadBuf = NULL;
    int                 MyIx;
    int                 NumUsedEntries;
    dcom_ulong32               Put_Ix;
    dcom_ulong32               IpmAddr_nw;
    Statistics::RX_STATISTICS_S *pRxStatistics;
    char                bMsgIsCompleted;
    dcom_uchar8               DoseId;
    dcom_ushort16              LatestSequenceNumber = 0;
    char                RxData[RXDATA_SIZE];
    char                IsUsedForReception;

#define zzzTIMER_TEST

#ifdef TIMER_TEST
    DWORD               StartTime;
#endif
    int * arg = (int*)pChNum;
    MyIx = *arg;  // Defines my thread number (PriorityChannel)
    delete arg;

    pRxStatistics = Statistics::GetPtrToRxStatistics(MyIx);

    if(*pDbg>=1) PrintDbg("*** RxThread(%d) starts.\n", MyIx);

    //================
    // Initializations
    //================

    IsUsedForReception = 0;

    // Check if this priority channel is configured for reception
    for(jj=0 ; jj<MAX_NUM_DEST_CHANNELS ; jj++)
    {
        IsUsedForReception = 0;

        result = CConfig::GetDestinationItem(jj, &IpmAddr_nw,
                                            &IsUsedForReception);
        if(result == -1) break;
        if(IpmAddr_nw == 0) break;
        if(IsUsedForReception) break;
    }

    if(!IsUsedForReception)
    {
        if(*pDbg>=2)
            PrintDbg("*** RxThread(%d) Not used - terminating.\n", MyIx);
        // ?? or terminate thread
        return(0);
    }

    //--------------------------
    // Create socket for Receive
    //--------------------------

    result = RxSock.CreateIpMulticastSocket(
                        1,0,    // Rx, not Tx
                        1, // 1 ==> disable loopback, but do not enable Rx
                        0, //no Tx, so no ttl
                        (dcom_ushort16) (CConfig::m_Dose_Port_Data + MyIx),
                        65536,  // Opt_so_rcvbuf_size,  new_050426
                        0);     // Opt_So_Rcvbuf_Timeout,

    if(result == -1)
    {
        PrintErr(DoseOs::Get_LastError(),
                "*** RxThread() Can not create receive data socket\n");
        DoseOs::Exit(2);
    }

    //--------------------------------------------------------------
    // Enable for reception of these IP Multicast adresses
    // These have been entered to CConfig by application at start up.
    //--------------------------------------------------------------

    for(jj=0 ; jj<MAX_NUM_DEST_CHANNELS ; jj++)
    {
        char    IsUsedForReception;

        result = CConfig::GetDestinationItem(jj, &IpmAddr_nw,
                                            &IsUsedForReception);
        if(result == -1) break;
        if(IpmAddr_nw == 0) break;

        if(IsUsedForReception)
        {
            if(*pDbg>=2)
                PrintDbg("*** RxThread(%d) Enable IPM %s.\n",
                        MyIx, DoseOs::Inet_Ntoa(IpmAddr_nw));
            RxSock.EnableForRxIpMulticast(IpmAddr_nw);
        }
    }

    //--------------------------------------------------
    // Create socket for sending ACK and prepare ACK msg
    //--------------------------------------------------

    result = TxSock.CreateIpMulticastSocket(
                                0,1,    // not Rx, but Tx
                                0,      // CConfig::m_IpMultiCastAddr_nw,
                                0,      // unicast socket, no ttl needed
                                0,      // CConfig::m_Dose_Port_Data_nw,
                                0,      // Opt_so_rcvbuf_size,
                                0);     // Opt_So_Rcvbuf_Timeout,

    if(result == -1)
    {
        PrintErr(DoseOs::Get_LastError(),
                "*** RxThread() Can not create send ACK socket\n");
        DoseOs::Exit(2);
    }

    TxAckMsg.Magic          = DOSE_MSG_MAGIC;
    TxAckMsg.MsgType        = MSG_TYPE_ACK;
    TxAckMsg.IpAddrFrom_nw  = CConfig::m_MyIpAddr_nw; // Senders IpAddr
    TxAckMsg.DoseIdFrom     = (unsigned char) g_MyDoseId;
    TxAckMsg.FragmentNumber = 0;
    TxAckMsg.TxQueueNumber  = (dcom_uchar8) MyIx;

    //======================
    // Loop here for ever
    //======================

    for(;;)
    {
        if(*pDbg>4)
            PrintDbg("*   RxThread[%d] LOOP.\n",MyIx);

        // new_050426
        //------------------------------------------------------------------
        // If there are several senders, messages might com so quick that
        // Application (DoseMain), can't handle the messages this thread puts
        // in g_Rx_Queue[MAX_RX_QUEUE] fast enough.
        // The code below, detects when the queue is near getting full and
        // slows down communication with Sleep().
        // The result is that it takes longer times for the senders to get
        // an ACK allowning them to send next message.
        // Incomming messages will be stored in the sockets internal queue.
        // It is important that this is long enough.
        // See RxSock.CreateIpMulticastSocket() param Opt_so_rcvbuf_size.
        //
        // ?? we must test if the time 5 ms is a good value ???
        // Also note that senders that get a timeout waiting for ACK
        // (40 ms) sends a new msg (we want to avoid that).
        //-------------------------------------------------------------------

        NumUsedEntries = g_RxQ[MyIx].Put_Ix - g_RxQ[MyIx].Get_Ix;

        if(NumUsedEntries < 0) NumUsedEntries += MAX_RX_QUEUE; //wrap

        if ((MAX_RX_QUEUE - NumUsedEntries) < 5)
            DoseOs::Sleep(5);  //??? is this a good value ???

        //------------------------------------------------
        // Wait for next msg
        //------------------------------------------------

        result = RxSock.RecvFrom2((char *) &MsgHdr, SIZEOF_UDP_MSG_HDR,
                                (char *) RxData, RXDATA_SIZE);

        //if(*pDbg>=4)
        //  PrintDbg("*** RxThread[%d] Ret RecvFrom. res=%d\n",MyIx, result);

        if(result < 0) // Rx error
        {
            PrintErr(DoseOs::Get_LastError(),
                    "*   RxThread[%d] got an unexpected error.\n", MyIx);

            DoseOs::Sleep(40); // to prevent wild loop

#ifdef DONT_KNOW_HOW_TO_PORT_WSAECONNRESET
            ErrCode = DoseOs::Get_LastError();
            // On a UPD-datagram socket this error would indicate that a previous
            // send operation resulted in an ICMP "Port Unreachable" message.
            if(ErrCode == WSAECONNRESET)
            {
                if(*pDbg>2)
                    PrintDbg("*   RxThread[%d] got a WSAECONNRESET\n",MyIx);

                DoseOs::Sleep(30); // to prevent wild loop

                continue;
            }
            else
            {
                PrintErr(ErrCode,"RxThread got an unexpected error.\n");
                DoseOs::Sleep(200); // to prevent wild loop
            }
#endif
            //Statistics_RxError++ ???
            continue;
        }

        //---------------------------------------
        // Got a message.
        // Check if it is valid
        //---------------------------------------
        DoseId = MsgHdr.DoseIdFrom;

        if(*pDbg>=3)
            PrintDbg("*** RxThread[%d] msg fr nod %d. SeqN=%u"
                        " DstBitMap=%X.%08X DstId=%d ########\n",
                        MyIx, DoseId, MsgHdr.SequenceNumber,
                        MsgHdr.DoseIdBitMap[1], MsgHdr.DoseIdBitMap[0],
                        MsgHdr.DestinationId);

        // Is the header valid ?
        if((MsgHdr.Magic != DOSE_MSG_MAGIC)
                             || (MsgHdr.MsgType != MSG_TYPE_DATA))
        {
            PrintErr(0,
                "*   RxThread[%d] Got an invalid Msg. Magic/Type=%X/%X\n",
                MyIx, MsgHdr.Magic, MsgHdr.MsgType);

            pRxStatistics->CountRxInvalidMsg++;
            continue;
        }

        // Is DoseIdFrom valid ?
        if(DoseId >= NODESTATUS_TABLE_SIZE)
        {
            PrintErr(0,"*   RxThread[%d] Got a msg with Invalid DoseId=%d\n",
                    MyIx, DoseId);
            pRxStatistics->CountRxInvalidMsg++;
            continue;
        }

        // Is it for me ?

        if((MsgHdr.DoseIdBitMap[g_MyDoseIdBitMap_Index]
                                                & g_MyDoseIdBitMap) == 0)
        {
            // Not for me
            // But it might be a PD to another node.
            // If so SequenceNumber on this channel from this node is changed
            // We must handle that.
            //
            // Should be improved to use the rx num  to update the expected.
            int     SeqNumSet;

            if(MsgHdr.DestinationId<64) SeqNumSet = 0;
            else                        SeqNumSet = MsgHdr.DestinationId - 64+2;
            if(*pDbg>=2)
            PrintDbg("*   RxThread[%d] Rx msg Not for me. BitMap= %X.%08X\n",
                    MyIx, MsgHdr.DoseIdBitMap[1], MsgHdr.DoseIdBitMap[0]);
            pRxStatistics->CountRxInvalidMsg++;
            continue;
        }

        // Is it from myself ? - During development I accept my own.
        // The normal case is that this code is used.
//#ifdef NO_LOOP_BACK
        if(MsgHdr.IpAddrFrom_nw == CConfig::m_MyIpAddr_nw)
        {
            if(*pDbg>=3)
                PrintDbg("*   RxThread[%d] Rx from myself - ignore\n",MyIx);
            continue; // from myself, ignore
        }
//#endif
        //============================================================
        // SequenceNumber.
        //
        // g_RxNodeStatus[].SequenceNumber contains the latest
        // received SequenceNumber for each node.
        //
        // Note that for fragmented messages, every fragment have
        // the same SequenceNumber.
        //
        // At startup, .SeqNumIsValid is false. This is to ignore the
        // SequenceNumber test the first time.
        //
        //==============================================================

        int     SeqNumSet;

        if(MsgHdr.DestinationId<64) SeqNumSet = 0;
        else                        SeqNumSet = MsgHdr.DestinationId - 64+2;

        LatestSequenceNumber
                = g_RxQ[MyIx].RxNodeStatus[DoseId].SeqNumber[SeqNumSet];

#define zzzFORCE_ERROR_LOST_FRAGMENT
// Testresult:
// Sender has sent 3 + 4 + 5(last) and gets timeout and retransmits 3+4+5 (OK)
// Receiver gets the correct msg. (OK)
// ERROR: receiver sends Nack for 4+5, but sender does not handle these.
#ifdef FORCE_ERROR_LOST_FRAGMENT
        {
            int flag_Lf = 0;
            if(MsgHdr.fragmentnumber == 1) flag_Lf = 1; //set for first fragment

            if(flag_Lf && ((MsgHdr.FragmentNumber & 0x7FFF) == 3))
            {
                flag_Lf = 0;  // clear after one lost fragment
                PrintDbg("FORCE Lost fragment 3\n");
                continue;
            }
        }
#endif

#define zzzFORCE_ERROR_DELAY_ACK
// Test result: OK
// Sender get timeout waiting for ACK and retransmits.
// Receiver ignores these but sends ACK.
// Sender ignores the ACKs
#ifdef FORCE_ERROR_DELAY_ACK
        {
            int flag_DelayAck;
            if(MsgHdr.FragmentNumber == 1) flag_DelayAck = 1; //set for first fragment

            if(flag_DelayAck && ((MsgHdr.FragmentNumber & 0x7FFF) == 5))
            {
                flag_DelayAck = 0;  // clear after one lost fragment
                PrintDbg("FORCE Delayed Ack Fragment = 5\n");
                Sleep(200);
            }
        }
#endif

#define zzzzFORCE_ERROR_DELAY_ACK_NONFRAGM
#ifdef FORCE_ERROR_DELAY_ACK_NONFRAGM
        {
            static int SimErrCount = 0;

            if((SimErrCount++ & 0x1F) == 0x1F)
            {
                PrintDbg("FORCE Delayed Ack\n");
                Sleep(200);
            }
        }
#endif

        //===========================================
        // A fragmented msg is received.
        // For a fragmented msg, FragmentNumber != 0
        //===========================================

#ifdef USE_RESTART_SEQNUM
        ... to show it is not used ...
        // Sender has restarted the SequenceNumber serie
        if(
            (MsgHdr.SequenceNumber == 0)
            &&
            (MsgHdr.Info & 0x80)
            &&
            //((MsgHdr.FragmentNumber & 0xFFFE) == 0 ) // non-fragm or first fragm
            (MsgHdr.FragmentNumber < 2) // non-fragm or first fragm
          )
        {
            if(*pDbg)
                if(LatestSequenceNumber != 65535)
                    PrintDbg("Clear SequenceNumber. LatestSequenceNumber=%d\n",
                                LatestSequenceNumber);

            LatestSequenceNumber // set so next = 0
                    = g_RxQ[MyIx].RxNodeStatus[DoseId].SeqNumber[SeqNumSet] = 65535;

            //if(*pDbg)
            //  PrintDbg("Clear SeqNumIsValid. LatestSequenceNumber=%d\n", LatestSequenceNumber);
        }
#endif
#ifndef USE_RESTART_SEQNUM
        if(
            (MsgHdr.SequenceNumber == 0)
            &&
            (MsgHdr.IsPoolDistribution & PD_FIRSTDATA)
            &&
            (MsgHdr.FragmentNumber < 2) // non-fragm or first fragm
          )
        {
            if(*pDbg)
                if(LatestSequenceNumber != 65535)
                    PrintDbg("Clear SequenceNumber. LatestSequenceNumber=%d\n",
                                LatestSequenceNumber);

            LatestSequenceNumber // set so next = 0
                    = g_RxQ[MyIx].RxNodeStatus[DoseId].SeqNumber[SeqNumSet] = 65535;

            //if(*pDbg)
            //  PrintDbg("Clear SeqNumIsValid. LatestSequenceNumber=%d\n", LatestSequenceNumber);
        }

#endif
        //---------------------------------------------

        // update the "last seen" timestamp so we know that the node is alive
        if(DoseId < NODESTATUS_TABLE_SIZE)
            g_pShm->NodeStatusTable[DoseId].LatestTime = DoseOs::Get_TickCount();

        if(MsgHdr.FragmentNumber) // If fragmented
        {
            if(*pDbg>3)
            PrintDbg("*** RxThread[%d] Rx Fragmented. SeqNum=%d FragmNum=%X\n",
                    MyIx,MsgHdr.SequenceNumber, MsgHdr.FragmentNumber);

            dcom_ushort16 MsgHdr_FragmentNumber;

            // get rid of flag 'Last segment'
            MsgHdr_FragmentNumber = (dcom_ushort16) (MsgHdr.FragmentNumber & 0x7FFF);

            //-----------------------------------------------------
            // First fragment - Check SequenceNumber
            // SequenceNumber should be 1+LatestSequenceNumber
            // For 'not acked' messages, this test is skipped.
            //-----------------------------------------------------
            if(MsgHdr_FragmentNumber == 1) // First fragment
            {
#ifdef TIMER_TEST
                StartTime = GetTickCount();
#endif
                if (
                    (MsgHdr.SequenceNumber == (dcom_ushort16)(1+LatestSequenceNumber)) //OK
                    ||
                    (MsgHdr.bWantAck == 0)
                   )
                {
                    // OK - Save current SequenceNumber
                    g_RxQ[MyIx].RxNodeStatus[DoseId].SeqNumber[SeqNumSet]
                                        = MsgHdr.SequenceNumber;
                    g_RxQ[MyIx].RxNodeStatus[DoseId].SeqNumIsValid[SeqNumSet] = 1;
                }
                else
                // SequenceNumber ERROR
                {
                    // if  same as previuos = duplicate
                    if(MsgHdr.SequenceNumber == LatestSequenceNumber)
                    {
                        // SeqNumIsValidThis is set to 0 at startup
                        // It is used to prevent a false 'out of synch' at startup
                        // This condition is OK
                        if(!g_RxQ[MyIx].RxNodeStatus[DoseId].SeqNumIsValid[SeqNumSet])
                        {
                            g_RxQ[MyIx].RxNodeStatus[DoseId].SeqNumIsValid[SeqNumSet]
                                            = 1;
                            // Save current SequenceNumber
                            g_RxQ[MyIx].RxNodeStatus[DoseId].SeqNumber[SeqNumSet]
                                            = MsgHdr.SequenceNumber;
                        }
                        else
                        {
                            // This is assumed to be a duplicate.
                            // Send Ack, but do not process the message
                            if(*pDbg>=2)
                            PrintDbg("*   RxThread[%d] Rx duplicate from node %d.\n",
                                    MyIx, DoseId);

                            if(MsgHdr.bWantAck)
                            {
                                Send_AckMsg(&TxSock, &TxAckMsg,
                                   MyIx, MsgHdr.IpAddrFrom_nw,
                                   MsgHdr.SequenceNumber, MsgHdr.TxMsgArray_Ix,
                                   MsgHdr.FragmentNumber, (dcom_ushort16)(0x100 | MsgHdr.Info));
                            }
                            pRxStatistics->CountRxDuplicate++;
                            continue;
                        }
                    } // end 'Same as previuos'
                    else
                    //---------------------------------------------------------
                    // Not same SequenceNumber as previuos and not the expected
                    //---------------------------------------------------------
                    {
                        // If the sending node is new (just started)
                        // (or this node just started), the condition is valid

                        if(g_RxQ[MyIx].RxNodeStatus[DoseId].SeqNumIsValid[SeqNumSet])
                        {
                            pRxStatistics->CountRxInvSeqNum++;
                            if(*pDbg>=2)
                                PrintDbg("*   RxThread[%d]"
                                    " Rx incorrect SeqNumber from node %d. Ignore "
                                    "Got/Exp = %X/%X\n",
                                    MyIx, DoseId, MsgHdr.SequenceNumber,
                                    1 + LatestSequenceNumber);

                            continue;
                        }

                        // This is startup condition. Synchronize SequenceNumber
                        g_RxQ[MyIx].RxNodeStatus[DoseId].SeqNumIsValid[SeqNumSet]=1;

                        // Save current SequenceNumber
                        g_RxQ[MyIx].RxNodeStatus[DoseId].SeqNumber[SeqNumSet]
                                            = MsgHdr.SequenceNumber;
                    } // end Not same SequenceNumber as previuos
                } // end SequenceNumber ERROR
                // If we come here, SequenceNumber was correct, or we have synchronized.
            } // end first fragment
            else
            //------------------------------------
            // A fragment, but not first fragment
            //------------------------------------
            if(MsgHdr_FragmentNumber > 1) // allways true
            {
                //---------------------------------------------------------
                // Check SequenceNumber
                // This is NOT OK since SequenceNumber is the same for all
                // fragments. Also note that we can not start receiving in
                // the middle of a fragmented message.
                //---------------------------------------------------------
                if(MsgHdr.SequenceNumber != LatestSequenceNumber) //not OK SeqNum
                {
                    // First fragments (one ore more) have been lost.
                    // By doing nothing here, the sender will get a timeout waiting
                    // for ack and retransmit the fragment with the result that that
                    // a recovery is made.
                    if (*pDbg || (MsgHdr_FragmentNumber > 3)) //3 == MAX_AHEAD_F
                        PrintDbg("*   Incorrect SequenceNumber Got/Exp=%d/%d FragNo=%d\n",
                                 MsgHdr.SequenceNumber, LatestSequenceNumber, MsgHdr_FragmentNumber);
                    // Improvement: add recovery code here ???
                    continue;
                } // end not OK SequenceNumber

                //---------------------------------------------
                // If we come here, SequenceNumber was correct
                // Check FragmentNumber
                //---------------------------------------------
                dcom_ushort16 CurrFragmentNumber;

                CurrFragmentNumber =
                        g_RxQ[MyIx].RxNodeStatus[DoseId].FragmentNumber;

                // This is the correct case
                if(MsgHdr_FragmentNumber == (1 + CurrFragmentNumber))
                {
                    // Set next expected
                    g_RxQ[MyIx].RxNodeStatus[DoseId].FragmentNumber
                                        = (dcom_ushort16)(CurrFragmentNumber + 1);
                }
                else
                // ERROR - incorrect FragmentNumber - No Ack
                // The entire message shall be thrown away.
                // If a buffer is allocated it must be set free.
                if(!MsgHdr.bWantAck)
                {
                    if(g_RxQ[MyIx].RxNodeStatus[DoseId].pBuf)
                    {
                        DoseCom_FreeBuff(g_RxQ[MyIx].RxNodeStatus[DoseId].pBuf);
                        g_RxQ[MyIx].RxNodeStatus[DoseId].pBuf = NULL;
                    }
                    continue;
                }
                else // ERROR - incorrect FragmentNumber - Want Ack
                {
                    // this is a retransmitted fragment we do not want
                    if(
                           (MsgHdr_FragmentNumber == CurrFragmentNumber)
                        || ((CurrFragmentNumber>4)
                                && (MsgHdr_FragmentNumber == (CurrFragmentNumber-4)))
                        || ((CurrFragmentNumber>3)
                                && (MsgHdr_FragmentNumber == (CurrFragmentNumber-3)))
                        || ((CurrFragmentNumber>2)
                                && (MsgHdr_FragmentNumber == (CurrFragmentNumber-2)))
                        || ((CurrFragmentNumber>1)
                                && (MsgHdr_FragmentNumber == (CurrFragmentNumber-1)))
                      )
                    {
                        // Got a duplicate fragment
                        if(MsgHdr.bWantAck)
                        {
                            Send_AckMsg(&TxSock, &TxAckMsg,
                               MyIx, MsgHdr.IpAddrFrom_nw,
                               MsgHdr.SequenceNumber, MsgHdr.TxMsgArray_Ix,
                               MsgHdr.FragmentNumber, (dcom_ushort16)(0x200 | MsgHdr.Info));
                        }
                        pRxStatistics->CountRxDuplicate++;

                        if(*pDbg)
                            PrintDbg("*   Incorrect FragmNum (duplicate). Got/Exp %X/%X\n",
                                    MsgHdr_FragmentNumber, 1+CurrFragmentNumber);
                        continue;
                    }
                    //else
                    // This is the case where we lost 1,2 or 3 fragments
                    // and got next after these.
                    // We should send a NACK. This causes the sender to retransmit
                    // starting with fragment
                    if(
                           (MsgHdr_FragmentNumber > (CurrFragmentNumber + 1))
                        && (MsgHdr_FragmentNumber < (CurrFragmentNumber + 5))
                      )
                    {
                        if(MsgHdr.bWantAck)
                        {
                            TxAckMsg.MsgType = MSG_TYPE_NACK;

                            Send_AckMsg(&TxSock, &TxAckMsg,
                                    MyIx, MsgHdr.IpAddrFrom_nw,
                                    MsgHdr.SequenceNumber, MsgHdr.TxMsgArray_Ix,
                                    MsgHdr_FragmentNumber,   // the number in the msg
                                    (dcom_ushort16)(1 + CurrFragmentNumber)); // expected this

                            TxAckMsg.MsgType = MSG_TYPE_ACK; // restore default
                        }
                        pRxStatistics->CountRxDuplicate++;
                        continue;
                    }
                    //else
                    // Totally out of sequence - to be improved
                    {
                        if(MsgHdr.Info == 0) // if not retransmit
                            pRxStatistics->CountRxInvFragmentNum++;

                        /// ???? if Retansmit && Curr - 4 < Frag < Curr
                        // I get this:
                        //ERROR: *   RxThread[1] Rx Fragment totally out of seq from DoseId 7.
                        //    Ignore Got/Exp=4/6 SeqNo=1, LatestSeqNo=1 Retrans=1
                        // MsgHdr.Info = Retransmit > 0 in this case

                        if  (
                                (MsgHdr.Info) // if Retransmit
                                &&
                                (MsgHdr_FragmentNumber < CurrFragmentNumber)

                            )
                        {
                            continue;
                        }

                        // ??? If we  started in the middle of a fragmented
                        // transmission then ????

                        // ??? MUST DO SOMETHING here ?????

                        //if(*pDbg>=3)
                        PrintErr(0,"*   RxThread[%d]"
                            " Rx Fragment totally out of seq from DoseId %d.\n    Ignore"
                            " Got/Exp=%X/%X SeqNo=%d, LatestSeqNo=%d Retrans=%d\n",
                            MyIx, DoseId,
                            MsgHdr_FragmentNumber, 1+CurrFragmentNumber,
                            MsgHdr.SequenceNumber, LatestSequenceNumber,
                            MsgHdr.Info);

                        continue;
                    }
                } // end ERROR - incorrect FragmentNumber
            } // end not first fragment

            //================================================================
            // We have received a fragmented msg with valid SequenceNumber
            // and FragmentNumber. It could be:
            //
            // 1) The first fragment in a fragmented msg - FragmentNumber = 1
            // 2) Not first and not last fragment in a fragmented msg
            // 3) The last fragment in a fragmented msg - FragmentNumber=0x80nn
            //
            // Put data on the receiver Queue. First check if full.
            // Wake up application
            //================================================================

            if(*pDbg>=3)
            {
                char tmp = 0;
                if(MsgHdr.Size >32) // do not print too much
                {
                    tmp = RxData[32]; RxData[32] = 0;
                }

                PrintDbg("*   RxThread[%d] Rx DATA(%d): <%s>\n",
                            MyIx, MsgHdr.Size, RxData);

                if(MsgHdr.Size >32) RxData[32] = tmp;  //restore
            }

            bMsgIsCompleted = FALSE;

            //---------------------------------------------------------------
            // 1) The first fragment in a fragmented msg - FragmentNumber = 1
            // Allocate a buffer for the entire msg
            //---------------------------------------------------------------

            if(MsgHdr.FragmentNumber == 1)
            {
                //NYTT 08-09-18 (2 rader)
                if(g_RxQ[MyIx].RxNodeStatus[DoseId].pBuf != NULL)
                {
                    DoseCom_FreeBuff(g_RxQ[MyIx].RxNodeStatus[DoseId].pBuf);
                    g_RxQ[MyIx].RxNodeStatus[DoseId].pBuf = NULL;
                }

                pReadBuf = DoseCom_GetBuff(MsgHdr.TotalSize);

                // what shall we do if this happens ??????????
                if (pReadBuf == NULL) // no more free buffers
                {
                    if(MsgHdr.bWantAck)
                        Send_AckMsg(&TxSock, &TxAckMsg,MyIx,MsgHdr.IpAddrFrom_nw,
                               MsgHdr.SequenceNumber, MsgHdr.TxMsgArray_Ix,1,
                               (dcom_ushort16)(0x8000 | MsgHdr.Info));

                    PrintErr(0, "*   RxThread[%d]"
                                " Can not get a buffer for receive data %d\n",
                                MyIx, DoseId);

                    pRxStatistics->CountRxOtherErr++;
                    continue;
                }
                g_RxQ[MyIx].RxNodeStatus[DoseId].bIsFragmented  = TRUE;
                g_RxQ[MyIx].RxNodeStatus[DoseId].FragmentNumber = 1;

                g_RxQ[MyIx].RxNodeStatus[DoseId].pBuf   = pReadBuf;
                g_RxQ[MyIx].RxNodeStatus[DoseId].BufPos = MsgHdr.Size;
                memcpy(pReadBuf, RxData, MsgHdr.Size);

                if(*pDbg>=4)
                    PrintDbg("*   RxThread[%d] Rx Fragment 1. Size=%d\n",
                        MyIx, MsgHdr.Size);
            }
            else
            // A 'non-first' when a 'first' is expected.
            if (!g_RxQ[MyIx].RxNodeStatus[DoseId].bIsFragmented)
            {
                if(*pDbg>=1)
                {
                    PrintDbg("*   RxThread[%d]"
                             " Rx Fragment %d from node %d when no fragment"
                             " was expected (discarding it)"
                             " SeqNo=%d, LastSeqNo=%d",
                             MyIx, MsgHdr.FragmentNumber, DoseId,
                             MsgHdr.SequenceNumber, LatestSequenceNumber);
                }
                // Statistics ????
                continue;
            }
            else
            //--------------------------------------------------------
            // 2) Not first and not last fragment in a fragmented msg
            //    Copy received fragment to buffer
            //--------------------------------------------------------
            if((MsgHdr.FragmentNumber & 0x8000) == 0)
            {
                int pos;

                // This is the case 'No Ack' and previous fragments lost
                if(g_RxQ[MyIx].RxNodeStatus[DoseId].pBuf == NULL) continue;

                pos = g_RxQ[MyIx].RxNodeStatus[DoseId].BufPos;
                memcpy(&g_RxQ[MyIx].RxNodeStatus[DoseId].pBuf[pos],
                        RxData, MsgHdr.Size);

                g_RxQ[MyIx].RxNodeStatus[DoseId].BufPos += MsgHdr.Size;

                if(*pDbg>=4)
                    PrintDbg("*   RxThread[%d] Rx Fragment %d. Size=%d\n",
                            MyIx, MsgHdr.FragmentNumber, MsgHdr.Size);
            }
            else
            //--------------------------------------------------------------------
            // 3) The last fragment in a fragmented msg - FragmentNumber = 0x80nn
            //    Copy received fragment to buffer
            //    Set 'bMsgIsCompleted'
            //--------------------------------------------------------------------
            //if(MsgHdr.FragmentNumber & 0x8000)
            {
                int pos;

                if(g_RxQ[MyIx].RxNodeStatus[DoseId].pBuf == NULL) continue;

                pos = g_RxQ[MyIx].RxNodeStatus[DoseId].BufPos;
                memcpy(&g_RxQ[MyIx].RxNodeStatus[DoseId].pBuf[pos],
                        RxData, MsgHdr.Size);

                g_RxQ[MyIx].RxNodeStatus[DoseId].BufPos += MsgHdr.Size;

                pReadBuf = g_RxQ[MyIx].RxNodeStatus[DoseId].pBuf;
                g_RxQ[MyIx].RxNodeStatus[DoseId].pBuf = NULL;

                bMsgIsCompleted = TRUE;

                if(*pDbg>=4)
                    PrintDbg("*   RxThread[%d] Rx LastFragment %X. Size=%d\n",
                                MyIx, MsgHdr.FragmentNumber, MsgHdr.Size);
#ifdef TIMER_TEST
                PrintDbg("Rx Msg Size=%u took %u ms\n",
                        MsgHdr.TotalSize, GetTickCount() - StartTime);
#endif
                g_pShm->Statistics.TotRxCount++;
                g_pShm->NodeStatusTable[DoseId].RxCount++;
            }
            if(MsgHdr.bWantAck)
                Send_AckMsg(&TxSock,&TxAckMsg,MyIx,MsgHdr.IpAddrFrom_nw,
                       MsgHdr.SequenceNumber, MsgHdr.TxMsgArray_Ix,
                       MsgHdr.FragmentNumber, MsgHdr.Info);

        } // end fragmented msg

        else
        //===========================
        // A Non-fragmented msg
        //===========================
        {
            // This is NOT what we expected
            if(MsgHdr.SequenceNumber != (dcom_ushort16)(1+LatestSequenceNumber))
            {
                if(!MsgHdr.bWantAck)
                {
                    if(*pDbg>1)
                        PrintDbg("*   RxThread[%d] NotAcked. DoseId=%d"
                            " Recover from invalid SequenceNumber=%d. Exp=%d\n",
                             MyIx, DoseId, MsgHdr.SequenceNumber,
                             1 + LatestSequenceNumber);

                    goto A_Valid_Msg_Is_Received;
                }
                //------------------------------------------------------------
                // This is not what we expected. The reason could be:
                //
                // 1) A 'We started after the sending node' start-up condition or
                //    a 'We have detected that the sending node has gone down->up
                //    SeqNumIsValid[SeqNumSet] is false.
                //    This is the first message in this set of SequenceNumber,
                //
                // 2) A 'The sending node restarted' start-up condition.
                //    SeqNumIsValid[SeqNumSet] is true.
                //    This is a PoolDistribution msg with SequenceNumber = 0
                //
                // 3) A retransmissin. If so,
                //    SequenceNumber = LatestSequenceNumber
                //    or               LatestSequenceNumber-1
                //    or               LatestSequenceNumber-2
                //    Ignore the msg, Send ACK
                //
                // 4) Previous messages(s) were lost. If so
                //    SequenceNumber = LatestSequenceNumber+2  (one missed)
                //                     LatestSequenceNumber+3  (two missed)
                //    Ignore the msg, Send NACK with expected SequenceNumber
                //
                // 5) Something else
                //--------------------------------------------------------------

                // Case 1 - OK - first msg from this node on this channel
                if(!g_RxQ[MyIx].RxNodeStatus[DoseId].SeqNumIsValid[SeqNumSet])
                {

                    // If first msg is a PD with not PD_FIRSTDATA bit set,
                    // 
                    // 'continue' will cause a lost ack and retransmit.
                    // Note that if no data (only PD_COMPLETE), this bit is set

                    if (MsgHdr.IsPoolDistribution &&
                       ((MsgHdr.IsPoolDistribution & PD_FIRSTDATA) == 0))
                    {
                        // We are expecting the first message in a pool distribution but
                        // are receiving a message "in the middle" of a pool distribution.
                        // This can be the case when we have detected a loss in the communication
                        // with the sender (and have therefor set SeqNumIsValid[SeqNumSet] to false).
                        // However it could be the case that the sender hasn't detected any
                        // communication failure and in this case the sender is expecting us to
                        // send ACKs otherwise the sender will get stuck.
                        // The solution from our side is to send an ACK but throw away the message. We
                        // the rely on the timer that will cause a sending of a request for
                        // pool distribution to the sender.

                        if(*pDbg) PrintDbg("Seems First PD data is lost\n");

                        if(MsgHdr.bWantAck)
                        {
                            Send_AckMsg(&TxSock,&TxAckMsg,MyIx,MsgHdr.IpAddrFrom_nw,
                                        MsgHdr.SequenceNumber, MsgHdr.TxMsgArray_Ix,
                                        MsgHdr.FragmentNumber, MsgHdr.Info);
                        }
                        continue;
                    }

                    g_RxQ[MyIx].RxNodeStatus[DoseId].SeqNumIsValid[SeqNumSet]=1;
                    // Save current SequenceNumber
                    g_RxQ[MyIx].RxNodeStatus[DoseId].SeqNumber[SeqNumSet]
                                            = MsgHdr.SequenceNumber;
                    goto A_Valid_Msg_Is_Received;
                }

                // Case 2 - OK -  - sender has (re)started
                if((MsgHdr.SequenceNumber == 0) && (MsgHdr.IsPoolDistribution))
                {
                    // This might be a restarting node.
                    // Clear all SeqNumIsValid and SeqNumber.
                    for(int jj=0 ; jj<(MAX_NUM_DEST_CHANNELS+2) ; jj++)
                    {
                        g_RxQ[MyIx].RxNodeStatus[DoseId].SeqNumber[jj] = 0;
                        g_RxQ[MyIx].RxNodeStatus[DoseId].SeqNumIsValid[jj] = 0;
                    }

                    g_RxQ[MyIx].RxNodeStatus[DoseId].SeqNumIsValid[SeqNumSet]=1;
                    // Save current SequenceNumber
                    g_RxQ[MyIx].RxNodeStatus[DoseId].SeqNumber[SeqNumSet]
                                            = MsgHdr.SequenceNumber;
                    goto A_Valid_Msg_Is_Received;
                }

                // Case 3 - retransmission
                if(
                    (MsgHdr.SequenceNumber == LatestSequenceNumber)
                    ||
                    (MsgHdr.SequenceNumber == (dcom_ushort16)(LatestSequenceNumber-1))
                    ||
                    (MsgHdr.SequenceNumber == (dcom_ushort16)(LatestSequenceNumber-2))
                  )
                {
                    if((MsgHdr.Info & 7) == 0) // If not retransmit indication
                    {
                        // This should never happen
                        if(*pDbg>=2)
                        PrintDbg("*   RxThread[%d] Rx duplicate from node %d. SeqN=%u"
                                 " But retransmit NOT indicated.\n",
                                 MyIx, DoseId, MsgHdr.SequenceNumber);
                    }
                    else
                    {
                        if(*pDbg>=2)
                        PrintDbg("*   RxThread[%d] Rx duplicate from node %d. SeqN=%u\n",
                                MyIx, DoseId, MsgHdr.SequenceNumber);
                    }

                    if(MsgHdr.bWantAck)
                        Send_AckMsg(&TxSock,&TxAckMsg,MyIx,MsgHdr.IpAddrFrom_nw,
                               MsgHdr.SequenceNumber, MsgHdr.TxMsgArray_Ix,
                                 MsgHdr.FragmentNumber, (dcom_ushort16)(0x100 | MsgHdr.Info));
                    continue;
                }

                // Case 4 - previous msg lost
                if(
                    (MsgHdr.SequenceNumber == (dcom_ushort16)(LatestSequenceNumber+2))
                    ||
                    (MsgHdr.SequenceNumber == (dcom_ushort16)(LatestSequenceNumber+3))
                  )
                {
                    if (*pDbg)
                        PrintDbg("*   RxThread[%d] Prev Msg Lost from node %d. SeqN=%u Exp=%u\n",
                                 MyIx, DoseId, MsgHdr.SequenceNumber, LatestSequenceNumber+1);


                    if(MsgHdr.bWantAck)
                    {
                        TxAckMsg.MsgType = MSG_TYPE_NACK;

                        Send_AckMsg(&TxSock,&TxAckMsg,MyIx,MsgHdr.IpAddrFrom_nw,
                               MsgHdr.SequenceNumber, MsgHdr.TxMsgArray_Ix,
                                 MsgHdr.FragmentNumber,
                                 (dcom_ushort16)(1 + LatestSequenceNumber)); //expected this

                        TxAckMsg.MsgType = MSG_TYPE_ACK;
                    }
                    continue;
                }

                // case 5 - This should never happen ... but apparently it does since the log
                // has appeared occasionally. We try to solve this situation by sending an ack (so the sender
                // doesn' get stuck), ignoring the message and request a pool distribution from the sender.

                PrintDbg("*   RxThread[%d] DoseId=%d Invalid SequenceNumber=%d. Expected=%d\n",
                             MyIx, DoseId, MsgHdr.SequenceNumber,
                             1 + LatestSequenceNumber);

                if(MsgHdr.bWantAck)
                {
                    Send_AckMsg(&TxSock,&TxAckMsg,MyIx,MsgHdr.IpAddrFrom_nw,
                           MsgHdr.SequenceNumber, MsgHdr.TxMsgArray_Ix,
                             MsgHdr.FragmentNumber, (dcom_ushort16)(0x100 | MsgHdr.Info));
                }

                // Save current SequenceNumber
                g_RxQ[MyIx].RxNodeStatus[DoseId].SeqNumber[SeqNumSet]
                                    = MsgHdr.SequenceNumber;

                WakeUp_RequestPoolDistribution(DoseId);

                continue;
            } // end SequenceNumber != ((1+LatestSequenceNumber)

A_Valid_Msg_Is_Received:
            //=======================================================
            // We have received a valid non-fragmented msg.
            //
            // 1) A non-fragmented msg - MsgHdr.FragmentNumber = 0;
            //
            // Put data on the receiver Queue. First check if full.
            // Wake up application
            //=======================================================

            // Save current SequenceNumber
            g_RxQ[MyIx].RxNodeStatus[DoseId].SeqNumber[SeqNumSet]
                                            = MsgHdr.SequenceNumber;
            g_RxQ[MyIx].RxNodeStatus[DoseId].SeqNumIsValid[SeqNumSet] = 1;

            if(*pDbg>=3)
            {
                char tmp = 0;
                if(MsgHdr.Size >32) // do not print too much
                {
                    tmp = RxData[32]; RxData[32] = 0;
                }
                PrintDbg("*   RxThread[%d] Rx DATA(%d): <%s>\n",
                            MyIx, MsgHdr.Size, RxData);

                if(MsgHdr.Size >32) RxData[32] = tmp;  //restore
            }
            g_pShm->Statistics.TotRxCount++;
            g_pShm->NodeStatusTable[DoseId].RxCount++;

            bMsgIsCompleted = FALSE;

            //----------------------------------------
            // A non-fragmented msg
            // Allocate a buffer for the entire msg
            //----------------------------------------

            //NYTT 08-09-18 (2 rad)
            if(g_RxQ[MyIx].RxNodeStatus[DoseId].pBuf != NULL)
            {
                DoseCom_FreeBuff(g_RxQ[MyIx].RxNodeStatus[DoseId].pBuf);
                g_RxQ[MyIx].RxNodeStatus[DoseId].pBuf = NULL;
            }

            if(MsgHdr.Size<16)
                pReadBuf = DoseCom_GetBuff(16);
            else
                pReadBuf = DoseCom_GetBuff(MsgHdr.Size);

            if (pReadBuf == NULL) // no more free buffers
            {
                //NYTT 08-09-18 (1 rader)
                g_RxQ[MyIx].RxNodeStatus[DoseId].pBuf = NULL;

                PrintErr(0, "*   RxThread[%d]"
                            " Can not get a buffer for receive data %d\n",
                            MyIx, DoseId);

                pRxStatistics->CountRxOtherErr++;

                if(MsgHdr.bWantAck)
                    Send_AckMsg(&TxSock,&TxAckMsg,MyIx,MsgHdr.IpAddrFrom_nw,
                           MsgHdr.SequenceNumber, MsgHdr.TxMsgArray_Ix, 0,
                           (dcom_ushort16)(0x8000 | MsgHdr.Info));
                continue;
            }

            memcpy(pReadBuf,RxData, MsgHdr.Size);

            bMsgIsCompleted = TRUE;

            if(*pDbg>=4)
                PrintDbg("*   RxThread[%d] Rx non-fragment Size=%d\n",
                        MyIx, MsgHdr.Size);

            if(MsgHdr.bWantAck)
                Send_AckMsg(&TxSock,&TxAckMsg,MyIx,MsgHdr.IpAddrFrom_nw,
                       MsgHdr.SequenceNumber, MsgHdr.TxMsgArray_Ix,
                       MsgHdr.FragmentNumber, MsgHdr.Info);

        } // end Non-fragmented msg

        //=========================================================
        // Common for fragmented and non-fragmented
        // The message (fragmented or not-fragmented) is completed.
        // Put it on the g_RxQ[] and wakeup application
        //=========================================================

        if(!bMsgIsCompleted) continue;

        if(*pDbg>=3)
            PrintDbg("*   RxThread[%d] Rx Msg is completed\n", MyIx);

        // This is a special msg that not shall be passed to appl
        if(MsgHdr.IsPoolDistribution & PD_ISCOMPLETE)
        {
            if(*pDbg>=3)
                PrintDbg("*   RxThread[%d] Got PdIsComplete for Node %d\n",
                        MyIx, DoseId);
            CNodeStatus::Set_HasReceivedPdComplete(DoseId);

            if(MsgHdr.bWantAck) pRxStatistics->CountRxWithAckOk++;
            else                pRxStatistics->CountRxNoAckOk++;

            //Free the allocated data
            DoseCom_FreeBuff(pReadBuf);
            pReadBuf = NULL;
            g_RxQ[MyIx].RxNodeStatus[DoseId].pBuf = NULL;

            continue;
        }

        // This is the first message for a PD
        if (MsgHdr.IsPoolDistribution & PD_FIRSTDATA)
        {
            CNodeStatus::Set_HasReceivedPdStart(DoseId);
        }

        // Wait until there is space on g_RxQ
        // new_050426 - shall we have a timeout ?
        int WaitCount = 0;
        for(;;) // Wait here until Appl has consumed at least one msg
        {
            if((g_RxQ[MyIx].Put_Ix + 1) >= MAX_RX_QUEUE) Next_PutIx = 0;
            else Next_PutIx = g_RxQ[MyIx].Put_Ix + 1;
            if(Next_PutIx != g_RxQ[MyIx].Get_Ix) break; // there is free space

            WaitCount++;
            pRxStatistics->CountRxMustWait++;

            //STLRHA: Added this kick to make sure that dose_main really
            //takes the message. Can probably be removed when we're a bit
            //more sure of dose_mains behaviour...

            WakeUp_Reader(MyIx);
            if(*pDbg>=4)
                PrintDbg("*   RxThread[%d] Must wait for app to free space (%d)\n",
                           MyIx, WaitCount);
            DoseOs::Sleep(5);
        }

        // Log max used queue length
        dcom_ushort16 NumUsed;

        // Log max used queue length
        if(Next_PutIx < g_RxQ[MyIx].Get_Ix) //if wrapped
            NumUsed = (dcom_ushort16)(MAX_RX_QUEUE + Next_PutIx - g_RxQ[MyIx].Get_Ix);
        else
            NumUsed = (dcom_ushort16) (Next_PutIx - g_RxQ[MyIx].Get_Ix);

        if(NumUsed > g_RxQ[MyIx].MaxUsedQueueLength)
                g_RxQ[MyIx].MaxUsedQueueLength = NumUsed;

        // Put it on the Queue

        Put_Ix = g_RxQ[MyIx].Put_Ix;

        g_RxQ[MyIx].RxQueue[Put_Ix].UsrMsgLength = MsgHdr.TotalSize;
        g_RxQ[MyIx].RxQueue[Put_Ix].pReadDataBuf = pReadBuf;
        g_RxQ[MyIx].RxQueue[Put_Ix].DoseId       = DoseId;

        g_RxQ[MyIx].Put_Ix = Next_PutIx;

        //NYTT 08-09-18
        // This indicates that the application is responsible to free
        // the memory buffer.
        // RxThread() must check this and if != NULL, free memory
        // it before allocating new. (in case transfer aborted)

        g_RxQ[MyIx].RxNodeStatus[DoseId].pBuf = NULL; //NYTT 08-09-18

        if(MsgHdr.bWantAck) pRxStatistics->CountRxWithAckOk++;
        else                pRxStatistics->CountRxNoAckOk++;

        WakeUp_Reader(MyIx); // signal application reader thread
    } // end for(;;)
}
/*--------------------- end RxThread() --------------*/

//########################################
// Section Entry routines
//########################################

/*********************************************************************
* Called by application - read a message from one of the Queues.
*
* Parameters:
* -----------
* RxUseBitMap     Contains a bitmap which PriorityChannels to read from
*                 Bit 0 = Ch 0, ...
* *pRxFromBitMap  Returns bitmap for the Queue data was read from.
* *pSize          Returns size of msg
*
* Returns:
*   DOSE_OK     If OK
*   DOSE_NO_MSG If nothing to read
**********************************************************************/
int CDoseComReceive::Read_Msg(dcom_ulong32 RxUseBitMap, dcom_ulong32 *pRxFromBitMap,
                                char **ppBuf, dcom_ulong32 *pSize,
                                 bool *pIsNative)
{
    int     qIx;
    dcom_ulong32   Get_Ix ;
    dcom_ulong32   BitMap = 1;


    // Scan through all defined by RxUseBitMap (in priority order)

    for(qIx=0 ; qIx<MAX_NUM_PRIO_CHANNELS ; qIx++, BitMap <<= 1)
    {
        if((BitMap & RxUseBitMap) == 0) continue; // not defined to use

        if(g_RxQ[qIx].Put_Ix == g_RxQ[qIx].Get_Ix)
            continue; // this Queue is empty
        // A not empty Queue is found
        break;
    }

    if( qIx == MAX_NUM_PRIO_CHANNELS) // we did not find anything
    {
        *pRxFromBitMap = 0;
        return(ERR_DOSECOM_NO_MSG);
    }

    // A not empty Queue is found. Get the msg.

    Get_Ix = g_RxQ[qIx].Get_Ix;

    *ppBuf  = g_RxQ[qIx].RxQueue[Get_Ix].pReadDataBuf;
    *pSize  = g_RxQ[qIx].RxQueue[Get_Ix].UsrMsgLength;

    if((Get_Ix+1) >= MAX_RX_QUEUE)
        g_RxQ[qIx].Get_Ix=0; else g_RxQ[qIx].Get_Ix++;

    *pRxFromBitMap = BitMap;

    *pIsNative = !!g_RxQ[qIx].RxQueue[Get_Ix].bIsNative; //!!-->char to bool

    return(ERR_DOSECOM_OK);
}

/**********************************************************
*
*
***********************************************************/
int CDoseComReceive::Receive_Init(dcom_ushort16 DoseId)
{
    int jj;
    unsigned long tid[MAX_NUM_PRIO_CHANNELS];
    g_MyDoseId             = DoseId;
    g_MyDoseIdBitMap       = 1<<(g_MyDoseId & 0x1F);
    g_MyDoseIdBitMap_Index = (g_MyDoseId<32) ? 0 : 1;

    g_pShm = CNodeStatus::GetNodeSharedDataPointer();

    //-------------------------------------------------------------------
    // Start receiver/transmitter Threads - on for each Priority Channel
    //-------------------------------------------------------------------

    for(jj=0 ; jj< MAX_NUM_PRIO_CHANNELS ; jj++)
    {
        int * arg = new int;
        *arg = jj;
        DoseOs::CreateThread(tid[jj], &RxThread, arg);

        DoseOs::Sleep(10);
    }
    DoseOs::Sleep(10);
    return(0);
}
/************************************************************
*
************************************************************/
void CDoseComReceive::Get_Info(char *pBuf)
{
#if (MAX_NUM_PRIO_CHANNELS == 6)
    sprintf(pBuf,"PutIx =%u/%u/%u/%u/%u/%u\n"
                 "GetIx =%u/%u/%u/%u/%u/%u\n"
                 "MaxUse=%d/%d/%d/%d/%d/%d\n",
        g_RxQ[0].Put_Ix, g_RxQ[1].Put_Ix,
        g_RxQ[2].Put_Ix, g_RxQ[3].Put_Ix,
        g_RxQ[4].Put_Ix, g_RxQ[5].Put_Ix,
        g_RxQ[0].Get_Ix, g_RxQ[1].Get_Ix,
        g_RxQ[2].Get_Ix, g_RxQ[3].Get_Ix,
        g_RxQ[4].Get_Ix, g_RxQ[5].Get_Ix,
        g_RxQ[0].MaxUsedQueueLength,
        g_RxQ[1].MaxUsedQueueLength,
        g_RxQ[2].MaxUsedQueueLength,
        g_RxQ[3].MaxUsedQueueLength,
        g_RxQ[4].MaxUsedQueueLength,
        g_RxQ[5].MaxUsedQueueLength);
#else
    sprintf(pBuf,"PutIx =%u/%u/%u/%u\nGetIx =%u/%u/%u/%u\n"
                 "MaxUse=%d/%d/%d/%d\n",
        g_RxQ[0].Put_Ix, g_RxQ[1].Put_Ix,
        g_RxQ[2].Put_Ix, g_RxQ[3].Put_Ix,
        g_RxQ[0].Get_Ix, g_RxQ[1].Get_Ix,
        g_RxQ[2].Get_Ix, g_RxQ[3].Get_Ix,
        g_RxQ[0].MaxUsedQueueLength,
        g_RxQ[1].MaxUsedQueueLength,
        g_RxQ[2].MaxUsedQueueLength,
        g_RxQ[3].MaxUsedQueueLength);
#endif
}
/*-------------------------- end DoseComReceive.cpp ------------------*/
