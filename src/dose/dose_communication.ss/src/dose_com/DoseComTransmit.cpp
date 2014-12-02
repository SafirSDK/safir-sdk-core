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

/**********************************************************************
* DoseComTransmit.cpp - a part of DoseComDll - For LINUX and WIN32
*
* Handles transmit messages
*
* Design:
* ------
*                event
*    ------------------------+    +-------------+
*       +-----------------+  +--->!             !
* Appl  ! TxQ[]           !       !TxThread     !      Nodes on
* ----->! Appl puts a msg !------>!sends the msg!----> Network
* Send  ! on one of the   !       !gets ACK from!
*       ! Queues          !   +-->!AckQueue     !
*       +-----------------+   !   +-------------+
*                             !     A
*                      +------+     !  event
*                      !            !
*               +----------+     +----------------+
*               !AckQueue[]!     !AckThread       !     Nodes on
*               !          !<----!receivs ACK and !<----Network
*               !          !     !puts in AckQueue!
*               +----------+     +----------------+
*
* Entries:
* ---------
* int  CDoseComTransmit::Xmit_Init()   // initialisation
* void CDoseComTransmit::Set_PoolDistributionIsCompleted()
* int  CDoseComTransmit::Xmit_Msg()    // Appl sends amsg
* void CDoseComTransmit::Get_Info()    // for monitoring
*
***************************************************************************/
#ifndef _CRT_SECURE_NO_DEPRECATE
#define _CRT_SECURE_NO_DEPRECATE
#endif

#define IS_USING_SOCKETS
#define IS_USING_THREADS

#include "DosePlatform.h"

#include <string.h> // LINUX memset(), strlen()

#include "DoseOsInterface.h"
#include "DoseComConfig.h"
#include "DoseComStatistics.h"
#include "../defs/DoseUdpMsg.h"
#include "DoseComTransmit.h"
#include "PrintError.h"
#include "IpmSocket.h"
#include "../defs/DoseNodeStatus.h"
#include "../defs/DoseCom_Interface.h"  // to get an error code

#include <ace/Thread_Mutex.h>
#include <ace/Guard_T.h>
#include <Safir/Dob/Internal/Atomic.h>

namespace Atomics = Safir::Dob::Internal::Atomics;

//--------------------
// Externals
//--------------------
extern void WakeUp_QueueNotFull(int PrioChannel);
extern int DoseCom_FreeBuff(char *pBuf);
extern volatile int * volatile pDbg;

//--------------------------------------------------------------------------
// If a receiver does not send an ACK, the msg is retransmitted at
// cyclic intervals. (TIMEOUT_WAITING_FOR_ACK ms).
// Before each retransmit, a check is made to find out if the node
// has been reported as 'down' by the KeepAlive function. (takes 2 sec ???).
// If so, the node is removed from the list of nodes that are expected to Ack,
// and the transmission will continue,
//
// But if by some reason the KeepAlive function reports the node as up, but
// no Acks are received (e.g. the receiver thread in DoseMain has crashed),
// there will be a 'retry forever' condition if we not enable the
// 'GIVEUP_AFTER_RETRIES' function.
//
// See Handle_Timeout() for more details.
//--------------------------------------------------------------------------
#define zzzGIVEUP_AFTER_RETRIES   // Enabl/Disable Giveup function via this
#ifdef GIVEUP_AFTER_RETRIES

#define MAX_XMIT_RETRIES    5
#define GIVEUP_TIMEOUT      5000  // ms

#endif

//-------------------------------------------
// Local static data
//-------------------------------------------

// We really don't rely on the lock free approach taken for the original design. This lock is
// used to protect the different threads from concurrent access to the transmit queues.
static ACE_Thread_Mutex g_threadLock;

volatile static DOSE_SHARED_DATA_S *g_pShm;
volatile static NODESTATUS_TABLE *g_pNodeStatusTable;
volatile static dcom_ushort16 g_MyDoseId = 0xFFFF;
volatile static Statistics::TX_STATISTICS_S *g_pTxStatistics;
volatile static int g_QueueIndex_CurrPoolDistribution = -1;

//-----------------------------------------------------------
// This is used to wakeup TxThread
// g_OsinEventHandler is created by Xmit_Thread()
//
// It is set by:
// 1) DOSE_Xmit_Msg() when appl has put a msg on the que
// 2) Ack_Thread() when an Ack has been received
//-----------------------------------------------------------

static DoseOs::CDoseComEvent *g_hTxEvent;

extern volatile int * volatile pDbg;

#define TIMEOUT_WAITING_FOR_ACK     100 // was 40  ms
#define WAITTIME_WAITING_FOR_ACK    40  // ms
#define WAITTIME_WAITING_NOT        (60*1000)   // 60 sec

//---------------------------------------------------------------------
// MTU=1516, EthHdr=16, IpHdr=20, UdpHdr=8 ==> MaxMsgSize not fragmented=1472.
// MyUdpHdr=32 ==> MaxSize Appl msg not fragmented = 1472-32=1440
//
// Selecting 1448 gives us a spare of 24 bytes
// FRAGMENT_SIZE = OurMsg + MyUdpHdr
// Max MsgSize for not fragmented = 1448 - 32 = 1416
//---------------------------------------------------------------------

#define FRAGMENT_SIZE 1448 // OurHdr+MsgSize

//-----------------------------------------------------------------
// When an ACK message is received it is placed here by Ack_Thread.
// Ack_Put_ix is then updated.
// TxThread is waked up by an event.
//
// The Xmit_Thread checks if Ack_Get_ix != Ack_Put_ix.
// If so, the ACK on the Queue handled. Ack_Get_ix is then updated
// by TxThread.
//------------------------------------------------------------------

// Size should be Max number of destinstions * MAX_AHEAD, but since the AckThread
// checks if buffer g_Ack_Queue[] is full, we don't need handle the max case.
#define MAX_ACK_QUEUE 128

// These are used by two different threads, so 'volatile' might be good here
volatile static dcom_ulong32 g_Ack_Put_ix = 0;
volatile static dcom_ulong32 g_Ack_Get_ix = 0;

volatile static struct
{
    volatile dcom_ulong32  SequenceNumber;
    volatile dcom_ulong32  IpAddrFrom_nw;
    volatile dcom_ushort16 FragmentNumber;
    volatile dcom_ushort16 Info;
    volatile dcom_uchar8  TxMsgArray_Ix;
    volatile dcom_uchar8  DoseIdFrom;
    volatile dcom_uchar8  TxQueueNumber;  // = PriorityChannel
    volatile dcom_uchar8  MsgType;
} g_Ack_Queue[MAX_ACK_QUEUE] = {{0}};

//=====================================================================
// Send algorithm.
// 1) When the TxQ[x] is empty and there are no pending Acks
//    PutIx = GetIxToSend = GetIxToAck
//
// 2) An application sends a msg by using routine Xmit_msg(). The msg is
//    placed in TxQ[x].TxMsgArr[PutIx].
//    PutIx is then increased by 1. Then TxThread is waked up by an event.
//    (first a check to prevent overflow is made).
//
// 3) When waked up, TxThread checks if GetIxToSend != PutIx
//    If so, TxThread checks number of 'Pending Acks' = GetIxToSend - GetIxToAck.
//    If this is < MAX_AHEAD_QUEUE, the message is sent.
//    and GetIxToSend is increased by 1.
//    If too many pending Acks, TxThread sleeps
//
// 4) The Ack_thread receives an Ack message
//
// ---------------------------------------------------------------------------
// PutIx       = Index for next position to place a msg on.
//               Increased by application after a new msg is added.
// GetIxToSend = Index for next pos to send.
//               Increased by TxThread when the msg has been sent and
//               when no targets to send to, and when msgbuff == NULL
//               For fragmented, it is updated when last fragment has been
//               sent (before it has been acked). (except for retransmit).
// GetIxToAck  = Index for next position to be acked.
//               Increased by TxThread - CleanUp_After_Msg_Completed() when the
//               msg has been acked. If no ack requested, increased to next ...
//
// PutIx GetIxToSend GetIxToAck State
// -----------------------------------------------
//  N       N           N       Idle
//  N+1     N           N       Appl has put a msg on queue
//  N+1     N           N       TxThread has sent first fragment
//  N+1     N+1         N       TxThread has sent last fragment or non fragmented
//  N+1     N+1         N+1     Last fragment or non-fragmented acked = Idle
//
// PutIx-Max       = GetIxToAck + MAX_XMIT_QUEUE - 1
// GetIxToSend-Max = GetIxToAck +�MAX_AHEAD_NF
//============================================================================

#define NUM_TX_QUEUES   MAX_NUM_PRIO_CHANNELS
#define MAX_XMIT_QUEUE  24
#define MAX_AHEAD_F     3   //
#define MAX_AHEAD_NF    2   // value 2 --> 3 ahead for non-fragmented

#define MASK_AHEAD      7   // used as: Index = FragmentNumber & MASK_AHEAD
#define MAX_AHEAD       MASK_AHEAD + 1

// Defines how many messages can be sent from a Queue before
// letting next Queue send.
// ### Set proper values ###
static const int g_MaxLapCount[NUM_TX_QUEUES] = {8,6,4,3,2,2};

// About volatile
// Ack_Thread() is not using anything in this struct
// Appl uses: PutIx, GetIxToAck, TtransmitQueueOverFlow,MaxQueueLength, TxMsgArr

typedef volatile struct
{
    volatile dcom_ushort16 PutIx;          // See comment above
    volatile dcom_ushort16 GetIxToAck;     // See comment above
    volatile dcom_ushort16 GetIxToSend;            // See comment above

    volatile dcom_ushort16 GetIxToSendHighWatermark; // GetIxToSend is "backed" to GetIxToAck when a nack is received
                                                     // so this variable is needed to be able to figure out if there
                                                     // is a chance that the nacked message will ever be sent again.

    volatile dcom_ushort16 MaxUsedQueueLength;

    // wA/wT tells us if Application/TxThread writes the data
    struct
    {
        volatile dcom_ulong64 DstBitMap64;       //wA
        volatile dcom_ulong64 ExpAckBitMap64[MAX_AHEAD]; //wT Nodes that not has sent Ack
                                            //Fragm: Ix = (FrNum&7). NonFr: Ix = 0;
        char    * volatile pMsgBuff;        //wA
        volatile dcom_ulong32  UsrMsgLength;       //wA
        volatile dcom_uchar8  DestinationId;      //wA (0-63) + (64-MAX_NUM_DEST_CHANNELS)
        volatile dcom_uchar8  IsPoolDistr;        //wA
        volatile dcom_uchar8  bUseAck;            //wA
        volatile dcom_uchar8  bIsFragmented;      //wT

        // These are bitmapped. Non-fragmented - bit 0 is used
        // Fragmented - bit number corresponds to  1<<(FragmentNumber & 3)

        volatile char    TransmitComplete;   //wT - Set when a msg or fragment is completed (ack rec)
        volatile dcom_uchar8   IsRetransmitting;   //wT one or more nodes needs rexmit
        dcom_uchar8   spare;              //SeqNumIsRestarted;
        volatile dcom_uchar8  ShallFreeBuffer;

        volatile dcom_ushort16  IsTransmitting;     // wT - Set when msg is sent
                                    // Non-fragm: bit 0 is used
                                    // Fragm: Bit 0-7 <--> 1<<(fragmNum & 7).
                                    //        Bit 15 set until entire msg is completed.

        volatile dcom_ushort16  SequenceNumber;     // wT To this IpM_Addr (to cmp with Acks)

        volatile dcom_ushort16  NotAckedFragment;   // wT First not acked fragment.
                                    // Init to 0 for non-fragmented msg.
                                    // Init to 1 for fragmented msg
        volatile dcom_ushort16  SentFragment;       // Latest sent fragment. Inc when fragm is sent.
        volatile dcom_ushort16  LastFragment;       // Last fragment to be sent
        volatile dcom_ushort16  AckAheadFragments[MAX_AHEAD];  // wT Temporary storage for received acks that have a higher
                                                               // fragment number than NotAckedFragment (They exist due
                                                               // to the sent ahead mechanism for fragments)
    } TxMsgArr[MAX_XMIT_QUEUE];     // 32 or 64

    volatile dcom_ulong64 CurrFragmentedMsgAckBitMap64;  // used for entire fragmented msg
    volatile dcom_ulong32   StartSendTime;
    volatile dcom_ushort16  TxSequenceNumber[64+MAX_NUM_DEST_CHANNELS]; //wT
#ifdef USE_RESTART_SEQNUM
    volatile dcom_uchar8   TxSeqNumIsRestarted[64+MAX_NUM_DEST_CHANNELS];  //wT
#endif
    volatile dcom_uchar8   bIsSendingFragmented;   //
    volatile dcom_uchar8   RetryCount;
    volatile dcom_ushort16  Last_PoolDistribution_Put_Ix;

    volatile int LapCount; // Cleared before sending, increased for each sent message

    // incremented by DOSE_Xmit_msg() when overflow
    // cleared by Xmit_Thread() when free entries in Queue
    boost::uint32_t TransmitQueueOverflow;

    volatile dcom_uchar8   bSendQueueNotOverflow;

} DOSE_TXQUEUE_S;

volatile static DOSE_TXQUEUE_S TxQ[NUM_TX_QUEUES] = {{0}};

//#############################################
// Section Ack_Thread() routines
//#############################################

/***************************************************
* This thread receives ACK messages from all nodes.
* All that is done is:
* 1) The ACK is put in the Ack_Queue[]
* 2) The Xmit_Thread is waked up.
*
* Note: ACKs are sent as UDP Unicast
*
* Need a separate thread for this so it can
* consume all ACKs at high speed.
******************************************************/

static THREAD_API Ack_Thread(void *)
{
    int     result;
    DOSE_UDP_ACK_MSG UdpMsg;
    CIpmSocket RxSock;


    if(*pDbg>2)
        PrintDbg("+++ Ack_Thread() Starts. AckPort=%d\n",
                    CConfig::m_Dose_Port_Ack);

    // Initialization

    result = RxSock.CreateIpMulticastSocket(
                        1,0,    // Rx, and not Tx
                        0,      //:m_IpMultiCastAddr_nw,
                        0,      // Rx only, no ttl
                        CConfig::m_Dose_Port_Ack,
                        65536,  // Opt_so_rcvbuf_size, AIWI: Increased this to the same value as used for the receiver thread.
                        0);     // Opt_So_Rcvbuf_Timeout,

    if(result == -1)
    {
        PrintErr(0,"+++ Ack_Thread() Create Socket failed. NOT Terminating.\n");
    }
    if(*pDbg>2)
        PrintDbg("+++ Ack_Thread(). Has created socket\n");

    //===============
    // Loop for ever
    //===============

    for(;;)
    {
        if(*pDbg>=5)
            PrintDbg("+++ Ack_Thread(). Call RxSock.RecvFrom2()\n");

        // Check if g_Ack_Queue[] is full. If so do not receive more until
        // there is more space.
        for (;;)
        {
            ACE_Guard<ACE_Thread_Mutex> lck(g_threadLock);
            
            if(
                    ( (g_Ack_Get_ix - g_Ack_Put_ix) == 1)
                    ||
                    ( (g_Ack_Get_ix == 0) && (g_Ack_Put_ix == (MAX_ACK_QUEUE-1)) )
                    
                    )
            {
                if(*pDbg>=5)
                {
                    PrintDbg("+++ Ack_Thread(). g_Ack_Queue[] is full, sleeping 30 ms. Ackix=%u Putix=%u\n");
                }
                lck.release();
                DoseOs::Sleep(30);
            }
            else
            {
                break;
            }
        }

        result = RxSock.RecvFrom2((char *) &UdpMsg, sizeof(UdpMsg), NULL,0);

        if(result == -1)
        {
            if(*pDbg)
                PrintErr(0, "    Ack_Thread(). RxSock.RecvFrom2() failed\n");
            DoseOs::Sleep(1000); continue;
        }

        if(*pDbg>=5) PrintDbg("+++ Ack_Thread() got a msg\n");

        if( UdpMsg.Magic != DOSE_MSG_MAGIC)
        {
            PrintDbg("+++ Ack_Thread() got a junk\n");
            continue; //got junk
        }

        if((UdpMsg.MsgType == MSG_TYPE_ACK) || (UdpMsg.MsgType == MSG_TYPE_NACK))
        {
            ACE_Guard<ACE_Thread_Mutex> lck(g_threadLock);

            if(*pDbg>=5)
            PrintDbg("+++ AckThread() MsgTyp=%s DoseId=%d "
                    "Seq=%d FrNum=%d Info=%d qIx=%u Put=%u\n",
                    (UdpMsg.MsgType == MSG_TYPE_ACK) ? "ACK" : "NACK",
                    UdpMsg.DoseIdFrom, UdpMsg.SequenceNumber,
                    UdpMsg.FragmentNumber, UdpMsg.Info, UdpMsg.TxQueueNumber, g_Ack_Put_ix);

            // Got an ack, put on the Queue, Wakeup
            g_Ack_Queue[g_Ack_Put_ix].MsgType        = UdpMsg.MsgType;
            g_Ack_Queue[g_Ack_Put_ix].SequenceNumber = UdpMsg.SequenceNumber;
            g_Ack_Queue[g_Ack_Put_ix].TxMsgArray_Ix  = UdpMsg.TxMsgArray_Ix;
            g_Ack_Queue[g_Ack_Put_ix].IpAddrFrom_nw  = UdpMsg.IpAddrFrom_nw;
            g_Ack_Queue[g_Ack_Put_ix].DoseIdFrom     = UdpMsg.DoseIdFrom;
            g_Ack_Queue[g_Ack_Put_ix].TxQueueNumber  = UdpMsg.TxQueueNumber;
            g_Ack_Queue[g_Ack_Put_ix].Info           = UdpMsg.Info;

            g_Ack_Queue[g_Ack_Put_ix].FragmentNumber = UdpMsg.FragmentNumber;

            if((g_Ack_Put_ix + 1) >= MAX_ACK_QUEUE) g_Ack_Put_ix = 0;
            else g_Ack_Put_ix++;

            //if(*pDbg>3)
            //  PrintDbg("+++ Ack_Thread() Set Event %d\n", DoseOs::Get_TickCount());

            //---------------------------------------------
            // Wakeup TxThread
            // Here we could add some condition to wakeup
            //  1) The AckQueue is becomming full
            //  2) TxQ[TxQueueNumber].TxMsgArr[TxMsgArray_Ix].ExpectedAckBitMap
            //---------------------------------------------

            // update the "last seen" timestamp so we know that the node is alive
            if(UdpMsg.DoseIdFrom < NODESTATUS_TABLE_SIZE)
                g_pNodeStatusTable[UdpMsg.DoseIdFrom].LatestTime = DoseOs::Get_TickCount();

            g_hTxEvent->Set();
        }
       // else
       // {
       //     //if(*pDbg) PrintDbg("+++ Ack_Thread() Got junk\n");;
       // }
    } // end for(;;)
#ifndef _MSC_VER //disable warning in msvc
    return 0;
#endif
}
/*------------------end  Ack_Thread() ---------------*/

//#############################################
// Section TxThread() routines
//#############################################

//Calculate current number of sent items, i.e used sliding window.
int CalculateCurrentSendAhead(int qIx)
{
    if (TxQ[qIx].GetIxToSend>=TxQ[qIx].GetIxToAck)
    {
        return TxQ[qIx].GetIxToSend-TxQ[qIx].GetIxToAck;
    }
    else
    {
        //This handles the wrap-around-case when IxToAck > IxToSend
        return MAX_XMIT_QUEUE -TxQ[qIx].GetIxToAck + TxQ[qIx].GetIxToSend;
    }    
}

void SetIndexToSend(int qIx, dcom_ushort16 toIndex)
{
    if(*pDbg>=3)
    {
        PrintDbg("#   SetIndexToSend(%d) P/S/A=%d/%d/%d, toIndex=%d\n",
                     qIx, TxQ[qIx].PutIx, TxQ[qIx].GetIxToSend, TxQ[qIx].GetIxToAck, toIndex);
    }

    int prevIxToSend = TxQ[qIx].GetIxToSend;
    TxQ[qIx].GetIxToSend = toIndex;

    TxQ[qIx].GetIxToSendHighWatermark = TxQ[qIx].GetIxToSend;

    //Calculate current number of sent items, i.e used sliding window
    int currentNumberOfSent = CalculateCurrentSendAhead(qIx);
    
    //Check if we are allowed to send one more message
    if (currentNumberOfSent>MAX_AHEAD_NF+1) //We are allowed to have 1+MAX_AHEAD_NF out at the same time
    {        
        //This should never happen. If we get here we want to send more messages when we already reached the maximum allowed sendAhead.
        PrintDbg("SetIndexToSend(%d) called with non-valid value. P/S/A=%d/%d/%d, toIndex=%d\n",
                 qIx, TxQ[qIx].PutIx, prevIxToSend, TxQ[qIx].GetIxToAck, toIndex);
        /*while(true)
        {
            DoseOs::Sleep(1000);
        }*/
    }
}

//Increase index to send and check that we not exceeds the sliding window.
void IncreaseIndexToSend(int qIx)
{
    if(*pDbg>=3)
    {
        PrintDbg("#   IncreaseIndexToSend(%d) P/S/A=%d/%d/%d\n",
                     qIx, TxQ[qIx].PutIx, TxQ[qIx].GetIxToSend, TxQ[qIx].GetIxToAck);
    }

    //Increase the sendIndex, handle wrap-around
    if((TxQ[qIx].GetIxToSend + 1) >= MAX_XMIT_QUEUE)
    {
        TxQ[qIx].GetIxToSend = 0;
    }
    else
    {
            TxQ[qIx].GetIxToSend++;
    }

    TxQ[qIx].GetIxToSendHighWatermark = TxQ[qIx].GetIxToSend;

    //Calculate current number of sent items, i.e used sliding window
    int currentNumberOfSent = CalculateCurrentSendAhead(qIx);
    
    if (currentNumberOfSent>MAX_AHEAD_NF+1) //We are allowed to have 1+MAX_AHEAD_NF out at the same time
    {
        //This should never happen. If we get here we want to send more messages when we already reached the maximum allowed sendAhead.
        PrintDbg("IncreaseIndexToSend(%d) called when not allowed. P/S/A=%d/%d/%d\n",
                 qIx, TxQ[qIx].PutIx, TxQ[qIx].GetIxToSend, TxQ[qIx].GetIxToAck);
        /*while(true)
        {
            DoseOs::Sleep(1000);
        }*/
    }
}

//Increase index to ack and check that we do not exceed the sliding window or move ixToAck before ixToSend.
void IncreaseIndexToAck(int qIx)
{
    if(*pDbg>=3)
    {
        PrintDbg("#   IncreaseIndexToAck(%d) P/S/A=%d/%d/%d\n",
                     qIx, TxQ[qIx].PutIx, TxQ[qIx].GetIxToSend, TxQ[qIx].GetIxToAck);
    }

    if (TxQ[qIx].GetIxToSend==TxQ[qIx].GetIxToAck)
    {
        //Not allowed to step IxToAck ahead of IxToSend
        PrintDbg("IncreaseIndexToAck(%d) called when not allowed. P/S/A=%d/%d/%d\n",
                 qIx, TxQ[qIx].PutIx, TxQ[qIx].GetIxToSend, TxQ[qIx].GetIxToAck);
        /*while(true)
        {
            DoseOs::Sleep(1000);
        }*/
    }

    if((TxQ[qIx].GetIxToAck + 1) >= MAX_XMIT_QUEUE)
    {
        TxQ[qIx].GetIxToAck = 0;
    }
    else
    {
        TxQ[qIx].GetIxToAck++;
    }

    int currentNumberOfSent = CalculateCurrentSendAhead(qIx);
    if (currentNumberOfSent>MAX_AHEAD_NF+1)
    {
        //This should never happen. If we get here the IncreaseIndexToAck increased the current used sliding window.
        PrintDbg("IncreaseIndexToAck(%d) increased the ahead gap to exceed the limit. P/S/A=%d/%d/%d\n",
                    qIx, TxQ[qIx].PutIx, TxQ[qIx].GetIxToSend, TxQ[qIx].GetIxToAck);
        /*while(true)
        {
            DoseOs::Sleep(1000);
        }*/
    }
}

/**************************************************************************
* Called from TxThread() when there are no targets to a message in
* TxQ[qIx].TxMsgArr[TxMsgArrIx]
*
* The message will be ignored (GetIxToSend updated) only when TxQ[qIx].GetIxToAck
* also can be updated. This is the case if TxMsgArrIx == TxQ[qIx].GetIxToAck.
*
* Returns true if the message has been ignored.
*
****************************************************************************/
static bool CleanUp_After_Msg_Ignored(int qIx, int TxMsgArrIx)
{
    if(*pDbg>=3)
    {
        PrintDbg("#   CleanUp_After_Msg_Ignored(%d) P/Gs/Ga=%d/%d/%d ArrIx=%d SeqN=%u\n",
                 qIx, TxQ[qIx].PutIx, TxQ[qIx].GetIxToSend, TxQ[qIx].GetIxToAck,
                 TxMsgArrIx, TxQ[qIx].TxMsgArr[TxMsgArrIx].SequenceNumber );
    }

    // We execute CleanUp_After_Msg_Ignored only if we know that both GetIxToAck and GetIxToSend
    // can be updated, which is the case if they points to the same index. This is to prevent
    // GetIxToSend to be increased in a way that makes the diff GetIxToSend - GetIxToAck greater than
    // the size of the sliding windows which will cause a stop in the outgoing communication.
    if (TxMsgArrIx != TxQ[qIx].GetIxToAck)
    {
        if (*pDbg>=3)
        {
            PrintDbg("#   CleanUp_After_Msg_Ignored(%d) ArrIx!=TxQ[qIx].GetIxToAck. Skipping cleanup!\n", qIx);
        }

        return false;  // *** Return ***
    }

    IncreaseIndexToSend(qIx);
    IncreaseIndexToAck(qIx);

    TxQ[qIx].RetryCount = 0;  // is this needed

    // Free buffer

    if(TxQ[qIx].TxMsgArr[TxMsgArrIx].ShallFreeBuffer)
    {
        if(*pDbg>5) PrintDbg("    FreeBuffer\n");
        DoseCom_FreeBuff((char *) TxQ[qIx].TxMsgArr[TxMsgArrIx].pMsgBuff);

        TxQ[qIx].TxMsgArr[TxMsgArrIx].pMsgBuff = NULL;

        TxQ[qIx].TxMsgArr[TxMsgArrIx].ShallFreeBuffer = 0;
    }

    //----------------------------------------------------
    // Check if this is a PD_COMPLETE msg. If so clean up.
    //----------------------------------------------------
    //071008
    if(TxQ[qIx].TxMsgArr[TxMsgArrIx].IsPoolDistr & PD_ISCOMPLETE)
    {
        if (*pDbg)
            PrintDbg("Clearing BitMapBeingPoolDistributed64\n");
        g_pShm->PoolDistributionWillEndSoon  = 0;
        g_pShm->PoolDistributionIsInProgress = 0;
        g_pShm->BitMapBeingPoolDistributed64 = (dcom_ulong64)0; // just in case

        // Normally, this flag (TxSeqNumIsRestarted) is set when PD_COMPLETE
        // is transmitted. Now we must do it here.
#ifdef USE_RESTART_SEQNUM
        int Destination_Id = TxQ[qIx].TxMsgArr[TxMsgArrIx].DestinationId;
        TxQ[qIx].TxSeqNumIsRestarted[Destination_Id] = 1;
        TxQ[qIx].TxSequenceNumber[Destination_Id]    = 0;
#endif
    }

    //----------------------------------------------------------
    // Shall we send a 'QueueOverflow' condition has ended event
    //-----------------------------------------------------------
    if(Atomics::atomic_read32(&TxQ[qIx].TransmitQueueOverflow) > 0)
    {
        if(*pDbg>4)
            PrintDbg("#   Decrementing TransmitQueueOverflow (%d) from %d\n",
                    qIx,Atomics::atomic_read32(&TxQ[qIx].TransmitQueueOverflow));

        Atomics::atomic_dec32(&TxQ[qIx].TransmitQueueOverflow);

        if(Atomics::atomic_read32(&TxQ[qIx].TransmitQueueOverflow) == 0)
        {
            TxQ[qIx].bSendQueueNotOverflow = 1;
        }
    }

    if(*pDbg>5)
    PrintDbg("#   CleanUp done P/Gs/Ga=%d/%d/%d\n",
            TxQ[qIx].PutIx,TxQ[qIx].GetIxToSend,TxQ[qIx].GetIxToAck);

    return true;
}
/*----------------- end CleanUp_After_Msg_Ignored() -------*/

/**************************************************************
* Called from TxThread() - Check_Pending_Ack_Queue()  when:
*
* 1) All ACKs has arrived - we are done with this msg (mode = 1)
* 2)       (mode = 2)
*
* Increases TxQ[qIx].GetIxToAck
*************************************************************/
static int CleanUp_After_Msg_Completed(int qIx)
{
    int GetIxToAck;
    int bIsFound;


    GetIxToAck = TxQ[qIx].GetIxToAck;

    if(*pDbg>=3)
    PrintDbg("#   CleanUp_After_Msg_Completed(%d) P/Gs/Ga=%d/%d/%d SeqN=%u\n",
            qIx, TxQ[qIx].PutIx,TxQ[qIx].GetIxToSend,TxQ[qIx].GetIxToAck,
            TxQ[qIx].TxMsgArr[GetIxToAck].SequenceNumber );

    TxQ[qIx].TxMsgArr[GetIxToAck].IsTransmitting   = 0;
    TxQ[qIx].TxMsgArr[GetIxToAck].TransmitComplete = 0;
    TxQ[qIx].TxMsgArr[GetIxToAck].IsRetransmitting = 0; // MUST be done here
    TxQ[qIx].bIsSendingFragmented = 0;

    //STLRHA: Reset retry count so that we dont continue counting from an old message...
    TxQ[qIx].RetryCount = 0;

    // Free buffer and increment g_Xmit_Get_ix

    if(TxQ[qIx].TxMsgArr[GetIxToAck].ShallFreeBuffer)
    {
        if(*pDbg>5) PrintDbg("    FreeBuffer\n");
        DoseCom_FreeBuff((char *) TxQ[qIx].TxMsgArr[GetIxToAck].pMsgBuff);

        TxQ[qIx].TxMsgArr[GetIxToAck].pMsgBuff = NULL;

        TxQ[qIx].TxMsgArr[GetIxToAck].ShallFreeBuffer = 0;
    }

    // Set next GetIxToAck.
    // Note that the next can be a msg with bUseAck=FALSE
    // If so, do more increase. until we reach GetIxToSend.

    if(TxQ[qIx].GetIxToAck != TxQ[qIx].GetIxToSend)
    {
        IncreaseIndexToAck(qIx);
    }

    if(*pDbg>3) PrintDbg("# * GetIxToAck --> %d (GetIxToSend=%d)\n",
                            TxQ[qIx].GetIxToAck, TxQ[qIx].GetIxToSend);

    //-------------------------------------------------
    // Check if PoolDistributionWillEndSoon flag is set
    // and there are no more PD messages to be sent.
    //-------------------------------------------------

    if(    (g_pShm->PoolDistributionWillEndSoon) // set from appl if pending msg
        && (qIx == g_QueueIndex_CurrPoolDistribution) )
    {
        // Check if there are any pending PD messages on TxQ[qIx]

        int GetIx = TxQ[qIx].GetIxToAck;
        bIsFound = 0;

        while(GetIx != TxQ[qIx].PutIx)
        {
            if(TxQ[qIx].TxMsgArr[GetIx].IsPoolDistr)
            {
                bIsFound = 1;
                break;
            }

            // set next GetIx
            if((GetIx + 1) >= MAX_XMIT_QUEUE)
                GetIx = 0;
            else
                GetIx++;
        }
        //PrintDbg("g_pShm->PoolDistributionWillEndSoon = %d Found = %d\n",
        //                  g_pShm->PoolDistributionWillEndSoon, bIsFound);

        if(!bIsFound)
        {
            if (*pDbg >= 1)
            {
                PrintDbg("Clearing BitMapBeingPoolDistributed64 (just in case)\n");
            }
            g_pShm->PoolDistributionWillEndSoon  = 0;
            g_pShm->PoolDistributionIsInProgress = 0;
            g_pShm->BitMapBeingPoolDistributed64 = (dcom_ulong64)0; // just in case
        }
    }

    //----------------------------------------------------------
    // Shall we send a 'QueueOverflow' condition has ended event
    //-----------------------------------------------------------
    if(Atomics::atomic_read32(&TxQ[qIx].TransmitQueueOverflow) > 0)
    {
        if(*pDbg>4)
            PrintDbg("#   Decrementing TransmitQueueOverflow (%d) from %d\n",
                    qIx,Atomics::atomic_read32(&TxQ[qIx].TransmitQueueOverflow));

        Atomics::atomic_dec32(&TxQ[qIx].TransmitQueueOverflow);

        if(Atomics::atomic_read32(&TxQ[qIx].TransmitQueueOverflow) == 0)
        {
            TxQ[qIx].bSendQueueNotOverflow = 1;
        }
    }

    if(*pDbg>5)
    PrintDbg("#   CleanUp done P/Gs/Ga=%d/%d/%d\n",
            TxQ[qIx].PutIx,TxQ[qIx].GetIxToSend,TxQ[qIx].GetIxToAck);

    return(0);
}
/*----------------- end CleanUp_After_Msg_Completed() -------*/

/**************************************************************************
* Called from TxThread() when a fragment has been acked by all receiving nodes
*
* Return true if message has been completed (all fragments are received), otherwise false.
****************************************************************************/
static bool HandleCompletedFragment(dcom_ushort16 fragmentNum,
                                    int           qIx,
                                    int           txMsgArrIx, 
                                    int           aheadIx)
{
    if(*pDbg>=3)
    {
        PrintDbg("#-  HandleCompletedFragment(qIx=%d) FragNum=%X IsXmit/XmitCompl=%X/%X"
        " Fragm data: Sent/Last/notAck=%X/%X/%X\n",
        qIx,
        fragmentNum,
        TxQ[qIx].TxMsgArr[txMsgArrIx].IsTransmitting,
        TxQ[qIx].TxMsgArr[txMsgArrIx].TransmitComplete,
        TxQ[qIx].TxMsgArr[txMsgArrIx].SentFragment,
        TxQ[qIx].TxMsgArr[txMsgArrIx].LastFragment,
        TxQ[qIx].TxMsgArr[txMsgArrIx].NotAckedFragment);
    }

    bool isMsgCompleted = false;

    TxQ[qIx].RetryCount = 0;

    dcom_ushort16 fragNumWithoutLastBit = fragmentNum & 0x7FFF;

    if (fragNumWithoutLastBit > TxQ[qIx].TxMsgArr[txMsgArrIx].NotAckedFragment)
    {
        // Fragment acked "ahead". Just record this fact and continue.
        TxQ[qIx].TxMsgArr[txMsgArrIx].AckAheadFragments[aheadIx] = fragNumWithoutLastBit;
    }
    else if (fragNumWithoutLastBit == TxQ[qIx].TxMsgArr[txMsgArrIx].NotAckedFragment)
    {
        // It's the expected next ack.

        for (;;)
        {
            if(TxQ[qIx].TxMsgArr[txMsgArrIx].NotAckedFragment == TxQ[qIx].TxMsgArr[txMsgArrIx].LastFragment)
            {
                if(*pDbg>=2)
                    PrintDbg("#-  TxThread[%d] LastFragm=%d TransmitComplete\n",
                    qIx, TxQ[qIx].TxMsgArr[txMsgArrIx].LastFragment);

                g_pShm->Statistics.TotTxCount++;
                g_pTxStatistics[qIx].CountTxWithAckOk++;

                isMsgCompleted = true;

                break;
            }

            // Next fragment.
            ++TxQ[qIx].TxMsgArr[txMsgArrIx].NotAckedFragment;
            // Check if the fragment has already been acked
            bool alreadyAcked = false;
            for (int i=0; i < MAX_AHEAD; ++i)
            {
                if (TxQ[qIx].TxMsgArr[txMsgArrIx].AckAheadFragments[i] == TxQ[qIx].TxMsgArr[txMsgArrIx].NotAckedFragment)
                {
                    // Fragment has already been acked
                    TxQ[qIx].TxMsgArr[txMsgArrIx].AckAheadFragments[i] = 0;
                    alreadyAcked = true;
                    break;
                }
            }
            if (!alreadyAcked)
            {
                break;
            }
        }
    }

    return isMsgCompleted;
}
/*----------------- end HandleCompletedFragment() -------*/

/*******************************************************************
* Called from TxThread() each time it has been waked up by an event.
* Has the Ack_Thread placed any acks on the Ack_Queue[].
* Cases:
* -Ack to a fragmented msg
* -Ack to a non- fragmented msg
* -NAck to a fragmented msg
* -NAck to a non- fragmented msg
*
* Returns:
* Bit 0 set if Tx from Queue 0 completed
* Bit 1 set if Tx from Queue 1 completed
* Bit 2 set if Tx from Queue 2 completed
* Bit 3 set if Tx from Queue 3 completed
*
* Sets TxQ[qIx].TxMsgArr[TxMsgArray_Ix].TransmitComplete if all ACKs
* received for msg or fragment.
*
* Clears bits in TxQ[qIx].TxMsgArr[TxMsgArray_Ix].ExpectedAckBitMap[]
********************************************************************/

static dcom_ulong32 Check_Pending_Ack_Queue(void)
{
    dcom_ulong32   SequenceNum;
    dcom_uchar8   DoseIdFrom;
    dcom_uchar8   qIx;
    dcom_uchar8   TxMsgArr_Ix;
    dcom_ulong32   dwResult = 0;
    dcom_ushort16  FragmentNum;

    if(*pDbg>5)
    {
        PrintDbg("Check_Pending_Ack_Queue() P/gS = %d/%d\n",g_Ack_Put_ix,g_Ack_Get_ix);
    }

    while(g_Ack_Get_ix != g_Ack_Put_ix)
    {
        SequenceNum = g_Ack_Queue[g_Ack_Get_ix].SequenceNumber;
        DoseIdFrom  = g_Ack_Queue[g_Ack_Get_ix].DoseIdFrom;
        qIx         = g_Ack_Queue[g_Ack_Get_ix].TxQueueNumber;
        TxMsgArr_Ix = g_Ack_Queue[g_Ack_Get_ix].TxMsgArray_Ix;
        // Is allways 0 for not fragmented
        FragmentNum = g_Ack_Queue[g_Ack_Get_ix].FragmentNumber;

        if(qIx >= NUM_TX_QUEUES)
        {
            PrintErr(0,"ACK Got invalid TxQueueNumber\n");
            goto Continue_WithNext;
        }

        if(*pDbg>5)
        {
        PrintDbg("#-  Got Ack SeqNum=%d DoseId=%d qIx=%d TxMsgArrIx=%d FragmNum=%X"
            " EXP=%X.%08X\n",
            SequenceNum, DoseIdFrom, qIx, TxMsgArr_Ix, FragmentNum,
            (dcom_ulong32)(TxQ[qIx].TxMsgArr[TxMsgArr_Ix].ExpAckBitMap64[0]>>32),
            (dcom_ulong32)(TxQ[qIx].TxMsgArr[TxMsgArr_Ix].ExpAckBitMap64[0] & 0xFFFFFFFF));
        }

        if(g_Ack_Queue[g_Ack_Get_ix].MsgType == MSG_TYPE_ACK) // the other is _NACK
        {
            //================================================================
            // Ack to a fragmented message
            // - SequenceNumber are the same for all fragments
            // The normal action is to clear bit in ExpAckBitMap64[Ahead_Ix]
            // that cotrresponds to DoseId. Then if all bits are cleared
            // set bit that corresponds to FragmentNumber in TransmitComplete.
            //
            // TO BE IMPLEMENTED ???
            // The receiver never sends an Ack if previous fragment not is acked.
            // This means that if an ack is lost, next indicates that we shall
            // perform actions as if the previous lost ack has been received.
            // The action is: Clear bit in ExpAckBitMap64[Ahead_Ix-1] and set
            // Completed if ExpAckBitMap64[Ahead_Ix-1] becomes zero.
            //=================================================================
            if(FragmentNum != 0) // if a fragmented msg
            {
                dcom_ulong32   Ahead_Ix;
                dcom_uchar8   FragmBit;
                int bThisFragmentIsCompleted;

                if(SequenceNum != TxQ[qIx].TxMsgArr[TxMsgArr_Ix].SequenceNumber) //ERROR
                {
                    if(*pDbg>1)
                        PrintDbg("#-  Got an Ack from DoseId %d with"
                                " Unexpected Sequence Number %d\n",
                                DoseIdFrom, SequenceNum);

                    //g_pTxStatistics[qIx].?????++
                    goto Continue_WithNext;

                    // Must add something here if we allow next msg to
                    // be sent ahead while fragemented is processed
                    // But currently that is not allowed.
                }

                bThisFragmentIsCompleted = 0;

                // Defines bit in IsTransmitting and index in ExpAckBitMap64[] and AckAheadFragments
                Ahead_Ix = FragmentNum & MASK_AHEAD;
                FragmBit =  (dcom_uchar8)(1 << (FragmentNum & 7));

                // Clear bit in Bitmask defining expected ACKs

                TxQ[qIx].TxMsgArr[TxMsgArr_Ix].ExpAckBitMap64[Ahead_Ix]
                                &= ~((dcom_ulong64)1 << DoseIdFrom);

                if(*pDbg>=4)
                PrintDbg("#-  IsTransmitting=%X FragmBit=%X\n",
                        TxQ[qIx].TxMsgArr[TxMsgArr_Ix].IsTransmitting,
                        FragmBit);

                // If Not IsTransmitting

                if((TxQ[qIx].TxMsgArr[TxMsgArr_Ix].IsTransmitting & FragmBit) == 0)
                {
                    if(*pDbg)
                    PrintDbg("#-  ERROR - Received Ack to not Xmitting queue entry."
                            " SeqNo=%d, doseId=%d\n"
                            "    Fragment=%X Retransmit=%d\n",
                            SequenceNum, DoseIdFrom, FragmentNum,
                            g_Ack_Queue[g_Ack_Get_ix].Info);

                    //g_pTxStatistics[qIx].?????++
                    goto Continue_WithNext;
                }

                // If this was the last ack (if all receivers has acked)
                if(TxQ[qIx].TxMsgArr[TxMsgArr_Ix].ExpAckBitMap64[Ahead_Ix]
                        == (dcom_ulong64)0)
                {
                    // This fragment is completed
                    TxQ[qIx].TxMsgArr[TxMsgArr_Ix].TransmitComplete |= FragmBit;
                    TxQ[qIx].TxMsgArr[TxMsgArr_Ix].IsTransmitting   &= ~FragmBit;

                    if(*pDbg>4)
                    PrintDbg("#-  XmitComp=%X (|%X)\n",
                        TxQ[qIx].TxMsgArr[TxMsgArr_Ix].TransmitComplete,FragmBit);

                    dwResult |= (1<<qIx);

                    bThisFragmentIsCompleted = FragmentNum;
                }

                if(!bThisFragmentIsCompleted) goto Continue_WithNext;

                //--------------------------
                // Fragment is completed
                //--------------------------
                dcom_ushort16 fragNumWithoutLastBit = FragmentNum & 0x7FFF;
                if(fragNumWithoutLastBit > TxQ[qIx].TxMsgArr[TxMsgArr_Ix].SentFragment ||
                   fragNumWithoutLastBit < TxQ[qIx].TxMsgArr[TxMsgArr_Ix].NotAckedFragment)
                {
                    if(*pDbg>1)
                        PrintDbg("#-  ******Discarding Erroneous ACK with seqno=%d fragm=%X from DoseId=%d"
                        " Fragm: Sent/Last/notAck=%X/%X/%X\n",
                        SequenceNum,
                        FragmentNum,
                        DoseIdFrom,
                        TxQ[qIx].TxMsgArr[TxMsgArr_Ix].SentFragment,
                        TxQ[qIx].TxMsgArr[TxMsgArr_Ix].LastFragment,
                        TxQ[qIx].TxMsgArr[TxMsgArr_Ix].NotAckedFragment);

                    goto Continue_WithNext;
                }

                bool msgCompleted = HandleCompletedFragment(FragmentNum, qIx, TxMsgArr_Ix, Ahead_Ix);

                if (msgCompleted)
                {
                    CleanUp_After_Msg_Completed(qIx);
                }

                goto Continue_WithNext;
            }   // end fragmented message

            //============================================================
            // Ack to NOT fragmented message
            // - We can be waiting for acks with different SequenceNumber
            //
            // GetIxToSend = Index for next pos to send.
            //               Increased by TxThread when the msg has been sent.
            // GetIxToAck  = Index for next position to be acked.
            //               Increased by TxThread when the msg has been acked.
            //               If no ack requested, increased to next ....
            // This means that valid received TxMsgArr_Ix is in range:
            // GetIxToAck <= Ix < GetIxToSend
            //============================================================
            // TODO handle case with a lost Ack. (Ack SeqNum+1, when SeqNum not is acked)

            int Err = 0;
            if(TxQ[qIx].GetIxToSend >= TxQ[qIx].GetIxToAck)
            {
                if(
                     (TxMsgArr_Ix <  TxQ[qIx].GetIxToAck)
                  || (TxMsgArr_Ix >= TxQ[qIx].GetIxToSend)
                  )
                  Err = 1;
            }
            else  //wrapped
            {
                if(
                     (TxMsgArr_Ix <  TxQ[qIx].GetIxToAck)
                  && (TxMsgArr_Ix >= TxQ[qIx].GetIxToSend)
                  )
                  Err = 1;
            }

            if(Err)
            {
                //?????? add handle retransmit
                if(*pDbg>1)
                PrintDbg("#-  ******Discarding Erroneous ACK with seqno=%d from"
                        " doseId=%d, TxMsgArr_Ix=%d, GetIxSent/Ack=%d/%d, qIx=%d\n",
                        SequenceNum, DoseIdFrom, TxMsgArr_Ix,
                        TxQ[qIx].GetIxToSend, TxQ[qIx].GetIxToAck, qIx);
                goto Continue_WithNext;
            }

            // Is it the expected SequenceNum

            if(SequenceNum == TxQ[qIx].TxMsgArr[TxMsgArr_Ix].SequenceNumber)
            {
                // Clear bit in Bitmask defining expected ACKs
                TxQ[qIx].TxMsgArr[TxMsgArr_Ix].ExpAckBitMap64[0]
                                        &= ~((dcom_ulong64)1 << DoseIdFrom);

                // If the Ack is to another than to GetIxToAck, we must not call

                if(TxQ[qIx].TxMsgArr[TxMsgArr_Ix].IsTransmitting)
                {
                    // If this was the last
                    if(TxQ[qIx].TxMsgArr[TxMsgArr_Ix].ExpAckBitMap64[0]
                                    == (dcom_ulong64)0)
                    {
                        //----------------------------------------------------
                        // This msg is completed
                        // If the Ack is to another than to GetIxToAck, (if
                        // there are different destination channels this is
                        // a normal case), we must not call
                        // CleanUp_After_Msg_Completed() because we might
                        // have to retransmit it.
                        //
                        // Case 1: Acks to same dest chan comes in reverese order
                        //          (could it be msg received in reverse order ????
                        // Case 2:
                        //  a) dest_nodes_N & dest_nodes_N+1) == 0
                        //  b) dest_nodes_N == dest_nodes_N+1) == 0
                        //  c) only some dest_nodes_N == dest_nodes_N+1
                        // TODO: chek if all cases are handled.
                        //----------------------------------------------------

                        TxQ[qIx].TxMsgArr[TxMsgArr_Ix].TransmitComplete |= 1;

                        if(TxMsgArr_Ix != TxQ[qIx].GetIxToAck)
                        {
                            if(*pDbg>1)
                            PrintDbg("#-  ACK ahead. seqno=%d from"
                                " doseId=%d, TxMsgArr_Ix=%d, "
                                "GetIxSent/Ack=%d/%d, qIx=%d\n",
                                SequenceNum, DoseIdFrom, TxMsgArr_Ix,
                                TxQ[qIx].GetIxToSend, TxQ[qIx].GetIxToAck, qIx);

                                goto Continue_WithNext;
                        }

                        // This msg is the expected to be acked

                        if(*pDbg>4)
                            PrintDbg("#-  XmitComp=%X\n",
                                TxQ[qIx].TxMsgArr[TxMsgArr_Ix].TransmitComplete);

                        dwResult |= (1<<qIx);

                        if(*pDbg>=3)
                            PrintDbg("#-   TxThread[%d] Not Fragm"
                                    " TransmitComplete. GetIx=%d\n",
                                    qIx, TxMsgArr_Ix);

                        g_pTxStatistics[qIx].CountTxWithAckOk++;
                        g_pShm->Statistics.TotTxCount++;

                        // This increments TxQ[qIx].GetIxToAck
                        // Note: this clears IsTransmitting, IsRetransmitting
                        CleanUp_After_Msg_Completed(qIx);

                        // Check if there are any completed after this msg
                        // as described above.

                        //if(*pDbg) PrintDbg("Check if any ahead to be cleared. GetIxToSend/Ack=%d/%d\n",
                        //                  TxQ[qIx].GetIxToSend, TxQ[qIx].GetIxToAck );

                        for(int jj=0 ; jj < 4 ; jj++)
                        {
                            if(TxQ[qIx].GetIxToAck == TxQ[qIx].GetIxToSend) break;
                            if(!TxQ[qIx].TxMsgArr[TxQ[qIx].GetIxToAck].TransmitComplete) break;

                            if(*pDbg>2)
                                PrintDbg("Clear msg ahead. GetIxToAck=%d\n",TxQ[qIx].GetIxToAck);

                            CleanUp_After_Msg_Completed(qIx);

                            g_pTxStatistics[qIx].CountTxWithAckOk++;
                            g_pShm->Statistics.TotTxCount++;
                        }
                        //if(*pDbg) PrintDbg("Check done: jj=%d GetIxToSend/Ack=%d/%d\n",
                        //                  jj, TxQ[qIx].GetIxToSend,TxQ[qIx].GetIxToAck );
                    }
                }
                else
                {
                    if(*pDbg>1)
                    PrintDbg("#-  **** Received Ack to not transmitting queue"
                            " entry. SeqNo=%d, doseId=%d Trans=%X\n",
                            SequenceNum,DoseIdFrom,
                            TxQ[qIx].TxMsgArr[TxMsgArr_Ix].IsTransmitting);
                }
                if(*pDbg>3)
                PrintDbg("#-  Ack from %d SeqNum=%d. New Expected=%IX.%08X\n",
                  DoseIdFrom, SequenceNum,
                  (dcom_ulong32)(TxQ[qIx].TxMsgArr[TxMsgArr_Ix].ExpAckBitMap64[0]>>32),
                  (dcom_ulong32)(TxQ[qIx].TxMsgArr[TxMsgArr_Ix].ExpAckBitMap64[0] & 0xFFFFFFFF));
            }
            else
            {
                if(*pDbg)
                    PrintDbg("#-  Got an Ack with Unexpected Seq Number %d\n",
                                SequenceNum);
            }
            goto Continue_WithNext;
            // --- end NOT fragmented message ---
        } // end ACK
        else
        if(g_Ack_Queue[g_Ack_Get_ix].MsgType == MSG_TYPE_NACK)
        {
            //===============================================================
            // NAck to a fragmented message
            //
            // NACK is sent if the receiver has lost a fragment and receives
            // a msg with  FragmentNum = Expected FragmentNumber +1, +2, +3
            // The action is to retransmit everything starting with first not
            // acked by this receiver.
            //
            // .Info holds the expected FragmentNumber
            // Action is implemented by forcing a timeout by modifying
            // StartSendTime. The result is as for a timeout but much faster.
            //================================================================
            if(FragmentNum != 0) // if a fragmented msg
            {
                // Check if this node considers the nacked fragment to have been
                // sent but not acked

                bool expectedSeqNbrIsWithinWindow = false;
                dcom_ushort16 ixToAck = TxQ[qIx].GetIxToAck;
                for(;;)
                {
                    if ((ixToAck == TxQ[qIx].GetIxToSend &&
                        TxQ[qIx].TxMsgArr[ixToAck].bIsFragmented &&
                        TxQ[qIx].TxMsgArr[ixToAck].IsTransmitting) ||
                        ixToAck != TxQ[qIx].GetIxToSendHighWatermark)
                    {
                        if (TxQ[qIx].TxMsgArr[ixToAck].SequenceNumber == g_Ack_Queue[g_Ack_Get_ix].SequenceNumber)
                        {
                            expectedSeqNbrIsWithinWindow = true;
                            break;
                        }
                    }

                    // Don't step beyond what we have actually sent
                    if (ixToAck == TxQ[qIx].GetIxToSendHighWatermark)
                    {
                        break;
                    }

                    if((ixToAck + 1) >= MAX_XMIT_QUEUE)
                    {
                        ixToAck = 0;
                    }
                    else
                    {
                        ++ixToAck;
                    }
                }

                if (expectedSeqNbrIsWithinWindow)
                {
                    if(!TxQ[qIx].TxMsgArr[TxMsgArr_Ix].IsRetransmitting)
                    {
                        TxQ[qIx].StartSendTime = DoseOs::Get_TickCount() - 500; //force timeout
                        if(*pDbg>2)
                            PrintDbg("Force Timeout. FragmentNum=%X\n", FragmentNum);
                    }
                }
                else
                {
                    // We got a NACK for a fragmented message but the sequence number is not within our sliding window.
                    // This is an "impossible" case probably caused by a bug. The solution for now is to simulate that this
                    // node loses contact with all other nodes, in order to force a resynchronization.

                    PrintErr(0, "TxThread[%d] Got a NACK for a fragmented message that is already acked! Simulating a node disconnect in order to force resynchronization\n", qIx);

                    g_pShm->InhibitOutgoingTraffic = true;
                    DoseOs::Sleep(6000);
                    CNodeStatus::CheckTimedOutNodes(true); // make this node treat all other nodes as down
                    g_pShm->InhibitOutgoingTraffic = false;
                }

                goto Continue_WithNext;
            }
            //===============================================================
            // NAck to a NON-fragmented message
            // -
            // TO BE IMPLEMENTED ???
            // NACK is sent if the receiver has lost a message and receives
            // a msg with SequenceNumber = Expected SequenceNumber +1, +2, +3
            // The action is to retransmit everything starting with first not
            // acked by this receiver.
            // If not implemented, this will be handled by the timeout function.
            //===============================================================
            else
            {
                if(*pDbg>1)
                PrintDbg("#-  IS IMPLEMENTED Got an Nack Seq=%d Info=%d\n",
                        SequenceNum, g_Ack_Queue[g_Ack_Get_ix].Info );

                // Check if this node considers the nacked message to have been
                // sent but not acked

                bool expectedSeqNbrIsWithinWindow = false;
                dcom_ushort16 ixToAck = TxQ[qIx].GetIxToAck;
                while (ixToAck != TxQ[qIx].GetIxToSendHighWatermark)
                {
                    if (TxQ[qIx].TxMsgArr[ixToAck].SequenceNumber == g_Ack_Queue[g_Ack_Get_ix].Info)
                    {
                        expectedSeqNbrIsWithinWindow = true;
                        break;
                    }
                    if((ixToAck + 1) >= MAX_XMIT_QUEUE)
                    {
                        ixToAck = 0;
                    }
                    else
                    {
                        ++ixToAck;
                    }
                }

                if (expectedSeqNbrIsWithinWindow)
                {
                    if(!TxQ[qIx].TxMsgArr[TxMsgArr_Ix].IsRetransmitting)
                    {
                        TxQ[qIx].StartSendTime = DoseOs::Get_TickCount() - 500; //force timeout
                        if(*pDbg>1)
                            PrintDbg("Force Timeout. FragmentNum=%X\n", FragmentNum);
                    }
                }
                else
                {
                    // We got a NACK for a message but the message that are expected by the receiver is not within our
                    // message sliding window.
                    // This is an "impossible" case probably caused by a bug. The solution for now is to simulate that this
                    // node loses contact with all other nodes, in order to force a resynchronization.

                    PrintErr(0, "TxThread[%d] Got a NACK with an expected message that is already acked! Simulating a node disconnect in order to force resynchronization\n", qIx);

                    g_pShm->InhibitOutgoingTraffic = true;
                    DoseOs::Sleep(6000);
                    CNodeStatus::CheckTimedOutNodes(true); // make this node treat all other nodes as down
                    g_pShm->InhibitOutgoingTraffic = false;                
                }

                goto Continue_WithNext;
            }
        } // end NACK
        else
        {
            if(*pDbg)
            PrintDbg("#-  ERROR Check_Pending_Ack_Queue() invalid Type = %X\n",
                g_Ack_Queue[g_Ack_Get_ix].MsgType);
        }

        //-----------------------------------
        // Continue with next next Ack_Get_Ix
        //-----------------------------------

Continue_WithNext:
        if((g_Ack_Get_ix + 1) >= MAX_ACK_QUEUE) g_Ack_Get_ix = 0;
        else g_Ack_Get_ix++;
    } // end while(g_Ack_Get_ix != g_Ack_Put_ix)


    //Make a check if the GetIxToAck is a message that already is completed. In that case call CleanUp_After_Msg_Completed.
    //A state like that should never occur and when it does it is due to bugs in dose_com.
    for (int q=0; q<NUM_TX_QUEUES; ++q)
    {
        int ackIx=TxQ[q].GetIxToAck;
        if (ackIx!=TxQ[q].GetIxToSend &&
            TxQ[q].TxMsgArr[ackIx].IsTransmitting==0 &&
            TxQ[q].TxMsgArr[ackIx].IsRetransmitting==0 &&
            TxQ[q].TxMsgArr[ackIx].TransmitComplete>0)
        {
            PrintDbg("Check_Pending_Ack_Queue found deadlock state in TxQ[%d] P/S/A=%d/%d/%d. IsTransmitting=%d, IsRetransmitting=%d, TransmitComplete=%d. Calling CleanUp_After_Msg_Completed to recover from this state.\n",
                 q, TxQ[q].PutIx, TxQ[q].GetIxToSend, TxQ[q].GetIxToAck, TxQ[q].TxMsgArr[ackIx].IsTransmitting, TxQ[q].TxMsgArr[ackIx].IsRetransmitting, TxQ[q].TxMsgArr[ackIx].TransmitComplete);
            CleanUp_After_Msg_Completed(q);
        }
    }

    return(dwResult);
}
/*--------------- end Check_Pending_Ack_Queue() ------------------*/

/*********************************************************ok
* Called from TxThread() when ...
*
* Returns 0-63 if one and only one bit is set in BitMap
**********************************************************/
static dcom_ulong32 Check_IfUniCast_CanBeUsed(dcom_ulong64 BitMap)
{
    dcom_ulong32   DestId = 999;
    int     jj;
    int     Count = 0;
    dcom_uchar8   Max = g_pShm->MaxUsedDoseId;


    for(jj=0 ; jj<=Max ; jj += 8, BitMap = BitMap>>8)
    {
        if((BitMap & 0xFF) == 0) continue;

        switch(BitMap & 0xFF)
        {
            case 0x1:  DestId = 0 + jj; if(++Count > 1) return(999); break;
            case 0x2:  DestId = 1 + jj; if(++Count > 1) return(999); break;
            case 0x4:  DestId = 2 + jj; if(++Count > 1) return(999); break;
            case 0x8:  DestId = 3 + jj; if(++Count > 1) return(999); break;
            case 0x10: DestId = 4 + jj; if(++Count > 1) return(999); break;
            case 0x20: DestId = 5 + jj; if(++Count > 1) return(999); break;
            case 0x40: DestId = 6 + jj; if(++Count > 1) return(999); break;
            case 0x80: DestId = 7 + jj; if(++Count > 1) return(999); break;
            default: return(999);
        }
    }
    return(DestId);
}

/*************************************************************
* Called from TxThread() when ...
*
*************************************************************/

int Build_Tx_Message(int qIx, int GetIx, int DestinationId,
                     char **ppData, DOSE_UDP_MSG_HDR *pTxMsgHdr)
{
    int ix;
    int Ahead_Ix = 0;


    //PrintDbg("    Build_Tx_Message() IsFragmented=%d SentFragment=%X GetIx=%d\n",
    //          TxQ[qIx].TxMsgArr[GetIx].bIsFragmented,
    //          TxQ[qIx].TxMsgArr[GetIx].SentFragment, GetIx);

    if(TxQ[qIx].TxMsgArr[GetIx].bIsFragmented
                && (TxQ[qIx].TxMsgArr[GetIx].SentFragment > 1))
    {
        Ahead_Ix = TxQ[qIx].TxMsgArr[GetIx].SentFragment & MASK_AHEAD;

        // get index for next to send ( = number of sent)

        ix = (TxQ[qIx].TxMsgArr[GetIx].SentFragment-1)
                            * (FRAGMENT_SIZE - SIZEOF_UDP_MSG_HDR);

        pTxMsgHdr->FragmentNumber = TxQ[qIx].TxMsgArr[GetIx].SentFragment;

        pTxMsgHdr->TotalSize   = TxQ[qIx].TxMsgArr[GetIx].UsrMsgLength;

        // Is it last fragment
        if(TxQ[qIx].TxMsgArr[GetIx].SentFragment
                        == TxQ[qIx].TxMsgArr[GetIx].LastFragment)
        {
            pTxMsgHdr->Size = (dcom_ushort16)(TxQ[qIx].TxMsgArr[GetIx].UsrMsgLength-ix);
            pTxMsgHdr->FragmentNumber |= 0x8000; //indicates last
        }
        else
        {
            pTxMsgHdr->Size = (dcom_ushort16)(FRAGMENT_SIZE - SIZEOF_UDP_MSG_HDR);
        }

        *ppData = (char *) &TxQ[qIx].TxMsgArr[GetIx].pMsgBuff[ix];
    }
    else
    if(TxQ[qIx].TxMsgArr[GetIx].bIsFragmented)
    {
        Ahead_Ix = TxQ[qIx].TxMsgArr[GetIx].SentFragment & MASK_AHEAD;
        *ppData = (char*)TxQ[qIx].TxMsgArr[GetIx].pMsgBuff;

        pTxMsgHdr->FragmentNumber = 1; // first fragmented segment
        pTxMsgHdr->Size      = (dcom_ushort16)(FRAGMENT_SIZE - SIZEOF_UDP_MSG_HDR);
        pTxMsgHdr->TotalSize = TxQ[qIx].TxMsgArr[GetIx].UsrMsgLength;
    }
    else
    {
        *ppData = (char*)TxQ[qIx].TxMsgArr[GetIx].pMsgBuff;

        pTxMsgHdr->FragmentNumber = 0; // not fragmented
        pTxMsgHdr->Size      = (dcom_ushort16) TxQ[qIx].TxMsgArr[GetIx].UsrMsgLength;
        pTxMsgHdr->TotalSize = pTxMsgHdr->Size;
    }

    pTxMsgHdr->Info = (dcom_uchar8) TxQ[qIx].TxMsgArr[GetIx].IsRetransmitting;

    pTxMsgHdr->IsPoolDistribution = TxQ[qIx].TxMsgArr[GetIx].IsPoolDistr;

    pTxMsgHdr->SequenceNumber = TxQ[qIx].TxMsgArr[GetIx].SequenceNumber;

    // When a PD_COMPLETE msg is built, TxSeqNumIsRestarted is set and SequenceNumber=0.
    // This is because only some nodes has received the PD (and updated SeqNum)
    // The Receiver synchronizes on condition: (.info & 0x80) && (SeqNum == 0)
    // Here we set a bit in .info if this flag is set

#ifdef USE_RESTART_SEQNUM
    ... to show it is not defined ...
    if(pTxMsgHdr->SequenceNumber == 0)
    {
        if(TxQ[qIx].TxSeqNumIsRestarted[DestinationId])
        {
            // Can't clear TxSeqNumIsRestarted here because of possible retransmit
            // TxQ[qIx].TxSeqNumIsRestarted[DestinationId] = 0;
            TxQ[qIx].TxMsgArr[GetIx].SeqNumIsRestarted = 0x80; //used for retransmit

            if(pTxMsgHdr->FragmentNumber < 2) // non-fragm + first fragm
                pTxMsgHdr->Info  |= 0x80;
        }
    }
    else
    {
        if  (
                !TxQ[qIx].TxMsgArr[GetIx].IsRetransmitting
                &&
                !TxQ[qIx].TxMsgArr[GetIx].IsPoolDistr  // PD has just set the flag
            )
        {
            TxQ[qIx].TxMsgArr[GetIx].SeqNumIsRestarted = 0;
            TxQ[qIx].TxSeqNumIsRestarted[DestinationId] = 0;
        }
    }
#endif

    pTxMsgHdr->DoseIdBitMap[0]
                = (dcom_ulong32)(TxQ[qIx].TxMsgArr[GetIx].ExpAckBitMap64[Ahead_Ix]
                                                            & (dcom_ulong64) 0xFFFFFFFF);
    pTxMsgHdr->DoseIdBitMap[1]
                = (dcom_ulong32)(TxQ[qIx].TxMsgArr[GetIx].ExpAckBitMap64[Ahead_Ix]>>32);

    //remove nodes that are down
    TxQ[qIx].TxMsgArr[GetIx].ExpAckBitMap64[Ahead_Ix] &= ~g_pShm->BitMapNodesDown64;

    pTxMsgHdr->TxMsgArray_Ix    = (dcom_uchar8)GetIx; //phase2
    pTxMsgHdr->bWantAck         = TxQ[qIx].TxMsgArr[GetIx].bUseAck;
    pTxMsgHdr->TxQueueNumber    = (dcom_uchar8)qIx;

    pTxMsgHdr->DestinationId = (dcom_uchar8) DestinationId;

    if(*pDbg>=3)
    PrintDbg("### Build_Tx_Msg(%d) "
            "Get/PutIx=%d/%d SeqN=%u FragN=%X DstId=%X Inf=%X ExpAck=%X.%08X\n",
            qIx, GetIx, TxQ[qIx].PutIx,
            pTxMsgHdr->SequenceNumber,pTxMsgHdr->FragmentNumber, DestinationId,
            pTxMsgHdr->Info,
            (dcom_ulong32)(TxQ[qIx].TxMsgArr[GetIx].ExpAckBitMap64[Ahead_Ix]>>32),
            (dcom_ulong32)(TxQ[qIx].TxMsgArr[GetIx].ExpAckBitMap64[Ahead_Ix]
                                                            & 0xFFFFFFFF));
    return(1);
}
/*----------------- end Build_Tx_Msg() --------------------*/

/************************************************************************
* Called from TxThread() when we have waited for an Ack more than ?? sec
*
* It has also been checked that the node not has been reported as 'Down'
* by the KeepAlive thread..
*
* Decide if we shall retransmit the message or give up.
*
* Returns:
* 'R' - The message should be retransmitted
* 'G' - Give up - timeout expired
* 'N' - All nodes down - No more nodes to send to - go on with next msg
*************************************************************************/
static int Handle_Timeout(int qIx)
{
    int GetIxToAck = TxQ[qIx].GetIxToAck;
    int Ahead_Ix = 0;

    //------------------------------------------------------------------------
    // Check if all the node has been reported as 'Down' by
    // the KeepAlive thread. (It takes a few seconds to detect this).
    // If so, the corresponding bit in g_pShm->BitMapNodesUp64 has been cleared.
    // If TRUE return 'N'
    //-------------------------------------------------------------------------
    if(TxQ[qIx].TxMsgArr[GetIxToAck].bIsFragmented) //????? nytt 070703????
        //Ahead_Ix = TxQ[qIx].TxMsgArr[GetIxToAck].SentFragment & MASK_AHEAD;
        Ahead_Ix = TxQ[qIx].TxMsgArr[GetIxToAck].NotAckedFragment & MASK_AHEAD;

    // If KeepAlive detects a node beeing PD goes down, the corresonding bit
    // in g_pShm->BitMapNodesNew/Up64 is cleared.
    // A target might be changed from new to up in the middle of the PD.
    // since new->up id when the target sends PD-complete.

     if(TxQ[qIx].TxMsgArr[GetIxToAck].IsPoolDistr)
        TxQ[qIx].TxMsgArr[GetIxToAck].ExpAckBitMap64[Ahead_Ix]
                        &= (g_pShm->BitMapNodesNew64 | g_pShm->BitMapNodesUp64);
    else
        TxQ[qIx].TxMsgArr[GetIxToAck].ExpAckBitMap64[Ahead_Ix]
                        &= (g_pShm->BitMapNodesNew64 | g_pShm->BitMapNodesUp64);

    // If no more nodes, the msg or fragment is completed
    if (TxQ[qIx].TxMsgArr[GetIxToAck].ExpAckBitMap64[Ahead_Ix] == (dcom_ulong64) 0)
    {
        if(*pDbg)

        PrintDbg("    No more nodes. Ahead_Ix=%d P/S/A=%d/%d/%d  SeqNtoAck=%u"
            " Fr-S/nA=%X/%X BitMapExp/Up=%X.%08X %X.%08X\n",
            Ahead_Ix, TxQ[qIx].PutIx,TxQ[qIx].GetIxToSend,
            TxQ[qIx].GetIxToAck,
            TxQ[qIx].TxMsgArr[GetIxToAck].SequenceNumber,
            TxQ[qIx].TxMsgArr[GetIxToAck].SentFragment,
            TxQ[qIx].TxMsgArr[GetIxToAck].NotAckedFragment,
            (dcom_ulong32)(TxQ[qIx].TxMsgArr[GetIxToAck].ExpAckBitMap64[Ahead_Ix]>>32),
            (dcom_ulong32)(TxQ[qIx].TxMsgArr[GetIxToAck].ExpAckBitMap64[Ahead_Ix]
                                                                & 0xFFFFFFFF),
            (dcom_ulong32)(g_pShm->BitMapNodesUp64>>32),
            (dcom_ulong32)(g_pShm->BitMapNodesUp64 & 0xFFFFFFFF));

        return('N');
    }

    //--------------------------------------------------------------------
    // Check if have retried a far to long time.
    // The normal cause that a node does not Ack, is that the node is down.
    // The 'is down' case is handled by the test above.
    //
    // This test is to handle the case where the receivers KeepAliveThread'
    // functions OK, but no ACKs are sent. It could be a heavyly loaded
    // application that does not consume any messages.
    // I guess it must be some kind of software or configuration error.
    //
    // The condition to give up is:
    // RetryCount > MAX_XMIT_RETRIES
    // AND
    // Time we have waited > GIVEUP_TIMEOUT
    //
    // If we are dealing with a fragmented msg, and one node does not ACK,
    // we still must continue sending to the other nodes.
    //---------------------------------------------------------------------

#ifdef GIVEUP_AFTER_RETRIES
    if  (
            (TxQ[qIx].RetryCount > MAX_XMIT_RETRIES)
         && ((GetTimeTick() - TxQ[qIx].StartSendTime) > GIVEUP_TIMEOUT)
        )
        return('G');
#endif

    //---------------------------------------------------
    // Timeout when waiting for ACK - we must retry.
    // Retry sending
    //----------------------------------------------------

    // The missing node is still in state 'Up'

    if(*pDbg>2)
    PrintDbg("#   TxThread[%d] ACK Timeout."
            "Retry send (%d) Seq=%u FragmS/nA=%X/%X GetIxToAck=%d\n",
            qIx, TxQ[qIx].RetryCount,
            TxQ[qIx].TxMsgArr[GetIxToAck].SequenceNumber,
            TxQ[qIx].TxMsgArr[GetIxToAck].SentFragment,
            TxQ[qIx].TxMsgArr[GetIxToAck].NotAckedFragment,
            GetIxToAck);

    TxQ[qIx].RetryCount++;

    g_pTxStatistics[qIx].CountTxRetransmit++;
    g_pShm->Statistics.ReTxCount++;

    // take a step back to the 'missing ack fragment'.
    if(TxQ[qIx].TxMsgArr[GetIxToAck].bIsFragmented)
    {
        if(TxQ[qIx].TxMsgArr[GetIxToAck].IsRetransmitting) //100)
            TxQ[qIx].TxMsgArr[GetIxToAck].IsRetransmitting++;
        else
        // Does not work for retransmit when retransmit
        TxQ[qIx].TxMsgArr[GetIxToAck].IsRetransmitting
            = (dcom_uchar8)(1 + TxQ[qIx].TxMsgArr[GetIxToAck].SentFragment
                        - TxQ[qIx].TxMsgArr[GetIxToAck].NotAckedFragment);

        // Must be like this since it is increased later
        TxQ[qIx].TxMsgArr[GetIxToAck].SentFragment
                = (dcom_ushort16)(TxQ[qIx].TxMsgArr[GetIxToAck].NotAckedFragment - 1);


        int Ahead_Ix = TxQ[qIx].TxMsgArr[GetIxToAck].NotAckedFragment & MASK_AHEAD;

        TxQ[qIx].TxMsgArr[GetIxToAck].ExpAckBitMap64[Ahead_Ix]
                                = TxQ[qIx].CurrFragmentedMsgAckBitMap64;

        // test - is this needed ?
        TxQ[qIx].TxMsgArr[GetIxToAck].ExpAckBitMap64[0]
                                = TxQ[qIx].CurrFragmentedMsgAckBitMap64;
        TxQ[qIx].TxMsgArr[GetIxToAck].ExpAckBitMap64[1]
                                = TxQ[qIx].CurrFragmentedMsgAckBitMap64;
        TxQ[qIx].TxMsgArr[GetIxToAck].ExpAckBitMap64[2]
                                = TxQ[qIx].CurrFragmentedMsgAckBitMap64;
        TxQ[qIx].TxMsgArr[GetIxToAck].ExpAckBitMap64[3]
                                = TxQ[qIx].CurrFragmentedMsgAckBitMap64;
        TxQ[qIx].TxMsgArr[GetIxToAck].ExpAckBitMap64[4]
                                = TxQ[qIx].CurrFragmentedMsgAckBitMap64;
        TxQ[qIx].TxMsgArr[GetIxToAck].ExpAckBitMap64[5]
                                = TxQ[qIx].CurrFragmentedMsgAckBitMap64;
        TxQ[qIx].TxMsgArr[GetIxToAck].ExpAckBitMap64[6]
                                = TxQ[qIx].CurrFragmentedMsgAckBitMap64;
        TxQ[qIx].TxMsgArr[GetIxToAck].ExpAckBitMap64[7]
                                = TxQ[qIx].CurrFragmentedMsgAckBitMap64;

        TxQ[qIx].TxMsgArr[GetIxToAck].IsTransmitting   = 0x8000;
        TxQ[qIx].TxMsgArr[GetIxToAck].TransmitComplete = 0;

    }
    else // not fragmented
    {
        // Set  GetIxToSend = GetIxToAck
        // Set Is Retransmiting in all sent from GetIxToAck to GetIxToSend
        if(*pDbg>2)
            PrintDbg("Timeout GetIxToSend %d --> %d\n",
                    TxQ[qIx].GetIxToSend, TxQ[qIx].GetIxToAck);

        int ix = GetIxToAck;
        for( ; ; )
        {
            if(ix == TxQ[qIx].GetIxToSend) break;

            // Not OK ???? - must use bitmap ???
            if(TxQ[qIx].TxMsgArr[ix].IsRetransmitting < 15) //100)
                TxQ[qIx].TxMsgArr[ix].IsRetransmitting++; // new 06-03-08

            TxQ[qIx].TxMsgArr[ix].IsTransmitting   = 0; // nytt 070721
            TxQ[qIx].TxMsgArr[ix].TransmitComplete = 0; // nytt 070721
            ix++;
            if(ix >= MAX_XMIT_QUEUE) ix = 0;
        }

        TxQ[qIx].GetIxToSend = TxQ[qIx].GetIxToAck;
    }
    return('R'); // ==> goto Send_The_Message;
}
/*-------------------- end Handle_Timeout() -------------*/

/***********************************************************
* This thread takes messages from a queue and transmits them
*
************************************************************/

static THREAD_API TxThread(void *)
{
    int     qIx;
    int     result;
    int     Ahead_Ix;
    dcom_uchar8   FragmBit;
    int     Destination_Id;
    int     SendTo_DestinationId;
    dcom_ulong32   CurrentTime;
    int     UseToSendIx;
    dcom_ulong32   WaitTimeOut = 1000;
    dcom_ulong32   TxMsgSize = 0;
    dcom_ulong64 MyBitMapInv64;
    bool    bThereMightBeMore = FALSE;
    char    *pData;
    int     bWaitingForAck;
    int     GetIxToAck;
    CIpmSocket TxSock;
    DOSE_UDP_MSG_HDR TxMsgHdr;

    // Init constant part of message

    TxMsgHdr.DoseIdFrom = (dcom_uchar8) g_MyDoseId;
    TxMsgHdr.IpAddrFrom_nw = CConfig::m_MyIpAddr_nw;
    TxMsgHdr.Magic      = DOSE_MSG_MAGIC;
    TxMsgHdr.MsgType    = MSG_TYPE_DATA;

    if(*pDbg>1)
        PrintDbg("### TxThread() Starts. DoseId =%d IpMultcastAddr=%s\n",
                g_MyDoseId,
                DoseOs::Inet_Ntoa(CConfig::m_BaseIpMultiCastAddr_nw));

    // Initialize socket
    result = TxSock.CreateIpMulticastSocket(
                        0,1,    // not Rx, but Tx
                        CConfig::m_BaseIpMultiCastAddr_nw,
                        CConfig::m_MulticastTtl,
                        0,      // m_Dose_Port_Ack_nw,
                        0,      // Opt_so_rcvbuf_size,
                        0);     // Opt_So_Rcvbuf_Timeout,

    if(result == -1)
    {
        PrintErr(0,
            "TxThread() Create IpMulticast Socket failed. NOT Terminating.\n");
        //return; ???
    }

    // Need this for clearing bit for myself
    MyBitMapInv64 = (dcom_ulong64)1 << g_MyDoseId;
    MyBitMapInv64 = ~MyBitMapInv64;  // invert

    if(*pDbg>=3)
        PrintDbg("### TxThread() MyBitMapInv=%X.%08X\n",
            (dcom_ulong32)(MyBitMapInv64>>32),(dcom_ulong32)(MyBitMapInv64 & 0xFFFFFFFF));

    //===============
    // Loop for ever
    //===============
    for(;;) // ForEver
    {
        //===========================================================
        // Wait for an Event - Can be waked up by:
        // 1) The Ack_Thread has received an ACK and put on Ack_Queue
        // 2) A new msg has been put on the Transmit_Queue
        // 3) Timeout
        //===========================================================
        if(!bThereMightBeMore)
        {
            if(*pDbg > 3)
                PrintDbg("##  TxThread() - Wait % d ms\n", WaitTimeOut);

            g_hTxEvent->WaitFor(WaitTimeOut);

            // Set default WaitTimeOut for next lap.
            // It might be changed to WAITTIME_WAITING_FOR_ACK if
            // we are waiting for an Ack on any TxQ[].

            WaitTimeOut = WAITTIME_WAITING_NOT;
        }
        else
        {
            if(*pDbg > 5) PrintDbg("### TxThread() There might be more\n");
            bThereMightBeMore = FALSE;
        }

        //===========================================================
        // Scan all,Queues
        // qIx is incremented when there is nothing more on the Queue
        //===========================================================
        for(int jj=0 ; jj < NUM_TX_QUEUES ; jj++) // Clears counters
        {
            TxQ[jj].LapCount = 0;
            TxQ[jj].bSendQueueNotOverflow = 0;
        }

        qIx = 0;
        while(qIx < NUM_TX_QUEUES)
        {
            ACE_Guard<ACE_Thread_Mutex> lck(g_threadLock);

            // There could be the following jobs:
            // 1) Timeout when waiting for Ack
            // 2) The AckThread has received an Ack
            //      Check this with routine Check_Pending_Ack_Queue();
            // 3) There are messages to be sent
            //      TxQ[qIx].GetIxToAck == TxQ[qIx].PutIx (if not to much ahead)
            //
            // TODO: A quick test to findout if there are any pending jobs.

            if (TxQ[qIx].GetIxToAck == TxQ[qIx].PutIx) // nothing to do
            {
                qIx++;
                continue; // go on with next Queue
            }

            if(*pDbg > 3)
                PrintDbg("##  TxThread[%d] next Queue Ix:P/S/nA = %u/%u/%u #######\n",
                    qIx, TxQ[qIx].PutIx,TxQ[qIx].GetIxToSend,TxQ[qIx].GetIxToAck);

            CurrentTime = DoseOs::Get_TickCount();

            bWaitingForAck    = FALSE;

            //------------------------------------------------------------
            // Are there any pending ACKs on the Ack_Queue.
            // Check_Pending_Ack_Queue() handles all completed messages
            // for all Queues.
            // Sets TxQ[qIx].TxMsgArr[GetIxToAck].TransmitComplete if all
            // ACKs received for msg or fragment.
            // Clears bits in TxQ[qIx].TxMsgArr[GetIxToAck].ExpectedAckBitMap[]
            // Might increase .GetIxToAck, if the msg is cmpleted
            // Returns != 0 if any completed messages (on any queue)
            //------------------------------------------------------------

            Check_Pending_Ack_Queue();

            //=========================================================
            // If we are transmitting, but is not completed.
            // If only a short time has expired, this is a normal case
            // and we shall continue waiting.
            // If timeout, ?????????
            // If waiting for more ACKs
            // if(ExpectedAckNodesBitMap)
            //=========================================================

            // If only bit 15 is set, all sent fragments are acked, but there are more to send.
            if(TxQ[qIx].TxMsgArr[TxQ[qIx].GetIxToAck].IsTransmitting & 0x7FFF)
            {
                GetIxToAck = TxQ[qIx].GetIxToAck;

                // The alternative for not-acked fragmented mentioned at the end of this routine
                // if(TxQ[qIx].TxMsgArr[UseToSendIx].bIsFragmented & 0x80)
                //     goto Continue_Fragmented_Msg;

                //if(*pDbg>=4)
                //PrintDbg("#   TxThread[%d] IsTransmitting T: %u-%u=%u\n",
                //      qIx, CurrentTime, TxQ[qIx].StartSendTime,
                //      CurrentTime - TxQ[qIx].StartSendTime);

                //----------------------------------------------------
                // Has time waiting for Ack expired
                // RetryCount is increased by one for each retransmit.
                // Times are in millisec.
                //----------------------------------------------------
                dcom_ulong32 MaxTimeToWaitForAck =
                            TIMEOUT_WAITING_FOR_ACK * (1+TxQ[qIx].RetryCount);

                if(MaxTimeToWaitForAck > 500) MaxTimeToWaitForAck = 500;

                if((CurrentTime - TxQ[qIx].StartSendTime) < MaxTimeToWaitForAck)
                {
                    // No timeout
                    WaitTimeOut = WAITTIME_WAITING_FOR_ACK;
                    bWaitingForAck = TRUE; //???? is this needed ???
                    // go on checking if allowed to send ahead
                }
                else // Yes - Timeout
                {
                    if(*pDbg>1)
                        PrintDbg("Call HandleTimeout() CurrentTime=%u StartTime=%u Diff=%d Rc=%d\n",
                                CurrentTime, TxQ[qIx].StartSendTime,
                                CurrentTime - TxQ[qIx].StartSendTime,
                                TxQ[qIx].RetryCount);

                    // The message has timed out
                    result = Handle_Timeout(qIx);

                    if(result == 'R') // Resend msg
                    {
                        // Improvement: We could resend to the nodes that not has acked only.

                        if(*pDbg>1)
                        PrintDbg("    Timeout - should resend "
                                    " T=%d/%d GetIxS/A=%X/%X\n",
                                    CurrentTime,
                                    CurrentTime - TxQ[qIx].StartSendTime,
                                    TxQ[qIx].GetIxToSend, TxQ[qIx].GetIxToAck);

                        if(TxQ[qIx].TxMsgArr[GetIxToAck].bIsFragmented) // Is fragmented
                        {
                            goto Continue_Fragmented_Msg;
                        }
                        // Not fragmented
                        Destination_Id = TxQ[qIx].TxMsgArr[GetIxToAck].DestinationId;
                        UseToSendIx = TxQ[qIx].GetIxToSend;
                        Ahead_Ix = 0;
                        TxQ[qIx].TxMsgArr[UseToSendIx].IsTransmitting = 1;

                        goto Send_The_Message;
                    }
                    else
                    if(result == 'N') // Last destination Node down - no more nodes to send to
                    {
                        if(TxQ[qIx].TxMsgArr[GetIxToAck].bIsFragmented) // Is fragmented
                        {
                            UseToSendIx = TxQ[qIx].GetIxToAck;
                            Ahead_Ix = TxQ[qIx].TxMsgArr[UseToSendIx].SentFragment & MASK_AHEAD;
                        }
                        else
                        {
                            UseToSendIx = TxQ[qIx].GetIxToSend;
                            Ahead_Ix = 0;
                        }

                        if (TxQ[qIx].TxMsgArr[UseToSendIx].bIsFragmented)
                        {
                            dcom_ushort16 FragNum = TxQ[qIx].TxMsgArr[UseToSendIx].NotAckedFragment;
                            dcom_ushort16 FragBit = (dcom_uchar8)(1<<(FragNum & 7));
                            dcom_ulong32  Ahead_Ix = FragNum & MASK_AHEAD;

                            //PrintDbg("NYTT FragNum=%X\n", FragNum);

                            // This fragment is completed
                            TxQ[qIx].TxMsgArr[UseToSendIx].TransmitComplete |= FragBit;
                            TxQ[qIx].TxMsgArr[UseToSendIx].IsTransmitting   &= ~FragBit;

                            bool msgCompleted = HandleCompletedFragment(FragNum, qIx, UseToSendIx, Ahead_Ix);

                            if (msgCompleted)
                            {
                                // See comment "If last fragment has been sent ...."  30 lines below".
                                if(TxQ[qIx].GetIxToSend == TxQ[qIx].GetIxToAck)
                                {
                                    IncreaseIndexToSend(qIx);
                                }
                                CleanUp_After_Msg_Completed(qIx);
                            }
                            else
                            {
                                Destination_Id = TxQ[qIx].TxMsgArr[GetIxToAck].DestinationId;

                                //070929
                                // -1 is since it is increased before used by sender
                                TxQ[qIx].TxMsgArr[UseToSendIx].SentFragment
                                    = TxQ[qIx].TxMsgArr[UseToSendIx].NotAckedFragment - 1;

                                // This is the case when all targets to the
                                // fragmented msg are down

                                if((TxQ[qIx].CurrFragmentedMsgAckBitMap64
                                     & (g_pShm->BitMapNodesUp64 | g_pShm->BitMapNodesNew64))
                                            == (dcom_ulong64) 0)
                                {
                                    // if no more targets for next fragment (entire msg)
                                    if(*pDbg>1)
                                        PrintDbg("All targets down for fragm msg - aborting\n");

                                    // If last fragment has been sent, GetIxToSend
                                    // already has been updated.
                                    // (it is incremented when last fragment is sent).
                                    // In that case, GetIxToSend shall not be incremented.
                                    // Before last fragm is sent GetIxToSend == GetIxToAck
                                    // After  last fragm is sent (but not acked)
                                    //                           GetIxToSend == (GetIxToAck+1)
                                    // CleanUp_After_Msg_Completed() will increment GetIxToAck

                                    if(TxQ[qIx].GetIxToSend == TxQ[qIx].GetIxToAck)
                                    {
                                        IncreaseIndexToSend(qIx);
                                    }


                                    CleanUp_After_Msg_Completed(qIx);  //,0
                                }
                                else
                                    goto Continue_Fragmented_Msg;
                            }
                            continue;
                        }

                        // Inc GetIxToSend if:
                        //  - last fragment AND not already increased (retransmit)
                        // This is because normally GetIxToSend is incremented when
                        // last fragment is sent

                        // Tested - executed OK when the target goes down
                        if (  (TxQ[qIx].TxMsgArr[UseToSendIx].bIsFragmented & 0x8000)
                           && (TxQ[qIx].GetIxToSend == TxQ[qIx].GetIxToAck))
                        {
                            IncreaseIndexToSend(qIx);

                            g_pTxStatistics[qIx].CountTxGiveUp++;

                            if(*pDbg>3)
                                PrintDbg("#5* GetIxToSend --> %d\n",TxQ[qIx].GetIxToSend);
                        }

                        CleanUp_After_Msg_Completed(qIx);

                        continue; // process next msg in this Queue
                    }

                    //else
                    // This is the case where the target still sends KeepAlive messages,
                    // but does not send an Ack.
                    // If we drop this message, we will get the same result next time
                    // and things will be very slow.
                    // If we set a flag, the node is down, how shall we recover from
                    // that state.
                    // To be implemented ???? (when I know how to)
                    // if(result == 'G') // Has given up
                    //
                    //  ;  we can go on with next msg
                }  // end Timeout
            } // end IsTransmitting

            //===============================================================
            // We come here if:
            // 1) If not waiting for Ack.
            //    Action: Send next msg or fragment (if any)
            //
            // 2) We are waiting for ACK, and has not timed out.
            //    Action: Check if allowed to send msg or fragment ahead.
            //            If so, send msg.
            //
            // 3) Timeout when waiting for Ack.
            //    Action resend msg or fraction.
            //
            // There are these cases:
            // 1) no more messages on the Queue
            // 2) A new msg
            // 3) Next fragment shall be sent (if there is any)
            //
            // PoolDistribution ?????
            //===============================================================

            //PrintDbg("#   TxThread[%d] Send msg next or next fragment Put=%X GetS/A=%d/%d\n",
            //      qIx, TxQ[qIx].PutIx, TxQ[qIx].GetIxToSend, TxQ[qIx].GetIxToAck);

            //--------------------------------------------------
            // Are there any pending Messages on the Xmit_Queue.
            // This includes sending ahead - Sliding Window func
            //--------------------------------------------------
            GetIxToAck = TxQ[qIx].GetIxToAck;

            // If waiting for Ack
            if(TxQ[qIx].TxMsgArr[GetIxToAck].IsTransmitting)
            {
                if(*pDbg>3)
                PrintDbg("#   qIx=%d IsTransmitting=%X bWaitingForAck=%X\n",
                        qIx, TxQ[qIx].TxMsgArr[GetIxToAck].IsTransmitting,
                        bWaitingForAck);

                //-----------------------------------------------------
                // Waiting for Ack on a fragment. If the latest was a
                // not last fragment, we can send some fragments ahead.
                //
                // Not allowed to start send any new messages when sending
                // a fragmented, so this ends up with send next fragment
                // or continue
                //-----------------------------------------------------
                if(TxQ[qIx].TxMsgArr[GetIxToAck].bIsFragmented)
                {
                    if(*pDbg>3)
                    PrintDbg("#   TxThread Fragment NotAcked=%X, Sent=%X Last=%X\n",
                        TxQ[qIx].TxMsgArr[GetIxToAck].NotAckedFragment,
                        TxQ[qIx].TxMsgArr[GetIxToAck].SentFragment, //Latest sent fragment
                        TxQ[qIx].TxMsgArr[GetIxToAck].LastFragment);//Last fragm to be sent

                    // if last is sent but not acked
                    if(TxQ[qIx].TxMsgArr[GetIxToAck].SentFragment
                            == TxQ[qIx].TxMsgArr[GetIxToAck].LastFragment)
                    {
                        if(*pDbg>3)
                            PrintDbg("#   Last sent but not acked. Ix P/S/nA=%u/%u/%u\n",
                                    TxQ[qIx].PutIx,TxQ[qIx].GetIxToSend,
                                    TxQ[qIx].GetIxToAck);

                        WaitTimeOut = WAITTIME_WAITING_FOR_ACK;
                        qIx++;
                        continue;  // with next Queue
                    }

                    // Do not send fragment ahead when retransmitting
                    if(
                        (TxQ[qIx].TxMsgArr[GetIxToAck].IsRetransmitting)
                        &&
                        ((TxQ[qIx].TxMsgArr[GetIxToAck].IsTransmitting & 0xFF) != 0)
                      )
                    {
                        // not allowed to send more ahead
                        WaitTimeOut = WAITTIME_WAITING_FOR_ACK;
                        qIx++;
                        continue; // go on with next Queue
                    }

                    //-------------------------------------------------
                    // In idle state NotAckedFragment = 1+SentFragment
                    // If we use (NotAckedFragment-1) it can be read as
                    // LastAckedFragment

                    if( (TxQ[qIx].TxMsgArr[GetIxToAck].SentFragment
                        - (TxQ[qIx].TxMsgArr[GetIxToAck].NotAckedFragment -1) )
                                < (MAX_AHEAD_F))  // e.g < 3 ==> allowed to send 3 ahead
                    {
                        goto Continue_Fragmented_Msg;
                    }

                    // not allowed to send more ahead
                    WaitTimeOut = WAITTIME_WAITING_FOR_ACK;
                    qIx++;
                    continue; // go on with next Queue
                } // end bIsFragmented
                else // Not fragmented
                //------------------------------------------------------------
                // Waiting for Ack on a non-fragmented msg.
                // We can send some msg ahead.
                //
                // Check if we can go ahead with next.
                //
                // GetIxToSend
                //   = Index for next pos to send.
                //     Increased by TxThread when the msg has been sent.
                // GetIxToAck
                //   = Index for next position to be acked.
                //     Increased by TxThread when the msg has been acked.
                //     If no ack requested, increased to next ....
                // PutIx
                //   = Index for next position to place a msg on.
                //     Increased by application after a new msg is added.
                //
                // If the new msg not shall be acked, we still wait (meaning
                // there is no special handling for these 'shall not be acked').
                //--------------------------------------------------------------
                {
                    int ahead;

                    // Do not send ahead when retransmitting.
                    // The reason is that the most common cause of retransmit
                    // is that the receiver is slow (loaded) and probably
                    // the ahead messages will also be 'not acked'.
                    if(TxQ[qIx].TxMsgArr[TxQ[qIx].GetIxToAck].IsRetransmitting)
                    {
                        qIx++;
                        WaitTimeOut = WAITTIME_WAITING_FOR_ACK;
                        continue;   // with next Queue
                    }

                    if(TxQ[qIx].GetIxToSend == TxQ[qIx].PutIx)
                    {
                        if(*pDbg>6)
                        PrintDbg("#   Not Fragmented, Queue Empty P/Gs/Ga=%d/%d/%d\n",
                            TxQ[qIx].PutIx,TxQ[qIx].GetIxToSend,TxQ[qIx].GetIxToAck);
                        qIx++;
                        continue;   // with next Queue
                    }

                    // Do not send a new msg ahead when we are sending a fragmented
                    if( TxQ[qIx].bIsSendingFragmented)
                    {
                        qIx++;
                        WaitTimeOut = WAITTIME_WAITING_FOR_ACK;
                        continue;   // with next Queue
                    }

                    // Added by JOOT 2012-02-06. Dont use send ahead before we have safely transmitted the first few messages
                    // in the seqNum serie.
                    // By doing this we avoid the problems that occurs when seqNo 0 is reordered and
                    // arrive after seqNo 1, 2, etc. Sending seq 0,1,2 will always be delivered in that order since we
                    // wait for all Ack's before sending next.
                    // There is still possible failure if seqNo 0 i duplicated, and delayed (if thats a realistic case)
                    // For example this case will end up in failure: 
                    //    If sending seq 0, 1, 2 and they arrive as 0, 1, 0, 2. Receiver will now
                    //    start Nack'ing 1 since it restarted when the duplicated 0 arrived.
                    if(TxQ[qIx].TxMsgArr[TxQ[qIx].GetIxToAck].SequenceNumber<MAX_AHEAD_NF+1)
                    {
                        qIx++;
                        WaitTimeOut = WAITTIME_WAITING_FOR_ACK;
                        continue;   // with next Queue
                    }

                    // Do not send a new msg ahead when there are to may pending acks
                    if(TxQ[qIx].GetIxToSend >= TxQ[qIx].GetIxToAck)
                        ahead = TxQ[qIx].GetIxToSend - TxQ[qIx].GetIxToAck;
                    else
                        ahead = MAX_XMIT_QUEUE
                                + TxQ[qIx].GetIxToSend - TxQ[qIx].GetIxToAck;

                    if(ahead > MAX_AHEAD_NF) // Too many ahead - not allowed to send next
                    {
                        qIx++;
                        continue;   // with next Queue
                    }

                    // Do not send ahead after PD_ISCOMPLETE msg
                    // (TxSeqNumIsRestarted is set when PD_ISCOMPLETE is sent)
#ifdef USE_RESTART_SEQNUM
                    if(TxQ[qIx].TxSeqNumIsRestarted[TxQ[qIx].TxMsgArr[TxQ[qIx].GetIxToSend].DestinationId])
                    {
                        qIx++;
                        WaitTimeOut = WAITTIME_WAITING_FOR_ACK;
                        continue;   // with next Queue
                    }
#endif
                    if(*pDbg>3)
                    PrintDbg("#   Send NF ahead P/Gs/Ga=%d/%d/%d\n",
                        TxQ[qIx].PutIx,TxQ[qIx].GetIxToSend,TxQ[qIx].GetIxToAck);

                    goto Begin_A_New_Msg;
                } // end  Waiting for Ack - Not fragmented
            } // end IsTransmitting

            // We come here only if not transmitting

            if(TxQ[qIx].PutIx == TxQ[qIx].GetIxToSend) // nothing to do
            {
                qIx++;
                continue;
            }
            else if (CalculateCurrentSendAhead(qIx)>MAX_AHEAD_NF)
            {
                PrintDbg(" Discard Begin_A_New_Msg TxQ[%d] P/S/A=%d/%d/%d\n",
                     qIx, TxQ[qIx].PutIx, TxQ[qIx].GetIxToSend, TxQ[qIx].GetIxToAck);
                qIx++;
                continue;
            }

            goto Begin_A_New_Msg;

            //------------------------------------------------------
            // We never come here. The code above makes a continue
            // or goto Continue_Fragmented_Msg or Begin_A_New_Msg.
            //------------------------------------------------------

Continue_Fragmented_Msg:
            //--------------------------------------------------
            // Continue sending a fragmented msg.
            // (fragments after first in a fragmented msg).
            // It has already been checked that this is allowed.
            //--------------------------------------------------

            if(*pDbg>3)
            PrintDbg("#   TxThread[%d] Continue fragment Put=%u GetS/A=%u/%u\n",
                    qIx, TxQ[qIx].PutIx, TxQ[qIx].GetIxToSend,
                    TxQ[qIx].GetIxToAck);

            GetIxToAck = TxQ[qIx].GetIxToAck;
            //PrintDbg("************* send next fragment: GetIx=%d\n",GetIx);

            Destination_Id = TxQ[qIx].TxMsgArr[GetIxToAck].DestinationId;

            TxQ[qIx].TxMsgArr[GetIxToAck].SentFragment++;

            Ahead_Ix = TxQ[qIx].TxMsgArr[GetIxToAck].SentFragment & MASK_AHEAD;
            FragmBit = (dcom_uchar8) (1 << (TxQ[qIx].TxMsgArr[GetIxToAck].SentFragment & 7));

            // Use the same ExpAckBitMap64 as when we started
            TxQ[qIx].TxMsgArr[GetIxToAck].ExpAckBitMap64[Ahead_Ix]
                                    = TxQ[qIx].CurrFragmentedMsgAckBitMap64;

            TxQ[qIx].TxMsgArr[GetIxToAck].IsTransmitting |= (FragmBit | 0x8000);

            // Normally TxQ[qIx].GetIxToSend == TxQ[qIx].GetIxToAck.
            // But if last is retransmitted this is not true.
            UseToSendIx = TxQ[qIx].GetIxToAck;

            goto Send_The_Message;

            // unreachable

            //=======================================================
            // Begin a new msg. Fragmented or not-fragmented
            // We have checked that it is OK to send next msg
            // Make preparation in TxQ[qIx].TxMsgArr[GetIx].xxx
            //========================================================
Begin_A_New_Msg:
            {
                int GetIxToSend;
                GetIxToSend = TxQ[qIx].GetIxToSend;

                Destination_Id = TxQ[qIx].TxMsgArr[GetIxToSend].DestinationId;

                if(*pDbg>3)
                PrintDbg("#   TxThread[%d] A new msg Put=%u GetS/A=%u/%u Dst=%u\n",
                        qIx, TxQ[qIx].PutIx, TxQ[qIx].GetIxToSend,
                        TxQ[qIx].GetIxToAck, Destination_Id);

                //----------------------------------------------------
                // Calculate ExpAckBitMap64
                // This is done also for NoAck messages, because we
                // want to know if there are any present targets
                //----------------------------------------------------

                TxMsgSize   = TxQ[qIx].TxMsgArr[GetIxToSend].UsrMsgLength
                            + SIZEOF_UDP_MSG_HDR;

                // Do not start a new msg if it is fragmented and there
                // are any pending Acks.
                if(TxMsgSize > FRAGMENT_SIZE) // is fragmented
                {
                    if(GetIxToSend != TxQ[qIx].GetIxToAck)
                    {
                        qIx++;
                        continue;   // with next Queue
                    }
                    Ahead_Ix = 1;
                }
                else
                    Ahead_Ix = 0;

                // A PoolDistribution allways starts with Sequencenumber = 0

                if(TxQ[qIx].TxMsgArr[GetIxToSend].IsPoolDistr) // PoolDistribution
                {
                    TxQ[qIx].TxMsgArr[GetIxToSend].ExpAckBitMap64[Ahead_Ix]
                       = g_pShm->BitMapBeingPoolDistributed64
                        & (g_pShm->BitMapNodesNew64 | g_pShm->BitMapNodesUp64);

                    if(TxQ[qIx].TxMsgArr[GetIxToSend].IsPoolDistr & PD_FIRSTDATA)
                        TxQ[qIx].TxSequenceNumber[Destination_Id] = 0;

                    //PrintDbg("BitMapBeingPoolDistributed64 = %X.%08X\n",
                    //   (dcom_ulong32)(g_pShm->BitMapBeingPoolDistributed64>>32),
                    //   (dcom_ulong32)(g_pShm->BitMapBeingPoolDistributed64 & 0xFFFFFFFF));
                }
                else // NOT PoolDistribution
                {
                    TxQ[qIx].TxMsgArr[GetIxToSend].ExpAckBitMap64[Ahead_Ix]
                          = g_pShm->BitMapNodesUp64 | g_pShm->BitMapNodesNew64;
                }

                // Clear bits that not are members of this Destination
                if(Destination_Id >= 64)
                {
                    int ChNum = Destination_Id - 64;
                    TxQ[qIx].TxMsgArr[GetIxToSend].ExpAckBitMap64[Ahead_Ix]
                                &= CConfig::m_BitMapDestChannelMembers64[ChNum];
                }
                else // Unicast
                    TxQ[qIx].TxMsgArr[GetIxToSend].ExpAckBitMap64[Ahead_Ix]
                                    &= ((dcom_ulong64)1<<Destination_Id);

                // Clear bit for me
                TxQ[qIx].TxMsgArr[GetIxToSend].ExpAckBitMap64[Ahead_Ix]
                                 &= MyBitMapInv64;

                if(*pDbg>3)
                PrintDbg("#   ExpAckBitMap: %X.%08X   Ahead_Ix=%d\n",
                  (dcom_ulong32)(TxQ[qIx].TxMsgArr[GetIxToSend].ExpAckBitMap64[Ahead_Ix]>>32),
                  (dcom_ulong32)(TxQ[qIx].TxMsgArr[GetIxToSend].ExpAckBitMap64[Ahead_Ix]
                                                                      & 0xFFFFFFFF),
                  Ahead_Ix);

                //-------------------------------------
                // Are there any targets to this message
                //-------------------------------------

                // If no targets
                if(TxQ[qIx].TxMsgArr[GetIxToSend].ExpAckBitMap64[Ahead_Ix]
                                    == (dcom_ulong64) 0)
                {
                    if(*pDbg>=2)
                        PrintDbg("#   TxThread[%d] There are no targets."
                          " Ignoring sending this msg ++++++++++++++++++\n",qIx);

                    g_pTxStatistics[qIx].CountTxNoTargets++;

                    
                    bool msgIgnored = CleanUp_After_Msg_Ignored(qIx, GetIxToSend);

                    if (msgIgnored)
                    {
                        continue; // continue with next item in this Queue
                    }
                    else
                    {
                        ++qIx;
                        continue;   // continue with next Queue
                    }
                } // end No targets

                //-------------------------------------------
                // There is a valid msg to be build and sent
                //-------------------------------------------

                pData = (char *) TxQ[qIx].TxMsgArr[GetIxToSend].pMsgBuff;

                TxMsgSize   = TxQ[qIx].TxMsgArr[GetIxToSend].UsrMsgLength
                            + SIZEOF_UDP_MSG_HDR;

                if(TxMsgSize > FRAGMENT_SIZE)
                {
                    TxQ[qIx].bIsSendingFragmented = 1;
                    Ahead_Ix = 1;
                    TxQ[qIx].TxMsgArr[GetIxToSend].bIsFragmented = TRUE;
                    TxQ[qIx].TxMsgArr[GetIxToSend].SentFragment  = 1;
                    TxQ[qIx].TxMsgArr[GetIxToSend].NotAckedFragment = 1;

                    TxQ[qIx].TxMsgArr[GetIxToSend].LastFragment = (dcom_ushort16)
                         (1 + (TxQ[qIx].TxMsgArr[GetIxToSend].UsrMsgLength - 1)
                                    / (FRAGMENT_SIZE - SIZEOF_UDP_MSG_HDR));

                    for (int i=0;i<MAX_AHEAD;++i) TxQ[qIx].TxMsgArr[GetIxToSend].AckAheadFragments[i]=0;

                    if(*pDbg>=4)
                        PrintDbg("#   TxThread[%d] A new frag Msg:"
                            " Size=%d. SentFragment = %X\n",
                            qIx, TxQ[qIx].TxMsgArr[GetIxToSend].UsrMsgLength,
                            TxQ[qIx].TxMsgArr[GetIxToSend].SentFragment);
                }
                else
                {
                    TxQ[qIx].bIsSendingFragmented = 0;
                    Ahead_Ix = 0;
                    TxQ[qIx].TxMsgArr[GetIxToSend].bIsFragmented = FALSE;
                }

                // save to be used by following fragments
                if(TxQ[qIx].TxMsgArr[GetIxToSend].bIsFragmented)
                {
                    TxQ[qIx].CurrFragmentedMsgAckBitMap64
                            = TxQ[qIx].TxMsgArr[GetIxToSend].ExpAckBitMap64[Ahead_Ix];
                }

                //First FragmentNumber = 1 ==> set bit 1
                if(TxQ[qIx].TxMsgArr[GetIxToSend].bIsFragmented)
                    TxQ[qIx].TxMsgArr[GetIxToSend].IsTransmitting = (1<<1) | 0x8000;
                else
                    TxQ[qIx].TxMsgArr[GetIxToSend].IsTransmitting = 1;

                TxQ[qIx].TxMsgArr[GetIxToSend].TransmitComplete = 0;

                // If we are retransmitting this, do not set a new SequenceNumber
                // IsRetransmitting is cleared at startup and when msg is completed

                if(!TxQ[qIx].TxMsgArr[GetIxToSend].IsRetransmitting)
                {
                    TxQ[qIx].TxMsgArr[GetIxToSend].SequenceNumber // get next to expect
                                    = TxQ[qIx].TxSequenceNumber[Destination_Id]++;

#ifdef USE_RESTART_SEQNUM
                    // Next msg will restart SeqNum
                    // This flag (TxSeqNumIsRestarted) is set when PD_COMPLETE is transmitted
                    // It will be cleared when any msg but ???

                    if(TxQ[qIx].TxMsgArr[GetIxToSend].IsPoolDistr & PD_ISCOMPLETE)
                    {
                        TxQ[qIx].TxSequenceNumber[Destination_Id]    = 0;
                        TxQ[qIx].TxSeqNumIsRestarted[Destination_Id] = 1;
                    }
#endif
                    TxQ[qIx].RetryCount = 0;
                }
                UseToSendIx = TxQ[qIx].GetIxToSend;

                //PrintDbg("****** qIx=%u SeqNum=%u, DstId=%u GetIxToSend=%u\n",
                //  qIx, TxQ[qIx].TxMsgArr[GetIxToSend].SequenceNumber, Destination_Id, GetIxToSend);
            } // end Begin a new msg

            //===========================================================
            // Build msg, thenSend (or ReSend) it
            // It could be:
            // A new or resent msg.
            // or a new or resent fragment (via 'goto Send_The_Message').
            //===========================================================
Send_The_Message:

            result = Build_Tx_Message(qIx,UseToSendIx,Destination_Id,
                                        &pData,&TxMsgHdr);

            if(TxQ[qIx].TxMsgArr[UseToSendIx].bIsFragmented)
            {
                if( TxQ[qIx].TxMsgArr[UseToSendIx].IsRetransmitting)
                TxQ[qIx].TxMsgArr[UseToSendIx].IsRetransmitting--;
            }

            // Clear bit for me - not needed here ???
            TxQ[qIx].TxMsgArr[UseToSendIx].ExpAckBitMap64[Ahead_Ix]
                                        &= MyBitMapInv64;

            TxQ[qIx].StartSendTime = DoseOs::Get_TickCount();

            if(TxQ[qIx].TxMsgArr[UseToSendIx].pMsgBuff == NULL)
            {
                // This should never happen
                PrintErr(0,
                    "#   TxThread[%d] NO Buffer allocated. Ptr is NULL\n",qIx);

                // Improvement: better Recovery
                //DoseOs::Sleep(1000); // Can't have a sleep now that we have introduced g_threadLock.

                // set next GetIxToSend
                if((UseToSendIx + 1) >= MAX_XMIT_QUEUE) SetIndexToSend(qIx, 0);
                else SetIndexToSend(qIx, (dcom_ushort16) (UseToSendIx + 1));

                if(*pDbg>3)
                    PrintDbg("#2* GetIxToSend --> %d\n", TxQ[qIx].GetIxToSend);

                // Note if not want ack ==> ???
                // ???? we could use continue here.
            }
            else // Send the msg
            {
                //--------------------------------------------------
                // If DestinationId>= 64, but ExpAckBitMap64[0]
                // only contains one target, we could use Unicast
                //--------------------------------------------------

                if(Destination_Id >= 64)
                {
                    result = Check_IfUniCast_CanBeUsed(
                            TxQ[qIx].TxMsgArr[UseToSendIx].ExpAckBitMap64[Ahead_Ix]);

                    //PrintDbg("result from Check_IfUniCast_CanBeUsed() = %d\n", result);

                    if(result < 64)
                        SendTo_DestinationId = result;
                    else
                        SendTo_DestinationId = Destination_Id;
                }
                else
                    SendTo_DestinationId = Destination_Id;

                // PrintDbg("***111*** qIx=%u SeqNum=%u, DstId=%u UseToSendIx=%u\n",
                //          qIx, TxQ[qIx].TxMsgArr[UseToSendIx].SequenceNumber,
                //          SendTo_DestinationId, UseToSendIx);

                 dcom_ulong32 DestIpAddr_nw
                    = CConfig::Get_DestinationIpAddress(SendTo_DestinationId);

                dcom_ushort16 DestPort = (dcom_ushort16) (CConfig::m_Dose_Port_Data + qIx);

                if(pData == NULL) // something is wrong
                {
                    PrintDbg("ERR pData == NULL: pMsgHdr=%X pData=%X Frag=%X Seq=%X\n",
                        (char *) &TxMsgHdr, pData, TxMsgHdr.FragmentNumber,
                        TxMsgHdr.SequenceNumber);

                    PrintDbg("  Put=%u GetS/A=%u/%u\n",
                        qIx, TxQ[qIx].PutIx, TxQ[qIx].GetIxToSend,
                        TxQ[qIx].GetIxToAck);

                    //DoseOs::Sleep(100); // Can't have a sleep now that we have introduced g_threadLock.
                }

                result = TxSock.SendTo2(DestIpAddr_nw, DestPort,
                                        (char *) &TxMsgHdr, SIZEOF_UDP_MSG_HDR,
                                        pData, TxMsgHdr.Size);

                if(result < 0)
                {
                    PrintDbg("ERR Send: pMsgHdr=%X pData=%X Frag=%X Seq=%X\n",
                        (char *) &TxMsgHdr, pData, TxMsgHdr.FragmentNumber,
                        TxMsgHdr.SequenceNumber);

                    //DoseOs::Sleep(100); // Can't have a sleep now that we have introduced g_threadLock.
                }

                if(TxMsgHdr.bWantAck)
                {
                    WaitTimeOut = WAITTIME_WAITING_FOR_ACK;

                    // Inc GetIxToSend if:
                    // - Not fragmented has been sent
                    // - last fragment AND not already increased (retransmit)
                    if ((!TxQ[qIx].TxMsgArr[UseToSendIx].bIsFragmented)
                        || (
                                (TxMsgHdr.FragmentNumber & 0x8000)
                             && (UseToSendIx == TxQ[qIx].GetIxToAck)
                           )
                       )
                    {
                        if(TxQ[qIx].GetIxToSend != TxQ[qIx].PutIx)
                        {
                            if((UseToSendIx + 1) >= MAX_XMIT_QUEUE)
                                SetIndexToSend(qIx, 0);                                
                            else
                                SetIndexToSend(qIx, (dcom_ushort16) (UseToSendIx + 1));                                                          

                            if(*pDbg>3)
                                PrintDbg("#3* GetIxToSend --> %d\n",TxQ[qIx].GetIxToSend);
                        }
                    }
                }
                else // No Ack shall be sent
                {
                    // Not fragmented
                    if (!TxQ[qIx].TxMsgArr[UseToSendIx].bIsFragmented)
                    {
                        g_pShm->Statistics.TotTxCount++;
                        g_pTxStatistics[qIx].CountTxNoAckOk++;

                        if(TxQ[qIx].GetIxToSend != TxQ[qIx].PutIx)
                        {
                            if((UseToSendIx + 1) >= MAX_XMIT_QUEUE)
                                SetIndexToSend(qIx, 0);                                
                            else
                                SetIndexToSend(qIx, (dcom_ushort16) (UseToSendIx + 1));                           

                            if(*pDbg>3)
                                PrintDbg("8* GetIxToSend --> %d\n",TxQ[qIx].GetIxToSend);
                        }

                        CleanUp_After_Msg_Completed(qIx);
                    }
                    else // Is fragmented
                    {
                        if(TxMsgHdr.FragmentNumber & 0x8000) // Last fragment
                        {
                            g_pShm->Statistics.TotTxCount++;
                            g_pTxStatistics[qIx].CountTxNoAckOk++;

                            if(TxQ[qIx].GetIxToSend != TxQ[qIx].PutIx)
                            {
                                if((UseToSendIx + 1) >= MAX_XMIT_QUEUE)
                                    SetIndexToSend(qIx, 0);                                
                                else
                                    SetIndexToSend(qIx, (dcom_ushort16) (UseToSendIx + 1));

                                if(*pDbg>3)
                                    PrintDbg("#9* GetIxToSend --> %d\n",TxQ[qIx].GetIxToSend);
                            }

                            CleanUp_After_Msg_Completed(qIx);
                        }
                        else // Not last
                        // We can continue with next fragment at once
                        {
                            goto Continue_Fragmented_Msg;
                            // An alternative (because the implementation above
                            // violates the priority requirement)
                            // TxQ[qIx].TxMsgArr[UseToSendIx].bIsFragmented |= 0x80;
                            // qIx++;
                        }
                    }
                }

                 // If we don't increase qIx, we will continue
                // with the same Queue

            } // end Send the msg

            // This causes "no wait" before restart scanning from TxQ[0]
            bThereMightBeMore = TRUE;

            // Allowed to send max this many before processing next queue
            // ### f�r test kan man l�gga en PrintDbg() h�r f�r att kolla om/n�r det intr�ffar
            if(++TxQ[qIx].LapCount >= g_MaxLapCount[qIx])
               qIx++;

        } // end for(qIx)

        for(int qix = 0; qix < NUM_TX_QUEUES ; ++qix)
        {
            if (TxQ[qix].bSendQueueNotOverflow)
            {
                WakeUp_QueueNotFull(qix);
                TxQ[qix].bSendQueueNotOverflow = 0;

                if(*pDbg > 3) PrintDbg("#   Called WakeUp_QueueNotFull(%d)\n", qix);
            }
        }

    } // end for(;;) // For ever
#ifndef _MSC_VER //disable warning in msvc
    return 0;
#endif
}
/*----------------- end TxThread() --------------------*/

//##################################################
// Section Entries
//##################################################

/***************************************************************************
* Context: Application
*
* When a message shall be transmitted it is placed on the Transmit Queue
* by the calling application. PutIx is then updated by application.
* TxThread is waked up by an event.
*
* A little problem:
* - If NumUsedEntries =  PutIx- GetIx > MAX, a flag TransmitQueueOverflow
*   is set and overflow is returned.
* - TxThread detects if TransmitQueueOverflow is set and sends an event
*   to application when free entries becomes available.
* - PutIx is only updated by this routine.
* - GetIx is updated by the TxThread. (could be updated in the middle of
*   the code below).
* - There must be some kind of semaphore handling to handle the case where
*   GetIx is changed when we are interrupted by TxThread.
*
* PoolDistribution:
* 0=No PD, 1=PD,
* 2=PdIsComplete msg. Called from Set_PoolDistributionIsCompleted()
***************************************************************************/

#define XMIT_QUEUE_MIN_FREE_ENTRIES 4
#define XMIT_QUEUE_HYSTERISIS        4

int CDoseComTransmit::Xmit_Msg(const char *pMsg, dcom_ulong32 MsgLength,
                               dcom_uchar8 PoolDistribution, dcom_uchar8 bUseAck,
                               int Priority, int Destination)
{
    ACE_Guard<ACE_Thread_Mutex> lck(g_threadLock);

    // Check if we should simulate a stop in the outgoing traffic
    if (g_pShm->InhibitOutgoingTraffic)
    {

        // if pooldistribution is complete, we need to clean up some stuff...
        if(PoolDistribution == PD_ISCOMPLETE) 
        {
            g_pShm->PoolDistributionWillEndSoon  = 0;
            g_pShm->PoolDistributionIsInProgress = 0;
            g_pShm->BitMapBeingPoolDistributed64 = (dcom_ulong64)0; // just in case
        }

        return(ERR_DOSECOM_OK);
    }

    dcom_ushort16  Next_PutIx;
    dcom_ushort16  NumUsed;
    int     Qix = Priority;
    int     PutIx;
    int     GetIx;
    int     MightBeOverFlow = 0;
    dcom_ulong64 ExpAckBitMap64;
    static int bNextPdIsFirst = 1;

    if(*pDbg>2)
        PrintDbg(">>> Xmit_Msg() lock protected entry Pd=%X Size=%d\n",
                PoolDistribution,MsgLength);

    // PoolDistribution has value 1 for Data.
    // We want 1 for first message, then 2
    if(PoolDistribution)
    {
        //need it saved for Set_PoolDistributionIsCompleted()
        g_QueueIndex_CurrPoolDistribution = Qix;

        if(PoolDistribution == PD_ISCOMPLETE)
        {
            // This is the case when no PD data
            if(bNextPdIsFirst) PoolDistribution |= PD_FIRSTDATA;
            bNextPdIsFirst = 1;
        }
        else
        {
            if(bNextPdIsFirst)
            {
                PoolDistribution = PD_FIRSTDATA;
                bNextPdIsFirst = 0;
            }
            else
            {
                PoolDistribution = PD_MOREDATA;
            }
        }
        // nytt 071009
        // Is PD - handle the case where all targets has gone down
         ExpAckBitMap64 = g_pShm->BitMapBeingPoolDistributed64
                        & (g_pShm->BitMapNodesNew64 | g_pShm->BitMapNodesUp64);
    } // end if(PoolDist...)
    else
         ExpAckBitMap64 = g_pShm->BitMapNodesNew64 | g_pShm->BitMapNodesUp64;

    // Clear bits that not are members of this Destination
    if(Destination >= 64)
        ExpAckBitMap64 &= CConfig::m_BitMapDestChannelMembers64[Destination - 64];
    else // Unicast
        ExpAckBitMap64 &= ((dcom_ulong64)1<<Destination);

    dcom_ulong64 MyBitMap64 = (dcom_ulong64)1 << g_MyDoseId;

    ExpAckBitMap64 &= ~MyBitMap64;  // Clear bit for me

    //-------------------------------------
    // Are there any targets to this message
    //-------------------------------------
    // If no targets
    if(*pDbg>2) PrintDbg(">>> Xmit_Msg() - check if any targets\n");

    if(ExpAckBitMap64 == (dcom_ulong64) 0)
    {
        if(*pDbg>=2)
              PrintDbg(">>> There are no targets. Ignore this msg. Dst=%d\n",
                      Destination);

        g_pTxStatistics[Priority].CountTxNoTargets++;

        if((PoolDistribution & PD_ISCOMPLETE) == 0) // not when PD_ISCOMPLETE
        {
            DoseCom_FreeBuff((char *) pMsg);
            return(0);
        }
        // If PD is in progress (but all targets are down), we must
        // pass the PD-complete to TxThread(), so it can make a proper
        // Cleanup.
        if(!g_pShm->PoolDistributionIsInProgress)
        {
           return(0); // PD not in progress - Cleanup not needed
        }

    } // end No targets

    // Calculate number of used entries on the Queue

    // Note: TxQ[Qix].GetIxToAck, but not TxQ[Qix].PutIx might be
    // changed by the TxThread. So a copy must be used in this operation.

    if((TxQ[Qix].PutIx + 1) >= MAX_XMIT_QUEUE) Next_PutIx = 0;
    else Next_PutIx = (dcom_ushort16) (TxQ[Qix].PutIx + 1);

    GetIx = TxQ[Qix].GetIxToAck;                                // [A]

    // BUG fixed 071010
    // if(Next_PutIx <= GetIx)   // this caused == to make num_used = 0
    if(Next_PutIx <= GetIx)      // if wrapped
        NumUsed = (dcom_ushort16) (MAX_XMIT_QUEUE + Next_PutIx - GetIx);
    else
        NumUsed = (dcom_ushort16) (Next_PutIx - GetIx);

    if(*pDbg>=3)
    {
        PrintDbg(">>> Xmit_Msg() ########## PD=%X Len=%d Dst=%u P/G=%u/%u\n",
                PoolDistribution, MsgLength, Destination, TxQ[Qix].PutIx, GetIx);
    }

    if(NumUsed > (MAX_XMIT_QUEUE - XMIT_QUEUE_MIN_FREE_ENTRIES))   // Start case 4)
    {
        if(*pDbg>4)
              PrintDbg(">   Xmit_Msg() Overflow in XmitQueue P/G=%X/%X -------\n",
                        Next_PutIx, GetIx);

        // If this is the case when called from routine
        // Set_PoolDistributionIsCompleted() we are allowed to use one more
        // position in the Queue. But only if not a total overflow. (which
        // never should happen). The reason for this is that application not
        // shall have to worry about overflow when signaling PD_ISCOMPLETE.

        //if(PoolDistribution & PD_ISCOMPLETE) // removed 08-10-06
        if(PoolDistribution & (PD_ISCOMPLETE | PD_FIRSTDATA)) // NEW 08-10-06
        {
            // This could only happen if application calls
            // Set_PoolDistributionIsCompleted() more than once,
            // which not should be done.
            if(Next_PutIx == GetIx) // no free entries
            {
                PrintDbg(">   Xmit_Msg() Overflow in XmitQueue for PD_ISCOMPLETE\n");

                // Want some free entries before signaling
                Atomics::atomic_write32(&TxQ[Qix].TransmitQueueOverflow, XMIT_QUEUE_HYSTERISIS); //  [B]

                MightBeOverFlow = 1;
            }
            // else one free entry, the msg can be stored on the Queue
            // Go on with 'Store the msg ...'
        }
        else
        {
//#define FORCE_ERROR  // to force error case 1). - interrupted by TxThread
//#ifdef FORCE_ERROR
//          DoseOs::Sleep(2000); // force error
//#endif
            //Want 2 free entries be fore signalling
            Atomics::atomic_write32(&TxQ[Qix].TransmitQueueOverflow, XMIT_QUEUE_HYSTERISIS);      // [B]

            //if(*pDbg>1)
            //   PrintDbg(">   Xmit_Msg()           After Sleep P/G=%u/%u\n",
            //   TxQ[Qix].PutIx, GetIx);

            MightBeOverFlow = 2;
        }
    }

    //-------------------------------------------------------------------------
    // Now there are the following cases:
    // 1) There was no overflow (bMightBeOverFlow == 0).
    //    We can go directly to 'Store the msg on the Queue'.
    //
    // 2) We have been interrupted by the TxThread between 'A' and 'B' or 'C'
    //    The TxThread has sent all messages so the Queue is empty.
    //    It could be 'NoAck' messages or the ack has come very fast.
    //    As a result, routine CleanUp_After_Msg_Completed() which clears the
    //    TxQ[Qix].TransmitQueueOverflow will never be called - a lock condition
    //    (This case is the reason for the 'retry code' below).
    //    We can go on with 'Store the msg on the Queue'.
    //
    // 3) We have been interrupted by the TxThread between 'A' and 'B' but all
    //    messages are not processed, so TxQ[Qix].TransmitQueueOverflow will
    //    soon be cleared. We can go on with 'Store the msg on the Queue'.
    //
    // 4) We have NOT been interrupted by the TxThread between 'A' and 'B'.
    //    NumUsed is not changed. (Queue still full).
    //    This is the only case OVERFLOW is returned. Since TransmitQueueOverflow
    //    was set before checking 'NumUsed', we are sure events will be sent
    //-------------------------------------------------------------------------

    // False for case 1). - So it is case 2), 3) or 4)
    if(MightBeOverFlow)
    {
        // Recalculculate  number of used entries on the Queue.

        GetIx = TxQ[Qix].GetIxToAck;                                // [C]

        if(Next_PutIx <= GetIx) //if wrapped
            NumUsed = (dcom_ushort16) (MAX_XMIT_QUEUE + Next_PutIx - GetIx);
        else
            NumUsed = (dcom_ushort16) (Next_PutIx - GetIx);

        // if Queue still is full. - Case 4)

        if(NumUsed >(MAX_XMIT_QUEUE-XMIT_QUEUE_MIN_FREE_ENTRIES))//Start case 4)
        {
            //if(PoolDistribution & PD_ISCOMPLETE) // removed 08-10-06
            if(PoolDistribution & (PD_ISCOMPLETE | PD_FIRSTDATA)) // NEW 08-10-06
            {
                if(Next_PutIx == GetIx)
                {
                    // We have serious problems here. ??????
                    // There is some SW bug, or a misbehaving application.

                    PrintDbg(">>  Xmit_Msg() Overflow in XmitQueue for"
                                " PD_ISCOMPLETE ******\n");

                    //Want some free entries before signaling
                     Atomics::atomic_write32(&TxQ[Qix].TransmitQueueOverflow, XMIT_QUEUE_HYSTERISIS); //[D]

                    g_pTxStatistics[Qix].CountTxOverflow++;
                    g_pShm->Statistics.TransmitQueueFullCount++;
                    return(ERR_DOSECOM_OVERFLOW); // not passed to application
                }
                // else there is one free entry, The msg can be stored on the Queue
            }
            else
            {
                //Sleep(2000); // force error
                //Want some free entries be fore signalling
                Atomics::atomic_write32(&TxQ[Qix].TransmitQueueOverflow, XMIT_QUEUE_HYSTERISIS);     // [D]

                g_pTxStatistics[Qix].CountTxOverflow++;
                g_pShm->Statistics.TransmitQueueFullCount++;
                return(ERR_DOSECOM_OVERFLOW);
            }
        } // end  case 4)

        //----------------------------------------------------------
        // Case 1), 2) or 3).
        // If we come here, we have been interupted by the TxThread
        // which has processed some or all messages on the Queue.
        //
        // (NumUsed <= MAX_XMIT_QUEUE - 2)
        // or it is a PD and NumUsed == (MAX_XMIT_QUEUE - 1)
        // In both cases the msg can be stored on the Queue.
        //----------------------------------------------------------
        if(*pDbg>3)
            PrintDbg(">   Xmit_Msg() Recovered after Overflow P/G=%u/%u\n",
                    TxQ[Qix].PutIx, GetIx);
    } // end MightBeOverFlow

    //----------------------------
    // Store the msg on the Queue
    //---------------------------

    // Statistics - Log max used queue length
    if(NumUsed > TxQ[Qix].MaxUsedQueueLength)
    {
        TxQ[Qix].MaxUsedQueueLength = NumUsed;
    }

    PutIx = TxQ[Qix].PutIx;

    //Nytt 08-09-30
    //-----------------------------------------------------------------
    // If the send routines has missed freeing a sendbuffer,
    // this code makes a recovery. (should never happen).
    //-----------------------------------------------------------------
    if(TxQ[Qix].TxMsgArr[PutIx].ShallFreeBuffer && (TxQ[Qix].TxMsgArr[PutIx].pMsgBuff != NULL))
    {
        DoseCom_FreeBuff((char *) TxQ[Qix].TxMsgArr[PutIx].pMsgBuff);

        PrintDbg("Xmit_Msg() Free lost buffer. Qix=%d PD=%X Ack=%X Dest=%X IsFr=%X TrComp=%C IsRetr=%X\n",
                 Qix,
                 TxQ[Qix].TxMsgArr[PutIx].IsPoolDistr,
                 TxQ[Qix].TxMsgArr[PutIx].bUseAck,
                 TxQ[Qix].TxMsgArr[PutIx].DestinationId,
                 TxQ[Qix].TxMsgArr[PutIx].bIsFragmented,
                 TxQ[Qix].TxMsgArr[PutIx].TransmitComplete,
                 TxQ[Qix].TxMsgArr[PutIx].IsRetransmitting);

        PrintDbg("ExpAck = %X.%08X %X.%08X %X.%08X %X.%08X UP=%X.%08X\n",
                 (dcom_ulong32) (TxQ[Qix].TxMsgArr[PutIx].ExpAckBitMap64[0]>>32),
                 (dcom_ulong32) (TxQ[Qix].TxMsgArr[PutIx].ExpAckBitMap64[0] & 0xFFFFFFFF),
                 (dcom_ulong32) (TxQ[Qix].TxMsgArr[PutIx].ExpAckBitMap64[1]>>32),
                 (dcom_ulong32) (TxQ[Qix].TxMsgArr[PutIx].ExpAckBitMap64[1] & 0xFFFFFFFF),
                 (dcom_ulong32) (TxQ[Qix].TxMsgArr[PutIx].ExpAckBitMap64[2]>>32),
                 (dcom_ulong32) (TxQ[Qix].TxMsgArr[PutIx].ExpAckBitMap64[2] & 0xFFFFFFFF),
                 (dcom_ulong32) (TxQ[Qix].TxMsgArr[PutIx].ExpAckBitMap64[3]>>32),
                 (dcom_ulong32) (TxQ[Qix].TxMsgArr[PutIx].ExpAckBitMap64[3] & 0xFFFFFFFF),
                 (dcom_ulong32) (g_pShm->BitMapNodesUp64>>32),
                 (dcom_ulong32) (g_pShm->BitMapNodesUp64 & 0xFFFFFFFF));
    }
    // end Nytt 08-09-30


    TxQ[Qix].TxMsgArr[PutIx].UsrMsgLength   = MsgLength;
    TxQ[Qix].TxMsgArr[PutIx].pMsgBuff       = (char *) pMsg;
    TxQ[Qix].TxMsgArr[PutIx].IsPoolDistr    = PoolDistribution;
    TxQ[Qix].TxMsgArr[PutIx].bUseAck        = bUseAck;
    TxQ[Qix].TxMsgArr[PutIx].DestinationId  = (dcom_uchar8)Destination;
    TxQ[Qix].TxMsgArr[PutIx].bIsFragmented  = 0;

    TxQ[Qix].TxMsgArr[PutIx].IsRetransmitting = 0;

     // only if called from Set_PoolDistributionIsCompleted()
    if(PoolDistribution & PD_ISCOMPLETE)
        TxQ[Qix].TxMsgArr[PutIx].ShallFreeBuffer = 0;
    else
        TxQ[Qix].TxMsgArr[PutIx].ShallFreeBuffer = 1;

    TxQ[Qix].PutIx = Next_PutIx;

    if(*pDbg>3) PrintDbg(">>* PutIx=%d\n", TxQ[Qix].PutIx);

    g_hTxEvent->Set();
    return(0);
}
/*------------------- end Xmit_Msg() ----------------------------*/

/***************************************************
* Context: Application
*
* Called from application when PD is completed 
****************************************************/
void CDoseComTransmit::Set_PoolDistributionIsCompleted(int Priority,
                                                       int DestinationId)
{
    static char *pMsg = (char *) "PdIsComplete";

    if(*pDbg>=2)
        PrintDbg(">>> Set_PoolDistributionIsCompleted() pIx/gIxS/gIxA=%d/%d/%d\n",
                TxQ[Priority].PutIx,
                TxQ[Priority].GetIxToSend, TxQ[Priority].GetIxToAck);

    // this is a flag to CleanUp_After_Msg_Completed()
    g_pShm->PoolDistributionWillEndSoon = 1;

    Xmit_Msg(pMsg, strlen(pMsg), PD_ISCOMPLETE, 1, Priority, DestinationId);
}

/***************************************************
* Context: Application
*
* Called once at startup
****************************************************/
int CDoseComTransmit::Xmit_Init(dcom_ushort16 DoseId)
{
    unsigned long tid1,tid2;
    int result;


    if(*pDbg) PrintDbg("Xmit_Init()\n");

    memset((void*)TxQ, 0, sizeof(DOSE_TXQUEUE_S) * NUM_TX_QUEUES);

    g_pTxStatistics = Statistics::GetPtrToTxStatistics(0);

    g_pShm = CNodeStatus::GetNodeSharedDataPointer();

    pDbg = &g_pShm->Debug;

    g_pNodeStatusTable = g_pShm->NodeStatusTable;

    g_MyDoseId = DoseId;

    if(*pDbg>2) PrintDbg(">>> Xmit_Init() Create Events\n");

    g_hTxEvent = new DoseOs::CDoseComEvent;
    g_hTxEvent->Create();

    // if(*pDbg>2)
    // PrintDbg(">>> Xmit_Init() Create Events done (%X)- Creating threads\n",
    //          g_hTxEvent);

    result = DoseOs::CreateThread(tid1, &Ack_Thread, NULL);

    if(*pDbg>2)
    PrintDbg(">>  Xmit_Init() Create Ack_Thread ==> %X\n", result);

    result = DoseOs::CreateThread(tid2, &TxThread, NULL);

    if(*pDbg>2)
    PrintDbg(">>  Xmit_Init() Create TxThread ==> %X\n", result);

    return(0);
}
/************************************************************
* Context: Application
*
************************************************************/
void CDoseComTransmit::Get_Info(char *pBuf)
{
#if (MAX_NUM_PRIO_CHANNELS == 6)
    sprintf(pBuf,"PutIx =%u/%u/%u/%u/%u/%u\n"
        "GetIx =%u/%u/%u/%u/%u/%u\n"
        "MaxUse=%u/%u/%u/%u/%u/%u\n",
        TxQ[0].PutIx,TxQ[1].PutIx,TxQ[2].PutIx,TxQ[3].PutIx,
        TxQ[4].PutIx,TxQ[5].PutIx,
        TxQ[0].GetIxToSend,TxQ[1].GetIxToSend,TxQ[2].GetIxToSend,TxQ[3].GetIxToSend,
        TxQ[4].GetIxToSend,TxQ[5].GetIxToSend,
        TxQ[0].MaxUsedQueueLength,TxQ[1].MaxUsedQueueLength,
        TxQ[2].MaxUsedQueueLength,TxQ[3].MaxUsedQueueLength,
        TxQ[4].MaxUsedQueueLength,TxQ[5].MaxUsedQueueLength);
#else
    sprintf(pBuf,"PutIx =%u/%u/%u/%u\nGetIx =%u/%u/%u/%u\n"
        "MaxUse=%u/%u/%u/%u\n",
        TxQ[0].PutIx,TxQ[1].PutIx,TxQ[2].PutIx,TxQ[3].PutIx,
        TxQ[0].GetIxToSend,TxQ[1].GetIxToSend,TxQ[2].GetIxToSend,TxQ[3].GetIxToSend,
        TxQ[0].MaxUsedQueueLength,TxQ[1].MaxUsedQueueLength,
        TxQ[2].MaxUsedQueueLength,TxQ[3].MaxUsedQueueLength);
#endif
}

/*----------------- end DoseComTransmit.cpp ----------------------*/
