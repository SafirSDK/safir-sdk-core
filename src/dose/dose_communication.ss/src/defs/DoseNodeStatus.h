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
* DoseNodeStatus.h - a part of DoseComDll - For LINUX and WIN32
**************************************************************************/

#ifndef ushort
typedef unsigned short ushort;
typedef unsigned char  uchar;
typedef unsigned long  ulong;
#endif

#define NODESTATUS_FREE      0   // not used
#define NODESTATUS_UP       'U'  // also defined in DoseCom_Interface.h
#define NODESTATUS_DOWN     'D'
#define NODESTATUS_NEW      'N'

#define KEEP_ALIVE_TIMEOUT  3000

#define POOLDISTRIBUTION_DELAYTIME  10000 //2900  // 2.9 sec

//------------------------------------------------------------------
// This is kept in a shared data area
// The only reason for this is that an external program, DoseMonitor
// be used to monoitor the state of DoseCom
//------------------------------------------------------------------

#define NODESTATUS_TABLE_SIZE 64 // must be in range 31 - 64

typedef volatile struct
{
    volatile uchar  Status;          // NODESTATUS_xxxx (Up/Down)
    volatile uchar  ToBeGivenToAppl; // Set by KeepAlive when Status change
                                     // cleared when Appl has fetched it
    volatile uchar  ToBePoolDistributed; // ??? needed Set by KeepAlive when a new up
    volatile uchar  HasReceivedPdComplete;

    volatile ushort RetryCount;     // # retry send to this node
    volatile ushort spare2;

    volatile ulong  RxCount;        // received from this node
    volatile ulong  IpAddr_nw;
    volatile ulong  LatestTime;     // Time when latest a msg arrived (keep alive)
    volatile ulong  TimeStamp;      // from KeepAlive msg. Time when node started.
} NODESTATUS_TABLE;

typedef volatile struct
{
    volatile ulong TotRxCount;    // Incremented by Rececive_Thread when new msg
    volatile ulong TotTxCount;    // Incremented by Xmit_Thread when new msg
    volatile ulong LostAckCount;  // Incremented by Xmit_Thread when lost Ack
    volatile ulong ReTxCount;     // Incremented by Xmit_Thread when rexmit msg
    volatile ulong zBufferOverFlowCount;   // Incremented when GetBuffer fails
    volatile ulong ReceiveQueueFullCount;  // Incremented when overflow in RxQueue
    volatile ulong TransmitQueueFullCount; // Incremented when overflow in TxQueue
} DOSE_STATISTICS;


typedef struct
{
    volatile NODESTATUS_TABLE   NodeStatusTable[NODESTATUS_TABLE_SIZE];
    volatile DOSE_STATISTICS        Statistics;

    //These are updated by KeepAliveThread
    volatile ulong64    BitMapNodesNew64;             // defines DOWN nodes
    volatile ulong64    BitMapNodesUp64;              // defines UP nodes
    volatile ulong64    BitMapNodesDown64;            // defines DOWN nodes
    volatile ulong64    BitMapToBePoolDistributed64;  // marked here when new
    volatile ulong64    BitMapBeingPoolDistributed64; // marked here when PD
    volatile ulong64    BitMapLatestPoolDistributed64;// marked here when PD

    volatile ulong  MyIpAddr_nw;
    volatile ulong  NetAddr_nw;
    volatile ulong  IpMulticastAddr_nw;
    volatile ushort DosePort;
    volatile uchar  MyDoseId;
    volatile uchar  MaxUsedDoseId;

    //These are updated by KeepAliveThread
    volatile ulong  PoolDistributionWillStartSoon; // a flag and a timer
    volatile ulong  PoolDistributionIsInProgress;
    volatile ulong  PoolDistributionWillEndSoon;
    //volatile ulong    zzzzzzz_LastTxQueuePosForPoolDistribution; //??????

    // Each node is member in a set of DestinationChannels
    // BitMapDestChannelMembers[][0] is for channel 0.
    // BitMapDestChannelMembers[][31] is for channel 31.
    // Bit 'NN' defines if DoseId 'NN' is a member
    // Since DoseId is requires 64 bit, two ulong are used.
    // Built by KeepAliveThread.
    // Used by TxThread which must make a quick lookup to
    // find out which nodes are  expected to send an Ack.
    //ulong BitMapDestChannelMembers[2][MAX_NUM_DEST_CHANNELS];

    // Debug levels
    //    0 - no debug
    // >= 1 - Start up basic
    // >= 2 - Start up details
    // >= 3 - Tx/Rx basic
    // >= 4 - Tx/Rx details
    // >= 5 - Ack
    // >= 6 - KeepAlive
    volatile int        Debug;    // used as if(*pDbg > XX) ....
} DOSE_SHARED_DATA_S;

//--------------------------------------------

class CNodeStatus
{
public:
    CNodeStatus();

public:
    static DOSE_SHARED_DATA_S *GetNodeSharedDataPointer(void);

    static int  UpdateNode_Up(uchar NodeId,
                            ulong IpAddr_nw, ulong TimeStamp);

    static int  CheckTimedOutNodes(void);

    static int  GetNodeInfo(ushort NodeId, ulong *pIpAddr,
                            ulong *pNodeStatus);

    static int InitNodeStatus(int mode);

    static ulong GetNextChangedNode(uchar *pNodeStatus,ulong *pIpAddr_nw);

    static void Get_Info(char *pBuff);

    static void Set_HasReceivedPdComplete(int DoseId);

private:
    static void SetNodeDownWhenInvalidTimeStamp(uchar DoseId);

    static void UpdateNodeStatusBitMap(void);
};
/*----------------------------- end DoseNodeStatus.H -------*/
