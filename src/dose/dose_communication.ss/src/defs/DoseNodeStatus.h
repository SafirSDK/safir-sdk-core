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

#ifndef dcom_ushort16
typedef unsigned short dcom_ushort16;
typedef unsigned char  dcom_uchar8;
typedef unsigned int  dcom_ulong32;
#endif

#define NODESTATUS_FREE      0   // not used
#define NODESTATUS_UP       'U'  // also defined in DoseCom_Interface.h
#define NODESTATUS_DOWN     'D'
#define NODESTATUS_NEW      'N'

#define KEEP_ALIVE_TIMEOUT  3000
#define NODESTATUS_NEW_TIMEOUT  30000       // timeout when stuck in status NEW

#define POOLDISTRIBUTION_DELAYTIME  10000 //wait time before starting pool distribution.

//------------------------------------------------------------------
// This is kept in a shared data area
// The only reason for this is that an external program, DoseMonitor
// be used to monoitor the state of DoseCom
//------------------------------------------------------------------

#define NODESTATUS_TABLE_SIZE 64 // must be in range 31 - 64

typedef volatile struct
{
    volatile dcom_uchar8  Status;          // NODESTATUS_xxxx (Up/Down)
    volatile dcom_uchar8  ToBeGivenToAppl; // Set by KeepAlive when Status change
                                           // cleared when Appl has fetched it
    volatile dcom_uchar8  ToBePoolDistributed; // Set by KeepAlive when a new node comes up
    volatile dcom_uchar8  HasReceivedPdComplete;  // Pool distribution received from this node
    volatile dcom_uchar8  HasReceivedPdStart;     // Pool distribution start received from this node
    volatile dcom_uchar8  ForcePoolDistribution;  // Force pool distribution to this node // miwn 2010-11-17

    volatile dcom_ushort16 RetryCount;     // # retry send to this node

    volatile dcom_ulong32  RxCount;        // received from this node
    volatile dcom_ulong32  IpAddr_nw;
    volatile dcom_ulong32  LatestTime;     // Time when latest a msg arrived (keep alive)
    volatile dcom_ulong32  TimeStamp;      // from KeepAlive msg. Time when node started.
    volatile dcom_ulong32  NewTime;        // Time when node set to new or pd request sent.
} NODESTATUS_TABLE;

typedef volatile struct
{
    volatile dcom_ulong32 TotRxCount;    // Incremented by Rececive_Thread when new msg
    volatile dcom_ulong32 TotTxCount;    // Incremented by Xmit_Thread when new msg
    volatile dcom_ulong32 LostAckCount;  // Incremented by Xmit_Thread when lost Ack
    volatile dcom_ulong32 ReTxCount;     // Incremented by Xmit_Thread when rexmit msg
    volatile dcom_ulong32 zBufferOverFlowCount;   // Incremented when GetBuffer fails
    volatile dcom_ulong32 ReceiveQueueFullCount;  // Incremented when overflow in RxQueue
    volatile dcom_ulong32 TransmitQueueFullCount; // Incremented when overflow in TxQueue
} DOSE_STATISTICS;


typedef struct
{
    volatile NODESTATUS_TABLE   NodeStatusTable[NODESTATUS_TABLE_SIZE];
    volatile DOSE_STATISTICS        Statistics;

    //These are updated by KeepAliveThread
    volatile dcom_ulong64    BitMapNodesNew64;             // defines DOWN nodes
    volatile dcom_ulong64    BitMapNodesUp64;              // defines UP nodes
    volatile dcom_ulong64    BitMapNodesDown64;            // defines DOWN nodes
    volatile dcom_ulong64    BitMapToBePoolDistributed64;  // marked here when new
    volatile dcom_ulong64    BitMapBeingPoolDistributed64; // marked here when PD
    volatile dcom_ulong64    BitMapLatestPoolDistributed64;// marked here when PD

    volatile dcom_ulong32  MyIpAddr_nw;
    volatile dcom_ulong32  NetAddr_nw;
    volatile dcom_ulong32  IpMulticastAddr_nw;
    volatile dcom_ushort16 DosePort;
    volatile dcom_uchar8  MyDoseId;
    volatile dcom_uchar8  MaxUsedDoseId;

    //These are updated by KeepAliveThread
    volatile dcom_ulong32  PoolDistributionWillStartSoon; // a flag and a timer
    volatile dcom_ulong32  PoolDistributionIsInProgress;
    volatile dcom_ulong32  PoolDistributionWillEndSoon;
    //volatile dcom_ulong32    zzzzzzz_LastTxQueuePosForPoolDistribution; //??????

    // Each node is member in a set of DestinationChannels
    // BitMapDestChannelMembers[][0] is for channel 0.
    // BitMapDestChannelMembers[][31] is for channel 31.
    // Bit 'NN' defines if DoseId 'NN' is a member
    // Since DoseId is requires 64 bit, two dcom_ulong32 are used.
    // Built by KeepAliveThread.
    // Used by TxThread which must make a quick lookup to
    // find out which nodes are  expected to send an Ack.
    //dcom_ulong32 BitMapDestChannelMembers[2][MAX_NUM_DEST_CHANNELS];

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

    static int  UpdateNode_Up(dcom_uchar8 NodeId,
                            dcom_ulong32 IpAddr_nw, dcom_ulong32 TimeStamp);

    static int  CheckTimedOutNodes(void);

    static int  GetNodeInfo(dcom_ushort16 NodeId, dcom_ulong32 *pIpAddr,
                            dcom_ulong32 *pNodeStatus);

    static int InitNodeStatus(int mode);

    static dcom_ulong32 GetNextChangedNode(dcom_uchar8 *pNodeStatus,dcom_ulong32 *pIpAddr_nw);

    static void Get_Info(char *pBuff);

    static void Set_HasReceivedPdComplete(int DoseId);

    static void Set_HasReceivedPdStart(int DoseId);

    static void ForcePoolDistribution(int DoseId);

private:
    static void SetNodeDownWhenInvalidTimeStamp(dcom_uchar8 DoseId);

    static void UpdateNodeStatusBitMap(void);

};
/*----------------------------- end DoseNodeStatus.H -------*/
