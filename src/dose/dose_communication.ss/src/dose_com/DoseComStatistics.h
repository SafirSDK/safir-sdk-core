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

#define MAX_NUM_NODES 64

namespace Statistics
{
    typedef struct // one instance/TxQueue
    {
        ulong   CountTxOverflow;    // Application send overflow
        ulong   CountTxNoAckOk;     // Application send Ok
        ulong   CountTxWithAckOk;   // Fragmented is counted as one
        ulong   CountTxNoTargets;
        ulong   CountTxGiveUp;
        ulong   CountTxRetransmit; //???[MAX_NUM_NODES];
        ulong   CountTxLostAck[MAX_NUM_NODES];
        ulong   CountDown[MAX_NUM_NODES];
    }  TX_STATISTICS_S;

    typedef struct // one instance/RxQueue
    {
        ulong   CountRxOverflow;    // overflow
        ulong   CountRxWithAckOk;   //
        ulong   CountRxNoAckOk;     //
        ulong   CountRxDuplicate;   //
        ulong   CountRxInvalidMsg;  // invalid messages
        ulong   CountRxInvSeqNum;
        ulong   CountRxMustWait;
        ulong   CountRxInvFragmentNum;
        ulong   CountRxOtherErr;
    }  RX_STATISTICS_S;

    RX_STATISTICS_S *GetPtrToRxStatistics(int Ix);
    TX_STATISTICS_S *GetPtrToTxStatistics(int Ix);
}

