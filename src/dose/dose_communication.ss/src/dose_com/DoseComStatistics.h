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
        dcom_ulong32   CountTxOverflow;    // Application send overflow
        dcom_ulong32   CountTxNoAckOk;     // Application send Ok
        dcom_ulong32   CountTxWithAckOk;   // Fragmented is counted as one
        dcom_ulong32   CountTxNoTargets;
        dcom_ulong32   CountTxGiveUp;
        dcom_ulong32   CountTxRetransmit; //???[MAX_NUM_NODES];
        dcom_ulong32   CountTxLostAck[MAX_NUM_NODES];
        dcom_ulong32   CountDown[MAX_NUM_NODES];
    }  TX_STATISTICS_S;

    typedef struct // one instance/RxQueue
    {
        dcom_ulong32   CountRxOverflow;    // overflow
        dcom_ulong32   CountRxWithAckOk;   //
        dcom_ulong32   CountRxNoAckOk;     //
        dcom_ulong32   CountRxDuplicate;   //
        dcom_ulong32   CountRxInvalidMsg;  // invalid messages
        dcom_ulong32   CountRxInvSeqNum;
        dcom_ulong32   CountRxMustWait;
        dcom_ulong32   CountRxInvFragmentNum;
        dcom_ulong32   CountRxOtherErr;
    }  RX_STATISTICS_S;

    RX_STATISTICS_S *GetPtrToRxStatistics(int Ix);
    TX_STATISTICS_S *GetPtrToTxStatistics(int Ix);
}

