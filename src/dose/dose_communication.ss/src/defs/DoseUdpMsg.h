/******************************************************************************
*
* Copyright Saab AB, 2004-2008 (http://www.safirsdk.com)
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

/************************************************************
* DoseUdpMsg.h - a part of DoseComDll - For LINUX and WIN32
*
* Defines network Messages between Dose
*************************************************************/

//============================================
// Messages
//============================================

#define DOSE_MSG_MAGIC              0x5F37  //
#define DOSE_MSG_MAGIC_SWAP         0x375F  //byte swapped

// to DOSE_UDP_DATA_PORT
#define MSG_TYPE_DATA               0xD1

// to DOSE_UDP_ACK_PORT
#define MSG_TYPE_ACK                0xD2
#define MSG_TYPE_NACK               0xD4

// to DOSE_UDP_KEEPALIVE_PORT
#define MSG_TYPE_KEEPALIVE          0xD7
#define MSG_TYPE_KEEPALIVE_START    0xD8
#define MSG_TYPE_GETINFO_1          0xDB

// IsPoolDistribution has this values
#define PD_FIRSTDATA                1   // First PD data
#define PD_MOREDATA                 2   // remaining PD data
#define PD_ISCOMPLETE               8   // PD is complete, might be ored with 1

// Data msg, sent as IpMulticst to all nodes

typedef unsigned short ushort;
typedef unsigned char  uchar;
typedef unsigned long  ulong;

typedef struct
{
    ushort  Magic;           // to be sure it is not some junk msg
    uchar   MsgType;         // MSG_TYPE_DATA
    uchar   DoseIdFrom;
    ulong   IpAddrFrom_nw;   // Senders IpAddr
    ulong   DoseIdBitMap[2]; // defines target nodes
    ulong   TotalSize;       // Total size of fragmented msg
                             // Receiver uses it to allocate a buffer
    ushort  Size;            // Size of data (in this fragment)
    ushort  FragmentNumber;  // bit 15 set for last fragment,
                             // 0=not fragmented
                             // others are fragment number 1...nn

    ushort  SequenceNumber;  // used by ACK
    uchar   Info;            // currently IsRetransmittig flags bit 0-3
    uchar   TxMsgArray_Ix;   // new fas2

    uchar   TxQueueNumber;   // new fas2 = PriorityChannel
    uchar   IsPoolDistribution;
                             // from each node. This defines which
    uchar   bWantAck;        //
    uchar   DestinationId;   // a
} DOSE_UDP_MSG_HDR;

#define SIZEOF_UDP_MSG_HDR (sizeof(DOSE_UDP_MSG_HDR))

// ACK is sent as unicast as response to received DATA msg

typedef struct
{
    ushort  Magic;          // to be sure it is note some junk msg
    uchar   MsgType;        // MSG_TYPE_ACK/NACK
    uchar   DoseIdFrom;
    ulong   IpAddrFrom_nw;  // Senders IpAddr
    ushort  SequenceNumber; // from received DATA msg
    uchar   TxQueueNumber;  // New fas2
    uchar   TxMsgArray_Ix;  // New fas2
    ushort  FragmentNumber; // received Fragment number

    ushort  Info;           // These bits are for debugging with a NW listener
                            // ACK:  bit 0-7  = info in received msg (a uchar)
                            //       bit 8-15 = 0      if OK,
                            //                  0x0100 if duplicate,
                            //                  0x0200 if duplicate fragment
                            //                  0x8000 if no more free buffers
                            //                =
                            // NACK: expected FragmentNumber
                            // or expected SequenceNumber
} DOSE_UDP_ACK_MSG;  // size = 16


// KeepAlive is sent as IP Multicast at short intervals between all nodes

typedef struct
{
    ushort  Magic;      // to be sure it is note some junk msg
    uchar   MsgType;    // MSG_TYPE_KEEPALIVE or MSG_TYPE_GETINFO_1
    uchar   DoseIdFrom;
    ulong   IpAddrFrom_nw;  // Senders IpAddr
    ulong   TimeStamp;      // the time the node was started
} DOSE_UDP_KEEPALIVE_MSG;

typedef struct
{
    ushort  Magic;      // to be sure it is note some junk msg
    uchar   MsgType;    // MSG_TYPE_KEEPALIVE or MSG_TYPE_GETINFO_1
    uchar   DoseIdFrom;
    ulong   IpAddrFrom_nw;  // Senders IpAddr
    ushort  RespPort_nw;
    uchar   ReqCode_1;
    uchar   ReqCode_2;
} DOSE_UDP_GETINFO_MSG;

/*--------------- end DoseUdpMsh.h -------------------*/
