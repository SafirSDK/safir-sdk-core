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

#ifndef _DoseCom_Interface_h
#define _DoseCom_Interface_h

#include "DoseCom_Interface_Classes.h"

#define MAX_NUM_DEST_CHANNELS 32 // Number of DestinationChannels for Multicast
#define MAX_NUM_PRIO_CHANNELS  6 // Numver of PriorityChannels

#if defined _WIN32
#  ifndef dcom_ulong64
     typedef unsigned _int64 dcom_ulong64;
#  endif
#  ifndef dcom_ulong32
     typedef unsigned int dcom_ulong32;
#  endif
#elif defined(linux) || defined(__linux) || defined(__linux__)
#  ifndef dcom_ulong64
     typedef unsigned long long dcom_ulong64;
#  endif
#  ifndef dcom_ulong32
     typedef unsigned int dcom_ulong32;
#  endif

   #ifndef _LINUX
      #define _LINUX   // used below
   #endif

#else
#  error "Unable to work out platform"
#endif



//--------------------------------------------------------------------
// Definition of DOSECOM_API below defines which routines shall be
// exported so they are visible from outside dose_com.
//
// Linux: This requires GCC 4.0 or higher
// In Linux are all synbols exported by default.
// There is a flag in the makefile: CPPFLAGS = -fvisibility=hidden
// This makes the default to be hidden.
// __attribute__((visibility("default"))) overrides this and sets
// it to be visible.
//---------------------------------------------------------------------

#if defined _WIN32
#ifndef DOSECOM_API
#ifdef DOSE_COM_EXPORTS  // This is defined when used by dose_com.dll
#define DOSECOM_API _declspec(dllexport)
#else
#define DOSECOM_API _declspec(dllimport)
#endif
#endif
#endif

#ifdef _LINUX
#define DOSECOM_API __attribute__((visibility("default")))
#endif

//-------------------------------------
// NodeStatus returned in
// DoseCom_GetNodeChange() arg Status
//-------------------------------------

// A new node has started sending KeepAlive messages.
// DoseCom is waiting for PoolDistribution completed
#define NODESTATUS_NEW      'N'

// DoseCom has got a PoolDistribution completed message
// The node is ready to be used
#define NODESTATUS_UP       'U'

//has stooped sending KeepAlive meszages
#define NODESTATUS_DOWN     'D'

//-------------------------------------------
// Error/return codes returned to application
//-------------------------------------------
#define ERR_DOSECOM_OK             0  // If OK
#define ERR_DOSECOM_INVALID_SIZE  'I' // If invalid Size parameter.
#define ERR_DOSECOM_INVALID_PARAM 'V' // Invalid parameter
#define ERR_DOSECOM_NO_MSG        'R' // If nothing to read
#define ERR_DOSECOM_OVERFLOW      'O' // Overflow in send Queue, retry later
#define ERR_DOSECOM_PD_NOTEXP     'P' // PoolDistrubution flag set when not
                                      // expected.
                                      // Meaning no distributePoolEvent has
                                      // been sent since latest
                                      // DoseCom_PoolDistributed() (#new1)
#define ERR_DOSECOM_NOT_INIT      'N' // When trying to access before init
                                      // completed

#define ERR_DOSECOM_OTHER_EXISTS  'E' // There exists other nodes at startup


//====================================
// Function prototypes
//====================================

//#ifdef __cplusplus
//extern "C"
//{
//#endif

//-----------------------------------------------------------------------
// Purpose: Define DoseDestinationChannel Configuration
// Must be called before after DoseCom_Init().
//
// This API is used to configure DoseDestinationChannels. It is called
// several times at startup.
// Note that this MUST be the same in all nodes.
//
// Parameters:
// ------------
// DestinationId        Corresponds to 'DestChannel' in table above
//
// IpMulticastAddress   Corresponds to 'IpMulticastAddr' in table above
//                      Defined as a string e.g. "224.1.2.3"
//
// BitMapDestChanMembers
//      A bitmap defining nodes that are members of this DestinationId.

// Returns:
// ERR_DOSECOM_OK (=0)       if OK
// ERR_DOSECOM_INVALID_PARAM
//                           If DestinationId is outside range
//                           64 - (64+MAX_NUM_DEST_CHANNELS)
//-----------------------------------------------------------------------

DOSECOM_API
int DoseCom_Add_DestinationId(int              DestinationId,
                              const char       *IpMulticastAddr,
                              dcom_ulong64     BitMapDestChanMembers);

//-------------------------------------------------------------
// Must be called directly after DoseCom_Add_DestinationId() and
// before everything else since it starts everything.
//
// Startup and Initialization method
// DoseMain has created these events. DoseCom saves them for later use.
//
// pAllocator
//      See comment for class DoseComAllocator above.
//
// multicastAddress
//      The IP multicast address used for KeepAlive messages.
//
// multicastTtl
//      The value to use for TTL (time to live) for the UDP packets.
//
// netAddress
// This info is used to define which network interface that shall be used
// for IpMulticast. This is important if a node is multihomed. (Has more
// than one Ipaddress). The Ip Address with the closest match to parameter
// netAddress will be used.
// The algorithm for 'closest match' is:
// Number of matching bits counting from MSB is counted.
// The Ip Address wiht the highest count is the 'closest match'.
// E.g. If there is one 192.x.x.x net and one 10.x.x.x net a good value
// on netAddres = 192.0.0.0.  (even 128.0.0.0 will work OK).
//
// ----
// The pNotificationHandler->NotifyXxxxx() parameters below are function
// pointers to callback routines imolemented in DoseMain. DoseCom does not
// know and does not care how they are implemented. They are executed in the
// context of different threads in DoseCom.
// There are no speed requirements on ...->NotifyNodeStatusChanged() and
// ...->NotifyStartPoolDistribution(). (300 ms will be ok).
//
// pNotificationHandler->NotifyIncomingData() is called from the thread that
// receives messages and sends Ack, (one for each priority channel). It is
// important that this not is blocked too long by DoseMain. DoseCom can handle
// a long delay but this affects communication speed for all nodes.
//
// pNotificationHandler->NotifyQueueNotFull() is called by the thread that
// sends messages to other nodes.  It is important that this not is blocked
// too long by DoseMain. DoseCom can handle a long delay but this affects
// communication speed.
//
// ---
// pNotificationHandler->NotifyIncomingData()
//      Is a callback routine that is called when there one or more
//      received messages to be read.
//      Parameter 'num' specifies the priority channel where the message
//      is received. Corresponds to DoseCom_Read() - RxUseBitMap parameter.
//      Use DoseCom_Read() until it returns DOSE_NO_MSG.
//
// pNotificationHandler->NotifyQueueNotFull()
//      Is a callback routine that is called when the send queue state is
//      changed from full to not full (if application has got an
//      'send queue overflow' error). The application can continue sending
//      at least 4 messages without getting 'send queue overflow'.
//      The send Queue has length = 24. Overflow is generated when the
//      length is > 20.
//      pQueueNotFullCallback is called when the length has been decreased
//      to 16.
//
// pNotificationHandler->NotifyNodeStatusChanged()
//      Is a callback routine that is called when one or more nodes has
//      changed up/down state. Use DoseCom_GetNodeChange() until it
//      returns false.
//
// pNotificationHandler->NotifyStartPoolDistribution()
//      Is a callback routine that is called when DoseCom detects one or
//      more new nodes, it first waits a few seconds (in case more nodes
//      will start almost at the same time). Then it signals DoseMain by
//      calling this routine. If Pool Distribution already is in progress,
//      DoseCom first waits until it is completed.
//      Application is supposed make a PoolDistribution then call
//      DoseCom_PoolDistributed() to tell DoseCom it is completed.
//      If there is nothing to PoolDistribute, then the application
//      MUST call DoseCom_PoolDistributed() at once.
//
// ----------------------------------------------------------------------
// DoseCom_Init() Waits max 2.3 sec to check if there are any other nodes
// around. The principle is to wait for KeepAlive messages from other nodes.
// These are sent every 1000 millisecond.
// If any node is detected, return is made at once.
//
//-----------------------------------------------------------------------
//
// Returns:
//   ERR_DOSECOM_OK (=0)       if OK and on other nodes are found
//   ERR_DOSECOM_OTHER_EXISTS  if OK one or more nodes found
//   ERR_DOSECOM_INVALID_PARAM if invalid DoseId
//-----------------------------------------------------------------------

DOSECOM_API
int DoseCom_Init(DoseComAllocator*  pAllocator,  // the base class
                 const char*        multicastAddress,
                 int                multicastTtl,
                 const char*        netAddress,
                 int                nodeId,
                 DoseComNotificationHandler* pNotificationHandler);

//----------------------------------------------------------------------
// Send a message
//
// Function:
// Send a message to all nodes or to a specified node.
// This routine puts the message on a queue, wakes up DoseCom, then returns.
// DoseCom will free the message buffer when the message has been sent.
//
// Parameters:
// pMsg         Buffer containing the msg. Is released by DoseCom
// MsgLength    Size of data to send. Max size = 1416 * 0x7FFF = 46_398_072
//              If MsgLength > 1416, it will be fragmented.
//
// PoolDistribution
//              If true, data shall only be sent to nodes that are
//              flagged as new since previous PoolDistribution started.
//              Nodes that becomes new during current PoolDistribution
//              will not be PoolDistributed this time but in next PoolDistr..
//
// bUseAck      Defines if to use the 'Acknowledge mechanism. (false=not use)
//
// Priority     Defines which PriorityChanel to send on
//            . 0 <= Priority < MAX_NUM_PRIO_CHANNELS
//
// DestinationId 00-63 is for unicast to a specified node.
//               64-   is to a defined IpMulticast group defined by
//                     DoseCom_Add_DestinationId()
//
// Returns:
// ERR_DOSECOM_OK            If OK
// ERR_DOSECOM_INVALID_PARAM Invalid parameter. E.g.
//                           Priority or DestinationId out of range.
// ERR_DOSECOM_INVALID_SIZE  If invalid Size parameter. Range = 1 - 63535
// ERR_DOSECOM_PD_NOTEXP     If a PoolDistribution when not expected  (#new1)
// ERR_DOSECOM_OVERFLOW      If Overflow in send Queue, retry later.
//                           The send buffer will not be set free nor modified.
//                           When there is space, DoseCom will send an
//                           queueNotFullEvent.
//--------------------------------------------------------------------
DOSECOM_API
int DoseCom_Send(const char *pMsg, dcom_ulong32 MsgLength,
                 bool   PoolDistribution,
                 bool   bUseAck,
                 int    Priority,
                 int    DestinationId);

//---------------------------------------------------------------------
// Receive a message
//
// Function:
// A non-blocking call to read data.
//
// Parameters:
// *ppBuf          Returns a pointer to the message.
//
// RxUseBitMap     Contains a bitmap which PriorityChannels to read from
//                 Bit 0 = Ch 0, Bit 1 = Ch 1, ...
//                 DoseCom scan through all defined by RxUseBitMap
//                 in priority order.
//
// *pRxFromBitMap  Returns bitmap for the Queue data was read from.
//                 (= 0 if nothing was read).
//
// *pSize          Returns size of msg
//
// pIsNative       Is true if the endian style is in native format, else false
//
// Returns:
// ERR_DOSECOM_OK           If OK
// ERR_DOSECOM_NO_MSG       If nothing to read
//
// Application is responsible for freeing the buffer.
//
//-----------------------------------------------------------------------

DOSECOM_API
int DoseCom_Read(dcom_ulong32 RxUseBitMap, dcom_ulong32 *pRxFromBitMap,
                 char **ppBuf, dcom_ulong32 *pSize,
                 bool *pIsNative);

//---------------------------------------------------------------------
// When DoseCom detects a NodeStatus change "Not present" --> UP <--> DOWN
// an event is sent to application. Application uses this routine to get
// information until no more data.
//
// Application contious calling this until there is no more changed nodes
// Application must check if state is UP/DOWN
//
// Returns:
//   true  if data about a node is returned
//   false if there was no more nodes to report
//---------------------------------------------------------------------

DOSECOM_API
bool DoseCom_GetNodeChange( dcom_ulong32  & DoseId, //out range 0-63
                            dcom_ulong32  & Status, //out=NODESTATUS_UP/DOWN
                            dcom_ulong32  & IpAddr_nw);//out network byteorder

//-------------------------------------------------------------
// Tell DoseCom that the pool distribution has ended.
// This will cause DoseCom to send a PD_IS_COMPLETED message
// to all nodes that are receiving a PD.
// The receiving DoseCom will change nodestatus for the sending
// node from NEW to UP.
//-------------------------------------------------------------

DOSECOM_API
void DoseCom_PoolDistributed(int Priority, int DestinationId);

//-------------------------------------------------------------
// Tell DoseCom to force pool distribution to node specified in 
// RequestorDoseId.
// This will cause DoseCom initialize a PD and notify application
// when it's time to start PD.
//-------------------------------------------------------------

DOSECOM_API
void DoseCom_ForcePoolDistribution(int RequestorDoseId);

//-------------------------------------------------------------
// Get own DoseId
//-------------------------------------------------------------

DOSECOM_API
void DoseCom_GetDoseId( dcom_ulong32  & DoseId);

//------------------------------------------------------------------------
// Get own Ip Address (if called before Init will return undefined number)
//------------------------------------------------------------------------

DOSECOM_API
void DoseCom_GetOwnIpAddr( dcom_ulong32 & IpAddr_nw);


//#ifdef __cplusplus
//}
//#endif

#endif //_DoseCom_Interface_h
