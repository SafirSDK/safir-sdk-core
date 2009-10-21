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

#ifdef _LINUX
#define INVALID_SOCKET (unsigned long)-1
#define SOCKET_ERROR    -1
typedef  unsigned long SOCKET;
typedef unsigned long IPADDR;
#define WSAGetLastError() errno
#endif

//--------------------------------------------------------------------
// Description of public routines in class CIpmSocket
//
// All Ip addresses and port numbers are in NetWork byte order.
//
//  GetOwnIpAddress(), GetOwnIpAddress_str();
//  -----------------------------------------
//  Returns IpAddress for the interface that is used to send
//  IpMulticast messages on the specified IpMulticastAddr_nw.
//  It is assumed that the routing table is setup correctly.
//  In a machine with only one network card,argument 'IpMulticastAddr_nw'
//  is ignored.
//  There are two routines. One takes the Ip Multicast address as a 32
//  bit variable in native format. The other as a dotted decimal string.
//
//  A default multicast route can be configured by specifying a network
//  route for 224.0.0.0. The example below shows the used command.
//
//  route add 224.0.0.0 192.1.2.3 0 #192.1.2.3 is a local interface
//
//  Additionally, routes for specific multicast addresses can be configured
//  just like any other host route. The example below shows the used command.
//
//  route add 224.1.2.3 192.5.6.7 0 #192.5.6.7 is a local interface
//
//
//  CreateIpMulticastSocket()
//  -------------------------
//  Creates a socket for receiving and/or sending IP Multicast.
//  Disables IP Multicast LoopBack.
//
//  Parameters:
//  bForReceive, bForSend:
//     Defines if the socket shall be used for Receive and/or for Send.
//
//  IpMulticastAddress,
//    Ignored if 'bForReceive' not set.
//    Is used to set up the socket for reception of this MC address.
//
//  Port:
//    Ignored if 'bForReceive' not set
//    Used for bind() on receive sockets
//
//  Opt_So_Rcvbuf_Size:
//    Ignored if 'bForReceive' not set or if 'Opt_So_Rcvbuf_Size' ==  0
//    Set total buff size for messages queued on the sockets receive queue
//    The default size is 8192 (measured by getsockopt()).
//    It seems to be so that when one message has caused overflow (no data lost)
//    the following messages are lost (thrown away)
//    Implemented by: setsockopt(.. SO_RCVBUF ...);
//
//  Opt_So_Rcvbuf_Timeout
//    Ignored if 'bForReceive' not set or if 'Opt_So_Rcvbuf_Timeout' ==  0
//    Sets a timeout that will be used on all RecvFrom2() (in ms)
//
//  //Opt_NonBlocking: (not implemented)
//  //  If this != 0, the socket is set to non-blocking
//
//
//  SendTo2()
//  --------------
//  Send a message. The sent message is passed in two separate buffers.
//  E.g. a header and a datapart.
//  IPADDR IpAddr_nw,
//  Port_nw,
//  char *pBuf1, unsigned long Size1
//  char *pBuf2, unsigned long Size2
//
//  RecvFrom2()
//  ---------------
//  Receive a message. The first part (Size1) of the message will be stored
//  in pBuf1, the remaing part in pBuf2,
//
//--------------------------------------------------------------------

typedef unsigned long IPADDR;

class CIpmSocket
{
public:
    CIpmSocket(void);

    static IPADDR Get_OwnIpAddress(unsigned long NetAddr_nw);

    long CreateIpMulticastSocket(int            bForReceive,
                                 int            bForSend,
                                 IPADDR         IpMulticastAddr_nw,
                                 unsigned short Port,
                                 unsigned long  Opt_so_rcvbuf_size,
                                 unsigned long  Opt_So_Rcvbuf_Timeout);
                                 //long         Opt_NonBlocking);

    int EnableForRxIpMulticast(unsigned long IpMulticastAddr_nw);

    int SendTo2( IPADDR IpAddr_nw,
                 unsigned short Port,
                 char *pBuf1, unsigned long Size1,
                 char *pBuf2, unsigned long Size2);

    int RecvFrom2( char *pBuf1, unsigned long Size1,
                   char *pBuf2, unsigned long Size2);

    int AreThereAnyPendingRxMessages(void);
private:
    static IPADDR Get_InterfaceForThisIpMulticastAddr(unsigned long IpmAddr_nw);
    static void Init_Sockets(void);

    unsigned long m_SockId;
    static IPADDR m_My_IpAddr_nw;
};
