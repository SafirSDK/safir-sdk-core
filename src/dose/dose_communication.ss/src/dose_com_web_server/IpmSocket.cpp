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

#include <winsock2.h>
#pragma warning(disable: 4127) // to prevent warning from MS inc file
#include <ws2tcpip.h>          // IP Multicast needs it
#include "PrintError.h"
#include "IpmSocket.h"

IPADDR CIpmSocket::m_My_IpAddr_nw = 0;

/*****************************************************************
*
********************************************************************/
CIpmSocket::CIpmSocket(void)
{
    m_SockId = INVALID_SOCKET;
}
/*****************************************************************
*
********************************************************************/

void CIpmSocket::Init_Sockets(void)
{
    WSADATA     WSAData;
    int         result;
    static      BOOL bIsInitialized = FALSE;

    if(bIsInitialized) return;

    if ((result = WSAStartup(MAKEWORD(2,0) ,&WSAData )) != 0)
    {
        bIsInitialized = TRUE;
        //PrintErr(GetLastError(),"WSAStartup failed %d\n", result);
        return;
    }
}

/*********************************************************
*
* Returns IpAddress for the interface that is used to send
* IpMulticast messages on sthe specified IpMulticastAddr_nw
*
* It is assumed that the routing table is setup correctly.
*
**********************************************************/

IPADDR CIpmSocket::GetOwnIpAddress(IPADDR NetAddr_nw)
{
    int     j1;
    int     NumNics;
    IPADDR  IpAddr_nw[8];  // list of Ip addresses
    struct hostent *pHostEnt;


    //if(m_My_IpAddr_nw != 0) return(m_My_IpAddr_nw); // cashed

    Init_Sockets();

    pHostEnt = gethostbyname("");

    if (pHostEnt == NULL)
    {
        PrintDbg("ERROR Can't get IpAddress\n");
        return(0);
    }
    for(j1=0 ; j1<8 ; j1++)
    {
        if(pHostEnt->h_addr_list[j1] == 0) break;
        IpAddr_nw[j1] = *(IPADDR *) pHostEnt->h_addr_list[j1];

        //dwTmp = IpAddr_nw[j1];
        //if(Debug) PrintDbg("%s IP=%s\n", pHostEnt->h_name,
        //              inet_ntoa( *(struct in_addr *)  &dwTmp));  //OK
    }
    NumNics = j1;

    m_My_IpAddr_nw = IpAddr_nw[0];

    if(NumNics == 1) // only one interface
    {
        //PrintDbg("ret %X\n", IpAddr_nw[0]);
        return(IpAddr_nw[0]);
    }

    //--------------------------------------------------------------------
    // Now we have a list of length j1 with addresses. Which to choose.
    // Select the address with closest match to NetWorkAddr_nw
    // (counted from left). But here from right since it is network format
    //--------------------------------------------------------------------

    DWORD Points;
    DWORD BestPoints = 0;
    DWORD BestPointsIndex  = 0;

    for(j1=0 ; j1 < NumNics ; j1++)
    {
        Points = 0;

        if((IpAddr_nw[j1] & 0xFF) == (NetAddr_nw & 0xFF)) //1.xx.xx.xx
            Points = 0x1000;

        if((IpAddr_nw[j1] & 0xFFFF) == (NetAddr_nw & 0xFFFF)) //1.2.xx.xx
            Points |= 0x100;

        if((IpAddr_nw[j1] & 0xFFFFF) == (NetAddr_nw & 0xFFFFF)) //1.2.3x.xx
            Points |= 0x10;

        if((IpAddr_nw[j1] & 0xFFFFFF) == (NetAddr_nw & 0xFFFFFF))
            Points |= 0x1;

        if(Points > BestPoints)
        {
            BestPoints = Points;
            BestPointsIndex = j1;
        }
    }

    if(BestPoints == 0)
        PrintErr(0, "No IpAddress match\n");

    m_My_IpAddr_nw = IpAddr_nw[BestPointsIndex];

    return(m_My_IpAddr_nw);
}
/*--------------- end GetOwnIpAddress() -----------------*/

/***********************************************************
*
************************************************************/

IPADDR CIpmSocket::GetOwnIpAddress_str(const char *pNetWorkAddr_str)
{
    if(pNetWorkAddr_str == NULL)
    return GetOwnIpAddress(inet_addr("224.0.0.0"));
    else
    return GetOwnIpAddress(inet_addr(pNetWorkAddr_str));
}


/*********************************************************************
*
*********************************************************************/
int CIpmSocket::EnableForRxIpMulticast(unsigned long IpMulticastAddr_nw)
{
    int             result;
    int             ttl = 1;
    struct ip_mreq  mreq;


    mreq.imr_multiaddr.s_addr = IpMulticastAddr_nw;
    mreq.imr_interface.s_addr = GetOwnIpAddress(IpMulticastAddr_nw);

    result = setsockopt( m_SockId, IPPROTO_IP, IP_ADD_MEMBERSHIP,
                        (char *) &mreq, sizeof(mreq));
    if (result != 0)
    {
        //PrintErr((int) WSAGetLastError(),
        //      "Can't setsockopt(IP_ADD_MEMBERSHIP) %s",
        //      inet_ntoa(*(struct in_addr *) &mreq.imr_interface.s_addr));
        return(-1);
    }

    return(0);
}

/************************************************************************
*
* what shall I return  Now SockId
************************************************************************/
long CIpmSocket::CreateIpMulticastSocket(
                            int             bForReceive,
                            int             bForSend,
                            IPADDR          IpMulticastAddr_nw,
                            unsigned short  Port,
                            unsigned long   Opt_So_Rcvbuf_Size,
                            unsigned long   Opt_So_RcvTimeo_Timeout)
{
    struct sockaddr_in  sname;
    SOCKET              Sock_id;
    int                 result;
    struct ip_mreq      mreq;

    int                 ttl;
    struct in_addr      addr;


    Init_Sockets();

    Sock_id = socket(AF_INET, SOCK_DGRAM, 0);
    if( Sock_id == INVALID_SOCKET )
    {
        //PrintErr(WSAGetLastError(),"socket() failed");
        return(INVALID_SOCKET); // = -1
    }

    //--------------------------------------
    // Bind
    //-------------------------------------

    if(bForReceive)
    {
        sname.sin_family      = AF_INET;
        sname.sin_addr.s_addr = INADDR_ANY;
        sname.sin_port        = htons(Port);

        if(bind(Sock_id,(struct sockaddr *)&sname,sizeof(sname)) == SOCKET_ERROR)
        {
            (void) closesocket (Sock_id);
            //PrintErr((int)WSAGetLastError(),"bind to (%d) failed",
            //          htons(Port));
            return(INVALID_SOCKET);
        }
    }


    //---------------------------------------------------------------------
    // Set total buff size for messages queued on the sockets receive queue
    // The default size is 8192 (measured by getsockopt()).
    // It seems to be so that when one message has caused overflow (no data lost)
    // the following messages are lost (thrown away)
    //---------------------------------------------------------------------

    if(bForReceive && Opt_So_Rcvbuf_Size)
    {
        result = setsockopt( Sock_id,  SOL_SOCKET, SO_RCVBUF,
                            (char *) &Opt_So_Rcvbuf_Size, sizeof(long));
        if(result == -1)
        {
            //PrintErr(GetLastError(),"setsockopt(SO_RCVBUF) failed");
        }
    }

    //---------------------------------------------------------------------
    // Sets a socket to timeout on recvfrom
    // Portation note: This is not supported by BSD (WinSock2 extension)
    //---------------------------------------------------------------------

    if(bForReceive && Opt_So_RcvTimeo_Timeout)
    {
        result = setsockopt( Sock_id,  SOL_SOCKET, SO_RCVTIMEO,
                            (char *) &Opt_So_RcvTimeo_Timeout, sizeof(long));
        if(result == -1)
        {
            //PrintErr(GetLastError(),"setsockopt(SO_RCVTIMEO) failed");
        }
    }

    //------------------------------------------------------------
    // Enable reception of IP Multicast datagrams
    //------------------------------------------------------------
    if(bForReceive && IpMulticastAddr_nw)
    {

// call this instead: m_SockId = Sock_id; + int CIpmSocket::EnableForRxIpMulticast(unsigned long IpMulticastAddr_nw)

        mreq.imr_multiaddr.s_addr = IpMulticastAddr_nw;
        mreq.imr_interface.s_addr = GetOwnIpAddress(IpMulticastAddr_nw);
        result = setsockopt( Sock_id, IPPROTO_IP, IP_ADD_MEMBERSHIP,
                        (char *) &mreq, sizeof(mreq));
        if (result != 0)
        {
            //PrintErr((int) WSAGetLastError(),
            //      "Can't setsockopt(IP_ADD_MEMBERSHIP) %s",
            //      inet_ntoa(*(struct in_addr *) &mreq.imr_interface.s_addr));
            return(INVALID_SOCKET);
        }

        // This prevents loopback to this receivesocket even
        // if sending is from another socket

        //PrintDbg("Disable IP_MULTICAST_LOOP\n");
        ttl = 0;

        result = setsockopt( Sock_id, IPPROTO_IP, IP_MULTICAST_LOOP,
                            (char *) &ttl, sizeof(char));
        if (result != 0)
        {
            //PrintErr((int)WSAGetLastError(),
            //      "Error at setsockopt(IP_MULTICAST_LOOP)");
            //return(INVALID_SOCKET);
        }
    }

    //---------------------------------------------------------------
    // This is to send IP Multicast
    // 1. TTL = 1 ==> restricted to the same subnet
    // 2. IP_MULTICAST_IF ==> Override the default interface for IP Multicast
    // 3. IP_MULTICAST_LOOP ==> disables sent msg to loopback
    //---------------------------------------------------------------
    if( bForSend && IpMulticastAddr_nw)
    {
        ttl = 1;
        result = setsockopt( Sock_id,  IPPROTO_IP, IP_MULTICAST_TTL,
                            (char *) &ttl, sizeof(ttl));
        if (result != 0)
        {
            //PrintErr((int)WSAGetLastError(),
            //      "Error at setsockopt(IP_MULTICAST_TTL)");
            return(INVALID_SOCKET);
        }

        //???? if not called, the default is used ???????
        // if INADDR_ANY, the default is used

        addr.s_addr = GetOwnIpAddress(IpMulticastAddr_nw); // it is already cashed

        result = setsockopt( Sock_id, IPPROTO_IP, IP_MULTICAST_IF,
                            (char *) &addr, sizeof(addr));
        if (result != 0)
        {
            //PrintErr((int)WSAGetLastError(),
            //      "Error at setsockopt(IP_MULTICAST_IF)");
            return(INVALID_SOCKET);
        }
    }

    m_SockId = Sock_id;

    return(Sock_id);
}

/*------------- end CreateIpMulticastSocket() -------------------*/


/********************************************************************
* The intention is to send header in one buffer and data in one buffer
*
* implementing scatter/gather type of I/O.
*
********************************************************************/

int CIpmSocket::SendTo2(IPADDR IpAddr_nw,
                                unsigned short Port,
                                char *pBuf1, unsigned long Size1,
                                char *pBuf2, unsigned long Size2)
{
    int     result;
    WSABUF  WsaBuff[2];
    unsigned long   dwNumberOfBytesSent;
    struct sockaddr_in  sockAddr;

    //PrintDbg("SendTo2() Ip=%X\n", IpAddr_nw);

    sockAddr.sin_family      = AF_INET;
    sockAddr.sin_addr.s_addr = IpAddr_nw;
    sockAddr.sin_port        = htons(Port);

    WsaBuff[0].buf = (char *) pBuf1;
    WsaBuff[0].len = Size1;
    WsaBuff[1].buf = (char *) pBuf2;
    WsaBuff[1].len = Size2;

    result = WSASendTo (m_SockId,  WsaBuff, (Size2 == 0) ? 1: 2,
                        &dwNumberOfBytesSent, 0,
                        (struct sockaddr *) &sockAddr,
                        sizeof(sockAddr), NULL, NULL);

    if(result == SOCKET_ERROR)
    {
        //PrintErr(WSAGetLastError(),"SendTo2(). WsaSendTo() failed\n");
        return (-1);
    }

    return(0); //OK
}
/*------------------ end Socket_SendTo2() -----------------*/


/*************************************************************************
* The intention is to receive header in one buffer and data in one buffer
*
* Returns:
*  if Ok:    # received bytes
*  if error: -1
*************************************************************************/

int CIpmSocket::RecvFrom2(char *pBuf1, unsigned long Size1,
                            char *pBuf2, unsigned long Size2)
{
    int     result;
    WSABUF  WsaBuff[2];
    unsigned long   NumberOfBytesRecvd;
    unsigned long   Flags = 0;

    WsaBuff[0].buf = (char *) pBuf1;    // receive header here
    WsaBuff[0].len = Size1;
    WsaBuff[1].buf = (char *) pBuf2;    // receive data here
    WsaBuff[1].len = Size2;


    //PrintDbg("WSARecvFrom(b/s b/s = %X %d %X %d\n",
    //      (int) WsaBuff[0].buf, Size1,(int) WsaBuff[1].buf, Size2);

    result = WSARecvFrom(m_SockId,
                        WsaBuff,    //LPWSABUF lpBuffers,
                        (Size2 == 0) ? 1: 2,            //DWORD dwBufferCount,
                        &NumberOfBytesRecvd,
                        &Flags,     //in/out
                        NULL,       //struct sockaddr FAR *lpFrom,
                        0,          //LPINT lpFromlen,
                        NULL,       //LPWSAOVERLAPPED lpOverlapped,
                        NULL);      //LPWSAOVERLAPPED_COMPLETION_ROUTINE lpCompletionRoutine
    if(result == 0) //OK
        return(NumberOfBytesRecvd);
    else
        return(-1);
}
/*--------------------- end IpmSocket.cpp -----------------*/
