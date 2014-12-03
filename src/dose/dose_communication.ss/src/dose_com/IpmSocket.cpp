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

/*************************************************************************
* IpmSocket.cpp - a part of DoseComDll - For LINUX and WIN32
*
* IP Multicast Sockets
*
* There are LOSTS OF #ifdef _WIN32/_LINUX to satisfy both platforms
**************************************************************************/
#ifndef _CRT_SECURE_NO_DEPRECATE
#define _CRT_SECURE_NO_DEPRECATE
#endif

#define IS_USING_SOCKETS
#include "DosePlatform.h"
#include "DoseOsInterface.h"
#include "PrintError.h"
#include "IpmSocket.h"
#include <stdlib.h>

#ifdef _LINUX
#define closesocket(xx) close(xx)
#endif

extern volatile int * volatile pDbg;

IPADDR CIpmSocket::m_My_IpAddr_nw = 0;

/*****************************************************************
*
********************************************************************/
CIpmSocket::CIpmSocket(void)
{
    m_SockId = INVALID_SOCKET;
}
/*****************************************************************
* private
********************************************************************/

void CIpmSocket::Init_Sockets(void)
{
#ifdef _WIN32
    WSADATA     WSAData;
    int         result;
    static      BOOL bIsInitialized = FALSE;

    if(bIsInitialized) return;

    if ((result = WSAStartup(MAKEWORD(2,0) ,&WSAData )) != 0)
    {
        bIsInitialized = TRUE;
        PrintErr(GetLastError(),"WSAStartup failed %d\n", result);
        return;
    }
#endif

#ifdef _LINUX
; // a dummy
#endif
}

/*********************************************************
*
* Returns IpAddress for the interface that is used to send
* IpMulticast messages on sthe specified IpMulticastAddr_nw
*
* It is assumed that the routing table is setup correctly.
*
*
* ??? _WIN32 - could be improved to compare by bit.
**********************************************************/

IPADDR CIpmSocket::Get_OwnIpAddress(IPADDR NetAddr_nw)
{
    IPADDR      Best_IpAddr_nw = 0; // default = error
    IPADDR      XorValue;
    IPADDR      Best_XorValue = 0xFFFFFFFF;
    IPADDR      NetAddr = htonl(NetAddr_nw);

    //PrintDbg("Get_OwnIpAddress() %s\n", inet_ntoa(*(struct in_addr *) &NetAddr_nw));

#ifdef _WIN32
    int     j1;
    int     NumNics;
    IPADDR  IpAddr_nw[8];  // list of Ip addresses
    struct hostent *pHostEnt;

    if(m_My_IpAddr_nw != 0) return(m_My_IpAddr_nw); // cashed

    Init_Sockets();

    pHostEnt = gethostbyname("");

    if (pHostEnt == NULL)
    {
        PrintDbg("ERROR Can't get IpAddress E=%d\n", errno);
        return(0);
    }
    for(j1=0 ; j1<8 ; j1++)
    {
        if(pHostEnt->h_addr_list[j1] == 0) break;
        IpAddr_nw[j1] = *(IPADDR *) pHostEnt->h_addr_list[j1];

        unsigned long dwTmp = IpAddr_nw[j1];
        if(*pDbg)
        {
            PrintDbg("%s IP=%s\n", pHostEnt->h_name,
                     inet_ntoa( *(struct in_addr *)  &dwTmp));  //OK
        }
    }
    NumNics = j1;

    m_My_IpAddr_nw = IpAddr_nw[0];

    if(NumNics == 1) // only one interface
    {
        m_My_IpAddr_nw = IpAddr_nw[0];
        return(IpAddr_nw[0]);
    }

    //--------------------------------------------------------------------
    // Now we have a list of length j1 with addresses. Which to choose.
    // Select the address with closest match to NetWorkAddr_nw
    // (counted from left).
    //--------------------------------------------------------------------

    for(j1=0 ; j1 < NumNics ; j1++)
    {
        XorValue = htonl(IpAddr_nw[j1]) ^ NetAddr;
        if(XorValue < Best_XorValue)
        {
            Best_XorValue = XorValue;
            Best_IpAddr_nw = IpAddr_nw[j1];
        }
    }
    m_My_IpAddr_nw = Best_IpAddr_nw;
    return(m_My_IpAddr_nw);
#endif //_WIN32

//----------------------------------------------------

#ifdef _LINUX
    unsigned long       IpAddr_nw; // default = error
    int                sockfd  = 1;
    struct ifreq       *ifr;  // size = 32
    struct ifconf      ifc;
    struct sockaddr_in sa;
    // buff[512] holds a number of struct ifreq.
    // Since size of these are 32, size 512 is enough for 512/32 ifreq.
    char buff[512];

    if (0 > (sockfd = socket(AF_INET, SOCK_DGRAM, IPPROTO_IP)))
    {
        PrintDbg("Cannot open socket.\n");
        exit(EXIT_FAILURE);
    }

    ifc.ifc_req = (ifreq *) buff;
    ifc.ifc_len = sizeof(buff);

    if (ioctl(sockfd, SIOCGIFCONF, &ifc))
    {
        perror("ioctl SIOCFIFCONF");
        exit(EXIT_FAILURE);
    }

    // Now, ifc contains info of all interfaces

    ifr = ifc.ifc_req;

    // for devices
    for ( ; (char *) ifr < (char *) ifc.ifc_req + ifc.ifc_len; ++ifr)
    {
        if (ifr->ifr_addr.sa_data == (ifr+1)->ifr_addr.sa_data)
            continue;  // duplicate, skip it

        if (ioctl(sockfd, SIOCGIFFLAGS, ifr))
            continue;  // failed to get flags, skip it

        //get value in two steps to avoid warning
        unsigned long* tmp = reinterpret_cast<unsigned long *>
            (ifr->ifr_addr.sa_data + sizeof sa.sin_port);

        IpAddr_nw = *tmp;

        XorValue = htonl(IpAddr_nw) ^ NetAddr;
        //PrintDbg("N=%X X=%X < %X I=%X %X\n",
        //      NetAddr, XorValue, Best_XorValue, Best_IpAddr_nw, IpAddr_nw);

        if(XorValue < Best_XorValue)
        {
            Best_XorValue = XorValue;
            Best_IpAddr_nw = IpAddr_nw;
        }

        //PrintDbg("Interface:  %s\n", ifr->ifr_name);
        //PrintDbg("IP Address: %s\n", inet_ntoa(*(struct in_addr *) &IpAddr_nw));
    } // end for()

    close(sockfd);

    //PrintDbg("My IP Address: %s\n",
    //      inet_ntoa(*(struct in_addr *) &Best_IpAddr_nw));

    m_My_IpAddr_nw = Best_IpAddr_nw;

    return Best_IpAddr_nw;
#endif // _LINUX
}
/*--------------- end GetOwnIpAddress() -----------------*/

/*********************************************************************
* ??? is this used - use it ?????
*********************************************************************/
int CIpmSocket::EnableForRxIpMulticast(unsigned long IpMulticastAddr_nw)
{
    int             result;
    struct ip_mreq  mreq;


    mreq.imr_multiaddr.s_addr = IpMulticastAddr_nw;
    mreq.imr_interface.s_addr = m_My_IpAddr_nw;

    result = setsockopt( m_SockId, IPPROTO_IP, IP_ADD_MEMBERSHIP,
                        (char *) &mreq, sizeof(mreq));
    if (result != 0)
    {
        int ErrCode = WSAGetLastError();
        char buf[32];

        strcpy(buf,inet_ntoa(*(struct in_addr *) &mreq.imr_multiaddr.s_addr));

        PrintErr(ErrCode,
                "Can't setsockopt(IP_ADD_MEMBERSHIP) IpM/IF %s / %s", buf,
                inet_ntoa(*(struct in_addr *) &mreq.imr_interface.s_addr));

        return(-1);
    }
    return(0);
}

/************************************************************************
*
* what shall I return  Now SockId
************************************************************************/
long CIpmSocket::CreateIpMulticastSocket(int             bForReceive,
                                         int             bForSend,
                                         IPADDR          IpMulticastAddr_nw,
                                         int             multicastTtl,
                                         unsigned short  Port,
                                         unsigned long   Opt_So_Rcvbuf_Size,
                                         unsigned long   Opt_So_RcvTimeo_Timeout)
{
    struct sockaddr_in  sname = {0};
    SOCKET              Sock_id;
    int                 result;
    struct ip_mreq      mreq;
    int                 ttl;
    struct in_addr      addr;


    Init_Sockets();

    Sock_id = socket(AF_INET, SOCK_DGRAM, 0);
    if( Sock_id == INVALID_SOCKET )
    {
        PrintErr(DoseOs::Get_LastError(),"socket() failed");
        return -1;
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
            PrintErr((int)DoseOs::Get_LastError(),"bind to (%d) failed",
                        Port);
            return -1;
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
            PrintErr(DoseOs::Get_LastError(),"setsockopt(SO_RCVBUF) failed");
        }

        Opt_So_Rcvbuf_Size = 0;
#ifdef _WIN32
        int Result = 4;
#endif
#ifdef _LINUX
        socklen_t Result = 4;
#endif
        getsockopt( Sock_id,  SOL_SOCKET, SO_RCVBUF, (char *) &Opt_So_Rcvbuf_Size, &Result);
        //PrintDbg("SO_RCVBUF ==> %d\n",Opt_So_Rcvbuf_Size);
    }

//  if(bForReceive)
//  {
//      Opt_So_Rcvbuf_Size = 0;
//      result = 4;
//      getsockopt( Sock_id,  SOL_SOCKET, SO_RCVBUF, (char *) &Opt_So_Rcvbuf_Size, &result);
//      PrintDbg("SO_RCVBUF ==> %d\n",Opt_So_Rcvbuf_Size);
//  }

    //---------------------------------------------------------------------
    // Sets a socket to timeout on recvfrom
    // Portation note: This is not supported by BSD (WinSock2 extension)
    //---------------------------------------------------------------------

    if(bForReceive && Opt_So_RcvTimeo_Timeout)
    {
        //p.rintf("Set socket timeout to %d\n", Opt_So_RcvTimeo_Timeout);

#ifdef _WIN32
        result = setsockopt( Sock_id,  SOL_SOCKET, SO_RCVTIMEO,
                            (char *) &Opt_So_RcvTimeo_Timeout, sizeof(long));
#endif
#ifdef _LINUX
        struct timeval TimeVal;
        TimeVal.tv_sec = Opt_So_RcvTimeo_Timeout / 1000;
        TimeVal.tv_usec = 1000 * (Opt_So_RcvTimeo_Timeout % 1000);
        result = setsockopt( Sock_id,  SOL_SOCKET, SO_RCVTIMEO,
                            (char *) &TimeVal, sizeof(TimeVal));
#endif
        if(result == -1)
        {
            PrintErr(DoseOs::Get_LastError(),"setsockopt(SO_RCVTIMEO) failed");
        }
    }

    //------------------------------------------------------------
    // Enable reception of IP Multicast datagrams
    //------------------------------------------------------------
    if(bForReceive && IpMulticastAddr_nw)
    {

// call this instead: m_SockId = Sock_id; + int CIpmSocket::EnableForRxIpMulticast(unsigned long IpMulticastAddr_nw)
//int   CIpmSocket::EnableForRxIpMulticast(unsigned long IpMulticastAddr_nw)

if(IpMulticastAddr_nw != 1)
{
        mreq.imr_multiaddr.s_addr = IpMulticastAddr_nw;
        mreq.imr_interface.s_addr = m_My_IpAddr_nw;

        result = setsockopt( Sock_id, IPPROTO_IP, IP_ADD_MEMBERSHIP,
                        (char *) &mreq, sizeof(mreq));
        if (result != 0)
        {
            int ErrCode = DoseOs::Get_LastError();
            char buf[32];

            strcpy(buf,inet_ntoa(*(struct in_addr *) &mreq.imr_multiaddr.s_addr));

            PrintErr(ErrCode,
                    "Can't setsockopt(IP_ADD_MEMBERSHIP) Ipm/IF %s / %s E=%d", buf,
                    inet_ntoa(*(struct in_addr *) &mreq.imr_interface.s_addr), ErrCode);

            return -1;
        }
}
        // This prevents loopback to this receivesocket even
        // if sending is from another socket

/*-----???? not when testing ---*/
        //PrintDbg("Disable IP_MULTICAST_LOOP\n");
        ttl = 0;

        result = setsockopt( Sock_id, IPPROTO_IP, IP_MULTICAST_LOOP,
                            (char *) &ttl, sizeof(char));
        if (result != 0)
        {
            PrintErr((int)DoseOs::Get_LastError(),
                    "Error at setsockopt(IP_MULTICAST_LOOP)");
            //return(INVALID_SOCKET);
        }
/*--------------*/
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
        if (multicastTtl > 1) ttl = multicastTtl;
        result = setsockopt( Sock_id,  IPPROTO_IP, IP_MULTICAST_TTL,
                            (char *) &ttl, sizeof(ttl));
        if (result != 0)
        {
            PrintErr((int)DoseOs::Get_LastError(),
                    "Error at setsockopt(IP_MULTICAST_TTL)");
            return -1;
        }

        //???? if not called, the default is used ???????
        // if INADDR_ANY, the default is used

        addr.s_addr = m_My_IpAddr_nw;

        result = setsockopt( Sock_id, IPPROTO_IP, IP_MULTICAST_IF,
                            (char *) &addr, sizeof(addr));
        if (result != 0)
        {
            PrintErr((int)DoseOs::Get_LastError(),
                    "Error at setsockopt(IP_MULTICAST_IF)");
            return -1;
        }
    }

    m_SockId = Sock_id;

    return(long)(Sock_id);
}
/*------------- end CreateIpMulticastSocket() -------------------*/

/********************************************************************
* The intention is to send header in one buffer and data in one buffer
*
* implementing scatter/gather type of I/O.
*
*********************************************************************/

int CIpmSocket::SendTo2(IPADDR IpAddr_nw,
                        unsigned short Port,
                        char *pBuf1, unsigned long Size1,
                        char *pBuf2, unsigned long Size2)
{
    int     result;
    struct sockaddr_in  sockAddr;
    char Buff[1600];


    if(*pDbg>3)
        if(Size2 != 0) // not KeepAlive
            PrintDbg("SendTo2() Ip=%X Size=%d/%d\n", IpAddr_nw, Size1, Size2);

    memcpy(Buff, pBuf1, Size1);
    if(Size2) memcpy(&Buff[Size1], pBuf2, Size2);

    sockAddr.sin_family      = AF_INET;
    sockAddr.sin_addr.s_addr = IpAddr_nw;
    sockAddr.sin_port        = htons(Port);

    result = sendto(m_SockId,  Buff, Size1+Size2, 0,
                    (struct sockaddr *) &sockAddr, sizeof(sockAddr));

    if(result == SOCKET_ERROR)
    {
        PrintErr(DoseOs::Get_LastError(),"SendTo2(). sendto() failed\n");
        return (-1);
    }

    return(0); //OK
}
/*------------------ end Socket_SendTo2() -----------------*/

#ifdef LINUX_IMPROVEMENT_SCATTERED_IO

int CIpmSocket::SendTo2(IPADDR IpAddr_nw,
                        unsigned short Port,
                        char *pBuf1, unsigned long Size1,
                        char *pBuf2, unsigned long Size2)
{
    int     result;
    struct  msghdr MsgHdr;  // see <sys/socket.h>
    struct  iovec IoVec[2];
    struct sockaddr_in  sockAddr;


    IoVec[0].iov_base = pBuf1;  // start addr
    IoVec[0].iov_len  = Size1;  // length
    IoVec[1].iov_base = pBuf2;
    IoVec[1].iov_len  = Size2;

    sockAddr.sin_family      = AF_INET;
    sockAddr.sin_addr.s_addr = IpAddr_nw;
    sockAddr.sin_port        = htons(Port);

    MsgHdr.msg_name     = (void *) &sockAddr;  // ??? optional ???
    MsgHdr.msg_namelen  = sizeof(sockAddr);
    MsgHdr.msg_iov      = IoVec;
    MsgHdr.msg_iovlen   = 2;

    // When sendmsg is called, msg_controllen should contain the length
    // of the available buffer in msg_control;
    //MsgHdr.msg_control    = ; // void*
    MsgHdr.msg_controllen   = 0;
    //MsgHdr.msg_flags      = 0;

    // Returns the number of bytes sent, or -1 if an error occurred.
    result = sendmsg(m_SockId, &MsgHdr, 0);

    if (result<0)
    {
        PrintDbg("Error on sendmsg() E=%d\n", errno);
        return (result);
    }
    return result;
}
#endif

/*************************************************************************
* The intention is to receive header in one buffer and data in one buffer
*
* Returns:
*  if Ok:    # received bytes
*  if error: -1
*************************************************************************/

int CIpmSocket::RecvFrom2(char *pBuf1, unsigned long Size1,
                            char *pBuf2, unsigned long Size2)
#ifdef _WIN32
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
    {
        //if(*pDbg>=1)
        //if(pBuf1[2] == (char) 0xD1)                   // MsgTypeData
        //  PrintDbg(">>> Rx FromId=%d SeqN=%u Frag=%X\n",
        //          pBuf1[3],                        // DoseId
        //          *(unsigned short *) &pBuf1[24],  // SequenceNumber
        //          *(unsigned short *) &pBuf1[22]); // FragmentNumber

        return(NumberOfBytesRecvd);
    }
    else
        return(-1);
}
#endif // _WIN32
//--------------------------------------------------
#ifdef _LINUX
{
    int     result;
    struct  msghdr MsgHdr;  // see <sys/socket.h>
    struct  iovec IoVec[2];


    IoVec[0].iov_base = pBuf1;  // start addr
    IoVec[0].iov_len  = Size1;  // length
    IoVec[1].iov_base = pBuf2;
    IoVec[1].iov_len  = Size2;

    //msg_name may be given as a null pointer if no names are desired or required.
    MsgHdr.msg_name     = NULL; // (void *) &SockAddr;  // ??? optional ???
    MsgHdr.msg_namelen  = 0;    // sizeof(SockAddr);
    MsgHdr.msg_iov      = IoVec;
    MsgHdr.msg_iovlen   = 2;

    // When recvmsg is called, msg_controllen should contain the length
    // of the available buffer in msg_control;
    //MsgHdr.msg_control    = ; // void*
    MsgHdr.msg_controllen   = 0;
    //MsgHdr.msg_flags      = 0;

    // Returns the number of bytes received, or -1 if an error occurred.
    result = recvmsg(m_SockId, &MsgHdr, 0);

    //??? timed out seems to be 11 ?????
    if (result<0)
    {
        if(errno == EWOULDBLOCK) return(0);
        return result;  //??????????
        //PrintDbg("Error on recvmsg() E=%d\n", errno);
    }
    return result;
}
#endif // _LINUX

/*********************************************************************
* Check if there are any pending messages on the socket
*
* Expected result: (verified by test)
* result = 0
* WIN32: param = number of bytes on the socket (sum of all msg)
* LINUX: param = number of bytes in the first msg on the socket
*                0 if no messages
*
* Returns: 0 if no pending messages
********************************************************************/
int CIpmSocket::AreThereAnyPendingRxMessages(void)
{
    int result;
    unsigned long param = 0;

#ifdef _WIN32
    result = ioctlsocket(m_SockId, FIONREAD, &param);
#endif
#ifdef _LINUX
    result = ioctl(m_SockId, FIONREAD, &param);
#endif
    if (result < 0)
    {
        PrintDbg("Cannot get ioctl FIONREAD.\n");
        exit(EXIT_FAILURE);
    }

    return(param); // 0 if nothing to read
}


/*-------------- end IpmSocket.cpp() ------------*/
