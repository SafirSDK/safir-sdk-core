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

/**************************************************************
* DoseSniffer.cpp - for LINUX and WIN32
*
* Function: See PrintHelp()
*
* Uses raw sockets which requires admin/root privilieges.
*
* Improvements:
* 2) Metod to make both in Win32 ????
* 3) TimeStamp
* 4) Output to file (and stdout)
* 5) Implement outgoing messages in Linux
*
*
* Compile without -DSNIFFER_ALL_MSG to create a prog that receives
* and displays DOSE messages only.
*
* Compile with -DSNIFFER_ALL_MSG to create a prog that receives
* and displays all UDP messages.
*
* LINUX: Compile as: g++ DoseSniffer.cpp -lrt
*
* Note: For Linux, only received messages are shown.
***************************************************************/

#ifndef SNIFFER_ALL_MSG
#define DOSE_SNIFFER    // Listen only to Dose messages
#endif

#ifdef _WIN32 //-----------------
#include <winsock2.h>
#include <windows.h>
#include "mstcpip.h"
#include "iphlpapi.h"
#include <stdio.h>
#include <stdlib.h>

typedef int socklen_t;

#elif defined(linux) || defined(__linux) || defined(__linux__)

#ifndef _LINUX
#define _LINUX   // used below
#endif

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <errno.h>
#include <string.h>  // bzero()
#include <unistd.h>  // close()

#define closesocket(xx) close(xx)

typedef unsigned long  SOCKET;
typedef unsigned long  DWORD;
typedef unsigned short WORD;
#define INVALID_SOCKET -1
#define SOCKET_ERROR -1
#define WSAGetLastError() errno

#else
#  error "Unable to work out platform"
#endif

#ifdef DOSE_SNIFFER
#include "../defs/DoseUdpMsg.h"

#endif  //---------------------

//-----------------------------------
// Global variables
//-----------------------------------

#ifdef DOSE_SNIFFER
static int g_ShowKeepAlive = 0;
#endif

#ifndef DOSE_SNIFFER
static int g_ShowHeader = 0;
static int g_MaxDataLength = 20;
static int g_ShowDataAsText = 0;
#endif

static int Debug = 0;
static int g_OutputFormat = 0;

static int g_Enable_Promiscuous_Mode = 1; // does not work for =0;

/*****************************************************************
*
******************************************************************/
static void PrintHelp(void)
{
    printf("-----------------------------------------------------------08-01-23----\n");
#ifdef DOSE_SNIFFER
    printf("DoseSniffer - receive and display DOSE network messages\n");
#else
    printf("Sniffer     - receive and display UDP network messages\n");
#endif
#ifdef _LINUX
    printf("              For Linux only incomming messages are shown\n");
#endif

    printf("------------------------------------------------------------------------\n");
    printf("Options:\n");
    printf("-F0         Output format 0. (default)\n");
    printf("-F1         Output format 1.\n");
#ifdef zzzzz_WIN32 // fungerar ej
    printf("\n");
    printf("-P          Set to Promiscuous Mode. (This tells the NIC to receive everything)\n");
    printf("            In this mode received and transmitted messages are displayed.\n");
    printf("            By default, only received messages are displayed.\n");
#endif
#ifdef _WIN32
    printf("\n");
    printf("-I=<IpAddr> Define IpAddress to us on multihomed machines.\n");
#endif

#ifdef DOSE_SNIFFER
    printf("\n");
    printf("-K          Show DOSE KeepAlive messages\n");
#else
    printf("\n");
    printf("-H          Show IP/UDP header\n");
    printf("-T          Show data as text (if text)\n");
    printf("-L=<length> Show max this length of data. Default=%d\n", g_MaxDataLength);
#endif
    printf("------------------------------------------------------------------------\n");
}

/*****************************************************************
* Create a RAW socket.
*
* Returns: socket number or INVALID_SOCKET
********************************************************************/
static SOCKET create_socket(DWORD IpAddr_nw)
{
    struct sockaddr_in  sname;
    SOCKET              sock_id;
    unsigned long       ioctl_cmd = 111;
    int                 result;
    DWORD dwBufferInLen= 1 ;
    DWORD dwBytesReturned = 0 ;
    socklen_t Opt_So_Rcvbuf_Size = 200000;
    socklen_t tmp;

    //--------------------
    // Create a raw socket
    //--------------------

#ifdef _WIN32
    sock_id = socket (AF_INET, SOCK_RAW, IPPROTO_IP); //not supported in LINUX
#endif
#ifdef _LINUX
    sock_id = socket (AF_INET, SOCK_RAW, IPPROTO_UDP); // OK
#endif

    if( sock_id == INVALID_SOCKET )
    {
        perror("socket() ==> ");
        printf("socket() failed %d. ", WSAGetLastError());
        return((SOCKET) INVALID_SOCKET);
    }

    //--------------------
    // Bind
    //--------------------

    sname.sin_family      = AF_INET;
    sname.sin_port        = 0;

#ifdef _LINUX
    sname.sin_addr.s_addr = 0;  // no difference
    //sname.sin_addr.s_addr = IpAddr_nw;
#endif
#ifdef _WIN32
    sname.sin_addr.s_addr = IpAddr_nw;  // INADDR_ANY does not work for WIN32
#endif

    //printf("bind() to IP = %X\n",htonl(IpAddr_nw));

    if( bind(sock_id,(struct sockaddr *) &sname,sizeof(sname)) == SOCKET_ERROR )
    {
#ifdef _WIN32
        printf("bind(%s) failed %d\n",inet_ntoa(* (struct in_addr *) &IpAddr_nw),
                WSAGetLastError());
#endif
#ifdef _LINUX
        printf("bind() failed %d\n", WSAGetLastError());
#endif
        (void) closesocket (sock_id);
        return((SOCKET)INVALID_SOCKET);
    }

    //------------------------------
    // Setr sockets receive buffer
    //------------------------------

    result = setsockopt( sock_id,  SOL_SOCKET, SO_RCVBUF,
                            (char *) &Opt_So_Rcvbuf_Size, sizeof(long));
    if(result == -1)
    {
        printf("setsockopt(SO_RCVBUF) failed. E=%d\n", WSAGetLastError());
    }

    Opt_So_Rcvbuf_Size = 0;
    tmp = 4;
    getsockopt(sock_id,SOL_SOCKET,SO_RCVBUF,(char *)&Opt_So_Rcvbuf_Size,&tmp);
    //printf("SO_RCVBUF ==> %d\n", Opt_So_Rcvbuf_Size);

#ifdef _WIN32
    DWORD dwBufferLen[10] ;

    // Enable reciving all messages (all port numbers)
    // This sets the NIC to promiscuous mode.
    // It also enables receiving outgoing data
    if(g_Enable_Promiscuous_Mode)
    {
        result = WSAIoctl(sock_id, SIO_RCVALL, &dwBufferInLen,
                            sizeof(dwBufferInLen),
                            &dwBufferLen, sizeof(dwBufferLen),
                            &dwBytesReturned , NULL , NULL );

        if(result == SOCKET_ERROR)
        {
            printf("WSAIoctl() failed %d. ", WSAGetLastError());
            closesocket(sock_id) ;
            return((SOCKET)INVALID_SOCKET);
        }
    }
#endif // _WIN32

#ifdef zzzzzz_LINUX // To be implemented when I know how to do it
    unsigned int dwBufferLen[10] ;

    // Enable reciving all messages (all port numbers)
    // This sets the NIC to promiscuous mode.
    // It also enables receiving outgoing data
    if(g_Enable_Promiscuous_Mode)
    {
        result = Ioctl(sock_id, SIO_RCVALL, &dwBufferInLen,
                            sizeof(dwBufferInLen),
                            &dwBufferLen, sizeof(dwBufferLen),
                            &dwBytesReturned , NULL , NULL );

        if(result == SOCKET_ERROR)
        {
            printf("WSAIoctl() failed %d. ", WSAGetLastError());
            closesocket(sock_id) ;
            return((SOCKET)INVALID_SOCKET);
        }
    }
#endif // _LINUX

    return(sock_id);
}
/*-----------------end create_socket() ----------------------*/

/****************************************************************
* Only for WIN32 since it is not needed for LINUX
*****************************************************************/
#ifdef _WIN32
static DWORD Get_IpAddr()
{
    // Here read all IPs of this host
    DWORD dwSize = 0 ;
    int bIsMultihomed = 0;


    GetIpAddrTable( NULL , &dwSize, FALSE ) ;

    PMIB_IPADDRTABLE pIpAddrTable = (PMIB_IPADDRTABLE )new BYTE [ dwSize ] ;

    if( pIpAddrTable )
    {
        if(GetIpAddrTable((PMIB_IPADDRTABLE)pIpAddrTable, // buff for IP table
                            &dwSize,    // size of buffer
                            FALSE )     // sort by IP address
                                 == NO_ERROR)
        {
            //Second is MS TCP loopback IP ( 127.0.0.1 )
            if( pIpAddrTable->dwNumEntries > 2 )
            {
                bIsMultihomed = TRUE ;
                for( int i = 0 ; i < (int)pIpAddrTable->dwNumEntries ; i++ )
                {
                    in_addr ina;
                    ina.S_un.S_addr = pIpAddrTable->table[i].dwAddr ;
                }
            }
        }
        delete [] pIpAddrTable ;
    }
    return(pIpAddrTable->table[0].dwAddr);
}
#endif // _WIN32

/**********************************************************
* SIO_RCVALL
* Enables a socket to receive all IP packets on the network.
* The socket handle passed to the WSAIoctl function must be of
* AF_INET address family, SOCK_RAW socket type, and IPPROTO_IP
* protocol. The socket also must be bound to an explicit local
* interface, which means that you cannot bind to INADDR_ANY.
* Once the socket is bound and the ioctl set, calls to the
* WSARecv or recv functions return IP datagrams passing through
* the given interface. Note that you must supply a sufficiently
* large buffer. Setting this ioctl requires Administrator privilege
* on the local machine. SIO_RCVALL is available in Windows 2000 and
* later versions of Windows.
*
* First is IP header 20 bytes, then UDP hdr 8 bytes, then data
*
* -------------------------------IP hdr
* 00:
* 01:
* 02
* 09: IP_PROTO - 0x11 = UDP
* 12: Src IPadr
* 16: Dst IPadr
* -------------------------------Udp Hdr starts at 20
* 20,21: src port
* 22,23: dst port
* -------------------------------Data starts at 28
*
***********************************************************/
static void Sniffer(char *pStrIpAddr)
{
    int     result;
    int     jj;
    DWORD   dwErr;
    DWORD   IpAddr_nw;
    SOCKET  SockId;
    unsigned char   RxBuff[2000];
    struct sockaddr_in fromAddr;
    char *pIpAddr;
    WORD ToPort;
    DWORD MsgNum = 0;
    socklen_t   fromLength;

#ifdef DOSE_SNIFFER
    DOSE_UDP_MSG_HDR *pDoseMsg;
    DOSE_UDP_ACK_MSG *pDoseAckMsg;

    pDoseMsg    = (DOSE_UDP_MSG_HDR *)  &RxBuff[28];
    pDoseAckMsg = (DOSE_UDP_ACK_MSG *)  &RxBuff[28];
#else
    int ShowText;
    int max;
#endif

#ifdef _WIN32

    if(pStrIpAddr == NULL)
    {
        IpAddr_nw = Get_IpAddr();
        pIpAddr = inet_ntoa( * (struct in_addr *) &IpAddr_nw ) ;
    }
    else
    {
        IpAddr_nw = inet_addr(pStrIpAddr);
        pIpAddr = inet_ntoa( * (struct in_addr *) &IpAddr_nw ) ;
    }
#endif

#ifdef _LINUX
    // Dummy - not used in LINUX
    pIpAddr   = (char *) "192.168.0.111";
    IpAddr_nw = inet_addr(pIpAddr); // not used in Linux
#endif

#ifdef DOSE_SNIFFER
    printf("-----------------------------------------------------------\n");
    printf("Sniffing Dose messages. Data and Ack messages are shown.\n");
    printf("-----------------------------------------------------------\n");
    printf("Id  = From DoseId\n");
    printf("Sq  = Sequence number (dec)\n");
    printf("Fr  = Fragment number (hex) - Shown if not zero.\n");
    printf("Inf = (if present) retransmitted msg\n");
    printf("(s=nnn) = Size of data in the message (hex)\n");
    printf("-----------------------------------------------------------\n");
#ifdef _WIN32
    printf("My IpAddr = %s\n", pIpAddr);
    printf("-----------------------------------------------------------\n");
#endif
#else
    printf("-----------------------------------------------------------\n");
    printf("Sniffing UDP messages\n");
#ifdef _WIN32
    printf("Using Adapter with IpAddr = %s\n", pIpAddr);
#endif
    if(g_Enable_Promiscuous_Mode)
    printf("Promiscuous Mode - Shows received and transmitted messages.\n");
    else
    printf("Shows only received messages, NOT transmitted messages.\n");
    printf("-----------------------------------------------------------\n");
#endif

    SockId = create_socket(IpAddr_nw);

    for(;;)
    {
        if(Debug) printf("Call recvfrom\n");
       fromLength = sizeof(fromAddr);

#ifdef _LINUX
        bzero(&fromAddr,fromLength);
#endif
#ifdef _WIN32
        memset(&fromAddr,0,fromLength);
#endif

        result = recvfrom(SockId, (char *) RxBuff, sizeof(RxBuff), 0,
                            (struct sockaddr *) &fromAddr, &fromLength);
        if(Debug) printf("Ret recvfrom res = %d\n", result);
        if(result == SOCKET_ERROR)
        {
            //perror("recvfrom() ==>\n");
            dwErr = WSAGetLastError();

            //printf("recvfrom Error = %d\n", dwErr);

#ifdef _WIN32
            if( dwErr == WSAETIMEDOUT) continue;
            printf("recvfrom Error = %d\n", dwErr);
            Sleep(1000);
#endif
            return;
            continue;
        }

        //===============================
        // Filter
        //===============================

        if(RxBuff[9] != 0x11) continue; // Ignore NON-UDP

        ToPort = ((RxBuff[22]<<8) & 0xFF00) | (RxBuff[23] & 0xFF);

#ifdef DOSE_SNIFFER
        if(pDoseMsg->Magic != DOSE_MSG_MAGIC) continue;
        if((ToPort < 6970) || (ToPort > 6976)) continue;
        if(!g_ShowKeepAlive && (ToPort == 6970)) continue;
        // could add more filter functions here
#endif

        //===============================
        // Print IP/UDP headers
        //===============================
        // Ip-hdr byte 0-19
        //  printf("Rx:(%d)\nIpHdr:  ", result-28);

        //  for(jj=0;jj<20;jj++)
        //      printf("%02X ", RxBuff[jj] & 0xFF);

        // Udp-hdr 20-27
        //  printf("\nUdpHdr: ");
        //      for(jj=20;jj<28;jj++) printf("%02X ", RxBuff[jj] & 0xFF);

        //===============================
        // Print source and destination
        //===============================
        if(g_OutputFormat == 1)
        {
            //printf("%u --------------------------------%s", MsgNum++,
            printf("%lu IPADR: %s", MsgNum++,
                    inet_ntoa(*(struct in_addr *) &RxBuff[12]));

            // Dst IpAddr:Port
            printf("-->%s:%d (s=%d)\n",
                    inet_ntoa(*(struct in_addr *) &RxBuff[16]), // IpAdr
                    (WORD) ((RxBuff[22]<<8) + RxBuff[23]),      // Port
                    result-28);                                 // msg length (data part)
        }
        else // default
        {
            printf("%3lu %-15s",MsgNum++,
                inet_ntoa(*(struct in_addr *) &RxBuff[12]));

            // Dst IpAddr:Port
            printf("-->%-15s:%d ",
                    inet_ntoa(*(struct in_addr *) &RxBuff[16]), // IpAdr
                    (WORD) ((RxBuff[22]<<8) + RxBuff[23]));      // Port
        }

        //===============================
        // Print Data
        //===============================
// Note on Linux the header seems to come on 28 ???
#ifndef DOSE_SNIFFER
        // Header
        if(g_ShowHeader)
        {
          printf("HDR : ");
          for(jj=0;jj<28;jj++) printf("%02X ", RxBuff[jj] & 0xFF);
          printf("\n");
        }
        // Data
        max = result - 28;
        if(max>g_MaxDataLength) max = g_MaxDataLength;

        printf("DATA:  ");
        for(jj=28;jj<(max+28);jj++)
            printf("%02X ", RxBuff[jj] & 0xFF);
        printf("\n");

       // Data as Text
       if(g_ShowDataAsText)
       {
            ShowText = 1;
            for(jj=28 ; jj<result ; jj++)
                if(RxBuff[jj] < 8) { ShowText = 0; break;}
            if(ShowText)
            {
                RxBuff[result] = 0;
                printf("%s\n",&RxBuff[28]);
            }
       }
#endif
        //===============================
        // Dose Parser
        // DoseId, Type, Seq, Frag
        //===============================

#ifdef DOSE_SNIFFER
        switch(pDoseMsg->MsgType)
        {
            case MSG_TYPE_DATA:
                if(pDoseMsg->FragmentNumber != 0)
                {
                    if(pDoseMsg->Info != 0)
                        printf("DoseData Id=%2d Sq=%3d Fr=%4X Inf=%X\n",
                                pDoseMsg->DoseIdFrom, pDoseMsg->SequenceNumber,
                                pDoseMsg->FragmentNumber, pDoseMsg->Info);
                    else
                        printf("DoseData Id=%2d Sq=%3d Fr=%4X\n",
                                pDoseMsg->DoseIdFrom, pDoseMsg->SequenceNumber,
                                pDoseMsg->FragmentNumber);
                }
                else
                    if(pDoseMsg->Info != 0)
                        printf("DoseData Id=%2d Sq=%3d Inf=%X\n",
                                pDoseMsg->DoseIdFrom, pDoseMsg->SequenceNumber,
                                pDoseMsg->Info);
                    else
                        printf("DoseData Id=%2d Sq=%3d\n",
                                pDoseMsg->DoseIdFrom, pDoseMsg->SequenceNumber);

                    break;

            case MSG_TYPE_KEEPALIVE:
                printf("DoseKeepAlive Id=%2d\n",    pDoseMsg->DoseIdFrom);
                break;

            case MSG_TYPE_ACK:
                printf("DoseAck  Id=%2d Sq=%3d Fr=%4X\n",
                        pDoseAckMsg->DoseIdFrom, pDoseAckMsg->SequenceNumber,
                        pDoseAckMsg->FragmentNumber);
                break;

            case MSG_TYPE_NACK:
                printf("DoseNack Id=%2d Sq=%3d Fr=%4X\n",
                        pDoseAckMsg->DoseIdFrom, pDoseAckMsg->SequenceNumber,
                        pDoseAckMsg->FragmentNumber);
                break;

            default:
                printf("???????? Data:   ");
                for(jj=28;jj<48;jj++) printf("%02X ", RxBuff[jj] & 0xFF);
                printf("\n");
                break;
        }
#endif
    }
}
/**********************************************************
*
***********************************************************/
int main(int argc, char **argv)
{
    int     an;
    int     result;
    char    *pIpAddr = NULL;

#ifdef _WIN32
    WSADATA WSAData;

    if ((result = WSAStartup(MAKEWORD(1,1) ,&WSAData )) != 0)
    {
        printf("WSAStartup failed %d\n", result);
        return(-1);
    }
#endif

    for(an=1 ; an<argc ; an++)
    {
        char *p = argv[an];
        if (p[0] == '-') p++;

        switch (p[0] & 0xDF)
        {
            case 'D': Debug++;  break;
            case 'F': g_OutputFormat = p[1] - '0'; break;
            case 'P': g_Enable_Promiscuous_Mode = 1; break;
            case 'I': pIpAddr = &p[2]; break;

#ifndef DOSE_SNIFFER
            case 'H': g_ShowHeader = 1; break;
            case 'T': g_ShowDataAsText = 1; break;
            case 'L':
                g_MaxDataLength = atoi(&p[2]);
                printf("Max data length to display = %d\n",g_MaxDataLength);
                break;
#endif
#ifdef DOSE_SNIFFER
            case 'K':   g_ShowKeepAlive = 1; break;
#endif
            default:
                PrintHelp(); return(-1);
        } // end switch
    } // end while

    Sniffer(pIpAddr);

    return(result);
}
/*------- end main() ------------------------*/



