/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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

/***************************************************************************
* DoseErrLog.cpp - for LINUX and WIN32
*
* Improvements:
* - Add param to print timestamp
* - add param to log to file
* - add print help
*
* Compile as: g++ -D_LINUX -o DoseErrLog DoseErrLog.cpp -lrt
*
* Tested OK on Linux 07-12-07 (after that it has been modified)
* Tested OK on WIN32 07-12-19
*
*****************************************************************************/

#ifdef _WIN32
#include <winsock2.h>
#include <windows.h>
#include "mstcpip.h"
#include "iphlpapi.h"

typedef int socklen_t;
#include <stdio.h>

#elif defined(linux) || defined(__linux) || defined(__linux__)

// ??? check what is needed ???
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>  // close()

typedef unsigned long SOCKET;
typedef unsigned long DWORD;
typedef unsigned short WORD;

#define closesocket(xx) close(xx)
#define INVALID_SOCKET -1
#define SOCKET_ERROR -1
#define WSAGetLastError() errno

#else
#  error "Unable to work out platform"
#endif

#define PRINTLOG_PORT 31221 // must agree with PrintErr.h

/*****************************************************************
* Creates a socket.
*
* Returns: socket number or INVALID_SOCKET
********************************************************************/
static SOCKET create_socket(void)
{
    struct sockaddr_in  sname;
    SOCKET              sock_id;
    unsigned long       ioctl_cmd = 111;
    int                 result;
    DWORD dwBytesReturned = 0 ;
    socklen_t Opt_So_Rcvbuf_Size = 100000;
    socklen_t tmp;

    sock_id = socket (AF_INET, SOCK_DGRAM, 0);
    if( sock_id == INVALID_SOCKET )
    {
        perror("socket() ==> ");
        printf("socket() failed %d. ", WSAGetLastError());
        return((SOCKET) INVALID_SOCKET);
    }

    // Bind
#ifdef _LINUX
    bzero(&sname, sizeof(sname));
#endif
#ifdef _WIN32
    memset(&sname, 0, sizeof(sname));
#endif

    sname.sin_family      = AF_INET;
    sname.sin_addr.s_addr = INADDR_ANY;
    sname.sin_port        = htons(PRINTLOG_PORT);

    if( bind(sock_id,(struct sockaddr *) &sname,sizeof(sname)) == SOCKET_ERROR )
    {
        printf("bind() failed %d\n", WSAGetLastError());
        (void) closesocket (sock_id);
        return((SOCKET)INVALID_SOCKET);
    }

    // Set sockets receive buffer - OK on WIN32
//#ifdef _WIN32
    result = setsockopt( sock_id,  SOL_SOCKET, SO_RCVBUF,
                            (char *) &Opt_So_Rcvbuf_Size, sizeof(long));
    if(result == -1)
    {
        printf("setsockopt(SO_RCVBUF) failed. E=%d\n", WSAGetLastError());
    }
//#endif

    Opt_So_Rcvbuf_Size = 0;
    tmp = 4;
    getsockopt( sock_id,  SOL_SOCKET, SO_RCVBUF, (char *) &Opt_So_Rcvbuf_Size, &tmp);
    printf("ReceiveBuffer = %d\n", Opt_So_Rcvbuf_Size);

    return(sock_id);
}
/*-----------------end create_socket() ----------------------*/

/**********************************************************
*
***********************************************************/
static void Logger(void)
{
    int     result;
    DWORD   dwErr;
    SOCKET  SockId;
    char    RxBuff[2000];
    struct sockaddr_in fromAddr;
    socklen_t fromLength;


    printf("DoseLogger.\n");
    SockId = create_socket();

    printf("Start receiving debug messages from Dose\n");
    printf("-----------------------------------------------\n");

    for(;;)
    {
        fromLength = sizeof(fromAddr);
        result = recvfrom(SockId, RxBuff, sizeof(RxBuff), 0,
                            (struct sockaddr *) &fromAddr, &fromLength);
        //printf("Ret recvfrom res = %d\n", result);
        if(result == SOCKET_ERROR)
        {
            perror("recvfrom() ==>\n");
            dwErr = WSAGetLastError();
            printf("recvfrom Error = %lu\n", dwErr);
            return;
            continue;
        }

        // Print Data

        RxBuff[result] = 0;
        printf("%s",RxBuff);
    }
}
/**********************************************************
*
***********************************************************/
int main(int argc, char **argv)
{
    int     an;
    int     result;

#ifdef _WIN32
    WSADATA WSAData;

    if ((result = WSAStartup(MAKEWORD(1,1) ,&WSAData )) != 0)
    {
        printf("WSAStartup failed %d\n", result);
        return(-1);
    }
#endif

    an = 0;
    while ((++an < argc) && !result)
    {
        switch (argv[an][1])
        {
            case '?':   break; //print_help(); break;
            //case 'd': Debug++;  break;

            default:
                printf("Invalid '-' command\n");
                return(1); // exit(1);
        } // end switch
    } // end while

    Logger();  // Returns only if some error

    return(result);
}
/*----------------- end DoseErrLog.cpp ------------------------*/



