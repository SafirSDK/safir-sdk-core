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

#include <windows.h>
#include <stdio.h>
#include <stdarg.h>
#include <process.h>

#define LOG_PORT 31221

/**********************************************************
* Args as printf
***********************************************************/

void PrintDbg( const char *format, ... )
{
    va_list marker;
    char    buffer[1024];
    int     Length;
    DWORD   NumberOfCharsWritten;
    static  HANDLE hConsoleOutput = INVALID_HANDLE_VALUE;


    va_start( marker, format );     /* Initialize variable arguments. */
    _vsnprintf(buffer, sizeof(buffer), format, marker);
    va_end( marker );

    Length = strlen(buffer);

    if(hConsoleOutput == INVALID_HANDLE_VALUE)
    {
        AllocConsole();
        hConsoleOutput = GetStdHandle(STD_OUTPUT_HANDLE);
    }

    WriteFile(hConsoleOutput,buffer,Length,
                &NumberOfCharsWritten, NULL);
}

/*********************************************************************
* Create a socket for receive data.
*
* returns: ERROR   if some error
*          sock_id if OK
**********************************************************************/
static int create_receive_socket(void)
{
   int      sock_id;
   int      result;
   long     Opt_So_Rcvbuf_Size = 200000;
   struct sockaddr_in sock_name;


   //if (Debug>1) printf("create_receive_socket\n");

   sock_id = socket(AF_INET,SOCK_DGRAM,0 );
   if (sock_id == -1)
   {
       PrintDbg("ERROR: Can't create socket %d\n",WSAGetLastError());
       return(-1);
   }

   sock_name.sin_family      = AF_INET;
   sock_name.sin_addr.s_addr = INADDR_ANY;
   sock_name.sin_port        = htons(LOG_PORT);

   result = bind(sock_id, (struct sockaddr *) &sock_name, sizeof(sock_name));
   if (result <0 )
   {
      PrintDbg("ERROR: Can't bind()\n");
      return(-1);
   }

    result = setsockopt(sock_id,  SOL_SOCKET, SO_RCVBUF,
                            (char *) &Opt_So_Rcvbuf_Size, sizeof(long));
    if(result == -1)
    {
        PrintDbg("setsockopt(SO_RCVBUF) failed");
    }

   return(sock_id);
}
/*------------- end create_receive_socket() ----------------*/


/****************************************************
*
*****************************************************/
static void Log_Thread(void *)
{
    int             sock_id;
    int             count;
    int             result = 0;
    struct sockaddr from_addr;
    int             fromlength;
    WSADATA         WSAData;
    char            RxBuff[1500];

    if (WSAStartup(MAKEWORD(2,2) ,&WSAData ) != 0) // start WinSock
    {
        PrintDbg("WSAStartup failed %d\n", result);
        return;
    }


    sock_id = create_receive_socket();
    if (sock_id < 0) return;

    /*---------------------------------------------
    * Loop forever, receive and display messages
    * If the first bye==0,then it is aspecial msg
    * else it is text
    *---------------------------------------------*/

    for (;;)
    {
        fromlength = sizeof(from_addr);

        count = recvfrom (sock_id, RxBuff, sizeof(RxBuff), 0,
                       (struct sockaddr *) &from_addr, &fromlength);
        if (count < 0 )
        {
            Sleep(1000);
            //printf("ERROR on recvfrom()\n");
            //return(-1);
        }
        RxBuff[count] = 0;
        PrintDbg("%s", RxBuff);
    }
}

/****************************************
*
*****************************************/
void Start_Log_Thread(void)
{
    _beginthread( &Log_Thread, 0, NULL);
}
/*------------------------ end  ---------------------*/
