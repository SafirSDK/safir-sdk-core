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

/*********************************************************************
* TinyWebSrv.CPP
*
* Contains:
* A C++ class with a tiny Webb Server that receives messages from a
* browser and calls an application callback routine.
**********************************************************************/

#include <windows.h>
#include <stdio.h>
#include <process.h>
#include "TinyWebSrv.h"

int Debug_Http = 0;

#define RUN_AS_A_THREAD
/***********************************************************
* Test With MicroSoft Webserver ==>
* These extensions: DLL, .EXE, .CLASS, no extension, .BAT
* gives: Content-Type: application/octet-stream
* Read RFC2616 to learn about this
************************************************************/
//????? use this ??????
static char *Get_ContentType(const char *pRealPath)
{
    int     size = strlen(pRealPath);
    char    *text_plain = "text/plain";
    char    *text_css   = "text/css";
    char    *text_html  = "text/html";
    char    *application_octet_stream = "application/octet-stream";
    const char  *ptr;


    // ---- Test if format ".x" ---

    if (size<2) return(application_octet_stream);

    if(pRealPath[size - 2] == '.')  // e.g. ".H"
    {
        ptr = &pRealPath[size - 2];
        switch(ptr[1] & 0xDF)
        {
            case 'H': // ".H"
            case 'C': // ".C"
                return(text_plain);
                break;
        }
        return("application/octet-stream");
    }

    // ---- Test if format ".xx" ---

    if (size<4) return(application_octet_stream);

    if(pRealPath[size - 3] == '.')
    {
        ptr = &pRealPath[size - 3];
        switch(ptr[1] & 0xDF)
        {
            case 'J':
                if (_strnicmp(ptr, ".JS",3) == 0) return(text_html);
                break;
        }
        return("application/octet-stream");
    }

    // ---- Test if format ".xxx" ---

    if (size<5) return(application_octet_stream);

    if(pRealPath[size - 4] == '.')
    {
        ptr = &pRealPath[size - 4];
        switch(ptr[1] & 0xDF)
        {
            case 'H':
                if (_strnicmp(ptr, ".HTM",4) == 0) return(text_html);
                break;
            case 'T':
                if (_strnicmp(ptr, ".TXT",4) == 0) return(text_plain);
                break;
            case 'L':
                if (_strnicmp(ptr, ".LOG",4) == 0) return(text_plain);
                break;
            case 'C':
                if (_strnicmp(ptr, ".CFG",4) == 0) return(text_plain);
                if (_strnicmp(ptr, ".CPP",4) == 0) return(text_plain);
                if (_strnicmp(ptr, ".CSS",4) == 0) return(text_css);
                break;
            case 'G':
                if (_strnicmp(ptr, ".GIF",4) == 0) return("image/gif");
                break;
            case 'J':
                if (_strnicmp(ptr, ".JPG",4) == 0) return("image/jpg");
                break;
        }
        return("application/octet-stream");
    }

    // ---- Test if format ".xxxx" ---

    if (size<6) return(application_octet_stream);

    if(pRealPath[size - 5] == '.')
    {
        ptr = &pRealPath[size - 5];
        switch(ptr[1] & 0xDF)
        {
            case 'H':
                if (_strnicmp(ptr, ".HTML",5) == 0) return(text_html);
                break;
            case 'J':
                if (_strnicmp(ptr, ".JAVA",5) == 0) return(text_plain);
                break;
        }
        return("application/octet-stream");
    }

    // default
    return(application_octet_stream);
}

/*****************************************************************OK
* public
*
* This is the first part of the message to the Browser
* This msg MUST be terminated by "\r\n\r\n"
*
* TODO: add lots of parameters
* Examples picked up from Internet
*----------------------------------
* A HTML file
* HTTP/1.1 200 OK
* Date: Wed, 27 Sep 2000 11:21:10 GMT
* Server: Apache/1.3.3
* Connection: close
* Content-Type: text/html
*
******************************************************************/
int CTinyWebSrv::Send_HttpHeader(SOCKET SockId, DWORD UrlStatus,
                                 const char *pExt) // defines filetype
{
    int     size;
    int     result;
    int     bytes_left;
    char    buffer[512];


    sprintf(buffer, "HTTP/1.1 %d OK\r\n", UrlStatus);
    size = strlen(buffer);

    //sprintf(&buffer[size],"Content-Type: text/html\r\n");

    sprintf(&buffer[size],"Content-Type: %s\r\n", Get_ContentType(pExt));
    size = strlen(buffer);

    strcat(&buffer[size],"\r\n");
    size = strlen(buffer);
    bytes_left = size;

    do
    {
        result = send(SockId,&buffer[(int)size - bytes_left],bytes_left,0);
        if (result < 0)
        {
            if ((WSAGetLastError()) == WSAEWOULDBLOCK )
            {
                Sleep(100);  // no idea to send too soon if buffer full
                continue;
            }
            else
                return(-1);
        }
        bytes_left -= result;
    } while (bytes_left > 0);
    return(0);
}
/*----------------- end CTinyWebSrv::Send_HttpHeader() ----------*/

/*****************************************************************OK
* private
* Almost like send() but handles case when all bytes not sent.
*
* Returns: 0 if OK, -1 if error
******************************************************************/

int CTinyWebSrv::SendText(SOCKET SockId, const char *pMsg, int size)
{
    int     result;
    int     bytes_left;


    if (pMsg == NULL) return(0);

    bytes_left = size;

    do
    {
        result = send( SockId,&pMsg[(int)size - bytes_left],bytes_left,0);

        if (result < 0)
        {
            if ((WSAGetLastError()) == WSAEWOULDBLOCK )
            {
                Sleep(100);  // no idea to send too soon if buffer full
                continue;
            }
            else
                return(-1);
        }
        bytes_left -= result;
    } while (bytes_left > 0);
    return(0);
}
/*---------------- end SendText() ---------------------*/

/***************************************************
* public
*
* send this text
*****************************************************/

void CTinyWebSrv::SendMsg(SOCKET SockId, const char *format, ... )
{
    va_list marker;
    char    buffer[1024];

    va_start( marker, format ); //Initialize variable arguments.

    _vsnprintf(buffer, sizeof(buffer), format, marker);

    va_end( marker ); /* needed ? Reset variable arguments */

    SendText(SockId, buffer, strlen(buffer));
}

/**************************************************************
* public
*
* Send a file to WebServer
*
* Returns -1 if error
*          0 if OK with Num sent bytes in *pTotalCount.
***************************************************************/

int CTinyWebSrv::SendFile(SOCKET SockId, const char *pFileName)
{
    int     result;
    int     ErrCode;
    char    buffer[1024];
    FILE    *pF;

    pF = fopen(pFileName,"r");
    if(pF == NULL)
    {
        ErrCode = GetLastError();
        printf("ERROR Can not open <%s>\n",pFileName);
        Send_HttpHeader(SockId, 400, "xxx.htm");

        SendMsg(SockId,
            "<HTML><BODY>\r\n"
            "ERROR Can not open file %s.<BR>"
            "Error code = %d</BODY></HTML>",
            pFileName, ErrCode);
        goto IsDone;
    }

    Send_HttpHeader(SockId, 200, pFileName); // onle extens used

    for(;;)
    {
        result = fread(buffer,1,1024, pF);

        if (result < 0)
        {
            break; //return(-1);
        }
        if(result == 0) break;

        result = SendText(SockId, buffer, result);

        if(result < 0)
        {
            return(-1); // some error
        }
    }

IsDone:
    if(pF) fclose(pF);
    return(0);
}
/*-------------------- end SendFile() --------------------------*/

/****************************************************************
* sends a small HTML page.
* Header with title, then 4 lines pMsgx (if != NULL), trailer.
******************************************************************/

//void  CTinyWebSrv::Send_MsgPage(const char *pMsg1,
//                           const char *pMsg2, const char *pMsg3,
//                           const char *pMsg4)
//{
//  Send_HttpHeader(200);
//  SendMsg("<HTML><HEAD></HEAD>\n<BODY>\n%s%s", pMsg1, pMsg2);
//  SendMsg("%s%s</BODY></HTML>",pMsg3, pMsg4);
//}
/*----------------- end Send_Msg_page() ---------------------*/

/****************************************************************
* sends a small HTML page.
* Header with title, then 4 lines pMsgx (if != NULL), trailer.
******************************************************************/

void  CTinyWebSrv::Send_ErrMsgPage(SOCKET SockId, int Status,
                                const char *pMsg1, const char *pMsg2,
                                const char *pMsg3, const char *pMsg4)
{
    Send_HttpHeader(SockId, Status, ".htm");
    SendMsg(SockId,"<HTML><HEAD></HEAD>\n<BODY>\n%s%s", pMsg1, pMsg2);
    SendMsg(SockId,"%s%s</BODY></HTML>",pMsg3, pMsg4);
}
/*----------------- end Send_Msg_page() ---------------------*/


//#####################################################
// Section Local routines
//#####################################################

/*****************************************************
*
* Create a socket(), bind(), listen()
* Returns sockid or if error INVALID_SOCKET
*****************************************************/

static SOCKET create_socket_for_accept(short port)
{
    SOCKET  sock_id;
    int     result;
    struct  sockaddr_in sock_addr;

    sock_id = socket(AF_INET,SOCK_STREAM,0);

    sock_addr.sin_family      = AF_INET;
    sock_addr.sin_addr.s_addr = INADDR_ANY;
    sock_addr.sin_port        = htons(port);

    result = bind(sock_id, (struct sockaddr *) &sock_addr,
                  sizeof(struct sockaddr));
    if(result == -1)
    {
        //WebPrintError(WSAGetLastError(), "create_socket_for_accept()",
        //              "bind()",0,0);
        return(INVALID_SOCKET);
    }
    else
    {
       if(Debug_Http>1)
           printf("bind() OK port = %d\n", port); //m_HttpPort);
    }

    result = listen (sock_id, SOMAXCONN);
    if(result == -1)
    {
        //WebPrintError(WSAGetLastError(), "create_socket_for_accept()",
        //                  "listen() error",0,0);
        return(INVALID_SOCKET);
    }
    else
      if(Debug_Http>1)
          printf("listen() OK result = %d\n", result);

    return(sock_id);
}
/*-------------- end create_socket_for_accept()-----------*/

/**********************************************************************
* Extract URL part from a request string.
* converts style "%nn" to a character.
*
* returns: "/aaa/bbb" if msg is as in example in handle_request() below
*
* A browser cmd like http://nere/prov.htm ==>
* GET /prov2.htm HTTP/1.0
*
* A link like <A href="prov2.htm">Min provsida</A><br> ==>
* GET /prov2.htm HTTP/1.0
* Referer: http://nere/prov.htm
***********************************************************************/

static char *GetUrl(char *pRxMsg)
{
    int     j1,j2;
    char    *pUrl;
    char    *pUrlEnd;
    char    ch;

    if(pRxMsg[0] == 'G') pUrl = &pRxMsg[4];  //GET
    else                 pUrl = &pRxMsg[5];  //POST

    pUrlEnd = strstr(pRxMsg, " HTTP");
    if (pUrlEnd == NULL)
    {
        return(NULL);
    }

    *pUrlEnd = 0; // terminator

    for (j1=0, j2=0 ; pUrl[j1] != 0 ; j1++)
    {
        if( pUrl[j1] != '%')
        {
            pUrl[j2++] = pUrl[j1];
            continue;
        }
        // convert e.g. "%20" to " "
        j1++;
        if ((pUrl[j1] >= '0') && (pUrl[j1] <= '9')) ch = pUrl[j1]-'0';
        else
        if ((pUrl[j1] >= 'A') && (pUrl[j1] <= 'F')) ch = pUrl[j1]-'A'+10;
        else
        if ((pUrl[j1] >= 'a') && (pUrl[j1] <= 'f')) ch = pUrl[j1]-'a'+10;
        else ch = 0;
        j1++;
        if ((pUrl[j1] >= '0') && (pUrl[j1] <= '9')) ch=16*ch + pUrl[j1]-'0';
        else
        if ((pUrl[j1] >= 'A') && (pUrl[j1] <= 'F')) ch=16*ch + pUrl[j1]-'A'+10;
        else
        if ((pUrl[j1] >= 'a') && (pUrl[j1] <= 'f')) ch=16*ch + pUrl[j1]-'a'+10;
        else ch = 16 * ch;

        pUrl[j2++] = ch;
    }
    pUrl[j2] = 0;

    *pUrlEnd = 0; // terminator
    if (Debug_Http>1) printf("URL = <%s>\n", pUrl);
    return(pUrl);
}
/*-------------------- end GetUrl()--------------*/

/***************************************************
* Read a line
* returns >1  a line with data
*          1  a blank line
*          0  connection closed
*         -1  some error
***************************************************/

int CTinyWebSrv::recvLine(SOCKET sock_id, char *pRxMsg)
{
    int rx_count;
    int pos = 0;


    for(;;)
    {
        rx_count = recv (sock_id, &pRxMsg[pos], 1,0);

        if (rx_count == 1)
        {
            if (pRxMsg[pos] == 0x0D) continue;
            if (pRxMsg[pos] == 0x0A)
            {
                pRxMsg[pos] = 0; return(pos+1);
            }
            pos++;
        }
        else break;
    }
    if (rx_count < 0 )
    {
        //WebPrintError(GetLastError(), "recvLine()",   "recv()",0,0);
        pRxMsg[pos] = 0;
        return(-1);
    }
    if (rx_count == 0 ) // connection closed
    {
        //if(Debug_Http>2) printf("WebSrvThread Connection closed\n");
        pRxMsg[pos] = 0;
        return(pos);
    }
    return(pos);
}
/*--------------------- end recvLine() ---------------*/

/******************************************************************
* This is a worker thread
* It is called when a connection with a browser has
* been established.
* Read header.
* Call the callbackroutine, which sends a response to the browser
*
* Note that POST requests are not supported.
********************************************************************/

void CTinyWebSrv::Worker(SOCKET sock_id_client)
{
    int             result;
    char            RxMsg[512];
    char            FirstRxMsg[1024];
    struct sockaddr_in PeerSockAddr;
    int             PeerSockAddr_len;
    CTinyWebSrv     *pWebSrv;
    HTTP_RX_HDR_S   HttpHdr;


    //HttpHdr.pCookie = "cookie"; //NULL;
    HttpHdr.pUrl = NULL;
    //HttpHdr.ContentLength = -1;

    if(Debug_Http) printf("WorkerThread start\n");

    pWebSrv = this;

    result = recvLine(sock_id_client, FirstRxMsg);

    if(Debug_Http>2)
        printf("FIRST%d=<%s>\n",result, FirstRxMsg);

    if (result <= 1) goto recover;

    PeerSockAddr_len = sizeof(struct sockaddr);
    getpeername (sock_id_client, (struct sockaddr *) &PeerSockAddr,
                 &PeerSockAddr_len);

    HttpHdr.PeerIpAddr = PeerSockAddr.sin_addr.S_un.S_addr;

    //------------------------------
    // method = GET
    //------------------------------
    if(strncmp(FirstRxMsg, "GET" ,3) == 0)
    {
        // Note: GetUrl() returns a pointer to a proper pos in FirstRxMsg
        //       Do not use FirstRxMgg to something else.
        HttpHdr.pUrl = GetUrl(FirstRxMsg);

        // ----------------------------------------------
        // Read rest of header
        // read until end of message (a blank line)
        //------------------------------------------------
        for(;;)
        {
            result = recvLine(sock_id_client,RxMsg);
            if(Debug_Http>7)
                printf("HDR%d=<%s>\n",result, RxMsg);

            if (result <= 1) break;

//          if (_strnicmp(RxMsg,"Content-Length:",15) == 0)
//          {
//              HttpHdr.ContentLength = atoi(&RxMsg[16]);
//              //printf("ContentLength=%d\n", Content_Length);
//          }
        } // end read rest of hdr

        m_pWebCallBack(sock_id_client, &HttpHdr);

        goto AllIsDone;
    } // end GET

recover: //--------------------------------------

    // we must send an error response, then close
//  if(Debug_Http) printf("WorkerThread RECOVER\n");
    Send_ErrMsgPage(sock_id_client,
        400, "Error when processing<br> ",
                             FirstRxMsg, NULL, NULL);
AllIsDone: //--------------------------------------

    (void) shutdown(sock_id_client,1);
    (void) closesocket(sock_id_client);

    if(Debug_Http>2) printf("Worker exit\n");
}
/*------------------- end CTinyWebSrv::Worker() -----------*/

/************************************************************
* This is a thread.
* Wait for a connection request from a Web Browser.
* Start a WorkerThread which sends a response to the browser
*
*************************************************************/

void __cdecl CTinyWebSrv::Server_Thread(void *lpdwParam)
{
    struct  sockaddr_in sock_addr;
    SOCKET          sock_id_client;
    int             addrlen;
    CTinyWebSrv     *pWebSrv;


    pWebSrv = (CTinyWebSrv *) lpdwParam;

    if(Debug_Http) printf("Server_Thread is running\n");

    /*-------------------------------------------
    * Create a socket(), bind(), listen() accept()
    *-------------------------------------------*/

    pWebSrv->m_SockId_http
        = create_socket_for_accept(pWebSrv->m_HttpPort);

    if(pWebSrv->m_SockId_http == INVALID_SOCKET)
    {
        char buff[300];
        int errCode = GetLastError();

        if(errCode == WSAEADDRINUSE)
            sprintf(buff, "DoseWebSrv Program can not start.\r\n"
                    "Check if another instance of the program is running.\r\n", errCode);
        else
            sprintf(buff, "Dose WebSrv Program can not start. (E=%d)\r\n", errCode);

        MessageBox(NULL, buff, "ERROR", MB_OK);
        exit(0);
    }
    /*-------------------------------------------
    * LOOP forever. Wait for connections
    *
    * Note: currently select() is not needed,
    * it is apreparation for future extensions.
    *-------------------------------------------*/
    for (;;)
    {
        /*---------------------------------------
        *  Wait for a connection from a Browser
        *----------------------------------------*/

        sock_addr.sin_family      = AF_INET;
        addrlen = sizeof(struct sockaddr);

        if (Debug_Http > 2)
            printf("Server_Thread() wait for a connection\n");

        addrlen = sizeof(struct sockaddr_in);
        sock_id_client = accept(pWebSrv->m_SockId_http,
                (struct sockaddr *) &sock_addr, &addrlen);

        if(sock_id_client == INVALID_SOCKET)
        {
            //if(Debug_Http)
            //  WebPrintError(WSAGetLastError(), "Server_Thread()",
            //                  "accept",0,0);

            Sleep(1000); continue;
        }
        pWebSrv->Worker(sock_id_client);
    } // end for(;;)
}
/*------- end CTinyWebSrv::Server_Thread --------*/

/*******************************************
* constructor
* starts the Thread()
* These may be overrideen by WebSrv.cfg data
********************************************/

CTinyWebSrv::CTinyWebSrv(short Port, void *pCallBack)
{
    int         result;
    WSADATA     WSAData;


    if ((result = WSAStartup(MAKEWORD(1,1) ,&WSAData )) != 0)
    {
        printf("WSAStartup failed %d\n", result);
        return;
    }

    m_pWebCallBack = (void (__cdecl *)(SOCKET, HTTP_RX_HDR_S *)) pCallBack;

    if (Port > 0)
        m_HttpPort = Port;

    if (Debug_Http>1) printf("call CreateThread() Port=%d\n", Port);

#ifdef RUN_AS_A_THREAD
    _beginthread( &Server_Thread, 0, (void *) this);
#else
    Server_Thread((void *) this);
#endif
    return;
}
/*--------------- end CTinyWebSrv::CTinyWebSrv() --------*/

/*******************************************
* destructor
* closes all sockets
********************************************/

CTinyWebSrv::~CTinyWebSrv()
{
    if (m_SockId_http != INVALID_SOCKET)
        (void) closesocket(m_SockId_http);
}
/*--------------------------------- TinyWebSrv.cpp -------*/
