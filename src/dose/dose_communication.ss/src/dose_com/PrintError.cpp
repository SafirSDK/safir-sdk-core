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

/******************************************************************
* PrintError.cpp - a part of DoseComDll - For LINUX and WIN32
*
* Opens a console window and writes text to it.
*
* Can be used from a GUI application which automatically
* opens a console window an writes in it.
*
* Entries:
* --------
* void PrintErr(int ErrorCode, const char *format, ... )
* void PrintDbg( const char *format, ... )
* void PrintSetMode(DWORD mode, char *pIpAddr)
*
* Currently implemented modes - see g_OutPutMode below.
*********************************************************************/

#ifndef _CRT_SECURE_NO_DEPRECATE
#define _CRT_SECURE_NO_DEPRECATE
#endif

#include <string>
#include <iostream>

#define  IS_USING_SOCKETS
#include "DosePlatform.h"
#include <stdio.h>
#include <stdarg.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include "PrintError.h"

#ifdef _LINUX
typedef unsigned long SOCKET;
#define INVALID_SOCKET (unsigned long)-1
#define _vsnprintf vsnprintf
#endif


// g_OutPutMode =
// 'U'  - send to UDP
// 'N'  - no print
// 'C'  - Print To Console
// 'R'  - Print to RAM
// 'L'  - Print to LLL
// 'S'  - stdout
// else - stdout

static unsigned long g_OutPutMode = 'L';  // might be modified by PrintSetMode()

static SOCKET g_SockId = INVALID_SOCKET;
static struct sockaddr_in g_SockName;

static char g_IpAddr[20] = SERVER_IPADDR;  // might be modified by PrintSetMode()

static char * Get_IpAddr() { return(g_IpAddr); }

static size_t g_ramBufSize = 300000;
static char* g_ramBuf = 0;
static char* g_ramFirstFree;

static unsigned int g_logSeq = 0;

//static char g_ProgName[PROGRAM_NAME_LENGTH] = {0};  //Note: this aso offset in msg to server

/***************************************************************
* Convert a UTF8-encoded std::string to std::wstring
****************************************************************/
static std::wstring ToWstring(const std::string & str)
{
    if (str.empty())
    {
        return std::wstring();
    }

    int left = 0;
    wchar_t *pwszBuf = new wchar_t[str.length() + 1];
    wchar_t *pwsz;
    unsigned long pos;

    pwsz = pwszBuf;

    std::string::const_iterator it;
    for( it = str.begin(); it != str.end(); ++it)
    {
        pos = (unsigned char) *it;
        if ((left == 0) ^ ((pos & 0xC0) != 0x80)) // Continuation byte mismatch
        {
            left = 0;
            *pwsz++ = L'#';
        }

        if (pos < 0x80) // 7-bit ASCII
        {
            *pwsz++ = (wchar_t) pos;
        }
        else if ((pos & 0xC0) == (0x80)) // Correct continuation
        {
            left--;
            *pwsz = (*pwsz << 6) + (wchar_t) (pos & 0x3F);
            if (left == 0)
                pwsz++;
        }
        else if ((pos & 0xE0) == (0xC0)) // First of 2
        {
            *pwsz = (wchar_t) (pos & 0x1F);
            left = 1;
        }
        else if ((pos & 0xF0) == (0xE0)) // First of 3
        {
            *pwsz = (wchar_t) (pos & 0x0F);
            left = 2;
        }
        else // Only the BMP is supported.
        {
            left = 0;
            *pwsz++ = L'#';
        }

    }

    std::wstring wstr( pwszBuf, pwsz - pwszBuf );

    delete [] pwszBuf;
    return wstr;
}
/*--------------------- end ToWstring() ------------------------*/

/***************************************************************
* Convert error number to a text
* Puts message in pBuff, returns a pointer to pBuff.
****************************************************************/

static char *Get_Err_Text(int err_code, char *pBuff, int maxSize)
{
    //------------------------
    // Is it an OS error
    //------------------------
    sprintf(pBuff,"E=%d - ", err_code);

#ifdef _WIN32
    int     result;
    char    *pTxt = NULL;
    int     len = static_cast<int>(strlen(pBuff));

    result = FormatMessage( FORMAT_MESSAGE_FROM_SYSTEM, NULL, err_code,
                            MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                            (LPTSTR) &pBuff[len], maxSize-len, NULL );
    if (result != 0)
    {
        return pBuff;
    }

    // Winsock errors
    switch(err_code)
    {
    case WSAEINTR:           pTxt= "WSAEINTR - "; break;
    case WSAEBADF:           pTxt= "WSAEBADF - "; break;
    case WSAEACCES:          pTxt= "WSAEACCES - "; break;
    case WSAEFAULT:          pTxt= "WSAEFAULT - "; break;
    case WSAEINVAL:          pTxt= "WSAEINVAL - "; break;
    case WSAEMFILE:          pTxt= "WSAEMFILE - "; break;

    case WSAEWOULDBLOCK:     pTxt= "WSAEWOULDBLOCK - "; break;
    case WSAEINPROGRESS:     pTxt= "WSAEINPROGRESS - "; break;
    case WSAEALREADY:        pTxt= "WSAEALREADY - "; break;
    case WSAENOTSOCK:        pTxt= "WSAENOTSOCK - "; break;
    case WSAEDESTADDRREQ:    pTxt= "WSAEDESTADDRREQ - "; break;
    case WSAEMSGSIZE:        pTxt= "WSAEMSGSIZE - "; break;
    case WSAEPROTOTYPE:      pTxt= "WSAEPROTOTYPE - "; break;
    case WSAENOPROTOOPT:     pTxt= "WSAENOPROTOOPT - "; break;
    case WSAEPROTONOSUPPORT: pTxt= "WSAEPROTONOSUPPORT - "; break;
    case WSAESOCKTNOSUPPORT: pTxt= "WSAESOCKTNOSUPPORT - "; break;
    case WSAEOPNOTSUPP:      pTxt= "WSAEOPNOTSUPP - "; break;
    case WSAEPFNOSUPPORT:    pTxt= "WSAEPFNOSUPPORT - "; break;
    case WSAEAFNOSUPPORT:    pTxt= "WSAEAFNOSUPPORT - "; break;
    case WSAEADDRINUSE:      pTxt= "WSAEADDRINUSE - "; break;
    case WSAEADDRNOTAVAIL:   pTxt= "WSAEADDRNOTAVAIL - "; break;
    case WSAENETDOWN:        pTxt= "WSAENETDOWN - "; break;
    case WSAENETUNREACH:     pTxt= "WSAENETUNREACH - "; break;
    case WSAENETRESET:       pTxt= "WSAENETRESET - "; break;
    case WSAECONNABORTED:    pTxt= "WSAECONNABORTED - "; break;
    case WSAECONNRESET:      pTxt= "WSAECONNRESET - "; break;
    case WSAENOBUFS:         pTxt= "WSAENOBUFS - "; break;
    case WSAEISCONN:         pTxt= "WSAEISCONN - "; break;
    case WSAENOTCONN:        pTxt= "WSAENOTCONN - "; break;
    case WSAESHUTDOWN:       pTxt= "WSAESHUTDOWN - "; break;
    case WSAETOOMANYREFS:    pTxt= "WSAETOOMANYREFS - "; break;
    case WSAETIMEDOUT:       pTxt= "WSAETIMEDOUT - "; break;
    case WSAECONNREFUSED:    pTxt= "WSAECONNREFUSED - "; break;
    case WSAELOOP:           pTxt= "WSAELOOP - "; break;
    case WSAENAMETOOLONG:    pTxt= "WSAENAMETOOLONG - "; break;
    case WSAEHOSTDOWN:       pTxt= "WSAEHOSTDOWN - "; break;
    case WSAEHOSTUNREACH:    pTxt= "WSAEHOSTUNREACH - "; break;
    case WSAENOTEMPTY:       pTxt= "WSAENOTEMPTY - "; break;
    case WSAEPROCLIM:        pTxt= "WSAEPROCLIM - "; break;
    case WSAEUSERS:          pTxt= "WSAEUSERS - "; break;
    case WSAEDQUOT:          pTxt= "WSAEDQUOT - "; break;
    case WSAESTALE:          pTxt= "WSAESTALE - "; break;
    case WSAEREMOTE:         pTxt= "WSAEREMOTE - "; break;
    case WSANOTINITIALISED:  pTxt= "WSANOTINITIALISED -"; break;

    default: if ((err_code >= WSABASEERR) && (err_code < WSABASEERR+200))
                 pTxt = "Unknown Winsock error";
             break;
    } // end switch

    if (pTxt == NULL) "Unknown error code";

    sprintf(pBuff,"E=%d: - %s", err_code, pTxt);
#endif

    return(pBuff);
}
/*--------------------- end Get_Err_text() ------------------------*/

/************************************************************
* local
************************************************************/
#ifdef _WIN32
static void PrintToConsole( const char *pTxt)
{
    int Length;
    DWORD NumberOfCharsWritten;
    static COORD dwWriteCoord = {1,1};
    static HANDLE hConsoleOutput = INVALID_HANDLE_VALUE;


    Length = static_cast<int>(strlen(pTxt));

    if(hConsoleOutput == INVALID_HANDLE_VALUE)
    {
        AllocConsole();
        hConsoleOutput = GetStdHandle(STD_OUTPUT_HANDLE);
    }

    WriteFile(hConsoleOutput,   //
                pTxt,           // characters
                Length,         // number of characters to write
                &NumberOfCharsWritten,
                NULL);
}
/*---------------------- end PrintToConsole() ----------------------*/
#endif

/************************************************
* Used first time - creates a send socket
*************************************************/

static SOCKET Create_Socket(void)
{
    SOCKET SockId;
#ifdef _WIN32
    WSADATA  WSAData;
    if (WSAStartup(MAKEWORD(2,2) ,&WSAData ) != 0)
    {
        std::wcout << "WSAStartup failed" << std::endl;
        return(INVALID_SOCKET);
    }
#endif

    SockId = socket(AF_INET,SOCK_DGRAM,0 );
    if (SockId == INVALID_SOCKET)
    {
        std::wcout << "ERROR: Can't create socket." << std::endl;
        return(INVALID_SOCKET);
    }

    g_SockName.sin_family      = AF_INET;
    g_SockName.sin_addr.s_addr = inet_addr(Get_IpAddr());
    g_SockName.sin_port        = htons(PRINTLOG_PORT);

    return(SockId);
}
/*------------------ end Create_Socket() ------------------*/

/**********************************************************
* Args as printf
***********************************************************/
static int PrintUdp( const char *pMsg0)
{
    if(g_SockId == INVALID_SOCKET)  // First time
    {
        g_SockId = Create_Socket();
        if (g_SockId == INVALID_SOCKET) return(-1);
    }

#ifdef zzzzzzzz_WIN32
    int     result;
    unsigned long   dwNumberOfBytesSent;
    int     NumWsaBuff;
    WSABUF  WsaBuff[4];

    if(g_SockId == INVALID_SOCKET)  // First time
    {
        g_SockId = Create_Socket();
        if (g_SockId == INVALID_SOCKET) return(-1);
    }

    WsaBuff[0].buf = (char *) pMsg0;
    WsaBuff[0].len = strlen(pMsg0);

    NumWsaBuff = 1;

    result = WSASendTo (g_SockId,  WsaBuff,NumWsaBuff,
                        &dwNumberOfBytesSent,0, //dwFlags,
                       (struct sockaddr *) &g_SockName,
                        sizeof(g_SockName), NULL, NULL);
#endif

//#ifdef _LINUX
    sendto(g_SockId, pMsg0, static_cast<int>(strlen(pMsg0)),0,
        (struct sockaddr *) &g_SockName, static_cast<int>(sizeof(g_SockName)));
//#endif
    return(0);
}
/*----------------- end PrintUdp() -----------------*/

void PrintRam(const char* log)
{
    if (g_ramBuf == 0)
    {
        // Allocate ram
        g_ramBuf = new char[g_ramBufSize];
        memset (g_ramBuf, 0, g_ramBufSize);
        g_ramBufSize -= 1; // always keep a null termination at the end
        g_ramFirstFree = g_ramBuf;
    }

    size_t logLen = strlen(log);

    if (logLen > g_ramBufSize)
    {
        std::wcout << "DoseCom log message to big for ram buffer!" << std::endl;
    }

    if (logLen > g_ramBufSize - (g_ramFirstFree - g_ramBuf))
    {
        // log doesn't fit in whats left of the buffer, start from the beginning.
        g_ramFirstFree = g_ramBuf;
    }

    strncpy(g_ramFirstFree, log, logLen);
    g_ramFirstFree += logLen;

    if (g_ramFirstFree >= g_ramBuf + g_ramBufSize)
    {
        g_ramFirstFree = g_ramBuf; 
    }
}

/**********************************************************
* Args as printf
***********************************************************/

void PrintDbg( const char *format, ... )
{
    va_list marker;
    char    resultBuf[1500];
    char    logBuf[1024];
    char    seqNoBuf[240];

    va_start( marker, format );     /* Initialize variable arguments. */

    _vsnprintf(logBuf, sizeof(logBuf), format, marker);

    va_end( marker ); /* needed ? Reset variable arguments */

    sprintf(seqNoBuf,"seq:%d", g_logSeq);
    ++g_logSeq;

    sprintf(resultBuf,"%s - %s", seqNoBuf, logBuf);

    if(g_OutPutMode == 'N') return;
#ifdef _WIN32
    if(g_OutPutMode == 'C')     PrintToConsole(resultBuf);
    else
#endif
    if(g_OutPutMode == 'U')     PrintUdp(resultBuf);
    else
    if(g_OutPutMode == 'R')     PrintRam(resultBuf);
    else
    if (g_OutPutMode == 'L')    lllog(1) << ToWstring(resultBuf) << std::flush; 
    else
    {
        std::wcout << ToWstring(resultBuf) << std::flush;
    }
}

/**********************************************************
* Args as printf
***********************************************************/

void PrintErr(int ErrorCode, const char *format, ... )
{
    va_list marker;
    char    Buf2[1024];
    char    Buf1[400];
    char    ErrBuf[240];

    va_start( marker, format );     /* Initialize variable arguments. */

    _vsnprintf(Buf1, sizeof(Buf1), format, marker);

    va_end( marker ); /* needed ? Reset variable arguments */

    if(g_OutPutMode == 'N') return;

    if(ErrorCode != 0)
    {
        Get_Err_Text(ErrorCode, ErrBuf, sizeof(ErrBuf));
        sprintf(Buf2,"ERROR: %s - %s\n",Buf1, ErrBuf);
    }
    else
    {
        sprintf(Buf2,"ERROR: %s\n",Buf1);
    }
#ifdef _WIN32
    if(g_OutPutMode == 'C') PrintToConsole(Buf2);
    else
#endif
    if(g_OutPutMode == 'U') PrintUdp(Buf2);
    else
    if(g_OutPutMode == 'R') PrintRam(Buf2);
    else
    if (g_OutPutMode == 'L') lllerr << ToWstring(Buf2) << std::flush;
    else
    {
        std::wcout << ToWstring(Buf2) << std::flush;
    }
}

/**********************************************************
* Turn printing on or off
*
* mode =
* 'U'  - send to UDP
* 'N'  - no print
* 'C'  - Print To Console
* 'S'  - stdout
* else - stdout
*
***********************************************************/
void PrintSetMode(wchar_t mode,
                  char *pIpAddr,        //dotted decimal or NULL
                  size_t ramBufSize) 
{
    std::wcout << "PrintSetMode(" << mode << "," << ToWstring(pIpAddr) << ")" << std::endl;

    if(pIpAddr != NULL)
        strncpy(g_IpAddr, pIpAddr, sizeof(g_IpAddr));
    if(mode != 0) g_OutPutMode = mode;

    g_ramBufSize = ramBufSize;
}

void FlushRamBuffer()
{
    std::wcout << ToWstring(g_ramBuf) << std::flush;
}

/*------------------------- end PrintError.cpp ----------------------*/
