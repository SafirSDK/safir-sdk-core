/******************************************************************************
*
* Copyright Saab AB, 2004-2013 (http://safir.sourceforge.net)
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
#include <Safir/Utilities/Internal/SystemLog.h>
#include "PrintError.h"

#ifdef _LINUX
typedef unsigned long SOCKET;
#define INVALID_SOCKET (unsigned long)-1
#define _vsnprintf vsnprintf
#endif




static unsigned int g_logSeq = 0;

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

    lllog(1) << ToWstring(resultBuf) << std::flush; 
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

    if(ErrorCode != 0)
    {
        Get_Err_Text(ErrorCode, ErrBuf, sizeof(ErrBuf));
        sprintf(Buf2,"ERROR: %s - %s",Buf1, ErrBuf);
    }
    else
    {
        sprintf(Buf2,"ERROR: %s",Buf1);
    }

    SEND_SYSTEM_LOG(Error,
                    << ToWstring(Buf2));
}


/*------------------------- end PrintError.cpp ----------------------*/
