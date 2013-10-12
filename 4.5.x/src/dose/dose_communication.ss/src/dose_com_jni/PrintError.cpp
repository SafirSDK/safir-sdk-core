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
* PrintError.cpp - a part of DoseJni - For LINUX and WIN32
*
* Entries:
* --------
* void PrintErr(int ErrorCode, const char *format, ... )
* void PrintDbg( const char *format, ... )
* void PrintSetMode(DWORD mode, char *pIpAddr)
*
* Currently implemented modes - see g_OutPutMode below.
*******************************************************************/

#define _CRT_SECURE_NO_DEPRECATE

#include <stdio.h>
#include <stdarg.h>

#if defined(linux) || defined(__linux) || defined(__linux__)
#define _vsnprintf vsnprintf
#endif

// g_OutPutMode =
// 'U'  - send to UDP - not implemented
// 'N'  - no print
// 'C'  - Print To Console - not implemented
// 'S'  - stdout
// else - stdout

static unsigned long g_OutPutMode = 'S';  // might be modified by PrintSetMode()

/**********************************************************
* Args as printf
***********************************************************/

void PrintDbg( const char *format, ... )
{
    va_list marker;
    char    buffer[1024];

    va_start( marker, format); // Initialize variable arguments.
    _vsnprintf(buffer, sizeof(buffer), format, marker);
    va_end( marker ); // needed ? Reset variable arguments

    if(g_OutPutMode == 'N') return;
    printf("%s", buffer); fflush(stdout);
}

/**********************************************************
* Args as printf
***********************************************************/

void PrintErr(int ErrorCode, const char *format, ... )
{
    va_list marker;
    char    Buf1[400];

    va_start( marker, format );     // Initialize variable arguments.
    _vsnprintf(Buf1, sizeof(Buf1), format, marker);
    va_end( marker ); // needed ? Reset variable arguments

    if(g_OutPutMode == 'N') return;

    printf("ERROR [%d]: %s\n", ErrorCode, Buf1);
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
void PrintSetMode(unsigned long mode, char *pIpAddr)  //dotted decimal or NULL
{
    printf("PrintSetMode(%lX,%s)\n",mode, pIpAddr);

    if(mode != 0) g_OutPutMode = mode;
}
/*------------------------- end PrintError.cpp ----------------------*/
