/******************************************************************************
*
* Copyright Saab AB, 2011-2013 (http://safir.sourceforge.net)
*
* Created by: Mikael Wennerberg / stmiwn
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

#include "dose_com_utils.h"
#include <stdio.h>
#include <stdarg.h>

#if defined(linux) || defined(__linux) || defined(__linux__)
#define _vsnprintf vsnprintf
#endif


#define IS_USING_SOCKETS


extern void PrintDbg( const char *format, ... );
extern void PrintErr(int ErrorCode, const char *format, ... );

#include <Safir/Dob/Internal/DoseComAux/DoseOsInterface.h>


static int Debug = 0;

int volatile * volatile pDbg = &Debug;  // DoseOsSharedMem.cpp needs it

void PrintDbg( const char *format, ... )
{
    va_list marker;
    char    buffer[1024];

    va_start( marker, format); // Initialize variable arguments.
    _vsnprintf(buffer, sizeof(buffer), format, marker);
    va_end( marker ); // needed ? Reset variable arguments

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

    printf("ERROR [%d]: %s\n", ErrorCode, Buf1);
}

DOSE_SHARED_DATA_S * Get_NodeSharedData_Pointer(void)
{
    static DOSE_SHARED_DATA_S * ptr = (DOSE_SHARED_DATA_S *)
        DoseOs::Shared_Memory_Create(sizeof(DOSE_SHARED_DATA_S));
    return ptr;
}

