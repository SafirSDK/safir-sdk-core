/******************************************************************************
*
* Copyright Saab AB, 2002-2008 (http://www.safirsdk.com)
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

#define META_CHAR 0x1B //esc

// For UDP send

#define PRINTLOG_PORT   31221

#define SERVER_IPADDR "127.0.0.1"

#define PROGRAM_NAME_LENGTH 16

/**********************************************************
* Args as printf
***********************************************************/

void PrintDbg( const char *format, ... );

/**********************************************************
* Args as printf
***********************************************************/

void PrintErr(int ErrorCode, const char *format, ... );

/********************************************************************
* Turn printing on or off
*
* mode =
*  0   - ignore parameter
* 'U'  - send to UDP
* 'N'  - no print
* 'C'  - Print To Console - not for Linux
* 'R'  - print to RAM
* 'S'  - stdout
* else - stdout
*
* pIpAddr -
* NULL  - ignore parameter
* dotted decimal - send to this in UDP mode, default = SERVER_IPADDR
*******************************************************************/
void PrintSetMode(wchar_t mode, char *pIpAddr);

void FlushRamBuffer();

/*------------------------- end PrintError.H ----------------------*/
