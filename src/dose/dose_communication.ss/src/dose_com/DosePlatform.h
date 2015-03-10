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

/*******************************************************************
* DosePlatform.h - a part of DoseComDll - For LINUX and WIN32
*
* All platform (LINUX/WIN32) include files are included via this file
*
* IS_USING_SOCKETS is defined by caller when needed
* IS_USING_THREADS is defined by caller when needed
*
* #ifdef _WIN32 must be defined for WIN32
* #ifdef _LINUX must be defined for LINUX
*******************************************************************/

//##################################
// Platform unique Include files
//##################################

#ifdef _WIN32
#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x500
#endif
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif

#ifdef IS_USING_SOCKETS
#include <winsock2.h>
//#pragma warning(disable: 4127) // to prevent warning from MS inc file
#include <ws2tcpip.h>            // IP Multicast needs it

#else
#include <windows.h>
#endif

#include <stdio.h>
#include <time.h>

#include <process.h>

//---------------------------------
#elif defined(linux) || defined(__linux) || defined(__linux__)

#ifndef _LINUX
#define _LINUX
#endif

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <time.h>
#include <unistd.h> // for usleep(), exit()
#include <sys/types.h>
#include <sys/shm.h>
#include <string.h>
#include <sys/times.h>
#include <sys/shm.h>

#ifdef IS_USING_SOCKETS
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <netinet/in.h> //htons()
#include <net/if.h>
#include <sys/ioctl.h>
#include <net/if_arp.h>

#endif

#ifdef IS_USING_THREADS
#include <pthread.h>     // pthread functions and data structures
#endif

#else
#  error "Unable to work out platform"
#endif

//##################################
// Platform unique defines
//##################################

#ifdef _WIN32
#define THREAD_API unsigned int __stdcall

#endif

//---------------------------------

#ifdef _LINUX

#define THREAD_API void *

#ifndef FALSE
#define FALSE   0
#define TRUE    1
#endif

#define _strnicmp strncasecmp

#endif


//dcom_ulong64
#if defined _WIN32
#  ifndef dcom_ulong64
     typedef unsigned _int64  dcom_ulong64;
#  endif
#elif defined _LINUX
#  ifndef dcom_ulong64
     typedef unsigned long long  dcom_ulong64;
#  endif
#else
#  error "Either _WIN32 or _LINUX must be defined!"
#endif