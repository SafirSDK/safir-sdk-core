/******************************************************************************
*
* Copyright Saab AB, 2007-2011 (http://www.safirsdk.com)
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

#ifndef _dose_test_util_h
#define _dose_test_util_h

#if defined _MSC_VER
#  ifdef DOSE_EXPORTS
#    define DOSE_UTIL_API __declspec(dllexport)
#  else
#    define DOSE_UTIL_API __declspec(dllimport)
#    pragma comment( lib, "dose_test_util.lib" )
#  endif
#  define CALLING_CONVENTION __cdecl
#elif defined __GNUC__
#  define DOSE_UTIL_API
#  if defined (__i386)
#    define CALLING_CONVENTION __attribute__((cdecl))
#  else
#    define CALLING_CONVENTION
#  endif
#endif

#ifdef __cplusplus
extern "C"
{
#endif


    //---------------------------------------------------------------------------
    // Dose_Com interface
    //---------------------------------------------------------------------------
    DOSE_UTIL_API void CALLING_CONVENTION InhibitOutgoingTraffic(const bool inhibit, bool& success);
    DOSE_UTIL_API void CALLING_CONVENTION InhibitOutgoingTrafficStatus(bool& isInhibited);

    //---------------------------------------------------------------------------
    // Multicast socket
    //---------------------------------------------------------------------------
    DOSE_UTIL_API void CALLING_CONVENTION JoinMulticastGroup(const char* const multicastAddress,
                                                             const int         multicastPort,
                                                             const char* const multicastNic);
    
    DOSE_UTIL_API void CALLING_CONVENTION ReceiveMulticastPacket(char* buf,
                                                                 int bufLen);

#ifdef __cplusplus
}
#endif

#endif //_dose_test_util_h
