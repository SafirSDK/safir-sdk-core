/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safir.sourceforge.net)
*
* Created by: Erik Adolfsson / sterad
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
#ifndef __DOUF_TIME_LIBRARY_H
#define __DOUF_TIME_LIBRARY_H

#include <Safir/Dob/Typesystem/Internal/KernelDefs.h>

#if defined _MSC_VER
#  ifdef DOUF_TIME_EXPORTS
#    define DOUF_TIME_API __declspec(dllexport)
#  else
#    define DOUF_TIME_API __declspec(dllimport)
#    pragma comment( lib, "douf_time_library.lib" )
#  endif
#  define CALLING_CONVENTION __cdecl
#elif defined __GNUC__
#  define DOUF_TIME_API
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

    //----------------------------------------------
    // Get current UTC time
    // If success is false there is a configuration error!
    //----------------------------------------------
    DOUF_TIME_API void CALLING_CONVENTION DoufTimeC_GetUtcTime(DotsC_Float64& utcTime, bool& success);

    //----------------------------------------------
    // Get local time offset according to GMT
    // If success is false there is a configuration error!
    //----------------------------------------------
    DOUF_TIME_API void CALLING_CONVENTION DoufTimeC_GetLocalTimeOffset(DotsC_Int32& offset, bool& success);

#ifdef __cplusplus
}
#endif

#endif //__DOUF_TIME_LIBRARY_H
