/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
*
* Created by: Lars Hagström / stlrha
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

#ifndef _dose_internal_export_defs_h
#define _dose_internal_export_defs_h

#if defined _MSC_VER
    #pragma warning (disable: 4251 4275)
    #ifdef DOSE_INT_EXPORTS
        #define DOSE_INTERNAL_API __declspec(dllexport)
    #else
        #define DOSE_INTERNAL_API __declspec(dllimport)
        #ifndef NDEBUG
            #pragma comment( lib, "dose_internald.lib" )
        #else
            #pragma comment( lib, "dose_internal.lib" )
       #endif
    #endif
#elif defined __GNUC__
    #define DOSE_INTERNAL_API
    #define __cdecl
#endif

#endif //_dose_internal_defs_h
