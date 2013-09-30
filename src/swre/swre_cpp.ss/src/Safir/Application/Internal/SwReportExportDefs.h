/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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
#ifndef _swre_export_defs_h
#define _swre_export_defs_h

#if defined _MSC_VER
    #ifdef SWRE_EXPORTS
        #define SWRE_API __declspec(dllexport)
    #else
        #define SWRE_API __declspec(dllimport)
        #ifdef NDEBUG
            #pragma comment( lib, "swre_interface_cpp.lib" )
        #else
            #pragma comment( lib, "swre_interface_cppd.lib" )
       #endif
    #endif
#elif defined __GNUC__
    #define SWRE_API
#endif

#endif //_swre_defs_h
