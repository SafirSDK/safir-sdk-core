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

#ifndef _dots_id_h
#define _dots_id_h

#if defined _MSC_VER
  #ifdef DOTS_ID_EXPORTS
    #define DOTS_ID_API __declspec(dllexport)
  #else
    #define DOTS_ID_API __declspec(dllimport)
    #ifndef NDEBUG
      #pragma comment( lib, "dots_idd.lib" )
    #else
      #pragma comment( lib, "dots_id.lib" )
    #endif
  #endif
#elif defined __GNUC__
  #define DOTS_ID_API
  #define __cdecl
#endif

#include <boost/cstdint.hpp>

extern "C"
{
    /**
     * Generates a 64 bits long integer hash from the provided string.
     *
     * @param str The string to hash.
     * @return A 64 bit hash.
     */
    DOTS_ID_API boost::int64_t __cdecl DotsId_Generate64(const char* str);

    /**
     * Generates a 64 bits long integer randomly.
     *
     * @return A 64 bit hash.
     */
    DOTS_ID_API boost::int64_t __cdecl DotsId_GenerateRandom64();
}

#endif
