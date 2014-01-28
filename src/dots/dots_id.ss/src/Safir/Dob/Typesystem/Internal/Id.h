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
#ifndef _dots_id_h
#define _dots_id_h

#include <Safir/Utilities/Internal/VisibilityHelpers.h>

#ifdef dots_id_EXPORTS
#  define DOTS_ID_API SAFIR_HELPER_DLL_EXPORT
#else
#  define DOTS_ID_API SAFIR_HELPER_DLL_IMPORT
#  define SAFIR_LIBRARY_NAME "dots_id"
#  define SAFIR_NO_DEBUG_LIBRARY_SUFFIX
#  include <Safir/Utilities/Internal/AutoLink.h>
#endif
#define DOTS_ID_LOCAL SAFIR_HELPER_DLL_LOCAL

#include <boost/cstdint.hpp>

extern "C"
{
    /**
     * Generates a 64 bits long integer hash from the provided string.
     *
     * @param str The string to hash.
     * @return A 64 bit hash.
     */
    DOTS_ID_API boost::int64_t DotsId_Generate64(const char* str);

    /**
     * Generates a 64 bits long integer randomly.
     *
     * @return A 64 bit hash.
     */
    DOTS_ID_API boost::int64_t DotsId_GenerateRandom64();
}

#endif
