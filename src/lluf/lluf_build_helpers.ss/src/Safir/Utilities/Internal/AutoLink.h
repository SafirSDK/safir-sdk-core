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

/* This file is inspired by boost/config/auto_link.hpp, but has 
 * far fewer options.
 *
 * Define SAFIR_LIBRARY_NAME to the name of the library, e.g. dots_cpp 
 *
 * For an example of how to use it check out UtilsExportDefs.h from lluf_utils.
 */

#  if !defined(SAFIR_NO_AUTOLINK)

#  if !defined (SAFIR_LIBRARY_NAME)
#    error "SAFIR_LIBRARY_NAME must be defined when using AutoLink.h"
#  endif


#  if defined (_MSC_VER) 

#    if defined(NDEBUG) || defined(SAFIR_NO_DEBUG_LIBRARY_SUFFIX)
#      define SAFIR_BUILD_TYPE
#    else
#      define SAFIR_BUILD_TYPE "d"
#    endif

#    pragma comment(lib, SAFIR_LIBRARY_NAME SAFIR_BUILD_TYPE ".lib")

#    undef SAFIR_BUILD_TYPE

#  endif

#  undef SAFIR_LIBRARY_NAME

#  ifdef SAFIR_NO_DEBUG_LIBRARY_SUFFIX
#    undef SAFIR_NO_DEBUG_LIBRARY_SUFFIX
#  endif

#endif
