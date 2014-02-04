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
#ifndef _lluf_utils_export_defs_h
#define _lluf_utils_export_defs_h

#include <Safir/Utilities/Internal/VisibilityHelpers.h>

#ifdef lluf_utils_EXPORTS
#  define LLUF_UTILS_API SAFIR_HELPER_DLL_EXPORT
#else
#  define LLUF_UTILS_API SAFIR_HELPER_DLL_IMPORT
#  define SAFIR_LIBRARY_NAME "lluf_utils"
#  include <Safir/Utilities/Internal/AutoLink.h>
#endif
#define LLUF_UTILS_LOCAL SAFIR_HELPER_DLL_LOCAL

#endif
