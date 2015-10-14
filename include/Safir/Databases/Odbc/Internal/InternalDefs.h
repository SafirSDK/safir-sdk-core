/******************************************************************************
*
* Copyright Saab AB, 2005-2013 (http://safirsdkcore.com)
*
* Created by: Jörgen Johansson / stjrjo
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
#ifndef Safir_Databases_Odbc_Internal_Internal_Defs_h
#define Safir_Databases_Odbc_Internal_Internal_Defs_h

#include <Safir/Utilities/Internal/VisibilityHelpers.h>

#ifdef olib_EXPORTS
#  define OLIB_API SAFIR_HELPER_DLL_EXPORT
#else
#  define OLIB_API SAFIR_HELPER_DLL_IMPORT
#  define SAFIR_LIBRARY_NAME "olib"
#  include <Safir/Utilities/Internal/AutoLink.h>
#endif
#define OLIB_LOCAL SAFIR_HELPER_DLL_LOCAL

#if defined _MSC_VER
#  ifndef WIN32_LEAN_AND_MEAN
#    define WIN32_LEAN_AND_MEAN
#  endif
#  include <windows.h>             // sql.h etc need windows.h
#  pragma comment(lib, "odbc32.lib")
#endif // _MSC_VER

#include <sqltypes.h>
#include <sql.h>
#include <sqlext.h>


#endif // Safir_Databases_Odbc_Internal_Internal_Defs_h
