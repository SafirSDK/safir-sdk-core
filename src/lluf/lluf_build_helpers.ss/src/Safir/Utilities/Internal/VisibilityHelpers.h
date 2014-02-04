/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
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
#ifndef __LLUF_VISIBILITY_HELPERS_H__
#define __LLUF_VISIBILITY_HELPERS_H__


// Generic helper definitions for shared library support
#if defined _WIN32 || defined __CYGWIN__
#  define SAFIR_HELPER_DLL_IMPORT __declspec(dllimport)
#  define SAFIR_HELPER_DLL_EXPORT __declspec(dllexport)
#  define SAFIR_HELPER_DLL_LOCAL
#else
#  if __GNUC__ >= 4
#    define SAFIR_HELPER_DLL_IMPORT __attribute__ ((visibility ("default")))
#    define SAFIR_HELPER_DLL_EXPORT __attribute__ ((visibility ("default")))
#    define SAFIR_HELPER_DLL_LOCAL  __attribute__ ((visibility ("hidden")))
#  else
#    define SAFIR_HELPER_DLL_IMPORT
#    define SAFIR_HELPER_DLL_EXPORT
#    define SAFIR_HELPER_DLL_LOCAL
#  endif
#endif


#endif

