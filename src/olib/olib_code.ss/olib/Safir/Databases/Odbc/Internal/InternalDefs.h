/******************************************************************************
*
* Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
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

#if defined _MSC_VER
  #pragma warning (disable: 4251 4275)
  #ifdef OLIB_EXPORTS
    #define OLIB_API __declspec(dllexport)
  #else
    #define OLIB_API __declspec(dllimport)

    #ifdef _DEBUG
      #pragma comment (lib, "olibd.lib")
    #else
      #pragma comment (lib, "olib.lib")
    #endif //_DEBUG
  #endif // OLIB_EXPORTS
#else
  #define OLIB_API
#endif // _MSC_VER

#include <ace/config.h>             // Needs windows.h

#include <sqltypes.h>
#include <sql.h>
#include <sqlext.h>

#if defined _MSC_VER
    #pragma comment(lib, "odbc32.lib")
#endif // _MSC_VER

#endif // Safir_Databases_Odbc_Internal_Internal_Defs_h
