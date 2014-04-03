/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safir.sourceforge.net)
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
#ifndef __SYSTEM_PICTURE_EXPORT_DEFS_H__
#define __SYSTEM_PICTURE_EXPORT_DEFS_H__

#if defined _MSC_VER
#  ifdef system_picture_EXPORTS
#    define DOB_SYSTEM_PICTURE_API __declspec(dllexport)
#  else
#    define DOB_SYSTEM_PICTURE_API __declspec(dllimport)
#    define SAFIR_LIBRARY_NAME "system_picture"
#    include <Safir/Utilities/Internal/AutoLink.h>
#  endif
#elif defined __GNUC__
#  define DOB_SYSTEM_PICTURE_API
#endif

#endif
