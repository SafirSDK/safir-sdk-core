/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safirsdkcore.com)
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
#ifndef __DOTS_INTERNAL_EXPORT_DEFS_H__
#define __DOTS_INTERNAL_EXPORT_DEFS_H__

#include <Safir/Utilities/Internal/VisibilityHelpers.h>

#ifdef dots_internal_EXPORTS
#  define DOTS_INTERNAL_API SAFIR_HELPER_DLL_EXPORT
#else
#  define DOTS_INTERNAL_API SAFIR_HELPER_DLL_IMPORT
#endif
#define DOTS_INTERNAL_LOCAL SAFIR_HELPER_DLL_LOCAL

#endif

