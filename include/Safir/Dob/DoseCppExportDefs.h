/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safirsdkcore.com)
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

#ifndef _dose_cpp_export_defs_h
#define _dose_cpp_export_defs_h

#include <Safir/Utilities/Internal/VisibilityHelpers.h>

#ifdef dose_cpp_EXPORTS
#  define DOSE_CPP_API SAFIR_HELPER_DLL_EXPORT
#else
#  define DOSE_CPP_API SAFIR_HELPER_DLL_IMPORT
#  define SAFIR_LIBRARY_NAME "dose_cpp"
#  include <Safir/Utilities/Internal/AutoLink.h>
#endif

#endif
