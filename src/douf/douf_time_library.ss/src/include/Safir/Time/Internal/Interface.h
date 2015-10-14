/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safirsdkcore.com)
*
* Created by: Erik Adolfsson / sterad
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
#ifndef __DOUF_TIME_LIBRARY_H
#define __DOUF_TIME_LIBRARY_H

#include <Safir/Utilities/Internal/VisibilityHelpers.h>

#ifdef douf_time_library_EXPORTS
#  define DOUF_TIME_LIBRARY_API SAFIR_HELPER_DLL_EXPORT
#else
#  define DOUF_TIME_LIBRARY_API SAFIR_HELPER_DLL_IMPORT
#endif
#define DOUF_TIME_LIBRARY_LOCAL SAFIR_HELPER_DLL_LOCAL

#include <Safir/Dob/Typesystem/LanguageInterfaceDefs.h>

#ifdef __cplusplus
extern "C"
{
#endif

    //----------------------------------------------
    // Get current UTC time
    // If success is false there is a configuration error!
    //----------------------------------------------
    DOUF_TIME_LIBRARY_API void DoufTimeC_GetUtcTime(DotsC_Float64& utcTime, bool& success);

    //----------------------------------------------
    // Get local time offset according to GMT
    // If success is false there is a configuration error!
    //----------------------------------------------
    DOUF_TIME_LIBRARY_API void DoufTimeC_GetLocalTimeOffset(DotsC_Int32& offset, bool& success);

#ifdef __cplusplus
}
#endif

#endif //__DOUF_TIME_LIBRARY_H
