/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
*
* Created by: Mikael Wennerberg / stmiwn
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

#ifndef _dose_test_util_h
#define _dose_test_util_h

#include <Safir/Utilities/Internal/VisibilityHelpers.h>

#ifdef dose_test_util_EXPORTS
#  define DOSE_TEST_UTIL_API SAFIR_HELPER_DLL_EXPORT
#else
#  define DOSE_TEST_UTIL_API SAFIR_HELPER_DLL_IMPORT
#endif
#define DOSE_TEST_UTIL_LOCAL SAFIR_HELPER_DLL_LOCAL

#ifdef __cplusplus
extern "C"
{
#endif


    //---------------------------------------------------------------------------
    // Dose_Com interface
    //---------------------------------------------------------------------------
    DOSE_TEST_UTIL_API void InhibitOutgoingTraffic(const bool inhibit, bool& success);
    DOSE_TEST_UTIL_API void InhibitOutgoingTrafficStatus(bool& isInhibited);

#ifdef __cplusplus
}
#endif

#endif //_dose_test_util_h
