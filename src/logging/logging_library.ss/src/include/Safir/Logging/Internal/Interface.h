/******************************************************************************
*
* Copyright Saab AB, 2013, 2026 (http://safirsdkcore.com)
*
* Created by: Anders Widén
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
#ifndef __SAFIR_LOGGING_LIBRARY_H__
#define __SAFIR_LOGGING_LIBRARY_H__

#include <Safir/Utilities/Internal/VisibilityHelpers.h>

#ifdef logging_library_EXPORTS
#  define LOGGING_LIBRARY_API SAFIR_HELPER_DLL_EXPORT
#else
#  define LOGGING_LIBRARY_API SAFIR_HELPER_DLL_IMPORT
#endif
#define LOGGING_LIBRARY_LOCAL SAFIR_HELPER_DLL_LOCAL

#include <cstdint>

extern "C"
{

    /**
     * Service for sending log messages to the native system logging mechanism.
     *
     * The service takes a severity, facility, and an arbitrary string.
     * The severity and facility levels conform to the syslog format as specified
     * in RFC 3164 and RFC 5424.
     *
     * @param [in] severity Severity according to RFC 3164/5424.
     * @param [in] facility Facility according to RFC 3164/5424.
     * @param [in] message Log text in utf-8.
     */
    LOGGING_LIBRARY_API void LoggingC_SendSystemLog(const std::int32_t severity,
                                                    const std::int32_t facility,
                                                    const char * const logMsg);
}

#endif

