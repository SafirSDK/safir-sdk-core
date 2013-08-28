/******************************************************************************
*
* Copyright Saab AB, 2013 (http://www.safirsdk.com)
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
#include <boost/cstdint.hpp>

#ifndef __SAFIR_LOGGING_LIBRARY_H__
#define __SAFIR_LOGGING_LIBRARY_H__

#if defined _MSC_VER
#  ifdef logging_library_EXPORTS
#    define LOGGING_LIBRARY_API __declspec(dllexport)
#  else
#    define LOGGING_LIBRARY_API __declspec(dllimport)
#    pragma comment( lib, "logging_library.lib" )
#  endif
#elif defined __GNUC__
#  define LOGGING_LIBRARY_API
#endif

extern "C"
{

    /**
     * Service for sending log messages to the native system logging mechanism.
     *
     * The service takes a severity and an arbitrary string.
     * The severity levels conforms to the ones used by the well known syslog format as specified
     * in http://www.ietf.org/rfc/rfc3164.txt.
     *
     * @param [in] severity Severity according to RFC 3164.
     * @param [in] message Log text.
     */
     LOGGING_LIBRARY_API void LoggingC_SendSystemLog(const boost::int32_t severity,
                                                    const char * const   logMsg);
}

#endif

