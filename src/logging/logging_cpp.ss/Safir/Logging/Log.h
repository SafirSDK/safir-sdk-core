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
#ifndef __SAFIR_LOGGING_LOG_H__
#define __SAFIR_LOGGING_LOG_H__

#if defined _MSC_VER
#  ifdef logging_cpp_EXPORTS
#    define LOGGING_EXPORTS __declspec(dllexport)
#  else
#    define LOGGING_EXPORTS __declspec(dllimport)
#    ifdef NDEBUG
#      pragma comment( lib, "logging_cpp.lib" )
#    else
#      pragma comment( lib, "logging_cppd.lib" )
#    endif
#  endif
#elif defined __GNUC__
#  define LOGGING_EXPORTS
#endif

#include <string>

namespace Safir
{
namespace Logging
{
    enum Severity
    {
        Emergency = 0,
        Alert,
        Critical,
        Error,
        Warning,
        Notice,
        Informational,
        Debug
    };


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
    LOGGING_EXPORTS void SendSystemLog(const Severity severity,
                                       const std::wstring& message);

}
}

#endif
