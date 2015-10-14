/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safirsdkcore.com)
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

#include <Safir/Utilities/Internal/VisibilityHelpers.h>

#ifdef logging_cpp_EXPORTS
#  define LOGGING_CPP_API SAFIR_HELPER_DLL_EXPORT
#else
#  define LOGGING_CPP_API SAFIR_HELPER_DLL_IMPORT
#  define SAFIR_LIBRARY_NAME "logging_cpp"
#  include <Safir/Utilities/Internal/AutoLink.h>
#endif
#define LOGGING_CPP_LOCAL SAFIR_HELPER_DLL_LOCAL

#include <string>

namespace Safir
{
namespace Logging
{
    /** 
     * Severity level according to RFC 3164. Please read the section on Safir Logging 
     * in the Safir SDK Core User's Guide for some recommendations on when to use the
     * various severity levels.
     **/
    enum Severity
    {
        /** RFC 3164 Description: System is unusable. */
        Emergency = 0,

        /** RFC 3164 Description: Action must be taken immediately. */
        Alert,

        /** RFC 3164 Description: Critical conditions. */
        Critical,

        /** RFC 3164 Description: Error conditions. */
        Error,

        /** RFC 3164 Description: Warning conditions. */
        Warning,

        /** RFC 3164 Description: Normal but significant condition. */
        Notice,

        /** RFC 3164 Description: Informational messages. */
        Informational,

        /** RFC 3164 Description: Debug-level messages. */
        Debug
    };


    /**
     * Send log messages to the system logging mechanism.
     *
     * The function takes a severity and an arbitrary string.
     * The severity levels conforms to the ones used by the well known syslog format as specified
     * in http://www.ietf.org/rfc/rfc3164.txt.
     *
     * @param [in] severity Severity according to RFC 3164.
     * @param [in] message Log text.
     */
    LOGGING_CPP_API void SendSystemLog(const Severity severity,
                                       const std::wstring& message);

}
}

#endif
