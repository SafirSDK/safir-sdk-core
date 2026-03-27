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
#ifndef __LLUF_SYSTEMLOG_H__
#define __LLUF_SYSTEMLOG_H__

#include <Safir/Utilities/Internal/UtilsExportDefs.h>
#include <Safir/Utilities/Internal/StringUtils.h>
#include <string>
#include <sstream>
#include <functional>


#define SEND_SYSTEM_LOG(severity, comment) \
    {std::wostringstream ostr_123; ostr_123 comment; Safir::Utilities::Internal::Log::Send(Safir::Utilities::Internal::Log::severity, Safir::Utilities::Internal::Log::Local0, ostr_123.str());}

namespace Safir
{
namespace Utilities
{
namespace Internal
{
namespace Log
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

    // Syslog facilities as defined in RFC 3164/5424
    // The numeric values are the facility codes (not shifted)
    enum Facility
    {
        Kernel = 0,
        User = 1,
        Mail = 2,
        Daemon = 3,
        Auth = 4,
        Syslog = 5,
        Lpr = 6,
        News = 7,
        Uucp = 8,
        Cron = 9,
        Authpriv = 10,
        Ftp = 11,
        Local0 = 16,
        Local1 = 17,
        Local2 = 18,
        Local3 = 19,
        Local4 = 20,
        Local5 = 21,
        Local6 = 22,
        Local7 = 23
    };

    /**
    * Service for sending log messages to the native system logging mechanism.
    *
    * The service takes a severity, facility, and an arbitrary string.
    * The severity and facility levels conform to the syslog format as specified
    * in RFC 3164 and RFC 5424.
    *
    * @param [in] severity Severity according to RFC 3164/5424.
    * @param [in] facility Facility according to RFC 3164/5424.
    * @param [in] text Log text.
    *
    */
    LLUF_INTERNAL_API void Send(const Severity severity, const Facility facility, const std::wstring& text);

    /**
     * Callback type for system log notifications.
     * Called when a system log message is sent.
     *
     * @param severity The severity of the log message.
     * @param facility The facility of the log message.
     * @param message The log message text (UTF-8 encoded).
     */
    using SystemLogCallback = std::function<void(Severity, Facility, const std::string&)>;

    /**
     * Set a callback to be notified when system log messages are sent.
     * Thread safety is the caller's responsibility.
     *
     * @param callback The callback function, or nullptr to unregister.
     */
    LLUF_INTERNAL_API void SetSystemLogCallback(SystemLogCallback callback);

}
}
}
}

#endif

