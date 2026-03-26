/******************************************************************************
*
* Copyright Saab AB, 2013, 2025 (http://safirsdkcore.com)
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

/* For C++20 or later we add utility functions with formatting.
 * Visual Studio does not update the __cplusplus macro unless the
 * `/Zc:__cplusplus` compiler switch is supplied, but it always sets
 * _MSVC_LANG to the correct value.  Check both to detect C++20.
 */
#if ((__cplusplus >= 202002L) || (defined(_MSVC_LANG) && (_MSVC_LANG >= 202002L)))
#  if defined(__has_include)
#    if __has_include(<format>)
#      include <format>
#    endif
#  endif
#endif

namespace Safir
{
namespace Logging
{
    /**
     * Severity level according to RFC 3164 and RFC 5424. Please read the section on Safir Logging
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
     * Syslog facility codes according to RFC 3164 and RFC 5424.
     * The numeric values are the facility codes as defined in the RFCs.
     **/
    enum Facility
    {
        /** Kernel messages. */
        Kernel = 0,

        /** User-level messages. */
        User = 1,

        /** Mail system. */
        Mail = 2,

        /** System daemons. */
        Daemon = 3,

        /** Security/authorization messages. */
        Auth = 4,

        /** Messages generated internally by syslogd. */
        Syslog = 5,

        /** Line printer subsystem. */
        Lpr = 6,

        /** Network news subsystem. */
        News = 7,

        /** UUCP subsystem. */
        Uucp = 8,

        /** Clock daemon. */
        Cron = 9,

        /** Security/authorization messages (private). */
        Authpriv = 10,

        /** FTP daemon. */
        Ftp = 11,

        /** Local use 0. */
        Local0 = 16,

        /** Local use 1. */
        Local1 = 17,

        /** Local use 2. */
        Local2 = 18,

        /** Local use 3. */
        Local3 = 19,

        /** Local use 4. */
        Local4 = 20,

        /** Local use 5. */
        Local5 = 21,

        /** Local use 6. */
        Local6 = 22,

        /** Local use 7. */
        Local7 = 23
    };


    /**
     * Send log messages to the system logging mechanism.
     *
     * The function takes a severity and an arbitrary string.
     * The severity levels conform to the syslog format as specified
     * in RFC 3164 and RFC 5424.
     *
     * Uses the Local0 facility by default.
     *
     * @param [in] severity Severity according to RFC 3164/5424.
     * @param [in] message Log text.
     */
    LOGGING_CPP_API void SendSystemLog(const Severity severity,
                                       const std::wstring& message);

    /**
     * Send log messages to the system logging mechanism.
     *
     * The function takes a severity, facility, and an arbitrary string.
     * The severity and facility levels conform to the syslog format as specified
     * in RFC 3164 and RFC 5424.
     *
     * @param [in] severity Severity according to RFC 3164/5424.
     * @param [in] facility Facility according to RFC 3164/5424.
     * @param [in] message Log text.
     */
    LOGGING_CPP_API void SendSystemLog(const Severity severity,
                                       const Facility facility,
                                       const std::wstring& message);

    // For C++20 or later we add utility functions with formatting
#if __cpp_lib_format
    namespace Internal
    {
        //declare an internal helper function
        LOGGING_CPP_API void LogFormattingException(const Severity severity,
                                                    const std::wstring& fmt,
                                                    const std::exception& e);
    }

    /**
     * Send log messages to the system logging mechanism, but with formatting functionality.
     *
     * Helper function that allows for logging using
     * the C++20 std::format facility and then forwards the resulting
     * string to the primary SendSystemLog overload.
     *
     * Uses the Local0 facility by default.
     *
     * Usage examples:
     *  // Simple integral parameter
     *  Safir::Logging::SendSystemLog(Safir::Logging::Informational, L"Current counter value: {}", 42);
     *
     *  // Mixing several parameters and literals
     *  Safir::Logging::SendSystemLog(Safir::Logging::Error,
     *                                L"Login failed for user \"{}\" from {}:{}",
     *                                userName,
     *                                remoteAddress,
     *                                remotePort);
     *
     * @param  severity  Severity according to RFC 3164/5424.
     * @param  message   Log text with formatting as accepted by std::format.
     * @param  args      Values that will be formatted into the log message.
     */
    template<typename... Args>
    void SendSystemLog(const Severity severity,
                       std::wformat_string<Args...> message,
                       Args&&... args)
    {
        try
        {
            SendSystemLog(severity, Local0, std::format(message, std::forward<Args>(args)...));
        }
        catch (const std::exception& e)
        {
            Internal::LogFormattingException(severity, std::wstring(message.get()), e);
        }
    }

    /**
     * Send log messages to the system logging mechanism, but with formatting functionality.
     *
     * Helper function that allows for logging using
     * the C++20 std::format facility and then forwards the resulting
     * string to the primary SendSystemLog overload.
     *
     * Usage examples:
     *  // Simple integral parameter
     *  Safir::Logging::SendSystemLog(Safir::Logging::Informational, Safir::Logging::Local0,
     *                                L"Current counter value: {}", 42);
     *
     *  // Mixing several parameters and literals
     *  Safir::Logging::SendSystemLog(Safir::Logging::Error,
     *                                Safir::Logging::Local1,
     *                                L"Login failed for user \"{}\" from {}:{}",
     *                                userName,
     *                                remoteAddress,
     *                                remotePort);
     *
     * @param  severity  Severity according to RFC 3164/5424.
     * @param  facility  Facility according to RFC 3164/5424.
     * @param  message   Log text with formatting as accepted by std::format.
     * @param  args      Values that will be formatted into the log message.
     */
    template<typename... Args>
    void SendSystemLog(const Severity severity,
                       const Facility facility,
                       std::wformat_string<Args...> message,
                       Args&&... args)
    {
        try
        {
            SendSystemLog(severity, facility, std::format(message, std::forward<Args>(args)...));
        }
        catch (const std::exception& e)
        {
            Internal::LogFormattingException(severity, std::wstring(message.get()), e);
        }
    }

    /**
     * Send a system log with severity Emergency.
     *
     * Example:
     *   Safir::Logging::SendEmergency(L"This is an Emergency log with a parameter {}", 123);
     */
    template<typename... Args>
    void SendEmergency(std::wformat_string<Args...> message, Args&&... args)
    {
        try
        {
            SendSystemLog(Emergency, Local0, std::format(message, std::forward<Args>(args)...));
        }
        catch (const std::exception& e)
        {
            Internal::LogFormattingException(Emergency, std::wstring(message.get()), e);
        }
    }

    /**
     * Send a system log with severity Alert.
     *
     * Example:
     *   Safir::Logging::SendAlert(L"This is an Alert log with a parameter {}", 123);
     */
    template<typename... Args>
    void SendAlert(std::wformat_string<Args...> message, Args&&... args)
    {
        try
        {
            SendSystemLog(Alert, Local0, std::format(message, std::forward<Args>(args)...));
        }
        catch (const std::exception& e)
        {
            Internal::LogFormattingException(Alert, std::wstring(message.get()), e);
        }
    }

    /**
     * Send a system log with severity Critical.
     *
     * Example:
     *   Safir::Logging::SendCritical(L"This is a Critical log with a parameter {}", 123);
     */
    template<typename... Args>
    void SendCritical(std::wformat_string<Args...> message, Args&&... args)
    {
        try
        {
            SendSystemLog(Critical, Local0, std::format(message, std::forward<Args>(args)...));
        }
        catch (const std::exception& e)
        {
            Internal::LogFormattingException(Critical, std::wstring(message.get()), e);
        }
    }

    /**
     * Send a system log with severity Error.
     *
     * Example:
     *   Safir::Logging::SendError(L"This is an Error log with a parameter {}", 123);
     */
    template<typename... Args>
    void SendError(std::wformat_string<Args...> message, Args&&... args)
    {
        try
        {
            SendSystemLog(Error, Local0, std::format(message, std::forward<Args>(args)...));
        }
        catch (const std::exception& e)
        {
            Internal::LogFormattingException(Error, std::wstring(message.get()), e);
        }
    }

    /**
     * Send a system log with severity Warning.
     *
     * Example:
     *   Safir::Logging::SendWarning(L"This is a Warning log with a parameter {}", 123);
     */
    template<typename... Args>
    void SendWarning(std::wformat_string<Args...> message, Args&&... args)
    {
        try
        {
            SendSystemLog(Warning, Local0, std::format(message, std::forward<Args>(args)...));
        }
        catch (const std::exception& e)
        {
            Internal::LogFormattingException(Warning, std::wstring(message.get()), e);
        }
    }

    /**
     * Send a system log with severity Notice.
     *
     * Example:
     *   Safir::Logging::SendNotice(L"This is a Notice log with a parameter {}", 123);
     */
    template<typename... Args>
    void SendNotice(std::wformat_string<Args...> message, Args&&... args)
    {
        try
        {
            SendSystemLog(Notice, Local0, std::format(message, std::forward<Args>(args)...));
        }
        catch (const std::exception& e)
        {
            Internal::LogFormattingException(Notice, std::wstring(message.get()), e);
        }
    }

    /**
     * Send a system log with severity Informational.
     *
     * Example:
     *   Safir::Logging::SendInformational(L"This is an Informational log with a parameter {}", 123);
     */
    template<typename... Args>
    void SendInformational(std::wformat_string<Args...> message, Args&&... args)
    {
        try
        {
            SendSystemLog(Informational, Local0, std::format(message, std::forward<Args>(args)...));
        }
        catch (const std::exception& e)
        {
            Internal::LogFormattingException(Informational, std::wstring(message.get()), e);
        }
    }

    /**
     * Send a system log with severity Debug.
     *
     * Example:
     *   Safir::Logging::SendDebug(L"This is a Debug log with a parameter {}", 123);
     */
    template<typename... Args>
    void SendDebug(std::wformat_string<Args...> message, Args&&... args)
    {
        try
        {
            SendSystemLog(Debug, Local0, std::format(message, std::forward<Args>(args)...));
        }
        catch (const std::exception& e)
        {
            Internal::LogFormattingException(Debug, std::wstring(message.get()), e);
        }
    }

#endif


}
}


#endif
