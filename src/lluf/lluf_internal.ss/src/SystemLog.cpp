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
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Utilities/ProcessInfo.h>
#include <Safir/Utilities/Internal/ConfigReader.h>
#include <Safir/Utilities/Internal/Expansion.h>
#include <Safir/Utilities/Internal/StringEncoding.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <boost/algorithm/string/replace.hpp>
#include <mutex>
#include <memory>

#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable: 4244 4267)
#endif

#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/asio.hpp>
#include <boost/asio/ip/udp.hpp>
#include <boost/asio/ip/host_name.hpp>

#if defined _MSC_VER
  #pragma warning (pop)
#endif

#include <iostream>

#if defined(linux) || defined(__linux) || defined(__linux__)

#include <syslog.h>

#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)

#include "LogWin32.h"

#endif

namespace Safir
{
namespace Utilities
{
namespace Internal
{
namespace Log
{

namespace
{

// Convert Facility enum to syslog facility value (shifted by 3 bits as per RFC 3164)
inline int FacilityToSyslog(const Facility facility)
{
    return static_cast<int>(facility) << 3;
}

// Convert Facility enum to lowercase string name
inline const char* FacilityToString(const Facility facility)
{
    switch (facility)
    {
        case Kernel: return "kern";
        case User: return "user";
        case Mail: return "mail";
        case Daemon: return "daemon";
        case Auth: return "auth";
        case Syslog: return "syslog";
        case Lpr: return "lpr";
        case News: return "news";
        case Uucp: return "uucp";
        case Cron: return "cron";
        case Authpriv: return "authpriv";
        case Ftp: return "ftp";
        case Local0: return "local0";
        case Local1: return "local1";
        case Local2: return "local2";
        case Local3: return "local3";
        case Local4: return "local4";
        case Local5: return "local5";
        case Local6: return "local6";
        case Local7: return "local7";
        default: return "unknown";
    }
}

// Convert Severity enum to lowercase string name
inline const char* SeverityToString(const Severity severity)
{
    switch (severity)
    {
        case Emergency: return "emerg";
        case Alert: return "alert";
        case Critical: return "crit";
        case Error: return "err";
        case Warning: return "warning";
        case Notice: return "notice";
        case Informational: return "info";
        case Debug: return "debug";
        default: return "unknown";
    }
}

std::string GetISO8601Timestamp()
{
    using namespace boost::posix_time;

    std::stringstream ss;

    // ISO 8601 format: YYYY-MM-DDTHH:MM:SS.ssssssZ (UTC)
    ss.imbue(std::locale(ss.getloc(), new time_facet("%Y-%m-%dT%H:%M:%S.%fZ")));
    
    auto now = microsec_clock::universal_time();
    ss << now;
    
    return ss.str();
}

}

class SystemLogImpl
{
    friend class SystemLogImplKeeper;
    friend void Send(const Severity, const Facility, const std::wstring&);
    friend void SetSystemLogCallback(SystemLogCallback callback);

private:
    //constructor is private, to make sure only SystemLogImplKeeper can create it
    SystemLogImpl()
        : m_pid(Safir::Utilities::ProcessInfo::GetPid()),
          m_processName(Safir::Utilities::ProcessInfo(m_pid).GetProcessName()),
#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
          m_eventLog(ToUtf16(m_processName)),
#endif
          m_nativeLogging(false),
          m_sendToSyslogServer(false),
          m_includeSafirInstance(false),
          m_syslogServerEndpoint(),
          m_io(),
          m_sock(m_io),
          m_lock()
    {
            boost::algorithm::replace_last(m_processName, ".exe", "");

            Safir::Utilities::Internal::ConfigReader configReader;

            m_nativeLogging = configReader.Logging().get<bool>("SystemLog.native_logging");
            m_sendToSyslogServer = configReader.Logging().get<bool>("SystemLog.send_to_syslog_server");
            m_replaceNewlines = configReader.Logging().get<bool>("SystemLog.replace_newline_with_space");
            m_includeSafirInstance = configReader.Logging().get<bool>("SystemLog.show_safir_instance");
            m_syslogLineLength = configReader.Logging().get<size_t>("SystemLog.truncate_syslog_to_bytes", 1024);

            if (m_nativeLogging)
            {
#if defined(linux) || defined(__linux) || defined(__linux__)

                // Include pid in log message
                // Facility is set to LOG_USER as a default, but actual facility
                // is passed explicitly in each syslog() call
                openlog(NULL, LOG_PID, LOG_USER);

#endif
            }

            if (m_sendToSyslogServer)
            {

                m_sock.open(boost::asio::ip::udp::v4());
                m_sock.bind(boost::asio::ip::udp::endpoint(boost::asio::ip::udp::v4(), 0));

                auto ipAddr = boost::asio::ip::make_address(configReader.Logging().get<std::string>("SystemLog.syslog_server_address"));
                auto port = configReader.Logging().get<unsigned short>("SystemLog.syslog_server_port");
                m_syslogServerEndpoint = boost::asio::ip::udp::endpoint(ipAddr, port);
            }
    }

public:
    ~SystemLogImpl()
    {
#if defined(linux) || defined(__linux) || defined(__linux__)

        closelog();
#endif
    }

    void SetCallback(SystemLogCallback callback)
    {
        m_callback = std::move(callback);
    }

    void Send(const Severity severity,
              const Facility facility,
              const std::wstring& text)
    {
        // Recursion guard: prevent infinite loops if callback calls SendSystemLog
        static thread_local bool inCallback = false;

        // Invoke callback first (before any other logging) if registered and not recursing
        if (m_callback && !inCallback)
        {
            inCallback = true;
            try
            {
                m_callback(severity, facility, ToUtf8(text));
            }
            catch (...)
            {
                // Ignore exceptions from callback
            }
            inCallback = false;
        }

        std::wstring logText;
        if (m_includeSafirInstance)
        {
            logText = L"(" + boost::lexical_cast<std::wstring>(
                          Safir::Utilities::Internal::Expansion::GetSafirInstance()) + L") " + text;
        }
        else
        {
            logText = text;
        }

        std::wstring textAscii = logText;
        //replace non-ascii chars
        for(std::wstring::iterator it = textAscii.begin();
            it != textAscii.end(); ++it)
        {
            if ((*it & ~0x7F) != 0)
            {
                *it = L'@';
            }
        }

        switch (severity)
        {
            // write all system logs to lll
            case Emergency:
            {
                lllog(1) << L"EMERGENCY: " << textAscii << std::endl;
            }
            break;

            case Alert:
            {
                lllog(1) << L"ALERT: " << textAscii << std::endl;
            }
            break;

            case Critical:
            {
                lllog(1) << L"CRITICAL: " << textAscii << std::endl;
            }
            break;

            case Error:
            {
                lllog(1) << L"ERROR: " << textAscii << std::endl;
            }
            break;

            case Warning:
            {
                lllog(1) << L"WARNING: " << textAscii << std::endl;
            }
            break;

            case Notice:
            {
                lllog(1) << L"NOTICE: " << textAscii << std::endl;
            }
            break;

            case Informational:
            {
                lllog(1) << L"INFORMATIONAL: " << textAscii << std::endl;
            }
            break;

            case Debug:
            {
                lllog(1) << L"DEBUG: " << textAscii << std::endl;
            }
            break;

            default:
                throw std::logic_error("SystemLogImpl::SendNativeLog: Unknown severity!");
        }

        if (m_nativeLogging)
        {
            SendNativeLog(severity, facility, ReplaceNewlines(logText));
        }

        if (m_sendToSyslogServer)
        {
            // Utf-8 is used when sending to a syslog server
            SendToSyslogServerRFC5424(severity, facility, ToUtf8(ReplaceNewlines(logText)));
        }
    }

private:

    //-------------------------------------------------------------------------
    void SendNativeLog(const Severity severity, const Facility facility, const std::wstring& text)
    {
#if defined(linux) || defined(__linux) || defined(__linux__)

        syslog(FacilityToSyslog(facility) | severity, "%s", ToUtf8(text).c_str());

#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)

        // Translate syslog severity to windows event log type
        WORD eventType = 0;
        switch (severity)
        {
            case Log::Emergency:
            case Log::Alert:
            case Log::Critical:
            case Log::Error:
            {
                eventType = EVENTLOG_ERROR_TYPE;
            }
            break;

            case Log::Warning:
            {
                eventType = EVENTLOG_WARNING_TYPE;
            }
            break;

            case Log::Notice:
            case Log::Informational:
            case Log::Debug:
            {
                eventType = EVENTLOG_INFORMATION_TYPE;
            }
            break;

            default:
                throw std::logic_error("LogImpl::SendNativeLog: Unknown severity!");
        }

        // Format message in EventSentry style: hostname[facility.severity]: message
        std::wstring formattedText = ToUtf16(boost::asio::ip::host_name()) +
                                     L"[" + ToUtf16(FacilityToString(facility)) +
                                     L"." + ToUtf16(SeverityToString(severity)) +
                                     L"]: " + text;

        std::lock_guard<std::mutex> lck(m_lock);

        m_eventLog.Send(eventType, formattedText);
#endif
    }

    //-------------------------------------------------------------------------
    void SendToSyslogServerRFC5424(const Severity severity,
                            const Facility facility,
                            const std::string& text)
    {
        std::ostringstream log;
        // RFC 5424 format: <PRI>VERSION TIMESTAMP HOSTNAME APP-NAME PROCID MSGID STRUCTURED-DATA MSG
        log << "<" << (FacilityToSyslog(facility) | severity) << ">1 "
            << GetISO8601Timestamp() << ' '
            << boost::asio::ip::host_name() << ' '
            << m_processName << ' '
            << m_pid << " - - " << text;

        std::string logStr = log.str();

        // RFC 5424 doesn't have a strict size limit like RFC 3164,
        // but we still apply truncation if configured
        if (logStr.size() > m_syslogLineLength)
        {
            // truncate string ...
            logStr.erase(m_syslogLineLength - 3, std::string::npos);

            // ... and put in "..." to indicate this
            logStr += "...";
        }

        // The asio socket is not thread safe
        std::lock_guard<std::mutex> lck(m_lock);

        m_sock.send_to(boost::asio::buffer(logStr.c_str(),
                                           logStr.size()),
                       m_syslogServerEndpoint);
    }

    //-------------------------------------------------------------------------
    std::wstring ReplaceNewlines(std::wstring text)
    {
        //remove any trailing newlines. RFC3164 does not require newlines at end of message.
        while (text.find_last_of(L"\n\r") == text.size() - 1)
        {
            text.erase(text.size() - 1);
        }

        if (!m_replaceNewlines)
        {
            return text;
        }
        else
        {
            for (std::wstring::iterator it = text.begin();
                 it != text.end(); ++it)
            {
                if (*it == '\n' || *it == '\r') //may produce two spaces on CRLF, but that
                {                               //would only happen if the error message is
                    *it = ' ';                  //read from a file with CRLF line endings.
                }
            }
            return text;
        }
    }

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4251)
#endif

    pid_t                           m_pid;
    std::string                     m_processName;

#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
    WindowsLogger                   m_eventLog;
#endif

    bool                            m_nativeLogging;
    bool                            m_sendToSyslogServer;
    bool                            m_replaceNewlines;
    bool                            m_includeSafirInstance;
    size_t                          m_syslogLineLength;
    boost::asio::ip::udp::endpoint  m_syslogServerEndpoint;
    boost::asio::io_context         m_io;
    boost::asio::ip::udp::socket    m_sock;
    std::mutex                      m_lock;
    SystemLogCallback               m_callback;

#ifdef _MSC_VER
#pragma warning(pop)
#endif

};

class SystemLogImplKeeper
{
public:
    static SystemLogImplKeeper& Instance()
    {
        std::call_once(SingletonHelper::m_onceFlag,[]{SingletonHelper::Instance();});

        if (destroyed)
        {
            throw std::runtime_error("Dead reference detected for singleton SystemLogImplKeeper");
        }

        return SingletonHelper::Instance();
    }

    const std::shared_ptr<SystemLogImpl> Get()
    {
        std::lock_guard<std::mutex> lck(m_lock);

        if (!m_impl)
        {
            m_impl = std::shared_ptr<SystemLogImpl>(new SystemLogImpl());
        }
        return m_impl;
    }

    void Reset()
    {
        std::lock_guard<std::mutex> lck(m_lock);
        m_impl.reset();
    }

private:
    SystemLogImplKeeper()
    {
    }

    ~SystemLogImplKeeper()
    {
        std::lock_guard<std::mutex> lck(m_lock);

        m_impl.reset();
        destroyed = true;
    }

    std::shared_ptr<SystemLogImpl>   m_impl;
    std::mutex                       m_lock;

    static bool                      destroyed;

    /**
     * This class is here to ensure that only the Instance method can get at the
     * instance, so as to be sure that call_once is used correctly.
     * Also makes it easier to grep for singletons in the code, if all
     * singletons use the same construction and helper-name.
     */
    struct SingletonHelper
    {
    private:
        friend SystemLogImplKeeper& SystemLogImplKeeper::Instance();

        static SystemLogImplKeeper& Instance()
        {
            static SystemLogImplKeeper instance;
            return instance;
        }
        static std::once_flag m_onceFlag;
    };

};

//mandatory static initialization
std::once_flag SystemLogImplKeeper::SingletonHelper::m_onceFlag;
bool SystemLogImplKeeper::destroyed = false;

void TrySendNativeLog(const Facility facility, const std::string& errTxt)
{
    try
    {
#if defined(linux) || defined(__linux) || defined(__linux__)
        syslog(FacilityToSyslog(facility) | Critical, "%s", errTxt.c_str());
#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
        WindowsLogger(L"Unknown process").Send(EVENTLOG_ERROR_TYPE, ToUtf16(errTxt));
#endif
    }
    catch (...)
    {
        // Nothing we can do here
    }
}

void Send(const Severity severity, const Facility facility, const std::wstring& text)
{
    try
    {
        SystemLogImplKeeper::Instance().Get()->Send(severity, facility, text);
    }
    catch (const std::runtime_error&)
    {
        // The singleton has been destroyed, most likely because Send is called from
        // another singleton's destructor.
        // Instead of an approach where we try to resurrect the SystemLogImplKeeper singleton
        // (Phoenix Singleton) we just create a temporary SystemLogImpl on the stack for this
        // specific call to Send.
        SystemLogImpl log;
        log.Send(severity, facility, text);
    }
    catch (const std::exception& e)
    {
        // If it is not possible to send a log we try to send a native log and
        // then just terminate the program.
        TrySendNativeLog(facility, e.what());
        exit(56);
    }
    catch (...)
    {
        // Something really bad happened. We have no information about what it is, just
        // terminate the program.
        TrySendNativeLog(facility, "Caught some really weird exception when trying to send a system log.");
        exit(57);
    }
}

void SetSystemLogCallback(SystemLogCallback callback)
{
    try
    {
        SystemLogImplKeeper::Instance().Get()->SetCallback(std::move(callback));
    }
    catch (const std::runtime_error&)
    {
        // The singleton has been destroyed, ignore
    }
}

}
}
}
}
