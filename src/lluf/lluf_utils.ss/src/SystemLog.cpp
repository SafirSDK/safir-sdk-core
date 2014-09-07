/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safir.sourceforge.net)
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
#include <Safir/Utilities/Internal/StringEncoding.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <boost/weak_ptr.hpp>
#include <boost/thread/once.hpp>
#include <boost/thread/locks.hpp>
#include <boost/thread/mutex.hpp>
#include <boost/algorithm/string/replace.hpp>

#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable: 4244 4267)
#endif

#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/asio/io_service.hpp>
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

// Syslog facility used for all Safir logs
const int SAFIR_FACILITY = (1 << 3); // 1 => user-level messages

std::string GetSyslogTimestamp()
{
    using namespace boost::posix_time;

    std::stringstream ss;

    ss.imbue(std::locale(ss.getloc(), new time_facet("%b %d %H:%M:%S")));
    ss << second_clock::local_time();
    return ss.str();
}

}

class LLUF_UTILS_API SystemLogImpl
{
    friend class SystemLogImplKeeper;
    friend void Send(const Severity, const std::wstring&);

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
          m_syslogServerEndpoint(),
          m_service(),
          m_sock(m_service),
          m_lock()
    {
        //this will allow wcout to coexist with cout and with printf/wprintf
        //which appears to be needed for java and dotnet.
        std::ios_base::sync_with_stdio(false);

        try
        {
            boost::algorithm::replace_last(m_processName, ".exe", "");

            Safir::Utilities::Internal::ConfigReader configReader;

            m_nativeLogging = configReader.Logging().get<bool>("SystemLog.native_logging");
            m_sendToSyslogServer = configReader.Logging().get<bool>("SystemLog.send_to_syslog_server");
            m_replaceNewlines = configReader.Logging().get<bool>("SystemLog.replace_newline_with_space");

            if (m_nativeLogging)
            {
#if defined(linux) || defined(__linux) || defined(__linux__)

                // Include pid in log message
                openlog(NULL, LOG_PID, SAFIR_FACILITY);

#endif
            }

            if (m_sendToSyslogServer)
            {

                m_sock.open(boost::asio::ip::udp::v4());
                m_sock.bind(boost::asio::ip::udp::endpoint(boost::asio::ip::udp::v4(), 0));

                m_syslogServerEndpoint =
                        boost::asio::ip::udp::endpoint
                            (boost::asio::ip::address::from_string
                                (configReader.Logging().get<std::string>("SystemLog.syslog_server_address")),
                                 configReader.Logging().get<unsigned short>("SystemLog.syslog_server_port"));
            }

        }
        catch (const std::exception& e)
        {
            // Something really bad has happened, we have to stop executing
            FatalError(ToUtf16(e.what()));
        }
    }

public:
    ~SystemLogImpl()
    {
#if defined(linux) || defined(__linux) || defined(__linux__)

        closelog();
#endif
    }

    void Send(const Severity severity,
              const std::wstring& text)
    {
        std::wstring textAscii = text;
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
            // fatal errors are written to std::wcerr
            case Emergency:
            {
                std::wcerr << L"EMERGENCY: " << textAscii << std::endl;
                lllog(0) << L"EMERGENCY: " << textAscii << std::endl;
            }
            break;

            case Alert:
            {
                std::wcerr << L"ALERT: " << textAscii << std::endl;
                lllog(0) << L"ALERT: " << textAscii << std::endl;
            }
            break;

            case Critical:
            {
                std::wcerr << L"CRITICAL: " << textAscii << std::endl;
                lllog(0) << L"CRITICAL: " << textAscii << std::endl;
            }
            break;

            case Error:
            {
                std::wcerr << L"ERROR: " << textAscii << std::endl;
                lllog(0) << L"ERROR: " << textAscii << std::endl;
            }
            break;

            case Warning:
            case Notice:
            case Informational:
            case Debug:
            {
                // No output to std::wcerr/lllog in these cases.
                ;
            }
            break;

            default:
                FatalError(L"SystemLogImpl::SendNativeLog: Unknown severity!");
        }

        if (m_nativeLogging)
        {
            SendNativeLog(severity, ReplaceNewlines(text));
        }

        if (m_sendToSyslogServer)
        {
            // Utf-8 is used when sending to a syslog server
            SendToSyslogServer(severity, ToUtf8(ReplaceNewlines(text)));
        }
    }

private:

    //-------------------------------------------------------------------------
    void SendNativeLog(const Severity severity, const std::wstring& text)
    {
#if defined(linux) || defined(__linux) || defined(__linux__)

        syslog(SAFIR_FACILITY | severity, "%s", ToUtf8(text).c_str());

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
                FatalError(L"LogImpl::SendNativeLog: Unknown severity!");
        }

        boost::lock_guard<boost::mutex> lck(m_lock);

        m_eventLog.Send(eventType, text);
#endif
    }

    //-------------------------------------------------------------------------
    void SendToSyslogServer(const Severity severity,
                            const std::string& text)
    {
        std::ostringstream log;
        log << "<" << (SAFIR_FACILITY | severity) << ">"
            << GetSyslogTimestamp() << ' '
            << boost::asio::ip::host_name() << ' '
            << m_processName << "[" << m_pid << "]: " << text;

        std::string logStr = log.str();

        const size_t LOG_MAX_SIZE = 1024;

        // RFC 3164 says that we must not send messages larger that 1024 bytes.
        if (logStr.size() > LOG_MAX_SIZE)
        {
            // truncate string ...
            logStr.erase(LOG_MAX_SIZE - 3, std::string::npos);

            // ... and put in "..." to indicate this
            logStr += "...";

        }

        // The asio socket is not thread safe
        boost::lock_guard<boost::mutex> lck(m_lock);

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

    //-------------------------------------------------------------------------
    void FatalError(const std::wstring& errTxt)
    {
        SendNativeLog(Critical, errTxt);
        std::wcerr << errTxt << std::endl;
        throw std::logic_error(ToUtf8(errTxt));
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
    boost::asio::ip::udp::endpoint  m_syslogServerEndpoint;
    boost::asio::io_service         m_service;
    boost::asio::ip::udp::socket    m_sock;
    boost::mutex                    m_lock;

#ifdef _MSC_VER
#pragma warning(pop)
#endif

};

class SystemLogImplKeeper
{
public:
    static SystemLogImplKeeper& Instance()
    {
        boost::call_once(SingletonHelper::m_onceFlag,boost::bind(SingletonHelper::Instance));
        
        if (destroyed)
        {
            throw std::runtime_error("Dead reference detected for singleton SystemLogImplKeeper");
        }
            
        return SingletonHelper::Instance();
    }

    const boost::shared_ptr<SystemLogImpl> Get()
    {
        boost::lock_guard<boost::mutex> lck(m_lock);

        if (!m_impl)
        {
            m_impl = boost::shared_ptr<SystemLogImpl>(new SystemLogImpl());
        }
        return m_impl;
    }
    
    void Reset()
    {
        boost::lock_guard<boost::mutex> lck(m_lock);
        m_impl.reset();
    }

private:
    SystemLogImplKeeper()
    {
    }

    ~SystemLogImplKeeper()
    {
        boost::lock_guard<boost::mutex> lck(m_lock);

        m_impl.reset();
        destroyed = true;
    }

    boost::shared_ptr<SystemLogImpl> m_impl;
    boost::mutex                     m_lock;
    
    static bool                      destroyed;

    /**
     * This class is here to ensure that only the Instance method can get at the
     * instance, so as to be sure that boost call_once is used correctly.
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
        static boost::once_flag m_onceFlag;
    };

};

//mandatory static initialization
boost::once_flag SystemLogImplKeeper::SingletonHelper::m_onceFlag = BOOST_ONCE_INIT;
bool SystemLogImplKeeper::destroyed = false;

void Open()
{
    try
    {
        SystemLogImplKeeper::Instance().Get();
    }
    catch (const std::runtime_error&)
    {
        // The singleton has been destroyed, most likely because Open is called from
        // another singleton's destructor.
        // We don't do anything in this case.
    }
}

void Send(const Severity severity, const std::wstring& text)
{
    try
    {
        SystemLogImplKeeper::Instance().Get()->Send(severity, text);
    }
    catch (const std::runtime_error&)
    {
        // The singleton has been destroyed, most likely because Send is called from
        // another singleton's destructor.
        // Instead of an approach where we try to resurrect the SystemLogImplKeeper singleton
        // (Phoenix Singleton) we just create a temporary SystemLogImpl on the stack for this
        // specific call to Send.
        SystemLogImpl log;
        log.Send(severity, text);
    }
}

void Close()
{
    try
    {
        SystemLogImplKeeper::Instance().Reset();
    }
    catch (const std::runtime_error&)
    {
        // The singleton has been destroyed, most likely because Close is called from
        // another singleton's destructor.
        // We don't do anything in this case.
    }
}

}
}
}
}

