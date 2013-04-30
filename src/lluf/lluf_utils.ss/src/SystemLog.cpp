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
#include <Safir/Utilities/SystemLog.h>
#include <Safir/Utilities/ProcessInfo.h>
#include <boost/weak_ptr.hpp>
#include <boost/thread/once.hpp>
#include <boost/thread/mutex.hpp>
#include <boost/property_tree/ini_parser.hpp>
#include <boost/property_tree/ptree.hpp>
#include <boost/filesystem/convenience.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/asio/io_service.hpp>
#include <boost/asio/ip/udp.hpp>
#include <boost/asio/ip/host_name.hpp>

#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable: 4244)
#endif

#include <boost/date_time/posix_time/posix_time.hpp>

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

namespace
{

// Syslog facility used for all Safir logs
const int SAFIR_FACILITY = (1 << 3); // 1 => user-level messages

std::string GetSyslogTimestamp()
{
    using namespace boost::posix_time;

    std::stringstream ss;

    ss.imbue(std::locale(ss.getloc(), new time_facet("%b %e %H:%M:%S")));
    ss << second_clock::local_time();
    return ss.str();
}

}

class LLUF_UTILS_API SystemLogImpl
{
    friend class SystemLogImplKeeper;

private:
    //constructor is private, to make sure only SystemLogImplKeeper can create it
    SystemLogImpl()
        : m_pid(Safir::Utilities::ProcessInfo::GetPid()),
          m_processName(Safir::Utilities::ProcessInfo(m_pid).GetProcessName()),
          m_nativeLogging(false),
          m_sendToSyslogServer(false),
          m_syslogServerEndpoint(),
          m_service(),
          m_sock(m_service),
#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
          m_eventLog(m_processName),
#endif
          m_lock()
    {
        try
        {

            using boost::property_tree::ptree;

            ptree root;

            read_ini(GetLogSettingsPath().string(), root);

            m_nativeLogging = root.get<bool>("SYSTEM-LOG.native-logging");
            m_sendToSyslogServer = root.get<bool>("SYSTEM-LOG.send-to-syslog-server");

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
                        boost::asio::ip::udp::endpoint(boost::asio::ip::address::from_string(root.get<std::string>("SYSTEM-LOG.syslog-server-address")),
                                                       root.get<unsigned short>("SYSTEM-LOG.syslog-server-port"));
            }

        }
        catch (const std::exception& e)
        {
            // Something really bad has happened, we have to stop executing
            FatalError(e.what());
        }
    }

public:
    ~SystemLogImpl()
    {
#if defined(linux) || defined(__linux) || defined(__linux__)

        closelog();
#endif
    }

    void Send(const SystemLog::Severity severity, const std::string& text)
    {
        boost::lock_guard<boost::mutex> lck(m_lock);

        //TODO TEST!!!!
        SendNativeLog(severity, text);

        if (m_nativeLogging)
        {
            SendNativeLog(severity, text);
        }

        if (m_sendToSyslogServer)
        {
            SendToSyslogServer(severity, text);
        }
    }

private:

    //-------------------------------------------------------------------------
    void SendNativeLog(const SystemLog::Severity severity, const std::string& text)
    {
        // To stop compiler warn about unused variables
        severity;
        text;

#if defined(linux) || defined(__linux) || defined(__linux__)

        syslog(SAFIR_FACILITY | severity, "%s", text.c_str());

#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
        m_eventLog.Send(text);
#endif
    }

    //-------------------------------------------------------------------------
    void SendToSyslogServer(const SystemLog::Severity severity, const std::string& text)
    {
        std::stringstream log;
        log << "<" << (SAFIR_FACILITY | severity) << ">"
            << GetSyslogTimestamp() << ' '
            << boost::asio::ip::host_name() << ' '
            << m_processName << "[" << m_pid << "]: " << text;

        m_sock.send_to(boost::asio::buffer(log.str().c_str(),
                                           log.str().size()),
                       m_syslogServerEndpoint);
    }

    //-------------------------------------------------------------------------
    const boost::filesystem::path GetLogSettingsPath()
    {
        const char * const env = getenv("SAFIR_RUNTIME");
        if (env == NULL)
        {
            FatalError("SAFIR_RUNTIME environment variable is not set");
        }
        boost::filesystem::path filename(env);

        filename /= "log";
        filename /= "logging.ini";
        return filename;
    }

    //-------------------------------------------------------------------------
    void FatalError(const std::string& errTxt)
    {
        SendNativeLog(SystemLog::Critical, errTxt);
        std::wcerr << errTxt.c_str() << std::endl;
        throw std::logic_error(errTxt);
    }

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4251)
#endif

    pid_t                           m_pid;
    std::string                     m_processName;
    bool                            m_nativeLogging;
    bool                            m_sendToSyslogServer;

    boost::asio::ip::udp::endpoint  m_syslogServerEndpoint;
    boost::asio::io_service         m_service;
    boost::asio::ip::udp::socket    m_sock;

#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
    Internal::WindowsLogger         m_eventLog;
#endif

    boost::mutex                    m_lock;

#ifdef _MSC_VER
#pragma warning(pop)
#endif

};

/**
* A singleton that holds a weak pointer to the impl which means
* that this singleton will never keep an impl "alive" on its own.
*/
class SystemLogImplKeeper
{
public:
    static SystemLogImplKeeper& Instance()
    {
        boost::call_once(SingletonHelper::m_onceFlag,boost::bind(SingletonHelper::Instance));
        return SingletonHelper::Instance();
    }

    const boost::shared_ptr<SystemLogImpl> Get()
    {
        boost::lock_guard<boost::mutex> lck(m_lock);

        boost::shared_ptr<SystemLogImpl> impl = m_weakImpl.lock();

        if (!impl)
        {
            // There is no impl instance, create one!
            impl = boost::shared_ptr<SystemLogImpl>(new SystemLogImpl());
            m_weakImpl = impl;
        }
        return impl;
    }

private:
    SystemLogImplKeeper() {}
    ~SystemLogImplKeeper() {}

    boost::mutex m_lock;

    boost::weak_ptr<SystemLogImpl> m_weakImpl;

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

SystemLog::SystemLog()
    : m_impl(SystemLogImplKeeper::Instance().Get())
{
}

SystemLog::~SystemLog()
{
}

void SystemLog::Send(const Severity severity, const std::string& text)
{
    m_impl->Send(severity, text);
}

}
}

