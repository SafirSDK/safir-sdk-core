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
#include <boost/thread/once.hpp>
#include <boost/thread/mutex.hpp>
#include <boost/property_tree/ini_parser.hpp>
#include <boost/property_tree/ptree.hpp>
#include <boost/filesystem/convenience.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/asio/io_service.hpp>
#include <boost/asio/ip/udp.hpp>

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

#endif

namespace Safir
{
namespace Utilities
{

namespace
{

// Syslog facility used for all Safir logs
const int SAFIR_FACILITY = (1 << 3); // 1 => user-level messages

//-------------------------------------------------------------------------
const boost::filesystem::path GetLogSettingsPath()
{
    const char * const env = getenv("SAFIR_RUNTIME");
    if (env == NULL)
    {
        throw std::logic_error("SAFIR_RUNTIME environment variable is not set");
    }
    boost::filesystem::path filename(env);

    filename /= "log";
    filename /= "logging.ini";
    return filename;
}

//-------------------------------------------------------------------------
bool LogSettingsFileExists()
{
    try
    {
        return boost::filesystem::exists(GetLogSettingsPath());
    }
    catch(const std::logic_error&)
    {
        return false;
    }
}

std::string GetSyslogTimestamp()
{
    using namespace boost::posix_time;

    //AWI:More here
    ptime now = second_clock::local_time();
   //date today = now.date(); //Get the date part out of the time
}

}

class LLUF_UTILS_API SystemLogImpl
{
    friend class ImplKeeper;

private:
    //constructor is private, to make sure only ImplKeeper can create it
    SystemLogImpl()
        : m_nativeLogging(false),
          m_sendToSyslogServer(false),
          m_syslogServerEndpoint(),
          m_service(),
          m_sock(m_service),
          m_lock()
    {
        // Read configuration from file if it exists otherwise use default values
        if (LogSettingsFileExists())
        {
            try
            {

                using boost::property_tree::ptree;

                ptree root;
                read_ini(GetLogSettingsPath().string(), root);

                m_nativeLogging = root.get<bool>("native-logging");
                m_sendToSyslogServer = root.get<bool>("send-to-syslog-server");

                if (m_nativeLogging)
                {
#if defined(linux) || defined(__linux) || defined(__linux__)

                    // Include pid in log message
                    openlog(NULL, LOG_PID, SAFIR_FACILITY);

#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)

                    throw std::logic_error("Currently no implementation for native logging on Windows!");
#endif
                }

                if (m_sendToSyslogServer)
                {

                    m_sock.open(boost::asio::ip::udp::v4());
                    m_sock.bind(boost::asio::ip::udp::endpoint(boost::asio::ip::udp::v4(), 0));

                    m_syslogServerEndpoint =
                            boost::asio::ip::udp::endpoint(boost::asio::ip::address::from_string(root.get<std::string>("syslog-server-address")),
                                                           root.get<int>("syslog-server-port"));
                }

            }
            catch (const std::exception& e)
            {
                // If there is a configuration file, we expect it to contain the expected parameters
                throw std::logic_error("The logging configuration file " + GetLogSettingsPath().string() +
                                       " is missing an expected parameter or has a malformed parameter " +
                                       e.what());
            }
        }

    }

public:
    ~SystemLogImpl()
    {

    }

    void Send(const SystemLog::Severity severity, const std::string& text)
    {
        boost::lock_guard<boost::mutex> lck(m_lock);

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

    void SendNativeLog(const SystemLog::Severity severity, const std::string& text)
    {
#if defined(linux) || defined(__linux) || defined(__linux__)

        syslog(SAFIR_FACILITY | severity, "%s", text.c_str());

#endif
    }

    //-------------------------------------------------------------------------
    void SendToSyslogServer(const SystemLog::Severity severity, const std::string& text)
    {
        std::stringstream log;
        log << "<" << (SAFIR_FACILITY | severity) << ">";

    }

    bool                            m_nativeLogging;
    bool                            m_sendToSyslogServer;

    boost::asio::ip::udp::endpoint  m_syslogServerEndpoint;
    boost::asio::io_service         m_service;
    boost::asio::ip::udp::socket    m_sock;

    boost::mutex                    m_lock;
};

/**
* A singleton that holds a weak pointer to the impl which means
* that this singleton will never keep an impl "alive" on its own.
*/
class ImplKeeper
{
public:
    static ImplKeeper& Instance()
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
    ImplKeeper();
    ~ImplKeeper();

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
        friend ImplKeeper& ImplKeeper::Instance();

        static ImplKeeper& Instance()
        {
            static ImplKeeper instance;
            return instance;
        }
        static boost::once_flag m_onceFlag;
    };

};

//mandatory static initialization
boost::once_flag ImplKeeper::SingletonHelper::m_onceFlag = BOOST_ONCE_INIT;

SystemLog::SystemLog()
    : m_impl(ImplKeeper::Instance().Get())
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


//namespace // anynomous namespace for implementation
//{
//    boost::mutex m_lock;

//    bool configurationRead = false;
//    bool syslogOpened = false;
//    bool sendSocketOpened = false;

//    // Configuration parameters
//    bool nativeLogging = false;
//    bool sendToSyslogServer = false;
//    boost::asio::ip::address syslogServerAddr = boost::asio::ip::address::from_string("127.0.0.1");
//    boost::asio::ip::udp::endpoint

//    // Socket stuff
//    const boost::asio::ip::address addr =
//      boost::asio::ip::address::from_string("127.0.0.1");
//    const unsigned short port = 31221;
//    const boost::asio::ip::udp::endpoint endpoint(addr, port);

//    boost::asio::io_service         syslogIoService;
//    boost::asio::ip::udp::socket    syslogSocket(syslogIoService);
//    boost::asio::ip::udp::endpoint  syslogDestination(boost::asio::ip::address_v4::from_string(syslogServerAddr), syslogServerPort);

//    //-------------------------------------------------------------------------
//    const boost::filesystem::path GetLogSettingsPath()
//    {
//        const char * const env = getenv("SAFIR_RUNTIME");
//        if (env == NULL)
//        {
//            throw std::logic_error("SAFIR_RUNTIME environment variable is not set");
//        }
//        boost::filesystem::path filename(env);

//        filename /= "log";
//        filename /= "logging.ini";
//        return filename;
//    }

//    //-------------------------------------------------------------------------
//    bool LogSettingsFileExists()
//    {
//        try
//        {
//            return boost::filesystem::exists(GetLogSettingsPath());
//        }
//        catch(const std::logic_error&)
//        {
//            return false;
//        }
//    }

//    //-------------------------------------------------------------------------
//    void ReadConfiguration()
//    {
//        boost::lock_guard<boost::mutex> lck(m_lock);

//        if (configurationRead)
//        {
//            return;
//        }

//        if (LogSettingsFileExists())
//        {
//            try
//            {

//                using boost::property_tree::ptree;

//                ptree root;
//                read_ini(GetLogSettingsPath().string(), root);

//                nativeLogging = root.get("native-logging", false);
//                sendToSyslogServer = root.get("send-to-syslog-server", false);

//                if (sendToSyslogServer)
//                {
//                    syslogServerAddr = root.get("syslog-server-address", "127.0.0.1");
//                    syslogServerPort = root.get("syslog-server-port", 514);
//                }

//            }
//            catch (const std::exception&)
//            {
//                // If there is no configuration file, it is malformed, or some value is missing,
//                // we rely on the default values.
//            }
//        }

//        configurationRead = true;

//        std::cout << "Native logging i set to " << nativeLogging << std::endl;
//    }

//    //-------------------------------------------------------------------------
//    void SendNativeLog(const LogSeverity::Level severity, const std::string& text)
//    {
//#if defined(linux) || defined(__linux) || defined(__linux__)

//        if (!syslogOpened)
//        {
//            //For simplicity we hard code the facilty to LOG_USER (generic user-level messages)
//            openlog(NULL, LOG_PID, LOG_USER);
//            syslogOpened = true;
//        }

//        syslog(severity, "%s", text.c_str());

//#endif
//    }

//    //-------------------------------------------------------------------------
//    void SendToSyslogServer(const LogSeverity::Level severity, const std::string& text)
//    {
//        if (!sendSocketOpened)
//        {
//            syslogSocket.open(boost::asio::ip::udp::v4());
//        }
//    }
//}

//void SystemLog(const LogSeverity::Level severity, const std::string& text)
//{
//    if (!configurationRead)
//    {
//        ReadConfiguration();
//    }

//    if (nativeLogging)
//    {
//        SendNativeLog(severity, text);
//    }

//    if (sendToSyslogServer)
//    {
//        SendToSyslogServer(severity, text);
//    }


//}


//}
//}

