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
#include <boost/thread/mutex.hpp>
#include <boost/property_tree/ini_parser.hpp>
#include <boost/property_tree/ptree.hpp>
#include <boost/filesystem/convenience.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/fstream.hpp>

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
    boost::mutex m_lock;

    bool configurationRead = false;

    bool syslogOpened = false;

    bool nativeLogging = false;
    bool syslogServerLogging = false;
    std::string syslogServerAddr = "127.0.0.1";
    unsigned short syslogServerPort = 514;

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

    void ReadConfiguration()
    {
        boost::lock_guard<boost::mutex> lck(m_lock);

        if (configurationRead)
        {
            return;
        }

        if (LogSettingsFileExists())
        {
            try
            {

                using boost::property_tree::ptree;

                ptree root;
                read_ini(GetLogSettingsPath().string(), root);

                nativeLogging = root.get("native-logging", false);
                sendToSyslogServer = root.get("send-to-syslog-server", false);

                if (sendToSyslogServer)
                {
                    syslogServerAddr = root.get("syslog-server-address", "127.0.0.1");
                    syslogServerPort = root.get("syslog-server-port", 514);
                }

            }
            catch (const std::exception&)
            {
                // If there is no configuration file, it is malformed, or some value is missing,
                // we rely on the default values.
            }
        }

        configurationRead = true;

        std::cout << "Native logging i set to " << nativeLogging << std::endl;
    }

    void NativeLog(const LogSeverity::Level severity, const std::string& text)
    {
#if defined(linux) || defined(__linux) || defined(__linux__)

        if (!syslogOpened)
        {
            //For simplicity we hard code the facilty to LOG_USER (generic user-level messages)
            openlog(NULL, LOG_PID, LOG_USER);
            syslogOpened = true;
        }

        syslog(severity, "%s", text.c_str());

#endif
    }
}

void SystemLog(const LogSeverity::Level severity, const std::string& text)
{
    if (!configurationRead)
    {
        ReadConfiguration();
    }

    if (nativeLogging)
    {
        NativeLog(severity, text);
    }


}


}
}
