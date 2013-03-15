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
#include <Safir/Utilities/LogInterface.h>
#include <boost/program_options.hpp>
#include <fstream>
#include <iostream>

#if defined(linux) || defined(__linux) || defined(__linux__)

#include <syslog.h>

#elif defined(_WIN32) || defined(__WIN32__) || defined(WIN32)

#endif

namespace Safir
{
namespace Utilities
{

namespace po = boost::program_options;

namespace
{
    bool syslogOpened = false;

    bool nativeLogging;

    void getConfiguration()
    {
        po::options_description desc;
        desc.add_options()
                ("native-logging", po::value<bool>(&nativeLogging)->default_value(true));

        po::variables_map vm;

        std::ifstream settings_file("lluf.ini");

        // Clear the map.
        vm = po::variables_map();

        po::store(po::parse_config_file(settings_file , desc ), vm );
        settings_file.close();
        po::notify(vm);

        std::cout << "Native logging i set to " << nativeLogging << std::endl;
    }

    void generateNativeLog(const LogInterface::Severity severity, const std::string& text)
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

void LogInterface::Log(const Severity severity, const std::string& text)
{
    getConfiguration();
}


}
}
