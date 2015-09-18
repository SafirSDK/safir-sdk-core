/******************************************************************************
*
* Copyright Saab AB, 2015 (http://safirsdkcore.com)
*
* Created by: Anders Widén / anders.widen@consoden.se
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
#pragma once

#include <Safir/Utilities/Internal/StringEncoding.h>
#include <Safir/Utilities/Internal/SystemLog.h>

#include <boost/tokenizer.hpp>
#include <string>
#include <vector>
#include <iostream>

//disable warnings in boost
#if defined _MSC_VER
#  pragma warning (push)
#  pragma warning (disable : 4100 4267 4251)
#endif

#include "boost/process.hpp"
#include "boost/process/mitigate.hpp"
#include <boost/filesystem.hpp>

#if defined _MSC_VER
#  pragma warning (pop)
#endif

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace Control
{
    void Log(const std::string& msg)
    {
        SEND_SYSTEM_LOG(Error, << msg.c_str());
        std::wcout << msg.c_str() << std::endl;
    }

    std::vector<std::string> Split(const std::string& cmdLineStr)
    {
        std::vector<std::string> res;

        boost::tokenizer<boost::escaped_list_separator<char>>
                tok(cmdLineStr,
                boost::escaped_list_separator<char>('\\', ' ', '\"'));
        for(auto it = tok.begin(); it != tok.end(); ++it)
        {
            res.push_back(*it);
        }
        return res;
    }

    std::pair<std::vector<std::string>, bool> GetArgv(const std::string& cmdLineStr,
                                                      const std::string& parameter)
    {
        auto argv = Split(cmdLineStr);

        if (argv.size() < 1)
        {
            std::stringstream os;
            os << "CTRL: Invalid command line string '" << cmdLineStr << "' Check parameter "
               << parameter;
            Log(os.str());
            return std::make_pair(argv, false);
        }

        boost::filesystem::path p(argv[0]);

        if (!p.is_absolute())
        {
            std::stringstream os;
            os << "CTRL: Command '" << argv[0] << "' is not given as an absolute path. Check parameter "
               << parameter;
            Log(os.str());
            return std::make_pair(argv, false);
        }

        if (!boost::filesystem::exists(p))
        {
            std::stringstream os;
            os << "CTRL: Command '" << argv[0] << "' does not exist. Check parameter " << parameter;
            Log(os.str());
            return std::make_pair(argv, false);
        }

        return std::make_pair(argv, true);
    }

    std::pair<std::vector<std::wstring>, bool> GetArgvW(const std::string& cmdLineStr,
                                                        const std::string& parameter)
    {
        std::pair<std::vector<std::string>, bool> argv = GetArgv(cmdLineStr, parameter);

        std::vector<std::wstring> wideArgv;
        for (auto it = argv.first.begin(); it != argv.first.end(); ++it)
        {
            wideArgv.push_back(Safir::Utilities::Internal::ToUtf16(*it));
        }

        return std::make_pair(wideArgv, argv.second);
    }

    void ExecuteCmd(const std::string& cmdLineStr, const std::string& parameter)
    {
#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
        auto argv = GetArgvW(cmdLineStr, parameter);
#elif defined(linux) || defined(__linux) || defined(__linux__)
        auto argv = GetArgv(cmdLineStr, parameter);
#endif


        if (argv.second == false)
        {
            // There is something wrong with the command line string
            return;
        }

        boost::system::error_code ec;
        auto child = boost::process::execute(boost::process::initializers::set_args(argv.first),
                                             boost::process::initializers::set_on_error(ec));
        auto exitCode = boost::process::wait_for_exit(child);

        if (ec)
        {
            std::stringstream os;
            os << "CTRL: Can't execute command '" << cmdLineStr << "' Error: " << ec.message()
               << " Check parameter " << parameter << " and/or OS configuration";
            Log(os.str());
        }
        else
        {

#if defined(_WIN32) || defined(__WIN32__) || defined(WIN32)
            if (exitCode != 0)
            {
                std::stringstream os;
                os << "CTRL: Command '" << cmdLineStr << "' has exited with status code "  << exitCode
                   << " Check parameter " << parameter << " and/or OS configuration";
                Log(os.str());
            }

#elif defined(linux) || defined(__linux) || defined(__linux__)
            if (WIFEXITED(exitCode))
            {
                auto status = WEXITSTATUS(exitCode);

                if (status != 0)
                {
                    std::stringstream os;
                    os << "CTRL: Command '" << cmdLineStr << "' has exited with status code " << status
                       << " Check parameter " << parameter << " and/or OS configuration";
                    Log(os.str());
                }
            }
            else if (WIFSIGNALED(exitCode))
            {
                auto signal = WTERMSIG(exitCode);

                std::stringstream os;
                os << "CTRL: Command '" << cmdLineStr << "' has exited due to signal "
                   << strsignal(signal) << " ("  << signal << ") Check parameter " << parameter
                   << " and/or OS configuration";
                Log(os.str());
            }
            else
            {
                std::stringstream os;
                os << "CTRL: Command '" << cmdLineStr << "' has exited with unexpected status code"
                                        << " Check parameter " << parameter << " and/or OS configuration";
                Log(os.str());
            }
#endif
        }
    }


}
}
}
}



