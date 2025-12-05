/******************************************************************************
*
* Copyright Saab AB, 2015 (http://safirsdkcore.com)
*
* Created by: Anders Widén/ anders.widen@consoden.se
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
#include <iostream>
#include <sstream>
#include <boost/algorithm/string.hpp>
#include <boost/program_options.hpp>
#include <Safir/Dob/Internal/ControlCmd.h>
#include <Safir/Logging/Log.h>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#pragma warning (disable: 4100)
#endif

#include <boost/asio.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif


class Cmd
{
public:
    Cmd(int argc, char * argv[]) :
        help(false),
        nodeCmd(false)
    {
        boost::program_options::options_description desc("Command line options");
        desc.add_options()
            ("help,h", "This help message")
            ("action,a",  boost::program_options::value<std::string>(),
             "Action (STOP|SHUTDOWN|REBOOT)")
            ("node-id,n", boost::program_options::value<int64_t>(&nodeId),
             "Node id to send action to. If not given action will be sent to all nodes. -1 means 'current node'.")
            ("timeout,t", boost::program_options::value<uint64_t>(&timeOut)->default_value(5),
             "The time in seconds to wait for a IPC connection to safir_control.");

        try
        {
            boost::program_options::variables_map vm;
            boost::program_options::store(boost::program_options::command_line_parser(argc, argv).options(desc).run(), vm);
            boost::program_options::notify(vm);

            if (vm.count("help"))
            {
                help=true;
                std::ostringstream ostr;
                ostr << desc;
                std::wcout << ostr.str().c_str() << std::endl;
                return;
            }

            if (vm.count("action") == 1)
            {
                action = vm["action"].as<std::string>();

                boost::to_upper(action);

                if (action == "STOP")
                {
                    cmdAction = Safir::Dob::Internal::Control::STOP;
                }
                else if (action == "SHUTDOWN")
                {
                    cmdAction = Safir::Dob::Internal::Control::SHUTDOWN;
                }
                else if (action == "REBOOT")
                {
                    cmdAction = Safir::Dob::Internal::Control::REBOOT;
                }
                else
                {
                    std::wcout << "Unknown action " << action.c_str() << std::endl;
                    help = true;
                }
            }
            else
            {
                std::wcout << "Must specify exactly one action." << std::endl;
                help=true;
            }

            if (vm.count("node-id") == 1)
            {
                nodeCmd = true;
            }
        }
        catch (const boost::program_options::error& e)
        {
            std::wcerr << "Command line error: " << e.what() << std::endl;
            std::ostringstream ostr;
            ostr << desc;
            std::wcout << ostr.str().c_str() << std::endl;
            ok = false;
        }

    }

    bool help;
    std::string action;
    Safir::Dob::Internal::Control::CommandAction cmdAction;
    bool nodeCmd;
    int64_t nodeId;
    uint64_t timeOut;
    bool ok = true;
};

int main(int argc, char * argv[])
{
    Cmd cmd(argc, argv);
    if (cmd.help)
    {
        return 0; //only show help
    }
    else if (!cmd.ok)
    {
        return 1;
    }

    try
    {
        boost::asio::io_context io;
        auto work = boost::asio::make_work_guard(io);

        // Construct a sender using the new async SendCmd API.
        Safir::Dob::Internal::Control::ControlCmdSender sender(io);

        // Result flag that will be set from the completion callback.
        bool success = false;

        // Compute timeout duration from CLI argument (seconds).
        const std::chrono::milliseconds timeoutMs(
                                                  static_cast<std::chrono::milliseconds::rep>(cmd.timeOut) * 1000);

        // Issue the command.
        if (cmd.nodeCmd)
        {
            sender.SendCmd(cmd.cmdAction,
                           cmd.nodeId,
                           timeoutMs,
                           [&success, &work](const std::error_code& err)
                           {
                               // Success is signaled by an empty error_code.
                               success = !err;
                               work.reset();
                           });
        }
        else
        {
            sender.SendCmd(cmd.cmdAction,
                           0,
                           timeoutMs,
                           [&success, &work](const std::error_code& err)
                           {
                               success = !err;
                               work.reset();
                           });
        }

        // Run the io_context until the completion callback resets the work guard.
        io.run();

        return success ? 0 : 1;
    }
    catch (const std::exception& e)
    {
        std::wostringstream ostr;
        ostr << "control_cli fatal error: " << e.what() << ". Could not send "
             << cmd.action.c_str() << " to safir_control.";
        std::wcerr << ostr.str() << std::endl;
        Safir::Logging::SendSystemLog(Safir::Logging::Error, ostr.str());
        return 1;
    }
    catch (...)
    {
        std::wostringstream ostr;
        ostr << "control_cli fatal error: unknown exception. Could not send "
             << cmd.action.c_str() << " to safir_control.";
        std::wcerr << ostr.str() << std::endl;
        Safir::Logging::SendSystemLog(Safir::Logging::Error, ostr.str());
        return 1;
    }
}
