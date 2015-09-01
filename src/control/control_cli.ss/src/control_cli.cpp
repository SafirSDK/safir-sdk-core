/******************************************************************************
*
* Copyright Saab AB, 2015 (http://safir.sourceforge.net)
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
#include <boost/algorithm/string.hpp>
#include <boost/program_options.hpp>
#include <Safir/Dob/Internal/ControlCmd.h>

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
                ("action",  boost::program_options::value<std::string>(),
                 "Action (STOP|SHUTDOWN|REBOOT)")
                ("node-id", boost::program_options::value<int64_t>(&nodeId),
                 "Node id to send action to. If not given action will be sent to all nodes.");

        boost::program_options::positional_options_description pd;
        pd.add("action", 1);
        pd.add("node-id", 2);

        boost::program_options::variables_map vm;
        boost::program_options::store(boost::program_options::command_line_parser(argc, argv).options(desc).positional(pd).run(), vm);
        boost::program_options::notify(vm);

        if (vm.count("help"))
        {
            help=true;
            std::cout << desc << std::endl;
            return;
        }

        if (vm.count("action") == 1)
        {
            auto action = vm["action"].as<std::string>();

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
                std::cout << "Unknown action " << action << std::endl;
                help = true;
            }
        }
        else
        {
            std::cout << "Must specify exactly one action." << std::endl;
            help=true;
        }

        if (vm.count("node-id") == 1)
        {
            nodeCmd = true;
        }
    }

    bool help;
    Safir::Dob::Internal::Control::CommandAction cmdAction;
    bool nodeCmd;
    int64_t nodeId;
};

int main(int argc, char * argv[])
{
    Cmd cmd(argc, argv);
    if (cmd.help) return 0; //only show help

    boost::asio::io_service ioService;
    boost::shared_ptr<boost::asio::io_service::work> work (new boost::asio::io_service::work(ioService));

    Safir::Dob::Internal::Control::ControlCmdSender sender(ioService,
                                                           [&cmd, &sender, &work]()
                                                           {
                                                                if (cmd.nodeCmd)
                                                                {
                                                                    sender.NodeCmd(cmd.cmdAction,
                                                                                   cmd.nodeId);
                                                                }
                                                                else
                                                                {
                                                                    sender.SystemCmd(cmd.cmdAction);
                                                                }

                                                                sender.Stop();
                                                                work.reset();
                                                            });



    sender.Start();

    ioService.run();

    return 0;
}

