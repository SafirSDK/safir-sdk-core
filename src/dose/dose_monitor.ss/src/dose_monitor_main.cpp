/******************************************************************************
*
* Copyright Saab AB, 2011 (http://www.safirsdk.com)
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
#include <iostream>
#include "ConnectionQueueMonitor.h"
#include <Safir/Utilities/Internal/LowLevelLogger.h>

#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable : 4251 4275 4512)
#endif

#include <boost/program_options.hpp>

#if defined _MSC_VER
  #pragma warning (pop)
#endif

namespace po = boost::program_options;

struct CommandLineResults
{
    int checkInterval;
    int maxQInactivityDuration;
    int noStalledQueues;
};


const CommandLineResults& HandleCommandLine(int argc, char* argv[])
{
    try
    {
        static CommandLineResults results;

        po::options_description desc("Allowed options");
        desc.add_options()
            ("help,h", "show help message")
            ("check-interval (seconds)", po::value<int>(&results.checkInterval)->default_value(20), "Interval between checks")
            ("max-queue-stall-time (seconds)", po::value<int>(&results.maxQInactivityDuration)->default_value(30),
             "How long to wait before a queue that isn't dispatched is considered to be stalled")
            ("no-stalled-queues", po::value<int>(&results.noStalledQueues)->default_value(1), "The number of queues that must be detected as stalled before dose_main is killed");

        po::variables_map vm;
        po::store(po::parse_command_line(argc, argv, desc), vm);
        po::notify(vm);

        if (vm.count("help"))
        {
            std::ostringstream ostr;
            ostr << desc;
            std::wcout << ostr.str().c_str() << std::endl;
            exit(0);
        }

        if (results.noStalledQueues < 1)
        {
            std::wcout << "Illegal parameter: 'no-stalled-queues' must be > 0!!" << std::endl;
            exit(-1);
        }

        return results;
    }
    catch (const std::exception & e)
    {
        std::wcout << "Got exception while parsing command line: "<< std::endl
            <<e.what() <<std::endl;
        exit(-1);
    }
}


int main(int argc, char* argv[])
{
    const CommandLineResults& commandLine = HandleCommandLine(argc,argv);

    std::wcout << "dose_monitor running ..." << std::endl;

    lllinfo << "dose_monitor parameters: "
            << "\ncheck-interval=" << commandLine.checkInterval
            << "\nmax-queue-stall-time=" << commandLine.maxQInactivityDuration
            << "\nno-stalled-queues=" << commandLine.noStalledQueues << std::endl;

    try
    {
        ConnectionQueueMonitor qMonitor(commandLine.checkInterval,
                                        boost::posix_time::seconds(commandLine.maxQInactivityDuration),
                                        commandLine.noStalledQueues);
        qMonitor.Start();

        while (ACE_Thread::join(NULL,NULL,NULL) != 0)
        {
            ACE_OS::sleep(10);
        }
    }
    catch(std::exception & e)
    {
        lllerr << "dose_monitor main Caught std::exception! Contents of exception is:\n"
                   << e.what() << std::endl;
    }
    catch (...)
    {
        lllerr << "dose_monitor main Caught ... exception!\n" << std::endl;
    }

    return 0;
}

