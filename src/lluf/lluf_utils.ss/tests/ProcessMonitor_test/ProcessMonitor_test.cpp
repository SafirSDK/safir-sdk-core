/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
*
* Created by: Lars Hagström / stlrha
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
#include <ace/OS_NS_unistd.h>
#include <boost/lexical_cast.hpp>

#include <Safir/Utilities/ProcessMonitor.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>

#include <iostream>

void callback(const pid_t& pid)
{
    lllout  << "callback() - ### running ### pid: " << pid << std::endl;

}


int main(int argc, char** argv)
{
    lllout  << "main() - running..." << std::endl;
    
    Safir::Utilities::ProcessMonitor monitor;

    lllout  << "main() - about to init..." << std::endl;

    monitor.Init(callback);

    lllout  << "main() - about to loop..." << std::endl;

    for(int i = 1; i < argc; ++i)
    {
        monitor.StartMonitorPid(boost::lexical_cast<pid_t>(argv[i]));

        lllout  << "main() - about to sleep..." << std::endl;
        ACE_OS::sleep(1);
    }
    

    for(int i = 0; i < 10; ++i)
    {
        lllout  << "Sleeping..." << std::endl;
        ACE_OS::sleep(5);
    }


    for(int i = 1; i < argc; ++i)
    {
        monitor.StopMonitorPid(boost::lexical_cast<pid_t>(argv[i]));
        
        lllout  << "main() - about to sleep..." << std::endl;
        ACE_OS::sleep(1);
    }

    lllout  << "Stopping..." << std::endl;

    return 0;
}

