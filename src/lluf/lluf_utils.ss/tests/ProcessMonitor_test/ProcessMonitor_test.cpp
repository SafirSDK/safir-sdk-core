/******************************************************************************
*
* Copyright Saab AB, 2007-2011 (http://www.safirsdk.com)
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
#include <Safir/Utilities/ProcessMonitor.h>
#include <boost/lexical_cast.hpp>
#include <iostream>
#include <vector>
#include <set>

//disable warnings in boost
#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable : 4244)
#endif

#include <boost/thread.hpp>

#if defined _MSC_VER
  #pragma warning (pop)
#endif

//we assume that the callback occurs from the same thread every time, so no need to lock.
std::set<pid_t> pids;

void callback(const pid_t pid)
{
    std::wcout << "Process with pid " << pid << " exited." << std::endl;
    pids.erase(pid);
    if(pids.empty()) 
    {
        exit(0);
    }
}


int main(int argc, char** argv)
{
    { //scope for the temporary variables
        const std::vector<std::string> pidStrings(argv + 1, argv + argc);
        
        for(std::vector<std::string>::const_iterator it = pidStrings.begin();
            it != pidStrings.end(); ++it) 
        {
            pids.insert(boost::lexical_cast<pid_t>(*it));
        }
    }
    
    Safir::Utilities::ProcessMonitor monitor;

    monitor.Init(callback);

    for(std::set<pid_t>::iterator it = pids.begin(); it != pids.end(); ++it)
    {
        monitor.StartMonitorPid(*it);
    }

    boost::this_thread::sleep(boost::posix_time::seconds(3));

    return 0;
}

