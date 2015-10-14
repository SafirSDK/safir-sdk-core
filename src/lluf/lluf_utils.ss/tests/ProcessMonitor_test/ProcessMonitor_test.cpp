/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safirsdkcore.com)
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

#if defined _MSC_VER
#  pragma warning (push)
#  pragma warning (disable : 4244)
#  pragma warning (disable : 4267)
#  pragma warning (disable: 4100)
#endif

#include <boost/thread.hpp>
#include <boost/asio.hpp>

#if defined _MSC_VER
#  pragma warning (pop)
#endif

void callback(const pid_t pid);

boost::asio::io_service ioService;
Safir::Utilities::ProcessMonitor monitor(ioService, callback, boost::chrono::milliseconds(50));

boost::mutex mtx;
std::set<pid_t> pids;

void callback(const pid_t pid)
{
    boost::lock_guard<boost::mutex> lck(mtx);
    std::wcout << "Process with pid " << pid << " exited." << std::endl;
    pids.erase(pid);
    if(pids.empty())
    {
        monitor.Stop();
    }
}


int main(int argc, char** argv)
{
    std::wcout << "Starting" << std::endl;
    const std::vector<std::string> pidStrings(argv + 1, argv + argc);

    for(std::vector<std::string>::const_iterator it = pidStrings.begin();
        it != pidStrings.end(); ++it)
    {
        pids.insert(boost::lexical_cast<pid_t>(*it));
    }
    std::wcout << "Got " << pids.size() << " pids to monitor" << std::endl;

    std::wcout << "Starting thread" << std::endl;

    boost::shared_ptr<boost::asio::io_service::work> work(new boost::asio::io_service::work(ioService));
    boost::thread thread(boost::bind(&boost::asio::io_service::run,&ioService));

    std::wcout << "Adding pids to monitor" << std::endl;
    for(std::vector<std::string>::const_iterator it = pidStrings.begin();
        it != pidStrings.end(); ++it)
    {
        monitor.StartMonitorPid(boost::lexical_cast<pid_t>(*it));
    }
    work.reset();

    std::wcout << "Running io_service" << std::endl;
    ioService.run();
    std::wcout << "Joining thread" << std::endl;
    thread.join();
    std::wcout << "Done, pids remaining " << pids.size() << std::endl;
    return static_cast<int>(pids.size());
}
