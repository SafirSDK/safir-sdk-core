/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <atomic>
//disable warnings in boost
#if defined _MSC_VER
#  pragma warning (push)
#  pragma warning (disable : 4244)
#  pragma warning (disable : 4267)
#endif

#include <boost/thread.hpp>
#include <boost/asio.hpp>

#if defined _MSC_VER
#  pragma warning (pop)
#endif

#include <iostream>
int main(const int argc, const char* argv[])
{
    const bool async = argc == 2 && argv[1] == std::string("async");

    boost::asio::io_service ioService;
    boost::thread_group threads;
    boost::asio::signal_set signalSet(ioService);
    std::atomic<bool> done(false);
    
    if (async)
    {
        Safir::Utilities::Internal::Internal::LowLevelLogger::Instance().SwitchToAsynchronousMode(ioService);
        threads.create_thread([&ioService]{ioService.run();});

        //Only some async tests are going to be stopped using the signal
        //everyone else just kills this program...
#if defined (_WIN32)
        signalSet.add(SIGABRT);
        signalSet.add(SIGBREAK);
        signalSet.add(SIGINT);
        signalSet.add(SIGTERM);
#else
        signalSet.add(SIGQUIT);
        signalSet.add(SIGINT);
        signalSet.add(SIGTERM);
#endif
        signalSet.async_wait([&done](const boost::system::error_code& error,
                                     const int /*signal_number*/)
                             {
                                 if (error)
                                 {
                                     std::wcout << "error" << std::endl;
                                 }
                                 done = true;
                                 Safir::Utilities::Internal::Internal::LowLevelLogger::Instance().StopAsynchronousLogger();
                             });
    }

    while(!done)
    {
        lllog(5) << "Hello, World!"<<std::endl;
        lllog(9) << "Goodbye cruel world!"<<std::endl;
        lllog(1) << 1234567890 << std::endl;
        boost::this_thread::sleep_for(boost::chrono::milliseconds(10));
        std::wcout << "Logging at "
                   << Safir::Utilities::Internal::Internal::LowLevelLogger::Instance().LogLevel() << std::endl;
    }

    threads.join_all();
    Safir::Utilities::Internal::Internal::LowLevelLogger::Instance().DestroyAsynchronousLogger();
    std::wcout << "exiting nicely" << std::endl;
    return 0;
}
