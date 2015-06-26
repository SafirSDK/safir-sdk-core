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

#include "DoseMainApp.h"
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/CrashReporter.h>
#include <boost/regex.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/asio.hpp>
#include <atomic>
#include <boost/filesystem/fstream.hpp>

//This is a sanity check to make sure we've taken down all the threads in dose_main
//before exiting. It is only implemented in Linux at the moment, which is okay since it
//is just an extra sanity check.
void CheckThreadCount()
{
#if defined(linux) || defined(__linux) || defined(__linux__)
    for (int i = 0;; ++i)
    {
        std::ifstream t("/proc/" + boost::lexical_cast<std::string>(getpid()) + "/status");
        const std::string str((std::istreambuf_iterator<char>(t)),
                              std::istreambuf_iterator<char>());

        const boost::regex re("^Threads:[[:space:]]*([0-9]+)$");
        boost::smatch what;

        if (!boost::regex_search(str,what,re))
        {
            throw std::runtime_error("Failed to read thread count for dose_main (no match in /proc file)");
        }

        if (what.size() != 2)
        {
            throw std::runtime_error("Failed to read thread count for dose_main (failed to parse /proc file)");
        }

        const auto threads = boost::lexical_cast<int>(what[1]);

        if (threads == 1)
        {
            return;
        }

        if (threads != 1 && i > 10)
        {
            throw std::logic_error("Unexpected number of threads in dose_main when exiting: " + what[1]);
        }

        boost::this_thread::sleep_for(boost::chrono::milliseconds(100));
    }

#endif
}

int main()
{
    //This log is required by the tests for checking that we don't do too much during elaboration
    lllog(1) << "dose_main entering main()" << std::endl;

    //ensure call to CrashReporter::Stop at application exit
    //Start is called in DoseMainApp
    boost::shared_ptr<void> crGuard(static_cast<void*>(0),
                                    [](void*){Safir::Utilities::CrashReporter::Stop();});

    boost::asio::io_service ioService;

    std::atomic<bool> success = true;

    try
    {
        Safir::Dob::Internal::DoseMainApp theApp(ioService);

        //Set number of threads to at least 2, or the number of cpu kernels
        auto nbrOfThreads = std::max<size_t>(10, boost::thread::hardware_concurrency());

        const auto run = [&ioService,&theApp,&success]
        {
            try
            {
                ioService.run();
                return;
            }
            catch (const std::exception & exc)
            {
                SEND_SYSTEM_LOG(Alert,
                                << "DOSE_MAIN: Caught 'std::exception' exception from io_service.run(): "
                                << "  '" << exc.what() << "'.");
                success.exchange(false);
            }
            catch (...)
            {
                SEND_SYSTEM_LOG(Alert,
                                << "DOSE_MAIN: Caught '...' exception from io_service.run().");
                success.exchange(false);
            }

            theApp.Stop();
        };

        boost::thread_group threads;
        for (unsigned int i = 0; i < nbrOfThreads-1; ++i)
        {
            threads.create_thread(run);
        }

        run();

        threads.join_all();

        crGuard.reset();

        //now check the thread count, all threads should be gone, except the main
        //thread.
        CheckThreadCount();
    }
    catch (const std::exception & exc)
    {
        SEND_SYSTEM_LOG(Alert,
                        << "DOSE_MAIN: Caught 'std::exception' exception: "
                        << "  '" << exc.what() << "'.");
        success.exchange(false);
    }
    catch (...)
    {
        SEND_SYSTEM_LOG(Alert,
                        << "DOSE_MAIN: Caught '...' exception.");
        success.exchange(false);
    }
    if (success)
    {
        std::wcout << "DOSE_MAIN: Exiting..." << std::endl;
    }
    else
    {
        std::wcout << "DOSE_MAIN: Exiting due to error..." << std::endl;
    }
    return success ? 0 : 1;
}
