/******************************************************************************
*
* Copyright Saab AB, 2015 (http://safirsdkcore.com/)
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

#include "StatusApp.h"
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/CrashReporter.h>
//#include <boost/regex.hpp>
//#include <boost/lexical_cast.hpp>
#include <boost/asio.hpp>
#include <boost/atomic.hpp>
//#include <boost/filesystem/fstream.hpp>


int main()
{

    //ensure call to CrashReporter::Stop at application exit
    //Start is called in StatusApp
    boost::shared_ptr<void> crGuard(static_cast<void*>(0),
                                    [](void*){Safir::Utilities::CrashReporter::Stop();});

    boost::asio::io_service ioService;

    boost::atomic<bool> success(true);

    try
    {
        StatusApp theApp(ioService);

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
                                << "SAFIR_STATUS: Caught 'std::exception' exception from io_service.run(): "
                                << "  '" << exc.what() << "'.");
                success.exchange(false);
            }
            catch (...)
            {
                SEND_SYSTEM_LOG(Alert,
                                << "SAFIR_STATUS: Caught '...' exception from io_service.run().");
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
    }
    catch (const std::exception & exc)
    {
        SEND_SYSTEM_LOG(Alert,
                        << "SAFIR_STATUS: Caught 'std::exception' exception: "
                        << "  '" << exc.what() << "'.");
        success.exchange(false);
    }
    catch (...)
    {
        SEND_SYSTEM_LOG(Alert,
                        << "SAFIR_STATUS: Caught '...' exception.");
        success.exchange(false);
    }
    if (success)
    {
        std::wcout << "SAFIR_STATUS: Exiting..." << std::endl;
    }
    else
    {
        std::wcout << "SAFIR_STATUS: Exiting due to error..." << std::endl;
    }
    return success ? 0 : 1;
}
