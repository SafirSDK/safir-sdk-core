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

#include "dose_main_app.h"
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/CrashReporter.h>
#include <boost/regex.hpp>
#include <boost/lexical_cast.hpp>


//This is a sanity check to make sure we've taken down all the threads in dose_main
//before exiting. It is only implemented in Linux at the moment, which is okay since it
//is just an extra sanity check.
void CheckThreadCount()
{
#if defined(linux) || defined(__linux) || defined(__linux__)
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

    if (threads != 1)
    {
        throw std::logic_error("Unexpected number of threads in dose_main when exiting: " + what[1]);
    }
#endif
}


int main()
{

    //ensure call to CrashReporter::Stop at application exit
    //Start is called in DoseApp
    boost::shared_ptr<void> crGuard(static_cast<void*>(0),
                                    [](void*){Safir::Utilities::CrashReporter::Stop();});

    try
    {
        Safir::Dob::Internal::DoseApp theApp;
        theApp.Run();

        crGuard.reset();

        //now check the thread count, all threads should be gone, except the main
        //thread.
        CheckThreadCount();
    }
    catch (const std::exception & exc)
    {
        SEND_SYSTEM_LOG(Alert,
                        << "dose_main: Caught 'std::exception' exception: "
                        << "  '" << exc.what() << "'.");
        return 1;
    }
    catch (...)
    {
        SEND_SYSTEM_LOG(Alert,
                        << "dose_main: Caught '...' exception.");
        return 1;
    }

    std::wcout << "Exiting..." << std::endl;
    return 0;
}
