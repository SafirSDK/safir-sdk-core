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

int main()
{
    //This log is required by the tests for checking that we don't do too much during elaboration
    lllog(1) << "dose_main entering main()" << std::endl;

    //ensure call to CrashReporter::Stop at application exit
    //Start is called in DoseApp
    boost::shared_ptr<void> guard(static_cast<void*>(0), 
                                  boost::bind(Safir::Utilities::CrashReporter::Stop));

    try 
    {
        {
            Safir::Dob::Internal::DoseApp theApp;
            theApp.Run();
        }
        std::wcout << "Exiting..." << std::endl;
        return 0;
    }
    catch (const std::exception & exc)
    {
        SEND_SYSTEM_LOG(Alert,
                        << "dose_main: Caught 'std::exception' exception: "
                        << "  '" << exc.what() << "'.");
    }
    catch (...)
    {
        SEND_SYSTEM_LOG(Alert,
                        << "dose_main: Caught '...' exception.");
    }
    return 1;
}

