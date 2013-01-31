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

#include "dose_main_app.h"
#include <Safir/Utilities/Internal/PanicLogging.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/CrashReporter.h>

int main()
{
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
        lllerr << "Exiting..." << std::endl;
        return 0;
    }
    catch (const std::exception & exc)
    {
        std::ostringstream ostr;
        ostr << "dose_main: Caught 'std::exception' exception: "
             << "  '" << exc.what() << "'." << std::endl;
        lllerr << ostr.str().c_str();
        Safir::Utilities::Internal::PanicLogging::Log(ostr.str());
    }
    catch (...)
    {
        std::ostringstream ostr;
        ostr << "dose_main: Caught '...' exception." <<std::endl;
        lllerr << ostr.str().c_str();
        Safir::Utilities::Internal::PanicLogging::Log(ostr.str());
    }
    return 1;
}

