/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safir.sourceforge.net)
*
* Created by: Petter Lönnstedt / stpeln
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
#include "app.h"
#include <Safir/Dob/Typesystem/Exceptions.h>
#include <Safir/Logging/Log.h>
#include <Safir/Application/CrashReporter.h>

int main(int argc, char *argv[])
{
    Safir::Application::ScopedCrashReporter crashReporter;

    try
    {
        VehicleMmiCppQt::App a(argc, argv);    

        // Enters the main event loop and waits until exit() is called. 
        // It is necessary to call this function to start event handling. 
        // The main event loop receives events from the window system 
        // and dispatches these to the application widgets.
        a.exec();
        return 0;
    }
    catch (const std::exception & e)
    {
        std::string str(e.what());
        Safir::Logging::SendSystemLog(
                    Safir::Logging::Critical,
                    L"Unexpected exception " +
                    Safir::Dob::Typesystem::Utilities::ToWstring(str) +
                    L" in VehicleMmiCppQt");
    }
    catch (...)
    {
        Safir::Logging::SendSystemLog(
                    Safir::Logging::Critical,
                    L"catch(...) exception in VehicleMmiCppQt");
    }
    return 1;
}
