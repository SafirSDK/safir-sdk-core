/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safir.sourceforge.net)
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
#include "DopeApp.h"
#include <Safir/Logging/Log.h>
#include <Safir/Application/CrashReporter.h>

int main()
{
    Safir::Application::ScopedCrashReporter scopedStartStop;

    try
    {
        DopeApp app;
        app.Run();
        return 0;
    }
    catch (const StartupError&)
    {
        //errors should already have been reported
    }
    catch (const std::exception& e)
    {
        Safir::Logging::SendSystemLog(Safir::Logging::Critical,
                                      L"Unhandled exception in main: " +
                                      Safir::Dob::Typesystem::Utilities::ToWstring(e.what()));
    }
    catch (...)
    {
        Safir::Logging::SendSystemLog(Safir::Logging::Critical,
                                      L"Unhandled '...' exception in main");
    }

    return 1;
}

