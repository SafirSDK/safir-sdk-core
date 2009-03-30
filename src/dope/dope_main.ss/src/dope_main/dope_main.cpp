/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
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
#include <Safir/SwReports/SwReport.h>

int main(int /*argc*/, char * argv[])
{
    try
    {
        Safir::Application::Tracer::SetProgramName(Safir::Dob::Typesystem::Utilities::ToWstring(argv[0]));
        DopeApp app;
        app.Run();
    }
    catch (const std::exception & e)
    {
        Safir::SwReports::SendFatalErrorReport(L"UnhandledException",
                                               L"main",
                                               Safir::Dob::Typesystem::Utilities::ToWstring(e.what()));
    }
    catch (...)
    {
        Safir::SwReports::SendFatalErrorReport(L"UnhandledException",
                                               L"main",
                                               L"A ... exception occurred somewhere in Dope. "
                                               L"Since it was not an exception derived from std::exception "
                                               L"I can't provide any more information, sorry.");
    }
    Safir::SwReports::Stop();
    return 0;
}

