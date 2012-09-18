/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
*
* Created by: Anders Widén / stawi
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
#include "test_app.h"
#include <Safir/Application/Tracer.h>
#include <Safir/SwReports/SwReport.h>

bool cyclic = false;
bool noTracer = false;
bool panicLog = false;

int main(int argc, char* argv[])
{

    if (argc > 1)
    {
        if (argv[1] == std::string("-cyclic"))
        {
            cyclic = true;
        }
        else if (argv[1] == std::string("-notracer"))
        {
            noTracer = true;
        }
        else if (argv[1] == std::string("-paniclog"))
        {
            panicLog = true;
        }
    }
        
    Safir::SwReports::SwReportStarter s;

    Safir::Application::Tracer::SetProgramName(Safir::Dob::Typesystem::Utilities::ToWstring(argv[0]));

    // Run the application
    SwreTest::TestApp app;
    app.Run();

    return 0;

}

