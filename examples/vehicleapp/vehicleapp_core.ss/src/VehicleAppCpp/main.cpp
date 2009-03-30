/******************************************************************************
*
* Copyright Saab AB, 2008-2009 (http://www.safirsdk.com)
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

#include "App.h"
#include <Safir/Dob/Typesystem/Exceptions.h>
#include <Safir/SwReports/SwReport.h>

int main(int /*argc*/, char* /*argv*/[])
{
    try
    {
        VehicleAppCpp::App app;
        app.Run();
        return 1;
    }
    catch (const std::exception & e)
    {
        std::string str(e.what());
        Safir::SwReports::SendFatalErrorReport(
            L"0",
            L"main",
            Safir::Dob::Typesystem::Utilities::ToWstring(str));
        return 0;
   }
   catch (...)
   {
       Safir::SwReports::SendFatalErrorReport(L"0",L"main", L"Unhandled Exception");
       return 0; 
   }
}
