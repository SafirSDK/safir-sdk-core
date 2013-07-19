/******************************************************************************
*
* Copyright Saab AB, 2013 (http://www.safirsdk.com)
*
* Created by: Anders Widén
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
#include <Safir/SwReports/SwReport.h>
#include <iostream>

int main()
{
    try
    {
        Safir::SwReports::SendFatalErrorReport(L"FatalErrorCode", L"swreport_sender", L"This is a fatal error text!");
        Safir::SwReports::SendErrorReport(L"ErrorCode", L"swreport_sender", L"This is an error text!");
        Safir::SwReports::SendResourceReport(L"ResourceId", false, L"This is a resource report text!");
        Safir::SwReports::SendProgrammingErrorReport(L"ProgrammingErrorCode", L"swreport_sender", L"This is a programming error text!");
        Safir::SwReports::SendProgramInfoReport(L"This is a program info text!");
    }
    catch (const std::exception& e)
    {
        std::wcout << "caught exception: " << e.what() << std::endl;
        return 1;
    }
    catch (...)
    {
        std::wcout << "caught exception" << std::endl;
        return 1;
    }

    return 0;
}


