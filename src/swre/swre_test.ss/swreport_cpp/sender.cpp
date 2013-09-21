/******************************************************************************
*
* Copyright Saab AB, 2013 (http://www.safirsdk.com)
*
* Created by: Lars Hagstrom / lars.hagstrom@consoden.se
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

//disable stupid incorrect microsoft warning.
#ifdef _MSC_VER
#pragma warning (disable : 4428)
#endif

int main()
{
    Safir::SwReports::SendFatalErrorReport(L"FatalError",
                                           L"here",
                                           L"Testing SendFatalErrorReport");

    Safir::SwReports::SendErrorReport(L"Error",
                                      L"there",
                                      L"Testing SendErrorReport");
    
    Safir::SwReports::SendResourceReport(L"ResourceReport",
                                         false,
                                         L"Testing SendResourceReport");

    Safir::SwReports::SendResourceReport(L"ResourceReport",
                                         true,
                                         L"Testing SendResourceReport");

    Safir::SwReports::SendProgrammingErrorReport(L"ProgrammingError",
                                                 L"everywhere",
                                                 L"Testing SendProgrammingErrorReport");

    Safir::SwReports::SendProgramInfoReport(L"Testing SendProgramInfoReport");


    Safir::SwReports::SendErrorReport(L"brynanuppafj\u00e4ssasponken", //ä
                                      L"Don't know\u203d", //interrobang
                                      L"Testing\nfunny characters");
    

    Safir::SwReports::SendProgramInfoReport(L"Finished!");


    return 0;
}


