/******************************************************************************
*
* Copyright Saab AB, 2011 (http://www.safirsdk.com)
*
* Created by: Lars Hagström
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
#include <Safir/Dob/Connection.h>
#include <iostream>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4244)
#endif

#include <boost/thread.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

class Dummy
    : public Safir::Dob::StopHandler
    , public Safir::Dob::Dispatcher
{
    void OnDoDispatch() {}
    void OnStopOrder() {}
};


int main(const int argc, const char* const argv [])
{
    if (argc != 2)
    {
        std::wcout << "Need one argument" << std::endl;
        return 1;
    }
    //first send some Safir::SwReports with an own connection (to get swre to attach, rather than
    //use bg thread.
    Dummy dummy;
    Safir::Dob::Connection conn;
    conn.Open(L"sender",L"",0,&dummy,&dummy);

    //These will be sent through the own connection
    Safir::SwReports::SendFatalErrorReport(L"FatalErrorCode", L"swreport_sender", L"Fatal error text 2 from " + Safir::Dob::Typesystem::Utilities::ToWstring(argv[1]));
    Safir::SwReports::SendErrorReport(L"ErrorCode", L"swreport_sender", L"Error text 2 from " + Safir::Dob::Typesystem::Utilities::ToWstring(argv[1]));
    Safir::SwReports::SendResourceReport(L"ResourceId", false, L"Resource report text 2 from " + Safir::Dob::Typesystem::Utilities::ToWstring(argv[1]));
    Safir::SwReports::SendProgrammingErrorReport(L"ProgrammingErrorCode", L"swreport_sender", L"Programming error text 2 from " + Safir::Dob::Typesystem::Utilities::ToWstring(argv[1]));
    Safir::SwReports::SendProgramInfoReport(L"Program info text 2 from " + Safir::Dob::Typesystem::Utilities::ToWstring(argv[1]));

    conn.Close();

    //send some Safir::SwReports without a connection (to get the bg thread to start)
    Safir::SwReports::SendFatalErrorReport(L"FatalErrorCode", L"swreport_sender", L"Fatal error text from " + Safir::Dob::Typesystem::Utilities::ToWstring(argv[1]));
    Safir::SwReports::SendErrorReport(L"ErrorCode", L"swreport_sender", L"Error text from " + Safir::Dob::Typesystem::Utilities::ToWstring(argv[1]));
    Safir::SwReports::SendResourceReport(L"ResourceId", false, L"Resource report text from " + Safir::Dob::Typesystem::Utilities::ToWstring(argv[1]));
    Safir::SwReports::SendProgrammingErrorReport(L"ProgrammingErrorCode", L"swreport_sender", L"Programming error text from " + Safir::Dob::Typesystem::Utilities::ToWstring(argv[1]));
    Safir::SwReports::SendProgramInfoReport(L"Program info text from " + Safir::Dob::Typesystem::Utilities::ToWstring(argv[1]));

    Safir::SwReports::Stop();
    return 0;
}


