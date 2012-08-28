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


int main()
{
    //first send some Safir::SwReports with an own connection (to get swre to attach, rather than
    //use bg thread.
    Dummy dummy;
    Safir::Dob::Connection conn;
    conn.Open(L"sender",L"",0,&dummy,&dummy);

    //These will be sent through the own connection
    Safir::SwReports::SendFatalErrorReport(L"FatalErrorCode", L"swreport_sender", L"Fatal error text 2");
    Safir::SwReports::SendErrorReport(L"ErrorCode", L"swreport_sender", L"Error text 2");
    Safir::SwReports::SendResourceReport(L"ResourceId", false, L"Resource report text 2");
    Safir::SwReports::SendProgrammingErrorReport(L"ProgrammingErrorCode", L"swreport_sender", L"Programming error text 2");
    Safir::SwReports::SendProgramInfoReport(L"Program info text 2");

    //sleep a little while to let dose_main empty queues. (Close does not
    //guarantee delivery of messages)
    boost::this_thread::sleep(boost::posix_time::milliseconds(200));

    conn.Close();

    //send some Safir::SwReports without a connection (to get the bg thread to start)
    Safir::SwReports::SendFatalErrorReport(L"FatalErrorCode", L"swreport_sender", L"Fatal error text");
    Safir::SwReports::SendErrorReport(L"ErrorCode", L"swreport_sender", L"Error text");
    Safir::SwReports::SendResourceReport(L"ResourceId", false, L"Resource report text");
    Safir::SwReports::SendProgrammingErrorReport(L"ProgrammingErrorCode", L"swreport_sender", L"Programming error text");
    Safir::SwReports::SendProgramInfoReport(L"Program info text");

    //sleep a little while to let dose_main empty queues.
    boost::this_thread::sleep(boost::posix_time::milliseconds(200));

    Safir::SwReports::Stop();
    return 0;
}


