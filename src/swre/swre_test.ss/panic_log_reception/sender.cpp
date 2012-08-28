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
#include <Safir/Utilities/Internal/PanicLogging.h>
#include <Safir/Dob/Connection.h>

class Dummy
    : public Safir::Dob::StopHandler
    , public Safir::Dob::Dispatcher
{
    void OnDoDispatch() {}
    void OnStopOrder() {}
};


int main()
{
    //open a connection, to ensure that dose_main et al have started before we send log.
    Dummy dummy;
    Safir::Dob::Connection conn;
    conn.Open(L"asdf",L"asdf",0,&dummy,&dummy);
    conn.Close();

    //send log
    Safir::Utilities::Internal::PanicLogging::Log("Testing PanicLogging");
    return 0;
}


