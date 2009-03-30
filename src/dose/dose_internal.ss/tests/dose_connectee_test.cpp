/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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

#include <iostream>
#include <Safir/Dob/Internal/Connections.h>

//this whole program is not up to date...

using namespace Safir::Dob::Internal;


class ConnectionCons:
    public Connections::ConnectionConsumer
{
    virtual ConnectResult CanAddConnection(const std::string & connectionName, const pid_t pid, const long context)
    { return Success; }
    virtual void HandleConnect(const ConnectionPtr & connection){std::wcout << connection->Name() << " has connected" << std::endl;}
    virtual void HandleDisconnect(const ConnectionPtr & connection){}
    virtual void DisconnectComplete(){}

};

void HandleConnect(const ConnectionPtr & connection, const long context)
{
    //    std::wcout << "HandleConnect was called with connection = " << connection->Name() << " " << connection->Id() << " and context = " << context << std::endl;
}

void HandleDisconnect(const ConnectionPtr & connection)
{
    //    std::wcout << "HandleDisconnect was called with connection = " << connection->Name() << " " << connection->Id() << std::endl;
}

unsigned int num = 0;
boost::posix_time::ptime last = boost::posix_time::microsec_clock::universal_time();

void HandleConnectionEvent(const ConnectionPtr & connection)
{
    ++num;
    if (num == 100000)
    {
        const boost::posix_time::ptime now = boost::posix_time::microsec_clock::universal_time();
        const boost::posix_time::time_duration d = now - last;
        std::wcout << "Getting signals at a rate of " << (1.0e9*num)/d.total_nanoseconds() << " Hz" << std::endl;

        last = now;
        num = 0;

    }
    //   std::wcout << "Handling event from " << connection->Name() << " id = " << connection->Id() << std::endl;
    if (connection->IsDead())
    {
        std::wcout << "  Connection is dead!" << std::endl;
        Connections::Instance().RemoveConnection(connection);
    }
}

int main()
{

    std::wcout << "Starting connectee" << std::endl;
    Connections::Instance().AllowConnect(-1);
    Connections::Instance().AllowConnect(0);
    ConnectionCons cons;

    for(;;)
    {
        bool connect;
        bool connectionOut;
        Connections::Instance().WaitForDoseMainSignal(connect, connectionOut);

        if (connect)
        {
            Connections::Instance().HandleConnect(cons);
        }
        if (connectionOut)
        {
            Connections::Instance().HandleConnectionOutEvents(HandleConnectionEvent);
        }
    }
    return 0;
}


