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
#include <boost/lexical_cast.hpp>
#include <ace/OS_NS_unistd.h>
#include <ace/Signal.h>
#include <ace/Thread.h>

using namespace Safir::Dob::Internal;


int main()
{
    ConnectResult result = Success;
    ConnectionPtr connection(NULL);

    do
    {
        Connections::Instance().Connect(std::string("Connector") + boost::lexical_cast<std::string>(rand()),
                                        0,
                                        result,
                                        connection);
    }
    while (result != Success);
    std::wcout << "Connected" << std::endl;

    unsigned int num = 0;
    boost::posix_time::ptime last = boost::posix_time::microsec_clock::universal_time();

    for(;;)
    {
        ++num;
        if (num == 100000)
        {
            const boost::posix_time::ptime now = boost::posix_time::microsec_clock::universal_time();
            const boost::posix_time::time_duration d = now - last;
            std::wcout << "Signalling at a rate of " << (1.0e9*num)/d.total_nanoseconds() << " Hz" << std::endl;

            last = now;
            num = 0;
        }
        ACE_Thread::yield();
        //        ACE_OS::sleep(ACE_Time_Value(0,0));
        connection->SignalOut();
    }

    return 0;
}


