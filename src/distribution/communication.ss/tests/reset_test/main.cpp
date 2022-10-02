/******************************************************************************
*
* Copyright Saab AB, 2022 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson / joel.ottosson@consoden.se
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
#define BOOST_TEST_MODULE CommunicationResetTest
#include <boost/test/unit_test.hpp>

#include "Sender.h"
#include "Receiver.h"

#include <iostream>

#if defined _MSC_VER
#  pragma warning (push)
#  pragma warning (disable : 4100)
#endif

#include <boost/thread.hpp>

#if defined _MSC_VER
#  pragma warning (pop)
#endif

BOOST_AUTO_TEST_CASE( communication_reset_node_unicast )
{
    Receiver receiver(1);
    receiver.Start("127.0.0.1:10002", 2);

    Sender sender2(2);
    sender2.Start();

    Sender sender3(3);
    sender3.Start();

    // Wait until receiver got some messages from sender2
    auto waitSec = 0;
    while(receiver.GetRecvCount(2) < 300 && !receiver.Error() && waitSec++ < 45)
    {
        std::cout << "recv sender2: " << receiver.GetRecvCount(2) << std::endl;
        boost::this_thread::sleep_for(boost::chrono::seconds(1));
    }
    std::cout << "recv sender2: " << receiver.GetRecvCount(2) << std::endl;
    BOOST_CHECK(!receiver.Error());
    BOOST_CHECK(receiver.GetRecvCount(2) >= 300);

    // This call will Stop and immediately Start Communication, and then inject a new seed.
    // After this call we do not expect to be connected to sender2 anymore. Instead we expect
    // to detect sender3 and start receiving data from sender3
    receiver.DetachAndRestart("127.0.0.1:10003", 3);

    // Wait until receiver got some messages from sender3
    waitSec = 0;
    while(receiver.GetRecvCount(3) < 300 && !receiver.Error() && waitSec++ < 45)
    {
        std::cout << "recv sender3: " << receiver.GetRecvCount(3) << std::endl;
        boost::this_thread::sleep_for(boost::chrono::seconds(1));
    }
    std::cout << "recv sender3: " << receiver.GetRecvCount(3) << std::endl;
    BOOST_CHECK(!receiver.Error());
    BOOST_CHECK(receiver.GetRecvCount(3) >= 300);

    // Stop is synchronous calls that wait until fully stopped.
    receiver.Stop();
    sender2.Stop();
    sender3.Stop();

    std::cout << "Test finished!" << std::endl;
    BOOST_CHECK(!receiver.Error());
}
