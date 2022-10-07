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

namespace Com = Safir::Dob::Internal::Com;

BOOST_AUTO_TEST_CASE( communication_reset_node_unicast )
{
    std::cout << "----- communication_reset_node_unicast -----" << std::endl;
    std::vector<Com::NodeTypeDefinition> nodeTypes
    {
        Com::NodeTypeDefinition(10, "S10", "224.90.90.241:10000", "", false, 1000, 10, 20, 10, {100}),
        Com::NodeTypeDefinition(11, "L11", "224.90.90.241:10011", "", true, 1000, 10, 20, 10, {100})
    };

    Receiver receiver(1, 11, nodeTypes);
    receiver.Start("127.0.0.1:10002", 2);

    Sender sender2(2, 10, nodeTypes);
    sender2.Start(11);

    Sender sender3(3, 10, nodeTypes);
    sender3.Start(11);

    // Wait until receiver got some messages from sender2
    auto waitSec = 0;
    while(receiver.GetRecvCount(2) < 300 && waitSec++ < 45)
    {
        boost::this_thread::sleep_for(boost::chrono::milliseconds(500));
    }
    std::cout << "recv sender2: " << receiver.GetRecvCount(2) << std::endl;
    BOOST_CHECK(receiver.GetRecvCount(2) >= 300);

    // This call will Stop and immediately Start Communication, and then inject a new seed.
    // After this call we do not expect to be connected to sender2 anymore. Instead we expect
    // to detect sender3 and start receiving data from sender3
    sender2.Exclude(1);
    receiver.DetachAndRestart("127.0.0.1:10003", 3);

    // Wait until receiver got some messages from sender3
    waitSec = 0;
    while(receiver.GetRecvCount(3) < 300 && waitSec++ < 45)
    {
        boost::this_thread::sleep_for(boost::chrono::milliseconds(500));
    }
    std::cout << "recv sender3: " << receiver.GetRecvCount(3) << std::endl;
    BOOST_CHECK(receiver.GetRecvCount(3) >= 300);

    // Stop is synchronous calls that wait until fully stopped.
    receiver.Stop();
    sender2.Stop();
    sender3.Stop();
}

BOOST_AUTO_TEST_CASE( communication_reset_node_multicast )
{
    std::cout << "----- communication_reset_node_multicast -----" << std::endl;
    std::vector<Com::NodeTypeDefinition> nodeTypes
    {
        Com::NodeTypeDefinition(10, "S10", "224.90.90.241:10000", "", false, 1000, 10, 20, 10, {100}),
        Com::NodeTypeDefinition(11, "L11", "224.90.90.241:10011", "", true, 1000, 10, 20, 10, {100})
    };

    Receiver receiver(1, 11, nodeTypes);
    receiver.Start("127.0.0.1:10002", 2);

    Sender sender2(2, 10, nodeTypes);
    sender2.Start(11);

    Sender sender3(3, 10, nodeTypes);
    sender3.Start(11);

    // Wait until receiver got some messages from sender2
    auto waitSec = 0;
    while(receiver.GetRecvCount(2) < 300 && waitSec++ < 45)
    {
        boost::this_thread::sleep_for(boost::chrono::milliseconds(500));
    }
    std::cout << "recv sender2: " << receiver.GetRecvCount(2) << std::endl;
    BOOST_CHECK(receiver.GetRecvCount(2) >= 300);

    // This call will Stop and immediately Start Communication, and then inject a new seed.
    // After this call we do not expect to be connected to sender2 anymore. Instead we expect
    // to detect sender3 and start receiving data from sender3
    sender2.Exclude(1);
    receiver.DetachAndRestart("127.0.0.1:10003", 3);

    // Wait until receiver got some messages from sender3
    waitSec = 0;
    while(receiver.GetRecvCount(3) < 300 && waitSec++ < 45)
    {
        boost::this_thread::sleep_for(boost::chrono::milliseconds(500));
    }
    std::cout << "recv sender3: " << receiver.GetRecvCount(3) << std::endl;
    BOOST_CHECK(receiver.GetRecvCount(3) >= 300);

    // Stop is synchronous calls that wait until fully stopped.
    receiver.Stop();
    sender2.Stop();
    sender3.Stop();
}



//int main() //MULTICAST
//{
//    const unsigned int numMsgRecv = 100;

//    auto check = [=](bool b) { if (!b) {std::cout << "***ERROR!!!" << std::endl; exit(1);}  };

//    for (int round = 0; round < 50; ++round)
//    {
//        std::cout << "--- Start round " << round << "---" << std::endl;

//        std::vector<Com::NodeTypeDefinition> nodeTypes
//        {
//            Com::NodeTypeDefinition(10, "S10", "224.90.90.241:10000", "", false, 1000, 10, 20, 10, {100}),
//            Com::NodeTypeDefinition(11, "L11", "224.90.90.241:10011", "", true, 1000, 10, 20, 10, {100})
//        };
//        Receiver receiver(1, 11, nodeTypes);
//        receiver.Start("127.0.0.1:10002", 2);

//        Sender sender2(2, 10, nodeTypes);
//        sender2.Start(11);

//        Sender sender3(3, 10, nodeTypes);
//        sender3.Start(11);

//        // Wait until receiver got some messages from sender2
//        auto waitSec = 0;
//        while(receiver.GetRecvCount(2) < numMsgRecv && !receiver.Error() && waitSec++ < 45)
//        {
//            boost::this_thread::sleep_for(boost::chrono::milliseconds(100));
//        }
//        std::cout << "recv sender2: " << receiver.GetRecvCount(2) << std::endl;
//        check(!receiver.Error());
//        check(receiver.GetRecvCount(2) >= numMsgRecv);

//        // This call will Stop and immediately Start Communication, and then inject a new seed.
//        // After this call we do not expect to be connected to sender2 anymore. Instead we expect
//        // to detect sender3 and start receiving data from sender3
//        std::cout << "Reset receiver" << std::endl;
//        sender2.Exclude(1);
//        receiver.DetachAndRestart("127.0.0.1:10003", 3);

//        // Wait until receiver got some messages from sender3
//        waitSec = 0;
//        while(receiver.GetRecvCount(3) < numMsgRecv && !receiver.Error() && waitSec++ < 45)
//        {
//            boost::this_thread::sleep_for(boost::chrono::milliseconds(100));
//        }
//        std::cout << "recv sender3: " << receiver.GetRecvCount(3) << std::endl;
//        check(!receiver.Error());
//        check(receiver.GetRecvCount(3) >= numMsgRecv);

//        // Stop is synchronous calls that wait until fully stopped.
//        receiver.Stop();
//        sender2.Stop();
//        sender3.Stop();

//        std::cout << "communication_reset_node_multicast!" << std::endl;
//        check(!receiver.Error());
//    }

//    std::cout << "Success! " << std::endl;
//    return 0;
//}

//int main() //UNICAST
//{
//    auto check = [=](bool b) { if (!b) {std::cout << "***ERROR!!!" << std::endl; exit(1);}  };

//    for (int round = 0; round < 100; ++round)
//    {
//        std::cout << "--- Start round " << round << "---" << std::endl;

//        std::vector<Com::NodeTypeDefinition> nodeTypes {Com::NodeTypeDefinition(10, "S10", "", "", false, 1000, 10, 20, 10, {100}), Com::NodeTypeDefinition(11, "L11", "", "", true, 1000, 10, 20, 10, {100})};
//        Receiver receiver(1, 11, nodeTypes);
//        receiver.Start("127.0.0.1:10002", 2);

//        Sender sender2(2, 10, nodeTypes);
//        sender2.Start();

//        Sender sender3(3, 10, nodeTypes);
//        sender3.Start();

//        // Wait until receiver got some messages from sender2
//        auto waitSec = 0;
//        while(receiver.GetRecvCount(2) < 300 && !receiver.Error() && waitSec++ < 45)
//        {
//            std::cout << "recv sender2: " << receiver.GetRecvCount(2) << std::endl;
//            boost::this_thread::sleep_for(boost::chrono::seconds(1));
//        }
//        std::cout << "recv sender2: " << receiver.GetRecvCount(2) << std::endl;
//        check(!receiver.Error());
//        check(receiver.GetRecvCount(2) >= 300);

//        // This call will Stop and immediately Start Communication, and then inject a new seed.
//        // After this call we do not expect to be connected to sender2 anymore. Instead we expect
//        // to detect sender3 and start receiving data from sender3
//        sender2.Exclude(1);
//        receiver.DetachAndRestart("127.0.0.1:10003", 3);

//        // Wait until receiver got some messages from sender3
//        waitSec = 0;
//        while(receiver.GetRecvCount(3) < 300 && !receiver.Error() && waitSec++ < 45)
//        {
//            std::cout << "recv sender3: " << receiver.GetRecvCount(3) << std::endl;
//            boost::this_thread::sleep_for(boost::chrono::seconds(1));
//        }
//        std::cout << "recv sender3: " << receiver.GetRecvCount(3) << std::endl;
//        check(!receiver.Error());
//        check(receiver.GetRecvCount(3) >= 300);

//        // Stop is synchronous calls that wait until fully stopped.
//        receiver.Stop();
//        sender2.Stop();
//        sender3.Stop();

//        std::cout << "communication_reset_node_unicast!" << std::endl;
//        check(!receiver.Error());
//    }

//    std::cout << "Success! " << std::endl;
//    return 0;
//}

