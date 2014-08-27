/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safir.sourceforge.net)
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
#include "DataReceiverTest.h"
#include "MessageQueueTest.h"
#include "HeartbeatSenderTest.h"
#include "DeliveryHandlerTest.h"
#include "DataSenderTest.h"
#include "DiscovererTest.h"


//**********************************
// Main - run all tests
//**********************************
int main(int argc, char* argv[])
{    
    boost::chrono::steady_clock::now();

    if (argc>1)
    {
        std::string testcase(argv[1]);
        if (testcase=="MessageQueueTest")
        {
            MessageQueueTest::Run();
        }
        else if (testcase=="DeliveryHandlerTest")
        {
            DeliveryHandlerTest::Run();
        }
        else if (testcase=="DataReceiverTest")
        {
            DataReceiverTest::Run();
        }
        else if (testcase=="HeartbeatSenderTest")
        {
            HeartbeatSenderTest::Run();
        }
        else if (testcase=="DataSenderTest")
        {
            DataSenderTest::Run();
        }
        else if (testcase=="DiscovererTest")
        {
            DiscovererTest::Run();
        }
    }
    else //run all tests
    {
        MessageQueueTest::Run();
        HeartbeatSenderTest::Run();
        DataReceiverTest::Run();
        DataSenderTest::Run();
        DeliveryHandlerTest::Run();
        DiscovererTest::Run();
    }

    std::cout<<"================================="<<std::endl;
    std::cout<<"All tests passed!"<<std::endl;
    std::cout<<"================================="<<std::endl;
    return 0;
}

