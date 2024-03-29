/******************************************************************************
*
* Copyright Saab AB, 2013-2022 (http://safirsdkcore.com)
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
#include "ResolverTest.h"
#include "CommunicationAllocatorTest.h"

std::atomic<bool> Safir::Dob::Internal::Com::Parameters::NetworkEnabled;
std::string Safir::Dob::Internal::Com::Parameters::LogPrefix;
//**********************************
// Main - run all tests
//**********************************
int main(int argc, char* argv[])
{
    Safir::Dob::Internal::Com::Parameters::NetworkEnabled = true;
    Safir::Dob::Internal::Com::Parameters::LogPrefix = "ComUnitTests";
    boost::chrono::steady_clock::now();
    try
    {
        if (argc>1)
        {
            std::string testcase(argv[1]);
            std::wcout << "Will run test case " << testcase.c_str() << std::endl;
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
            else if (testcase=="ResolverTest")
            {
                ResolverTest::Run();
            }
            else if (testcase=="AllocatorTest")
            {
                AllocatorTest::Run();
            }
            else
            {
                std::wcout << "Unknown test" << std::endl;
                return 1;
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
            ResolverTest::Run();
            AllocatorTest::Run();
        }

        std::wcout<<"================================="<<std::endl;
        std::wcout<<"All tests passed!"<<std::endl;
        std::wcout<<"================================="<<std::endl;
        return 0;
    }
    catch (const std::exception& e)
    {
        std::wcout << "Caught exception during tests: " << e.what() << std::endl;
        return 1;
    }
    catch (...)
    {
        std::wcout << "Caught ... exception during tests" << std::endl;
        return 1;
    }
}

