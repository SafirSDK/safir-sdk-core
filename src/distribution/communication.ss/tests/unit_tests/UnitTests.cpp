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
#include "ReaderTest.h"
#include "WriterTest.h"
#include "MessageQueueTest.h"
#include "HeartbeatSenderTest.h"
#include "DeliveryHandlerTest.h"
#include "AckedDataSenderTest.h"
#include "DiscovererTest.h"


//**********************************
// Main - run all tests
//**********************************
int main(int argc, char* argv[])
{    
    if (argc>1)
    {
        std::string run(argv[1]);
        if (run=="MessageQueueTest")
        {
            MessageQueueTest test;
            test.Run();
        }
//        else if (run=="DeliveryHandlerTest")
//        {
//            DeliveryHandlerTest test;
//            test.Run();
//        }
//        else if (run=="ReaderTest")
//        {
//            ReaderTest test;
//            test.Run();
//        }
//        else if (run=="WriterTest")
//        {
//            WriterTest test;
//            test.Run();
//        }
//        else if (run=="HeartBeatSenderTest")
//        {
//            HeartBeatSenderTest test;
//            test.Run();
//        }
//        else if (run=="AckedDataSenderTest")
//        {
//            AckedDataSenderTest test;
//            test.Run();
//        }
//        else if (run=="DiscovererTest")
//        {
//            DiscovererTest test;
//            test.Run();
//        }
//        else
//        {
//            std::cout<<"Valid test case args are: MessageQueueTest, DeliveryHandlerTest, ReaderTest, WriterTest, DiscovererTest"<<std::endl;
//            return 0;
//        }
//        std::cout<<"================================="<<std::endl;
//        std::cout<<"All tests passed!"<<std::endl;
//        std::cout<<"================================="<<std::endl;
    }
    else //run all tests
    {
        {(MessageQueueTest()).Run();}
//        //{(DeliveryHandlerTest()).Run();}
//        //{(ReaderTest()).Run();}
//        //{(WriterTest()).Run();}
//        //{(HeartBeatSenderTest()).Run();}
//        {(AckedDataSenderTest()).Run();}
//        //{(DiscovererTest()).Run();}

//        //if we get here all tests passed without errors
//        std::cout<<"================================="<<std::endl;
//        std::cout<<"All tests passed!"<<std::endl;
//        std::cout<<"================================="<<std::endl;
    }
    return 0;
}

