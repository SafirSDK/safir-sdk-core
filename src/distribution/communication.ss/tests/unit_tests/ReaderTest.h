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
#ifndef _SAFIR_COM_READER_TEST_H_
#define _SAFIR_COM_READERTEST_H_

#include "fwd.h"

class ReaderTest
{
public:
    void Run()
    {
//        std::cout<<"Trace: "<<__LINE__<<std::endl;

//        //--------------
//        //set up Reader
//        //--------------
//        std::string lastReceived;
//        size_t receiveCount=0;
//        bool isReady=true;
//        boost::asio::io_service readerIos;
//        auto readerWrk=boost::make_shared<boost::asio::io_service::work>(readerIos);

//        auto onRecv=[&](const char* data, size_t size) -> bool
//        {
//            std::string str(data, data+size);
//            if (lastReceived==str)
//            {
//                std::cout<<"Recv duplicate"<<std::endl;
//            }
//            lastReceived=str;
//            ++receiveCount;
//            return isReady;
//        };
//        auto recvReady=[&]
//        {
//            std::cout<<"IsReady="<<isReady<<std::endl;
//            return isReady;
//        };
//        Com::Node testReaderNode("ReaderTest", 123, "127.0.0.1:10000", "239.192.1.1:11000");
//        Com::Reader reader(readerIos, testReaderNode, onRecv, recvReady);
//        reader.Start();

//        std::cout<<"Trace: "<<__LINE__<<std::endl;

//        //--------------------
//        //set up test sender
//        //--------------------
//        auto runTest=[&](const std::string& sendToAddress)
//        {
//            lastReceived.clear();
//            receiveCount=0;
//            isReady=true;

//            boost::asio::io_service senderIos;
//            boost::asio::io_service::work senderWrk(senderIos);
//            auto multicastEndpoint=Com::Node::CreateEndpoint(sendToAddress);
//            boost::asio::ip::udp::socket socket(senderIos, multicastEndpoint.protocol());

//            auto send=[&](const std::string& msg)
//            {
//                socket.send_to(boost::asio::buffer(msg.c_str(), msg.size()), multicastEndpoint);
//                //socket.send_to(boost::asio::buffer(msg.c_str(), msg.size()), testReaderNode.Endpoint());
//                senderIos.poll();
//            };

//            auto readUntil=[&](const std::string& expected)
//            {
//                size_t initialRecvCount=receiveCount;
//                for(;;)
//                {
//                    readerIos.poll();

//                    if (receiveCount>initialRecvCount)
//                    {
//                        std::cout<<"RecvUntil got "<<(receiveCount-initialRecvCount)<<" messages in the loop. LastRecv="<<lastReceived<<std::endl;
//                    }

//                    if (lastReceived==expected)
//                    {
//                        return;
//                    }

//                    Wait(50);
//                }
//            };

//            //----------------
//            // testing
//            //----------------
//            std::cout<<"Trace: "<<__LINE__<<std::endl;

//            send("1");
//            readUntil("1");

//            CHECK(lastReceived=="1");
//            send("2");
//            readUntil("2");
//            CHECK(lastReceived=="2");

//            std::cout<<"Trace: "<<__LINE__<<std::endl;

//            isReady=false;
//            send("3");//this is also expected to be received before isReady=false comes in effect. This is because there already is a posted Receive.
//            readUntil("3");
//            CHECKMSG(lastReceived=="3", lastReceived);

//            std::cout<<"Trace: "<<__LINE__<<std::endl;

//            send("4");
//            Wait(200);
//            readerIos.poll();
//            CHECKMSG(lastReceived=="3", lastReceived);
//            std::cout<<"Trace: "<<__LINE__<<std::endl;
//            isReady=true;
//            readUntil("4");
//            CHECKMSG(lastReceived=="4", lastReceived);
//            CHECKMSG(receiveCount==4, receiveCount);

//            std::cout<<"Trace: "<<__LINE__<<std::endl;

//            send("5");
//            send("6");
//            send("7");

//            CHECKMSG(lastReceived=="4", lastReceived);

//            std::cout<<"Trace: "<<__LINE__<<std::endl;

//            readUntil("7");
//            CHECKMSG(lastReceived=="7", lastReceived);

//            senderIos.stop();
//        };

//        std::cout<<"Trace: "<<__LINE__<<std::endl;
//        std::cout<<"Trace: start unicast test"<<std::endl;
//        //Run tests for unicast and multicast
//        runTest(testReaderNode.UnicastAddress());

//        std::cout<<"Trace: "<<__LINE__<<std::endl;
//        std::cout<<"Trace: start multicast test"<<std::endl;
//        runTest(testReaderNode.MulticastAddress());

//        readerWrk.reset(); //remove work
//        for (int i=0; i<10; ++i)
//        {
//            readerIos.poll();
//            CHECK(!readerIos.stopped()); //not expected to stop, should continue to receive
//            Wait(100);
//        }

//        std::cout<<"Stopping reader"<<std::endl;
//        reader.Stop();
//        for (int i=0; i<25; ++i)
//        {
//            readerIos.poll();
//            if (readerIos.stopped())
//                break;
//        }

//        CHECKMSG(readerIos.stopped(), "Stop reader failed");

//        std::cout<<"Reader tests passed"<<std::endl;

    }

private:

};

#endif
