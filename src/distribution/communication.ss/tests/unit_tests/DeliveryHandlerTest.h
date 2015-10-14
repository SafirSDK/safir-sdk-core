/******************************************************************************
*
* Copyright Saab AB, 2013-2015 (http://safirsdkcore.com)
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
#pragma once

#include "fwd.h"

class DeliveryHandlerTest
{
public:
    static void Run()
    {
        std::cout<<"DeliveryHandler started"<<std::endl;

        boost::atomic<unsigned int> go(0);
        auto SetReady=[&]{go=1;};
        auto WaitUntilReady=[&]
        {
            while(go==0)
                Wait(20);
            go=0;
        };

        //const int Interval=1000;
        boost::asio::io_service io;
        auto work=boost::make_shared<boost::asio::io_service::work>(io);

        boost::thread_group threads;
        for (int i = 0; i < 9; ++i)
        {
            threads.create_thread([&]{io.run();});
        }
        boost::asio::io_service::strand strand(io);

        Com::DeliveryHandlerBasic<DeliveryHandlerTest::TestWriter> dh(strand, 1, 4);
        
        dh.SetGotRecvCallback(boost::bind(&DeliveryHandlerTest::GotReceiveFrom, _1));

        dh.SetReceiver(boost::bind(&DeliveryHandlerTest::OnRecv, _1, _2, _3, _4), 0, [=](size_t s){return new char[s];}, [](const char * data){delete[] data;});
        dh.Start();

        TRACELINE

        for (int64_t id=2; id<=4; ++id)
        {
            received[id]=0;
            acked[id]=0;
        }

        dh.AddNode(Com::Node("2", 2, 1, "127.0.0.1:2", "", true));
        dh.AddNode(Com::Node("3", 3, 1, "127.0.0.1:3", "", true));
        dh.AddNode(Com::Node("4", 4, 1, "127.0.0.1:4", "", true));

        TRACELINE
        dh.IncludeNode(2);
        dh.IncludeNode(3);
        dh.IncludeNode(4);

        TRACELINE
        CHECK(dh.NumberOfUndeliveredMessages()==0);

//        MessageHeader(int64_t senderId_,
//                      int64_t receiverId_,
//                      int64_t dataType_ ,
//                      uint8_t sendMethod_,
//                      uint8_t deliveryGuarantee_,
//                      uint64_t sequenceNumber_,
//                      size_t totalContentSize_,
//                      size_t fragmentContentSize_,
//                      uint16_t numberOfFragments_,
//                      uint16_t fragmentNumber_,
//                      size_t fragmentOffset_)

        //Send one non-fragmented from each node to node 1, before welcome is received.
        for (int64_t id=2; id<=4; ++id)
        {
            auto payload="hello";
            auto size=strlen(payload);
            Com::MessageHeader header(id, 1, 0, Com::MultiReceiverSendMethod, Com::Acked, 1, size, size, 1, 0, 0);
            header.ackNow=1;
            dh.ReceivedApplicationData(&header, payload);
        }

        TRACELINE

        dh.m_receiveStrand.post([&]{SetReady();});
        WaitUntilReady();
        dh.m_deliverStrand.post([&]{SetReady();});
        WaitUntilReady();

        TRACELINE
        for (int64_t id=2; id<=4; ++id)
        {
            CHECK(received[id]==0);
            CHECK(acked[id]==0);
        }

        CHECK(dh.m_nodes.find(2)->second.ackedMultiReceiverChannel.welcome==UINT64_MAX);
        CHECK(dh.m_nodes.find(3)->second.ackedMultiReceiverChannel.welcome==UINT64_MAX);
        CHECK(dh.m_nodes.find(4)->second.ackedMultiReceiverChannel.welcome==UINT64_MAX);

        TRACELINE


        //Send welcome
        for (int64_t id=2; id<=4; ++id)
        {
            int64_t welcomeNodeId=1;
            auto size=sizeof(int64_t);
            Com::MessageHeader header(id, 1, Com::WelcomeDataType, Com::MultiReceiverSendMethod, Com::Acked, 10, size, size, 1, 0, 0);
            header.ackNow=1;
            dh.ReceivedApplicationData(&header, reinterpret_cast<const char*>(&welcomeNodeId));
        }

        dh.m_receiveStrand.post([&]{SetReady();});
        WaitUntilReady();
        dh.m_deliverStrand.post([&]{SetReady();});
        WaitUntilReady();

        CHECK(dh.m_nodes.find(2)->second.ackedMultiReceiverChannel.welcome==10);
        CHECK(dh.m_nodes.find(3)->second.ackedMultiReceiverChannel.welcome==10);
        CHECK(dh.m_nodes.find(4)->second.ackedMultiReceiverChannel.welcome==10);

        TRACELINE
        for (int64_t id=2; id<=4; ++id)
        {
            CHECKMSG(received[id]==0, received[id]); //no application data received
            CHECKMSG(acked[id]==10, acked[id]); //we have acked the welcome
        }

        //Send one non-fragmented from each node to node 1, after welcome is received
        for (int64_t id=2; id<=4; ++id)
        {
            auto payload="hello";
            auto size=strlen(payload);
            Com::MessageHeader header(id, 1, 0, Com::MultiReceiverSendMethod, Com::Acked, 11, size, size, 1, 0, 0);
            header.ackNow=1;
            dh.ReceivedApplicationData(&header, payload);
        }

        TRACELINE

        dh.m_receiveStrand.post([&]{SetReady();});
        WaitUntilReady();
        dh.m_deliverStrand.post([&]{SetReady();});
        WaitUntilReady();

        TRACELINE
        for (int64_t id=2; id<=4; ++id)
        {
            CHECK(received[id]==1);
            CHECK(acked[id]==11);
        }

        TRACELINE

        //Send fragmented message
        for (int frag=0; frag<4; ++frag)
        {
            const char* msg="ABCDEFGH";
            uint64_t seq=12+frag;
            const size_t fragmentSize=2;
            const uint16_t numberOfFragments=4;

            for (int64_t id=2; id<=4; ++id)
            {
                size_t fragmentOffset=2*frag;
                const char* payload=msg+fragmentOffset;

                Com::MessageHeader header(id, 1, 0, Com::MultiReceiverSendMethod, Com::Acked, seq, strlen(msg), fragmentSize, numberOfFragments, static_cast<uint16_t>(frag), fragmentOffset);
                header.ackNow=1;
                dh.ReceivedApplicationData(&header, payload);
            }
        }

        dh.m_receiveStrand.post([&]{SetReady();});
        WaitUntilReady();
        dh.m_deliverStrand.post([&]{SetReady();});
        WaitUntilReady();

        TRACELINE
        for (int64_t id=2; id<=4; ++id)
        {
            CHECKMSG(received[id]==2, received[id]);
            CHECKMSG(acked[id]==15, acked[id]);
        }

        TRACELINE        

        //---------------------------------------------
        // Test unacked messages
        //---------------------------------------------
        //Send one non-fragmented message to each node
        for (int64_t id=2; id<=4; ++id)
        {
            auto payload="hello";
            auto size=strlen(payload);
            Com::MessageHeader header(id, 1, 0, Com::MultiReceiverSendMethod, Com::Unacked, 16, size, size, 1, 0, 0);
            dh.ReceivedApplicationData(&header, payload);
        }

        dh.m_receiveStrand.post([&]{SetReady();});
        WaitUntilReady();
        dh.m_deliverStrand.post([&]{SetReady();});
        WaitUntilReady();

        TRACELINE
        for (int64_t id=2; id<=4; ++id)
        {
            CHECKMSG(received[id]==3, received[id]);
            CHECKMSG(acked[id]==15, acked[id]);
        }

        DumpNodeInfo(dh);

        std::cout<<"========================="<<std::endl;
        std::cout<<"Unacked fragmented"<<std::endl;
        std::cout<<"========================="<<std::endl;
        //Send fragmented message
        for (int frag=0; frag<4; ++frag)
        {
            const char* msg="ABCDEFGH";
            uint64_t seq=17+frag;
            const size_t fragmentSize=2;
            const uint16_t numberOfFragments=4;

            for (int64_t id=2; id<=4; ++id)
            {
                size_t fragmentOffset=2*frag;
                const char* payload=msg+fragmentOffset;

                Com::MessageHeader header(id, 1, 0, Com::MultiReceiverSendMethod, Com::Unacked, seq, strlen(msg), fragmentSize, numberOfFragments, static_cast<uint16_t>(frag), fragmentOffset);
                dh.ReceivedApplicationData(&header, payload);
            }
        }

        dh.m_receiveStrand.post([&]{SetReady();});
        WaitUntilReady();
        dh.m_deliverStrand.post([&]{SetReady();});
        WaitUntilReady();

        TRACELINE
        for (int64_t id=2; id<=4; ++id)
        {
            CHECKMSG(received[id]==4, received[id]);
            CHECKMSG(acked[id]==15, acked[id]);
        }


        TRACELINE
        DumpNodeInfo(dh);
        DumpReceived();
        TRACELINE

        std::cout<<"========================="<<std::endl;
        std::cout<<"Unacked missed fragment"<<std::endl;
        std::cout<<"========================="<<std::endl;
        //Send one fragment in the middle, the entire message is supposed to be ignored
        //and expected seqNo should be the start of the next message.
        {
            const char* msg="12345678";
            uint64_t seq=22;
            const size_t fragmentSize=2;
            const uint16_t numberOfFragments=4;
            size_t fragmentOffset=2;
            const char* payload=msg+fragmentOffset;

            for (int64_t id=2; id<=4; ++id)
            {
                Com::MessageHeader header(id, 1, 0, Com::MultiReceiverSendMethod, Com::Unacked, seq, strlen(msg), fragmentSize, numberOfFragments, 1, fragmentOffset);
                dh.ReceivedApplicationData(&header, payload);
            }
        }

        dh.m_receiveStrand.post([&]{SetReady();});
        WaitUntilReady();
        dh.m_deliverStrand.post([&]{SetReady();});
        WaitUntilReady();

        TRACELINE
        for (int64_t id=2; id<=4; ++id)
        {
            CHECKMSG(received[id]==4, received[id]);
            CHECKMSG(acked[id]==15, acked[id]);
        }


        TRACELINE
        DumpNodeInfo(dh);
        DumpReceived();
        TRACELINE

        std::cout<<"========================="<<std::endl;
        std::cout<<"Unacked one non-fragmented"<<std::endl;
        std::cout<<"========================="<<std::endl;
        //Send one non-fragmented message to each node
        for (int64_t id=2; id<=4; ++id)
        {
            auto payload="hello";
            auto size=strlen(payload);
            Com::MessageHeader header(id, 1, 0, Com::MultiReceiverSendMethod, Com::Unacked, 25, size, size, 1, 0, 0);
            dh.ReceivedApplicationData(&header, payload);
        }

        dh.m_receiveStrand.post([&]{SetReady();});
        WaitUntilReady();
        dh.m_deliverStrand.post([&]{SetReady();});
        WaitUntilReady();

        TRACELINE
        DumpNodeInfo(dh);
        DumpReceived();

        TRACELINE
        for (int64_t id=2; id<=4; ++id)
        {
            CHECKMSG(received[id]==5, received[id]);
            CHECKMSG(acked[id]==15, acked[id]);
            CHECK(dh.m_nodes.find(id)->second.unackedMultiReceiverChannel.lastInSequence==25);
        }

        TRACELINE

        dh.Stop();
        work.reset();
        threads.join_all();
        std::cout<<"DeliveryHandler tests passed"<<std::endl;
    }


private:
    static boost::mutex mutex;
    static std::map<int64_t, int> received;
    static std::map<int64_t, uint64_t> acked;

    struct TestSendPolicy
    {
        void Send(const boost::shared_ptr<Com::Ack>& ack,
                  boost::asio::ip::udp::socket& /*socket*/,
                  const boost::asio::ip::udp::endpoint& to)
        {
            //send ack
            std::cout<<"Send Ack to port "<<to.port()<<" with seq "<<ack->sequenceNumber<<std::endl;
            acked[ack->commonHeader.receiverId]=ack->sequenceNumber;
        }
    };

    typedef Com::Writer<Com::Ack, DeliveryHandlerTest::TestSendPolicy> TestWriter;


    static void OnRecv(int64_t fromNodeId, int64_t /*fromNodeType*/, const char* data, size_t size)
    {
        std::string msg(data, size);
        //std::cout<<"OnRecv from "<<fromNodeId<<": "<<msg<<std::endl;
        received[fromNodeId]++;
        delete[] data; //receiver is responsible for deleting data
    }

    static void GotReceiveFrom(int64_t /*fromNodeId*/)
    {
        //std::cout<<"GotReceiveFrom "<<fromNodeId<<std::endl;
    }

    static void DumpReceived()
    {
        for (auto vt = received.cbegin(); vt != received.cend(); ++vt)
        {
            std::cout<<"node_"<<vt->first<<": received="<<vt->second<<", acked="<<acked[vt->first]<<std::endl;
        }
    }

    static void DumpNodeInfo(Com::DeliveryHandlerBasic<DeliveryHandlerTest::TestWriter>& dh)
    {
        for (auto vt = dh.m_nodes.begin(); vt != dh.m_nodes.end(); ++vt)
        {
            Com::DeliveryHandlerBasic<DeliveryHandlerTest::TestWriter>::NodeInfo& ni=vt->second;
            std::cout<<"Node: "<<ni.node.name<<std::endl;
            std::cout<<"    Channel: unacked_singel, lastInSeq: "<<ni.unackedSingleReceiverChannel.lastInSequence<<", biggestSeq: "<<ni.unackedSingleReceiverChannel.biggestSequence<<std::endl;
            std::cout<<"    Channel: unacked_multi, lastInSeq: "<<ni.unackedMultiReceiverChannel.lastInSequence<<", biggestSeq: "<<ni.unackedMultiReceiverChannel.biggestSequence<<std::endl;
            std::cout<<"    Channel: acked_single, lastInSeq: "<<ni.ackedSingleReceiverChannel.lastInSequence<<", biggestSeq: "<<ni.ackedSingleReceiverChannel.biggestSequence<<std::endl;
            std::cout<<"    Channel: acked_multi, lastInSeq: "<<ni.ackedMultiReceiverChannel.lastInSequence<<", biggestSeq: "<<ni.ackedMultiReceiverChannel.biggestSequence<<std::endl;
        }
    }
};

boost::mutex DeliveryHandlerTest::mutex;
std::map<int64_t, int> DeliveryHandlerTest::received;
std::map<int64_t, uint64_t> DeliveryHandlerTest::acked;
