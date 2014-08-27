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
#ifndef _SAFIR_COM_DELIVERY_HANDLER_TEST_H_
#define _SAFIR_COM_DELIVERY_HANDLER_TEST_H_

#include "fwd.h"

class DeliveryHandlerTest
{
public:
    static void Run()
    {
        std::cout<<"DeliveryHandler started"<<std::endl;

        std::atomic_uint go{0};
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

        Com::DeliveryHandlerBasic<DeliveryHandlerTest::TestWriter> dh(io, 1, 4);
        dh.SetGotRecvCallback([=](int64_t id){DeliveryHandlerTest::GotReceiveFrom(id);});
        dh.SetReceiver([=](int64_t n, int64_t nt, const boost::shared_ptr<char[]>& d, size_t s){DeliveryHandlerTest::OnRecv(n, nt, d, s);}, 0);

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
//                      uint16_t sendMethod_,
//                      uint64_t sequenceNumber_,
//                      size_t totalContentSize_,
//                      size_t fragmentContentSize_,
//                      uint16_t numberOfFragments_,
//                      uint16_t fragmentNumber_,
//                      size_t fragmentOffset_)

        //Send one non-fragmented message to each node
        for (int64_t id=2; id<=4; ++id)
        {
            auto payload="hello";
            auto size=strlen(payload);
            Com::MessageHeader header(id, 1, 0, Com::MultiReceiverSendMethod, Com::Acked, 1, size, size, 1, 0, 0);
            header.crc=Com::CalculateCrc32(payload, size);
            dh.ReceivedApplicationData(&header, payload);
        }

        TRACELINE

        dh.m_deliverStrand.post([&]{SetReady();});
        WaitUntilReady();

        TRACELINE
        for (int64_t id=2; id<=4; ++id)
        {
            CHECK(received[id]==1);
            CHECK(acked[id]==1);
        }

        TRACELINE
        //Send fragmented message
        for (int frag=0; frag<4; ++frag)
        {
            const char* msg="ABCDEFGH";
            uint64_t seq=2+frag;
            const size_t fragmentSize=2;
            const uint16_t numberOfFragments=4;

            for (int64_t id=2; id<=4; ++id)
            {
                size_t fragmentOffset=2*frag;
                const char* payload=msg+fragmentOffset;

                Com::MessageHeader header(id, 1, 0, Com::MultiReceiverSendMethod, Com::Acked, seq, strlen(msg), fragmentSize, numberOfFragments, static_cast<uint16_t>(frag), fragmentOffset);
                header.crc=Com::CalculateCrc32(payload, static_cast<size_t>(fragmentSize));
                if (frag==3)
                    header.crc=Com::CalculateCrc32(msg, strlen(msg));
                dh.ReceivedApplicationData(&header, payload);
            }
        }

        TRACELINE
        dh.m_deliverStrand.post([&]{SetReady();});
        WaitUntilReady();

        TRACELINE
        DumpReceived();

        TRACELINE
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
                  const boost::asio::ip::udp::endpoint& /*to*/)
        {
            //send ack
            acked[ack->commonHeader.receiverId]=ack->sequenceNumber;
        }
    };

    typedef Com::Writer<Com::Ack, DeliveryHandlerTest::TestSendPolicy> TestWriter;


    static void OnRecv(int64_t fromNodeId, int64_t /*fromNodeType*/, const boost::shared_ptr<char[]>& data, size_t size)
    {
        std::string msg(data.get(), size);
        std::cout<<"OnRecv: "<<msg<<std::endl;
        received[fromNodeId]++;
    }

    static void GotReceiveFrom(int64_t /*fromNodeId*/)
    {
        //std::cout<<"GotReceiveFrom "<<fromNodeId<<std::endl;
    }

    static void DumpReceived()
    {
        for (auto& vt : received)
        {
            std::cout<<"node_"<<vt.first<<": received="<<vt.second<<", acked="<<acked[vt.first]<<std::endl;
        }
    }
};

boost::mutex DeliveryHandlerTest::mutex;
std::map<int64_t, int> DeliveryHandlerTest::received;
std::map<int64_t, uint64_t> DeliveryHandlerTest::acked;

#endif
