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
#ifndef _SAFIR_COM_ACKED_DATA_SENDER_TEST_H_
#define _SAFIR_COM_ACKED_DATA_SENDER_TEST_H_

#include "fwd.h"

class AckedDataSenderTest
{
public:
    void Run()
    {
        boost::asio::io_service io;
        auto work=boost::make_shared<boost::asio::io_service::work>(io);
        boost::thread_group threads;
        for (int i = 0; i < 9; ++i)
        {
            threads.create_thread([&]{io.run();});
        }

        //-------------------
        // Tests
        //-------------------
        AckedSender sender(io, 1, 1, 4, "224.90.90.241:10000", 500); //ntId, nId, ipV, mc, waitForAck
        sender.SetRetransmitCallback([&](int64_t id){std::cout<<"Retransmit to "<<id<<std::endl;});
        sender.SetNotFullCallback([&](int64_t id){std::cout<<"QueueNotFull nodeType "<<id<<std::endl;}, 50);
        sender.Start();
        sender.AddNode(2, "127.0.0.1:2");
        sender.AddNode(3, "127.0.0.1:3");
        sender.IncludeNode(2);
        sender.IncludeNode(3);

        sender.m_strand.post([&]{CHECK(sender.m_nodes.size()==2);});

        sender.AddNode(4, "127.0.0.1:4");
        sender.AddNode(5, "127.0.0.1:5");

        sender.m_strand.post([&]
        {
            CHECK(sender.m_nodes.size()==4);
            CHECK(sender.m_nodes.find(2)->second.systemNode==true);
            CHECK(sender.m_nodes.find(3)->second.systemNode==true);
            CHECK(sender.m_nodes.find(4)->second.systemNode==false);
            CHECK(sender.m_nodes.find(5)->second.systemNode==false);
        });

        sender.RemoveNode(4);
        sender.RemoveNode(5);

        sender.m_strand.post([&]{CHECK(sender.m_nodes.size()==2);});

        sender.AddToSendQueue(0, MakeShared("1"), 1, 1);


        Wait(1000);
        sender.Stop();

        work.reset();
        threads.join_all();
        std::cout<<"AckedDataSenderTest tests passed"<<std::endl;
    }

private:

    static boost::mutex mutex;
    static std::map<unsigned short, int> received;

    struct TestSendPolicy
    {
        void Send(const boost::shared_ptr<Com::UserData>& val,
                  boost::asio::ip::udp::socket& /*socket*/,
                  const boost::asio::ip::udp::endpoint& to)
        {
            boost::mutex::scoped_lock lock(mutex);
            std::string s(val->fragment, val->fragment+val->header.fragmentContentSize);
            std::cout<<"Writer.Send to_port: "<<to.port()<<", seq: "<<val->header.sequenceNumber<<", data: "<<s<<std::endl;
        }
    };

    typedef Com::Writer<Com::UserData, AckedDataSenderTest::TestSendPolicy> TestWriter;
    typedef Com::AckedDataSenderBasic<TestWriter> AckedSender;

    void OnQueueNotFull()
    {
        std::cout<<"callback OnQueueNotFull"<<std::endl;

    }

    void OnRetransmit(int64_t /*toId*/)
    {
        std::cout<<"callback OnRetransmit"<<std::endl;

    }
};

boost::mutex AckedDataSenderTest::mutex;
std::map<unsigned short, int> AckedDataSenderTest::received;

#endif
