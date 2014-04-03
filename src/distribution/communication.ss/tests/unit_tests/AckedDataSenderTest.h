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

        Com::Node me {"Test", 100, "127.0.0.1:10000", "239.192.1.1:11000"};
        Com::AckedDataSenderBasic<AckedDataSenderTest::TestWriter> sender(io, me);
        sender.SetNotFullCallback([=]{OnQueueNotFull();}, 50);
        sender.SetRetransmitCallback([=](boost::int64_t to){OnRetransmit(to);});
        sender.Start();

        //Add nodes
        sender.AddNode(Com::Node("Test_1", 1, "127.0.0.1:10001", "239.192.1.1:11000"));
        sender.AddNode(Com::Node("Test_2", 2, "127.0.0.1:10002", ""));
        sender.AddNode(Com::Node("Test_3", 3, "127.0.0.1:10003", ""));
        sender.AddNode(Com::Node("Test_4", 4, "127.0.0.1:10004", "239.192.1.1:11000"));
        sender.SetSystemNode(1, true);
        sender.SetSystemNode(2, true);
        sender.SetSystemNode(3, true);
        sender.SetSystemNode(4, true);

        //Add to send queue
        sender.AddToSendQueue(0, MakeShared("1"), 1, 0);
        sender.AddToSendQueue(0, MakeShared("2"), 1, 0);
        sender.AddToSendQueue(0, MakeShared("3"), 1, 0);
        sender.AddToSendQueue(0, MakeShared("4"), 1, 0);

        //Send Window

        //Full queue handling and QueueNotFull callback

        //Fragmentation

        //Ack

        //SystemNodes

        //SendAll and SendTo

        //Queue size

        //Stop
        //Wait(100);
        work.reset();
        sender.Stop();
        threads.join_all();

        Wait(10);
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

    void OnQueueNotFull()
    {
        std::cout<<"callback OnQueueNotFull"<<std::endl;

    }

    void OnRetransmit(boost::int64_t /*toId*/)
    {
        std::cout<<"callback OnRetransmit"<<std::endl;

    }

};

boost::mutex AckedDataSenderTest::mutex;
std::map<unsigned short, int> AckedDataSenderTest::received;

#endif
