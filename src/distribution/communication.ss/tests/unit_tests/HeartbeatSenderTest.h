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
#ifndef _SAFIR_COM_HEART_BEAT_TEST_H_
#define _SAFIR_COM_HEART_BEAT_TEST_H_

#include "fwd.h"

class HeartbeatSenderTest
{
public:
    void Run()
    {
//        boost::asio::io_service io;
//        auto work=boost::make_shared<boost::asio::io_service::work>(io);

//        boost::thread_group threads;
//        for (int i = 0; i < 9; ++i)
//        {
//            threads.create_thread([&]{io.run();});
//        }

//        Com::Node me{"Test", 100, "127.0.0.1:10000", "239.192.1.1:11000"};
//        Com::HeartBeatSenderBasic<HeartbeatSenderTest::TestWriter> hb(io, me);
//        hb.Start();

//        Wait(Com::Parameters::HeartBeatInterval+200);
//        {
//            boost::mutex::scoped_lock lock(mutex);
//            CHECK(received.size()==0);
//        }

//        hb.AddNode(Com::Node("Test_1", 1, "127.0.0.1:10001", "239.192.1.1:11000"));
//        hb.AddNode(Com::Node("Test_2", 2, "127.0.0.1:10002", ""));
//        hb.AddNode(Com::Node("Test_3", 3, "127.0.0.1:10003", ""));
//        hb.AddNode(Com::Node("Test_4", 4, "127.0.0.1:10004", "239.192.1.1:11000"));
//        hb.SetSystemNode(1, true);
//        hb.SetSystemNode(2, true);
//        hb.SetSystemNode(3, true);
//        hb.SetSystemNode(4, true);

//        int last11000=0;
//        int last10002=0;
//        int last10003=0;
//        for (int i=0; i<3; ++i)
//        {
//            Wait(Com::Parameters::HeartBeatInterval+100);

//            boost::mutex::scoped_lock lock(mutex);
//            CHECK(received.size()==3);
//            CHECK(received[11000]>last11000);
//            CHECK(received[10002]>last10002);
//            CHECK(received[10003]>last10003);
//            last11000=received[11000];
//            last10002=received[10002];
//            last10003=received[10003];
//        }

//        std::cout<<"Stopping HeartBeatSender..."<<std::endl;
//        hb.Stop();
//        work.reset();
//        threads.join_all();
//        std::cout<<"HeartBeatSender stopped"<<std::endl;

//        last11000=received[11000];
//        last10002=received[10002];
//        last10003=received[10003];
//        Wait(Com::Parameters::HeartBeatInterval+100);
//        CHECK(received[11000]==last11000);
//        CHECK(received[10002]==last10002);
//        CHECK(received[10003]==last10003);

//        std::cout<<"HeartBeatSender tests passed"<<std::endl;
    }

private:
    static boost::mutex mutex;
    static std::map<unsigned short, int> received;

    struct TestSendPolicy
    {
        void Send(const boost::shared_ptr<Com::Heartbeat>& /*val*/,
                  boost::asio::ip::udp::socket& /*socket*/,
                  const boost::asio::ip::udp::endpoint& to)
        {
            boost::mutex::scoped_lock lock(mutex);
            auto it=HeartbeatSenderTest::received.find(to.port());
            if (it!=HeartbeatSenderTest::received.end())
            {
                ++(it->second);
            }
            else
            {
                HeartbeatSenderTest::received.insert(std::make_pair(to.port(), 1));
            }
        }
    };

    typedef Com::Writer<Com::Heartbeat, HeartbeatSenderTest::TestSendPolicy> TestWriter;
};

boost::mutex HeartbeatSenderTest::mutex;
std::map<unsigned short, int> HeartbeatSenderTest::received;

#endif
