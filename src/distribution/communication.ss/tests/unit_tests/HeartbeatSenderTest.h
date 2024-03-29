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

class HeartbeatSenderTest
{
public:
    static void Run()
    {
        std::cout<<"HeartBeatSender started"<<std::endl;
        const int Interval=1000;
        boost::asio::io_context io;
        auto work=boost::asio::make_work_guard(io);

        boost::thread_group threads;
        for (int i = 0; i < 9; ++i)
        {
            threads.create_thread([&]{io.run();});
        }

        TRACELINE

        //Multicast enabled HeartbeatSender
        Com::HeartbeatSenderBasic<HeartbeatSenderTest::TestWriter> hb1(io, 1, 100, 4, "127.0.0.1:10000", "239.192.1.1:11000", Interval);
        hb1.Start();
        int last11000=0;
        Wait(Interval+200);
        {
            boost::mutex::scoped_lock lock(mutex);
            CHECK(received[11000]>last11000);
            last11000=received[11000];
        }

        TRACELINE

        hb1.AddNode(10001, "127.0.0.1:10001");
        hb1.AddNode(10002, "127.0.0.1:10002");
        Wait(Interval+200);
        {
            boost::mutex::scoped_lock lock(mutex);
            CHECK(received[11000]>last11000);

            CHECK(received.find(10001)==received.end());
            CHECK(received.find(10002)==received.end());
        }

        TRACELINE
        //Only unicast enabled HeartbeatSender
        Com::HeartbeatSenderBasic<HeartbeatSenderTest::TestWriter> hb2(io, 1, 100, 4, "127.0.0.1:10000", "", Interval);
        hb2.Start();

        TRACELINE
        hb2.AddNode(10003, "127.0.0.1:10003");
        hb2.AddNode(10004, "127.0.0.1:10004");
        int last10003=0;
        int last10004=0;
        Wait(Interval+200);
        {
            boost::mutex::scoped_lock lock(mutex);
            CHECK(received[10003]>last10003);
            CHECK(received[10004]>last10004);
        }
        TRACELINE

        hb1.Stop();
        hb2.Stop();

        TRACELINE

        work.reset();
        threads.join_all();
        std::cout<<"HeartBeatSender tests passed"<<std::endl;
    }

private:
    static boost::mutex mutex;
    static std::map<unsigned short, int> received;

    struct TestSendPolicy
    {
        bool Send(const std::shared_ptr<Com::Heartbeat>& /*val*/,
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
            return true;
        }
    };

    typedef Com::Writer<Com::Heartbeat, HeartbeatSenderTest::TestSendPolicy> TestWriter;
};

boost::mutex HeartbeatSenderTest::mutex;
std::map<unsigned short, int> HeartbeatSenderTest::received;
