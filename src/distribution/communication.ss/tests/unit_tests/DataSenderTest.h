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
    static void Run()
    {
        std::cout<<"AckedDataSenderTest started"<<std::endl;

        boost::asio::io_service io;
        auto work=boost::make_shared<boost::asio::io_service::work>(io);
        boost::thread_group threads;
        for (int i = 0; i < 9; ++i)
        {
            threads.create_thread([&]{io.run();});
        }

        TRACELINE

        //-------------------
        // Tests
        //-------------------
        Sender sender(io, Com::Acked, 1, 1, 4, "127.0.0.1:10000", "224.90.90.241:10000", 500, Com::MessageHeaderSize+3); //ntId, nId, ipV, mc, waitForAck, fragmentSize

        std::atomic_uint go{0};
        auto WaitUntilReady=[&]
        {
            sender.m_strand.post([&]{go=1;});

            while(go==0)
                Wait(20);
            go=0;
        };


        sender.SetRetransmitCallback([=](int64_t id){std::cout<<"Retransmit to "<<id<<std::endl;});
        sender.SetNotFullCallback([=](int64_t id){std::cout<<"QueueNotFull nodeType "<<id<<std::endl;}, 50);
        sender.Start();
        sender.AddNode(2, "127.0.0.1:2");
        sender.AddNode(3, "127.0.0.1:3");
        sender.IncludeNode(2);
        sender.IncludeNode(3);

        //Check that welcome messages have been posted
        sender.m_strand.post([&]
        {
            CHECK(sender.m_nodes.size()==2);
            CHECK(sender.m_nodes.find(2)->second.systemNode==true);
            CHECK(sender.m_nodes.find(2)->second.welcome==1);
            CHECK(sender.m_nodes.find(3)->second.systemNode==true);
            CHECK(sender.m_nodes.find(3)->second.welcome==2);
            CHECKMSG(sender.SendQueueSize()==2, sender.SendQueueSize());
        });

        //Ack welcome messages
        TRACELINE
        sender.HandleAck(Ack(2, 1, 2, Com::MultiReceiverSendMethod));  //Ack(sender, receiver, seqNo, sendMethod)
        sender.HandleAck(Ack(3, 1, 2, Com::MultiReceiverSendMethod));  //Ack(sender, receiver, seqNo, sendMethod)
        sender.m_strand.post([&]
        {
            CHECKMSG(sender.SendQueueSize()==0, sender.SendQueueSize());
        });

        TRACELINE
        sender.m_strand.post([&]{CHECK(sender.m_nodes.size()==2);});

        TRACELINE
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

        TRACELINE

        WaitUntilReady();

        sender.RemoveNode(4);
        sender.RemoveNode(5);

        sender.m_strand.post([&]{CHECK(sender.m_nodes.size()==2);});

        TRACELINE
        //add messages, seq: 3,4,5
        sender.AddToSendQueue(0, MakeShared("1"), 1, 1); //toId, data, size, dataType
        sender.AddToSendQueue(0, MakeShared("2"), 1, 1);
        sender.AddToSendQueue(0, MakeShared("3"), 1, 1);


        TRACELINE
        sender.m_strand.post([&]
        {
            std::cout<<sender.SendQueueToString()<<std::endl;

            boost::mutex::scoped_lock lock(mutex);
            CHECK(sent.size()>0);
            std::string ss(sent.front()->fragment, sent.front()->header.fragmentContentSize);
            CHECK(GetSentData(0)=="1"); //seq 3
            CHECK(GetSentData(1)=="2"); //seq 4
            CHECK(GetSentData(2)=="3"); //seq 5
            CHECKMSG(sender.SendQueueSize()==3, sender.SendQueueSize());
        });

        TRACELINE
        WaitUntilReady();
        sender.HandleAck(Ack(2, 1, 3, Com::MultiReceiverSendMethod));  //Ack(sender, receiver, seqNo, sendMethod)
        sender.m_strand.post([&]{CHECKMSG(sender.SendQueueSize()==3, sender.SendQueueSize());});
        WaitUntilReady();
        sender.m_strand.post([&]{std::cout<<sender.SendQueueToString()<<std::endl;});
        WaitUntilReady();
        sender.HandleAck(Ack(3, 1, 5, Com::MultiReceiverSendMethod));
        sender.m_strand.post([&]{std::cout<<sender.SendQueueToString()<<std::endl;});
        sender.m_strand.post([&]{CHECKMSG(sender.SendQueueSize()==2, sender.SendQueueSize());});
        WaitUntilReady();
        sender.m_strand.post([&]{std::cout<<sender.SendQueueToString()<<std::endl;});
        WaitUntilReady();
        sender.HandleAck(Ack(2, 1, 5, Com::MultiReceiverSendMethod));
        sender.m_strand.post([&]{CHECKMSG(sender.SendQueueSize()==0, sender.SendQueueSize());});
        WaitUntilReady();
        sender.m_strand.post([&]{std::cout<<sender.SendQueueToString()<<std::endl;});

        TRACELINE
        sent.clear(); //clear sent just for convenience

        //Send fragmented message
        {
            std::string large="ABCDEFGHIJKLMNOPQRSTUVXYZ"; //9 fragments, seq: 6,7,8,9,10,11,12,13,14
            sender.AddToSendQueue(0, MakeShared(large), large.size(), 1);
        }

        TRACELINE
        sender.m_strand.post([&]
        {
            std::cout<<"--- SendQueue with fragmented message ---"<<std::endl;
            std::cout<<sender.SendQueueToString()<<std::endl;
            CHECKMSG(sender.SendQueueSize()==9, sender.SendQueueSize());
        });

        // now send queue looks like this waiting for both recv R2 and R3 to ack all messages:
        // ------------------------------------------------------------------------------------------------------------------------------------------------------
        // | seq 6 (R2,R3) | seq 7 (R2,R3) | seq 8 (R2,R3) | seq 9 (R2,R3) | seq 10 (R2,R3) | seq 11 (R2,R3) | seq 12 (R2,R3) | seq 13 (R2,R3) | seq 14 (R2,R3) |
        // ------------------------------------------------------------------------------------------------------------------------------------------------------

        {
            //R2 ack all except seq 10.
            auto ack=Ack(2, 1, 14, Com::MultiReceiverSendMethod);
            ack.missing[4]=1;  //index for seq 10 is calculated Ack.Seq-10, i.e 14-10=4
            sender.HandleAck(ack);
            sender.m_strand.post([&]{std::cout<<"R2 ack all but seq 10"<<std::endl; std::cout<<sender.SendQueueToString()<<std::endl; });
            sender.m_strand.post([&]{CHECKMSG(sender.SendQueueSize()==9, sender.SendQueueSize());});
        }
        // now send queue looks like this:
        // ------------------------------------------------------------------------------------------------------------------------------
        // | seq 6 (R3) | seq 7 (R3) | seq 8 (R3) | seq 9 (R3) | seq 10 (R2,R3) | seq 11 (R3) | seq 12 (R3) | seq 13 (R3) | seq 14 (R3) |
        // ------------------------------------------------------------------------------------------------------------------------------
        {
            //R3 ack all except seq 12.
            auto ack=Ack(3, 1, 14, Com::MultiReceiverSendMethod);
            ack.missing[2]=1;  //index for seq 12 is calculated Ack.Seq-12, i.e 14-12=2
            sender.HandleAck(ack);
            sender.m_strand.post([&]{std::cout<<"R3 ack all but seq 12"<<std::endl; std::cout<<sender.SendQueueToString()<<std::endl;});
            sender.m_strand.post([&]{CHECKMSG(sender.SendQueueSize()==5, sender.SendQueueSize());});
        }
        // now send queue looks like this:
        // --------------------------------------------------------
        // | seq 10 (R2) | seq 11 | seq 12 (R3) | seq 13 | seq 14 |
        // --------------------------------------------------------

        TRACELINE
        //R2 ack seq 10
        sender.HandleAck(Ack(2, 1, 10, Com::MultiReceiverSendMethod));
        sender.m_strand.post([&]{std::cout<<"R2 ack seq 10"<<std::endl; std::cout<<sender.SendQueueToString()<<std::endl;});
        sender.m_strand.post([&]{CHECKMSG(sender.SendQueueSize()==3, sender.SendQueueSize());});
        // now send queue looks like this:
        // ---------------------------------
        // | seq 12 (R3) | seq 13 | seq 14 |
        // ---------------------------------

        std::cout<<"*****Wait for resending 12 to R3"<<std::endl;
        Wait(5000);

        std::cout<<"*****Continue"<<std::endl;

        sender.HandleAck(Ack(3, 1, 12, Com::MultiReceiverSendMethod));
        sender.m_strand.post([&]{std::cout<<"R3 ack seq 12"<<std::endl; std::cout<<sender.SendQueueToString()<<std::endl;});

        //now send queue is empty, when 12 is acked also 13 and 14 are removed since they are already acked
        sender.m_strand.post([&]{CHECKMSG(sender.SendQueueSize()==0, sender.SendQueueSize());});

        TRACELINE
        sender.m_strand.post([&]
        {
            sender.Stop();
            work.reset();
        });

        TRACELINE

        threads.join_all();
        std::cout<<"AckedDataSenderTest tests passed"<<std::endl;
    }

private:

    static boost::mutex mutex;
    static std::vector< boost::shared_ptr<Com::UserData> > sent;

    static Com::Ack Ack(int64_t sender, int64_t receiver, uint64_t seqNo, uint8_t sendMethod)
    {
        Com::Ack a(sender, receiver, seqNo, sendMethod);
        for (size_t i=0; i<Com::Parameters::SlidingWindowSize; ++i)
        {
            a.missing[i]=0;
        }
        return a;
    }

    static std::string GetSentData(size_t index)
    {
        const auto& p=sent[index];
        return std::string(p->fragment, p->header.fragmentContentSize);
    }

    struct TestSendPolicy
    {
        void Send(const boost::shared_ptr<Com::UserData>& val,
                  boost::asio::ip::udp::socket& /*socket*/,
                  const boost::asio::ip::udp::endpoint& to)
        {
            boost::mutex::scoped_lock lock(mutex);

            if (Com::IsCommunicationDataType(val->header.commonHeader.dataType))
            {
                return;
            }

            std::string s(val->fragment, val->header.fragmentContentSize);
            std::cout<<"Writer.Send to_port: "<<to.port()<<", seq: "<<val->header.sequenceNumber<<", data: '"<<s<<"'"<<std::endl;
            sent.push_back(val);
        }
    };

    typedef Com::Writer<Com::UserData, AckedDataSenderTest::TestSendPolicy> TestWriter;
    typedef Com::DataSenderBasic<TestWriter> Sender;

    static void OnQueueNotFull()
    {
        std::cout<<"callback OnQueueNotFull"<<std::endl;

    }

    static void OnRetransmit(int64_t /*toId*/)
    {
        std::cout<<"callback OnRetransmit"<<std::endl;

    }
};

boost::mutex AckedDataSenderTest::mutex;
std::vector< boost::shared_ptr<Com::UserData> > AckedDataSenderTest::sent;

//------------------------------------
// Unacked messages
//------------------------------------
class UnackedDataSenderTest
{
public:
    static void Run()
    {
        std::cout<<"UnackedDataSenderTest started"<<std::endl;

        boost::asio::io_service io;
        auto work=boost::make_shared<boost::asio::io_service::work>(io);
        boost::thread_group threads;
        for (int i = 0; i < 9; ++i)
        {
            threads.create_thread([&]{io.run();});
        }

        TRACELINE

        //-------------------
        // Tests
        //-------------------
        Sender sender(io, Com::Unacked, 1, 1, 4, "127.0.0.1:10000", "224.90.90.241:10000", 500, 10); //ntId, nId, ipV, mc, waitForAck, fragmentSize

        std::atomic_uint go{0};
        auto WaitUntilReady=[&]
        {
            sender.m_strand.post([&]{go=1;});

            while(go==0)
                Wait(20);
            go=0;
        };


        sender.SetRetransmitCallback([=](int64_t id){std::cout<<"Retransmit to "<<id<<std::endl;});
        sender.SetNotFullCallback([=](int64_t id){std::cout<<"QueueNotFull nodeType "<<id<<std::endl;}, 50);
        sender.Start();
        sender.AddNode(2, "127.0.0.1:2");
        sender.AddNode(3, "127.0.0.1:3");
        sender.IncludeNode(2);
        sender.IncludeNode(3);

        TRACELINE
        sender.m_strand.post([&]{CHECK(sender.m_nodes.size()==2);});

        TRACELINE
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

        TRACELINE

        WaitUntilReady();

        sender.RemoveNode(4);
        sender.RemoveNode(5);

        sender.m_strand.post([&]{CHECK(sender.m_nodes.size()==2);});

        TRACELINE
        sender.AddToSendQueue(0, MakeShared("1"), 1, 1);
        sender.AddToSendQueue(0, MakeShared("2"), 1, 1);
        sender.AddToSendQueue(0, MakeShared("3"), 1, 1);

        TRACELINE
        sender.m_strand.post([&]
        {
            boost::mutex::scoped_lock lock(mutex);
            CHECK(sent.size()>0);
            std::cout<<"SentSize: "<<sent.size()<<std::endl;
            std::string ss(sent.front()->fragment, sent.front()->header.fragmentContentSize);
            CHECK(ss=="1");
        });


        sender.m_strand.post([&]
        {
            boost::mutex::scoped_lock lock(mutex);
            CHECKMSG(sender.m_sendQueue.size()==0, sender.m_sendQueue.size());
            CHECKMSG(sender.SendQueueSize()==0, sender.SendQueueSize());
        });

        TRACELINE
        sender.m_strand.post([&]
        {
            sender.Stop();
            work.reset();
        });

        TRACELINE

        threads.join_all();
        std::cout<<"UnackedDataSenderTest tests passed"<<std::endl;
    }

private:

    static boost::mutex mutex;
    static std::queue< boost::shared_ptr<Com::UserData> > sent;

    struct TestSendPolicy
    {
        void Send(const boost::shared_ptr<Com::UserData>& val,
                  boost::asio::ip::udp::socket& /*socket*/,
                  const boost::asio::ip::udp::endpoint& to)
        {
            boost::mutex::scoped_lock lock(mutex);
            std::string s(val->fragment, val->fragment+val->header.fragmentContentSize);
            std::string ss(val->fragment, val->header.fragmentContentSize);
            std::cout<<"Writer.Send to_port: "<<to.port()<<", seq: "<<val->header.sequenceNumber<<", data: '"<<s<<"', '"<<ss<<"'"<<std::endl;
            sent.push(val);
        }
    };

    typedef Com::Writer<Com::UserData, UnackedDataSenderTest::TestSendPolicy> TestWriter;
    typedef Com::DataSenderBasic<TestWriter> Sender;

    static void OnQueueNotFull()
    {
        std::cout<<"callback OnQueueNotFull"<<std::endl;

    }

    static void OnRetransmit(int64_t /*toId*/)
    {
        std::cout<<"callback OnRetransmit"<<std::endl;

    }
};

boost::mutex UnackedDataSenderTest::mutex;
std::queue< boost::shared_ptr<Com::UserData> > UnackedDataSenderTest::sent;


struct DataSenderTest
{
    static void Run()
    {
        AckedDataSenderTest::Run();
        UnackedDataSenderTest::Run();
    }
};

#endif
