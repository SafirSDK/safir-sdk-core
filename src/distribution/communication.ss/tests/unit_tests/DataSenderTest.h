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

class AckedDataSenderTest
{
public:
    static void Run()
    {
        std::wcout<<"AckedDataSenderTest started"<<std::endl;

        boost::asio::io_context io;
        auto work=boost::asio::make_work_guard(io);
        boost::thread_group threads;
        for (int i = 0; i < 9; ++i)
        {
            threads.create_thread([&]{io.run();});
        }

        TRACELINE

        //-------------------
        // Tests
        //-------------------
        std::vector<int> retryTimeout;
        retryTimeout.push_back(500);
        Sender sender(io, Com::Acked, 1, 1, 4, "127.0.0.1:10000", "224.90.90.241:10000", SlidingWindowSize, RequestAckThreshold, retryTimeout, Com::MessageHeaderSize+3); //ntId, nId, ipV, mc, waitForAck, fragmentSize

        std::atomic<unsigned int> go(0);
        auto WaitUntilReady=[&]
        {
            boost::asio::post(sender.m_strand, [&]{go=1;});

            while(go==0)
                Wait(20);
            go=0;
        };

        bool gotQueueNotFull1=false, gotQueueNotFull2=false;
        sender.SetNotFullCallback([&](int64_t id){gotQueueNotFull1=true; std::wcout<<"QueueNotFull 1 nodeType "<<id<<std::endl;});
        sender.SetNotFullCallback([&](int64_t id){gotQueueNotFull2=true; std::wcout<<"QueueNotFull 2 nodeType "<<id<<std::endl;});
        sender.SetRetransmitCallback([=](int64_t id, size_t tc){std::wcout<<"Retransmit to "<<id<<", transmitCount: "<<tc<<std::endl;});

        // Test request ack calculations
        {
            Com::MessageHeader header(1, 0, 1, Com::MultiReceiverSendMethod, Com::Acked, 1/*seq*/, 10, 10, 1, 0, 0);

            header.ackNow=3; //invalid value just to see that the correct is alway set
            header.sequenceNumber=20;
            sender.SetRequestAck(header);
            CHECK(header.ackNow==1);

            header.ackNow=3; //invalid value just to see that the correct is alway set
            header.sequenceNumber=21;
            sender.SetRequestAck(header);
            CHECK(header.ackNow==0);

            header.ackNow=3; //invalid value just to see that the correct is alway set
            header.sequenceNumber=22;
            sender.SetRequestAck(header);
            CHECK(header.ackNow==0);

            header.ackNow=3; //invalid value just to see that the correct is alway set
            header.sequenceNumber=23;
            sender.SetRequestAck(header);
            CHECK(header.ackNow==0);

            header.ackNow=3; //invalid value just to see that the correct is alway set
            header.sequenceNumber=24;
            sender.SetRequestAck(header);
            CHECK(header.ackNow==0);

            header.ackNow=3; //invalid value just to see that the correct is alway set
            header.sequenceNumber=25;
            sender.SetRequestAck(header);
            CHECK(header.ackNow==1);

            header.ackNow=3; //invalid value just to see that the correct is alway set
            header.sequenceNumber=1;
            sender.SetRequestAck(header);
            CHECK(header.ackNow==0);

            header.ackNow=3; //invalid value just to see that the correct is alway set
            header.sequenceNumber=1;
            header.sendMethod=Com::SingleReceiverSendMethod;
            sender.SetRequestAck(header);
            CHECK(header.ackNow==1);
        }

        sender.Start();

        sender.AddNode(2, "127.0.0.1:2");
        sender.AddNode(3, "127.0.0.1:3");
        sender.IncludeNode(2);
        sender.IncludeNode(3);

        //Check that welcome messages have been posted
        boost::asio::post(sender.m_strand, [&]
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
        boost::asio::post(sender.m_strand, [&]
        {
            CHECKMSG(sender.SendQueueSize()==0, sender.SendQueueSize());
        });

        TRACELINE
        boost::asio::post(sender.m_strand, [&]{CHECK(sender.m_nodes.size()==2);});

        TRACELINE
        sender.AddNode(4, "127.0.0.1:4");
        sender.AddNode(5, "127.0.0.1:5");

        boost::asio::post(sender.m_strand, [&]
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

        boost::asio::post(sender.m_strand, [&]{CHECK(sender.m_nodes.size()==2);});

        TRACELINE
        //add messages, seq: 3,4,5
        sender.AddToSendQueue(0, MakeShared("1"), 1, 1); //toId, data, size, dataType
        sender.AddToSendQueue(0, MakeShared("2"), 1, 1);
        sender.AddToSendQueue(0, MakeShared("3"), 1, 1);


        TRACELINE
        boost::asio::post(sender.m_strand, [&]
        {
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

        //Ack all messages
        sender.HandleAck(Ack(2, 1, 3, Com::MultiReceiverSendMethod));  //Ack(sender, receiver, seqNo, sendMethod)
        boost::asio::post(sender.m_strand, [&]{CHECKMSG(sender.SendQueueSize()==3, sender.SendQueueSize());});
        WaitUntilReady();
        sender.HandleAck(Ack(3, 1, 5, Com::MultiReceiverSendMethod));
        boost::asio::post(sender.m_strand, [&]{CHECKMSG(sender.SendQueueSize()==2, sender.SendQueueSize());});
        WaitUntilReady();
        sender.HandleAck(Ack(2, 1, 5, Com::MultiReceiverSendMethod));
        boost::asio::post(sender.m_strand, [&]{CHECKMSG(sender.SendQueueSize()==0, sender.SendQueueSize());});
        WaitUntilReady();

        TRACELINE
        sent.clear(); //clear sent just for convenience

        //Send fragmented message
        {
            std::string large="ABCDEFGHIJKLMNOPQRSTUVXYZ"; //9 fragments, seq: 6,7,8,9,10,11,12,13,14
            sender.AddToSendQueue(0, MakeShared(large), large.size(), 1);
        }

        TRACELINE
        boost::asio::post(sender.m_strand, [&]
        {
            std::wcout<<"--- SendQueue with fragmented message ---"<<std::endl;
            std::wcout<<sender.SendQueueToString().c_str()<<std::endl;
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
            boost::asio::post(sender.m_strand, [&]{std::wcout<<"R2 ack all but seq 10"<<std::endl; std::wcout<<sender.SendQueueToString().c_str()<<std::endl; });
            boost::asio::post(sender.m_strand, [&]{CHECKMSG(sender.SendQueueSize()==9, sender.SendQueueSize());});
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
            boost::asio::post(sender.m_strand, [&]{std::wcout<<"R3 ack all but seq 12"<<std::endl; std::wcout<<sender.SendQueueToString().c_str()<<std::endl;});
            boost::asio::post(sender.m_strand, [&]{CHECKMSG(sender.SendQueueSize()==5, sender.SendQueueSize());});
        }
        // now send queue looks like this:
        // --------------------------------------------------------
        // | seq 10 (R2) | seq 11 | seq 12 (R3) | seq 13 | seq 14 |
        // --------------------------------------------------------

        TRACELINE
        //R2 ack seq 10
        sender.HandleAck(Ack(2, 1, 10, Com::MultiReceiverSendMethod));
        boost::asio::post(sender.m_strand, [&]{std::wcout<<"R2 ack seq 10"<<std::endl; std::wcout<<sender.SendQueueToString().c_str()<<std::endl;});
        boost::asio::post(sender.m_strand, [&]{CHECKMSG(sender.SendQueueSize()==3, sender.SendQueueSize());});
        // now send queue looks like this:
        // ---------------------------------
        // | seq 12 (R3) | seq 13 | seq 14 |
        // ---------------------------------

        std::wcout<<"*****Wait for resending 12 to R3"<<std::endl;
        Wait(5000);

        std::wcout<<"*****Continue"<<std::endl;

        sender.HandleAck(Ack(3, 1, 12, Com::MultiReceiverSendMethod));
        boost::asio::post(sender.m_strand, [&]{std::wcout<<"R3 ack seq 12"<<std::endl; std::wcout<<sender.SendQueueToString().c_str()<<std::endl;});

        //now send queue is empty, when 12 is acked also 13 and 14 are removed since they are already acked
        boost::asio::post(sender.m_strand, [&]{CHECKMSG(sender.SendQueueSize()==0, sender.SendQueueSize());});

        TRACELINE

        WaitUntilReady();

        for(;;)
        {
            if (!sender.AddToSendQueue(0, MakeShared("1"), 1, 1)) //toId, data, size, dataType
                break;
        }

        uint64_t  lastAcked=14;
        auto lastPostedSeq=static_cast<uint64_t>(lastAcked+Com::Parameters::SendQueueSize);

        std::wcout<<"last posted to send queue: "<<lastPostedSeq<<std::endl;

        while(lastAcked<lastPostedSeq)
        {
            Wait(100);
            lastAcked=LastSentSeq();
            sender.HandleAck(Ack(2, 1, lastAcked, Com::MultiReceiverSendMethod)); //ack half sendQueue from R2
            sender.HandleAck(Ack(3, 1, lastAcked, Com::MultiReceiverSendMethod)); //ack half sendQueue from R3

            std::wcout<<"last sent: "<<lastAcked<<", lastPosted: "<<lastPostedSeq<<std::endl;
            bool dfaf=lastAcked<lastPostedSeq;
            std::wcout<<"lastAcked<lastPostedSeq = "<<std::boolalpha<<dfaf<<std::endl;
        }

        std::wcout<<"Done - lastPosted: "<<lastPostedSeq<<", lastAcked: "<<lastAcked<<std::endl;
        WaitUntilReady();

        CHECK(gotQueueNotFull1);
        CHECK(gotQueueNotFull2);

        TRACELINE
        boost::asio::post(sender.m_strand, [&]
        {
            sender.Stop();
            work.reset();
        });

        TRACELINE

        threads.join_all();
        std::wcout<<"AckedDataSenderTest tests passed"<<std::endl;
    }

private:
    static const size_t SlidingWindowSize = 10;
    static const size_t RequestAckThreshold = 5;

    static boost::mutex mutex;
    static std::vector< std::shared_ptr<Com::UserData> > sent;

    static Com::Ack Ack(int64_t sender, int64_t receiver, uint64_t seqNo, uint8_t sendMethod)
    {
        Com::Ack a(sender, receiver, seqNo, sendMethod);
        for (size_t i=0; i<Com::Parameters::MaxSlidingWindowSize; ++i)
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

    static uint64_t LastSentSeq()
    {
        boost::mutex::scoped_lock lock(mutex);
        return sent.back()->header.sequenceNumber;
    }

    struct TestSendPolicy
    {
        bool Send(const std::shared_ptr<Com::UserData>& val,
                  boost::asio::ip::udp::socket& /*socket*/,
                  const boost::asio::ip::udp::endpoint& /*to*/)
        {
            boost::mutex::scoped_lock lock(mutex);

            if (Com::IsCommunicationDataType(val->header.commonHeader.dataType))
            {
                return true;
            }

            auto expectedAckNow = (val->header.sendMethod==Com::SingleReceiverSendMethod) ||
                    val->transmitCount>1 ||
                    (val->header.sequenceNumber % RequestAckThreshold==0) ? 1 : 0;
            if (val->header.ackNow!=expectedAckNow)
            {
                std::wcout<<L"Unexpected ackNow - "<<val->header.ToString().c_str()<<std::endl;
            }
            CHECK(val->header.ackNow==expectedAckNow);

            std::string s(val->fragment, val->header.fragmentContentSize);
            //std::wcout<<"Writer.Send to_port: "<<to.port()<<", seq: "<<val->header.sequenceNumber<<", data: '"<<s<<"'"<<std::endl;
            sent.push_back(val);
            return true;
        }
    };

    typedef Com::Writer<Com::UserData, AckedDataSenderTest::TestSendPolicy> TestWriter;
    typedef Com::DataSenderBasic<TestWriter> Sender;
};

boost::mutex AckedDataSenderTest::mutex;
std::vector< std::shared_ptr<Com::UserData> > AckedDataSenderTest::sent;

//------------------------------------
// Unacked messages
//------------------------------------
class UnackedDataSenderTest
{
public:
    static void Run()
    {
        std::wcout<<"UnackedDataSenderTest started"<<std::endl;

        boost::asio::io_context io;
        auto work=boost::asio::make_work_guard(io);
        boost::thread_group threads;
        for (int i = 0; i < 9; ++i)
        {
            threads.create_thread([&]{io.run();});
        }

        TRACELINE

        //-------------------
        // Tests
        //-------------------
        std::vector<int> retryTimeout;
        retryTimeout.push_back(500);
        Sender sender(io, Com::Unacked, 1, 1, 4, "127.0.0.1:10000", "224.90.90.241:10000", 20, 10, retryTimeout, 10); //ntId, nId, ipV, mc, waitForAck, fragmentSize

        std::atomic<unsigned int> go(0);
        auto WaitUntilReady=[&]
        {
            boost::asio::post(sender.m_strand, [&]{go=1;});

            while(go==0)
                Wait(20);
            go=0;
        };

        sender.Start();
        sender.AddNode(2, "127.0.0.1:2");
        sender.AddNode(3, "127.0.0.1:3");
        sender.IncludeNode(2);
        sender.IncludeNode(3);

        TRACELINE
        boost::asio::post(sender.m_strand, [&]{CHECK(sender.m_nodes.size()==2);});

        TRACELINE
        sender.AddNode(4, "127.0.0.1:4");
        sender.AddNode(5, "127.0.0.1:5");

        boost::asio::post(sender.m_strand, [&]
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

        boost::asio::post(sender.m_strand, [&]{CHECK(sender.m_nodes.size()==2);});

        TRACELINE
        sender.AddToSendQueue(0, MakeShared("1"), 1, 1);
        sender.AddToSendQueue(0, MakeShared("2"), 1, 1);
        sender.AddToSendQueue(0, MakeShared("3"), 1, 1);

        TRACELINE
        boost::asio::post(sender.m_strand, [&]
        {
            boost::mutex::scoped_lock lock(mutex);
            CHECK(sent.size()>0);
            std::wcout<<"SentSize: "<<sent.size()<<std::endl;
            std::string ss(sent.front()->fragment, sent.front()->header.fragmentContentSize);
            CHECK(ss=="1");
        });


        boost::asio::post(sender.m_strand, [&]
        {
            boost::mutex::scoped_lock lock(mutex);
            CHECKMSG(sender.m_sendQueue.size()==0, sender.m_sendQueue.size());
            CHECKMSG(sender.SendQueueSize()==0, sender.SendQueueSize());
        });

        TRACELINE
        boost::asio::post(sender.m_strand, [&]
        {
            sender.Stop();
            work.reset();
        });

        TRACELINE

        threads.join_all();
        std::wcout<<"UnackedDataSenderTest tests passed"<<std::endl;
    }

private:

    static boost::mutex mutex;
    static std::queue< std::shared_ptr<Com::UserData> > sent;

    struct TestSendPolicy
    {
        bool Send(const std::shared_ptr<Com::UserData>& val,
                  boost::asio::ip::udp::socket& /*socket*/,
                  const boost::asio::ip::udp::endpoint& /*to*/)
        {
            boost::mutex::scoped_lock lock(mutex);
            std::string s(val->fragment, val->fragment+val->header.fragmentContentSize);
            std::string ss(val->fragment, val->header.fragmentContentSize);
            sent.push(val);
            return true;
        }
    };

    typedef Com::Writer<Com::UserData, UnackedDataSenderTest::TestSendPolicy> TestWriter;
    typedef Com::DataSenderBasic<TestWriter> Sender;

    static void OnQueueNotFull()
    {
        std::wcout<<"callback OnQueueNotFull"<<std::endl;

    }

    static void OnRetransmit(int64_t /*toId*/)
    {
        std::wcout<<"callback OnRetransmit"<<std::endl;

    }
};

boost::mutex UnackedDataSenderTest::mutex;
std::queue< std::shared_ptr<Com::UserData> > UnackedDataSenderTest::sent;

//------------------------------------
// Acked with small sliding window
//------------------------------------
class SmallWindowSenderTest
{
public:
    static void Run()
    {
        std::wcout<<"SmallWindowSenderTest started"<<std::endl;

        boost::asio::io_context io;
        auto work=boost::asio::make_work_guard(io);
        boost::thread_group threads;
        for (int i = 0; i < 9; ++i)
        {
            threads.create_thread([&]{io.run();});
        }

        TRACELINE

        //-------------------
        // Tests
        //-------------------
        std::vector<int> retryTimeout;
        retryTimeout.push_back(1000);
        retryTimeout.push_back(3000);
        Sender sender(io, Com::Acked, 1, 1, 4, "127.0.0.1:10000", "224.90.90.241:10000", SlidingWindowSize, RequestAckThreshold, retryTimeout, Com::MessageHeaderSize+3); //ntId, nId, ipV, mc, waitForAck, fragmentSize

        std::atomic<unsigned int> go(0);
        auto WaitUntilReady=[&]
        {
            boost::asio::post(sender.m_strand, [&]{go=1;});

            while(go==0)
                Wait(20);
            go=0;
        };

        TRACELINE

        bool gotQueueNotFull1=false, gotQueueNotFull2=false;
        sender.SetNotFullCallback([&](int64_t id){gotQueueNotFull1=true; std::wcout<<"QueueNotFull 1 nodeType "<<id<<std::endl;});
        sender.SetNotFullCallback([&](int64_t id){gotQueueNotFull2=true; std::wcout<<"QueueNotFull 2 nodeType "<<id<<std::endl;});
        sender.SetRetransmitCallback([=](int64_t id, size_t tc){std::wcout<<"Retransmit to "<<id<<", transmitCount: "<<tc<<std::endl;});
        sender.Start();

        TRACELINE

        // Test retranmit time calculations
        CHECK(sender.GetRetryTimeout(0)==boost::chrono::milliseconds(1000));
        CHECK(sender.GetRetryTimeout(1)==boost::chrono::milliseconds(1000));
        CHECK(sender.GetRetryTimeout(2)==boost::chrono::milliseconds(3000));
        CHECK(sender.GetRetryTimeout(3)==boost::chrono::milliseconds(3000));

        // Test request ack calculations
        {
            Com::MessageHeader header(1, 0, 1, Com::MultiReceiverSendMethod, Com::Acked, 1/*seq*/, 10, 10, 1, 0, 0);

            header.ackNow=3; //invalid value just to see that the correct is alway set
            sender.SetRequestAck(header);
            CHECK(header.ackNow==1);

            header.ackNow=3; //invalid value just to see that the correct is alway set
            header.sequenceNumber=2;
            sender.SetRequestAck(header);
            CHECK(header.ackNow==1);

            header.ackNow=3; //invalid value just to see that the correct is alway set
            header.sequenceNumber=3;
            sender.SetRequestAck(header);
            CHECK(header.ackNow==1);

            header.ackNow=3; //invalid value just to see that the correct is alway set
            header.sequenceNumber=1;
            header.sendMethod=Com::SingleReceiverSendMethod;
            sender.SetRequestAck(header);
            CHECK(header.ackNow==1);
        }

        sender.AddNode(2, "127.0.0.1:2");
        sender.AddNode(3, "127.0.0.1:3");
        sender.IncludeNode(2); //generates welcome with seq 1
        sender.IncludeNode(3); //generates welcome with seq 2

        TRACELINE

        //Check that welcome messages have been posted
        boost::asio::post(sender.m_strand, [&]
        {
            CHECK(sender.m_nodes.size()==2);
            CHECK(sender.m_nodes.find(2)->second.systemNode==true);
            CHECK(sender.m_nodes.find(2)->second.welcome==1);
            CHECK(sender.m_nodes.find(3)->second.systemNode==true);
            CHECK(sender.m_nodes.find(3)->second.welcome==2);
            CHECKMSG(sender.SendQueueSize()==2, sender.SendQueueSize());
            CHECK(sender.m_sendQueue[0]->transmitCount>0); //only one welcome has been sent since sendWindow is 1
            CHECK(sender.m_sendQueue[1]->transmitCount==0); //still not sent
        });

        WaitUntilReady();

        TRACELINE
        //Ack welcome first messages
        sender.HandleAck(Ack(2, 1, 1, Com::MultiReceiverSendMethod));  //Ack(sender, receiver, seqNo, sendMethod)
        TRACELINE
        WaitUntilReady();

        CHECK(sender.m_sendQueue[0]->header.sequenceNumber==2); //check that first welcome has been removed and the second is now first in queue
        CHECK(sender.m_sendQueue[0]->transmitCount>0); //and the second welcome has now been sent

        //Ack welcome second messages
        sender.HandleAck(Ack(2, 1, 2, Com::MultiReceiverSendMethod));  //Ack(sender, receiver, seqNo, sendMethod)
        sender.HandleAck(Ack(3, 1, 2, Com::MultiReceiverSendMethod));  //Ack(sender, receiver, seqNo, sendMethod)

        boost::asio::post(sender.m_strand, [&]
        {
            //all messages should now have been removed
            CHECKMSG(sender.SendQueueSize()==0, sender.SendQueueSize());
        });

        WaitUntilReady();

        TRACELINE
        boost::asio::post(sender.m_strand, [&]{CHECK(sender.m_nodes.size()==2);});

        TRACELINE
        sender.AddNode(4, "127.0.0.1:4");
        sender.AddNode(5, "127.0.0.1:5");

        boost::asio::post(sender.m_strand, [&]
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

        boost::asio::post(sender.m_strand, [&]{CHECK(sender.m_nodes.size()==2);});

        WaitUntilReady();
        CHECK(sender.m_sendQueue.empty());

        TRACELINE
        //add messages, seq: 3,4,5
        sender.AddToSendQueue(0, MakeShared("1"), 1, 1); //toId, data, size, dataType
        sender.AddToSendQueue(0, MakeShared("2"), 1, 1);
        sender.AddToSendQueue(0, MakeShared("3"), 1, 1);


        TRACELINE
        boost::asio::post(sender.m_strand, [&]
        {
            boost::mutex::scoped_lock lock(mutex);
            CHECK(sent.size()==1);
            CHECK(GetSentData(0)=="1"); //seq 3
            CHECKMSG(sender.SendQueueSize()==3, sender.SendQueueSize());
        });

        TRACELINE
        WaitUntilReady();
        sender.HandleAck(Ack(2, 1, 3, Com::MultiReceiverSendMethod));  //Ack(sender, receiver, seqNo, sendMethod)
        boost::asio::post(sender.m_strand, [&]{CHECKMSG(sender.SendQueueSize()==3, sender.SendQueueSize());});
        WaitUntilReady();
        WaitUntilReady();
        sender.HandleAck(Ack(3, 1, 3, Com::MultiReceiverSendMethod)); //Ack(sender, receiver, seqNo, sendMethod)
        boost::asio::post(sender.m_strand, [&]{std::wcout<<sender.SendQueueToString().c_str()<<std::endl;});
        boost::asio::post(sender.m_strand, [&]{CHECKMSG(sender.SendQueueSize()==2, sender.SendQueueSize());});
        WaitUntilReady();
        boost::asio::post(sender.m_strand, [&]{std::wcout<<sender.SendQueueToString().c_str()<<std::endl;});
        WaitUntilReady();
        sender.HandleAck(Ack(2, 1, 4, Com::MultiReceiverSendMethod));
        sender.HandleAck(Ack(3, 1, 4, Com::MultiReceiverSendMethod));
        boost::asio::post(sender.m_strand, [&]{CHECKMSG(sender.SendQueueSize()==1, sender.SendQueueSize());});
        WaitUntilReady();
        sender.HandleAck(Ack(2, 1, 5, Com::MultiReceiverSendMethod));
        sender.HandleAck(Ack(3, 1, 5, Com::MultiReceiverSendMethod));
        boost::asio::post(sender.m_strand, [&]{CHECKMSG(sender.SendQueueSize()==0, sender.SendQueueSize());});
        WaitUntilReady();

        TRACELINE
        sent.clear(); //clear sent just for convenience

        //Send fragmented message
        {
            std::string large="ABCDEFGHIJKLMNOPQRSTUVXYZ"; //9 fragments, seq: 6,7,8,9,10,11,12,13,14
            sender.AddToSendQueue(0, MakeShared(large), large.size(), 1);
        }

        TRACELINE
        boost::asio::post(sender.m_strand, [&]
        {
            std::wcout<<"--- SendQueue with fragmented message ---"<<std::endl;
            std::wcout<<sender.SendQueueToString().c_str()<<std::endl;
            CHECKMSG(sender.SendQueueSize()==9, sender.SendQueueSize());
            CHECK(sender.m_sendQueue[0]->transmitCount==1);
        });

        // now send queue looks like this waiting for both recv R2 and R3 to ack all messages:
        // ------------------------------------------------------------------------------------------------------------------------------------------------------
        // | seq 6 (R2,R3) | seq 7 (R2,R3) | seq 8 (R2,R3) | seq 9 (R2,R3) | seq 10 (R2,R3) | seq 11 (R2,R3) | seq 12 (R2,R3) | seq 13 (R2,R3) | seq 14 (R2,R3) |
        // ------------------------------------------------------------------------------------------------------------------------------------------------------
        WaitUntilReady();


        std::wcout<<"*****Wait for resending 6 to R2,R3"<<std::endl;
        Wait(1600);
        boost::asio::post(sender.m_strand, [&]
        {
            CHECK(sender.m_sendQueue[0]->transmitCount==2);
        });
        WaitUntilReady();
        Wait(1000);
        boost::asio::post(sender.m_strand, [&]
        {
            CHECK(sender.m_sendQueue[0]->transmitCount==2);
        });
        WaitUntilReady();
        Wait(2000);
        boost::asio::post(sender.m_strand, [&]
        {
            CHECK(sender.m_sendQueue[0]->transmitCount==3);
            for (size_t i=0; i<9; ++i)
            {
                CHECK(sender.m_sendQueue[i]->header.ackNow==1);
            }
        });
        WaitUntilReady();

        //finsih send queue
        for (uint64_t seq=6; seq<15; ++seq)
        {
            sender.HandleAck(Ack(2, 1, seq, Com::MultiReceiverSendMethod)); //Ack(sender, receiver, seqNo, sendMethod)
            sender.HandleAck(Ack(3, 1, seq, Com::MultiReceiverSendMethod));
            WaitUntilReady();
        }

        boost::asio::post(sender.m_strand, [&]{CHECKMSG(sender.SendQueueSize()==0, sender.SendQueueSize());});

        TRACELINE
        boost::asio::post(sender.m_strand, [&]
        {
            sender.Stop();
            work.reset();
        });

        TRACELINE

        threads.join_all();
        std::wcout<<"SmallWindowSenderTest tests passed"<<std::endl;
    }

private:

    static const size_t SlidingWindowSize = 1;
    static const size_t RequestAckThreshold = 1;

    static boost::mutex mutex;
    static std::vector< std::shared_ptr<Com::UserData> > sent;

    static Com::Ack Ack(int64_t sender, int64_t receiver, uint64_t seqNo, uint8_t sendMethod)
    {
        Com::Ack a(sender, receiver, seqNo, sendMethod);
        for (size_t i=0; i<SlidingWindowSize; ++i)
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

    static uint64_t LastSentSeq()
    {
        boost::mutex::scoped_lock lock(mutex);
        return sent.back()->header.sequenceNumber;
    }

    struct TestSendPolicy
    {
        bool Send(const std::shared_ptr<Com::UserData>& val,
                  boost::asio::ip::udp::socket& /*socket*/,
                  const boost::asio::ip::udp::endpoint& /*to*/)
        {
            boost::mutex::scoped_lock lock(mutex);

            if (Com::IsCommunicationDataType(val->header.commonHeader.dataType))
            {
                return true;
            }

            auto expectedAckNow = (val->header.sendMethod==Com::SingleReceiverSendMethod) ||
                    val->transmitCount>1 ||
                    (val->header.sequenceNumber % RequestAckThreshold==0) ? 1 : 0;
            if (val->header.ackNow!=expectedAckNow)
            {
                std::wcout<<L"Unexpected ackNow - "<<val->header.ToString().c_str()<<std::endl;
            }
            CHECK(val->header.ackNow==expectedAckNow);

            std::string s(val->fragment, val->header.fragmentContentSize);
            //std::wcout<<"Writer.Send to_port: "<<to.port()<<", seq: "<<val->header.sequenceNumber<<", data: '"<<s<<"'"<<std::endl;
            sent.push_back(val);
            return true;
        }
    };

    typedef Com::Writer<Com::UserData, SmallWindowSenderTest::TestSendPolicy> TestWriter;
    typedef Com::DataSenderBasic<TestWriter> Sender;
};

boost::mutex SmallWindowSenderTest::mutex;
std::vector< std::shared_ptr<Com::UserData> > SmallWindowSenderTest::sent;

//------------------------------------
// Test retransmission of messages
//------------------------------------
class RetransmissionTest
{
public:
    static void Run()
    {
        std::wcout<<"RetransmissionTest started"<<std::endl;

        boost::asio::io_context io;
        auto work=boost::asio::make_work_guard(io);
        boost::thread_group threads;
        for (int i = 0; i < 9; ++i)
        {
            threads.create_thread([&]{io.run();});
        }

        TRACELINE

        //-------------------
        // Tests
        //-------------------
        std::vector<int> retryTimeout;
        retryTimeout.push_back(500);
        Sender sender(io, Com::Acked, 1, 1, 4, "127.0.0.1:10000", "224.90.90.241:10000", SlidingWindowSize, RequestAckThreshold, retryTimeout, Com::MessageHeaderSize+3); //ntId, nId, ipV, mc, waitForAck, fragmentSize

        std::atomic<unsigned int> go(0);
        auto WaitUntilReady=[&]
        {
            boost::asio::post(sender.m_strand, [&]{go=1;});

            while(go==0)
                Wait(20);
            go=0;
        };

        TRACELINE

        sender.SetRetransmitCallback([=](int64_t id, size_t tc)
        {
            boost::mutex::scoped_lock lock(mutex);
            retransmit.push_back(std::pair<int64_t, size_t>(id, tc));
        });

        sender.Start();

        TRACELINE

        sender.AddNode(2, "127.0.0.1:2");
        sender.AddNode(3, "127.0.0.1:3");
        sender.IncludeNode(2); //generates welcome with seq 1
        sender.IncludeNode(3); //generates welcome with seq 2

        TRACELINE

        //Check that welcome messages have been posted
        boost::asio::post(sender.m_strand, [&]
        {
            CHECK(sender.m_nodes.size()==2);
            CHECK(sender.m_nodes.find(2)->second.systemNode==true);
            CHECK(sender.m_nodes.find(2)->second.welcome==1);
            CHECK(sender.m_nodes.find(3)->second.systemNode==true);
            CHECK(sender.m_nodes.find(3)->second.welcome==2);
            CHECKMSG(sender.SendQueueSize()==2, sender.SendQueueSize());
            CHECK(sender.m_sendQueue[0]->transmitCount>0); //only one welcome has been sent since sendWindow is 1
            CHECK(sender.m_sendQueue[1]->transmitCount>0); //still not sent
        });

        WaitUntilReady();

        TRACELINE
        //Ack welcome messages
        sender.HandleAck(Ack(2, 1, 1, Com::MultiReceiverSendMethod));  //Ack(sender, receiver, seqNo, sendMethod)
        sender.HandleAck(Ack(2, 1, 2, Com::MultiReceiverSendMethod));  //Ack(sender, receiver, seqNo, sendMethod)
        sender.HandleAck(Ack(3, 1, 2, Com::MultiReceiverSendMethod));  //Ack(sender, receiver, seqNo, sendMethod)
        TRACELINE
        WaitUntilReady();

        boost::asio::post(sender.m_strand, [&]
        {
            //all messages should now have been removed
            CHECK(sender.m_sendQueue.empty());
            CHECK(sender.m_nodes.size()==2);

            boost::mutex::scoped_lock lock(mutex);
            CHECK(retransmit.empty());
            sent.clear();
        });

        WaitUntilReady();

        TRACELINE

        TRACELINE

        // Test retransmission when sending multireceiver
        sender.AddToSendQueue(0, MakeShared("1"), 1, 1); //toId, data, size, dataType
        Wait(800);
        boost::asio::post(sender.m_strand, [&]
        {
            boost::mutex::scoped_lock lock(mutex);
            CHECK(sender.m_sendQueue.size()==1);
            CHECK(sender.m_sendQueue[0]->receivers.size()==2);
            CHECK(sender.m_sendQueue[0]->receivers.find(2)!=sender.m_sendQueue[0]->receivers.end());
            CHECK(sender.m_sendQueue[0]->receivers.find(3)!=sender.m_sendQueue[0]->receivers.end());

            CHECKMSG(sender.m_sendQueue[0]->transmitCount==2, sender.m_sendQueue[0]->transmitCount); //has been resent once
            CHECK(retransmit.size()==2);

            //retransmit to node 2 with transmitCount 2
            CHECK(retransmit[0].first==2); //toNode
            CHECK(retransmit[0].second==2); //transmitCount

            //retransmit to node 2 with transmitCount 3
            CHECK(retransmit[1].first==3); //toNode
            CHECK(retransmit[1].second==2); //transmitCount
        });

        TRACELINE
        WaitUntilReady();

        //node 2 will ack the only message in sendQueue (seq=3)
        sender.HandleAck(Ack(2, 1, 3, Com::MultiReceiverSendMethod));  //Ack(sender, receiver, seqNo, sendMethod)
        boost::asio::post(sender.m_strand, [&]
        {
            CHECKMSG(sender.SendQueueSize()==1, sender.SendQueueSize());
            CHECK(sender.m_sendQueue[0]->receivers.size()==1);
            CHECK(sender.m_sendQueue[0]->receivers.find(3)!=sender.m_sendQueue[0]->receivers.end());
            CHECK(sender.m_sendQueue[0]->transmitCount==2); //has still been sent 2 times
        });

        WaitUntilReady();

        TRACELINE

        //wait for 2 more resends to node 3
        Wait(1000);
        boost::asio::post(sender.m_strand, [&]
        {
            boost::mutex::scoped_lock lock(mutex);
            CHECK(sender.m_sendQueue.size()==1);
            CHECK(sender.m_sendQueue[0]->receivers.size()==1);
            CHECK(sender.m_sendQueue[0]->receivers.find(3)!=sender.m_sendQueue[0]->receivers.end());

            //now msg should have been sent 4 times, first two to both node 2 and 3, and last two only to node 3
            CHECKMSG(sender.m_sendQueue[0]->transmitCount==4, sender.m_sendQueue[0]->transmitCount); //has been resent once
            CHECK(retransmit.size()==4);

            //retransmit to node 3 with transmitCount 3
            CHECK(retransmit[2].first==3); //toNode
            CHECK(retransmit[2].second==3); //transmitCount

            //retransmit to node 3 with transmitCount 4
            CHECK(retransmit[3].first==3); //toNode
            CHECK(retransmit[3].second==4); //transmitCount

            retransmit.clear(); //for convenience
        });

        //node 3 will ack the only message in sendQueue (seq=3)
        sender.HandleAck(Ack(3, 1, 3, Com::MultiReceiverSendMethod)); //Ack(sender, receiver, seqNo, sendMethod)
        boost::asio::post(sender.m_strand, [&]{CHECKMSG(sender.SendQueueSize()==0, sender.SendQueueSize());});
        WaitUntilReady();

        TRACELINE

        //no more retransmission expected
        Wait(800);
        boost::asio::post(sender.m_strand, [&]
        {
            boost::mutex::scoped_lock lock(mutex);
            CHECK(sender.m_sendQueue.empty());
            CHECK(retransmit.empty());
            sent.clear(); //also clear sent, from now on we only expect Ping messages to be sent
        });

        //be quiet long enough, then DataSender shall send a Ping
        while (sender.m_sendQueue.empty())
        {
            Wait(3000);
        }
        Wait(2000);

        TRACELINE
        boost::asio::post(sender.m_strand, [&]
        {
            boost::mutex::scoped_lock lock(mutex);
            CHECK(sender.m_sendQueue.size()==1);
            CHECK(sender.SendQueueSize()==1);
            CHECK(sender.m_sendQueue.front()->header.commonHeader.dataType==Safir::Dob::Internal::Com::PingDataType);
            CHECK(sender.m_sendQueue.front()->header.ackNow==1);
            CHECK(sent.size()>1);
            CHECK(sent.size()>0);
            CHECK(sender.m_sendQueue.front()->transmitCount>0);
            CHECK(retransmit.size()>0);
            CHECK(retransmit.back().second==sender.m_sendQueue.front()->transmitCount);
        });

        WaitUntilReady();

        sender.HandleAck(Ack(2, 1, 4, Com::MultiReceiverSendMethod));  //Ack(sender, receiver, seqNo, sendMethod)
        sender.HandleAck(Ack(3, 1, 4, Com::MultiReceiverSendMethod));  //Ack(sender, receiver, seqNo, sendMethod)

        boost::asio::post(sender.m_strand, [&]{CHECKMSG(sender.SendQueueSize()==0, sender.SendQueueSize());});
        WaitUntilReady();

        //finished
        TRACELINE
        boost::asio::post(sender.m_strand, [&]
        {
            sender.Stop();
            work.reset();
        });

        TRACELINE

        threads.join_all();
        std::wcout<<"RetransmissionTest tests passed"<<std::endl;
    }

private:

    static const size_t SlidingWindowSize = 10;
    static const size_t RequestAckThreshold = 1;

    static boost::mutex mutex;
    static std::vector< std::shared_ptr<Com::UserData> > sent;
    static std::vector< std::pair<int64_t, size_t> > retransmit; //pair<toId, transmitCount>

    static Com::Ack Ack(int64_t sender, int64_t receiver, uint64_t seqNo, uint8_t sendMethod)
    {
        Com::Ack a(sender, receiver, seqNo, sendMethod);
        for (size_t i=0; i<SlidingWindowSize; ++i)
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

    static uint64_t LastSentSeq()
    {
        boost::mutex::scoped_lock lock(mutex);
        return sent.back()->header.sequenceNumber;
    }

    struct TestSendPolicy
    {
        bool Send(const std::shared_ptr<Com::UserData>& val,
                  boost::asio::ip::udp::socket& /*socket*/,
                  const boost::asio::ip::udp::endpoint& /*to*/)
        {
            boost::mutex::scoped_lock lock(mutex);

            if (Com::WelcomeDataType==val->header.commonHeader.dataType)
                return true;

            auto expectedAckNow = (val->header.sendMethod==Com::SingleReceiverSendMethod) ||
                    val->transmitCount>1 ||
                    (val->header.sequenceNumber % RequestAckThreshold==0) ? 1 : 0;
            if (val->header.ackNow!=expectedAckNow)
            {
                std::wcout<<L"Unexpected ackNow - "<<val->header.ToString().c_str()<<std::endl;
            }
            CHECK(val->header.ackNow==expectedAckNow);

            std::string s(val->fragment, val->header.fragmentContentSize);
            //std::wcout<<"Writer.Send to_port: "<<to.port()<<", seq: "<<val->header.sequenceNumber<<", data: '"<<s<<"'"<<std::endl;
            sent.push_back(val);
            return true;
        }
    };

    typedef Com::Writer<Com::UserData, RetransmissionTest::TestSendPolicy> TestWriter;
    typedef Com::DataSenderBasic<TestWriter> Sender;
};

boost::mutex RetransmissionTest::mutex;
std::vector< std::shared_ptr<Com::UserData> > RetransmissionTest::sent;
std::vector< std::pair<int64_t, size_t> > RetransmissionTest::retransmit;

//------------------------------------
// Test simulation of network up/down
//------------------------------------
class DataSenderSimulateNetworkUpDownTest
{
public:
    static void Run()
    {
        std::wcout<<"DataSenderSimulateNetworkUpDownTest started"<<std::endl;

        boost::asio::io_context io;
        auto work=boost::asio::make_work_guard(io);
        boost::thread_group threads;
        for (int i = 0; i < 1; ++i)
        {
            threads.create_thread([&]{io.run();});
        }

        TestWriter writer(io, 4);
        boost::asio::ip::udp::endpoint ep(boost::asio::ip::make_address("127.0.0.1"), 10000);

        CHECK(sent.size() == 0);
        auto m0 = std::make_shared<std::string>("0");
        auto m1 = std::make_shared<std::string>("1");
        writer.SendTo(m0, ep);
        writer.SendTo(m1, ep);
        CHECK(sent.size() == 2);
        CHECK(sent.at(0) == "0");
        CHECK(sent.at(1) == "1");

        Safir::Dob::Internal::Com::Parameters::NetworkEnabled = false;

        auto m2 = std::make_shared<std::string>("2");
        auto m3 = std::make_shared<std::string>("3");
        writer.SendTo(m2, ep);
        writer.SendTo(m3, ep);
        CHECK(sent.size() == 2);
        CHECK(sent.at(0) == "0");
        CHECK(sent.at(1) == "1");

        Safir::Dob::Internal::Com::Parameters::NetworkEnabled = true;

        writer.SendTo(m2, ep);
        writer.SendTo(m3, ep);
        CHECK(sent.size() == 4);
        CHECK(sent.at(0) == "0");
        CHECK(sent.at(1) == "1");
        CHECK(sent.at(2) == "2");
        CHECK(sent.at(3) == "3");

        work.reset();
        threads.join_all();
        std::wcout<<"DataSenderSimulateNetworkUpDownTest tests passed"<<std::endl;
    }

private:

    static std::vector<std::string> sent;

    struct TestSendPolicy
    {
        bool Send(const std::shared_ptr<std::string>& val,
                  boost::asio::ip::udp::socket& /*socket*/,
                  const boost::asio::ip::udp::endpoint& /*to*/)
        {
            sent.push_back(*val);
            return true;
        }
    };

    typedef Com::Writer<std::string, DataSenderSimulateNetworkUpDownTest::TestSendPolicy> TestWriter;
};

std::vector<std::string> DataSenderSimulateNetworkUpDownTest::sent;

//-----------------------
// Start Sender tests
//-----------------------
struct DataSenderTest
{
    static void Run()
    {
        AckedDataSenderTest::Run();
        SmallWindowSenderTest::Run();
        RetransmissionTest::Run();
        UnackedDataSenderTest::Run();
        DataSenderSimulateNetworkUpDownTest::Run();
    }
};
