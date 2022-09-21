/******************************************************************************
*
* Copyright Saab AB, 2013-2022 (http://safirsdkcore.com)
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

//------------------------------------------------
// Test that discover is sent to seeds
//------------------------------------------------
class DiscoverToSeed
{
public:
    static void Run()
    {
        std::wcout<<"SendDiscoverToSeed started"<<std::endl;
        boost::asio::io_context io;
        auto work=boost::asio::make_work_guard(io);
        boost::thread_group threads;
        for (int i = 0; i < 9; ++i)
        {
            threads.create_thread([&]{io.run();});
        }

        //----------------------
        // Test
        //----------------------
        Discoverer s0(io, CreateNode(100), 1500, std::set<int64_t>{2, 3}, [&](const Com::Node&){});
        Discoverer s1(io, CreateNode(200), 1500, std::set<int64_t>{2, 3}, [&](const Com::Node&){});
        Discoverer n0(io, CreateNode(0), 1500, std::set<int64_t>{2, 3}, [&](const Com::Node&){});
        Discoverer n1(io, CreateNode(1), 1500, std::set<int64_t>{2, 3}, [&](const Com::Node&){});
        Discoverer n2(io, CreateNode(2), 1500, std::set<int64_t>{2, 3}, [&](const Com::Node&){});

        std::vector<std::string> seeds;
        
        seeds.push_back("127.0.0.1:10100");
        seeds.push_back("127.0.0.1:10200");

        n0.AddSeeds(seeds);
        n1.AddSeeds(seeds);
        n2.AddSeeds(seeds);

        s0.Start();
        n0.Start();
        n1.Start();
        n2.Start();
        s1.Start();

        Wait(3100); //after this time all nodes should have sent discovers
        std::wcout<<"--- All should have sent discovers ---"<<std::endl;
        std::wcout<<"n0 NumSeeds: "<<n0.m_seeds.size()<<", sentDiscovers: "<<discoversSentToSeed100[0]<<"/"<<discoversSentToSeed200[0]<<std::endl;
        std::wcout<<"n1 NumSeeds: "<<n1.m_seeds.size()<<", sentDiscovers: "<<discoversSentToSeed100[1]<<"/"<<discoversSentToSeed200[1]<<std::endl;
        std::wcout<<"n2 NumSeeds: "<<n2.m_seeds.size()<<", sentDiscovers: "<<discoversSentToSeed100[2]<<"/"<<discoversSentToSeed200[2]<<std::endl;

        Wait(3100); //after this time all nodes should have sent discovers

        bool passed=false;
        {
            boost::mutex::scoped_lock lock(mutex);
            passed= discoversSentToSeed100[0]>1 && discoversSentToSeed100[1]>1 && discoversSentToSeed100[2]>1 &&
                    discoversSentToSeed200[0]>1 && discoversSentToSeed200[1]>1 && discoversSentToSeed200[2]>1;
        }
        CHECK(passed);

        //send node info from seed nodes, that makes them removed from seed-list
        Com::CommunicationMessage_NodeInfo ni0;
        ni0.set_number_of_packets(1);
        ni0.set_packet_number(0);
        ni0.set_sent_from_id(LlufId_Generate64("127.0.0.1:10100"));
        ni0.mutable_sent_from_node()->set_name("s0");
        ni0.mutable_sent_from_node()->set_node_id(100);
        ni0.mutable_sent_from_node()->set_control_address("127.0.0.1:10100");
        ni0.mutable_sent_from_node()->set_data_address("127.0.0.1:10100");

        Com::CommunicationMessage_NodeInfo ni1;
        ni1.set_number_of_packets(1);
        ni1.set_packet_number(0);
        ni1.set_sent_from_id(LlufId_Generate64("127.0.0.1:10200"));
        ni1.mutable_sent_from_node()->set_name("s1");
        ni1.mutable_sent_from_node()->set_node_id(200);
        ni1.mutable_sent_from_node()->set_control_address("127.0.0.1:10200");
        ni1.mutable_sent_from_node()->set_data_address("127.0.0.1:10200");

        n0.HandleReceivedNodeInfo(ni0);
        n0.HandleReceivedNodeInfo(ni1);
        n1.HandleReceivedNodeInfo(ni0);
        n1.HandleReceivedNodeInfo(ni1);
        n2.HandleReceivedNodeInfo(ni0);
        n2.HandleReceivedNodeInfo(ni1);

        //Wait for HandleReceivedNodeInfo to be executed
        std::atomic<int> fence(0);
        boost::asio::post(n0.m_strand, [&]{++fence;});
        boost::asio::post(n1.m_strand, [&]{++fence;});
        boost::asio::post(n2.m_strand, [&]{++fence;});
        while(fence<3)
        {
            Wait(1000);
        }

        //reset sent discovers
        {
            boost::mutex::scoped_lock lock(mutex);
            discoversSentToSeed100[0]=0;
            discoversSentToSeed200[0]=0;
            discoversSentToSeed100[1]=0;
            discoversSentToSeed200[1]=0;
            discoversSentToSeed100[2]=0;
            discoversSentToSeed200[2]=0;
        }

        Wait(3100);

        //now we should not send any more discovers to node 100 and 200 (seed nodes)
        std::wcout<<"--- After HandleReceivedNodeInfo ---"<<std::endl;
        std::wcout<<"n0 NumSeeds: "<<n0.m_seeds.size()<<", sentDiscovers: "<<discoversSentToSeed100[0]<<"/"<<discoversSentToSeed200[0]<<std::endl;
        std::wcout<<"n1 NumSeeds: "<<n1.m_seeds.size()<<", sentDiscovers: "<<discoversSentToSeed100[1]<<"/"<<discoversSentToSeed200[1]<<std::endl;
        std::wcout<<"n2 NumSeeds: "<<n2.m_seeds.size()<<", sentDiscovers: "<<discoversSentToSeed100[2]<<"/"<<discoversSentToSeed200[2]<<std::endl;
        Wait(500);
        std::wcout<<"--- After HandleReceivedNodeInfo ---"<<std::endl;
        std::wcout<<"n0 NumSeeds: "<<n0.m_seeds.size()<<", sentDiscovers: "<<discoversSentToSeed100[0]<<"/"<<discoversSentToSeed200[0]<<std::endl;
        std::wcout<<"n1 NumSeeds: "<<n1.m_seeds.size()<<", sentDiscovers: "<<discoversSentToSeed100[1]<<"/"<<discoversSentToSeed200[1]<<std::endl;
        std::wcout<<"n2 NumSeeds: "<<n2.m_seeds.size()<<", sentDiscovers: "<<discoversSentToSeed100[2]<<"/"<<discoversSentToSeed200[2]<<std::endl;

        {
            boost::mutex::scoped_lock lock(mutex);
            CHECK(n0.m_seeds.empty());
            CHECK(n1.m_seeds.empty());
            CHECK(n2.m_seeds.empty());

            CHECK(discoversSentToSeed100[0]==0);
            CHECK(discoversSentToSeed200[0]==0);
            CHECK(discoversSentToSeed100[1]==0);
            CHECK(discoversSentToSeed200[1]==0);
            CHECK(discoversSentToSeed100[2]==0);
            CHECK(discoversSentToSeed200[2]==0);
        }


        //exclude seed-nodes, that should move them back to seed-list
        n0.ExcludeNode(100);
        n0.ExcludeNode(200);
        n1.ExcludeNode(100);
        n1.ExcludeNode(200);
        n2.ExcludeNode(100);
        n2.ExcludeNode(200);

        Wait(3100);  //should be enogh to get discovers

        //now since the seeds have been excluded we expect to get discovers again
        std::wcout<<"--- After Exclude ---"<<std::endl;
        std::wcout<<"n0 NumSeeds: "<<n0.m_seeds.size()<<", sentDiscovers: "<<discoversSentToSeed100[0]<<"/"<<discoversSentToSeed200[0]<<std::endl;
        std::wcout<<"n1 NumSeeds: "<<n1.m_seeds.size()<<", sentDiscovers: "<<discoversSentToSeed100[1]<<"/"<<discoversSentToSeed200[1]<<std::endl;
        std::wcout<<"n2 NumSeeds: "<<n2.m_seeds.size()<<", sentDiscovers: "<<discoversSentToSeed100[2]<<"/"<<discoversSentToSeed200[2]<<std::endl;


        {
            boost::mutex::scoped_lock lock(mutex);
            CHECK(n0.m_seeds.size()==2);
            CHECK(n1.m_seeds.size()==2);
            CHECK(n2.m_seeds.size()==2);

            CHECK(discoversSentToSeed100[0]>0);
            CHECK(discoversSentToSeed200[0]>0);
            CHECK(discoversSentToSeed100[1]>0);
            CHECK(discoversSentToSeed200[1]>0);
            CHECK(discoversSentToSeed100[2]>0);
            CHECK(discoversSentToSeed200[2]>0);
        }

        s0.Stop();
        n0.Stop();
        n1.Stop();
        n2.Stop();
        s1.Stop();

        //-----------
        // shutdown
        //-----------
        work.reset();
        threads.join_all();
        std::wcout<<"SendDiscoverToSeed tests passed"<<std::endl;
    }

private:

    static boost::mutex mutex;
    static std::map<int64_t, int> discoversSentToSeed100;
    static std::map<int64_t, int> discoversSentToSeed200;

    static Com::Node CreateNode(int64_t i)
    {
        return Com::Node(std::string("discoverer_")+boost::lexical_cast<std::string>(i), i, 1, std::string("127.0.0.1:")+boost::lexical_cast<std::string>(10000+i), "", true);
    }

    struct TestSendPolicy
    {
        void Send(const Com::UserDataPtr val,
                  boost::asio::ip::udp::socket& /*socket*/,
                  const boost::asio::ip::udp::endpoint& to)
        {
            boost::mutex::scoped_lock lock(mutex);
            Com::CommunicationMessage cm;
            cm.ParseFromArray(val->message.get(), static_cast<int>(val->header.totalContentSize));
            if (cm.has_discover())
            {
                //std::wcout<<"From "<<val->header.commonHeader.senderId<<" to "<<to.port()<<std::endl;
                if (to.port()==10100)
                    discoversSentToSeed100[val->header.commonHeader.senderId]++;
                else if (to.port()==10200)
                    discoversSentToSeed200[val->header.commonHeader.senderId]++;
            }
        }
    };

    typedef Com::Writer<Com::UserData, TestSendPolicy> TestWriter;
    typedef Com::DiscovererBasic<TestWriter> Discoverer;
};

boost::mutex DiscoverToSeed::mutex;
std::map<int64_t, int> DiscoverToSeed::discoversSentToSeed100;
std::map<int64_t, int> DiscoverToSeed::discoversSentToSeed200;

//--------------------------------------------------
// Test receive discover
//--------------------------------------------------
class HandleDiscover
{
public:
    static void Run()
    {
        std::wcout<<"HandleDiscover started"<<std::endl;

        boost::asio::io_context io;
        auto work=boost::asio::make_work_guard(io);
        boost::thread_group threads;
        for (int i = 0; i < 9; ++i)
        {
            threads.create_thread([&]{io.run();});
        }

        //----------------------
        // Test
        //----------------------
        {
            boost::mutex::scoped_lock lock(mutex);
            discoverState.insert(std::make_pair(10000, Info(10000, io)));
            discoverState.insert(std::make_pair(10001, Info(10001, io)));
            discoverState.insert(std::make_pair(10002, Info(10002, io)));
        }

        std::vector<std::string> seeds;
        seeds.push_back("127.0.0.1:10000");
        
        {
            boost::mutex::scoped_lock lock(mutex);
            discoverState[10001].discover->AddSeeds(seeds);
            discoverState[10002].discover->AddSeeds(seeds);
            discoverState[10000].discover->Start();
            discoverState[10001].discover->Start();
        }

        //Wait unti node 1000 and 1001 have found each other
        for (int i=0; i<50; ++i)
        {
            Deliver();
            {
                boost::mutex::scoped_lock lock(mutex);
                if (discoverState[10000].newNodes.size()==1 &&
                    discoverState[10001].newNodes.size()==1)
                    break;
            }
            Wait(500);
        }

        //now start a third node
        {
            boost::mutex::scoped_lock lock(mutex);
            discoverState[10002].discover->Start();
        }

        //Wait until all nodes have found each other
        for (int i=0; i<50; ++i)
        {
            Deliver();
            {
                boost::mutex::scoped_lock lock(mutex);
                if (discoverState[10000].newNodes.size()==2 &&
                    discoverState[10001].newNodes.size()==2 &&
                    discoverState[10002].newNodes.size()==2)
                    break;
            }
            Wait(500);
        }

        //Stop all discoverers
        {
            boost::mutex::scoped_lock lock(mutex);
            discoverState[10000].discover->Stop();
            discoverState[10001].discover->Stop();
            discoverState[10002].discover->Stop();
        }

        //-----------
        // shutdown
        //-----------
        work.reset();
        threads.join_all();

        {
            boost::mutex::scoped_lock lock(mutex);
            discoverState.clear(); //this step is important because discoverState has references to the io_service in this scope.
        }

        std::wcout<<"HandleDiscover tests passed"<<std::endl;
    }

private:

    static boost::mutex mutex;

    static void DumpInfo()
    {
        boost::mutex::scoped_lock lock(mutex);
        std::wcout<<"------ Info ------"<<std::endl;
        
        for (auto vt = discoverState.cbegin(); vt != discoverState.cend(); ++vt)
        {
            std::wcout<<"Id: "<<vt->first<<std::endl;
            std::wcout<<"Nodes: ";
            
            for (auto id = vt->second.newNodes.cbegin(); id != vt->second.newNodes.cend(); ++id)
            {
                std::wcout<<*id<<", ";
            }
            std::wcout<<std::endl;

            std::wcout<<"DiscoverTo: ";
            for (auto id = vt->second.sentDiscoverTo.cbegin(); id != vt->second.sentDiscoverTo.cend(); ++id)
            {
                std::wcout<<*id<<", ";
            }
            std::wcout<<std::endl;

            std::wcout<<"NodeInfoTo: ";
            for (auto id = vt->second.sentNodeInfoTo.cbegin(); id != vt->second.sentNodeInfoTo.cend(); ++id)
            {
                std::wcout<<*id<<", ";
            }
            std::wcout<<std::endl<<std::endl;
        }
        std::wcout<<"------------------\n"<<std::endl;
    }

    static Com::Node CreateNode(int64_t i)
    {
        return Com::Node(std::string("discoverer_")+boost::lexical_cast<std::string>(i), i, 1, std::string("127.0.0.1:")+boost::lexical_cast<std::string>(i), "", true);
    }

    static void OnNewNode(int64_t toId, const Com::Node& node)
    {
        boost::mutex::scoped_lock lock(mutex);
        discoverState[toId].newNodes.push_back(node.nodeId);
    }

    struct TestSendPolicy
    {
        void Send(const Com::UserDataPtr val,
                  boost::asio::ip::udp::socket& /*socket*/,
                  const boost::asio::ip::udp::endpoint& to)
        {
            boost::mutex::scoped_lock lock(mutex);
            Com::CommunicationMessage cm;
            cm.ParseFromArray(val->message.get(), static_cast<int>(val->header.totalContentSize));
            if (cm.has_discover())
            {
                discoverState[val->header.commonHeader.senderId].sentDiscoverTo.push_back(to.port());
            }
            else if (cm.has_node_info())
            {
                discoverState[val->header.commonHeader.senderId].sentNodeInfoTo.push_back(to.port());
            }

            discoverState[static_cast<int64_t>(to.port())].recvQueue.push(cm);
        }
    };

    typedef Com::Writer<Com::UserData, TestSendPolicy> TestWriter;
    typedef Com::DiscovererBasic<TestWriter> Discoverer;

    struct Info
    {
        std::vector<int64_t> newNodes;
        std::vector<int64_t> sentDiscoverTo;
        std::vector<int64_t> sentNodeInfoTo;
        std::queue<Com::CommunicationMessage> recvQueue;
        std::shared_ptr<HandleDiscover::Discoverer> discover;

        Info() {}

        Info(int64_t id, boost::asio::io_context& io)
        {
            discover.reset(new HandleDiscover::Discoverer(io, HandleDiscover::CreateNode(id), 1500, std::set<int64_t>{2, 3}, [=](const Com::Node& n){HandleDiscover::OnNewNode(id, n);}));
        }
    };
    static std::map<int64_t, HandleDiscover::Info> discoverState;

    static void Deliver()
    {
        boost::mutex::scoped_lock lock(mutex);
        for (auto vt = discoverState.cbegin(); vt != discoverState.cend(); ++vt)
        {
            auto& dp=vt->second.discover;
            HandleDiscover::Deliver(*dp, vt->first);
        }
    }

    static void Deliver(Discoverer& discoverer, int64_t id)
    {
        while (!discoverState[id].recvQueue.empty())
        {
            auto& cm=discoverState[id].recvQueue.front();
            if (cm.has_discover())
            {
                discoverer.HandleReceivedDiscover(cm.discover());
            }
            if (cm.has_node_info())
            {
                discoverer.HandleReceivedNodeInfo(cm.node_info());
            }
            discoverState[id].recvQueue.pop();
        }
    }
};

boost::mutex HandleDiscover::mutex;
std::map<int64_t, HandleDiscover::Info> HandleDiscover::discoverState;


//--------------------------------------------------
// Test light nodes - DiscoverWithLightNodes
//--------------------------------------------------
class DiscoverWithLightNodes
{
public:
    static void Run()
    {
        std::wcout<<"DiscoverWithLightNodes started"<<std::endl;

        boost::asio::io_context io;
        auto work=boost::asio::make_work_guard(io);
        boost::thread_group threads;
        for (int i = 0; i < 9; ++i)
        {
            threads.create_thread([&]{io.run();});
        }

        //----------------------
        // Test
        //----------------------
        {
            boost::mutex::scoped_lock lock(mutex);
            discoverState.insert(std::make_pair(10000, Info(10000, 1, io)));
            discoverState.insert(std::make_pair(10001, Info(10001, 1, io)));
            discoverState.insert(std::make_pair(10002, Info(10002, 2, io)));
            discoverState.insert(std::make_pair(10003, Info(10003, 3, io)));
        }

        {
            boost::mutex::scoped_lock lock(mutex);
            discoverState[10001].discover->AddSeeds({"127.0.0.1:10000"});
            discoverState[10002].discover->AddSeeds({"127.0.0.1:10000"});
            discoverState[10003].discover->AddSeeds({"127.0.0.1:10001"});
            discoverState[10000].discover->Start();
            discoverState[10001].discover->Start();
            discoverState[10002].discover->Start();
            discoverState[10003].discover->Start();
        }

        //Wait unti node 1000 and 1001 have found each other
        bool passed = false;
        for (int i=0; i<250; ++i)
        {
            Deliver();
            {
                boost::mutex::scoped_lock lock(mutex);
                if (discoverState[10000].newNodes == std::set<int64_t>{10001, 10002, 10003} &&
                    discoverState[10000].sentDiscoverTo == std::set<int64_t>{10001, 10002, 10003} &&
                    discoverState[10000].sentNodeInfoTo == std::set<int64_t>{10001, 10002, 10003} &&

                    discoverState[10001].newNodes == std::set<int64_t>{10000, 10002, 10003} &&
                    discoverState[10001].sentDiscoverTo == std::set<int64_t>{10000, 10002, 10003} &&
                    discoverState[10001].sentNodeInfoTo == std::set<int64_t>{10000, 10002, 10003} &&

                    discoverState[10002].newNodes == std::set<int64_t>{10000, 10001} &&
                    discoverState[10002].sentDiscoverTo == std::set<int64_t>{10000, 10001} &&
                    discoverState[10002].sentNodeInfoTo == std::set<int64_t>{10000, 10001} &&

                    discoverState[10003].newNodes == std::set<int64_t>{10000, 10001} &&
                    discoverState[10003].sentDiscoverTo == std::set<int64_t>{10000, 10001} &&
                    discoverState[10003].sentNodeInfoTo == std::set<int64_t>{10000, 10001})
                {
                    passed = true;
                    break;
                }
            }
            Wait(100);
        }

        if (!passed)
        {
            DumpInfo();
        }
        CHECK(passed);

        // Simulate that all nodes except node 10000 dissapears and tries to re-join.
        // Only the lightNodes 10002, 10003 shall be allowed to join again.
        for (auto id : std::vector<int64_t>{10001, 10002, 10003})
        {
            boost::mutex::scoped_lock lock(mutex);
            discoverState[id].discover->Stop();
            discoverState[10000].discover->ExcludeNode(id);
            discoverState[10000].newNodes.erase(id);
            discoverState[10000].sentDiscoverTo.erase(id);
            discoverState[10000].sentNodeInfoTo.erase(id);
        }

        Wait(3100);

        discoverState.erase(10001);
        discoverState.erase(10002);
        discoverState.erase(10003);

        {
            // Add nodes 10001, 10002, 10003 again with same id and with 10000 as seed.
            boost::mutex::scoped_lock lock(mutex);
            discoverState.insert(std::make_pair(10001, Info(10001, 1, io)));
            discoverState.insert(std::make_pair(10002, Info(10002, 2, io)));
            discoverState.insert(std::make_pair(10003, Info(10003, 3, io)));

            discoverState[10001].discover->AddSeeds({"127.0.0.1:10000"});
            discoverState[10002].discover->AddSeeds({"127.0.0.1:10000"});
            discoverState[10003].discover->AddSeeds({"127.0.0.1:10000"});

            discoverState[10001].discover->Start();
            discoverState[10002].discover->Start();
            discoverState[10003].discover->Start();
        }

        // let node discovering run for a while
        passed = false;
        for (int i=0; i<100; ++i)
        {
            Deliver();
            {
                boost::mutex::scoped_lock lock(mutex);
                if (discoverState[10000].newNodes == std::set<int64_t>{10002, 10003} &&
                    discoverState[10000].sentDiscoverTo == std::set<int64_t>{10002, 10003} &&
                    discoverState[10000].sentNodeInfoTo == std::set<int64_t>{10002, 10003} &&

                    discoverState[10001].newNodes == std::set<int64_t>{} &&
                    discoverState[10001].sentDiscoverTo == std::set<int64_t>{10000} &&
                    discoverState[10001].sentNodeInfoTo == std::set<int64_t>{} &&

                    discoverState[10002].newNodes == std::set<int64_t>{10000} &&
                    discoverState[10002].sentDiscoverTo == std::set<int64_t>{10000} &&
                    discoverState[10002].sentNodeInfoTo == std::set<int64_t>{10000} &&

                    discoverState[10003].newNodes == std::set<int64_t>{10000} &&
                    discoverState[10003].sentDiscoverTo == std::set<int64_t>{10000} &&
                    discoverState[10003].sentNodeInfoTo == std::set<int64_t>{10000})
                {
                    passed = true;
                    break;
                }
            }
            Wait(100);
        }

        if (!passed)
        {
            DumpInfo();
        }
        CHECK(passed);


        // Add a new server node
        {
            boost::mutex::scoped_lock lock(mutex);
            discoverState.insert(std::make_pair(10004, Info(10004, 1, io)));
            discoverState[10004].discover->AddSeeds({"127.0.0.1:10000"});
            discoverState[10004].discover->Start();
        }

        // let node discovering run for a while
        passed = false;
        for (int i=0; i<100; ++i)
        {
            Deliver();
            {
                boost::mutex::scoped_lock lock(mutex);
                if (discoverState[10000].newNodes == std::set<int64_t>{10002, 10003, 10004} &&
                    discoverState[10000].sentDiscoverTo == std::set<int64_t>{10002, 10003, 10004} &&
                    discoverState[10000].sentNodeInfoTo == std::set<int64_t>{10002, 10003, 10004} &&

                    discoverState[10001].newNodes == std::set<int64_t>{} &&
                    discoverState[10001].sentDiscoverTo == std::set<int64_t>{10000} &&
                    discoverState[10001].sentNodeInfoTo == std::set<int64_t>{} &&

                    discoverState[10002].newNodes == std::set<int64_t>{10000, 10004} &&
                    discoverState[10002].sentDiscoverTo == std::set<int64_t>{10000, 10004} &&
                    discoverState[10002].sentNodeInfoTo == std::set<int64_t>{10000, 10004} &&

                    discoverState[10003].newNodes == std::set<int64_t>{10000, 10004} &&
                    discoverState[10003].sentDiscoverTo == std::set<int64_t>{10000, 10004} &&
                    discoverState[10003].sentNodeInfoTo == std::set<int64_t>{10000, 10004} &&

                    discoverState[10004].newNodes == std::set<int64_t>{10000, 10002, 10003} &&
                    discoverState[10004].sentDiscoverTo == std::set<int64_t>{10000, 10002, 10003} &&
                    discoverState[10004].sentNodeInfoTo == std::set<int64_t>{10000, 10002, 10003})
                {
                    passed = true;
                    break;
                }
            }
            Wait(100);
        }

        if (!passed)
        {
            DumpInfo();
        }

        CHECK(passed);


        // Reset communication by first clearing seeds and then excluding all nodes
        {
            boost::mutex::scoped_lock lock(mutex);
            // discoverState[10002].discover->ClearSeeds();
        }



        //Stop all discoverers
        {
            boost::mutex::scoped_lock lock(mutex);
            for (auto kv : discoverState)
            {
                kv.second.discover->Stop();
            }
        }

        //-----------
        // shutdown
        //-----------
        work.reset();
        threads.join_all();

        {
            boost::mutex::scoped_lock lock(mutex);
            discoverState.clear(); //this step is important because discoverState has references to the io_service in this scope.
        }

        std::wcout<<"DiscoverWithLightNodes tests passed"<<std::endl;
    }

private:
    static boost::mutex mutex;

    static Com::Node CreateNode(int64_t nodeId, int64_t nodeType)
    {
        return Com::Node(std::string("discoverer_")+boost::lexical_cast<std::string>(nodeId), nodeId, nodeType, std::string("127.0.0.1:")+boost::lexical_cast<std::string>(nodeId), "", true);
    }

    static void OnNewNode(int64_t toId, const Com::Node& node)
    {
        boost::mutex::scoped_lock lock(mutex);
        discoverState[toId].newNodes.insert(node.nodeId);
    }

    static void DumpInfo()
    {
        boost::mutex::scoped_lock lock(mutex);
        std::wcout<<"------ Info ------"<<std::endl;

        for (auto vt = discoverState.cbegin(); vt != discoverState.cend(); ++vt)
        {
            std::wcout<<"Id: "<<vt->first<<std::endl;
            std::wcout<<"Nodes: ";

            for (auto id = vt->second.newNodes.cbegin(); id != vt->second.newNodes.cend(); ++id)
            {
                std::wcout<<*id<<", ";
            }
            std::wcout<<std::endl;

            std::wcout<<"DiscoverTo: ";
            for (auto id = vt->second.sentDiscoverTo.cbegin(); id != vt->second.sentDiscoverTo.cend(); ++id)
            {
                std::wcout<<*id<<", ";
            }
            std::wcout<<std::endl;

            std::wcout<<"NodeInfoTo: ";
            for (auto id = vt->second.sentNodeInfoTo.cbegin(); id != vt->second.sentNodeInfoTo.cend(); ++id)
            {
                std::wcout<<*id<<", ";
            }
            std::wcout<<std::endl<<std::endl;
        }
        std::wcout<<"------------------\n"<<std::endl;
    }

    struct TestSendPolicy
    {
        void Send(const Com::UserDataPtr val,
                  boost::asio::ip::udp::socket& /*socket*/,
                  const boost::asio::ip::udp::endpoint& to)
        {
            boost::mutex::scoped_lock lock(mutex);
            Com::CommunicationMessage cm;
            cm.ParseFromArray(val->message.get(), static_cast<int>(val->header.totalContentSize));
            if (cm.has_discover())
            {
                discoverState[val->header.commonHeader.senderId].sentDiscoverTo.insert(to.port());
            }
            else if (cm.has_node_info())
            {
                discoverState[val->header.commonHeader.senderId].sentNodeInfoTo.insert(to.port());
            }

            discoverState[static_cast<int64_t>(to.port())].recvQueue.push(cm);
        }
    };
    typedef Com::Writer<Com::UserData, TestSendPolicy> TestWriter;
    typedef Com::DiscovererBasic<TestWriter> Discoverer;

    struct Info
    {
        std::set<int64_t> newNodes;
        std::set<int64_t> sentDiscoverTo;
        std::set<int64_t> sentNodeInfoTo;
        std::queue<Com::CommunicationMessage> recvQueue;
        std::shared_ptr<DiscoverWithLightNodes::Discoverer> discover;

        Info() {}

        Info(int64_t nodeId, int64_t nodeType, boost::asio::io_context& io)
        {
            discover.reset(new DiscoverWithLightNodes::Discoverer(io, DiscoverWithLightNodes::CreateNode(nodeId, nodeType), 1500, std::set<int64_t>{2, 3}, [=](const Com::Node& n){DiscoverWithLightNodes::OnNewNode(nodeId, n);}));
        }
    };
    static std::map<int64_t, DiscoverWithLightNodes::Info> discoverState;

    static void Deliver()
    {
        boost::mutex::scoped_lock lock(mutex);
        for (auto vt = discoverState.cbegin(); vt != discoverState.cend(); ++vt)
        {
            auto& dp=vt->second.discover;
            DiscoverWithLightNodes::Deliver(*dp, vt->first);
        }
    }

    static void Deliver(Discoverer& discoverer, int64_t id)
    {
        while (!discoverState[id].recvQueue.empty())
        {
            auto& cm=discoverState[id].recvQueue.front();
            if (cm.has_discover())
            {
                discoverer.HandleReceivedDiscover(cm.discover());
            }
            if (cm.has_node_info())
            {
                discoverer.HandleReceivedNodeInfo(cm.node_info());
            }
            discoverState[id].recvQueue.pop();
        }
    }

};
boost::mutex DiscoverWithLightNodes::mutex;
std::map<int64_t, DiscoverWithLightNodes::Info> DiscoverWithLightNodes::discoverState;




struct DiscovererTest
{
    static void Run()
    {
        DiscoverToSeed::Run();
        HandleDiscover::Run();
        DiscoverWithLightNodes::Run();
    }
};
