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
#ifndef _SAFIR_COM_DISCOVERER_TEST_H_
#define _SAFIR_COM_DISCOVERER_TEST_H_

#include "fwd.h"

//------------------------------------------------
// Test that discover is sent to seeds
//------------------------------------------------
class DiscoverToSeed
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

        //----------------------
        // Test
        //----------------------
        Discoverer s0(io, CreateNode(100), [&](const Com::Node&){});
        Discoverer s1(io, CreateNode(200), [&](const Com::Node&){});
        Discoverer n0(io, CreateNode(0), [&](const Com::Node&){});
        Discoverer n1(io, CreateNode(1), [&](const Com::Node&){});
        Discoverer n2(io, CreateNode(2), [&](const Com::Node&){});

        s0.m_timeoutInterval={100, 200};
        s1.m_timeoutInterval={100, 200};
        n0.m_timeoutInterval={100, 200};
        n1.m_timeoutInterval={100, 200};
        n2.m_timeoutInterval={100, 200};

        std::vector<std::string> seeds{"127.0.0.1:10100", "127.0.0.1:10200"};
        n0.AddSeeds(seeds);
        n1.AddSeeds(seeds);
        n2.AddSeeds(seeds);

        s0.Start();
        n0.Start();
        n1.Start();
        n2.Start();
        s1.Start();

        Wait(3000);

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

        bool passed=false;
        {
            boost::mutex::scoped_lock lock(mutex);            
            passed= discoversSentToSeed100[0]>1 && discoversSentToSeed100[1]>1 && discoversSentToSeed100[2]>1 &&
                    discoversSentToSeed200[0]>1 && discoversSentToSeed200[1]>1 && discoversSentToSeed200[2]>1;
        }

        CHECK(passed);

        std::cout<<"SendDiscoverToSeed tests passed"<<std::endl;
    }

private:

    static boost::mutex mutex;
    static std::map<int64_t, int> discoversSentToSeed100;
    static std::map<int64_t, int> discoversSentToSeed200;

    static Com::Node CreateNode(int i)
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
                //std::cout<<"From "<<val->header.commonHeader.senderId<<" to "<<to.port()<<std::endl;
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
    void Run()
    {
        boost::asio::io_service io;
        auto work=boost::make_shared<boost::asio::io_service::work>(io);
        boost::thread_group threads;
        for (int i = 0; i < 9; ++i)
        {
            threads.create_thread([&]{io.run();});
        }

        //----------------------
        // Test
        //----------------------
        discoverState.insert(std::make_pair(10000, Info(10000, io)));
        discoverState.insert(std::make_pair(10001, Info(10001, io)));
        discoverState.insert(std::make_pair(10002, Info(10002, io)));

        auto Deliver=[&]()
        {
            boost::mutex::scoped_lock lock(mutex);
            for (auto& vt : discoverState)
            {
                auto& dp=vt.second.discover;
                HandleDiscover::Deliver(*dp, vt.first);
            }
        };

        std::vector<std::string> seeds{"127.0.0.1:10000"};
        discoverState[10001].discover->AddSeeds(seeds);
        discoverState[10002].discover->AddSeeds(seeds);

        discoverState[10000].discover->Start();
        discoverState[10001].discover->Start();

        Wait(1000); //this is way more than the send discover timer, so hereafter we should have got a discover
        Deliver();

        CHECK(discoverState[10001].sentDiscoverTo.size()>0);
        CHECK(discoverState[10001].sentDiscoverTo.back()==10000);

        Wait(1000);
        Deliver();

        CHECK(discoverState[10000].sentNodeInfoTo.size()>0);
        CHECK(discoverState[10000].sentNodeInfoTo.back()==10001);

        discoverState[10002].discover->Start();

        Wait(500);
        for (int i=0; i<10; ++i)
        {
            Deliver();
            Wait(200);
        }

        for (auto&& vt : discoverState)
        {
            vt.second.discover->Stop();
            CHECK(vt.second.recvQueue.empty());
            CHECK(vt.second.newNodes.size()==2);
        }

        //check that 10000 has got discover and nodeInfo from both the others
        CHECK(std::find(discoverState[10000].sentDiscoverTo.begin(), discoverState[10000].sentDiscoverTo.end(), 10001)!=discoverState[10000].sentDiscoverTo.end());
        CHECK(std::find(discoverState[10000].sentDiscoverTo.begin(), discoverState[10000].sentDiscoverTo.end(), 10002)!=discoverState[10000].sentDiscoverTo.end());
        CHECK(std::find(discoverState[10000].sentNodeInfoTo.begin(), discoverState[10000].sentNodeInfoTo.end(), 10001)!=discoverState[10000].sentNodeInfoTo.end());
        CHECK(std::find(discoverState[10000].sentNodeInfoTo.begin(), discoverState[10000].sentNodeInfoTo.end(), 10002)!=discoverState[10000].sentNodeInfoTo.end());
        //check that 10001 has got discover and nodeInfo from both the others
        CHECK(std::find(discoverState[10001].sentDiscoverTo.begin(), discoverState[10001].sentDiscoverTo.end(), 10000)!=discoverState[10001].sentDiscoverTo.end());
        CHECK(std::find(discoverState[10001].sentDiscoverTo.begin(), discoverState[10001].sentDiscoverTo.end(), 10002)!=discoverState[10001].sentDiscoverTo.end());
        CHECK(std::find(discoverState[10001].sentNodeInfoTo.begin(), discoverState[10001].sentNodeInfoTo.end(), 10000)!=discoverState[10001].sentNodeInfoTo.end());
        CHECK(std::find(discoverState[10001].sentNodeInfoTo.begin(), discoverState[10001].sentNodeInfoTo.end(), 10002)!=discoverState[10001].sentNodeInfoTo.end());
        //check that 10002 has got discover and nodeInfo from both the others
        CHECK(std::find(discoverState[10002].sentDiscoverTo.begin(), discoverState[10002].sentDiscoverTo.end(), 10000)!=discoverState[10002].sentDiscoverTo.end());
        CHECK(std::find(discoverState[10002].sentDiscoverTo.begin(), discoverState[10002].sentDiscoverTo.end(), 10001)!=discoverState[10002].sentDiscoverTo.end());
        CHECK(std::find(discoverState[10002].sentNodeInfoTo.begin(), discoverState[10002].sentNodeInfoTo.end(), 10000)!=discoverState[10002].sentNodeInfoTo.end());
        CHECK(std::find(discoverState[10002].sentNodeInfoTo.begin(), discoverState[10002].sentNodeInfoTo.end(), 10001)!=discoverState[10002].sentNodeInfoTo.end());

        //-----------
        // shutdown
        //-----------
        discoverState.clear();
        work.reset();
        threads.join_all();

        std::cout<<"HandleDiscover tests passed"<<std::endl;
    }

private:

    static boost::mutex mutex;

    static void DumpInfo()
    {
        std::cout<<"------ Info ------"<<std::endl;
        for (auto&& vt : discoverState)
        {
            boost::mutex::scoped_lock lock(mutex);
            std::cout<<"Id: "<<vt.first<<std::endl;
            std::cout<<"Nodes: ";
            for (auto id : vt.second.newNodes) {std::cout<<id<<", ";}
            std::cout<<std::endl;

            std::cout<<"DiscoverTo: ";
            for (auto id : vt.second.sentDiscoverTo) {std::cout<<id<<", ";}
            std::cout<<std::endl;

            std::cout<<"NodeInfoTo: ";
            for (auto id : vt.second.sentNodeInfoTo) {std::cout<<id<<", ";}
            std::cout<<std::endl<<std::endl;
        }
        std::cout<<"------------------\n"<<std::endl;
    }

    static Com::Node CreateNode(int64_t i) //TODO: LAHA: changed this from int to int64_t to get rid of warning. ok?
    {
        return Com::Node(std::string("discoverer_")+boost::lexical_cast<std::string>(i), i, 1, std::string("127.0.0.1:")+boost::lexical_cast<std::string>(i), "", true);
    }

    static void OnNewNode(int64_t toId, const Com::Node& node)
    {
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
        boost::shared_ptr<HandleDiscover::Discoverer> discover;

        Info() {}

        Info(int64_t id, boost::asio::io_service& io)
            :discover(boost::make_shared<HandleDiscover::Discoverer>(io, HandleDiscover::CreateNode(id), [=](const Com::Node& n){HandleDiscover::OnNewNode(id, n);}))
        {
            discover->m_timeoutInterval={100,200};
        }
    };
    static std::map<int64_t, HandleDiscover::Info> discoverState;

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

//--------------------------------------

struct DiscovererTest
{
    void Run()
    {
        DiscoverToSeed().Run();
        HandleDiscover().Run();
    }
};

#endif
