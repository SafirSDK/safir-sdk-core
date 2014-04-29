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
#ifndef _SAFIR_COM_WRITER_TEST_H_
#define _SAFIR_COM_WRITER_TEST_H_

#include "fwd.h"

class WriterTest
{
public:
    void Run()
    {
//        boost::asio::io_service io;
//        Com::Node me{"Test", 100, "127.0.0.1:10000", "239.192.1.1:11000"};
//        Com::Writer<std::string, WriterTest::TestSendPolicy> writer(io, me);

//        m_nodes.insert(std::make_pair(1, Com::Node{"Test_1", 1, "127.0.0.1:10001", "239.192.1.1:11000"}));
//        m_nodes.insert(std::make_pair(2, Com::Node{"Test_2", 2, "127.0.0.1:10002", ""}));
//        m_nodes.insert(std::make_pair(3, Com::Node{"Test_3", 3, "127.0.0.1:10003", ""}));
//        m_nodes.insert(std::make_pair(4, Com::Node{"Test_4", 4, "127.0.0.1:10004", "239.192.1.1:11000"}));

//        //Unicast
//        {
//            boost::shared_ptr<std::string> strPtr=boost::make_shared<std::string>("Kalle");
//            writer.SendTo(strPtr, Node(1)->Endpoint());
//            auto it=received.find(10001);
//            CHECK(it!=received.end());
//            CHECKMSG(it->first==10001, it->first);
//            CHECKMSG(it->second=="Kalle", it->second);
//            received.clear();boost::asio::io_service io;
//            Com::Node me{"Test", 100, "127.0.0.1:10000", "239.192.1.1:11000"};
//            Com::Writer<std::string, WriterTest::TestSendPolicy> writer(io, me);
//        }

//        //multicast
//        {
//            writer.SendMulticast(boost::make_shared<std::string>("Svarre"));
//            auto it=received.find(11000);
//            CHECK(it!=received.end());
//            CHECKMSG(it->first==11000, it->first);
//            CHECKMSG(it->second=="Svarre", it->second);
//            received.clear();
//        }

//        //all system nodes - no system nodes exist
//        {
//            writer.SendToAllSystemNodes(boost::make_shared<std::string>("Bazinga"), m_nodes);
//            CHECK(received.empty());
//        }

//        //send to all system nodes - all nodes are system nodes and sender is multicast enabled
//        {
//            for (size_t i=1; i<=m_nodes.size(); ++i)
//            {
//                Node(i)->SetSystemNode(true);
//            }

//            writer.SendToAllSystemNodes(boost::make_shared<std::string>("Bazinga"), m_nodes);
//            CHECK(received.size()==3);

//            CHECK(received.find(11000)!=received.end());
//            CHECK(received.find(10002)!=received.end());
//            CHECK(received.find(10003)!=received.end());
//            for (auto& vt : received)
//            {
//                CHECK(vt.second=="Bazinga");
//            }
//            received.clear();
//        }

//        //send to all system nodes - some nodes are system nodes and sender is multicast enabled
//        {
//            Node(1)->SetSystemNode(false);
//            Node(2)->SetSystemNode(false);

//            writer.SendToAllSystemNodes(boost::make_shared<std::string>("Abc123"), m_nodes);
//            CHECK(received.size()==2);

//            CHECK(received.find(11000)!=received.end());
//            CHECK(received.find(10003)!=received.end());
//            for (auto& vt : received)
//            {
//                CHECK(vt.second=="Abc123");
//            }
//            received.clear();
//        }

//        //send to all system nodes - all nodes are system nodes and sender is NOT multicast enabled
//        {
//            boost::asio::io_service io2;
//            Com::Node me2{"Test", 100, "127.0.0.1:10000", ""};
//            Com::Writer<std::string, WriterTest::TestSendPolicy> writer2(io2, me2);

//            Node(1)->SetSystemNode(true);
//            Node(2)->SetSystemNode(true);

//            writer2.SendToAllSystemNodes(boost::make_shared<std::string>("Murmel"), m_nodes);
//            CHECK(received.size()==4);

//            CHECK(received.find(10001)!=received.end());
//            CHECK(received.find(10002)!=received.end());
//            CHECK(received.find(10003)!=received.end());
//            CHECK(received.find(10004)!=received.end());
//            for (auto& vt : received)
//            {
//                CHECK(vt.second=="Murmel");
//            }
//            received.clear();
//        }

//        std::cout<<"Writer tests passed"<<std::endl;
    }

private:
    Com::NodeMap m_nodes;
    Com::Node* Node(boost::uint64_t id)
    {
        auto it=m_nodes.find(id);
        if (it!=m_nodes.end())
        {
            return &it->second;
        }
        return nullptr;
    }

    static std::map<unsigned short, std::string> received;

    struct TestSendPolicy
    {
        void Send(const boost::shared_ptr<std::string>& val,
                  boost::asio::ip::udp::socket& /*socket*/,
                  const boost::asio::ip::udp::endpoint& to)
        {
            WriterTest::received.insert(std::make_pair(to.port(), *val));
        }
    };

};

std::map<unsigned short, std::string> WriterTest::received;

#endif
