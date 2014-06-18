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
        Com::Node me("d1", 1, 1, "127.0.0.1:10000", "", true);
        Discoverer s0(io, CreateNode(100), [&](const Com::Node& n){});
        Discoverer s1(io, CreateNode(200), [&](const Com::Node& n){});
        Discoverer n0(io, CreateNode(0), [&](const Com::Node& n){});
        Discoverer n1(io, CreateNode(1), [&](const Com::Node& n){});
        Discoverer n2(io, CreateNode(2), [&](const Com::Node& n){});

        std::vector<std::string> seeds{"127.0.0.1:10100", "127.0.0.1:10200"};
        n0.AddSeeds(seeds);
        n1.AddSeeds(seeds);
        n2.AddSeeds(seeds);

        s0.Start();
        n0.Start();
        n1.Start();
        n2.Start();
        s1.Start();

        Wait(10000);

        s0.Stop();
        n0.Stop();
        n1.Stop();
        n2.Stop();
        s1.Stop();

        //-----------
        // shutdown
        //-----------
        work.reset();
        io.stop();
        threads.join_all();

        bool passed=false;
        {
            boost::mutex::scoped_lock lock(mutex);
            passed= discoversSentToSeed100[0]>1 && discoversSentToSeed100[1]>1 && discoversSentToSeed100[2]>1 &&
                    discoversSentToSeed200[0]>1 && discoversSentToSeed200[1]>1 && discoversSentToSeed200[2]>1;
        }

        if (!passed)
            exit(1);

        std::cout<<"SendDiscoverToSeed tests passed"<<std::endl;
    }

private:

    static boost::mutex mutex;
    static std::map<boost::int64_t, int> discoversSentToSeed100;
    static std::map<boost::int64_t, int> discoversSentToSeed200;

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
std::map<boost::int64_t, int> DiscoverToSeed::discoversSentToSeed100;
std::map<boost::int64_t, int> DiscoverToSeed::discoversSentToSeed200;

//--------------------------------------------------

struct DiscovererTest
{
    void Run()
    {
        DiscoverToSeed().Run();
    }
};

#endif
