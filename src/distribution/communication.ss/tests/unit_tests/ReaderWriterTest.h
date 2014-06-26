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
#ifndef _SAFIR_COM_READER_TEST_H_
#define _SAFIR_COM_READERTEST_H_

#include "fwd.h"

inline boost::shared_ptr<int> Int(int i) {return boost::make_shared<int>(i);}

class ReaderWriterTest
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

        //--------------------------
        // Setup
        //--------------------------
        Com::Reader reader(io, "127.0.0.1:10000", "239.192.1.1:11000", [=](const char* d, size_t s){return Recv(d,s);}, [=]{return IsReady();});
        reader.Start();
        Com::Writer<int> writer(io, 4, "239.192.1.1:11000");
        auto endpoint=Com::Utilities::CreateEndpoint("127.0.0.1:10000");
        auto SendUnicast=[&](int i){writer.SendTo(boost::make_shared<int>(i), endpoint);};
        auto SendMulticast=[&](int i){writer.SendMulticast(boost::make_shared<int>(i));};

        //--------------------------
        // Test starts
        //--------------------------

        //Unicast tests
        SendUnicast(1);
        SendUnicast(2);
        Wait(1000);

        SetReady(false);
        SendUnicast(3); //after this send reader will be blocked

        CHECK(received.front()==1);
        received.pop();
        CHECK(received.front()==2);
        received.pop();
        Wait(1000);
        CHECK(received.front()==3);
        received.pop();
        CHECK(received.empty());

        SendUnicast(4);
        SendUnicast(5);
        Wait(1000);
        CHECK(received.empty());

        SetReady(true);
        Wait(1000);
        CHECK(received.front()==4);
        received.pop();
        CHECK(received.front()==5);
        received.pop();
        CHECK(received.empty());

        //Multicast tests
        SendMulticast(1);
        SendMulticast(2);
        Wait(1000);

        SetReady(false);
        SendMulticast(3); //after this send reader will be blocked

        CHECK(received.front()==1);
        received.pop();
        CHECK(received.front()==2);
        received.pop();
        Wait(1000);
        CHECK(received.front()==3);
        received.pop();
        CHECK(received.empty());

        SendMulticast(4);
        SendMulticast(5);
        Wait(1000);
        CHECK(received.empty());

        SetReady(true);
        Wait(1000);
        CHECK(received.front()==4);
        received.pop();
        CHECK(received.front()==5);
        received.pop();
        CHECK(received.empty());

        reader.Stop();
        work.reset();
        threads.join_all();
        std::cout<<"ReaderWriterTest tests passed"<<std::endl;
    }

private:
    static boost::mutex mutex;
    static std::queue<int> received;
    static bool isReady;

    bool IsReady() const
    {
        boost::mutex::scoped_lock lock(mutex);
        return isReady;
    }

    void SetReady(bool val)
    {
        boost::mutex::scoped_lock lock(mutex);
        isReady=val;
    }

    bool Recv(const char* data, size_t size)
    {
        CHECK(size==sizeof(int));
        boost::mutex::scoped_lock lock(mutex);

        int val=*reinterpret_cast<const int*>(data);
        received.emplace(val);
        return isReady;
    }
};

boost::mutex ReaderWriterTest::mutex;
std::queue<int> ReaderWriterTest::received;
bool ReaderWriterTest::isReady=true;

#endif
