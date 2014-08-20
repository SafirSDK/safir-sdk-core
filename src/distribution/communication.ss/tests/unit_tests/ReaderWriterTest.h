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
        std::atomic_uint go{0};
        auto SetReady=[&]{go=1;};
        auto WaitUntilReady=[&]
        {
            while(go==0)
                Wait(20);
            go=0;
        };

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
        Com::DataReceiver reader(io, "127.0.0.1:10000", "239.192.1.1:11000", [=](const char* d, size_t s){return Recv(d,s);}, [=]{return IsReaderReady();});
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
        WaitUntilNotEmpty(__LINE__);
        CHECKMSG(received.front()==1, (received.empty() ? -1 : received.front()));
        received.pop();

        SendUnicast(2);
        WaitUntilNotEmpty(__LINE__);
        CHECKMSG(received.front()==2, (received.empty() ? -1 : received.front()));
        received.pop();

        SetReaderReady(false);
        SendUnicast(3); //after this send reader will be blocked

        WaitUntilNotEmpty(__LINE__);
        CHECKMSG(received.front()==3, (received.empty() ? -1 : received.front()));
        received.pop();

        io.post([&]{SetReady();});
        WaitUntilReady();

        CHECKMSG(received.empty(), received.size());

        SendUnicast(4);
        SendUnicast(5);

        io.post([&]{SetReady();});
        WaitUntilReady();

        Wait(1000); //reader shall be blocked.

        CHECKMSG(received.empty(), received.size());

        SetReaderReady(true);

        WaitUntilNotEmpty(__LINE__);
        CHECKMSG(received.front()==4, (received.empty() ? -1 : received.front()));
        received.pop();

        WaitUntilNotEmpty(__LINE__);
        CHECKMSG(received.front()==5, (received.empty() ? -1 : received.front()));
        received.pop();

        CHECKMSG(received.empty(), received.size());

        //Multicast tests
        SendMulticast(1);
        WaitUntilNotEmpty(__LINE__);
        CHECKMSG(received.front()==1, (received.empty() ? -1 : received.front()));
        received.pop();

        SendMulticast(2);
        WaitUntilNotEmpty(__LINE__);
        CHECKMSG(received.front()==2, (received.empty() ? -1 : received.front()));
        received.pop();


        SetReaderReady(false);

        SendMulticast(3); //after this send reader will be blocked
        WaitUntilNotEmpty(__LINE__);
        CHECKMSG(received.front()==3, (received.empty() ? -1 : received.front()));
        received.pop();

        CHECKMSG(received.empty(), received.size());

        SendMulticast(4);
        SendMulticast(5);

        io.post([&]{SetReady();});
        WaitUntilReady();

        Wait(1000);

        CHECKMSG(received.empty(), received.size());

        SetReaderReady(true);

        WaitUntilNotEmpty(__LINE__);
        CHECKMSG(received.front()==4, (received.empty() ? -1 : received.front()));
        received.pop();

        WaitUntilNotEmpty(__LINE__);
        CHECKMSG(received.front()==5, (received.empty() ? -1 : received.front()));
        received.pop();

        CHECKMSG(received.empty(), received.size());

        io.post([&]{SetReady();});
        WaitUntilReady();

        reader.Stop();
        work.reset();
        threads.join_all();
        std::cout<<"ReaderWriterTest tests passed"<<std::endl;
    }

private:
    static boost::mutex mutex;
    static std::queue<int> received;
    static bool isReady;

    void WaitUntilNotEmpty(int line) const
    {
        for (int i=0; i<1000; ++i)
        {
            {
                boost::mutex::scoped_lock lock(mutex);
                if (!received.empty())
                    return;
            }
            Wait(50);
        }
        std::cout<<"Wait forever at "<<line<<std::endl;
    }

    bool IsReaderReady() const
    {
        boost::mutex::scoped_lock lock(mutex);
        return isReady;
    }

    void SetReaderReady(bool val)
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
