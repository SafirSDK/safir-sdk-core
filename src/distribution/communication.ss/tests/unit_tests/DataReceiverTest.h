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

class DataReceiverTest
{
public:
    static void Run()
    {
        std::cout<<"DataReceiverTest started"<<std::endl;

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
        receiver.reset(new TestDataReceiver(io, "127.0.0.1:10000", "239.192.1.1:11000", [=](const char* d, size_t s){return DataReceiverTest::Recv(d,s);}, [=]{return DataReceiverTest::IsReaderReady();}));
        receiver->Start();

        //--------------------------
        // Unicast tests
        //--------------------------
        std::cout<<"UNICAST_TEST"<<std::endl;
        TRACELINE

        SendUnicast(1);
        SendUnicast(2);
        SendUnicast(3);
        TRACELINE
        for(;;)
        {
            Wait(200);
            {
                boost::mutex::scoped_lock lock(mutex);
                if (received.size()==3)
                    break;
            }
        }
        TRACELINE

        SetReaderReady(false);
        SendUnicast(4); //will also be sent
        SendUnicast(5); //will not be sent until SetReaderReady(true)
        Wait(2000); //If the '5' has still not been received after this time, we can assume it will never come, just as expected.

        TRACELINE
        for(;;)
        {
            Wait(200);
            {
                boost::mutex::scoped_lock lock(mutex);
                if (received.size()==4)
                {
                    TRACELINE
                    break;
                }
            }
        }
        TRACELINE

        //Check that all is as expected
        {
            boost::mutex::scoped_lock lock(mutex);
            CHECK(received.front()==1);
            received.pop();
            CHECK(received.front()==2);
            received.pop();
            CHECK(received.front()==3);
            received.pop();
            CHECK(received.front()==4);
            received.pop();
            CHECK(received.empty()); //now it must be empty otherwize the '5' has arrived.
        }
        TRACELINE

        SetReaderReady(true); //receiver is ready again, now the '5' is expected to arrive

        TRACELINE
        for(;;)
        {
            Wait(200);
            {
                boost::mutex::scoped_lock lock(mutex);
                if (!received.empty())
                {
                    TRACELINE
                    break;
                }
            }
        }

        TRACELINE

        //Check that all is as expected
        {
            boost::mutex::scoped_lock lock(mutex);
            CHECK(received.front()==5);
            received.pop();
            CHECK(received.empty());
        }

        TRACELINE

        //--------------------------
        // Multicast tests
        //--------------------------
        SendMulticast(1);
        SendMulticast(2);
        SendMulticast(3);

        std::cout<<"MULTICAST_TEST"<<std::endl;
        TRACELINE

        for(;;)
        {
            Wait(200);
            {
                boost::mutex::scoped_lock lock(mutex);
                if (received.size()==3)
                {
                    TRACELINE
                    break;
                }
            }
        }

        TRACELINE
        SetReaderReady(false);
        SendMulticast(4); //will also be sent
        SendMulticast(5); //will not be sent until SetReaderReady(true)
        TRACELINE
        Wait(2000); //If the '5' has still not been received after this time, we can assume it will never come, just as expected.

        TRACELINE
        for(;;)
        {
            Wait(200);
            {
                boost::mutex::scoped_lock lock(mutex);
                if (received.size()==4)
                {
                    TRACELINE
                    break;
                }
            }
        }

        TRACELINE
        //Check that all is as expected
        {
            boost::mutex::scoped_lock lock(mutex);
            CHECK(received.front()==1);
            received.pop();
            CHECK(received.front()==2);
            received.pop();
            CHECK(received.front()==3);
            received.pop();
            CHECK(received.front()==4);
            received.pop();
            CHECK(received.empty()); //now it must be empty otherwize the '5' has arrived.
        }
        TRACELINE
        SetReaderReady(true); //receiver is ready again, now the '5' is expected to arrive
        TRACELINE

        for(;;)
        {
            Wait(200);
            {
                boost::mutex::scoped_lock lock(mutex);
                if (!received.empty())
                {
                    TRACELINE
                    break;
                }
            }
        }

        TRACELINE

        //Check that all is as expected
        {
            boost::mutex::scoped_lock lock(mutex);
            CHECK(received.front()==5);
            received.pop();
            CHECK(received.empty());
        }

        TRACELINE

        receiver->Stop();
        work.reset();
        running=false;
        threads.join_all();
        receiver.reset(); //make sure the DataReceiver is destructed before the io_service and the mutex
        TRACELINE
        std::cout<<"DataReceiverTest tests passed"<<std::endl;
    }

private:
    static boost::mutex mutex;
    static std::queue<int> received;
    static std::queue<int> sentUnicast;
    static std::queue<int> sentMulticast;
    static bool running;
    static bool isReady;

    struct TestReader
    {
        void AsyncReceive(char* buf,
                          size_t bufSize,
                          boost::asio::ip::udp::socket* socket,
                          const boost::function< void(const boost::system::error_code&, size_t) >& completionHandler)
        {
            CHECK(bufSize>=sizeof(int));
            bool unicast=(socket->local_endpoint().port()==10000);
            std::queue<int>* sendQueue=unicast ? &sentUnicast : &sentMulticast;
            receiver->Strand().get_io_service().post([&, sendQueue, buf, completionHandler]
            {
                bool received=false;

                while (running)
                {
                    {
                        boost::mutex::scoped_lock lock(mutex);
                        if (!sendQueue->empty())
                        {
                            memcpy(buf, reinterpret_cast<const char*>(&(sendQueue->front())), sizeof(int));
                            sendQueue->pop();
                            received=true;
                            break;
                        }
                    }
                    Wait(100);
                }

                if (received)
                {
                    completionHandler(boost::system::error_code(), sizeof(int));
                }
            });
        }
    };

    typedef Com::DataReceiverType<TestReader> TestDataReceiver;

    static std::unique_ptr<TestDataReceiver> receiver;

    static void SendUnicast(int val)
    {
        boost::mutex::scoped_lock lock(mutex);
        sentUnicast.push(val);
    }

    static void SendMulticast(int val)
    {
        boost::mutex::scoped_lock lock(mutex);
        sentMulticast.push(val);
    }

    static bool IsReaderReady()
    {
        boost::mutex::scoped_lock lock(mutex);
        return isReady;
    }

    static void SetReaderReady(bool val)
    {
        boost::mutex::scoped_lock lock(mutex);
        isReady=val;
    }

    static bool Recv(const char* data, size_t size)
    {
        CHECK(size==sizeof(int));
        boost::mutex::scoped_lock lock(mutex);
        int val=*reinterpret_cast<const int*>(data);
        received.push(val);
        return isReady;
    }
};

boost::mutex DataReceiverTest::mutex;
std::queue<int> DataReceiverTest::received;
std::queue<int> DataReceiverTest::sentUnicast;
std::queue<int> DataReceiverTest::sentMulticast;
bool DataReceiverTest::running=true;
bool DataReceiverTest::isReady=true;
std::unique_ptr<DataReceiverTest::TestDataReceiver> DataReceiverTest::receiver;

#endif
