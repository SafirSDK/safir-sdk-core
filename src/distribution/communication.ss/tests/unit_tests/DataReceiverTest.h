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
#include <memory>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4244)
#pragma warning (disable: 4245)
#pragma warning (disable: 4701)
#endif

#include <boost/crc.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif


inline std::shared_ptr<int> Int(int i) {return std::make_shared<int>(i);}

class DataReceiverTester
{
public:
    static void Run()
    {
        std::cout<<"DataReceiverTester started"<<std::endl;

        boost::asio::io_context io;
        auto work = boost::asio::make_work_guard(io);
        boost::thread_group threads;
        for (int i = 0; i < 9; ++i)
        {
            threads.create_thread([&]{io.run();});
        }
        boost::asio::io_context::strand strand(io);

        //--------------------------
        // Setup
        //--------------------------
        TRACELINE


        receiver.reset(new TestDataReceiver
                       (1, strand,
                        "127.0.0.1:10000",
                        "239.192.1.1:11000",
                        [](const char* data, size_t size, bool /*multicast*/){return Recv(data,size);},
                        []{return IsReaderReady();}));

        TRACELINE
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
        std::cout<<"DataReceiverTester tests passed"<<std::endl;
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
                          const std::function< void(const boost::system::error_code&, size_t) >& completionHandler)
        {
            CHECK(bufSize>=sizeof(int)+sizeof(uint32_t)); //int and checksum
            bool unicast=(socket->local_endpoint().port()==10000);
            std::queue<int>* sendQueue=unicast ? &sentUnicast : &sentMulticast;

            boost::asio::post(receiver->m_strand.context(), [&, sendQueue, buf, completionHandler]
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

                            //calc crc
                            boost::crc_32_type crc;
                            crc.process_bytes(static_cast<const void*>(buf), sizeof(int));
                            uint32_t checksum=crc.checksum();
                            memcpy(buf+sizeof(int), reinterpret_cast<const char*>(&checksum), sizeof(checksum));
                            break;
                        }
                    }
                    Wait(100);
                }

                if (received)
                {
                    completionHandler(boost::system::error_code(), sizeof(int)+sizeof(uint32_t));
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

boost::mutex DataReceiverTester::mutex;
std::queue<int> DataReceiverTester::received;
std::queue<int> DataReceiverTester::sentUnicast;
std::queue<int> DataReceiverTester::sentMulticast;
bool DataReceiverTester::running=true;
bool DataReceiverTester::isReady=true;
std::unique_ptr<DataReceiverTester::TestDataReceiver> DataReceiverTester::receiver;

//------------------------------------
// Test simulation of network up/down
//------------------------------------
class DataReceiverSimulateNetworkUpDownTest
{
public:
    DataReceiverSimulateNetworkUpDownTest()
        :work(boost::asio::make_work_guard(io))
        ,ep(boost::asio::ip::make_address("127.0.0.1"), 10123)
        ,recvSocket(io, ep.protocol())
        ,senderSocket(io, ep.protocol())

    {
        senderSocket.set_option(boost::asio::ip::udp::socket::reuse_address(true));
        recvSocket.set_option(boost::asio::ip::udp::socket::reuse_address(true));
        recvSocket.bind(ep);
    }

    void Run()
    {
        std::wcout<<"DataReceiverSimulateNetworkUpDownTest started"<<std::endl;
        boost::thread_group threads;
        for (int i = 0; i < 1; ++i)
        {
            threads.create_thread([&]{io.run();});
        }

        // start the SocketReader that we are going to test
        Read();

        Wait(1000);

        std::string m0 = "a";
        std::string m1 = "b";
        senderSocket.send_to(boost::asio::buffer(m0), ep);
        Wait(100);
        senderSocket.send_to(boost::asio::buffer(m1), ep);

        TRACELINE;
        Expect({"a", "b"});

        TRACELINE;
        // simulate network down
        Safir::Dob::Internal::Com::Parameters::NetworkEnabled = false;

        // One more message is also expected to arrive since the socket is already an an async_receive.
        // This should not be a problem since heartbeats are sent frequently and then the DataReceiver will notice that
        // we have entered the simulated network-down-mode.
        std::string expected = "expected";
        std::string notExpected = "not expected";
        senderSocket.send_to(boost::asio::buffer(expected), ep);
        Wait(100);
        senderSocket.send_to(boost::asio::buffer(notExpected), ep);
        senderSocket.send_to(boost::asio::buffer(notExpected), ep);
        senderSocket.send_to(boost::asio::buffer(notExpected), ep);
        Wait(100);
        senderSocket.send_to(boost::asio::buffer(notExpected), ep);
        senderSocket.send_to(boost::asio::buffer(notExpected), ep);
        senderSocket.send_to(boost::asio::buffer(notExpected), ep);

        TRACELINE;
        Expect({"a", "b", "expected"});

        Wait(500);
        Safir::Dob::Internal::Com::Parameters::NetworkEnabled = true;
        std::string m2 = "c";
        std::string m3 = "d";
        senderSocket.send_to(boost::asio::buffer(m2), ep);
        Wait(100);
        senderSocket.send_to(boost::asio::buffer(m3), ep);

        TRACELINE;
        Expect({"a", "b", "expected", "c", "d"});

        TRACELINE;
        // end program
        senderSocket.close();
        recvSocket.close();
        work.reset();
        threads.join_all();
        std::wcout<<"DataReceiverSimulateNetworkUpDownTest tests passed"<<std::endl;
    }

private:
    boost::mutex mutex;
    char buf[1024];
    std::vector<std::string> recvMessages;
    boost::asio::io_context io;
    boost::asio::executor_work_guard<boost::asio::io_context::executor_type> work;
    boost::asio::ip::udp::endpoint ep;
    boost::asio::ip::udp::socket recvSocket;
    boost::asio::ip::udp::socket senderSocket;

    Safir::Dob::Internal::Com::SocketReader reader;

    void Read()
    {
        reader.AsyncReceive(buf, 1024, &recvSocket, [this](const boost::system::error_code& ec, size_t size){HandleRecv(ec, size);});
    }

    void HandleRecv(const boost::system::error_code& ec, size_t size)
    {
        if (ec)
        {
            return;
        }

        std::string val(buf, buf + size);
        {
            boost::mutex::scoped_lock lock(mutex);
            recvMessages.push_back(val);
        }

        Read();
    }

    void Expect(const std::vector<std::string>& expected)
    {
        int totalWait = 0;

        // Wait for at most 20 sec for all data to arrive
        while (totalWait < 20000)
        {
            Wait(500);
            totalWait+=500;
            {
                boost::mutex::scoped_lock lock(mutex);
                if (recvMessages.size() >= expected.size())
                {
                    break;
                }

            }
        }

        // Check that data is the expected
        {
            boost::mutex::scoped_lock lock(mutex);
            if (recvMessages.size() != expected.size())
            {
                std::wcout << L"*** ERROR ***" << std::endl;
                Dump(expected, recvMessages);
            }
            CHECKMSG(recvMessages.size() == expected.size(), recvMessages.size());

            for (size_t i = 0; i < expected.size(); ++i)
            {
                if (recvMessages[i] != expected[i])
                {
                    std::wcout << L"*** ERROR ***" << std::endl;
                    Dump(expected, recvMessages);
                }
                CHECK(recvMessages[i] == expected[i]);
            }
        }
    }

    void Dump(const std::vector<std::string>& e, const std::vector<std::string>& r) const
    {
        std::wcout << L"--- Expected:" << std::endl;
        int index = 0;
        for (const auto& m : e)
        {
            std::wcout << L"    " << (index++) << L". " << m.c_str() << std::endl;
        }

        std::wcout << L"--- Received:" << std::endl;
        index = 0;
        for (const auto& m : r)
        {
            std::wcout << L"    " << (index++) << L". " << m.c_str() << std::endl;
        }
    }

};


//--------------------------------
// Start DataReceiverTest tests
//--------------------------------
struct DataReceiverTest
{
    static void Run()
    {
        DataReceiverTester::Run();
        DataReceiverSimulateNetworkUpDownTest().Run();
    }
};
