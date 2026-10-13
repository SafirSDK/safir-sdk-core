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
                       (strand,
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
        receiver.reset(); //make sure the DataReceiver is destructed before the io_context and the mutex
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


//------------------------------------------------------------------
// Bad datagrams and failed reads must not stop a DataReceiver
//------------------------------------------------------------------
// Runs a DataReceiver on real loopback sockets, with a reader that can make
// reads fail.
//  1. Datagrams too short to hold a crc arrive on each socket. They must be
//     thrown away, and both sockets must go on delivering.
//  2. One read fails on each socket. Both must deliver datagrams again (#614).
//  3. Every unicast read fails for a while. The receiver must keep retrying
//     without spinning, and deliver again once reads work.
// The multicast half is skipped on hosts where multicast does not work on loopback.
class DataReceiverReadErrorTest
{
public:
    static void Run()
    {
        std::wcout<<"DataReceiverReadErrorTest started"<<std::endl;

        boost::asio::io_context io;
        auto work = boost::asio::make_work_guard(io);
        boost::thread_group threads;
        for (int i = 0; i < 3; ++i)
        {
            threads.create_thread([&]{io.run();});
        }
        boost::asio::io_context::strand strand(io);

        const auto loopback = boost::asio::ip::make_address("127.0.0.1");
        const boost::asio::ip::udp::endpoint unicastEp(loopback, FreePort(io));
        const boost::asio::ip::udp::endpoint multicastEp(boost::asio::ip::make_address("239.192.1.1"), FreePort(io));
        unicastPort = unicastEp.port();

        FailingDataReceiver receiver(strand,
                                     Endpoint(unicastEp),
                                     Endpoint(multicastEp),
                                     [](const char* data, size_t size, bool multicast){return Recv(data, size, multicast);},
                                     []{return true;});
        receiver.Start();

        boost::asio::ip::udp::socket sender(io, boost::asio::ip::udp::v4());
        boost::system::error_code ignored;
        sender.set_option(boost::asio::ip::multicast::outbound_interface(loopback.to_v4()), ignored);
        sender.set_option(boost::asio::ip::multicast::enable_loopback(true), ignored);

        int value = 0;
        const auto sendBoth = [&]
        {
            ++value;
            Send(sender, unicastEp, value);
            Send(sender, multicastEp, value);
        };

        //Both read loops work to begin with. Multicast gets a couple of seconds to show up.
        auto deadline = std::chrono::steady_clock::now() + std::chrono::seconds(10);
        const auto multicastDeadline = std::chrono::steady_clock::now() + std::chrono::seconds(2);
        while (Last(lastUnicast) == 0 || (Last(lastMulticast) == 0 && std::chrono::steady_clock::now() < multicastDeadline))
        {
            CHECKMSG(std::chrono::steady_clock::now() < deadline, "No unicast received before any error was injected");
            sendBoth();
            Wait(100);
        }
        const bool multicastWorks = Last(lastMulticast) != 0;
        if (!multicastWorks)
        {
            std::wcout<<"No multicast on loopback on this host, testing unicast only"<<std::endl;
        }
        TRACELINE

        //--------------------------------------------
        // Datagrams too short to hold a crc
        //--------------------------------------------
        //These used to crash the receiver, so getting past this at all is most of the test.
        int lastBeforeError = value;
        for (size_t size = 0; size < sizeof(uint32_t); ++size)
        {
            const char data[sizeof(uint32_t)] = {};
            boost::system::error_code ignoredError;
            sender.send_to(boost::asio::buffer(data, size), unicastEp, 0, ignoredError);
            sender.send_to(boost::asio::buffer(data, size), multicastEp, 0, ignoredError);
        }
        deadline = std::chrono::steady_clock::now() + std::chrono::seconds(10);
        while (Last(lastUnicast) <= lastBeforeError || (multicastWorks && Last(lastMulticast) <= lastBeforeError))
        {
            CHECKMSG(std::chrono::steady_clock::now() < deadline, "Nothing received after datagrams too short to hold a crc");
            sendBoth();
            Wait(100);
        }
        TRACELINE

        //--------------------------
        // A single failed read
        //--------------------------
        //The datagram that completes the failing read is lost.
        failUnicast = true;
        failMulticast = multicastWorks;
        deadline = std::chrono::steady_clock::now() + std::chrono::seconds(10);
        while (failUnicast || failMulticast)
        {
            CHECKMSG(std::chrono::steady_clock::now() < deadline, "The read errors were never injected");
            sendBoth();
            Wait(100);
        }
        lastBeforeError = value;
        TRACELINE

        deadline = std::chrono::steady_clock::now() + std::chrono::seconds(10);
        while (Last(lastUnicast) <= lastBeforeError || (multicastWorks && Last(lastMulticast) <= lastBeforeError))
        {
            CHECKMSG(std::chrono::steady_clock::now() < deadline, "Read loop is dead after a single read error (#614)");
            sendBoth();
            Wait(100);
        }
        TRACELINE

        //--------------------------
        // Reads that keep failing
        //--------------------------
        //The read that is already waiting on the socket is a real one, so it takes
        //a datagram to get to the first read that fails.
        unicastAttempts = 0;
        unicastDown = true;
        deadline = std::chrono::steady_clock::now() + std::chrono::seconds(10);
        while (unicastAttempts == 0)
        {
            CHECKMSG(std::chrono::steady_clock::now() < deadline, "No read was started while reads were failing");
            sendBoth();
            Wait(10);
        }
        Wait(3000);
        const int attempts = unicastAttempts;
        std::wcout<<"Read attempts in 3 seconds of failing reads: "<<attempts<<std::endl;
        //The delay doubles from 10 ms to a second, so 3 seconds is 9 or 10 attempts.
        CHECKMSG(attempts >= 5, "The receiver gave up or is retrying far too seldom");
        CHECKMSG(attempts <= 20, "The receiver is retrying too often");
        lastBeforeError = value;
        unicastDown = false;
        TRACELINE

        deadline = std::chrono::steady_clock::now() + std::chrono::seconds(10);
        while (Last(lastUnicast) <= lastBeforeError)
        {
            CHECKMSG(std::chrono::steady_clock::now() < deadline, "Unicast read loop did not recover after a period of failing reads");
            sendBoth();
            Wait(100);
        }
        TRACELINE

        receiver.Stop();
        Wait(200);
        sender.close();
        work.reset();
        threads.join_all();

        std::wcout<<"DataReceiverReadErrorTest tests passed"<<std::endl;
    }

private:
    static boost::mutex mutex;
    static int lastUnicast;
    static int lastMulticast;
    static unsigned short unicastPort;
    static std::atomic<bool> failUnicast; //fail the next unicast read that gets a datagram
    static std::atomic<bool> failMulticast; //the same for multicast
    static std::atomic<bool> unicastDown; //fail every unicast read at once
    static std::atomic<int> unicastAttempts; //unicast reads started while unicastDown

    //A SocketReader whose reads can be made to fail.
    struct FailingReader : private Com::SocketReader
    {
        void AsyncReceive(char* buf,
                          size_t bufSize,
                          boost::asio::ip::udp::socket* socket,
                          const std::function< void(const boost::system::error_code&, size_t) >& completionHandler)
        {
            const bool unicast = socket->local_endpoint().port() == unicastPort;
            if (unicast && unicastDown)
            {
                ++unicastAttempts;
                boost::asio::post(socket->get_executor(), [completionHandler]{completionHandler(boost::asio::error::no_buffer_space, 0);});
                return;
            }

            auto& fail = unicast ? failUnicast : failMulticast;
            Com::SocketReader::AsyncReceive(buf, bufSize, socket, [&fail, completionHandler](const boost::system::error_code& ec, size_t size)
            {
                if (!ec && fail.exchange(false))
                {
                    completionHandler(boost::asio::error::no_buffer_space, 0);
                }
                else
                {
                    completionHandler(ec, size);
                }
            });
        }
    };

    typedef Com::DataReceiverType<FailingReader> FailingDataReceiver;

    static unsigned short FreePort(boost::asio::io_context& io)
    {
        boost::asio::ip::udp::socket socket(io, boost::asio::ip::udp::endpoint(boost::asio::ip::make_address("127.0.0.1"), 0));
        return socket.local_endpoint().port();
    }

    static std::string Endpoint(const boost::asio::ip::udp::endpoint& ep)
    {
        return ep.address().to_string() + ":" + boost::lexical_cast<std::string>(ep.port());
    }

    static void Send(boost::asio::ip::udp::socket& socket, const boost::asio::ip::udp::endpoint& to, int val)
    {
        char data[sizeof(int) + sizeof(uint32_t)];
        memcpy(data, &val, sizeof(int));
        boost::crc_32_type crc;
        crc.process_bytes(static_cast<const void*>(data), sizeof(int));
        const uint32_t checksum = crc.checksum();
        memcpy(data + sizeof(int), &checksum, sizeof(checksum));
        //A host without a multicast route refuses the send, and then there is just no multicast half.
        boost::system::error_code ignored;
        socket.send_to(boost::asio::buffer(data), to, 0, ignored);
    }

    static bool Recv(const char* data, size_t size, bool multicast)
    {
        CHECK(size == sizeof(int));
        int val;
        memcpy(&val, data, sizeof(int));
        boost::mutex::scoped_lock lock(mutex);
        (multicast ? lastMulticast : lastUnicast) = val;
        return true;
    }

    static int Last(const int& last)
    {
        boost::mutex::scoped_lock lock(mutex);
        return last;
    }
};

boost::mutex DataReceiverReadErrorTest::mutex;
int DataReceiverReadErrorTest::lastUnicast = 0;
int DataReceiverReadErrorTest::lastMulticast = 0;
unsigned short DataReceiverReadErrorTest::unicastPort = 0;
std::atomic<bool> DataReceiverReadErrorTest::failUnicast(false);
std::atomic<bool> DataReceiverReadErrorTest::failMulticast(false);
std::atomic<bool> DataReceiverReadErrorTest::unicastDown(false);
std::atomic<int> DataReceiverReadErrorTest::unicastAttempts(0);

//--------------------------------
// Start DataReceiverTest tests
//--------------------------------
struct DataReceiverTest
{
    static void Run()
    {
        DataReceiverTester::Run();
        DataReceiverSimulateNetworkUpDownTest().Run();
        DataReceiverReadErrorTest::Run();
    }
};
