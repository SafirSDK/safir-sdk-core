/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
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
#include <Safir/Utilities/Internal/MakeUnique.h>
#include <Safir/Dob/Internal/RawStatistics.h>
#include "../../src/RawPublisherRemote.h"

#define BOOST_TEST_MODULE RawPublisherRemoteTest
#include <boost/test/unit_test.hpp>


using namespace Safir::Dob::Internal::SP;


int numPerform = 0;
int numSend10 = 0;
int numSend20 = 0;
size_t gsize = 0;
StatisticsCallback cb;

class Handler
{
public:
    void AddRawChangedCallback(const StatisticsCallback& callback)
    {
        cb = callback;
    }

    void PerformOnMyStatisticsMessage(const std::function<void(std::unique_ptr<char []> data,
                                      const size_t size)> & fn) const
    {
        std::wcout << "Perform " << numPerform << std::endl;
        const size_t size = 10;
        auto data = std::unique_ptr<char[]>(new char[size]);
        strcpy(data.get(),"123456789");
        gsize = size;
        fn(std::move(data), size);

        ++numPerform;
        if (numPerform == 10)
        {
            stopCall();
        }
    }

    std::function<void()> stopCall;
};

class Communication
{
public:
    bool Send(int64_t nodeId,
              int64_t nodeTypeId,
              const Safir::Utilities::Internal::SharedCharArray& data,
              size_t size,
              int64_t /*dataTypeIdentifier*/,
              bool acked)
    {
        BOOST_CHECK(nodeId == 0);
        BOOST_CHECK(acked);
        BOOST_CHECK(size == gsize);
        BOOST_CHECK(data != nullptr);
        BOOST_CHECK(0 == strcmp(data.get(), "123456789"));
        std::wcout << "Send " << nodeTypeId << std::endl;
        BOOST_CHECK(nodeTypeId == 10 || nodeTypeId == 20);
        if (nodeTypeId == 10)
        {
            ++numSend10;
            return true;
        }
        else
        {
            ++numSend20;
            return numSend20 % 2 == 0; //cause some overflows
        }

    }
};

BOOST_AUTO_TEST_CASE( send_ten )
{
    Handler h;
    Communication communication;
    boost::asio::io_service ioService;

    std::vector<boost::chrono::steady_clock::duration> retryTimeouts;
    retryTimeouts.push_back(boost::chrono::seconds(1));
    retryTimeouts.push_back(boost::chrono::seconds(2));

    std::map<int64_t, NodeType> nodeTypes;
    nodeTypes.insert(std::make_pair(10, NodeType(10,"mupp",false,boost::chrono::seconds(1),10,retryTimeouts)));
    nodeTypes.insert(std::make_pair(20, NodeType(20,"tupp",true,boost::chrono::seconds(1),22,retryTimeouts)));

    RawPublisherRemoteBasic<::Handler, ::Communication> publisher
        (L"", ioService,communication,nodeTypes,"foo",h,boost::chrono::milliseconds(10));

    h.stopCall = [&]{publisher.Stop();};
    ioService.run();

    BOOST_CHECK(numPerform == 10);
    BOOST_CHECK(numSend10 == 5);
    BOOST_CHECK(numSend20 == 10);
}



BOOST_AUTO_TEST_CASE( callbacks )
{
    numPerform = 0;
    numSend10 = 0;
    numSend20 = 0;
    std::wcout << "testing callbacks" << std::endl;
    Handler h;
    Communication communication;
    boost::asio::io_service ioService;

    std::vector<boost::chrono::steady_clock::duration> retryTimeouts;
    retryTimeouts.push_back(boost::chrono::seconds(1));
    retryTimeouts.push_back(boost::chrono::seconds(2));

    std::map<int64_t, NodeType> nodeTypes;
    nodeTypes.insert(std::make_pair(10, NodeType(10,"mupp",false,boost::chrono::seconds(1),10,retryTimeouts)));
    nodeTypes.insert(std::make_pair(20, NodeType(20,"tupp",true,boost::chrono::seconds(1),22,retryTimeouts)));

    RawPublisherRemoteBasic<::Handler, ::Communication> publisher
        (L"", ioService,communication,nodeTypes,"foo",h,boost::chrono::hours(10));

    h.stopCall = [&]{publisher.Stop();};
    std::shared_ptr<void> cs;

    cb(RawStatistics(), RawChanges(RawChanges::NODES_CHANGED), cs);
    cb(RawStatistics(), RawChanges(RawChanges::NODES_CHANGED), cs);
    cb(RawStatistics(), RawChanges(RawChanges::NODES_CHANGED), cs);
    cb(RawStatistics(), RawChanges(RawChanges::NODES_CHANGED), cs);
    cb(RawStatistics(), RawChanges(RawChanges::NODES_CHANGED), cs);
    cb(RawStatistics(), RawChanges(RawChanges::METADATA_CHANGED), cs);
    cb(RawStatistics(), RawChanges(RawChanges::METADATA_CHANGED), cs);
    cb(RawStatistics(), RawChanges(RawChanges::METADATA_CHANGED), cs);
    cb(RawStatistics(), RawChanges(RawChanges::METADATA_CHANGED), cs);
    cb(RawStatistics(), RawChanges(RawChanges::NODES_CHANGED), cs);
    cb(RawStatistics(), RawChanges(RawChanges::METADATA_CHANGED), cs);

    ioService.run();

    BOOST_CHECK_EQUAL(numPerform, 10);
    BOOST_CHECK_EQUAL(numSend10, 10);
    BOOST_CHECK_EQUAL(numSend20, 10);
}
