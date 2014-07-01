/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safir.sourceforge.net)
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
#include "../src/Coordinator.h"
#include "../src/MessageWrapperCreators.h"
#include <Safir/Utilities/Internal/MakeUnique.h>
#include <boost/thread.hpp>
#include <boost/thread/mutex.hpp>
#include <memory>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4244)
#pragma warning (disable: 4127)
#endif

#include "NodeStatisticsMessage.pb.h"

#ifdef _MSC_VER
#pragma warning (pop)
#endif


#define BOOST_TEST_MODULE CoordinatorHandlerTest
#include <boost/test/unit_test.hpp>


//We need thread safe variants for some of the tests that use multiple threads in the ioService.
boost::mutex testMtx;
#define SAFE_BOOST_CHECK(p) {boost::lock_guard<boost::mutex> lck(testMtx); BOOST_CHECK(p);}
#define SAFE_BOOST_FAIL(s) {boost::lock_guard<boost::mutex> lck(testMtx); BOOST_FAIL(s);}
#define SAFE_BOOST_CHECK_NE(L, R) {boost::lock_guard<boost::mutex> lck(testMtx); BOOST_CHECK_NE(L, R);}
#define SAFE_BOOST_CHECK_EQUAL(L, R) {boost::lock_guard<boost::mutex> lck(testMtx); BOOST_CHECK_EQUAL(L, R);}
#define SAFE_BOOST_TEST_MESSAGE(m) {boost::lock_guard<boost::mutex> lck(testMtx); BOOST_TEST_MESSAGE(m);}

using namespace Safir::Dob::Internal::SP;


RawStatistics GetRawWithOneNode()
{
    auto msg = Safir::make_unique<NodeStatisticsMessage>();

    msg->set_name("myself");
    msg->set_id(1000);
    msg->set_node_type_id(10);
    //    msg->set_election_id(91);

    auto node = msg->add_node_info();
    
    node->set_name("remote1");
    node->set_id(1001);
    node->set_node_type_id(10);
    node->set_is_dead(false);
    node->set_is_long_gone(false);
    
    return RawStatisticsCreator::Create(std::move(msg));
}


typedef std::function<void(const RawStatistics& statistics)> StatisticsCallback;


class CommunicationStub
{
public:
    void ExcludeNode(int64_t nodeId)
    {

    }

};

class RawHandlerStub
{
public:
    void SetElectionId(const int64_t nodeId_, const int64_t electionId_)
    {
        electedId = nodeId_;
        electionId = electionId_;
    }

    void AddNodesChangedCallback(const StatisticsCallback& callback)
    {
        BOOST_CHECK(nodesCb == nullptr);
        nodesCb = callback;
    }

    void AddRawChangedCallback(const StatisticsCallback& callback)
    {
        BOOST_CHECK(rawCb == nullptr);
        rawCb = callback;
    }

    void SetDeadNode(int64_t nodeId)
    {

    }

    StatisticsCallback nodesCb;
    StatisticsCallback rawCb;

    int64_t electedId = 0;
    int64_t electionId = 0;
};


class ElectionHandlerStub
{
public:
    ElectionHandlerStub(boost::asio::io_service& ioService,
                        CommunicationStub& communication,
                        const int64_t id,
                        const std::map<int64_t, NodeType>& nodeTypes,
                        const char* const receiverId,
                        const std::function<void(const int64_t nodeId, 
                                                 const int64_t electionId)>& electionCompleteCallback_)
        : electionCompleteCallback(electionCompleteCallback_)
    {
        lastInstance = this;
    }

    bool IsElected() const
    {
        return false;
    }

    void NodesChanged(RawStatistics statistics)
    {
        nodesChangedCalled = true;
    }

    void Stop()
    {
        stopped = true;
    }

    bool stopped = false;
    bool nodesChangedCalled = false;

    const std::function<void(const int64_t nodeId, 
                             const int64_t electionId)> electionCompleteCallback;


    static ElectionHandlerStub* lastInstance;
};

ElectionHandlerStub* ElectionHandlerStub::lastInstance = nullptr;

struct Fixture
{
    Fixture()
        : coordinator(ioService,
                      comm,
                      "asdf",
                      10,
                      10,
                      "klopp",
                      "flupp",
                      GetNodeTypes(),
                      "snoop",
                      rh)
    {
        SAFE_BOOST_TEST_MESSAGE("setup fixture");
    }
    
    ~Fixture()
    {
        SAFE_BOOST_TEST_MESSAGE("teardown fixture");
    }

    static std::map<int64_t, NodeType> GetNodeTypes()
    {
        std::map<int64_t, NodeType> nodeTypes;
        nodeTypes.insert(std::make_pair(10, NodeType(10,"mupp",false,boost::chrono::milliseconds(1),10,boost::chrono::milliseconds(1))));
        nodeTypes.insert(std::make_pair(20, NodeType(20,"tupp",true,boost::chrono::hours(1),22,boost::chrono::hours(1))));
        return nodeTypes;
    }


    boost::asio::io_service ioService;
    CommunicationStub comm;
    RawHandlerStub rh;

    CoordinatorBasic<CommunicationStub, RawHandlerStub, ElectionHandlerStub> coordinator;
};

BOOST_FIXTURE_TEST_SUITE( s, Fixture )

BOOST_AUTO_TEST_CASE( start_stop )
{
    coordinator.Stop();
    ioService.run();
    BOOST_REQUIRE(ElectionHandlerStub::lastInstance != nullptr);
    BOOST_CHECK(ElectionHandlerStub::lastInstance->stopped);
    BOOST_CHECK(rh.nodesCb != nullptr);
    BOOST_CHECK(rh.rawCb != nullptr);
}

BOOST_AUTO_TEST_CASE( perform_only_own_unelected )
{
    bool callbackCalled = false;
    coordinator.PerformOnStateMessage([&callbackCalled](std::unique_ptr<char []> data, 
                                         const size_t size)
                                      {
                                          callbackCalled = true;
                                      },
                                      0,
                                      true);
    ioService.run();
    BOOST_CHECK(!callbackCalled);
}


BOOST_AUTO_TEST_CASE( perform_no_state_received )
{
    bool callbackCalled = false;
    coordinator.PerformOnStateMessage([&callbackCalled](std::unique_ptr<char []> data, 
                                         const size_t size)
                                      {
                                          callbackCalled = true;
                                      },
                                      0,
                                      false);
    ioService.run();
    BOOST_CHECK(!callbackCalled);
}

BOOST_AUTO_TEST_CASE( election_id_propagation )
{
    BOOST_CHECK_EQUAL(rh.electedId,0);
    BOOST_CHECK_EQUAL(rh.electionId,0);

    ElectionHandlerStub::lastInstance->electionCompleteCallback(111,100);
    ioService.run();
    BOOST_CHECK_EQUAL(rh.electedId,111);
    BOOST_CHECK_EQUAL(rh.electionId,100);
}


BOOST_AUTO_TEST_CASE( nodes_changed_propagation )
{
    BOOST_CHECK(!ElectionHandlerStub::lastInstance->nodesChangedCalled);
    rh.nodesCb(GetRawWithOneNode());
    ioService.run();
    BOOST_CHECK(ElectionHandlerStub::lastInstance->nodesChangedCalled);

}

BOOST_AUTO_TEST_SUITE_END()

