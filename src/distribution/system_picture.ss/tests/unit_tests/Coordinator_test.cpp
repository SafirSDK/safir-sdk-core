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
#include "../../src/Coordinator.h"
#include "../../src/MessageWrapperCreators.h"
#include <Safir/Utilities/Internal/MakeUnique.h>
#include <memory>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4244)
#pragma warning (disable: 4127)
#endif

#include "RawStatisticsMessage.pb.h"

#ifdef _MSC_VER
#pragma warning (pop)
#endif

#define BOOST_TEST_MODULE CoordinatorHandlerTest
#include <boost/test/unit_test.hpp>

using namespace Safir::Dob::Internal::SP;

boost::shared_ptr<void> cs;

RawStatistics GetRawWithOneNode()
{
    auto msg = Safir::make_unique<RawStatisticsMessage>();

    msg->set_name("myself");
    msg->set_id(1000);
    msg->set_node_type_id(10);
    msg->set_election_id(100);

    auto node = msg->add_node_info();

    node->set_name("remote1");
    node->set_id(1001);
    node->set_node_type_id(10);
    node->set_control_address("remote1:control");
    node->set_data_address("remote1:data");
    node->set_is_dead(false);
    node->set_is_long_gone(false);

    return RawStatisticsCreator::Create(std::move(msg));
}


RawStatistics GetRawWithTwoNodes()
{
    auto msg = Safir::make_unique<RawStatisticsMessage>();

    msg->set_name("myself");
    msg->set_id(1000);
    msg->set_node_type_id(10);
    msg->set_election_id(100);

    auto node = msg->add_node_info();

    node->set_name("remote1");
    node->set_id(1001);
    node->set_node_type_id(10);
    node->set_control_address("remote1:control");
    node->set_data_address("remote1:data");
    node->set_is_dead(false);
    node->set_is_long_gone(false);


    node = msg->add_node_info();

    node->set_name("remote2");
    node->set_id(1002);
    node->set_node_type_id(10);
    node->set_control_address("remote2:control");
    node->set_data_address("remote2:data");
    node->set_is_dead(false);
    node->set_is_long_gone(false);

    return RawStatisticsCreator::Create(std::move(msg));
}

RawStatistics GetRawWithOneNodeAndRemoteRaw()
{
    auto msg = Safir::make_unique<RawStatisticsMessage>();

    msg->set_name("myself");
    msg->set_id(1000);
    msg->set_node_type_id(10);
    msg->set_election_id(100);

    auto node = msg->add_node_info();

    node->set_name("remote1");
    node->set_id(1001);
    node->set_node_type_id(10);
    node->set_control_address("remote1:control");
    node->set_data_address("remote1:data");
    node->set_is_dead(false);
    node->set_is_long_gone(false);

    auto remote = node->mutable_remote_statistics();

    remote->set_name("remote1");
    remote->set_id(1001);
    remote->set_node_type_id(10);
    remote->set_election_id(100);

    auto rnode = remote->add_node_info();
    rnode->set_name("myself");
    rnode->set_id(1000);
    rnode->set_node_type_id(10);
    rnode->set_is_dead(false);
    rnode->set_is_long_gone(false);


    return RawStatisticsCreator::Create(std::move(msg));
}

RawStatistics GetRawWithTwoNodesAndRemoteRaw(bool oneDead, bool longGone)
{
    auto msg = Safir::make_unique<RawStatisticsMessage>();

    msg->set_name("myself");
    msg->set_id(1000);
    msg->set_election_id(100);

    //Add node remote1
    auto node = msg->add_node_info();

    node->set_name("remote1");
    node->set_id(1001);
    node->set_is_dead(oneDead && longGone);
    node->set_is_long_gone(oneDead && longGone);

    auto remote = node->mutable_remote_statistics();

    remote->set_name("remote1");
    remote->set_id(1001);
    remote->set_election_id(100);

    auto rnode = remote->add_node_info();
    rnode->set_name("myself");
    rnode->set_id(1000);
    rnode->set_is_dead(false);
    rnode->set_is_long_gone(false);

    rnode = remote->add_node_info();
    rnode->set_name("remote2");
    rnode->set_id(1002);
    rnode->set_is_dead(false);
    rnode->set_is_long_gone(false);

    //add node remote2
    node = msg->add_node_info();

    node->set_name("remote2");
    node->set_id(1002);
    node->set_is_dead(false);
    node->set_is_long_gone(false);

    remote = node->mutable_remote_statistics();

    remote->set_name("remote2");
    remote->set_id(1002);
    remote->set_election_id(100);

    rnode = remote->add_node_info();
    rnode->set_name("myself");
    rnode->set_id(1000);
    rnode->set_is_dead(false);
    rnode->set_is_long_gone(false);

    rnode = remote->add_node_info();
    rnode->set_name("remote1");
    rnode->set_id(1001);
    rnode->set_is_dead(oneDead && !longGone);
    rnode->set_is_long_gone(false);

    return RawStatisticsCreator::Create(std::move(msg));
}


SystemStateMessage GetStateWithOneNode()
{
    SystemStateMessage msg;
    msg.set_elected_id(1001);
    msg.set_election_id(100);

    auto node = msg.add_node_info();
    node->set_name("remote1");
    node->set_id(1001);
    node->set_node_type_id(10);
    node->set_control_address("remote1:control");
    node->set_data_address("remote1:data");
    node->set_is_dead(false);

    node = msg.add_node_info();
    node->set_name("myself");
    node->set_id(1000);
    node->set_node_type_id(10);
    node->set_control_address("klopp");
    node->set_data_address("flupp");
    node->set_is_dead(false);
    return msg;
}

SystemStateMessage GetStateWithTwoNodes()
{
    SystemStateMessage msg;
    msg.set_elected_id(1001);
    msg.set_election_id(100);

    auto node = msg.add_node_info();
    node->set_name("remote1");
    node->set_id(1001);
    node->set_node_type_id(10);
    node->set_control_address("remote1:control");
    node->set_data_address("remote1:data");
    node->set_is_dead(false);

    node = msg.add_node_info();
    node->set_name("myself");
    node->set_id(1000);
    node->set_node_type_id(10);
    node->set_control_address("klopp");
    node->set_data_address("flupp");
    node->set_is_dead(false);

    node = msg.add_node_info();
    node->set_name("remote2");
    node->set_id(1002);
    node->set_node_type_id(10);
    node->set_control_address("remote2:control");
    node->set_data_address("remote2:data");
    node->set_is_dead(true); //!!!!

    return msg;
}


typedef std::function<void(const RawStatistics& statistics,
                           const RawChanges& flags,
                           boost::shared_ptr<void> completionSignaller)> StatisticsCallback;


class CommunicationStub
{
public:
    void ExcludeNode(int64_t nodeId)
    {
        excludedNodes.insert(nodeId);
    }

    std::set<int64_t> excludedNodes;
};

class RawHandlerStub
{
public:
    void SetElectionId(const int64_t nodeId_, const int64_t electionId_)
    {
        electedId = nodeId_;
        electionId = electionId_;
    }

    /*    void AddNodesChangedCallback(const StatisticsCallback& callback)
    {
        BOOST_CHECK(nodesCb == nullptr);
        nodesCb = callback;
        }*/

    void AddRawChangedCallback(const StatisticsCallback& callback)
    {
        BOOST_CHECK(rawCb == nullptr);
        rawCb = callback;
    }

    void SetDeadNode(int64_t nodeId)
    {
        deadNodes.insert(nodeId);
    }

    StatisticsCallback rawCb;

    int64_t electedId = 0;
    int64_t electionId = 0;

    std::set<int64_t> deadNodes;
};


class ElectionHandlerStub
{
public:
    ElectionHandlerStub(boost::asio::io_service& /*ioService*/,
                        CommunicationStub& /*communication*/,
                        const int64_t id_,
                        const std::map<int64_t, NodeType>& /*nodeTypes*/,
                        const char* const /*receiverId*/,
                        const std::function<void(const int64_t nodeId,
                                                 const int64_t electionId)>& electionCompleteCallback_)
        : id(id_)
        , electionCompleteCallback(electionCompleteCallback_)
    {
        lastInstance = this;
    }

    bool IsElected() const
    {
        return electedId == id;
    }

    bool IsElected(int64_t node) const
    {
        return electedId == node;
    }

    void NodesChanged(RawStatistics statistics)
    {
        nodesChangedCalled = true;
    }

    void Stop()
    {
        stopped = true;
    }

    const int64_t id;
    int64_t electedId = 0;

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
                      "myself",
                      1000,
                      10,
                      "klopp",
                      "flupp",
                      GetNodeTypes(),
                      "snoop",
                      rh)
    {
        BOOST_TEST_MESSAGE("setup fixture");
    }

    ~Fixture()
    {
        BOOST_TEST_MESSAGE("teardown fixture");
    }

    static std::map<int64_t, NodeType> GetNodeTypes()
    {
        std::map<int64_t, NodeType> nodeTypes;
        nodeTypes.insert(std::make_pair(10, NodeType(10,
                                                     "mupp",
                                                     false,
                                                     boost::chrono::milliseconds(1),
                                                     10,
                                                     boost::chrono::milliseconds(1))));
        nodeTypes.insert(std::make_pair(20, NodeType(20,
                                                     "tupp",
                                                     true,
                                                     boost::chrono::hours(1),
                                                     22,
                                                     boost::chrono::hours(1))));
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
    BOOST_CHECK(rh.rawCb != nullptr);
}

BOOST_AUTO_TEST_CASE( perform_only_own_unelected )
{
    bool callbackCalled = false;
    coordinator.PerformOnStateMessage([&callbackCalled](std::unique_ptr<char []> /*data*/,
                                                        const size_t /*size*/)
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
    coordinator.PerformOnStateMessage([&callbackCalled](std::unique_ptr<char []> /*data*/,
                                                        const size_t /*size*/)
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
    rh.rawCb(GetRawWithOneNode(),RawChanges(RawChanges::NODES_CHANGED), cs);
    ioService.run();
    BOOST_CHECK(ElectionHandlerStub::lastInstance->nodesChangedCalled);
}

BOOST_AUTO_TEST_CASE( simple_state_production )
{
    ElectionHandlerStub::lastInstance->electedId = 1000;
    ElectionHandlerStub::lastInstance->electionCompleteCallback(1000,100);
    rh.rawCb(GetRawWithOneNode(),RawChanges(RawChanges::NODES_CHANGED),cs);
    bool callbackCalled = false;
    coordinator.PerformOnStateMessage([&callbackCalled](std::unique_ptr<char []> /*data*/,
                                                        const size_t /*size*/)
                                      {
                                          callbackCalled = true;
                                      },
                                      0,
                                      true);

    ioService.run();
    BOOST_CHECK(ElectionHandlerStub::lastInstance->nodesChangedCalled);
    BOOST_CHECK(!callbackCalled);

    rh.rawCb(GetRawWithOneNodeAndRemoteRaw(),RawChanges(RawChanges::NEW_REMOTE_STATISTICS),cs);

    SystemStateMessage stateMessage;
    coordinator.PerformOnStateMessage([&callbackCalled,&stateMessage](std::unique_ptr<char []> data,
                                                                      const size_t size)
                                      {
                                          callbackCalled = true;
                                          stateMessage.ParseFromArray(data.get(),static_cast<int>(size));
                                      },
                                      0,
                                      true);

    ioService.reset();
    ioService.run();
    BOOST_REQUIRE(callbackCalled);
    BOOST_CHECK_EQUAL(stateMessage.elected_id(),1000);
    BOOST_CHECK_EQUAL(stateMessage.election_id(),100);
    BOOST_CHECK_EQUAL(stateMessage.node_info_size(),2);

    BOOST_CHECK_EQUAL(stateMessage.node_info(0).name(),"myself");
    BOOST_CHECK_EQUAL(stateMessage.node_info(0).id(),1000);
    BOOST_CHECK_EQUAL(stateMessage.node_info(0).node_type_id(),10);
    BOOST_CHECK_EQUAL(stateMessage.node_info(0).control_address(),"klopp");
    BOOST_CHECK_EQUAL(stateMessage.node_info(0).data_address(),"flupp");
    BOOST_CHECK(!stateMessage.node_info(0).is_dead());

    BOOST_CHECK_EQUAL(stateMessage.node_info(1).name(),"remote1");
    BOOST_CHECK_EQUAL(stateMessage.node_info(1).id(),1001);
    BOOST_CHECK_EQUAL(stateMessage.node_info(1).node_type_id(),10);
    BOOST_CHECK_EQUAL(stateMessage.node_info(1).control_address(),"remote1:control");
    BOOST_CHECK_EQUAL(stateMessage.node_info(1).data_address(),"remote1:data");
    BOOST_CHECK(!stateMessage.node_info(1).is_dead());

    BOOST_CHECK(comm.excludedNodes.empty());
    BOOST_CHECK(rh.deadNodes.empty());
}


BOOST_AUTO_TEST_CASE( propagate_state_from_other )
{
    ElectionHandlerStub::lastInstance->electedId = 1001;
    ElectionHandlerStub::lastInstance->electionCompleteCallback(1001,100);
    rh.rawCb(GetRawWithOneNode(),RawChanges(RawChanges::NODES_CHANGED),cs);
    auto state = GetStateWithOneNode();

    const size_t size = state.ByteSize();
    auto data = boost::shared_ptr<char[]>(new char[size]);
    state.SerializeWithCachedSizesToArray
        (reinterpret_cast<google::protobuf::uint8*>(data.get()));

    coordinator.NewRemoteStatistics(1001,data,size);
    bool callbackCalled = false;

    coordinator.PerformOnStateMessage([&callbackCalled](std::unique_ptr<char []> /*data*/,
                                                        const size_t /*size*/)
                                      {
                                          callbackCalled = true;
                                      },
                                      0,
                                      true);
    ioService.run();
    BOOST_CHECK(!callbackCalled);


    SystemStateMessage stateMessage;
    coordinator.PerformOnStateMessage([&callbackCalled,&stateMessage](std::unique_ptr<char []> data,
                                                                      const size_t size)
                                      {
                                          callbackCalled = true;
                                          stateMessage.ParseFromArray(data.get(),static_cast<int>(size));
                                      },
                                      0,
                                      false);

    ioService.reset();
    ioService.run();
    BOOST_REQUIRE(callbackCalled);
    BOOST_CHECK_EQUAL(stateMessage.elected_id(),1001);
    BOOST_CHECK_EQUAL(stateMessage.election_id(),100);
    BOOST_CHECK_EQUAL(stateMessage.node_info_size(),2);


    BOOST_CHECK_EQUAL(stateMessage.node_info(0).name(),"remote1");
    BOOST_CHECK_EQUAL(stateMessage.node_info(0).id(),1001);
    BOOST_CHECK_EQUAL(stateMessage.node_info(0).node_type_id(),10);
    BOOST_CHECK_EQUAL(stateMessage.node_info(0).control_address(),"remote1:control");
    BOOST_CHECK_EQUAL(stateMessage.node_info(0).data_address(),"remote1:data");
    BOOST_CHECK(!stateMessage.node_info(0).is_dead());

    BOOST_CHECK_EQUAL(stateMessage.node_info(1).name(),"myself");
    BOOST_CHECK_EQUAL(stateMessage.node_info(1).id(),1000);
    BOOST_CHECK_EQUAL(stateMessage.node_info(1).node_type_id(),10);
    BOOST_CHECK_EQUAL(stateMessage.node_info(1).control_address(),"klopp");
    BOOST_CHECK_EQUAL(stateMessage.node_info(1).data_address(),"flupp");
    BOOST_CHECK(!stateMessage.node_info(1).is_dead());

    BOOST_CHECK(comm.excludedNodes.empty());
    BOOST_CHECK(rh.deadNodes.empty());
}

BOOST_AUTO_TEST_CASE( remote_from_other_with_dead )
{
    ElectionHandlerStub::lastInstance->electedId = 1001;
    ElectionHandlerStub::lastInstance->electionCompleteCallback(1001,100);
    rh.rawCb(GetRawWithTwoNodes(),RawChanges(RawChanges::NODES_CHANGED),cs);
    auto state = GetStateWithTwoNodes();

    const size_t size = state.ByteSize();
    auto data = boost::shared_ptr<char[]>(new char[size]);
    state.SerializeWithCachedSizesToArray
        (reinterpret_cast<google::protobuf::uint8*>(data.get()));

    coordinator.NewRemoteStatistics(1001,data,size);

    ioService.run();

    BOOST_CHECK(rh.deadNodes.size() == 1);
    BOOST_CHECK(rh.deadNodes.find(1002) != rh.deadNodes.end());

    BOOST_CHECK(comm.excludedNodes.size() == 1);
    BOOST_CHECK(comm.excludedNodes.find(1002) != comm.excludedNodes.end());


    bool callbackCalled = false;

    SystemStateMessage stateMessage;
    coordinator.PerformOnStateMessage([&callbackCalled,&stateMessage](std::unique_ptr<char []> data,
                                                                      const size_t size)
                                      {
                                          callbackCalled = true;
                                          stateMessage.ParseFromArray(data.get(),static_cast<int>(size));
                                      },
                                      0,
                                      false);
    ioService.reset();
    ioService.run();

    BOOST_REQUIRE(callbackCalled);

    BOOST_CHECK_EQUAL(stateMessage.elected_id(),1001);
    BOOST_CHECK_EQUAL(stateMessage.election_id(),100);
    BOOST_CHECK_EQUAL(stateMessage.node_info_size(),3);

    BOOST_CHECK_EQUAL(stateMessage.node_info(0).name(),"remote1");
    BOOST_CHECK_EQUAL(stateMessage.node_info(0).id(),1001);
    BOOST_CHECK_EQUAL(stateMessage.node_info(0).node_type_id(),10);
    BOOST_CHECK_EQUAL(stateMessage.node_info(0).control_address(),"remote1:control");
    BOOST_CHECK_EQUAL(stateMessage.node_info(0).data_address(),"remote1:data");
    BOOST_CHECK(!stateMessage.node_info(0).is_dead());

    BOOST_CHECK_EQUAL(stateMessage.node_info(1).name(),"myself");
    BOOST_CHECK_EQUAL(stateMessage.node_info(1).id(),1000);
    BOOST_CHECK_EQUAL(stateMessage.node_info(1).node_type_id(),10);
    BOOST_CHECK_EQUAL(stateMessage.node_info(1).control_address(),"klopp");
    BOOST_CHECK_EQUAL(stateMessage.node_info(1).data_address(),"flupp");
    BOOST_CHECK(!stateMessage.node_info(1).is_dead());

    BOOST_CHECK_EQUAL(stateMessage.node_info(2).name(),"remote2");
    BOOST_CHECK_EQUAL(stateMessage.node_info(2).id(),1002);
    BOOST_CHECK_EQUAL(stateMessage.node_info(2).node_type_id(),10);
    BOOST_CHECK_EQUAL(stateMessage.node_info(2).control_address(),"remote2:control");
    BOOST_CHECK_EQUAL(stateMessage.node_info(2).data_address(),"remote2:data");
    BOOST_CHECK(stateMessage.node_info(2).is_dead());
}

BOOST_AUTO_TEST_CASE( remote_reports_dead )
{
    ElectionHandlerStub::lastInstance->electedId = 1000;
    ElectionHandlerStub::lastInstance->electionCompleteCallback(1000,100);
    rh.rawCb(GetRawWithTwoNodesAndRemoteRaw(false,false),RawChanges(RawChanges::NODES_CHANGED),cs);

    bool callbackCalled = false;
    SystemStateMessage stateMessage;
    coordinator.PerformOnStateMessage([&callbackCalled,&stateMessage](std::unique_ptr<char []> data,
                                                                      const size_t size)
                                      {
                                          callbackCalled = true;
                                          stateMessage.ParseFromArray(data.get(),static_cast<int>(size));
                                      },
                                      0,
                                      true);

    ioService.run();
    BOOST_REQUIRE(callbackCalled);
    BOOST_CHECK_EQUAL(stateMessage.elected_id(),1000);
    BOOST_CHECK_EQUAL(stateMessage.node_info_size(),3);

    BOOST_CHECK_EQUAL(stateMessage.node_info(0).id(),1000);
    BOOST_CHECK(!stateMessage.node_info(0).is_dead());

    BOOST_CHECK_EQUAL(stateMessage.node_info(1).name(),"remote1");
    BOOST_CHECK(!stateMessage.node_info(1).is_dead());

    BOOST_CHECK_EQUAL(stateMessage.node_info(2).name(),"remote2");
    BOOST_CHECK(!stateMessage.node_info(2).is_dead());

    BOOST_CHECK(comm.excludedNodes.empty());
    BOOST_CHECK(rh.deadNodes.empty());


    rh.rawCb(GetRawWithTwoNodesAndRemoteRaw(true,false),RawChanges(RawChanges::NEW_REMOTE_STATISTICS),cs);

    callbackCalled = false;
    coordinator.PerformOnStateMessage([&callbackCalled,&stateMessage](std::unique_ptr<char []> data,
                                                                      const size_t size)
                                      {
                                          callbackCalled = true;
                                          stateMessage.ParseFromArray(data.get(),static_cast<int>(size));
                                      },
                                      0,
                                      true);

    ioService.reset();
    ioService.run();
    BOOST_REQUIRE(callbackCalled);
    BOOST_CHECK_EQUAL(stateMessage.elected_id(),1000);
    BOOST_CHECK_EQUAL(stateMessage.node_info_size(),3);

    BOOST_CHECK_EQUAL(stateMessage.node_info(0).id(),1000);
    BOOST_CHECK(!stateMessage.node_info(0).is_dead());

    BOOST_CHECK_EQUAL(stateMessage.node_info(1).name(),"remote1");
    BOOST_CHECK(stateMessage.node_info(1).is_dead());

    BOOST_CHECK_EQUAL(stateMessage.node_info(2).name(),"remote2");
    BOOST_CHECK(!stateMessage.node_info(2).is_dead());

    BOOST_CHECK(rh.deadNodes.size() == 1);
    BOOST_CHECK(rh.deadNodes.find(1001) != rh.deadNodes.end());

    BOOST_CHECK(comm.excludedNodes.size() == 1);
    BOOST_CHECK(comm.excludedNodes.find(1001) != comm.excludedNodes.end());
}


BOOST_AUTO_TEST_CASE( ignore_long_gone_nodes )
{
    ElectionHandlerStub::lastInstance->electedId = 1000;
    ElectionHandlerStub::lastInstance->electionCompleteCallback(1000,100);

    rh.rawCb(GetRawWithTwoNodesAndRemoteRaw(true,true),
             RawChanges(RawChanges::NODES_CHANGED | RawChanges::NEW_REMOTE_STATISTICS),
             cs);

    bool callbackCalled = false;
    SystemStateMessage stateMessage;
    coordinator.PerformOnStateMessage([&callbackCalled,&stateMessage](std::unique_ptr<char []> data,
                                                                      const size_t size)
                                      {
                                          callbackCalled = true;
                                          stateMessage.ParseFromArray(data.get(),static_cast<int>(size));
                                      },
                                      0,
                                      true);

    ioService.run();
    BOOST_REQUIRE(callbackCalled);
    BOOST_CHECK_EQUAL(stateMessage.elected_id(),1000);
    BOOST_CHECK_EQUAL(stateMessage.node_info_size(),2);

    BOOST_CHECK_EQUAL(stateMessage.node_info(0).id(),1000);
    BOOST_CHECK(!stateMessage.node_info(0).is_dead());

    BOOST_CHECK_EQUAL(stateMessage.node_info(1).name(),"remote2");
    BOOST_CHECK_EQUAL(stateMessage.node_info(1).id(),1002);
    BOOST_CHECK(!stateMessage.node_info(1).is_dead());

    BOOST_CHECK(rh.deadNodes.empty());
    BOOST_CHECK(comm.excludedNodes.empty());
}

BOOST_AUTO_TEST_SUITE_END()