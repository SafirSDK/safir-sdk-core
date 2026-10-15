/******************************************************************************
*
* Copyright Saab AB, 2014, 2026 (http://safirsdkcore.com)
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
#  pragma warning (push)
#  pragma warning (disable: 4244)
#  pragma warning (disable: 4245)
#  pragma warning (disable: 4127)
#  pragma warning (disable: 4701)
#  pragma warning (disable: 4100)
#  pragma warning (disable: 4141)
#  pragma warning (disable: 4267)
#  pragma warning (disable: 4189)
#endif

#include "RawStatisticsMessage.pb.h"

#ifdef _MSC_VER
#  pragma warning (pop)
#endif

#define BOOST_TEST_MODULE CoordinatorHandlerTest
#include <boost/test/unit_test.hpp>

using namespace Safir::Dob::Internal::SP;

std::shared_ptr<void> cs;

RawStatistics GetRawWithOneNode(const std::vector<int64_t>& nodeTypeIds)
{
    BOOST_REQUIRE_EQUAL(nodeTypeIds.size(),2U);
    auto msg = Safir::make_unique<RawStatisticsMessage>();

    msg->set_name("myself");
    msg->set_id(1000);
    msg->set_node_type_id(nodeTypeIds.at(0));
    msg->set_election_id(100);
    msg->set_incarnation_id(1000);
    auto node = msg->add_node_info();

    node->set_name("remote1");
    node->set_id(1001);
    node->set_node_type_id(nodeTypeIds.at(1));
    node->set_control_address("remote1:control");
    node->set_data_address("remote1:data");
    node->set_is_dead(false);

    return RawStatisticsCreator::Create(std::move(msg));
}

RawStatistics GetRawDetachedLightnode()
{
    auto msg = Safir::make_unique<RawStatisticsMessage>();

    msg->set_name("myself");
    msg->set_id(1000);
    msg->set_node_type_id(15);
    msg->set_election_id(100);
    msg->set_incarnation_id(1000);

    return RawStatisticsCreator::Create(std::move(msg));
}


RawStatistics GetRawWithTwoNodes()
{
    auto msg = Safir::make_unique<RawStatisticsMessage>();

    msg->set_name("myself");
    msg->set_id(1000);
    msg->set_node_type_id(10);
    msg->set_election_id(100);
    msg->set_incarnation_id(1000);

    auto node = msg->add_node_info();

    node->set_name("remote1");
    node->set_id(1001);
    node->set_node_type_id(10);
    node->set_control_address("remote1:control");
    node->set_data_address("remote1:data");
    node->set_is_dead(false);


    node = msg->add_node_info();

    node->set_name("remote2");
    node->set_id(1002);
    node->set_node_type_id(10);
    node->set_control_address("remote2:control");
    node->set_data_address("remote2:data");
    node->set_is_dead(false);

    return RawStatisticsCreator::Create(std::move(msg));
}

RawStatistics GetRawWithOneNodeAndRemoteRaw(const std::vector<int64_t>& nodeTypeIds)
{
    BOOST_REQUIRE_EQUAL(nodeTypeIds.size(),2U);
    auto msg = Safir::make_unique<RawStatisticsMessage>();

    msg->set_name("myself");
    msg->set_id(1000);
    msg->set_node_type_id(nodeTypeIds.at(0));
    msg->set_election_id(100);
    msg->set_incarnation_id(1000);

    auto node = msg->add_node_info();

    node->set_name("remote1");
    node->set_id(1001);
    node->set_node_type_id(nodeTypeIds.at(1));
    node->set_control_address("remote1:control");
    node->set_data_address("remote1:data");
    node->set_is_dead(false);

    auto remote = node->mutable_remote_statistics();

    remote->set_name("remote1");
    remote->set_id(1001);
    remote->set_node_type_id(nodeTypeIds.at(1));
    remote->set_election_id(100);

    auto rnode = remote->add_node_info();
    rnode->set_name("myself");
    rnode->set_id(1000);
    rnode->set_node_type_id(nodeTypeIds.at(0));
    rnode->set_is_dead(false);

    return RawStatisticsCreator::Create(std::move(msg));
}


RawStatistics GetRawWithTwoNodesAndRemoteRaw(bool iThinkANodeIsDead, bool remoteThinksANodeIsDead)
{
    auto msg = Safir::make_unique<RawStatisticsMessage>();

    msg->set_name("myself");
    msg->set_id(1000);
    msg->set_node_type_id(10);
    msg->set_election_id(100);
    msg->set_incarnation_id(1000);

    //Add node remote1
    {
        auto node = msg->add_node_info();

        node->set_name("remote1");
        node->set_id(1001);
        node->set_node_type_id(10);
        node->set_is_dead(iThinkANodeIsDead);

        auto remote = node->mutable_remote_statistics();
        remote->set_name("remote1");
        remote->set_id(1001);
        remote->set_node_type_id(10);
        remote->set_election_id(100);

        {
            auto rnode = remote->add_node_info();
            rnode->set_name("myself");
            rnode->set_id(1000);
            rnode->set_node_type_id(10);
            rnode->set_is_dead(false);
        }

        {
            auto rnode = remote->add_node_info();
            rnode->set_name("remote2");
            rnode->set_id(1002);
            rnode->set_node_type_id(10);
            rnode->set_is_dead(false);
        }
    }

    //add node remote2
    {
        auto node = msg->add_node_info();

        node->set_name("remote2");
        node->set_id(1002);
        node->set_node_type_id(10);
        node->set_is_dead(false);

        auto remote = node->mutable_remote_statistics();
        remote->set_name("remote2");
        remote->set_id(1002);
        remote->set_node_type_id(10);
        remote->set_election_id(100);

        {
            auto rnode = remote->add_node_info();
            rnode->set_name("myself");
            rnode->set_id(1000);
            rnode->set_node_type_id(10);
            rnode->set_is_dead(false);
        }

        {
            auto rnode = remote->add_node_info();
            rnode->set_name("remote1");
            rnode->set_id(1001);
            rnode->set_node_type_id(10);
            rnode->set_is_dead(remoteThinksANodeIsDead);
        }
    }

    return RawStatisticsCreator::Create(std::move(msg));
}

RawStatistics GetRawWithTwoLightNodesAndRemoteRaw(bool invalidDeadness)
{
    auto msg = Safir::make_unique<RawStatisticsMessage>();

    msg->set_name("myself");
    msg->set_id(1000);
    msg->set_node_type_id(10);
    msg->set_election_id(100);
    msg->set_incarnation_id(1000);

    //Add node remote1
    {
        auto node = msg->add_node_info();

        node->set_name("remote1");
        node->set_id(1001);
        node->set_node_type_id(15);
        node->set_is_dead(false);

        auto remote = node->mutable_remote_statistics();

        remote->set_name("remote1");
        remote->set_id(1001);
        remote->set_node_type_id(15);
        remote->set_election_id(100);

        {
            auto rnode = remote->add_node_info();
            rnode->set_name("myself");
            rnode->set_id(1000);
            rnode->set_node_type_id(10);
            rnode->set_is_dead(false);
        }

        if (invalidDeadness)
        {
            auto rnode = remote->add_node_info();
            rnode->set_name("remote2");
            rnode->set_id(1002);
            rnode->set_node_type_id(10);
            rnode->set_is_dead(true);
        }
    }

    //add node remote2
    {
        auto node = msg->add_node_info();

        node->set_name("remote2");
        node->set_id(1002);
        node->set_node_type_id(15);
        node->set_is_dead(false);

        auto remote = node->mutable_remote_statistics();

        remote->set_name("remote2");
        remote->set_id(1002);
        remote->set_node_type_id(15);
        remote->set_election_id(100);

        auto rnode = remote->add_node_info();
        rnode->set_name("myself");
        rnode->set_id(1000);
        rnode->set_node_type_id(10);
        rnode->set_is_dead(false);
    }
    return RawStatisticsCreator::Create(std::move(msg));
}

RawStatistics GetRawWithTwoNormalAndTwoLightNodesAndRemoteRaw(bool lightnodesFullyConnected,
                                                              bool normalNodesFullyConnected)
{
    auto msg = Safir::make_unique<RawStatisticsMessage>();

    msg->set_name("myself");
    msg->set_id(1000);
    msg->set_node_type_id(10);
    msg->set_election_id(100);
    msg->set_incarnation_id(1000);

    //Add node remote
    {
        auto node = msg->add_node_info();

        node->set_name("remote");
        node->set_id(1001);
        node->set_node_type_id(10);
        node->set_is_dead(false);

        auto remote = node->mutable_remote_statistics();
        remote->set_name("remote");
        remote->set_id(1001);
        remote->set_node_type_id(10);
        remote->set_election_id(100);

        {
            auto rnode = remote->add_node_info();
            rnode->set_name("myself");
            rnode->set_id(1000);
            rnode->set_node_type_id(10);
            rnode->set_is_dead(false);
        }

        {
            auto rnode = remote->add_node_info();
            rnode->set_name("light1");
            rnode->set_id(1002);
            rnode->set_node_type_id(15);
            rnode->set_is_dead(false);
        }

        if (normalNodesFullyConnected)
        {
            auto rnode = remote->add_node_info();
            rnode->set_name("light2");
            rnode->set_id(1003);
            rnode->set_node_type_id(15);
            rnode->set_is_dead(false);
        }
    }

    //add node light1
    {
        auto node = msg->add_node_info();

        node->set_name("light1");
        node->set_id(1002);
        node->set_node_type_id(15);
        node->set_is_dead(false);

        auto remote = node->mutable_remote_statistics();
        remote->set_name("light1");
        remote->set_id(1002);
        remote->set_node_type_id(15);
        remote->set_election_id(100);

        {
            auto rnode = remote->add_node_info();
            rnode->set_name("myself");
            rnode->set_id(1000);
            rnode->set_node_type_id(10);
            rnode->set_is_dead(false);
        }

        if (lightnodesFullyConnected)
        {
            auto rnode = remote->add_node_info();
            rnode->set_name("remote");
            rnode->set_id(1001);
            rnode->set_node_type_id(10);
            rnode->set_is_dead(false);
        }
    }

    //add node light2
    {
        auto node = msg->add_node_info();

        node->set_name("light2");
        node->set_id(1003);
        node->set_node_type_id(15);
        node->set_is_dead(false);

        auto remote = node->mutable_remote_statistics();
        remote->set_name("light2");
        remote->set_id(1003);
        remote->set_node_type_id(15);
        remote->set_election_id(100);

        {
            auto rnode = remote->add_node_info();
            rnode->set_name("myself");
            rnode->set_id(1000);
            rnode->set_node_type_id(10);
            rnode->set_is_dead(false);
        }

        {
            auto rnode = remote->add_node_info();
            rnode->set_name("remote");
            rnode->set_id(1001);
            rnode->set_node_type_id(10);
            rnode->set_is_dead(false);
        }
    }

    return RawStatisticsCreator::Create(std::move(msg));

}

RawStatistics GetRawWithTwoNodesAndOneRemoteRaw()
{
    auto msg = Safir::make_unique<RawStatisticsMessage>();

    msg->set_name("myself");
    msg->set_id(1000);
    msg->set_node_type_id(10);
    msg->set_election_id(100);
    msg->set_incarnation_id(1000);

    //add node remote2
    {
        auto node = msg->add_node_info();

        node->set_name("remote2");
        node->set_id(1002);
        node->set_node_type_id(10);
        node->set_is_dead(false);

        auto remote = node->mutable_remote_statistics();

        remote->set_name("remote2");
        remote->set_id(1002);
        remote->set_node_type_id(10);
        remote->set_election_id(100);

        auto rnode = remote->add_node_info();
        rnode->set_name("myself");
        rnode->set_id(1000);
        rnode->set_node_type_id(10);
        rnode->set_is_dead(false);
    }

    //Add node remote1
    {
        auto node = msg->add_node_info();

        node->set_name("remote1");
        node->set_id(1001);
        node->set_node_type_id(10);
        node->set_is_dead(false);

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

        rnode = remote->add_node_info();
        rnode->set_name("remote2");
        rnode->set_id(1002);
        rnode->set_node_type_id(10);
        rnode->set_is_dead(false);
    }

    return RawStatisticsCreator::Create(std::move(msg));
}


SystemStateMessage GetStateWithOneNode()
{
    SystemStateMessage msg;
    msg.set_elected_id(1001);
    msg.set_election_id(100);
    msg.set_is_detached(false);

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

SystemStateMessage GetStateWithOneLightnode()
{
    SystemStateMessage msg;
    msg.set_elected_id(1001);
    msg.set_election_id(100);
    msg.set_is_detached(false);

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
    node->set_node_type_id(15);
    node->set_control_address("klopp");
    node->set_data_address("flupp");
    node->set_is_dead(false);
    return msg;
}


//A state from the elected coordinator that does not contain this node yet, which
//is what the first state from a newly elected coordinator looks like before it has
//received our raw data. The Coordinator is supposed to discard such a state.
SystemStateMessage GetStateWithoutSelf()
{
    SystemStateMessage msg;
    msg.set_elected_id(1001);
    msg.set_election_id(100);
    msg.set_is_detached(false);

    auto node = msg.add_node_info();
    node->set_name("remote1");
    node->set_id(1001);
    node->set_node_type_id(10);
    node->set_control_address("remote1:control");
    node->set_data_address("remote1:data");
    node->set_is_dead(false);
    return msg;
}


//An inconsistent state, in which remote2 is listed both as dead and as alive. No
//correct coordinator produces this, but a state can come from another node, so the
//Coordinator has sanity checks for it.
SystemStateMessage GetStateWithANodeBothDeadAndAlive()
{
    SystemStateMessage msg;
    msg.set_elected_id(1001);
    msg.set_election_id(100);
    msg.set_is_detached(false);

    for (const auto& node: {std::make_tuple("remote1", 1001, false),
                            std::make_tuple("myself", 1000, false),
                            std::make_tuple("remote2", 1002, true),
                            std::make_tuple("remote2", 1002, false)})
    {
        auto nodeInfo = msg.add_node_info();
        nodeInfo->set_name(std::get<0>(node));
        nodeInfo->set_id(std::get<1>(node));
        nodeInfo->set_node_type_id(10);
        nodeInfo->set_control_address("control");
        nodeInfo->set_data_address("data");
        nodeInfo->set_is_dead(std::get<2>(node));
    }
    return msg;
}


SystemStateMessage GetStateWithTwoNodes()
{
    SystemStateMessage msg;
    msg.set_elected_id(1001);
    msg.set_election_id(100);
    msg.set_is_detached(false);

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
                           std::shared_ptr<void> completionSignaller)> StatisticsCallback;


class CommunicationStub
{
public:

};

class RawHandlerStub
{
public:

    RawHandlerStub() = default;

    void SetElectionId(const int64_t nodeId_, const int64_t electionId_)
    {
        electedId = nodeId_;
        electionId = electionId_;
    }

    void FormSystem(const int64_t /*incarnationId_*/)
    {

    }

    void AddRawChangedCallback(const StatisticsCallback& callback)
    {
        BOOST_CHECK(rawCb == NULL);
        rawCb = callback;
    }

    void ExcludeNode(int64_t nodeId)
    {
        deadNodes.insert(nodeId);
    }

    void ResurrectNode(int64_t nodeId)
    {
        deadNodes.erase(nodeId);
    }

    void SetNodeIsDetached()
    {

    }

    StatisticsCallback rawCb;

    int64_t electedId = 0;
    int64_t electionId = 0;
    std::set<int64_t> deadNodes;
};


class ElectionHandlerStub
{
public:
    ElectionHandlerStub(const std::wstring& /*logPrefix*/,
                        boost::asio::io_context::strand& /*strand*/,
                        CommunicationStub& /*communication*/,
                        const int64_t id_,
                        const int64_t /*nodeTypeId_*/,
                        const std::map<int64_t, NodeType>& /*nodeTypes*/,
                        const std::chrono::steady_clock::duration& /*aloneTimeout*/,
                        const char* const /*receiverId*/,
                        const std::function<void(const int64_t nodeId,
                                                 const int64_t electionId)>& electionCompleteCallback_,
                        const std::function<void(const int64_t incarnationId)>& /*setIncarnationIdCallback_*/)
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

    bool IsElectionDetached() const
    {
        return isDetached;
    }

    bool IsLightNode() const
    {
        return isLightNode;
    }

    void NodesChanged(RawStatistics /*statistics*/, std::shared_ptr<void> /*completionSignaller*/)
    {
        nodesChangedCalled = true;
    }

    void ForceElection()
    {
        ++forceElectionCount;
    }

    void Stop()
    {
        stopped = true;
    }

    const int64_t id;
    int64_t electedId = 0;
    bool isDetached = false;
    bool isLightNode = false;
    bool stopped = false;
    bool nodesChangedCalled = false;
    int forceElectionCount = 0;

    const std::function<void(const int64_t nodeId,
                             const int64_t electionId)> electionCompleteCallback;


    static ElectionHandlerStub* lastInstance;
};

ElectionHandlerStub* ElectionHandlerStub::lastInstance = nullptr;

template <int64_t ownNodeType>
struct Fixture
{
    Fixture()
        : strand(ioContext)
        , coordinator(L"",
                      strand,
                      comm,
                      "myself",
                      1000,
                      ownNodeType,
                      "klopp",
                      "flupp",
                      GetNodeTypes(),
                      std::chrono::seconds(0),
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
        std::vector<std::chrono::steady_clock::duration> retryTimeouts;
        retryTimeouts.push_back(std::chrono::milliseconds(1));
        retryTimeouts.push_back(std::chrono::milliseconds(2));

        std::map<int64_t, NodeType> nodeTypes;
        nodeTypes.insert(std::make_pair(10, NodeType(10,
                                                     "mupp",
                                                     false,
                                                     std::chrono::milliseconds(1),
                                                     10,
                                                     retryTimeouts)));

        nodeTypes.insert(std::make_pair(15, NodeType(15,
                                                     "lupp",
                                                     true,
                                                     std::chrono::milliseconds(1),
                                                     15,
                                                     retryTimeouts)));

        retryTimeouts.clear();
        retryTimeouts.push_back(std::chrono::hours(1));
        retryTimeouts.push_back(std::chrono::hours(2));

        nodeTypes.insert(std::make_pair(20, NodeType(20,
                                                     "tupp",
                                                     true,
                                                     std::chrono::hours(1),
                                                     22,
                                                     retryTimeouts)));
        return nodeTypes;
    }


    boost::asio::io_context ioContext;
    boost::asio::io_context::strand strand;
    CommunicationStub comm;
    RawHandlerStub rh;

    CoordinatorBasic<CommunicationStub, RawHandlerStub, ElectionHandlerStub> coordinator;
};

BOOST_FIXTURE_TEST_SUITE( self_is_normal, Fixture<10> )

BOOST_AUTO_TEST_CASE( start_stop )
{
    coordinator.Stop();
    ioContext.run();
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
                                      true);
    ioContext.run();
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
                                      false);
    ioContext.run();
    BOOST_CHECK(!callbackCalled);
}

BOOST_AUTO_TEST_CASE( election_id_propagation )
{
    BOOST_CHECK_EQUAL(rh.electedId,0);
    BOOST_CHECK_EQUAL(rh.electionId,0);

    ElectionHandlerStub::lastInstance->electionCompleteCallback(111,100);
    ioContext.run();
    BOOST_CHECK_EQUAL(rh.electedId,111);
    BOOST_CHECK_EQUAL(rh.electionId,100);
}


BOOST_AUTO_TEST_CASE( nodes_changed_propagation )
{
    BOOST_CHECK(!ElectionHandlerStub::lastInstance->nodesChangedCalled);
    rh.rawCb(GetRawWithOneNode({10,10}),RawChanges(RawChanges::NODES_CHANGED), cs);
    ioContext.run();
    BOOST_CHECK(ElectionHandlerStub::lastInstance->nodesChangedCalled);
}

BOOST_AUTO_TEST_CASE( simple_state_production )
{
    ElectionHandlerStub::lastInstance->electedId = 1000;
    ElectionHandlerStub::lastInstance->electionCompleteCallback(1000,100);
    rh.rawCb(GetRawWithOneNode({10,10}),RawChanges(RawChanges::NODES_CHANGED),cs);
    bool callbackCalled = false;
    coordinator.PerformOnStateMessage([&callbackCalled](std::unique_ptr<char []> /*data*/,
                                                        const size_t /*size*/)
                                      {
                                          callbackCalled = true;
                                      },
                                      true);

    ioContext.run();
    BOOST_CHECK(ElectionHandlerStub::lastInstance->nodesChangedCalled);
    BOOST_CHECK(!callbackCalled);

    rh.rawCb(GetRawWithOneNodeAndRemoteRaw({10,10}),RawChanges(RawChanges::NEW_REMOTE_STATISTICS),cs);

    SystemStateMessage stateMessage;
    coordinator.PerformOnStateMessage([&callbackCalled,&stateMessage](std::unique_ptr<char []> data,
                                                                      const size_t size)
                                      {
                                          callbackCalled = true;
                                          stateMessage.ParseFromArray(data.get(),static_cast<int>(size));
                                      },
                                      true);

    ioContext.restart();
    ioContext.run();
    BOOST_REQUIRE(callbackCalled);
    BOOST_CHECK_EQUAL(stateMessage.elected_id(),1000);
    BOOST_CHECK_EQUAL(stateMessage.election_id(),100);
    BOOST_REQUIRE_EQUAL(stateMessage.node_info_size(),2);

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

    BOOST_CHECK(rh.deadNodes.empty());
}

BOOST_AUTO_TEST_CASE( simple_state_production_other_is_light )
{
    ElectionHandlerStub::lastInstance->electedId = 1000;
    ElectionHandlerStub::lastInstance->electionCompleteCallback(1000,100);
    rh.rawCb(GetRawWithOneNode({10,15}),RawChanges(RawChanges::NODES_CHANGED),cs);
    bool callbackCalled = false;
    coordinator.PerformOnStateMessage([&callbackCalled](std::unique_ptr<char []> /*data*/,
                                                        const size_t /*size*/)
                                      {
                                          callbackCalled = true;
                                      },
                                      true);

    ioContext.run();
    BOOST_CHECK(ElectionHandlerStub::lastInstance->nodesChangedCalled);
    BOOST_CHECK(!callbackCalled);

    rh.rawCb(GetRawWithOneNodeAndRemoteRaw({10,15}),RawChanges(RawChanges::NEW_REMOTE_STATISTICS),cs);

    SystemStateMessage stateMessage;
    coordinator.PerformOnStateMessage([&callbackCalled,&stateMessage](std::unique_ptr<char []> data,
                                                                      const size_t size)
                                      {
                                          callbackCalled = true;
                                          stateMessage.ParseFromArray(data.get(),static_cast<int>(size));
                                      },
                                      true);

    ioContext.restart();
    ioContext.run();
    BOOST_REQUIRE(callbackCalled);
    BOOST_CHECK_EQUAL(stateMessage.elected_id(),1000);
    BOOST_CHECK_EQUAL(stateMessage.election_id(),100);
    BOOST_REQUIRE_EQUAL(stateMessage.node_info_size(),2);

    BOOST_CHECK_EQUAL(stateMessage.node_info(0).name(),"myself");
    BOOST_CHECK_EQUAL(stateMessage.node_info(0).id(),1000);
    BOOST_CHECK_EQUAL(stateMessage.node_info(0).node_type_id(),10);
    BOOST_CHECK_EQUAL(stateMessage.node_info(0).control_address(),"klopp");
    BOOST_CHECK_EQUAL(stateMessage.node_info(0).data_address(),"flupp");
    BOOST_CHECK(!stateMessage.node_info(0).is_dead());

    BOOST_CHECK_EQUAL(stateMessage.node_info(1).name(),"remote1");
    BOOST_CHECK_EQUAL(stateMessage.node_info(1).id(),1001);
    BOOST_CHECK_EQUAL(stateMessage.node_info(1).node_type_id(),15);
    BOOST_CHECK_EQUAL(stateMessage.node_info(1).control_address(),"remote1:control");
    BOOST_CHECK_EQUAL(stateMessage.node_info(1).data_address(),"remote1:data");
    BOOST_CHECK(!stateMessage.node_info(1).is_dead());

    BOOST_CHECK(rh.deadNodes.empty());
}



BOOST_AUTO_TEST_CASE( propagate_state_from_other )
{
    ElectionHandlerStub::lastInstance->electedId = 1001;
    ElectionHandlerStub::lastInstance->electionCompleteCallback(1001,100);
    rh.rawCb(GetRawWithOneNode({10,10}),RawChanges(RawChanges::NODES_CHANGED),cs);
    auto state = GetStateWithOneNode();

    const size_t size = state.ByteSizeLong();
    auto data = Safir::Utilities::Internal::SharedCharArray(new char[size]);
    state.SerializeWithCachedSizesToArray
        (reinterpret_cast<google::protobuf::uint8*>(data.get()));

    coordinator.NewRemoteStatistics(1001,data,size);
    bool callbackCalled = false;

    coordinator.PerformOnStateMessage([&callbackCalled](std::unique_ptr<char []> /*data*/,
                                                        const size_t /*size*/)
                                      {
                                          callbackCalled = true;
                                      },
                                      true);
    ioContext.run();
    BOOST_CHECK(!callbackCalled);


    SystemStateMessage stateMessage;
    coordinator.PerformOnStateMessage([&callbackCalled,&stateMessage](std::unique_ptr<char []> data,
                                                                      const size_t size)
                                      {
                                          callbackCalled = true;
                                          stateMessage.ParseFromArray(data.get(),static_cast<int>(size));
                                      },
                                      false);

    ioContext.restart();
    ioContext.run();
    BOOST_REQUIRE(callbackCalled);
    BOOST_CHECK_EQUAL(stateMessage.elected_id(),1001);
    BOOST_CHECK_EQUAL(stateMessage.election_id(),100);
    BOOST_REQUIRE_EQUAL(stateMessage.node_info_size(),2);

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

    BOOST_CHECK(rh.deadNodes.empty());
}

BOOST_AUTO_TEST_CASE( remote_from_other_with_dead )
{
    ElectionHandlerStub::lastInstance->electedId = 1001;
    ElectionHandlerStub::lastInstance->electionCompleteCallback(1001,100);
    rh.rawCb(GetRawWithTwoNodes(),RawChanges(RawChanges::NODES_CHANGED),cs);
    auto state = GetStateWithTwoNodes();

    const size_t size = state.ByteSizeLong();
    auto data = Safir::Utilities::Internal::SharedCharArray(new char[size]);
    state.SerializeWithCachedSizesToArray
        (reinterpret_cast<google::protobuf::uint8*>(data.get()));

    coordinator.NewRemoteStatistics(1001,data,size);

    ioContext.run();

    BOOST_CHECK(rh.deadNodes.size() == 1);
    BOOST_CHECK(rh.deadNodes.find(1002) != rh.deadNodes.end());

    bool callbackCalled = false;

    SystemStateMessage stateMessage;
    coordinator.PerformOnStateMessage([&callbackCalled,&stateMessage](std::unique_ptr<char []> data,
                                                                      const size_t size)
                                      {
                                          callbackCalled = true;
                                          stateMessage.ParseFromArray(data.get(),static_cast<int>(size));
                                      },
                                      false);
    ioContext.restart();
    ioContext.run();

    BOOST_REQUIRE(callbackCalled);

    BOOST_CHECK_EQUAL(stateMessage.elected_id(),1001);
    BOOST_CHECK_EQUAL(stateMessage.election_id(),100);
    BOOST_REQUIRE_EQUAL(stateMessage.node_info_size(),3);

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
                                      true);

    ioContext.run();
    BOOST_REQUIRE(callbackCalled);
    BOOST_CHECK_EQUAL(stateMessage.elected_id(),1000);
    BOOST_REQUIRE_EQUAL(stateMessage.node_info_size(),3);

    BOOST_CHECK_EQUAL(stateMessage.node_info(0).id(),1000);
    BOOST_CHECK(!stateMessage.node_info(0).is_dead());

    BOOST_CHECK_EQUAL(stateMessage.node_info(1).name(),"remote1");
    BOOST_CHECK(!stateMessage.node_info(1).is_dead());

    BOOST_CHECK_EQUAL(stateMessage.node_info(2).name(),"remote2");
    BOOST_CHECK(!stateMessage.node_info(2).is_dead());

    BOOST_CHECK(rh.deadNodes.empty());


    rh.rawCb(GetRawWithTwoNodesAndRemoteRaw(false,true),RawChanges(RawChanges::NEW_REMOTE_STATISTICS),cs);

    callbackCalled = false;
    coordinator.PerformOnStateMessage([&callbackCalled,&stateMessage](std::unique_ptr<char []> data,
                                                                      const size_t size)
                                      {
                                          callbackCalled = true;
                                          stateMessage.ParseFromArray(data.get(),static_cast<int>(size));
                                      },
                                      true);

    ioContext.restart();
    ioContext.run();
    BOOST_REQUIRE(!callbackCalled);
    BOOST_CHECK_EQUAL(stateMessage.elected_id(),1000);
    BOOST_REQUIRE_EQUAL(stateMessage.node_info_size(),3);

    BOOST_CHECK_EQUAL(stateMessage.node_info(0).id(),1000);
    BOOST_CHECK(!stateMessage.node_info(0).is_dead());

    BOOST_CHECK_EQUAL(stateMessage.node_info(1).name(),"remote1");
    BOOST_CHECK(!stateMessage.node_info(1).is_dead());

    BOOST_CHECK_EQUAL(stateMessage.node_info(2).name(),"remote2");
    BOOST_CHECK(!stateMessage.node_info(2).is_dead());
    BOOST_CHECK(rh.deadNodes.size() == 1);
    BOOST_CHECK(rh.deadNodes.find(1001) != rh.deadNodes.end());

    rh.rawCb(GetRawWithTwoNodesAndRemoteRaw(true,true),RawChanges(RawChanges::NEW_REMOTE_STATISTICS),cs);

    callbackCalled = false;
    coordinator.PerformOnStateMessage([&callbackCalled,&stateMessage](std::unique_ptr<char []> data,
                                                                      const size_t size)
                                      {
                                          callbackCalled = true;
                                          stateMessage.ParseFromArray(data.get(),static_cast<int>(size));
                                      },
                                      true);

    ioContext.restart();
    ioContext.run();

    BOOST_REQUIRE(callbackCalled);
    BOOST_CHECK_EQUAL(stateMessage.elected_id(),1000);
    BOOST_REQUIRE_EQUAL(stateMessage.node_info_size(),3);

    BOOST_CHECK_EQUAL(stateMessage.node_info(0).id(),1000);
    BOOST_CHECK(!stateMessage.node_info(0).is_dead());

    BOOST_CHECK_EQUAL(stateMessage.node_info(1).name(),"remote1");
    BOOST_CHECK(stateMessage.node_info(1).is_dead());

    BOOST_CHECK_EQUAL(stateMessage.node_info(2).name(),"remote2");
    BOOST_CHECK(!stateMessage.node_info(2).is_dead());

    BOOST_CHECK(rh.deadNodes.size() == 1);
    BOOST_CHECK(rh.deadNodes.find(1001) != rh.deadNodes.end());
}

/* Not sure what this test tests at the moment.
 * It used to test the long_gone flag in RAW data, but that flag has been removed.
 */
BOOST_AUTO_TEST_CASE( ignore_long_gone_flag )
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
                                      true);

    ioContext.run();
    BOOST_REQUIRE(callbackCalled);
    BOOST_CHECK_EQUAL(stateMessage.elected_id(),1000);
    BOOST_REQUIRE_EQUAL(stateMessage.node_info_size(),3);

    BOOST_CHECK_EQUAL(stateMessage.node_info(0).id(),1000);
    BOOST_CHECK(!stateMessage.node_info(0).is_dead());

    BOOST_CHECK_EQUAL(stateMessage.node_info(1).name(),"remote2");
    BOOST_CHECK_EQUAL(stateMessage.node_info(1).id(),1002);
    BOOST_CHECK(!stateMessage.node_info(1).is_dead());

    BOOST_CHECK_EQUAL(stateMessage.node_info(2).name(),"remote1");
    BOOST_CHECK_EQUAL(stateMessage.node_info(2).id(),1001);
    BOOST_CHECK(stateMessage.node_info(2).is_dead());

    BOOST_CHECK(rh.deadNodes.empty());
}

/* This tc checks that a new remote node cannot make the coordinator produce a state
   that is inconsistent with previous state*/
BOOST_AUTO_TEST_CASE( state_sequence_consistent )
{
    ElectionHandlerStub::lastInstance->electedId = 1000;
    ElectionHandlerStub::lastInstance->electionCompleteCallback(1000,100);
    rh.rawCb(GetRawWithOneNodeAndRemoteRaw({10,10}),RawChanges(RawChanges::NODES_CHANGED),cs);
    bool callbackCalled = false;
    SystemStateMessage stateMessage;
    coordinator.PerformOnStateMessage([&callbackCalled,&stateMessage](std::unique_ptr<char []> data,
                                                                      const size_t size)
                                      {
                                          callbackCalled = true;
                                          stateMessage.ParseFromArray(data.get(),static_cast<int>(size));
                                      },
                                      true);

    ioContext.run();
    BOOST_REQUIRE(callbackCalled);
    callbackCalled = false;

    BOOST_REQUIRE_EQUAL(stateMessage.node_info_size(),2);
    BOOST_CHECK_EQUAL(stateMessage.node_info(0).id(),1000);
    BOOST_CHECK(!stateMessage.node_info(0).is_dead());
    BOOST_CHECK_EQUAL(stateMessage.node_info(1).id(),1001);
    BOOST_CHECK(!stateMessage.node_info(1).is_dead());

    rh.rawCb(GetRawWithTwoNodesAndOneRemoteRaw(),
             RawChanges(RawChanges::NEW_REMOTE_STATISTICS|RawChanges::NODES_CHANGED),cs);

    coordinator.PerformOnStateMessage([&callbackCalled,&stateMessage](std::unique_ptr<char []> data,
                                                                      const size_t size)
                                      {
                                          callbackCalled = true;
                                          stateMessage.ParseFromArray(data.get(),static_cast<int>(size));
                                      },
                                      true);

    ioContext.restart();
    ioContext.run();
    BOOST_REQUIRE(callbackCalled);
    BOOST_REQUIRE_EQUAL(stateMessage.node_info_size(),2);
    BOOST_CHECK_EQUAL(stateMessage.node_info(0).id(),1000);
    BOOST_CHECK(!stateMessage.node_info(0).is_dead());
    BOOST_CHECK_EQUAL(stateMessage.node_info(1).id(),1001);
    BOOST_CHECK(!stateMessage.node_info(1).is_dead());

    BOOST_CHECK(rh.deadNodes.empty());
}

BOOST_AUTO_TEST_CASE( other_node_claims_to_be_detached )
{
    ElectionHandlerStub::lastInstance->electedId = 1001;
    ElectionHandlerStub::lastInstance->electionCompleteCallback(1001,100);
    rh.rawCb(GetRawWithOneNode({10,11}),RawChanges(RawChanges::NODES_CHANGED),cs);
    auto state = GetStateWithOneNode();
    state.set_is_detached(true);

    const size_t size = state.ByteSizeLong();
    auto data = Safir::Utilities::Internal::SharedCharArray(new char[size]);
    state.SerializeWithCachedSizesToArray
        (reinterpret_cast<google::protobuf::uint8*>(data.get()));

    coordinator.NewRemoteStatistics(1001,data,size);
    bool callbackCalled = false;

    coordinator.PerformOnStateMessage([&callbackCalled](std::unique_ptr<char []> /*data*/,
                                                        const size_t /*size*/)
                                      {
                                          callbackCalled = true;
                                      },
                                      true);
    BOOST_CHECK_THROW(ioContext.run(), std::logic_error);
    BOOST_CHECK(!callbackCalled);
}


BOOST_AUTO_TEST_CASE( normal_and_two_lightnodes )
{
    //check that two lightnodes that cannot see each other doesn't make the coordinator think anyone is dead
    ElectionHandlerStub::lastInstance->electedId = 1000;
    ElectionHandlerStub::lastInstance->electionCompleteCallback(1000,100);
    rh.rawCb(GetRawWithTwoLightNodesAndRemoteRaw(false),RawChanges(RawChanges::NODES_CHANGED),cs);

    bool callbackCalled = false;
    SystemStateMessage stateMessage;
    coordinator.PerformOnStateMessage([&callbackCalled,&stateMessage](std::unique_ptr<char []> data,
                                                                      const size_t size)
                                      {
                                          callbackCalled = true;
                                          stateMessage.ParseFromArray(data.get(),static_cast<int>(size));
                                      },
                                      true);

    ioContext.run();
    BOOST_REQUIRE(callbackCalled);
    BOOST_CHECK_EQUAL(stateMessage.elected_id(),1000);
    BOOST_CHECK(!stateMessage.is_detached());
    BOOST_REQUIRE_EQUAL(stateMessage.node_info_size(),3);

    BOOST_CHECK_EQUAL(stateMessage.node_info(0).id(),1000);
    BOOST_CHECK(!stateMessage.node_info(0).is_dead());

    BOOST_CHECK_EQUAL(stateMessage.node_info(1).name(),"remote1");
    BOOST_CHECK(!stateMessage.node_info(1).is_dead());

    BOOST_CHECK_EQUAL(stateMessage.node_info(2).name(),"remote2");
    BOOST_CHECK(!stateMessage.node_info(2).is_dead());

    BOOST_CHECK(rh.deadNodes.empty());

}

//A node that appears in the last state as both dead and alive is an inconsistency
//that the Coordinator is supposed to catch. The exemption for nodes we're busy
//resurrecting must not turn into an exemption for everything else.
BOOST_AUTO_TEST_CASE( node_both_dead_and_alive_in_last_state )
{
    //take in the inconsistent state from the elected node 1001
    ElectionHandlerStub::lastInstance->electedId = 1001;
    ElectionHandlerStub::lastInstance->electionCompleteCallback(1001,100);
    rh.rawCb(GetRawWithOneNodeAndRemoteRaw({10,10}),RawChanges(RawChanges::NODES_CHANGED),cs);

    auto state = GetStateWithANodeBothDeadAndAlive();
    const size_t size = state.ByteSizeLong();
    auto data = Safir::Utilities::Internal::SharedCharArray(new char[size]);
    state.SerializeWithCachedSizesToArray
        (reinterpret_cast<google::protobuf::uint8*>(data.get()));
    coordinator.NewRemoteStatistics(1001,data,size);
    ioContext.run();

    //now get ourselves elected, so that we start producing states from that one
    ElectionHandlerStub::lastInstance->electedId = 1000;
    ElectionHandlerStub::lastInstance->electionCompleteCallback(1000,100);
    rh.rawCb(GetRawWithOneNodeAndRemoteRaw({10,10}),RawChanges(RawChanges::NEW_REMOTE_STATISTICS),cs);

    ioContext.restart();
    BOOST_CHECK_EXCEPTION(ioContext.run(), std::logic_error,
                          [](const std::logic_error& e)
                          {
                              return std::string(e.what()) ==
                                  "Live node was already defined as dead in last state!";
                          });
}

BOOST_AUTO_TEST_CASE( normal_and_two_lightnodes_invalid_dead )
{
    //check that a lightnode cannot mark another lightnode as dead
    //check that two lightnodes that cannot see each other doesn't make the coordinator think anyone is dead
    ElectionHandlerStub::lastInstance->electedId = 1000;
    ElectionHandlerStub::lastInstance->electionCompleteCallback(1000,100);
    rh.rawCb(GetRawWithTwoLightNodesAndRemoteRaw(false),RawChanges(RawChanges::NODES_CHANGED),cs);

    {
        bool callbackCalled = false;
        SystemStateMessage stateMessage;
        coordinator.PerformOnStateMessage([&callbackCalled,&stateMessage](std::unique_ptr<char []> data,
                                                                          const size_t size)
                                          {
                                              callbackCalled = true;
                                              stateMessage.ParseFromArray(data.get(),static_cast<int>(size));
                                          },
                                          true);

        ioContext.run();
        BOOST_REQUIRE(callbackCalled);
        BOOST_CHECK_EQUAL(stateMessage.elected_id(),1000);
        BOOST_CHECK(!stateMessage.is_detached());
        BOOST_REQUIRE_EQUAL(stateMessage.node_info_size(),3);

        BOOST_CHECK_EQUAL(stateMessage.node_info(0).id(),1000);
        BOOST_CHECK(!stateMessage.node_info(0).is_dead());

        BOOST_CHECK_EQUAL(stateMessage.node_info(1).name(),"remote1");
        BOOST_CHECK(!stateMessage.node_info(1).is_dead());

        BOOST_CHECK_EQUAL(stateMessage.node_info(2).name(),"remote2");
        BOOST_CHECK(!stateMessage.node_info(2).is_dead());

        BOOST_CHECK(rh.deadNodes.empty());
    }

    //send an invalid raw
    rh.rawCb(GetRawWithTwoLightNodesAndRemoteRaw(true),RawChanges(RawChanges::NODES_CHANGED),cs);
    {
        bool callbackCalled = false;
        SystemStateMessage stateMessage;
        coordinator.PerformOnStateMessage([&callbackCalled,&stateMessage](std::unique_ptr<char []> data,
                                                                          const size_t size)
                                          {
                                              callbackCalled = true;
                                              stateMessage.ParseFromArray(data.get(),static_cast<int>(size));
                                          },
                                          true);
        ioContext.restart();
        ioContext.run();
        BOOST_REQUIRE(callbackCalled);
        BOOST_CHECK_EQUAL(stateMessage.elected_id(),1000);
        BOOST_CHECK(!stateMessage.is_detached());
        BOOST_REQUIRE_EQUAL(stateMessage.node_info_size(),3);

        BOOST_CHECK_EQUAL(stateMessage.node_info(0).id(),1000);
        BOOST_CHECK(!stateMessage.node_info(0).is_dead());

        BOOST_CHECK_EQUAL(stateMessage.node_info(1).name(),"remote1");
        BOOST_CHECK(!stateMessage.node_info(1).is_dead());

        BOOST_CHECK_EQUAL(stateMessage.node_info(2).name(),"remote2");
        BOOST_CHECK(!stateMessage.node_info(2).is_dead());

        BOOST_CHECK(rh.deadNodes.empty());
    }

}

BOOST_AUTO_TEST_CASE( two_normal_nodes_and_two_lightnodes_with_partial_connectivity )
{
    //myself: sees everything
    //remote: sees myself,light1
    //light1: sees myself
    //light2: sees myself, remote
    ElectionHandlerStub::lastInstance->electedId = 1000;
    ElectionHandlerStub::lastInstance->electionCompleteCallback(1000,100);
    rh.rawCb(GetRawWithTwoNormalAndTwoLightNodesAndRemoteRaw(false,false),RawChanges(RawChanges::NODES_CHANGED),cs);

    {
        bool callbackCalled = false;
        SystemStateMessage stateMessage;
        coordinator.PerformOnStateMessage([&callbackCalled,&stateMessage](std::unique_ptr<char []> data,
                                                                          const size_t size)
                                          {
                                              callbackCalled = true;
                                              stateMessage.ParseFromArray(data.get(),static_cast<int>(size));
                                          },
                                          true);

        ioContext.run();
        BOOST_REQUIRE(callbackCalled);
        BOOST_CHECK_EQUAL(stateMessage.elected_id(),1000);
        BOOST_CHECK(!stateMessage.is_detached());
        BOOST_REQUIRE_EQUAL(stateMessage.node_info_size(),3);

        BOOST_CHECK_EQUAL(stateMessage.node_info(0).id(),1000);
        BOOST_CHECK(!stateMessage.node_info(0).is_dead());

        BOOST_CHECK_EQUAL(stateMessage.node_info(1).name(),"remote");
        BOOST_CHECK(!stateMessage.node_info(1).is_dead());

        BOOST_CHECK_EQUAL(stateMessage.node_info(2).name(),"light1");
        BOOST_CHECK(!stateMessage.node_info(2).is_dead());

        BOOST_CHECK(rh.deadNodes.empty());
    }

    //myself: sees everything
    //remote: sees myself,light1, light2
    //light1: sees myself
    //light2: sees myself, remote
    rh.rawCb(GetRawWithTwoNormalAndTwoLightNodesAndRemoteRaw(false,true),RawChanges(RawChanges::NODES_CHANGED),cs);

    {
        bool callbackCalled = false;
        SystemStateMessage stateMessage;
        coordinator.PerformOnStateMessage([&callbackCalled,&stateMessage](std::unique_ptr<char []> data,
                                                                          const size_t size)
                                          {
                                              callbackCalled = true;
                                              stateMessage.ParseFromArray(data.get(),static_cast<int>(size));
                                          },
                                          true);
        ioContext.restart();
        ioContext.run();
        BOOST_REQUIRE(callbackCalled);
        BOOST_CHECK_EQUAL(stateMessage.elected_id(),1000);
        BOOST_CHECK(!stateMessage.is_detached());
        BOOST_REQUIRE_EQUAL(stateMessage.node_info_size(),4);

        BOOST_CHECK_EQUAL(stateMessage.node_info(0).id(),1000);
        BOOST_CHECK(!stateMessage.node_info(0).is_dead());

        BOOST_CHECK_EQUAL(stateMessage.node_info(1).name(),"remote");
        BOOST_CHECK(!stateMessage.node_info(1).is_dead());

        BOOST_CHECK_EQUAL(stateMessage.node_info(2).name(),"light1");
        BOOST_CHECK(!stateMessage.node_info(2).is_dead());

        BOOST_CHECK_EQUAL(stateMessage.node_info(3).name(),"light2");
        BOOST_CHECK(!stateMessage.node_info(3).is_dead());

        BOOST_CHECK(rh.deadNodes.empty());

    }

    //myself: sees everything
    //remote: sees myself,light1, light2
    //light1: sees myself, remote
    //light2: sees myself, remote
    rh.rawCb(GetRawWithTwoNormalAndTwoLightNodesAndRemoteRaw(true,true),RawChanges(RawChanges::NODES_CHANGED),cs);

    {
        bool callbackCalled = false;
        SystemStateMessage stateMessage;
        coordinator.PerformOnStateMessage([&callbackCalled,&stateMessage](std::unique_ptr<char []> data,
                                                                          const size_t size)
                                          {
                                              callbackCalled = true;
                                              stateMessage.ParseFromArray(data.get(),static_cast<int>(size));
                                          },
                                          true);
        ioContext.restart();
        ioContext.run();
        BOOST_REQUIRE(callbackCalled);
        BOOST_CHECK_EQUAL(stateMessage.elected_id(),1000);
        BOOST_CHECK(!stateMessage.is_detached());
        BOOST_REQUIRE_EQUAL(stateMessage.node_info_size(),4);

        BOOST_CHECK_EQUAL(stateMessage.node_info(0).id(),1000);
        BOOST_CHECK(!stateMessage.node_info(0).is_dead());

        BOOST_CHECK_EQUAL(stateMessage.node_info(1).name(),"remote");
        BOOST_CHECK(!stateMessage.node_info(1).is_dead());

        BOOST_CHECK_EQUAL(stateMessage.node_info(2).name(),"light1");
        BOOST_CHECK(!stateMessage.node_info(2).is_dead());

        BOOST_CHECK_EQUAL(stateMessage.node_info(3).name(),"light2");
        BOOST_CHECK(!stateMessage.node_info(3).is_dead());

        BOOST_CHECK(rh.deadNodes.empty());

    }


}

//The coordinator forces a reelection when it has failed 60 state updates in a row,
//to get itself unstuck. Separate short bursts of failures must not add up to that.
BOOST_AUTO_TEST_CASE( failed_state_updates_are_consecutive )
{
    //Drive UpdateMyState without any new raw data, which fails the prerequisites
    //for as long as remote1 has sent us no raw data of its own.
    const auto failRepeatedly = [this](const int times)
                                {
                                    for (int i = 0; i < times; ++i)
                                    {
                                        coordinator.PerformOnStateMessage([](std::unique_ptr<char []>,
                                                                             const size_t){},
                                                                          true);
                                        ioContext.restart();
                                        ioContext.run();
                                    }
                                };

    ElectionHandlerStub::lastInstance->electedId = 1000;
    ElectionHandlerStub::lastInstance->electionCompleteCallback(1000,100);

    //first burst of failures
    rh.rawCb(GetRawWithOneNode({10,10}),RawChanges(RawChanges::NODES_CHANGED),cs);
    ioContext.run();
    failRepeatedly(34);
    BOOST_CHECK_EQUAL(ElectionHandlerStub::lastInstance->forceElectionCount,0);

    //one successful state update in between
    rh.rawCb(GetRawWithOneNodeAndRemoteRaw({10,10}),RawChanges(RawChanges::NEW_REMOTE_STATISTICS),cs);
    bool stateWasProduced = false;
    coordinator.PerformOnStateMessage([&stateWasProduced](std::unique_ptr<char []>,
                                                          const size_t)
                                      {
                                          stateWasProduced = true;
                                      },
                                      true);
    ioContext.restart();
    ioContext.run();
    BOOST_REQUIRE(stateWasProduced);

    //second burst of failures
    rh.rawCb(GetRawWithOneNode({10,10}),RawChanges(RawChanges::NEW_REMOTE_STATISTICS),cs);
    ioContext.restart();
    ioContext.run();
    failRepeatedly(34);

    //Neither burst came anywhere near 60 failures in a row.
    BOOST_CHECK_EQUAL(ElectionHandlerStub::lastInstance->forceElectionCount,0);

    //But a long enough unbroken run of failures still does force a reelection.
    failRepeatedly(30);
    BOOST_CHECK_EQUAL(ElectionHandlerStub::lastInstance->forceElectionCount,1);
}

BOOST_AUTO_TEST_SUITE_END()
BOOST_FIXTURE_TEST_SUITE( self_is_light, Fixture<15> )

BOOST_AUTO_TEST_CASE( detached_lightnode )
{
    //check that a detached lightnode produces a correct state
    ElectionHandlerStub::lastInstance->isLightNode = true;
    ElectionHandlerStub::lastInstance->electedId = 1000;
    ElectionHandlerStub::lastInstance->isDetached = true;
    ElectionHandlerStub::lastInstance->electionCompleteCallback(1000,100);
    rh.rawCb(GetRawDetachedLightnode(),RawChanges(RawChanges::NODES_CHANGED),cs);
    bool callbackCalled = false;
    SystemStateMessage stateMessage;
    coordinator.PerformOnStateMessage([&callbackCalled,&stateMessage](std::unique_ptr<char []> data,
                                                                      const size_t size)
                                      {
                                          callbackCalled = true;
                                          stateMessage.ParseFromArray(data.get(),static_cast<int>(size));
                                      },
                                      true);

    ioContext.run();
    BOOST_CHECK(ElectionHandlerStub::lastInstance->nodesChangedCalled);
    BOOST_CHECK(callbackCalled);

    BOOST_CHECK_EQUAL(stateMessage.elected_id(),1000);
    BOOST_CHECK_EQUAL(stateMessage.election_id(),100);
    BOOST_CHECK(stateMessage.is_detached());
    BOOST_REQUIRE_EQUAL(stateMessage.node_info_size(),1);

    BOOST_CHECK_EQUAL(stateMessage.node_info(0).name(),"myself");
    BOOST_CHECK_EQUAL(stateMessage.node_info(0).id(),1000);
    BOOST_CHECK_EQUAL(stateMessage.node_info(0).node_type_id(),15);
    BOOST_CHECK_EQUAL(stateMessage.node_info(0).control_address(),"klopp");
    BOOST_CHECK_EQUAL(stateMessage.node_info(0).data_address(),"flupp");
    BOOST_CHECK(!stateMessage.node_info(0).is_dead());

    BOOST_CHECK(rh.deadNodes.empty());
}

//A state that we discard must not leave any of its fields behind in our own state.
//is_detached matters most: it is what tells the RawHandler's join validation that
//this light node is allowed to adopt the new coordinator's incarnation id, so
//clearing it on the strength of a rejected state stopped us reattaching.
BOOST_AUTO_TEST_CASE( discarded_state_does_not_clear_detached )
{
    bool detached = false;
    const auto readDetached = [this,&detached]
                              {
                                  boost::asio::dispatch(strand,
                                                        [this,&detached]
                                                        {detached = coordinator.IsDetached();});
                                  ioContext.restart();
                                  ioContext.run();
                              };

    //get ourselves into the detached state
    ElectionHandlerStub::lastInstance->isLightNode = true;
    ElectionHandlerStub::lastInstance->electedId = 1000;
    ElectionHandlerStub::lastInstance->isDetached = true;
    ElectionHandlerStub::lastInstance->electionCompleteCallback(1000,100);
    rh.rawCb(GetRawDetachedLightnode(),RawChanges(RawChanges::NODES_CHANGED),cs);
    readDetached();
    BOOST_REQUIRE(detached);

    //Node 1001 turns up and wins the election, so we accept its victory. It has not
    //got our raw data yet, so its first system state does not contain us and we
    //discard it.
    ElectionHandlerStub::lastInstance->isDetached = false;
    ElectionHandlerStub::lastInstance->electedId = 1001;
    ElectionHandlerStub::lastInstance->electionCompleteCallback(1001,101);

    {
        auto state = GetStateWithoutSelf();
        const size_t size = state.ByteSizeLong();
        auto data = Safir::Utilities::Internal::SharedCharArray(new char[size]);
        state.SerializeWithCachedSizesToArray
            (reinterpret_cast<google::protobuf::uint8*>(data.get()));
        coordinator.NewRemoteStatistics(1001,data,size);
        ioContext.restart();
        ioContext.run();
    }

    //We discarded that state, so we have not attached to anything yet.
    readDetached();
    BOOST_CHECK(detached);

    //And once node 1001 does send us a state that we are in, we attach.
    {
        auto state = GetStateWithOneLightnode();
        const size_t size = state.ByteSizeLong();
        auto data = Safir::Utilities::Internal::SharedCharArray(new char[size]);
        state.SerializeWithCachedSizesToArray
            (reinterpret_cast<google::protobuf::uint8*>(data.get()));
        coordinator.NewRemoteStatistics(1001,data,size);
        ioContext.restart();
        ioContext.run();
    }

    readDetached();
    BOOST_CHECK(!detached);
}

BOOST_AUTO_TEST_CASE( detach_and_reattach )
{
    ElectionHandlerStub::lastInstance->isLightNode = true;
    ElectionHandlerStub::lastInstance->electedId = 1001;
    ElectionHandlerStub::lastInstance->electionCompleteCallback(1001,100);
    rh.rawCb(GetRawWithOneNodeAndRemoteRaw({15,10}),RawChanges(RawChanges::NODES_CHANGED),cs);
    {
        auto state = GetStateWithOneLightnode();

        const size_t size = state.ByteSizeLong();
        auto data = Safir::Utilities::Internal::SharedCharArray(new char[size]);
        state.SerializeWithCachedSizesToArray
            (reinterpret_cast<google::protobuf::uint8*>(data.get()));

        coordinator.NewRemoteStatistics(1001,data,size);

        bool callbackCalled = false;

        coordinator.PerformOnStateMessage([&callbackCalled](std::unique_ptr<char []> /*data*/,
                                                            const size_t /*size*/)
                                          {
                                              callbackCalled = true;
                                          },
                                          true);
        ioContext.run();
        BOOST_CHECK(!callbackCalled);
    }
    ElectionHandlerStub::lastInstance->isDetached = true;
    ElectionHandlerStub::lastInstance->electedId = 1000;
    ElectionHandlerStub::lastInstance->electionCompleteCallback(1000,101);
    rh.rawCb(GetRawDetachedLightnode(),RawChanges(RawChanges::NODES_CHANGED),cs);

    {
        bool callbackCalled = false;
        SystemStateMessage stateMessage;
        coordinator.PerformOnStateMessage([&callbackCalled,&stateMessage](std::unique_ptr<char []> data,
                                                                          const size_t size)
                                          {
                                              callbackCalled = true;
                                              stateMessage.ParseFromArray(data.get(),static_cast<int>(size));
                                          },
                                          true);
        ioContext.restart();
        ioContext.run();
        BOOST_CHECK(ElectionHandlerStub::lastInstance->nodesChangedCalled);
        BOOST_CHECK(callbackCalled);

        BOOST_CHECK_EQUAL(stateMessage.elected_id(),1000);
        BOOST_CHECK_EQUAL(stateMessage.election_id(),101);
        BOOST_CHECK(stateMessage.is_detached());
        BOOST_REQUIRE_EQUAL(stateMessage.node_info_size(),1);

        BOOST_CHECK_EQUAL(stateMessage.node_info(0).name(),"myself");
        BOOST_CHECK_EQUAL(stateMessage.node_info(0).id(),1000);
        BOOST_CHECK_EQUAL(stateMessage.node_info(0).node_type_id(),15);
        BOOST_CHECK_EQUAL(stateMessage.node_info(0).control_address(),"klopp");
        BOOST_CHECK_EQUAL(stateMessage.node_info(0).data_address(),"flupp");
        BOOST_CHECK(!stateMessage.node_info(0).is_dead());
        BOOST_CHECK(rh.deadNodes.empty());
    }

    ElectionHandlerStub::lastInstance->isDetached = false;
    ElectionHandlerStub::lastInstance->electedId = 1001;
    ElectionHandlerStub::lastInstance->electionCompleteCallback(1001,102);
    rh.rawCb(GetRawWithOneNodeAndRemoteRaw({15,10}),RawChanges(RawChanges::NODES_CHANGED),cs);
    {
        auto state = GetStateWithOneLightnode();

        const size_t size = state.ByteSizeLong();
        auto data = Safir::Utilities::Internal::SharedCharArray(new char[size]);
        state.SerializeWithCachedSizesToArray
            (reinterpret_cast<google::protobuf::uint8*>(data.get()));

        coordinator.NewRemoteStatistics(1001,data,size);

        bool callbackCalled = false;
        SystemStateMessage stateMessage;
        coordinator.PerformOnStateMessage([&callbackCalled,&stateMessage](std::unique_ptr<char []> data,
                                                                          const size_t size)
                                          {
                                              callbackCalled = true;
                                              stateMessage.ParseFromArray(data.get(),static_cast<int>(size));
                                          },
                                          false);
        ioContext.restart();
        ioContext.run();
        BOOST_CHECK(ElectionHandlerStub::lastInstance->nodesChangedCalled);
        BOOST_CHECK(callbackCalled);

        BOOST_CHECK_EQUAL(stateMessage.elected_id(),1001);
        BOOST_CHECK_EQUAL(stateMessage.election_id(),100);
        BOOST_CHECK(!stateMessage.is_detached());
        BOOST_REQUIRE_EQUAL(stateMessage.node_info_size(),2);

        BOOST_CHECK_EQUAL(stateMessage.node_info(0).name(),"remote1");
        BOOST_CHECK(!stateMessage.node_info(0).is_dead());

        BOOST_CHECK_EQUAL(stateMessage.node_info(1).name(),"myself");
        BOOST_CHECK(!stateMessage.node_info(1).is_dead());

        BOOST_CHECK(rh.deadNodes.empty());
    }


}

BOOST_AUTO_TEST_SUITE_END()
