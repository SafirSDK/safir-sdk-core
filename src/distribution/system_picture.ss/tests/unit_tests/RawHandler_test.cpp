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
#include "../../src/RawHandler.h"
#include <boost/thread.hpp>
#include <set>

#define BOOST_TEST_MODULE RawHandlerTest
#include <boost/test/unit_test.hpp>

class Communication
{
public:

   //Callbacks functions used in Communications public interface.
    typedef std::function<void(const std::string& name,
                               int64_t nodeId,
                               int64_t nodeTypeId,
                               const std::string& controlAddress,
                               const std::string& dataAddress)> NewNode;
    typedef std::function<void(int64_t fromNodeId)> GotReceiveFrom;
    typedef std::function<void(int64_t toNodeId)> RetransmitTo;

    void SetNewNodeCallback(const NewNode& callback)
    {
        BOOST_CHECK(newNodeCb == nullptr);
        newNodeCb = callback;
    }

    void SetGotReceiveFromCallback(const GotReceiveFrom& callback)
    {
        BOOST_CHECK(gotReceiveFromCb == nullptr);
        gotReceiveFromCb = callback;
    }

    void SetRetransmitToCallback(const RetransmitTo& callback)
    {
        BOOST_CHECK(retransmitToCb == nullptr);
        retransmitToCb = callback;
    }

    void IncludeNode(int64_t nodeId)
    {
        includedNodes.insert(nodeId);
    }

    void ExcludeNode(int64_t nodeId)
    {
        excludedNodes.insert(nodeId);
        if (excludeCb != nullptr)
        {
            excludeCb();
        }
    }

    NewNode newNodeCb;
    GotReceiveFrom gotReceiveFromCb;
    RetransmitTo retransmitToCb;

    std::set<int64_t> includedNodes;
    std::set<int64_t> excludedNodes;

    std::function<void()> excludeCb;
};

using namespace Safir::Dob::Internal::SP;

struct Fixture
{
    Fixture()
        : rh (ioService,comm,"plopp",10,100,"asdfasdf","qwerty",GetNodeTypes(),true)
    {
        BOOST_TEST_MESSAGE( "setup fixture" );
    }

    ~Fixture()
    {
        BOOST_TEST_MESSAGE( "teardown fixture" );
    }

    static std::map<int64_t, NodeType> GetNodeTypes()
    {
        std::map<int64_t, NodeType> nodeTypes;
        nodeTypes.insert(std::make_pair(10, NodeType(10,
                                                     "mupp",
                                                     false,
                                                     boost::chrono::milliseconds(1),
                                                     10,
                                                     boost::chrono::seconds(1))));
        nodeTypes.insert(std::make_pair(20, NodeType(20,
                                                     "tupp",
                                                     true,
                                                     boost::chrono::seconds(1),
                                                     22,
                                                     boost::chrono::seconds(1))));
        return nodeTypes;
    }

    Communication comm;
    boost::asio::io_service ioService;

    RawHandlerBasic<::Communication> rh;

};

BOOST_FIXTURE_TEST_SUITE( s, Fixture )

BOOST_AUTO_TEST_CASE( start_stop )
{
    BOOST_CHECK(comm.newNodeCb != nullptr);
    BOOST_CHECK(comm.gotReceiveFromCb != nullptr);
    BOOST_CHECK(comm.retransmitToCb != nullptr);

    rh.Stop();
    ioService.run();
}


BOOST_AUTO_TEST_CASE( receive_from_not_known )
{
    comm.gotReceiveFromCb(10);
    BOOST_CHECK_THROW(ioService.run(), std::logic_error);
}

BOOST_AUTO_TEST_CASE( retransmit_to_not_known )
{
    comm.retransmitToCb(10);
    BOOST_CHECK_THROW(ioService.run(), std::logic_error);
}


BOOST_AUTO_TEST_CASE( new_node )
{
    comm.newNodeCb("asdf",11,10,"asdf","asdf");
    comm.gotReceiveFromCb(11);
    comm.gotReceiveFromCb(11);
    comm.gotReceiveFromCb(11);
    rh.Stop();
    BOOST_CHECK_NO_THROW(ioService.run());
    BOOST_CHECK(comm.includedNodes == std::set<int64_t>{11});
    BOOST_CHECK(comm.excludedNodes.empty());

    //TODO: check actual receive count?
}

BOOST_AUTO_TEST_CASE( new_node_of_unknown_type )
{
    comm.newNodeCb("asdf",11,11,"asdf","asdf");
    BOOST_CHECK_THROW(ioService.run(), std::logic_error);
}

BOOST_AUTO_TEST_CASE( new_node_with_my_id )
{
    comm.newNodeCb("asdf",10,10,"asdf","asdf");
    BOOST_CHECK_THROW(ioService.run(), std::logic_error);
}

BOOST_AUTO_TEST_CASE( new_node_twice )
{
    comm.newNodeCb("asdf",11,10,"asdf","asdf");
    comm.newNodeCb("asdf",11,10,"asdf","asdf");
    BOOST_CHECK_THROW(ioService.run(), std::logic_error);
    BOOST_CHECK(comm.includedNodes == std::set<int64_t>{11});

}

BOOST_AUTO_TEST_CASE( exclude_node )
{
    comm.excludeCb=[&]{rh.Stop();};

    comm.newNodeCb("asdf",11,10,"asdf","asdf");
    comm.gotReceiveFromCb(11);
    comm.gotReceiveFromCb(11);
    comm.gotReceiveFromCb(11);

    BOOST_CHECK_NO_THROW(ioService.run());
    BOOST_CHECK(comm.includedNodes == std::set<int64_t>{11});
    BOOST_CHECK(comm.excludedNodes == std::set<int64_t>{11});
}

void CheckStatisticsCommon(const RawStatistics& statistics)
{
    BOOST_CHECK_EQUAL(statistics.Name(), "plopp");
    BOOST_CHECK_EQUAL(statistics.Id(), 10);
    BOOST_CHECK_EQUAL(statistics.NodeTypeId(), 100);
    BOOST_CHECK_EQUAL(statistics.ControlAddress(), "asdfasdf");
    BOOST_CHECK_EQUAL(statistics.DataAddress(), "qwerty");
    BOOST_CHECK_EQUAL(statistics.ElectionId(), 0);
    BOOST_CHECK_EQUAL(statistics.Size(), 1);
    BOOST_CHECK_EQUAL(statistics.Name(0), "asdf");
    BOOST_CHECK_EQUAL(statistics.Id(0), 11);
    BOOST_CHECK_EQUAL(statistics.NodeTypeId(0), 10);
    BOOST_CHECK_EQUAL(statistics.ControlAddress(0), "asdffff");
    BOOST_CHECK_EQUAL(statistics.DataAddress(0), "asdfqqqq");
    BOOST_CHECK(!statistics.IsLongGone(0));
}

BOOST_AUTO_TEST_CASE( nodes_changed_add_callback )
{
    int cbCalls = 0;
    rh.AddRawChangedCallback([&](const RawStatistics& statistics,
                                 const RawChanges& flags)
                               {
                                   ++cbCalls;
                                   CheckStatisticsCommon(statistics);

                                   BOOST_CHECK(flags.NodesChanged());
                                   BOOST_CHECK(!flags.NewRemoteStatistics());
                                   BOOST_CHECK(!flags.ElectionIdChanged());

                                   BOOST_CHECK(!statistics.IsDead(0));
                                   BOOST_CHECK(statistics.ControlReceiveCount(0) == 0);
                                   BOOST_CHECK(statistics.ControlRetransmitCount(0) == 0);
                                   BOOST_CHECK(statistics.DataReceiveCount(0) == 0);
                                   BOOST_CHECK(statistics.DataRetransmitCount(0) == 0);
                                   BOOST_CHECK(!statistics.HasRemoteStatistics(0));

                               });
    comm.newNodeCb("asdf",11,10,"asdffff","asdfqqqq");
    comm.gotReceiveFromCb(11);
    comm.gotReceiveFromCb(11);
    comm.gotReceiveFromCb(11);
    rh.Stop();
    BOOST_CHECK_NO_THROW(ioService.run());
    BOOST_CHECK_EQUAL(cbCalls, 1);
}


BOOST_AUTO_TEST_CASE( nodes_changed_removed_callback )
{
    comm.excludeCb=[&]{rh.Stop();};

    int cbCalls = 0;
    rh.AddRawChangedCallback([&](const RawStatistics& statistics,
                                 const RawChanges& flags)
                               {
                                   ++cbCalls;
                                   CheckStatisticsCommon(statistics);

                                   BOOST_CHECK(flags.NodesChanged());
                                   BOOST_CHECK(!flags.NewRemoteStatistics());
                                   BOOST_CHECK(!flags.ElectionIdChanged());

                                   BOOST_CHECK(statistics.DataReceiveCount(0) == 0);
                                   BOOST_CHECK(statistics.DataRetransmitCount(0) == 0);

                                   if (cbCalls == 1)
                                   {
                                       BOOST_CHECK(!statistics.IsDead(0));
                                       BOOST_CHECK(statistics.ControlReceiveCount(0) == 0);
                                       BOOST_CHECK(statistics.ControlRetransmitCount(0) == 0);
                                   }
                                   else
                                   {
                                       BOOST_CHECK(statistics.IsDead(0));
                                       BOOST_CHECK(statistics.ControlReceiveCount(0) == 3);
                                       BOOST_CHECK(statistics.ControlRetransmitCount(0) == 1);
                                   }
                               });
    comm.newNodeCb("asdf",11,10,"asdffff","asdfqqqq");
    comm.gotReceiveFromCb(11);
    comm.gotReceiveFromCb(11);
    comm.gotReceiveFromCb(11);
    comm.retransmitToCb(11);

    BOOST_CHECK_NO_THROW(ioService.run());
    BOOST_CHECK_EQUAL(cbCalls, 2);
}

std::unique_ptr<RawStatisticsMessage> GetProtobuf()
{
    auto msg = Safir::make_unique<RawStatisticsMessage>();

    msg->set_name("foo");
    msg->set_id(110);
    msg->set_node_type_id(190);
    msg->set_control_address("asdfasdfdd");
    msg->set_data_address("foobar");
    msg->set_election_id(91);

    auto node = msg->add_node_info();

    node->set_name("1");
    node->set_id(1);
    node->set_node_type_id(101);
    node->set_control_address(":fobar!");
    node->set_data_address(":flopp");
    node->set_is_dead(false);
    node->set_is_long_gone(false);
    node->set_control_receive_count(1000);
    node->set_control_retransmit_count(100);
    node->set_data_receive_count(5000);
    node->set_data_retransmit_count(500);
    return std::move(msg);
}

void CheckRemotesCommon(const RawStatistics& remote)
{
    BOOST_CHECK_EQUAL(remote.Name(), "foo");
    BOOST_CHECK_EQUAL(remote.Id(), 110);
    BOOST_CHECK_EQUAL(remote.NodeTypeId(), 190);
    BOOST_CHECK_EQUAL(remote.ControlAddress(), "asdfasdfdd");
    BOOST_CHECK_EQUAL(remote.DataAddress(), "foobar");
    BOOST_CHECK_EQUAL(remote.ElectionId(), 91);
    BOOST_CHECK_EQUAL(remote.Size(), 1);
    BOOST_CHECK_EQUAL(remote.Name(0), "1");
    BOOST_CHECK_EQUAL(remote.Id(0), 1);
    BOOST_CHECK_EQUAL(remote.NodeTypeId(0), 101);
    BOOST_CHECK_EQUAL(remote.ControlAddress(0), ":fobar!");
    BOOST_CHECK_EQUAL(remote.DataAddress(0), ":flopp");
    BOOST_CHECK(!remote.IsDead(0));
    BOOST_CHECK(!remote.IsLongGone(0));
    BOOST_CHECK(remote.ControlReceiveCount(0) == 1000);
    BOOST_CHECK(remote.ControlRetransmitCount(0) == 100);
    BOOST_CHECK(remote.DataReceiveCount(0) == 5000);
    BOOST_CHECK(remote.DataRetransmitCount(0) == 500);
}

BOOST_AUTO_TEST_CASE( raw_changed_callback )
{
    comm.excludeCb=[&]{rh.Stop();};
    int cbCalls = 0;
    rh.AddRawChangedCallback([&](const RawStatistics& statistics,
                                 const RawChanges& flags)
                               {
                                   ++cbCalls;
                                   CheckStatisticsCommon(statistics);

                                   BOOST_CHECK(!flags.ElectionIdChanged());

                                   if (cbCalls == 1)
                                   {
                                       BOOST_CHECK(flags.NodesChanged());
                                       BOOST_CHECK(!flags.NewRemoteStatistics());

                                       BOOST_CHECK(!statistics.IsDead(0));
                                       BOOST_CHECK(!statistics.HasRemoteStatistics(0));
                                   }
                                   else if (cbCalls == 2)
                                   {
                                       BOOST_CHECK(!flags.NodesChanged());
                                       BOOST_CHECK(flags.NewRemoteStatistics());

                                       BOOST_CHECK(!statistics.IsDead(0));
                                       BOOST_CHECK(statistics.HasRemoteStatistics(0));
                                       CheckRemotesCommon(statistics.RemoteStatistics(0));
                                   }
                                   else
                                   {
                                       BOOST_CHECK(flags.NodesChanged());
                                       BOOST_CHECK(!flags.NewRemoteStatistics());

                                       BOOST_CHECK(statistics.IsDead(0));
                                       BOOST_CHECK(statistics.HasRemoteStatistics(0));
                                   }
                               });
    comm.newNodeCb("asdf",11,10,"asdffff","asdfqqqq");

    auto msg = GetProtobuf();
    const size_t size = msg->ByteSize();
    auto data = boost::make_shared<char[]>(size);
    msg->SerializeWithCachedSizesToArray(reinterpret_cast<google::protobuf::uint8*>(data.get()));
    rh.NewRemoteStatistics(11,data,size);

    BOOST_CHECK_NO_THROW(ioService.run());
    BOOST_CHECK_EQUAL(cbCalls, 3);
}


BOOST_AUTO_TEST_CASE( election_id_changed_callback)
{
    comm.excludeCb=[&]{rh.Stop();};
    int cbCalls = 0;
    rh.AddRawChangedCallback([&](const RawStatistics& statistics,
                                 const RawChanges& flags)
                             {
                                 ++cbCalls;

                                 BOOST_CHECK(!flags.NewRemoteStatistics());
                                 if (cbCalls == 1)
                                 {
                                     BOOST_CHECK(flags.NodesChanged());
                                     BOOST_CHECK(!flags.ElectionIdChanged());
                                 }
                                 else if (cbCalls == 2)
                                 {
                                     BOOST_CHECK(!flags.NodesChanged());
                                     BOOST_CHECK(flags.ElectionIdChanged());
                                     BOOST_CHECK_EQUAL(statistics.ElectionId(), 199);
                                 }
                                 else
                                 {
                                     BOOST_CHECK(flags.NodesChanged());
                                     BOOST_CHECK(!flags.ElectionIdChanged());
                                     BOOST_CHECK_EQUAL(statistics.ElectionId(), 199);
                                 }
                             });
    comm.newNodeCb("asdf",11,10,"asdffff","asdfqqqq");
    rh.SetElectionId(11, 199);

    BOOST_CHECK_NO_THROW(ioService.run());
    BOOST_CHECK_EQUAL(cbCalls, 3);
}

BOOST_AUTO_TEST_CASE( set_dead_node )
{
    int cbCalls = 0;
    rh.AddRawChangedCallback([&](const RawStatistics& statistics,
                                 const RawChanges& flags)
                               {
                                   ++cbCalls;

                                   BOOST_CHECK(flags.NodesChanged());
                                   BOOST_CHECK(!flags.NewRemoteStatistics());
                                   BOOST_CHECK(!flags.ElectionIdChanged());

                                   if (cbCalls == 1)
                                   {
                                       BOOST_CHECK_EQUAL(statistics.Size(), 1);
                                       BOOST_CHECK(!statistics.IsDead(0));
                                   }
                                   else if (cbCalls == 2)
                                   {
                                       BOOST_CHECK_EQUAL(statistics.Size(), 2);
                                       BOOST_CHECK(statistics.IsDead(0));
                                       BOOST_CHECK(!statistics.IsDead(1));
                                   }
                               });
    comm.newNodeCb("asdf",11,10,"asdffff","asdfqqqq");
    rh.SetDeadNode(11);

    //triggers another callback so that we get to check the SetDeadNode result.
    comm.newNodeCb("asdf",12,10,"asdffff","asdfqqqq");

    rh.Stop();
    BOOST_CHECK_NO_THROW(ioService.run());
    BOOST_CHECK_EQUAL(cbCalls, 2);
}

BOOST_AUTO_TEST_CASE( perform_on_all )
{
    comm.newNodeCb("asdf",11,10,"asdffff","asdfqqqq");

    auto msg = GetProtobuf();
    const size_t size = msg->ByteSize();
    auto data = boost::make_shared<char[]>(size);
    msg->SerializeWithCachedSizesToArray(reinterpret_cast<google::protobuf::uint8*>(data.get()));
    rh.NewRemoteStatistics(11,data,size);

    rh.PerformOnAllStatisticsMessage([&](std::unique_ptr<char[]> data,
                                         const size_t size)
                                     {
                                         auto msg = Safir::make_unique<RawStatisticsMessage>();
                                         msg->ParseFromArray(data.get(),static_cast<int>(size));
                                         auto statistics = RawStatisticsCreator::Create(std::move(msg));
                                         CheckStatisticsCommon(statistics);

                                         BOOST_CHECK(!statistics.IsDead(0));
                                         BOOST_CHECK(statistics.HasRemoteStatistics(0));
                                         CheckRemotesCommon(statistics.RemoteStatistics(0));
                                     },
                                     0);
    rh.Stop();
    BOOST_CHECK_NO_THROW(ioService.run());
}

BOOST_AUTO_TEST_CASE( perform_on_my )
{
    comm.newNodeCb("asdf",11,10,"asdffff","asdfqqqq");

    auto msg = GetProtobuf();
    const size_t size = msg->ByteSize();
    auto data = boost::make_shared<char[]>(size);
    msg->SerializeWithCachedSizesToArray(reinterpret_cast<google::protobuf::uint8*>(data.get()));
    rh.NewRemoteStatistics(11,data,size);

    rh.PerformOnMyStatisticsMessage([&](boost::shared_ptr<char[]> data,
                                         const size_t size)
                                     {
                                         auto msg = Safir::make_unique<RawStatisticsMessage>();
                                         msg->ParseFromArray(data.get(),static_cast<int>(size));
                                         auto statistics = RawStatisticsCreator::Create(std::move(msg));
                                         CheckStatisticsCommon(statistics);

                                         BOOST_CHECK(!statistics.IsDead(0));
                                         BOOST_CHECK(!statistics.HasRemoteStatistics(0));
                                     },
                                     0);
    rh.Stop();
    BOOST_CHECK_NO_THROW(ioService.run());
}

BOOST_AUTO_TEST_SUITE_END()
