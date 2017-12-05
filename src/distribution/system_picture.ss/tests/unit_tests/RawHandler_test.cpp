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
#include "../../src/RawHandler.h"
#include <set>

#if defined _MSC_VER
#  pragma warning (push)
#  pragma warning (disable : 4100)
#endif

#include <boost/thread.hpp>

#if defined _MSC_VER
#  pragma warning (pop)
#endif

#define BOOST_TEST_MODULE RawHandlerTest
#include <boost/test/unit_test.hpp>

class Communication
{
public:

   //Callbacks functions used in Communications public interface.
    typedef boost::function<void(const std::string& name,
                                 int64_t nodeId,
                                 int64_t nodeTypeId,
                                 const std::string& controlAddress,
                                 const std::string& dataAddress,
                                 bool multicast)> NewNode;
    typedef boost::function<void(int64_t fromNodeId,
                                 bool isMulticast,
                                 bool isDuplicate)> GotReceiveFrom;
    typedef boost::function<void(int64_t toNodeId, size_t transmitCount)> RetransmitTo;

    void SetNewNodeCallback(const NewNode& callback)
    {
        BOOST_CHECK(newNodeCb == NULL);
        newNodeCb = callback;
    }

    void SetGotReceiveFromCallback(const GotReceiveFrom& callback)
    {
        BOOST_CHECK(gotReceiveFromCb == NULL);
        gotReceiveFromCb = callback;
    }

    void SetRetransmitToCallback(const RetransmitTo& callback)
    {
        BOOST_CHECK(retransmitToCb == NULL);
        retransmitToCb = callback;
    }

    void IncludeNode(int64_t nodeId)
    {
        includedNodes.insert(nodeId);
    }

    void ExcludeNode(int64_t nodeId)
    {
        excludedNodes.insert(nodeId);
        if (!excludeCb.empty())
        {
            excludeCb();
        }
    }

    NewNode newNodeCb;
    GotReceiveFrom gotReceiveFromCb;
    RetransmitTo retransmitToCb;

    std::set<int64_t> includedNodes;
    std::set<int64_t> excludedNodes;

    boost::function<void()> excludeCb;
};

using namespace Safir::Dob::Internal::SP;

std::unique_ptr<RawStatisticsMessage> GetProtobuf(bool setIncarnation)
{
    auto msg = Safir::make_unique<RawStatisticsMessage>();

    msg->set_name("foo");
    msg->set_id(110);
    msg->set_node_type_id(190);
    msg->set_control_address("asdfasdfdd");
    msg->set_data_address("foobar");
    msg->set_election_id(91);

    if (setIncarnation)
    {
        msg->set_incarnation_id(12345);
    }

    auto node = msg->add_node_info();

    node->set_name("1");
    node->set_id(1);
    node->set_node_type_id(101);
    node->set_control_address(":fobar!");
    node->set_data_address(":flopp");
    node->set_is_dead(false);
    node->set_control_receive_count(1000);
    node->set_control_duplicate_count(2000);
    node->set_control_retransmit_count(100);
    node->set_data_receive_count(5000);
    node->set_data_duplicate_count(10000);
    node->set_data_retransmit_count(500);

    return msg;
}

struct Fixture
{
    Fixture()
        : formSystemDeniesBeforeOk(0),
          formSystemCallsBeforeJoin(10000)
    {
        rh.reset(new RawHandlerBasic<::Communication>(ioService,comm,"plopp",10,100,"asdfasdf","qwerty",
                                                      GetNodeTypes(), true,
                                                      [this](const int64_t id)
                                                      {return ValidateJoinSystem(id);},
                                                      [this](const int64_t id)
                                                      {return ValidateFormSystem(id);}));
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

    bool ValidateJoinSystem(const int64_t id)
    {
        joinSystemIncarnations.insert(id);
        return forbiddenJoinIncarnations.find(id) == forbiddenJoinIncarnations.end();
    }

    bool ValidateFormSystem(const int64_t id)
    {
        formSystemIncarnations.insert(id);

        if (formSystemCallsBeforeJoin == 0)
        {
            comm.newNodeCb("asdf",11,10,"asdffff","asdfqqqq", false);

            auto msg = GetProtobuf(true);
            const size_t size = msg->ByteSize();
            auto data = boost::make_shared<char[]>(size);
            msg->SerializeWithCachedSizesToArray(reinterpret_cast<google::protobuf::uint8*>(data.get()));
            rh->NewRemoteStatistics(11,data,size);
        }
        else
        {
            --formSystemCallsBeforeJoin;
        }

        if (formSystemDeniesBeforeOk == 0)
        {
            return true;
        }
        else
        {
            --formSystemDeniesBeforeOk;
            return false;
        }
    }

    Communication comm;
    boost::asio::io_service ioService;

    std::unique_ptr<RawHandlerBasic<::Communication>> rh;

    std::set<int64_t> forbiddenJoinIncarnations;
    std::set<int64_t> joinSystemIncarnations;
    std::set<int64_t> formSystemIncarnations;
    unsigned int formSystemDeniesBeforeOk;
    unsigned int formSystemCallsBeforeJoin;
};

BOOST_FIXTURE_TEST_SUITE( s, Fixture )

BOOST_AUTO_TEST_CASE( start_stop )
{
    BOOST_CHECK(!comm.newNodeCb.empty());
    BOOST_CHECK(!comm.gotReceiveFromCb.empty());
    BOOST_CHECK(!comm.retransmitToCb.empty());
    BOOST_CHECK(joinSystemIncarnations.empty());
    BOOST_CHECK(formSystemIncarnations.empty());

    rh->Stop();
    ioService.run();
}


BOOST_AUTO_TEST_CASE( receive_from_not_known )
{
    comm.gotReceiveFromCb(10,false,false);
    BOOST_CHECK_THROW(ioService.run(), std::logic_error);
}

BOOST_AUTO_TEST_CASE( retransmit_to_not_known )
{
    comm.retransmitToCb(10, 2);
    BOOST_CHECK_THROW(ioService.run(), std::logic_error);
}


BOOST_AUTO_TEST_CASE( new_node )
{
    comm.newNodeCb("asdf",11,10,"asdf","asdf", false);
    comm.gotReceiveFromCb(11,false,false);
    comm.gotReceiveFromCb(11,false,false);
    comm.gotReceiveFromCb(11,false,false);
    rh->Stop();

    std::set<int64_t> correctNodes;
    correctNodes.insert(11);

    BOOST_CHECK_NO_THROW(ioService.run());
    BOOST_CHECK(comm.includedNodes == correctNodes);
    BOOST_CHECK(comm.excludedNodes.empty());
}

BOOST_AUTO_TEST_CASE( new_node_of_unknown_type )
{
    comm.newNodeCb("asdf",11,11,"asdf","asdf", false);
    BOOST_CHECK_THROW(ioService.run(), std::logic_error);
}

BOOST_AUTO_TEST_CASE( new_node_with_my_id )
{
    comm.newNodeCb("asdf",10,10,"asdf","asdf", false);
    BOOST_CHECK_THROW(ioService.run(), std::logic_error);
}

BOOST_AUTO_TEST_CASE( new_node_twice )
{
    comm.newNodeCb("asdf",11,10,"asdf","asdf", false);
    comm.newNodeCb("asdf",11,10,"asdf","asdf", false);

    std::set<int64_t> correctNodes;
    correctNodes.insert(11);

    BOOST_CHECK_THROW(ioService.run(), std::logic_error);
    BOOST_CHECK(comm.includedNodes == correctNodes);

}

BOOST_AUTO_TEST_CASE( exclude_node_unicast )
{
    bool stopped = false;
    comm.excludeCb=[&]{rh->Stop();stopped=true;};

    comm.newNodeCb("asdf",11,10,"asdf","asdf", false);
    comm.gotReceiveFromCb(11,false,false);

    std::set<int64_t> correctNodes;
    correctNodes.insert(11);

    BOOST_CHECK_NO_THROW(ioService.run());

    BOOST_CHECK(comm.includedNodes == correctNodes);
    BOOST_CHECK(comm.excludedNodes == correctNodes);
    BOOST_CHECK(stopped);
}

BOOST_AUTO_TEST_CASE( exclude_node_multicast )
{
    //check that even if we get unicast data (but no multicast data) we will get excluded
    //when we're running in mc mode.
    bool stopped = false;
    comm.excludeCb=[&]{rh->Stop();stopped=true;};

    comm.newNodeCb("asdf",11,10,"asdf","asdf", true);
    comm.gotReceiveFromCb(11,false,false);

    std::set<int64_t> correctNodes;
    correctNodes.insert(11);

    int i = 0;
    while(ioService.run_one())
    {
        ++i;
        if (!stopped)
        {
            comm.gotReceiveFromCb(11,false,false);
        }
    }
    BOOST_CHECK(i > 10);
    BOOST_CHECK(comm.includedNodes == correctNodes);
    BOOST_CHECK(comm.excludedNodes == correctNodes);
    BOOST_CHECK(stopped);
}


BOOST_AUTO_TEST_CASE( exclude_node_due_to_retransmit_no_recv )
{
    comm.newNodeCb("asdf",11,10,"asdf","asdf", false);
    comm.newNodeCb("asdf",12,10,"asdf","asdf", false);
    comm.newNodeCb("asdf",13,10,"asdf","asdf", false);
    comm.newNodeCb("asdf",14,10,"asdf","asdf", false);
    comm.newNodeCb("asdf",15,10,"asdf","asdf", false);
    comm.retransmitToCb(11,1);
    comm.retransmitToCb(12,19);
    comm.retransmitToCb(13,20);
    comm.retransmitToCb(14,100);
    comm.retransmitToCb(15,1);
    comm.retransmitToCb(15,20);
    rh->Stop();

    BOOST_CHECK_NO_THROW(ioService.run());

    BOOST_CHECK(comm.excludedNodes.empty());
}

BOOST_AUTO_TEST_CASE( exclude_node_due_to_retransmit )
{
    comm.newNodeCb("asdf",11,10,"asdf","asdf", false);
    comm.newNodeCb("asdf",12,10,"asdf","asdf", false);
    comm.newNodeCb("asdf",13,10,"asdf","asdf", false);
    comm.newNodeCb("asdf",14,10,"asdf","asdf", false);
    comm.newNodeCb("asdf",15,10,"asdf","asdf", false);
    comm.gotReceiveFromCb(11,false,false);
    comm.gotReceiveFromCb(12,false,false);
    comm.gotReceiveFromCb(13,false,false);
    comm.gotReceiveFromCb(14,false,false);
    comm.gotReceiveFromCb(15,false,false);
    comm.retransmitToCb(11,1);
    comm.retransmitToCb(12,19);
    comm.retransmitToCb(13,20);
    comm.retransmitToCb(14,100);
    comm.retransmitToCb(15,1);
    comm.retransmitToCb(15,20);
    rh->Stop();

    std::set<int64_t> correctNodes;
    correctNodes.insert(13);
    correctNodes.insert(14);
    correctNodes.insert(15);

    BOOST_CHECK_NO_THROW(ioService.run());

    BOOST_CHECK(comm.excludedNodes == correctNodes);
}

void CheckStatisticsCommon(const RawStatistics& statistics, int externalNodes)
{
    BOOST_CHECK_EQUAL(statistics.Name(), "plopp");
    BOOST_CHECK_EQUAL(statistics.Id(), 10);
    BOOST_CHECK_EQUAL(statistics.NodeTypeId(), 100);
    BOOST_CHECK_EQUAL(statistics.ControlAddress(), "asdfasdf");
    BOOST_CHECK_EQUAL(statistics.DataAddress(), "qwerty");
    BOOST_CHECK_EQUAL(statistics.ElectionId(), 0);
    BOOST_CHECK_EQUAL(statistics.Size(), externalNodes);

    for (int i = 0; i < externalNodes; ++i)
    {

        BOOST_CHECK_EQUAL(statistics.Name(i), "asdf");
        BOOST_CHECK_EQUAL(statistics.Id(i), 11);
        BOOST_CHECK_EQUAL(statistics.NodeTypeId(i), 10);
        BOOST_CHECK_EQUAL(statistics.ControlAddress(i), "asdffff");
        BOOST_CHECK_EQUAL(statistics.DataAddress(i), "asdfqqqq");
    }
}


BOOST_AUTO_TEST_CASE( nodes_changed_add_callback )
{
    int cbCalls = 0;
    rh->AddRawChangedCallback([&](const RawStatistics& statistics,
                                  const RawChanges& flags,
                                  boost::shared_ptr<void> /*completionSignaller*/)
                              {
                                  ++cbCalls;
                                  CheckStatisticsCommon(statistics, 1);

                                  BOOST_CHECK(flags.NodesChanged());
                                  BOOST_CHECK(!flags.NewRemoteStatistics());
                                  BOOST_CHECK(!flags.MetadataChanged());

                                  BOOST_CHECK(!statistics.IsDead(0));
                                  BOOST_CHECK(statistics.ControlReceiveCount(0) == 0);
                                  BOOST_CHECK(statistics.ControlDuplicateCount(0) == 0);
                                  BOOST_CHECK(statistics.ControlRetransmitCount(0) == 0);
                                  BOOST_CHECK(statistics.DataReceiveCount(0) == 0);
                                  BOOST_CHECK(statistics.DataDuplicateCount(0) == 0);
                                  BOOST_CHECK(statistics.DataRetransmitCount(0) == 0);
                                  BOOST_CHECK(!statistics.HasRemoteStatistics(0));
                              });
    comm.newNodeCb("asdf",11,10,"asdffff","asdfqqqq", false);
    comm.gotReceiveFromCb(11,false,false);
    comm.gotReceiveFromCb(11,false,false);
    comm.gotReceiveFromCb(11,false,false);
    rh->Stop();
    BOOST_CHECK_NO_THROW(ioService.run());
    BOOST_CHECK_EQUAL(cbCalls, 1);
}


BOOST_AUTO_TEST_CASE( nodes_changed_removed_callback )
{
    comm.excludeCb=[&]{rh->Stop();};

    int cbCalls = 0;
    rh->AddRawChangedCallback([&](const RawStatistics& statistics,
                                 const RawChanges& flags,
                                 boost::shared_ptr<void> /*completionSignaller*/)
                               {
                                   ++cbCalls;
                                   CheckStatisticsCommon(statistics, 1);

                                   BOOST_CHECK(flags.NodesChanged());
                                   BOOST_CHECK(!flags.NewRemoteStatistics());
                                   BOOST_CHECK(!flags.MetadataChanged());

                                   BOOST_CHECK(statistics.DataReceiveCount(0) == 0);
                                   BOOST_CHECK(statistics.DataDuplicateCount(0) == 0);
                                   BOOST_CHECK(statistics.DataRetransmitCount(0) == 0);

                                   if (cbCalls == 1)
                                   {
                                       BOOST_CHECK(!statistics.IsDead(0));
                                       BOOST_CHECK(statistics.ControlReceiveCount(0) == 0);
                                       BOOST_CHECK(statistics.ControlDuplicateCount(0) == 0);
                                       BOOST_CHECK(statistics.ControlRetransmitCount(0) == 0);
                                   }
                                   else
                                   {
                                       BOOST_CHECK(statistics.IsDead(0));
                                       BOOST_CHECK(statistics.ControlReceiveCount(0) == 2);
                                       BOOST_CHECK(statistics.ControlDuplicateCount(0) == 1);
                                       BOOST_CHECK(statistics.ControlRetransmitCount(0) == 1);
                                   }
                               });
    comm.newNodeCb("asdf",11,10,"asdffff","asdfqqqq", false);
    comm.gotReceiveFromCb(11, false, false);
    comm.gotReceiveFromCb(11, false, false);
    comm.gotReceiveFromCb(11, false, true);

    comm.retransmitToCb(11, 2);

    BOOST_CHECK_NO_THROW(ioService.run());
    BOOST_CHECK_EQUAL(cbCalls, 2);
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
    BOOST_CHECK(remote.ControlReceiveCount(0) == 1000);
    BOOST_CHECK(remote.ControlDuplicateCount(0) == 2000);
    BOOST_CHECK(remote.ControlRetransmitCount(0) == 100);
    BOOST_CHECK(remote.DataReceiveCount(0) == 5000);
    BOOST_CHECK(remote.DataDuplicateCount(0) == 10000);
    BOOST_CHECK(remote.DataRetransmitCount(0) == 500);
}

BOOST_AUTO_TEST_CASE( raw_changed_callback )
{
    comm.excludeCb=[&]{rh->Stop();};
    int cbCalls = 0;
    rh->AddRawChangedCallback([&](const RawStatistics& statistics,
                                  const RawChanges& flags,
                                  boost::shared_ptr<void> /*completionSignaller*/)
                               {
                                   ++cbCalls;
                                   CheckStatisticsCommon(statistics, 1);

                                   if (cbCalls == 1)
                                   {
                                       BOOST_CHECK(!flags.MetadataChanged());
                                       BOOST_CHECK(flags.NodesChanged());
                                       BOOST_CHECK(!flags.NewRemoteStatistics());

                                       BOOST_CHECK(!statistics.IsDead(0));
                                       BOOST_CHECK(!statistics.HasRemoteStatistics(0));
                                   }
                                   else if (cbCalls == 2)
                                   {
                                       BOOST_CHECK(flags.MetadataChanged());
                                       BOOST_CHECK(!flags.NodesChanged());
                                       BOOST_CHECK(flags.NewRemoteStatistics());

                                       BOOST_CHECK(!statistics.IsDead(0));
                                       BOOST_CHECK(statistics.HasRemoteStatistics(0));
                                       CheckRemotesCommon(statistics.RemoteStatistics(0));
                                   }
                                   else
                                   {
                                       BOOST_CHECK(!flags.MetadataChanged());
                                       BOOST_CHECK(flags.NodesChanged());
                                       BOOST_CHECK(!flags.NewRemoteStatistics());

                                       BOOST_CHECK(statistics.IsDead(0));
                                       BOOST_CHECK(statistics.HasRemoteStatistics(0));
                                   }
                               });
    comm.newNodeCb("asdf",11,10,"asdffff","asdfqqqq", false);

    auto msg = GetProtobuf(true);
    const size_t size = msg->ByteSize();
    auto data = boost::make_shared<char[]>(size);
    msg->SerializeWithCachedSizesToArray(reinterpret_cast<google::protobuf::uint8*>(data.get()));
    rh->NewRemoteStatistics(11,data,size);

    BOOST_CHECK_NO_THROW(ioService.run());
    BOOST_CHECK_EQUAL(cbCalls, 3);
    BOOST_REQUIRE_EQUAL(joinSystemIncarnations.size(), 1U);
    BOOST_CHECK_EQUAL(*joinSystemIncarnations.begin(), 12345);
    BOOST_REQUIRE_EQUAL(formSystemIncarnations.size(), 0U);
}



BOOST_AUTO_TEST_CASE( no_incarnations_discard )
{
    comm.excludeCb=[&]{rh->Stop();};
    int cbCalls = 0;
    rh->AddRawChangedCallback([&](const RawStatistics& statistics,
                                  const RawChanges& flags,
                                  boost::shared_ptr<void> /*completionSignaller*/)
                               {
                                   ++cbCalls;
                                   CheckStatisticsCommon(statistics, 1);

                                   BOOST_CHECK(!flags.MetadataChanged());
                                   BOOST_CHECK(flags.NodesChanged());
                                   BOOST_CHECK(!flags.NewRemoteStatistics());

                                   if (cbCalls == 1)
                                   {
                                       BOOST_CHECK(!statistics.IsDead(0));
                                       BOOST_CHECK(!statistics.HasRemoteStatistics(0));
                                   }
                                   else if (cbCalls == 2)
                                   {
                                       BOOST_CHECK(statistics.IsDead(0));
                                       BOOST_CHECK(!statistics.HasRemoteStatistics(0));
                                   }

                               });
    comm.newNodeCb("asdf",11,10,"asdffff","asdfqqqq", false);

    auto msg = GetProtobuf(false);
    const size_t size = msg->ByteSize();
    auto data = boost::make_shared<char[]>(size);
    msg->SerializeWithCachedSizesToArray(reinterpret_cast<google::protobuf::uint8*>(data.get()));
    rh->NewRemoteStatistics(11,data,size);

    BOOST_CHECK_NO_THROW(ioService.run());
    BOOST_CHECK_EQUAL(cbCalls, 2);
    BOOST_REQUIRE_EQUAL(joinSystemIncarnations.size(), 0U);
    BOOST_REQUIRE_EQUAL(formSystemIncarnations.size(), 0U);
}


BOOST_AUTO_TEST_CASE( election_id_changed_callback)
{
    comm.excludeCb=[&]{rh->Stop();};
    int cbCalls = 0;
    rh->AddRawChangedCallback([&](const RawStatistics& statistics,
                                  const RawChanges& flags,
                                  boost::shared_ptr<void> /*completionSignaller*/)
                             {
                                 ++cbCalls;

                                 BOOST_CHECK(!flags.NewRemoteStatistics());
                                 if (cbCalls == 1)
                                 {
                                     BOOST_CHECK(flags.NodesChanged());
                                     BOOST_CHECK(!flags.MetadataChanged());
                                 }
                                 else if (cbCalls == 2)
                                 {
                                     BOOST_CHECK(!flags.NodesChanged());
                                     BOOST_CHECK(flags.MetadataChanged());
                                     BOOST_CHECK_EQUAL(statistics.ElectionId(), 199);
                                 }
                                 else
                                 {
                                     BOOST_CHECK(flags.NodesChanged());
                                     BOOST_CHECK(!flags.MetadataChanged());
                                     BOOST_CHECK_EQUAL(statistics.ElectionId(), 199);
                                 }
                             });
    comm.newNodeCb("asdf",11,10,"asdffff","asdfqqqq", false);
    rh->SetElectionId(11, 199);

    BOOST_CHECK_NO_THROW(ioService.run());
    BOOST_CHECK_EQUAL(cbCalls, 3);
}

BOOST_AUTO_TEST_CASE( join_system_callback)
{
    comm.excludeCb=[&]{rh->Stop();};
    int cbCalls = 0;
    rh->AddRawChangedCallback([&](const RawStatistics& statistics,
                                  const RawChanges& flags,
                                  boost::shared_ptr<void> /*completionSignaller*/)
                               {
                                   ++cbCalls;
                                   CheckStatisticsCommon(statistics, 1);

                                   if (cbCalls == 1)
                                   {
                                       BOOST_CHECK(!flags.MetadataChanged());
                                       BOOST_CHECK_EQUAL(statistics.IncarnationId(),0);
                                   }
                                   else if (cbCalls == 2)
                                   {
                                       BOOST_CHECK(flags.MetadataChanged());
                                       BOOST_CHECK_EQUAL(statistics.IncarnationId(),12345);
                                   }
                                   else
                                   {
                                       BOOST_CHECK(!flags.MetadataChanged());
                                       BOOST_CHECK_EQUAL(statistics.IncarnationId(),12345);

                                   }
                               });
    comm.newNodeCb("asdf",11,10,"asdffff","asdfqqqq", false);

    auto msg = GetProtobuf(true);
    const size_t size = msg->ByteSize();
    auto data = boost::make_shared<char[]>(size);
    msg->SerializeWithCachedSizesToArray(reinterpret_cast<google::protobuf::uint8*>(data.get()));
    rh->NewRemoteStatistics(11,data,size);

    BOOST_CHECK_NO_THROW(ioService.run());
    BOOST_CHECK_EQUAL(cbCalls, 3);
    BOOST_REQUIRE_EQUAL(joinSystemIncarnations.size(), 1U);
    BOOST_CHECK_EQUAL(*joinSystemIncarnations.begin(), 12345);
    BOOST_REQUIRE_EQUAL(formSystemIncarnations.size(), 0U);
}

BOOST_AUTO_TEST_CASE( join_system_forbid)
{
    forbiddenJoinIncarnations.insert(12345);

    comm.excludeCb=[&]{rh->Stop();};
    int cbCalls = 0;
    rh->AddRawChangedCallback([&](const RawStatistics& statistics,
                                  const RawChanges& flags,
                                  boost::shared_ptr<void> /*completionSignaller*/)
                               {
                                   ++cbCalls;
                                   CheckStatisticsCommon(statistics, 1);

                                   BOOST_CHECK(!flags.MetadataChanged());
                                   BOOST_CHECK_EQUAL(statistics.IncarnationId(),0);

                                   if (cbCalls == 1)
                                   {
                                       BOOST_CHECK(!statistics.IsDead(0));
                                   }
                                   else
                                   {
                                       BOOST_CHECK(statistics.IsDead(0));
                                   }
                               });
    comm.newNodeCb("asdf",11,10,"asdffff","asdfqqqq", false);

    auto msg = GetProtobuf(true);
    const size_t size = msg->ByteSize();
    auto data = boost::make_shared<char[]>(size);
    msg->SerializeWithCachedSizesToArray(reinterpret_cast<google::protobuf::uint8*>(data.get()));
    rh->NewRemoteStatistics(11,data,size);

    BOOST_CHECK_NO_THROW(ioService.run());
    BOOST_CHECK_EQUAL(cbCalls, 2);
    BOOST_REQUIRE_EQUAL(joinSystemIncarnations.size(), 1U);
    BOOST_CHECK_EQUAL(*joinSystemIncarnations.begin(), 12345);
    BOOST_REQUIRE_EQUAL(formSystemIncarnations.size(), 0U);
}

BOOST_AUTO_TEST_CASE( form_system )
{
    formSystemDeniesBeforeOk = 0;

    int cbCalls = 0;
    rh->AddRawChangedCallback([&](const RawStatistics& statistics,
                                  const RawChanges& flags,
                                  boost::shared_ptr<void> /*completionSignaller*/)
                               {
                                   ++cbCalls;
                                   CheckStatisticsCommon(statistics, 0);

                                   BOOST_CHECK(flags.MetadataChanged());
                                   BOOST_CHECK_EQUAL(statistics.IncarnationId(),54321);

                                   rh->Stop();

                               });

    rh->FormSystem(54321);

    BOOST_CHECK_NO_THROW(ioService.run());
    BOOST_CHECK_EQUAL(cbCalls, 1);
    BOOST_CHECK_EQUAL(joinSystemIncarnations.size(), 0U);
    BOOST_REQUIRE_EQUAL(formSystemIncarnations.size(), 1U);
    BOOST_CHECK_EQUAL(*formSystemIncarnations.begin(), 54321);
}

BOOST_AUTO_TEST_CASE( form_system_denies )
{
    formSystemDeniesBeforeOk = 1;

    int cbCalls = 0;
    rh->AddRawChangedCallback([&](const RawStatistics& statistics,
                                  const RawChanges& flags,
                                  boost::shared_ptr<void> /*completionSignaller*/)
                               {
                                   ++cbCalls;
                                   CheckStatisticsCommon(statistics, 0);

                                   BOOST_CHECK(flags.MetadataChanged());
                                   BOOST_CHECK_EQUAL(statistics.IncarnationId(),54321);

                                   rh->Stop();
                               });

    rh->FormSystem(54321);
    rh->FormSystem(54321);

    BOOST_CHECK_NO_THROW(ioService.run());
    BOOST_CHECK_EQUAL(cbCalls, 1);
    BOOST_CHECK_EQUAL(joinSystemIncarnations.size(), 0U);
    BOOST_CHECK_EQUAL(formSystemDeniesBeforeOk, 0U);
    BOOST_REQUIRE_EQUAL(formSystemIncarnations.size(), 1U);
    BOOST_CHECK_EQUAL(*formSystemIncarnations.begin(), 54321);
}

BOOST_AUTO_TEST_CASE( form_system_delay_then_join )
{
    formSystemDeniesBeforeOk = 9999999; //never say ok in form system callback
    formSystemCallsBeforeJoin = 1;

    comm.excludeCb=[&]{rh->Stop();};

    int cbCalls = 0;
    rh->AddRawChangedCallback([&](const RawStatistics& statistics,
                                  const RawChanges& flags,
                                  boost::shared_ptr<void> /*completionSignaller*/)
                               {
                                   ++cbCalls;
                                   CheckStatisticsCommon(statistics, 1);

                                   if (cbCalls == 1)
                                   {
                                       BOOST_CHECK(!flags.MetadataChanged());
                                       BOOST_CHECK_EQUAL(statistics.IncarnationId(),0);
                                   }
                                   else if (cbCalls == 2)
                                   {
                                       BOOST_CHECK(flags.MetadataChanged());
                                       BOOST_CHECK_EQUAL(statistics.IncarnationId(),12345);
                                   }
                                   else
                                   {
                                       BOOST_CHECK(!flags.MetadataChanged());
                                       BOOST_CHECK_EQUAL(statistics.IncarnationId(),12345);
                                   }
                               });
    rh->FormSystem(54321);
    rh->FormSystem(54321);

    BOOST_CHECK_NO_THROW(ioService.run());
    BOOST_CHECK_EQUAL(cbCalls, 3);
    BOOST_CHECK_EQUAL(joinSystemIncarnations.size(), 1U);
    BOOST_CHECK_EQUAL(*joinSystemIncarnations.begin(), 12345);
    BOOST_REQUIRE_EQUAL(formSystemIncarnations.size(), 1U);
    BOOST_CHECK_EQUAL(*formSystemIncarnations.begin(), 54321);

}

BOOST_AUTO_TEST_CASE( explicit_exclude_node )
{
    int cbCalls = 0;
    rh->AddRawChangedCallback([&](const RawStatistics& statistics,
                                  const RawChanges& flags,
                                  boost::shared_ptr<void> /*completionSignaller*/)
                              {
                                   ++cbCalls;

                                   BOOST_CHECK(flags.NodesChanged());
                                   BOOST_CHECK(!flags.NewRemoteStatistics());
                                   BOOST_CHECK(!flags.MetadataChanged());

                                   if (cbCalls == 1)
                                   {
                                       BOOST_CHECK_EQUAL(statistics.Size(), 1);
                                       BOOST_CHECK(!statistics.IsDead(0));
                                   }
                                   else if (cbCalls == 2)
                                   {
                                       BOOST_CHECK_EQUAL(statistics.Size(), 1);
                                       BOOST_CHECK(statistics.IsDead(0));
                                   }
                                   else if (cbCalls == 3)
                                   {
                                       BOOST_CHECK_EQUAL(statistics.Size(), 2);
                                       BOOST_CHECK(statistics.IsDead(0));
                                       BOOST_CHECK(!statistics.IsDead(1));
                                   }
                               });
    comm.newNodeCb("asdf",11,10,"asdffff","asdfqqqq", false);
    rh->ExcludeNode(11);

    //triggers another callback so that we get to check the ExcludeNode result.
    comm.newNodeCb("asdf",12,10,"asdffff","asdfqqqq", false);

    rh->Stop();
    BOOST_CHECK_NO_THROW(ioService.run());
    BOOST_CHECK_EQUAL(cbCalls, 3);
    BOOST_REQUIRE_EQUAL(comm.excludedNodes.size(),1U);
    BOOST_CHECK_EQUAL(*comm.excludedNodes.begin(),11);
}

BOOST_AUTO_TEST_CASE( recently_dead_nodes )
{
    int cbCalls = 0;
    rh->AddRawChangedCallback([&](const RawStatistics& statistics,
                                  const RawChanges& flags,
                                  boost::shared_ptr<void> /*completionSignaller*/)
                               {
                                   ++cbCalls;

                                   BOOST_CHECK(flags.NodesChanged());
                                   BOOST_CHECK(!flags.NewRemoteStatistics());
                                   BOOST_CHECK(!flags.MetadataChanged());

                                   if (cbCalls == 1)
                                   {
                                       BOOST_CHECK_EQUAL(statistics.Size(), 1);
                                       BOOST_CHECK(!statistics.IsDead(0));
                                   }
                                   else if (cbCalls == 2)
                                   {
                                       BOOST_CHECK_EQUAL(statistics.Size(), 1);
                                       BOOST_CHECK(statistics.IsDead(0));
                                   }
                                   else if (cbCalls == 3)
                                   {
                                       BOOST_CHECK_EQUAL(statistics.Size(), 2);
                                       BOOST_CHECK(statistics.IsDead(0));
                                       BOOST_CHECK(!statistics.IsDead(1));
                                   }
                               });
    comm.newNodeCb("asdf",11,10,"asdffff","asdfqqqq", false);
    std::vector<int64_t> dead;
    dead.push_back(11);
    dead.push_back(100);
    dead.push_back(2900);
    rh->RecentlyDeadNodes(dead);

    //triggers another callback so that we get to check the ExcludeNode result.
    comm.newNodeCb("asdf",12,10,"asdffff","asdfqqqq", false);

    rh->Stop();
    ioService.run();
    //BOOST_CHECK_NO_THROW(ioService.run());
    BOOST_CHECK_EQUAL(cbCalls, 3);
}

BOOST_AUTO_TEST_CASE( perform_on_all )
{
    comm.newNodeCb("asdf",11,10,"asdffff","asdfqqqq", false);

    auto msg = GetProtobuf(true);
    const size_t size = msg->ByteSize();
    auto data = boost::make_shared<char[]>(size);
    msg->SerializeWithCachedSizesToArray(reinterpret_cast<google::protobuf::uint8*>(data.get()));
    rh->NewRemoteStatistics(11,data,size);

    rh->PerformOnAllStatisticsMessage([&](const std::unique_ptr<char[]>& data,
                                          const size_t size)
                                     {
                                         auto msg = Safir::make_unique<RawStatisticsMessage>();
                                         msg->ParseFromArray(data.get(),static_cast<int>(size));
                                         auto statistics = RawStatisticsCreator::Create(std::move(msg));
                                         CheckStatisticsCommon(statistics, 1);

                                         BOOST_CHECK(!statistics.IsDead(0));
                                         BOOST_CHECK(statistics.HasRemoteStatistics(0));
                                         CheckRemotesCommon(statistics.RemoteStatistics(0));
                                     });
    rh->Stop();
    BOOST_CHECK_NO_THROW(ioService.run());
}

BOOST_AUTO_TEST_CASE( perform_on_my )
{
    comm.newNodeCb("asdf",11,10,"asdffff","asdfqqqq", false);

    auto msg = GetProtobuf(false);
    const size_t size = msg->ByteSize();
    auto data = boost::make_shared<char[]>(size);
    msg->SerializeWithCachedSizesToArray(reinterpret_cast<google::protobuf::uint8*>(data.get()));
    rh->NewRemoteStatistics(11,data,size);

    rh->PerformOnMyStatisticsMessage([&](const std::unique_ptr<char[]> & data,
                                         const size_t size)
                                     {
                                         auto msg = Safir::make_unique<RawStatisticsMessage>();
                                         msg->ParseFromArray(data.get(),static_cast<int>(size));
                                         auto statistics = RawStatisticsCreator::Create(std::move(msg));
                                         CheckStatisticsCommon(statistics, 1);

                                         BOOST_CHECK(!statistics.IsDead(0));
                                         BOOST_CHECK(!statistics.HasRemoteStatistics(0));
                                     });
    rh->Stop();
    BOOST_CHECK_NO_THROW(ioService.run());
}

BOOST_AUTO_TEST_SUITE_END()


BOOST_AUTO_TEST_CASE(completion_signaller)
{
    int calls = 0;
    boost::shared_ptr<void> p(static_cast<void*>(nullptr), [&calls](void*){++calls;});
    BOOST_CHECK_EQUAL(p.use_count(), 1);
    BOOST_CHECK_EQUAL(calls, 0);
    p.reset();
    BOOST_CHECK_EQUAL(p.use_count(), 0);
    BOOST_CHECK_EQUAL(calls, 1);
    p.reset();
    BOOST_CHECK_EQUAL(p.use_count(), 0);
    BOOST_CHECK_EQUAL(calls, 1);
}


BOOST_AUTO_TEST_CASE(completion_signaller_more)
{
    int calls = 0;
    boost::shared_ptr<void> p(static_cast<void*>(nullptr), [&calls](void*){++calls;});
    BOOST_CHECK_EQUAL(p.use_count(), 1);
    BOOST_CHECK_EQUAL(calls, 0);
    {
        auto p2 = p;
        BOOST_CHECK_EQUAL(p.use_count(), 2);
        p.reset();
        BOOST_CHECK_EQUAL(p.use_count(), 0);
        BOOST_CHECK_EQUAL(p2.use_count(), 1);
        BOOST_CHECK_EQUAL(calls, 0);
    }
    BOOST_CHECK_EQUAL(calls, 1);
}
