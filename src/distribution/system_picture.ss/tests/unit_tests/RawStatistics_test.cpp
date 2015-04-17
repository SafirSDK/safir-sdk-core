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
#include <Safir/Dob/Internal/RawStatistics.h>
#include "../../src/MessageWrapperCreators.h"
#include <Safir/Utilities/Internal/MakeUnique.h>
#include <boost/lexical_cast.hpp>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4244)
#pragma warning (disable: 4127)
#endif

#include "RawStatisticsMessage.pb.h"

#ifdef _MSC_VER
#pragma warning (pop)
#endif

#define BOOST_TEST_MODULE MyTest
#include <boost/test/unit_test.hpp>


using namespace Safir::Dob::Internal::SP;


std::unique_ptr<RawStatisticsMessage> GetProtobuf(bool empty,bool recursive)
{
    auto msg = Safir::make_unique<RawStatisticsMessage>();
    if (empty)
    {
        return std::move(msg);
    }

    msg->set_name("foo");
    msg->set_id(10);
    msg->set_node_type_id(190);
    msg->set_control_address("asdfasdf");
    msg->set_data_address("foobar");
    msg->set_election_id(91);
    msg->set_incarnation_id(1001);

    for (int i = 0; i < 5; ++i)
    {
        const auto iAsStr = boost::lexical_cast<std::string>(i);
        auto node = msg->add_node_info();

        node->set_name(iAsStr);
        node->set_id(i);
        node->set_node_type_id(i + 100);
        node->set_control_address(iAsStr + ":fobar!");
        node->set_data_address(iAsStr + ":flopp");
        node->set_is_dead(i%2==0);
        node->set_control_receive_count(i + 1000);
        node->set_control_retransmit_count(100 + i);
        node->set_data_receive_count(i + 5000);
        node->set_data_retransmit_count(500 + i);

        if (recursive)
        {
            auto remote = node->mutable_remote_statistics();

            remote->set_name("hodor" + iAsStr);
            remote->set_id(i);
            remote->set_node_type_id(190 + i);
            remote->set_control_address("asdfasdf" + iAsStr);
            remote->set_data_address("foobar" + iAsStr);
            remote->set_election_id(91 + i);
            remote->set_incarnation_id(1001 + i);

            for (int j = 0; j < 5; ++j)
            {
                const auto jAsStr = boost::lexical_cast<std::string>(j);
                auto rnode = remote->add_node_info();

                rnode->set_name(iAsStr +jAsStr);
                rnode->set_id(i*j);
                rnode->set_node_type_id(i*j + 100);
                rnode->set_control_address(iAsStr + ":fobar!" + jAsStr);
                rnode->set_data_address(iAsStr + ":flopp" + jAsStr);
                rnode->set_is_dead((i + j)%2==0);
                rnode->set_control_receive_count(i*j + 1000);
                rnode->set_control_retransmit_count(100 + i*j);
                rnode->set_data_receive_count(i*j + 5000);
                rnode->set_data_retransmit_count(500 + i*j);

                remote->mutable_more_dead_nodes()->Add(1000 + j);
            }

        }

    }

    msg->mutable_more_dead_nodes()->Add(1200);

    return std::move(msg);

}

BOOST_AUTO_TEST_CASE( test_invalid )
{
    const RawStatistics r;
    BOOST_CHECK(!r.Valid());
    RawStatistics r2 = r;
    BOOST_CHECK(!r.Valid());
    BOOST_CHECK(!r2.Valid());
}

BOOST_AUTO_TEST_CASE( test_empty )
{
    const auto r = RawStatisticsCreator::Create(GetProtobuf(true,false));
    BOOST_CHECK(r.Valid());
    RawStatistics r2;
    r2=r;
    BOOST_CHECK(r.Valid());
    BOOST_CHECK(r2.Valid());

    BOOST_CHECK(r.Size() == 0);
    BOOST_CHECK(r.MoreDeadNodesSize() == 0);
}


BOOST_AUTO_TEST_CASE( test_one_level )
{
    const auto r = RawStatisticsCreator::Create(GetProtobuf(false,false));
    BOOST_CHECK(r.Valid());
    BOOST_CHECK(r.Name() == "foo");
    BOOST_CHECK(r.Id() == 10);
    BOOST_CHECK(r.NodeTypeId() == 190);
    BOOST_CHECK(r.ControlAddress() == "asdfasdf");
    BOOST_CHECK(r.DataAddress() == "foobar");
    BOOST_CHECK(r.ElectionId() == 91);
    BOOST_CHECK(r.IncarnationId() == 1001);

    BOOST_CHECK(r.Size() == 5);

    for (int i = 0; i < 5; ++i)
    {
        BOOST_CHECK(r.Name(i) == boost::lexical_cast<std::string>(i));

        BOOST_CHECK(r.Id(i) == i);
        BOOST_CHECK(r.NodeTypeId(i) == i + 100);
        BOOST_CHECK(r.ControlAddress(i) == boost::lexical_cast<std::string>(i) + ":fobar!");
        BOOST_CHECK(r.DataAddress(i) == boost::lexical_cast<std::string>(i) + ":flopp");
        BOOST_CHECK(r.IsDead(i) == (i%2==0));
        BOOST_CHECK(r.ControlReceiveCount(i) == static_cast<uint32_t>(i + 1000));
        BOOST_CHECK(r.ControlRetransmitCount(i) == static_cast<uint32_t>(100 + i));
        BOOST_CHECK(r.DataReceiveCount(i) == static_cast<uint32_t>(i + 5000));
        BOOST_CHECK(r.DataRetransmitCount(i) == static_cast<uint32_t>(500 + i));
        BOOST_CHECK(!r.HasRemoteStatistics(i));
    }

    BOOST_CHECK_EQUAL(r.MoreDeadNodesSize(), 1);
    BOOST_CHECK_EQUAL(r.MoreDeadNodes(0), 1200);
}

BOOST_AUTO_TEST_CASE( test_two_levels )
{
    const auto r = RawStatisticsCreator::Create(GetProtobuf(false,true));
    BOOST_CHECK(r.Size() == 5);

    for (int i = 0; i < 5; ++i)
    {
        const auto iAsStr = boost::lexical_cast<std::string>(i);

        BOOST_CHECK(r.HasRemoteStatistics(i));
        const auto remote = r.RemoteStatistics(i);
        BOOST_CHECK(remote.Valid());
        BOOST_CHECK(remote.Name() == "hodor" + iAsStr);
        BOOST_CHECK(remote.Id() == i);
        BOOST_CHECK(remote.NodeTypeId() == 190 + i);
        BOOST_CHECK(remote.ControlAddress() == "asdfasdf" + iAsStr);
        BOOST_CHECK(remote.DataAddress() == "foobar" + iAsStr);
        BOOST_CHECK(remote.ElectionId() == 91 + i);
        BOOST_CHECK(remote.IncarnationId() == 1001 + i);

        BOOST_CHECK(remote.Size() == 5);
        BOOST_CHECK_EQUAL(remote.MoreDeadNodesSize(), 5);
        for (int j = 0; j < 5; ++j)
        {
            const auto jAsStr = boost::lexical_cast<std::string>(j);

            BOOST_CHECK(remote.Name(j) == iAsStr + jAsStr);

            BOOST_CHECK(remote.Id(j) == i*j);
            BOOST_CHECK(remote.NodeTypeId(j) == i*j + 100);
            BOOST_CHECK(remote.ControlAddress(j) == iAsStr + ":fobar!" + jAsStr);
            BOOST_CHECK(remote.DataAddress(j) == iAsStr + ":flopp" + jAsStr);
            BOOST_CHECK(remote.IsDead(j) == ((i+j)%2==0));
            BOOST_CHECK(remote.ControlReceiveCount(j) == static_cast<uint32_t>(i*j + 1000));
            BOOST_CHECK(remote.ControlRetransmitCount(j) == static_cast<uint32_t>(100 + i*j));
            BOOST_CHECK(remote.DataReceiveCount(j) == static_cast<uint32_t>(i*j + 5000));
            BOOST_CHECK(remote.DataRetransmitCount(j) == static_cast<uint32_t>(500 + i*j));
            BOOST_CHECK(!remote.HasRemoteStatistics(j));
            BOOST_CHECK_EQUAL(remote.MoreDeadNodes(j), 1000 + j);
        }
    }
    BOOST_CHECK_EQUAL(r.MoreDeadNodesSize(), 1);
    BOOST_CHECK_EQUAL(r.MoreDeadNodes(0), 1200);
}
