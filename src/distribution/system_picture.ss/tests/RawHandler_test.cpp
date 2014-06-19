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
#include "../src/RawHandler.h"
#include <boost/thread.hpp>
#include <set>

#define BOOST_TEST_MODULE RawHandlerTest
#include <boost/test/unit_test.hpp>

class Communication
{
public:

   //Callbacks functions used in Communications public interface.
    typedef std::function<void(const std::string& name, int64_t nodeId, int64_t nodeTypeId, const std::string& controlAddress, const std::string& dataAddress)> NewNode;
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
        : rh (ioService,comm,"plopp",10,100,"asdfasdf","qwerty",GetNodeTypes())
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
        nodeTypes.insert(std::make_pair(10, NodeType(10,"mupp",false,boost::chrono::milliseconds(1),10)));
        nodeTypes.insert(std::make_pair(20, NodeType(20,"tupp",true,boost::chrono::seconds(1),22)));
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


BOOST_AUTO_TEST_SUITE_END()
