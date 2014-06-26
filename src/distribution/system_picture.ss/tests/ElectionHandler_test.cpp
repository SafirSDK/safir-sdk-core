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
#include "../src/ElectionHandler.h"
#include "../src/MessageWrapperCreators.h"
#include <Safir/Utilities/Internal/MakeUnique.h>
#include <boost/thread.hpp>
#include <boost/thread/mutex.hpp>
#include <memory>


#ifdef _MSC_VER
#  pragma warning (push)
#  pragma warning (disable: 4244)
#  pragma warning (disable: 4127)
#endif

#include "NodeStatisticsMessage.pb.h"

#ifdef _MSC_VER
#  pragma warning (pop)
#endif


#define BOOST_TEST_MODULE ElectionHandlerTest
#include <boost/test/unit_test.hpp>


//We need thread safe variants for some of the tests that use multiple threads in the ioService.
boost::mutex testMtx;
#define SAFE_BOOST_CHECK(p) {boost::lock_guard<boost::mutex> lck(testMtx); BOOST_CHECK(p);}
#define SAFE_BOOST_CHECK_NE(L, R) {boost::lock_guard<boost::mutex> lck(testMtx); BOOST_CHECK_NE(L, R);}
#define SAFE_BOOST_CHECK_EQUAL(L, R) {boost::lock_guard<boost::mutex> lck(testMtx); BOOST_CHECK_EQUAL(L, R);}
#define SAFE_BOOST_TEST_MESSAGE(m) {boost::lock_guard<boost::mutex> lck(testMtx); BOOST_TEST_MESSAGE(m);}

class Communication
{
public:
    explicit Communication(boost::asio::io_service& ioService_, const int64_t id_)
        : ioService(ioService_)
        , id(id_)
    {
        allComms.insert(std::make_pair(id_,this));
    }

   //Callbacks functions used in Communications public interface.
    typedef std::function<void(const std::string& name, int64_t nodeId, int64_t nodeTypeId, const std::string& controlAddress, const std::string& dataAddress)> NewNode;
    typedef std::function<void(int64_t fromNodeId)> GotReceiveFrom;
    typedef std::function<void(int64_t toNodeId)> RetransmitTo;
    typedef std::function<void(int64_t fromNodeId, int64_t fromNodeType, const boost::shared_ptr<char[]>& data, size_t size)> ReceiveData;

    void SetDataReceiver(const ReceiveData& callback, const int64_t /*dataTypeIdentifier*/)
    {
        receiveDataCb = callback;
    }


    bool SendToNode(const int64_t nodeId, 
                    const int64_t /*nodeTypeId*/, 
                    const boost::shared_ptr<char[]>& data,
                    const size_t size, 
                    const int64_t /*dataTypeIdentifier*/,
                    const bool ack)
    {
        SAFE_BOOST_TEST_MESSAGE("SendToNode " << id << " -> " << nodeId);
        SAFE_BOOST_CHECK(ack);

        SAFE_BOOST_CHECK_NE(nodeId, id); //not to ourselves!
        for (auto&& comm: allComms)
        {
            //send to all but ourselves
            if (comm.second->id == nodeId)
            {
                SAFE_BOOST_TEST_MESSAGE(" - Sending to node " << comm.second->id);
                comm.second->receiveDataCb(id,10,data,size);
            }
        }

        return true;
    }

    bool SendToNodeType(const int64_t nodeTypeId,
                        const boost::shared_ptr<char[]>& data, 
                        const size_t size, 
                        const int64_t /*dataTypeIdentifier*/, 
                        const bool ack)
    {
        SAFE_BOOST_TEST_MESSAGE("SendToNodeType from node " << id << " -> node type " << nodeTypeId);
        SAFE_BOOST_CHECK(ack);


        //All nodes are of the same type


        for (auto&& comm: allComms)
        {
            //send to all but ourselves
            if (comm.second->id != id)
            {
                SAFE_BOOST_TEST_MESSAGE(" - Sending to node " << comm.second->id);
                comm.second->receiveDataCb(id,10,data,size);
            }
        }

        return true;
    }

    boost::asio::io_service& ioService;
    const int64_t id;

    ReceiveData receiveDataCb;
    

    static void ClearAll() {allComms.clear();}
    static std::map<int64_t,Communication*>  allComms;
};

//static initialization
std::map<int64_t,Communication*> Communication::allComms;

using namespace Safir::Dob::Internal::SP;

struct Node
{
    Node(boost::asio::io_service& ioService, const int64_t id_)
        : id(id_)
        , comm(ioService,id)
        , eh(ioService,
             comm,
             id_,
             GetNodeTypes(),
             "not used",
             [this](const int64_t nodeId, 
                    const int64_t electionId)
             {
                 ElectionComplete(nodeId,electionId);
             })
        , electedNode(0)
        , electionId(0)
    {
        SAFE_BOOST_TEST_MESSAGE("Create node " << id);
    }
    
    ~Node()
    {
        SAFE_BOOST_TEST_MESSAGE("Destroy node");
    }

    void ElectionComplete(const int64_t nodeId_,
                          const int64_t electionId_)
    {
        SAFE_BOOST_TEST_MESSAGE("ElectionComplete for node " << id);
        electedNode = nodeId_;
        electionId = electionId_;
    }

    static std::map<int64_t, NodeType> GetNodeTypes()
    {
        std::map<int64_t, NodeType> nodeTypes;
        nodeTypes.insert(std::make_pair(10, NodeType(10,"mupp",false,boost::chrono::milliseconds(1),10,boost::chrono::milliseconds(1))));
        nodeTypes.insert(std::make_pair(20, NodeType(20,"tupp",true,boost::chrono::hours(1),22,boost::chrono::hours(1))));
        return nodeTypes;
    }

    const int64_t id;
    Communication comm;

    ElectionHandlerBasic<Communication> eh;

    int64_t electedNode;
    int64_t electionId;
    
};

struct Fixture
{
    Fixture()
    {
        SAFE_BOOST_TEST_MESSAGE( "setup fixture" );
        Communication::ClearAll();

        nodes.push_back(Safir::make_unique<Node>(ioService,nextNodeId));
        ++nextNodeId;
    }
    
    ~Fixture()
    {
        SAFE_BOOST_TEST_MESSAGE( "teardown fixture" );

        if (nodes.size() == 1)
        {
            if(nodes[0]->eh.IsElected())
            {
                SAFE_BOOST_CHECK_EQUAL(nodes[0]->electedNode, 10);
                SAFE_BOOST_CHECK(nodes[0]->electionId != 0);
            }

            
        }
    }

    void AddNode()
    {
        nodes.push_back(Safir::make_unique<Node>(ioService,nextNodeId));
        ++nextNodeId;
    }

    void SendNodesChanged()
    {
        auto msg = Safir::make_unique<NodeStatisticsMessage>();
        for(auto&& node: nodes)
        {
            auto ni = msg->add_node_info();
            ni->set_id(node->id);
        }
        
        const auto raw = RawStatisticsCreator::Create(std::move(msg));
        for(auto&& node: nodes)
        {
            node->eh.NodesChanged(raw);
        }        
    }

    boost::asio::io_service ioService;
    
    int nextNodeId = 10;
    std::vector<std::unique_ptr<Node>> nodes;
};

BOOST_FIXTURE_TEST_SUITE( s, Fixture )

BOOST_AUTO_TEST_CASE( start_stop )
{
    BOOST_CHECK(nodes[0]->comm.receiveDataCb != nullptr);
    BOOST_CHECK(!nodes[0]->eh.IsElected());

    //this is kind of an implementation detail...
    BOOST_CHECK(nodes[0]->eh.IsElected(std::numeric_limits<int64_t>::min()));

    nodes[0]->eh.Stop();
    ioService.run();

    //The stop call should ensure that the timer never elapses, so we don't get an "alone" election
    BOOST_CHECK(!nodes[0]->eh.IsElected());
}

BOOST_AUTO_TEST_CASE( elect_self )
{
    BOOST_CHECK(!nodes[0]->eh.IsElected());
    ioService.run();
    BOOST_CHECK(nodes[0]->eh.IsElected());
    BOOST_CHECK(nodes[0]->eh.IsElected(10));
    BOOST_CHECK(!nodes[0]->eh.IsElected(20));
}

BOOST_AUTO_TEST_CASE( two_nodes )
{
    AddNode();
    SendNodesChanged();
    BOOST_CHECK(!nodes[0]->eh.IsElected());
    BOOST_CHECK(!nodes[1]->eh.IsElected());
    ioService.run();
    BOOST_CHECK(!nodes[0]->eh.IsElected());
    BOOST_CHECK(nodes[1]->eh.IsElected());
}

BOOST_AUTO_TEST_CASE( three_nodes )
{
    AddNode();
    AddNode();
    SendNodesChanged();
    BOOST_CHECK(!nodes[0]->eh.IsElected());
    BOOST_CHECK(!nodes[1]->eh.IsElected());
    BOOST_CHECK(!nodes[2]->eh.IsElected());
    ioService.run();
    BOOST_CHECK(!nodes[0]->eh.IsElected());
    BOOST_CHECK(!nodes[1]->eh.IsElected());
    BOOST_CHECK(nodes[2]->eh.IsElected());
}

BOOST_AUTO_TEST_CASE( lots_of_nodes )
{
    const int numNodes = 1000;

    //remember that one node is automatically added by the fixture
    for (int i = 0; i < numNodes - 1 ; ++i)
    {
        AddNode();
    }
    SendNodesChanged();
    for (int i = 0; i < numNodes; ++i)
    {
        SAFE_BOOST_CHECK(!nodes[i]->eh.IsElected());
    }

    
    boost::thread_group threads;
    for (int i = 0; i < 9; ++i)
    {
        threads.create_thread([this]{ioService.run();});
    }

    ioService.run();
    threads.join_all();


    for (int i = 0; i < numNodes - 1; ++i)
    {
        SAFE_BOOST_CHECK(!nodes[i]->eh.IsElected());
    }

    SAFE_BOOST_CHECK(nodes[numNodes - 1]->eh.IsElected());
}


BOOST_AUTO_TEST_SUITE_END()
