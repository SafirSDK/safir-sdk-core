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
#include "../../src/ElectionHandler.h"
#include "../../src/MessageWrapperCreators.h"
#include <Safir/Utilities/Internal/MakeUnique.h>
#include <boost/thread.hpp>
#include <boost/thread/mutex.hpp>
#include <memory>


#ifdef _MSC_VER
#  pragma warning (push)
#  pragma warning (disable: 4244)
#  pragma warning (disable: 4127)
#endif

#include "RawStatisticsMessage.pb.h"

#ifdef _MSC_VER
#  pragma warning (pop)
#endif

#define BOOST_TEST_MODULE ElectionHandlerTest
#include <boost/test/unit_test.hpp>


//We need thread safe variants for some of the tests that use multiple threads in the ioService.
boost::mutex testMtx;
#define SAFE_BOOST_CHECK(p) {boost::lock_guard<boost::mutex> lck(testMtx); BOOST_CHECK(p);}
#define SAFE_BOOST_FAIL(s) {boost::lock_guard<boost::mutex> lck(testMtx); BOOST_FAIL(s);}
#define SAFE_BOOST_CHECK_NE(L, R) {boost::lock_guard<boost::mutex> lck(testMtx); BOOST_CHECK_NE(L, R);}
#define SAFE_BOOST_CHECK_EQUAL(L, R) {boost::lock_guard<boost::mutex> lck(testMtx); BOOST_CHECK_EQUAL(L, R);}
#define SAFE_BOOST_TEST_MESSAGE(m) {boost::lock_guard<boost::mutex> lck(testMtx); BOOST_TEST_MESSAGE(m);}

class Communication;

/** this class is just a singleton containing all communication instances */
class Connector
{
public:
    static Connector& Instance()
    {
        static Connector instance;
        return instance;
    }

    void Add(const int64_t id, Communication* comm)
    {
        boost::lock_guard<boost::recursive_mutex> lck(m_mutex);
        m_allComms.insert(std::make_pair(id,comm));
    }

    void Reset()
    {
        boost::lock_guard<boost::recursive_mutex> lck(m_mutex);
        m_allComms.clear();
        m_overflows = false;
    }

    void Remove(const int64_t id)
    {
        boost::lock_guard<boost::recursive_mutex> lck(m_mutex);
        m_allComms.find(id)->second = nullptr;
    }

    bool SendTo(const int64_t nodeId,
                const int64_t sender,
                const boost::shared_ptr<char[]>& data,
                const size_t size);

    bool SendAll(const int64_t sender,
                 const boost::shared_ptr<char[]>& data,
                 const size_t size);

    void EnableOverflows() {m_overflows = true;}
private:
    Connector()
        : m_overflows(false)
    {

    }

    ~Connector() {}

    bool DeliverMessage() const
    {
        if (m_overflows)
        {
            return rand() % 4 != 0; // 75% will be delivered
        }
        else
        {
            return true;
        }
    }

    std::map<int64_t,Communication*>  m_allComms;
    boost::recursive_mutex m_mutex;
    bool m_overflows;
};

class Communication
{
public:
    explicit Communication(boost::asio::io_service& ioService_, const int64_t id_)
        : ioService(ioService_)
        , id(id_)
    {
        Connector::Instance().Add(id,this);
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


    bool Send(const int64_t nodeId,
              const int64_t nodeTypeId,
              const boost::shared_ptr<char[]>& data,
              const size_t size,
              const int64_t /*dataTypeIdentifier*/,
              const bool ack)
    {
        if (nodeId == 0)
        {
            SAFE_BOOST_TEST_MESSAGE("SendToNodeType from node " << id << " -> node type " << nodeTypeId);
        }
        else
        {
            SAFE_BOOST_TEST_MESSAGE("SendToNode " << id << " -> " << nodeId);
        }

        SAFE_BOOST_CHECK(ack);

        SAFE_BOOST_CHECK_NE(nodeId, id); //not to ourselves!

        if (nodeId == 0)
        {
            return Connector::Instance().SendAll(id,data,size);
        }
        else
        {
            return Connector::Instance().SendTo(nodeId,id,data,size);
        }
    }

    boost::asio::io_service& ioService;
    const int64_t id;

    ReceiveData receiveDataCb;

};



//static initialization
//std::map<int64_t,Communication*> Communication::allComms;
//bool Communication::overflows = false;


bool Connector::SendTo(const int64_t nodeId,
                       const int64_t sender,
                       const boost::shared_ptr<char[]>& data,
                       const size_t size)
{
    if (!DeliverMessage())
    {
        return false;
    }

    boost::lock_guard<boost::recursive_mutex> lck(m_mutex);

    for (auto&& comm: m_allComms)
    {
        if (comm.second == nullptr)
        {
            continue;
        }

        //send to the intended node
        if (comm.first == nodeId)
        {
            SAFE_BOOST_TEST_MESSAGE(" - Sending to node " << comm.first);
            comm.second->receiveDataCb(sender,10,data,size);
            return true;
        }
    }

    SAFE_BOOST_FAIL("Invalid node id!");
    return true;
}


bool Connector::SendAll(const int64_t sender,
                        const boost::shared_ptr<char[]>& data,
                        const size_t size)
{
    if (!DeliverMessage())
    {
        return false;
    }

    boost::lock_guard<boost::recursive_mutex> lck(m_mutex);

    //All nodes are of the same type

    for (auto&& comm: m_allComms)
    {
        if (comm.second == nullptr)
        {
            continue;
        }

        //send to all but ourselves
        if (comm.first != sender)
        {
            SAFE_BOOST_TEST_MESSAGE(" - Sending to node " << sender);
            comm.second->receiveDataCb(sender,10,data,size);
        }
    }

    return true;
}


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
        SAFE_BOOST_TEST_MESSAGE("ElectionComplete for node " << id << ". Elected node " << nodeId_);
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

    bool removed = false;

};

struct Fixture
{
    Fixture()
    {
        SAFE_BOOST_TEST_MESSAGE("setup fixture");

        Connector::Instance().Reset();

        for (int i = 0; i < boost::unit_test::framework::master_test_suite().argc; ++i)
        {
            if (std::string(boost::unit_test::framework::master_test_suite().argv[i]) == "--overflows")
            {
                SAFE_BOOST_TEST_MESSAGE("Communication stub will generate overflows");
                Connector::Instance().EnableOverflows();
            }
        }


        nodes.push_back(Safir::make_unique<Node>(ioService,nextNodeId));
        ++nextNodeId;
    }

    ~Fixture()
    {
        SAFE_BOOST_TEST_MESSAGE("teardown fixture");

        if (nodes.size() == 1)
        {
            if(nodes[0]->eh.IsElected())
            {
                SAFE_BOOST_CHECK_EQUAL(nodes[0]->electedNode, 10);
                SAFE_BOOST_CHECK_NE(nodes[0]->electionId, 0);
            }
            return;
        }

        //check that all nodes agree on who's elected

        const auto electedNode = nodes[0]->electedNode;
        const auto electionId = nodes[0]->electionId;

        for(auto&& node: nodes)
        {
            if (node->removed)
            {
                continue;
            }

            SAFE_BOOST_CHECK_EQUAL(node->electedNode, electedNode)
            SAFE_BOOST_CHECK_EQUAL(node->electionId, electionId)
        }

    }

    void AddNode()
    {
        nodes.push_back(Safir::make_unique<Node>(ioService,nextNodeId));
        ++nextNodeId;
    }

    void RemoveNode(const size_t which)
    {
        SAFE_BOOST_TEST_MESSAGE("Removing node " << which);
        Connector::Instance().Remove(nodes[which]->id);
        //nodes[which] = nullptr;
        nodes[which]->removed = true;
    }

    void SendNodesChanged()
    {
        auto msg = Safir::make_unique<RawStatisticsMessage>();
        for(auto&& node: nodes)
        {
            auto ni = msg->add_node_info();
            ni->set_id(node->id);

            if (node->removed)
            {
                ni->set_is_dead(true);
            }
        }

        boost::shared_ptr<void> dummy;
        const auto raw = RawStatisticsCreator::Create(std::move(msg));
        for(auto&& node: nodes)
        {
            if (node->removed)
            {
                continue;
            }

            node->eh.NodesChanged(raw,dummy);
        }
    }

    void RunIoService(int numThreads = 1)
    {
        ioService.reset();

        boost::thread_group threads;
        for (int i = 0; i < numThreads - 1; ++i)
        {
            threads.create_thread([this]{ioService.run();});
        }

        ioService.run();
        threads.join_all();
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
    RunIoService();

    //The stop call should ensure that the timer never elapses, so we don't get an "alone" election
    BOOST_CHECK(!nodes[0]->eh.IsElected());
}

BOOST_AUTO_TEST_CASE( elect_self )
{
    BOOST_CHECK(!nodes[0]->eh.IsElected());
    RunIoService();
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
    RunIoService();
    BOOST_CHECK(!nodes[0]->eh.IsElected());
    BOOST_CHECK(nodes[1]->eh.IsElected());
}

BOOST_AUTO_TEST_CASE( two_nodes_remove_elected )
{
    AddNode();
    SendNodesChanged();
    BOOST_CHECK(!nodes[0]->eh.IsElected());
    BOOST_CHECK(!nodes[1]->eh.IsElected());
    RunIoService();
    BOOST_CHECK(!nodes[0]->eh.IsElected());
    BOOST_CHECK(nodes[1]->eh.IsElected());

    RemoveNode(1);
    SendNodesChanged();
    BOOST_CHECK(!nodes[0]->eh.IsElected());

    RunIoService();
    BOOST_CHECK(nodes[0]->eh.IsElected());
}

BOOST_AUTO_TEST_CASE( three_nodes )
{
    AddNode();
    AddNode();
    SendNodesChanged();
    BOOST_CHECK(!nodes[0]->eh.IsElected());
    BOOST_CHECK(!nodes[1]->eh.IsElected());
    BOOST_CHECK(!nodes[2]->eh.IsElected());
    RunIoService();
    BOOST_CHECK(!nodes[0]->eh.IsElected());
    BOOST_CHECK(!nodes[1]->eh.IsElected());
    BOOST_CHECK(nodes[2]->eh.IsElected());
}

BOOST_AUTO_TEST_CASE( three_nodes_with_reelection )
{
    AddNode();
    SendNodesChanged();
    BOOST_CHECK(!nodes[0]->eh.IsElected());
    BOOST_CHECK(!nodes[1]->eh.IsElected());
    RunIoService();
    BOOST_CHECK(!nodes[0]->eh.IsElected());
    BOOST_CHECK(nodes[1]->eh.IsElected());

    AddNode();
    SendNodesChanged();
    BOOST_CHECK(!nodes[0]->eh.IsElected());
    BOOST_CHECK(nodes[1]->eh.IsElected());
    BOOST_CHECK(!nodes[2]->eh.IsElected());

    RunIoService();
    BOOST_CHECK(!nodes[0]->eh.IsElected());
    BOOST_CHECK(!nodes[1]->eh.IsElected());
    BOOST_CHECK(nodes[2]->eh.IsElected());
}

BOOST_AUTO_TEST_CASE( three_nodes_remove_unelected )
{
    AddNode();
    AddNode();
    SendNodesChanged();
    BOOST_CHECK(!nodes[0]->eh.IsElected());
    BOOST_CHECK(!nodes[1]->eh.IsElected());
    BOOST_CHECK(!nodes[2]->eh.IsElected());
    RunIoService();
    BOOST_CHECK(!nodes[0]->eh.IsElected());
    BOOST_CHECK(!nodes[1]->eh.IsElected());
    BOOST_CHECK(nodes[2]->eh.IsElected());

    RemoveNode(1);
    SendNodesChanged();
    BOOST_CHECK(!nodes[0]->eh.IsElected());
    BOOST_CHECK(nodes[2]->eh.IsElected());

    RunIoService();
    BOOST_CHECK(!nodes[0]->eh.IsElected());
    BOOST_CHECK(nodes[2]->eh.IsElected());
}


BOOST_AUTO_TEST_CASE( three_nodes_remove_elected )
{
    AddNode();
    AddNode();
    SendNodesChanged();
    BOOST_CHECK(!nodes[0]->eh.IsElected());
    BOOST_CHECK(!nodes[1]->eh.IsElected());
    BOOST_CHECK(!nodes[2]->eh.IsElected());
    RunIoService();
    BOOST_CHECK(!nodes[0]->eh.IsElected());
    BOOST_CHECK(!nodes[1]->eh.IsElected());
    BOOST_CHECK(nodes[2]->eh.IsElected());

    RemoveNode(2);
    SendNodesChanged();
    BOOST_CHECK(!nodes[0]->eh.IsElected());
    BOOST_CHECK(!nodes[1]->eh.IsElected());

    RunIoService();
    BOOST_CHECK(!nodes[0]->eh.IsElected());
    BOOST_CHECK(nodes[1]->eh.IsElected());
}


BOOST_AUTO_TEST_CASE( lots_of_nodes )
{
    //windows debug builds run out of memory when using too many nodes :-)
#if defined (_MSC_VER) && !defined(NDEBUG)
    const int numNodes = 100;
#else
    const int numNodes = 1000;
#endif

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

    RunIoService(10);

    for (int i = 0; i < numNodes - 1; ++i)
    {
        SAFE_BOOST_CHECK(!nodes[i]->eh.IsElected());
    }

    SAFE_BOOST_CHECK(nodes[numNodes - 1]->eh.IsElected());
}


BOOST_AUTO_TEST_CASE( lots_of_nodes_remove_some )
{
    //windows debug builds run out of memory when using too many nodes :-)
#if defined (_MSC_VER) && !defined(NDEBUG)
    const int numNodes = 100;
#else
    const int numNodes = 1000;
#endif

    //remember that one node is automatically added by the fixture
    for (int i = 0; i < numNodes - 1; ++i)
    {
        AddNode();
    }

    SendNodesChanged();

    for (int i = 0; i < numNodes; ++i)
    {
        SAFE_BOOST_CHECK(!nodes[i]->eh.IsElected());
    }

    RunIoService(10);

    for (int i = 0; i < numNodes - 1; ++i)
    {
        SAFE_BOOST_CHECK(!nodes[i]->eh.IsElected());
    }

    SAFE_BOOST_CHECK(nodes[numNodes - 1]->eh.IsElected());


    //remove some nodes (e.g. 900 to 1000)
    for (int i = numNodes - numNodes / 10; i < numNodes; ++i)
    {
        RemoveNode(i);
    }


    //remove some more (e.g. 250 to 500)
    for (int i = numNodes / 4; i < numNodes / 2; ++i)
    {
        RemoveNode(i);
    }

    SendNodesChanged();

    RunIoService(10);

    SAFE_BOOST_CHECK(nodes[numNodes - numNodes/10 - 1]->eh.IsElected());

}

BOOST_AUTO_TEST_CASE( remove_during_election )
{
    const int numNodes = 100;

    //remember that one node is automatically added by the fixture
    for (int i = 0; i < numNodes - 1 ; ++i)
    {
        AddNode();
    }

    SendNodesChanged();


    boost::thread_group threads;
    for (int i = 0; i < 6; ++i)
    {
        threads.create_thread([this]{ioService.run();});
    }

    //will keep node 0 and 1
    for (int i = numNodes - 1; i > 1; --i)
    {
        RemoveNode(i);
        SendNodesChanged();
    }

    threads.join_all();

    SAFE_BOOST_CHECK(!nodes[0]->eh.IsElected());
    SAFE_BOOST_CHECK(nodes[0]->eh.IsElected(11));
    SAFE_BOOST_CHECK(nodes[1]->eh.IsElected());
    SAFE_BOOST_CHECK(nodes[1]->eh.IsElected(11));
    SAFE_BOOST_CHECK(!nodes[1]->removed);
    SAFE_BOOST_CHECK(nodes[2]->removed);
}


BOOST_AUTO_TEST_SUITE_END()
