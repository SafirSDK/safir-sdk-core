/******************************************************************************
*
* Copyright Saab AB, 2014, 2022 (http://safirsdkcore.com)
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
#include <boost/thread/mutex.hpp>
#include <boost/thread/barrier.hpp>
#include <memory>
#include <iostream>
#include <time.h>

#ifdef _MSC_VER
#  pragma warning (push)
#  pragma warning (disable: 4127)
#  pragma warning (disable: 4100)
#endif

#include "RawStatisticsMessage.pb.h"
#include <boost/thread.hpp>

#ifdef _MSC_VER
#  pragma warning (pop)
#endif

#define BOOST_TEST_MODULE ElectionHandlerTest
#include <boost/test/unit_test.hpp>

//We need thread safe variants for some of the tests that use multiple threads in the ioService.
boost::mutex testMtx;
#define SAFE_BOOST_CHECK(p) {boost::lock_guard<boost::mutex> lck123(testMtx); BOOST_CHECK(p);}
#define SAFE_BOOST_FAIL(s) {boost::lock_guard<boost::mutex> lck123(testMtx); lllog(9) << s << std::endl; BOOST_FAIL(s);}
#define SAFE_BOOST_CHECK_NE(L, R) {boost::lock_guard<boost::mutex> lck123(testMtx); BOOST_CHECK_NE(L, R);}
#define SAFE_BOOST_CHECK_EQUAL(L, R) {boost::lock_guard<boost::mutex> lck123(testMtx); BOOST_CHECK_EQUAL(L, R);}
#define SAFE_BOOST_TEST_MESSAGE(m) {boost::lock_guard<boost::mutex> lck123(testMtx); lllog(9) << m << std::endl; BOOST_TEST_MESSAGE(m);}

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
                const Safir::Utilities::Internal::SharedConstCharArray& data,
                const size_t size);

    bool SendAll(const int64_t sender,
                 const Safir::Utilities::Internal::SharedConstCharArray& data,
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
    typedef std::function<void(int64_t fromNodeId,
                               int64_t fromNodeType,
                               const char* const data,
                               size_t size)> ReceiveData;

    typedef std::function<char*(size_t size)> Allocator;
    typedef std::function<void(const char *)> DeAllocator;

    void SetDataReceiver(const ReceiveData& callback, const int64_t /*dataTypeIdentifier*/, const Allocator& alloc, const DeAllocator& dealloc)
    {
        receiveDataCb = callback;
        allocator = alloc;
        deallocator = dealloc;
    }


    bool Send(const int64_t nodeId,
              const int64_t nodeTypeId,
              const Safir::Utilities::Internal::SharedConstCharArray& data,
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
    Allocator allocator;
    DeAllocator deallocator;

};


bool Connector::SendTo(const int64_t nodeId,
                       const int64_t sender,
                       const Safir::Utilities::Internal::SharedConstCharArray& data,
                       const size_t size)
{
    if (!DeliverMessage())
    {
        return false;
    }

    boost::lock_guard<boost::recursive_mutex> lck(m_mutex);


    for (auto comm = m_allComms.cbegin(); comm != m_allComms.cend(); ++comm)
    {
        //send to the intended node
        if (comm->first == nodeId)
        {
            if (comm->second == nullptr)
            {
                SAFE_BOOST_TEST_MESSAGE(" - Not sending to node " << comm->first << " since it has been removed");
                return true;
            }

            SAFE_BOOST_TEST_MESSAGE(" - Sending to node " << comm->first);
            char* dataCopy = comm->second->allocator(size);
            memcpy(dataCopy,data.get(),size);
            comm->second->receiveDataCb(sender,10,dataCopy,size);
            return true;
        }
    }

    SAFE_BOOST_FAIL("SendTo from node id " << sender << " to invalid node id " << nodeId);
    return true;
}


bool Connector::SendAll(const int64_t sender,
                        const Safir::Utilities::Internal::SharedConstCharArray& data,
                        const size_t size)
{
    if (!DeliverMessage())
    {
        return false;
    }

    boost::lock_guard<boost::recursive_mutex> lck(m_mutex);

    //All nodes are of the same type


    for (auto comm = m_allComms.cbegin(); comm != m_allComms.cend(); ++comm)
    {
        if (comm->second == nullptr)
        {
            continue;
        }

        //send to all but ourselves
        if (comm->first != sender)
        {
            SAFE_BOOST_TEST_MESSAGE(" - Sending to node " << sender);
            char* dataCopy = comm->second->allocator(size);
            memcpy(dataCopy,data.get(),size);
            comm->second->receiveDataCb(sender,10,dataCopy,size);
        }
    }

    return true;
}


using namespace Safir::Dob::Internal::SP;

struct Node
{
    Node(boost::asio::io_service& ioService, const int64_t id_, const int64_t nodeTypeId_)
        : id(id_)
        , strand(ioService)
        , nodeTypeId(nodeTypeId_)
        , comm(ioService,id)
        , electedNode(0)
        , electionId(0)
        , incarnationId(0)
        , removed(false)
    {
        eh.reset(new ElectionHandlerBasic<Communication>(std::to_wstring(id) + L": ",
                                                         strand,
                                                         comm,
                                                         id_,
                                                         nodeTypeId_,
                                                         GetNodeTypes(),
                                                         boost::chrono::seconds(0), //use auto aloneTimeout
                                                         "not used",
                                                         [this](const int64_t nodeId,
                                                                const int64_t electionId)
                                                         {
                                                             ElectionComplete(nodeId,electionId);
                                                         },
                                                         [this](const int64_t incarnationId_)
                                                         {
                                                             BOOST_CHECK_NE(incarnationId_, 0);
                                                             if (incarnationId == 0)
                                                             {
                                                                 incarnationId = incarnationId_;
                                                             }
                                                         }));

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
        std::vector<boost::chrono::steady_clock::duration> retryTimeouts;
        retryTimeouts.push_back(boost::chrono::milliseconds(1));
        retryTimeouts.push_back(boost::chrono::milliseconds(2));

        std::map<int64_t, NodeType> nodeTypes;
        nodeTypes.insert(std::make_pair(10, NodeType(10,"mupp",false,boost::chrono::milliseconds(1),10,retryTimeouts)));

        nodeTypes.insert(std::make_pair(15, NodeType(15,"flopp",true,boost::chrono::milliseconds(1),10,retryTimeouts)));

        retryTimeouts.clear();
        retryTimeouts.push_back(boost::chrono::hours(1));
        retryTimeouts.push_back(boost::chrono::hours(2));

        nodeTypes.insert(std::make_pair(20, NodeType(20,"tupp",true,boost::chrono::hours(1),22,retryTimeouts)));
        return nodeTypes;
    }

    const int64_t id;
    boost::asio::io_service::strand strand;

    int64_t nodeTypeId;
    Communication comm;
    std::unique_ptr<ElectionHandlerBasic<Communication>> eh;

    int64_t electedNode;
    int64_t electionId;

    int64_t incarnationId;

    bool removed;

};

template <int64_t ownNodeType>
struct Fixture
{
    Fixture()
        : nextNodeId(10)
    {
        SAFE_BOOST_TEST_MESSAGE("Set up fixture");
        //const auto seed = static_cast<unsigned int>(time(nullptr));
        const auto seed = static_cast<unsigned int>(1681462503U);
        std::wcout << "Using random seed " << seed << std::endl;
        srand(seed);
        Connector::Instance().Reset();

        SAFE_BOOST_TEST_MESSAGE("Connector reset");

        for (int i = 0; i < boost::unit_test::framework::master_test_suite().argc; ++i)
        {
            if (std::string(boost::unit_test::framework::master_test_suite().argv[i]) == "--overflows")
            {
                SAFE_BOOST_TEST_MESSAGE("Communication stub will generate overflows");
                Connector::Instance().EnableOverflows();
            }
        }


        nodes.push_back(Safir::make_unique<Node>(ioService,nextNodeId,ownNodeType));
        ++nextNodeId;

        SAFE_BOOST_TEST_MESSAGE("Fixture setup complete");
    }

    ~Fixture()
    {
        SAFE_BOOST_TEST_MESSAGE("teardown fixture");
        if (std::current_exception() != nullptr)
        {
            SAFE_BOOST_TEST_MESSAGE("Exception is being thrown, not performing any checks");
            return;
        }

        if (nodes.size() == 1)
        {
            if(nodes.at(0)->eh->IsElected())
            {
                SAFE_BOOST_CHECK_EQUAL(nodes.at(0)->electedNode, 10);
                SAFE_BOOST_CHECK_NE(nodes.at(0)->electionId, 0);
            }
            return;
        }

        //check that all nodes agree on who's elected

        const auto electedNode = nodes.at(0)->electedNode;
        const auto electionId = nodes.at(0)->electionId;

        for (auto node = nodes.cbegin(); node != nodes.cend(); ++node)
        {
            if ((*node)->removed || (*node)->eh->IsElectionDetached())
            {
                continue;
            }

            SAFE_BOOST_CHECK_EQUAL((*node)->electedNode, electedNode)
            SAFE_BOOST_CHECK_EQUAL((*node)->electionId, electionId)
        }

    }

    void AddNode()
    {
        nodes.push_back(Safir::make_unique<Node>(ioService,nextNodeId,10));
        ++nextNodeId;
    }

    void AddLightNode()
    {
        nodes.push_back(Safir::make_unique<Node>(ioService,nextNodeId,15));
        ++nextNodeId;
    }

    void RemoveNode(const size_t which)
    {
        SAFE_BOOST_TEST_MESSAGE("Removing node " << which);
        Connector::Instance().Remove(nodes.at(which)->id);
        nodes.at(which)->removed = true;
    }

    void SendNodesChanged(bool setIncarnationId)
    {
        std::shared_ptr<void> dummy;
        const auto nodeTypes = Node::GetNodeTypes();
        for (auto node = nodes.cbegin(); node != nodes.cend(); ++node)
        {
            if ((*node)->removed)
            {
                continue;
            }

            auto msg = Safir::make_unique<RawStatisticsMessage>();
            msg->set_id((*node)->id);
            msg->set_node_type_id((*node)->nodeTypeId);
            const bool isLight = nodeTypes.at((*node)->nodeTypeId).isLightNode;

            if (setIncarnationId)
            {
                msg->set_incarnation_id(111111);
            }

            for (auto node2 = nodes.cbegin(); node2 != nodes.cend(); ++node2)
            {
                const bool isLight2 = nodeTypes.at((*node2)->nodeTypeId).isLightNode;
                if (isLight && isLight2)
                {
                    continue;
                }
                auto ni = msg->add_node_info();
                ni->set_id((*node2)->id);
                ni->set_node_type_id((*node2)->nodeTypeId);
                if ((*node2)->removed)
                {
                    ni->set_is_dead(true);
                }
            }

            const auto raw = RawStatisticsCreator::Create(std::move(msg));

            (*node)->eh->NodesChanged(raw,dummy);
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

    int nextNodeId;
    std::vector<std::unique_ptr<Node>> nodes;
};

BOOST_FIXTURE_TEST_SUITE( self_is_normal, Fixture<10> )

BOOST_AUTO_TEST_CASE( start_stop )
{
    BOOST_CHECK(nodes.at(0)->comm.receiveDataCb != nullptr);
    BOOST_CHECK(!nodes.at(0)->eh->IsElected());

    //this is kind of an implementation detail...
    BOOST_CHECK(nodes.at(0)->eh->IsElected(std::numeric_limits<int64_t>::min()));

    nodes.at(0)->eh->Stop();
    RunIoService();

    //The stop call should ensure that the timer never elapses, so we don't get an "alone" election
    BOOST_CHECK(!nodes.at(0)->eh->IsElected());
}

BOOST_AUTO_TEST_CASE( elect_self )
{
    BOOST_CHECK(!nodes.at(0)->eh->IsElected());
    BOOST_CHECK(!nodes.at(0)->eh->IsElectionDetached());
    RunIoService();
    BOOST_CHECK(nodes.at(0)->eh->IsElected());
    BOOST_CHECK(nodes.at(0)->eh->IsElected(10));
    BOOST_CHECK(!nodes.at(0)->eh->IsElected(20));
    BOOST_CHECK(!nodes.at(0)->eh->IsElectionDetached());
    BOOST_CHECK_NE(nodes.at(0)->incarnationId,0);
}

BOOST_AUTO_TEST_CASE( two_nodes )
{
    AddNode();
    SendNodesChanged(true);
    BOOST_CHECK(!nodes.at(0)->eh->IsElected());
    BOOST_CHECK(!nodes.at(0)->eh->IsElectionDetached());
    BOOST_CHECK(!nodes.at(1)->eh->IsElected());
    BOOST_CHECK(!nodes.at(1)->eh->IsElectionDetached());
    RunIoService();
    BOOST_CHECK(!nodes.at(0)->eh->IsElected());
    BOOST_CHECK(!nodes.at(0)->eh->IsElectionDetached());
    BOOST_CHECK(nodes.at(1)->eh->IsElected());
    BOOST_CHECK(!nodes.at(1)->eh->IsElectionDetached());
    BOOST_CHECK_EQUAL(nodes.at(0)->incarnationId, 0);
    BOOST_CHECK_EQUAL(nodes.at(1)->incarnationId, 0);
}

BOOST_AUTO_TEST_CASE( two_nodes_other_light )
{
    AddLightNode();
    SendNodesChanged(true);
    BOOST_CHECK(!nodes.at(0)->eh->IsElected());
    BOOST_CHECK(!nodes.at(0)->eh->IsElectionDetached());
    BOOST_CHECK(!nodes.at(1)->eh->IsElected());
    BOOST_CHECK(!nodes.at(1)->eh->IsElectionDetached());
    RunIoService();
    BOOST_CHECK(nodes.at(0)->eh->IsElected());
    BOOST_CHECK(!nodes.at(0)->eh->IsElectionDetached());
    BOOST_CHECK(!nodes.at(1)->eh->IsElected());
    BOOST_CHECK(!nodes.at(1)->eh->IsElectionDetached());
    BOOST_CHECK_EQUAL(nodes.at(0)->incarnationId, 0);
    BOOST_CHECK_EQUAL(nodes.at(1)->incarnationId, 0);
}


BOOST_AUTO_TEST_CASE( two_nodes_no_incarnation )
{
    AddNode();
    SendNodesChanged(false);
    BOOST_CHECK(!nodes.at(0)->eh->IsElected());
    BOOST_CHECK(!nodes.at(1)->eh->IsElected());
    RunIoService();
    BOOST_CHECK(!nodes.at(0)->eh->IsElected());
    BOOST_CHECK(nodes.at(1)->eh->IsElected());
    BOOST_CHECK_NE(nodes.at(1)->incarnationId, 0);
}

BOOST_AUTO_TEST_CASE( two_nodes_remove_elected )
{
    AddNode();
    SendNodesChanged(true);
    BOOST_CHECK(!nodes.at(0)->eh->IsElected());
    BOOST_CHECK(!nodes.at(1)->eh->IsElected());
    RunIoService();
    BOOST_CHECK(!nodes.at(0)->eh->IsElected());
    BOOST_CHECK(nodes.at(1)->eh->IsElected());
    BOOST_CHECK_EQUAL(nodes.at(0)->incarnationId, 0);
    BOOST_CHECK_EQUAL(nodes.at(1)->incarnationId, 0);
    RemoveNode(1);
    SendNodesChanged(true);
    BOOST_CHECK(!nodes.at(0)->eh->IsElected());

    RunIoService();
    BOOST_CHECK(nodes.at(0)->eh->IsElected());
    BOOST_CHECK_EQUAL(nodes.at(0)->incarnationId, 0);

}

BOOST_AUTO_TEST_CASE( three_nodes )
{
    AddNode();
    AddNode();
    SendNodesChanged(true);
    BOOST_CHECK(!nodes.at(0)->eh->IsElected());
    BOOST_CHECK(!nodes.at(1)->eh->IsElected());
    BOOST_CHECK(!nodes.at(2)->eh->IsElected());
    RunIoService();
    BOOST_CHECK(!nodes.at(0)->eh->IsElected());
    BOOST_CHECK(!nodes.at(1)->eh->IsElected());
    BOOST_CHECK(nodes.at(2)->eh->IsElected());
}

BOOST_AUTO_TEST_CASE( three_nodes_with_reelection )
{
    AddNode();
    SendNodesChanged(true);
    BOOST_CHECK(!nodes.at(0)->eh->IsElected());
    BOOST_CHECK(!nodes.at(1)->eh->IsElected());
    RunIoService();
    BOOST_CHECK(!nodes.at(0)->eh->IsElected());
    BOOST_CHECK(nodes.at(1)->eh->IsElected());

    AddNode();
    SendNodesChanged(true);
    BOOST_CHECK(!nodes.at(0)->eh->IsElected());
    BOOST_CHECK(nodes.at(1)->eh->IsElected());
    BOOST_CHECK(!nodes.at(2)->eh->IsElected());

    RunIoService();
    BOOST_CHECK(!nodes.at(0)->eh->IsElected());
    BOOST_CHECK(!nodes.at(1)->eh->IsElected());
    BOOST_CHECK(nodes.at(2)->eh->IsElected());
}

BOOST_AUTO_TEST_CASE( three_nodes_remove_unelected )
{
    AddNode();
    AddNode();
    SendNodesChanged(true);
    BOOST_CHECK(!nodes.at(0)->eh->IsElected());
    BOOST_CHECK(!nodes.at(1)->eh->IsElected());
    BOOST_CHECK(!nodes.at(2)->eh->IsElected());
    RunIoService();
    BOOST_CHECK(!nodes.at(0)->eh->IsElected());
    BOOST_CHECK(!nodes.at(1)->eh->IsElected());
    BOOST_CHECK(nodes.at(2)->eh->IsElected());

    RemoveNode(1);
    SendNodesChanged(true);
    BOOST_CHECK(!nodes.at(0)->eh->IsElected());
    BOOST_CHECK(nodes.at(2)->eh->IsElected());

    RunIoService();
    BOOST_CHECK(!nodes.at(0)->eh->IsElected());
    BOOST_CHECK(nodes.at(2)->eh->IsElected());
}


BOOST_AUTO_TEST_CASE( three_nodes_remove_elected )
{
    AddNode();
    AddNode();
    SendNodesChanged(true);
    BOOST_CHECK(!nodes.at(0)->eh->IsElected());
    BOOST_CHECK(!nodes.at(1)->eh->IsElected());
    BOOST_CHECK(!nodes.at(2)->eh->IsElected());
    RunIoService();
    BOOST_CHECK(!nodes.at(0)->eh->IsElected());
    BOOST_CHECK(!nodes.at(1)->eh->IsElected());
    BOOST_CHECK(nodes.at(2)->eh->IsElected());

    RemoveNode(2);
    SendNodesChanged(true);
    BOOST_CHECK(!nodes.at(0)->eh->IsElected());
    BOOST_CHECK(!nodes.at(1)->eh->IsElected());

    RunIoService();
    BOOST_CHECK(!nodes.at(0)->eh->IsElected());
    BOOST_CHECK(nodes.at(1)->eh->IsElected());
}


BOOST_AUTO_TEST_CASE( lots_of_nodes )
{
#if !defined(NDEBUG) || defined(__arm__)
    const int numNodes = 10;
#elif defined(_MSC_VER)
    const int numNodes = 20;
#else
    const int numNodes = 100;
#endif

    //remember that one node is automatically added by the fixture
    for (int i = 0; i < numNodes - 1 ; ++i)
    {
        AddNode();
    }
    SendNodesChanged(true);
    for (int i = 0; i < numNodes; ++i)
    {
        SAFE_BOOST_CHECK(!nodes.at(i)->eh->IsElected());
    }

    RunIoService(10);

    for (int i = 0; i < numNodes - 1; ++i)
    {
        SAFE_BOOST_CHECK(!nodes.at(i)->eh->IsElected());
    }

    SAFE_BOOST_CHECK(nodes.at(numNodes - 1)->eh->IsElected());
}


BOOST_AUTO_TEST_CASE( lots_of_nodes_remove_some )
{
#if !defined(NDEBUG) || defined(__arm__)
    const int numNodes = 10;
#elif defined(_MSC_VER)
    const int numNodes = 20;
#else
    const int numNodes = 100;
#endif

    //remember that one node is automatically added by the fixture
    for (int i = 0; i < numNodes - 1; ++i)
    {
        AddNode();
    }

    SendNodesChanged(true);

    for (int i = 0; i < numNodes; ++i)
    {
        SAFE_BOOST_CHECK(!nodes.at(i)->eh->IsElected());
    }

    RunIoService(10);

    for (int i = 0; i < numNodes - 1; ++i)
    {
        SAFE_BOOST_CHECK(!nodes.at(i)->eh->IsElected());
    }

    SAFE_BOOST_CHECK(nodes.at(numNodes - 1)->eh->IsElected());


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

    SendNodesChanged(true);

    RunIoService(10);

    SAFE_BOOST_CHECK(nodes.at(numNodes - numNodes/10 - 1)->eh->IsElected());

}

BOOST_AUTO_TEST_CASE( remove_during_election )
{
    const int numNodes = 30;

    //remember that one node is automatically added by the fixture
    for (int i = 0; i < numNodes - 1 ; ++i)
    {
        AddNode();
    }

    SendNodesChanged(true);


    boost::thread_group threads;
    for (int i = 0; i < 6; ++i)
    {
        threads.create_thread([this]{ioService.run();});
    }

    //will keep node 0 and 1
    for (int i = numNodes - 1; i > 1; --i)
    {
        RemoveNode(i);
        SendNodesChanged(true);
    }

    threads.join_all();

    SAFE_BOOST_CHECK(!nodes.at(0)->eh->IsElected());
    SAFE_BOOST_CHECK(nodes.at(0)->eh->IsElected(11));
    SAFE_BOOST_CHECK(nodes.at(1)->eh->IsElected());
    SAFE_BOOST_CHECK(nodes.at(1)->eh->IsElected(11));
    SAFE_BOOST_CHECK(!nodes.at(1)->removed);
    SAFE_BOOST_CHECK(nodes.at(2)->removed);
}

BOOST_AUTO_TEST_CASE( remove_during_election_some_light )
{
    const int numNodes = 20; //has to be even
    const int numThreads = 6;

    //remember that one node is automatically added by the fixture
    AddLightNode();

    for (int i = 0; i < numNodes - 2 ; i+=2)
    {
        AddNode();
        AddLightNode();
    }

    SendNodesChanged(true);

    boost::barrier barrier(numThreads + 1);
    boost::thread_group threads;
    for (int i = 0; i < numThreads; ++i)
    {
        threads.create_thread([this, &barrier, i]
        {
            ioService.run();
            SendNodesChanged(true);
            lllog(1) << "Thread " << i << " waiting at first barrier" << std::endl;
            barrier.wait();
            lllog(1) << "Thread " << i << " waiting at second barrier" << std::endl;
            barrier.wait();
            ioService.run();
        });
    }

    lllog(1) << "Main thread waiting at first barrier" << std::endl;
    barrier.wait();
    AddNode();
    AddLightNode();
    SendNodesChanged(true);
    ioService.reset();
    lllog(1) << "Main thread waiting at second barrier" << std::endl;
    barrier.wait();

    //    lllog(1) << "Removing nodes" << std::endl;
    //will keep node 0 to 3
    for (int i = numNodes - 1 + 2; i > 3; --i)
    {
        RemoveNode(i);
        SendNodesChanged(true);
    }

    threads.join_all();
    lllog(1) << "Checking results" << std::endl;

    SAFE_BOOST_CHECK(!nodes.at(0)->eh->IsElected());
    SAFE_BOOST_CHECK(!nodes.at(0)->eh->IsElectionDetached());
    SAFE_BOOST_CHECK(!nodes.at(1)->eh->IsElected());
    SAFE_BOOST_CHECK(!nodes.at(1)->eh->IsElectionDetached());
    SAFE_BOOST_CHECK(nodes.at(2)->eh->IsElected());
    SAFE_BOOST_CHECK(!nodes.at(2)->eh->IsElectionDetached());
    SAFE_BOOST_CHECK(!nodes.at(3)->eh->IsElected());
    SAFE_BOOST_CHECK(!nodes.at(3)->eh->IsElectionDetached());
    SAFE_BOOST_CHECK(nodes.at(0)->eh->IsElected(12));
    SAFE_BOOST_CHECK(nodes.at(1)->eh->IsElected(12));
    SAFE_BOOST_CHECK(nodes.at(2)->eh->IsElected(12));
    SAFE_BOOST_CHECK(nodes.at(3)->eh->IsElected(12));
}


BOOST_AUTO_TEST_SUITE_END()


BOOST_FIXTURE_TEST_SUITE( self_is_light, Fixture<15> )

BOOST_AUTO_TEST_CASE( elect_self )
{
    BOOST_CHECK(!nodes.at(0)->eh->IsElected());
    BOOST_CHECK(!nodes.at(0)->eh->IsElectionDetached());
    RunIoService();
    BOOST_CHECK(nodes.at(0)->eh->IsElected());
    BOOST_CHECK(nodes.at(0)->eh->IsElected(10));
    BOOST_CHECK(!nodes.at(0)->eh->IsElected(20));
    BOOST_CHECK(nodes.at(0)->eh->IsElectionDetached());
    BOOST_CHECK_NE(nodes.at(0)->incarnationId,0);
}


BOOST_AUTO_TEST_CASE( two_nodes )
{
    AddNode();
    SendNodesChanged(true);
    BOOST_CHECK(!nodes.at(0)->eh->IsElected());
    BOOST_CHECK(!nodes.at(0)->eh->IsElectionDetached());
    BOOST_CHECK(!nodes.at(1)->eh->IsElected());
    BOOST_CHECK(!nodes.at(1)->eh->IsElectionDetached());
    RunIoService();
    BOOST_CHECK(!nodes.at(0)->eh->IsElected());
    BOOST_CHECK(!nodes.at(0)->eh->IsElectionDetached());
    BOOST_CHECK(nodes.at(1)->eh->IsElected());
    BOOST_CHECK(!nodes.at(1)->eh->IsElectionDetached());
    BOOST_CHECK_EQUAL(nodes.at(0)->incarnationId, 0);
    BOOST_CHECK_EQUAL(nodes.at(1)->incarnationId, 0);
}

BOOST_AUTO_TEST_CASE( two_nodes_no_incarnation )
{
    AddNode();
    SendNodesChanged(false);
    BOOST_CHECK(!nodes.at(0)->eh->IsElected());
    BOOST_CHECK(!nodes.at(0)->eh->IsElectionDetached());
    BOOST_CHECK(!nodes.at(1)->eh->IsElected());
    BOOST_CHECK(!nodes.at(1)->eh->IsElectionDetached());
    RunIoService();
    BOOST_CHECK(!nodes.at(0)->eh->IsElected());
    BOOST_CHECK(!nodes.at(0)->eh->IsElectionDetached());
    BOOST_CHECK(nodes.at(1)->eh->IsElected());
    BOOST_CHECK(!nodes.at(1)->eh->IsElectionDetached());
    BOOST_CHECK_NE(nodes.at(1)->incarnationId, 0);
    BOOST_CHECK_EQUAL(nodes.at(0)->incarnationId, 0);
}

BOOST_AUTO_TEST_CASE( make_node_detached )
{
    AddNode();
    SendNodesChanged(true);
    BOOST_CHECK(!nodes.at(0)->eh->IsElected());
    BOOST_CHECK(!nodes.at(0)->eh->IsElectionDetached());
    BOOST_CHECK(!nodes.at(1)->eh->IsElected());
    BOOST_CHECK(!nodes.at(1)->eh->IsElectionDetached());
    RunIoService();
    BOOST_CHECK(!nodes.at(0)->eh->IsElected());
    BOOST_CHECK(!nodes.at(0)->eh->IsElectionDetached());
    BOOST_CHECK(nodes.at(1)->eh->IsElected());
    BOOST_CHECK(!nodes.at(1)->eh->IsElectionDetached());
    BOOST_CHECK_EQUAL(nodes.at(0)->incarnationId, 0);
    BOOST_CHECK_EQUAL(nodes.at(1)->incarnationId, 0);

    RemoveNode(1);
    SendNodesChanged(true);
    RunIoService();

    BOOST_CHECK(nodes.at(0)->eh->IsElected());
    BOOST_CHECK(nodes.at(0)->eh->IsElectionDetached());
}

BOOST_AUTO_TEST_CASE( reattach )
{
    AddNode();
    SendNodesChanged(true);
    RunIoService();
    RemoveNode(1);
    SendNodesChanged(true);
    RunIoService();

    BOOST_CHECK(nodes.at(0)->eh->IsElected());
    BOOST_CHECK(nodes.at(0)->eh->IsElectionDetached());

    AddNode();
    SendNodesChanged(true);
    RunIoService();
    BOOST_CHECK(!nodes.at(0)->eh->IsElected());
    BOOST_CHECK(!nodes.at(0)->eh->IsElectionDetached());
    BOOST_CHECK(nodes.at(2)->eh->IsElected());
    BOOST_CHECK(!nodes.at(2)->eh->IsElectionDetached());
}

BOOST_AUTO_TEST_CASE( two_light_nodes )
{
    AddNode();
    AddLightNode();
    SendNodesChanged(true);
    BOOST_CHECK(!nodes.at(0)->eh->IsElected());
    BOOST_CHECK(!nodes.at(0)->eh->IsElectionDetached());
    BOOST_CHECK(!nodes.at(1)->eh->IsElected());
    BOOST_CHECK(!nodes.at(1)->eh->IsElectionDetached());
    BOOST_CHECK(!nodes.at(2)->eh->IsElected());
    BOOST_CHECK(!nodes.at(2)->eh->IsElectionDetached());
    RunIoService();
    BOOST_CHECK(!nodes.at(0)->eh->IsElected());
    BOOST_CHECK(!nodes.at(0)->eh->IsElectionDetached());
    BOOST_CHECK(nodes.at(1)->eh->IsElected());
    BOOST_CHECK(!nodes.at(1)->eh->IsElectionDetached());
    BOOST_CHECK(!nodes.at(2)->eh->IsElected());
    BOOST_CHECK(!nodes.at(2)->eh->IsElectionDetached());
    BOOST_CHECK_EQUAL(nodes.at(0)->incarnationId, 0);
    BOOST_CHECK_EQUAL(nodes.at(1)->incarnationId, 0);
    BOOST_CHECK_EQUAL(nodes.at(2)->incarnationId, 0);
}

BOOST_AUTO_TEST_CASE( two_nodes_kill_normal )
{
    AddNode();
    AddLightNode();
    SendNodesChanged(true);
    RunIoService();
    BOOST_CHECK(!nodes.at(0)->eh->IsElected());
    BOOST_CHECK(!nodes.at(0)->eh->IsElectionDetached());
    BOOST_CHECK(nodes.at(1)->eh->IsElected());
    BOOST_CHECK(!nodes.at(1)->eh->IsElectionDetached());
    BOOST_CHECK(!nodes.at(2)->eh->IsElected());
    BOOST_CHECK(!nodes.at(2)->eh->IsElectionDetached());

    RemoveNode(1);
    SendNodesChanged(true);
    RunIoService();
    BOOST_CHECK(nodes.at(0)->eh->IsElected());
    BOOST_CHECK(nodes.at(0)->eh->IsElectionDetached());
    BOOST_CHECK(nodes.at(2)->eh->IsElected());
    BOOST_CHECK(nodes.at(2)->eh->IsElectionDetached());

}

BOOST_AUTO_TEST_CASE( reattach_two )
{
    AddNode();
    AddLightNode();
    SendNodesChanged(true);
    RunIoService();

    RemoveNode(1);
    SendNodesChanged(true);
    RunIoService();
    BOOST_CHECK(nodes.at(0)->eh->IsElected());
    BOOST_CHECK(nodes.at(0)->eh->IsElectionDetached());
    BOOST_CHECK(nodes.at(2)->eh->IsElected());
    BOOST_CHECK(nodes.at(2)->eh->IsElectionDetached());

    AddNode();
    SendNodesChanged(true);
    RunIoService();
    BOOST_CHECK(!nodes.at(0)->eh->IsElected());
    BOOST_CHECK(!nodes.at(0)->eh->IsElectionDetached());
    BOOST_CHECK(!nodes.at(2)->eh->IsElected());
    BOOST_CHECK(!nodes.at(2)->eh->IsElectionDetached());
    BOOST_CHECK(nodes.at(3)->eh->IsElected());
    BOOST_CHECK(!nodes.at(3)->eh->IsElectionDetached());
}

BOOST_AUTO_TEST_CASE( two_normal_two_light )
{
    AddNode(); //nodeid 11
    AddNode(); //nodeid 12
    AddLightNode();
    SendNodesChanged(true);
    RunIoService();
    BOOST_CHECK(!nodes.at(0)->eh->IsElected());
    BOOST_CHECK(!nodes.at(0)->eh->IsElectionDetached());
    BOOST_CHECK(!nodes.at(1)->eh->IsElected());
    BOOST_CHECK(!nodes.at(1)->eh->IsElectionDetached());
    BOOST_CHECK(nodes.at(2)->eh->IsElected());
    BOOST_CHECK(!nodes.at(2)->eh->IsElectionDetached());
    BOOST_CHECK(!nodes.at(3)->eh->IsElected());
    BOOST_CHECK(!nodes.at(3)->eh->IsElectionDetached());
    BOOST_CHECK(nodes.at(0)->eh->IsElected(12));
    BOOST_CHECK(nodes.at(1)->eh->IsElected(12));
    BOOST_CHECK(nodes.at(2)->eh->IsElected(12));
    BOOST_CHECK(nodes.at(3)->eh->IsElected(12));

    RemoveNode(2);
    SendNodesChanged(true);
    RunIoService();
    BOOST_CHECK(!nodes.at(0)->eh->IsElected());
    BOOST_CHECK(!nodes.at(0)->eh->IsElectionDetached());
    BOOST_CHECK(nodes.at(1)->eh->IsElected());
    BOOST_CHECK(!nodes.at(1)->eh->IsElectionDetached());
    BOOST_CHECK(!nodes.at(3)->eh->IsElected());
    BOOST_CHECK(!nodes.at(3)->eh->IsElectionDetached());
    BOOST_CHECK(nodes.at(0)->eh->IsElected(11));
    BOOST_CHECK(nodes.at(1)->eh->IsElected(11));
    BOOST_CHECK(nodes.at(3)->eh->IsElected(11));
}


BOOST_AUTO_TEST_SUITE_END()
