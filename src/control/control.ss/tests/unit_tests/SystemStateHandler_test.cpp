/******************************************************************************
*
* Copyright Saab AB, 2015 (http://safirsdkcore.com)
*
* Created by: Anders Widén / anders.widen@consoden.se
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
#include "../../src/SystemStateHandler.h"

#define BOOST_TEST_MODULE SystemStateHandlerTests
#include <boost/test/unit_test.hpp>

using namespace Safir::Dob::Internal::Control;

class SystemState
{
public:

    struct Node
    {
        Node() {}

        Node(std::string         _name,
             int64_t             _nodeId,
             int64_t             _nodeTypeId,
             std::string         _controlAddress,
             std::string         _dataAddress,
             bool                _isDead)

        : name(_name),
          nodeId(_nodeId),
          nodeTypeId(_nodeTypeId),
          controlAddress(_controlAddress),
          dataAddress(_dataAddress),
          isDead(_isDead)
        {}

        std::string         name;
        int64_t             nodeId;
        int64_t             nodeTypeId;
        std::string         controlAddress;
        std::string         dataAddress;
        bool                isDead;
    };

    std::vector<Node> nodes;
    bool detached = false;

    bool IsDetached() const
    {
        return detached;
    }

    int Size() const
    {
        return static_cast<int>(nodes.size());
    }

    int64_t Id(const int index) const
    {
        return nodes[index].nodeId;
    }

    bool IsDead(const int index) const
    {
        return nodes[index].isDead;
    }

    const std::string& Name(const int index) const
    {
        return nodes[index].name;
    }

    int64_t NodeTypeId(const int index) const
    {
        return nodes[index].nodeTypeId;
    }

    const std::string& ControlAddress(const int index) const
    {
        return nodes[index].controlAddress;
    }

    const std::string& DataAddress(const int index) const
    {
        return nodes[index].dataAddress;
    }

    std::string ToJson() const
    {
        return "";
    }

};

SystemState::Node n1("Node1",
                     11111,
                     1010,
                     "192.168.11.11",
                     "192.168.111.111",
                     false);

SystemState::Node n2("Node2",
                     22222,
                     2020,
                     "192.168.22.22",
                     "192.168.222.222",
                     false);

SystemState::Node ownNode("OwnNode",
                          99999,
                          9090,
                          "192.168.0.1",
                          "192.168.0.2",
                          false);

typedef SystemStateHandlerBasic<SystemState> TestSystemStateHandler;

BOOST_AUTO_TEST_CASE( callback_order )
{
    auto ownNodeIncludedCbCounter = 0;
    auto nodeIncludedCbCounter = 0;
    auto nodeDownCbCounter = 0;

    TestSystemStateHandler ssh(99999, false, {},
                             [&ownNodeIncludedCbCounter, &nodeIncludedCbCounter]
                             (const Node& node)
                             {
                                 if (node.nodeId == 99999)
                                 {
                                    ++ownNodeIncludedCbCounter;
                                    BOOST_CHECK(nodeIncludedCbCounter == 2);
                                 }
                                 else
                                 {
                                    ++nodeIncludedCbCounter;
                                    BOOST_CHECK(ownNodeIncludedCbCounter == 0);
                                 }

                             },
                             [&nodeDownCbCounter]
                             (const int64_t /*nodeId*/, int64_t /*nodetypeId*/)
                             {
                                ++nodeDownCbCounter;
                             });


    SystemState ss;
    ss.nodes.push_back(n1);
    ss.nodes.push_back(ownNode);
    ss.nodes.push_back(n2);

    ssh.SetNewState(ss);

    BOOST_CHECK(ownNodeIncludedCbCounter == 1);
    BOOST_CHECK(nodeIncludedCbCounter == 2);
    BOOST_CHECK(nodeDownCbCounter == 0);
}


BOOST_AUTO_TEST_CASE( add_delete_node )
{
    auto ownNodeIncludedCbCounter = 0;
    auto nodeIncludedCbCounter = 0;
    auto nodeDownCbCounter = 0;

    int64_t latestOwnNode;
    int64_t latestIncludedNode;
    int64_t latestDownNodeId;
    int64_t latestDownNodeTypeId;

    TestSystemStateHandler ssh(99999, false, {},
                             [&ownNodeIncludedCbCounter,
                              &latestOwnNode,
                              &nodeIncludedCbCounter,
                              &latestIncludedNode]
                             (const Node& node)
                             {
                                if (node.nodeId == 99999)
                                {
                                    latestOwnNode = node.nodeId;
                                    ++ownNodeIncludedCbCounter;
                                }
                                else
                                {
                                    latestIncludedNode = node.nodeId;
                                     ++nodeIncludedCbCounter;
                                }
                             },
                             [&nodeDownCbCounter, &latestDownNodeId, &latestDownNodeTypeId]
                             (const int64_t nodeId, const int64_t nodeTypeId)
                             {
                                latestDownNodeId = nodeId;
                                latestDownNodeTypeId = nodeTypeId;
                                ++nodeDownCbCounter;
                             });


    SystemState ss;
    ss.nodes.push_back(n1);
    ss.nodes.push_back(ownNode);

    ssh.SetNewState(ss);

    BOOST_CHECK(ownNodeIncludedCbCounter == 1);
    BOOST_CHECK(nodeIncludedCbCounter == 1);
    BOOST_CHECK(nodeDownCbCounter == 0);

    BOOST_CHECK(latestOwnNode == 99999);
    BOOST_CHECK(latestIncludedNode == 11111);

    ss.nodes.push_back(n2);
    ssh.SetNewState(ss);

    BOOST_CHECK(ownNodeIncludedCbCounter == 1);
    BOOST_CHECK(nodeIncludedCbCounter == 2);
    BOOST_CHECK(nodeDownCbCounter == 0);

    BOOST_CHECK(latestIncludedNode == 22222);

    ss.nodes.erase(ss.nodes.begin());
    ssh.SetNewState(ss);

    BOOST_CHECK(nodeDownCbCounter == 1);
    BOOST_CHECK(latestDownNodeId == 11111);
    BOOST_CHECK(latestDownNodeTypeId == 1010);

}

BOOST_AUTO_TEST_CASE( detach_node )
{
    std::vector<int64_t> includedNodes;
    std::vector<int64_t> excludedNodes;

    TestSystemStateHandler ssh(99999, false, {},
    [&includedNodes](const Node& node)
    {
        includedNodes.push_back(node.nodeId);
    },
    [&excludedNodes](const int64_t nodeId, int64_t /*nodetypeId*/)
    {
        excludedNodes.push_back(nodeId);
    });

    SystemState ss;
    ss.nodes.push_back(n1);
    ss.nodes.push_back(n2);
    ss.nodes.push_back(ownNode);
    ssh.SetNewState(ss);

    BOOST_CHECK(includedNodes.size() == 3);
    BOOST_CHECK(excludedNodes.size() == 0);

    ssh.SetNewState(ss);

    BOOST_CHECK(includedNodes.size() == 3);
    BOOST_CHECK(excludedNodes.size() == 0);

    includedNodes.clear();
    ssh.SetDetached(true);

    BOOST_CHECK(includedNodes.size() == 0);
    BOOST_CHECK(excludedNodes.size() == 0);
    BOOST_CHECK(ssh.IsDetached());

    ssh.SetNewState(ss);
    BOOST_CHECK(includedNodes.size() == 0);
    BOOST_CHECK(excludedNodes.size() == 0);
    BOOST_CHECK(ssh.IsDetached());

    ssh.SetDetached(false);
    ssh.SetNewState(ss);
    BOOST_CHECK(includedNodes.size() == 3);
    BOOST_CHECK(excludedNodes.size() == 0);
    BOOST_CHECK(!ssh.IsDetached());
}

BOOST_AUTO_TEST_CASE( ignore_detached_state )
{
    std::vector<int64_t> includedNodes;
    std::vector<int64_t> excludedNodes;

    TestSystemStateHandler ssh(99999, true, {},
    [&includedNodes](const Node& node)
    {
        includedNodes.push_back(node.nodeId);
    },
    [&excludedNodes](const int64_t nodeId, int64_t /*nodetypeId*/)
    {
        excludedNodes.push_back(nodeId);
    });

    SystemState ss;
    ss.detached = true;
    ss.nodes.push_back(n1);
    ss.nodes.push_back(n2);
    ss.nodes.push_back(ownNode);
    ssh.SetNewState(ss);

    BOOST_CHECK(includedNodes.size() == 0);
    BOOST_CHECK(excludedNodes.size() == 0);
}

BOOST_AUTO_TEST_CASE( light_node_ignore_other_lightnodes )
{
    std::vector<int64_t> includedNodes;
    std::vector<int64_t> excludedNodes;

    TestSystemStateHandler ssh(99999, true, {9090, 2020},
    [&includedNodes](const Node& node)
    {
        includedNodes.push_back(node.nodeId);
    },
    [&excludedNodes](const int64_t nodeId, int64_t /*nodetypeId*/)
    {
        excludedNodes.push_back(nodeId);
    });

    SystemState ss;
    ss.nodes.push_back(n1); //lightnode
    ss.nodes.push_back(n2);
    ss.nodes.push_back(ownNode); //lightnode
    ssh.SetNewState(ss);

    BOOST_CHECK(includedNodes.size() == 2);
    BOOST_CHECK(excludedNodes.size() == 0);

    ss.nodes.clear();
    ss.nodes.push_back(ownNode);

    ssh.SetNewState(ss);
    BOOST_CHECK(includedNodes.size() == 2);
    BOOST_CHECK(excludedNodes.size() == 1);
}
