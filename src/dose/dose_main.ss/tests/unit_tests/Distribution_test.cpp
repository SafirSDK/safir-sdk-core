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
#include "../../src/Distribution.h"
#include <Safir/Dob/NodeInfo.h>
#include <Safir/Dob/ProcessInfo.h>
#include <Safir/Dob/PersistentDataReady.h>

#define BOOST_TEST_MODULE DistributionTests
#include <boost/test/unit_test.hpp>

struct NodeTypeDefinition
{
    int64_t id;
    std::string name;
    std::string controlMulticastAddress;
    std::string dataMulticastAddress;
    int heartbeatInterval;
    int retryTimeout;
};

class Communication
{
public:

    Communication(Safir::Dob::Internal::Com::DataModeTag,
                  boost::asio::io_service& /*ioService*/,
                  const std::string& /*nodeName*/,
                  int64_t /*nodeId*/, //0 is not a valid id.
                  int64_t /*nodeTypeId*/,
                  const Safir::Dob::Internal::Com::ResolvedAddress& /*dataAddress*/,
                  const std::vector<Safir::Dob::Internal::Com::NodeTypeDefinition>& /*nodeTypes*/,
                  int /*fragmentSize*/)
    {}

    void Start() {}
    void Stop() {}
    void InjectNode(const std::string& /*name*/, int64_t /*id*/, int64_t /*nodeTypeId*/, const std::string& /*dataAddress*/) {}
};

class SP
{
public:

    SP(Safir::Dob::Internal::SP::slave_tag_t,
       boost::asio::io_service& /*ioService*/,
       Communication& /*communication*/,
       const std::string& /*name*/,
       const int64_t /*id*/,
       const int64_t /*nodeTypeId*/,
       const std::map<int64_t, Safir::Dob::Internal::SP::NodeType>& /*nodeTypes*/)
    {}

    void ExcludeNode(const int64_t /*id*/) {}
};

struct NodeType
{
    std::string name;
    std::int64_t id;
    bool isLightNode;
    std::string multicastAddressControl;
    std::string multicastAddressData;
    int heartbeatInterval;
    int maxLostHeartbeats;
    int slidingWindowSize;
    int ackRequestThreshold;
    std::vector<int> retryTimeout;
};

class Config
{
public:
    Config()
    {
        nodeTypesParam.push_back({"test",878787,false,"","",10,10,10,10,{10}});
    }
    
    std::vector<NodeType> nodeTypesParam;
    int fragmentSize;
};

struct Fixture
{

    Fixture ()
        : ioContext()
        , distribution(ioContext,
                      "Pelle",
                      6565,
                      878787,
                      "127.0.0.1:5555") {}

    boost::asio::io_context ioContext;
    Safir::Dob::Internal::DistributionBasic<Communication, SP, Config> distribution;
};

BOOST_FIXTURE_TEST_SUITE( s, Fixture )

BOOST_AUTO_TEST_CASE( node_subscription )
{
    std::vector<int64_t> included;
    std::vector<int64_t> excluded;
    distribution.SubscribeNodeEvents([&](const std::string& /*nodeName*/,
                                         int64_t nodeId,
                                         int64_t /*nodeTypeId*/,
                                         const std::string& /*dataAddress*/)
                                     {
                                         included.push_back(nodeId);
                                     },
                                     [&](int64_t nodeId, int64_t /*nodeTypeId*/)
                                     {
                                         excluded.push_back(nodeId);
                                     });

    distribution.Start();
    distribution.InjectNode("node10", 10, 0, "127.0.0.1:1010");
    distribution.InjectNode("node20", 20, 0, "127.0.0.1:1020");
    distribution.ExcludeNode(10, 0);
    distribution.ExcludeNode(20, 0);

    BOOST_CHECK(included[0]==10);
    BOOST_CHECK(included[1]==20);
    BOOST_CHECK(excluded[0]==10);
    BOOST_CHECK(excluded[1]==20);
}

BOOST_AUTO_TEST_CASE( node_detached )
{
    std::vector<int64_t> included;
    std::vector<int64_t> excluded;
    bool detached = false;
    bool detachedSub2 = false;
    distribution.SubscribeNodeEvents([&](const std::string& /*nodeName*/,
                                         int64_t nodeId,
                                         int64_t /*nodeTypeId*/,
                                         const std::string& /*dataAddress*/)
                                     {
                                         included.push_back(nodeId);
                                     },
                                     [&](int64_t nodeId, int64_t /*nodeTypeId*/)
                                     {
                                         excluded.push_back(nodeId);
                                     });
    distribution.SubscribeAttachedDetached([&](bool) { detached = false; }, [&]() { detached = true; });
    distribution.SubscribeAttachedDetached([&](bool) { detachedSub2 = false; }, [&]() { detachedSub2 = true; });


    distribution.Start();
    distribution.InjectNode("node10", 10, 0, "127.0.0.1:1010");
    distribution.InjectNode("node20", 20, 0, "127.0.0.1:1020");

    BOOST_CHECK(included.size() == 2);
    BOOST_CHECK(included[0]==10);
    BOOST_CHECK(included[1]==20);
    BOOST_CHECK(excluded.empty());
    BOOST_CHECK(!detached);
    BOOST_CHECK(!detachedSub2);

    distribution.SetDetached();

    BOOST_CHECK(excluded.size() == 2);
    BOOST_CHECK(excluded[0]==10);
    BOOST_CHECK(excluded[1]==20);
    BOOST_CHECK(detached);
    BOOST_CHECK(detachedSub2);
}

BOOST_AUTO_TEST_SUITE_END()
