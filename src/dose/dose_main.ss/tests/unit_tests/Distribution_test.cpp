/******************************************************************************
*
* Copyright Consoden AB, 2015 (http://safir.sourceforge.net)
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
                  boost::asio::io_service& ioService,
                  const std::string& nodeName,
                  int64_t nodeId, //0 is not a valid id.
                  int64_t nodeTypeId,
                  const std::string& dataAddress,
                  const std::vector<Safir::Dob::Internal::Com::NodeTypeDefinition>& nodeTypes)
    {}

    void InjectNode(const std::string& /*name*/, int64_t /*id*/, int64_t /*nodeTypeId*/, const std::string& /*dataAddress*/) {}
};

class SP
{
public:

    SP(Safir::Dob::Internal::SP::slave_tag_t,
       boost::asio::io_service& ioService,
       Communication& communication,
       const std::string& name,
       const int64_t id,
       const int64_t nodeTypeId,
       const std::string& dataAddress,
       const std::map<int64_t, Safir::Dob::Internal::SP::NodeType>& nodeTypes)
    {}
};

struct NodeType
{
    std::string name;
    boost::int64_t id;
    bool isLight;
    std::set<std::string> talksTo;
    std::string multicastAddressControl;
    std::string multicastAddressData;
    int heartbeatInterval;
    int maxLostHeartbeats;
    int slidingWindowSize;
    int retryTimeout;
    std::vector<std::string> wantedTypes;
    std::vector<std::string> unwantedTypes;
};

class Config
{
public:

    std::vector<NodeType> nodeTypesParam;
};

BOOST_AUTO_TEST_CASE( first_test )
{
    boost::asio::io_service ioService;

    Safir::Dob::Internal::DistributionBasic<Communication, SP, Config> d(ioService,
                                                                         "Pelle",
                                                                         6565,
                                                                         878787,
                                                                         "127.0.0.1:5555");

    //-------------------------
    //Test node subscription
    //-------------------------
    {
        std::vector<int64_t> included;
        std::vector<int64_t> excluded;
        d.SubscribeNodeEvents([&](const std::string& nodeName, int64_t nodeId, int64_t nodeTypeId, const std::string& dataAddress)
                                {included.push_back(nodeId);},
                              [&](int64_t nodeId, int64_t nodeTypeId)
                                {excluded.push_back(nodeId);});

        d.InjectNode("node10", 10, 0, "127.0.0.1:1010");
        d.InjectNode("node20", 20, 0, "127.0.0.1:1020");
        d.ExcludeNode(10, 0);
        d.ExcludeNode(20, 0);

        BOOST_CHECK(included[0]==10);
        BOOST_CHECK(included[1]==20);
        BOOST_CHECK(excluded[0]==10);
        BOOST_CHECK(excluded[1]==20);
    }

    bool ok = true;
    BOOST_CHECK(ok);
}

