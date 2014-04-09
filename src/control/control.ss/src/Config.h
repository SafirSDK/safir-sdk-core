/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safir.sourceforge.net)
*
* Created by: Anders Widén / ander.widen@consoden.se
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
#ifndef __CONTROL_CONFIG_H__
#define __CONTROL_CONFIG_H__

#include <string>
#include <vector>
#include <set>

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace Control
{

    struct NodeType
    {
        NodeType(const std::string& name_,
                 const bool isLight_,
                 const std::set<std::string>& talksTo_,
                 const std::string& multicastAddress_,
                 const int controlPort_,
                 const int dataPort_,
                 const int heartbeatInterval_,
                 const int maxLostHeartbeats_,
                 const int slidingWindowSize_,
                 const int retryTimeout_,
                 const std::vector<std::string>& wantedTypes_,
                 const std::vector<std::string>& unwantedTypes_)

            : name(name_),
              isLight(isLight_),
              talksTo(talksTo_),
              multicastAddress(multicastAddress_),
              controlPort(controlPort_),
              dataPort(dataPort_),
              heartbeatInterval(heartbeatInterval_),
              maxLostHeartbeats(maxLostHeartbeats_),
              slidingWindowSize(slidingWindowSize_),
              retryTimeout(retryTimeout_),
              wantedTypes(wantedTypes_),
              unwantedTypes(unwantedTypes_)
        {}

        const std::string name;
        const bool isLight;
        const std::set<std::string> talksTo;
        const std::string multicastAddress;
        const int controlPort;
        const int dataPort;
        const int heartbeatInterval;
        const int maxLostHeartbeats;
        const int slidingWindowSize;
        const int retryTimeout;
        const std::vector<std::string> wantedTypes;
        const std::vector<std::string> unwantedTypes;
    };

    // This class parses configuration parameters needed by Control.
    class Config
    {
    public:

        Config();

        std::vector<NodeType> GetNodeTypes() const;

    private:

        std::vector<NodeType> m_nodeTypes;
    };
}
}
}
}

#endif

