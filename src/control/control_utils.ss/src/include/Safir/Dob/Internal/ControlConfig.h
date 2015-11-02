/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safirsdkcore.com)
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
#pragma once

#include <string>
#include <vector>
#include <set>
#include <sstream>
#include <Safir/Utilities/Internal/Id.h>
#include <Safir/Dob/NodeParameters.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include <Safir/Dob/Typesystem/Utilities.h>


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
                 const std::string& multicastAddressControl_,
                 const std::string& multicastAddressData_,
                 const int heartbeatInterval_,
                 const int maxLostHeartbeats_,
                 const int slidingWindowSize_,
                 const int retryTimeout_)

            : name(name_),
              id(LlufId_Generate64(name_.c_str())),
              multicastAddressControl(multicastAddressControl_),
              multicastAddressData(multicastAddressData_),
              heartbeatInterval(heartbeatInterval_),
              maxLostHeartbeats(maxLostHeartbeats_),
              slidingWindowSize(slidingWindowSize_),
              retryTimeout(retryTimeout_)
        {}

        std::string name;
        boost::int64_t id;
        std::string multicastAddressControl;
        std::string multicastAddressData;
        int heartbeatInterval;
        int maxLostHeartbeats;
        int slidingWindowSize;
        int retryTimeout;
    };

    struct ThisNode
    {
        ThisNode() {}

        ThisNode(const std::string& controlAddress_,
                 const std::string dataAddress_,
                 const std::vector<std::string> seeds_,
                 const std::string name_,
                 const std::string nodeType_)

            : controlAddress(controlAddress_),
              dataAddress(dataAddress_),
              seeds(seeds_),
              name(name_),
              nodeType(nodeType_),
              nodeTypeId(LlufId_Generate64(nodeType_.c_str()))
        {}

        std::string controlAddress;
        std::string dataAddress;
        std::vector<std::string> seeds;
        std::string name;
        std::string nodeType;
        boost::int64_t nodeTypeId;
    };

    // This class reads configuration parameters and make some sanity checks for parameters needed by Control.
    class Config
    {
    public:

        Config()
        {
            using Safir::Dob::Typesystem::Utilities::ToUtf8;


            //incarnation blacklist
            if (Safir::Dob::NodeParameters::IncarnationBlacklistFilename().length() == 0)
            {
                throw std::logic_error("Parameter IncarnationBlacklistFilename is expected to be a string with length > 0");
            }

            incarnationBlacklistFileName = ToUtf8(Safir::Dob::NodeParameters::IncarnationBlacklistFilename());

            // NodeTypes
            if (Safir::Dob::NodeParameters::NodeTypesArraySize() < 1)
            {
                throw std::logic_error("Parameter NodeTypesArray is expected to be an array with length > 0");
            }

            // First, get all node type names
            std::set<std::string> allNodeTypes;
            for (Safir::Dob::Typesystem::ArrayIndex i = 0;
                 i < Safir::Dob::NodeParameters::NodeTypesArraySize();
                 ++i)
            {
                if (Safir::Dob::NodeParameters::NodeTypes(i)->Name().IsNull())
                {
                    throw std::logic_error
                            ("Parameter error: "
                             "A Safir.Dob.NodeType is expected to contain member Name");
                }

                auto name = ToUtf8(Safir::Dob::NodeParameters::NodeTypes(i)->Name());

                if (!allNodeTypes.insert(name).second)
                {
                    throw std::logic_error("Parameter error: "
                                           "Node type " + name + " multiple definitions");
                }
            }

            std::vector<NodeType> nodeTypes;

            for (Safir::Dob::Typesystem::ArrayIndex i = 0;
                 i < Safir::Dob::NodeParameters::NodeTypesArraySize();
                 ++i)
            {
                // Name
                auto nt = Safir::Dob::NodeParameters::NodeTypes(i);
                const std::string nodeTypeName =
                        ToUtf8(nt->Name().GetVal());

                // MulticastAddressControl
                std::string multicastAddressControl;
                if (!nt->MulticastAddressControl().IsNull())
                {
                    multicastAddressControl = ToUtf8(nt->MulticastAddressControl());
                }

                // MulticastAddressData
                std::string multicastAddressData;
                if (!nt->MulticastAddressData().IsNull())
                {
                    multicastAddressData = ToUtf8(nt->MulticastAddressData());
                }

                if (nt->MulticastAddressControl().IsNull() != nt->MulticastAddressData().IsNull() ||
                    multicastAddressControl.empty() != multicastAddressData.empty())
                {
                    throw std::logic_error("Parameter error: Data and control channels need to be configured in the same way");
                }

                // HeartbeatInterval
                if (nt->HeartbeatInterval().IsNull())
                {
                    throw std::logic_error("Parameter error: "
                                           "Node type " + nodeTypeName + ": HeartbeatInterval is mandatory");
                }

                auto heartbeatInterval = nt->HeartbeatInterval();

                // MaxLostHeartbeats
                if (nt->MaxLostHeartbeats().IsNull())
                {
                    throw std::logic_error("Parameter error: "
                                           "Node type " + nodeTypeName + ": MaxLostHeartbeats is mandatory");
                }

                auto maxLostHeartbeats = nt->MaxLostHeartbeats();

                // SlidingWindowsSize
                if (nt->SlidingWindowsSize().IsNull())
                {
                    throw std::logic_error("Parameter error: "
                                           "Node type " + nodeTypeName + ": SlidingWindowsSize is mandatory");
                }

                auto slidingWindowsSize = nt->SlidingWindowsSize();

                // RetryTimeout
                if (nt->RetryTimeout().IsNull())
                {
                    throw std::logic_error("Parameter error: "
                                           "Node type " + nodeTypeName + ": RetryTimeout is mandatory");
                }

                auto retryTimeout = nt->RetryTimeout();

                nodeTypesParam.push_back(NodeType(nodeTypeName,
                                                  multicastAddressControl,
                                                  multicastAddressData,
                                                  static_cast<int>(heartbeatInterval * 1000), // seconds -> milliseconds
                                                  maxLostHeartbeats,
                                                  slidingWindowsSize,
                                                  static_cast<int>(retryTimeout * 1000))); // seconds -> milliseconds

            }

            // Check that there are no duplicated ip addresses
            std::set<std::string> ipAddr;

            for (auto i = nodeTypesParam.cbegin(); i != nodeTypesParam.cend(); ++i)
            {
                if (!i->multicastAddressControl.empty())
                {
                    if (!ipAddr.insert(i->multicastAddressControl).second)
                    {
                        throw std::logic_error("Parameter error: Duplicated ip addresses!");
                    }
                }

                if (!i->multicastAddressData.empty())
                {
                    if (!ipAddr.insert(i->multicastAddressData).second)
                    {
                        throw std::logic_error("Parameter error: Duplicated ip addresses!");
                    }
                }
            }

            // WaitForNodeTypes
            for (Safir::Dob::Typesystem::ArrayIndex i = 0;
                 i < Safir::Dob::NodeParameters::WaitForNodeTypesArraySize();
                 ++i)
            {
                auto nodeTypeName = ToUtf8(Safir::Dob::NodeParameters::WaitForNodeTypes(i));

                if (nodeTypeName.empty())
                {
                    throw  std::logic_error("Parameter error: WaitForNodeTypes: Empty value not allowed!");
                }

                bool found = false;
                for (auto ntIt = nodeTypesParam.cbegin(); ntIt != nodeTypesParam.cend(); ++ntIt)
                {
                    if (ntIt->name == nodeTypeName)
                    {
                        if (!waitForNodeTypes.insert(ntIt->id).second)
                        {
                            throw  std::logic_error("Parameter error: WaitForNodeTypes: Duplicated node types!");
                        }
                        found = true;
                        break;
                    }
                }
                if (!found)
                {
                    throw std::logic_error("Parameter error: WaitForNodeTypes: " + nodeTypeName +
                                           " is not a valid node type");
                }
            }

            // ThisNode
            std::string controlAddress = ToUtf8(Safir::Dob::ThisNodeParameters::ControlAddress());

            std::string dataAddress = ToUtf8(Safir::Dob::ThisNodeParameters::DataAddress());

            std::vector<std::string> seeds;

            for (Safir::Dob::Typesystem::ArrayIndex i = 0;
                 i < Safir::Dob::ThisNodeParameters::SeedsArraySize();
                 ++i)
            {
                seeds.push_back(ToUtf8(Safir::Dob::ThisNodeParameters::Seeds(i)));
            }

            std::string name = ToUtf8(Safir::Dob::ThisNodeParameters::Name());

            std::string nodeType = ToUtf8(Safir::Dob::ThisNodeParameters::NodeType());

            thisNodeParam = ThisNode(controlAddress,
                                     dataAddress,
                                     seeds,
                                     name,
                                     nodeType);


            bool nodeTypeValid = false;

            for (auto nt = nodeTypesParam.cbegin(); nt != nodeTypesParam.cend(); ++nt)
            {
                if (nt->id == thisNodeParam.nodeTypeId)
                {
                    nodeTypeValid = true;
                    break;
                }
            }

            if (!nodeTypeValid)
            {
                throw std::logic_error("Parameter error: " +
                                       thisNodeParam.name + " is not a valid node type!");
            }

        }

        std::string GetName(boost::int64_t nodeTypeId)
        {
            for (auto it = nodeTypesParam.cbegin(); it != nodeTypesParam.cend(); ++it)
            {
                if (nodeTypeId == it->id)
                {
                    return it->name;
                }
            }

            // Not found, return id as a string
            std::ostringstream os;
            os << nodeTypeId;
            return os.str();
        }

        std::vector<NodeType> nodeTypesParam;
        ThisNode thisNodeParam;
        std::string incarnationBlacklistFileName;
        std::set<boost::int64_t> waitForNodeTypes;
    };
}
}
}
}


