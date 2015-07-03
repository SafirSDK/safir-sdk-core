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
#pragma once

#include <string>
#include <vector>
#include <set>
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
                 const bool isLight_,
                 const std::set<std::string>& talksTo_,
                 const std::string& multicastAddressControl_,
                 const std::string& multicastAddressData_,
                 const int heartbeatInterval_,
                 const int maxLostHeartbeats_,
                 const int slidingWindowSize_,
                 const int retryTimeout_,
                 const std::vector<std::string>& wantedTypes_,
                 const std::vector<std::string>& unwantedTypes_)

            : name(name_),
              id(LlufId_Generate64(name_.c_str())),
              isLight(isLight_),
              talksTo(talksTo_),
              multicastAddressControl(multicastAddressControl_),
              multicastAddressData(multicastAddressData_),
              heartbeatInterval(heartbeatInterval_),
              maxLostHeartbeats(maxLostHeartbeats_),
              slidingWindowSize(slidingWindowSize_),
              retryTimeout(retryTimeout_),
              wantedTypes(wantedTypes_),
              unwantedTypes(unwantedTypes_)
        {}

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
            // NodeTypes
            using Safir::Dob::Typesystem::Utilities::ToUtf8;

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

                // IsLight
                if (nt->IsLight().IsNull())
                {
                    throw std::logic_error("Parameter error: "
                                           "Node type " + nodeTypeName + ": IsLight is mandatory");
                }
                auto isLight = nt->IsLight();

                // TalksTo
                std::set<std::string> talksTo;
                for (Safir::Dob::Typesystem::ArrayIndex i = 0;
                     i < nt->TalksToArraySize();
                     ++i)
                {
                    if (!nt->TalksTo()[i].IsNull())
                    {
                        talksTo.insert(ToUtf8(nt->TalksTo()[i].GetVal()));
                    }
                }

                if (talksTo.size() > 0)
                {
                    if (!isLight)
                    {
                        throw std::logic_error("Parameter error: "
                                               "Node type " + nodeTypeName +
                                               ": TalksTo can only be specified for Light nodes!");
                    }

                    // Check that the TalksTo nodes exists
                    for (auto talksToName = talksTo.cbegin(); talksToName != talksTo.cend(); ++talksToName)
                    {
                        if (allNodeTypes.find(*talksToName) == allNodeTypes.end())
                        {
                            throw std::logic_error("Parameter error: "
                                                   "Node type " + nodeTypeName + ": TalksTo node " +
                                                   *talksToName + "does not exist!");
                        }
                    }
                }
                else
                {
                    // Non Light nodes talks to all node types
                    talksTo = allNodeTypes;
                }

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

                // WantedTypes
                std::vector<std::string> wantedTypes;
                for (Safir::Dob::Typesystem::ArrayIndex i = 0;
                     i < nt->WantedTypesArraySize();
                     ++i)
                {
                    if (!nt->WantedTypes()[i].IsNull())
                    {
                        std::string wt = ToUtf8(nt->WantedTypes()[i].GetVal());

                        if (!isLight && wt != ".*")
                        {
                            throw std::logic_error("Parameter error: "
                                                   "Node type " + nodeTypeName +
                                                   ": WantedTypes can only be .* for non Light nodes!");
                        }
                        wantedTypes.push_back(wt);
                    }
                }

                // UnwantedTypes
                std::vector<std::string> unwantedTypes;

                for (Safir::Dob::Typesystem::ArrayIndex i = 0;
                     i < nt->UnwantedTypesArraySize();
                     ++i)
                {
                    if (!nt->UnwantedTypes()[i].IsNull())
                    {
                        if (!isLight)
                        {
                            throw std::logic_error("Parameter error: "
                                                   "Node type " + nodeTypeName +
                                                   ": UnwantedTypes can be specified only for non Light nodes!");
                        }
                        unwantedTypes.push_back(ToUtf8(nt->UnwantedTypes()[i].GetVal()));
                    }
                }

                nodeTypesParam.push_back(NodeType(nodeTypeName,
                                                  isLight,
                                                  talksTo,
                                                  multicastAddressControl,
                                                  multicastAddressData,
                                                  static_cast<int>(heartbeatInterval * 1000), // seconds -> milliseconds
                                                  maxLostHeartbeats,
                                                  slidingWindowsSize,
                                                  static_cast<int>(retryTimeout * 1000), // seconds -> milliseconds
                                                  wantedTypes,
                                                  unwantedTypes));

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

        std::vector<NodeType> nodeTypesParam;
        ThisNode thisNodeParam;
    };
}
}
}
}


