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

        const std::string name;
        const boost::int64_t id;
        const bool isLight;
        const std::set<std::string> talksTo;
        const std::string multicastAddressControl;
        const std::string multicastAddressData;
        const int heartbeatInterval;
        const int maxLostHeartbeats;
        const int slidingWindowSize;
        const int retryTimeout;
        const std::vector<std::string> wantedTypes;
        const std::vector<std::string> unwantedTypes;
    };

    // This class reads configuration parameters and make some sanity checks for parameters needed by Control.
    class Config
    {
    public:

        Config()
        {
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
                    for (const auto& talksToName: talksTo)
                    {
                        if (allNodeTypes.find(talksToName) == allNodeTypes.end())
                        {
                            throw std::logic_error("Parameter error: "
                                                   "Node type " + nodeTypeName + ": TalksTo node " +
                                                   talksToName + "does not exist!");
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

                m_nodeTypes.push_back(NodeType(nodeTypeName,
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
            for (const auto& i : m_nodeTypes)
            {
                if (!i.multicastAddressControl.empty())
                {
                    if (!ipAddr.insert(i.multicastAddressControl).second)
                    {
                        throw std::logic_error("Parameter error: Duplicated ip addresses!");
                    }
                }

                if (!i.multicastAddressData.empty())
                {
                    if (!ipAddr.insert(i.multicastAddressData).second)
                    {
                        throw std::logic_error("Parameter error: Duplicated ip addresses!");
                    }
                }
            }
        }

        std::vector<NodeType> GetNodeTypes() const
        {
            return m_nodeTypes;
        }

    private:

        std::vector<NodeType> m_nodeTypes;
    };
}
}
}
}


