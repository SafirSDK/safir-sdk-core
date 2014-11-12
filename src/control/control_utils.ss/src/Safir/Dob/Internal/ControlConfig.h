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
#include <Safir/Utilities/Internal/ConfigReader.h>
#include <Safir/Dob/Typesystem/ToolSupport/BlobLayout.h>
#include <Safir/Dob/Typesystem/ToolSupport/TypeParser.h>
#include <Safir/Dob/Typesystem/ToolSupport/TypeUtilities.h>


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

    // This class parses configuration parameters needed by Control.
    class Config
    {
    public:

        Config()
        {
            namespace ts = Safir::Dob::Typesystem::ToolSupport;

            Safir::Utilities::Internal::ConfigReader reader;

            std::vector<boost::filesystem::path> douDirs;

            for(const std::string& s : Safir::Utilities::Internal::ConfigHelper::GetDouDirectories(reader))
            {
                douDirs.push_back(boost::filesystem::path(s));
            }

            auto repo = ts::ParseTypeDefinitions(douDirs);
            auto bl = ts::BlobLayout<ts::TypeRepository>(repo.get());

            std::string paramClass("Safir.Dob.NodeParameters");
            const auto npcd = repo->GetClass(ts::TypeUtilities::CalculateTypeId(paramClass));
            if (npcd == nullptr)
            {
                throw std::logic_error("Can't find parameter class " + paramClass);
            }

            const std::string paramName("NodeTypes");
            const auto pd = ts::TypeUtilities::GetParameterByName<ts::ClassDescription, ts::ParameterDescription>(npcd, paramName);
            if (pd == nullptr)
            {
                throw std::logic_error("Can't find parameter " + paramName + " in " + paramClass);
            }

            if (!pd->IsArray() ||
                pd->GetArraySize() < 1 ||
                bl.GetTypeId(pd->GetObjectValue(0).first) != ts::TypeUtilities::CalculateTypeId("Safir.Dob.NodeType"))
            {
                throw std::logic_error("Parameter " + paramName + " in " + paramClass +
                                       " is expected to be an array with length > 0 and elements of type Safir.Dob.NodeType!" );
            }

            std::string nodeTypeClass("Safir.Dob.NodeType");
            const auto ntcd = repo->GetClass(ts::TypeUtilities::CalculateTypeId(nodeTypeClass));

            // First, get all node type names
            std::set<std::string> allNodeTypes;
            for (int paramIdx = 0; paramIdx < pd->GetArraySize(); ++paramIdx)
            {
                auto blob = pd->GetObjectValue(paramIdx).first;

                DotsC_Int32 dummy;

                const char* name;
                auto ms = bl.GetDynamicMember(blob, ntcd->GetMemberIndex("Name"), 0, name, dummy);
                if (ms.IsNull())
                {
                    throw std::logic_error(nodeTypeClass + " is expected to contain member Name");
                }
                if (!allNodeTypes.insert(name).second)
                {
                    throw std::logic_error("Node type " + std::string(name) + " multiple definitions");
                }
            }

            for (int paramIdx = 0; paramIdx < pd->GetArraySize(); ++paramIdx)
            {
                auto blob = pd->GetObjectValue(paramIdx).first;

                // Name

                DotsC_Int32 dummy;
                const char* name;
                bl.GetDynamicMember(blob, ntcd->GetMemberIndex("Name"), 0, name, dummy);
                const std::string nodeTypeName(name);

                // IsLight
                bool isLight;
                auto ms = bl.GetBoolMember(blob, ntcd->GetMemberIndex("IsLight"), 0, isLight);
                if (ms.IsNull())
                {
                    throw std::logic_error("Node type " + nodeTypeName + ": IsLight is mandatory");
                }

                // TalksTo
                std::set<std::string> talksTo;
                const auto ttmd = ntcd->GetMember(ntcd->GetMemberIndex("TalksTo"));
                for (int i = 0; i < ttmd->GetArraySize(); ++i)
                {
                    ms = bl.GetMemberStatus(blob, ntcd->GetMemberIndex("TalksTo"), i);

                    if (!ms.IsNull())
                    {
                        const char* name;
                        bl.GetDynamicMember(blob, ntcd->GetMemberIndex("TalksTo"), i, name, dummy);
                        talksTo.insert(name);
                    }
                }

                if (talksTo.size() > 0)
                {
                    if (!isLight)
                    {
                        throw std::logic_error("Node type " + nodeTypeName +
                                               ": TalksTo can only be specified for Light nodes!");
                    }

                    // Check that the TalksTo nodes exists
                    for (const auto& talksToName: talksTo)
                    {
                        if (allNodeTypes.find(talksToName) == allNodeTypes.end())
                        {
                            throw std::logic_error("Node type " + nodeTypeName + ": TalksTo node " +
                                                   talksToName + "does not exist!");
                        }
                    }
                }
                else
                {
                    // Non Light nodes talks to all node types
                    talksTo = allNodeTypes;
                }

                const char* const EMPTY_STRING = "";

                // MulticastAddressControl
                const char* multicastAddressControl;
                ms = bl.GetDynamicMember(blob, ntcd->GetMemberIndex("MulticastAddressControl"), 0, multicastAddressControl, dummy);
                if (ms.IsNull())
                {
                    multicastAddressControl = EMPTY_STRING;
                }

                // MulticastAddressData
                const char* multicastAddressData;
                ms = bl.GetDynamicMember(blob, ntcd->GetMemberIndex("MulticastAddressData"), 0, multicastAddressData, dummy);
                if (ms.IsNull())
                {
                    multicastAddressData = EMPTY_STRING;
                }

                // HeartbeatInterval
                double heartbeatInterval;
                ms = bl.GetFloat64Member(blob, ntcd->GetMemberIndex("HeartbeatInterval"), 0, heartbeatInterval);
                if (ms.IsNull())
                {
                    throw std::logic_error("Node type " + nodeTypeName + ": HeartbeatInterval is mandatory");
                }

                // MaxLostHeartbeats
                int maxLostHeartbeats;
                ms = bl.GetInt32Member(blob, ntcd->GetMemberIndex("MaxLostHeartbeats"), 0, maxLostHeartbeats);
                if (ms.IsNull())
                {
                    throw std::logic_error("Node type " + nodeTypeName + ": MaxLostHeartbeats is mandatory");
                }

                // SlidingWindowsSize
                int slidingWindowsSize;
                ms = bl.GetInt32Member(blob, ntcd->GetMemberIndex("SlidingWindowsSize"), 0, slidingWindowsSize);
                if (ms.IsNull())
                {
                    throw std::logic_error("Node type " + nodeTypeName + ": SlidingWindowsSize is mandatory");
                }

                // RetryTimeout
                double retryTimeout;
                ms = bl.GetFloat64Member(blob, ntcd->GetMemberIndex("RetryTimeout"), 0, retryTimeout);
                if (ms.IsNull())
                {
                    throw std::logic_error("Node type " + nodeTypeName + ": RetryTimeout is mandatory");
                }

                // WantedTypes
                std::vector<std::string> wantedTypes;
                const auto wtmd = ntcd->GetMember(ntcd->GetMemberIndex("WantedTypes"));
                for (int i = 0; i < wtmd->GetArraySize(); ++i)
                {
                    auto ms = bl.GetMemberStatus(blob, ntcd->GetMemberIndex("WantedTypes"), i);

                    if (!ms.IsNull())
                    {
                        const char* wt;
                        bl.GetDynamicMember(blob, ntcd->GetMemberIndex("WantedTypes"), i, wt, dummy);
                        std::string regexp(wt);
                        if (!isLight && regexp != ".*")
                        {
                            throw std::logic_error("Node type " + nodeTypeName +
                                                   ": WantedTypes can only be .* for non Light nodes!");
                        }
                        wantedTypes.push_back(wt);
                    }
                }

                // UnwantedTypes
                std::vector<std::string> unwantedTypes;
                const auto uwtmd = ntcd->GetMember(ntcd->GetMemberIndex("UnwantedTypes"));

                for (int i = 0; i < uwtmd->GetArraySize(); ++i)
                {
                    auto ms = bl.GetMemberStatus(blob, ntcd->GetMemberIndex("UnwantedTypes"), i);

                    if (!ms.IsNull())
                    {
                        if (!isLight)
                        {
                            throw std::logic_error("Node type " + nodeTypeName +
                                                   ": UnwantedTypes can be specified only for non Light nodes!");
                        }

                        const char* uwt;
                        bl.GetDynamicMember(blob, ntcd->GetMemberIndex("UnwantedTypes"), i, uwt, dummy);
                        std::string regExp(uwt);
                        unwantedTypes.push_back(regExp);
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
                        throw std::logic_error("Duplicated ip addresses!");
                    }
                }

                if (!i.multicastAddressData.empty())
                {
                    if (!ipAddr.insert(i.multicastAddressData).second)
                    {
                        throw std::logic_error("Duplicated ip addresses!");
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

