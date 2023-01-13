/******************************************************************************
*
* Copyright Saab AB, 2015 (http://safirsdkcore.com)
*
* Created by: Anders Widn / anders.widen@consoden.se
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

#include <Safir/Dob/Internal/Communication.h>
#include <Safir/Dob/Internal/ControlConfig.h>
#include <Safir/Dob/Internal/SystemPicture.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <boost/chrono.hpp>
#include <boost/noncopyable.hpp>
#include <functional>
#include <map>
#include <string>
#include <vector>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#endif

#include <boost/asio.hpp>

#ifdef _MSC_VER

#pragma warning (pop)
#endif
namespace Safir
{
namespace Dob
{
namespace Internal
{
    typedef std::function<void(const std::string&  nodeName,
                               int64_t             nodeId,
                               int64_t             nodeTypeId,
                               const std::string&  dataAddress)> OnInjectNode;

    typedef std::function<void(int64_t nodeId,
                               int64_t nodeTypeId)> OnExcludeNode;

    typedef std::function<void(bool isDetached)> OnDetachedStateChanged;

    typedef std::function<void()> OnDetached;
    typedef std::function<void(bool sameSystem)> OnAttached;


    // Class that encapsulates the Communication and System Picture instances
    //
    template <typename CommunicationT, typename SystemPictureT, typename ConfigT>
    class DistributionBasic
        : private boost::noncopyable
    {
    public:
        DistributionBasic(boost::asio::io_service&  ioService,
                          const std::string&        ownNodeName,
                          int64_t                   ownNodeId,
                          int64_t                   ownNodeTypeId,
                          const std::string&        ownDataAddress)
            : m_nodeId(ownNodeId),
              m_communication(),
              m_sp(),
              m_config(),
              m_injectCallbacks(),
              m_excludeCallbacks(),
              m_detachedCallbacks(),
              m_attachedCallbacks(),
              m_liveNodes(),
              m_nodeTypeIds(CalculateNodeTypeIds(m_config)),
              m_lightNodeTypeIds(CalculateLightNodeTypeIds(m_config)),
              m_nodeState(Normal),
              m_started(false)
        {
            m_isLightNode = IsLightNode(ownNodeTypeId);

            // Create and populate structures that are needed when creating the Communication and
            // SP instances.
            std::vector<Com::NodeTypeDefinition> commNodeTypes;
            std::map<std::int64_t, typename SP::NodeType> spNodeTypes;


            for (auto nt = m_config.nodeTypesParam.cbegin(); nt != m_config.nodeTypesParam.cend(); ++nt)
            {
                commNodeTypes.push_back(Com::NodeTypeDefinition(nt->id,
                                         nt->name,
                                         nt->multicastAddressControl,
                                         nt->multicastAddressData,
                                         nt->isLightNode,
                                         nt->heartbeatInterval,
                                         nt->maxLostHeartbeats,
                                         nt->slidingWindowSize,
                                         nt->ackRequestThreshold,
                                         nt->retryTimeout));

                std::vector<boost::chrono::steady_clock::duration> retryTimeouts;
                for (auto rt = nt->retryTimeout.cbegin(); rt != nt->retryTimeout.cend(); ++rt)
                {
                    retryTimeouts.push_back(boost::chrono::milliseconds(*rt));
                }

                spNodeTypes.insert(std::make_pair(nt->id,
                                                  SP::NodeType(nt->id,
                                                               nt->name,
                                                               nt->isLightNode,
                                                               boost::chrono::milliseconds(nt->heartbeatInterval),
                                                               nt->maxLostHeartbeats,
                                                               retryTimeouts)));
            }

            m_communication.reset(new CommunicationT(Com::dataModeTag,
                                                     ioService,
                                                     ownNodeName,
                                                     ownNodeId,
                                                     ownNodeTypeId,
                                                     Com::ResolvedAddress(ownDataAddress),
                                                     commNodeTypes,
                                                     m_config.fragmentSize));

            m_sp.reset(new SystemPictureT(SP::slave_tag,
                                          ioService,
                                          *m_communication,
                                          ownNodeName,
                                          ownNodeId,
                                          ownNodeTypeId,
                                          spNodeTypes));

        }

        void Start()
        {
            m_communication->Start();
            m_started = true;
        }

        // Stop the internal workings of this class.
        // Must be called before destroying the object.
        void Stop()
        {
            m_sp->Stop();
            m_communication->Stop();
            m_started = false;
        }

        // Subscribe for new injected nodes and excluded nodes.
        void SubscribeNodeEvents(const OnInjectNode& onInjectNode, const OnExcludeNode& onExcludeNode)
        {
            m_injectCallbacks.push_back(onInjectNode);
            m_excludeCallbacks.push_back(onExcludeNode);
        }

        // Subscribe for lightnode detache state changes.
        void SubscribeAttachedDetached(const OnAttached& onAttached, const OnDetached& onDetached)
        {
            m_attachedCallbacks.push_back(onAttached);
            m_detachedCallbacks.push_back(onDetached);
        }

        // Inject an external node
        void InjectNode(const std::string& nodeName,
                        int64_t            nodeId,
                        int64_t            nodeTypeId,
                        const std::string& dataAddress)
        {
            ENSURE(m_started, << "InjectNode before Communication has been started!");
            m_liveNodes.insert({nodeId, nodeTypeId});
            m_communication->InjectNode(nodeName,
                                        nodeId,
                                        nodeTypeId,
                                        dataAddress);

            for (const auto& cb : m_injectCallbacks)
            {
                cb(nodeName, nodeId, nodeTypeId, dataAddress);
            }
        }

        void ExcludeNode(int64_t nodeId, int64_t nodeTypeId)
        {
            if (m_liveNodes.erase(nodeId) > 0)
            {
                m_sp->ExcludeNode(nodeId);
                for (const auto& cb : m_excludeCallbacks)
                {
                    cb(nodeId, nodeTypeId);
                }
            }
        }

        void StoppedNodeIndication(int64_t nodeId)
        {
            // check that the node has not already been deleted
            auto it = m_liveNodes.find(nodeId);
            if (it != std::end(m_liveNodes))
            {
                ExcludeNode(nodeId, it->second);
            }
        }

        bool IsDetached() const
        {
            return m_nodeState == Detached;
        }

        void SetAttached(bool sameSystem)
        {
            if (m_nodeState == Attached)
            {
                return; // already attached
            }

            m_nodeState = Attached;

            // Notify subscribers.
            for (const auto& cb : m_attachedCallbacks)
            {
                cb(sameSystem);
            }
        }

        void SetDetached()
        {
            if (m_nodeState == Detached)
            {
                return; // already detached
            }

            m_nodeState = Detached;

            // Notify subscribers.
            for (const auto& cb : m_detachedCallbacks)
            {
                cb();
            }

            // If we toggle to detached mode, first exclude all remote nodes.
            auto nodesCopy = m_liveNodes;
            for (const auto& kv : nodesCopy)
            {
                ExcludeNode(kv.first, kv.second);
            }
        }

        CommunicationT& GetCommunication()
        {
            return *m_communication;
        }

        const CommunicationT& GetCommunication() const
        {
            return *m_communication;
        }

        const ConfigT& GetNodeTypeConfiguration() const
        {
            return m_config;
        }

        int64_t GetNodeId() const
        {
            return m_nodeId;
        }

        const std::vector<int64_t>& GetNodeTypeIds() const
        {
            return m_nodeTypeIds;
        }

        // Is this node a light node
        bool IsLightNode() const
        {
            return m_isLightNode;
        }

        // Check if specific nodeType is lightNode
        bool IsLightNode(int64_t nodeTypeId) const
        {
#ifndef NDEBUG
            //Just a sanity check to see that we haven't mixed up our ids...
            ENSURE(std::find(m_nodeTypeIds.begin(), m_nodeTypeIds.end(), nodeTypeId) != std::end(m_nodeTypeIds),
                   << "Value " << nodeTypeId << " passed to IsLightNode does not appear to be a node type id.");
#endif
            return m_lightNodeTypeIds.find(nodeTypeId) != std::end(m_lightNodeTypeIds);
        }

    private:
        static std::vector<int64_t> CalculateNodeTypeIds(const ConfigT& config)
        {
            std::vector<int64_t> nodeTypeIds;

            for (auto nt = config.nodeTypesParam.cbegin(); nt != config.nodeTypesParam.cend(); ++nt)
            {
                nodeTypeIds.push_back(nt->id);
            }

            std::sort(nodeTypeIds.begin(),nodeTypeIds.end());
            nodeTypeIds.shrink_to_fit();
            return nodeTypeIds;
        }

        static std::set<int64_t> CalculateLightNodeTypeIds(const ConfigT& config)
        {
            std::set<int64_t> lightNodeTypeIds;

            for (const auto& nt : config.nodeTypesParam)
            {
                if (nt.isLightNode)
                {
                    lightNodeTypeIds.insert(nt.id);
                }
            }

            return lightNodeTypeIds;
        }

        const int64_t m_nodeId;
        bool m_isLightNode; // Is this node a lightNode
        std::unique_ptr<CommunicationT> m_communication;
        std::unique_ptr<SystemPictureT> m_sp;
        const ConfigT m_config;
        std::vector<OnInjectNode> m_injectCallbacks;
        std::vector<OnExcludeNode> m_excludeCallbacks;
        std::vector<OnDetached> m_detachedCallbacks;
        std::vector<OnAttached> m_attachedCallbacks;
        std::map<int64_t,int64_t> m_liveNodes;

        //this is a sorted vector of node type ids
        //it is a vector rather than a set since it is likely to be small and
        //we're going to loop over it frequently.
        const std::vector<int64_t> m_nodeTypeIds;

        // Set of all nodeTypeIds that are lightNodes.
        const std::set<int64_t> m_lightNodeTypeIds;

        enum NodeState
        {
            Normal = 0,
            Detached,
            Attached
        };
        NodeState m_nodeState;
        bool m_started;
    };

    typedef DistributionBasic<Com::Communication, SP::SystemPicture, Control::Config> Distribution;
}
}
}
