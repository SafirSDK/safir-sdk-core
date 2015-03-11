/******************************************************************************
*
* Copyright Consoden AB, 2015 (http://safir.sourceforge.net)
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
#include <Safir/Dob/Internal/SystemPicture.h>
#include <Safir/Dob/Internal/ControlConfig.h>
#include <boost/noncopyable.hpp>
#include <boost/chrono.hpp>
#include <string>
#include <vector>
#include <functional>
#include <map>

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
    using OnInjectNode = std::function<void(const std::string& nodeName, int64_t nodeId, int64_t nodeTypeId, const std::string& dataAddress)>;
    using OnExcludeNode = std::function<void(int64_t nodeId, int64_t nodeTypeId)>;

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
            : m_communication(),
              m_sp(),
              m_started(false)
        {
            // Create and populate structures that are needed when creating the Communication and
            // SP instances.
            std::vector<Com::NodeTypeDefinition> commNodeTypes;
            std::map<boost::int64_t, typename SP::NodeType> spNodeTypes;

            for (const auto& nt: m_config.nodeTypesParam)
            {
                commNodeTypes.push_back({nt.id,
                                         nt.name,
                                         nt.multicastAddressControl,
                                         nt.multicastAddressData,
                                         nt.heartbeatInterval,
                                         nt.retryTimeout});

                spNodeTypes.insert(std::make_pair(nt.id,
                                                  SP::NodeType(nt.id,
                                                               nt.name,
                                                               nt.isLight,
                                                               boost::chrono::milliseconds(nt.heartbeatInterval),
                                                               nt.maxLostHeartbeats,
                                                               boost::chrono::milliseconds(nt.retryTimeout))));
            }

            m_communication.reset(new CommunicationT(Com::dataModeTag,
                                                     ioService,
                                                     ownNodeName,
                                                     ownNodeId,
                                                     ownNodeTypeId,
                                                     ownDataAddress,
                                                     commNodeTypes));

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

        //subscribe for new injected nodes and excluded nodes.
        void SubscribeNodeEvents(const OnInjectNode& onInjectNode, const OnExcludeNode& onExcludeNode)
        {
            m_injectCallbacks.push_back(onInjectNode);
            m_excludeCallbacks.push_back(onExcludeNode);
        }

        // Inject an external node
        void InjectNode(const std::string& nodeName,
                        int64_t            nodeId,
                        int64_t            nodeTypeId,
                        const std::string& dataAddress)
        {
            m_communication->InjectNode(nodeName,
                                        nodeId,
                                        nodeTypeId,
                                        dataAddress);

            for (auto& cb : m_injectCallbacks)
            {
                cb(nodeName, nodeId, nodeTypeId, dataAddress);
            }
        }

        void ExcludeNode(int64_t nodeId, int64_t nodeTypeId)
        {
            for (auto& cb : m_excludeCallbacks)

            {
                cb(nodeId, nodeTypeId);
            }
        }


        CommunicationT& GetCommunication()
        {
            return *m_communication;
        }

        const ConfigT& GetNodeTypeConfiguration() const
        {
            return m_config;
        }

    private:

        std::unique_ptr<CommunicationT> m_communication;
        std::unique_ptr<SystemPictureT> m_sp;
        const ConfigT m_config;
        std::vector<OnInjectNode> m_injectCallbacks;
        std::vector<OnExcludeNode> m_excludeCallbacks;

        bool m_started;
    };

    typedef DistributionBasic<Com::Communication, SP::SystemPicture, Control::Config> Distribution;
}
}
}
