/******************************************************************************
*
* Copyright Saab AB, 2014-2015 (http://safirsdkcore.com)
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
#pragma once

#include <Safir/Dob/Internal/SystemState.h>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#endif

#include <boost/asio.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

#include <set>
#include <string>
#include <unordered_map>

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace Control
{

    struct Node
    {
        Node(const std::string& name_,
             int64_t nodeId_,
             int64_t nodeTypeId_,
             const std::string& controlAddress_,
             const std::string& dataAddress_)
            : name(name_),
              nodeId(nodeId_),
              nodeTypeId(nodeTypeId_),
              controlAddress(controlAddress_),
              dataAddress(dataAddress_)
        {
        }

        std::string         name;
        int64_t             nodeId;
        int64_t             nodeTypeId;
        const std::string   controlAddress;
        const std::string   dataAddress;
    };

    /**
     * @brief The SystemStateHandler class holds the current system state. When a new
     * state is set this class figures out what has happened and the appropriate
     * callbacks are called.
     */
    template<typename SystemState>
    class SystemStateHandlerBasic
    {
    public:

        typedef std::function<void(const Node& node)> NodeIncludedCb;

        typedef std::function<void(const int64_t nodeId, const int64_t nodeTypeId)> NodeDownCb;

        SystemStateHandlerBasic(const int64_t ownNodeId,
                                const bool isLightNode,
                                const std::set<int64_t>& lightNodeTypeIds,
                                const NodeIncludedCb& nodeIncludedCb,
                                const NodeDownCb& nodeDownCb)
            : m_ownNodeId(ownNodeId),
              m_isLightNode(isLightNode),
              m_lightNodeTypeIds(lightNodeTypeIds),
              m_systemState(),
              m_detached(false),
              m_nodeIncludedCb(nodeIncludedCb),
              m_nodeDownCb(nodeDownCb)
        {
        }

        void SetNewState(const SystemState& newState)
        {
            if (m_detached || newState.IsDetached())
            {
                // We will always get a formSystem-callback when a lightNode becomes detached.
                // All detach handling is done in ControlApp formSystem.
                // In detached mode we don't accept new states, first we want a form/join system.
                return;
            }

            // First, check that own node is part of the system state.
            auto ownNodeFound = false;

            for (int ix = 0; ix < newState.Size(); ++ix)
            {
                if (newState.Id(ix) == m_ownNodeId)
                {
                    ownNodeFound = true;
                    break;
                }
            }
            if (!ownNodeFound)
            {
                std::ostringstream os;
                os << "CTRL: Got a system state where own nod id (" << m_ownNodeId << ") is not present!";
                throw std::logic_error(os.str());
            }

            auto ownNodeIncluded = false;

            std::set<int64_t> existingNodeIds;

            for (int ix = 0; ix < newState.Size(); ++ix)
            {
                auto nodeId = newState.Id(ix);

                if (m_isLightNode && m_ownNodeId != nodeId && IsLightNode(newState.NodeTypeId(ix)))
                {
                    continue; // LightNodes don't interact, ignore lightNodes if this node is also a lightNode.
                }

                existingNodeIds.insert(nodeId);

                if (m_systemState.find(nodeId) == m_systemState.end())
                {
                    // This is a node we haven't seen before

                    if (newState.IsDead(ix))
                    {
                        // A new node that is marked as dead. Skip it!
                        continue;
                    }


                    Node newNode(newState.Name(ix),
                                 newState.Id(ix),
                                 newState.NodeTypeId(ix),
                                 newState.ControlAddress(ix),
                                 newState.DataAddress(ix));

                    m_systemState.insert( std::make_pair(nodeId, newNode) );

                    if (newNode.nodeId == m_ownNodeId)
                    {
                        // Own node has been included in system state but we must make the callback
                        // for any other new nodes first
                        ownNodeIncluded = true;
                    }
                    else
                    {
                       m_nodeIncludedCb(newNode);
                    }
                }
                else
                {
                    // We already know about this node
                    if (!newState.IsDead(ix))
                    {
                        // It is still alive
                        continue;
                    }

                    // Node is dead
                    m_systemState.erase(nodeId);
                    m_nodeDownCb(nodeId, newState.NodeTypeId(ix));
                }
            }

            if (ownNodeIncluded)
            {
                m_nodeIncludedCb(m_systemState.at(m_ownNodeId));
            }

            // Check if we have any nodes that have disapeared from system state
            for (auto pos = m_systemState.begin(); pos != m_systemState.end(); /*increment below*/)
            {
                if (existingNodeIds.find(pos->first) == existingNodeIds.end())
                {
                    m_nodeDownCb(pos->first, pos->second.nodeTypeId);
                    pos = m_systemState.erase(pos);
                }
                else
                {
                    ++pos;
                }
            }
        }

        void SetDetached(bool detached)
        {
            m_detached = detached;
            if (m_detached)
            {
                m_systemState.clear();
            }
        }

        bool IsDetached() const
        {
            return m_detached;
        }

    private:
        const int64_t   m_ownNodeId;
        const bool m_isLightNode;
        std::set<int64_t> m_lightNodeTypeIds;
        std::unordered_map<int64_t /*nodeId*/, Node>   m_systemState;
        bool m_detached;

        NodeIncludedCb          m_nodeIncludedCb;
        NodeDownCb              m_nodeDownCb;

        bool IsLightNode(int64_t nodeTypeId) const
        {
            return m_lightNodeTypeIds.find(nodeTypeId) != std::end(m_lightNodeTypeIds);
        }
    };

    typedef SystemStateHandlerBasic<Safir::Dob::Internal::SP::SystemState> SystemStateHandler;

}
}
}
}



