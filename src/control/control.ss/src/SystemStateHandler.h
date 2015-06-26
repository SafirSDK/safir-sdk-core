/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safir.sourceforge.net)
* Copyright Consoden AB, 2014 (http://www.consoden.se)
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

#include <boost/function.hpp>
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

        typedef boost::function<void(const Node& node)> NodeIncludedCb;

        typedef boost::function<void(const int64_t nodeId, const int64_t nodeTypeId)> NodeDownCb;

        SystemStateHandlerBasic(const int64_t                    ownNodeId,
                                const NodeIncludedCb&            nodeIncludedCb,
                                const NodeDownCb&                nodeDownCb)
            : m_ownNodeId(ownNodeId),
              m_systemState(),
              m_nodeIncludedCb(nodeIncludedCb),
              m_nodeDownCb(nodeDownCb)
        {
        }

        void SetNewState(const SystemState& newState)
        {
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

    private:

        int64_t                 m_ownNodeId;

        std::unordered_map<int64_t, Node> m_systemState;

        NodeIncludedCb          m_nodeIncludedCb;
        NodeDownCb              m_nodeDownCb;

    };

    typedef SystemStateHandlerBasic<Safir::Dob::Internal::SP::SystemState> SystemStateHandler;

}
}
}
}



