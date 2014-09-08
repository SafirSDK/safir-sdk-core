/******************************************************************************
*
* Copyright Saab AB, 2014 (http://safir.sourceforge.net)
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
#include "SystemStateHandler.h"
#include <set>

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace Control
{

    SystemStateHandler::SystemStateHandler(boost::asio::io_service::strand& strand,
                                           const NodeUpCb&                  nodeUpCb,
                                           const NodeDownCb&                nodeDownCb)

        : m_strand(strand),
          m_systemState(),
          m_nodeUpCb(nodeUpCb),
          m_nodeDownCb(nodeDownCb)
    {
    }

    void SystemStateHandler::SetNewState(const Safir::Dob::Internal::SP::SystemState& newState)
    {
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


                Node newNode {newState.Name(ix),
                              newState.Id(ix),
                              newState.NodeTypeId(ix),
                              newState.ControlAddress(ix),
                              newState.DataAddress(ix)};

                m_systemState.insert({nodeId, newNode});
                m_nodeUpCb(newNode);

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
                m_nodeDownCb(nodeId);
            }
        }

        // Check if we have any nodes that have disapeared from system state
        for (auto pos = m_systemState.begin(); pos != m_systemState.end(); /*increment below*/)
        {
            if (existingNodeIds.find(pos->first) == existingNodeIds.end())
            {
                m_nodeDownCb(pos->first);
                pos = m_systemState.erase(pos);
            }
            else
            {
                ++pos;
            }
        }
    }
}
}
}
}


