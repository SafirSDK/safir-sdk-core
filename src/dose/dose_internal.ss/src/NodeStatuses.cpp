/******************************************************************************
*
* Copyright Saab AB, 2011-2013 (http://safir.sourceforge.net)
*
* Created by: Anders Widén / stawi
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
#include <Safir/Dob/Internal/NodeStatuses.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Dob/NodeParameters.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    NodeStatuses* NodeStatuses::m_instance = NULL;

    NodeStatuses& NodeStatuses::Instance()
    {
        ENSURE(m_instance != NULL, << "NodeStatuses::Instance was called before Initialize!!!");
        return *m_instance;
    }

    void NodeStatuses::Initialize()
    {
        m_instance = GetSharedMemory().find_or_construct<NodeStatuses>("NODE_STATUS")(private_constructor_t());
    }

    NodeStatuses::NodeStatuses(private_constructor_t)
        : m_nbrOfNodes(Dob::NodeParameters::NumberOfNodes()),
          m_nodeStatuses (m_nbrOfNodes, Dob::NodeStatus::Expected)
    {
    }

    void NodeStatuses::SetNodeStatus(const Dob::Typesystem::Int32 nodeNumber, NodeStatus::Enumeration status)
    {
        ENSURE(nodeNumber >= 0 && nodeNumber < m_nbrOfNodes, << "NodeStatuses::SetNodeStatus Invalid node number");

        m_nodeStatuses[nodeNumber] = status;
    }

    NodeStatus::Enumeration NodeStatuses::GetNodeStatus(const Dob::Typesystem::Int32 nodeNumber) const
    {
        ENSURE(nodeNumber >= 0 && nodeNumber < m_nbrOfNodes, << "NodeStatuses::GetNodeStatus Invalid node number");

        return m_nodeStatuses[nodeNumber];
    }

    NodeStatuses::Status NodeStatuses::GetNodeStatuses() const
    {
        Status status;

        for (int i = 0; i < m_nbrOfNodes; ++i)
        {
            status.push_back(m_nodeStatuses[i]);
        }
        return status;
    }

    bool NodeStatuses::AnyNodeHasStatus(NodeStatus::Enumeration status) const
    {
        for (int i = 0; i < m_nbrOfNodes; ++i)
        {
            if (m_nodeStatuses[i] == status)
            {
                return true;
            }
        }
        return false;
    }

}
}
}
