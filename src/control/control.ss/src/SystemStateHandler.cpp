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

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace Control
{

    SystemStateHandler::SystemStateHandler(boost::asio::io_service::strand& strand,
                                           const NodeNewCb&                 nodeNewCb,
                                           const NodeUpCb&                  nodeUpCb,
                                           const NodeDeadCb&                nodeDeadCb,
                                           const CoordinatorElectedCb&      coordinatorElectedCb)
        : m_strand(strand),
          m_currentState(),
          m_nodeNewCb(nodeNewCb),
          m_nodeUpCb(nodeUpCb),
          m_nodeDeadCb(nodeDeadCb),
          m_coordinatorElectedCb(coordinatorElectedCb)
    {

    }

    void SystemStateHandler::SetNewState(const Safir::Dob::Internal::SP::SystemState& newState)
    {
        for (int idx = 0; idx < newState.Size(); ++idx)
        {
            if (idx > m_currentState.Size())
            {
                m_nodeNewCb(idx);
            }

            // TODO Mera händelser här
        }
    }
}
}
}
}


