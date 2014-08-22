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
#pragma once

#include <boost/asio.hpp>
#include <boost/function.hpp>
#include <Safir/Dob/Internal/SystemState.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace Control
{
    /**
     * @brief The SystemStateHandler class holds the current system state. When a new
     * state is set this class figures out what has happened and the appropriate
     * callbacks are called.
     */
    class SystemStateHandler
    {
    public:

        typedef boost::function<void(const int index)> NodeNewCb;
        typedef boost::function<void(const int index)> NodeUpCb;
        typedef boost::function<void(const int index)> NodeDeadCb;
        typedef boost::function<void(const int index)> CoordinatorElectedCb;

        explicit SystemStateHandler(boost::asio::io_service::strand& strand,
                                    const NodeNewCb&                 nodeNewCb,
                                    const NodeUpCb&                  nodeUpCb,
                                    const NodeDeadCb&                nodeDeadCb,
                                    const CoordinatorElectedCb&      coordinatorElectedCb);

        void SetNewState(const Safir::Dob::Internal::SP::SystemState& newState);

    private:

        boost::asio::io_service::strand& m_strand;

        Safir::Dob::Internal::SP::SystemState m_currentState;

        NodeNewCb               m_nodeNewCb;
        NodeUpCb                m_nodeUpCb;
        NodeDeadCb              m_nodeDeadCb;
        CoordinatorElectedCb    m_coordinatorElectedCb;


    };

}
}
}
}



