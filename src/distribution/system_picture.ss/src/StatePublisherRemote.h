/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
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
#include <Safir/Dob/Internal/SystemPictureDefs.h>
#include <Safir/Utilities/Internal/Id.h>
#include <Safir/Utilities/Internal/AsioPeriodicTimer.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include "Coordinator.h"

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace SP
{
    template <class CoordinatorT, class Communication>
    class StatePublisherRemoteBasic
    {
    public:
        StatePublisherRemoteBasic(boost::asio::io_context& ioContext,
                                  Communication& communication,
                                  const std::map<int64_t, NodeType>& nodeTypes,
                                  const char* const senderId,
                                  CoordinatorT& coordinator,
                                  const std::chrono::steady_clock::duration& period)
            : m_communication(communication)
            , m_senderId(LlufId_Generate64(senderId))
            , m_nodeTypes(nodeTypes)
            , m_coordinator(coordinator)
            , m_publishTimer()
        {
            m_publishTimer.reset(new Safir::Utilities::Internal::AsioPeriodicTimer(ioContext,
                                                                                   period,
                                                                                   [this](const boost::system::error_code& error)
                                                                                   {
                                                                                        Publish(error);
                                                                                   }));


            m_publishTimer->Start();
        }

        void Stop()
        {
            m_publishTimer->Stop();
        }

    private:
        void Publish(const boost::system::error_code& error)
        {
            if (error)
            {
                SEND_SYSTEM_LOG(Alert,
                                << "Unexpected error in StatePublisherRemote::Publish: " << error);
                throw std::logic_error("Unexpected error in StatePublisherRemote::Publish");
            }

            m_coordinator.PerformOnStateMessage([this](const Safir::Utilities::Internal::SharedCharArray& data, const size_t size)
            {
                lllog(8) << "Publishing system state to other nodes" << std::endl;

                for (auto it = m_nodeTypes.cbegin(); it != m_nodeTypes.cend(); ++it)
                {
                    const bool sent = m_communication.Send(0, it->second.id, data, size, m_senderId, true);
                    if (!sent)
                    {
                        lllog(8) << "StatePublisherRemote: Overflow when sending to node type "
                                 << it->second.name.c_str() << std::endl;
                        //No retry handling, since we send cyclically
                    }
                }
            },
                                                true); //we can only send own states
        }

        Communication& m_communication;
        const uint64_t m_senderId;
        const std::map<int64_t, NodeType> m_nodeTypes;
        CoordinatorT& m_coordinator;
        std::unique_ptr<Safir::Utilities::Internal::AsioPeriodicTimer> m_publishTimer;
    };

    typedef StatePublisherRemoteBasic<Coordinator, Com::Communication> StatePublisherRemote;
}
}
}
}
