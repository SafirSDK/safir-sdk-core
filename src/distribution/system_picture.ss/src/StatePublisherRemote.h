/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safir.sourceforge.net)
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
    template <class Handler, class Communication>
    class StatePublisherRemoteBasic
    {
    public:
        StatePublisherRemoteBasic(boost::asio::io_service& ioService,
                                  Communication& communication,
                                  const std::map<int64_t, NodeType>& nodeTypes,
                                  const char* const senderId,
                                  Handler& coordinator,
                                  const boost::chrono::steady_clock::duration& period)
            : m_communication(communication)
            , m_senderId(LlufId_Generate64(senderId))
            , m_nodeTypes(nodeTypes)
            , m_coordinator(coordinator)
            , m_publishTimer(ioService, 
                             period,
                             [this](const boost::system::error_code& error)
                             {
                                 Publish(error);
                             })
        {
            m_publishTimer.Start();
        }

        void Stop()
        {
            m_publishTimer.Stop();
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

#ifdef CHECK_CRC
            const int crcBytes = sizeof(int);
#else
            const int crcBytes = 0;
#endif
            
            m_coordinator.PerformOnStateMessage([this,crcBytes](const boost::shared_ptr<char[]>& data, const size_t size)
            {
                lllog(8) << "Publishing system state to other nodes" << std::endl;
            
#ifdef CHECK_CRC
                const int crc = GetCrc32(data.get(), size - crcBytes);
                memcpy(data.get() + size - crcBytes, &crc, sizeof(int));
#endif
                
                for (const auto& it: m_nodeTypes)
                {
                    const bool sent = m_communication.SendToNodeType(it.second.id, std::move(data), size, m_senderId);
                    if (!sent)
                    {
                        lllog(8) << "StatePublisherRemote: Overflow when sending to node type " 
                                 << it.second.name.c_str() << std::endl;
                        //No retry handling, since we send cyclically
                    }
                }
            },
                                                crcBytes,
                                                true); //we can only send own states
        }

        Communication& m_communication;
        const uint64_t m_senderId;
        const std::map<int64_t, NodeType> m_nodeTypes;
        Handler& m_coordinator;
        Safir::Utilities::Internal::AsioPeriodicTimer m_publishTimer;
    };

    typedef StatePublisherRemoteBasic<Coordinator, Com::Communication> StatePublisherRemote;
}
}
}
}


