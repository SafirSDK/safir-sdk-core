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

#include <Safir/Utilities/Internal/IpcPublisher.h>
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
    /**
     * Responsible for publishing state data locally on this computer/node.
     * E.g. for dobexplorer or other SP instance to use.
     */
    template <class CoordinatorT, class IpcPublisherT>
    class StatePublisherLocalBasic
    {
    public:
        StatePublisherLocalBasic(boost::asio::io_service& ioService,
                                 CoordinatorT& coordinator,
                                 const char* const name,
                                 const boost::chrono::steady_clock::duration& period)
            : m_coordinator(coordinator)
            , m_publisher(ioService,name,nullptr,nullptr)
            , m_publishTimer(ioService,
                             period,
                             [this](const boost::system::error_code& error)
                             {
                                 Publish(error);
                             })
        {
            m_publishTimer.Start();
            m_publisher.Start();
        }

        void Stop()
        {
            m_publishTimer.Stop();
            m_publisher.Stop();
        }

    private:
        void Publish(const boost::system::error_code& error)
        {
            if (error)
            {
                SEND_SYSTEM_LOG(Alert,
                                << "Unexpected error in StatePublisherLocal::Publish: " << error);
                throw std::logic_error("Unexpected error in StatePublisherLocal::Publish");
            }

            lllog(8) << "Publishing system state over ipc" << std::endl;

            m_coordinator.PerformOnStateMessage([this](std::unique_ptr<char[]> data, const size_t size)
                                                {
                                                    m_publisher.Send(std::move(data), static_cast<uint32_t>(size));
                                                },
                                                false); //ok to send anyones state
        }

        CoordinatorT& m_coordinator;
        IpcPublisherT m_publisher;
        Safir::Utilities::Internal::AsioPeriodicTimer m_publishTimer;
    };

    typedef StatePublisherLocalBasic<Coordinator, Safir::Utilities::Internal::IpcPublisher> StatePublisherLocal;
}
}
}
}
