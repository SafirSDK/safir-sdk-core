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
#ifndef __STATE_PUBLISHER_LOCAL_H__
#define __STATE_PUBLISHER_LOCAL_H__

#include <Safir/Utilities/Internal/IpcPublisher.h>
#include <Safir/Utilities/Internal/AsioPeriodicTimer.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace SP
{
    using Safir::Utilities::Internal::AsioPeriodicTimer;
    /**
     * Responsible for publishing state data locally on this computer/node.
     * E.g. for dobexplorer or other SP instance to use.
     */
    class StatePublisherLocal
    {
    public:
        StatePublisherLocal(const boost::shared_ptr<boost::asio::io_service>& ioService,
                            const boost::shared_ptr<Collator>& collator,
                            const char* const name)
            : m_collator(collator)
            , m_publisher(Safir::Utilities::Internal::IpcPublisher::Create(*ioService,name))
            , m_publishTimer(AsioPeriodicTimer::Create(*ioService, 
                                                       boost::chrono::seconds(1),
                                                       [this](const boost::system::error_code& error)
                                                       {
                                                           Publish(error);
                                                       }))
        {
            m_publishTimer->Start();
            m_publisher->Start();
        }

        void Stop()
        {
            m_publishTimer->Stop();
            m_publisher->Stop();
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

            m_collator->PerformOnStateMessage([this](const boost::shared_ptr<char[]>& data, const size_t size)
                                              {
                                                  m_publisher->Send(data, static_cast<boost::uint32_t>(size));
                                              });
        }
        
        const boost::shared_ptr<Collator> m_collator;
        const boost::shared_ptr<Safir::Utilities::Internal::IpcPublisher> m_publisher;
        boost::shared_ptr<Safir::Utilities::Internal::AsioPeriodicTimer> m_publishTimer;
    };
}
}
}
}

#endif

