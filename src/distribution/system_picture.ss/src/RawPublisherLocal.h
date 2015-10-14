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

#include <Safir/Utilities/Internal/IpcPublisher.h>
#include <Safir/Utilities/Internal/AsioPeriodicTimer.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include "RawHandler.h"

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace SP
{
    /**
     * Responsible for publishing raw data locally on this computer/node.
     * E.g. for dobexplorer or other SP instance to use.
     */
    template <class RawHandlerT, class IpcPublisherT>
    class RawPublisherLocalBasic
    {
    public:
        /**
         * The all parameter controls whether remote raw data is sent or just our own data.
         */
        RawPublisherLocalBasic(boost::asio::io_service& ioService,
                               RawHandlerT& rawHandler,
                               const char* const name,
                               const boost::chrono::steady_clock::duration& period,
                               const bool all)
            : m_rawHandler(rawHandler)
            , m_publisher(ioService,name,NULL,NULL)
            , m_all(all)
        {
            m_publishTimer.reset(new Safir::Utilities::Internal::AsioPeriodicTimer(ioService,
                                                                                   period,
                                                                                   [this](const boost::system::error_code& error)
                                                                                   {
                                                                                       Publish(error);
                                                                                   }));

            m_publishTimer->Start();
            m_publisher.Start();
        }

        void Stop()
        {
            m_publishTimer->Stop();
            m_publisher.Stop();
        }

    private:
        void Publish(const boost::system::error_code& error)
        {
            if (error)
            {
                SEND_SYSTEM_LOG(Alert,
                                << "Unexpected error in RawPublisherLocal::Publish: " << error);
                throw std::logic_error("Unexpected error in RawPublisherLocal::Publish");
            }

            lllog(8) << "Publishing raw statistics over ipc" << std::endl;

            const auto sender = [this](std::unique_ptr<char[]> data,
                                       const size_t size)
                {
                    m_publisher.Send(std::move(data), static_cast<uint32_t>(size));
                };

            if (m_all)
            {
                m_rawHandler.PerformOnAllStatisticsMessage(sender);
            }
            else
            {
                m_rawHandler.PerformOnMyStatisticsMessage(sender);
            }
        }

        RawHandlerT& m_rawHandler;
        IpcPublisherT m_publisher;
        std::unique_ptr<Safir::Utilities::Internal::AsioPeriodicTimer> m_publishTimer;
        const bool m_all;
    };

    typedef RawPublisherLocalBasic<RawHandler, Safir::Utilities::Internal::IpcPublisher> RawPublisherLocal;
}
}
}
}
