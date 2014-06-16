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
#ifndef __RAW_PUBLISHER_LOCAL_H__
#define __RAW_PUBLISHER_LOCAL_H__

#include <Safir/Utilities/Internal/IpcPublisher.h>
#include <Safir/Utilities/Internal/AsioPeriodicTimer.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include "CrcUtils.h"
#include "RawHandler.h"

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
     * Responsible for publishing raw data locally on this computer/node.
     * E.g. for dobexplorer or other SP instance to use.
     */
    template <class Handler, class Publisher>
    class RawPublisherLocalBasic
    {
    public:
        RawPublisherLocalBasic(boost::asio::io_service& ioService,
                               Handler& rawHandler,
                               const char* const name,
                               const boost::chrono::steady_clock::duration& period)
            : m_rawHandler(rawHandler)
            , m_publisher(ioService,name)
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
                                << "Unexpected error in RawPublisherLocal::Publish: " << error);
                throw std::logic_error("Unexpected error in RawPublisherLocal::Publish");
            }

            lllog(8) << "Publishing raw statistics over ipc" << std::endl;

#ifdef CHECK_CRC
            const int crcBytes = sizeof(int);
#else
            const int crcBytes = 0;
#endif

            m_rawHandler.PerformOnAllStatisticsMessage([this,crcBytes](std::unique_ptr<char[]> data, const size_t size)
                                                       {
#ifdef CHECK_CRC
                                                           const int crc = GetCrc32(data.get(), size - crcBytes);
                                                           memcpy(data.get() + size - crcBytes, &crc, sizeof(int));
#endif
                                                           m_publisher.Send(std::move(data), static_cast<uint32_t>(size));
                                                       },
                                                       crcBytes);
        }
        
        Handler& m_rawHandler;
        Publisher m_publisher;
        Safir::Utilities::Internal::AsioPeriodicTimer m_publishTimer;
    };

    typedef RawPublisherLocalBasic<RawHandler, Safir::Utilities::Internal::IpcPublisher> RawPublisherLocal;
}
}
}
}

#endif

