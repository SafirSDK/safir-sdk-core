/******************************************************************************
*
* Copyright Saab AB, 2015 (http://safirsdkcore.com)
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

#include <Safir/Utilities/Internal/SystemLog.h>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#endif

#include <boost/asio.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class AsioLatencyMonitor
    {
    public:
        explicit AsioLatencyMonitor(const std::string& identifier,
                                    const std::chrono::steady_clock::duration& warningThreshold,
                                    boost::asio::io_context::strand& strand)
            : m_identifier(identifier)
            , m_tolerance(warningThreshold)
            , m_strand(strand)
            , m_timer(m_strand.context())
            , m_stop(false)
        {
            ScheduleTimer();
        }

        void Stop()
        {
            m_stop = true;
            boost::asio::post(m_strand,[this]()
                          {
                              m_timer.cancel();
                          });
        }

    private:
        void ScheduleTimer()
        {
            if (m_stop)
            {
                return;
            }

            m_timer.expires_after(std::chrono::seconds(1));
            m_timer.async_wait([this](const boost::system::error_code& error)
                               {
                                   if (error || m_stop)
                                   {
                                       return;
                                   }

                                   const auto latency = std::chrono::duration_cast<std::chrono::milliseconds>
                                       (std::chrono::steady_clock::now() - m_timer.expiry());

                                   if (latency > m_tolerance)
                                   {
                                       SEND_SYSTEM_LOG(Warning, << "Boost.Asio latency for '"
                                                       << m_identifier.c_str() << "' is at " << latency.count()
                                                       << " ms. If this happens a lot your system is overloaded and may start misbehaving.");
                                   }

                                   //schedule next latency check
                                   ScheduleTimer();
                               });

        }

        const std::string m_identifier;
        const std::chrono::steady_clock::duration m_tolerance;
        boost::asio::io_context::strand& m_strand;
        boost::asio::steady_timer m_timer;

        std::atomic<bool> m_stop;
    };
}
}
}
