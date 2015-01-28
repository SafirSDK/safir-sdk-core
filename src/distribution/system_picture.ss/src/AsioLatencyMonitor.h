/******************************************************************************
*
* Copyright Saab AB, 2015 (http://safir.sourceforge.net)
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
        explicit AsioLatencyMonitor(boost::asio::io_service::strand& strand)
            : m_strand(strand)
            , m_timer(m_strand.get_io_service())
            , m_stop(false)
        {
            ScheduleTimer();
        }

        void Stop()
        {
            m_stop = true;
        }

    private:
        void ScheduleTimer()
        {
            if (m_stop)
            {
                return;
            }

            m_timer.expires_from_now(boost::chrono::seconds(1));
            m_timer.async_wait([this](const boost::system::error_code& error)
                               {
                                   if (error || m_stop)
                                   {
                                       return;
                                   }

                                   MeasureLatency();
                               });

        }

        void MeasureLatency()
        {
            const auto postTime = boost::chrono::steady_clock::now();
            m_strand.post([this,postTime]
                          {
                              const auto latency = boost::chrono::duration_cast<boost::chrono::milliseconds>
                                  (boost::chrono::steady_clock::now() - postTime);

                              if (latency > boost::chrono::milliseconds(100))
                              {
                                  lllog(0) << "Warning: Boost.Asio latency is at " << latency << std::endl;
                              }

                              //schedule next latency check
                              ScheduleTimer();
                          });
        }

        boost::asio::io_service::strand& m_strand;
        boost::asio::steady_timer m_timer;

        std::atomic<bool> m_stop;
    };
}
}
}
