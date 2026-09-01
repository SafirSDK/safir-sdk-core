/******************************************************************************
*
* Copyright Saab AB, 2015, 2026 (http://safirsdkcore.com)
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
    /**
     * Warns when a periodic timer handler runs late, i.e. when the io_context
     * is not able to dispatch work on time.
     *
     * Note what this does and does not measure. The handler is bound to a
     * strand that this class owns and that nothing else ever posts to, so
     * there is never anything queued ahead of it: what is measured is the
     * scheduling latency of the io_context as a whole, not the backlog of any
     * particular strand. On a multi-threaded io_context an individual strand
     * can be far behind while this monitor stays silent, so do not read a
     * quiet monitor as "no queue is backed up".
     */
    class AsioLatencyMonitor
    {
    public:
        explicit AsioLatencyMonitor(const std::string& identifier,
                                    const std::chrono::steady_clock::duration& warningThreshold,
                                    boost::asio::io_context& ioContext)
            : m_identifier(identifier)
            , m_tolerance(warningThreshold)
            , m_strand(ioContext)
            , m_timer(ioContext)
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

            //The handler must run in m_strand so that it is serialized against the
            //cancel() that Stop() posts there. Without that, an in-flight handler
            //can call expires_after()/async_wait() on m_timer while another thread
            //of the io_context is calling cancel() on it, which is undefined
            //behaviour.
            m_timer.async_wait(boost::asio::bind_executor
                               (m_strand,
                                [this](const boost::system::error_code& error)
                                {
                                    if (error || m_stop)
                                    {
                                        return;
                                    }

                                    const auto latency = std::chrono::duration_cast<std::chrono::milliseconds>
                                        (std::chrono::steady_clock::now() - m_timer.expiry());

                                    if (latency > m_tolerance)
                                    {
                                        const auto threshold = std::chrono::duration_cast
                                            <std::chrono::milliseconds>(m_tolerance).count();

                                        //Report what was measured and what it puts at risk, but
                                        //do not guess at a cause: this cannot tell an overloaded
                                        //machine from a long-running handler or a descheduled
                                        //virtual machine.
                                        SEND_SYSTEM_LOG(Warning, << "Event loop latency for '"
                                                        << m_identifier.c_str() << "' is "
                                                        << latency.count() << " ms, over the "
                                                        << threshold << " ms threshold. Timers and "
                                                        << "heartbeats are running late; if this "
                                                        << "persists, nodes may be falsely "
                                                        << "considered dead.");
                                    }

                                    //schedule next latency check
                                    ScheduleTimer();
                                }));

        }

        const std::string m_identifier;
        const std::chrono::steady_clock::duration m_tolerance;

        //Private on purpose: it exists to serialize operations on m_timer, and
        //because nothing else posts to it the monitor keeps measuring the
        //io_context rather than some queue's backlog. Do not post other work here.
        boost::asio::io_context::strand m_strand;
        boost::asio::steady_timer m_timer;

        std::atomic<bool> m_stop;
    };
}
}
}
