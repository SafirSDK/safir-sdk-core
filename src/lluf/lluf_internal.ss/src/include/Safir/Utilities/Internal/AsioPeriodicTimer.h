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
#ifndef __LLUF_ASIO_PERIODIC_TIMER_H__
#define __LLUF_ASIO_PERIODIC_TIMER_H__

#include <atomic>
#include <functional>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#endif

#include <boost/chrono.hpp>
#include <boost/asio.hpp>
#include <boost/asio/steady_timer.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

namespace Safir
{
namespace Utilities
{
namespace Internal
{
    /**
     * Cyclic timer helper class.
     *
     * Guarantees that only one handler will be called at a time.
     * The timer will be rescheduled after the handler is called,
     * but with the timeout set so that it includes the time
     * that the handler callback took.
     */
    class AsioPeriodicTimer
    {
    public:
        AsioPeriodicTimer(boost::asio::io_context& io,
                          const std::chrono::steady_clock::duration& period,
                          const std::function<void(const boost::system::error_code& error)>& handler)
            : m_strand(io)
            , m_timer(io)
            , m_period(period)
            , m_handler(handler)
            , m_started(false)
        {

        }

        /**
         * Start the timer.
         * Thread safe, and multiple calls will be ignored.
         */
        void Start()
        {
            const bool was_started = m_started.exchange(true);
            if (!was_started)
            {
                boost::asio::dispatch(m_strand, [this]
                                  {
                                      m_timer.expires_after(m_period);
                                      ScheduleTimer();
                                  });
            }
        }

        /**
         * Stop the timer.
         * Thread safe, and multiple calls will be ignored.
         */
        void Stop()
        {
            const bool was_started = m_started.exchange(false);
            if (was_started)
            {
                boost::asio::dispatch(m_strand, [this]
                                  {
                                      m_timer.cancel();
                                  });
            }

        }

    private:
        AsioPeriodicTimer(const AsioPeriodicTimer&) = delete;
        const AsioPeriodicTimer& operator=(const AsioPeriodicTimer&) = delete;

        void Timeout(const boost::system::error_code& error)
        {
            if (!m_started)
            {
                return;
            }

            //Set timeout before calling the handler so that we get a timer
            //that does not drift when handler calls take a long time.
            if (!error)
            {
                m_timer.expires_after(m_period);
            }

            m_handler(error);

            if (!error)
            {
                ScheduleTimer();
            }
        }

        void ScheduleTimer()
        {
            m_timer.async_wait(boost::asio::bind_executor(m_strand, [this](const boost::system::error_code& error)
                                             {
                                                 Timeout(error);
                                             }));
        }

        boost::asio::io_context::strand m_strand;
        boost::asio::steady_timer m_timer;
        const std::chrono::steady_clock::duration m_period;
        const std::function<void(const boost::system::error_code& error)> m_handler;
        std::atomic<bool> m_started;
    };
}
}
}

#endif
