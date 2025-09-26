/******************************************************************************
*
* Copyright Saab AB, 2025 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / lars@foldspace.nu
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

#include <boost/asio.hpp>
#include <boost/asio/ip/udp.hpp>
#include <memory>
#include <array>
#include <string>
#include <utility>
#include <chrono>
#include <cstdint>
#include <atomic>
#include <mutex>
#include <deque>
#include <Safir/Application/Internal/TracerData.h>
#include <Safir/Application/TracerParameters.h>
#include <Safir/Dob/Typesystem/Utilities.h>
#include <Safir/Dob/Internal/Resolver.h>
#include <Safir/Logging/Log.h>

/**
 * Helper responsible for forwarding tracer data to interested receivers
 * using UDP multicast.
 *
 * Incoming UTF-8 payloads are buffered for a short time window (50 ms) and
 * then aggregated into datagrams that fit within a single UDP packet
 * (maximum 1472 bytes).  The datagrams are transmitted to the multicast
 * group specified by parameters.
 *
 * All access to internal state is posted onto the supplied
 * boost::asio::io_context, providing thread-safety without explicit locks.
 */
class TracerDataSender
{
public:
    explicit TracerDataSender(boost::asio::io_context& ioContext, const std::int64_t senderId)
        : m_incarnationId(0)
        , m_senderId(senderId)
        , m_nextSequence(0)
        , m_ioContext(ioContext)
        , m_socket(ioContext)
        , m_timer(ioContext)
        , m_timerActive(false)
        , m_enabled(false)
        , m_programName()
        , m_nodeName()
        , m_multicastEndpoint(
              boost::asio::ip::make_address(
                  Safir::Dob::Typesystem::Utilities::ToUtf8(
                      Safir::Application::TracerParameters::Address())),
              static_cast<unsigned short>(Safir::Application::TracerParameters::Port()))
    {
        using namespace Safir::Dob::Internal::Com;
        const auto localIf = Resolver::ResolveLocalEndpoint(Safir::Dob::Typesystem::Utilities::ToUtf8
                                                                  (Safir::Dob::ThisNodeParameters::DataAddress()));
        if (localIf.empty())
        {
            Safir::Logging::SendSystemLog
                (Safir::Logging::Error,
                 L"Debug Tracer could not resolve address in Safir.Dob.ThisNodeParameters.DataAddress, "
                 L"data will not be sent over the Tracer UDP multicast protocol.");
            return;
        }

        using boost::asio::ip::udp;
        m_socket.open(udp::v4());
        // Allow sender to receive its own packets (useful for local tools)
        m_socket.set_option(boost::asio::ip::multicast::enable_loopback(true));

        m_socket.set_option(boost::asio::ip::multicast::outbound_interface
                            (Resolver::StringToEndpoint(localIf).address().to_v4()));
        m_socket.bind(udp::endpoint(udp::v4(), 0));

        // Construction finished successfully – enable sending
        m_enabled = true;
    }

    TracerDataSender(const TracerDataSender&)            = delete;
    TracerDataSender& operator=(const TracerDataSender&) = delete;

    void Send(const std::string& utf8Payload)
    {
        // If construction failed we do nothing
        if (!m_enabled.load())
        {
            return;
        }

        {
            std::lock_guard<std::mutex> lock(m_inboxMutex);
            m_inbox.emplace_back(utf8Payload);
        }

        // schedule exactly one flush handler
        if (!m_flushScheduled.test_and_set(std::memory_order_acq_rel))
        {
            boost::asio::post(m_ioContext,
                [this]
                {
                    FlushInbox();
                });
        }

    }

public:

    void SetIncarnationId(const std::int64_t incarnationId)
    {
        if (!m_enabled.load()) { return; }
        boost::asio::post(m_ioContext,
            [this, incarnationId] { m_incarnationId = incarnationId; });
    }

    /**
     * Set the UTF-8 program name that will later be placed in outgoing
     * tracer datagrams.  The update is posted onto the io_context to keep
     * concurrent access to member data thread-safe.
     */
    void SetProgramName(const std::string& programName)
    {
        if (!m_enabled.load()) { return; }
        boost::asio::post(m_ioContext,
            [this, programName]
            {
                std::string truncated = programName;
                if (truncated.size() > 255) { truncated.resize(255); }
                m_programName = std::move(truncated);
                m_maxPayloadSize =
                    TracerMaxPacketSize - sizeof(TracerDataHeader) - m_programName.size() - m_nodeName.size();
            });
    }

    /**
     * Set the UTF-8 node name that will later be placed in outgoing
     * tracer datagrams.  The update is executed on the io_context to keep
     * access thread-safe and lock-free.
     */
    void SetNodeName(const std::string& nodeName)
    {
        if (!m_enabled.load()) { return; }
        boost::asio::post(m_ioContext,
            [this, nodeName]
            {
                std::string truncated = nodeName;
                if (truncated.size() > 255) { truncated.resize(255); }
                m_nodeName = std::move(truncated);
                m_maxPayloadSize =
                    TracerMaxPacketSize - sizeof(TracerDataHeader) - m_programName.size() - m_nodeName.size();
            });
    }

private:
    std::int64_t                 m_incarnationId;
    const std::int64_t           m_senderId;
    std::uint32_t                m_nextSequence;
    boost::asio::io_context&     m_ioContext;
    boost::asio::ip::udp::socket m_socket;
    boost::asio::steady_timer    m_timer;
    bool                         m_timerActive;
    std::atomic_bool             m_enabled;
    std::string                  m_pendingPayload;
    std::string                  m_programName;
    std::string                  m_nodeName;
    std::size_t                  m_maxPayloadSize{TracerMaxPacketSize - sizeof(TracerDataHeader)};
    const boost::asio::ip::udp::endpoint m_multicastEndpoint;

    // ------------------------------------------------------------------
    //  Inbox and coordination to avoid flooding io_context queue
    // ------------------------------------------------------------------
    std::deque<std::string>     m_inbox;
    std::mutex                  m_inboxMutex;
    std::atomic_flag            m_flushScheduled = ATOMIC_FLAG_INIT;

    void FlushInbox()
    {
        // Move all queued payloads into the pending buffer
        std::deque<std::string> local;
        {
            std::lock_guard<std::mutex> lock(m_inboxMutex);
            local.swap(m_inbox);
        }
        for (auto& p : local)
        {
            m_pendingPayload += std::move(p);
        }
        // Allow next producer to queue another flush
        m_flushScheduled.clear(std::memory_order_release);

        // (Re)start aggregation timer if not already running
        if (!m_timerActive)
        {
            m_timerActive = true;
            m_timer.expires_after(std::chrono::milliseconds(50));
            m_timer.async_wait([this](const boost::system::error_code& ec)
            {
                if (ec)
                {
                    m_timerActive = false;
                    return;
                }
                SendAggregated();
            });
        }
    }

    void SendAggregated()
    {
        using boost::asio::ip::udp;

        if (m_pendingPayload.empty())
        {
            m_timerActive = false;
            return;
        }

        struct OutgoingMessage
        {
            TracerDataHeader header;
            std::string      payload;
        };

        std::string localPayload = std::move(m_pendingPayload);
        m_pendingPayload.clear();

        // Payload capacity will be calculated per-packet, because it depends on
        // the lengths of the variable-size program/node names that precede it.

        std::size_t offset = 0;
        while (offset < localPayload.size())
        {
            const std::size_t chunkLen = std::min(m_maxPayloadSize, localPayload.size() - offset);
            const bool        isLast   = (offset + chunkLen >= localPayload.size());
            const std::uint64_t timestampUsec =
                static_cast<std::uint64_t>(
                    std::chrono::duration_cast<std::chrono::microseconds>(
                        std::chrono::system_clock::now().time_since_epoch()).count());

            auto msg = std::make_shared<OutgoingMessage>(
                OutgoingMessage{{m_incarnationId,
                                 m_senderId,
                                 m_nextSequence++,
                                 static_cast<std::uint32_t>(chunkLen),
                                 timestampUsec,
                                 static_cast<std::uint8_t>(m_programName.size()),
                                 static_cast<std::uint8_t>(m_nodeName.size()),
                                 0},
                                std::string(localPayload.data() + offset, chunkLen)});

            std::array<boost::asio::const_buffer, 4> bufs{
                boost::asio::buffer(&msg->header, sizeof(msg->header)),
                boost::asio::buffer(m_programName.data(), m_programName.size()),
                boost::asio::buffer(m_nodeName.data(), m_nodeName.size()),
                boost::asio::buffer(msg->payload)
            };

            m_socket.async_send_to(bufs,
                                   m_multicastEndpoint,
                                   [this, isLast, msg](const boost::system::error_code&, std::size_t)
            {
                if (isLast)
                {
                    if (!m_pendingPayload.empty())
                    {
                        // More data arrived while sending – schedule another aggregation.
                        boost::asio::post(m_ioContext,
                                          [this]
                                          {
                                              SendAggregated();
                                          });
                    }
                    else
                    {
                        m_timerActive = false;
                    }
                }
            });

            offset += chunkLen;
        }
    }
};
