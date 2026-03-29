/******************************************************************************
*
* Copyright Saab AB, 2026 (http://safirsdkcore.com)
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
#include "TraceBuffer.h"

TraceBuffer::TraceBuffer()
    : m_prefixPendingSyslog(true)
    , m_prefixPendingTracer(true)
    , m_overflowDetected(false)
{
}

void TraceBuffer::Append(wchar_t ch,
                         bool toStdout,
                         bool toUdp,
                         bool toSyslog,
                         const std::wstring& prefix,
                         const std::wstring& prefixAscii)
{
    std::lock_guard<std::mutex> lock(m_lock);
    AppendCharInternal(ch, toStdout, toUdp, toSyslog, prefix, prefixAscii);
}

void TraceBuffer::AppendString(const std::wstring& str,
                               bool toStdout,
                               bool toUdp,
                               bool toSyslog,
                               const std::wstring& prefix,
                               const std::wstring& prefixAscii)
{
    std::lock_guard<std::mutex> lock(m_lock);

    // Process all characters while holding lock once
    for (wchar_t ch : str)
    {
        AppendCharInternal(ch, toStdout, toUdp, toSyslog, prefix, prefixAscii);
    }
}

void TraceBuffer::AppendCharInternal(wchar_t ch,
                                     bool toStdout,
                                     bool toUdp,
                                     bool toSyslog,
                                     const std::wstring& prefix,
                                     const std::wstring& prefixAscii)
{
    // NOTE: Caller must hold m_lock

    // Handle syslog prefix
    if (m_prefixPendingSyslog && toSyslog)
    {
        m_syslogLineBuffer.append(prefix);
        m_syslogLineBuffer.append(L": ");
        m_prefixPendingSyslog = false;
    }

    // Handle stdout/UDP prefix
    if (m_prefixPendingTracer)
    {
        if (toStdout)
        {
            m_stdoutBuffer.append(prefixAscii);
            m_stdoutBuffer.append(L": ");
        }

        if (toUdp)
        {
            m_udpBuffer.append(prefix);
            m_udpBuffer.append(L": ");
        }

        m_prefixPendingTracer = false;
    }

    // Append character to stdout buffer
    if (toStdout)
    {
        // Strip non-ASCII chars for stdout
        if ((ch & ~0x7F) == 0)
        {
            m_stdoutBuffer.push_back(ch);
        }
        else
        {
            m_stdoutBuffer.push_back('@');
        }
    }

    // Append to syslog line buffer
    if (toSyslog)
    {
        if (ch == '\n')
        {
            // Complete line - queue it for deferred sending
            m_pendingSyslogLines.push(std::move(m_syslogLineBuffer));
        }
        else
        {
            m_syslogLineBuffer.push_back(ch);
        }
    }

    // Append to UDP buffer
    if (toUdp)
    {
        m_udpBuffer.push_back(ch);
    }

    // Handle newline: reset prefix state
    if (ch == '\n')
    {
        m_prefixPendingSyslog = true;
        m_prefixPendingTracer = true;

        // Check for overflow - set flag atomically, caller will handle
        if (m_stdoutBuffer.size() > 50000 || m_udpBuffer.size() > 50000)
        {
            m_overflowDetected.store(true, std::memory_order_release);
            m_stdoutBuffer.clear();
            m_udpBuffer.clear();
            // Also clear syslog buffers to prevent unbounded growth
            m_syslogLineBuffer.clear();
            std::queue<std::wstring>().swap(m_pendingSyslogLines); //std::queue has no clear()
        }
    }
}

void TraceBuffer::AppendSyslogForward(const std::wstring& formatted, bool toStdout, bool toUdp)
{
    std::lock_guard<std::mutex> lock(m_lock);

    // If we're mid-line in the trace output, start on a new line to keep syslog
    // messages cleanly separated.
    if (!m_prefixPendingTracer)
    {
        if (toStdout)
        {
            m_stdoutBuffer.append(L"\n");
        }
        if (toUdp)
        {
            m_udpBuffer.append(L"\n");
        }
        m_prefixPendingTracer = true;
    }

    if (toStdout)
    {
        m_stdoutBuffer.append(formatted);
    }

    if (toUdp)
    {
        m_udpBuffer.append(formatted);
    }
}

TraceBuffer::DrainedData TraceBuffer::Drain()
{
    std::lock_guard<std::mutex> lock(m_lock);

    DrainedData data;
    data.out = std::move(m_stdoutBuffer);
    data.udp = std::move(m_udpBuffer);

    return data;
}

std::optional<std::wstring> TraceBuffer::DrainSyslogLine()
{
    std::lock_guard<std::mutex> lock(m_lock);

    if (m_pendingSyslogLines.empty())
    {
        return std::nullopt;
    }

    std::wstring line = std::move(m_pendingSyslogLines.front());
    m_pendingSyslogLines.pop();
    return line;
}

bool TraceBuffer::ConsumeOverflowFlag()
{
    return m_overflowDetected.exchange(false, std::memory_order_acq_rel);
}
