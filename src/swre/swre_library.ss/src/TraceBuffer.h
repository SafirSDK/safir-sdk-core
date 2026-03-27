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
#pragma once

#include <atomic>
#include <mutex>
#include <optional>
#include <queue>
#include <string>

/**
 * TraceBuffer isolates buffer management from the Library class.
 *
 * CRITICAL DESIGN PRINCIPLE: This class NEVER calls external code while
 * holding its lock. All external calls (SendSystemLog, std::wcout, UDP send)
 * must happen AFTER releasing the lock in the caller.
 *
 * This design eliminates the deadlock that occurred when TraceInternal
 * called SendSystemLog while holding the lock, and the syslog callback
 * (OnSystemLog) tried to acquire the same lock.
 */
class TraceBuffer
{
public:
    struct DrainedData
    {
        std::wstring stdout;
        std::wstring udp;
    };

    TraceBuffer();

    /**
     * Append a character to the appropriate buffers.
     * Called from TraceInternal under its lock context.
     *
     * @param ch            The character to append
     * @param toStdout      Whether to append to stdout buffer
     * @param toUdp         Whether to append to UDP buffer
     * @param toSyslog      Whether to append to syslog buffer
     * @param prefix        The prefix string (wide)
     * @param prefixAscii   The ASCII-safe prefix string
     */
    void Append(wchar_t ch,
                bool toStdout,
                bool toUdp,
                bool toSyslog,
                const std::wstring& prefix,
                const std::wstring& prefixAscii);

    /**
     * Append an entire string to the appropriate buffers.
     * Takes lock once and processes all characters, avoiding lock overhead
     * for long strings (important when strings are 500+ characters).
     *
     * @param str           The string to append
     * @param toStdout      Whether to append to stdout buffer
     * @param toUdp         Whether to append to UDP buffer
     * @param toSyslog      Whether to append to syslog buffer
     * @param prefix        The prefix string (wide)
     * @param prefixAscii   The ASCII-safe prefix string
     */
    void AppendString(const std::wstring& str,
                      bool toStdout,
                      bool toUdp,
                      bool toSyslog,
                      const std::wstring& prefix,
                      const std::wstring& prefixAscii);

    /**
     * Append a pre-formatted syslog forward message.
     * Called from OnSystemLog to forward syslog messages to tracer output.
     *
     * @param formatted     The formatted message (includes newline)
     * @param toStdout      Whether to append to stdout buffer
     * @param toUdp         Whether to append to UDP buffer
     */
    void AppendSyslogForward(const std::wstring& formatted, bool toStdout, bool toUdp);

    /**
     * Drain the stdout and UDP buffers.
     * Returns the buffered data; caller outputs OUTSIDE the lock.
     */
    DrainedData Drain();

    /**
     * Drain one pending syslog line for deferred sending.
     * Returns nullopt if no pending lines.
     */
    std::optional<std::wstring> DrainSyslogLine();

    /**
     * Check and clear the overflow flag atomically.
     * Returns true if overflow was detected since last check.
     */
    bool ConsumeOverflowFlag();

private:
    // Internal helper that appends a single character. Caller must hold m_lock.
    void AppendCharInternal(wchar_t ch,
                            bool toStdout,
                            bool toUdp,
                            bool toSyslog,
                            const std::wstring& prefix,
                            const std::wstring& prefixAscii);

    std::mutex m_lock;
    std::wstring m_stdoutBuffer;
    std::wstring m_udpBuffer;
    std::wstring m_syslogLineBuffer;  // Accumulates until newline
    std::queue<std::wstring> m_pendingSyslogLines;
    bool m_prefixPendingSyslog;  // Tracks line state for syslog buffer
    bool m_prefixPendingTracer;  // Tracks line state for stdout/UDP buffers
    std::atomic<bool> m_overflowDetected;
};
