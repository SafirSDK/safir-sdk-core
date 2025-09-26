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

#include <QObject>
#include <QUdpSocket>
#include <QHostAddress>
#include <QBuffer>
#include <QString>
#include <QByteArray>
#include <unordered_map>
#include <boost/circular_buffer.hpp>
#include <chrono>
#include <cstdint>
#include <atomic>
#include <memory>
#include <boost/lockfree/spsc_queue.hpp>
#include <functional>
#include <QThread>
#include <QTimer>
#include "log_entry.h"

class TracerDataReceiver : public QObject
{
    Q_OBJECT
public:
    explicit TracerDataReceiver();
    ~TracerDataReceiver() override;

    // Stop the worker thread and move this object back to the GUI thread.
    // Call exactly once from the GUI thread during application shutdown.
    void stop();

    // Update the incarnation id to filter on (0 disables filtering)
    void setIncarnationFilter(std::int64_t incarnationId);

    // -----------------------------------------------------------------
    // Fast, thread-safe accessors for statistics (atomic load only)
    // -----------------------------------------------------------------
    std::uint64_t receivedCount() const noexcept { return m_receivedCount.load(); }
    std::uint64_t droppedCount()  const noexcept { return m_droppedCount.load(); }

    // Register callback that will receive batches in the GUI thread
    void setBatchCallback(std::function<void(std::vector<LogEntry>&&)> cb);

public:
    // Query after construction; the values never change.
    const QString& socketStatusText()    const noexcept { return m_statusText; }
    const QString& socketStatusTooltip() const noexcept { return m_statusTip; }

private slots:
    void processPendingDatagrams();
    void drainDatagramQueue();

private:
    // Handle one complete, validated UDP datagram
    void handlePacket(const QByteArray& datagram,
                      const QHostAddress& sender,
                      quint16 senderPort);

    QUdpSocket* m_socket;
    QBuffer  m_payloadBuffer;

    // ---------------------------------------------------------------------
    // Worker-thread infrastructure and lock-free queue for parsed payloads
    // ---------------------------------------------------------------------
    // ------------------------------------------------------------------
    //  Datagram with metadata passed from worker thread to GUI thread
    // ------------------------------------------------------------------

    boost::lockfree::spsc_queue<
        LogEntry*,
        boost::lockfree::capacity<4096>> m_queue;
    QThread* m_workerThread{nullptr};
    QTimer   m_queueTimer;

    // Callback into the GUI layer (set by LogModel)
    std::function<void(std::vector<LogEntry>&&)> m_batchCallback;


    // Current incarnation filter used when validating incoming tracer packets
    std::atomic<std::int64_t> m_incarnationId{0};

    // Per-sender tracking state: next expected sequence number and a
    // 64-element sliding window of the most recently accepted sequences
    struct SenderState
    {
        std::uint32_t nextExpected{0};
        boost::circular_buffer<std::uint32_t> recent{64};
    };
    std::unordered_map<std::int64_t, SenderState> m_senderState;

    // Total number of dropped payloads (queue overflow) or packets detected
    // missing via sequence-number gaps.  Read from GUI thread if needed.
    std::atomic<std::uint64_t> m_droppedCount{0};

    // Total number of packets that have been received and accepted
    std::atomic<std::uint64_t> m_receivedCount{0};

    // ------------------------------------------------------------------
    // Immutable status reported to GUI
    // ------------------------------------------------------------------
    QString m_statusText  = QStringLiteral("Tracer: --");
    QString m_statusTip   = QString();
};
