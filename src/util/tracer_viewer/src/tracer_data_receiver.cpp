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
#include "tracer_data_receiver.h"

#include <QApplication>
#include <QDebug>
#include <QNetworkInterface>
#include <QString>
#include <QThread>
#include <QTimer>
#include <QTimeZone>
#include <QtNetwork>
#include <Safir/Application/Internal/TracerData.h>
#include <Safir/Application/TracerParameters.h>
#include <Safir/Dob/Typesystem/Utilities.h>
#include <Safir/Dob/Internal/Resolver.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include <boost/lockfree/spsc_queue.hpp>
#include <boost/circular_buffer.hpp>
#include <algorithm>
#include <iostream>
#include <memory>
#include <chrono>
#include "log_entry.h"
#include <vector>
#include <QStringList>

namespace {

    /**
     * Return only the IP/host part of an "addr:port" string.
     *
     * If the string cannot be parsed by Resolver::SplitAddress an empty string is returned
     */
    QString extractAddress(const std::string& addrPort)
    {
        std::string ip;
        unsigned short port = 0;
        if (!Safir::Dob::Internal::Com::Resolver::SplitAddress(addrPort, ip, port))
        {
            return "";
        }

        return QString::fromStdString(ip);
    }

    QString getListenInterfaceAddress()
    {
        using namespace Safir::Dob::Internal::Com;
        using namespace Safir::Dob;

        const std::string endpointAddr =
            Resolver::ResolveLocalEndpoint(Typesystem::Utilities::ToUtf8(ThisNodeParameters::DataAddress()));
        return extractAddress(endpointAddr);
    }

    /**
     * Locate and return the first network interface that owns the given IP
     * address.  An invalid QNetworkInterface is returned if no match is found or
     * if the input string is empty.
     */
    QNetworkInterface findInterfaceForAddress(const QString& address)
    {
        qDebug() << "findInterfaceForAddress: looking for interface with address" << address;

        if (address.isEmpty())
        {
            qDebug() << "Empty address, returning invalid interface";
            return QNetworkInterface();
        }

        const QHostAddress targetAddr(address);

        for (const QNetworkInterface& iface : QNetworkInterface::allInterfaces())
        {
            qDebug() << "findInterfaceForAddress: evaluating interface"
                               << iface.humanReadableName();

            bool matchFound = false;
            for (const QNetworkAddressEntry& addrEntry : iface.addressEntries())
            {
                qDebug() << "findInterfaceForAddress:  -> address "
                                   << addrEntry.ip().toString();

                if (addrEntry.ip() == targetAddr)
                {
                    qDebug() << "findInterfaceForAddress:     ** MATCH **";
                    qDebug() << "findInterfaceForAddress: selected interface"
                                       << iface.humanReadableName()
                                       << "with address " << addrEntry.ip().toString();
                    return iface;   // First (and only) match needed
                }
            }

            if (!matchFound)
            {
                qDebug() << "findInterfaceForAddress:  -> no matching address on this interface";
            }
        }

        return QNetworkInterface(); // No suitable interface found
    }

    /**
     * Join the multicast group on the resolved data interface and – if that join
     * succeeds – also on the first available loop-back interface.  
     *
     * The loop-back join is *skipped* when the local interface join fails to avoid
     * masking problems with the primary interface.
     *
     * \return A QStringList with the human-readable names of the interfaces that
     *         were successfully joined (may be empty on total failure).
     */
    static std::vector<QNetworkInterface> joinDefaultInterfaces(QUdpSocket*           socket,
                                                                const QHostAddress&    group,
                                                                const QNetworkInterface& localIface)
    {
        std::vector<QNetworkInterface> joined;

        if (!socket || !localIface.isValid())
            return joined;

        // First try the resolved data interface
        if (socket->joinMulticastGroup(group, localIface))
        {
            joined.push_back(localIface);

            // Only attempt loop-back after successful local join
            for (const QNetworkInterface& iface : QNetworkInterface::allInterfaces())
            {
                if ((iface.flags() & QNetworkInterface::IsLoopBack) &&
                    (iface.flags() & QNetworkInterface::IsUp))
                {
                    if (socket->joinMulticastGroup(group, iface))
                    {
                        joined.push_back(iface);
                    }
                    break;      // only handle the first loop-back interface
                }
            }
        }

        return joined;
    }

    /**
     * Fallback join: iterate over every interface that is up and multicast-capable
     * (or the loop-back interface) and join the multicast group.  All interfaces
     * successfully joined are returned.
     */
    static std::vector<QNetworkInterface> joinFallbackInterfaces(QUdpSocket* socket,
                                                                 const QHostAddress& group)
    {
        std::vector<QNetworkInterface> joined;
        if (!socket)
            return joined;

        for (const QNetworkInterface& iface : QNetworkInterface::allInterfaces())
        {
            if ((iface.flags() & QNetworkInterface::IsUp) &&
                (iface.flags() & QNetworkInterface::IsRunning) &&
                ((iface.flags() & QNetworkInterface::CanMulticast) ||
                 (iface.flags() & QNetworkInterface::IsLoopBack)))
            {
                if (socket->joinMulticastGroup(group, iface))
                {
                    joined.push_back(iface);
                }
            }
        }
        return joined;
    }

    /**
     * Convert a list of interfaces into a readable debug string, e.g.
     * "eth0 (192.168.1.2), lo (127.0.0.1)".
     */
    static QString interfacesToDebugString(const std::vector<QNetworkInterface>& ifaces)
    {
        QStringList parts;
        for (const QNetworkInterface& iface : ifaces)
        {
            QStringList addrList;
            for (const QNetworkAddressEntry& entry : iface.addressEntries())
            {
                const QHostAddress ip = entry.ip();
                if (!ip.isNull() &&
                    (ip.protocol() == QAbstractSocket::IPv4Protocol ||
                     ip.protocol() == QAbstractSocket::IPv6Protocol))
                {
                    addrList << ip.toString();
                }
            }

            if (addrList.isEmpty())
            {
                parts << iface.humanReadableName();
            }
            else
            {
                parts << QStringLiteral("%1 (%2)")
                             .arg(iface.humanReadableName(),
                                  addrList.join(u", "));
            }
        }
        return parts.join(u", ");
    }

} // anonymous namespace

TracerDataReceiver::TracerDataReceiver()
    : QObject(),
      m_socket(new QUdpSocket(this)),
      m_payloadBuffer(this),
      m_queue()
{
    m_payloadBuffer.open(QIODevice::ReadWrite);

    // Retrieve address and port from global TracerParameters
    const auto groupAddress = QHostAddress(QString::fromStdWString(Safir::Application::TracerParameters::Address()));
    const auto port = static_cast<quint16>(Safir::Application::TracerParameters::Port());

    qDebug() << "TracerDataReceiver: Listening on" << groupAddress.toString() << ":" << port;

    // Bind using an address family that matches the multicast group to make
    // sure we can actually join it.  AnyIPv6 enables dual-stack reception on
    // most platforms.
    QHostAddress bindAddress = (groupAddress.protocol() == QAbstractSocket::IPv6Protocol)
                               ? QHostAddress::AnyIPv6
                               : QHostAddress::AnyIPv4;

    if (!m_socket->bind(bindAddress,
                        port,
                        QUdpSocket::ShareAddress | QUdpSocket::ReuseAddressHint))
    {
        qWarning() << "TracerDataReceiver: Failed to bind UDP socket on port" << port
                   << "using address" << bindAddress.toString()
                   << ":" << m_socket->errorString();

        m_statusText = QStringLiteral("Tracer: ERROR");
        m_statusTip  = QStringLiteral("Bind failed for %1:%2\nReason: %3")
                           .arg(bindAddress.toString())
                           .arg(port)
                           .arg(m_socket->errorString());
        return;
    }

    // ------------------------------------------------------------------
    // Join multicast group on the interface resolved from
    // Safir.Dob.ThisNodeParameters.DataAddress(), falling back to the
    // default interface if the resolution fails.  Always add loop-back
    // interface as well so that locally published traffic is received.
    // ------------------------------------------------------------------
    const QNetworkInterface localIface = findInterfaceForAddress(getListenInterfaceAddress());

    std::vector<QNetworkInterface> joinedInterfaces;

    joinedInterfaces = joinDefaultInterfaces(m_socket, groupAddress, localIface);
    const bool fallbackMode = joinedInterfaces.empty();
    if (fallbackMode)
    {
        joinedInterfaces = joinFallbackInterfaces(m_socket, groupAddress);
    }

    if (joinedInterfaces.empty())
    {
        qWarning() << "TracerDataReceiver: Failed to join multicast group"
                   << groupAddress.toString() << ":" << m_socket->errorString();

        m_statusText = QStringLiteral("Tracer: ERROR");
        m_statusTip  = QStringLiteral("Failed to join %1:%2 on all reasonable interfaces.")
                           .arg(groupAddress.toString())
                           .arg(port);
        return;
    }

    m_statusText = fallbackMode ? QStringLiteral("Tracer: FALLBACK")
                                : QStringLiteral("Tracer: OK");

    qDebug() << "TracerDataReceiver: Joined multicast group"
             << groupAddress.toString()
             << " on "
             << interfacesToDebugString(joinedInterfaces);
 
    m_statusTip = QStringLiteral("Group: %1:%2\nInterfaces: %3")
        .arg(groupAddress.toString())
        .arg(port)
        .arg(interfacesToDebugString(joinedInterfaces));

    connect(m_socket, &QUdpSocket::readyRead,
            this, &TracerDataReceiver::processPendingDatagrams);

    // ------------------------------------------------------------
    // Move receiver to worker thread and start 30 ms drain timer
    // ------------------------------------------------------------
    m_queueTimer.setInterval(30);
    connect(&m_queueTimer, &QTimer::timeout,
            this, &TracerDataReceiver::drainDatagramQueue,
            Qt::DirectConnection); //force execution in GUI thread

    m_workerThread = new QThread(this);
    connect(m_workerThread, &QThread::started,
            &m_queueTimer, QOverload<>::of(&QTimer::start));

    moveToThread(m_workerThread);
    m_workerThread->start();
}

void TracerDataReceiver::setBatchCallback(std::function<void(std::vector<LogEntry>&&)> cb)
{
    m_batchCallback = std::move(cb);
}


void TracerDataReceiver::drainDatagramQueue()
{
    std::vector<LogEntry> batch;
    batch.reserve(1024);

    LogEntry* entry = nullptr;
    while (m_queue.pop(entry))
    {
        batch.emplace_back(std::move(*entry));
        delete entry;
    }

    if (!batch.empty())
    {
#ifndef NDEBUG
        // Ensure we are on the GUI thread when forwarding to the model
        Q_ASSERT(QThread::currentThread() == qApp->thread());
#endif
        if (m_batchCallback)
        {
            m_batchCallback(std::move(batch));
        }
    }
}

TracerDataReceiver::~TracerDataReceiver()
{
    // After stop() the receiver is moved back to the GUI thread, therefore
    // the destructor is expected to run there.
    Q_ASSERT(QThread::currentThread() == qApp->thread());

    // Stop periodic draining
    m_queueTimer.stop();

    // Clean up any payloads still left in the queue
    LogEntry* entry = nullptr;
    while (m_queue.pop(entry))
    {
        delete entry;
    }

    qDebug() << "~TracerDataReceiver";
}

void TracerDataReceiver::stop()
{
    if (!m_workerThread)
        return;

    // stop() must NOT be called from the worker thread itself
    Q_ASSERT(QThread::currentThread() != m_workerThread);

    QThread* guiThread = qApp->thread();

    // ------------------------------------------------------------------
    // 1) Re-parent this receiver to the GUI thread.
    //    moveToThread() must be executed in *current* owner thread
    //    (the worker thread), so we queue a blocking lambda there.
    // ------------------------------------------------------------------
    QMetaObject::invokeMethod(this,
                              [this, guiThread]()
                              {
                                  this->moveToThread(guiThread);
                              },
                              Qt::BlockingQueuedConnection);

    // ------------------------------------------------------------------
    // 2) Now the receiver lives in the GUI thread, we can shut down the
    //    worker thread safely from here.
    // ------------------------------------------------------------------
    m_workerThread->quit();
    m_workerThread->wait();

    // Delete QThread object in GUI thread
    m_workerThread->deleteLater();
    m_workerThread = nullptr;
}

void TracerDataReceiver::setIncarnationFilter(const std::int64_t id)
{
    m_incarnationId.store(id);
}

void TracerDataReceiver::processPendingDatagrams()
{
    while (m_socket->hasPendingDatagrams())
    {
        QByteArray datagram;
        datagram.resize(static_cast<int>(m_socket->pendingDatagramSize()));

        QHostAddress sender;
        quint16 senderPort = 0;

        if (m_socket->readDatagram(datagram.data(),
                                   datagram.size(),
                                   &sender,
                                   &senderPort) <= 0)
        {
            continue;
        }

        handlePacket(datagram, sender, senderPort);
    }
}

void TracerDataReceiver::handlePacket(const QByteArray& datagram,
                                      const QHostAddress& sender,
                                      quint16 senderPort)
{
    if (static_cast<std::size_t>(datagram.size()) < sizeof(TracerDataHeader))
    {
        qWarning() << "TracerDataReceiver: Datagram too small (" << datagram.size()
                   << " bytes) – expected at least " << sizeof(TracerDataHeader);
        return;
    }

    const TracerDataHeader* header =
        reinterpret_cast<const TracerDataHeader*>(datagram.constData());

    // Filter by incarnation id if enabled
    const auto currentFilter = m_incarnationId.load();
    if (currentFilter != 0 && header->incarnationId != currentFilter)
    {
        return;
    }

    const std::size_t expectedBytes =
        sizeof(TracerDataHeader) +
        header->programNameLength +
        header->nodeNameLength +
        header->payloadLength;

    if (static_cast<std::size_t>(datagram.size()) < expectedBytes)
    {
        qWarning() << "TracerDataReceiver: Truncated datagram from"
                   << sender.toString() << ":" << senderPort
                   << "(got" << datagram.size() << "bytes, expected"
                   << expectedBytes << ")";
        return;
    }

    const std::int64_t senderId = header->senderId;

    // ------------------------------------------------------------------
    // Duplicate / out-of-order detection using per-sender sliding window
    // ------------------------------------------------------------------
    auto& state = m_senderState[senderId];
    const std::uint32_t seq         = header->sequenceNumber;
    const std::uint32_t expectedSeq = state.nextExpected;
    const bool firstPacket = (state.recent.empty() && expectedSeq == 0);

    // --------------------------------------------------------------
    // Parse variable-length strings
    // --------------------------------------------------------------
    const char* p = datagram.constData() + sizeof(TracerDataHeader);

    QString programName = QString::fromUtf8(p, header->programNameLength);
    p += header->programNameLength;

    QString nodeName = QString::fromUtf8(p, header->nodeNameLength);
    p += header->nodeNameLength;

    QString payloadText = QString::fromUtf8(p, header->payloadLength);

    // --------------------------------------------------------------
    // Re-assemble payload with any trailing fragment kept from the
    // previous packet belonging to this sender.
    // --------------------------------------------------------------
    QString& tail = m_lineTail[senderId];
    if (!tail.isEmpty())
    {
        payloadText.prepend(tail);
        tail.clear();
    }

    QStringList lines;
    bool addOutOfOrderSuffix = false;

    if (seq == expectedSeq)
    {
        // In-order packet
        state.nextExpected++;
    }
    else if (seq > expectedSeq)
    {
        if (firstPacket)
        {
            // First packet from this sender: initialise sequence tracking
            state.nextExpected = seq + 1;
        }
        else
        {
            // Gap – packets missing before this one
            const std::uint32_t missing = seq - expectedSeq;
            lines << QStringLiteral("### %1 packet(s) missing before sequence %2 from %3@%4 ###")
                         .arg(missing)
                         .arg(seq)
                         .arg(programName)
                         .arg(nodeName);

            m_droppedCount.fetch_add(missing, std::memory_order_relaxed);
            state.nextExpected = seq + 1;
        }
    }
    else // seq < expectedSeq  => duplicate *or* late
    {
        const bool duplicate =
            std::find(state.recent.begin(), state.recent.end(), seq) != state.recent.end();

        if (duplicate)
        {
            return;             // Silently drop exact duplicates
        }

        // Late / out-of-order – keep but mark
        lines << QStringLiteral("### Out-of-order packet sequence %1 expected ≥ %2 from %3@%4 ###")
                     .arg(seq)
                     .arg(expectedSeq)
                     .arg(programName)
                     .arg(nodeName);
        addOutOfOrderSuffix = true;
    }

    // Remember this sequence number for future duplicate detection
    state.recent.push_back(seq);

    // --------------------------------------------------------------
    // Split into individual lines while preserving a possible tail
    // fragment that lacks the terminating newline.
    // --------------------------------------------------------------
    lines = payloadText.split(u'\n', Qt::KeepEmptyParts);

    if (!payloadText.endsWith(u'\n') && !lines.isEmpty())
    {
        tail = lines.takeLast();          // keep unfinished part for next packet
    }

    // Remove empty elements that may have been introduced by KeepEmptyParts
    lines.erase(std::remove_if(lines.begin(), lines.end(),
                               [](const QString& s){ return s.isEmpty(); }),
                lines.end());

    if (addOutOfOrderSuffix)
    {
        lines << QStringLiteral("### End of out-of-order payload ###");
    }

    const QDateTime sendTimeDt =
        QDateTime::fromMSecsSinceEpoch(static_cast<qint64>(header->timestampUsec / 1000),
#if (QT_VERSION >= QT_VERSION_CHECK(6, 5, 0))
                                       QTimeZone::UTC);
#else
                                       Qt::UTC);
#endif
    const QDateTime receiveTimeDt = QDateTime::currentDateTimeUtc();

    for (const QString& line : lines)
    {
        const int sepPos = line.indexOf(u':');
        QString prefix;
        QString msg;
        if (sepPos > -1)
        {
            prefix = line.left(sepPos);
            msg    = line.mid(sepPos + 1).trimmed();
        }
        else
        {
            msg = line;
        }

        LogEntry* entry = new LogEntry{sendTimeDt,
                                       receiveTimeDt,
                                       programName,
                                       nodeName,
                                       prefix,
                                       msg};

        if (!m_queue.push(entry))
        {
            m_droppedCount.fetch_add(1, std::memory_order_relaxed);
            delete entry;
        }
        else
        {
            m_receivedCount.fetch_add(1, std::memory_order_relaxed);
        }
    }
}
