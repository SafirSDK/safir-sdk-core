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
#include "log_model.h"
#include <QAbstractItemView>
#include <fstream>
#include <sstream>
#include <iomanip>
#include <QRandomGenerator>
#include <cstddef>
#include <QFile>
#include <QTextStream>
#include "tracer_data_receiver.h"

LogModel::LogModel(const std::shared_ptr<TracerDataReceiver>& dataReceiver, QObject* parent)
    : QAbstractTableModel(parent)
    , m_dataReceiver(dataReceiver)
{
    constexpr std::size_t kMaxRows = 500'000; // adjust as needed
    m_entries.set_capacity(kMaxRows);

    // Register callback to receive ready batches
    if (m_dataReceiver)
    {
        m_dataReceiver->setBatchCallback(
            [this](std::vector<LogEntry>&& batch)
            {
#ifndef NDEBUG
                Q_ASSERT(QThread::currentThread() == this->thread());
#endif
                addEntries(std::move(batch));
            });
    }
}

/* ---------------------------------------------------------------------------------
 *  Construct model from a CSV file produced by tracer_listener --csv
 * --------------------------------------------------------------------------------*/
LogModel::LogModel(const QString& csvFilePath, QObject* parent)
    : QAbstractTableModel(parent)
    , m_dataReceiver(nullptr)
{
    // ------------------------------------------------------------------
    //  Size circular buffer exactly to the number of data rows
    //  in the CSV file (header line excluded)
    // ------------------------------------------------------------------
    std::ifstream in(csvFilePath.toStdString());
    if (!in)
    {
        throw std::runtime_error("Failed to open CSV file: "
                                 + csvFilePath.toStdString());
    }

    std::size_t lineCount = 0;
    std::string line;
    while (std::getline(in, line))
        ++lineCount;

    if (lineCount > 0)
        --lineCount;                // deduct header row

    m_entries.set_capacity(lineCount);

    // Rewind stream for actual parsing
    in.clear();
    in.seekg(0);

    beginResetModel();

    // Skip header line
    std::getline(in, line);

    while (std::getline(in, line))
    {
        if (line.empty())
            continue;

        std::istringstream iss(line);
        std::string sendStr, recvStr, progStr, nodeStr, prefixStr, msgStr;

        iss >> std::quoted(sendStr);
        if (iss.peek() == ',') iss.get();
        iss >> std::quoted(recvStr);
        if (iss.peek() == ',') iss.get();
        iss >> std::quoted(progStr);
        if (iss.peek() == ',') iss.get();
        iss >> std::quoted(nodeStr);
        if (iss.peek() == ',') iss.get();
        iss >> std::quoted(prefixStr);
        if (iss.peek() == ',') iss.get();
        iss >> std::quoted(msgStr);

        const QDateTime sendT =
            QDateTime::fromString(QString::fromStdString(sendStr),
                                  Qt::ISODateWithMs).toUTC();
        const QDateTime recvT =
            QDateTime::fromString(QString::fromStdString(recvStr),
                                  Qt::ISODateWithMs).toUTC();

        m_entries.push_back(LogEntry{sendT,
                                     recvT,
                                     QString::fromStdString(progStr),
                                     QString::fromStdString(nodeStr),
                                     QString::fromStdString(prefixStr),
                                     QString::fromStdString(msgStr)});
    }

    endResetModel();
}

LogModel::~LogModel() = default;

int LogModel::rowCount(const QModelIndex& parent) const
{
    if (parent.isValid())
    {
        return 0;
    }
    return static_cast<int>(m_entries.size());
}

int LogModel::columnCount(const QModelIndex& parent) const
{
    if (parent.isValid())
    {
        return 0;
    }
    return static_cast<int>(ColumnCount);
}

QVariant LogModel::headerData(int section, Qt::Orientation orientation, int role) const
{
    if (orientation != Qt::Horizontal)
    {
        return QVariant();
    }

    // Hide certain columns by default.
    if (role == HideColumnByDefaultRole)
    {
        switch (section)
        {
        case ReceiveTime:
        case NodeName:
            return true;   // hide these columns
        default:
            return false;  // other columns visible
        }
    }

    // Provide tool-tips for the header cells.
    if (role == Qt::ToolTipRole)
    {
        switch (section)
        {
        case SendTime:    return QStringLiteral("Time when the log message was sent");
        case ReceiveTime: return QStringLiteral("Time when the message was received");
        case ProgramName: return QStringLiteral("Name of the program that produced the log");
        case NodeName:    return QStringLiteral("Name of the node where the program ran");
        case Prefix:      return QStringLiteral("Tracer prefix used for this log");
        case Message:     return QStringLiteral("The log message text");
        default:          return QVariant();
        }
    }

    // Left-align header labels.
    if (role == Qt::TextAlignmentRole)
    {
        return static_cast<int>(Qt::AlignLeft | Qt::AlignVCenter);
    }

    // Regular display text.
    if (role != Qt::DisplayRole)
    {
        return QVariant();
    }

    switch (section)
    {
    case SendTime:    return QStringLiteral("Timestamp");
    case ReceiveTime: return QStringLiteral("Receive Time");
    case ProgramName: return QStringLiteral("Program");
    case NodeName:    return QStringLiteral("Node");
    case Prefix:      return QStringLiteral("Prefix");
    case Message:     return QStringLiteral("Message");
    default:          return QVariant();
    }
}

Qt::ItemFlags LogModel::flags(const QModelIndex& index) const
{
    if (!index.isValid())
        return Qt::NoItemFlags;

    // Allow the delegate to create its read-only editor while still keeping the
    // cells non-editable from the model’s point of view.
    return Qt::ItemIsSelectable |
           Qt::ItemIsEnabled   |
           Qt::ItemIsEditable;   // needed so QTableView will invoke the delegate
}

QVariant LogModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid())
    {
        return QVariant();
    }

    // --------------------------------------------------------------
    //  Highlight colours
    // --------------------------------------------------------------
    if (role == Qt::BackgroundRole || role == Qt::ForegroundRole)
    {
        const LogEntry& entry = m_entries.at(index.row());
        for (const auto& rule : m_highlightRules)
        {
            if (rule.regex.isValid() &&
                (rule.regex.match(entry.message).hasMatch()
                 || rule.regex.match(entry.programName).hasMatch()
                 || rule.regex.match(entry.nodeName).hasMatch()
                 || rule.regex.match(entry.prefix).hasMatch()))
            {
                if (role == Qt::BackgroundRole)
                    return rule.color;
                return rule.color.lightness() < 128 ? QColor(Qt::white)
                                                     : QColor(Qt::black);
            }
        }
        return QVariant();
    }

    // Return the same contents for both display and tooltip roles
    if (role != Qt::DisplayRole && role != Qt::ToolTipRole)
    {
        return QVariant();
    }

    const LogEntry& entry = m_entries.at(index.row());

    switch (index.column())
    {
    case SendTime:
        return entry.sendTime.toString(QStringLiteral("yyyy-MM-dd hh:mm:ss.zzz"));
    case ReceiveTime:
        return entry.receiveTime.toString(QStringLiteral("yyyy-MM-dd hh:mm:ss.zzz"));
    case ProgramName:
        return entry.programName;
    case NodeName:
        return entry.nodeName;
    case Prefix:
        return entry.prefix;
    case Message:
        return entry.message;
    default:
        return QVariant();
    }
}

void LogModel::addEntry(const LogEntry& entry)
{
    // If the circular buffer is already full the next push_back will
    // silently drop the first element.  Tell the view that this row is
    // going away and remove it explicitly so the model and view stay
    // in sync.
    if (m_entries.full())
    {
        beginRemoveRows(QModelIndex(), 0, 0);
        m_entries.pop_front();
        endRemoveRows();
    }

    const int newRow = static_cast<int>(m_entries.size());

    beginInsertRows(QModelIndex(), newRow, newRow);
    m_entries.push_back(entry);
    endInsertRows();
}

void LogModel::GenerateTestData()
{
    const int batchSize =
        static_cast<int>(m_entries.capacity() - m_entries.size());
    if (batchSize <= 0)
        return;

    std::vector<LogEntry> batch;
    batch.reserve(batchSize);

    static const QStringList programs = { "navigation", "sensors", "comms",
                                          "ai", "control", "diagnostics",
                                          "planning", "mapping", "ui" };
    static const QStringList nodes    = { "NodeA", "NodeB", "NodeC",
                                          "NodeD", "NodeE" };
    static const QStringList prefixes = { "stuff", "things", "items", "clutter",
                                          "miscellaneous"};
    static const QStringList messages = { "System boot completed successfully and all subsystems are online",
                                          "Periodic heartbeat from health-monitor received without anomalies",
                                          "Parameter value updated after calibration routine execution",
                                          "New trajectory computed by guidance module with 15 waypoints",
                                          "Critical fault detected in sensor fusion pipeline – entering safe mode",
                                          "Sensor calibration finished with status OK",
                                          "Battery level low warning triggered at 20 percent",
                                          "Route replanned due to detected obstacle on current path",
                                          "Network latency exceeded threshold – switching to backup channel",
                                          "Subsystem reboot initiated by watchdog after timeout",
                                          "Temperature reading exceeded operational limits – activating cooling",
                                          "Firmware upgrade completed and verified checksum",
                                          "Operator command received: switch to autonomous mode",
                                          "Data logging started for diagnostics session",
                                          "Error recovering from previous failure – escalation in progress" };

    auto* rng = QRandomGenerator::global();

    for (int i = 0; i < batchSize; ++i)
    {
        LogEntry e;
        const auto now = QDateTime::currentDateTimeUtc();
        e.receiveTime = now;
        e.sendTime    = now.addMSecs(-rng->bounded(0, 1000));
        e.programName = programs[rng->bounded(programs.size())];
        e.nodeName    = nodes[rng->bounded(nodes.size())];
        e.prefix      = prefixes[rng->bounded(prefixes.size())];
        e.message     = messages[rng->bounded(messages.size())];
        batch.push_back(std::move(e));
    }

    addEntries(std::move(batch));
}

bool LogModel::SaveToCsv(const QString& filePath) const
{
    QFile f(filePath);
    if (!f.open(QIODevice::WriteOnly | QIODevice::Text))
        return false;

    QTextStream out(&f);
    out << "\"sendTime\",\"recvTime\",\"program\",\"node\",\"prefix\",\"message\"\n";

    const auto escape = [](QString s)
    {
        s.replace(u'"', QStringLiteral("\"\""));
        return QLatin1Char('"') + s + QLatin1Char('"');
    };

    for (const LogEntry& e : m_entries)
    {
        out << escape(e.sendTime.toString(QStringLiteral("yyyy-MM-dd hh:mm:ss.zzz"))) << ','
            << escape(e.receiveTime.toString(QStringLiteral("yyyy-MM-dd hh:mm:ss.zzz"))) << ','
            << escape(e.programName) << ','
            << escape(e.nodeName) << ','
            << escape(e.prefix)   << ','
            << escape(e.message) << '\n';
    }
    return true;
}

void LogModel::clear()
{
    if (m_entries.empty())
        return;

    const int last = static_cast<int>(m_entries.size()) - 1;
    beginRemoveRows(QModelIndex(), 0, last);
    m_entries.clear();
    endRemoveRows();
}

std::size_t LogModel::bufferSize() const
{
    return m_entries.size();
}

std::size_t LogModel::bufferCapacity() const
{
    return m_entries.capacity();
}

void LogModel::SetHighlightRules(const std::vector<HighlightRule>& rules)
{
    m_highlightRules = rules;
    if (!m_entries.empty())
    {
        emit dataChanged(index(0,0),
                         index(static_cast<int>(m_entries.size())-1, ColumnCount-1),
                         {Qt::BackgroundRole, Qt::ForegroundRole});
    }
}

void LogModel::addEntries(std::vector<LogEntry>&& entries)
{
    if (entries.empty())
        return;

    // Calculate how many existing rows must be removed to make room.
    const int overflow =
        static_cast<int>(m_entries.size() + entries.size() - m_entries.capacity());

    if (overflow > 0)
    {
        beginRemoveRows(QModelIndex(), 0, overflow - 1);
        for (int i = 0; i < overflow; ++i)
            m_entries.pop_front();
        endRemoveRows();
    }

    const int first = static_cast<int>(m_entries.size());
    const int last  = first + static_cast<int>(entries.size()) - 1;

    beginInsertRows(QModelIndex(), first, last);
    for (LogEntry& e : entries)
        m_entries.push_back(std::move(e));
    endInsertRows();
}

