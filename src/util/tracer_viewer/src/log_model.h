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

#include <QAbstractTableModel>
#include <boost/circular_buffer.hpp>
#include <boost/circular_buffer/space_optimized.hpp>
#include <QString>
#include <QDateTime>
#include <memory>
#include <vector>
#include "log_entry.h"
#include "highlight_rule.h"
class TracerDataReceiver;

class LogModel
    : public QAbstractTableModel
{
    Q_OBJECT

public:
    /** Listen to the dataReceiver and fill the model from it. */
    explicit LogModel(const std::shared_ptr<TracerDataReceiver>& dataReceiver, QObject* parent);

    /** Construct model by reading all entries from a CSV file. No live updates. */
    explicit LogModel(const QString& csvFilePath, QObject* parent = nullptr);

    ~LogModel() override;

    enum ModelRoles {HideColumnByDefaultRole = Qt::UserRole};

    // Column identifiers
    enum Column
    {
        SendTime = 0,
        ReceiveTime,
        ProgramName,
        NodeName,
        Prefix,
        Message,
        ColumnCount
    };

    // QAbstractTableModel interface
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    int columnCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const override;
    QVariant headerData(int section,
                        Qt::Orientation orientation,
                        int role = Qt::DisplayRole) const override;

    Qt::ItemFlags flags(const QModelIndex& index) const override;

    // Convenience function to add a single log entry
    void addEntry(const LogEntry& entry);

    // Efficient bulk-insert of many entries at once
    void addEntries(std::vector<LogEntry>&& entries);

    // ------------------------------------------------------------------
    //  Test-data helper
    // ------------------------------------------------------------------
    /** Fill the circular buffer with randomly generated log entries. */
    void GenerateTestData();

    // ------------------------------------------------------------------
    //  Circular-buffer statistics
    // ------------------------------------------------------------------
    std::size_t bufferSize()     const;
    std::size_t bufferCapacity() const;
    /** Remove all entries from the circular buffer. */
    void clear();

    /** Export *all* entries currently stored in m_entries to a CSV file
     *  compatible with tracer_listener --csv.
     *  Returns true on success, false on failure (e.g. I/O error). */
    bool SaveToCsv(const QString& filePath) const;
    void SetHighlightRules(const std::vector<HighlightRule>& rules);

private:
    const std::shared_ptr<TracerDataReceiver> m_dataReceiver;
    boost::circular_buffer_space_optimized<LogEntry> m_entries;
    std::vector<HighlightRule> m_highlightRules;
};

