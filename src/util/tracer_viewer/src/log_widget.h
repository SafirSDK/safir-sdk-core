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

#include <QWidget>
#include <QTimer>
#include <set>
#include <cstddef>
#include "highlight_rule.h"

class QTableView;
class LogModel;
class QHBoxLayout;
class QLineEdit;
class QScrollArea;
class QTableView;
class TracerDataReceiver;
class SettingsManager;

class LogWidget
    : public QWidget
{
    Q_OBJECT

public:

    LogWidget(const std::shared_ptr<SettingsManager>& settingsManager, const std::shared_ptr<TracerDataReceiver>& dataReceiver, QWidget* parent);
    LogWidget(const QString& file, QWidget* parent);
    ~LogWidget();

    void OnFilterTextChanged(const int column, const QString &text);
    void OnSectionResized(const int logicalIndex, const int oldSize, const int newSize);
    void OnSectionCountChanged(const int oldCount, const int newCount);
    void PositionFilters();
    void OnCustomContextMenuRequestedHeader(const QPoint& pos);
    void RunColumnContextMenu(const QPoint& globalPos, const int logicalIndex);

    // table-cell context menu helpers
    void OnCustomContextMenuRequestedTable(const QPoint& pos);
    void CopySelectionToClipboard();

    // ------------------------------------------------------------------
    //  Circular-buffer statistics helpers
    // ------------------------------------------------------------------
    std::size_t bufferSize()     const;
    std::size_t bufferCapacity() const;

    /** Clear all rows from the log view. */
    void ClearBuffer();
    void SetHighlightRules(const std::vector<HighlightRule>& rules);

public slots:
    /**
     * Enable or disable follow mode.
     *
     * When follow mode is enabled the view will automatically scroll
     * to the newest log entry when new rows are appended, and any
     * existing row selection will be cleared.
     */
    void SetFollowModeEnabled(bool enabled = true);
    /** Scroll view to the newest entry without toggling follow mode. */
    void JumpToLast();
    /** Fill the log's buffer with random test data. */
    void GenerateTestData();

    /** Export the current log contents to a CSV file compatible with
     *  tracer_listener --csv.
     *
     *  Returns true on success, false on failure (e.g. I/O error). */
    bool SaveToCsv(const QString& filePath) const;

signals:
    /** Emitted whenever the follow-mode flag changes. */
    void FollowModeChanged(bool enabled);

protected:
    /** Re-compute row height when the widget’s style or font changes. */
    void changeEvent(QEvent* e) override;

private:
    LogWidget(const std::shared_ptr<SettingsManager>& settingsManager, LogModel* model, QWidget* parent);

    QTableView* m_table;
    QWidget* m_filterArea;
    QHBoxLayout* m_filterAreaLayout;
    QScrollArea* m_filterScroller;
    QList<QWidget*> m_filters;
    LogModel* const m_model;
    std::shared_ptr<SettingsManager> m_settingsManager;

    void  SaveColumnState() const;
    void  LoadColumnState();
    void  RecomputeRowHeight();

    // Debounce helpers
    QTimer*               m_filterDebounceTimer = nullptr;
    std::set<int>         m_pendingFilterColumns;

    // Follow-mode flag: when true, the view keeps the latest row visible.
    bool                  m_followMode = true;
    // Guard flag: when true the selectionChanged-handler will ignore the
    // signal.  Used while we programmatically clear the selection when
    // enabling follow-mode so that follow-mode is not disabled again.
    bool                  m_ignoreSelectionChange = false;

    // View-stability helpers: remember which row is currently at the top
    // of the viewport and how many rows will be removed so we can keep
    // the same logical row visible when the buffer drops old entries.
    int                   m_topRowBeforeRemoval = -1;
    int                   m_rowsAboutToBeRemoved = 0;
};
