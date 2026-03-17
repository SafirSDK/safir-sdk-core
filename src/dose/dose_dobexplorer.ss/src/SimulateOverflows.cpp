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

#include "SimulateOverflows.h"
#include <Safir/Utilities/Internal/Expansion.h>
#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Dob/Internal/Connection.h>

#include <QAbstractItemView>
#include <QBrush>
#include <QColor>
#include <QHeaderView>
#include <QIcon>
#include <QPainter>
#include <QPen>
#include <QPixmap>
#include <QRegularExpression>
#include <QSignalBlocker>
#include <QStyle>
#include <QApplication>
#include <QToolButton>
#include <QPushButton>
#include <QStyledItemDelegate>
#include <QLineEdit>

namespace
{
    QIcon CreateGreenPlusIcon(const QSize& size)
    {
        const QColor green(0, 160, 0);
        QPixmap pm(size);
        pm.fill(Qt::transparent);

        QPainter p(&pm);
        p.setRenderHint(QPainter::Antialiasing, true);

        QPen pen(green);
        // Keep it visible but not too thick; scale with size.
        const int w = std::max(2, size.width() / 8);
        pen.setWidth(w);
        pen.setCapStyle(Qt::RoundCap);
        p.setPen(pen);

        const int cx = size.width() / 2;
        const int cy = size.height() / 2;
        const int margin = std::max(3, size.width() / 5);

        p.drawLine(QPoint(cx, margin), QPoint(cx, size.height() - margin));
        p.drawLine(QPoint(margin, cy), QPoint(size.width() - margin, cy));

        p.end();

        return QIcon(pm);
    }

    QIcon CreateRedMinusIcon(const QSize& size)
    {
        const QColor red(200, 0, 0);
        QPixmap pm(size);
        pm.fill(Qt::transparent);

        QPainter p(&pm);
        p.setRenderHint(QPainter::Antialiasing, true);

        QPen pen(red);
        const int w = std::max(2, size.width() / 8);
        pen.setWidth(w);
        pen.setCapStyle(Qt::RoundCap);
        p.setPen(pen);

        const int cy = size.height() / 2;
        const int margin = std::max(3, size.width() / 5);

        p.drawLine(QPoint(margin, cy), QPoint(size.width() - margin, cy));

        p.end();

        return QIcon(pm);
    }

}

namespace
{
    // Blend base text toward background for a consistent “grayed out” placeholder, even on dark themes
    static inline QColor DerivedPlaceholderColor(const QStyleOptionViewItem& opt)
    {
        const bool selected = (opt.state & QStyle::State_Selected);
        const QPalette& pal = opt.palette;
        const QColor baseText = pal.color(selected ? QPalette::HighlightedText : QPalette::Text);
        const QColor bg = pal.brush(selected ? QPalette::Highlight : QPalette::Base).color();

        const qreal t = 0.4; // 40% text, 60% background
        const int r = int(baseText.red()   * t + bg.red()   * (1.0 - t));
        const int g = int(baseText.green() * t + bg.green() * (1.0 - t));
        const int b = int(baseText.blue()  * t + bg.blue()  * (1.0 - t));
        return QColor(r, g, b);
    }

    class PatternPlaceholderDelegate : public QStyledItemDelegate
    {
    public:
        explicit PatternPlaceholderDelegate(int targetColumn, const QString& placeholder, QObject* parent = nullptr)
            : QStyledItemDelegate(parent)
            , m_targetColumn(targetColumn)
            , m_placeholder(placeholder)
        {}

        QWidget* createEditor(QWidget* parent, const QStyleOptionViewItem& option, const QModelIndex& index) const override
        {
            QWidget* ed = QStyledItemDelegate::createEditor(parent, option, index);
            if (index.column() == m_targetColumn)
            {
                if (auto* le = qobject_cast<QLineEdit*>(ed))
                {
                    le->setPlaceholderText(m_placeholder);
                    // Make placeholder visibly subdued even on dark themes
                    QPalette pal = le->palette();
                    pal.setColor(QPalette::PlaceholderText, DerivedPlaceholderColor(option));
                    le->setPalette(pal);
                }
            }
            return ed;
        }

        void paint(QPainter* painter, const QStyleOptionViewItem& option, const QModelIndex& index) const override
        {
            QStyleOptionViewItem opt(option);
            initStyleOption(&opt, index);

            if (index.column() == m_targetColumn)
            {
                const QString text = index.data(Qt::DisplayRole).toString();
                if (text.trimmed().isEmpty())
                {
                    // Draw background and focus without any text
                    QStyleOptionViewItem bgOpt(opt);
                    bgOpt.text.clear();
                    QStyledItemDelegate::paint(painter, bgOpt, index);

                    // Then draw the placeholder text explicitly so styles cannot drop it
                    QStyle* style = opt.widget ? opt.widget->style() : QApplication::style();
                    const QRect textRect = style->subElementRect(QStyle::SE_ItemViewItemText, &opt, opt.widget);

                    // Use a derived placeholder color that’s dim relative to background
                    const QColor ph = DerivedPlaceholderColor(opt);

                    painter->save();
                    painter->setPen(ph);
                    // Use the same alignment as the style would
                    painter->drawText(textRect, opt.displayAlignment, m_placeholder);
                    painter->restore();
                    return;
                }
            }

            QStyledItemDelegate::paint(painter, opt, index);
        }

    private:
        int m_targetColumn;
        QString m_placeholder;
    };
}

SimulateOverflows::SimulateOverflows(QWidget* parent):
    QWidget(parent),
    m_dirty(false)
{
    setupUi(this); // this sets up GUI

    // Ensure expected columns (UI file also defines this, but keep it robust)
    rulesTable->setColumnCount(5);
    rulesTable->setHorizontalHeaderLabels(QStringList() << "Pattern" << "Out" << "In" << "Matches" << "");

    // Replace "+" text with an icon on the add button
    if (addRowButton != nullptr)
    {
        const QSize iconSize(16, 16);
        addRowButton->setText(QString());
        addRowButton->setIcon(CreateGreenPlusIcon(iconSize));
        addRowButton->setIconSize(iconSize);
        addRowButton->setToolButtonStyle(Qt::ToolButtonIconOnly);
    }

    // Table behavior
    rulesTable->setSelectionBehavior(QAbstractItemView::SelectRows);
    rulesTable->setSelectionMode(QAbstractItemView::SingleSelection);
    rulesTable->setAlternatingRowColors(true);
    rulesTable->setSortingEnabled(false);
    if (rulesTable->verticalHeader() != nullptr)
    {
        rulesTable->verticalHeader()->setVisible(false);
    }

    // Column sizes
    if (rulesTable->horizontalHeader() != nullptr)
    {
        rulesTable->horizontalHeader()->setStretchLastSection(false);
        rulesTable->horizontalHeader()->setSectionResizeMode(PatternColumn, QHeaderView::Stretch);
        rulesTable->horizontalHeader()->setSectionResizeMode(OutColumn, QHeaderView::ResizeToContents);
        rulesTable->horizontalHeader()->setSectionResizeMode(InColumn, QHeaderView::ResizeToContents);
        rulesTable->horizontalHeader()->setSectionResizeMode(MatchesColumn, QHeaderView::ResizeToContents);
        rulesTable->horizontalHeader()->setSectionResizeMode(DeleteColumn, QHeaderView::ResizeToContents);
    }

    // Install placeholder delegate for the Pattern column
    rulesTable->setItemDelegateForColumn(
        PatternColumn,
        new PatternPlaceholderDelegate(PatternColumn, tr("<Connection name pattern>"), rulesTable));

    // Hook up signals
    connect(addRowButton, &QToolButton::clicked, this, &SimulateOverflows::onAddRow);
    connect(rulesTable, &QTableWidget::itemChanged, this, &SimulateOverflows::onPatternItemChanged);
    if (unappliedLabel != nullptr)
    {
        unappliedLabel->setVisible(false);
    }
    if (applyButton != nullptr)
    {
        applyButton->setEnabled(false);
        connect(applyButton, &QPushButton::clicked, this, &SimulateOverflows::onApply);
    }
    if (clearButton != nullptr)
    {
        connect(clearButton, &QPushButton::clicked, this, &SimulateOverflows::onClear);
    }

    // Start with one empty row for convenience
    addRow(QString(), false, false);
}

void SimulateOverflows::onAddRow()
{
    addRow(QString(), false, false);

    const int row = rulesTable->rowCount() - 1;
    if (row >= 0)
    {
        rulesTable->setCurrentCell(row, PatternColumn);
        if (auto* item = rulesTable->item(row, PatternColumn))
        {
            rulesTable->editItem(item);
        }
    }
}

void SimulateOverflows::addRow(const QString& pattern, bool outChecked, bool inChecked)
{
    const int row = rulesTable->rowCount();
    rulesTable->insertRow(row);

    // Pattern cell (editable)
    auto* patternItem = new QTableWidgetItem(pattern);
    patternItem->setFlags(patternItem->flags() | Qt::ItemIsEditable);
    rulesTable->setItem(row, PatternColumn, patternItem);

    // Out checkbox
    auto* outItem = new QTableWidgetItem();
    outItem->setFlags((outItem->flags() | Qt::ItemIsUserCheckable | Qt::ItemIsEnabled | Qt::ItemIsSelectable) & ~Qt::ItemIsEditable);
    outItem->setCheckState(outChecked ? Qt::Checked : Qt::Unchecked);
    outItem->setText(QString());
    rulesTable->setItem(row, OutColumn, outItem);

    // In checkbox
    auto* inItem = new QTableWidgetItem();
    inItem->setFlags((inItem->flags() | Qt::ItemIsUserCheckable | Qt::ItemIsEnabled | Qt::ItemIsSelectable) & ~Qt::ItemIsEditable);
    inItem->setCheckState(inChecked ? Qt::Checked : Qt::Unchecked);
    inItem->setText(QString());
    rulesTable->setItem(row, InColumn, inItem);

    // Matches (read-only text)
    auto* matchesItem = new QTableWidgetItem();
    matchesItem->setFlags((matchesItem->flags() | Qt::ItemIsEnabled | Qt::ItemIsSelectable) & ~Qt::ItemIsEditable & ~Qt::ItemIsUserCheckable);
    matchesItem->setText(QString());
    rulesTable->setItem(row, MatchesColumn, matchesItem);

    // Delete (red minus) button
    auto* deleteButton = new QToolButton(rulesTable);
    deleteButton->setAutoRaise(true);
    deleteButton->setToolTip(QStringLiteral("Remove this rule"));

    const QSize iconSize(16, 16);
    deleteButton->setIcon(CreateRedMinusIcon(iconSize));
    deleteButton->setIconSize(iconSize);
    deleteButton->setToolButtonStyle(Qt::ToolButtonIconOnly);

    connect(deleteButton, &QToolButton::clicked, this, [this]() {
        // Identify the row by locating which cellWidget matches sender()
        QObject* s = sender();
        for (int r = 0; r < rulesTable->rowCount(); ++r)
        {
            if (rulesTable->cellWidget(r, DeleteColumn) == s)
            {
                removeRow(r);
                return;
            }
        }
    });

    rulesTable->setCellWidget(row, DeleteColumn, deleteButton);

    validatePatternCell(row);
}

void SimulateOverflows::removeRow(int row)
{
    if (row < 0 || row >= rulesTable->rowCount())
    {
        return;
    }

    // Avoid itemChanged recursion during removal
    QTableWidgetItem* patItem = rulesTable->item(row, PatternColumn);
    const bool hadNonEmptyPattern = (patItem != nullptr) && !patItem->text().trimmed().isEmpty();

    const QSignalBlocker blocker(rulesTable);
    rulesTable->removeRow(row);
    if (hadNonEmptyPattern)
    {
        setDirty(true);
    }
}

void SimulateOverflows::onPatternItemChanged(QTableWidgetItem* item)
{
    if (item == nullptr)
    {
        return;
    }

    const int col = item->column();
    const int row = item->row();
    if (col == PatternColumn)
    {
        validatePatternCell(row);
        QTableWidgetItem* patItem = rulesTable->item(row, PatternColumn);
        const bool hasPattern = (patItem != nullptr) && !patItem->text().trimmed().isEmpty();
        if (hasPattern)
        {
            setDirty(true);
        }
        return;
    }
    if (col == OutColumn || col == InColumn)
    {
        QTableWidgetItem* patItem = rulesTable->item(row, PatternColumn);
        const bool hasPattern = (patItem != nullptr) && !patItem->text().trimmed().isEmpty();
        if (hasPattern)
        {
            setDirty(true);
        }
        return;
    }
}

void SimulateOverflows::validatePatternCell(int row)
{
    if (row < 0 || row >= rulesTable->rowCount())
    {
        return;
    }

    auto* item = rulesTable->item(row, PatternColumn);
    if (item == nullptr)
    {
        return;
    }

    const QString pattern = item->text();

    // Empty allowed
    if (pattern.trimmed().isEmpty())
    {
        item->setBackground(QBrush());
        item->setToolTip(QStringLiteral("Case-insensitive regular expression."));
        updateApplyButtonState();
        return;
    }

    QRegularExpression re(pattern, QRegularExpression::CaseInsensitiveOption);
    if (!re.isValid())
    {
        item->setBackground(QBrush(QColor(255, 210, 210)));
        item->setToolTip(QStringLiteral("Invalid regular expression: %1").arg(re.errorString()));
    }
    else
    {
        item->setBackground(QBrush());
        item->setToolTip(QStringLiteral("Case-insensitive regular expression."));
    }
    updateApplyButtonState();
}

void SimulateOverflows::setDirty(bool dirty)
{
    if (m_dirty == dirty)
    {
        return;
    }
    m_dirty = dirty;
    if (unappliedLabel != nullptr)
    {
        unappliedLabel->setVisible(m_dirty);
    }
    updateApplyButtonState();
}

void SimulateOverflows::updateApplyButtonState()
{
    if (applyButton == nullptr)
    {
        return;
    }

    // Apply button is enabled if dirty and no invalid patterns exist
    bool hasInvalidPattern = false;
    for (int row = 0; row < rulesTable->rowCount(); ++row)
    {
        auto* patternItem = rulesTable->item(row, PatternColumn);
        if (patternItem == nullptr)
        {
            continue;
        }

        const QString pattern = patternItem->text().trimmed();
        if (pattern.isEmpty())
        {
            continue;
        }

        QRegularExpression re(pattern, QRegularExpression::CaseInsensitiveOption);
        if (!re.isValid())
        {
            hasInvalidPattern = true;
            break;
        }
    }

    applyButton->setEnabled(m_dirty && !hasInvalidPattern);
}

std::vector<SimulateOverflows::PatternInfo> SimulateOverflows::collectPatterns() const
{
    std::vector<PatternInfo> patterns;

    for (int row = 0; row < rulesTable->rowCount(); ++row)
    {
        auto* patternItem = rulesTable->item(row, PatternColumn);
        auto* outItem = rulesTable->item(row, OutColumn);
        auto* inItem = rulesTable->item(row, InColumn);

        if (patternItem == nullptr || outItem == nullptr || inItem == nullptr)
        {
            patterns.push_back({});
            continue;
        }

        PatternInfo info;
        const QString pattern = patternItem->text().trimmed();
        info.outChecked = (outItem->checkState() == Qt::Checked);
        info.inChecked = (inItem->checkState() == Qt::Checked);

        if (pattern.isEmpty())
        {
            info.status = PatternInfo::Empty;
        }
        else
        {
            info.regex = QRegularExpression(pattern, QRegularExpression::CaseInsensitiveOption);
            if (info.regex.isValid())
            {
                info.status = PatternInfo::Valid;
            }
            else
            {
                info.status = PatternInfo::Invalid;
            }
        }

        patterns.push_back(std::move(info));
    }

    return patterns;
}

void SimulateOverflows::updateMatchesColumn()
{
    using namespace Safir::Dob::Internal;

    // Collect all patterns from the table
    std::vector<PatternInfo> patterns = collectPatterns();

    // Single loop through connections - no UI updates in here to avoid holding locks
    Connections::Instance().ForEachConnectionPtr([&patterns](const ConnectionPtr& connection)
    {
        if (!connection)
        {
            return;
        }

        const QString connectionName = QString::fromUtf8(connection->NameWithCounter());

        for (auto& pattern : patterns)
        {
            if (pattern.status == PatternInfo::Valid && pattern.regex.match(connectionName).hasMatch())
            {
                ++pattern.matchCount;
            }
        }
    });

    // Now update UI with collected data
    for (int row = 0; row < rulesTable->rowCount() && row < static_cast<int>(patterns.size()); ++row)
    {
        auto* matchesItem = rulesTable->item(row, MatchesColumn);
        if (matchesItem == nullptr)
        {
            continue;
        }

        const auto& pattern = patterns[row];

        switch (pattern.status)
        {
        case PatternInfo::Valid:
            matchesItem->setText(QString::number(pattern.matchCount));
            matchesItem->setForeground(QBrush());
            break;

        case PatternInfo::Empty:
            matchesItem->setText(QString());
            matchesItem->setForeground(QBrush());
            break;

        case PatternInfo::Invalid:
            matchesItem->setText(QStringLiteral("(invalid pattern)"));
            matchesItem->setForeground(QBrush(QColor(200, 0, 0)));
            break;
        }
    }
}

void SimulateOverflows::onApply()
{
    using namespace Safir::Dob::Internal;

    // Disable Apply button during execution to prevent re-entrancy
    if (applyButton != nullptr)
    {
        applyButton->setEnabled(false);
    }

    // Collect all patterns from the table
    std::vector<PatternInfo> allPatterns = collectPatterns();

    // Build a list of only valid, non-empty patterns for simulation
    std::vector<PatternInfo> validPatterns;
    for (const auto& pattern : allPatterns)
    {
        if (pattern.status == PatternInfo::Valid)
        {
            validPatterns.push_back(pattern);
        }
    }

    // Single loop through connections - no UI updates in here to avoid holding locks
    // Note: "First pattern wins" - if a connection matches multiple patterns,
    // only the first matching pattern's in/out flags are applied.
    Connections::Instance().ForEachConnectionPtr([&validPatterns](const ConnectionPtr& connection)
    {
        if (!connection)
        {
            return;
        }

        const QString connectionName = QString::fromUtf8(connection->NameWithCounter());

        // Find first matching pattern and count all matches
        int firstMatchIndex = -1;

        for (size_t i = 0; i < validPatterns.size(); ++i)
        {
            if (validPatterns[i].regex.match(connectionName).hasMatch())
            {
                ++validPatterns[i].matchCount;

                // Remember the first pattern that matched (first-wins behavior)
                if (firstMatchIndex == -1)
                {
                    firstMatchIndex = static_cast<int>(i);
                }
            }
        }

        // Apply simulation based on first matching pattern only
        if (firstMatchIndex >= 0)
        {
            const auto& firstMatch = validPatterns[firstMatchIndex];
            connection->SimulateOverflows(firstMatch.inChecked, firstMatch.outChecked);
        }
        else
        {
            // No patterns match, clear any existing simulation
            connection->SimulateOverflows(false, false);
        }
    });

    // Update the UI with match counts (after releasing connection locks)
    int validPatternIndex = 0;
    for (int row = 0; row < rulesTable->rowCount() && row < static_cast<int>(allPatterns.size()); ++row)
    {
        auto* matchesItem = rulesTable->item(row, MatchesColumn);
        if (matchesItem == nullptr)
        {
            continue;
        }

        const auto& pattern = allPatterns[row];

        switch (pattern.status)
        {
        case PatternInfo::Valid:
            if (validPatternIndex < static_cast<int>(validPatterns.size()))
            {
                matchesItem->setText(QString::number(validPatterns[validPatternIndex].matchCount));
                matchesItem->setForeground(QBrush());
                ++validPatternIndex;
            }
            break;

        case PatternInfo::Empty:
            matchesItem->setText(QString());
            matchesItem->setForeground(QBrush());
            break;

        case PatternInfo::Invalid:
            matchesItem->setText(QStringLiteral("(invalid pattern)"));
            matchesItem->setForeground(QBrush(QColor(200, 0, 0)));
            break;
        }
    }

    setDirty(false);

    // Re-enable Apply button after execution completes
    updateApplyButtonState();
}

void SimulateOverflows::onClear()
{
    using namespace Safir::Dob::Internal;

    // Clear overflow simulation on all connections
    Connections::Instance().ForEachConnectionPtr([](const ConnectionPtr& conn)
    {
        if (conn)
        {
            conn->SimulateOverflows(false, false);
        }
    });

    // Clear all rows from the table
    const QSignalBlocker blocker(rulesTable);
    rulesTable->setRowCount(0);

    // Add one empty row for convenience
    addRow(QString(), false, false);

    // Clear dirty state
    setDirty(false);
}
