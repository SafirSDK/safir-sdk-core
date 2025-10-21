/******************************************************************************
*
* Copyright Saab AB, 2025 (http://safirsdkcore.com)
*
* Created by: Lars Hagström
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
#include "highlight_widget.h"
#include "ui_highlight_widget.h"
#include "edit_button_delegate.h"
#include "settings_manager.h"

#include <QTableWidget>
#include <QHeaderView>
#include <QPushButton>
#include <QColor>
#include <QColorDialog>
#include <QDebug>
#include <QRandomGenerator>
#include <QSet>

namespace {
static const QColor kCategoryPalette[] = {
    QColor("#E15759"), QColor("#EDC948"), QColor("#76B7B2"),
    QColor("#59A14F"), QColor("#F28E2B"), QColor("#B07AA1"),
    QColor("#FF9DA7"), QColor("#9C755F"), QColor("#BAB0AC")
};
static constexpr int kCategoryPaletteCount = sizeof(kCategoryPalette) / sizeof(kCategoryPalette[0]);
// Minimum squared Euclidean distance (RGB) to regard two colours as distinct
static constexpr int kMinDist2 = 100 * 100;
}

HighlightWidget::HighlightWidget(const std::shared_ptr<SettingsManager>& settingsManager, QWidget* parent)
    : QWidget(parent)
    , m_settingsManager(settingsManager)
{
    auto* ui = new Ui::HighlightWidget;
    ui->setupUi(this);

    m_table        = ui->tableWidget;
    m_addButton    = ui->addButton;
    m_removeButton = ui->removeButton;
    m_moveUpButton = ui->moveUpButton;
    m_moveDownButton = ui->moveDownButton;

    m_table->setColumnCount(2);
    m_table->setHorizontalHeaderLabels({tr("Pattern"),
                                         QString()});
    m_table->horizontalHeader()->setStretchLastSection(false);
    m_table->horizontalHeader()->setSectionResizeMode(0, QHeaderView::Stretch);
    m_table->horizontalHeader()->setSectionResizeMode(1, QHeaderView::Fixed);
    m_table->setColumnWidth(1, 32);        // keep colour column narrow
    // Left-align header labels
    m_table->horizontalHeader()->setDefaultAlignment(Qt::AlignLeft | Qt::AlignVCenter);
    // Hide the vertical header (row numbers)
    m_table->verticalHeader()->setVisible(false);
    // Allow selection of individual cells; we will disable selection on non-pattern cells
    m_table->setSelectionBehavior(QAbstractItemView::SelectItems);

    m_table->setWordWrap(false);
    m_table->setShowGrid(false);
    m_table->setHorizontalScrollMode(QAbstractItemView::ScrollPerPixel);
    m_table->setMouseTracking(true);   // needed so delegate receives hover state

    // Put an edit-button delegate in the Colour column (index 1)
    auto* editDelegate = new EditButtonDelegate(m_table);   // ownership by parent
    editDelegate->setSuppressHover(true);                   // no hover highlight
    m_table->setItemDelegateForColumn(1, editDelegate);

    connect(m_addButton,    &QPushButton::clicked,
            this,           &HighlightWidget::OnAddRow);
    connect(m_removeButton, &QPushButton::clicked,
            this,           &HighlightWidget::OnRemoveSelected);
    connect(m_moveUpButton, &QPushButton::clicked,
            this,           &HighlightWidget::OnMoveUp);
    connect(m_moveDownButton, &QPushButton::clicked,
            this,           &HighlightWidget::OnMoveDown);

    connect(m_table, &QTableWidget::cellChanged,
            this,     &HighlightWidget::OnCellChanged);
    connect(m_table, &QTableWidget::cellClicked,
            this,     &HighlightWidget::OnColorCellClicked);
    connect(m_table, &QTableWidget::itemSelectionChanged,
            this,     &HighlightWidget::OnSelectionChanged);

    // Initially, nothing is selected
    m_moveUpButton->setEnabled(false);
    m_moveDownButton->setEnabled(false);
    m_removeButton->setEnabled(false);

    // ------------------------------------------------------------------
    // Add two default rows: "error" and "warning"
    // ------------------------------------------------------------------
    OnAddRow();                                         // row 0
    if (auto* item0 = m_table->item(0, 0))
        item0->setText("error");

    OnAddRow();                                         // row 1
    if (auto* item1 = m_table->item(1, 0))
        item1->setText("warning");

    // ------------------------------------------------------------------
    // Replace defaults with any user-saved rules
    // ------------------------------------------------------------------
    const auto saved = m_settingsManager->loadHighlightRules();
    if (!saved.empty())
    {
        m_table->setRowCount(0);
        for (const auto& r : saved)
        {
            OnAddRow();
            const int row = m_table->rowCount() - 1;
            if (auto* patItem = m_table->item(row, 0))
                patItem->setText(r.regex.pattern());
            if (auto* colItem = m_table->item(row, 1))
                colItem->setBackground(r.color);
        }
    }

    // Persist whenever the user edits the table
    connect(this, &HighlightWidget::rulesChanged, this, [this] {
        m_settingsManager->saveHighlightRules(rules());
    });
}

// ------------------------------------------------------------------
void HighlightWidget::OnAddRow()
{
    const int newRow = m_table->rowCount();
    m_table->insertRow(newRow);

    // ------------------------------------------------------------------
    // Choose a colour:
    //  * First 10 rows use the well-known “Tableau 10” categorical palette
    //  * Subsequent rows get a random colour that is not “too close” to any
    //    colour already present in the table.
    // ------------------------------------------------------------------
    QColor colour;
    if (newRow < kCategoryPaletteCount)
    {
        colour = kCategoryPalette[newRow];
    }
    else
    {
        // Collect colours that are already in use
        std::vector<QColor> used;
        for (int r = 0; r < newRow; ++r)
            if (auto* item = m_table->item(r, 1))
                used.push_back(item->background().color());

        const auto tooClose = [&used](const QColor& c) {
            for (const QColor& u : used)
            {
                int dr = c.red()   - u.red();
                int dg = c.green() - u.green();
                int db = c.blue()  - u.blue();
                if (dr*dr + dg*dg + db*db < kMinDist2)
                    return true;        // not distinct enough
            }
            return false;
        };

        int guard = 0;
        do
        {
            colour = QColor::fromRgb(QRandomGenerator::global()->bounded(256),
                                     QRandomGenerator::global()->bounded(256),
                                     QRandomGenerator::global()->bounded(256));
        } while (tooClose(colour) && ++guard < 500);
    }

    // Create the colour cell
    auto* colourItem = new QTableWidgetItem();
    colourItem->setFlags(colourItem->flags() & ~Qt::ItemIsSelectable);   // colour column not selectable
    colourItem->setBackground(colour);
    m_table->setItem(newRow, 1, colourItem);
    m_table->setItem(newRow, 0, new QTableWidgetItem());

    EmitRulesChanged();
}

void HighlightWidget::OnRemoveSelected()
{
    const auto ranges = m_table->selectedRanges();
    for (const auto& range : ranges)
        for (int r = range.bottomRow(); r >= range.topRow(); --r)
            m_table->removeRow(r);
    EmitRulesChanged();
}

void HighlightWidget::OnCellChanged(int row, int column)
{
    if (column == 1)   // Colour column
        UpdateColorCell(row);

    EmitRulesChanged();
}

void HighlightWidget::EmitRulesChanged()
{
    emit rulesChanged();
}

void HighlightWidget::UpdateColorCell(int row)
{
    if (row < 0 || row >= m_table->rowCount())
        return;

    // Ensure the colour cell has an item
    QTableWidgetItem* item = m_table->item(row, 1);
    if (!item)
    {
        item = new QTableWidgetItem();
        item->setFlags(item->flags() & ~Qt::ItemIsSelectable);   // colour column not selectable
        m_table->setItem(row, 1, item);
    }

    // Use the existing background colour if valid, otherwise default to transparent white
    QColor colour = item->background().color().isValid()
                        ? item->background().color()
                        : QColor(255, 255, 255);

    item->setBackground(colour);
    //item->setText(QString());      // keep cell text empty – colour stored in background
}

void HighlightWidget::OnColorCellClicked(int row, int column)
{
    qDebug() << "[HighlightWidget] OnColorCellClicked row:" << row << "column:" << column;
    if (column != 1) {
        qDebug() << "[HighlightWidget] Clicked column is not the Colour column (index 1) – ignoring";
        return;
    }

    QTableWidgetItem* item = m_table->item(row, 1);
    qDebug() << "[HighlightWidget] Initial colour:" << item->background().color();

    const QColor chosen = QColorDialog::getColor(item->background().color(), this, tr("Select colour"));
    if (!chosen.isValid()) {
        qDebug() << "[HighlightWidget] Colour dialog cancelled or invalid colour selected";
        return;
    }
    qDebug() << "[HighlightWidget] User chose colour:" << chosen;

    if (!item)
    {
        item = new QTableWidgetItem();
        item->setFlags(item->flags() & ~Qt::ItemIsSelectable);   // colour column not selectable
        m_table->setItem(row, 1, item);
    }

    item->setBackground(chosen);
    item->setText(QString());      // do not store colour in text

    EmitRulesChanged();
}

// ------------------------------------------------------------------
std::vector<HighlightRule> HighlightWidget::rules() const
{
    std::vector<HighlightRule> list;
    for (int r = 0; r < m_table->rowCount(); ++r)
    {
        const QString rx  = m_table->item(r,0) ? m_table->item(r,0)->text() : QString();
        if (rx.isEmpty())
            continue;

        HighlightRule rule;
        rule.regex = QRegularExpression(rx, QRegularExpression::CaseInsensitiveOption);

        QColor bgColour = (m_table->item(r,1) && m_table->item(r,1)->background().color().isValid())
                              ? m_table->item(r,1)->background().color()
                              : QColor(255, 255, 255, 0);   // default transparent

        rule.color = bgColour;
        list.push_back(rule);
    }
    return list;
}

void HighlightWidget::OnMoveUp()
{
    const auto ranges = m_table->selectedRanges();
    if (ranges.isEmpty())
        return;

    int row = ranges.first().topRow();
    if (row <= 0)
        return;

    for (int col = 0; col < m_table->columnCount(); ++col)
    {
        QTableWidgetItem* upper   = m_table->takeItem(row - 1, col);
        QTableWidgetItem* current = m_table->takeItem(row,     col);
        m_table->setItem(row - 1, col, current);
        m_table->setItem(row,     col, upper);
    }

    m_table->selectRow(row - 1);
    EmitRulesChanged();
}

void HighlightWidget::OnMoveDown()
{
    const auto ranges = m_table->selectedRanges();
    if (ranges.isEmpty())
        return;

    int row = ranges.first().topRow();
    if (row >= m_table->rowCount() - 1)
        return;

    for (int col = 0; col < m_table->columnCount(); ++col)
    {
        QTableWidgetItem* current = m_table->takeItem(row,     col);
        QTableWidgetItem* lower   = m_table->takeItem(row + 1, col);
        m_table->setItem(row,     col, lower);
        m_table->setItem(row + 1, col, current);
    }

    m_table->selectRow(row + 1);
    EmitRulesChanged();
}

void HighlightWidget::OnSelectionChanged()
{
    const bool hasSelection = !m_table->selectedRanges().isEmpty();
    m_removeButton->setEnabled(hasSelection);

    if (!hasSelection)
    {
        m_moveUpButton->setEnabled(false);
        m_moveDownButton->setEnabled(false);
        return;
    }

    const int row = m_table->selectedRanges().first().topRow();
    m_moveUpButton->setEnabled(row > 0);
    m_moveDownButton->setEnabled(row < m_table->rowCount() - 1);
}
