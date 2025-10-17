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
#include "log_widget.h"
#include "log_model.h"
#include "settings_manager.h"
#include "highlight_rule.h"
#include <QTableView>
#include <QScrollArea>
#include <QScrollBar>
#include <QVBoxLayout>
#include <QHeaderView>
#include <QSortFilterProxyModel>
#include <QLineEdit>
#include <QMenu>
#include <QTimer>
#include <QFontDatabase>
#include <QClipboard>
#include <QGuiApplication>
#include <QStringList>
#include <set>
#include <QShortcut>
#include <QRegularExpression>
#include <QItemSelection>
#include "selectable_text_delegate.h"
#include <algorithm>
#include <QDebug>

namespace
{
    //Load our preferred log widget font
    inline QFont fixedFont()
    {
        const int id = QFontDatabase::addApplicationFont(":/fonts/JetBrainsMono-Light.ttf");
        const QString family = (id >= 0)
                             ? QFontDatabase::applicationFontFamilies(id).first()
                             : QString();

        QFont f = family.isEmpty()
                ? QFontDatabase::systemFont(QFontDatabase::FixedFont)
                : QFont(family);
        f.setStyleHint(QFont::Monospace);
        f.setWeight(QFont::Light);

        // make the font smaller for a more compact view
        f.setPointSize(8);

        qDebug() << "fixedFont():"
                 << "fontResourceId =" << id
                 << "loadedFamily ="   << family
                 << "usingFallback ="  << family.isEmpty()
                 << "finalFamily ="    << f.family()
                 << "pointSize ="      << f.pointSize()
                 << "weight ="         << f.weight();
        return f;
    }
}

class ColumnSortFilterProxyModel
    : public QSortFilterProxyModel
{
public:
    ColumnSortFilterProxyModel(QWidget* parent)
        : QSortFilterProxyModel(parent)
    {
    }

    void clearFilterRegularExpression(const int column)
    {
        if (auto* lm = qobject_cast<LogModel*>(sourceModel()))
            lm->clearFilter(column);
        invalidateFilter();
    }

    void setFilterRegularExpression(const int column, const QRegularExpression& regex)
    {
        if (auto* lm = qobject_cast<LogModel*>(sourceModel()))
            lm->setFilter(column, regex);
        invalidateFilter();
    }

protected:
    bool filterAcceptsRow(int, const QModelIndex&) const override
    {
        /* filtering handled by LogModel – always accept here */
        return true;
    }


private:
};


LogWidget::LogWidget(const std::shared_ptr<SettingsManager>& settingsManager, LogModel* model, QWidget* parent)
    : QWidget(parent)
    , m_table(new QTableView(this))
    , m_filterArea(new QFrame())
    , m_filterAreaLayout(new QHBoxLayout(m_filterArea))
    , m_filterScroller(new QScrollArea(this))
    , m_fixedFont(fixedFont())
    , m_model(model)
    , m_settingsManager(settingsManager)
    , m_filterDebounceTimer(new QTimer(this))
    , m_followMode(true)
{

    auto* layout = new QVBoxLayout(this);
    layout->addWidget(m_table,10000);

    m_filterArea->setLayout(m_filterAreaLayout);
    m_filterAreaLayout->setSpacing(2);
    m_filterAreaLayout->setContentsMargins(0,0,0,0);
    m_filterArea->setSizePolicy(QSizePolicy::Fixed,QSizePolicy::Fixed);
    m_filterScroller->setWidget(m_filterArea);
    layout->addWidget(m_filterScroller,1);
    m_filterScroller->show();
    m_filterArea->show();
    m_filterScroller->show();
    m_filterScroller->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
    m_filterScroller->setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
    m_filterScroller->horizontalScrollBar()->setEnabled(false);
    m_filterScroller->verticalScrollBar()->setEnabled(false);
    m_filterScroller->setStyleSheet("QScrollArea {border:none;padding:0px;}");

    //connect the header count, sizes and positions
    connect(m_table->horizontalHeader(),&QHeaderView::sectionResized, this, &LogWidget::OnSectionResized);
    connect(m_table->horizontalHeader(),&QHeaderView::sectionCountChanged, this, &LogWidget::OnSectionCountChanged);
    connect(m_table->horizontalHeader(),&QHeaderView::geometriesChanged, this, &LogWidget::PositionFilters);
    connect(m_table->horizontalScrollBar(), &QAbstractSlider::rangeChanged, this,  &LogWidget::PositionFilters);
    connect(m_table->horizontalScrollBar(), &QAbstractSlider::actionTriggered, this, &LogWidget::PositionFilters);

    m_table->setWordWrap(false);
    m_table->setShowGrid(false);                      // remove cell grid lines
    m_table->setSortingEnabled(false);        // disable column sorting
    m_table->setHorizontalScrollMode(QAbstractItemView::ScrollPerPixel);
    m_table->setFont(m_fixedFont);

    // ------------------------------------------------------------------
    // Compute tight single-line row height for the current font.
    // ------------------------------------------------------------------
    {
        const QFontMetrics fm(m_table->font());
        const int rowH     = fm.lineSpacing() + 1; // ascent + descent + leading + a little more

        qDebug() << "LogWidget:" << "row height =" << rowH;

        auto* vh = m_table->verticalHeader();
        vh->setSectionResizeMode(QHeaderView::Fixed);
        vh->setMinimumSectionSize(rowH);
        vh->setMaximumSectionSize(rowH);
        vh->setDefaultSectionSize(rowH);
    }

    // allow in-cell substring selection with a read-only delegate
    m_table->setItemDelegate(new SelectableTextDelegate(m_table));
    // Single-click continues to select entire rows; the read-only editor that
    // allows substring selection is now opened only on double-click (or F2).
    m_table->setEditTriggers(QAbstractItemView::DoubleClicked |
                             QAbstractItemView::EditKeyPressed);

    // global debounce timer setup
    m_filterDebounceTimer->setSingleShot(true);
    m_filterDebounceTimer->setInterval(250);
    connect(m_filterDebounceTimer, &QTimer::timeout, this,
            [this]
            {
                for (int col : m_pendingFilterColumns)
                {
                    const auto* widget = qobject_cast<QLineEdit*>(m_filters[col]);
                    const QString text = widget ? widget->text() : QString();
                    OnFilterTextChanged(col, text);
                }
                m_pendingFilterColumns.clear();
            });
    /* Handle modelReset emitted after bulk updates when filters are active */
    connect(m_model, &QAbstractItemModel::modelReset, this,
            [this]()
            {
                if (m_followMode)
                    m_table->scrollToBottom();
            });
    // Disable follow-mode when the user manually moves the vertical scroll-bar
    connect(m_table->verticalScrollBar(), &QAbstractSlider::actionTriggered, this,
            [this](int){ SetFollowModeEnabled(false); });
    m_table->setSelectionBehavior(QTableView::SelectRows);
    m_table->horizontalHeader()->setHighlightSections(false);
    m_table->horizontalHeader()->setStretchLastSection(true);
    m_table->verticalHeader()->setVisible(false);

    m_table->horizontalHeader()->setContextMenuPolicy(Qt::CustomContextMenu);
    m_table->setContextMenuPolicy(Qt::CustomContextMenu);

    //connect(m_table, &QTableView::clicked, this, &LogWidget::OnClicked);
    //connect(m_table, &QTableView::doubleClicked, this, &LogWidget::OnDoubleClicked);
    connect(m_table->horizontalHeader(), &QWidget::customContextMenuRequested, this, &LogWidget::OnCustomContextMenuRequestedHeader);
    connect(m_table, &QWidget::customContextMenuRequested, this, &LogWidget::OnCustomContextMenuRequestedTable);

    QMetaObject::invokeMethod(this,[this]
        {
            for (int i = 0; i < m_table->horizontalHeader()->count(); ++i)
            {
                auto data = m_model->headerData(i,Qt::Horizontal, LogModel::HideColumnByDefaultRole);
                if (data.isValid() && data.toBool())
                {
                    m_table->hideColumn(i);
                }
            }
        },
        Qt::QueuedConnection);

    m_table->setModel(m_model);

    // restore any saved column sizes / visibility
    LoadColumnState();
    // save once when the widget is being destroyed
    /* removed unsafe destroyed()-connection */

    // ------------------------------------------------------------------
    //  Any user selection (mouse, keyboard, etc.) disables follow-mode
    // ------------------------------------------------------------------
    connect(m_table->selectionModel(), &QItemSelectionModel::selectionChanged,
            this,
            [this](const QItemSelection&, const QItemSelection&)
            {
                if (m_ignoreSelectionChange)
                    return;                     // internal change – ignore
                SetFollowModeEnabled(false);    // user interaction → stop follow
            });


    // ------------------------------------------------------------------
    //  Set sensible default column widths
    // ------------------------------------------------------------------
    {
        const QFontMetrics fm = m_table->fontMetrics();

        // "yyyy-MM-dd hh:mm:ss.zzz" → 23 characters (+ padding)
        const int tsWidth = fm.horizontalAdvance(
                                QStringLiteral("2025-12-31 23:59:59.999")) + 12;
        m_table->setColumnWidth(LogModel::SendTime,    tsWidth);
        m_table->setColumnWidth(LogModel::ReceiveTime, tsWidth);

        // Program name ≈ 12 characters
        const int progWidth = fm.horizontalAdvance(QString(12, QLatin1Char('W'))) + 12;
        m_table->setColumnWidth(LogModel::ProgramName, progWidth);

        // Node name ≈ 12 characters
        const int nodeWidth = fm.horizontalAdvance(QString(12, QLatin1Char('W'))) + 12;
        m_table->setColumnWidth(LogModel::NodeName,    nodeWidth);

        // Prefix ≈ 16 characters
        const int prefixWidth = fm.horizontalAdvance(QString(16, QLatin1Char('W'))) + 12;
        m_table->setColumnWidth(LogModel::Prefix, prefixWidth);
    }

    // Keep view at bottom when new rows are appended while follow mode is on.
    connect(m_model, &QAbstractItemModel::rowsInserted, this,
            [this](const QModelIndex&, int, int)
            {
                if (m_followMode)
                {
                    m_table->scrollToBottom();
                    return;
                }
            });

    // ------------------------------------------------------------------
    //  Keep current top row visible when follow-mode is OFF and old rows
    //  are evicted from the circular buffer.
    // ------------------------------------------------------------------
    connect(m_model, &QAbstractItemModel::rowsAboutToBeRemoved, this,
            [this](const QModelIndex&, int first, int last)
            {
                Q_UNUSED(first);
                if (!m_followMode)
                {
                    m_topRowBeforeRemoval  = m_table->rowAt(0);          // row currently at top
                    m_rowsAboutToBeRemoved = last - first + 1;           // how many will vanish
                }
            });

    connect(m_model, &QAbstractItemModel::rowsRemoved, this,
            [this](const QModelIndex&, int, int)
            {
                if (!m_followMode && m_topRowBeforeRemoval >= 0)
                {
                    const int newTop = std::max(0,
                        m_topRowBeforeRemoval - m_rowsAboutToBeRemoved);
                    m_table->scrollTo(m_model->index(newTop, 0),
                                      QAbstractItemView::PositionAtTop);
                    m_topRowBeforeRemoval  = -1;
                    m_rowsAboutToBeRemoved = 0;
                }
            });

    // ------------------------------------------------------------------
    //  Global shortcut: Ctrl+C copies current selection (rows or in-cell)
    // ------------------------------------------------------------------
    auto* copyShortcut = new QShortcut(QKeySequence::Copy, this);
    connect(copyShortcut, &QShortcut::activated,
            this, &LogWidget::CopySelectionToClipboard);
}

LogWidget::LogWidget(const std::shared_ptr<SettingsManager>& settingsManager,
                     const std::shared_ptr<TracerDataReceiver>& dataReceiver,
                     QWidget* parent)
    : LogWidget(settingsManager, new LogModel(dataReceiver, nullptr), parent)
{
    m_model->setParent(this);
}

LogWidget::LogWidget(const QString& file, QWidget* parent)
    : LogWidget(nullptr, new LogModel(file, nullptr), parent)
{
    m_model->setParent(this);
}



void LogWidget::OnCustomContextMenuRequestedTable(const QPoint& pos)
{
    QMenu menu(this);
    auto* copyAction = new QAction(tr("Copy"), &menu);
    copyAction->setShortcut(QKeySequence::Copy);
    copyAction->setShortcutVisibleInContextMenu(true);
    menu.addAction(copyAction);

    const QAction* chosen = menu.exec(m_table->viewport()->mapToGlobal(pos));
    if (chosen == copyAction)
    {
        CopySelectionToClipboard();
    }
}

void LogWidget::CopySelectionToClipboard()
{
    const auto rows = m_table->selectionModel()->selectedRows();
    if (rows.isEmpty())
        return;

    QStringList lines;
    lines.reserve(rows.count());

    for (const QModelIndex& row : rows)
    {
        QStringList cols;
        cols.reserve(LogModel::ColumnCount);
        for (int c = 0; c < LogModel::ColumnCount; ++c)
        {
            cols << m_model->data(m_model->index(row.row(), c)).toString();
        }
        lines << cols.join(u'\t');
    }

    QGuiApplication::clipboard()->setText(lines.join(u'\n'));
}

LogWidget::~LogWidget()
{
    SaveColumnState();          // safe: runs before sub-objects are destroyed
}

void LogWidget::OnFilterTextChanged(const int column, const QString &text)
{
    const QRegularExpression regex(text,QRegularExpression::CaseInsensitiveOption);

    if (text.isEmpty())
    {
        m_model->clearFilter(column);
    }
    else if (!regex.isValid())
    {
        m_filters[column]->setStyleSheet("background:red;");
        m_filters[column]->setToolTip(regex.errorString());
        return;
    }
    else
    {
        m_model->setFilter(column, regex);
    }

    m_filters[column]->setStyleSheet("");
    m_filters[column]->setToolTip("");

}



void LogWidget::OnSectionResized(const int logicalIndex, const int /*oldSize*/, const int newSize)
{
    m_filters[logicalIndex]->setHidden(newSize == 0);
    if (newSize != 0)
    {
        m_filters[logicalIndex]->setFixedWidth(newSize-2);
    }

    PositionFilters();
    SaveColumnState();
}

void LogWidget::OnSectionCountChanged(const int /*oldCount*/, const int newCount)
{
    while (auto* item = m_filterAreaLayout->takeAt(0))
    {
        if (auto* widget = item->widget())
        {
            widget->deleteLater();
        }
        delete item;
    }
    m_filters.clear();

    m_filterAreaLayout->addSpacing(2);
    auto placeholder = QString("%1 Filter").arg(QString::fromUtf8("\xF0\x9F\x94\x8D")); // utf-8 Left-Pointing Magnifying Glass
    for (int i = 0; i < newCount; ++i)
    {
        auto* le = new QLineEdit(this);
        m_filters.push_back(le);

        le->setPlaceholderText(placeholder);
        le->setClearButtonEnabled(true);
        le->setFont(m_fixedFont);
        le->setFixedWidth(m_table->columnWidth(i)-2);
        le->setToolTip(tr("Accepts regular expression."));
        connect(le, &QLineEdit::textChanged, this,
                [this, i]
                {
                    m_pendingFilterColumns.insert(i);
                    m_filterDebounceTimer->start();
                });
        m_filterAreaLayout->addWidget(le,1);
    }

    m_filters.push_back(new QWidget(this));
    m_filters.back()->setFixedHeight(10);

    m_filterAreaLayout->addWidget(m_filters.back(),1);

    PositionFilters();
}


void LogWidget::OnCustomContextMenuRequestedHeader(const QPoint& pos)
{
    const auto logicalIndex = m_table->horizontalHeader()->logicalIndexAt(pos);
    const auto globalPos = m_table->horizontalHeader()->mapToGlobal(pos);
    RunColumnContextMenu(globalPos, logicalIndex);
}

void LogWidget::RunColumnContextMenu(const QPoint& globalPos, const int logicalIndex)
{
    QMenu menu(this);
    auto* resizeColumnAction = new QAction(tr("Resize this column to contents"));
    auto* resizeAllColumnAction = new QAction(tr("Resize all columns to contents"));
    auto* hideAction = new QAction(tr("Hide this column"));
    auto* showAllAction = new QAction(tr("Show all columns"));
    auto* hideAllAction = new QAction(tr("Hide all columns"));
    if (logicalIndex != -1)
    {
        menu.addAction(resizeColumnAction);
        menu.addAction(resizeAllColumnAction);
        menu.addSeparator();
        menu.addAction(hideAction);
        menu.addSeparator();
    }

    menu.addAction(showAllAction);
    menu.addAction(hideAllAction);
    menu.addSeparator();
    for (int i = 0; i < m_table->horizontalHeader()->count(); ++i)
    {
        auto* action = new QAction(m_table->model()->headerData(i, Qt::Horizontal).toString());
        action->setProperty("columnNumber", i);
        action->setCheckable(true);
        action->setChecked(!m_table->isColumnHidden(i));
        menu.addAction(action);
    }

    const auto* const chosenAction = menu.exec(globalPos);

    if (chosenAction == nullptr)
    {
        return;
    }
    else if (chosenAction == resizeColumnAction)
    {
        m_table->resizeColumnToContents(logicalIndex);
    }
    else if (chosenAction == resizeAllColumnAction)
    {
        m_table->resizeColumnsToContents();
    }
    else if (chosenAction == hideAction)
    {
        m_table->hideColumn(logicalIndex);
    }
    else if (chosenAction == showAllAction)
    {
        for (int i = 0; i < m_table->horizontalHeader()->count(); ++i)
        {
            m_table->showColumn(i);
        }
    }
    else if (chosenAction == hideAllAction)
    {
        for (int i = 0; i < m_table->horizontalHeader()->count(); ++i)
        {
            m_table->hideColumn(i);
        }
    }
    else
    {
        auto column = chosenAction->property("columnNumber").toInt();
        m_table->setColumnHidden(column, !chosenAction->isChecked());
    }
    SaveColumnState();
}

void LogWidget::PositionFilters()
{
    QMetaObject::invokeMethod(this,[this]
        {
            m_filters.back()->setFixedWidth(std::max(0,m_table->contentsRect().width() -
                                                     m_table->columnViewportPosition(m_table->horizontalHeader()->count() -1) -
                                                     m_table->columnWidth(m_table->horizontalHeader()->count() -1)));
            m_filterArea->setFixedSize(m_filterAreaLayout->minimumSize());
            m_filterScroller->setFixedHeight(m_filterArea->height());
            m_filterScroller->horizontalScrollBar()->setSliderPosition(m_table->horizontalScrollBar()->sliderPosition());
        },
        Qt::QueuedConnection);
}

std::size_t LogWidget::bufferSize() const
{
    return m_model ? m_model->bufferSize() : 0;
}

std::size_t LogWidget::bufferCapacity() const
{
    return m_model ? m_model->bufferCapacity() : 0;
}

void LogWidget::ClearBuffer()
{
    if (m_model)
        m_model->clear();
}

void LogWidget::SetFollowModeEnabled(bool enabled)
{
    if (m_followMode == enabled)
        return;                     // no change – no signal

    m_followMode = enabled;
    emit FollowModeChanged(enabled);

    if (enabled)
    {
        // Ensure current latest row is visible when enabling follow
        m_table->scrollToBottom();

        // Clear any current selection so the view does not jump when new
        // rows arrive while follow-mode is active.  Guard flag prevents the
        // resulting selectionChanged signal from disabling follow-mode again.
        m_ignoreSelectionChange = true;
        m_table->clearSelection();
        m_ignoreSelectionChange = false;
    }
}

void LogWidget::JumpToLast()
{
    if (m_table)
        m_table->scrollToBottom();
}

bool LogWidget::SaveToCsv(const QString& filePath) const
{
    return m_model ? m_model->SaveToCsv(filePath) : false;
}

void LogWidget::SetHighlightRules(const std::vector<HighlightRule>& rules)
{
    if (m_model)
        m_model->SetHighlightRules(rules);
}

void LogWidget::GenerateTestData()
{
    if (m_model)
        m_model->GenerateTestData();
}

/* --------------------------------------------------------------------
 *  Persist and restore column sizes + visibility
 * ------------------------------------------------------------------*/
void LogWidget::SaveColumnState() const
{
    if (!m_settingsManager)
        return;

    QList<int>   sizes;
    QList<bool>  visible;
    const int cols = m_table->horizontalHeader()->count();
    sizes.reserve(cols);
    visible.reserve(cols);

    for (int c = 0; c < cols; ++c)
    {
        sizes   << m_table->columnWidth(c);
        visible << !m_table->isColumnHidden(c);
    }
    m_settingsManager->saveColumnState(QStringLiteral("LogWidget"), sizes, visible);
}

void LogWidget::LoadColumnState()
{
    if (!m_settingsManager)
        return;

    QList<int>   sizes;
    QList<bool>  visible;
    if (!m_settingsManager->loadColumnState(QStringLiteral("LogWidget"), sizes, visible))
        return;

    const int cols = m_table->horizontalHeader()->count();
    const int n    = std::min(cols, static_cast<int>(sizes.size()));

    for (int c = 0; c < n; ++c)
    {
        m_table->setColumnWidth(c, sizes[c]);
        m_table->setColumnHidden(c, !visible[c]);
    }
}
