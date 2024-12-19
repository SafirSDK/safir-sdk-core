/******************************************************************************
*
* Copyright Saab AB, 2014, 2022, 2024 (http://safirsdkcore.com)
*
* Created by: Patrik Fundberg / patrik.fundberg@saabgroup.com
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
#include "instanceswidget.h"

#include <QAction>
#include <QCheckBox>
#include <QHBoxLayout>
#include <QHeaderView>
#include <QLineEdit>
#include <QMenu>
#include <QScrollArea>
#include <QScrollBar>
#include <QSortFilterProxyModel>
#include <QTableView>
#include <QVBoxLayout>
#include <QWidgetAction>
#include "entityinstancesmodel.h"
#include "messageinstancesmodel.h"
#include "typesystemrepository.h"
#include <map>

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
        m_filters.erase(column);
        invalidateFilter();
    }

    void setFilterRegularExpression(const int column, const QRegularExpression& regex)
    {
        m_filters[column] = regex;
        invalidateFilter();
    }

protected:
    bool filterAcceptsRow(int source_row, const QModelIndex&) const override
    {
        for (const auto& filter: m_filters)
        {
            const auto column = filter.first;
            const auto& regex = filter.second;
            const auto& data = sourceModel()->data(sourceModel()->index(source_row, column), filterRole()).toString();
            if (!regex.match(data).hasMatch())
            {
                return false;
            }
        }
        return true;
    }

private:
    std::map<int, QRegularExpression> m_filters;
};

InstancesWidget::InstancesWidget(QWidget* parent)
    : QWidget(parent)
    , m_table(new QTableView(this))
    , m_filterArea(new QFrame())
    , m_filterAreaLayout(new QHBoxLayout(m_filterArea))
    , m_filterScroller(new QScrollArea(this))
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
    connect(m_table->horizontalHeader(),&QHeaderView::sectionResized, this, &InstancesWidget::OnSectionResized);
    connect(m_table->horizontalHeader(),&QHeaderView::sectionCountChanged, this, &InstancesWidget::OnSectionCountChanged);
    connect(m_table->horizontalHeader(),&QHeaderView::geometriesChanged, this, &InstancesWidget::PositionFilters);
    connect(m_table->horizontalScrollBar(), &QAbstractSlider::rangeChanged, this,  &InstancesWidget::PositionFilters);
    connect(m_table->horizontalScrollBar(), &QAbstractSlider::actionTriggered, this, &InstancesWidget::PositionFilters);

    m_table->setSortingEnabled(true);
    m_table->setHorizontalScrollMode(QAbstractItemView::ScrollPerPixel);
    m_table->setSelectionBehavior(QTableView::SelectRows);
    m_table->horizontalHeader()->setHighlightSections(false);
    m_table->verticalHeader()->setVisible(false);

    m_table->horizontalHeader()->setContextMenuPolicy(Qt::CustomContextMenu);
    m_table->setContextMenuPolicy(Qt::CustomContextMenu);

    connect(m_table, &QTableView::doubleClicked, this, &InstancesWidget::OnDoubleClicked);
    connect(m_table->horizontalHeader(), &QWidget::customContextMenuRequested, this, &InstancesWidget::OnCustomContextMenuRequestedHeader);
    connect(m_table, &QWidget::customContextMenuRequested, this, &InstancesWidget::OnCustomContextMenuRequestedTable);

    QMetaObject::invokeMethod(this,[this]
        {
            for (int i = 0; i < m_table->horizontalHeader()->count(); ++i)
            {
                auto data = m_proxyModel->headerData(i,Qt::Horizontal, InstancesModelUtils::HideColumnByDefaultRole);
                if (data.isValid() && data.toBool())
                {
                    m_table->hideColumn(i);
                }
            }
        },
        Qt::QueuedConnection);
}

InstancesWidget::InstancesWidget(DobHandler* dob,
                                 int64_t typeId,
                                 bool includeSubclasses,
                                 QWidget* parent)
    : InstancesWidget(parent)
{
    const auto* cls = TypesystemRepository::Instance().GetClass(typeId);
    if (cls != nullptr && cls->dobBaseClass == TypesystemRepository::Entity)
    {
        m_sourceModelEntities = new EntityInstancesModel(dob, typeId, includeSubclasses, this);
        m_proxyModel = new ColumnSortFilterProxyModel(this);
        m_proxyModel->setSourceModel(m_sourceModelEntities);
        m_proxyModel->setFilterRole(InstancesModelUtils::FilterRole);
        m_table->setModel(m_proxyModel);

        connect(m_sourceModelEntities,&EntityInstancesModel::statusBarInfoChanged,
                this, &InstancesWidget::statusBarInfoChanged);
    }
    else
    {
        throw std::logic_error("Failed to find that Entity type for InstancesWidget");
    }

    //this ought to just use the default sizes from the columninfo
    m_table->resizeColumnsToContents();

}

InstancesWidget::InstancesWidget(DobHandler* dob,
                                 int64_t typeId,
                                 const Safir::Dob::Typesystem::ChannelId& channel,
                                 bool includeSubclasses,
                                 QWidget* parent)
    : InstancesWidget(parent)
{
    const auto* cls = TypesystemRepository::Instance().GetClass(typeId);
    if (cls != nullptr && cls->dobBaseClass == TypesystemRepository::Message)
    {
        m_sourceModelMessages = new MessageInstancesModel(dob, typeId, channel, includeSubclasses, this);
        m_proxyModel = new ColumnSortFilterProxyModel(this);
        m_proxyModel->setSourceModel(m_sourceModelMessages);
        m_proxyModel->setFilterRole(InstancesModelUtils::FilterRole);
        m_table->setModel(m_proxyModel);

        connect(m_sourceModelMessages,&MessageInstancesModel::statusBarInfoChanged,
                this, &InstancesWidget::statusBarInfoChanged);
    }
    else
    {
        throw std::logic_error("Failed to find that Message type for InstancesWidget");
    }

    //this ought to just use the default sizes from the columninfo
    m_table->resizeColumnsToContents();
}


InstancesWidget::~InstancesWidget()
{

}


void InstancesWidget::OnDoubleClicked(const QModelIndex &index)
{
    if (!index.isValid())
    {
        return;
    }

    const auto sourceIndex = m_proxyModel->mapToSource(index);
    if (!sourceIndex.isValid())
    {
        return;
    }
    if (m_sourceModelEntities != nullptr)
    {
        const auto& info = m_sourceModelEntities->getRow(sourceIndex.row());
        emit OpenObjectEdit(QString::fromStdWString(info.handlerId.ToString()),
                            info.entityId.GetInstanceId().GetRawValue(),
                            info.entity);
    }
    else if (m_sourceModelMessages != nullptr)
    {
        const auto& info = m_sourceModelMessages->getRow(sourceIndex.row());
        emit OpenObjectEdit(QString::fromStdWString(info.channelId.ToString()),
                            -1,
                            info.message);
    }
}

void InstancesWidget::OnFilterTextChanged(const int column, const QString &text)
{
    const QRegularExpression regex(text,QRegularExpression::CaseInsensitiveOption);

    if (text.isEmpty())
    {
        m_proxyModel->clearFilterRegularExpression(column);
    }
    else if (!regex.isValid())
    {
        m_filters[column]->setStyleSheet("background:red;");
        m_filters[column]->setToolTip(regex.errorString());
        return;
    }
    else
    {
        m_proxyModel->setFilterRegularExpression(column, regex);
    }

    m_filters[column]->setStyleSheet("");
    m_filters[column]->setToolTip("");

}



void InstancesWidget::OnSectionResized(const int logicalIndex, const int /*oldSize*/, const int newSize)
{
    m_filters[logicalIndex]->setHidden(newSize == 0);
    if (newSize != 0)
    {
        m_filters[logicalIndex]->setFixedWidth(newSize-2);
    }

    PositionFilters();
}

void InstancesWidget::OnSectionCountChanged(const int /*oldCount*/, const int newCount)
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
    auto placeholder = QString("%1  Filter").arg(QString::fromUtf8("\xF0\x9F\x94\x8D")); // utf-8 Left-Pointing Magnifying Glass
    for (int i = 0; i < newCount; ++i)
    {
        auto* le = new QLineEdit(this);
        m_filters.push_back(le);

        le->setPlaceholderText(placeholder);
        le->setClearButtonEnabled(true);
        le->setFixedWidth(m_table->columnWidth(i)-2);
        le->setToolTip(tr("Accepts regular expression."));
        connect(le,&QLineEdit::textChanged,this,
                [this,i](const QString& text){OnFilterTextChanged(i,text);});
        m_filterAreaLayout->addWidget(le,1);
    }

    m_filters.push_back(new QWidget(this));
    m_filters.back()->setFixedHeight(10);

    m_filterAreaLayout->addWidget(m_filters.back(),1);

    PositionFilters();
}


void InstancesWidget::OnCustomContextMenuRequestedHeader(const QPoint& pos)
{
    const auto logicalIndex = m_table->horizontalHeader()->logicalIndexAt(pos);
    const auto globalPos = m_table->horizontalHeader()->mapToGlobal(pos);
    RunColumnContextMenu(globalPos, logicalIndex);
}


void InstancesWidget::OnCustomContextMenuRequestedTable(const QPoint& pos)
{
    if (m_table->horizontalHeader()->hiddenSectionCount() == m_table->horizontalHeader()->count())
    {
        const auto globalPos = m_table->mapToGlobal(pos);
        RunColumnContextMenu(globalPos, -1);
    }
}

void InstancesWidget::RunColumnContextMenu(const QPoint& globalPos, const int logicalIndex)
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
}

void InstancesWidget::PositionFilters()
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

QStringList InstancesWidget::statusBarInfo() const
{
    if (m_sourceModelEntities != nullptr)
    {
        return m_sourceModelEntities->statusBarInfo();
    }
    else if (m_sourceModelMessages != nullptr)
    {
        return m_sourceModelMessages->statusBarInfo();
    }
    throw std::logic_error("Not implemented");
}
