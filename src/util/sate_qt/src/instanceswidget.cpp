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

#include <QTableView>
#include <QScrollBar>
#include <QHeaderView>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QMenu>
#include <QAction>
#include <QLineEdit>
#include <QCheckBox>
#include <QWidgetAction>
#include <QSortFilterProxyModel>
#include <iostream>
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
    , m_filterArea(new QWidget(this))
    , m_filterAreaLayout(new QHBoxLayout(m_filterArea))
{
    auto* layout = new QVBoxLayout(this);
    layout->addWidget(m_table,100);
    layout->addWidget(m_filterArea,1);

    m_filterArea->setLayout(m_filterAreaLayout);
    m_filterAreaLayout->setSpacing(2);
    m_filterAreaLayout->setContentsMargins(0,0,0,0);

    //connect the header count and sizes
    connect(m_table->horizontalHeader(),&QHeaderView::sectionResized, this, &InstancesWidget::OnSectionResized);
    connect(m_table->horizontalHeader(),&QHeaderView::sectionCountChanged, this, &InstancesWidget::OnSectionCountChanged);

    m_table->setSortingEnabled(true);
    m_table->setSelectionBehavior(QTableView::SelectRows);
    m_table->horizontalHeader()->setHighlightSections(false);
    m_table->verticalHeader()->setVisible(false);
    m_table->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
    m_table->horizontalHeader()->setContextMenuPolicy(Qt::CustomContextMenu);
    m_table->setContextMenuPolicy(Qt::CustomContextMenu);

    connect(m_table, &QTableView::doubleClicked, this, &InstancesWidget::OnDoubleClicked);
    connect(m_table->horizontalHeader(), &QWidget::customContextMenuRequested, this, &InstancesWidget::OnCustomContextMenuRequestedHeader);
    connect(m_table, &QWidget::customContextMenuRequested, this, &InstancesWidget::OnCustomContextMenuRequestedTable);

    //Resize table columns after the table has been populated
    QMetaObject::invokeMethod(this, [this]{m_table->resizeColumnsToContents();}, Qt::QueuedConnection);
    //TODO use default comumn sizes and add a button somewhere to resize to contents
}

InstancesWidget::InstancesWidget(DobInterface* dob,
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
        m_proxyModel->setFilterRole(EntityInstancesModel::FilterRole);
        m_table->setModel(m_proxyModel);
    }
    else
    {
        throw std::logic_error("Failed to find that Entity type for InstancesWidget");
    }
}

InstancesWidget::InstancesWidget(DobInterface* dob,
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
        m_proxyModel->setFilterRole(MessageInstancesModel::FilterRole);
        m_table->setModel(m_proxyModel);
    }
    else
    {
        throw std::logic_error("Failed to find that Message type for InstancesWidget");
    }
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
        emit OpenObjectEdit(info.entityId.GetTypeId(),
                            QString::fromStdWString(info.handlerId.ToString()),
                            info.entityId.GetInstanceId().GetRawValue(),
                            info.entity);
    }
    else if (m_sourceModelMessages != nullptr)
    {
        const auto& info = m_sourceModelMessages->getRow(sourceIndex.row());
        emit OpenObjectEdit(info.typeId,
                            QString::fromStdWString(info.channelId.ToString()),
                            0,
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
    for (int i = 0; i < newCount; ++i)
    {
        m_filters.push_back(new QLineEdit(this));
        m_filters.back()->setPlaceholderText("Filter");
        m_filters.back()->setClearButtonEnabled(true);
        m_filters.back()->setFixedWidth(m_table->columnWidth(i)-2);
        connect(m_filters.back(),&QLineEdit::textChanged,this,
                [this,i](const QString& text){OnFilterTextChanged(i,text);});
        m_filterAreaLayout->addWidget(m_filters.back(),1);
    }
    m_filterAreaLayout->addStretch(100);
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
    auto* hideAction = new QAction(tr("Hide this column"));
    auto* showAllAction = new QAction(tr("Show all columns"));
    auto* hideAllAction = new QAction(tr("Hide all columns"));
    if (logicalIndex != -1)
    {
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
