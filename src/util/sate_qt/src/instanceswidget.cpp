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
#include <QLineEdit>
#include <QSortFilterProxyModel>
#include <iostream>
#include "entityinstancesmodel.h"
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


InstancesWidget::InstancesWidget(DobInterface* dob, int64_t typeId, bool includeSubclasses, QWidget* parent)
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

    const auto* cls = TypesystemRepository::Instance().GetClass(typeId);
    if (cls == nullptr)
    {
        throw std::logic_error("Failed to find type for InstancesWidget");
    }
    else if (cls->dobBaseClass == TypesystemRepository::Entity)
    {
        m_sourceModel = new EntityInstancesModel(dob, typeId, includeSubclasses, this);
        m_proxyModel = new ColumnSortFilterProxyModel(this);
        m_proxyModel->setSourceModel(m_sourceModel);
        m_proxyModel->setFilterRole(EntityInstancesModel::FilterRole);
        m_table->setModel(m_proxyModel);
    }
    else
    {
        throw std::logic_error("InstancesWidget only supports entities at the moment.");
    }

    m_table->setSortingEnabled(true);
    m_table->setSelectionBehavior(QTableView::SelectRows);
    m_table->horizontalHeader()->setHighlightSections(false);
    m_table->verticalHeader()->setVisible(false);
    m_table->resizeColumnsToContents();
    m_table->setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
    m_table->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);

    connect(m_table, &QTableView::doubleClicked, this, &InstancesWidget::OnDoubleClicked);
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
    const auto& info = m_sourceModel->getRow(sourceIndex.row());
    emit OpenObjectEdit(info.entityId.GetTypeId(),
                        QString::fromStdWString(info.handlerId.ToString()),
                        info.entityId.GetInstanceId().GetRawValue(),
                        info.entity);
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
    m_filters[logicalIndex]->setFixedWidth(newSize-2);
}

void InstancesWidget::OnSectionCountChanged(const int /*oldCount*/, const int newCount)
{
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

