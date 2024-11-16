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

#include <QHeaderView>
#include <QSortFilterProxyModel>
#include <iostream>
#include "entityinstancesmodel.h"
#include "typesystemrepository.h"

InstancesWidget::InstancesWidget(DobInterface* dob, int64_t typeId, bool includeSubclasses, QWidget* parent)
    : QTableView(parent)
{
    const auto* cls = TypesystemRepository::Instance().GetClass(typeId);
    if (cls == nullptr)
    {
        throw std::logic_error("Failed to find type for InstancesWidget");
    }
    else if (cls->dobBaseClass == TypesystemRepository::Entity)
    {
        m_sourceModel = new EntityInstancesModel(dob, typeId, includeSubclasses, this);
        m_proxyModel = new QSortFilterProxyModel(this);
        m_proxyModel->setSourceModel(m_sourceModel);
        setModel(m_proxyModel);
    }
    else
    {
        throw std::logic_error("InstancesWidget only supports entities at the moment.");
    }

    setSortingEnabled(true);
    setSelectionBehavior(SelectRows);
    horizontalHeader()->setStretchLastSection(true);
    horizontalHeader()->setHighlightSections(false);
    verticalHeader()->setVisible(false);
    resizeColumnsToContents();

    connect(this, &QTableView::doubleClicked, this, &InstancesWidget::OnDoubleClicked);
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

    //    m_sourceModel->getObject

    //TODO: emit signal
}
