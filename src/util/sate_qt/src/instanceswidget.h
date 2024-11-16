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
#pragma once

#include "dobinterface.h"
#include <QTableView>

class EntityInstancesModel;
class QSortFilterProxyModel;

class InstancesWidget
    : public QTableView
{
    Q_OBJECT

public:
    explicit InstancesWidget(DobInterface* dob, int64_t typeId, bool includeSubclasses, QWidget* parent);

    ~InstancesWidget() override;

signals:
    void OpenObjectEdit(int64_t typeId,
                        QString channelHandler,
                        int64_t instance,
                        const Safir::Dob::Typesystem::ObjectPtr& object);

private slots:
    void OnDoubleClicked(const QModelIndex &index);
private:
    QSortFilterProxyModel* m_proxyModel = nullptr;
    EntityInstancesModel* m_sourceModel = nullptr;
};

