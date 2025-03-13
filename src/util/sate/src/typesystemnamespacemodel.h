/******************************************************************************
*
* Copyright Saab AB, 2024 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson
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

#include <QAbstractItemModel>
#include <QSortFilterProxyModel>
#include "typesystemrepository.h"
#include "typesystemrepository.h"

class DobHandler;

class TypesystemNamespaceModel : public QAbstractItemModel
{
    Q_OBJECT

public:
    explicit TypesystemNamespaceModel(DobHandler* dob, QObject* parent);

    // Header:
    QVariant headerData(int section, Qt::Orientation orientation, int role = Qt::DisplayRole) const override;

    // Basic functionality:
    QModelIndex index(int row, int column,
                      const QModelIndex &parent = QModelIndex()) const override;
    QModelIndex parent(const QModelIndex &index) const override;

    int rowCount(const QModelIndex &parent = QModelIndex()) const override;
    int columnCount(const QModelIndex &parent = QModelIndex()) const override;

    QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const override;

private slots:
    void OnNumberOfInstancesChanged(const int64_t typeId);
private:
    // Get a valid index in children or classes vector of ns. If bool is true its a namespace, else class
    std::pair<int, bool> RowIndex(int row, const TypesystemRepository::DobNamespace* ns) const;

    DobHandler* m_dob;
};
