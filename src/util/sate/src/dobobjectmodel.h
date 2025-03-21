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
#include "membertreeitem.h"
#include <Safir/Dob/Typesystem/Object.h>

class DobObjectModel : public QAbstractItemModel
{
    Q_OBJECT

public:
    static const int FilterRole = 1000;
    static const int InternalDataRole = 1001;

    explicit DobObjectModel(int64_t typeId, QObject *parent);
    explicit DobObjectModel(int64_t typeId, const Safir::Dob::Typesystem::ObjectPtr& obj, QObject *parent);

    // Header:
    QVariant headerData(int section,
                        Qt::Orientation orientation,
                        int role = Qt::DisplayRole) const override;

    // Basic functionality:
    QModelIndex index(int row, int column, const QModelIndex &parent = QModelIndex()) const override;
    QModelIndex parent(const QModelIndex &index) const override;

    int rowCount(const QModelIndex &parent = QModelIndex()) const override;
    int columnCount(const QModelIndex &parent = QModelIndex()) const override;

    QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const override;

    // Editable:
    bool setData(const QModelIndex &index, const QVariant &value, int role = Qt::EditRole) override;

    Qt::ItemFlags flags(const QModelIndex &index) const override;

    const MemberTreeItem* InvisibleRoot() const;
    void LiveUpdateModel(const Safir::Dob::Typesystem::ObjectPtr& obj);
    void DisableLiveUpdate() { m_liveUpdate = false; }


signals:
    void OpenEditor(const QModelIndex& index);

private:
    std::unique_ptr<MemberTreeItem> m_invisibleRootItem;
    int64_t m_typeId;
    bool m_liveUpdate = false;
};
