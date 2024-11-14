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

#include "dobinterface.h"
#include <QAbstractItemModel>
#include <deque>

class ReceivedModel : public QAbstractItemModel
{
    Q_OBJECT

public:

    struct ReceivedObjectInfo
    {
        bool isRead;
        int64_t typeId;
        int64_t instance;
        QString channelHandler;
        QString description;
        Safir::Dob::Typesystem::ObjectPtr object;

        ReceivedObjectInfo(int64_t tid, int64_t inst, const QString& ch, const QString& descr, const Safir::Dob::Typesystem::ObjectPtr& ptr)
            : isRead(false), typeId(tid), instance(inst), channelHandler(ch), description(descr), object(ptr)
        {
        }

        ReceivedObjectInfo(ReceivedObjectInfo& r) = default;
    };

    explicit ReceivedModel(DobInterface* dob, QObject *parent = nullptr);

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

    ReceivedObjectInfo ReadItem(int row);

private:

    std::deque<ReceivedObjectInfo> m_items;

};
