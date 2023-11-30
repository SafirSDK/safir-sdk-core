/******************************************************************************
*
* Copyright Saab AB, 2015 (http://safirsdkcore.com)
*
* Created by: Samuel Waxin / samuel.waxin@consoden.se
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


#include <Safir/Dob/Connection.h>
#include <Safir/Dob/NodeInfo.h>
#include <set>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4127)
#pragma warning (disable: 4244)
#pragma warning (disable: 4251)
#pragma warning (disable: 4702)
#pragma warning (disable: 4800)
#endif

#include <QAbstractTableModel>
#include <QColor>
#include <QMessageBox>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

enum TableColumn
{
    NAME_COLUMN = 0,
    IP_COLUMN,
    TYPE_COLUMN,
    NODE_ID_COLUMN,
    NODE_STATE_COLUMN,
    MEMORY_LEVEL_COLUMN,
    NO_COLUMNS,
};

class NodeTableModel : public QAbstractTableModel,
                      public Safir::Dob::EntitySubscriber
{
public:
    NodeTableModel(QObject *parent, Safir::Dob::Connection& connection);

    //Dob stuff
    virtual void OnNewEntity(const Safir::Dob::EntityProxy entityProxy) override;
    virtual void OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy) override;
    virtual void OnDeletedEntity(const Safir::Dob::EntityProxy entityProxy, const bool /*del*/) override;

    int rowCount(const QModelIndex &parent = QModelIndex()) const Q_DECL_OVERRIDE;
    int columnCount(const QModelIndex &parent = QModelIndex()) const Q_DECL_OVERRIDE;

    QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const Q_DECL_OVERRIDE;
    QVariant headerData(int section, Qt::Orientation orientation, int role = Qt::DisplayRole) const Q_DECL_OVERRIDE;

    void setOwnNodeId(int64_t ownNodeId) { beginResetModel(); m_ownNodeId = ownNodeId;  endResetModel(); }

signals:

public slots:

private:

    Safir::Dob::Connection& m_dobConnection;
    std::vector<Safir::Dob::Typesystem::EntityId> m_nodeInfos;
    int64_t m_ownNodeId;
    std::set<std::wstring> m_lightNodeTypeNames;
};

