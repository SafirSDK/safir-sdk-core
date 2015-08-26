/******************************************************************************
*
* Copyright Saab AB, 2015 (http://safir.sourceforge.net)
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
#include "NodeTableModel.h"




NodeTableModel::NodeTableModel(QObject *parent)
    : QAbstractTableModel(parent)
{
    m_dobConnection.Attach();

    m_dobConnection.SubscribeEntity(Safir::Dob::NodeInfo::ClassTypeId, this );
}


int NodeTableModel::rowCount(const QModelIndex & /* parent */) const
{
    return m_nodeInfos.size();
}

int NodeTableModel::columnCount(const QModelIndex & /* parent */) const
{
    return NO_COLUMNS;
}

QVariant NodeTableModel::headerData(int section,
                                   Qt::Orientation orientation,
                                   int role) const
{
    if (role != Qt::DisplayRole && role != Qt::TextAlignmentRole)
        return QVariant();

    if(role == Qt::TextAlignmentRole )
    {
        return Qt::AlignLeft;
    }

    switch (section) {
    case NAME_COLUMN:
        return QString("Name");
        break;
    case IP_COLUMN:
        return QString("IP-Address");
        break;
    case TYPE_COLUMN:
        return QString("Node type");
        break;
    case NODE_ID_COLUMN:
        return QString("Node id");
        break;
    default:
        break;
    }

    return QVariant();
}

QVariant NodeTableModel::data(const QModelIndex &index, int role) const
{
    if (!index.isValid())
        return QVariant();


    if (role == Qt::DisplayRole)
    {
        Safir::Dob::NodeInfoPtr nodeInfo
                = boost::dynamic_pointer_cast<Safir::Dob::NodeInfo>(m_dobConnection.Read(m_nodeInfos.at(index.row())).GetEntity());

        switch (index.column()) {
        case NAME_COLUMN:
            return QString::fromStdWString(nodeInfo->NodeName().GetVal());
            break;
        case IP_COLUMN:
            return QString::fromStdWString(nodeInfo->IpAddress().GetVal());
            break;
        case TYPE_COLUMN:
            return QString::fromStdWString(nodeInfo->NodeType().GetVal());
            break;
        case NODE_ID_COLUMN:
            return (qint64) m_nodeInfos.at(index.row()).GetInstanceId().GetRawValue();
            break;
        default:
            break;
        }

        return QVariant();
    }

    return QVariant();
}


//DOB Stuff
void NodeTableModel::OnNewEntity(const Safir::Dob::EntityProxy entityProxy)
{
    beginInsertRows(QModelIndex(), m_nodeInfos.size(), m_nodeInfos.size());
    m_nodeInfos.push_back(entityProxy.GetEntityId());
    endInsertRows();
}

void NodeTableModel::OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy)
{
    for (unsigned int x = 0; x < m_nodeInfos.size(); ++x)
    {
        if (m_nodeInfos.at(x) == entityProxy.GetEntityId())
        {
           emit dataChanged( createIndex(x, 0), createIndex(x, NO_COLUMNS-1) );
           break;
        }
    }
}

void NodeTableModel::OnDeletedEntity(const Safir::Dob::EntityProxy entityProxy, const bool /*del*/)
{
    for (unsigned int x = 0; x < m_nodeInfos.size(); ++x)
    {
        if (m_nodeInfos.at(x) == entityProxy.GetEntityId())
        {
            beginRemoveRows(QModelIndex(), x, x);
            m_nodeInfos.erase(m_nodeInfos.begin()+x);
            endRemoveRows();
            break;
        }
    }

}

