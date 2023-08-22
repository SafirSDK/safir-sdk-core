/******************************************************************************
*
* Copyright Saab AB, 2015, 2022-2023 (http://safirsdkcore.com)
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
#include "Safir/Dob/NotFoundException.h"
#include <Safir/Dob/LowMemoryException.h>
#include <Safir/Dob/NodeParameters.h>
#include <QMessageBox>

NodeTableModel::NodeTableModel(QObject *parent, Safir::Dob::Connection& connection)
    : QAbstractTableModel(parent)
    , m_dobConnection(connection)
{
    try
    {
        m_dobConnection.SubscribeEntity(Safir::Dob::NodeInfo::ClassTypeId, this );
    }
    catch (const Safir::Dob::LowMemoryException&)
    {
        QMessageBox::critical(nullptr,
                              tr("Low Memory"),
                              tr("Cannot subscribe to node information due to low Dob shared memory."));
    }

    for (auto i = 0; i < Safir::Dob::NodeParameters::NodeTypesArraySize(); ++i)
    {
        const auto& nt = Safir::Dob::NodeParameters::NodeTypes(i);

        if (!nt->IsLightNode().IsNull() && nt->IsLightNode().GetVal() == true)
        {
            m_lightNodeTypeNames.insert(Safir::Dob::NodeParameters::NodeTypes(i)->Name().GetVal());
        }
    }
}


int NodeTableModel::rowCount(const QModelIndex & /* parent */) const
{
    return static_cast<int>(m_nodeInfos.size());
}

int NodeTableModel::columnCount(const QModelIndex & /* parent */) const
{
    return NO_COLUMNS;
}

QVariant NodeTableModel::headerData(int section,
                                   Qt::Orientation /*orientation*/,
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
    case NODE_STATE:
        return QString("State");
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

    //quickly leave if it is a role we don't want to handle
    if (role != Qt::DisplayRole && role != Qt::BackgroundRole && role != Qt::ToolTipRole)
    {
        return QVariant();
    }

    Safir::Dob::NodeInfoPtr nodeInfo = nullptr;

    try
    {
        nodeInfo = std::dynamic_pointer_cast<Safir::Dob::NodeInfo>(m_dobConnection.Read(m_nodeInfos.at(index.row())).GetEntity());
    }
    catch (const Safir::Dob::LowMemoryException&)
    {
        //handle nullptr below
    }
    catch (const Safir::Dob::NotFoundException& /*ex*/)
    {
        return QVariant(); //do nothing, we end up here sometimes when the system is going down
    }

    if (role == Qt::DisplayRole)
    {
        if (nodeInfo == nullptr)
        {
            return "...";
        }

        switch (index.column()) {
        case NAME_COLUMN:
            return QString::fromStdWString(nodeInfo->NodeName().GetVal());
            break;
        case IP_COLUMN:
            return QString::fromStdWString(nodeInfo->IpAddress().GetVal());
            break;
        case TYPE_COLUMN:
            {
                const bool isLight = m_lightNodeTypeNames.find(nodeInfo->NodeType().GetVal()) != m_lightNodeTypeNames.end();
                return QString::fromStdWString(nodeInfo->NodeType().GetVal()) + QString::fromStdWString(isLight ? L"  \u24c1 " : L"");
            }
            break;
        case NODE_ID_COLUMN:
            return (qint64) m_nodeInfos.at(index.row()).GetInstanceId().GetRawValue();
            break;
        case NODE_STATE:
            return QString::fromStdWString(Safir::Dob::NodeState::ToString(nodeInfo->State().GetVal()));
        default:
            break;
        }

        return QVariant();
    }
    else if (role == Qt::BackgroundRole)
    {
        //give the local node a slight tint
        if (nodeInfo != nullptr && m_nodeInfos.at(index.row()).GetInstanceId().GetRawValue() == m_ownNodeId)
        {
            return  QColor(164, 237, 166, 127);
        }

        return QVariant();
    }
    else if (nodeInfo != nullptr && role == Qt::ToolTipRole && index.column() == TYPE_COLUMN)
    {
        const bool isLight = m_lightNodeTypeNames.find(nodeInfo->NodeType().GetVal()) != m_lightNodeTypeNames.end();
        if (isLight)
        {
            return tr("Light Node");
        }
    }
    else if (nodeInfo == nullptr && role == Qt::ToolTipRole)
    {
        return tr("Failed to read node information, due to low Dob shared memory");
    }

    return QVariant();
}


//DOB Stuff
void NodeTableModel::OnNewEntity(const Safir::Dob::EntityProxy entityProxy)
{
    beginInsertRows(QModelIndex(), static_cast<int>(m_nodeInfos.size()), static_cast<int>(m_nodeInfos.size()));
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
