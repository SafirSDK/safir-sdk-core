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
#include "entityinstancesmodel.h"

#include <iostream>

#include <Safir/Dob/Entity.h>
#include <QSize>



EntityInstancesModel::EntityInstancesModel(DobHandler* dob,
                                           const Safir::Dob::Typesystem::TypeId typeId,
                                           bool includeSubclasses,
                                           QObject* parent)
    : QAbstractTableModel(parent)
    , m_dob(dob)
    , m_typeId(typeId)
    , m_includeSubclasses(includeSubclasses)
{
    setupColumns();
    connect(m_dob, &DobHandler::OnEntity, this, &EntityInstancesModel::OnEntity);
    m_dob->SubscribeEntity(m_typeId,Safir::Dob::Typesystem::InstanceId(), m_includeSubclasses);
}

EntityInstancesModel::~EntityInstancesModel()
{

}

EntityInstancesModel::Info EntityInstancesModel::getRow(int row) const
{
    auto it = m_entities.begin();
    std::advance(it,row);
    return it->second;
}

QStringList EntityInstancesModel::statusBarInfo() const
{
    return {(m_includeSubclasses?"Recursive":""),
            tr("Instances: %1").arg(m_entities.size())};
}

int EntityInstancesModel::rowCount(const QModelIndex& /*parent*/) const
{
    return static_cast<int>(m_entities.size());
}

int EntityInstancesModel::columnCount(const QModelIndex& /*parent*/) const
{
    return m_columnInfoList.count();
}

QVariant EntityInstancesModel::headerData(const int section, const Qt::Orientation orientation, const int role) const
{
    if(orientation == Qt::Horizontal &&
       section >= 0 && section < m_columnInfoList.count())
    {
        ColumnInfoPtr columnInfo = m_columnInfoList.at(section);
        if(columnInfo)
        {
            switch (role)
            {
            case Qt::DisplayRole:
                return columnInfo->Name();
            }
        }
    }

    return QVariant();
}

QVariant EntityInstancesModel::data(const QModelIndex& index, const int role) const
{
    using namespace Safir::Dob::Typesystem;

    if (!index.isValid())
    {
        return QVariant();
    }

    //first get rid of all roles we don't handle
    if (role != Qt::DisplayRole && role != Qt::ToolTipRole && role != FilterRole && role != Qt::TextAlignmentRole)
    {
        return QVariant();
    }

    auto columnInfo = m_columnInfoList.at(index.column());

    if (role == Qt::TextAlignmentRole)
    {
        return QVariant(columnInfo->Alignment());
    }


    const auto entity = std::next(m_entities.cbegin(), index.row());

    if (columnInfo->GetColumnType() == ColumnInfo::TypeName)
    {
        return QString::fromStdWString(Safir::Dob::Typesystem::Operations::GetName(entity->first.GetTypeId()));
    }
    if (columnInfo->GetColumnType() == ColumnInfo::InstanceId)
    {
        return static_cast<qlonglong>(entity->first.GetInstanceId().GetRawValue());
    }

    return MemberToQVariant(entity->second.entity,columnInfo,role);
}


void EntityInstancesModel::setupColumns()
{
    if(!Safir::Dob::Typesystem::Operations::Exists(m_typeId) ||
       !Safir::Dob::Typesystem::Operations::IsOfType(m_typeId,Safir::Dob::Entity::ClassTypeId))
    {
        throw std::logic_error("Invalid type");
    }

    if (m_includeSubclasses)
    {
        m_columnInfoList.append(ColumnInfo::CreateOtherColumn(ColumnInfo::TypeName,tr("TypeName")));
    }
    m_columnInfoList.append(ColumnInfo::CreateOtherColumn(ColumnInfo::InstanceId,tr("InstanceId")));

    AddMembers(m_typeId, m_columnInfoList);
}

void EntityInstancesModel::OnEntity(const sdt::EntityId& entityId,
                                    const sdt::HandlerId& handlerId,
                                    const Safir::Dob::EntityPtr& entity,
                                    const DobInterface::EntityOperation operation)
{
    if (!m_includeSubclasses && entityId.GetTypeId() != m_typeId)
    {
        //not a type we're looking for
        return;
    }
    if(m_includeSubclasses && !Safir::Dob::Typesystem::Operations::IsOfType(entityId.GetTypeId(), m_typeId))
    {
        //not a type we're looking for
        return;
    }

    switch(operation)
    {
    case DobInterface::NewEntity:
        {
            const auto result = m_entities.insert(std::make_pair(entityId, Info{entityId, handlerId, entity}));
            if (result.second)
            {
                const auto row = std::distance(m_entities.begin(), result.first);
                beginInsertRows(QModelIndex(), row, row);
                endInsertRows();
            }
            emit statusBarInfoChanged();
        }
        break;
    case DobInterface::UpdatedEntity:
        {
            const auto result = m_entities.insert_or_assign(entityId,
                                                            Info{entityId, handlerId, entity});
            const auto row = std::distance(m_entities.begin(), result.first);
            emit dataChanged(index(row,0), index(row, m_columnInfoList.count() - 1));
        }
        break;
    case DobInterface::DeletedEntity:
        {
            const auto row = std::distance(m_entities.begin(), m_entities.find(entityId));
            beginRemoveRows(QModelIndex(),row,row);
            m_entities.erase(entityId);
            endRemoveRows();
            emit statusBarInfoChanged();
        }
        break;
    }
}
