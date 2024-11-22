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
#include "messageinstancesmodel.h"

#include <iostream>

#include <Safir/Dob/Typesystem/Members.h>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Dob/Message.h>
#include <Safir/Time/TimeProvider.h>
#include <QSize>


MessageInstancesModel::MessageInstancesModel(DobInterface* dob,
                                             const Safir::Dob::Typesystem::TypeId typeId,
                                             const Safir::Dob::Typesystem::ChannelId& channel,
                                             bool includeSubclasses,
                                             QObject* parent)
    : QAbstractTableModel(parent)
    , m_dob(dob)
    , m_typeId(typeId)
    , m_channel(channel)
    , m_includeSubclasses(includeSubclasses)
{
    setupColumns();
    connect(m_dob, &DobInterface::OnMessage, this, &MessageInstancesModel::OnMessage);
    m_dob->SubscribeMessage(m_typeId, channel, m_includeSubclasses);
}

MessageInstancesModel::~MessageInstancesModel()
{

}


MessageInstancesModel::Info MessageInstancesModel::getRow(int row) const
{
    return m_messages[row];
}

int MessageInstancesModel::rowCount(const QModelIndex& /*parent*/) const
{
    return static_cast<int>(m_messages.size());
}

int MessageInstancesModel::columnCount(const QModelIndex& /*parent*/) const
{
    return m_columnInfoList.count();
}

QVariant MessageInstancesModel::headerData(const int section, const Qt::Orientation orientation, const int role) const
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

QVariant MessageInstancesModel::data(const QModelIndex& index, const int role) const
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

    const auto& messageInfo = m_messages.at(index.row());

    if (columnInfo->GetColumnType() == ColumnInfo::Timestamp)
    {
        return messageInfo.receiveTime.toString("hh:mm:ss.zzz");
    }
    if (columnInfo->GetColumnType() == ColumnInfo::TypeName)
    {
        return QString::fromStdWString(Safir::Dob::Typesystem::Operations::GetName(messageInfo.typeId));
    }
    if (columnInfo->GetColumnType() == ColumnInfo::ChannelId)
    {
        return QString::fromStdWString(messageInfo.channelId.ToString());
    }

    return MemberToQVariant(messageInfo.message,columnInfo,role);
}


void MessageInstancesModel::setupColumns()
{
    if(!Safir::Dob::Typesystem::Operations::Exists(m_typeId) ||
       !Safir::Dob::Typesystem::Operations::IsOfType(m_typeId,Safir::Dob::Message::ClassTypeId))
    {
        throw std::logic_error("Invalid type");
    }

    m_columnInfoList.append(ColumnInfo::CreateOtherColumn(ColumnInfo::Timestamp, tr("Receive time")));
    if (m_includeSubclasses)
    {
        m_columnInfoList.append(ColumnInfo::CreateOtherColumn(ColumnInfo::TypeName,tr("TypeName")));
    }
    m_columnInfoList.append(ColumnInfo::CreateOtherColumn(ColumnInfo::ChannelId,tr("ChannelId")));

    AddMembers(m_typeId, m_columnInfoList);
}

void MessageInstancesModel::OnMessage(const int64_t typeId,
                                      const sdt::ChannelId& channel,
                                      const Safir::Dob::MessagePtr& message)
{
    if (!m_includeSubclasses && typeId != m_typeId)
    {
        //not a type we're looking for
        return;
    }
    if(m_includeSubclasses && !Safir::Dob::Typesystem::Operations::IsOfType(typeId, m_typeId))
    {
        //not a type we're looking for
        return;
    }
    if (m_channel != Safir::Dob::Typesystem::ChannelId::ALL_CHANNELS && channel != m_channel)
    {
        //not a channel we're looking for
        return;
    }

    m_messages.push_front(Info{QDateTime::currentDateTime(),
                               typeId,
                               channel,
                               message});

    beginInsertRows(QModelIndex(), 0, 0);
    endInsertRows();

    if (m_messages.size() > m_maxRows)
    {
        beginRemoveRows(QModelIndex(), static_cast<int>(m_maxRows), static_cast<int>(m_messages.size() - 1));
        while(m_messages.size() > m_maxRows)
        {
            m_messages.pop_back();
        }
        endRemoveRows();
    }
}

