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


MessageInstancesModel::MessageInstancesModel(DobHandler* dob,
                                             const Safir::Dob::Typesystem::TypeId typeId,
                                             const Safir::Dob::Typesystem::ChannelId& channel,
                                             bool includeSubclasses,
                                             QObject* parent)
    : QAbstractTableModel(parent)
    , m_typeId(typeId)
    , m_channel(channel)
    , m_includeSubclasses(includeSubclasses)
{
    setupColumns();

    m_timer.setInterval(500);

    connect(dob, &DobHandler::OnMessage, this, &MessageInstancesModel::OnMessage);
    connect(&m_timer, &QTimer::timeout, this, &MessageInstancesModel::OnTimeout);

    m_timer.start(500);
}

MessageInstancesModel::~MessageInstancesModel()
{

}


MessageInstancesModel::Info MessageInstancesModel::getRow(int row) const
{
    return m_messages[row];
}

QStringList MessageInstancesModel::statusBarInfo() const
{
    return {(m_includeSubclasses?"Recursive":""),
            (m_includeSubclasses?"View is showing subclasses too.":"View is not showing subclasses."),

            tr("Showing: %1").arg(m_messages.size()),
            "Number of messages shown in view.",

            tr("Total: %1").arg(m_numReceived),
            "Total number of messages received since view was opened.",

            (m_messages.size() >= m_maxRows?"Discarding":""),
            (m_messages.size() >= m_maxRows?"Max messages in view reached, discarding old ones\n"
                                            "when new are received.":"")};
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
            case Qt::SizeHintRole:
                return QSize(columnInfo->DefaultColumnWidth(), 20);
            case HideColumnByDefaultRole:
                return columnInfo->HiddenByDefault();
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

    const auto& columnInfo = m_columnInfoList.at(index.column());

    switch (role)
    {
    case Qt::TextAlignmentRole:
        return QVariant(columnInfo->Alignment());

    case Qt::BackgroundRole:
        {
            const auto& messageInfo = m_messages.at(index.row());
            return MemberColor(messageInfo.message, DobInterface::NewEntity, messageInfo.greenUntil, columnInfo);
        }

    case Qt::DisplayRole:
    case Qt::ToolTipRole:
    case FilterRole:
        {
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
    }

    return QVariant();
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

void MessageInstancesModel::OnMessage(const sdt::ChannelId& channel,
                                      const Safir::Dob::MessagePtr& message)
{
    auto typeId = message->GetTypeId();
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
                               message,
                               std::chrono::steady_clock::now() + std::chrono::seconds(5)});

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

    ++m_numReceived;
    emit statusBarInfoChanged();
}

void MessageInstancesModel::OnTimeout()
{
    const auto now = std::chrono::steady_clock::now();
    for (size_t row = 0; row < m_messages.size(); ++row)
    {
        if (m_messages[row].greenUntil == std::chrono::steady_clock::time_point())
        {
            break;
        }

        if (m_messages[row].greenUntil <= now)
        {
            m_messages[row].greenUntil = std::chrono::steady_clock::time_point();
            emit dataChanged(index(static_cast<int>(row),0),
                             index(static_cast<int>(row), m_columnInfoList.count() - 1), {Qt::BackgroundRole});
        }
    }
}
