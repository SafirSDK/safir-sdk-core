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
#include "receivedmodel.h"
#include "iconfactory.h"
#include <QDebug>

namespace
{
static const int NumberOfColumns = 4;
static const int MaxNumberOfRows = 10;

QString Str(DobInterface::EntityOperation op)
{
    switch (op) {
    case DobInterface::NewEntity:
        return "New Entity";
    case DobInterface::UpdatedEntity:
        return "Updated Entity";
    case DobInterface::DeletedEntity:
        return "Deleted Entity";
    }
    return "";
}

QString Str(DobInterface::RequestCategory category)
{
    switch (category)
    {
    case DobInterface::CreateEntity:
        return "Create Request";
    case DobInterface::UpdateEntity:
        return "Update Request";
    case DobInterface::DeleteEntity:
        return "Delete Request";
    case DobInterface::Service:
        return "Service Request";
        break;
    }
    return "";
}
}

ReceivedModel::ReceivedModel(DobHandler* dob, QObject *parent)
    : QAbstractItemModel(parent)
{
    connect(dob, &DobHandler::OnEntity, this, [this](const sdt::EntityId& entityId, const sdt::HandlerId& handler, const Safir::Dob::EntityPtr& entity, DobInterface::EntityOperation operation)
    {
        beginResetModel();
        m_items.emplace_front(entityId.GetTypeId(), entityId.GetInstanceId().GetRawValue(), QString::fromStdWString(handler.ToString()), Str(operation), entity);
        if (m_items.size() > MaxNumberOfRows)
        {
            m_items.pop_back();
        }
        endResetModel();
    });

    connect(dob, &DobHandler::OnMessage, [this](int64_t typeId, const sdt::ChannelId& channel, const Safir::Dob::MessagePtr& message)
    {
        beginResetModel();
        m_items.emplace_front(typeId, 0, QString::fromStdWString(channel.ToString()), "Received Message", message);
        if (m_items.size() > MaxNumberOfRows)
        {
            m_items.pop_back();
        }
        endResetModel();
    });

    connect(dob, &DobHandler::OnRequest, [this](const Safir::Dob::Typesystem::ObjectPtr& request, DobInterface::RequestCategory category)
    {
        beginResetModel();
        m_items.emplace_front(request->GetTypeId(), 0, "", Str(category), request);
        if (m_items.size() > MaxNumberOfRows)
        {
            m_items.pop_back();
        }
        endResetModel();
    });

    connect(dob, &DobHandler::OnResponse, [this](const Safir::Dob::ResponsePtr& response)
    {
        beginResetModel();
        m_items.emplace_front(response->GetTypeId(), 0, "", "Received Response", response);
        if (m_items.size() > MaxNumberOfRows)
        {
            m_items.pop_back();
        }
        endResetModel();
    });
}

QVariant ReceivedModel::headerData(int section, Qt::Orientation orientation, int role) const
{
    if (role == Qt::DisplayRole && orientation == Qt::Horizontal)
    {
        switch (section)
        {
        case 0: return "Read";
        case 1: return "Description";
        case 2: return "Type";
        case 3: return "Handler/Channel";
        }
    }

    return {};
}

QModelIndex ReceivedModel::index(int row, int column, const QModelIndex &parent) const
{
    if (!parent.isValid() && column < NumberOfColumns && row < static_cast<int>(m_items.size()))
    {
        return createIndex(row, column);
    }

    return {};
}

QModelIndex ReceivedModel::parent(const QModelIndex &/*index*/) const
{
    return {};
}

int ReceivedModel::rowCount(const QModelIndex &parent) const
{
    if (!parent.isValid())
    {
        return static_cast<int>(m_items.size());
    }

    return 0;
}

int ReceivedModel::columnCount(const QModelIndex &/*parent*/) const
{
    return NumberOfColumns;
}

QVariant ReceivedModel::data(const QModelIndex &index, int role) const
{
    if (!index.isValid() || index.row() >= static_cast<int>(m_items.size()) || static_cast<int>(index.column()) >= NumberOfColumns)
    {
        return {};
    }

    if (role == Qt::DisplayRole)
    {
        if (index.column() == 0)
        {
            const auto& item = m_items.at(static_cast<size_t>(index.row()));
            return item.isRead ? QString::fromStdWString(L"\u2713") : "";
        }
        else if (index.column() == 1)
        {
            const auto& item = m_items.at(static_cast<size_t>(index.row()));
            return item.description;
        }
        else if (index.column() == 2)
        {
            const auto& item = m_items.at(static_cast<size_t>(index.row()));
            const auto cls = TypesystemRepository::Instance().GetClass(item.typeId);
            return (cls->dobBaseClass == TypesystemRepository::Entity) ? cls->name + ", inst: " + QString::number(item.instance) : cls->name;
        }
        else if (index.column() == 3)
        {
            return m_items.at(static_cast<size_t>(index.row())).channelHandler;
        }
    }
    else if (role == Qt::DecorationRole && index.column() == 1)
    {
        const auto& item = m_items.at(static_cast<size_t>(index.row()));
        if (item.description.contains("Request"))
        {
            return IconFactory::GetIcon(TypesystemRepository::Service, false, false);
        }
        else
        {
            const auto cls = TypesystemRepository::Instance().GetClass(item.typeId);
            return IconFactory::GetIcon(cls->dobBaseClass, false, false);
        }
    }

    return {};
}

ReceivedModel::ReceivedObjectInfo ReceivedModel::ReadItem(int row)
{
    auto index = static_cast<size_t>(row);
    if (m_items.size() > index)
    {
        auto& item = m_items.at(index);
        item.isRead = true;
        emit dataChanged(createIndex(row, 0), createIndex(row, 0), {Qt::DisplayRole});
        return item; // return copy
    }

    return ReceivedModel::ReceivedObjectInfo(0,0, "", "", nullptr);
}
