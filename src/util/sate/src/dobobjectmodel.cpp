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
#include "dobobjectmodel.h"

#include <QColor>
#include <QFont>
#include <QTreeView>
#include <QTimer>
#include <QDebug>

namespace {
static const int NumberOfColumns = 5;
}

DobObjectModel::DobObjectModel(int64_t typeId, QObject *parent)
    : QAbstractItemModel(parent)
    , m_invisibleRootItem(new MemberTreeItem(TypesystemRepository::Instance().GetClass(typeId)))
    , m_typeId(typeId)
{    
}

DobObjectModel::DobObjectModel(int64_t typeId, const Safir::Dob::Typesystem::ObjectPtr& obj, QObject *parent)
    : QAbstractItemModel(parent)
    , m_invisibleRootItem(new MemberTreeItem(TypesystemRepository::Instance().GetClass(typeId), obj))
    , m_typeId(typeId)
{
}

void DobObjectModel::LiveUpdateModel(const Safir::Dob::Typesystem::ObjectPtr& obj)
{
    m_liveUpdate = true;
    beginResetModel();
    m_invisibleRootItem = std::make_unique<MemberTreeItem>(TypesystemRepository::Instance().GetClass(m_typeId), obj);
    endResetModel();
}

const MemberTreeItem* DobObjectModel::InvisibleRoot() const
{
    return m_invisibleRootItem.get();
}

QVariant DobObjectModel::headerData(int section, Qt::Orientation orientation, int role) const
{
    if (section < NumberOfColumns && orientation == Qt::Horizontal && role == Qt::DisplayRole)
    {
        switch (section)
        {
        case 0: return "Member";
        case 1: return "Value";
        case 2: return "Null";
        case 3: return "Changed";
        case 4: return "Type";
        }
    }
    return {};
}

QModelIndex DobObjectModel::index(int row, int column, const QModelIndex &parent) const
{
    if (row < 0 || column < 0 || column >= NumberOfColumns)
    {
        return{}; // invalid index
    }

    if (!parent.isValid() || parent.internalPointer() == m_invisibleRootItem.get())
    {
        // Root item
        if (row < m_invisibleRootItem->NumberOfChildMembers())
        {
            return createIndex(row, column, m_invisibleRootItem->GetChildMember(row));
        }
        return{}; // invalid index
    }

    // Not the root, i.e nested objects or a container member (array/seq/dict)
    auto parentItem =static_cast<MemberTreeItem*>(parent.internalPointer());
    if (row < parentItem->NumberOfChildMembers())
    {
        return createIndex(row, column, parentItem->GetChildMember(row));
    }

    return{};
}

QModelIndex DobObjectModel::parent(const QModelIndex &index) const
{    
    if (!index.isValid() || index.internalPointer() == m_invisibleRootItem.get())
    {
        return{};
    }

    auto childItem = static_cast<MemberTreeItem*>(index.internalPointer());
    auto parentItem = childItem->GetParentMember();

    if (parentItem != m_invisibleRootItem.get() && parentItem != nullptr)
    {
        return createIndex(parentItem->RowNumber(), 0, parentItem);

    }

    return {};
}

int DobObjectModel::rowCount(const QModelIndex &parent) const
{
    const MemberTreeItem* parentItem = parent.isValid()
        ? static_cast<const MemberTreeItem*>(parent.internalPointer())
        : m_invisibleRootItem.get();

    return parentItem->NumberOfChildMembers();
}

int DobObjectModel::columnCount(const QModelIndex &/*parent*/) const
{
    return NumberOfColumns;
}

QVariant DobObjectModel::data(const QModelIndex &index, int role) const
{
    if (!index.isValid())
    {
        return {};
    }

    switch (role)
    {
    case Qt::DisplayRole:
    {
        auto item = static_cast<const MemberTreeItem*>(index.internalPointer());
        switch (index.column())
        {
        case 0: return item->GetName();
        case 1: return item->GetValue();
        case 2: return item->IsNull() ? QString::fromUtf8("\u2713") : "";
        case 3: return item->IsChanged() ? QString::fromUtf8("\u2713") : "";
        case 4: return TypesystemRepository::Instance().GetTypeName(item->GetMemberInfo()->memberType, item->GetMemberInfo()->memberTypeId);
        }
    }
    break;

    case DobObjectModel::FilterRole:
    {
        auto item = static_cast<const MemberTreeItem*>(index.internalPointer());
        switch (index.column())
        {
        case 0: return item->GetName();
        case 1: return item->GetValue();
        case 2: return item->IsNull() ? "true" : "false";
        case 3: return item->IsChanged() ? "true" : "false";
        case 4: return TypesystemRepository::Instance().GetTypeName(item->GetMemberInfo()->memberType, item->GetMemberInfo()->memberTypeId);
        }
    }
    break;

    case DobObjectModel::InternalDataRole:
    {
        return QVariant::fromValue(index.internalPointer());
    }
    break;

    case Qt::ForegroundRole:
    {
        if (index.column() == 0)
        {
            auto item = static_cast<const MemberTreeItem*>(index.internalPointer());
            auto isContainer = item->GetMemberInfo()->collectionType != SingleValueCollectionType;

            // Blue color if item name is a key, i.e container but not rootItem #74C0FC
            return (isContainer && !item->IsContainerRootItem()) ? QColor(116, 192, 252) :QVariant{};
        }
        if (index.column() == 1)
        {
            auto item = static_cast<const MemberTreeItem*>(index.internalPointer());
            if (item->IsContainerRootItem())
            {
                auto collectionType = item->GetMemberInfo()->collectionType;
                if (item->NumberOfChildMembers() > 0 && (collectionType == SequenceCollectionType || collectionType == DictionaryCollectionType))
                {
                    return QColor(116, 192, 252); // blue
                }
                else if (collectionType == ArrayCollectionType)
                {
                    for (int i = 0; i < item->NumberOfChildMembers(); ++i)
                    {
                        if (!item->GetConstChildMember(i)->IsNull())
                        {
                            return QColor(116, 192, 252);
                        }
                    }
                }
            }

            return item->IsNull() ? QColor(135, 134, 132) : QColor(250, 185, 0);
        }
    }
    break;

    case Qt::BackgroundRole:
    {
        if (m_liveUpdate)
        {
            auto item = static_cast<const MemberTreeItem*>(index.internalPointer());
            if (item->IsChanged())
            {
                return QColor(116, 192, 252, 100); // transparent blue
            }
        }
    }
    break;

    case Qt::TextAlignmentRole:
    {
        if (index.column() == 2 || index.column() == 3)
        {
            return Qt::AlignHCenter;
        }
    }
    break;

    case Qt::FontRole:
    {
        if (index.column() == 1)
        {
            auto item = static_cast<const MemberTreeItem*>(index.internalPointer());
            if (item->IsContainerRootItem() || item->IsObjectRootItem())
            {
                QFont font;
                font.setBold(true);
                return font;
            }
        }
    }
    break;
    }

    return {};
}

bool DobObjectModel::setData(const QModelIndex &index, const QVariant &value, int role)
{
    if (!index.isValid())
    {
        return false;
    }
    if (role != Qt::EditRole && role != MemberTreeItem::DeleteItemRole)
    {
        return false;
    }

    auto item = static_cast<MemberTreeItem*>(index.internalPointer());

    // ---------------------
    // Changed flag column
    // ---------------------
    if (index.column() == 3 && item->IsChanged() != value.toBool())
    {
        item->SetChanged(value.toBool());
        emit dataChanged(index, index, { role });
        return true;
    }

    // ---------------------
    // Value column
    // ---------------------
    if (index.column() == 1)
    {
        auto childCount = item->NumberOfChildMembers();
        
        // --- Handle delete iems, clear container or delete single item in a container. ---
        if (role == MemberTreeItem::DeleteItemRole)
        {
            // If the item is a container root, clear all children.
            if (item->IsContainerRootItem())
            {
                // Rows will be deleted.
                auto parentIndex = createIndex(index.row(), 0, index.internalPointer());
                beginRemoveRows(parentIndex, 0, childCount - 1);
                item->SetNull(true); // Will clear all children.
                item->SetChanged(true);
                endRemoveRows();
            }
            else
            {
                // If the item is a single value in a sequence or dictionary, remove that item from parent container.
                auto parentContainer = item->GetParentMember();
                if (parentContainer->IsContainerRootItem())
                {
                    // The delete must be deferred to after the editor has been closed.
                    auto row = item->RowNumber();
                    auto parentIndex = index.parent();
                    QTimer::singleShot(0, [this, parentIndex, row]
                    {
                        auto pi = static_cast<MemberTreeItem*>(parentIndex.internalPointer());
                        beginRemoveRows(parentIndex, row, row);
                        pi->DeleteChild(row);
                        endRemoveRows();
                    });
                }
            }
            return true;
        }

        // --- Handle setNull ---
        if (value.isNull())
        {
            if (item->IsContainerRootItem() &&
                (item->GetMemberInfo()->collectionType == SequenceCollectionType || item->GetMemberInfo()->collectionType == DictionaryCollectionType))
            {
                return false; // We only clear sequences or dictionaries by DeleteItemRole
            }

            if (!item->IsNull())
            {
                // Set value to null
                if (childCount > 0)
                {
                    // Rows will be deleted.
                    auto parentIndex = createIndex(index.row(), 0, index.internalPointer());
                    beginRemoveRows(parentIndex, 0, childCount - 1);
                    item->SetNull(true);
                    item->SetChanged(true);
                    endRemoveRows();
                }
                else
                {
                    item->SetNull(true);
                    item->SetChanged(true);
                }
                emit dataChanged(createIndex(index.row(), 0, index.internalPointer()), createIndex(index.row(), 3, index.internalPointer()), { role });
            }
            return true;
        }

        // --- Handle insert row in a container ---
        if (item->IsContainerRootItem())
        {
            if (item->GetMemberInfo()->collectionType == DictionaryCollectionType && item->HasChildWithKey(value.toString()))
            {
                return false; // Key already exist in dictionary.
            }

            auto parentIndex = createIndex(index.row(), 0, index.internalPointer());
            beginInsertRows(parentIndex, childCount, childCount);
            item->SetValue(value.toString());
            item->SetChanged(true);
            endInsertRows();
            emit dataChanged(parentIndex, createIndex(index.row(), 3, index.internalPointer()));
            return true;
        }

        // --- Handle init object value ---
        if (item->IsObjectRootItem())
        {
            auto className = value.toString();
            if (!item->IsNull() && item->GetMemberClass()->name == className)
            {
                return false; // Change to same type is ignored.
            }
            auto parentIndex = createIndex(index.row(), 0, index.internalPointer());
            auto numberOfMembers = TypesystemRepository::Instance().GetClass(className)->totalNumberOfMembers;
            if (childCount < numberOfMembers)
            {
                beginInsertRows(parentIndex, childCount, numberOfMembers - 1);
                item->SetValue(className);
                item->SetChanged(true);
                endInsertRows();
            }
            else if (childCount > numberOfMembers)
            {
                beginRemoveRows(parentIndex, numberOfMembers, childCount - 1);
                item->SetValue(className);
                item->SetChanged(true);
                endRemoveRows();
            }
            else
            {
                item->SetValue(className);
                item->SetChanged(true);
            }

            emit dataChanged(createIndex(index.row(), 0, index.internalPointer()), createIndex(index.row(), 3, index.internalPointer()), { role });

            if (numberOfMembers > 0)
            {
                emit dataChanged(this->index(0, 0, index), this->index(numberOfMembers-1, NumberOfColumns-1, index), { role });
                auto valIndex = DobObjectModel::index(0, 1, index);
                QTimer::singleShot(0, [this, valIndex]{
                    emit OpenEditor(valIndex);
                });
            }

            return true;
        }

        // --- Handle edit simple values, strings, numbers enums etc. ---
        if (value.toString() != item->GetValue())
        {
            item->SetValue(value.toString());
            item->SetChanged(true);
            emit dataChanged(createIndex(index.row(), 0, index.internalPointer()), createIndex(index.row(), 3, index.internalPointer()), { role });
            return true;
        }
    }

    return false;
}

Qt::ItemFlags DobObjectModel::flags(const QModelIndex &index) const
{
    if (!index.isValid())
        return Qt::NoItemFlags;

    if (index.column() == 1 || index.column() == 2 || index.column() == 3)
    {
        return QAbstractItemModel::flags(index) | Qt::ItemIsEditable;
    }

    return QAbstractItemModel::flags(index);
    
}
