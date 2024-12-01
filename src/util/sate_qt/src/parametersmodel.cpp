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
#include "parametersmodel.h"
#include <QColor>
#include <QFont>
#include <Safir/Dob/Typesystem/Parameters.h>
#include <memory>

namespace sdt = Safir::Dob::Typesystem;

namespace {
static const int NumberOfColumns = 5;
}

ParametersModel::ParametersModel(int64_t typeId, QObject *parent)
    : QAbstractItemModel(parent)
    , m_typeId(typeId)
{
    SetupModel();
}

QVariant ParametersModel::headerData(int section, Qt::Orientation orientation, int role) const
{
    if (section < NumberOfColumns && orientation == Qt::Horizontal && role == Qt::DisplayRole)
    {
        switch (section)
        {
        case 0: return "Name";
        case 1: return "Value";
        case 2: return "Type";
        }
    }
    return {};
}

QModelIndex ParametersModel::index(int row, int column, const QModelIndex &parent) const
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
    auto childItem = static_cast<MemberTreeItem*>(parent.internalPointer())->GetChildMember(row);
    if (childItem != nullptr)
    {
        return createIndex(row, column, childItem);
    }

    return{};
}

QModelIndex ParametersModel::parent(const QModelIndex &index) const
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

int ParametersModel::rowCount(const QModelIndex &parent) const
{
    const MemberTreeItem* parentItem = parent.isValid()
                                           ? static_cast<const MemberTreeItem*>(parent.internalPointer())
                                           : m_invisibleRootItem.get();

    return parentItem->NumberOfChildMembers();
}

int ParametersModel::columnCount(const QModelIndex &/*parent*/) const
{
    return NumberOfColumns;
}

QVariant ParametersModel::data(const QModelIndex &index, int role) const
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
        case 2: return TypesystemRepository::Instance().GetTypeName(item->GetMemberInfo()->memberType, item->GetMemberInfo()->memberTypeId);
        }
    }
    break;

    case ParametersModel::FilterRole:
    {
        auto item = static_cast<const MemberTreeItem*>(index.internalPointer());
        switch (index.column())
        {
        case 0: return item->GetName();
        case 1: return item->GetValue();
        case 2: return TypesystemRepository::Instance().GetTypeName(item->GetMemberInfo()->memberType, item->GetMemberInfo()->memberTypeId);
        }
    }
    break;

    case ParametersModel::InternalDataRole:
    {
        return QVariant::fromValue(index.internalPointer());
    }
    break;

    case Qt::ForegroundRole:
    {
        if (index.column() == 1)
        {
            auto item = static_cast<const MemberTreeItem*>(index.internalPointer());
            bool hasNonNullValues = false;
            if (item->IsContainerRootItem() || item->IsObjectRootItem())
            {
                for (int i = 0; i < item->NumberOfChildMembers(); ++i)
                {
                    if (!item->GetConstChildMember(i)->IsNull())
                    {
                        hasNonNullValues = true;
                        break;
                    }
                }
            }
            else
            {
                hasNonNullValues = !item->IsNull();
            }

            return hasNonNullValues ? QColor(250, 185, 0) : QColor(135, 134, 132);
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

void ParametersModel::SetupModel()
{
    TypesystemRepository::DobMember m;
    m.name = "root";
    m_parameterMembers.push_back(m);
    m_invisibleRootItem = std::make_unique<MemberTreeItem>(nullptr, &m_parameterMembers.back());

    sdt::MemberType parameterType;
    sdt::MemberType keyType;
    std::wstring parameterName;
    sdt::TypeId parameterTypeId;
    sdt::TypeId keyTypeId;
    sdt::CollectionType collectionType;
    sdt::Int32 numberOfValues;

    auto num = sdt::Parameters::GetNumberOfParameters(m_typeId);
    for (int parIx = 0; parIx < num; ++parIx)
    {
        sdt::Parameters::GetInfo(m_typeId, parIx, parameterType, keyType, parameterName, parameterTypeId, keyTypeId, collectionType, numberOfValues);

        TypesystemRepository::DobMember m;
        m.name = QString::fromStdWString(parameterName);
        m.memberType = parameterType;
        m.memberTypeId = parameterTypeId;
        m.keyType = keyType;
        m.keyTypeId = keyTypeId;
        m.collectionType = collectionType;
        m.arrayLength = numberOfValues;
        m_parameterMembers.push_back(m);

        auto memberItem = std::make_unique<MemberTreeItem>(m_invisibleRootItem.get(), &m_parameterMembers.back());
        auto mi = memberItem.get();
        m_invisibleRootItem->AddChild(std::move(memberItem));

    }
}
