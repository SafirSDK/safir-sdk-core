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
#include "typesysteminheritancemodel.h"
#include "iconfactory.h"
#include <QSize>
#include <QDebug>
#include <QIcon>

TypesystemInheritanceModel::TypesystemInheritanceModel(QObject* parent)
    : QAbstractItemModel(parent)
{
}

TypesystemInheritanceModel::~TypesystemInheritanceModel()
{
}

QVariant TypesystemInheritanceModel::headerData(int /*section*/, Qt::Orientation /*orientation*/, int /*role*/) const
{
    return {};
}

QModelIndex TypesystemInheritanceModel::index(int row, int column, const QModelIndex &parent) const
{
    if (!parent.isValid())
    {
        if (row == 0 && column == 0)
        {
            return createIndex(row, column, TypesystemRepository::Instance().GetRootObject());
        }
        return {}; // invalid index
    }

    size_t ix = static_cast<size_t>(row);
    auto parentPtr = static_cast<TypesystemRepository::DobClass*>(parent.internalPointer());
    if (parentPtr != nullptr && column == 0 && ix < parentPtr->children.size())
    {
        return createIndex(row, column, parentPtr->children[ix]);
    }

    return {}; // invalid index
}

QModelIndex TypesystemInheritanceModel::parent(const QModelIndex &index) const
{
    if (!index.isValid())
    {
        return {};
    }

    // This is a bit complicated. We want to find the index of our parent. So we must go back to our grand parent to wind out
    // which row our parent is in our grand parents children-vector.
    auto me = static_cast<const TypesystemRepository::DobClass*>(index.internalPointer());
    if (me != nullptr && me->parent != nullptr)
    {
        if (me->parent->parent != nullptr)
        {
            // We have parent and grand parent. row number for our parent in grandparents children-vector
            auto it = std::find(me->parent->parent->children.begin(), me->parent->parent->children.end(), me->parent);
            size_t ix = std::distance(me->parent->parent->children.begin(), it);
            return createIndex(static_cast<int>(ix), 0, me->parent);
        }
        else
        {
            // We have a parent but no grand parent. Then our parent must have row = 0
            return createIndex(0, 0, me->parent);
        }
    }

    // We have no parent, i.e we are the root node
    return {};
}

int TypesystemInheritanceModel::rowCount(const QModelIndex &parent) const
{
    if (!parent.isValid())
    {
        return 1;
    }

    auto ptr = static_cast<TypesystemRepository::DobClass*>(parent.internalPointer());
    return static_cast<int>(ptr->children.size());
}

int TypesystemInheritanceModel::columnCount(const QModelIndex &/*parent*/) const
{
    return 1;
}

QVariant TypesystemInheritanceModel::data(const QModelIndex &index, int role) const
{
    if (!index.isValid())
    {
        return QVariant();
    }

    auto ptr = static_cast<TypesystemRepository::DobClass*>(index.internalPointer());

    switch (role)
    {
    case Qt::DisplayRole:
        return ptr->name;

    case Qt::DecorationRole:
        return IconFactory::GetIcon(ptr->dobBaseClass, false, false);

    case TypesystemRepository::DobBaseClassRole:
        return ptr->dobBaseClass;

    case TypesystemRepository::DobTypeIdRole:
        return QVariant::fromValue(ptr->typeId);

    default:
        return{};
    }
}

// -----------------------------------------------------
// TypesystemFilterProxyModel
// -----------------------------------------------------
TypesystemFilterProxyModel::TypesystemFilterProxyModel(QObject* parent) : QSortFilterProxyModel(parent)
{
    setRecursiveFilteringEnabled(true); // show parent nodes when a child node is matching a search filter
    setFilterCaseSensitivity(Qt::CaseInsensitive);
}

void TypesystemFilterProxyModel::SetFilter(const QString& filter)
{
    //setFilterRegularExpression(QRegularExpression::wildcardToRegularExpression(filter.isEmpty() ? "*" : "*" + filter + "*"));
    m_filter = filter;
    invalidateFilter();
}

bool TypesystemFilterProxyModel::filterAcceptsRow(int sourceRow, const QModelIndex& sourceParent) const
{
    if (m_filter.isEmpty())
    {
        return true;
    }
    auto ix = sourceModel()->index(sourceRow, 0, sourceParent);
    if (ix.isValid())
    {
        QString val = ix.data().toString();
        bool hit = val.contains(m_filter, Qt::CaseInsensitive);
        if (hit)
        {
            return true;
        }
    }
    return false;
}

QVariant TypesystemFilterProxyModel::data(const QModelIndex &index, int role) const
{
    if (role == Qt::BackgroundRole && !m_filter.isEmpty() && index.data().toString().contains(m_filter, Qt::CaseInsensitive))
    {
        return QColorConstants::Svg::gray;
    }

    return QSortFilterProxyModel::data(index, role);
}
