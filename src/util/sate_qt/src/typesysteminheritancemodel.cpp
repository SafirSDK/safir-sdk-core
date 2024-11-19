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

namespace
{
    //Qt5 and Qt6 have different constness in the third parameter of the createIndex function.
    //This casting function allows us to be compatible with both.
#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
    template<class T>
    const void* compat_cast(const T* ptr) {return ptr;}
#else
    template<class T>
    void* compat_cast(const T* ptr) {return const_cast<void*>(static_cast<const void*>(ptr));}
#endif

}

TypesystemInheritanceModel::TypesystemInheritanceModel(QObject* parent)
    : QAbstractItemModel(parent)
    , m_rootEnum(new TypesystemRepository::DobEnum())
{
    m_rootEnum->name = "Enums";
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
    if (column != 0)
    {
        return {}; // invalid index
    }

    if (!parent.isValid())
    {
        if (row == 0)
        {
            return createIndex(row, column, compat_cast(TypesystemRepository::Instance().GetRootObject()));
        }
        else if (row == 1)
        {
            return createIndex(row, column, m_rootEnum.get());
        }
        return {}; // invalid index
    }

    size_t ix = static_cast<size_t>(row);
    auto parentPtr = static_cast<const TypesystemRepository::DobUnit*>(parent.internalPointer());
    if (parentPtr->category == TypesystemRepository::Class)
    {
        auto clsPtr = static_cast<const TypesystemRepository::DobClass*>(parent.internalPointer());
        if (clsPtr != nullptr && ix < clsPtr->children.size())
        {
            return createIndex(row, column, compat_cast(clsPtr->children[ix]));
        }
    }
    else if (parentPtr->category == TypesystemRepository::Enum)
    {
        if (ix < TypesystemRepository::Instance().EnumsSorted().size())
        {
            return createIndex(row, column, compat_cast(TypesystemRepository::Instance().EnumsSorted()[ix]));
        }
    }

    return {}; // invalid index
}

QModelIndex TypesystemInheritanceModel::parent(const QModelIndex &index) const
{
    if (!index.isValid())
    {
        return {};
    }

    auto dobUnit = static_cast<const TypesystemRepository::DobUnit*>(index.internalPointer());

    if (dobUnit->category == TypesystemRepository::Class)
    {
        // This is a bit complicated. We want to find the index of our parent. So we must go back to our grand parent to find out
        // which row our parent is in our grand parents children-vector.
        auto me = static_cast<const TypesystemRepository::DobClass*>(index.internalPointer());
        if (me != nullptr && me->parent != nullptr)
        {
            if (me->parent->parent != nullptr)
            {
                // We have parent and grand parent. row number for our parent in grandparents children-vector
                auto it = std::find(me->parent->parent->children.begin(), me->parent->parent->children.end(), me->parent);
                size_t ix = std::distance(me->parent->parent->children.begin(), it);
                return createIndex(static_cast<int>(ix), 0, compat_cast(me->parent));
            }
            else
            {
                // We have a parent but no grand parent. Then our parent must have row = 0
                return createIndex(0, 0, compat_cast(me->parent));
            }
        }
    }
    else if (dobUnit->category == TypesystemRepository::Enum)
    {
        auto me = static_cast<const TypesystemRepository::DobEnum*>(index.internalPointer());
        if (me != m_rootEnum.get())
        {
            return createIndex(1, 0, compat_cast(m_rootEnum.get()));
        }
    }

    // We have no parent, i.e we are the root node
    return {};
}

int TypesystemInheritanceModel::rowCount(const QModelIndex &parent) const
{
    if (!parent.isValid())
    {
        return 2;
    }

    auto dobUnit = static_cast<const TypesystemRepository::DobUnit*>(parent.internalPointer());
    if (dobUnit->category == TypesystemRepository::Class)
    {
        auto cls = static_cast<TypesystemRepository::DobClass*>(parent.internalPointer());
        return static_cast<int>(cls->children.size());
    }
    else if (dobUnit->category == TypesystemRepository::Enum)
    {
        auto en = static_cast<TypesystemRepository::DobEnum*>(parent.internalPointer());
        if (en == m_rootEnum.get())
        {
            return static_cast<int>(TypesystemRepository::Instance().EnumsSorted().size());
        }
    }

    return 0;
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

    auto dobUnit = static_cast<const TypesystemRepository::DobUnit*>(index.internalPointer());
    if (dobUnit->category == TypesystemRepository::Class)
    {
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
    else if (dobUnit->category == TypesystemRepository::Enum)
    {
        auto ptr = static_cast<TypesystemRepository::DobEnum*>(index.internalPointer());
        switch (role)
        {
        case Qt::DisplayRole:
            return ptr->name;

        case Qt::DecorationRole:
            return ptr != m_rootEnum.get() ? IconFactory::GetEnumIcon() : QVariant();

        case TypesystemRepository::DobTypeIdRole:
            return ptr != m_rootEnum.get() ? QVariant::fromValue(ptr->typeId) : QVariant();

        default:
            return{};
        }
    }

    return {};
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
        return val.contains(m_filter, Qt::CaseInsensitive);
    }
    return false;
}

QVariant TypesystemFilterProxyModel::data(const QModelIndex &index, int role) const
{
    if (role == Qt::BackgroundRole && !m_filter.isEmpty() && index.data().toString().contains(m_filter, Qt::CaseInsensitive))
    {
        return QColor(128,128,128); //Gray. The QColorConstants don't seem to work in vs2015...
    }

    return QSortFilterProxyModel::data(index, role);
}
