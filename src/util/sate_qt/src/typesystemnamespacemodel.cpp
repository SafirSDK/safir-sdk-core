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
#include "typesystemnamespacemodel.h"
#include "iconfactory.h"
#include <QDebug>

TypesystemNamespaceModel::TypesystemNamespaceModel(QObject* parent)
    : QAbstractItemModel(parent)
{
}

QVariant TypesystemNamespaceModel::headerData(int /*section*/, Qt::Orientation /*orientation*/, int /*role*/) const
{
    return{};
}

QModelIndex TypesystemNamespaceModel::index(int row, int column, const QModelIndex &parent) const
{
    if (!parent.isValid())
    {
        const auto rowIndex = static_cast<size_t>(row);
        auto rootNs = TypesystemRepository::Instance().GetRootNamespaces();
        if (rowIndex < rootNs.size() && column == 0)
        {
            return createIndex(row, column, rootNs[rowIndex]);
        }

        return {}; // invalid index
    }

    auto parentPtr = static_cast<TypesystemRepository::DobNamespace*>(parent.internalPointer());
    if (parentPtr != nullptr && column == 0)
    {
        auto ix = RowIndex(row, parentPtr);
        if (ix.first > -1)
        {
            if (ix.second)
            {
                // Namespace node
                return createIndex(row, column, parentPtr->children[static_cast<size_t>(ix.first)]);
            }
            else
            {
                // Class node
                return createIndex(row, column, parentPtr->classes[static_cast<size_t>(ix.first)]);
            }
        }
    }

    return {}; // invalid index
}

QModelIndex TypesystemNamespaceModel::parent(const QModelIndex &index) const
{
    if (!index.isValid())
    {
        return {};
    }

    auto du = static_cast<const TypesystemRepository::DobUnit*>(index.internalPointer());

    const TypesystemRepository::DobNamespace* parent = (du->category == TypesystemRepository::Namespace) ?
                static_cast<const TypesystemRepository::DobNamespace*>(du)->parent :
                static_cast<const TypesystemRepository::DobClass*>(du)->namespaze;

    // This is a bit complicated. We want to find the index of our parent. So we must go back to our grand parent to wind out
    // which row our parent is in our grand parents children-vector.
    if (parent != nullptr)
    {
        if (parent->parent != nullptr)
        {
            // We have parent and grand parent. Find row number for our parent in grandparents children-vector
            auto it = std::find(parent->parent->children.begin(), parent->parent->children.end(), parent);
            size_t ix = std::distance(parent->parent->children.begin(), it);
            return createIndex(static_cast<int>(ix), 0, parent);
        }
        else
        {
            // We have a parent but no grand parent. Then our parent is a top namespace
            auto rootNamespaces = TypesystemRepository::Instance().GetRootNamespaces();
            auto it = std::find(rootNamespaces.begin(), rootNamespaces.end(), parent);
            size_t ix = std::distance(rootNamespaces.begin(), it);
            return createIndex(static_cast<int>(ix), 0, parent);
        }
    }

    // We have no parent, i.e we are a root namespace
    return {};
}

int TypesystemNamespaceModel::rowCount(const QModelIndex &parent) const
{
    if (!parent.isValid())
    {
        return static_cast<int>(TypesystemRepository::Instance().GetRootNamespaces().size());
    }

    auto du = static_cast<const TypesystemRepository::DobUnit*>(parent.internalPointer());
    if (du->category == TypesystemRepository::Namespace)
    {
        auto parentPtr = static_cast<const TypesystemRepository::DobNamespace*>(du);
        return static_cast<int>(parentPtr->children.size() + parentPtr->classes.size());
    }
    else
    {
        return 0;
    }

}

int TypesystemNamespaceModel::columnCount(const QModelIndex& /*parent*/) const
{
    return 1;
}

QVariant TypesystemNamespaceModel::data(const QModelIndex &index, int role) const
{
    if (!index.isValid())
    {
        return {};
    }

    auto du = static_cast<const TypesystemRepository::DobUnit*>(index.internalPointer());

    switch (role)
    {
    case Qt::DisplayRole:
    {
        if (du->category == TypesystemRepository::Namespace)
        {
            return static_cast<const TypesystemRepository::DobNamespace*>(du)->name;
        }
        else
        {
            return static_cast<const TypesystemRepository::DobClass*>(du)->name;
        }
    }

    case Qt::DecorationRole:
    {
        if (du->category == TypesystemRepository::Namespace)
        {
            return IconFactory::GetNamespaceIcon();
        }
        else
        {
            return IconFactory::GetIcon(static_cast<const TypesystemRepository::DobClass*>(du)->dobBaseClass, false, false);
        }
    }

    case TypesystemRepository::DobBaseClassRole:
    {
        if (du->category == TypesystemRepository::Namespace)
        {
            return {};
        }
        else
        {
            return static_cast<const TypesystemRepository::DobClass*>(du)->dobBaseClass;
        }
    }

    case TypesystemRepository::DobTypeIdRole:
    {
        if (du->category == TypesystemRepository::Namespace)
        {
            return {};
        }
        else
        {
            return QVariant::fromValue(static_cast<const TypesystemRepository::DobClass*>(du)->typeId);
        }
    }

    default:
        return {};
    }
}

// Get a valid index in children or classes vector of ns. If bool is true its a namespace, else class
std::pair<int, bool> TypesystemNamespaceModel::RowIndex(int row, const TypesystemRepository::DobNamespace* ns) const
{
    auto r = static_cast<size_t>(row);
    if (r < ns->children.size())
    {
        return std::make_pair(row, true);
    }

    auto classIx = row - static_cast<int>(ns->children.size());
    if (classIx < static_cast<int>(ns->classes.size()))
    {
        return std::make_pair(classIx, false);
    }

    return std::make_pair(-1, false);
}


