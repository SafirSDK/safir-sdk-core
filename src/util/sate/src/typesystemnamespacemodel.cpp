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
#include "dobhandler.h"
#include <QDebug>

namespace
{
#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
    template<class T>
    const void* compat_cast(const T* ptr) {return ptr;}
#else
    template<class T>
    void* compat_cast(const T* ptr) {return const_cast<void*>(static_cast<const void*>(ptr));}
#endif

}

TypesystemNamespaceModel::TypesystemNamespaceModel(DobHandler* dob, QObject* parent)
    : QAbstractItemModel(parent)
    , m_dob(dob)
{
    connect(m_dob,&DobHandler::NumberOfInstancesChanged, this, &TypesystemNamespaceModel::OnNumberOfInstancesChanged);
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
        const auto& rootNs = TypesystemRepository::Instance().GetRootNamespaces();
        if (rowIndex < rootNs.size() && (column == 0 || column == 1))
        {
            return createIndex(row, column, compat_cast(rootNs[rowIndex]));
        }

        return {}; // invalid index
    }

    auto parentPtr = static_cast<TypesystemRepository::DobNamespace*>(parent.internalPointer());
    if (parentPtr != nullptr && (column == 0 || column == 1))
    {
        auto ix = RowIndex(row, parentPtr);
        if (ix.first > -1)
        {
            if (ix.second)
            {
                // Namespace node
                return createIndex(row, column, compat_cast(parentPtr->children[static_cast<size_t>(ix.first)]));
            }
            else
            {
                // Class node
                return createIndex(row, column, compat_cast(parentPtr->units[static_cast<size_t>(ix.first)]));
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

    const TypesystemRepository::DobNamespace* parent = nullptr;
    switch (du->category)
    {
    case TypesystemRepository::Namespace:
        parent = static_cast<const TypesystemRepository::DobNamespace*>(du)->parent;
        break;
    case TypesystemRepository::Class:
        parent = static_cast<const TypesystemRepository::DobClass*>(du)->namespaze;
        break;
    case TypesystemRepository::Enum:
        parent = static_cast<const TypesystemRepository::DobEnum*>(du)->namespaze;
        break;
    default:
        throw std::logic_error("Unexpected category in TypesystemNamespaceModel");
    }
    
    // This is a bit complicated. We want to find the index of our parent. So we must go back to our grand parent to wind out
    // which row our parent is in our grand parents children-vector.
    if (parent != nullptr)
    {
        if (parent->parent != nullptr)
        {
            // We have parent and grand parent. Find row number for our parent in grandparents children-vector
            auto it = std::find(parent->parent->children.begin(), parent->parent->children.end(), parent);
            size_t ix = std::distance(parent->parent->children.begin(), it);
            return createIndex(static_cast<int>(ix), 0, compat_cast(parent));
        }
        else
        {
            // We have a parent but no grand parent. Then our parent is a top namespace
            const auto& rootNamespaces = TypesystemRepository::Instance().GetRootNamespaces();
            auto it = std::find(rootNamespaces.begin(), rootNamespaces.end(), parent);
            size_t ix = std::distance(rootNamespaces.begin(), it);
            return createIndex(static_cast<int>(ix), 0, compat_cast(parent));
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
        return static_cast<int>(parentPtr->children.size() + parentPtr->units.size());
    }
    else
    {
        return 0;
    }

}

int TypesystemNamespaceModel::columnCount(const QModelIndex& /*parent*/) const
{
    return 2;
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
    case Qt::ToolTipRole:
        if (index.column() == 1)
        {
            if (du->category == TypesystemRepository::Class)
            {
                const auto* cls = static_cast<const TypesystemRepository::DobClass*>(du);
                if (cls->dobBaseClass == TypesystemRepository::Entity)
                {
                    const auto instances = m_dob->NumberOfInstances(cls->typeId);
                    if (instances >= 0)
                    {
                        return tr("The number of instances of this class (not including subclasses)\n"
                                  "that Sate has received in its subscription.");
                    }
                }
            }
        }

        return du->name;

    case Qt::DisplayRole:
        if (index.column() == 1)
        {
            if (du->category == TypesystemRepository::Class)
            {
                const auto* cls = static_cast<const TypesystemRepository::DobClass*>(du);
                if (cls->dobBaseClass == TypesystemRepository::Entity)
                {
                    const auto instances = m_dob->NumberOfInstances(cls->typeId);
                    if (instances >= 0)
                    {
                        return static_cast<qlonglong>(instances);
                    }
                }
            }
            return {};
        }
        return du->name;

    case Qt::TextAlignmentRole:
        {
            if (index.column() == 1)
            {
                return int(Qt::AlignRight|Qt::AlignVCenter);
            }
            return {};
        }
    case Qt::DecorationRole:
    {
        if (index.column() == 1)
        {
            return {};
        }
        switch (du->category)
        {
        case TypesystemRepository::Class:
        {
            auto cls = static_cast<const TypesystemRepository::DobClass*>(du);
            auto reg = m_dob->GetMyRegistration(cls->typeId);
            if (reg != nullptr)
            {
                return IconFactory::GetIcon(cls->dobBaseClass, reg->pending ? IconFactory::Pending : IconFactory::Register);
            }
            auto sub = m_dob->GetMySubscription(cls->typeId);
            if (sub != nullptr)
            {
                return IconFactory::GetIcon(cls->dobBaseClass, sub->includeSubclasses ? IconFactory::SubscribeRecursive : IconFactory::Subscribe);
            }
            return IconFactory::GetIcon(cls->dobBaseClass);
        }
        case TypesystemRepository::Enum: return IconFactory::GetEnumIcon();
        case TypesystemRepository::Namespace: return IconFactory::GetNamespaceIcon();
        default: return {};
        }
    }

    case TypesystemRepository::DobBaseClassRole:
    {
        if (du->category == TypesystemRepository::Class)
        {
            return static_cast<const TypesystemRepository::DobClass*>(du)->dobBaseClass;
        }

        return {};
    }

    case TypesystemRepository::DobTypeIdRole:
    {
        if (du->category == TypesystemRepository::Class)
        {
            return QVariant::fromValue(static_cast<const TypesystemRepository::DobClass*>(du)->typeId);
        }
        else if (du->category == TypesystemRepository::Enum)
        {
            return QVariant::fromValue(static_cast<const TypesystemRepository::DobEnum*>(du)->typeId);
        }
        return {};
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
    if (classIx < static_cast<int>(ns->units.size()))
    {
        return std::make_pair(classIx, false);
    }

    return std::make_pair(-1, false);
}

void TypesystemNamespaceModel::OnNumberOfInstancesChanged(const int64_t typeId)
{
    const auto ix = match(index(0,1), TypesystemRepository::DobTypeIdRole, QVariant::fromValue(typeId), 1, Qt::MatchFlags(Qt::MatchExactly | Qt::MatchRecursive));
    if (ix.size() != 1 || !ix.constFirst().isValid())
    {
        throw std::logic_error("OnNumberOfInstancesChanged: Something went terribly wrong in the type tree");
    }
    emit dataChanged(ix.constFirst(),ix.constFirst());
}
