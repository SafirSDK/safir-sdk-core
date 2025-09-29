/******************************************************************************
*
* Copyright Saab AB, 2025 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / lars@foldspace.nu
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
#include "tracer_status_model.h"
#include <Safir/Dob/Typesystem/InstanceId.h>
#include <Safir/Dob/Typesystem/EntityId.h>
#include <Safir/Dob/OverflowException.h>
#include <Safir/Application/TracerStatus.h>
#include <memory>
#include <iterator>
#include <QString>
#include <cstdint>
#include <algorithm>
#include <QApplication>
#include <QMainWindow>
#include <QStatusBar>
#include "common.h"

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

TracerStatusModel::TracerStatusModel(QObject* parent, Safir::Dob::Connection& dobConnection)
    : QAbstractItemModel(parent)
    , m_dobConnection(dobConnection)
{
    // Subscribe to TracerStatus entities so the model stays up-to-date
    dobConnection.SubscribeEntity(Safir::Application::TracerStatus::ClassTypeId, this);
    dobConnection.SubscribeRegistration(Safir::Application::TracerStatus::ClassTypeId,
                                        Safir::Dob::Typesystem::HandlerId::ALL_HANDLERS,
                                        true,
                                        true,
                                        this);
}

QModelIndex TracerStatusModel::index(int row, int column, const QModelIndex& parent) const
{
    if (parent.isValid()
        || column < 0 || column >= ColumnCount
        || row < 0 || row >= static_cast<int>(m_rowOrder.size()))
    {
        return {};
    }
    return createIndex(row, column);
}

QModelIndex TracerStatusModel::parent(const QModelIndex& /*child*/) const
{
    return {};
}

int TracerStatusModel::rowCount(const QModelIndex& parent) const
{
    if (parent.isValid())
    {
        return 0;
    }
    return static_cast<int>(m_rowOrder.size());
}

int TracerStatusModel::columnCount(const QModelIndex& /*parent*/) const
{
    return ColumnCount;
}

QVariant TracerStatusModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid())
    {
        return {};
    }

    const std::int64_t instanceId = m_rowOrder.at(index.row());
    const auto& info = m_apps.at(instanceId);

    switch (index.column())
    {
    case ProgramNameColumn:
        if (role == Qt::DisplayRole)
            return info.programName;
        break;
    case NodeNameColumn:
        if (role == Qt::DisplayRole)
            return info.nodeName;
        break;
    case StateColumn:
        if (role == Qt::TextAlignmentRole)
            return QVariant(Qt::AlignLeft | Qt::AlignVCenter);
        if (role == Qt::CheckStateRole)
            return info.state;
        break;
    case EditColumn:
        if (role == Qt::DisplayRole)
            return QStringLiteral("...");
        break;
    default:
        break;
    }

    return {};
}

void TracerStatusModel::HandleEntity(const Safir::Dob::EntityProxy& entityProxy)
{
    const auto tracerStatus = std::static_pointer_cast<const Safir::Application::TracerStatus>(entityProxy.GetEntity());

    //ignore tracers that have not registered any prefixes
    if (tracerStatus->Prefixes().empty())
    {
        return;
    }

    // Extract program and node names (treat null as "<null>")
    const QString programName =
        tracerStatus->ProgramName().IsNull()
            ? QStringLiteral("<null>")
            : QString::fromStdWString(tracerStatus->ProgramName().GetVal());

    const QString nodeName =
        tracerStatus->NodeName().IsNull()
            ? QStringLiteral("<null>")
            : QString::fromStdWString(tracerStatus->NodeName().GetVal());

    // Determine tri-state enabled status from all Prefixes
    bool anyEnabled  = false;
    bool allEnabled  = true;
    for (const auto& p : tracerStatus->Prefixes())
    {
        const bool val = !p.second.IsNull() && p.second.GetVal();
        anyEnabled |= val;
        allEnabled &= val;
    }
    Qt::CheckState state;
    if (allEnabled && anyEnabled)
        state = Qt::Checked;
    else if (anyEnabled)
        state = Qt::PartiallyChecked;
    else
        state = Qt::Unchecked;

    const std::int64_t instanceId = entityProxy.GetInstanceId().GetRawValue();
    const bool isNew = (m_apps.find(instanceId) == m_apps.end());

    if (isNew)
    {
        const int row = static_cast<int>(m_rowOrder.size());
        beginInsertRows(QModelIndex(), row, row);
        m_rowOrder.push_back(instanceId);
        m_apps[instanceId] = { programName, nodeName, state };
        endInsertRows();
    }
    else
    {
        m_apps[instanceId] = { programName, nodeName, state };
        const int row = static_cast<int>(std::distance(m_rowOrder.begin(),
                                                       std::find(m_rowOrder.begin(), m_rowOrder.end(), instanceId)));
        emit dataChanged(index(row, 0), index(row, ColumnCount - 1),
                         { Qt::DisplayRole, Qt::CheckStateRole });
    }
}



void TracerStatusModel::OnDeletedEntity(const Safir::Dob::EntityProxy entityProxy, const bool /*deprecated*/)
{
    const std::int64_t instanceId = entityProxy.GetPrevious().GetInstanceId().GetRawValue();
    auto itRow = std::find(m_rowOrder.begin(), m_rowOrder.end(), instanceId);
    if (itRow != m_rowOrder.end())
    {
        const int row = static_cast<int>(std::distance(m_rowOrder.begin(), itRow));
        beginRemoveRows(QModelIndex(), row, row);
        m_rowOrder.erase(itRow);
        m_apps.erase(instanceId);
        endRemoveRows();
    }
}


bool TracerStatusModel::setData(const QModelIndex& index, const QVariant& value, int role)
{
    if (!index.isValid())
    {
        return false;
    }

    const std::int64_t instanceId = m_rowOrder.at(index.row());

    /* ----------------------------------------------------------
     * Enabled checkbox
     * ---------------------------------------------------------- */
    if (index.column() == StateColumn && role == Qt::CheckStateRole)
    {
        auto& info = m_apps[instanceId];
        const Qt::CheckState cs = static_cast<Qt::CheckState>(value.toInt());
        if (cs == Qt::PartiallyChecked)
        {
            return false;    // user cannot set "partial" explicitly
        }
        if (info.state == cs)
        {
            return false; // no change
        }

        info.state = cs;
        const bool newEnabled = (cs == Qt::Checked);

        try
        {
            const Safir::Dob::Typesystem::EntityId entityId(
                Safir::Application::TracerStatus::ClassTypeId,
                Safir::Dob::Typesystem::InstanceId(instanceId));

            const auto request =
                std::static_pointer_cast<Safir::Application::TracerStatus>(
                    m_dobConnection.Read(entityId).GetEntity());

            for (auto& p : request->Prefixes())
            {
                p.second.SetVal(newEnabled);
            }

            m_dobConnection.UpdateRequest(request,
                                          Safir::Dob::Typesystem::InstanceId(instanceId),
                                          this);
        }
        catch (const Safir::Dob::OverflowException&)
        {
            ShowStatusBarMessage(tr("Got an overflow, try again."));
        }
        catch (const std::exception& ex)
        {
            ShowStatusBarMessage(tr("Failed to send UpdateRequest: ") + ex.what());
        }

        emit dataChanged(index, index, {Qt::CheckStateRole});
        return true;
    }

    /* ----------------------------------------------------------
     * "…" edit button
     * ---------------------------------------------------------- */
    if (index.column() == EditColumn && role == Qt::EditRole)
    {
        emit editRequested(instanceId);
        return true;
    }

    return false;
}

Qt::ItemFlags TracerStatusModel::flags(const QModelIndex& index) const
{
    if (!index.isValid())
    {
        return Qt::NoItemFlags;
    }

    // Entire rows are selectable; the view is configured with SelectRows.
    Qt::ItemFlags f = Qt::ItemIsEnabled | Qt::ItemIsSelectable;

    if (index.column() == StateColumn)
    {
        f |= Qt::ItemIsUserCheckable;
    }
    return f;
}

QVariant TracerStatusModel::headerData(int section, Qt::Orientation orientation, int role) const
{
    if (orientation == Qt::Horizontal && role == Qt::DisplayRole)
    {
        switch (section)
        {
        case ProgramNameColumn: return QStringLiteral("Program");
        case NodeNameColumn:    return QStringLiteral("Node");
        case StateColumn:       return QStringLiteral("On");
        case EditColumn:        return QStringLiteral("");
        default:                return {};
        }
    }
    return QAbstractItemModel::headerData(section, orientation, role);
}

/*------------------------------------------------------------------------------
 * Safir::Dob::Requestor callbacks
 *----------------------------------------------------------------------------*/
void TracerStatusModel::OnResponse(const Safir::Dob::ResponseProxy responseProxy)
{
    if (!responseProxy.IsSuccess())
    {
        ShowStatusBarMessage(tr("Failed to set tracer, got an error response."));
    }
}

void TracerStatusModel::OnNotRequestOverflow()
{

}

void TracerStatusModel::OnRegistered(const Safir::Dob::Typesystem::TypeId /*typeId*/,
                                     const Safir::Dob::Typesystem::HandlerId& /*handlerId*/)
{
    emit registrationStatusChanged(true);
}

void TracerStatusModel::OnUnregistered(const Safir::Dob::Typesystem::TypeId /*typeId*/,
                                       const Safir::Dob::Typesystem::HandlerId& /*handlerId*/)
{
    emit registrationStatusChanged(false);
}
