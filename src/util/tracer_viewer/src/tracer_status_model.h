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
#pragma once

#include <QAbstractItemModel>
#include <Safir/Dob/Connection.h>
#include <QVariant>
#include <map>
#include <cstdint>
#include <vector>
#include <Safir/Dob/Typesystem/InstanceId.h>
#include <Safir/Application/TracerStatus.h>

class TracerStatusModel
    : public QAbstractItemModel
    , public Safir::Dob::EntitySubscriber
    , public Safir::Dob::RegistrationSubscriber
    , public Safir::Dob::Requestor
{
    Q_OBJECT
public:
    explicit TracerStatusModel(QObject* parent, Safir::Dob::Connection& dobConnection);

    QModelIndex index(int row, int column, const QModelIndex &parent = QModelIndex()) const override;
    QModelIndex parent(const QModelIndex &child) const override;
    int rowCount(const QModelIndex &parent = QModelIndex()) const override;
    int columnCount(const QModelIndex &parent = QModelIndex()) const override;
    QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const override;
    bool setData(const QModelIndex &index, const QVariant &value, int role = Qt::EditRole) override;
    QVariant headerData(int section, Qt::Orientation orientation, int role = Qt::DisplayRole) const override;
    Qt::ItemFlags flags(const QModelIndex &index) const override;

    void OnNewEntity(const Safir::Dob::EntityProxy entityProxy) override { HandleEntity(entityProxy); }
    void OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy) override { HandleEntity(entityProxy); }
    void OnDeletedEntity(const Safir::Dob::EntityProxy entityProxy, const bool deprecated) override;
    void OnResponse(const Safir::Dob::ResponseProxy responseProxy) override;
    void OnNotRequestOverflow() override;
    void OnRegistered(const Safir::Dob::Typesystem::TypeId typeId,
                      const Safir::Dob::Typesystem::HandlerId& handlerId) override;

     void OnUnregistered(const Safir::Dob::Typesystem::TypeId typeId,
                         const Safir::Dob::Typesystem::HandlerId& handlerId) override;

    // Public enumeration for column indices
    enum Column
    {
        ProgramNameColumn = 0,
        NodeNameColumn    = 1,
        StateColumn       = 2,
        EditColumn        = 3,
        ColumnCount       = 4
    };

signals:
    /**
     * Emitted when the user presses the "…" button on a row.
     *
     * @param instanceId  Instance id of the TracerStatus entity belonging to the row.
     */
    void editRequested(std::int64_t instanceId);

    void registrationStatusChanged(const int registered);
private:
    void HandleEntity(const Safir::Dob::EntityProxy& entityProxy);

    /* Qt::CheckState representation of the "enabled" column */
    struct AppInfo
    {
        QString programName;
        QString nodeName;
        Qt::CheckState state;
    };

    std::map<std::int64_t, AppInfo> m_apps;
    std::vector<std::int64_t> m_rowOrder;

    /* Connection used to publish update requests back to DOB */
    Safir::Dob::Connection& m_dobConnection;
};
