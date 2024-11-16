/******************************************************************************
*
* Copyright Saab AB, 2014, 2022 (http://safirsdkcore.com)
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
#pragma once

#include <QAbstractTableModel>

#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Consumer.h>
#include <Safir/Dob/Connection.h>

#include "columninfo.h"
#include "dobinterface.h"

class EntityInstancesModel
    : public QAbstractTableModel
//    , public Safir::Dob::EntitySubscriber
{
    Q_OBJECT
public:
    EntityInstancesModel(DobInterface* dob,
                         const Safir::Dob::Typesystem::TypeId typeId,
                         bool includeSubclasses,
                         QObject* parent);

    ~EntityInstancesModel() override;

    enum Second64Format
    {
        FloatingPoint,
        LocalTime,
        UtcTime
    };

    void setSecond64Format(const Second64Format format);

    struct Info
    {
        Safir::Dob::Typesystem::EntityId entityId;
        Safir::Dob::Typesystem::HandlerId handlerId;
        Safir::Dob::EntityPtr entity;
    };

    Info getRow(int row) const;
private:
    int rowCount(const QModelIndex& parent) const override;
    int columnCount(const QModelIndex& parent) const override;
    QVariant headerData(int section, Qt::Orientation orientation, int role) const override;
    QVariant data(const QModelIndex& index, int role) const override;

    void setupColumns();

    QVariant ContainerToVariant(const Safir::Dob::Typesystem::ContainerBase& container,
                                const Safir::Dob::Typesystem::MemberType memberType,
                                const Safir::Dob::Typesystem::TypeId memberTypeId) const;

    QStringList SequenceToStrings(const Safir::Dob::Typesystem::ContainerBase& container,
                                  const Safir::Dob::Typesystem::MemberType memberType,
                                  const Safir::Dob::Typesystem::TypeId memberTypeId) const;

    QStringList DictionaryToStrings(const Safir::Dob::Typesystem::DictionaryContainerBase& container,
                                    const Safir::Dob::Typesystem::MemberType keyType,
                                    const Safir::Dob::Typesystem::MemberType memberType,
                                    const Safir::Dob::Typesystem::TypeId memberTypeId,
                                    const Safir::Dob::Typesystem::TypeId keyTypeId) const;

    QVariant Second64ToVariant(const Safir::Dob::Typesystem::Si64::Second seconds) const;
private slots:
    void OnEntity(const sdt::EntityId& entityId,
                  const sdt::HandlerId& handler,
                  const Safir::Dob::EntityPtr& entity,
                  DobInterface::EntityOperation operation);

private:
    DobInterface* const m_dob;
    const Safir::Dob::Typesystem::TypeId m_typeId;
    const bool m_includeSubclasses;
    ColumnInfoList m_columnInfoList;
    Second64Format m_second64Format;

    std::map<Safir::Dob::Typesystem::EntityId, Info> m_entities;
};
