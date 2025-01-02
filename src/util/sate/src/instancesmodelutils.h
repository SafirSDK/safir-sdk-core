/******************************************************************************
*
* Copyright Saab AB, 2024 (http://safirsdkcore.com)
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

#include <Safir/Dob/Typesystem/DictionaryContainer.h>
#include <QVariant>
#include <QStringList>

#include "columninfo.h"
#include <chrono>

class InstancesModelUtils
{
public:
    enum InstancesModelRoles {FilterRole = Qt::UserRole, HideColumnByDefaultRole};


    static void AddMembers(const int64_t typeId, ColumnInfoList& columnInfoList);

    static QVariant MemberToQVariant(const Safir::Dob::Typesystem::ObjectPtr& object,
                                     const ColumnInfoPtr& columnInfo,
                                     const int role);

    static QVariant MemberColor(const Safir::Dob::Typesystem::ObjectPtr& object,
                                const bool deleted,
                                const std::chrono::steady_clock::time_point& greenUntil,
                                const ColumnInfoPtr& columnInfo);

private:
    static QVariant ContainerToVariant(const Safir::Dob::Typesystem::ContainerBase& container,
                                       const Safir::Dob::Typesystem::MemberType memberType,
                                       const Safir::Dob::Typesystem::TypeId memberTypeId,
                                       const int role);

    static QStringList SequenceToStrings(const Safir::Dob::Typesystem::ContainerBase& container,
                                         const Safir::Dob::Typesystem::MemberType memberType,
                                         const Safir::Dob::Typesystem::TypeId memberTypeId);

    static QStringList DictionaryToStrings(const Safir::Dob::Typesystem::DictionaryContainerBase& container,
                                           const Safir::Dob::Typesystem::MemberType keyType,
                                           const Safir::Dob::Typesystem::MemberType memberType,
                                           const Safir::Dob::Typesystem::TypeId memberTypeId,
                                           const Safir::Dob::Typesystem::TypeId keyTypeId);

    static QVariant Second64ToVariant(const Safir::Dob::Typesystem::Si64::Second seconds,
                                      const int role);
};
