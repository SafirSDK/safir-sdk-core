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

#include <Safir/Dob/Typesystem/Defs.h>

#include <QSharedPointer>
#include <QString>
#include <QObject>

class ColumnInfo;

typedef QSharedPointer<ColumnInfo> ColumnInfoPtr;
typedef QList<ColumnInfoPtr> ColumnInfoList;


class ColumnInfo
    : public QObject
{
    Q_OBJECT
public:
    ~ColumnInfo();

    //! Static factory method.
    static ColumnInfoPtr Create(const int width,
                                const QString &name);
    //! Static factory method.
    static ColumnInfoPtr Create(const int width,
                                const QString &name,
                                const Safir::Dob::Typesystem::TypeId typeId,
                                const Safir::Dob::Typesystem::MemberIndex memberIndex,
                                const Safir::Dob::Typesystem::MemberType memberType,
                                const Safir::Dob::Typesystem::MemberType keyType,
                                const Safir::Dob::Typesystem::TypeId memberTypeId,
                                const Safir::Dob::Typesystem::TypeId keyTypeId,
                                const Safir::Dob::Typesystem::CollectionType collectionType,
                                const Safir::Dob::Typesystem::Int32 arrayLength);

    // Is entity id column or not.
    bool IsEntityIdColumn() const { return m_entityIdColumn; }

    int Width() const { return m_width; }
    QString Name() const { return m_name; }

    Safir::Dob::Typesystem::TypeId TypeId() const { return m_typeId; }
    Safir::Dob::Typesystem::Int32 MemberIndex() const { return m_memberIndex; }
    Safir::Dob::Typesystem::MemberType KeyType() const { return m_keyType; }
    Safir::Dob::Typesystem::MemberType MemberType() const { return m_memberType; }
    Safir::Dob::Typesystem::TypeId MemberTypeId() const { return m_memberTypeId; }
    Safir::Dob::Typesystem::TypeId KeyTypeId() const { return m_keyTypeId; }
    Safir::Dob::Typesystem::CollectionType CollectionType() const {return m_collectionType;}
    Safir::Dob::Typesystem::Int32 ArrayLength() const {return m_arrayLength;}

private:

    ColumnInfo(int width,
               const QString &name);
    ColumnInfo(int width,
               const QString &name,
               const Safir::Dob::Typesystem::TypeId typeId,
               const Safir::Dob::Typesystem::MemberIndex memberIndex,
               const Safir::Dob::Typesystem::MemberType memberType,
               const Safir::Dob::Typesystem::MemberType keyType,
               const Safir::Dob::Typesystem::TypeId memberTypeId,
               const Safir::Dob::Typesystem::TypeId keyTypeId,
               const Safir::Dob::Typesystem::CollectionType collectionType,
               const Safir::Dob::Typesystem::Int32 arrayLength);

    // Member variables.
    const bool m_entityIdColumn;
    const int m_width;
    const QString m_name;
    const Safir::Dob::Typesystem::TypeId m_typeId;
    const Safir::Dob::Typesystem::Int32 m_memberIndex;
    const Safir::Dob::Typesystem::MemberType m_memberType;
    const Safir::Dob::Typesystem::MemberType m_keyType;
    const Safir::Dob::Typesystem::TypeId m_memberTypeId;
    const Safir::Dob::Typesystem::TypeId m_keyTypeId;
    const Safir::Dob::Typesystem::CollectionType m_collectionType;
    const Safir::Dob::Typesystem::Int32 m_arrayLength;

};
