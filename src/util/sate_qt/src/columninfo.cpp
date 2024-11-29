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
#include "columninfo.h"

ColumnInfo::ColumnInfo(const ColumnType columnType, const QString &name)
    : m_columnType(columnType)
    , m_name(name)
{

}


///////////////////////////////////////////////////////////////////////////////
//! Constructor.
///////////////////////////////////////////////////////////////////////////////
ColumnInfo::ColumnInfo(const QString &name,
                       const Safir::Dob::Typesystem::TypeId typeId,
                       const Safir::Dob::Typesystem::MemberIndex memberIndex,
                       const Safir::Dob::Typesystem::MemberType memberType,
                       const Safir::Dob::Typesystem::MemberType keyType,
                       const Safir::Dob::Typesystem::TypeId memberTypeId,
                       const Safir::Dob::Typesystem::TypeId keyTypeId,
                       const Safir::Dob::Typesystem::CollectionType collectionType,
                       const Safir::Dob::Typesystem::Int32 arrayLength)
    : m_columnType(Member)
    , m_name(name)
    , m_typeId(typeId)
    , m_memberIndex(memberIndex)
    , m_memberType(memberType)
    , m_keyType(keyType)
    , m_memberTypeId(memberTypeId)
    , m_keyTypeId(keyTypeId)
    , m_collectionType(collectionType)
    , m_arrayLength(arrayLength)
{

}

ColumnInfo::~ColumnInfo() = default;

ColumnInfoPtr ColumnInfo::CreateOtherColumn(const ColumnType columnType, const QString &name)
{
    return ColumnInfoPtr(new ColumnInfo(columnType,name));
}

ColumnInfoPtr ColumnInfo::CreateMemberColumn(const QString &name,
                                             const Safir::Dob::Typesystem::TypeId typeId,
                                             const Safir::Dob::Typesystem::MemberIndex memberIndex,
                                             const Safir::Dob::Typesystem::MemberType memberType,
                                             const Safir::Dob::Typesystem::MemberType keyType,
                                             const Safir::Dob::Typesystem::TypeId memberTypeId,
                                             const Safir::Dob::Typesystem::TypeId keyTypeId,
                                             const Safir::Dob::Typesystem::CollectionType collectionType,
                                             const Safir::Dob::Typesystem::Int32 arrayLength)
{
    return ColumnInfoPtr(new ColumnInfo(name,typeId,memberIndex,memberType,keyType,memberTypeId,keyTypeId,collectionType,arrayLength));
}


Qt::Alignment ColumnInfo::Alignment() const
{
    switch (m_memberType)
    {
    case BooleanMemberType:
    case EnumerationMemberType:
        return Qt::AlignLeft | Qt::AlignVCenter;
    case Int32MemberType:
    case Int64MemberType:
    case Float32MemberType:
    case Float64MemberType:
        return Qt::AlignRight | Qt::AlignVCenter;
    case TypeIdMemberType:
        return Qt::AlignLeft | Qt::AlignVCenter;
    case InstanceIdMemberType:
        return Qt::AlignRight | Qt::AlignVCenter;
    case EntityIdMemberType:
    case ChannelIdMemberType:
    case HandlerIdMemberType:
    case StringMemberType:
    case ObjectMemberType:
    case BinaryMemberType:
        return Qt::AlignLeft | Qt::AlignVCenter;
        //  SI32 Types
    case Ampere32MemberType:
    case CubicMeter32MemberType:
    case Hertz32MemberType:
    case Joule32MemberType:
    case Kelvin32MemberType:
    case Kilogram32MemberType:
    case Meter32MemberType:
    case MeterPerSecond32MemberType:
    case MeterPerSecondSquared32MemberType:
    case Newton32MemberType:
    case Pascal32MemberType:
    case Radian32MemberType:
    case RadianPerSecond32MemberType:
    case RadianPerSecondSquared32MemberType:
    case Second32MemberType:
    case SquareMeter32MemberType:
    case Steradian32MemberType:
    case Volt32MemberType:
    case Watt32MemberType:

        //  SI Long Types
    case Ampere64MemberType:
    case CubicMeter64MemberType:
    case Hertz64MemberType:
    case Joule64MemberType:
    case Kelvin64MemberType:
    case Kilogram64MemberType:
    case Meter64MemberType:
    case MeterPerSecond64MemberType:
    case MeterPerSecondSquared64MemberType:
    case Newton64MemberType:
    case Pascal64MemberType:
    case Radian64MemberType:
    case RadianPerSecond64MemberType:
    case RadianPerSecondSquared64MemberType:
    case Second64MemberType:
    case SquareMeter64MemberType:
    case Steradian64MemberType:
    case Volt64MemberType:
    case Watt64MemberType:
        return Qt::AlignRight | Qt::AlignVCenter;
    }

    return Qt::AlignLeft;
}


int ColumnInfo::DefaultColumnWidth() const
{
    switch(m_columnType)
    {
    case TypeName:
    case InstanceId:
    case ChannelId:
        return 180;
    case Timestamp:
        return 100;
    case Member:
        break;
    };

    switch(m_memberType)
    {
    case StringMemberType:
        return 150;
    case EntityIdMemberType:
        return 320;
    case TypeIdMemberType:
    case InstanceIdMemberType:
        return 180;
    case Int32MemberType:
    case Int64MemberType:
        return 100;
    case EnumerationMemberType:
        return 150;
    case ChannelIdMemberType:
    case HandlerIdMemberType:
    case ObjectMemberType:
    case BinaryMemberType:
    case Float64MemberType:
    case Ampere64MemberType:
    case CubicMeter64MemberType:
    case Hertz64MemberType:
    case Joule64MemberType:
    case Kelvin64MemberType:
    case Kilogram64MemberType:
    case Meter64MemberType:
    case MeterPerSecond64MemberType:
    case MeterPerSecondSquared64MemberType:
    case Newton64MemberType:
    case Pascal64MemberType:
    case Radian64MemberType:
    case RadianPerSecond64MemberType:
    case RadianPerSecondSquared64MemberType:
    case SquareMeter64MemberType:
    case Steradian64MemberType:
    case Volt64MemberType:
    case Watt64MemberType:
    case Float32MemberType:
    case Ampere32MemberType:
    case CubicMeter32MemberType:
    case Hertz32MemberType:
    case Joule32MemberType:
    case Kelvin32MemberType:
    case Kilogram32MemberType:
    case Meter32MemberType:
    case MeterPerSecond32MemberType:
    case MeterPerSecondSquared32MemberType:
    case Newton32MemberType:
    case Pascal32MemberType:
    case Radian32MemberType:
    case RadianPerSecond32MemberType:
    case RadianPerSecondSquared32MemberType:
    case Second32MemberType:
    case SquareMeter32MemberType:
    case Steradian32MemberType:
    case Volt32MemberType:
    case Watt32MemberType:
        return 100;
    case Second64MemberType:
        return 120;
    case BooleanMemberType:
        return 50;
    }

    return 50;
}
