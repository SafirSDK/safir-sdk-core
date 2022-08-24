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
#include "ColumnInfo.h"

ColumnInfo::ColumnInfo(const int width,
                       const QString &name)
    : m_entityIdColumn(true)
    , m_width(width)
    , m_name(name)
    , m_typeId(0)
    , m_memberIndex(0)
    , m_memberType(BooleanMemberType)
    , m_keyType(BooleanMemberType)
    , m_memberTypeId(0)
    , m_keyTypeId(0)
    , m_collectionType(SingleValueCollectionType)
    , m_arrayLength(0)
{

}


///////////////////////////////////////////////////////////////////////////////
//! Constructor.
///////////////////////////////////////////////////////////////////////////////
ColumnInfo::ColumnInfo(int width,
                       const QString &name,
                       const Safir::Dob::Typesystem::TypeId typeId,
                       const Safir::Dob::Typesystem::MemberIndex memberIndex,
                       const Safir::Dob::Typesystem::MemberType memberType,
                       const Safir::Dob::Typesystem::MemberType keyType,
                       const Safir::Dob::Typesystem::TypeId memberTypeId,
                       const Safir::Dob::Typesystem::TypeId keyTypeId,
                       const Safir::Dob::Typesystem::CollectionType collectionType,
                       const Safir::Dob::Typesystem::Int32 arrayLength)
    : m_entityIdColumn(false)
    , m_width(width)
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


ColumnInfoPtr ColumnInfo::Create(int width,
                                 const QString &name)
{
    return ColumnInfoPtr(new ColumnInfo(width,name));
}


ColumnInfoPtr ColumnInfo::Create(const int width,
                                 const QString &name,
                                 const Safir::Dob::Typesystem::TypeId typeId,
                                 const Safir::Dob::Typesystem::MemberIndex memberIndex,
                                 const Safir::Dob::Typesystem::MemberType memberType,
                                 const Safir::Dob::Typesystem::MemberType keyType,
                                 const Safir::Dob::Typesystem::TypeId memberTypeId,
                                 const Safir::Dob::Typesystem::TypeId keyTypeId,
                                 const Safir::Dob::Typesystem::CollectionType collectionType,
                                 const Safir::Dob::Typesystem::Int32 arrayLength)
{
    return ColumnInfoPtr(new ColumnInfo(width,name,typeId,memberIndex,memberType,keyType,memberTypeId,keyTypeId,collectionType,arrayLength));
}


