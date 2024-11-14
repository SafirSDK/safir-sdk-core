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
#pragma once

#include <qvariant.h>
#include "typesystemrepository.h"
#include <Safir/Dob/Typesystem/Object.h>

class MemberTreeItem
{
public:
    static const int DeleteItemRole = 1000;

    // Create invisible root, empty or with data
    MemberTreeItem(const TypesystemRepository::DobClass* cls); // Only use this ctor for the invisible root in DobObjectModel
    MemberTreeItem(const TypesystemRepository::DobClass* cls, const Safir::Dob::Typesystem::ObjectConstPtr& obj); // Only use this ctor for the invisible root in DobObjectModel

    // Create submember, null item
    MemberTreeItem(MemberTreeItem* parent, const TypesystemRepository::DobMember* member);
    MemberTreeItem(MemberTreeItem* parent, const TypesystemRepository::DobMember* member, const Safir::Dob::Typesystem::ContainerBase& cb);

    // Members with data created from an object


    const TypesystemRepository::DobMember* GetMemberInfo() const;
    QString GetName() const;
    void SetNull(bool val);
    bool IsNull() const;
    void SetChanged(bool val);
    bool IsChanged() const;
    void SetValue(const QString& value);
    QString GetValue() const;
    void SetKey(const QString& key);
    QString GetKey() const;


    int NumberOfChildMembers() const;
    const MemberTreeItem* GetConstChildMember(int row) const;
    MemberTreeItem* GetChildMember(int row);
    MemberTreeItem* GetParentMember();
    int RowNumber() const; // Returns the index this item is in the parents children list
    void DeleteChild(int row);

    // only meaningful if MemberType is Object
    void SetMemberClass(const TypesystemRepository::DobClass* c);
    const TypesystemRepository::DobClass* GetMemberClass() const;

    // Returns true if this item is just a rootItem for an Object, i.e this item does not contain any real data.
    bool IsObjectRootItem() const;

    // Returns true if this item is just a rootItem for a container: Array, Sequence or Dictionary, i.e this item does not contain any real data.
    bool IsContainerRootItem() const;

    void DebugPrint() const;

private:
    bool m_isNull = true;
    bool m_isChanged = false;
    QString m_value;
    QString m_key;
    const TypesystemRepository::DobMember* m_member = nullptr;
    const TypesystemRepository::DobClass* m_cls = nullptr; // Only valid if this member is an object type.

    MemberTreeItem* m_parent = nullptr;
    std::vector<std::unique_ptr<MemberTreeItem>> m_children; // Object members and array/seq/dictionaries can have children
    bool m_isObjectRoot = false;
    bool m_isContainerRoot = false;

    void SetupObject(); // Empty object
    void SetupObject(const Safir::Dob::Typesystem::ObjectConstPtr& obj); // Object with data
    void SetupMember(const Safir::Dob::Typesystem::ContainerBase& cb);

    void SetItemValue(const Safir::Dob::Typesystem::ContainerBase& cb);
    void SetItemSequenceValues(const Safir::Dob::Typesystem::ContainerBase& cb);
    void SetItemDictionaryValues(const Safir::Dob::Typesystem::ContainerBase& cb);
};
