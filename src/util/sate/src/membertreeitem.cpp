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
#include "membertreeitem.h"
#include "utilities.h"
#include <Safir/Dob/Typesystem/Internal/BlobOperations.h>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Typesystem/ToolSupport/Internal/BasicTypeOperations.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Dob/Typesystem/DictionaryContainer.h>
#include <QDebug>

namespace
{
QString TypeIdToString(int64_t typeId)
{
    try
    {
        return QString::fromStdWString(Safir::Dob::Typesystem::Operations::GetName(typeId));
    }
    catch (const Safir::Dob::Typesystem::IllegalValueException&)
    {
        return QString::number(typeId);
    }
}

QString GetTypeName(Safir::Dob::Typesystem::MemberType memberType, int64_t typeId)
{
    using namespace Safir::Dob::Typesystem::ToolSupport::Internal::BasicTypeOperations;
    return IsBasicMemberType(memberType) ? QString::fromStdString(MemberTypeToString(memberType)) : QString::fromStdWString(Safir::Dob::Typesystem::Operations::GetName(typeId));
}

void Print(MemberTreeItem* item, QString indent)
{
    qDebug() << indent << item->GetName();
    for (int i = 0; i < item->NumberOfChildMembers(); ++i)
    {
        auto c = item->GetChildMember(i);
        Print(c, indent + "  ");
    }
}

}

MemberTreeItem::MemberTreeItem(const TypesystemRepository::DobClass* cls)
    : m_isNull(false)
    , m_value(cls->name)
    , m_cls(cls)
{
    SetupObject();
}

// Members with data created from an object
MemberTreeItem::MemberTreeItem(const TypesystemRepository::DobClass* cls, const Safir::Dob::Typesystem::ObjectConstPtr& obj)
    : m_isNull(false)
    , m_value(cls->name)
    , m_cls(cls)
{
    //qDebug() << QString::fromStdWString(Safir::Dob::Typesystem::Serialization::ToJson(obj));
    SetupObject(obj);
}

MemberTreeItem::MemberTreeItem(MemberTreeItem* parent, const TypesystemRepository::DobMember* member)
    : m_member(member)
    , m_parent(parent)
{
    if (member->memberType == ObjectMemberType)
    {
        m_isObjectRoot = true;
    }
}

MemberTreeItem::MemberTreeItem(MemberTreeItem* parent, const TypesystemRepository::DobMember* member, const Safir::Dob::Typesystem::ContainerBase& cb)
    : m_member(member)
    , m_parent(parent)
{
    SetupMember(cb);
}

const TypesystemRepository::DobMember* MemberTreeItem::GetMemberInfo() const
{
    return m_member;
}

void MemberTreeItem::SetMemberInfo(const TypesystemRepository::DobMember* memberInfo)
{
    m_member = memberInfo;
}

QString MemberTreeItem::GetName() const
{
    return m_key.isEmpty() ? m_member->name : QString("[%1]").arg(m_key);
}

void MemberTreeItem::SetNull(bool val)
{
    m_isNull = val;
    if (m_isNull)
    {
        m_cls = nullptr;
        m_children.clear();

        if (!m_isContainerRoot)
        {
            m_value.clear();
        }
    }
}

bool MemberTreeItem::IsNull() const
{
    if (m_isContainerRoot)
    {
        if (m_member->collectionType == SequenceCollectionType || m_member->collectionType == DictionaryCollectionType)
        {
            return m_children.empty();
        }

        // ArrayCollectionType - ArrayContainer doesn't have null-flag but for convenience show null in Gui if all items are null.
        for (const auto& i : m_children)
        {
            if (!i->m_isNull)
            {
                return false;
            }
        }
        return true;
    }

    return m_isNull;
}

void MemberTreeItem::SetChanged(bool val)
{
    m_isChanged = val;
    if (m_isContainerRoot && m_member->collectionType == DictionaryCollectionType)
    {
        for (auto& child : m_children)
        {
            child->m_isChanged = val;
        }
    }
}

bool MemberTreeItem::IsChanged() const
{
    if (m_isContainerRoot)
    {
        if (m_member->collectionType == ArrayCollectionType)
        {
            for (const auto& i : m_children)
            {
                if (i->m_isChanged)
                {
                    return true;
                }
            }
            return false;
        }
    }

    return m_isChanged;
}

void MemberTreeItem::SetValue(const QString& value)
{
    if (m_isContainerRoot)
    {
        switch (m_member->collectionType)
        {
        case SingleValueCollectionType:
        {
            // Can't happen
        }
        break;

        case ArrayCollectionType:
        {
            // Can't happen
        }
        break;

        case SequenceCollectionType:
        {
            auto seqItem = std::make_unique<MemberTreeItem>(this, m_member);
            seqItem->SetNull(false);
            seqItem->m_key = QString::number(m_children.size());
            seqItem->m_value = value;

            if (m_member->memberType == ObjectMemberType)
            {
                seqItem->m_cls = TypesystemRepository::Instance().GetClass(value);
                seqItem->SetupObject();
            }

            m_children.emplace_back(std::move(seqItem));
            m_isChanged = true;
        }
        break;

        case DictionaryCollectionType:
        {
            auto dictItem = std::make_unique<MemberTreeItem>(this, m_member);
            dictItem->m_key = value;
            dictItem->m_isChanged = true;
            m_children.emplace_back(std::move(dictItem));
            m_isChanged = true;
        }
        break;

        }
    }
    else // Not container root, just a value
    {
        m_value = value;
        m_isNull = false;
        m_isChanged = true;

        if (m_member->memberType == ObjectMemberType && m_isObjectRoot)
        {
            m_children.clear();
            m_cls = TypesystemRepository::Instance().GetClass(m_value);
            SetupObject();
        }
    }
}

bool MemberTreeItem::HasChildWithKey(const QString& key) const
{
    auto it = std::find_if(m_children.cbegin(), m_children.cend(), [&key](const auto& ptr){ return ptr->m_key == key;});
    return it != m_children.cend();
}

QString MemberTreeItem::GetValue() const
{
    return m_value;
}

void MemberTreeItem::SetKey(const QString& key)
{
    m_key = key;
}

QString MemberTreeItem::GetKey() const
{
    return m_key;
}

bool MemberTreeItem::IsObjectRootItem() const
{
    return m_isObjectRoot;
}

bool MemberTreeItem::IsContainerRootItem() const
{
    return m_isContainerRoot;
}

void MemberTreeItem::SetIsContainerRootItem(const QString& name)
{
    m_isContainerRoot = true;
    m_value = name;
}

int MemberTreeItem::NumberOfChildMembers() const
{
    return static_cast<int>(m_children.size());
}

const MemberTreeItem* MemberTreeItem::GetConstChildMember(int row) const
{
    return m_children.at(row).get();
}

MemberTreeItem* MemberTreeItem::GetChildMember(int row)
{
    return m_children.at(row).get();
}

MemberTreeItem* MemberTreeItem::GetParentMember()
{
    return m_parent;
}

int MemberTreeItem::RowNumber() const
{
    if (m_parent == nullptr)
    {
        return 0;
    }

    const auto it = std::find_if(m_parent->m_children.cbegin(), m_parent->m_children.cend(),
        [this](const std::unique_ptr<MemberTreeItem>& item)
        {
            return item.get() == this;
        });

    if (it != m_parent->m_children.cend())
    {
        return std::distance(m_parent->m_children.cbegin(), it);
    }

    return -1;
}

void MemberTreeItem::DeleteChild(int row)
{
    auto ix = static_cast<size_t>(row);
    m_children.erase(m_children.begin() + ix);
}

void MemberTreeItem::AddChild(std::unique_ptr<MemberTreeItem>&& child)
{
    child->m_parent = this;
    m_children.emplace_back(std::move(child));
}

void MemberTreeItem::SetMemberClass(const TypesystemRepository::DobClass* c)
{
    m_cls = c;
}

const TypesystemRepository::DobClass* MemberTreeItem::GetMemberClass() const
{
    return m_cls;
}

void MemberTreeItem::SetupObject()
{
    using namespace Safir::Dob::Typesystem;

    m_isObjectRoot = true;

    // Create empty object
    for (int memberIndex = 0; memberIndex < m_cls->totalNumberOfMembers; ++memberIndex)
    {
        const auto& member = m_cls->GetMember(memberIndex);

        switch (member.collectionType)
        {
        case SingleValueCollectionType:
        {
            m_children.emplace_back(std::make_unique<MemberTreeItem>(this, &member));
        }
        break;

        case ArrayCollectionType:
        {
            auto arrayRoot = std::make_unique<MemberTreeItem>(this, &member);
            arrayRoot->m_isContainerRoot = true;
            arrayRoot->m_value = "<array>";
            for (int arrIx = 0; arrIx < member.arrayLength; ++arrIx)
            {
                auto arrayItem = std::make_unique<MemberTreeItem>(arrayRoot.get(), &member);
                arrayItem->SetKey(QString::number(arrIx));
                arrayRoot->m_children.emplace_back(std::move(arrayItem));
            }
            m_children.emplace_back(std::move(arrayRoot));
        }
        break;

        case SequenceCollectionType:
        {
            auto seqRoot = std::make_unique<MemberTreeItem>(this, &member);
            seqRoot->m_isContainerRoot = true;
            seqRoot->m_value = "<sequence>";
            m_children.emplace_back(std::move(seqRoot));
        }
        break;

        case DictionaryCollectionType:
        {
            auto dictRoot = std::make_unique<MemberTreeItem>(this, &member);
            dictRoot->m_isContainerRoot = true;
            dictRoot->m_value = "<dictionary keyType=" + GetTypeName(member.keyType, member.keyTypeId) + ">";
            m_children.emplace_back(std::move(dictRoot));
        }
        break;
        }
    }
}

void MemberTreeItem::SetupObject(const Safir::Dob::Typesystem::ObjectConstPtr& object)
{
    using namespace Safir::Dob::Typesystem;

    if (!object)
    {
        SetupObject();
        return;
    }

    m_isObjectRoot = true;

    // Create object with data
    for (int memberIndex = 0; memberIndex < m_cls->totalNumberOfMembers; ++memberIndex)
    {
        const auto& member = m_cls->GetMember(memberIndex);

        switch (member.collectionType)
        {
        case SingleValueCollectionType:
        {
            const Safir::Dob::Typesystem::ContainerBase& cb = object->GetMember(memberIndex, 0);
            m_children.emplace_back(std::make_unique<MemberTreeItem>(this, &member, cb));
        }
        break;

        case ArrayCollectionType:
        {
            auto arrayRoot = std::make_unique<MemberTreeItem>(this, &member);
            arrayRoot->m_isContainerRoot = true;
            arrayRoot->m_value = "<array>";
            for (int arrIx = 0; arrIx < member.arrayLength; ++arrIx)
            {
                const Safir::Dob::Typesystem::ContainerBase& cb = object->GetMember(memberIndex, arrIx);
                auto arrayItem = std::make_unique<MemberTreeItem>(arrayRoot.get(), &member, cb);
                arrayItem->SetKey(QString::number(arrIx));
                arrayRoot->m_children.emplace_back(std::move(arrayItem));
            }
            m_children.emplace_back(std::move(arrayRoot));
        }
        break;

        case SequenceCollectionType:
        {
            const Safir::Dob::Typesystem::ContainerBase& cb = object->GetMember(memberIndex, 0);
            m_children.emplace_back(std::make_unique<MemberTreeItem>(this, &member, cb));
        }
        break;

        case DictionaryCollectionType:
        {
            const Safir::Dob::Typesystem::ContainerBase& cb = object->GetMember(memberIndex, 0);
            m_children.emplace_back(std::make_unique<MemberTreeItem>(this, &member, cb));
        }
        break;
        }
    }
}

void MemberTreeItem::SetupMember(const Safir::Dob::Typesystem::ContainerBase& cb)
{
    switch (m_member->collectionType)
    {
    case SingleValueCollectionType:
    {
        SetItemValue(cb);
    }
    break;

    case ArrayCollectionType:
    {
        SetItemValue(cb);
    }
    break;

    case SequenceCollectionType:
    {
        SetItemSequenceValues(cb);
    }
    break;

    case DictionaryCollectionType:
    {
        SetItemDictionaryValues(cb);
    }
    break;
    }
}

void MemberTreeItem::SetItemValue(const Safir::Dob::Typesystem::ContainerBase& cb)
{
    using namespace Safir::Dob::Typesystem;

    m_isChanged = cb.IsChanged();
    m_isNull = cb.IsNull();
    if (m_isNull)
    {
        return;
    }


    switch (m_member->memberType)
    {
    case BooleanMemberType:
    {
        const auto c = static_cast<const BooleanContainer*>(&cb);
        m_value = c->GetVal() ? "true" : "false";
    }
    break;

    case EnumerationMemberType:
    {
        const auto c = static_cast<const EnumerationContainerBase*>(&cb);
        m_value = QString::fromStdWString(Operations::GetEnumerationValueName(m_member->memberTypeId, c->GetOrdinal()));
    }
    break;

    case Int32MemberType:
    {
        const auto c = static_cast<const Int32Container*>(&cb);
        m_value = QString::number(c->GetVal());
    }
    break;

    case Int64MemberType:
    {
        const auto c = static_cast<const Int64Container*>(&cb);
        m_value = QString::number(c->GetVal());
    }
    break;

    case Float32MemberType:
    {
        const auto c = static_cast<const Float32Container*>(&cb);
        m_value = QString::number(c->GetVal());
    }
    break;


    case Float64MemberType:
    {
        const auto c = static_cast<const Float64Container*>(&cb);
        m_value = QString::number(c->GetVal());
    }
    break;

    case TypeIdMemberType:
    {
        const auto c = static_cast<const TypeIdContainer*>(&cb);
        m_value = TypeIdToString(c->GetVal());
    }
    break;

    case InstanceIdMemberType:
    {
        const auto c = static_cast<const InstanceIdContainer*>(&cb);
        m_value = QString::fromStdWString(c->GetVal().ToString());
    }
    break;

    case EntityIdMemberType:
    {
        const auto c = static_cast<const EntityIdContainer*>(&cb);
        m_value = ::Utilities::EntityIdToString(c->GetVal());
    }
    break;

    case ChannelIdMemberType:
    {
        const auto c = static_cast<const ChannelIdContainer*>(&cb);
        m_value = QString::fromStdWString(c->GetVal().ToString());
    }
    break;

    case HandlerIdMemberType:
    {
        const auto c = static_cast<const HandlerIdContainer*>(&cb);
        m_value = QString::fromStdWString(c->GetVal().ToString());
    }
    break;

    case StringMemberType:
    {
        const auto c = static_cast<const StringContainer*>(&cb);
        m_value = QString::fromStdWString(c->GetVal());
    }
    break;

    case BinaryMemberType:
    {
        const auto c = static_cast<const BinaryContainer*>(&cb);
        auto b64 = Safir::Dob::Typesystem::Utilities::BinaryToBase64(c->GetVal());
        m_value = QString::fromStdString(b64);
    }
    break;

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
    {
        const auto c = static_cast<const Float32Container*>(&cb);
        m_value = QString::number(c->GetVal());
    }
    break;

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
    {
        const auto c = static_cast<const Float64Container*>(&cb);
        m_value = QString::number(c->GetVal());
    }
    break;

    case ObjectMemberType:
    {
        const auto objCont = static_cast<const ObjectContainerBase*>(&cb);
        auto object = objCont->GetObjectPointer();
        auto objTypeId = object->GetTypeId();
        m_cls = TypesystemRepository::Instance().GetClass(objTypeId);
        m_value = QString("<%1>").arg(m_cls->name);
        SetupObject(object);
    }
    break;
    }
}

void MemberTreeItem::SetItemSequenceValues(const Safir::Dob::Typesystem::ContainerBase& cb)
{
    using namespace Safir::Dob::Typesystem;

    m_value = "<sequence>";
    m_isContainerRoot = true;
    m_isChanged = cb.IsChanged();
    m_isNull = cb.IsNull();
    if (m_isNull)
    {
        return;
    }

    int count = 0;
    switch (m_member->memberType)
    {
    case BooleanMemberType:
    {
        const auto c = static_cast<const BooleanSequenceContainer*>(&cb);
        for (auto it = c->begin(); it != c->end(); ++it)
        {
            auto seqItem = std::make_unique<MemberTreeItem>(this, m_member);
            seqItem->SetNull(false);
            seqItem->m_value = (*it) ? "true" : "false";
            seqItem->m_key = QString::number(count++);
            m_children.emplace_back(std::move(seqItem));
        }
    }
    break;

    case EnumerationMemberType:
    {
        const auto c = static_cast<const EnumerationSequenceContainerBase*>(&cb);
        for (size_t i = 0; i < c->size(); ++i)
        {
            auto seqItem = std::make_unique<MemberTreeItem>(this, m_member);
            seqItem->SetNull(false);
            seqItem->m_value = QString::fromStdWString(Operations::GetEnumerationValueName(m_member->memberTypeId, c->GetOrdinal(i)));
            seqItem->m_key = QString::number(count++);
            m_children.emplace_back(std::move(seqItem));
        }
    }
    break;

    case Int32MemberType:
    {
        const auto c = static_cast<const Int32SequenceContainer*>(&cb);
        for (auto it = c->begin(); it != c->end(); ++it)
        {
            auto seqItem = std::make_unique<MemberTreeItem>(this, m_member);
            seqItem->SetNull(false);
            seqItem->m_value = QString::number(*it);
            seqItem->m_key = QString::number(count++);
            m_children.emplace_back(std::move(seqItem));
        }
    }
    break;

    case Int64MemberType:
    {
        const auto c = static_cast<const Int64SequenceContainer*>(&cb);
        for (auto it = c->begin(); it != c->end(); ++it)
        {
            auto seqItem = std::make_unique<MemberTreeItem>(this, m_member);
            seqItem->SetNull(false);
            seqItem->m_value = QString::number(*it);
            seqItem->m_key = QString::number(count++);
            m_children.emplace_back(std::move(seqItem));
        }
    }
    break;

    case Float32MemberType:
    {
        const auto c = static_cast<const Float32SequenceContainer*>(&cb);
        for (auto it = c->begin(); it != c->end(); ++it)
        {
            auto seqItem = std::make_unique<MemberTreeItem>(this, m_member);
            seqItem->SetNull(false);
            seqItem->m_value = QString::number(*it);
            seqItem->m_key = QString::number(count++);
            m_children.emplace_back(std::move(seqItem));
        }
    }
    break;


    case Float64MemberType:
    {
        const auto c = static_cast<const Float64SequenceContainer*>(&cb);
        for (auto it = c->begin(); it != c->end(); ++it)
        {
            auto seqItem = std::make_unique<MemberTreeItem>(this, m_member);
            seqItem->SetNull(false);
            seqItem->m_value = QString::number(*it);
            seqItem->m_key = QString::number(count++);
            m_children.emplace_back(std::move(seqItem));
        }
    }
    break;

    case TypeIdMemberType:
    {
        const auto c = static_cast<const TypeIdSequenceContainer*>(&cb);
        for (auto it = c->begin(); it != c->end(); ++it)
        {
            auto seqItem = std::make_unique<MemberTreeItem>(this, m_member);
            seqItem->SetNull(false);
            seqItem->m_value = TypeIdToString(*it);
            seqItem->m_key = QString::number(count++);
            m_children.emplace_back(std::move(seqItem));
        }
    }
    break;

    case InstanceIdMemberType:
    {
        const auto c = static_cast<const InstanceIdSequenceContainer*>(&cb);
        for (auto it = c->begin(); it != c->end(); ++it)
        {
            auto seqItem = std::make_unique<MemberTreeItem>(this, m_member);
            seqItem->SetNull(false);
            seqItem->m_value = QString::fromStdWString(it->ToString());
            seqItem->m_key = QString::number(count++);
            m_children.emplace_back(std::move(seqItem));
        }
    }
    break;

    case EntityIdMemberType:
    {
        const auto c = static_cast<const EntityIdSequenceContainer*>(&cb);
        for (auto it = c->begin(); it != c->end(); ++it)
        {
            auto seqItem = std::make_unique<MemberTreeItem>(this, m_member);
            seqItem->SetNull(false);
            seqItem->m_value = ::Utilities::EntityIdToString(*it);
            seqItem->m_key = QString::number(count++);
            m_children.emplace_back(std::move(seqItem));
        }
    }
    break;

    case ChannelIdMemberType:
    {
        const auto c = static_cast<const ChannelIdSequenceContainer*>(&cb);
        for (auto it = c->begin(); it != c->end(); ++it)
        {
            auto seqItem = std::make_unique<MemberTreeItem>(this, m_member);
            seqItem->SetNull(false);
            seqItem->m_value = QString::fromStdWString(it->ToString());
            seqItem->m_key = QString::number(count++);
            m_children.emplace_back(std::move(seqItem));
        }
    }
    break;

    case HandlerIdMemberType:
    {
        const auto c = static_cast<const HandlerIdSequenceContainer*>(&cb);
        for (auto it = c->begin(); it != c->end(); ++it)
        {
            auto seqItem = std::make_unique<MemberTreeItem>(this, m_member);
            seqItem->SetNull(false);
            seqItem->m_value = QString::fromStdWString(it->ToString());
            seqItem->m_key = QString::number(count++);
            m_children.emplace_back(std::move(seqItem));
        }
    }
    break;

    case StringMemberType:
    {
        const auto c = static_cast<const StringSequenceContainer*>(&cb);
        for (auto it = c->begin(); it != c->end(); ++it)
        {
            auto seqItem = std::make_unique<MemberTreeItem>(this, m_member);
            seqItem->SetNull(false);
            seqItem->m_value = QString::fromStdWString(*it);
            seqItem->m_key = QString::number(count++);
            m_children.emplace_back(std::move(seqItem));
        }
    }
    break;

    case BinaryMemberType:
    {
        const auto c = static_cast<const BinarySequenceContainer*>(&cb);
        for (auto it = c->begin(); it != c->end(); ++it)
        {
            auto seqItem = std::make_unique<MemberTreeItem>(this, m_member);
            seqItem->SetNull(false);
            auto b64 = Safir::Dob::Typesystem::Utilities::BinaryToBase64(*it);
            seqItem->m_value = QString::fromStdString(b64);
            seqItem->m_key = QString::number(count++);
            m_children.emplace_back(std::move(seqItem));
        }
    }
    break;

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
    {
        const auto c = static_cast<const Float32SequenceContainer*>(&cb);
        for (auto it = c->begin(); it != c->end(); ++it)
        {
            auto seqItem = std::make_unique<MemberTreeItem>(this, m_member);
            seqItem->SetNull(false);
            seqItem->m_value = QString::number(*it);
            seqItem->m_key = QString::number(count++);
            m_children.emplace_back(std::move(seqItem));
        }
    }
    break;

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
    {
        const auto c = static_cast<const Float64SequenceContainer*>(&cb);        
        for (auto it = c->begin(); it != c->end(); ++it)
        {
            auto seqItem = std::make_unique<MemberTreeItem>(this, m_member);
            seqItem->SetNull(false);
            seqItem->m_value = QString::number(*it);
            seqItem->m_key = QString::number(count++);
            m_children.emplace_back(std::move(seqItem));
        }
    }
    break;

    case ObjectMemberType:
    {
        const auto objCont = dynamic_cast<const GenericObjectSequenceContainerBase*>(&cb);
        for (size_t i = 0; i < objCont->size(); ++i)
        {
            const auto object = objCont->GetObj(i);
            const auto cls = TypesystemRepository::Instance().GetClass(object->GetTypeId());
            auto seqItem = std::make_unique<MemberTreeItem>(cls, object);
            seqItem->SetMemberInfo(m_member);
            seqItem->m_key = QString::number(count++);
            AddChild(std::move(seqItem));
        }
    }
    break;
    }

}

void MemberTreeItem::SetItemDictionaryValues(const Safir::Dob::Typesystem::ContainerBase& cb)
{
    using namespace Safir::Dob::Typesystem;

    m_value = QString("<dictionary keyType=%1>").arg(GetTypeName(m_member->keyType, m_member->keyTypeId));
    m_isContainerRoot = true;
    m_isChanged = cb.IsChanged();
    m_isNull = cb.IsNull();
    if (m_isNull)
    {
        return;
    }

    const auto dc = static_cast<const DictionaryContainerBase*>(&cb);

    for (size_t i = 0; i < dc->size(); ++i)
    {
        auto dictItem = std::make_unique<MemberTreeItem>(this, m_member);

        // ----------------
        // Handle key
        // ----------------
        switch (m_member->keyType)
        {
        case EnumerationMemberType:
        {
            auto ordinal = dc->GetKeyAt<Safir::Dob::Typesystem::EnumerationValue>(i);
            dictItem->m_key = QString::fromStdWString(Operations::GetEnumerationValueName(m_member->keyTypeId, ordinal));
        }
        break;

        case Int32MemberType:
        {
            auto val = dc->GetKeyAt<Int32>(i);
            dictItem->m_key = QString::number(val);
        }
        break;

        case Int64MemberType:
        {
            auto val = dc->GetKeyAt<Int64>(i);
            dictItem->m_key = QString::number(val);
        }
        break;

        case TypeIdMemberType:
        {
            auto val = dc->GetKeyAt<TypeId>(i);
            dictItem->m_key = TypeIdToString(val);
        }
        break;

        case InstanceIdMemberType:
        {
            auto val = dc->GetKeyAt<InstanceId>(i);
            dictItem->m_key = QString::fromStdWString(val.ToString());
        }
        break;

        case EntityIdMemberType:
        {
            auto val = dc->GetKeyAt<EntityId>(i);
            dictItem->m_key = ::Utilities::EntityIdToString(val);
        }
        break;

        case ChannelIdMemberType:
        {
            auto val = dc->GetKeyAt<ChannelId>(i);
            dictItem->m_key = QString::fromStdWString(val.ToString());
        }
        break;

        case HandlerIdMemberType:
        {
            auto val = dc->GetKeyAt<HandlerId>(i);
            dictItem->m_key = QString::fromStdWString(val.ToString());
        }
        break;

        case StringMemberType:
        {
            auto val = dc->GetKeyAt<std::wstring>(i);
            dictItem->m_key = QString::fromStdWString(val);
        }
        break;

        default:
            break;
        }

        // ----------------
        // Handle value
        // ----------------
        const auto& valCont = dc->GetValueContainerAt(i);
        //dictItem
        dictItem->m_isChanged = valCont.IsChanged();
        dictItem->m_isNull = valCont.IsNull();

        if (!dictItem->m_isNull)
        {
            switch (m_member->memberType)
            {
            case BooleanMemberType:
            {
                const auto c = static_cast<const BooleanContainer*>(&valCont);
                dictItem->m_value = c->GetVal() ? "true" : "false";
            }
            break;

            case EnumerationMemberType:
            {
                const auto c = static_cast<const EnumerationContainerBase*>(&valCont);
                dictItem->m_value = QString::fromStdWString(Operations::GetEnumerationValueName(m_member->memberTypeId, c->GetOrdinal()));
            }
            break;

            case Int32MemberType:
            {
                const auto c = static_cast<const Int32Container*>(&valCont);
                dictItem->m_value = QString::number(c->GetVal());
            }
            break;

            case Int64MemberType:
            {
                const auto c = static_cast<const Int64Container*>(&valCont);
                dictItem->m_value = QString::number(c->GetVal());
            }
            break;

            case Float32MemberType:
            {
                const auto c = static_cast<const Float32Container*>(&valCont);
                dictItem->m_value = QString::number(c->GetVal());
            }
            break;


            case Float64MemberType:
            {
                const auto c = static_cast<const Float64Container*>(&valCont);
                m_value = QString::number(c->GetVal());
            }
            break;

            case TypeIdMemberType:
            {
                const auto c = static_cast<const TypeIdContainer*>(&valCont);
                dictItem->m_value = TypeIdToString(c->GetVal());
            }
            break;

            case InstanceIdMemberType:
            {
                const auto c = static_cast<const InstanceIdContainer*>(&valCont);
                dictItem->m_value = QString::fromStdWString(c->GetVal().ToString());
            }
            break;

            case EntityIdMemberType:
            {
                const auto c = static_cast<const EntityIdContainer*>(&valCont);
                dictItem->m_value = ::Utilities::EntityIdToString(c->GetVal());
            }
            break;

            case ChannelIdMemberType:
            {
                const auto c = static_cast<const ChannelIdContainer*>(&valCont);
                dictItem->m_value = QString::fromStdWString(c->GetVal().ToString());
            }
            break;

            case HandlerIdMemberType:
            {
                const auto c = static_cast<const HandlerIdContainer*>(&valCont);
                dictItem->m_value = QString::fromStdWString(c->GetVal().ToString());
            }
            break;

            case StringMemberType:
            {
                const auto c = static_cast<const StringContainer*>(&valCont);
                dictItem->m_value = QString::fromStdWString(c->GetVal());
            }
            break;

            case BinaryMemberType:
            {
                const auto c = static_cast<const BinaryContainer*>(&valCont);
                auto b64 = Safir::Dob::Typesystem::Utilities::BinaryToBase64(c->GetVal());
                dictItem->m_value = QString::fromStdString(b64);
            }
            break;

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
            {
                const auto c = static_cast<const Float32Container*>(&valCont);
                dictItem->m_value = QString::number(c->GetVal());
            }
            break;

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
            {
                const auto c = static_cast<const Float64Container*>(&valCont);
                dictItem->m_value = QString::number(c->GetVal());
            }
            break;

            case ObjectMemberType:
            {
                const auto objCont = static_cast<const ObjectContainerBase*>(&valCont);
                auto object = objCont->GetObjectPointer();
                dictItem->m_cls = TypesystemRepository::Instance().GetClass(object->GetTypeId());
                dictItem->m_value = dictItem->m_cls->name;
                dictItem->SetupObject(object);
            }
            break;
            }
        }

        m_children.emplace_back(std::move(dictItem));

    } // end of loop


}

void MemberTreeItem::DebugPrint() const
{
    Print(const_cast<MemberTreeItem*>(this), "");
}
