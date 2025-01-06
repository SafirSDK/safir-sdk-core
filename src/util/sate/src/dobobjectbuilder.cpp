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
#include "dobobjectbuilder.h"
#include "typesystemrepository.h"

#include <Safir/Dob/Typesystem/ObjectFactory.h>
#include <Safir/Dob/Typesystem/ValueContainers.h>
#include <Safir/Dob/Typesystem/ObjectContainer.h>
#include "Safir/Dob/Typesystem/DictionaryContainer.h"
#include "Safir/Dob/Typesystem/EnumerationContainerBase.h"
#include "Safir/Dob/Typesystem/SequenceContainer.h"

#include <QRegularExpression>

namespace sdt = Safir::Dob::Typesystem;

namespace
{
    sdt::TypeId ToTypeId(const QString& s)
    {
        bool ok;
        sdt::TypeId typeId = s.trimmed().toLongLong(&ok, 10);
        return ok ? typeId : sdt::Operations::GetTypeId(s.trimmed().toStdWString());
    }

    template <class T>
    T ToHashType(const QString& s)
    {
        bool ok;
        int64_t num = s.trimmed().toLongLong(&ok, 10);
        return ok ? T(num) : T(s.trimmed().toStdWString());
    }
}

// ----------------------------
// Static helpers
// ----------------------------
std::pair<bool, Safir::Dob::Typesystem::EntityId> DobObjectBuilder::EntityIdFromString(const QString& str)
{
    auto stringList = str.split(QRegularExpression("\\s+"), Qt::SkipEmptyParts);
    if (stringList.size() < 2)
    {
        return std::make_pair(false, sdt::EntityId());
    }

    sdt::EntityId eid(ToTypeId(stringList.at(0)), ToHashType<sdt::InstanceId>(stringList.at(1)));
    return std::make_pair(true, eid);
}

// ----------------------------
// Class DobObjectBuilder
// ----------------------------
Safir::Dob::Typesystem::ObjectPtr DobObjectBuilder::CreateObject(const MemberTreeItem* objectRoot) const
{
    if (objectRoot == nullptr || objectRoot->GetMemberClass() == nullptr || !objectRoot->IsObjectRootItem() || objectRoot->IsNull())
    {
        return nullptr;
    }

    auto cls = objectRoot->GetMemberClass();
    auto obj = sdt::ObjectFactory::Instance().CreateObject(cls->typeId);

    for (auto memberIndex = 0; memberIndex < cls->totalNumberOfMembers; ++memberIndex)
    {
        const auto& member = cls->GetMember(memberIndex);
        const auto memberItem = objectRoot->GetConstChildMember(memberIndex);

        switch (member.collectionType)
        {
        case SingleValueCollectionType:
        {
            auto& container = obj->GetMember(memberIndex, 0);
            SetSingleValue(memberItem, container);
        }
        break;

        case ArrayCollectionType:
        {
            for (auto arrIndex = 0; arrIndex < member.arrayLength; ++arrIndex)
            {
                auto& container = obj->GetMember(memberIndex, arrIndex);
                auto arrayMemberItem = memberItem->GetConstChildMember(arrIndex);
                SetSingleValue(arrayMemberItem, container);
            }
        }
        break;

        case SequenceCollectionType:
        {
            auto& container = obj->GetMember(memberIndex, 0);
            SetSequenceValues(memberItem, container);
        }
        break;

        case DictionaryCollectionType:
        {
            auto& container = obj->GetMember(memberIndex, 0);
            SetDictionaryValues(memberItem, container);
        }
        break;
        }
    }

    return obj;
}

void DobObjectBuilder::SetSingleValue(const MemberTreeItem* mi, Safir::Dob::Typesystem::ContainerBase& cb) const
{
    using namespace Safir::Dob::Typesystem;

    if (mi->IsNull())
    {
        cb.SetNull();
        cb.SetChanged(mi->IsChanged());
        return;
    }

    switch (mi->GetMemberInfo()->memberType)
    {
    case BooleanMemberType:
    {
        auto c = static_cast<BooleanContainer*>(&cb);
        c->SetVal(mi->GetValue().toLower() == "true");
    }
    break;

    case EnumerationMemberType:
    {
        auto c = static_cast<EnumerationContainerBase*>(&cb);
        auto ordinal = Operations::GetEnumerationValue(mi->GetMemberInfo()->memberTypeId, mi->GetValue().toStdWString());
        c->SetOrdinal(ordinal);
    }
    break;

    case Int32MemberType:
    {
        auto c = static_cast<Int32Container*>(&cb);
        c->SetVal(mi->GetValue().toInt());
    }
    break;

    case Int64MemberType:
    {
        auto c = static_cast<Int64Container*>(&cb);
        c->SetVal(mi->GetValue().toLongLong());
    }
    break;

    case Float32MemberType:
    {
        auto c = static_cast<Float32Container*>(&cb);
        c->SetVal(mi->GetValue().toFloat());
    }
    break;


    case Float64MemberType:
    {
        auto c = static_cast<Float64Container*>(&cb);
        c->SetVal(mi->GetValue().toDouble());
    }
    break;

    case TypeIdMemberType:
    {
        auto c = static_cast<TypeIdContainer*>(&cb);
        bool ok;
        sdt::TypeId typeId = mi->GetValue().toLongLong(&ok, 10);
        if (ok)
        {
            c->SetVal(typeId);
        }
        else
        {
            c->SetVal(Operations::GetTypeId(mi->GetValue().toStdWString()));
        }
    }
    break;

    case InstanceIdMemberType:
    {
        auto c = static_cast<InstanceIdContainer*>(&cb);
        auto val = ToHashType<sdt::InstanceId>(mi->GetValue());
        c->SetVal(val);
    }
    break;

    case EntityIdMemberType:
    {
        auto eid = EntityIdFromString(mi->GetValue());
        if (eid.first)
        {
            auto c = static_cast<EntityIdContainer*>(&cb);
            c->SetVal(eid.second);
        }
        else
        {
            cb.SetNull();
        }
    }
    break;

    case ChannelIdMemberType:
    {
        auto c = static_cast<ChannelIdContainer*>(&cb);
        auto val = ToHashType<sdt::ChannelId>(mi->GetValue());
        c->SetVal(val);
    }
    break;

    case HandlerIdMemberType:
    {
        auto c = static_cast<HandlerIdContainer*>(&cb);
        auto val = ToHashType<sdt::HandlerId>(mi->GetValue());
        c->SetVal(val);
    }
    break;

    case StringMemberType:
    {
        auto c = static_cast<StringContainer*>(&cb);
        c->SetVal(mi->GetValue().toStdWString());
    }
    break;

    case BinaryMemberType:
    {
        auto c = static_cast<BinaryContainer*>(&cb);
        Binary bin;
        Utilities::Base64ToBinary(mi->GetValue().toStdString(), bin);
        if (bin.empty())
        {
            auto byteArray = mi->GetValue().toUtf8().toBase64();
            bin = { byteArray.begin(), byteArray.end() };
        }
        c->SetVal(bin);
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
        auto c = static_cast<Float32Container*>(&cb);
        c->SetVal(mi->GetValue().toFloat());
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
        auto c = static_cast<Float64Container*>(&cb);
        c->SetVal(mi->GetValue().toDouble());
    }
    break;

    case ObjectMemberType:
    {
        auto objCont = static_cast<ObjectContainerBase*>(&cb);
        auto obj = CreateObject(mi);
        objCont->SetPtr(obj);

    }
    break;
    }

    cb.SetChanged(mi->IsChanged());
}

void DobObjectBuilder::SetSequenceValues(const MemberTreeItem* mi, Safir::Dob::Typesystem::ContainerBase& cb) const
{
    using namespace Safir::Dob::Typesystem;

    if (mi->IsNull())
    {
        cb.SetNull();
        cb.SetChanged(mi->IsChanged());
        return;
    }

    switch (mi->GetMemberInfo()->memberType)
    {
    case BooleanMemberType:
    {
        auto c = static_cast<BooleanSequenceContainer*>(&cb);
        for (int i = 0; i < mi->NumberOfChildMembers(); ++i)
        {
            const auto& m = mi->GetConstChildMember(i);
            c->push_back(m->GetValue().toLower() == "true");
        }
    }
    break;

    case EnumerationMemberType:
    {
        auto c = static_cast<EnumerationSequenceContainerBase*>(&cb);
        for (int i = 0; i < mi->NumberOfChildMembers(); ++i)
        {
            const auto& m = mi->GetConstChildMember(i);
            auto ordinal = Operations::GetEnumerationValue(m->GetMemberInfo()->memberTypeId, m->GetValue().toStdWString());
            c->PushBackOrdinal(ordinal);
        }
    }
    break;

    case Int32MemberType:
    {
        auto c = static_cast<Int32SequenceContainer*>(&cb);
        for (int i = 0; i < mi->NumberOfChildMembers(); ++i)
        {
            const auto& m = mi->GetConstChildMember(i);
            c->push_back(m->GetValue().toInt());
        }
    }
    break;

    case Int64MemberType:
    {
        auto c = static_cast<Int64SequenceContainer*>(&cb);
        for (int i = 0; i < mi->NumberOfChildMembers(); ++i)
        {
            const auto& m = mi->GetConstChildMember(i);
            c->push_back(m->GetValue().toLongLong());
        }
    }
    break;

    case Float32MemberType:
    {
        auto c = static_cast<Float32SequenceContainer*>(&cb);
        for (int i = 0; i < mi->NumberOfChildMembers(); ++i)
        {
            const auto& m = mi->GetConstChildMember(i);
            c->push_back(m->GetValue().toFloat());
        }
    }
    break;


    case Float64MemberType:
    {
        auto c = static_cast<Float64SequenceContainer*>(&cb);
        for (int i = 0; i < mi->NumberOfChildMembers(); ++i)
        {
            const auto& m = mi->GetConstChildMember(i);
            c->push_back(m->GetValue().toDouble());
        }
    }
    break;

    case TypeIdMemberType:
    {
        auto c = static_cast<TypeIdSequenceContainer*>(&cb);
        for (int i = 0; i < mi->NumberOfChildMembers(); ++i)
        {
            const auto& m = mi->GetConstChildMember(i);

            bool ok;
            sdt::TypeId typeId = m->GetValue().toLongLong(&ok, 10);
            if (ok)
            {
                c->push_back(typeId);
            }
            else
            {
                c->push_back(Operations::GetTypeId(m->GetValue().toStdWString()));
            }
        }
    }
    break;

    case InstanceIdMemberType:
    {
        auto c = static_cast<InstanceIdSequenceContainer*>(&cb);
        for (int i = 0; i < mi->NumberOfChildMembers(); ++i)
        {
            const auto& m = mi->GetConstChildMember(i);
            auto val = ToHashType<sdt::InstanceId>(m->GetValue());
            c->push_back(val);
        }
    }
    break;

    case EntityIdMemberType:
    {
        auto c = static_cast<EntityIdSequenceContainer*>(&cb);
        for (int i = 0; i < mi->NumberOfChildMembers(); ++i)
        {
            const auto& m = mi->GetConstChildMember(i);
            auto eid = EntityIdFromString(m->GetValue());
            if (eid.first)
            {
                c->push_back(eid.second);
            }
        }
    }
    break;

    case ChannelIdMemberType:
    {
        auto c = static_cast<ChannelIdSequenceContainer*>(&cb);
        for (int i = 0; i < mi->NumberOfChildMembers(); ++i)
        {
            const auto& m = mi->GetConstChildMember(i);
            auto val = ToHashType<sdt::ChannelId>(m->GetValue());
            c->push_back(val);
        }
    }
    break;

    case HandlerIdMemberType:
    {
        auto c = static_cast<HandlerIdSequenceContainer*>(&cb);
        for (int i = 0; i < mi->NumberOfChildMembers(); ++i)
        {
            const auto& m = mi->GetConstChildMember(i);
            auto val = ToHashType<sdt::HandlerId>(m->GetValue());
            c->push_back(val);
        }
    }
    break;

    case StringMemberType:
    {
        auto c = static_cast<StringSequenceContainer*>(&cb);
        for (int i = 0; i < mi->NumberOfChildMembers(); ++i)
        {
            const auto& m = mi->GetConstChildMember(i);
            c->push_back(m->GetValue().toStdWString());
        }
    }
    break;

    case BinaryMemberType:
    {
        auto c = static_cast<BinarySequenceContainer*>(&cb);
        for (int i = 0; i < mi->NumberOfChildMembers(); ++i)
        {
            const auto& m = mi->GetConstChildMember(i);
            Binary bin;
            Utilities::Base64ToBinary(m->GetValue().toStdString(), bin);
            if (bin.empty())
            {
                auto byteArray = m->GetValue().toUtf8().toBase64();
                bin = { byteArray.begin(), byteArray.end() };
            }
            c->push_back(bin);
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
        auto c = static_cast<Float32SequenceContainer*>(&cb);
        for (int i = 0; i < mi->NumberOfChildMembers(); ++i)
        {
            const auto& m = mi->GetConstChildMember(i);
            c->push_back(m->GetValue().toFloat());
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
        auto c = static_cast<Float64SequenceContainer*>(&cb);
        for (int i = 0; i < mi->NumberOfChildMembers(); ++i)
        {
            const auto& m = mi->GetConstChildMember(i);
            c->push_back(m->GetValue().toDouble());
        }
    }
    break;

    case ObjectMemberType:
    {
        auto objCont = dynamic_cast<GenericObjectSequenceContainerBase*>(&cb);
        for (int i = 0; i < mi->NumberOfChildMembers(); ++i)
        {
            const auto& m = mi->GetConstChildMember(i);
            auto obj = CreateObject(m);
            objCont->PushBackObjectPointer(obj);
        }
    }
    break;
    }

    cb.SetChanged(mi->IsChanged());
}

void DobObjectBuilder::SetDictionaryValues(const MemberTreeItem* mi, Safir::Dob::Typesystem::ContainerBase& cb) const
{
    using namespace Safir::Dob::Typesystem;

    if (mi->IsNull())
    {
        cb.SetNull();
        cb.SetChanged(mi->IsChanged());
        return;
    }

    auto dc = static_cast<DictionaryContainerBase*>(&cb);

    switch (mi->GetMemberInfo()->keyType)
    {
    case EnumerationMemberType:
    {
        for (int i = 0; i < mi->NumberOfChildMembers(); ++i)
        {
            const auto& m = mi->GetConstChildMember(i);
            auto key = Operations::GetEnumerationValue(m->GetMemberInfo()->keyTypeId, m->GetKey().toStdWString());
            auto& c = dc->InsertNull(key);
            SetSingleValue(m, c);
        }
    }
    break;

    case Int32MemberType:
    {
        for (int i = 0; i < mi->NumberOfChildMembers(); ++i)
        {
            const auto& m = mi->GetConstChildMember(i);
            auto key = m->GetKey().toInt();
            auto& c = dc->InsertNull(key);
            SetSingleValue(m, c);
        }
    }
    break;

    case Int64MemberType:
    {
        for (int i = 0; i < mi->NumberOfChildMembers(); ++i)
        {
            const auto& m = mi->GetConstChildMember(i);
            auto key = m->GetKey().toLongLong();
            auto& c = dc->InsertNull(key);
            SetSingleValue(m, c);
        }
    }
    break;

    case TypeIdMemberType:
    {
        for (int i = 0; i < mi->NumberOfChildMembers(); ++i)
        {
            const auto& m = mi->GetConstChildMember(i);
            bool ok;
            sdt::TypeId key = m->GetKey().toLongLong(&ok, 10);
            if (!ok)
            {
                key = Operations::GetTypeId(m->GetValue().toStdWString());
            }
            auto& c = dc->InsertNull(key);
            SetSingleValue(m, c);
        }
    }
    break;

    case InstanceIdMemberType:
    {
        for (int i = 0; i < mi->NumberOfChildMembers(); ++i)
        {
            const auto& m = mi->GetConstChildMember(i);
            auto key = ToHashType<sdt::InstanceId>(m->GetKey());
            auto& c = dc->InsertNull(key);
            SetSingleValue(m, c);
        }
    }
    break;

    case EntityIdMemberType:
    {
        for (int i = 0; i < mi->NumberOfChildMembers(); ++i)
        {
            const auto& m = mi->GetConstChildMember(i);
            auto key = EntityIdFromString(m->GetKey());
            if (key.first)
            {
                auto& c = dc->InsertNull(key.second);
                SetSingleValue(m, c);
            }
        }
    }
    break;

    case ChannelIdMemberType:
    {
        for (int i = 0; i < mi->NumberOfChildMembers(); ++i)
        {
            const auto& m = mi->GetConstChildMember(i);
            auto key = ToHashType<sdt::ChannelId>(m->GetKey());
            auto& c = dc->InsertNull(key);
            SetSingleValue(m, c);
        }
    }
    break;

    case HandlerIdMemberType:
    {
        for (int i = 0; i < mi->NumberOfChildMembers(); ++i)
        {
            const auto& m = mi->GetConstChildMember(i);
            auto key = ToHashType<sdt::HandlerId>(m->GetKey());
            auto& c = dc->InsertNull(key);
            SetSingleValue(m, c);
        }
    }
    break;

    case StringMemberType:
    {
        for (int i = 0; i < mi->NumberOfChildMembers(); ++i)
        {
            const auto& m = mi->GetConstChildMember(i);
            auto key = m->GetKey().toStdWString();
            auto& c = dc->InsertNull(key);
            SetSingleValue(m, c);
        }
    }
    break;

    default:
        break;
    }

    cb.SetChanged(mi->IsChanged());
}
