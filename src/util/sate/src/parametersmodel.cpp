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
#include "parametersmodel.h"
#include <QColor>
#include <QFont>
#include <QDateTime>
#include <Safir/Dob/Typesystem/Parameters.h>
#include <Safir/Dob/Typesystem/ToolSupport/Internal/BasicTypeOperations.h>
#include <memory>

namespace sdt = Safir::Dob::Typesystem;

namespace {
static const int NumberOfColumns = 3;

QString Str(const std::wstring& s) { return QString::fromStdWString(s); }
}

ParametersModel::ParametersModel(int64_t typeId, QObject *parent)
    : QAbstractItemModel(parent)
    , m_typeId(typeId)
{
    SetupModel();
}

QVariant ParametersModel::headerData(int section, Qt::Orientation orientation, int role) const
{
    if (section < NumberOfColumns && orientation == Qt::Horizontal && role == Qt::DisplayRole)
    {
        switch (section)
        {
        case 0: return "Name";
        case 1: return "Value";
        case 2: return "Type";
        }
    }
    return {};
}

QModelIndex ParametersModel::index(int row, int column, const QModelIndex &parent) const
{
    if (row < 0 || column < 0 || column >= NumberOfColumns)
    {
        return{}; // invalid index
    }

    if (!parent.isValid() || parent.internalPointer() == m_invisibleRootItem.get())
    {
        // Root item
        if (row < m_invisibleRootItem->NumberOfChildMembers())
        {
            return createIndex(row, column, m_invisibleRootItem->GetChildMember(row));
        }
        return{}; // invalid index
    }

    // Not the root, i.e nested objects or a container member (array/seq/dict)
    auto childItem = static_cast<MemberTreeItem*>(parent.internalPointer())->GetChildMember(row);
    if (childItem != nullptr)
    {
        return createIndex(row, column, childItem);
    }

    return{};
}

QModelIndex ParametersModel::parent(const QModelIndex &index) const
{
    if (!index.isValid() || index.internalPointer() == m_invisibleRootItem.get())
    {
        return{};
    }

    auto childItem = static_cast<MemberTreeItem*>(index.internalPointer());
    auto parentItem = childItem->GetParentMember();

    if (parentItem != m_invisibleRootItem.get() && parentItem != nullptr)
    {
        return createIndex(parentItem->RowNumber(), 0, parentItem);

    }

    return {};
}

int ParametersModel::rowCount(const QModelIndex &parent) const
{
    const MemberTreeItem* parentItem = parent.isValid()
                                           ? static_cast<const MemberTreeItem*>(parent.internalPointer())
                                           : m_invisibleRootItem.get();

    return parentItem->NumberOfChildMembers();
}

int ParametersModel::columnCount(const QModelIndex &/*parent*/) const
{
    return NumberOfColumns;
}

QVariant ParametersModel::data(const QModelIndex &index, int role) const
{
    if (!index.isValid())
    {
        return {};
    }

    switch (role)
    {
    case Qt::DisplayRole:
    {
        auto item = static_cast<const MemberTreeItem*>(index.internalPointer());
        switch (index.column())
        {
        case 0: return item->GetName();
        case 1: return item->GetValue();
        case 2: return TypesystemRepository::Instance().GetTypeName(item->GetMemberInfo()->memberType, item->GetMemberInfo()->memberTypeId);
        }
    }
    break;

    case ParametersModel::FilterRole:
    {
        auto item = static_cast<const MemberTreeItem*>(index.internalPointer());
        switch (index.column())
        {
        case 0: return item->GetName();
        case 1: return item->GetValue();
        case 2: return TypesystemRepository::Instance().GetTypeName(item->GetMemberInfo()->memberType, item->GetMemberInfo()->memberTypeId);
        }
    }
    break;

    case ParametersModel::InternalDataRole:
    {
        return QVariant::fromValue(index.internalPointer());
    }
    break;

    case Qt::ForegroundRole:
    {
        if (index.column() == 0)
        {
            auto item = static_cast<const MemberTreeItem*>(index.internalPointer());
            auto isContainer = item->GetMemberInfo()->collectionType != SingleValueCollectionType;

            // Blue color if item name is a key
            return (isContainer && !item->IsContainerRootItem()) ? QColor(116, 192, 252) :QVariant{};
        }
        else if (index.column() == 1)
        {
            auto item = static_cast<const MemberTreeItem*>(index.internalPointer());
            if (item->IsContainerRootItem())
            {
                for (int i = 0; i < item->NumberOfChildMembers(); ++i)
                {
                    if (!item->GetConstChildMember(i)->IsNull())
                    {
                        return QColor(116, 192, 252);
                    }
                }
            }

            return item->IsNull() ? QColor(135, 134, 132) : QColor(250, 185, 0);
        }
    }
    break;

    case Qt::FontRole:
    {
        if (index.column() == 1)
        {
            auto item = static_cast<const MemberTreeItem*>(index.internalPointer());
            if (item->IsContainerRootItem() || item->IsObjectRootItem())
            {
                QFont font;
                font.setBold(true);
                return font;
            }
        }
    }
    break;

    case Qt::ToolTipRole:
    {
        auto item = static_cast<const MemberTreeItem*>(index.internalPointer());

        if (!item->IsNull() && !item->IsContainerRootItem())
        {
            auto mt = item->GetMemberInfo()->memberType;
            if (mt == Second64MemberType || mt == Second32MemberType)
            {
                bool ok;
                auto seconds = item->GetValue().toDouble(&ok);
                if (ok)
                {
                    auto  dt = QDateTime::fromSecsSinceEpoch(seconds, Qt::UTC);
                    return QString("GMT: %1\nLocal: %2").arg(dt.toString("yyyy-MM-dd hh:mm::ss"), dt.toLocalTime().toString("yyyy-MM-dd hh:mm::ss"));
                }
            }
        }
    }
    break;

    }

    return {};
}

void ParametersModel::SetupModel()
{
    TypesystemRepository::DobMember rootM;
    rootM.name = "root";
    m_parameterMembers.push_back(rootM);
    m_invisibleRootItem = std::make_unique<MemberTreeItem>(nullptr, &m_parameterMembers.back());

    sdt::MemberType parameterType;
    sdt::MemberType keyType;
    std::wstring parameterName;
    sdt::TypeId parameterTypeId;
    sdt::TypeId keyTypeId;
    sdt::CollectionType collectionType;
    sdt::Int32 numberOfValues;

    auto num = sdt::Parameters::GetNumberOfParameters(m_typeId);
    for (int parameterIndex = 0; parameterIndex < num; ++parameterIndex)
    {
        sdt::Parameters::GetInfo(m_typeId, parameterIndex, parameterType, keyType, parameterName, parameterTypeId, keyTypeId, collectionType, numberOfValues);

        auto name = QString::fromStdWString(parameterName);
        if (name.contains('@'))
        {
            continue; // hidden parameter
        }

        TypesystemRepository::DobMember m;
        m.name = QString::fromStdWString(parameterName);
        m.memberType = parameterType;
        m.memberTypeId = parameterTypeId;
        m.keyType = keyType;
        m.keyTypeId = keyTypeId;
        m.collectionType = collectionType;
        m.arrayLength = numberOfValues;
        m_parameterMembers.push_back(m);
        auto& memberRef = m_parameterMembers.back();

        switch (m.collectionType)
        {
        case SingleValueCollectionType:
        {
            CreateItem(memberRef, parameterIndex, 0, m_invisibleRootItem.get());
        }
        break;

        case ArrayCollectionType:
        {
            auto arrayRoot = std::make_unique<MemberTreeItem>(m_invisibleRootItem.get(), &memberRef);
            arrayRoot->SetIsContainerRootItem("<array>");
            for (int arrIx = 0; arrIx < m.arrayLength; ++arrIx)
            {
                CreateItem(memberRef, parameterIndex, arrIx, arrayRoot.get());
            }
            m_invisibleRootItem->AddChild(std::move(arrayRoot));
        }
        break;

        case SequenceCollectionType:
        {
            auto seqRoot = std::make_unique<MemberTreeItem>(m_invisibleRootItem.get(), &memberRef);
            seqRoot->SetIsContainerRootItem("<sequence>");
            for (int arrIx = 0; arrIx < m.arrayLength; ++arrIx)
            {
                CreateItem(memberRef, parameterIndex, arrIx, seqRoot.get());
            }
            m_invisibleRootItem->AddChild(std::move(seqRoot));
        }
        break;

        case DictionaryCollectionType:
        {
            using namespace Safir::Dob::Typesystem::ToolSupport::Internal::BasicTypeOperations;
            auto dictRoot = std::make_unique<MemberTreeItem>(m_invisibleRootItem.get(), &memberRef);
            auto keyTypeName = IsBasicMemberType(m.keyType) ? QString::fromStdString(MemberTypeToString(m.keyType)) : QString::fromStdWString(Safir::Dob::Typesystem::Operations::GetName(m.keyTypeId));
            dictRoot->SetIsContainerRootItem(QString("<dictionary keyType=%1>").arg(keyTypeName));
            for (int arrIx = 0; arrIx < m.arrayLength; ++arrIx)
            {
                CreateItem(memberRef, parameterIndex, arrIx, dictRoot.get());
            }
            m_invisibleRootItem->AddChild(std::move(dictRoot));
        }
        break;
        }
    }
}

void ParametersModel::CreateItem(const TypesystemRepository::DobMember& memberInfo, int parameterIndex, int arrayIndex, MemberTreeItem* parent) const
{
    std::unique_ptr<MemberTreeItem> mi;

    switch (memberInfo.memberType)
    {
    case BooleanMemberType:
    {
        mi = std::make_unique<MemberTreeItem>(parent, &memberInfo);
        auto val = sdt::Parameters::GetBoolean(m_typeId, parameterIndex, arrayIndex);
        mi->SetValue(val ? "true" : "false");
    }
    break;

    case EnumerationMemberType:
    {
        mi = std::make_unique<MemberTreeItem>(parent, &memberInfo);
        auto ordinal = sdt::Parameters::GetEnumeration(m_typeId, parameterIndex, arrayIndex);
        auto val = sdt::Operations::GetEnumerationValueName(memberInfo.memberTypeId, ordinal);
        mi->SetValue(Str(val));
    }
    break;

    case Int32MemberType:
    {
        mi = std::make_unique<MemberTreeItem>(parent, &memberInfo);
        auto val = sdt::Parameters::GetInt32(m_typeId, parameterIndex, arrayIndex);
        mi->SetValue(QString::number(val));
    }
    break;

    case Int64MemberType:
    {
        mi = std::make_unique<MemberTreeItem>(parent, &memberInfo);
        auto val = sdt::Parameters::GetInt64(m_typeId, parameterIndex, arrayIndex);
        mi->SetValue(QString::number(val));
    }
    break;

    case Float32MemberType:
    {
        mi = std::make_unique<MemberTreeItem>(parent, &memberInfo);
        auto val = sdt::Parameters::GetFloat32(m_typeId, parameterIndex, arrayIndex);
        mi->SetValue(QString::number(val));
    }
    break;


    case Float64MemberType:
    {
        mi = std::make_unique<MemberTreeItem>(parent, &memberInfo);
        auto val = sdt::Parameters::GetFloat64(m_typeId, parameterIndex, arrayIndex);
        mi->SetValue(QString::number(val));
    }
    break;

    case TypeIdMemberType:
    {
        mi = std::make_unique<MemberTreeItem>(parent, &memberInfo);
        auto val = sdt::Parameters::GetTypeId(m_typeId, parameterIndex, arrayIndex);
        mi->SetValue(Str(sdt::Operations::GetName(val)));
    }
    break;

    case InstanceIdMemberType:
    {
        mi = std::make_unique<MemberTreeItem>(parent, &memberInfo);
        auto val = sdt::Parameters::GetInstanceId(m_typeId, parameterIndex, arrayIndex);
        mi->SetValue(Str(val.ToString()));
    }
    break;

    case EntityIdMemberType:
    {
        mi = std::make_unique<MemberTreeItem>(parent, &memberInfo);
        auto val = sdt::Parameters::GetEntityId(m_typeId, parameterIndex, arrayIndex);
        auto eidStr = QString("%1 : %2").arg(Str(sdt::Operations::GetName(val.GetTypeId())), Str(val.GetInstanceId().ToString()));
        mi->SetValue(eidStr);
    }
    break;

    case ChannelIdMemberType:
    {
        mi = std::make_unique<MemberTreeItem>(parent, &memberInfo);
        auto val = sdt::Parameters::GetChannelId(m_typeId, parameterIndex, arrayIndex);
        mi->SetValue(Str(val.ToString()));
    }
    break;

    case HandlerIdMemberType:
    {
        mi = std::make_unique<MemberTreeItem>(parent, &memberInfo);
        auto val = sdt::Parameters::GetHandlerId(m_typeId, parameterIndex, arrayIndex);
        mi->SetValue(Str(val.ToString()));
    }
    break;

    case StringMemberType:
    {
        mi = std::make_unique<MemberTreeItem>(parent, &memberInfo);
        auto val = sdt::Parameters::GetString(m_typeId, parameterIndex, arrayIndex);
        mi->SetValue(Str(val));
    }
    break;

    case BinaryMemberType:
    {
        mi = std::make_unique<MemberTreeItem>(parent, &memberInfo);
        auto val = sdt::Parameters::GetBinary(m_typeId, parameterIndex, arrayIndex);
        mi->SetValue(QString::fromStdString(sdt::Utilities::BinaryToBase64(val)));
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
        mi = std::make_unique<MemberTreeItem>(parent, &memberInfo);
        auto val = sdt::Parameters::GetFloat32(m_typeId, parameterIndex, arrayIndex);
        mi->SetValue(QString::number(val));
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
        mi = std::make_unique<MemberTreeItem>(parent, &memberInfo);
        auto val = sdt::Parameters::GetFloat64(m_typeId, parameterIndex, arrayIndex);
        mi->SetValue(QString::number(val));
    }
    break;

    case ObjectMemberType:
    {
        auto obj = sdt::Parameters::GetObject(m_typeId, parameterIndex, arrayIndex);
        auto dc = TypesystemRepository::Instance().GetClass(memberInfo.memberTypeId);
        mi = std::make_unique<MemberTreeItem>(dc, obj);
        mi->SetMemberInfo(&memberInfo);
    }
        break;
    }

    // Handle keys
    if (memberInfo.collectionType == ArrayCollectionType || memberInfo.collectionType == SequenceCollectionType)
    {
        mi->SetKey(QString::number(arrayIndex));
    }
    else if (memberInfo.collectionType == DictionaryCollectionType)
    {
        switch (memberInfo.keyType)
        {
        case EnumerationMemberType:
        {
            auto ordinal = sdt::Parameters::GetEnumerationDictionaryKey(m_typeId, parameterIndex, arrayIndex);
            auto val = sdt::Operations::GetEnumerationValueName(memberInfo.keyTypeId, ordinal);
            mi->SetKey(Str(val));
        }
        break;

        case Int32MemberType:
        {
            auto val = sdt::Parameters::GetInt32DictionaryKey(m_typeId, parameterIndex, arrayIndex);
            mi->SetKey(QString::number(val));
        }
        break;

        case Int64MemberType:
        {
            auto val = sdt::Parameters::GetInt64DictionaryKey(m_typeId, parameterIndex, arrayIndex);
            mi->SetKey(QString::number(val));
        }
        break;

        case TypeIdMemberType:
        {
            auto val = sdt::Parameters::GetTypeIdDictionaryKey(m_typeId, parameterIndex, arrayIndex);
            mi->SetKey(Str(sdt::Operations::GetName(val)));
        }
        break;

        case InstanceIdMemberType:
        {
            auto val = sdt::Parameters::GetInstanceIdDictionaryKey(m_typeId, parameterIndex, arrayIndex);
            mi->SetKey(Str(val.ToString()));
        }
        break;

        case EntityIdMemberType:
        {
            auto val = sdt::Parameters::GetEntityIdDictionaryKey(m_typeId, parameterIndex, arrayIndex);
            auto eidStr = QString("%1 : %2").arg(Str(sdt::Operations::GetName(val.GetTypeId())), Str(val.GetInstanceId().ToString()));
            mi->SetKey(eidStr);
        }
        break;

        case ChannelIdMemberType:
        {
            auto val = sdt::Parameters::GetChannelIdDictionaryKey(m_typeId, parameterIndex, arrayIndex);
            mi->SetKey(Str(val.ToString()));
        }
        break;

        case HandlerIdMemberType:
        {
            auto val = sdt::Parameters::GetHandlerIdDictionaryKey(m_typeId, parameterIndex, arrayIndex);
            mi->SetKey(Str(val.ToString()));
        }
        break;

        case StringMemberType:
        {
            auto val = sdt::Parameters::GetStringDictionaryKey(m_typeId, parameterIndex, arrayIndex);
            mi->SetKey(Str(val));
        }
        break;

        default:
            break;
        }
    }

    parent->AddChild(std::move(mi));
}
