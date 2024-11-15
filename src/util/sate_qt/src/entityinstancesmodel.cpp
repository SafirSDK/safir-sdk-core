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
#include "entityinstancesmodel.h"

#include <iostream>

#include <Safir/Dob/Typesystem/Members.h>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Dob/Entity.h>
#include <Safir/Time/TimeProvider.h>
#include <QSize>

namespace
{
    int ColumnWidth(const Safir::Dob::Typesystem::MemberType memberType)
    {
        switch(memberType)
        {
        case StringMemberType:
            return 150;
        case EntityIdMemberType:
            return 320;
        case TypeIdMemberType:
        case InstanceIdMemberType:
            return 150;
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


    template<class T, class U>
    QStringList SequenceToStrings(const Safir::Dob::Typesystem::ContainerBase& container, const U& transform)
    {
        QStringList result;
        std::transform(static_cast<const T&>(container).begin(),
                       static_cast<const T&>(container).end(),
                       std::back_inserter(result),
                       transform);
        return result;
    }

    template<class T>
    QStringList SequenceToStrings(const Safir::Dob::Typesystem::ContainerBase& container)
    {
        QStringList result;
        std::transform(static_cast<const T&>(container).begin(),
                       static_cast<const T&>(container).end(),
                       std::back_inserter(result),
                       [](const auto& v) {return QVariant(v).toString();});
        return result;
    }

    template<>
    QStringList SequenceToStrings<Safir::Dob::Typesystem::Int64SequenceContainer>(const Safir::Dob::Typesystem::ContainerBase& container)
    {
        QStringList result;
        std::transform(static_cast<const Safir::Dob::Typesystem::Int64SequenceContainer&>(container).begin(),
                       static_cast<const Safir::Dob::Typesystem::Int64SequenceContainer&>(container).end(),
                       std::back_inserter(result),
                       [](const auto& v) {return QVariant(static_cast<qint64>(v)).toString();});
        return result;
    }


}



EntityInstancesModel::EntityInstancesModel(DobInterface* dob,
                                           const Safir::Dob::Typesystem::TypeId typeId,
                                           bool includeSubclasses,
                                           QObject* parent)
    : QAbstractTableModel(parent)
    , m_dob(dob)
    , m_typeId(typeId)
    , m_includeSubclasses(includeSubclasses)
{
    setupColumns();
    connect(m_dob, &DobInterface::OnEntity, this, &EntityInstancesModel::OnEntity);
    m_dob->SubscribeEntity(m_typeId,Safir::Dob::Typesystem::InstanceId(), m_includeSubclasses);
}

EntityInstancesModel::~EntityInstancesModel()
{

}


int EntityInstancesModel::rowCount(const QModelIndex& /*parent*/) const
{
    return static_cast<int>(m_entities.size());
}

int EntityInstancesModel::columnCount(const QModelIndex& /*parent*/) const
{
    return m_columnInfoList.count();
}

QVariant EntityInstancesModel::headerData(const int section, const Qt::Orientation orientation, const int role) const
{
    if(orientation == Qt::Horizontal &&
       section >= 0 && section < m_columnInfoList.count())
    {
        ColumnInfoPtr columnInfo = m_columnInfoList.at(section);
        if(columnInfo)
        {
            switch (role)
            {
            case Qt::DisplayRole:
                return columnInfo->Name();
            case Qt::SizeHintRole:
                return QSize(columnInfo->Width(), 20);
            }
        }
    }

    return QVariant();
}

QVariant EntityInstancesModel::data(const QModelIndex& index, const int role) const
{
    using namespace Safir::Dob::Typesystem;

    if (role != Qt::DisplayRole && role != Qt::ToolTipRole)
    {
        return QVariant();
    }
    auto columnInfo = m_columnInfoList.at(index.column());

    const auto entity = std::next(m_entities.cbegin(), index.row());

    //Handle first column
    if (columnInfo->IsEntityIdColumn())
    {
        return static_cast<qlonglong>(entity->first.GetInstanceId().GetRawValue());
    }

    switch (columnInfo->CollectionType())
    {
    case SingleValueCollectionType:
        {
            const auto& container = entity->second->GetMember(columnInfo->MemberIndex(),0);
            if (container.IsNull())
            {
                return QVariant();
            }

            return ContainerToVariant(container, columnInfo->MemberType(), columnInfo->MemberTypeId());
        }

    case ArrayCollectionType:
        {
            QStringList result;
            for (int i = 0; i < columnInfo->ArrayLength(); ++i)
            {
                const auto& container = entity->second->GetMember(columnInfo->MemberIndex(),i);
                if (!container.IsNull())
                {
                    result += ContainerToVariant(container, columnInfo->MemberType(), columnInfo->MemberTypeId()).toString();
                }
            }
            if (result.empty())
            {
                return QVariant();
            }
            else
            {
                return "[" + result.join(", ") + "]";
            }
        }

    case SequenceCollectionType:
        {
            const auto& container = entity->second->GetMember(columnInfo->MemberIndex(),0);
            if (container.IsNull())
            {
                return QVariant();
            }
            return "[" + SequenceToStrings(container,
                                           columnInfo->MemberType(),
                                           columnInfo->MemberTypeId()).join(", ") + "]";
        }

    case DictionaryCollectionType:
        {
            const auto& container = entity->second->GetMember(columnInfo->MemberIndex(),0);
            if (container.IsNull())
            {
                return QVariant();
            }
            return "{" + DictionaryToStrings(static_cast<const DictionaryContainerBase&>(container),
                                             columnInfo->KeyType(),
                                             columnInfo->MemberType(),
                                             columnInfo->MemberTypeId(),
                                             columnInfo->KeyTypeId()).join(", ") + "}";
        }

    }
    throw std::logic_error("Unhandled CollectionType");
}


void EntityInstancesModel::setupColumns()
{
    if(!Safir::Dob::Typesystem::Operations::Exists(m_typeId) ||
       !Safir::Dob::Typesystem::Operations::IsOfType(m_typeId,Safir::Dob::Entity::ClassTypeId))
    {
        throw std::logic_error("Invalid type");
    }

    Safir::Dob::Typesystem::MemberType memberType;
    const char *memberName;
    Safir::Dob::Typesystem::TypeId memberTypeId;
    Safir::Dob::Typesystem::Int32 stringLength;
    Safir::Dob::Typesystem::Int32 arrayLength;
    Safir::Dob::Typesystem::MemberType keyType;
    Safir::Dob::Typesystem::TypeId keyTypeId;
    Safir::Dob::Typesystem::CollectionType collectionType;

    m_columnInfoList.append(ColumnInfo::Create(ColumnWidth(TypeIdMemberType),tr("InstanceId")));

    Safir::Dob::Typesystem::Int32 numberOfMembers = Safir::Dob::Typesystem::Members::GetNumberOfMembers(m_typeId);
    for(Safir::Dob::Typesystem::Int32 memberIndex = 0 ; memberIndex < numberOfMembers ; ++memberIndex)
    {
        Safir::Dob::Typesystem::Members::GetInfo(m_typeId,
                                                 memberIndex,
                                                 memberType,
                                                 keyType,
                                                 memberName,
                                                 memberTypeId,
                                                 keyTypeId,
                                                 stringLength,
                                                 collectionType,
                                                 arrayLength);

        const auto columnName = QString::fromUtf8(memberName);
        ColumnInfoPtr columnInfo = ColumnInfo::Create(ColumnWidth(memberType),columnName,m_typeId,memberIndex,memberType,keyType,memberTypeId,keyTypeId,collectionType,arrayLength);
        m_columnInfoList.append(columnInfo);
    }
}

void EntityInstancesModel::OnEntity(const sdt::EntityId& entityId,
                                    const sdt::HandlerId& /*handler*/,
                                    const Safir::Dob::EntityPtr& entity,
                                    const DobInterface::EntityOperation operation)
{
    switch(operation)
    {
    case DobInterface::NewEntity:
        {
            const auto result = m_entities.insert(std::make_pair(entityId, entity));
            const auto row = std::distance(m_entities.begin(), result.first);
            beginInsertRows(QModelIndex(), row, row);
            endInsertRows();
        }
        break;
    case DobInterface::UpdatedEntity:
        {
            const auto result = m_entities.insert_or_assign(entityId,
                                                            entity);
            const auto row = std::distance(m_entities.begin(), result.first);
            emit dataChanged(index(row,0), index(row, m_columnInfoList.count() - 1));
        }
        break;
    case DobInterface::DeletedEntity:
        {
            const auto row = std::distance(m_entities.begin(), m_entities.find(entityId));
            beginRemoveRows(QModelIndex(),row,row);
            m_entities.erase(entityId);
            endRemoveRows();
        }
        break;
    }
}


#if 0
void EntityInstancesModel::OnNewEntity(const Safir::Dob::EntityProxy entityProxy)
{
}

void EntityInstancesModel::OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy)
{
}

void EntityInstancesModel::OnDeletedEntity(const Safir::Dob::EntityProxy entityProxy, const bool /*deprecated*/)
{
}
#endif

void EntityInstancesModel::setSecond64Format(const Second64Format format)
{
    if (m_second64Format == format)
    {
        return;
    }

    beginResetModel();
    m_second64Format = format;
    endResetModel();
}


QVariant EntityInstancesModel::ContainerToVariant(const Safir::Dob::Typesystem::ContainerBase& container,
                                         const Safir::Dob::Typesystem::MemberType memberType,
                                         const Safir::Dob::Typesystem::TypeId memberTypeId) const
{
    using namespace Safir::Dob::Typesystem;
    switch (memberType)
    {
    case BooleanMemberType:
        return static_cast<const BooleanContainer&>(container).GetVal();
    case EnumerationMemberType:
        return QString::fromStdWString(Operations::GetEnumerationValueName
                                       (memberTypeId,
                                        static_cast<const EnumerationContainerBase&>(container).GetOrdinal()));
    case Int32MemberType:
        return static_cast<const Int32Container&>(container).GetVal();
    case Int64MemberType:
    case TypeIdMemberType:
        return static_cast<qlonglong>(static_cast<const Int64Container&>(container).GetVal());
    case InstanceIdMemberType:
        return static_cast<qlonglong>(static_cast<const InstanceIdContainer&>(container).GetVal().GetRawValue());
    case EntityIdMemberType:
        return QString::fromStdWString(static_cast<const EntityIdContainer&>(container).GetVal().ToString());
    case ChannelIdMemberType:
        return static_cast<qlonglong>(static_cast<const ChannelIdContainer&>(container).GetVal().GetRawValue());
    case HandlerIdMemberType:
        return static_cast<qlonglong>(static_cast<const HandlerIdContainer&>(container).GetVal().GetRawValue());
    case StringMemberType:
        return QString::fromStdWString(static_cast<const StringContainer&>(container).GetVal());
    case ObjectMemberType:
        return QString::fromStdWString(Serialization::ToJson
                                       (static_cast<const ObjectContainerBase&>(container).GetObjectPointer()));
    case BinaryMemberType:
        return QString::fromStdString(Utilities::BinaryToBase64
                                      (static_cast<const BinaryContainer&>(container).GetVal()));

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
        return static_cast<const Float32Container&>(container).GetVal();

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
        return static_cast<const Float64Container&>(container).GetVal();
    case Second64MemberType:
        return Second64ToVariant(static_cast<const Float64Container&>(container).GetVal());
    }
    throw std::logic_error("Unhandled MemberType");
}

QStringList EntityInstancesModel::SequenceToStrings(const Safir::Dob::Typesystem::ContainerBase& container,
                                           const Safir::Dob::Typesystem::MemberType memberType,
                                           const Safir::Dob::Typesystem::TypeId memberTypeId) const
{
    using namespace Safir::Dob::Typesystem;
    switch (memberType)
    {
    case BooleanMemberType:
        return ::SequenceToStrings<BooleanSequenceContainer>(container);
    case EnumerationMemberType:
        {
            const auto& enums = dynamic_cast<const EnumerationSequenceContainerBase&>(container);
            QStringList result;
            for (size_t i = 0; i < enums.size(); ++i)
            {
                result += QString::fromStdWString(Operations::GetEnumerationValueName(memberTypeId, enums.GetOrdinal(i)));
            }
            return result;
        }
    case Int32MemberType:
        return ::SequenceToStrings<Int32SequenceContainer>(container);
    case Int64MemberType:
    case TypeIdMemberType:
        return ::SequenceToStrings<Int64SequenceContainer>(container);
    case InstanceIdMemberType:
        return ::SequenceToStrings<InstanceIdSequenceContainer>(container,
                                                              [](const auto& v){return QString::fromStdWString(v.ToString());});
    case EntityIdMemberType:
        return ::SequenceToStrings<EntityIdSequenceContainer>(container,
                                                            [](const auto& v){return QString::fromStdWString(v.ToString());});
    case ChannelIdMemberType:
        return ::SequenceToStrings<ChannelIdSequenceContainer>(container,
                                                             [](const auto& v){return QString::fromStdWString(v.ToString());});
    case HandlerIdMemberType:
        return ::SequenceToStrings<HandlerIdSequenceContainer>(container,
                                                             [](const auto& v){return QString::fromStdWString(v.ToString());});
    case StringMemberType:
        return ::SequenceToStrings<StringSequenceContainer>(container,
                                                          [](const auto& v){return QString::fromStdWString(v);});
    case ObjectMemberType:
        {
            const auto& objects = dynamic_cast<const GenericObjectSequenceContainerBase&>(container);
            QStringList result;
            for (size_t i = 0; i < objects.size(); ++i)
            {
                result += QString::fromStdWString(Serialization::ToJson(objects.GetObjectPointer(i)));
            }
            return result;
        }

    case BinaryMemberType:
        return ::SequenceToStrings<BinarySequenceContainer>
            (container, [](const auto& v){return QString::fromStdString(Utilities::BinaryToBase64(v));});

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
        return ::SequenceToStrings<Float32SequenceContainer>(container);

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
        return ::SequenceToStrings<Float64SequenceContainer>(container);
    case Second64MemberType:
        return ::SequenceToStrings<Float64SequenceContainer>
            (container, [this](const auto& v){return Second64ToVariant(v).toString();});
    }
    throw std::logic_error("Unhandled MemberType");
}

QStringList EntityInstancesModel::DictionaryToStrings(const Safir::Dob::Typesystem::DictionaryContainerBase& container,
                                             const Safir::Dob::Typesystem::MemberType keyType,
                                             const Safir::Dob::Typesystem::MemberType memberType,
                                             const Safir::Dob::Typesystem::TypeId memberTypeId,
                                             const Safir::Dob::Typesystem::TypeId keyTypeId) const
{
    QStringList result;
    using namespace Safir::Dob::Typesystem;
    for (size_t i = 0; i < container.size(); ++i)
    {
        result.append("");
        switch (keyType)
        {
        case EnumerationMemberType:
            result.last() += QString::fromStdWString(Operations::GetEnumerationValueName(keyTypeId,
                                                                                         container.GetKeyAt<Int32>(i)));
            break;
        case Int32MemberType:
            result.last() += QString::number(container.GetKeyAt<Int32>(i));
            break;
        case Int64MemberType:
        case TypeIdMemberType:
            result.last() += QString::number(container.GetKeyAt<Int64>(i));
            break;
        case InstanceIdMemberType:
            result.last() += QString::fromStdWString(container.GetKeyAt<InstanceId>(i).ToString());
            break;
        case EntityIdMemberType:
            result.last() += QString::fromStdWString(container.GetKeyAt<EntityId>(i).ToString());
            break;
        case ChannelIdMemberType:
            result.last() += QString::fromStdWString(container.GetKeyAt<ChannelId>(i).ToString());
            break;
        case HandlerIdMemberType:
            result.last() += QString::fromStdWString(container.GetKeyAt<HandlerId>(i).ToString());
            break;
        case StringMemberType:
            result.last() += QString::fromStdWString(container.GetKeyAt<std::wstring>(i));
            break;

        default:
            result.last() += "Unsupported keyType";
        }

        auto& valueContainer = container.GetValueContainerAt(i);
        if (valueContainer.IsNull())
        {
            result.last() += ": null";
        }
        else
        {
            result.last() += ": " + ContainerToVariant(valueContainer, memberType, memberTypeId).toString();
        }
    }
    return result;
}


QVariant EntityInstancesModel::Second64ToVariant(const Safir::Dob::Typesystem::Si64::Second seconds) const
{
    switch (m_second64Format)
    {
    case FloatingPoint:
        return seconds;
    case LocalTime:
        return QString::fromStdString(boost::posix_time::to_iso_extended_string(Safir::Time::TimeProvider::ToLocalTime(seconds))).replace("T", " ");
    case UtcTime:
        return QString::fromStdString(boost::posix_time::to_iso_extended_string(Safir::Time::TimeProvider::ToPtime(seconds))).replace("T", " ");
    }
    throw std::logic_error("Invalid Second64 format");
}

