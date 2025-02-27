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
#include "instancesmodelutils.h"

#include "utilities.h"
#include <Safir/Dob/Typesystem/ValueContainers.h>
#include <Safir/Dob/Typesystem/EnumerationContainerBase.h>
#include <Safir/Dob/Typesystem/SequenceContainer.h>
#include <Safir/Dob/Typesystem/DictionaryContainer.h>
#include <Safir/Dob/Typesystem/Members.h>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Time/TimeProvider.h>

#include <QColor>

namespace
{

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


QVariant InstancesModelUtils::ContainerToVariant(const Safir::Dob::Typesystem::ContainerBase& container,
                                                 const Safir::Dob::Typesystem::MemberType memberType,
                                                 const Safir::Dob::Typesystem::TypeId memberTypeId,
                                                 const int role)
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
        return static_cast<qlonglong>(static_cast<const Int64Container&>(container).GetVal());
    case TypeIdMemberType:
        try
        {
            return QString::fromStdWString(Safir::Dob::Typesystem::Operations::GetName
                                           (static_cast<const TypeIdContainer&>(container).GetVal()));
        }
        catch(const Safir::Dob::Typesystem::IllegalValueException&)
        {
            return static_cast<qlonglong>(static_cast<const Int64Container&>(container).GetVal());
        }

    case InstanceIdMemberType:
        return static_cast<qlonglong>(static_cast<const InstanceIdContainer&>(container).GetVal().GetRawValue());
    case EntityIdMemberType:
        return ::Utilities::EntityIdToString(static_cast<const EntityIdContainer&>(container).GetVal());
    case ChannelIdMemberType:
        return static_cast<qlonglong>(static_cast<const ChannelIdContainer&>(container).GetVal().GetRawValue());
    case HandlerIdMemberType:
        return static_cast<qlonglong>(static_cast<const HandlerIdContainer&>(container).GetVal().GetRawValue());
    case StringMemberType:
        return QString::fromStdWString(static_cast<const StringContainer&>(container).GetVal());
    case ObjectMemberType:
        if (role == Qt::DisplayRole)
        {
            return "<object>";
        }
        return QString::fromStdWString(Serialization::ToJson
                                       (static_cast<const ObjectContainerBase&>(container).GetObjectPointer()));
    case BinaryMemberType:
        return QString::fromStdString(Safir::Dob::Typesystem::Utilities::BinaryToBase64
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
    case SquareMeter32MemberType:
    case Steradian32MemberType:
    case Volt32MemberType:
    case Watt32MemberType:
        return static_cast<const Float32Container&>(container).GetVal();

    case Second32MemberType:
        return Second64ToVariant(static_cast<const Float64Container&>(container).GetVal(), role);

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
        return Second64ToVariant(static_cast<const Float64Container&>(container).GetVal(), role);
    }
    throw std::logic_error("Unhandled MemberType");
}

QStringList InstancesModelUtils::SequenceToStrings(const Safir::Dob::Typesystem::ContainerBase& container,
                                                    const Safir::Dob::Typesystem::MemberType memberType,
                                                    const Safir::Dob::Typesystem::TypeId memberTypeId)
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
        return ::SequenceToStrings<Int64SequenceContainer>(container);
    case TypeIdMemberType:
        return ::SequenceToStrings<TypeIdSequenceContainer>(container,[](const TypeId v)
        {
            try
            {
                return QString::fromStdWString(Safir::Dob::Typesystem::Operations::GetName(v));
            }
            catch(const Safir::Dob::Typesystem::IllegalValueException&)
            {
                return QVariant(static_cast<qint64>(v)).toString();
            }
        });

    case InstanceIdMemberType:
        return ::SequenceToStrings<InstanceIdSequenceContainer>(container,
                                                              [](const auto& v){return QString::fromStdWString(v.ToString());});
    case EntityIdMemberType:
        return ::SequenceToStrings<EntityIdSequenceContainer>(container,
                                                              [](const auto& v){return ::Utilities::EntityIdToString(v);});
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
            (container, [](const auto& v){return QString::fromStdString(Safir::Dob::Typesystem::Utilities::BinaryToBase64(v));});

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
            (container, [](const auto& v){return Second64ToVariant(v, Qt::DisplayRole).toString();});
    }
    throw std::logic_error("Unhandled MemberType");
}

QStringList InstancesModelUtils::DictionaryToStrings(const Safir::Dob::Typesystem::DictionaryContainerBase& container,
                                                     const Safir::Dob::Typesystem::MemberType keyType,
                                                     const Safir::Dob::Typesystem::MemberType memberType,
                                                     const Safir::Dob::Typesystem::TypeId memberTypeId,
                                                     const Safir::Dob::Typesystem::TypeId keyTypeId)
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
            result.last() += ::Utilities::EntityIdToString(container.GetKeyAt<EntityId>(i));
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
            result.last() += ": " + ContainerToVariant(valueContainer, memberType, memberTypeId, Qt::DisplayRole).toString();
        }
    }
    return result;
}


QVariant InstancesModelUtils::Second64ToVariant(const Safir::Dob::Typesystem::Si64::Second seconds, const int role)
{
    if (role == Qt::ToolTipRole)
    {
        return QString("Seconds:\t%1\n"
                       "UTC:\t%2\n"
                       "Local:\t%3")
            .arg(seconds)
            .arg(QString::fromStdString(boost::posix_time::to_iso_extended_string(Safir::Time::TimeProvider::ToPtime(seconds))).replace("T", " "))
            .arg(QString::fromStdString(boost::posix_time::to_iso_extended_string(Safir::Time::TimeProvider::ToLocalTime(seconds))).replace("T", " "));
    }
    else
    {
        const auto time = Safir::Time::TimeProvider::ToPtime(seconds);
        if (time.date().year() > 2015 && time.date().year() < 2100)
        {
            return QString::fromStdString(boost::posix_time::to_iso_extended_string(time)).replace("T", " ");
        }
        else
        {
            return seconds;
        }
    }
}


void InstancesModelUtils::AddMembers(const int64_t typeId, ColumnInfoList& columnInfoList)
{
    Safir::Dob::Typesystem::MemberType memberType;
    const char *memberName;
    Safir::Dob::Typesystem::TypeId memberTypeId;
    Safir::Dob::Typesystem::Int32 stringLength;
    Safir::Dob::Typesystem::Int32 arrayLength;
    Safir::Dob::Typesystem::MemberType keyType;
    Safir::Dob::Typesystem::TypeId keyTypeId;
    Safir::Dob::Typesystem::CollectionType collectionType;

    Safir::Dob::Typesystem::Int32 numberOfMembers = Safir::Dob::Typesystem::Members::GetNumberOfMembers(typeId);
    for(Safir::Dob::Typesystem::Int32 memberIndex = 0 ; memberIndex < numberOfMembers ; ++memberIndex)
    {
        Safir::Dob::Typesystem::Members::GetInfo(typeId,
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
        ColumnInfoPtr columnInfo = ColumnInfo::CreateMemberColumn
            (columnName,typeId,memberIndex,memberType,keyType,memberTypeId,keyTypeId,collectionType,arrayLength);
        columnInfoList.append(columnInfo);
    }
}

QVariant InstancesModelUtils::MemberToQVariant(const Safir::Dob::Typesystem::ObjectPtr& object,
                                               const ColumnInfoPtr& columnInfo,
                                               const int role)
{
    using namespace Safir::Dob::Typesystem;

    switch (columnInfo->CollectionType())
    {
    case SingleValueCollectionType:
        {
            const auto& container = object->GetMember(columnInfo->MemberIndex(),0);
            if (container.IsNull())
            {
                return QVariant();
            }

            return ContainerToVariant(container, columnInfo->MemberType(), columnInfo->MemberTypeId(), role);
        }

    case ArrayCollectionType:
        {
            if (role == Qt::DisplayRole)
            {
                return "<array>";
            }
            QStringList result;
            for (int i = 0; i < columnInfo->ArrayLength(); ++i)
            {
                const auto& container = object->GetMember(columnInfo->MemberIndex(),i);
                if (!container.IsNull())
                {
                    result += ContainerToVariant(container, columnInfo->MemberType(), columnInfo->MemberTypeId(), Qt::DisplayRole).toString();
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
            if (role == Qt::DisplayRole)
            {
                return "<sequence>";
            }
            const auto& container = object->GetMember(columnInfo->MemberIndex(),0);
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
            if (role == Qt::DisplayRole)
            {
                return "<dictionary>";
            }

            const auto& container = object->GetMember(columnInfo->MemberIndex(),0);
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


QVariant InstancesModelUtils::MemberColor(const Safir::Dob::Typesystem::ObjectPtr& object,
                                          const bool deleted,
                                          const std::chrono::steady_clock::time_point& greenUntil,
                                          const ColumnInfoPtr& columnInfo)
{
    using namespace Safir::Dob::Typesystem;

    if (deleted)
    {
        return QColor(255,0,0,100);
    }
    else if (greenUntil != std::chrono::steady_clock::time_point() &&
             greenUntil > std::chrono::steady_clock::now())
    {
        return QColor(0,255,0,100);
    }

    bool changed = false;

    switch (columnInfo->CollectionType())
    {
    case SingleValueCollectionType:
        {
            const auto& container = object->GetMember(columnInfo->MemberIndex(),0);
            changed = container.IsChanged();
        }
        break;

    case ArrayCollectionType:
        {
            for (int i = 0; i < columnInfo->ArrayLength(); ++i)
            {
                const auto& container = object->GetMember(columnInfo->MemberIndex(),i);
                if (container.IsChanged())
                {
                    changed = true;
                    break;
                }
            }
        }
        break;

    case SequenceCollectionType:
        {
            const auto& container = object->GetMember(columnInfo->MemberIndex(),0);
            changed = container.IsChanged();
        }
        break;

    case DictionaryCollectionType:
        {
            const auto& container = object->GetMember(columnInfo->MemberIndex(),0);
            changed = container.IsChanged();
        }
        break;

    }

    if (changed)
    {
        return QColor(116, 192, 252, 100); // transparent blue
    }
    else
    {
        return QVariant();
    }
}
