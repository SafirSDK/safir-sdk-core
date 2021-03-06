/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / stlrha
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
#include <Safir/Dob/Internal/TimestampOperations.h>
#include <Safir/Dob/Internal/StateDeleter.h>
#include <Safir/Dob/Internal/DistributionData.h>
#include <Safir/Dob/Internal/InjectionKindTable.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Typesystem/Internal/BlobOperations.h>
#include <Safir/Dob/Typesystem/ObjectFactory.h>
#include <Safir/Dob/Typesystem/Internal/InternalOperations.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Dob/Typesystem/Members.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{

    TimestampOperations::MergeMemberResult
    TimestampOperations::MergeMember(const Typesystem::TypeId typeId,
                                     const Typesystem::MemberIndex member,
                                     const Typesystem::Int64 intoTimestamp,
                                     const Typesystem::Int64 fromTimestamp,
                                     const Typesystem::ObjectPtr & intoObject,
                                     const Typesystem::ObjectConstPtr & fromObject)
    {
        if (fromTimestamp > intoTimestamp)
        {
            Safir::Dob::Typesystem::MemberType memberType;
            Safir::Dob::Typesystem::MemberType keyType;
            const char * memberName;
            Typesystem::TypeId memberTypeId;
            Typesystem::TypeId keyTypeId;
            Typesystem::Int32 stringLength;
            Typesystem::CollectionType collectionType;
            Typesystem::Int32 arrayLength;

            Safir::Dob::Typesystem::Members::GetInfo(typeId,
                                                     member,
                                                     memberType,
                                                     keyType,
                                                     memberName,
                                                     memberTypeId,
                                                     keyTypeId,
                                                     stringLength,
                                                     collectionType,
                                                     arrayLength);

            bool fromIsChanged = false;
            for (int index = 0; index < arrayLength;++index)
            {
                if (fromObject->GetMember(member,index).IsChanged())
                {
                    fromIsChanged = true;
                    break;
                }
            }

            if (!fromIsChanged)
            {
                SEND_SYSTEM_LOG(Critical,
                                << "There was an unexpected timestamp on member "
                                << memberName << ". The into timestamp is " << intoTimestamp
                                << " and the from timestamp is " << fromTimestamp
                                << ", but the changeflag is not set!");
            }

            for (int index = 0; index < arrayLength;++index)
            {
                intoObject->GetMember(member,index).Copy(fromObject->GetMember(member,index));
            }
            return std::make_pair(fromTimestamp, true);
        }
        return std::make_pair(intoTimestamp, false);
    }


    const TimestampOperations::MergeResult
    TimestampOperations::Merge(const DistributionData & into, const DistributionData & from)
    {
        const Typesystem::TypeId typeId = into.GetTypeId();
        ENSURE(typeId == from.GetTypeId(), << "TimestampOperations::Merge: The types must be the same for the merge operation to be possible. into = " <<
               Safir::Dob::Typesystem::Operations::GetName(typeId) << ", from = " << Safir::Dob::Typesystem::Operations::GetName(from.GetTypeId()));

        ENSURE(InjectionKindTable::Instance().IsInjectable(typeId), << "TimestampOperations::Merge: It is only possible to merge states if the type is Injectable");

        //If one of them has a null blob
        //  if it has a later top-level time, return that one
        //  if it has an earlier time, return the other one

        //check the first
        if (!into.HasBlob())
        {
            //Note: if the timestamps are equal we always choose the one that was deleted.
            if (into.GetTopTimestamp() >= from.GetTopTimestamp())
            {
                return std::make_pair(into.GetEntityStateCopy(true), false);
            }
            else
            {
                DistributionData result = DistributionData(no_state_tag);

                if (from.HasBlob())
                {
                    result = into.GetEntityStateCopy(from.GetBlob(), false);

                    Dob::Typesystem::Int32 numberOfMembers = Safir::Dob::Typesystem::Members::GetNumberOfMembers(typeId);
                    Typesystem::Int64* resultTimestamps = result.GetMemberTimestamps();
                    const Typesystem::Int64* fromTimestamps = from.GetMemberTimestamps();

                    for (int member = 0; member < numberOfMembers; ++member)
                    {
                        resultTimestamps[member] = fromTimestamps[member];
                    }
                }
                else
                {
                    result = into.GetEntityStateCopy(false);
                }

                result.SetTopTimestamp(from.GetTopTimestamp());

                return std::make_pair(result, true);
            }
        }

        //check the second
        if (!from.HasBlob())
        {
            //Note: if the timestamps are equal we always choose the one that was deleted.
            if (from.GetTopTimestamp() >= into.GetTopTimestamp())
            {
                DistributionData result = into.GetEntityStateCopy(false);
                result.SetTopTimestamp(from.GetTopTimestamp());
                return std::make_pair(result, true);
            }
            else
            {
                return std::make_pair(into.GetEntityStateCopy(true), false);
            }
        }

        //now we know that both have blobs and therefore timestamps.

        bool isMerged = false;

        const Typesystem::Int64 * intoTimestamps = into.GetMemberTimestamps();
        const Typesystem::Int64 * fromTimestamps = from.GetMemberTimestamps();

        const Typesystem::MemberIndex numberOfMembers =
            Safir::Dob::Typesystem::Members::GetNumberOfMembers(typeId);
        std::vector<MergeMemberResult> mergedTimestamps(numberOfMembers);

        Typesystem::ObjectPtr intoObject =
            Typesystem::ObjectFactory::Instance().CreateObject(into.GetBlob());
        const Typesystem::ObjectConstPtr fromObject =
            Typesystem::ObjectFactory::Instance().CreateObject(from.GetBlob());

        for (int member = 0; member < numberOfMembers; ++member)
        {
            mergedTimestamps[member] =
                MergeMember(typeId, member, intoTimestamps[member], fromTimestamps[member], intoObject, fromObject);
        }

        Typesystem::BinarySerialization bin;
        Typesystem::Serialization::ToBinary(intoObject,bin);
        DistributionData result = into.GetEntityStateCopy(&bin[0], false);

        if (from.GetTopTimestamp() > into.GetTopTimestamp())
        {
            result.SetTopTimestamp(from.GetTopTimestamp());
            isMerged = true;
        }
        else
        {
            result.SetTopTimestamp(into.GetTopTimestamp());
        }

        Typesystem::Int64 * resultTimestamps = result.GetMemberTimestamps();

        for (int member = 0; member < numberOfMembers; ++member)
        {
            resultTimestamps[member] = mergedTimestamps[member].first;

            if (mergedTimestamps[member].second == true)
            {
                isMerged = true;
            }
        }
        return std::make_pair(result, isMerged);
    }


    const TimestampOperations::MergeResult
    TimestampOperations::Merge(const DistributionData & into,
                               const char * const blob,
                               const Typesystem::Int64 timestamp)
    {
        //we implement this function using the other functions in this class.
        //This is probably not the optimal way of implementing it, but it is easy.
        DistributionData from(no_state_tag);

        if (blob == NULL)
        {
            from = into.GetEntityStateCopy(false);
        }
        else
        {
            from = into.GetEntityStateCopy(blob, false);
        }
        SetTimestampForChangedMembers(from,timestamp,true);
        return Merge(into,from);
    }

    void
    TimestampOperations::SetTimestampForChangedMembers(DistributionData&        entityState,
                                                       const Typesystem::Int64  timestamp,
                                                       const bool               resetUnchangedMembers)
    {
        SetTimestampForChangedMembers(entityState, nullptr, timestamp, resetUnchangedMembers);
    }

    void
    TimestampOperations::SetTimestampForChangedMembers(DistributionData&        entityState,
                                                       const char* const        changeFlagsBlobTemplate,
                                                       const Typesystem::Int64  timestamp,
                                                       const bool               resetUnchangedMembers)
    {
        const Typesystem::TypeId typeId = entityState.GetTypeId();

        ENSURE(InjectionKindTable::Instance().IsInjectable(typeId),
            << "TimestampOperations::SetTimestampForChangedMembers: It is only possible to call this method if type is Injectable");

        if (!entityState.HasBlob())
        {
            return;
        }

        std::unique_ptr<Typesystem::Internal::BlobReadHelper> reader;

        if (changeFlagsBlobTemplate == nullptr)
        {
            // Change flags shall be read from the target entity
            reader.reset(new Typesystem::Internal::BlobReadHelper(entityState.GetBlob()));
        }
        else
        {
            // Change flags shall be read from template blob
            reader.reset(new Typesystem::Internal::BlobReadHelper(changeFlagsBlobTemplate));
        }

        entityState.SetTopTimestamp(timestamp);

        const Typesystem::MemberIndex numberOfMembers =
            Safir::Dob::Typesystem::Members::GetNumberOfMembers(typeId);

        for (int member = 0; member < numberOfMembers; ++member)
        {
            if (reader->IsChangedRecursive(member))
            {
                entityState.GetMemberTimestamps()[member] = timestamp;
            }
            else if (resetUnchangedMembers)
            {
                entityState.GetMemberTimestamps()[member] = 0;
            }
        }
    }

    void
    TimestampOperations::SetTimestampForAllMembers(DistributionData&        entityState,
                                                   const Typesystem::Int64  timestamp)
    {
        const Typesystem::TypeId typeId = entityState.GetTypeId();

        ENSURE(InjectionKindTable::Instance().IsInjectable(typeId),
            << "TimestampOperations::SetTimestampForAllMembers: It is only possible to call this method if type is Injectable");

        entityState.SetTopTimestamp(timestamp);

        if (!entityState.HasBlob())
        {
            return;
        }


        const Typesystem::MemberIndex numberOfMembers =
            Safir::Dob::Typesystem::Members::GetNumberOfMembers(typeId);

        for (int member = 0; member < numberOfMembers; ++member)
        {
            entityState.GetMemberTimestamps()[member] = timestamp;
        }
    }

    void
    TimestampOperations::SetChangeFlags(const DistributionData& previous,
                                        const DistributionData& current,
                                        Typesystem::Internal::BlobWriteHelper& writer)
    {
        const Typesystem::TypeId typeId = current.GetTypeId();
        ENSURE(InjectionKindTable::Instance().IsInjectable(typeId),
            << "TimestampOperations::SetChangeFlags: It is only possible to call this method if the type is Injectable");

        ENSURE(previous.HasBlob() && current.HasBlob(),
               << "TimestampOperations::SetChangeFlags: Both states must have blobs!");

        if (previous.GetTopTimestamp() >= current.GetTopTimestamp())
        {
            return;
        }

        const Typesystem::MemberIndex numberOfMembers =
            Safir::Dob::Typesystem::Members::GetNumberOfMembers(typeId);

        for (int member = 0; member < numberOfMembers; ++member)
        {
            if (current.GetMemberTimestamps()[member] > previous.GetMemberTimestamps()[member])
            {
                const auto collectionType = writer.GetCollectionType(member);
                if (collectionType == SequenceCollectionType || collectionType == DictionaryCollectionType)
                {
                    writer.SetChangedTopLevel(member, true);
                }
                else
                {
                    const Typesystem::Int32 arraySize = Typesystem::Members::GetArraySize(typeId,member);
                    for (Typesystem::ArrayIndex index = 0; index < arraySize; ++index)
                    {
                        writer.SetChangedHere(member, index, true);
                    }
                }
            }
        }
    }

    bool TimestampOperations::HaveChanges(const DistributionData & realState,
                                          const DistributionData & injection)
    {
        const Typesystem::TypeId typeId = realState.GetTypeId();
        ENSURE(typeId == injection.GetTypeId(),
            << "TimestampOperations::Merge: The types must be the same for the "
            << "merge operation to be possible. realState = "
            << Safir::Dob::Typesystem::Operations::GetName(typeId)
            << ", injection = "
            << Safir::Dob::Typesystem::Operations::GetName(injection.GetTypeId()));

        ENSURE(InjectionKindTable::Instance().IsInjectable(typeId),
            << "TimestampOperations::Merge: It is only possible to merge states if the type is Injectable");

        //If one of them has a null blob
        //Compare the top timestamps, and return true if realState is later.
        //if the timestamps are equal we must return true only if realState
        //is the one with null blob, since in a merge we choose the deleted one on equality.

        if (!realState.HasBlob() || !injection.HasBlob())
        {
            if (realState.GetTopTimestamp() > injection.GetTopTimestamp())
            {
                return true;
            }
            else if (realState.GetTopTimestamp() < injection.GetTopTimestamp())
            {
                return false;
            }
            else //equal!
            {
                return !realState.HasBlob();
            }
        }

        //now we know that both have blobs and therefore timestamps.

        const Typesystem::Int64 * realStateTimestamps = realState.GetMemberTimestamps();
        const Typesystem::Int64 * injectionTimestamps = injection.GetMemberTimestamps();

        const Typesystem::MemberIndex numberOfMembers =
            Safir::Dob::Typesystem::Members::GetNumberOfMembers(typeId);

        for (int member = 0; member < numberOfMembers; ++member)
        {
            if (realStateTimestamps[member] < injectionTimestamps[member])
            {
                return false;
            }
        }
        return true;
    }
}
}
}

