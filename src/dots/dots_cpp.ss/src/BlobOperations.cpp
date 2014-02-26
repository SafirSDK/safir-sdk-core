/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safir.sourceforge.net)
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

#include <Safir/Dob/Typesystem/BlobOperations.h>
#include <Safir/Dob/Typesystem/Internal/Kernel.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Dob/Typesystem/Exceptions.h>
#include <Safir/Dob/Typesystem/ObjectFactory.h>
#include <cstring>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
    TypeId
    BlobOperations::GetTypeId(char const * const blob)
    {
        assert(blob != NULL);
        return DotsC_GetTypeId(blob);
    }

    Int32 BlobOperations::GetSize(char const * const blob)
    {
        assert(blob != NULL);
        return DotsC_GetSize(blob);
    }

    bool BlobOperations::IsChanged(char const * const blob)
    {
        assert(blob != NULL);
        return DotsC_IsAnythingChanged(blob);
    }

    Int32
    BlobOperations::GetInitialSize(const TypeId typeId)
    {
        return DotsC_GetInitialSize(typeId);
    }

    //
    // Bool
    //
    void BlobOperations::Set(char * const blob,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index,
                             const bool value,
                             const bool isNull,
                             const bool isChanged)
    {
        assert(blob != NULL);
        DotsC_SetBooleanMemberInPreallocated(value,
                                             isNull,
                                             isChanged,
                                             blob,
                                             member,
                                             index);
    }

    void BlobOperations::Get(char const * const blob,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index,
                             bool & value,
                             bool & isNull,
                             bool & isChanged)
    {
        assert(blob != NULL);
        DotsC_GetBooleanMember(blob,
                               member,
                               index,
                               value,
                               isNull,
                               isChanged);
    }

    //
    // Int32
    //
    void BlobOperations::Set(char * const blob,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index,
                             const Int32 value,
                             const bool isNull,
                             const bool isChanged)
    {
        assert(blob != NULL);
        DotsC_SetInt32MemberInPreallocated(value,
                                           isNull,
                                           isChanged,
                                           blob,
                                           member,
                                           index);
    }

    void BlobOperations::Get(char const * const blob,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index,
                             Int32 & value,
                             bool & isNull,
                             bool & isChanged)
    {
        assert(blob != NULL);
        DotsC_GetInt32Member(blob,
                             member,
                             index,
                             value,
                             isNull,
                             isChanged);
    }

    //
    // Int64
    //
    void BlobOperations::Set(char * const blob,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index,
                             const Int64 value,
                             const bool isNull,
                             const bool isChanged)
    {
        assert(blob != NULL);
        DotsC_SetInt64MemberInPreallocated(value,
                                             isNull,
                                             isChanged,
                                             blob,
                                             member,
                                             index);
    }

    void BlobOperations::Get(char const * const blob,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index,
                             Int64 & value,
                             bool & isNull,
                             bool & isChanged)
    {
        assert(blob != NULL);
        DotsC_GetInt64Member(blob,
                             member,
                             index,
                             value,
                             isNull,
                             isChanged);
    }

    //
    // Float32
    //
    void BlobOperations::Set(char * const blob,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index,
                             const Float32 value,
                             const bool isNull,
                             const bool isChanged)
    {
        assert(blob != NULL);
        DotsC_SetFloat32MemberInPreallocated(value,
                                             isNull,
                                             isChanged,
                                             blob,
                                             member,
                                             index);
    }

    void BlobOperations::Get(char const * const blob,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index,
                             Float32 & value,
                             bool & isNull,
                             bool & isChanged)
    {
        assert(blob != NULL);
        DotsC_GetFloat32Member(blob,
                             member,
                             index,
                             value,
                             isNull,
                             isChanged);
    }

    //
    // Float64
    //
    void BlobOperations::Set(char * const blob,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index,
                             const Float64 value,
                             const bool isNull,
                             const bool isChanged)
    {
        assert(blob != NULL);
        DotsC_SetFloat64MemberInPreallocated(value,
                                             isNull,
                                             isChanged,
                                             blob,
                                             member,
                                             index);
    }

    void BlobOperations::Get(char const * const blob,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index,
                             Float64 & value,
                             bool & isNull,
                             bool & isChanged)
    {
        assert(blob != NULL);
        DotsC_GetFloat64Member(blob,
                             member,
                             index,
                             value,
                             isNull,
                             isChanged);
    }

    //
    // InstanceId
    //
    void BlobOperations::Get(char const * const blob,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index,
                             InstanceId & value,
                             bool & isNull,
                             bool & isChanged)
    {
        assert(blob != NULL);
        Int64 hashVal;
        const char * strVal = NULL;
        DotsC_GetHashedIdMember(blob,
                                member,
                                index,
                                hashVal,
                                strVal,
                                isNull,
                                isChanged);
        if (!isNull)
        {
            if (strVal != NULL)
            {
                value = InstanceId(hashVal,Utilities::ToWstring(strVal));
            }
            else
            {
                value = InstanceId(hashVal);
            }
        }
    }

    //
    // EntityId
    //
    void BlobOperations::Get(char const * const blob,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index,
                             EntityId & value,
                             bool & isNull,
                             bool & isChanged)
    {
        assert(blob != NULL);
        DotsC_EntityId eid;
        const char * strVal = NULL;
        DotsC_GetEntityIdMember(blob,
                                member,
                                index,
                                eid,
                                strVal,
                                isNull,
                                isChanged);
        if (!isNull)
        {
            if (strVal != NULL)
            {
                value = EntityId(eid.typeId,InstanceId(eid.instanceId,Utilities::ToWstring(strVal)));
            }
            else
            {
                value = EntityId(eid.typeId,InstanceId(eid.instanceId));
            }
        }
    }

    //
    // ChannelId
    //
    void BlobOperations::Get(char const * const blob,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index,
                             ChannelId & value,
                             bool & isNull,
                             bool & isChanged)
    {
        assert(blob != NULL);
        Int64 hashVal;
        const char * strVal = NULL;
        DotsC_GetHashedIdMember(blob,
                                member,
                                index,
                                hashVal,
                                strVal,
                                isNull,
                                isChanged);
        if (!isNull)
        {
            if (strVal != NULL)
            {
                value = ChannelId(hashVal, Utilities::ToWstring(strVal));
            }
            else
            {
                value = ChannelId(hashVal);
            }
        }
    }

    //
    // HandlerId
    //
    void BlobOperations::Get(char const * const blob,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index,
                             HandlerId & value,
                             bool & isNull,
                             bool & isChanged)
    {
        assert(blob != NULL);
        Int64 hashVal;
        const char * strVal = NULL;
        DotsC_GetHashedIdMember(blob,
                                member,
                                index,
                                hashVal,
                                strVal,
                                isNull,
                                isChanged);
        if (!isNull)
        {
            if (strVal != NULL)
            {
                value = HandlerId(hashVal,Utilities::ToWstring(strVal));
            }
            else
            {
                value = HandlerId(hashVal);
            }
        }
    }
    //
    // String
    //
    void BlobOperations::Get(char const * const blob,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index,
                             std::wstring & value,
                             bool & isNull,
                             bool & isChanged)
    {
        assert(blob != NULL);
        const char * str;
        DotsC_GetStringMember(blob,
                              member,
                              index,
                              str,
                              isNull,
                              isChanged);
        if (!isNull)
        {
            value = Utilities::ToWstring(str);
        }
    }

    //
    // Object
    //
    void BlobOperations::Get(char * const blob,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index,
                             const char * & childBlob,
                             bool & isNull,
                             bool & isChanged)
    {
        assert(blob != NULL);
        DotsC_GetObjectMember(blob,
                              member,
                              index,
                              childBlob,
                              isNull,
                              isChanged);
    }

    //
    // Binary
    //
    void BlobOperations::Get(char const * const blob,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index,
                             Dob::Typesystem::Binary & value,
                             bool & isNull,
                             bool & isChanged)
    {
        assert(blob != NULL);
        const char* bin;
        DotsC_Int32 size;
        DotsC_GetBinaryMember(blob,
                              member,
                              index,
                              bin,
                              size,
                              isNull,
                              isChanged);
        if (!isNull)
        {
            value.reserve(size);
            value.assign(bin,bin+size);
        }
    }


    /**********************************************************************
     *
     *  Container Set and Get
     *
     **********************************************************************/

    void
    BlobOperations::Set(const BooleanContainer & value,
                        char * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        Set(blob,
            member,
            index,
            value.m_Value,
            value.m_bIsNull,
            value.m_bIsChanged);
    }

    void
    BlobOperations::Get(BooleanContainer & value,
                        char const * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        Get(blob,
            member,
            index,
            value.m_Value,
            value.m_bIsNull,
            value.m_bIsChanged);
    }

    void
    BlobOperations::Set(const EnumerationContainerBase & value,
                        char * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        Set(blob,
            member,
            index,
            value.m_Value,
            value.m_bIsNull,
            value.m_bIsChanged);
    }

    void
    BlobOperations::Get(EnumerationContainerBase & value,
                        char const * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        Get(blob,
            member,
            index,
            value.m_Value,
            value.m_bIsNull,
            value.m_bIsChanged);
    }

    void
    BlobOperations::Set(const Int32Container & value,
                        char * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        Set(blob,
            member,
            index,
            value.m_Value,
            value.m_bIsNull,
            value.m_bIsChanged);
    }

    void
    BlobOperations::Get(Int32Container & value,
                        char const * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        Get(blob,
            member,
            index,
            value.m_Value,
            value.m_bIsNull,
            value.m_bIsChanged);
    }

    void
    BlobOperations::Set(const Int64Container & value,
                        char * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        Set(blob,
            member,
            index,
            value.m_Value,
            value.m_bIsNull,
            value.m_bIsChanged);
    }

    void
    BlobOperations::Get(Int64Container & value,
                        char const * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        Get(blob,
            member,
            index,
            value.m_Value,
            value.m_bIsNull,
            value.m_bIsChanged);
    }

    void
    BlobOperations::Set(const Float32Container & value,
                        char * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        Set(blob,
            member,
            index,
            value.m_Value,
            value.m_bIsNull,
            value.m_bIsChanged);
    }

    void
    BlobOperations::Get(Float32Container & value,
                        char const * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        Get(blob,
            member,
            index,
            value.m_Value,
            value.m_bIsNull,
            value.m_bIsChanged);
    }

    void
    BlobOperations::Set(const Float64Container & value,
                        char * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        Set(blob,
            member,
            index,
            value.m_Value,
            value.m_bIsNull,
            value.m_bIsChanged);
    }

    void
    BlobOperations::Get(Float64Container & value,
                        char const * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        Get(blob,
            member,
            index,
            value.m_Value,
            value.m_bIsNull,
            value.m_bIsChanged);
    }


    void
    BlobOperations::Set(const InstanceIdContainer & value,
                        char * const blob,
                        char * & beginningOfUnused,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        if (!value.IsNull())
        {
            const Int32 stringLength = value.m_Value.Utf8StringLength();
            DotsC_SetHashedIdMemberInPreallocated
                (value.m_Value.GetRawValue(),
                (stringLength == 0 ? NULL : value.m_Value.Utf8String().c_str()),
                stringLength,
                value.IsNull(),
                value.IsChanged(),
                blob,
                member,
                index,
                beginningOfUnused);
        }
        else if(value.IsChanged())
        {
            DotsC_SetNullMember(blob, member, index);
        }
    }

    void
    BlobOperations::Get(InstanceIdContainer & value,
                        char const * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        Get(blob, member, index, value.m_Value,value.m_bIsNull,value.m_bIsChanged);
    }



    void
    BlobOperations::Set(const EntityIdContainer & value,
                        char * const blob,
                        char * & beginningOfUnused,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        if (!value.IsNull())
        {
            const Int32 stringLength = value.m_Value.GetInstanceId().Utf8StringLength();
            DotsC_EntityId eid;
            eid.instanceId = value.m_Value.GetInstanceId().GetRawValue();
            eid.typeId = value.m_Value.GetTypeId();
            DotsC_SetEntityIdMemberInPreallocated(eid,
                (stringLength == 0 ? NULL : value.m_Value.GetInstanceId().Utf8String().c_str()),
                stringLength,
                value.IsNull(),
                value.IsChanged(),
                blob,
                member,
                index,
                beginningOfUnused);
        }
        else if(value.IsChanged())
        {
            DotsC_SetNullMember(blob, member, index);
        }
    }

    void
    BlobOperations::Get(EntityIdContainer & value,
                        char const * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        Get(blob, member, index, value.m_Value,value.m_bIsNull,value.m_bIsChanged);
    }



    void
    BlobOperations::Set(const ChannelIdContainer & value,
                        char * const blob,
                        char * & beginningOfUnused,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        if (!value.IsNull())
        {
            const Int32 stringLength = value.m_Value.Utf8StringLength();
            DotsC_SetHashedIdMemberInPreallocated(value.m_Value.GetRawValue(),
                (stringLength == 0 ? NULL : value.m_Value.Utf8String().c_str()),
                stringLength,
                value.IsNull(),
                value.IsChanged(),
                blob,
                member,
                index,
                beginningOfUnused);
        }
        else if(value.IsChanged())
        {
            DotsC_SetNullMember(blob, member, index);
        }
    }

    void
    BlobOperations::Get(ChannelIdContainer & value,
                        char const * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        Get(blob, member, index, value.m_Value,value.m_bIsNull,value.m_bIsChanged);
    }



    void
    BlobOperations::Set(const HandlerIdContainer & value,
                        char * const blob,
                        char * & beginningOfUnused,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        if (!value.IsNull())
        {
            const Int32 stringLength = value.m_Value.Utf8StringLength();
            DotsC_SetHashedIdMemberInPreallocated(value.m_Value.GetRawValue(),
                (stringLength == 0 ? NULL : value.m_Value.Utf8String().c_str()),
                stringLength,
                value.IsNull(),
                value.IsChanged(),
                blob,
                member,
                index,
                beginningOfUnused);
        }
        else if(value.IsChanged())
        {
            DotsC_SetNullMember(blob, member, index);
        }
    }

    void
    BlobOperations::Get(HandlerIdContainer & value,
                        char const * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        Get(blob, member, index, value.m_Value,value.m_bIsNull,value.m_bIsChanged);
    }

    void
    BlobOperations::Set(const StringContainer & value,
                        char * const blob,
                        char * & beginningOfUnused,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        if (!value.IsNull())
        {
            char * const stringStart = beginningOfUnused;
            const Int32 stringLength = value.Utf8StringLength();
            DotsC_CreateStringMember(blob,
                                     stringLength,
                                     member,
                                     index,
                                     value.IsChanged(),
                                     beginningOfUnused);

            strncpy(stringStart,value.Utf8String().c_str(),stringLength);
        }
        else if(value.IsChanged())
        {
            DotsC_SetNullMember(blob, member, index);
        }
    }

    void
    BlobOperations::Get(StringContainer & value,
                        char const * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        assert(blob != NULL);
        const char * str;
        DotsC_GetStringMember(blob,
                              member,
                              index,
                              str,
                              value.m_bIsNull,
                              value.m_bIsChanged);
        if (!value.m_bIsNull)
        {
            value.m_CachedUtf8String = str;
            value.m_Value = Utilities::ToWstring(value.m_CachedUtf8String);
        }
    }



    void
    BlobOperations::Set(const ObjectContainerBase & object,
                        char * const blob,
                        char * & beginningOfUnused,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        if (!object.IsNull())
        {
            char * const childBlob = beginningOfUnused;
            DotsC_CreateObjectMember(blob,
                                     object.GetObjectPointer()->CalculateBlobSize(), //can we cache this value from the last call?
                                     object.GetObjectPointer()->GetTypeId(),
                                     member,
                                     index,
                                     object.IsChangedHere(),
                                     beginningOfUnused);
            object.GetObjectPointer()->WriteToBlob(childBlob,beginningOfUnused);
        }
        else if(object.IsChangedHere())
        {
            DotsC_SetNullMember(blob, member, index);
        }
    }

    void
    BlobOperations::Get(ObjectContainerBase & object,
                        char const * const blob,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        const char * childBlob = NULL;
        bool childIsNull = true;
        DotsC_GetObjectMember(blob,
                              member,
                              index,
                              childBlob,
                              childIsNull,
                              object.m_bIsChanged);
        if (childIsNull)
        {
            object.ResetObjectPointer();
        }
        else
        {
            object.SetObjectPointer(Dob::Typesystem::ObjectFactory::Instance().CreateObject(childBlob));
        }
    }

    void BlobOperations::Set(const BinaryContainer & binary,
                            char * const blob,
                            char * & beginningOfUnused,
                            const Dob::Typesystem::MemberIndex member,
                            const Dob::Typesystem::ArrayIndex index)
    {
        if (!binary.IsNull())
        {
            char * const binaryStart = beginningOfUnused;
            const Int32 binarySize = static_cast<Int32>(binary.GetVal().size());
            DotsC_CreateBinaryMember(blob,
                                     binarySize,
                                     member,
                                     index,
                                     binary.IsChanged(),
                                     beginningOfUnused);
            if (binarySize != 0)
            {
                memcpy(binaryStart, &binary.GetVal()[0], binarySize);
            }
        }
        else if(binary.IsChanged())
        {
            DotsC_SetNullMember(blob, member, index);
        }
    }


    void BlobOperations::Get(BinaryContainer & binary,
                            char const * const blob,
                            const Dob::Typesystem::MemberIndex member,
                            const Dob::Typesystem::ArrayIndex index)
    {
        bool isChanged, isNull;
        Binary bin;
        Get(blob, member, index, bin, isNull, isChanged);
        if (isNull)
            binary.SetNull();
        else
            binary.SetVal(bin);

        binary.SetChanged(isChanged);
    }

    //
    // SetNull
    //
    void
    BlobOperations::SetNull(char * const blob,
                            const Dob::Typesystem::MemberIndex member,
                            const Dob::Typesystem::ArrayIndex index)
    {
        DotsC_SetNullMember(blob, member, index);
    }

    //
    // IsChanged
    //
    bool
    BlobOperations::IsChanged(const char * const blob,
                              const Dob::Typesystem::MemberIndex member,
                              const Dob::Typesystem::ArrayIndex index)
    {
        return DotsC_IsChangedMember(blob, member, index);
    }
}
}
}
