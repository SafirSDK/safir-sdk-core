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

#include <Safir/Dob/Typesystem/Internal/BlobOperations.h>
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
namespace Internal
{
    namespace
    {
        DotsC_KeyValMode Conv(BlobOperations::KeyValueMode m)
        {
            return m==BlobOperations::ValueMode ? DotsC_ValueMode : DotsC_KeyMode;
        }

        template <class T>
        T ToHashedType(DotsC_Int64 hash, const char* optionalStr)
        {
            return (optionalStr==NULL) ? T(hash) : T(Safir::Dob::Typesystem::Utilities::ToWstring(optionalStr));
        }
    }

    TypeId BlobOperations::GetTypeId(char const * const blob)
    {
        assert(blob != NULL);
        return DotsC_GetTypeId(blob);
    }

    Int32 BlobOperations::GetSize(char const * const blob)
    {
        assert(blob != NULL);
        return DotsC_GetSize(blob);
    }

//    bool BlobOperations::IsChanged(Safir::Dob::Typesystem::Int64 handle)
//    {
//        return false; //DotsC_IsAnythingChanged(blob);
//    }

//    bool BlobOperations::IsChanged(Safir::Dob::Typesystem::Int64 handle,
//                              const Dob::Typesystem::MemberIndex member,
//                              const Dob::Typesystem::ArrayIndex index)
//    {
//        return false; //DotsC_IsChangedMember(blob, member, index);
//    }

    /**********************************************************************
     *
     *  Container Set and Get
     *
     **********************************************************************/

    void
    BlobOperations::Set(const BooleanContainer & value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        DotsC_SetWriteCursor(handle, member, index);
        DotsC_SetWriteMode(handle, DotsC_ValueMode);
        DotsC_WriteMemberStatus(handle, value.m_bIsNull, value.m_bIsChanged);
        if (!value.m_bIsNull)
        {
            DotsC_WriteBooleanMember(handle, value.m_Value);
        }
    }

    void
    BlobOperations::Get(BooleanContainer & value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        DotsC_SetReadCursor(handle, member, index);
        DotsC_SetReadMode(handle, DotsC_ValueMode);
        DotsC_ReadMemberStatus(handle, value.m_bIsNull, value.m_bIsChanged);
        if (!value.m_bIsNull)
        {
            DotsC_ReadBooleanMember(handle, value.m_Value);
        }
    }

    void
    BlobOperations::Set(const EnumerationContainerBase & value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        DotsC_SetWriteCursor(handle, member, index);
        DotsC_SetWriteMode(handle, DotsC_ValueMode);
        DotsC_WriteMemberStatus(handle, value.m_bIsNull, value.m_bIsChanged);
        if (!value.m_bIsNull)
        {
            DotsC_WriteInt32Member(handle, value.m_Value);
        }
    }

    void
    BlobOperations::Get(EnumerationContainerBase & value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        DotsC_SetReadCursor(handle, member, index);
        DotsC_SetReadMode(handle, DotsC_ValueMode);
        DotsC_ReadMemberStatus(handle, value.m_bIsNull, value.m_bIsChanged);
        if (!value.m_bIsNull)
        {
            DotsC_ReadInt32Member(handle, value.m_Value);
        }
    }

    void
    BlobOperations::Set(const Int32Container & value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        DotsC_SetWriteCursor(handle, member, index);
        DotsC_SetWriteMode(handle, DotsC_ValueMode);
        DotsC_WriteMemberStatus(handle, value.m_bIsNull, value.m_bIsChanged);
        if (!value.m_bIsNull)
        {
            DotsC_WriteInt32Member(handle, value.m_Value);
        }
    }

    void
    BlobOperations::Get(Int32Container & value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        DotsC_SetReadCursor(handle, member, index);
        DotsC_SetReadMode(handle, DotsC_ValueMode);
        DotsC_ReadMemberStatus(handle, value.m_bIsNull, value.m_bIsChanged);
        if (!value.m_bIsNull)
        {
            DotsC_ReadInt32Member(handle, value.m_Value);
        }
    }

    void
    BlobOperations::Set(const Int64Container & value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        DotsC_SetWriteCursor(handle, member, index);
        DotsC_SetWriteMode(handle, DotsC_ValueMode);
        DotsC_WriteMemberStatus(handle, value.m_bIsNull, value.m_bIsChanged);
        if (!value.m_bIsNull)
        {
            DotsC_WriteInt64Member(handle, value.m_Value);
        }
    }

    void
    BlobOperations::Get(Int64Container & value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        DotsC_SetReadCursor(handle, member, index);
        DotsC_SetReadMode(handle, DotsC_ValueMode);
        DotsC_ReadMemberStatus(handle, value.m_bIsNull, value.m_bIsChanged);
        if (!value.m_bIsNull)
        {
            DotsC_ReadInt64Member(handle, value.m_Value);
        }
    }

    void
    BlobOperations::Set(const Float32Container & value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        DotsC_SetWriteCursor(handle, member, index);
        DotsC_SetWriteMode(handle, DotsC_ValueMode);
        DotsC_WriteMemberStatus(handle, value.m_bIsNull, value.m_bIsChanged);
        if (!value.m_bIsNull)
        {
            DotsC_WriteFloat32Member(handle, value.m_Value);
        }
    }

    void
    BlobOperations::Get(Float32Container & value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        DotsC_SetReadCursor(handle, member, index);
        DotsC_SetReadMode(handle, DotsC_ValueMode);
        DotsC_ReadMemberStatus(handle, value.m_bIsNull, value.m_bIsChanged);
        if (!value.m_bIsNull)
        {
            DotsC_ReadFloat32Member(handle, value.m_Value);
        }
    }

    void
    BlobOperations::Set(const Float64Container & value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        DotsC_SetWriteCursor(handle, member, index);
        DotsC_SetWriteMode(handle, DotsC_ValueMode);
        DotsC_WriteMemberStatus(handle, value.m_bIsNull, value.m_bIsChanged);
        if (!value.m_bIsNull)
        {
            DotsC_WriteFloat64Member(handle, value.m_Value);
        }
    }

    void
    BlobOperations::Get(Float64Container & value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        DotsC_SetReadCursor(handle, member, index);
        DotsC_SetReadMode(handle, DotsC_ValueMode);
        DotsC_ReadMemberStatus(handle, value.m_bIsNull, value.m_bIsChanged);
        if (!value.m_bIsNull)
        {
            DotsC_ReadFloat64Member(handle, value.m_Value);
        }
    }

    void
    BlobOperations::Set(const EntityIdContainer & value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        DotsC_SetWriteCursor(handle, member, index);
        DotsC_SetWriteMode(handle, DotsC_ValueMode);
        DotsC_WriteMemberStatus(handle, value.m_bIsNull, value.m_bIsChanged);
        if (!value.m_bIsNull)
        {
            const char* instStr=(value.m_Value.GetInstanceId().Utf8StringLength()==0 ? NULL : value.m_Value.GetInstanceId().Utf8String().c_str());
            DotsC_EntityId eid;
            eid.instanceId = value.m_Value.GetInstanceId().GetRawValue();
            eid.typeId = value.m_Value.GetTypeId();
            DotsC_WriteEntityIdMember(handle, eid, instStr);
        }
    }

    void
    BlobOperations::Get(EntityIdContainer & value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        DotsC_SetReadCursor(handle, member, index);
        DotsC_SetReadMode(handle, DotsC_ValueMode);
        DotsC_ReadMemberStatus(handle, value.m_bIsNull, value.m_bIsChanged);
        if (!value.m_bIsNull)
        {
            DotsC_EntityId eid;
            const char* instStr;
            DotsC_ReadEntityIdMember(handle, eid, instStr);
            if (instStr==NULL)
            {
                value.m_Value=Safir::Dob::Typesystem::EntityId(eid.typeId, Safir::Dob::Typesystem::InstanceId(eid.instanceId));
            }
            else
            {
                value.m_Value=Safir::Dob::Typesystem::EntityId(eid.typeId, Safir::Dob::Typesystem::InstanceId(Safir::Dob::Typesystem::Utilities::ToWstring(instStr)));
            }
        }
    }

    void
    BlobOperations::Set(const InstanceIdContainer & value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        DotsC_SetWriteCursor(handle, member, index);
        DotsC_SetWriteMode(handle, DotsC_ValueMode);
        DotsC_WriteMemberStatus(handle, value.m_bIsNull, value.m_bIsChanged);
        if (!value.m_bIsNull)
        {
            const char* str=value.m_Value.Utf8StringLength()==0 ? NULL : value.m_Value.Utf8String().c_str();
            DotsC_WriteHashedMember(handle, value.m_Value.GetRawValue(), str);
        }
    }

    void
    BlobOperations::Get(InstanceIdContainer & value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        DotsC_SetReadCursor(handle, member, index);
        DotsC_SetReadMode(handle, DotsC_ValueMode);
        DotsC_ReadMemberStatus(handle, value.m_bIsNull, value.m_bIsChanged);
        if (!value.m_bIsNull)
        {
            Safir::Dob::Typesystem::Int64 hash;
            const char* optionalStr;
            DotsC_ReadHashedMember(handle, hash, optionalStr);
            if (optionalStr==NULL)
            {
                value.m_Value=Safir::Dob::Typesystem::InstanceId(hash);
            }
            else
            {
                value.m_Value=Safir::Dob::Typesystem::InstanceId(Safir::Dob::Typesystem::Utilities::ToWstring(optionalStr));
            }
        }
    }

    void
    BlobOperations::Set(const ChannelIdContainer & value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        DotsC_SetWriteCursor(handle, member, index);
        DotsC_SetWriteMode(handle, DotsC_ValueMode);
        DotsC_WriteMemberStatus(handle, value.m_bIsNull, value.m_bIsChanged);
        if (!value.m_bIsNull)
        {
            const char* str=value.m_Value.Utf8StringLength()==0 ? NULL : value.m_Value.Utf8String().c_str();
            DotsC_WriteHashedMember(handle, value.m_Value.GetRawValue(), str);
        }
    }

    void
    BlobOperations::Get(ChannelIdContainer & value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        DotsC_SetReadCursor(handle, member, index);
        DotsC_SetReadMode(handle, DotsC_ValueMode);
        DotsC_ReadMemberStatus(handle, value.m_bIsNull, value.m_bIsChanged);
        if (!value.m_bIsNull)
        {
            Safir::Dob::Typesystem::Int64 hash;
            const char* optionalStr;
            DotsC_ReadHashedMember(handle, hash, optionalStr);
            if (optionalStr==NULL)
            {
                value.m_Value=Safir::Dob::Typesystem::ChannelId(hash);
            }
            else
            {
                value.m_Value=Safir::Dob::Typesystem::ChannelId(Safir::Dob::Typesystem::Utilities::ToWstring(optionalStr));
            }
        }
    }



    void
    BlobOperations::Set(const HandlerIdContainer & value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        DotsC_SetWriteCursor(handle, member, index);
        DotsC_SetWriteMode(handle, DotsC_ValueMode);
        DotsC_WriteMemberStatus(handle, value.m_bIsNull, value.m_bIsChanged);
        if (!value.m_bIsNull)
        {
            const char* str=value.m_Value.Utf8StringLength()==0 ? NULL : value.m_Value.Utf8String().c_str();
            DotsC_WriteHashedMember(handle, value.m_Value.GetRawValue(), str);
        }
    }

    void
    BlobOperations::Get(HandlerIdContainer & value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        DotsC_SetReadCursor(handle, member, index);
        DotsC_SetReadMode(handle, DotsC_ValueMode);
        DotsC_ReadMemberStatus(handle, value.m_bIsNull, value.m_bIsChanged);
        if (!value.m_bIsNull)
        {
            Safir::Dob::Typesystem::Int64 hash;
            const char* optionalStr;
            DotsC_ReadHashedMember(handle, hash, optionalStr);
            if (optionalStr==NULL)
            {
                value.m_Value=Safir::Dob::Typesystem::HandlerId(hash);
            }
            else
            {
                value.m_Value=Safir::Dob::Typesystem::HandlerId(Safir::Dob::Typesystem::Utilities::ToWstring(optionalStr));
            }
        }
    }

    void
    BlobOperations::Set(const StringContainer & value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        DotsC_SetWriteCursor(handle, member, index);
        DotsC_SetWriteMode(handle, DotsC_ValueMode);
        DotsC_WriteMemberStatus(handle, value.m_bIsNull, value.m_bIsChanged);
        if (!value.m_bIsNull)
        {
            DotsC_WriteStringMember(handle, value.Utf8String().c_str());
        }
    }

    void
    BlobOperations::Get(StringContainer & value,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        DotsC_SetReadCursor(handle, member, index);
        DotsC_SetReadMode(handle, DotsC_ValueMode);
        DotsC_ReadMemberStatus(handle, value.m_bIsNull, value.m_bIsChanged);
        if (!value.m_bIsNull)
        {
            const char* str;
            DotsC_ReadStringMember(handle, str);
            value.m_Value=Safir::Dob::Typesystem::Utilities::ToWstring(str);
        }
    }



    void
    BlobOperations::Set(const ObjectContainerBase & object,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        DotsC_SetWriteCursor(handle, member, index);
        DotsC_SetWriteMode(handle, DotsC_ValueMode);
        DotsC_WriteMemberStatus(handle, object.IsNull(), object.IsChanged());
        if (!object.IsNull())
        {
            Safir::Dob::Typesystem::Int64 objHandle=DotsC_CreateBlobWriter(object.GetObjectPointer()->GetTypeId());
            object.GetObjectPointer()->WriteToBlob(objHandle);
            Binary blob(static_cast<size_t>(DotsC_CalculateBlobSize(objHandle)));
            DotsC_WriteBlob(objHandle, &blob[0]);
            DotsC_DeleteBlobWriter(objHandle);
            DotsC_WriteObjectMember(handle, &blob[0]);
        }
    }

    void
    BlobOperations::Get(ObjectContainerBase & object,
                        Safir::Dob::Typesystem::Int64 handle,
                        const Dob::Typesystem::MemberIndex member,
                        const Dob::Typesystem::ArrayIndex index)
    {
        bool isNull;
        DotsC_SetReadCursor(handle, member, index);
        DotsC_SetReadMode(handle, DotsC_ValueMode);
        DotsC_ReadMemberStatus(handle, isNull, object.m_bIsChanged);

        if (!isNull)
        {
            const char* blob;
            DotsC_ReadObjectMember(handle, blob);
            object.SetObjectPointer(Dob::Typesystem::ObjectFactory::Instance().CreateObject(blob));
        }
        else
        {
            object.ResetObjectPointer();;
        }
    }

    void BlobOperations::Set(const BinaryContainer & binary,
                            Safir::Dob::Typesystem::Int64 handle,
                            const Dob::Typesystem::MemberIndex member,
                            const Dob::Typesystem::ArrayIndex index)
    {
        DotsC_SetWriteCursor(handle, member, index);
        DotsC_SetWriteMode(handle, DotsC_ValueMode);
        DotsC_WriteMemberStatus(handle, binary.IsNull(), binary.IsChanged());
        if (!binary.IsNull())
        {
            DotsC_WriteBinaryMember(handle, &(binary.m_Value[0]), static_cast<DotsC_Int32>(binary.m_Value.size()));
        }
    }


    void BlobOperations::Get(BinaryContainer & binary,
                            Safir::Dob::Typesystem::Int64 handle,
                            const Dob::Typesystem::MemberIndex member,
                            const Dob::Typesystem::ArrayIndex index)
    {
        DotsC_SetReadCursor(handle, member, index);
        DotsC_SetReadMode(handle, DotsC_ValueMode);
        DotsC_ReadMemberStatus(handle, binary.m_bIsNull, binary.m_bIsChanged);
        if (!binary.m_bIsNull)
        {
            const char* val;
            DotsC_Int32 size;
            DotsC_ReadBinaryMember(handle, val, size);
            binary.m_Value.resize(static_cast<size_t>(size));
            memcpy(&(binary.m_Value[0]), val, binary.m_Value.size());
        }
    }

    //-------------------------------------------------------------
    // Get value operations
    //-------------------------------------------------------------
    void BlobOperations::Get(bool& val,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex valueIndex,
                             KeyValueMode mode)
    {
        if (mode==KeyMode)
        {
            throw std::logic_error("BlobOperation.GetBoolean called with mode=KeyMode. Only ValueMode is allowed for this type!");
        }

        DotsC_SetReadCursor(handle, member, valueIndex);
        DotsC_SetReadMode(handle, DotsC_ValueMode);
        DotsC_ReadBooleanMember(handle, val);
    }

    void BlobOperations::Get(Safir::Dob::Typesystem::Int32& val,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex valueIndex,
                             KeyValueMode mode)
    {
        DotsC_SetReadCursor(handle, member, valueIndex);
        DotsC_SetReadMode(handle, Conv(mode));
        DotsC_ReadInt32Member(handle, val);
    }

    void BlobOperations::Get(Safir::Dob::Typesystem::Int64& val,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex valueIndex,
                             KeyValueMode mode)
    {
        DotsC_SetReadCursor(handle, member, valueIndex);
        DotsC_SetReadMode(handle, Conv(mode));
        DotsC_ReadInt64Member(handle, val);
    }

    void BlobOperations::Get(Safir::Dob::Typesystem::Float32 & val,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex valueIndex,
                             KeyValueMode mode)
    {
        if (mode==KeyMode)
        {
            throw std::logic_error("BlobOperation.GetFloat32 called with mode=KeyMode. Only ValueMode is allowed for this type!");
        }

        DotsC_SetReadCursor(handle, member, valueIndex);
        DotsC_SetReadMode(handle, DotsC_ValueMode);
        DotsC_ReadFloat32Member(handle, val);
    }

    void BlobOperations::Get(Safir::Dob::Typesystem::Float64& val,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex valueIndex,
                             KeyValueMode mode)
    {
        if (mode==KeyMode)
        {
            throw std::logic_error("BlobOperation.GetFloat64 called with mode=KeyMode. Only ValueMode is allowed for this type!");
        }

        DotsC_SetReadCursor(handle, member, valueIndex);
        DotsC_SetReadMode(handle, DotsC_ValueMode);
        DotsC_ReadFloat64Member(handle, val);
    }

    void BlobOperations::Get(std::wstring& val,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex valueIndex,
                             KeyValueMode mode)
    {
        DotsC_SetReadCursor(handle, member, valueIndex);
        DotsC_SetReadMode(handle, Conv(mode));
        const char* str;
        DotsC_ReadStringMember(handle, str);
        val=Safir::Dob::Typesystem::Utilities::ToWstring(str);
    }

    void BlobOperations::Get(Safir::Dob::Typesystem::InstanceId& val,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex valueIndex,
                             KeyValueMode mode)
    {
        DotsC_SetReadCursor(handle, member, valueIndex);
        DotsC_SetReadMode(handle, Conv(mode));
        DotsC_Int64 hash;
        const char* optionalStr;
        DotsC_ReadHashedMember(handle, hash, optionalStr);
        val=ToHashedType<Safir::Dob::Typesystem::InstanceId>(hash, optionalStr);
    }

    void BlobOperations::Get(Safir::Dob::Typesystem::HandlerId& val,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex valueIndex,
                             KeyValueMode mode)
    {
        DotsC_SetReadCursor(handle, member, valueIndex);
        DotsC_SetReadMode(handle, Conv(mode));
        DotsC_Int64 hash;
        const char* optionalStr;
        DotsC_ReadHashedMember(handle, hash, optionalStr);
        val=ToHashedType<Safir::Dob::Typesystem::HandlerId>(hash, optionalStr);
    }

    void BlobOperations::Get(Safir::Dob::Typesystem::ChannelId& val,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex valueIndex,
                             KeyValueMode mode)
    {
        DotsC_SetReadCursor(handle, member, valueIndex);
        DotsC_SetReadMode(handle, Conv(mode));
        DotsC_Int64 hash;
        const char* optionalStr;
        DotsC_ReadHashedMember(handle, hash, optionalStr);
        val=ToHashedType<Safir::Dob::Typesystem::ChannelId>(hash, optionalStr);
    }

    void BlobOperations::Get(Safir::Dob::Typesystem::EntityId& val,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex valueIndex,
                             KeyValueMode mode)
    {
        DotsC_SetReadCursor(handle, member, valueIndex);
        DotsC_SetReadMode(handle, Conv(mode));
        DotsC_EntityId eid;
        const char* optionalStr;
        DotsC_ReadEntityIdMember(handle, eid, optionalStr);
        val=Safir::Dob::Typesystem::EntityId(eid.typeId, ToHashedType<Safir::Dob::Typesystem::InstanceId>(eid.instanceId, optionalStr));
    }

    void BlobOperations::Get(Safir::Dob::Typesystem::ObjectPtr& val,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex valueIndex,
                             KeyValueMode mode)
    {
        if (mode==KeyMode)
        {
            throw std::logic_error("BlobOperation.GetObject called with mode=KeyMode. Only ValueMode is allowed for this type!");
        }

        DotsC_SetReadCursor(handle, member, valueIndex);
        DotsC_SetReadMode(handle, DotsC_ValueMode);
        const char* blob;
        DotsC_ReadObjectMember(handle, blob);
        val=Dob::Typesystem::ObjectFactory::Instance().CreateObject(blob);
    }

    void BlobOperations::Get(Safir::Dob::Typesystem::Binary& val,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex valueIndex,
                             KeyValueMode mode)
    {
        if (mode==KeyMode)
        {
            throw std::logic_error("BlobOperation.GetBinary called with mode=KeyMode. Only ValueMode is allowed for this type!");
        }

        DotsC_SetReadCursor(handle, member, valueIndex);
        DotsC_SetReadMode(handle, DotsC_ValueMode);
        const char* bin;
        DotsC_Int32 size;
        DotsC_ReadBinaryMember(handle, bin, size);
        val.clear();
        val.resize(static_cast<size_t>(size));
        memcpy(&(val[0]), bin, val.size());
    }
}
}
}
}
