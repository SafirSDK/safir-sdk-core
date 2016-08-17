/******************************************************************************
*
* Copyright Saab AB, 2004-2015 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson / joot
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
* GNU General Public License for more Internals.
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

    char* BlobOperations::CreateCopy(const char* blob)
    {
        char* copy;
        DotsC_CreateCopyOfBlob(copy, blob);
        return copy;
    }

    void BlobOperations::Delete(char* & blob)
    {
        DotsC_DeleteBlob(blob);
        blob=NULL;
    }

    /**********************************************************************
     *
     *  Container Set and Get
     *
     **********************************************************************/

    void BlobOperations::Set(const BooleanContainer& value,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index)
    {
        Set(value.m_Value, value.IsNull(), value.IsChanged(), handle, member, index, ValueMode);
    }

    void BlobOperations::Get(BooleanContainer& value,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index)
    {
        Get(value.m_Value, value.m_bIsNull, value.m_bIsChanged, handle, member, index, ValueMode);
    }

    void BlobOperations::Set(const EnumerationContainerBase& value,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index)
    {
        Set(value.m_Value, value.IsNull(), value.IsChanged(), handle, member, index, ValueMode);
    }

    void BlobOperations::Get(EnumerationContainerBase& value,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index)
    {
        Get(value.m_Value, value.m_bIsNull, value.m_bIsChanged, handle, member, index, ValueMode);
    }

    void BlobOperations::Set(const Int32Container& value,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index)
    {
        Set(value.m_Value, value.IsNull(), value.IsChanged(), handle, member, index, ValueMode);
    }

    void BlobOperations::Get(Int32Container& value,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index)
    {
        Get(value.m_Value, value.m_bIsNull, value.m_bIsChanged, handle, member, index, ValueMode);
    }

    void BlobOperations::Set(const Int64Container& value,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index)
    {
        Set(value.m_Value, value.IsNull(), value.IsChanged(), handle, member, index, ValueMode);
    }

    void BlobOperations::Get(Int64Container& value,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index)
    {
        Get(value.m_Value, value.m_bIsNull, value.m_bIsChanged, handle, member, index, ValueMode);
    }

    void BlobOperations::Set(const Float32Container& value,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index)
    {
        Set(value.m_Value, value.IsNull(), value.IsChanged(), handle, member, index, ValueMode);
    }

    void BlobOperations::Get(Float32Container& value,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index)
    {
        Get(value.m_Value, value.m_bIsNull, value.m_bIsChanged, handle, member, index, ValueMode);
    }

    void BlobOperations::Set(const Float64Container& value,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index)
    {
       Set(value.m_Value, value.IsNull(), value.IsChanged(), handle, member, index, ValueMode);
    }

    void BlobOperations::Get(Float64Container& value,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index)
    {
        Get(value.m_Value, value.m_bIsNull, value.m_bIsChanged, handle, member, index, ValueMode);
    }

    void BlobOperations::Set(const EntityIdContainer& value,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index)
    {
        Set(value.m_Value, value.IsNull(), value.IsChanged(), handle, member, index, ValueMode);
    }

    void BlobOperations::Get(EntityIdContainer& value,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index)
    {
        Get(value.m_Value, value.m_bIsNull, value.m_bIsChanged, handle, member, index, ValueMode);
    }

    void BlobOperations::Set(const InstanceIdContainer& value,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index)
    {
       Set(value.m_Value, value.IsNull(), value.IsChanged(), handle, member, index, ValueMode);
    }

    void BlobOperations::Get(InstanceIdContainer& value,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index)
    {
        Get(value.m_Value, value.m_bIsNull, value.m_bIsChanged, handle, member, index, ValueMode);
    }

    void BlobOperations::Set(const ChannelIdContainer& value,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index)
    {
        Set(value.m_Value, value.IsNull(), value.IsChanged(), handle, member, index, ValueMode);
    }

    void BlobOperations::Get(ChannelIdContainer& value,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index)
    {
        Get(value.m_Value, value.m_bIsNull, value.m_bIsChanged, handle, member, index, ValueMode);
    }



    void BlobOperations::Set(const HandlerIdContainer& value,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index)
    {
        Set(value.m_Value, value.IsNull(), value.IsChanged(), handle, member, index, ValueMode);
    }

    void BlobOperations::Get(HandlerIdContainer& value,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index)
    {
        Get(value.m_Value, value.m_bIsNull, value.m_bIsChanged, handle, member, index, ValueMode);
    }

    void BlobOperations::Set(const StringContainer& value,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index)
    {
        Set(value.m_Value, value.IsNull(), value.IsChanged(), handle, member, index, ValueMode);
    }

    void BlobOperations::Get(StringContainer& value,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index)
    {
        Get(value.m_Value, value.m_bIsNull, value.m_bIsChanged, handle, member, index, ValueMode);
    }



    void BlobOperations::Set(const ObjectContainerBase& value,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index)
    {
        Set(value.GetObjectPointer(), value.IsNull(), value.m_bIsChanged, handle, member, index, ValueMode);
    }

    void BlobOperations::Get(ObjectContainerBase& object,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index)
    {
        bool isNull;
        ObjectPtr p;
        Get(p, isNull, object.m_bIsChanged, handle, member, index, ValueMode);
        if (!isNull)
        {
            object.SetObjectPointer(p);
        }
        else
        {
            object.ResetObjectPointer();
        }
    }

    void BlobOperations::Set(const BinaryContainer& value,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index)
    {
       Set(value.m_Value, value.IsNull(), value.IsChanged(), handle, member, index, ValueMode);
    }


    void BlobOperations::Get(BinaryContainer& value,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex index)
    {
        Get(value.m_Value, value.m_bIsNull, value.m_bIsChanged, handle, member, index, ValueMode);
    }

    //-------------------------------------------------------------
    // Get value operations
    //-------------------------------------------------------------
    void BlobOperations::Get(bool& val,
                             bool& isNull,
                             bool& isChanged,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex valueIndex,
                             KeyValueMode mode)
    {
        if (mode==KeyMode)
        {
            throw std::logic_error("BlobOperation.GetBoolean called with mode=KeyMode. Only ValueMode is allowed for this type!");
        }

        DotsC_ReadBooleanMember(handle, val, isNull, isChanged, member, valueIndex, Conv(mode));
    }

    void BlobOperations::Get(Safir::Dob::Typesystem::Int32& val,
                             bool& isNull,
                             bool& isChanged,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex valueIndex,
                             KeyValueMode mode)
    {
        DotsC_ReadInt32Member(handle, val, isNull, isChanged, member, valueIndex, Conv(mode));
    }

    void BlobOperations::Get(Safir::Dob::Typesystem::Int64& val,
                             bool& isNull,
                             bool& isChanged,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex valueIndex,
                             KeyValueMode mode)
    {
        DotsC_ReadInt64Member(handle, val, isNull, isChanged, member, valueIndex, Conv(mode));
    }

    void BlobOperations::Get(Safir::Dob::Typesystem::Float32& val,
                             bool& isNull,
                             bool& isChanged,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex valueIndex,
                             KeyValueMode mode)
    {
        if (mode==KeyMode)
        {
            throw std::logic_error("BlobOperation.GetFloat32 called with mode=KeyMode. Only ValueMode is allowed for this type!");
        }

        DotsC_ReadFloat32Member(handle, val, isNull, isChanged, member, valueIndex, Conv(mode));
    }

    void BlobOperations::Get(Safir::Dob::Typesystem::Float64& val,
                             bool& isNull,
                             bool& isChanged,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex valueIndex,
                             KeyValueMode mode)
    {
        if (mode==KeyMode)
        {
            throw std::logic_error("BlobOperation.GetFloat64 called with mode=KeyMode. Only ValueMode is allowed for this type!");
        }

        DotsC_ReadFloat64Member(handle, val, isNull, isChanged, member, valueIndex, Conv(mode));
    }

    void BlobOperations::Get(std::wstring& val,
                             bool& isNull,
                             bool& isChanged,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex valueIndex,
                             KeyValueMode mode)
    {
        const char* str;
        DotsC_ReadStringMember(handle, str, isNull, isChanged, member, valueIndex, Conv(mode));
        if (!isNull)
        {
            val=Safir::Dob::Typesystem::Utilities::ToWstring(str);
        }
    }

    void BlobOperations::Get(Safir::Dob::Typesystem::InstanceId& val,
                             bool& isNull,
                             bool& isChanged,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex valueIndex,
                             KeyValueMode mode)
    {
        DotsC_Int64 hash;
        const char* optionalStr;
        DotsC_ReadHashedMember(handle, hash, optionalStr, isNull, isChanged, member, valueIndex, Conv(mode));
        if (!isNull)
        {
            val=ToHashedType<Safir::Dob::Typesystem::InstanceId>(hash, optionalStr);
        }
    }

    void BlobOperations::Get(Safir::Dob::Typesystem::HandlerId& val,
                             bool& isNull,
                             bool& isChanged,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex valueIndex,
                             KeyValueMode mode)
    {
        DotsC_Int64 hash;
        const char* optionalStr;
        DotsC_ReadHashedMember(handle, hash, optionalStr, isNull, isChanged, member, valueIndex, Conv(mode));
        if (!isNull)
        {
            val=ToHashedType<Safir::Dob::Typesystem::HandlerId>(hash, optionalStr);
        }
    }

    void BlobOperations::Get(Safir::Dob::Typesystem::ChannelId& val,
                             bool& isNull,
                             bool& isChanged,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex valueIndex,
                             KeyValueMode mode)
    {
        DotsC_Int64 hash;
        const char* optionalStr;
        DotsC_ReadHashedMember(handle, hash, optionalStr, isNull, isChanged, member, valueIndex, Conv(mode));
        if (!isNull)
        {
            val=ToHashedType<Safir::Dob::Typesystem::ChannelId>(hash, optionalStr);
        }
    }

    void BlobOperations::Get(Safir::Dob::Typesystem::EntityId& val,
                             bool& isNull,
                             bool& isChanged,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex valueIndex,
                             KeyValueMode mode)
    {
        DotsC_EntityId eid;
        const char* optionalStr;
        DotsC_ReadEntityIdMember(handle, eid, optionalStr, isNull, isChanged, member, valueIndex, Conv(mode));
        if (!isNull)
        {
            val=Safir::Dob::Typesystem::EntityId(eid.typeId, ToHashedType<Safir::Dob::Typesystem::InstanceId>(eid.instanceId, optionalStr));
        }
    }

    void BlobOperations::Get(Safir::Dob::Typesystem::ObjectPtr& val,
                             bool& isNull,
                             bool& isChanged,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex valueIndex,
                             KeyValueMode mode)
    {
        if (mode==KeyMode)
        {
            throw std::logic_error("BlobOperation.GetObject called with mode=KeyMode. Only ValueMode is allowed for this type!");
        }

        const char* blob;
        DotsC_ReadObjectMember(handle, blob, isNull, isChanged, member, valueIndex, Conv(mode));
        if (!isNull)
        {
            val=Dob::Typesystem::ObjectFactory::Instance().CreateObject(blob);
        }
        else
        {
            val.reset();
        }
    }

    void BlobOperations::Get(Safir::Dob::Typesystem::Binary& val,
                             bool& isNull,
                             bool& isChanged,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex valueIndex,
                             KeyValueMode mode)
    {
        if (mode==KeyMode)
        {
            throw std::logic_error("BlobOperation.GetBinary called with mode=KeyMode. Only ValueMode is allowed for this type!");
        }

        val.clear();
        const char* bin;
        DotsC_Int32 size;
        DotsC_ReadBinaryMember(handle, bin, size, isNull, isChanged, member, valueIndex, Conv(mode));
        if (!isNull)
        {
            val.resize(static_cast<size_t>(size));
            val.assign(bin,bin+size);
        }
    }

    //---------------------------------------------------------
    // Set values
    //--------------------------------------------------------
    void BlobOperations::Set(bool val,
                             bool isNull,
                             bool isChanged,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex valueIndex,
                             KeyValueMode mode)
    {
        if (mode==KeyMode)
        {
            throw std::logic_error("BlobOperation.Set(bool) called with mode=KeyMode. Only ValueMode is allowed for this type!");
        }

        DotsC_WriteBooleanMember(handle, val, isNull, isChanged, member, valueIndex, Conv(mode));
    }

    void BlobOperations::Set(Safir::Dob::Typesystem::Int32 val,
                             bool isNull,
                             bool isChanged,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex valueIndex,
                             KeyValueMode mode)
    {
        DotsC_WriteInt32Member(handle, val, isNull, isChanged, member, valueIndex, Conv(mode));
    }

    void BlobOperations::Set(Safir::Dob::Typesystem::Int64 val,
                             bool isNull,
                             bool isChanged,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex valueIndex,
                             KeyValueMode mode)
    {
        DotsC_WriteInt64Member(handle, val, isNull, isChanged, member, valueIndex, Conv(mode));
    }

    void BlobOperations::Set(Safir::Dob::Typesystem::Float32 val,
                             bool isNull,
                             bool isChanged,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex valueIndex,
                             KeyValueMode mode)
    {
        if (mode==KeyMode)
        {
            throw std::logic_error("BlobOperation.Set(Float32) called with mode=KeyMode. Only ValueMode is allowed for this type!");
        }

        DotsC_WriteFloat32Member(handle, val, isNull, isChanged, member, valueIndex, Conv(mode));
    }

    void BlobOperations::Set(Safir::Dob::Typesystem::Float64 val,
                             bool isNull,
                             bool isChanged,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex valueIndex,
                             KeyValueMode mode)
    {
        if (mode==KeyMode)
        {
            throw std::logic_error("BlobOperation.Set(Float64) called with mode=KeyMode. Only ValueMode is allowed for this type!");
        }

        DotsC_WriteFloat64Member(handle, val, isNull, isChanged, member, valueIndex, Conv(mode));
    }

    void BlobOperations::Set(const std::wstring& val,
                             bool isNull,
                             bool isChanged,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex valueIndex,
                             KeyValueMode mode)
    {
        std::string utf8=Safir::Dob::Typesystem::Utilities::ToUtf8(val);
        DotsC_WriteStringMember(handle, utf8.c_str(),
                                isNull, isChanged, member, valueIndex, Conv(mode));
    }

    void BlobOperations::Set(const Safir::Dob::Typesystem::InstanceId& val,
                             bool isNull,
                             bool isChanged,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex valueIndex,
                             KeyValueMode mode)
    {
        std::string utf8=Safir::Dob::Typesystem::Utilities::ToUtf8(val.GetRawString());
        const char* str=utf8.empty() ? NULL : utf8.c_str();
        DotsC_WriteHashedMember(handle, val.GetRawValue(), str, isNull, isChanged, member, valueIndex, Conv(mode));
    }

    void BlobOperations::Set(const Safir::Dob::Typesystem::HandlerId& val,
                             bool isNull,
                             bool isChanged,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex valueIndex,
                             KeyValueMode mode)
    {
        std::string utf8=Safir::Dob::Typesystem::Utilities::ToUtf8(val.GetRawString());
        const char* str=utf8.empty() ? NULL : utf8.c_str();
        DotsC_WriteHashedMember(handle, val.GetRawValue(), str, isNull, isChanged, member, valueIndex, Conv(mode));
    }

    void BlobOperations::Set(const Safir::Dob::Typesystem::ChannelId& val,
                             bool isNull,
                             bool isChanged,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex valueIndex,
                             KeyValueMode mode)
    {
        std::string utf8=Safir::Dob::Typesystem::Utilities::ToUtf8(val.GetRawString());
        const char* str=utf8.empty() ? NULL : utf8.c_str();
        DotsC_WriteHashedMember(handle, val.GetRawValue(), str, isNull, isChanged, member, valueIndex, Conv(mode));
    }

    void BlobOperations::Set(const Safir::Dob::Typesystem::EntityId& val,
                             bool isNull,
                             bool isChanged,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex valueIndex,
                             KeyValueMode mode)
    {
        std::string utf8=Safir::Dob::Typesystem::Utilities::ToUtf8(val.GetInstanceId().GetRawString());
        const char* str=utf8.empty() ? NULL : utf8.c_str();
        DotsC_EntityId eid;
        eid.instanceId = val.GetInstanceId().GetRawValue();
        eid.typeId = val.GetTypeId();
        DotsC_WriteEntityIdMember(handle, eid, str, isNull, isChanged, member, valueIndex, Conv(mode));
    }

    void BlobOperations::Set(const Safir::Dob::Typesystem::ObjectPtr& val,
                             bool isNull,
                             bool isChanged,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex valueIndex,
                             KeyValueMode mode)
    {
        if (mode==KeyMode)
        {
            throw std::logic_error("BlobOperation.Set(Object) called with mode=KeyMode. Only ValueMode is allowed for this type!");
        }

        if (isNull)
        {
            DotsC_WriteObjectMember(handle, NULL, isNull, isChanged, member, valueIndex, Conv(mode));
        }
        else
        {
            Safir::Dob::Typesystem::Int64 objHandle=DotsC_CreateBlobWriter(val->GetTypeId());
            val->WriteToBlob(objHandle);
            Binary blob(static_cast<size_t>(DotsC_CalculateBlobSize(objHandle)));
            DotsC_WriteBlob(objHandle,&blob[0]);
            DotsC_DeleteBlobWriter(objHandle);
            DotsC_WriteObjectMember(handle, &blob[0], isNull, isChanged, member, valueIndex, Conv(mode));
        }
    }

    void BlobOperations::Set(const Safir::Dob::Typesystem::Binary& val,
                             bool isNull,
                             bool isChanged,
                             Safir::Dob::Typesystem::Int64 handle,
                             const Dob::Typesystem::MemberIndex member,
                             const Dob::Typesystem::ArrayIndex valueIndex,
                             KeyValueMode mode)
    {
        if (mode==KeyMode)
        {
            throw std::logic_error("BlobOperation.Set(Binary) called with mode=KeyMode. Only ValueMode is allowed for this type!");
        }

        if (isNull || val.empty())
        {
            DotsC_WriteBinaryMember(handle, NULL, 0, isNull, isChanged, member, valueIndex, Conv(mode));
        }
        else
        {
            DotsC_WriteBinaryMember(handle, &(val[0]), static_cast<DotsC_Int32>(val.size()), isNull, isChanged, member, valueIndex, Conv(mode));
        }
    }


    /**********************************************************************
     *
     *  Helper class for reading change flags
     *
     **********************************************************************/
    BlobReadHelper::BlobReadHelper(const char* blob)
        :m_handle(DotsC_CreateBlobReader(blob))
    {
    }

    BlobReadHelper::~BlobReadHelper()
    {
        DotsC_DeleteBlobReader(m_handle);
    }

    bool BlobReadHelper::IsChanged(const Dob::Typesystem::MemberIndex member,
                                         const Dob::Typesystem::ArrayIndex index) const
    {
        bool isNull, isChanged;
        DotsC_ReadMemberStatus(m_handle, isNull, isChanged, member, index);
        return isChanged;
    }

    /**********************************************************************
     *
     *  Helper class for writing change flags and diff blobs
     *
     **********************************************************************/
    BlobWriteHelper::BlobWriteHelper(const char* blob)
        :m_handle(DotsC_CreateBlobWriterFromBlob(blob))
    {
    }

    BlobWriteHelper::~BlobWriteHelper()
    {
        DotsC_DeleteBlobWriter(m_handle);
    }

    void BlobWriteHelper::SetChangedHere(const Dob::Typesystem::MemberIndex member,
                                        Dob::Typesystem::ArrayIndex index,
                                        bool val)
    {
        DotsC_WriteChangeFlag(m_handle, member, index, val);
    }

    void BlobWriteHelper::SetAllChanged(bool val)
    {
        DotsC_WriteAllChangeFlags(m_handle, val);
    }

    bool BlobWriteHelper::Diff(const char* otherBlob)
    {
        DotsC_Handle reader=DotsC_CreateBlobReader(otherBlob);
        bool hadDiffs=DotsC_MarkChanges(reader, m_handle);
        DotsC_DeleteBlobReader(reader);
        return hadDiffs;
    }

    char* BlobWriteHelper::ToBlob() const
    {
        DotsC_Int32 size=DotsC_CalculateBlobSize(m_handle);
        char* blob=DotsC_AllocateBlob(size);
        DotsC_WriteBlob(m_handle, blob);
        return blob;
    }

    //Copy blob to preallocated memory
    DotsC_Int32 BlobWriteHelper::CalculatedSize() const
    {
        return DotsC_CalculateBlobSize(m_handle);
    }

    void BlobWriteHelper::ToBlob(char* blobBuffer) const
    {
        DotsC_WriteBlob(m_handle, blobBuffer);
    }
}
}
}
}
