/******************************************************************************
*
* Copyright Saab AB, 2004-2013 (http://www.safirsdk.com)
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
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/
#include <Safir/Dob/Typesystem/Internal/BlobLayout.h>
#include "BlobLayoutImpl.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    BlobLayout::BlobLayout(const TypeRepository* repository)
        :m_impl(new BlobLayoutImpl(repository))
    {
    }

    //---------------------
    // Blob operations
    //---------------------
    //Create a blob
    void BlobLayout::CreateBlob(const DotsC_TypeId typeId, char * & blob) const
    {
        m_impl->CreateBlob(typeId, blob);

    }

    //Delete a blob created with create blob
    void BlobLayout::DeleteBlob(char * & blob) const
    {
        m_impl->DeleteBlob(blob);

    }

    //format a piece of blank memory to be a blob of a desired type
    //does not check that the size of the blob is correct.
    void BlobLayout::FormatBlob(char * const blob,
                    const DotsC_Int32 blobSize,
                    const DotsC_TypeId typeId,
                    char * & beginningOfUnused) const
    {
        m_impl->FormatBlob(blob, blobSize, typeId, beginningOfUnused);
    }

    DotsC_Int32 BlobLayout::GetSize(const char * const blob) const
    {
        return m_impl->GetSize(blob);
    }

    DotsC_TypeId BlobLayout::GetTypeId(const char * const blob) const
    {
        return m_impl->GetTypeId(blob);
    }

    bool BlobLayout::IsAnythingChanged(const char * const blob) const
    {
        return m_impl->IsAnythingChanged(blob);
    }

    void BlobLayout::SetChanged(char * const blob, const bool changed) const
    {
        m_impl->SetChanged(blob, changed);
    }

    void BlobLayout::ResetChanged(char * const blob) const
    {
        m_impl->ResetChanged(blob);
    }

    void BlobLayout::MergeChanges(const char * const val, char * & blob) const
    {
        m_impl->MergeChanges(val, blob);
    }

    bool BlobLayout::SetChangedSinceLastRead(const char * const lastRead, char * const current) const
    {
        return m_impl->SetChangedSinceLastRead(lastRead, current);
    }

    //---------------------
    // Get members
    //---------------------
    DotsC_MemberStatus BlobLayout::GetMemberStatus(const char * const blob,
                                   const DotsC_MemberIndex member,
                                   const DotsC_ArrayIndex index) const
    {
        return m_impl->GetStatus(blob, member, index);
    }

    DotsC_MemberStatus BlobLayout::GetBoolMember(const char * const blob,
                                   const DotsC_MemberIndex member,
                                   const DotsC_ArrayIndex index,
                                   bool & val) const
    {
        return m_impl->GetMember<bool>(blob, member, index, val);
    }

    DotsC_MemberStatus BlobLayout::GetInt32Member(const char * const blob,
                                   const DotsC_MemberIndex member,
                                   const DotsC_ArrayIndex index,
                                   DotsC_Int32& val) const
    {
        return m_impl->GetMember<DotsC_Int32>(blob, member, index, val);
    }

    DotsC_MemberStatus BlobLayout::GetInt64Member(const char * const blob,
                                   const DotsC_MemberIndex member,
                                   const DotsC_ArrayIndex index,
                                   DotsC_Int64& val) const
    {
        return m_impl->GetMember<DotsC_Int64>(blob, member, index, val);
    }

    DotsC_MemberStatus BlobLayout::GetFloat32Member(const char * const blob,
                                   const DotsC_MemberIndex member,
                                   const DotsC_ArrayIndex index,
                                   DotsC_Float32& val) const
    {
        return m_impl->GetMember<DotsC_Float32>(blob, member, index, val);
    }

    DotsC_MemberStatus BlobLayout::GetFloat64Member(const char * const blob,
                                   const DotsC_MemberIndex member,
                                   const DotsC_ArrayIndex index,
                                   DotsC_Float64& val) const
    {
        return m_impl->GetMember<DotsC_Float64>(blob, member, index, val);
    }

    DotsC_MemberStatus BlobLayout::GetHashedMember(const char * const blob,
                                       const DotsC_MemberIndex member,
                                       const DotsC_ArrayIndex index,
                                       DotsC_Int64 & hashVal,
                                       const char * & strVal) const
    {
        return m_impl->GetMemberWithOptionalString(blob, member, index, hashVal, strVal);
    }

    DotsC_MemberStatus BlobLayout::GetDynamicMember(const char * const blob,
                                          const DotsC_MemberIndex member,
                                          const DotsC_ArrayIndex index,
                                          const char * & val, //out
                                          DotsC_Int32 & binarySize) const
    {
        return m_impl->GetDynamicMember(blob, member, index, val, binarySize);
    }


    ///---------------------
    // Set members
    //----------------------
    void BlobLayout::SetMemberStatus(const bool isNull,
                   const bool isChanged,
                   char * const blob,
                   const DotsC_MemberIndex member,
                   const DotsC_ArrayIndex index) const
    {
        m_impl->SetStatus(isNull, isChanged, blob, member, index);
    }

    void BlobLayout::SetBoolMember(const bool val,
                   char * blob,
                   const DotsC_MemberIndex member,
                   const DotsC_ArrayIndex index) const
    {
        m_impl->SetMember<bool>(val, blob, member, index);
    }

    void BlobLayout::SetInt32Member(const DotsC_Int32 val,
                   char * blob,
                   const DotsC_MemberIndex member,
                   const DotsC_ArrayIndex index) const
    {
        m_impl->SetMember<DotsC_Int32>(val, blob, member, index);
    }

    void BlobLayout::SetInt64Member(const DotsC_Int64 val,
                   char * blob,
                   const DotsC_MemberIndex member,
                   const DotsC_ArrayIndex index) const
    {
        m_impl->SetMember<DotsC_Int64>(val, blob, member, index);
    }

    void BlobLayout::SetFloat32Member(const DotsC_Float32 val,
                   char * blob,
                   const DotsC_MemberIndex member,
                   const DotsC_ArrayIndex index) const
    {
        m_impl->SetMember<DotsC_Float32>(val, blob, member, index);
    }

    void BlobLayout::SetFloat64Member(const DotsC_Float64 val,
                   char * blob,
                   const DotsC_MemberIndex member,
                   const DotsC_ArrayIndex index) const
    {
        m_impl->SetMember<DotsC_Float64>(val, blob, member, index);
    }

    void BlobLayout::SetMemberWithOptionalString(const DotsC_Int64 hashVal,
                                     const char * const strVal,
                                     char * & blob,
                                     const DotsC_MemberIndex member,
                                     const DotsC_ArrayIndex index) const
    {
        m_impl->SetMemberWithOptionalString(hashVal, strVal, blob, member, index);
    }

    void BlobLayout::SetDynamicMember(const char * const val,
                          const DotsC_Int32 binarySize, //only used if type is Binary
                          char * & blob,
                          const DotsC_MemberIndex member,
                          const DotsC_ArrayIndex index) const
    {
        m_impl->SetDynamicMember(val, binarySize, blob, member, index);
    }

    //-------------------------
    // Create dynamic members
    //-------------------------

    //the new blob will be placed at the position that beginningOfUnused points to _before_ the call
    //after the call beginningOfUnused will be updated to point past the newly created object
    void BlobLayout::CreateObjectMember(char * const insideBlob,
                            const DotsC_Int32 size,
                            const DotsC_TypeId typeId,
                            const DotsC_MemberIndex member,
                            const DotsC_ArrayIndex index,
                            const bool isChanged,
                            char * & beginningOfUnused) const
    {
        m_impl->CreateObjectMember(insideBlob, size, typeId, member, index, isChanged, beginningOfUnused);
    }

    //the new string will be placed at the position that beginningOfUnused points to _before_ the call
    //after the call beginningOfUnused will be updated to point past the newly created string
    void BlobLayout::CreateStringMember(char * const insideBlob,
                            const DotsC_Int32 stringLength, //remember the null-termination!
                            const DotsC_MemberIndex member,
                            const DotsC_ArrayIndex index,
                            const bool isChanged,
                            char * & beginningOfUnused) const
    {
        m_impl->CreateStringMember(insideBlob, stringLength, member, index, isChanged, beginningOfUnused);
    }

    //the new binary will be placed at the position that beginningOfUnused points to _before_ the call
    //after the call beginningOfUnused will be updated to point past the newly created binary
    void BlobLayout::CreateBinaryMember(char * const insideBlob,
                            const DotsC_Int32 binarySize,
                            const DotsC_MemberIndex member,
                            const DotsC_ArrayIndex index,
                            const bool isChanged,
                            char * & beginningOfUnused) const
    {
        m_impl->CreateBinaryMember(insideBlob, binarySize, member, index, isChanged, beginningOfUnused);
    }

    //if the string is non-null the new hash and string will be placed at the position that beginningOfUnused
    //if the string is null the dynamic part of the blob will be untouched.
    //points to _before_ the call. After the call beginningOfUnused will be updated to point past the newly set data.
    void BlobLayout::CreateAndSetMemberWithOptionalString(char * const blob,
                                              const DotsC_Int64 hashVal,
                                              const char * const strVal,
                                              const DotsC_Int32 stringLength,
                                              const DotsC_MemberIndex member,
                                              const DotsC_ArrayIndex index,
                                              const bool isChanged,
                                              char * & beginningOfUnused) const
    {
        m_impl->CreateAndSetMemberWithOptionalString(blob, hashVal, strVal, stringLength, member, index, isChanged, beginningOfUnused);
    }
}
}
}
}
