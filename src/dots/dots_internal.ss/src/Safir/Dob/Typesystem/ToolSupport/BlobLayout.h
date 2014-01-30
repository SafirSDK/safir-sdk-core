/******************************************************************************
*
* Copyright Saab AB, 2004-2013 (http://safir.sourceforge.net)
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
#ifndef __DOTS_INTERNAL_BLOB_LAYOUT_H__
#define __DOTS_INTERNAL_BLOB_LAYOUT_H__

#include <Safir/Dob/Typesystem/ToolSupport/Internal/BlobLayoutImpl.h>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace ToolSupport
{
    /**
     * @brief Operations on blobs. Creation of blobs and insertion/update of data in blobs.
     */
    template <class RepT>
    class BlobLayout
    {
    public:
        //ctor
        BlobLayout(const RepT* repository)
            :m_impl(repository)
        {
        }

        //---------------------
        // Blob operations
        //---------------------
        //Create a blob
        void CreateBlob(const DotsC_TypeId typeId, char * & blob) const
        {
            m_impl.CreateBlob(typeId, blob);
        }

        //Delete a blob created with create blob
        void DeleteBlob(char * & blob) const
        {
            m_impl.DeleteBlob(blob);

        }

        //format a piece of blank memory to be a blob of a desired type
        //does not check that the size of the blob is correct.
        void FormatBlob(char * const blob,
                        const DotsC_Int32 blobSize,
                        const DotsC_TypeId typeId,
                        char * & beginningOfUnused) const
        {
            m_impl.FormatBlob(blob, blobSize, typeId, beginningOfUnused);
        }

        DotsC_Int32 GetSize(const char * const blob) const
        {
            return m_impl.GetSize(blob);
        }

        DotsC_TypeId GetTypeId(const char * const blob) const
        {
            return m_impl.GetTypeId(blob);
        }

        bool IsAnythingChanged(const char * const blob) const
        {
            return m_impl.IsAnythingChanged(blob);
        }

        void SetChanged(char * const blob, const bool changed) const
        {
            m_impl.SetChanged(blob, changed);
        }

        void ResetChanged(char * const blob) const
        {
            m_impl.ResetChanged(blob);
        }

        void MergeChanges(const char * const val, char * & blob) const
        {
            m_impl.MergeChanges(val, blob);
        }

        bool SetChangedSinceLastRead(const char * const lastRead, char * const current) const
        {
            return m_impl.SetChangedSinceLastRead(lastRead, current);
        }

        //---------------------
        // Get members
        //---------------------
        DotsC_MemberStatus GetMemberStatus(const char * const blob,
                                       const DotsC_MemberIndex member,
                                       const DotsC_ArrayIndex index) const
        {
            return m_impl.GetStatus(blob, member, index);
        }

        DotsC_MemberStatus GetBoolMember(const char * const blob,
                                       const DotsC_MemberIndex member,
                                       const DotsC_ArrayIndex index,
                                       bool & val) const
        {
            return m_impl.template GetMember<bool>(blob, member, index, val);
        }

        DotsC_MemberStatus GetEnumMember(const char * const blob,
                                         const DotsC_MemberIndex member,
                                         const DotsC_ArrayIndex index,
                                         DotsC_EnumerationValue& val) const
        {
            return m_impl.template GetMember<DotsC_EnumerationValue>(blob, member, index, val);
        }

        DotsC_MemberStatus GetInt32Member(const char * const blob,
                                       const DotsC_MemberIndex member,
                                       const DotsC_ArrayIndex index,
                                       DotsC_Int32& val) const
        {
            return m_impl.template GetMember<DotsC_Int32>(blob, member, index, val);
        }

        DotsC_MemberStatus GetInt64Member(const char * const blob,
                                       const DotsC_MemberIndex member,
                                       const DotsC_ArrayIndex index,
                                       DotsC_Int64& val) const
        {
            return m_impl.template GetMember<DotsC_Int64>(blob, member, index, val);
        }

        DotsC_MemberStatus GetFloat32Member(const char * const blob,
                                       const DotsC_MemberIndex member,
                                       const DotsC_ArrayIndex index,
                                       DotsC_Float32& val) const
        {
            return m_impl.template GetMember<DotsC_Float32>(blob, member, index, val);
        }

        DotsC_MemberStatus GetFloat64Member(const char * const blob,
                                       const DotsC_MemberIndex member,
                                       const DotsC_ArrayIndex index,
                                       DotsC_Float64& val) const
        {
            return m_impl.template GetMember<DotsC_Float64>(blob, member, index, val);
        }

        DotsC_MemberStatus GetHashedMember(const char * const blob,
                                           const DotsC_MemberIndex member,
                                           const DotsC_ArrayIndex index,
                                           DotsC_Int64 & hashVal,
                                           const char * & strVal) const
        {
            return m_impl.template GetMemberWithOptionalString<DotsC_Int64>(blob, member, index, hashVal, strVal);
        }

        DotsC_MemberStatus GetEntityIdMember(const char * const blob,
                                             const DotsC_MemberIndex member,
                                             const DotsC_ArrayIndex index,
                                             DotsC_EntityId & entityId,
                                             const char * & strVal) const
        {
            return m_impl.template GetMemberWithOptionalString<DotsC_EntityId>(blob, member, index, entityId, strVal);
        }

        DotsC_MemberStatus GetDynamicMember(const char * const blob,
                                              const DotsC_MemberIndex member,
                                              const DotsC_ArrayIndex index,
                                              const char * & val, //out
                                              DotsC_Int32 & binarySize) const
        {
            return m_impl.GetDynamicMember(blob, member, index, val, binarySize);
        }

        DotsC_MemberStatus GetWritableDynamicMember(char* blob,
                                                    const DotsC_MemberIndex member,
                                                    const DotsC_ArrayIndex index,
                                                    char*& val, //out
                                                    DotsC_Int32& binarySize) const
        {
            return m_impl.GetDynamicMember(blob, member, index, val, binarySize);
        }


        ///---------------------
        // Set members
        //----------------------
        void SetMemberStatus(const bool isNull,
                       const bool isChanged,
                       char * const blob,
                       const DotsC_MemberIndex member,
                       const DotsC_ArrayIndex index) const
        {
            m_impl.SetStatus(isNull, isChanged, blob, member, index);
        }

        void SetBoolMember(const bool val,
                       char * blob,
                       const DotsC_MemberIndex member,
                       const DotsC_ArrayIndex index) const
        {
            m_impl.template SetMember<bool>(val, blob, member, index);
        }

        void SetEnumMember(const DotsC_EnumerationValue val,
                           char * blob,
                           const DotsC_MemberIndex member,
                           const DotsC_ArrayIndex index) const
        {
            m_impl.template SetMember<DotsC_EnumerationValue>(val, blob, member, index);
        }

        void SetInt32Member(const DotsC_Int32 val,
                       char * blob,
                       const DotsC_MemberIndex member,
                       const DotsC_ArrayIndex index) const
        {
            m_impl.template SetMember<DotsC_Int32>(val, blob, member, index);
        }

        void SetInt64Member(const DotsC_Int64 val,
                       char * blob,
                       const DotsC_MemberIndex member,
                       const DotsC_ArrayIndex index) const
        {
            m_impl.template SetMember<DotsC_Int64>(val, blob, member, index);
        }

        void SetFloat32Member(const DotsC_Float32 val,
                       char * blob,
                       const DotsC_MemberIndex member,
                       const DotsC_ArrayIndex index) const
        {
            m_impl.template SetMember<DotsC_Float32>(val, blob, member, index);
        }

        void SetFloat64Member(const DotsC_Float64 val,
                       char * blob,
                       const DotsC_MemberIndex member,
                       const DotsC_ArrayIndex index) const
        {
            m_impl.template SetMember<DotsC_Float64>(val, blob, member, index);
        }

        void SetHashedMember(const DotsC_Int64 hashVal,
                             const char * const strVal,
                             char * & blob,
                             const DotsC_MemberIndex member,
                             const DotsC_ArrayIndex index) const
        {
            m_impl.SetMemberWithOptionalString(hashVal, strVal, blob, member, index);
        }

        void SetEntityIdMember(const DotsC_EntityId entityId,
                               const char * const strVal,
                               char * & blob,
                               const DotsC_MemberIndex member,
                               const DotsC_ArrayIndex index) const
        {
            m_impl.SetMemberWithOptionalString(entityId, strVal, blob, member, index);
        }

        void SetDynamicMember(const char * const val,
                              const DotsC_Int32 binarySize, //only used if type is Binary
                              char * & blob,
                              const DotsC_MemberIndex member,
                              const DotsC_ArrayIndex index) const
        {
            m_impl.SetDynamicMember(val, binarySize, blob, member, index);
        }

        //-------------------------
        // Create dynamic members
        //-------------------------

        //the new blob will be placed at the position that beginningOfUnused points to _before_ the call
        //after the call beginningOfUnused will be updated to point past the newly created object
        void CreateObjectMember(char * const insideBlob,
                                const DotsC_Int32 size,
                                const DotsC_TypeId typeId,
                                const DotsC_MemberIndex member,
                                const DotsC_ArrayIndex index,
                                const bool isChanged,
                                char * & beginningOfUnused) const
        {
            m_impl.CreateObjectMember(insideBlob, size, typeId, member, index, isChanged, beginningOfUnused);
        }

        //the new string will be placed at the position that beginningOfUnused points to _before_ the call
        //after the call beginningOfUnused will be updated to point past the newly created string
        void CreateStringMember(char * const insideBlob,
                                const DotsC_Int32 stringLength, //remember the null-termination!
                                const DotsC_MemberIndex member,
                                const DotsC_ArrayIndex index,
                                const bool isChanged,
                                char * & beginningOfUnused) const
        {
            m_impl.CreateStringMember(insideBlob, stringLength, member, index, isChanged, beginningOfUnused);
        }

        //the new binary will be placed at the position that beginningOfUnused points to _before_ the call
        //after the call beginningOfUnused will be updated to point past the newly created binary
        void CreateBinaryMember(char * const insideBlob,
                                const DotsC_Int32 binarySize,
                                const DotsC_MemberIndex member,
                                const DotsC_ArrayIndex index,
                                const bool isChanged,
                                char * & beginningOfUnused) const
        {
            m_impl.CreateBinaryMember(insideBlob, binarySize, member, index, isChanged, beginningOfUnused);
        }

        //if the string is non-null the new hash and string will be placed at the position that beginningOfUnused
        //if the string is null the dynamic part of the blob will be untouched.
        //points to _before_ the call. After the call beginningOfUnused will be updated to point past the newly set data.
        template <class T>
        void CreateAndSetMemberWithOptionalString(char * const blob,
                                                  const T hashVal,
                                                  const char * const strVal,
                                                  const DotsC_Int32 stringLength,
                                                  const DotsC_MemberIndex member,
                                                  const DotsC_ArrayIndex index,
                                                  const bool isChanged,
                                                  char * & beginningOfUnused) const
        {
            m_impl.CreateAndSetMemberWithOptionalString<T>(blob, hashVal, strVal, stringLength, member, index, isChanged, beginningOfUnused);
        }

    private:
        const Internal::BlobLayoutImpl<RepT> m_impl;
    };
}
}
}
} //end namespace Safir::Dob::Typesystem::Internal

#endif
