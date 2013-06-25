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
#ifndef __DOTS_INTERNAL_BLOB_LAYOUT_H__
#define __DOTS_INTERNAL_BLOB_LAYOUT_H__

#include <Safir/Dob/Typesystem/Internal/KernelDefs2.h>
#include <boost/shared_ptr.hpp>

#if defined _MSC_VER
    #if defined DOTS_INTERNAL_EXPORTS
        #define DOTS_API __declspec(dllexport)
    #else
        #define DOTS_API __declspec(dllimport)
        #define SAFIR_LIBRARY_NAME "dots_internal"
        #include <Safir/Utilities/Internal/AutoLink.h>
    #endif
#elif defined __GNUC__
    #define DOTS_API
    #define __cdecl
#endif

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    class TypeRepository;
    class BlobLayoutImpl;

    /**
     * @brief Operations on blobs. Creation of blobs and insertion/update of data in blobs.
     */
    class DOTS_API BlobLayout
    {
    public:
        //ctor
        BlobLayout(const TypeRepository* repository);

        //---------------------
        // Blob operations
        //---------------------
        //Create a blob
        void CreateBlob(const DotsC_TypeId typeId, char * & blob) const;

        //Delete a blob created with create blob
        void DeleteBlob(char * & blob) const;

        //format a piece of blank memory to be a blob of a desired type
        //does not check that the size of the blob is correct.
        void FormatBlob(char * const blob,
                        const DotsC_Int32 blobSize,
                        const DotsC_TypeId typeId,
                        char * & beginningOfUnused) const; //points to the beginning of the unused space

        DotsC_Int32 GetSize(const char * const blob) const;

        DotsC_TypeId GetTypeId(const char * const blob) const;

        bool IsAnythingChanged(const char * const blob) const;

        void SetChanged(char * const blob, const bool changed) const;

        void ResetChanged(char * const blob) const;

        void MergeChanges(const char * const val, char * & blob) const;

        bool SetChangedSinceLastRead(const char * const lastRead, char * const current) const;

        //---------------------
        // Get members
        //---------------------
        DotsC_MemberStatus GetMemberStatus(const char * const blob,
                                       const DotsC_MemberIndex member,
                                       const DotsC_ArrayIndex index) const;

        DotsC_MemberStatus GetBoolMember(const char * const blob,
                                       const DotsC_MemberIndex member,
                                       const DotsC_ArrayIndex index,
                                       bool & val) const;

        DotsC_MemberStatus GetInt32Member(const char * const blob,
                                       const DotsC_MemberIndex member,
                                       const DotsC_ArrayIndex index,
                                       DotsC_Int32& val) const;

        DotsC_MemberStatus GetInt64Member(const char * const blob,
                                       const DotsC_MemberIndex member,
                                       const DotsC_ArrayIndex index,
                                       DotsC_Int64& val) const;

        DotsC_MemberStatus GetFloat32Member(const char * const blob,
                                       const DotsC_MemberIndex member,
                                       const DotsC_ArrayIndex index,
                                       DotsC_Float32& val) const;

        DotsC_MemberStatus GetFloat64Member(const char * const blob,
                                       const DotsC_MemberIndex member,
                                       const DotsC_ArrayIndex index,
                                       DotsC_Float64& val) const;

        DotsC_MemberStatus GetHashedMember(const char * const blob,
                                           const DotsC_MemberIndex member,
                                           const DotsC_ArrayIndex index,
                                           DotsC_Int64 & hashVal,
                                           const char * & strVal) const;

        DotsC_MemberStatus GetDynamicMember(const char * const blob,
                                              const DotsC_MemberIndex member,
                                              const DotsC_ArrayIndex index,
                                              const char * & val, //out
                                              DotsC_Int32 & binarySize) const;


        ///---------------------
        // Set members
        //----------------------
        void SetMemberStatus(const bool isNull,
                       const bool isChanged,
                       char * const blob,
                       const DotsC_MemberIndex member,
                       const DotsC_ArrayIndex index) const;

        void SetBoolMember(const bool val,
                       char * blob,
                       const DotsC_MemberIndex member,
                       const DotsC_ArrayIndex index) const;

        void SetInt32Member(const DotsC_Int32 val,
                       char * blob,
                       const DotsC_MemberIndex member,
                       const DotsC_ArrayIndex index) const;

        void SetInt64Member(const DotsC_Int64 val,
                       char * blob,
                       const DotsC_MemberIndex member,
                       const DotsC_ArrayIndex index) const;

        void SetFloat32Member(const DotsC_Float32 val,
                       char * blob,
                       const DotsC_MemberIndex member,
                       const DotsC_ArrayIndex index) const;

        void SetFloat64Member(const DotsC_Float64 val,
                       char * blob,
                       const DotsC_MemberIndex member,
                       const DotsC_ArrayIndex index) const;

        void SetMemberWithOptionalString(const DotsC_Int64 hashVal,
                                         const char * const strVal,
                                         char * & blob,
                                         const DotsC_MemberIndex member,
                                         const DotsC_ArrayIndex index) const;

        void SetDynamicMember(const char * const val,
                              const DotsC_Int32 binarySize, //only used if type is Binary
                              char * & blob,
                              const DotsC_MemberIndex member,
                              const DotsC_ArrayIndex index) const;

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
                                char * & beginningOfUnused) const;

        //the new string will be placed at the position that beginningOfUnused points to _before_ the call
        //after the call beginningOfUnused will be updated to point past the newly created string
        void CreateStringMember(char * const insideBlob,
                                const DotsC_Int32 stringLength, //remember the null-termination!
                                const DotsC_MemberIndex member,
                                const DotsC_ArrayIndex index,
                                const bool isChanged,
                                char * & beginningOfUnused) const;

        //the new binary will be placed at the position that beginningOfUnused points to _before_ the call
        //after the call beginningOfUnused will be updated to point past the newly created binary
        void CreateBinaryMember(char * const insideBlob,
                                const DotsC_Int32 binarySize,
                                const DotsC_MemberIndex member,
                                const DotsC_ArrayIndex index,
                                const bool isChanged,
                                char * & beginningOfUnused) const;

        //if the string is non-null the new hash and string will be placed at the position that beginningOfUnused
        //if the string is null the dynamic part of the blob will be untouched.
        //points to _before_ the call. After the call beginningOfUnused will be updated to point past the newly set data.
        void CreateAndSetMemberWithOptionalString(char * const blob,
                                                  const DotsC_Int64 hashVal,
                                                  const char * const strVal,
                                                  const DotsC_Int32 stringLength,
                                                  const DotsC_MemberIndex member,
                                                  const DotsC_ArrayIndex index,
                                                  const bool isChanged,
                                                  char * & beginningOfUnused) const;

    private:

#ifdef _MSC_VER
#pragma warning(disable:4251) //waring shared_ptr needs dll-interface
#endif
        boost::shared_ptr<const BlobLayoutImpl> m_impl;
#ifdef _MSC_VER
#pragma warning(default:4251)
#endif
    };
}
}
}
} //end namespace Safir::Dob::Typesystem::Internal

#endif
