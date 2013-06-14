/******************************************************************************
*
* Copyright Saab AB, 2004-2008 (http://www.safirsdk.com)
*
* Created by: Joel Ottosson / stjoot
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

#ifndef __DOTS_INTERNAL_BLOBLAYOUTIMPL_H__
#define __DOTS_INTERNAL_BLOBLAYOUTIMPL_H__

#include "InternalDefs.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    class TypeRepository;

    /**
         * Operations on blobs. Creation of blobs and insertion/update of data in blobs.
         */
    class BlobLayoutImpl
    {
    private:

    #pragma pack (push)
    #pragma pack(1)

        struct BlobHeader
        {
            Size   size;
            TypeId typeId;
        };

    #pragma pack (pop)

    private:
        template <class T>
        static inline T * AnyPtrCast(char * const blob)
        {
            return static_cast<T*>(static_cast<void*>(blob));
        }

        template <class T>
        static inline const T * AnyPtrCast(const char * const blob)
        {
            return AnyPtrCast<T>(const_cast<char*>(blob));
        }


    public:
        //**********************************************************
        // Blob operations
        //**********************************************************
        BlobLayoutImpl(const TypeRepository* repository)
            :m_repository(repository)
        {
        }

        void CreateBlob(const TypeId typeId,
                        char * & blob) const;

        inline void DeleteBlob(char * & blob) const {if (blob != NULL) {delete [] blob; blob=NULL;}}

        //format a piece of blank memory to be a blob of a desired type
        //does not check that the size of the blob is correct.
        void FormatBlob(char * const blob,
                        const Size blobSize,
                        const TypeId typeId,
                        char * & beginningOfUnused) const;


        //the new blob will be placed at the position that beginningOfUnused points to _before_ the call
        //after the call beginningOfUnused will be updated to point past the newly created object
        void CreateObjectMember(char * const insideBlob,
                                const Size size,
                                const TypeId typeId,
                                const MemberIndex member,
                                const ArrayIndex index,
                                const bool isChanged,
                                char * & beginningOfUnused) const;

        //the new string will be placed at the position that beginningOfUnused points to _before_ the call
        //after the call beginningOfUnused will be updated to point past the newly created string
        void CreateStringMember(char * const insideBlob,
                                const Int32 stringLength, //remember the null-termination!
                                const MemberIndex member,
                                const ArrayIndex index,
                                const bool isChanged,
                                char * & beginningOfUnused) const;

        //the new binary will be placed at the position that beginningOfUnused points to _before_ the call
        //after the call beginningOfUnused will be updated to point past the newly created binary
        void CreateBinaryMember(char * const insideBlob,
                                const Int32 binarySize,
                                const MemberIndex member,
                                const ArrayIndex index,
                                const bool isChanged,
                                char * & beginningOfUnused) const;

        //if the string is non-null the new hash and string will be placed at the position that beginningOfUnused
        //if the string is null the dynamic part of the blob will be untouched.
        //points to _before_ the call. After the call beginningOfUnused will be updated to point past the newly set data.
        template <class T>
        void CreateAndSetMemberWithOptionalString(char * const blob,
                                                  const T hashVal,
                                                  const char * const strVal,
                                                  const Int32 stringLength,
                                                  const MemberIndex member,
                                                  const ArrayIndex index,
                                                  const bool isChanged,
                                                  char * & beginningOfUnused) const
        {
            const size_t startOfElement=GetOffset(blob, member)+(MEMBER_STATUS_LENGTH+sizeof(T))*index;
            if (strVal != NULL) //if we have a string
            {
                char * const dataLocation=beginningOfUnused;
                *AnyPtrCast<Offset>(blob + startOfElement + MEMBER_STATUS_LENGTH)=static_cast<Offset>(beginningOfUnused - blob);
                beginningOfUnused += sizeof(T) + sizeof(Int32) + stringLength; //the value + the string length + the string itself

                *AnyPtrCast<T>(dataLocation)=hashVal;
                *AnyPtrCast<Int32>(dataLocation + sizeof(T))=stringLength;
                strncpy(dataLocation + sizeof(T) + sizeof(Int32),strVal,stringLength);
            }
            else //no string
            {
                *AnyPtrCast<T>(blob+startOfElement+MEMBER_STATUS_LENGTH)=hashVal;
            }

            //Set status
            DotsC_MemberStatus memberStatus;
            memberStatus.SetNull(false);
            memberStatus.SetChanged(isChanged);
            memberStatus.SetDynamicPart(strVal!=NULL);
            blob[startOfElement]=memberStatus.RawValue();
        }

        //Get header info
        inline Size GetSize(const char * const blob) const {return AnyPtrCast<BlobHeader>(blob)->size;}
        inline TypeId GetTypeId(const char * const blob) const {return AnyPtrCast<BlobHeader>(blob)->typeId;}

        DotsC_MemberStatus GetStatus(const char * const blob,
                                       const MemberIndex member,
                                       const ArrayIndex index) const;

        void SetStatus(const bool isNull,
                       const bool isChanged,
                       char * const blob,
                       const MemberIndex member,
                       const ArrayIndex index) const;

        bool IsAnythingChanged(const char * const blob) const;
        void SetChanged(char * const blob, const bool changed) const;
        void ResetChanged(char * const blob) const;
        void MergeChanges(const char * const val,
                          char * & blob) const;
        bool SetChangedSinceLastRead(const char * const lastRead,
                                     char * const current) const;

        //Objects and Strings
        DotsC_MemberStatus GetDynamicMember(char * const blob,
                                              const MemberIndex member,
                                              const ArrayIndex index,
                                              char * & val, //out
                                              Int32 & binarySize) const; //out, only used if type is Binary

        //const version of the above
        DotsC_MemberStatus GetDynamicMember(const char * const blob,
                                              const MemberIndex member,
                                              const ArrayIndex index,
                                              const char * & val, //out
                                              Int32 & binarySize) const
        {
            return GetDynamicMember(const_cast<char * const>(blob),member,index,const_cast<char*&>(val),binarySize);
        }




        void SetDynamicMember(const char * const val,
                              const Int32 binarySize, //only used if type is Binary
                              char * & blob,
                              const MemberIndex member,
                              const ArrayIndex index) const;

        //Basic types - not string and object
        template <typename T>
        DotsC_MemberStatus GetMember(const char * const blob,
                                       const MemberIndex member,
                                       const ArrayIndex index,
                                       T & t) const
        {
            size_t startOfElement=GetOffset(blob, member)+(MEMBER_STATUS_LENGTH+sizeof(T))*index;
            t=*AnyPtrCast<T>(blob+startOfElement+MEMBER_STATUS_LENGTH);
            const char* tmp=blob + startOfElement;
            DotsC_MemberStatus s(tmp[0]);
            return s;
        }

        template <typename T>
        void SetMember(const T val,
                       char * blob,
                       const MemberIndex member,
                       const ArrayIndex index) const
        {
            size_t startOfElement=GetOffset(blob, member)+(MEMBER_STATUS_LENGTH+sizeof(T))*index;
            T* t=AnyPtrCast<T>(blob+startOfElement+MEMBER_STATUS_LENGTH);
            (*t)=val;
        }


        template <typename T>
        DotsC_MemberStatus GetMemberWithOptionalString(const char * const blob,
                                                         const MemberIndex member,
                                                         const ArrayIndex index,
                                                         T & val,
                                                         const char * & strVal) const
        {
            const size_t startOfElement=GetOffset(blob, member)+(MEMBER_STATUS_LENGTH+sizeof(T))*index;
            const DotsC_MemberStatus status(blob[startOfElement]);
            if (status.HasDynamicPart())
            {
                const Offset dynOffs=*AnyPtrCast<Offset>(blob + startOfElement + MEMBER_STATUS_LENGTH);
                val=*AnyPtrCast<T>(blob + dynOffs);
                strVal=blob + dynOffs + sizeof(T) + sizeof(Int32);
                assert(strVal[0] != '\0');
            }
            else
            {
                val=*AnyPtrCast<T>(blob + startOfElement + MEMBER_STATUS_LENGTH);
                strVal=NULL;
            }

            return status;
        }


        template <typename T>
        void SetMemberWithOptionalString(const T val,
                                         const char * const strVal,
                                         char * & blob,
                                         const MemberIndex member,
                                         const ArrayIndex index) const;

    private:
        const TypeRepository* m_repository;

        Offset GetOffset(const char * const blob, const MemberIndex member) const;
        void SetOffset(char * const blob, const MemberIndex member, const Offset offset) const;
        Size GetNumberOfBytesInString(const char* s, size_t maxNumberOfLetters) const;

        void GetOffsetDynamicOffsetAndSize(const char * const blob,
                                           const MemberIndex member,
                                           const ArrayIndex index,
                                           Offset& staticOffs,
                                           Offset & dynOffs,
                                           Size & dynSize) const;

        void SetDynamicOffset(char * const blob,
                              const MemberIndex member,
                              const ArrayIndex index,
                              const Offset dynOffs) const;

        void SetDynamicSize(char * const blob,
                            const MemberIndex member,
                            const ArrayIndex index,
                            const Size& dynSize) const;

        bool IsOfType(TypeId theType, TypeId ofTypeId) const;

        template <typename T>
        void ChangedSinceLast(const char * const lastRead,
                              const char * const current,
                              MemberIndex member,
                              int ix,
                              char * stat,
                              bool & changed) const//changed will be set to true if changed. Its never set to false
        {
            T cVal, lVal;
            GetMember<T>(current, member, ix, cVal);
            GetMember<T>(lastRead, member, ix, lVal);
            if (cVal!=lVal)
            {
                DotsC_MemberStatus status(stat[0]);
                status.SetChanged(true);
                stat[0]=status.RawValue();
                changed=true;
            }
        }

    };
}
}
}
}
#endif
