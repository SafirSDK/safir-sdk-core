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

#ifndef _dots_blob_layout_h
#define _dots_blob_layout_h

#include "dots_internal_defs.h"
#include <boost/type_traits.hpp>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    /**
     * Operations on blobs. Creation of blobs and insertion/update of data in blobs.
     */
    class BlobLayout
    {
    private:

#pragma pack (push)
#pragma pack(4)

        struct BlobHeader
        {
            Size   size;
            Size   padding;
            TypeId typeId;
        };

#pragma pack (pop)

    public:
        //**********************************************************
        // Blob specification
        //**********************************************************
        static const Size OFFSET_SIZE                   = 0;
        static const Size OFFSET_TYPE_ID                = 8;
        static const Size OFFSET_HEADER_LENGTH          = 16;

        static const Size MEMBER_STATUS_LENGTH          = sizeof(char) * 8; //1

        static const Size OFFSET_MEMBER_LENGTH          =   sizeof(Offset) * 2; //4

        static const Size DYNAMIC_MEMBER_SIZE           =   sizeof(Offset) * 2 + sizeof(Size) * 2; //8
    private:

        BOOST_STATIC_ASSERT(sizeof(BlobLayout::BlobHeader) == BlobLayout::OFFSET_HEADER_LENGTH);
        BOOST_STATIC_ASSERT(sizeof(char) == 1);
        BOOST_STATIC_ASSERT(sizeof(Offset) == 4);
        BOOST_STATIC_ASSERT(sizeof(Size) == 4);
        BOOST_STATIC_ASSERT(sizeof(Offset) <= sizeof(Int64)); //this is to ensure that an offset can fit into a hashedId member.

        template <class T>
        static inline T * AnyPtrCast(char * const blob)
        {
            CheckAlignment<T>(blob);
            return static_cast<T*>(static_cast<void*>(blob));
        }

        template <class T>
        static inline const T * AnyPtrCast(const char * const blob)
        {
            CheckAlignment<T>(blob);
            return AnyPtrCast<T>(const_cast<char*>(blob));
        }


    public:
        //**********************************************************
        // Blob operations
        //**********************************************************
        static void CreateBlob(const TypeId typeId,
                               char * & blob);

        static inline void DeleteBlob(char * & blob) {if (blob != NULL) {delete [] blob; blob = NULL;}}

        //format a piece of blank memory to be a blob of a desired type
        //does not check that the size of the blob is correct.
        static void FormatBlob(char * const blob,
                               const Size blobSize,
                               const TypeId typeId,
                               char * & beginningOfUnused); //points to the beginning of the unused space


        //the new blob will be placed at the position that beginningOfUnused points to _before_ the call
        //after the call beginningOfUnused will be updated to point past the newly created object
        static void CreateObjectMember(char * const insideBlob,
                                       const Size size,
                                       const TypeId typeId,
                                       const MemberIndex member,
                                       const ArrayIndex index,
                                       const bool isChanged,
                                       char * & beginningOfUnused);

        //the new string will be placed at the position that beginningOfUnused points to _before_ the call
        //after the call beginningOfUnused will be updated to point past the newly created string
        static void CreateStringMember(char * const insideBlob,
                                       const Int32 stringLength, //remember the null-termination!
                                       const MemberIndex member,
                                       const ArrayIndex index,
                                       const bool isChanged,
                                       char * & beginningOfUnused);

        //the new binary will be placed at the position that beginningOfUnused points to _before_ the call
        //after the call beginningOfUnused will be updated to point past the newly created binary
        static void CreateBinaryMember(char * const insideBlob,
                                       const Int32 binarySize,
                                       const MemberIndex member,
                                       const ArrayIndex index,
                                       const bool isChanged,
                                       char * & beginningOfUnused);

        //if the string is non-null the new hash and string will be placed at the position that beginningOfUnused
        //if the string is null the dynamic part of the blob will be untouched.
        //points to _before_ the call. After the call beginningOfUnused will be updated to point past the newly set data.
        template <class T>
        static void CreateAndSetMemberWithOptionalString(char * const blob,
                                                         const T hashVal,
                                                         const char * const strVal,
                                                         const Int32 stringLength,
                                                         const MemberIndex member,
                                                         const ArrayIndex index,
                                                         const bool isChanged,
                                                         char * & beginningOfUnused)
        {
            const size_t startOfElement=GetOffset(blob, member)+(MEMBER_STATUS_LENGTH+sizeof(T))*index;
            if (strVal != NULL) //if we have a string
            {
                char * const dataLocation = beginningOfUnused;
                *AnyPtrCast<Offset>(blob + startOfElement + MEMBER_STATUS_LENGTH) = static_cast<Offset>(beginningOfUnused - blob);
                beginningOfUnused += sizeof(T) + sizeof(Int32)*2 + stringLength; //the value + the string length + the string itself
                beginningOfUnused += Padding(stringLength);
                *AnyPtrCast<T>(dataLocation) = hashVal;
                *AnyPtrCast<Int32>(dataLocation + sizeof(T)) = stringLength;
                strncpy(dataLocation + sizeof(T) + sizeof(Int32)*2,strVal,stringLength);
            }
            else //no string
            {
                *AnyPtrCast<T>(blob+startOfElement+MEMBER_STATUS_LENGTH) = hashVal;
            }

            //Set status
            blob[startOfElement] = MemberStatusHandler::ToInternalFormat(false,isChanged,strVal != NULL);
        }

        //Get header info
        static inline Size GetSize(const char * const blob) {return AnyPtrCast<BlobHeader>(blob)->size;}
        static inline TypeId GetTypeId(const char * const blob) {return AnyPtrCast<BlobHeader>(blob)->typeId;}

        static InternalMemberStatus GetStatus(const char * const blob,
                                              const MemberIndex member,
                                              const ArrayIndex index);

        static void SetStatus(const bool isNull,
                              const bool isChanged,
                              char * const blob,
                              const MemberIndex member,
                              const ArrayIndex index);

        static bool IsAnythingChanged(const char * const blob);
        static void SetChanged(char * const blob, const bool changed);
        static void ResetChanged(char * const blob);
        static void MergeChanges(const char * const val,
                                 char * & blob);
        static bool SetChangedSinceLastRead(const char * const lastRead,
                                            char * const current);

        //Objects and Strings
        static InternalMemberStatus GetDynamicMember(char * const blob,
                                                     const MemberIndex member,
                                                     const ArrayIndex index,
                                                     char * & val, //out
                                                     Int32 & binarySize); //out, only used if type is Binary

        //const version of the above
        static InternalMemberStatus GetDynamicMember(const char * const blob,
                                                     const MemberIndex member,
                                                     const ArrayIndex index,
                                                     const char * & val, //out
                                                     Int32 & binarySize)
        {
            return GetDynamicMember(const_cast<char * const>(blob),member,index,const_cast<char*&>(val),binarySize);
        }




        static void SetDynamicMember(const char * const val,
                                     const Int32 binarySize, //only used if type is Binary
                                     char * & blob,
                                     const MemberIndex member,
                                     const ArrayIndex index);

        template <typename T>
        static void CheckAlignment(const char * const blob)
        {
            if (reinterpret_cast<const uintptr_t>(blob) % boost::alignment_of<T>::value != 0)
            {
                std::wcerr << "Unaligned data of type " << typeid(T).name() 
                           << ": expected = " 
                           << boost::alignment_of<T>::value 
                           << " got " 
                           << (reinterpret_cast<const uintptr_t>(blob) % boost::alignment_of<T>::value) << std::endl;                
                exit(1);
            }
        }

        //Basic types - not string and object
        template <typename T>
        static InternalMemberStatus GetMember(const char * const blob,
                                              const MemberIndex member,
                                              const ArrayIndex index,
                                              T & t)
        {
            size_t startOfElement=GetOffset(blob, member)+(MEMBER_STATUS_LENGTH+sizeof(T))*index;
            t=*AnyPtrCast<T>(blob+startOfElement+MEMBER_STATUS_LENGTH);
            const char* tmp=blob + startOfElement;
            InternalMemberStatus s=static_cast<InternalMemberStatus>(tmp[0]);
            return s;
        }

        template <typename T>
        static void SetMember(const T val,
                              char * blob,
                              const MemberIndex member,
                              const ArrayIndex index)
        {
            size_t startOfElement=GetOffset(blob, member)+(MEMBER_STATUS_LENGTH+sizeof(T))*index;
            T* t=AnyPtrCast<T>(blob+startOfElement+MEMBER_STATUS_LENGTH);
            (*t)=val;
        }


        template <typename T>
        static InternalMemberStatus GetMemberWithOptionalString(const char * const blob,
                                                                const MemberIndex member,
                                                                const ArrayIndex index,
                                                                T & val,
                                                                const char * & strVal)
        {
            const size_t startOfElement=GetOffset(blob, member)+(MEMBER_STATUS_LENGTH+sizeof(T))*index;
            const InternalMemberStatus status = blob[startOfElement];
            if (MemberStatusHandler::HasDynamicPart(status))
            {
                const Offset dynOffs = *AnyPtrCast<Offset>(blob + startOfElement + MEMBER_STATUS_LENGTH);
                val = *AnyPtrCast<T>(blob + dynOffs);
                strVal = blob + dynOffs + sizeof(T) + sizeof(Int32) * 2;
                assert(strVal[0] != '\0');
            }
            else
            {
                val = *AnyPtrCast<T>(blob + startOfElement + MEMBER_STATUS_LENGTH);
                strVal = NULL;
            }

            return status;
        }


        template <typename T>
        static void SetMemberWithOptionalString(const T val,
                                                const char * const strVal,
                                                char * & blob,
                                                const MemberIndex member,
                                                const ArrayIndex index);

    private:
        BlobLayout(); //declared but not defined to prevent creation of this class

        static Int32 Padding(const Int32 size);
        
        static Offset GetOffset(const char * const blob, const MemberIndex member);
        static void SetOffset(char * const blob, const MemberIndex member, const Offset offset);
        static Size GetNumberOfBytesInString(const char* s, size_t maxNumberOfLetters);

        static void GetDynamicOffsetAndSize(const char * const blob,
                                            const MemberIndex member,
                                            const ArrayIndex index,
                                            Offset & dynOffs,
                                            Size & dynSize);

        static void SetDynamicOffset(char * const blob,
                                     const MemberIndex member,
                                     const ArrayIndex index,
                                     const Offset dynOffs);

        static void SetDynamicSize(char * const blob,
                                   const MemberIndex member,
                                   const ArrayIndex index,
                                   const Size& dynSize);

        template <typename T>
        static void ChangedSinceLast(const char * const lastRead,
                                     const char * const current,
                                     MemberIndex member,
                                     int ix,
                                     char * stat,
                                     bool & changed) //changed will be set to true if changed. Its never set to false
        {
            T cVal, lVal;
            GetMember<T>(current, member, ix, cVal);
            GetMember<T>(lastRead, member, ix, lVal);
            if (cVal!=lVal)
            {
                MemberStatusHandler::SetChanged(stat[0],true);
                changed=true;
            }
        }

    };
}
}
}
}
#endif
