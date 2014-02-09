/******************************************************************************
*
* Copyright Saab AB, 2004-2008 (http://safir.sourceforge.net)
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
* GNU General Public License for more Internals.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/

#ifndef __DOTS_INTERNAL_Internal_BLOB_LAYOUT_IMPL_H__
#define __DOTS_INTERNAL_Internal_BLOB_LAYOUT_IMPL_H__

#include <Safir/Dob/Typesystem/ToolSupport/TypeRepository.h>
#include <Safir/Dob/Typesystem/ToolSupport/Internal/InternalDefs.h>
#include <Safir/Dob/Typesystem/ToolSupport/Internal/BasicTypeOperations.h>
#include <string.h>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace ToolSupport
{
namespace Internal
{
    class MemberStatus
    {
    public:

        MemberStatus()
            :m_status(MemberStatus::NULL_FLAG_MASK) {}

        MemberStatus(char status)
            :m_status(status) {}

        char RawValue() const {return m_status;}

        bool HasChanged() const {return (m_status & CHANGE_FLAG_MASK) != 0;}
        bool IsNull() const {return (m_status & NULL_FLAG_MASK) != 0;}
        bool HasDynamicPart() const {return (m_status & SEMIDYNAMIC_FLAG_MASK) != 0;}

        void SetChanged(bool changed) {Set(changed, MemberStatus::CHANGE_FLAG_MASK);}
        void SetNull(bool isNull) {Set(isNull, MemberStatus::NULL_FLAG_MASK);}
        void SetDynamicPart(bool hasDynamicPart) {Set(hasDynamicPart, MemberStatus::SEMIDYNAMIC_FLAG_MASK);}

    private:
        char m_status;
        static const char NULL_FLAG_MASK=0x1;
        static const char CHANGE_FLAG_MASK=0x2;
        static const char SEMIDYNAMIC_FLAG_MASK=0x4; //means that a HashedId member or EntityId has a string.
        void Set(bool val, char mask)
        {
            if (val)
            {
                m_status |= mask;
            }
            else
            {
                m_status &= (0xff ^ mask);
            }
        }
    };


#pragma pack (push)
#pragma pack(4)
    /** This struct is really private to BlobLayoutImpl, but because of a GCC bug
     * which makes pragma pack inside a template be very unreliable it has to 
     * stay outside the template class. So here it is...
     */
    struct BlobHeader
    {
        Size   size;
        DotsC_TypeId typeId;
    };
#pragma pack (pop)


    BOOST_STATIC_ASSERT(sizeof(DotsC_TypeId) == 8);
    BOOST_STATIC_ASSERT(sizeof(BlobHeader) == 12);
    BOOST_STATIC_ASSERT(sizeof(MemberStatus) == 1);


    /**
    * Operations on blobs. Creation of blobs and insertion/update of data in blobs.
    */
    template <class RepT, class Traits=Safir::Dob::Typesystem::ToolSupport::TypeRepositoryTraits<RepT> >
    class BlobLayoutImpl
    {
    public:
        typedef typename Traits::RepositoryType RepositoryType;
        typedef typename Traits::ClassDescriptionType ClassDescriptionType;
        typedef typename Traits::MemberDescriptionType MemberDescriptionType;
        typedef typename Traits::PropertyDescriptionType PropertyDescriptionType;
        typedef typename Traits::ExceptionDescriptionType ExceptionDescriptionType;
        typedef typename Traits::ParameterDescriptionType ParameterDescriptionType;
        typedef typename Traits::EnumDescriptionType EnumDescriptionType;
        typedef typename Traits::MemberMappingDescriptionType MemberMappingDescriptionType;
        typedef typename Traits::PropertyMappingDescriptionType PropertyMappingDescriptionType;
        typedef typename Traits::CreateRoutineDescriptionType CreateRoutineDescriptionType;

    private:



        template <class T>
        static inline void Write(char* const blob, const T val)
        {
#ifdef NO_UNALIGNED_ACCESS
            memcpy(blob,&val,sizeof(T));
#else
            *static_cast<T*>(static_cast<void*>(blob)) = val;
#endif
        }

        template <class T>
        static inline T Read(const char* const blob)
        {
#ifdef NO_UNALIGNED_ACCESS
            T val;
            memcpy(&val,blob,sizeof(T));
            return val;
#else
            return *static_cast<const T*>(static_cast<const void*>(blob));
#endif
        }


    public:
        //**********************************************************
        // Blob operations
        //**********************************************************
        BlobLayoutImpl(const RepositoryType* repository)
            :m_repository(repository)
        {
        }

        void CreateBlob(const DotsC_TypeId typeId, char * & blob) const
        {
            const ClassDescriptionType* const cd=m_repository->GetClass(typeId);

            if (cd == NULL)
            {
                blob=NULL;
                return;
            }

            const DotsC_MemberIndex numMembers=cd->GetNumberOfMembers();
            const BlobHeader header={static_cast<Size>(cd->InitialSize()), typeId};
            blob=new char[header.size];
            Write(blob, header);

            const MemberStatus defaultStatus;
            Offset currentPos=OFFSET_HEADER_LENGTH+numMembers*OFFSET_MEMBER_LENGTH; //start of data part
            for (DotsC_MemberIndex i=0; i < numMembers; i++)
            {
                SetOffset(blob, i, currentPos);
                const MemberDescriptionType* const memberDesc=cd->GetMember(i);
                Size tmpSize=MEMBER_STATUS_LENGTH+BasicTypeOperations::SizeOfType(memberDesc->GetMemberType());
                for (Size ai=0; ai<static_cast<Size>(memberDesc->GetArraySize()); ai++)
                {
                    char* s=blob + currentPos + ai*tmpSize;
                    Write(s, defaultStatus.RawValue());
                    if (memberDesc->GetMemberType() == StringMemberType || memberDesc->GetMemberType() == ObjectMemberType || memberDesc->GetMemberType() == BinaryMemberType)
                    {
                        Write(s+MEMBER_STATUS_LENGTH, 0);
                        Write(s+MEMBER_STATUS_LENGTH+OFFSET_MEMBER_LENGTH, 0);
                    }
                }
                tmpSize*=memberDesc->GetArraySize();
                currentPos+=tmpSize;
            }
        }

        inline void DeleteBlob(char * & blob) const {if (blob != NULL) {delete [] blob; blob=NULL;}}

        //format a piece of blank memory to be a blob of a desired type
        //does not check that the size of the blob is correct.
        void FormatBlob(char * const blob,
                        const Size blobSize,
                        const DotsC_TypeId typeId,
                        char * & beginningOfUnused) const
        {
            const ClassDescriptionType* cd=m_repository->GetClass(typeId);
            beginningOfUnused=blob + cd->InitialSize();
            const DotsC_MemberIndex numMembers=cd->GetNumberOfMembers();
            const BlobHeader header={blobSize, typeId};
            Write(blob, header);

            const MemberStatus defaultStatus;
            Offset currentPos=OFFSET_HEADER_LENGTH+numMembers*OFFSET_MEMBER_LENGTH; //start of data part
            for (DotsC_MemberIndex i=0; i<numMembers; i++)
            {
                SetOffset(blob, i, currentPos);
                const MemberDescriptionType* memberDesc=cd->GetMember(i);
                Size tmpSize=MEMBER_STATUS_LENGTH+BasicTypeOperations::SizeOfType(memberDesc->GetMemberType());
                for (Size ai=0; ai<static_cast<Size>(memberDesc->GetArraySize()); ai++)
                {
                    char* s=blob + currentPos + ai*tmpSize;
                    Write(s, defaultStatus.RawValue());
                    if (memberDesc->GetMemberType()==StringMemberType || memberDesc->GetMemberType()==ObjectMemberType || memberDesc->GetMemberType()==BinaryMemberType)
                    {
                        Write(s+MEMBER_STATUS_LENGTH, 0);
                        Write(s+MEMBER_STATUS_LENGTH+OFFSET_MEMBER_LENGTH, 0);
                    }
                }
                tmpSize*=memberDesc->GetArraySize();
                currentPos+=tmpSize;
            }
        }


        //the new blob will be placed at the position that beginningOfUnused points to _before_ the call
        //after the call beginningOfUnused will be updated to point past the newly created object
        void CreateObjectMember(char * const insideBlob,
                                const Size blobSize,
                                const DotsC_TypeId typeId,
                                const DotsC_MemberIndex member,
                                const DotsC_ArrayIndex index,
                                const bool isChanged,
                                char * & beginningOfUnused) const
        {
            const ClassDescriptionType* cd=m_repository->GetClass(typeId);
            char * const childBlob=beginningOfUnused;
            beginningOfUnused += cd->InitialSize();//blobSize;
            SetDynamicOffset(insideBlob,member,index,static_cast<Offset>(childBlob - insideBlob));
            SetDynamicSize(insideBlob, member, index, blobSize);
            SetStatus(false,isChanged,insideBlob,member,index);
            const DotsC_MemberIndex numMembers=cd->GetNumberOfMembers();
            const BlobHeader header={blobSize, typeId};
            Write(childBlob, header);

            const MemberStatus defaultStatus;
            Offset currentPos=static_cast<Offset>(OFFSET_HEADER_LENGTH+numMembers*OFFSET_MEMBER_LENGTH); //start of data part
            for (DotsC_MemberIndex i=0; i<numMembers; i++)
            {
                SetOffset(childBlob, static_cast<DotsC_MemberIndex>(i), currentPos);
                const MemberDescriptionType* memberDesc=cd->GetMember(i);
                Size tmpSize=MEMBER_STATUS_LENGTH+BasicTypeOperations::SizeOfType(memberDesc->GetMemberType());
                for (Size ai=0; ai<static_cast<Size>(memberDesc->GetArraySize()); ai++)
                {
                    char* s=childBlob+currentPos+ai*tmpSize;
                    Write(s, defaultStatus.RawValue());
                    if (memberDesc->GetMemberType()==StringMemberType || memberDesc->GetMemberType()==ObjectMemberType || memberDesc->GetMemberType()==BinaryMemberType)
                    {
                        Write(s+MEMBER_STATUS_LENGTH, 0);
                        Write(s+MEMBER_STATUS_LENGTH+OFFSET_MEMBER_LENGTH, 0);
                    }
                }
                tmpSize*=memberDesc->GetArraySize();
                currentPos+=tmpSize;
            }
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
            char * const string=beginningOfUnused;
            beginningOfUnused += stringLength;
            SetDynamicOffset(insideBlob,member,index,static_cast<Offset>(string - insideBlob));
            SetDynamicSize(insideBlob, member, index, stringLength);
            SetStatus(false,isChanged,insideBlob,member,index);
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
            char * const binary=beginningOfUnused;
            beginningOfUnused += binarySize;
            SetDynamicOffset(insideBlob,member,index,static_cast<Offset>(binary - insideBlob));
            SetDynamicSize(insideBlob, member, index, binarySize);
            SetStatus(false,isChanged,insideBlob,member,index);
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
            const size_t startOfElement=GetOffset(blob, member)+(MEMBER_STATUS_LENGTH+sizeof(T))*index;
            if (strVal != NULL) //if we have a string
            {
                char * const dataLocation=beginningOfUnused;
                Write(blob + startOfElement + MEMBER_STATUS_LENGTH, static_cast<Offset>(beginningOfUnused - blob));
                beginningOfUnused += sizeof(T) + sizeof(DotsC_Int32) + stringLength; //the value + the string length + the string itself

                Write(dataLocation, hashVal);
                Write(dataLocation + sizeof(T), stringLength);
                strncpy(dataLocation + sizeof(T) + sizeof(DotsC_Int32),strVal,stringLength);
            }
            else //no string
            {
                Write(blob+startOfElement+MEMBER_STATUS_LENGTH, hashVal);
            }

            //Set status
            MemberStatus memberStatus;
            memberStatus.SetNull(false);
            memberStatus.SetChanged(isChanged);
            memberStatus.SetDynamicPart(strVal!=NULL);
            blob[startOfElement]=memberStatus.RawValue();
        }

        //Get header info
        inline Size GetSize(const char * const blob) const {return Read<BlobHeader>(blob).size;}
        inline DotsC_TypeId GetTypeId(const char * const blob) const {return Read<BlobHeader>(blob).typeId;}

        MemberStatus GetStatus(const char * const blob,
                                     const DotsC_MemberIndex member,
                                     const DotsC_ArrayIndex index) const
        {
            const MemberDescriptionType* const mde=m_repository->GetClass(GetTypeId(blob))->GetMember(member);
            const Size memberSize=BasicTypeOperations::SizeOfType(mde->GetMemberType());
            const size_t startOfElement=GetOffset(blob, member)+(MEMBER_STATUS_LENGTH+memberSize)*index;
            return MemberStatus(Read<char>(blob+startOfElement));
        }

        void SetStatus( bool isNull,
                        bool isChanged,
                        char * const blob,
                        DotsC_MemberIndex member,
                        const DotsC_ArrayIndex index) const
        {
            const MemberDescriptionType* memberDesc=m_repository->GetClass(GetTypeId(blob))->GetMember(member);
            Size size=BasicTypeOperations::SizeOfType(memberDesc->GetMemberType())+MEMBER_STATUS_LENGTH;
            char oldStatus = Read<char>(blob + GetOffset(blob, member) + index*size);
            MemberStatus status(oldStatus);
            status.SetNull(isNull);
            status.SetChanged(isChanged);
            Write(blob+GetOffset(blob, member)+index*size, status.RawValue());
        }

        bool IsAnythingChanged(const char * const blob) const
        {
            const ClassDescriptionType* cde=m_repository->GetClass(GetTypeId(blob));
            size_t noMembers=cde->GetNumberOfMembers();
            for (size_t i=0; i<noMembers; i++)
            {
                DotsC_MemberIndex member=static_cast<DotsC_MemberIndex>(i);
                const MemberDescriptionType* memberDesc=cde->GetMember(member);

                Size size=MEMBER_STATUS_LENGTH+BasicTypeOperations::SizeOfType(memberDesc->GetMemberType());

                for (Size ix=0; ix<static_cast<Size>(memberDesc->GetArraySize()); ix++)
                {
                    MemberStatus status(Read<char>(blob + GetOffset(blob, member) + ix*size));
                    if (status.HasChanged())
                    {
                        return true;
                    }

                    if (memberDesc->GetMemberType() == ObjectMemberType && !status.IsNull())
                    {
                        const char * childBlob;
                        DotsC_Int32 dummy=0;
                        GetDynamicMember(blob,member,ix, childBlob, dummy);
                        if (IsAnythingChanged(childBlob))
                        {
                            return true;
                        }
                    }
                }
            }
            return false;
        }

        void SetChanged(char * const blob, const bool changed) const
        {
            const ClassDescriptionType* cde=m_repository->GetClass(GetTypeId(blob));
            size_t noMembers=cde->GetNumberOfMembers();
            for (size_t i=0; i<noMembers; i++)
            {
                DotsC_MemberIndex member=static_cast<DotsC_MemberIndex>(i);
                const MemberDescriptionType* memberDesc=cde->GetMember(member);
                Size size=MEMBER_STATUS_LENGTH+BasicTypeOperations::SizeOfType(memberDesc->GetMemberType());
                Offset memOffs=GetOffset(blob, member);
                for (Size ix=0; ix<static_cast<Size>(memberDesc->GetArraySize()); ix++)
                {
                    char* s=blob+memOffs+ix*size;
                    MemberStatus status(s[0]);
                    status.SetChanged(changed);
                    s[0]=status.RawValue();

                    if (memberDesc->GetMemberType()==ObjectMemberType)
                    {
                        Offset staticOffs, dynOffs;
                        Size size;
                        GetOffsetDynamicOffsetAndSize(blob,
                                                      static_cast<DotsC_MemberIndex>(i), ix,
                                                      staticOffs, dynOffs, size);
                        if (dynOffs != 0)
                        {
                            SetChanged(blob + dynOffs, changed);
                        }
                    }
                }
            }
        }

        void ResetChanged(char * blob) const
        {
            const ClassDescriptionType* cde=m_repository->GetClass(GetTypeId(blob));
            size_t noMembers=cde->GetNumberOfMembers();
            for (size_t i=0; i<noMembers; i++)
            {
                DotsC_MemberIndex member=static_cast<DotsC_MemberIndex>(i);
                const MemberDescriptionType* memberDesc=cde->GetMember(member);
                Size size=MEMBER_STATUS_LENGTH+BasicTypeOperations::SizeOfType(memberDesc->GetMemberType());
                Offset memOffs=GetOffset(blob, member);
                for (Size ix=0; ix<static_cast<Size>(memberDesc->GetArraySize()); ix++)
                {
                    char* s=blob+memOffs+ix*size;
                    MemberStatus status(s[0]);
                    status.SetChanged(false);
                    s[0]=status.RawValue();
                }
            }
        }

        void MergeChanges(const char * const val, char * & blob) const
        {
            DotsC_Int32 dummy=0;

            if (GetTypeId(val) != GetTypeId(blob))
            {
                //can't merge if not of the same type
                return;
            }

            const ClassDescriptionType* cde=m_repository->GetClass(GetTypeId(blob));
            size_t noMembers=cde->GetNumberOfMembers();

            for (size_t i=0; i<noMembers; i++)
            {
                DotsC_MemberIndex member=static_cast<DotsC_MemberIndex>(i);
                const MemberDescriptionType* memberDesc=cde->GetMember(member);

                Size size=MEMBER_STATUS_LENGTH+BasicTypeOperations::SizeOfType(memberDesc->GetMemberType());
                for (Size ix=0; ix<static_cast<Size>(memberDesc->GetArraySize()); ix++)
                {
                    MemberStatus status(Read<char>(val+GetOffset(val, member)+ix*size));
                    const bool isNull=status.IsNull();
                    const bool isChanged=status.HasChanged();

                    if (isChanged)
                    {
                        SetStatus(isNull, isChanged, blob, member, ix);
                        if (!isNull)
                        {
                            switch(memberDesc->GetMemberType())
                            {
                            case BooleanMemberType:
                            {
                                bool out;
                                GetMember<bool>(val, member, ix, out);
                                SetMember<bool>(out, blob, member, ix);
                            }
                                break;
                            case EnumerationMemberType:
                            {
                                EnumInternal out;
                                GetMember<EnumInternal>(val, member, ix, out);
                                SetMember<EnumInternal>(out, blob, member, ix);
                            }
                                break;
                            case Int32MemberType:
                            {
                                DotsC_Int32 out;
                                GetMember<DotsC_Int32>(val, member, ix, out);
                                SetMember<DotsC_Int32>(out, blob, member, ix);
                            }
                                break;
                            case Int64MemberType:
                            {
                                DotsC_Int64 out;
                                GetMember<DotsC_Int64>(val, member, ix, out);
                                SetMember<DotsC_Int64>(out, blob, member, ix);
                            }
                                break;
                            case Float32MemberType:
                            {
                                DotsC_Float32 out;
                                GetMember<DotsC_Float32>(val, member, ix, out);
                                SetMember<DotsC_Float32>(out, blob, member, ix);
                            }
                                break;
                            case Float64MemberType:
                            {
                                DotsC_Float64 out;
                                GetMember<DotsC_Float64>(val, member, ix, out);
                                SetMember<DotsC_Float64>(out, blob, member, ix);
                            }
                                break;
                            case TypeIdMemberType:
                            {
                                DotsC_TypeId out;
                                GetMember<DotsC_TypeId>(val, member, ix, out);
                                SetMember<DotsC_TypeId>(out, blob, member, ix);
                            }
                                break;
                            case InstanceIdMemberType:
                            case ChannelIdMemberType:
                            case HandlerIdMemberType:
                            {
                                DotsC_Int64 hashVal;
                                const char * strVal;
                                GetMemberWithOptionalString(val,member,ix,hashVal,strVal);
                                SetMemberWithOptionalString(hashVal,strVal,blob,member,ix);
                            }
                                break;
                            case EntityIdMemberType:
                            {
                                DotsC_EntityId out;
                                const char * instanceIdStr;
                                GetMemberWithOptionalString(val, member, ix, out, instanceIdStr);
                                SetMemberWithOptionalString(out, instanceIdStr, blob, member, ix);
                            }
                                break;
                            case StringMemberType:
                            {
                                const char* out;
                                GetDynamicMember(val, member, ix, out, dummy);
                                SetDynamicMember(out, dummy, blob, member, ix);
                            }
                                break;
                            case ObjectMemberType:
                            {
                                const char * out;
                                GetDynamicMember(val, member, ix, out, dummy);
                                SetDynamicMember(out, dummy, blob, member, ix);
                            }
                            case BinaryMemberType:
                            {
                                const char * out;
                                GetDynamicMember(val, member, ix, out, dummy);
                                SetDynamicMember(out, dummy, blob, member, ix);
                            }
                                break;
                                //SI Types
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
                            {
                                DotsC_Float32 out;
                                GetMember<DotsC_Float32>(val, member, ix, out);
                                SetMember<DotsC_Float32>(out, blob, member, ix);
                            }
                                break;
                                //SI Long Types
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
                            case Second64MemberType:
                            case SquareMeter64MemberType:
                            case Steradian64MemberType:
                            case Volt64MemberType:
                            case Watt64MemberType:
                            {
                                DotsC_Float64 out;
                                GetMember<DotsC_Float64>(val, member, ix, out);
                                SetMember<DotsC_Float64>(out, blob, member, ix);
                            }
                                break;
                            }
                        }
                    }
                }
            }
        }

        bool SetChangedSinceLastRead(const char * lastRead, char * current) const
        {
            DotsC_Int32 dummy=0;
            bool changed=false;

            if (GetTypeId(current) != GetTypeId(lastRead))
            {   //if they're not of the same type there is no point in continuing,
                //but it would signify a change if it is a recursive call, so we return true.
                return true;
            }

            const ClassDescriptionType* cde=m_repository->GetClass(GetTypeId(current));
            size_t noMembers=cde->GetNumberOfMembers();

            for (size_t i=0; i<noMembers; i++)
            {
                DotsC_MemberIndex member=static_cast<DotsC_MemberIndex>(i);
                const MemberDescriptionType* memberDesc=cde->GetMember(member);
                Size size=MEMBER_STATUS_LENGTH+BasicTypeOperations::SizeOfType(memberDesc->GetMemberType());
                for (Size ix=0; ix<static_cast<Size>(memberDesc->GetArraySize()); ix++)
                {
                    char* currStat=current+GetOffset(current, member)+ix*size;
                    MemberStatus currStatus(currStat[0]);
                    bool currentNull=currStatus.IsNull();

                    const char* lastStat=lastRead+GetOffset(lastRead, member)+ix*size;
                    MemberStatus lastStatus(lastStat[0]);
                    bool lastNull=lastStatus.IsNull();

                    if (currentNull!=lastNull) //changed from val->null or null->val
                    {
                        currStatus.SetChanged(true);
                        currStat[0]=currStatus.RawValue();
                        changed=true;
                        if (lastNull && memberDesc->GetMemberType() == ObjectMemberType)
                        {
                            char * cVal;
                            GetDynamicMember(current, member, ix, cVal, dummy);
                            SetChanged(cVal,true);
                        }
                    }
                    else if (!currentNull) //not null, compare value to see if member has changed
                    {
                        switch(memberDesc->GetMemberType())
                        {
                        case BooleanMemberType:
                        {
                            ChangedSinceLast<bool>(lastRead, current, member, ix, currStat, changed);
                        }
                            break;
                        case EnumerationMemberType:
                        {
                            ChangedSinceLast<EnumInternal>(lastRead, current, member, ix, currStat, changed);
                        }
                            break;
                        case Int32MemberType:
                        {
                            ChangedSinceLast<DotsC_Int32>(lastRead, current, member, ix, currStat, changed);
                        }
                            break;
                        case Int64MemberType:
                        {
                            ChangedSinceLast<DotsC_Int64>(lastRead, current, member, ix, currStat, changed);
                        }
                            break;
                        case Float32MemberType:
                        {
                            ChangedSinceLast<DotsC_Float32>(lastRead, current, member, ix, currStat, changed);
                        }
                            break;
                        case Float64MemberType:
                        {
                            ChangedSinceLast<DotsC_Float64>(lastRead, current, member, ix, currStat, changed);
                        }
                            break;
                        case TypeIdMemberType:
                        {
                            ChangedSinceLast<DotsC_TypeId>(lastRead, current, member, ix, currStat, changed);
                        }
                            break;

                        case InstanceIdMemberType:
                        case ChannelIdMemberType:
                        case HandlerIdMemberType:
                        {
                            DotsC_Int64 curHashVal, lastHashVal;
                            const char * curStrVal;
                            const char * lastStrVal;
                            GetMemberWithOptionalString(current,member,ix,curHashVal,curStrVal);
                            GetMemberWithOptionalString(lastRead,member,ix,lastHashVal,lastStrVal);
                            if (curHashVal != lastHashVal)
                            {
                                currStatus.SetNull(false);
                                currStatus.SetChanged(true);
                                currStatus.SetDynamicPart(curStrVal!=NULL);
                                currStat[0]=currStatus.RawValue();
                                changed=true;
                            }
                        }
                            break;

                        case EntityIdMemberType:
                        {
                            DotsC_EntityId curHashVal, lastHashVal;
                            const char * curStrVal;
                            const char * lastStrVal;
                            GetMemberWithOptionalString(current,member,ix,curHashVal,curStrVal);
                            GetMemberWithOptionalString(lastRead,member,ix,lastHashVal,lastStrVal);
                            if (curHashVal != lastHashVal)
                            {
                                currStatus.SetNull(false);
                                currStatus.SetChanged(true);
                                currStatus.SetDynamicPart(curStrVal!=NULL);
                                currStat[0]=currStatus.RawValue();
                                changed=true;
                            }
                        }
                            break;
                        case StringMemberType:
                        {
                            const char* cVal;
                            const char* lVal;
                            GetDynamicMember(current, member, ix, cVal, dummy);
                            GetDynamicMember(lastRead, member, ix, lVal, dummy);
                            if (strcmp(cVal, lVal)!=0)
                            {
                                currStatus.SetNull(false);
                                currStatus.SetChanged(true);
                                currStat[0]=currStatus.RawValue();
                                changed=true;
                            }
                        }
                            break;
                        case ObjectMemberType:
                        {
                            char * cVal;
                            const char * lVal;
                            GetDynamicMember(current, member, ix, cVal, dummy);
                            GetDynamicMember(lastRead, member, ix, lVal, dummy);
                            if (SetChangedSinceLastRead(lVal, cVal))
                            {
                                currStatus.SetNull(false);
                                currStatus.SetChanged(true);
                                currStat[0]=currStatus.RawValue();
                                changed=true;
                            }
                        }
                            break;
                        case BinaryMemberType:
                        {
                            const char * cVal;
                            const char * lVal;
                            DotsC_Int32 cSize, lSize;
                            GetDynamicMember(current, member, ix, cVal, cSize);
                            GetDynamicMember(lastRead, member, ix, lVal, lSize);
                            if ((cSize!=lSize) || (memcmp(cVal, lVal, cSize)!=0))
                            {
                                currStatus.SetNull(false);
                                currStatus.SetChanged(true);
                                currStat[0]=currStatus.RawValue();
                                changed=true;
                            }
                        }
                            break;
                            //SI Types
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
                        {
                            ChangedSinceLast<DotsC_Float32>(lastRead, current, member, ix, currStat, changed);
                        }
                            break;
                            //SI Long Types
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
                        case Second64MemberType:
                        case SquareMeter64MemberType:
                        case Steradian64MemberType:
                        case Volt64MemberType:
                        case Watt64MemberType:
                        {
                            ChangedSinceLast<DotsC_Float64>(lastRead, current, member, ix, currStat, changed);
                        }
                            break;
                        }
                    }
                }
            }
            return changed;
        }

        //Objects and Strings
        MemberStatus GetDynamicMember(char* blob,
                                            const DotsC_MemberIndex member,
                                            const DotsC_ArrayIndex index,
                                            char* &val,
                                            DotsC_Int32& size) const
        {
            size = 0;
            Offset offset;
            Offset dynOffs;
            Size dynSize;
            GetOffsetDynamicOffsetAndSize(blob, member, index, offset, dynOffs, dynSize);

            const MemberStatus status(Read<char>(blob+offset));
            if (status.IsNull() || dynOffs==0)
            {
                const MemberDescriptionType* memberDesc=m_repository->GetClass(GetTypeId(blob))->GetMember(member);
                if (!status.IsNull() &&
                        (memberDesc->GetMemberType() == BinaryMemberType ||
                         memberDesc->GetMemberType() == StringMemberType))
                {
                    static const char * EMPTY_STRING="";
                    val=const_cast<char*>(EMPTY_STRING);
                }
                else
                {
                    val=NULL;
                }
            }
            else
            {
                const MemberDescriptionType* memberDesc=m_repository->GetClass(GetTypeId(blob))->GetMember(member);
                if (memberDesc->GetMemberType() == BinaryMemberType)
                {
                    size = static_cast<DotsC_Int32>(dynSize);
                }
                val=blob+dynOffs;
            }
            return status;
        }

        //const version of the above
        MemberStatus GetDynamicMember(const char * const blob,
                                            const DotsC_MemberIndex member,
                                            const DotsC_ArrayIndex index,
                                            const char * & val, //out
                                            DotsC_Int32 & binarySize) const
        {
            return GetDynamicMember(const_cast<char*>(blob),member,index,const_cast<char*&>(val),binarySize);
        }

        void SetDynamicMember(const char * const val,
                              const DotsC_Int32 binarySize,
                              char * & blob,
                              const DotsC_MemberIndex member,
                              const DotsC_ArrayIndex index) const
        {
            const ClassDescriptionType* cde=m_repository->GetClass(GetTypeId(blob));
            const MemberDescriptionType* memberDesc=cde->GetMember(member);

            Offset staticOffset, dynamicOffset;
            Size allocatedSize;
            GetOffsetDynamicOffsetAndSize(blob, member, index, staticOffset, dynamicOffset, allocatedSize);


            Size newSize=0;

            if (memberDesc->GetMemberType()==StringMemberType)
            {
                newSize=GetNumberOfBytesInString(val, memberDesc->GetMaxLength())+1; //extra for '\0'
            }
            else if (memberDesc->GetMemberType()==BinaryMemberType)
            {
                newSize=binarySize+sizeof(Size); //add size before binary data
            }
            else //ObjectMemberType
            {
                //verify new blob is of correct type
                const char * const newBlob=val;
                newSize=GetSize(newBlob);
            }

            if (newSize<=allocatedSize) //no allocation needed
            {
                if (memberDesc->GetMemberType()==BinaryMemberType)
                {
                    memcpy(blob+dynamicOffset, &binarySize, sizeof(Size));
                    memcpy(blob+dynamicOffset+sizeof(Size), val, binarySize);
                }
                else
                {
                    memcpy(blob+dynamicOffset, val, newSize);
                }
                return;
            }

            //----- Alright, we got some hard work to do... -----

            //calculate size of the new blob
            const Offset END_OF_STATIC= cde->InitialSize();
            Size newBlobSize=GetSize(blob)+newSize-allocatedSize;
            char * const newObj=new char[newBlobSize];
            memcpy(newObj, blob, END_OF_STATIC); //copy the static part
            Write(newObj+OFFSET_SIZE, newBlobSize); //set new size

            Offset currentDynOffs=0;
            DotsC_MemberIndex lastMember=static_cast<DotsC_MemberIndex>(cde->GetNumberOfMembers()-1);

            for (DotsC_MemberIndex mem=0; mem<=lastMember; mem++)
            {
                const MemberDescriptionType* lm=cde->GetMember(mem);
                if (lm->GetMemberType()==ObjectMemberType || lm->GetMemberType()==StringMemberType || lm->GetMemberType()==BinaryMemberType)
                {
                    for (Size ix=0; ix<static_cast<Size>(lm->GetArraySize()); ix++)
                    {
                        Offset dummyStaticOffset, tmpDof;
                        Size tmpDsz;
                        GetOffsetDynamicOffsetAndSize(blob, mem, ix, dummyStaticOffset, tmpDof, tmpDsz);

                        if (mem!=member || ix!=static_cast<Size>(index))
                        {
                            memcpy(newObj+END_OF_STATIC+currentDynOffs,
                                   blob+tmpDof, tmpDsz);
                            if (tmpDsz == 0)
                            {
                                SetDynamicOffset(newObj, mem, ix, 0);
                            }
                            else
                            {
                                SetDynamicOffset(newObj, mem, ix, END_OF_STATIC+currentDynOffs);
                            }
                            currentDynOffs+=tmpDsz;

                        }
                        else //the member this call is all about
                        {
                            char * const tmpBlob=newObj+END_OF_STATIC+currentDynOffs;

                            if (memberDesc->GetMemberType()==BinaryMemberType)
                            {
                                memcpy(tmpBlob, &binarySize, sizeof(Size));
                                memcpy(tmpBlob+sizeof(Size), val, binarySize);
                            }
                            else //ObjectMemberType or StringMemberType
                            {
                                memcpy(tmpBlob, val, newSize);

                                if (memberDesc->GetMemberType()==StringMemberType)
                                    tmpBlob[newSize-1]='\0'; //if string set last byte to null
                            }

                            SetDynamicSize(newObj, mem, ix, newSize);
                            SetDynamicOffset(newObj, mem, ix, END_OF_STATIC+currentDynOffs);
                            currentDynOffs+=newSize;
                        }


                    }
                }
                else if (lm->GetMemberType() == InstanceIdMemberType ||
                         lm->GetMemberType() == ChannelIdMemberType ||
                         lm->GetMemberType() == HandlerIdMemberType  ||
                         lm->GetMemberType() == EntityIdMemberType)
                {
                    for (Size ix=0; ix<static_cast<Size>(lm->GetArraySize()); ix++)
                    {
                        const Size memberSize=BasicTypeOperations::SizeOfType(lm->GetMemberType());
                        const size_t startOfElement=GetOffset(blob, mem)+(MEMBER_STATUS_LENGTH+memberSize)*ix;
                        const MemberStatus status(blob[startOfElement]);
                        if (status.HasDynamicPart())
                        { //there is only need to do anything if the member had a dynamic part in the old blob
                            Write(newObj+startOfElement+MEMBER_STATUS_LENGTH, END_OF_STATIC+currentDynOffs);
                            const char * const dataLocation=blob+Read<Offset>(blob+startOfElement+MEMBER_STATUS_LENGTH);
                            const DotsC_Int32 currentStringLength=Read<DotsC_Int32>(dataLocation+memberSize);
                            memcpy(newObj+END_OF_STATIC+currentDynOffs,dataLocation,memberSize + sizeof(DotsC_Int32) + currentStringLength);
                            currentDynOffs += memberSize + sizeof(DotsC_Int32) + currentStringLength;
                        }
                    }
                }

            }

            //delete old blob
            DeleteBlob(blob);
            blob=newObj;

            //Assertion that we calculated the blobSize correct.
    #ifndef NDEBUG
            if (newBlobSize != static_cast<Size>(END_OF_STATIC+currentDynOffs))
                throw "Blobsize was not correct calculated!";
    #endif

        }

        //Basic types - not string and object
        template <typename T>
        MemberStatus GetMember(const char * const blob,
                                     const DotsC_MemberIndex member,
                                     const DotsC_ArrayIndex index,
                                     T & t) const
        {
            size_t startOfElement=GetOffset(blob, member)+(MEMBER_STATUS_LENGTH+sizeof(T))*index;
            t=Read<T>(blob+startOfElement+MEMBER_STATUS_LENGTH);
            const char* tmp=blob + startOfElement;
            MemberStatus s(tmp[0]);
            return s;
        }
public:

        template <typename T>
        void SetMember(const T val,
                       char * blob,
                       const DotsC_MemberIndex member,
                       const DotsC_ArrayIndex index) const
        {
            size_t startOfElement=GetOffset(blob, member)+(MEMBER_STATUS_LENGTH+sizeof(T))*index;
            Write<T>(blob+startOfElement+MEMBER_STATUS_LENGTH, val);
        }


        template <typename T>
        MemberStatus GetMemberWithOptionalString(const char * const blob,
                                                       const DotsC_MemberIndex member,
                                                       const DotsC_ArrayIndex index,
                                                       T & val,
                                                       const char * & strVal) const
        {
            const size_t startOfElement=GetOffset(blob, member)+(MEMBER_STATUS_LENGTH+sizeof(T))*index;
            const MemberStatus status(blob[startOfElement]);
            if (status.HasDynamicPart())
            {
                const Offset dynOffs=Read<Offset>(blob + startOfElement + MEMBER_STATUS_LENGTH);
                val=Read<T>(blob + dynOffs);
                strVal=blob + dynOffs + sizeof(T) + sizeof(DotsC_Int32);
                assert(strVal[0] != '\0');
            }
            else
            {
                val=Read<T>(blob + startOfElement + MEMBER_STATUS_LENGTH);
                strVal=NULL;
            }

            return status;
        }


        template <typename T>
        void SetMemberWithOptionalString(const T val,
                                         const char * const strVal,
                                         char * & blob,
                                         const DotsC_MemberIndex member,
                                         const DotsC_ArrayIndex index) const
        {
            const size_t startOfElement=GetOffset(blob, member)+(MEMBER_STATUS_LENGTH+sizeof(T))*index;
            if (strVal == NULL) // no string, this will be easy!
            {
                Write<T>(blob+startOfElement+MEMBER_STATUS_LENGTH, val);
                MemberStatus status;
                status.SetNull(false);
                status.SetChanged(true);
                status.SetDynamicPart(false);
                blob[startOfElement]=status.RawValue();
            }
            else
            {
                const DotsC_Int32 newStringLength=static_cast<DotsC_Int32>(strlen(strVal)) + 1;
                char * dataLocation=NULL;
                DotsC_Int32 currentStringLength=0;
                const MemberStatus status(blob[startOfElement]);
                const bool currentlyHasDynamicPart=status.HasDynamicPart();
                if (currentlyHasDynamicPart)
                {
                    dataLocation=blob+Read<Offset>(blob+startOfElement+MEMBER_STATUS_LENGTH);
                    currentStringLength=Read<DotsC_Int32>(dataLocation+sizeof(T));
                    if (newStringLength <= currentStringLength) //easy! we just fit the new string in the old.
                    {
                        Write(dataLocation, val);
                        Write(dataLocation+sizeof(T), newStringLength);
                        strncpy(dataLocation+sizeof(T)+sizeof(DotsC_Int32), strVal, newStringLength);
                        return;
                    }
                }
                //we now have the hard cases left:
                // 1 no previous dynamic part, we need to reallocate the blob and set the dynamic part
                // 2 previous dynamic part is too small, we need to reallocate the blob and set the dynamic part.

                const ClassDescriptionType* cde=m_repository->GetClass(GetTypeId(blob));

                //----- Alright, we got some hard work to do... -----

                //calculate size of the new blob
                const Offset END_OF_STATIC=cde->InitialSize();
                Size newBlobSize=GetSize(blob)+newStringLength;
                if (currentStringLength != 0)
                {
                    newBlobSize -= currentStringLength;
                }
                else
                {
                    newBlobSize += sizeof(T) + sizeof(DotsC_Int32);
                }
                char * const newObj=new char[newBlobSize];
                memcpy(newObj, blob, END_OF_STATIC); //copy the static part
                Write(newObj+OFFSET_SIZE, newBlobSize); //set new size

                Offset currentDynOffs=0;
                const DotsC_MemberIndex lastMember=static_cast<DotsC_MemberIndex>(cde->GetNumberOfMembers()-1);

                for (DotsC_MemberIndex mem=0; mem<=lastMember; mem++)
                {
                    const MemberDescriptionType* lm=cde->GetMember(mem);
                    if (lm->GetMemberType()==ObjectMemberType || lm->GetMemberType()==StringMemberType || lm->GetMemberType()==BinaryMemberType)
                    {
                        for (Size ix=0; ix<static_cast<Size>(lm->GetArraySize()); ix++)
                        {
                            Offset dummyStaticOffset, tmpDof;
                            Size tmpDsz;
                            GetOffsetDynamicOffsetAndSize(blob, mem, ix, dummyStaticOffset, tmpDof, tmpDsz);

                            memcpy(newObj+END_OF_STATIC+currentDynOffs,
                                   blob+tmpDof, tmpDsz);
                            if (tmpDsz == 0)
                            {
                                SetDynamicOffset(newObj, mem, ix, 0);
                            }
                            else
                            {
                                SetDynamicOffset(newObj, mem, ix, END_OF_STATIC+currentDynOffs);
                            }
                            currentDynOffs+=tmpDsz;
                        }
                    }
                    else if (lm->GetMemberType() == InstanceIdMemberType ||
                             lm->GetMemberType() == ChannelIdMemberType ||
                             lm->GetMemberType() == HandlerIdMemberType  ||
                             lm->GetMemberType() == EntityIdMemberType)
                    {
                        for (Size ix=0; ix<static_cast<Size>(lm->GetArraySize()); ix++)
                        {
                            if (mem!=member || ix!=static_cast<Size>(index))
                            {
                                const Size memSize=BasicTypeOperations::SizeOfType(lm->GetMemberType());
                                const size_t startOfElement=GetOffset(blob, mem)+(MEMBER_STATUS_LENGTH+memSize)*ix;
                                const MemberStatus status(blob[startOfElement]);
                                if (status.HasDynamicPart())
                                { //there is only need to do anything if the member had a dynamic part in the old blob
                                    Write(newObj+startOfElement+MEMBER_STATUS_LENGTH, END_OF_STATIC+currentDynOffs);
                                    const char * const dataLocation=blob+Read<Offset>(blob+startOfElement+MEMBER_STATUS_LENGTH);
                                    const DotsC_Int32 currentStringLength=Read<DotsC_Int32>(dataLocation+memSize);
                                    memcpy(newObj+END_OF_STATIC+currentDynOffs,dataLocation,memSize + sizeof(DotsC_Int32) + currentStringLength);
                                    currentDynOffs += memSize + sizeof(DotsC_Int32) + currentStringLength;
                                }
                            }
                            else //the member this call is all about
                            {
                                const size_t startOfElement=GetOffset(newObj, member)+(MEMBER_STATUS_LENGTH+sizeof(T))*index;
                                MemberStatus status;
                                status.SetNull(false);
                                status.SetChanged(true);
                                status.SetDynamicPart(true);
                                newObj[startOfElement]=status.RawValue();
                                Write<Offset>(newObj+startOfElement+MEMBER_STATUS_LENGTH, END_OF_STATIC+currentDynOffs);

                                Write(newObj + END_OF_STATIC + currentDynOffs, val);
                                Write(newObj + END_OF_STATIC + currentDynOffs + sizeof(T), newStringLength);
                                strncpy(newObj + END_OF_STATIC + currentDynOffs + sizeof(T) + sizeof(DotsC_Int32), strVal, newStringLength);
                                currentDynOffs += sizeof(T) + sizeof(DotsC_Int32) + newStringLength;
                            }
                        }
                    }
                }

                //delete old blob
                DeleteBlob(blob);
                blob=newObj;

                //Assertion that we calculated the blobSize correct.
    #ifndef NDEBUG
                if (newBlobSize != static_cast<Size>(END_OF_STATIC+currentDynOffs))
                    throw "Blobsize was not correctly calculated!";
    #endif
            }
        }

    private:
        const RepositoryType* m_repository;

        Offset GetOffset(const char * const blob, const DotsC_MemberIndex member) const
        {
            return Read<Offset>(blob+OFFSET_HEADER_LENGTH+member*OFFSET_MEMBER_LENGTH);
        }

        void SetOffset(char * const blob, const DotsC_MemberIndex member, const Offset offset) const
        {
            Write(blob+OFFSET_HEADER_LENGTH+member*OFFSET_MEMBER_LENGTH, offset);
        }

        Size GetNumberOfBytesInString(const char* s, size_t maxNumberOfLetters) const
        {
            Size noLetters=0;
            Size noChars=0;
            const unsigned char MASK       =0xC0;
            const unsigned char UTF_CONTINUE_BYTE  =0x80;
            Size length=static_cast<Size>(strlen(s));
            while (noChars<length && noLetters<maxNumberOfLetters)
            {
                if ( (s[noChars] & MASK) != UTF_CONTINUE_BYTE )
                {
                    noLetters++;
                }
                noChars++;

            }
            if (noLetters==maxNumberOfLetters && noChars<length && ( (s[noChars] & MASK) == UTF_CONTINUE_BYTE ))
                noChars++;
            return noChars;
        }

        void GetOffsetDynamicOffsetAndSize(const char * const blob,
                                           const DotsC_MemberIndex member,
                                           const DotsC_ArrayIndex index,
                                           Offset& staticOffs,
                                           Offset & dynOffs,
                                           Size & dynSize) const
        {
            staticOffs=GetOffset(blob, member)+(MEMBER_STATUS_LENGTH+DYNAMIC_MEMBER_SIZE)*index;
            dynOffs=Read<Offset>(blob+staticOffs+MEMBER_STATUS_LENGTH);
            dynSize=Read<Size>(blob+staticOffs+MEMBER_STATUS_LENGTH+OFFSET_MEMBER_LENGTH);

        }

        void SetDynamicOffset(char * const blob,
                              const DotsC_MemberIndex member,
                              const DotsC_ArrayIndex index,
                              const Offset dynOffs) const
        {
            Offset of=GetOffset(blob, member)+(MEMBER_STATUS_LENGTH+DYNAMIC_MEMBER_SIZE)*index;
            Write(blob+of+MEMBER_STATUS_LENGTH, dynOffs);
        }

        void SetDynamicSize(char * const blob,
                            const DotsC_MemberIndex member,
                            const DotsC_ArrayIndex index,
                            const Size& dynSize) const
        {
            Offset of=GetOffset(blob, member)+(MEMBER_STATUS_LENGTH+DYNAMIC_MEMBER_SIZE)*index;
            Write(blob+of+MEMBER_STATUS_LENGTH+OFFSET_MEMBER_LENGTH,dynSize);
        }

        bool IsOfType(DotsC_TypeId theType, DotsC_TypeId ofType) const
        {
            //Same type is always compatible
            if (theType==ofType)
            {
                return true;
            }

            return BasicTypeOperations::IsOfType(m_repository, ObjectMemberType, theType, ObjectMemberType, ofType);
        }

        template <typename T>
        void ChangedSinceLast(const char * const lastRead,
                              const char * const current,
                              DotsC_MemberIndex member,
                              int ix,
                              char * stat,
                              bool & changed) const//changed will be set to true if changed. Its never set to false
        {
            T cVal, lVal;
            GetMember<T>(current, member, ix, cVal);
            GetMember<T>(lastRead, member, ix, lVal);
            if (cVal!=lVal)
            {
                MemberStatus status(stat[0]);
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
}
#endif
