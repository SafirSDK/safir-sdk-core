/******************************************************************************
*
* Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
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
#include <Safir/Dob/Typesystem/Internal/TypeRepository.h>
#include "BlobLayoutImpl.h"
#include "BasicTypes.h"

static const char * EMPTY_STRING="";

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    Offset BlobLayoutImpl::GetOffset(const char * const blob, const MemberIndex member) const
    {
        return *AnyPtrCast<Offset>(blob+OFFSET_HEADER_LENGTH+member*OFFSET_MEMBER_LENGTH);
    }

    void BlobLayoutImpl::SetOffset(char * const blob, const MemberIndex member, const Offset offset) const
    {
        *AnyPtrCast<Offset>(blob+OFFSET_HEADER_LENGTH+member*OFFSET_MEMBER_LENGTH)=offset;
    }

    DotsC_MemberStatus BlobLayoutImpl::GetStatus(const char * const blob,
                                               const MemberIndex member,
                                               const ArrayIndex index) const
    {
        //        MemberDescription* mde=Repository::classes().GetMember(GetTypeId(blob), member);
        const MemberDescription * const mde=m_repository->GetClass(GetTypeId(blob))->GetMember(member);
        const Size memberSize=BasicTypes::Instance().SizeOfType(mde->GetMemberType());
        const size_t startOfElement=GetOffset(blob, member)+(MEMBER_STATUS_LENGTH+memberSize)*index;
        return DotsC_MemberStatus(*AnyPtrCast<char>(blob + startOfElement));
    }

    void BlobLayoutImpl::CreateBlob(const TypeId typeId, char * & blob) const
    {
        const ClassDescription * const cd=m_repository->GetClass(typeId);

        if (cd == NULL)
        {
            blob=NULL;
            return;
        }

        const MemberIndex numMembers=cd->GetNumberOfMembers();
        const Size blobSize=cd->InitialSize();
        blob=new char[blobSize];

        BlobHeader & header=*AnyPtrCast<BlobHeader>(blob);
        header.size=blobSize;
        header.typeId=typeId;

        const DotsC_MemberStatus defaultStatus;
        Offset currentPos=OFFSET_HEADER_LENGTH+numMembers*OFFSET_MEMBER_LENGTH; //start of data part
        for (MemberIndex i=0; i < numMembers; i++)
        {
            SetOffset(blob, i, currentPos);
            const MemberDescription * const memberDesc=cd->GetMember(i);
            Size tmpSize=MEMBER_STATUS_LENGTH+BasicTypes::Instance().SizeOfType(memberDesc->GetMemberType());
            for (Size ai=0; ai<static_cast<Size>(memberDesc->GetArraySize()); ai++)
            {
                char* s=AnyPtrCast<char>(blob + currentPos + ai*tmpSize);

                s[0]=defaultStatus.RawValue();
                if (memberDesc->GetMemberType() == StringMemberType || memberDesc->GetMemberType() == ObjectMemberType || memberDesc->GetMemberType() == BinaryMemberType)
                {
                    Offset* dynPartOff=AnyPtrCast<Offset>(s+MEMBER_STATUS_LENGTH);
                    (*dynPartOff)=0;
                    Size* dynPartSize=AnyPtrCast<Size>(s+MEMBER_STATUS_LENGTH+OFFSET_MEMBER_LENGTH);
                    (*dynPartSize)=0;
                }
            }
            tmpSize*=memberDesc->GetArraySize();
            currentPos+=tmpSize;
        }
    }

    void BlobLayoutImpl::FormatBlob(char * const blob,
                                const Size blobSize,
                                const TypeId typeId,
                                char * & beginningOfUnused) const
    {
        const ClassDescription * cd=m_repository->GetClass(typeId);
        beginningOfUnused=blob + cd->InitialSize();
        const MemberIndex numMembers=cd->GetNumberOfMembers();
        BlobHeader & header=*AnyPtrCast<BlobHeader>(blob);
        header.size=blobSize;
        header.typeId=typeId;

        const DotsC_MemberStatus defaultStatus;
        Offset currentPos=OFFSET_HEADER_LENGTH+numMembers*OFFSET_MEMBER_LENGTH; //start of data part
        for (MemberIndex i=0; i<numMembers; i++)
        {
            SetOffset(blob, i, currentPos);
            const MemberDescription * memberDesc=cd->GetMember(i);
            Size tmpSize=MEMBER_STATUS_LENGTH+BasicTypes::Instance().SizeOfType(memberDesc->GetMemberType());
            for (Size ai=0; ai<static_cast<Size>(memberDesc->GetArraySize()); ai++)
            {
                char* s=AnyPtrCast<char> (blob + currentPos + ai*tmpSize);
                s[0]=defaultStatus.RawValue();
                if (memberDesc->GetMemberType()==StringMemberType || memberDesc->GetMemberType()==ObjectMemberType || memberDesc->GetMemberType()==BinaryMemberType)
                {
                    Offset* dynPartOff=AnyPtrCast<Offset>(s+MEMBER_STATUS_LENGTH);
                    (*dynPartOff)=0;
                    Size* dynPartSize=AnyPtrCast<Size>(s+MEMBER_STATUS_LENGTH+OFFSET_MEMBER_LENGTH);
                    (*dynPartSize)=0;
                }
            }
            tmpSize*=memberDesc->GetArraySize();
            currentPos+=tmpSize;
        }
    }

    void BlobLayoutImpl::CreateObjectMember(char * const insideBlob,
                                        const Size blobSize,
                                        const TypeId typeId,
                                        const MemberIndex member,
                                        const ArrayIndex index,
                                        const bool isChanged,
                                        char * & beginningOfUnused) const
    {
        const ClassDescription * cd=m_repository->GetClass(typeId);
        char * const childBlob=beginningOfUnused;
        beginningOfUnused += cd->InitialSize();//blobSize;
        SetDynamicOffset(insideBlob,member,index,static_cast<Offset>(childBlob - insideBlob));
        SetDynamicSize(insideBlob, member, index, blobSize);
        SetStatus(false,isChanged,insideBlob,member,index);
        const MemberIndex numMembers=cd->GetNumberOfMembers();
        BlobHeader & header=*AnyPtrCast<BlobHeader>(childBlob);
        header.size=blobSize;
        header.typeId=typeId;

        const DotsC_MemberStatus defaultStatus;
        Offset currentPos=static_cast<Offset>(OFFSET_HEADER_LENGTH+numMembers*OFFSET_MEMBER_LENGTH); //start of data part
        for (MemberIndex i=0; i<numMembers; i++)
        {
            SetOffset(childBlob, static_cast<MemberIndex>(i), currentPos);
            const MemberDescription * memberDesc=cd->GetMember(i);
            Size tmpSize=MEMBER_STATUS_LENGTH+BasicTypes::Instance().SizeOfType(memberDesc->GetMemberType());
            for (Size ai=0; ai<static_cast<Size>(memberDesc->GetArraySize()); ai++)
            {
                char* s=AnyPtrCast<char> (childBlob + currentPos + ai*tmpSize);
                s[0]=defaultStatus.RawValue();
                if (memberDesc->GetMemberType()==StringMemberType || memberDesc->GetMemberType()==ObjectMemberType || memberDesc->GetMemberType()==BinaryMemberType)
                {
                    Offset* dynPartOff=AnyPtrCast<Offset>(s+MEMBER_STATUS_LENGTH);
                    (*dynPartOff)=0;
                    Size* dynPartSize=AnyPtrCast<Size>(s+MEMBER_STATUS_LENGTH+OFFSET_MEMBER_LENGTH);
                    (*dynPartSize)=0;
                }
            }
            tmpSize*=memberDesc->GetArraySize();
            currentPos+=tmpSize;
        }
    }

    void BlobLayoutImpl::CreateStringMember(char * const insideBlob,
                                        const Int32 stringLength, //remember the null-termination!
                                        const MemberIndex member,
                                        const ArrayIndex index,
                                        const bool isChanged,
                                        char * & beginningOfUnused) const
    {
        char * const string=beginningOfUnused;
        beginningOfUnused += stringLength;
        SetDynamicOffset(insideBlob,member,index,static_cast<Offset>(string - insideBlob));
        SetDynamicSize(insideBlob, member, index, stringLength);
        SetStatus(false,isChanged,insideBlob,member,index);
    }

    void BlobLayoutImpl::CreateBinaryMember(char * const insideBlob,
                                        const Int32 binarySize,
                                        const MemberIndex member,
                                        const ArrayIndex index,
                                        const bool isChanged,
                                        char * & beginningOfUnused) const
    {
        char * const binary=beginningOfUnused;
        beginningOfUnused += binarySize;
        SetDynamicOffset(insideBlob,member,index,static_cast<Offset>(binary - insideBlob));
        SetDynamicSize(insideBlob, member, index, binarySize);
        SetStatus(false,isChanged,insideBlob,member,index);
    }

    void BlobLayoutImpl::SetStatus( bool isNull,
                                bool isChanged,
                                char * const blob,
                                MemberIndex member,
                                const ArrayIndex index) const
    {
        const MemberDescription * memberDesc=m_repository->GetClass(GetTypeId(blob))->GetMember(member);
        Size size=BasicTypes::Instance().SizeOfType(memberDesc->GetMemberType())+MEMBER_STATUS_LENGTH;
        char oldStatus = *AnyPtrCast<char>(blob + GetOffset(blob, member) + index*size);
        DotsC_MemberStatus status(oldStatus);
        status.SetNull(isNull);
        status.SetChanged(isChanged);
        *AnyPtrCast<char>(blob + GetOffset(blob, member) + index*size) = status.RawValue();
    }

    bool BlobLayoutImpl::IsAnythingChanged(const char * const blob) const
    {
        const ClassDescription * cde=m_repository->GetClass(GetTypeId(blob));
        size_t noMembers=cde->GetNumberOfMembers();
        for (size_t i=0; i<noMembers; i++)
        {
            MemberIndex member=static_cast<MemberIndex>(i);
            const MemberDescription * memberDesc=cde->GetMember(member);

            Size size=MEMBER_STATUS_LENGTH+BasicTypes::Instance().SizeOfType(memberDesc->GetMemberType());

            for (Size ix=0; ix<static_cast<Size>(memberDesc->GetArraySize()); ix++)
            {
                DotsC_MemberStatus status(*AnyPtrCast<char>(blob + GetOffset(blob, member) + ix*size));
                if (status.HasChanged())
                {
                    return true;
                }

                if (memberDesc->GetMemberType() == ObjectMemberType && !status.IsNull())
                {
                    const char * childBlob;
                    Int32 dummy=0;
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

    void BlobLayoutImpl::SetChanged(char * const blob, const bool changed) const
    {
        const ClassDescription * cde=m_repository->GetClass(GetTypeId(blob));
        size_t noMembers=cde->GetNumberOfMembers();
        for (size_t i=0; i<noMembers; i++)
        {
            MemberIndex member=static_cast<MemberIndex>(i);
            const MemberDescription * memberDesc=cde->GetMember(member);
            Size size=MEMBER_STATUS_LENGTH+BasicTypes::Instance().SizeOfType(memberDesc->GetMemberType());
            Offset memOffs=GetOffset(blob, member);
            for (Size ix=0; ix<static_cast<Size>(memberDesc->GetArraySize()); ix++)
            {
                char* s= AnyPtrCast<char>(blob + memOffs + ix*size);
                DotsC_MemberStatus status(s[0]);
                status.SetChanged(changed);
                s[0]=status.RawValue();

                if (memberDesc->GetMemberType()==ObjectMemberType)
                {
                    Offset staticOffs, dynOffs;
                    Size size;
                    GetOffsetDynamicOffsetAndSize(blob,
                                            static_cast<MemberIndex>(i), ix,
                                            staticOffs, dynOffs, size);
                    if (dynOffs != 0)
                    {
                        SetChanged(blob + dynOffs, changed);
                    }
                }
            }
        }
    }

    void BlobLayoutImpl::ResetChanged(char * blob) const
    {
        const ClassDescription * cde=m_repository->GetClass(GetTypeId(blob));
        size_t noMembers=cde->GetNumberOfMembers();
        for (size_t i=0; i<noMembers; i++)
        {
            MemberIndex member=static_cast<MemberIndex>(i);
            const MemberDescription * memberDesc=cde->GetMember(member);
            Size size=MEMBER_STATUS_LENGTH+BasicTypes::Instance().SizeOfType(memberDesc->GetMemberType());
            Offset memOffs=GetOffset(blob, member);
            for (Size ix=0; ix<static_cast<Size>(memberDesc->GetArraySize()); ix++)
            {
                char* s=AnyPtrCast<char>(blob + memOffs + ix*size);
                DotsC_MemberStatus status(s[0]);
                status.SetChanged(false);
                s[0]=status.RawValue();
            }
        }
    }

    void BlobLayoutImpl::MergeChanges(const char * const val, char * & blob) const
    {
        Int32 dummy=0;

        if (GetTypeId(val) != GetTypeId(blob))
        {
            //can't merge if not of the same type
            return;
        }

        const ClassDescription * cde=m_repository->GetClass(GetTypeId(blob));
        size_t noMembers=cde->GetNumberOfMembers();

        for (size_t i=0; i<noMembers; i++)
        {
            MemberIndex member=static_cast<MemberIndex>(i);
            const MemberDescription * memberDesc=cde->GetMember(member);

            Size size=MEMBER_STATUS_LENGTH+BasicTypes::Instance().SizeOfType(memberDesc->GetMemberType());
            for (Size ix=0; ix<static_cast<Size>(memberDesc->GetArraySize()); ix++)
            {
                DotsC_MemberStatus status(*AnyPtrCast<char>(val + GetOffset(val, member) + ix*size));
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
                                    Int32 out;
                                    GetMember<Int32>(val, member, ix, out);
                                    SetMember<Int32>(out, blob, member, ix);
                                }
                                break;
                            case Int64MemberType:
                                {
                                    Int64 out;
                                    GetMember<Int64>(val, member, ix, out);
                                    SetMember<Int64>(out, blob, member, ix);
                                }
                                break;
                            case Float32MemberType:
                                {
                                    Float32 out;
                                    GetMember<Float32>(val, member, ix, out);
                                    SetMember<Float32>(out, blob, member, ix);
                                }
                                break;
                            case Float64MemberType:
                                {
                                    Float64 out;
                                    GetMember<Float64>(val, member, ix, out);
                                    SetMember<Float64>(out, blob, member, ix);
                                }
                                break;
                            case TypeIdMemberType:
                                {
                                    TypeId out;
                                    GetMember<TypeId>(val, member, ix, out);
                                    SetMember<TypeId>(out, blob, member, ix);
                                }
                                break;
                            case InstanceIdMemberType:
                            case ChannelIdMemberType:
                            case HandlerIdMemberType:
                                {
                                    Int64 hashVal;
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
                                    Float32 out;
                                    GetMember<Float32>(val, member, ix, out);
                                    SetMember<Float32>(out, blob, member, ix);
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
                                    Float64 out;
                                    GetMember<Float64>(val, member, ix, out);
                                    SetMember<Float64>(out, blob, member, ix);
                                }
                                break;
                            }
                        }
                }
            }
        }
    }

    DotsC_MemberStatus BlobLayoutImpl::GetDynamicMember(char * blob,
                                                      const MemberIndex member,
                                                      const ArrayIndex index,
                                                      char* &val,
                                                      Int32& size) const
    {
        size = 0;
        Offset offset;
        Offset dynOffs;
        Size dynSize;
        GetOffsetDynamicOffsetAndSize(blob, member, index, offset, dynOffs, dynSize);

        const DotsC_MemberStatus status(*AnyPtrCast<char> (blob + offset));
        if (status.IsNull() || dynOffs==0)
        {
            const MemberDescription * memberDesc=m_repository->GetClass(GetTypeId(blob))->GetMember(member);
            if (!status.IsNull() &&
                (memberDesc->GetMemberType() == BinaryMemberType ||
                 memberDesc->GetMemberType() == StringMemberType))
            {
                val=const_cast<char*>(EMPTY_STRING);
            }
            else
            {
                val=NULL;
            }
        }
        else
        {
            const MemberDescription * memberDesc=m_repository->GetClass(GetTypeId(blob))->GetMember(member);
            if (memberDesc->GetMemberType() == BinaryMemberType)
            {
                size = static_cast<Int32>(dynSize);
            }
            val=blob+dynOffs;
        }
        return status;
    }

    void BlobLayoutImpl::SetDynamicMember(const char * const val,
                                      const Int32 binarySize,
                                      char * & blob,
                                      const MemberIndex member,
                                      const ArrayIndex index) const
    {
        const ClassDescription * cde=m_repository->GetClass(GetTypeId(blob));
        const MemberDescription * memberDesc=cde->GetMember(member);

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
#ifndef NDEBUG
            if (!IsOfType(GetTypeId(newBlob), memberDesc->GetTypeId()))
            {
                throw InternalException("Bad class type!",__FILE__,__LINE__);
            }
#endif
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
        Size* totSize=AnyPtrCast<Size>(newObj+OFFSET_SIZE); //set new size
        (*totSize)=newBlobSize;

        Offset currentDynOffs=0;
        MemberIndex lastMember=static_cast<MemberIndex>(cde->GetNumberOfMembers()-1);

        for (MemberIndex mem=0; mem<=lastMember; mem++)
        {
            const MemberDescription * lm=cde->GetMember(mem);
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
                    const Size memberSize=BasicTypes::Instance().SizeOfType(lm->GetMemberType());
                    const size_t startOfElement=GetOffset(blob, mem)+(MEMBER_STATUS_LENGTH+memberSize)*ix;
                    const DotsC_MemberStatus status(blob[startOfElement]);
                    if (status.HasDynamicPart())
                    { //there is only need to do anything if the member had a dynamic part in the old blob
                        *AnyPtrCast<Offset>(newObj + startOfElement + MEMBER_STATUS_LENGTH)=END_OF_STATIC + currentDynOffs;
                        const char * const dataLocation=blob + *AnyPtrCast<Offset>(blob + startOfElement + MEMBER_STATUS_LENGTH);
                        const Int32 currentStringLength=*AnyPtrCast<Int32>(dataLocation + memberSize);
                        memcpy(newObj+END_OF_STATIC+currentDynOffs,dataLocation,memberSize + sizeof(Int32) + currentStringLength);
                        currentDynOffs += memberSize + sizeof(Int32) + currentStringLength;
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

    Size BlobLayoutImpl::GetNumberOfBytesInString(const char* s, size_t maxNumberOfLetters) const
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

    void BlobLayoutImpl::GetOffsetDynamicOffsetAndSize(const char * const blob,
                                                       const MemberIndex member,
                                                       const ArrayIndex index,
                                                       Offset& staticOffs,
                                                       Offset & dynOffs,
                                                       Size & dynSize) const
    {
        staticOffs=GetOffset(blob, member)+(MEMBER_STATUS_LENGTH+DYNAMIC_MEMBER_SIZE)*index;
        dynOffs=*AnyPtrCast<Offset>(blob+staticOffs+MEMBER_STATUS_LENGTH);
        dynSize=*AnyPtrCast<Size>(blob+staticOffs+MEMBER_STATUS_LENGTH+OFFSET_MEMBER_LENGTH);

    }

    void BlobLayoutImpl::SetDynamicOffset(char * const blob,
                                      const MemberIndex member,
                                      const ArrayIndex index,
                                      const Offset dynOffs) const
    {
        Offset of=GetOffset(blob, member)+(MEMBER_STATUS_LENGTH+DYNAMIC_MEMBER_SIZE)*index;
        Offset* dof=AnyPtrCast<Offset>(blob+of+MEMBER_STATUS_LENGTH);
        (*dof)=dynOffs;
    }

    void BlobLayoutImpl::SetDynamicSize(char * const blob,
                                    const MemberIndex member,
                                    const ArrayIndex index,
                                    const Size& dynSize) const
    {
        Offset of=GetOffset(blob, member)+(MEMBER_STATUS_LENGTH+DYNAMIC_MEMBER_SIZE)*index;
        Size* dsz=AnyPtrCast<Size>(blob+of+MEMBER_STATUS_LENGTH+OFFSET_MEMBER_LENGTH);
        (*dsz)=dynSize;
    }

    bool BlobLayoutImpl::SetChangedSinceLastRead(const char * lastRead, char * current) const
    {
        Int32 dummy=0;
        bool changed=false;

        if (GetTypeId(current) != GetTypeId(lastRead))
        {   //if they're not of the same type there is no point in continuing,
            //but it would signify a change if it is a recursive call, so we return true.
            return true;
        }

        const ClassDescription * cde=m_repository->GetClass(GetTypeId(current));
        size_t noMembers=cde->GetNumberOfMembers();

        for (size_t i=0; i<noMembers; i++)
        {
            MemberIndex member=static_cast<MemberIndex>(i);
            const MemberDescription * memberDesc=cde->GetMember(member);
            Size size=MEMBER_STATUS_LENGTH+BasicTypes::Instance().SizeOfType(memberDesc->GetMemberType());
            for (Size ix=0; ix<static_cast<Size>(memberDesc->GetArraySize()); ix++)
            {
                char* currStat=AnyPtrCast<char>(current + GetOffset(current, member) + ix*size);
                DotsC_MemberStatus currStatus(currStat[0]);
                bool currentNull=currStatus.IsNull();

                const char* lastStat=AnyPtrCast<char>(lastRead + GetOffset(lastRead, member) + ix*size);
                DotsC_MemberStatus lastStatus(lastStat[0]);
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
                            ChangedSinceLast<Int32>(lastRead, current, member, ix, currStat, changed);
                        }
                        break;
                    case Int64MemberType:
                        {
                            ChangedSinceLast<Int64>(lastRead, current, member, ix, currStat, changed);
                        }
                        break;
                    case Float32MemberType:
                        {
                            ChangedSinceLast<Float32>(lastRead, current, member, ix, currStat, changed);
                        }
                        break;
                    case Float64MemberType:
                        {
                            ChangedSinceLast<Float64>(lastRead, current, member, ix, currStat, changed);
                        }
                        break;
                    case TypeIdMemberType:
                        {
                            ChangedSinceLast<TypeId>(lastRead, current, member, ix, currStat, changed);
                        }
                        break;

                    case InstanceIdMemberType:
                    case ChannelIdMemberType:
                    case HandlerIdMemberType:
                        {
                            Int64 curHashVal, lastHashVal;
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
                            Int32 cSize, lSize;
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
                            ChangedSinceLast<Float32>(lastRead, current, member, ix, currStat, changed);
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
                            ChangedSinceLast<Float64>(lastRead, current, member, ix, currStat, changed);
                        }
                        break;
                    }
                }
            }
        }
        return changed;
    }


    template <typename T>
    void BlobLayoutImpl::SetMemberWithOptionalString(const T val,
                                                 const char * const strVal,
                                                 char * & blob,
                                                 const MemberIndex member,
                                                 const ArrayIndex index) const
    {
        const size_t startOfElement=GetOffset(blob, member)+(MEMBER_STATUS_LENGTH+sizeof(T))*index;
        if (strVal == NULL) // no string, this will be easy!
        {
            *AnyPtrCast<T>(blob+startOfElement+MEMBER_STATUS_LENGTH)=val;
            DotsC_MemberStatus status;
            status.SetNull(false);
            status.SetChanged(true);
            status.SetDynamicPart(false);
            blob[startOfElement]=status.RawValue();
        }
        else
        {
            const Int32 newStringLength=static_cast<Int32>(strlen(strVal)) + 1;
            char * dataLocation=NULL;
            Int32 currentStringLength=0;
            const DotsC_MemberStatus status(blob[startOfElement]);
            const bool currentlyHasDynamicPart=status.HasDynamicPart();
            if (currentlyHasDynamicPart)
            {
                dataLocation=blob + *AnyPtrCast<Offset>(blob + startOfElement + MEMBER_STATUS_LENGTH);
                currentStringLength=*AnyPtrCast<Int32>(dataLocation + sizeof(T));
                if (newStringLength <= currentStringLength) //easy! we just fit the new string in the old.
                {
                    *AnyPtrCast<T>(dataLocation)=val;
                    *AnyPtrCast<Int32>(dataLocation + sizeof(T))=newStringLength;
                    strncpy(dataLocation + sizeof(T) + sizeof(Int32), strVal, newStringLength);
                    return;
                }
            }
            //we now have the hard cases left:
            // 1 no previous dynamic part, we need to reallocate the blob and set the dynamic part
            // 2 previous dynamic part is too small, we need to reallocate the blob and set the dynamic part.

            const ClassDescription * cde=m_repository->GetClass(GetTypeId(blob));

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
                newBlobSize += sizeof(T) + sizeof(Int32);
            }
            char * const newObj=new char[newBlobSize];
            memcpy(newObj, blob, END_OF_STATIC); //copy the static part
            Size * totSize=AnyPtrCast<Size>(newObj+OFFSET_SIZE); //set new size
            (*totSize)=newBlobSize;

            Offset currentDynOffs=0;
            const MemberIndex lastMember=static_cast<MemberIndex>(cde->GetNumberOfMembers()-1);

            for (MemberIndex mem=0; mem<=lastMember; mem++)
            {
                const MemberDescription * lm=cde->GetMember(mem);
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
                            const Size memSize=BasicTypes::Instance().SizeOfType(lm->GetMemberType());
                            const size_t startOfElement=GetOffset(blob, mem)+(MEMBER_STATUS_LENGTH+memSize)*ix;
                            const DotsC_MemberStatus status(blob[startOfElement]);
                            if (status.HasDynamicPart())
                            { //there is only need to do anything if the member had a dynamic part in the old blob
                                *AnyPtrCast<Offset>(newObj + startOfElement + MEMBER_STATUS_LENGTH)=END_OF_STATIC + currentDynOffs;
                                const char * const dataLocation=blob + *AnyPtrCast<Offset>(blob + startOfElement + MEMBER_STATUS_LENGTH);
                                const Int32 currentStringLength=*AnyPtrCast<Int32>(dataLocation + memSize);
                                memcpy(newObj+END_OF_STATIC+currentDynOffs,dataLocation,memSize + sizeof(Int32) + currentStringLength);
                                currentDynOffs += memSize + sizeof(Int32) + currentStringLength;
                            }
                        }
                        else //the member this call is all about
                        {
                            const size_t startOfElement=GetOffset(newObj, member)+(MEMBER_STATUS_LENGTH+sizeof(T))*index;
                            DotsC_MemberStatus status;
                            status.SetNull(false);
                            status.SetChanged(true);
                            status.SetDynamicPart(true);
                            newObj[startOfElement]=status.RawValue();
                            *AnyPtrCast<Offset>(newObj + startOfElement + MEMBER_STATUS_LENGTH)=END_OF_STATIC + currentDynOffs;

                            *AnyPtrCast<T>(newObj + END_OF_STATIC + currentDynOffs)=val;
                            *AnyPtrCast<Int32>(newObj + END_OF_STATIC + currentDynOffs + sizeof(T))=newStringLength;
                            strncpy(newObj + END_OF_STATIC + currentDynOffs + sizeof(T) + sizeof(Int32), strVal, newStringLength);
                            currentDynOffs += sizeof(T) + sizeof(Int32) + newStringLength;
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

    bool BlobLayoutImpl::IsOfType(TypeId theType, TypeId ofType) const
    {
        //Same type is always compatible
        if (theType==ofType)
        {
            return true;
        }

        //Check object types and handle inheritance.
        const ClassDescription* tmpClass=m_repository->GetClass(theType);
        while (tmpClass)
        {
            if (tmpClass->GetTypeId()==ofType)
                return true;

            tmpClass=tmpClass->GetBaseClass();
        }

        return false;
    }



}
}
}
}
