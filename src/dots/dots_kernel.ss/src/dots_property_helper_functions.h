/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
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

#ifndef __DOTS_PROPERTY_HELPER_FUNCTIONS_H__
#define __DOTS_PROPERTY_HELPER_FUNCTIONS_H__
#include "dots_blob_layout.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{

    template <class T>
    void GetPropertyValue(const char * const blob,
                          const TypeId property,
                          const MemberIndex member,
                          const ArrayIndex index,
                          T & val,
                          bool & isNull,
                          bool & isChanged,
                          DotsC_ErrorCode & errorCode)
    {
        isNull = true;
        isChanged = false;
        errorCode = NoError;
        bool isInherited;
        const PropertyMappingDescription * const pmd=Repository::Classes().FindClass(BlobLayout::GetTypeId(blob))->FindPropertyMapping(property, isInherited);

        if (pmd == NULL)
        {
            return;
        }

        const MemberMapping * const mm = pmd->GetMemberMapping(member);

        if (mm != NULL)
        {
            switch (mm->GetMappingKind())
            {
            case MappedToNull:
                break;

            case MappedToMember:
                {
                    const ClassMemberReference * cmr = mm->GetMemberReference();
                    const char * referencedBlob = blob;
                    //Follow the reference into objects (the last one is skipped since that goes into a member, not an object)
                    for (Size ii = 0; ii < cmr->size() - 1; ++ii)
                    {
                        const MemberReferenceElement ref = cmr->at(ii);
                        bool refIsNull = false, refIsChanged = false;
                        DotsC_GetObjectMember(referencedBlob, ref.m_classMember, ref.m_index, referencedBlob, refIsNull, refIsChanged);
                        if (refIsChanged) //if one level is changed then we regard the property as changed
                        {
                            isChanged = true;
                        }
                        if (refIsNull) //if one level is null the property is null
                        {
                            isNull = true;
                            errorCode = UnableToDereferenceProperty;
                            return;
                        }

                    }
                    MemberReferenceElement ref = cmr->back();

                    bool refIsChanged = false;
                    if (ref.m_index == -1)//pointing at an array, use the index from the function call
                    {
                        MemberStatusHandler::ToExternalFormat(BlobLayout::GetMember<T>(referencedBlob, ref.m_classMember, index, val), isNull, refIsChanged);
                    }
                    else
                    {
                        MemberStatusHandler::ToExternalFormat(BlobLayout::GetMember<T>(referencedBlob, ref.m_classMember, ref.m_index, val), isNull, refIsChanged);
                    }

                    if (refIsChanged) //if one level is changed then we regard the property as changed
                    {
                        isChanged = true;
                    }
                }
                break;
            case MappedToParameter:
                {
                    isNull = false;
                    const ParameterDescription * const pd=mm->GetParameter();
                    val=*(pd->Value<T>(index));
                }
                break;
            }
        }
    }

    template <class T>
    void SetPropertyValue(const T val,
                          char * & blob,
                          const TypeId property,
                          const MemberIndex member,
                          const ArrayIndex index,
                          DotsC_ErrorCode & errorCode)
    {
        errorCode = NoError;

        bool isInherited;
        const PropertyMappingDescription * const pmd=Repository::Classes().FindClass(BlobLayout::GetTypeId(blob))->FindPropertyMapping(property, isInherited);

        if (pmd == NULL)
        {
            return;
        }

        const MemberMapping * const mm = pmd->GetMemberMapping(member);

        if (mm != NULL)
        {
            switch (mm->GetMappingKind())
            {
            case MappedToNull:
                errorCode = ReadOnlyProperty;
                break;

            case MappedToMember:
                {
                    const ClassMemberReference * cmr = mm->GetMemberReference();
                    char * referencedBlob = blob;
                    //Follow the reference into objects (the last one is skipped since that goes into a member, not an object)
                    for (Size ii = 0; ii < cmr->size() - 1; ++ii)
                    {
                        const MemberReferenceElement ref = cmr->at(ii);
                        bool refIsNull = false, refIsChanged = false;
                        char * childBlob;
                        DotsC_GetWriteableObjectMember(referencedBlob, ref.m_classMember, ref.m_index, childBlob, refIsNull, refIsChanged);
                        if (refIsNull)
                        {
                            errorCode = UnableToDereferenceProperty;
                            return;
                        }

                        BlobLayout::SetStatus(false, true, referencedBlob, ref.m_classMember, ref.m_index);

                        referencedBlob = childBlob;
                    }
                    MemberReferenceElement ref = cmr->back();

                    if (ref.m_index == -1)//pointing at an array, use the index from the function call
                    {
                        BlobLayout::SetMember<T>(val, referencedBlob, ref.m_classMember, index);
                        BlobLayout::SetStatus(false, true, referencedBlob, ref.m_classMember, index);
                    }
                    else
                    {
                        BlobLayout::SetMember<T>(val,referencedBlob, ref.m_classMember, ref.m_index);
                        BlobLayout::SetStatus(false, true, referencedBlob, ref.m_classMember, ref.m_index);
                    }
                }
                break;
            case MappedToParameter:
                errorCode = ReadOnlyProperty;
                break;
            }
        }
    }

    void GetPropertyStatus(const char * const blob,
                           const TypeId property,
                           const MemberIndex member,
                           const ArrayIndex index,
                           bool & isNull,
                           bool & isChanged,
                           DotsC_ErrorCode & errorCode)
    {
        isNull = true;
        isChanged = false;
        errorCode = NoError;

        bool isInherited;
        const PropertyMappingDescription * const pmd=Repository::Classes().FindClass(BlobLayout::GetTypeId(blob))->FindPropertyMapping(property, isInherited);

        if (pmd == NULL)
        {
            return;
        }

        const MemberMapping * const mm = pmd->GetMemberMapping(member);

        if (mm != NULL)
        {
            switch (mm->GetMappingKind())
            {
            case MappedToNull:
                break;

            case MappedToMember:
                {
                    const ClassMemberReference * cmr = mm->GetMemberReference();
                    const char * referencedBlob = blob;
                    //Follow the reference into objects (the last one is skipped since that goes into a member, not an object)
                    for (Size ii = 0; ii < cmr->size() - 1; ++ii)
                    {
                        const MemberReferenceElement ref = cmr->at(ii);
                        bool refIsNull = false, refIsChanged = false;
                        DotsC_GetObjectMember(referencedBlob, ref.m_classMember, ref.m_index, referencedBlob, refIsNull, refIsChanged);
                        if (refIsChanged) //if one level is changed then we regard the property as changed
                        {
                            isChanged = true;
                        }
                        if (refIsNull) //if one level is null the property is null
                        {
                            errorCode = UnableToDereferenceProperty;
                            isNull = true;
                            return;
                        }
                    }
                    const MemberReferenceElement ref = cmr->back();

                    bool refIsChanged = false;
                    if (ref.m_index == -1)//pointing at an array, use the index from the function call
                    {
                        MemberStatusHandler::ToExternalFormat(BlobLayout::GetStatus(referencedBlob, ref.m_classMember, index), isNull, refIsChanged);
                    }
                    else
                    {
                        MemberStatusHandler::ToExternalFormat(BlobLayout::GetStatus(referencedBlob, ref.m_classMember, ref.m_index), isNull, refIsChanged);
                    }

                    if (refIsChanged) //if one level is changed then we regard the property as changed
                    {
                        isChanged = true;
                    }
                }
                break;
            case MappedToParameter:
                isNull = false;
                break;
            }
        }
    }

    void GetDynamicPropertyValue(const char * const blob,
                                 const TypeId property,
                                 const MemberIndex member,
                                 const ArrayIndex index,
                                 const char*& val,
                                 Int32& binarySize,
                                 bool & isNull,
                                 bool & isChanged,
                                 DotsC_ErrorCode & errorCode)
    {
        isNull = true;
        isChanged = false;
        errorCode = NoError;

        bool isInherited;
        const PropertyMappingDescription * const pmd=Repository::Classes().FindClass(BlobLayout::GetTypeId(blob))->FindPropertyMapping(property, isInherited);

        if (pmd == NULL)
        {
            return;
        }

        const MemberMapping * const mm = pmd->GetMemberMapping(member);

        if (mm != NULL)
        {
            switch (mm->GetMappingKind())
            {
            case MappedToNull:
                break;

            case MappedToMember:
                {
                    const ClassMemberReference * cmr = mm->GetMemberReference();
                    const char * referencedBlob = blob;
                    //Follow the reference into objects (the last one is skipped since that goes into a member, not an object)
                    for (Size ii = 0; ii < cmr->size() - 1; ++ii)
                    {
                        const MemberReferenceElement ref = cmr->at(ii);
                        bool refIsNull = false, refIsChanged = false;
                        DotsC_GetObjectMember(referencedBlob, ref.m_classMember, ref.m_index, referencedBlob, refIsNull, refIsChanged);

                        if (refIsChanged) //if one level is changed then we regard the property as changed
                        {
                            isChanged = true;
                        }
                        if (refIsNull) //if one level is null the property is null
                        {
                            isNull = true;
                            errorCode = UnableToDereferenceProperty;
                            return;
                        }
                    }
                    const MemberReferenceElement ref = cmr->back();

                    bool refIsChanged = false;
                    if (ref.m_index == -1)//pointing at an array, use the index from the function call
                    {
                        MemberStatusHandler::ToExternalFormat(BlobLayout::GetDynamicMember(referencedBlob, ref.m_classMember, index, val, binarySize), isNull, refIsChanged);
                    }
                    else
                    {
                        MemberStatusHandler::ToExternalFormat(BlobLayout::GetDynamicMember(referencedBlob, ref.m_classMember, ref.m_index, val, binarySize), isNull, refIsChanged);
                    }

                    if (refIsChanged) //if one level is changed then we regard the property as changed
                    {
                        isChanged = true;
                    }
                }
                break;
            case MappedToParameter:
                {
                    isNull = false;
                    const ParameterDescription * const pd = mm->GetParameter();
                    val=pd->Value<char>(index).get();
                }
                break;
            }
        }
    }

    void SetDynamicPropertyValue(const char* val,
                                 Size binarySize,
                                 char * & blob,
                                 const TypeId property,
                                 const MemberIndex member,
                                 const ArrayIndex index,
                                 DotsC_ErrorCode & errorCode)
    {
        errorCode = NoError;

        bool isInherited;
        const PropertyMappingDescription * const pmd=Repository::Classes().FindClass(BlobLayout::GetTypeId(blob))->FindPropertyMapping(property, isInherited);

        if (pmd == NULL)
        {
            return;
        }

        const MemberMapping * const mm = pmd->GetMemberMapping(member);

        if (mm != NULL)
        {
            switch (mm->GetMappingKind())
            {
            case MappedToNull:
                errorCode = ReadOnlyProperty;
                break;

            case MappedToMember:
                {
                    const ClassMemberReference * cmr = mm->GetMemberReference();

                    if (cmr->size() > 2)
                    {
                        errorCode = UnableToDereferenceProperty;
                        return; //cannot set property since we need recursive reallocation...
                    }

                    const MemberReferenceElement ref = cmr->back();

                    if (ref.m_index == -1)//pointing at an array, use the index from the function call
                    {
                        BlobLayout::SetDynamicMember(val, binarySize, blob /*referencedBlob*/, ref.m_classMember, index);
                        BlobLayout::SetStatus(false, true, blob /*referencedBlob*/, ref.m_classMember, index);
                    }
                    else
                    {
                        BlobLayout::SetDynamicMember(val, binarySize, blob /*referencedBlob*/, ref.m_classMember, ref.m_index);
                        BlobLayout::SetStatus(false, true, blob /*referencedBlob*/, ref.m_classMember, ref.m_index);
                    }

                }
                break;
            case MappedToParameter:
                errorCode = ReadOnlyProperty;
            }
        }
    }


    template <class T>
    void GetPropertyValueWithOptionalString(const char * const blob,
                                            const TypeId property,
                                            const MemberIndex member,
                                            const ArrayIndex index,
                                            T & val,
                                            const char * & strVal,
                                            bool & isNull,
                                            bool & isChanged,
                                            DotsC_ErrorCode & errorCode)
    {
        isNull = true;
        isChanged = false;
        errorCode = NoError;
        bool isInherited;
        const PropertyMappingDescription * const pmd=Repository::Classes().FindClass(BlobLayout::GetTypeId(blob))->FindPropertyMapping(property, isInherited);

        if (pmd == NULL)
        {
            return;
        }

        const MemberMapping * const mm = pmd->GetMemberMapping(member);

        if (mm != NULL)
        {
            switch (mm->GetMappingKind())
            {
            case MappedToNull:
                break;

            case MappedToMember:
                {
                    const ClassMemberReference * cmr = mm->GetMemberReference();
                    const char * referencedBlob = blob;
                    //Follow the reference into objects (the last one is skipped since that goes into a member, not an object)
                    for (Size ii = 0; ii < cmr->size() - 1; ++ii)
                    {
                        const MemberReferenceElement ref = cmr->at(ii);
                        bool refIsNull = false, refIsChanged = false;
                        DotsC_GetObjectMember(referencedBlob, ref.m_classMember, ref.m_index, referencedBlob, refIsNull, refIsChanged);
                        if (refIsChanged) //if one level is changed then we regard the property as changed
                        {
                            isChanged = true;
                        }
                        if (refIsNull) //if one level is null the property is null
                        {
                            isNull = true;
                            errorCode = UnableToDereferenceProperty;
                            return;
                        }

                    }
                    MemberReferenceElement ref = cmr->back();

                    bool refIsChanged = false;
                    if (ref.m_index == -1)//pointing at an array, use the index from the function call
                    {
                        MemberStatusHandler::ToExternalFormat
                            (BlobLayout::GetMemberWithOptionalString(referencedBlob, ref.m_classMember, index, val, strVal), isNull, refIsChanged);
                    }
                    else
                    {
                        MemberStatusHandler::ToExternalFormat
                            (BlobLayout::GetMemberWithOptionalString(referencedBlob, ref.m_classMember, ref.m_index, val, strVal), isNull, refIsChanged);
                    }

                    if (refIsChanged) //if one level is changed then we regard the property as changed
                    {
                        isChanged = true;
                    }
                }
                break;
            case MappedToParameter:
                {
                    isNull = false;
                    const ParameterDescription * const pd=mm->GetParameter();
                    pd->ValueWithOptionalString(index,val,strVal);
                }
                break;
            }
        }
    }



    template <class T>
    void SetPropertyValueWithOptionalString(const T val,
        const char * const strVal,
        char * & blob,
        const TypeId property,
        const MemberIndex member,
        const ArrayIndex index,
        DotsC_ErrorCode & errorCode)
    {
        errorCode = NoError;

        bool isInherited;
        const PropertyMappingDescription * const pmd=Repository::Classes().FindClass(BlobLayout::GetTypeId(blob))->FindPropertyMapping(property, isInherited);

        if (pmd == NULL)
        {
            return;
        }

        const MemberMapping * const mm = pmd->GetMemberMapping(member);

        if (mm != NULL)
        {
            switch (mm->GetMappingKind())
            {
            case MappedToNull:
                errorCode = ReadOnlyProperty;
                break;

            case MappedToMember:
                {
                    const ClassMemberReference * cmr = mm->GetMemberReference();

                    if (cmr->size() > 2)
                    {
                        errorCode = UnableToDereferenceProperty;
                        return; //cannot set property since we need recursive reallocation...
                    }

                    const MemberReferenceElement ref = cmr->back();

                    if (ref.m_index == -1)//pointing at an array, use the index from the function call
                    {
                        BlobLayout::SetMemberWithOptionalString(val,strVal, blob, ref.m_classMember, index);
                        BlobLayout::SetStatus(false, true, blob, ref.m_classMember, index);
                    }
                    else
                    {
                        BlobLayout::SetMemberWithOptionalString(val,strVal,blob, ref.m_classMember, ref.m_index);
                        BlobLayout::SetStatus(false, true, blob, ref.m_classMember, ref.m_index);
                    }
                }
                break;

            case MappedToParameter:
                errorCode = ReadOnlyProperty;
                break;
            }
        }

    }

}
}
}
}
#endif
