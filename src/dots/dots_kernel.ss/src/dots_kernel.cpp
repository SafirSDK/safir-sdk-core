/******************************************************************************
*
* Copyright Saab AB, 2004-2013 (http://safir.sourceforge.net)
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

#include <iostream>
#include <math.h>

#include <boost/static_assert.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/exception.hpp>
#include <boost/thread/once.hpp>
#include <boost/static_assert.hpp>

#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Utilities/Internal/ConfigReader.h>

#include <Safir/Dob/Typesystem/Internal/Kernel.h>
#include <Safir/Dob/Typesystem/Internal/BlobLayout.h>
#include <Safir/Dob/Typesystem/Internal/Serialization.h>

#include "dots_init_helper.h"
#include "dots_exception_keeper.h"

//disable warnings in boost
#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable : 4127)
#endif

#include <boost/date_time/posix_time/posix_time.hpp>

//and enable the warnings again
#if defined _MSC_VER
  #pragma warning (pop)
#endif

using namespace Safir::Dob::Typesystem::Internal;

namespace 
{
    void CALLING_CONVENTION DeleteBytePointer(char* & ptr)
    {
        if (ptr!=NULL)
        {
            delete [] ptr;
            ptr=NULL;
        }
    }

    void GetCompleteTypeInternal(const ClassDescriptionShm * const cd,
                                 DotsC_TypeId * buf,
                                 const DotsC_Int32 bufSize,
                                 DotsC_Int32 & noResults)
    {
        if (noResults>=bufSize || cd==NULL)
        {
            return;
        }

        buf[noResults++]=cd->GetTypeId();
        for (int i=0; i<cd->GetNumberOfDescendants(); ++i)
        {
            GetCompleteTypeInternal(cd->GetDescendant(i), buf, bufSize, noResults);
        }
    }

    bool GetPropertyParameterInternal(const DotsC_TypeId typeId,
                                      const DotsC_TypeId propertyId,
                                      const DotsC_MemberIndex member,
                                      const DotsC_ArrayIndex index, //memberIndex
                                      const ParameterDescriptionShm*& parameter,
                                      int& parameterIndex)
    {
        bool isInherited;
        const PropertyMappingDescriptionShm* pmd=RepositoryKeeper::GetRepository()->GetClass(typeId)->GetPropertyMapping(propertyId, isInherited);

        if (pmd==NULL)
        {
            return false;
        }

        const MemberMappingDescriptionShm* mm=pmd->GetMemberMapping(member);

        if (mm==NULL || mm->GetMappingKind()!=MappedToParameter)
        {
            return false;
        }

        std::pair<const ParameterDescriptionShm*, int> param=mm->GetParameter();
        parameter=param.first;

        if (pmd->GetProperty()->GetMember(member)->IsArray())
        {
            //if the member is an array we use the specified index.
            parameterIndex=index;
        }
        else
        {
            //If not array we have to use what's been declared in dou-file in case the parameter is an array.
            parameterIndex=param.second;
        }

        return true;
    }
}

//********************************************************
//* Type information operations
//********************************************************
DotsC_Int32 DotsC_NumberOfTypeIds()
{
    Init();
    return DotsC_NumberOfClasses()+DotsC_NumberOfProperties()+DotsC_NumberOfEnumerations()+DotsC_NumberOfExceptions();
}

//Get the number of classes defined in the system
DotsC_Int32 DotsC_NumberOfClasses()
{
    Init();
    return RepositoryKeeper::GetRepository()->GetNumberOfClasses();
}

DotsC_Int32 DotsC_NumberOfProperties()
{
    Init();
    return RepositoryKeeper::GetRepository()->GetNumberOfProperties();
}

DotsC_Int32 DotsC_NumberOfEnumerations()
{
    Init();
    return RepositoryKeeper::GetRepository()->GetNumberOfEnums();
}

DotsC_Int32 DotsC_NumberOfExceptions()
{
    Init();
    return RepositoryKeeper::GetRepository()->GetNumberOfExceptions();
}

//Get a list of all type id's that exists in the system. Buf is a pointer to an array of size bufSize. The
//out parameter size defines how many type id's that were inserted into buf.
void DotsC_GetAllTypeIds(DotsC_TypeId* buf, DotsC_Int32 bufSize, DotsC_Int32& size)
{
    Init();
    std::set<DotsC_TypeId> types;
    RepositoryKeeper::GetRepository()->GetAllClassTypeIds(types);
    RepositoryKeeper::GetRepository()->GetAllEnumTypeIds(types);
    RepositoryKeeper::GetRepository()->GetAllExceptionTypeIds(types);
    RepositoryKeeper::GetRepository()->GetAllPropertyTypeIds(types);
    if (types.size()>static_cast<size_t>(bufSize))
    {
        size=bufSize;
    }
    else
    {
        size=types.size();
    }

    std::set<DotsC_TypeId>::const_iterator it=types.begin();
    for (int i=0; i<size; ++i)
    {
        buf[i]=*it;
        ++it;
    }
}

bool DotsC_TypeExists(const DotsC_TypeId typeId)
{
    Init();
    return (DotsC_IsClass(typeId) || DotsC_IsProperty(typeId) || DotsC_IsEnumeration(typeId) || DotsC_IsException(typeId));
}

bool DotsC_IsClass(const DotsC_TypeId typeId)
{
    Init();
    return RepositoryKeeper::GetRepository()->GetClass(typeId)!=NULL;
}

bool DotsC_IsProperty(const DotsC_TypeId typeId)
{
    Init();
    return RepositoryKeeper::GetRepository()->GetProperty(typeId)!=NULL;
}

bool DotsC_IsEnumeration(const DotsC_TypeId typeId)
{
    Init();
    return RepositoryKeeper::GetRepository()->GetEnum(typeId)!=NULL;
}

bool DotsC_IsException(const DotsC_TypeId typeId)
{
    Init();
    return RepositoryKeeper::GetRepository()->GetException(typeId)!=NULL;
}

DotsC_TypeId DotsC_TypeIdFromName(const char* typeName)
{
    Init();
    return Safir::Dob::Typesystem::Internal::TypeUtilities::CalculateTypeId(typeName);
}

const char* DotsC_GetTypeName(const DotsC_TypeId typeId)
{
    Init();
    return TypeUtilities::GetTypeName(RepositoryKeeper::GetRepository(), typeId);
}


DotsC_Int32 DotsC_GetNumberOfEnumerationValues(const DotsC_TypeId enumId)
{
    Init();
    const EnumDescriptionShm* ed=RepositoryKeeper::GetRepository()->GetEnum(enumId);
    if (ed!=NULL)
    {
        return ed->GetNumberOfValues();
    }
    return -1;
}

const char* DotsC_GetEnumerationValueName(const DotsC_TypeId enumId, DotsC_EnumerationValue enumVal)
{
    Init();
    const EnumDescriptionShm* ed=RepositoryKeeper::GetRepository()->GetEnum(enumId);
    if (ed!=NULL)
    {
        return ed->GetValueName(enumVal);
    }
    return NULL;
}

DotsC_EnumerationValue DotsC_EnumerationValueFromName(const DotsC_TypeId enumId, const char* enumValueName)
{
    Init();
    const EnumDescriptionShm* ed=RepositoryKeeper::GetRepository()->GetEnum(enumId);
    if (ed!=NULL)
    {
        return Safir::Dob::Typesystem::Internal::TypeUtilities::GetIndexOfEnumValue(ed, enumValueName);
    }
    return -1;
}

//********************************************************
//* Base operations on blobs
//********************************************************
//create default blob
void DotsC_CreateBlob(const DotsC_TypeId typeId,
                      char* & blob)
{
    Init();
    RepositoryKeeper::GetBlobLayout()->CreateBlob(typeId, blob);
}

void DotsC_DeleteBlob(char* & blob)
{
    Init();
    RepositoryKeeper::GetBlobLayout()->DeleteBlob(blob);
}

void DotsC_CreateCopyOfBlob(char* & to,
                            const char* const from)
{
    Init();
    const size_t size=static_cast<size_t>(RepositoryKeeper::GetBlobLayout()->GetSize(from));
    to=new char[size];
    memcpy(to, from, size);
}

DotsC_TypeId DotsC_GetTypeId(const char* const blob)
{
    Init();
    return RepositoryKeeper::GetBlobLayout()->GetTypeId(blob);
}

//Gives the total size of the blob
DotsC_Int32 DotsC_GetSize(const char* const blob)
{
    Init();
    return RepositoryKeeper::GetBlobLayout()->GetSize(blob);
}

// IsAnythingChanged
bool DotsC_IsAnythingChanged(const char* const blob)
{
    Init();
    return RepositoryKeeper::GetBlobLayout()->IsAnythingChanged(blob);
}


//Reset changed flags for the members
void DotsC_ResetChanged(char* const blob)
{
    Init();
    RepositoryKeeper::GetBlobLayout()->ResetChanged(blob);
}

//Reset changed flags for the members
void DotsC_SetChanged(char* const blob,
                      const bool changed)
{
    Init();
    RepositoryKeeper::GetBlobLayout()->SetChanged(blob,changed);
}

void DotsC_SetChangedHere(char* const blob,
                          const DotsC_MemberIndex member,
                          const DotsC_ArrayIndex index,
                          const bool changed)
{
    Init();
    const DotsC_MemberStatus status=RepositoryKeeper::GetBlobLayout()->GetMemberStatus(blob, member, index);
    RepositoryKeeper::GetBlobLayout()->SetMemberStatus(status.IsNull(), changed, blob, member, index);
}

void DotsC_SetChangedMembers(const char* const val, char* & blob)
{
    Init();
    RepositoryKeeper::GetBlobLayout()->MergeChanges(val, blob);
}


void DotsC_SetChangedSinceLastRead(const char* const lastRead,
                                   char* const current)
{
    Init();
    RepositoryKeeper::GetBlobLayout()->SetChangedSinceLastRead(lastRead, current);
}

//***********************************************************
//* Functions for retrieving member info about object types
//***********************************************************
//Functions for retrieving member info about object types
DotsC_Int32 DotsC_GetNumberOfMembers(const DotsC_TypeId typeId)
{
    Init();
    {       
        const ClassDescriptionShm* cd=RepositoryKeeper::GetRepository()->GetClass(typeId);
        if (cd!=NULL)
        {
            return cd->GetNumberOfMembers();
        }
    }

    {
        const PropertyDescriptionShm* pd=RepositoryKeeper::GetRepository()->GetProperty(typeId);
        if (pd!=NULL)
        {
            return pd->GetNumberOfMembers();
        }
    }
    return -1;
}

DotsC_MemberIndex DotsC_GetMemberId(const DotsC_TypeId typeId,
                              const char* const memberName)
{
    Init();
    {
        const ClassDescriptionShm* cd=RepositoryKeeper::GetRepository()->GetClass(typeId);
        if (cd!=NULL)
        {
            return cd->GetMemberIndex(memberName);
        }
    }

    {
        const PropertyDescriptionShm* pd=RepositoryKeeper::GetRepository()->GetProperty(typeId);
        if (pd!=NULL)
        {
            return pd->GetMemberIndex(memberName);
        }
    }
    return -1;
}

const char* DotsC_GetMemberName(const DotsC_TypeId typeId, const DotsC_MemberIndex member)
{
    Init();
    {
        const ClassDescriptionShm* cd=RepositoryKeeper::GetRepository()->GetClass(typeId);
        if (cd!=NULL)
        {
            return cd->GetMember(member)->GetName();
        }
    }

    {
        const PropertyDescriptionShm* pd=RepositoryKeeper::GetRepository()->GetProperty(typeId);
        if (pd!=NULL)
        {
            return pd->GetMember(member)->GetName();
        }
    }
    return NULL;
}

DotsC_TypeId DotsC_GetComplexMemberTypeId(const DotsC_TypeId typeId, const DotsC_MemberIndex member)
{
    Init();
    const ClassDescriptionShm* cd=RepositoryKeeper::GetRepository()->GetClass(typeId);
    const MemberDescriptionShm* md=NULL;
    if (cd!=NULL)
    {
        md=cd->GetMember(member);
    }
    else
    {
        const PropertyDescriptionShm* pd=RepositoryKeeper::GetRepository()->GetProperty(typeId);
        if (pd!=NULL)
        {
            md=pd->GetMember(member);
        }
        else
        {
            return -1; //the type was not a property either
        }
    }

    if (md->GetMemberType()==ObjectMemberType)
    {
        return md->GetTypeId();
    }
    else if (md->GetMemberType()==EnumerationMemberType)
    {
        return md->GetTypeId();
    }
    return -1;
}

void DotsC_GetMemberInfo(const DotsC_TypeId typeId,  //in
                         const DotsC_MemberIndex member,  //in
                         DotsC_MemberType& memberType,//out
                         const char* & memberName,           //out
                         DotsC_TypeId & complexType,   //out
                         DotsC_Int32 & stringLength,   //out
                         bool & isArray,                      //out
                         DotsC_Int32 & arrayLength)    //out
{
    Init();
    const MemberDescriptionShm* memberDesc;
    complexType=-1;
    const ClassDescriptionShm* cd=RepositoryKeeper::GetRepository()->GetClass(typeId);
    stringLength=-1;
    if (cd!=NULL)
    {
        memberDesc=cd->GetMember(member);
        if (memberDesc==NULL)
        {
            // there is no error code, so set all out fields to invalid (-1 or null)
            memberName=NULL;
            complexType=-1;
            arrayLength=-1;
            return;
        }
    }
    else
    {
        const PropertyDescriptionShm* pd=RepositoryKeeper::GetRepository()->GetProperty(typeId);
        if (pd==NULL)
        {
            // there is no error code, so set all out fields to invalid (-1 or null)
            memberName=NULL;
            complexType=-1;
            arrayLength=-1;
            return;
        }
        memberDesc=pd->GetMember(member);
    }

    if (memberDesc->GetMemberType()==StringMemberType)
    {
        stringLength=memberDesc->GetMaxLength();
    }

    memberType=memberDesc->GetMemberType();
    memberName=memberDesc->GetName();
    isArray=memberDesc->IsArray();
    arrayLength=memberDesc->GetArraySize();
    if (memberType==ObjectMemberType || memberType==EnumerationMemberType)
    {
        complexType=memberDesc->GetTypeId();
    }
}

DotsC_Int32 DotsC_GetMemberArraySize(const DotsC_TypeId typeId, const DotsC_MemberIndex member)
{
    Init();
    const ClassDescriptionShm* cd=RepositoryKeeper::GetRepository()->GetClass(typeId);
    if (cd!=NULL)
    {
        return cd->GetMember(member)->GetArraySize();
    }
    return -1;
}

DotsC_Int32 DotsC_GetStringMemberMaxLength(const DotsC_TypeId typeId, const DotsC_MemberIndex member)
{
    Init();
    const ClassDescriptionShm* cd=RepositoryKeeper::GetRepository()->GetClass(typeId);
    if (cd==NULL)
    {
        return -1;
    }

    const MemberDescriptionShm* const memberDesc= cd->GetMember(member);
    if (memberDesc->GetMemberType()==StringMemberType)
    {
        return memberDesc->GetMaxLength();
    }
    else
    {
        return -1;
    }
}

DotsC_Int32 DotsC_GetMemberArraySizeProperty(const DotsC_TypeId classId, const DotsC_TypeId propertyId, const DotsC_MemberIndex propertyMember)
{
    Init();
    bool isInherited;
    const ClassDescriptionShm* cd=RepositoryKeeper::GetRepository()->GetClass(classId);
    if (cd==NULL)
    {
        return -1;
    }
    const PropertyMappingDescriptionShm* pmd=cd->GetPropertyMapping(propertyId, isInherited);
    if (pmd==NULL)
    {
        return -1;
    }

    const MemberMappingDescriptionShm* mm=pmd->GetMemberMapping(propertyMember);
    if (mm==NULL)
    {
        return -1;
    }

    switch (mm->GetMappingKind())
    {
    case MappedToNull:
    {
        return 1; //it is one long (just the null...)
    }

    case MappedToMember:
    {
        DotsC_TypeId parent=classId;
        int refDepth=mm->MemberReferenceDepth();
        for (int i=0; i<refDepth-1; ++i)
        {
            std::pair<DotsC_MemberIndex, DotsC_ArrayIndex> ref=mm->GetMemberReference(i);
            parent=DotsC_GetComplexMemberTypeId(parent, ref.first);
        }
        return DotsC_GetMemberArraySize(parent, mm->GetMemberReference(refDepth-1).first);
    }

    case MappedToParameter:
    {
        return mm->GetParameter().first->GetArraySize();
    }
    }
    //will never get here!
    return -1;
}

DotsC_Int32 DotsC_GetStringMemberMaxLengthProperty(const DotsC_TypeId classId, const DotsC_TypeId propertyId, const DotsC_MemberIndex propertyMember)
{

    Init();
    bool isInherited;
    const ClassDescriptionShm* cd=RepositoryKeeper::GetRepository()->GetClass(classId);
    if (cd==NULL)
    {
        return -1;
    }
    const PropertyMappingDescriptionShm* pmd=cd->GetPropertyMapping(propertyId, isInherited);
    if (pmd==NULL)
    {
        return -1;
    }

    const MemberMappingDescriptionShm* mm=pmd->GetMemberMapping(propertyMember);
    if (mm==NULL)
    {
        return -1;
    }

    switch (mm->GetMappingKind())
    {
    case MappedToNull:
    {
        return 1; //it is one long (just the null...)
    }

    case MappedToMember:
    {
        DotsC_TypeId parent=classId;
        int refDepth=mm->MemberReferenceDepth();
        for (int i=0; i<refDepth-1; ++i)
        {
            std::pair<DotsC_MemberIndex, DotsC_ArrayIndex> ref=mm->GetMemberReference(i);
            parent=DotsC_GetComplexMemberTypeId(parent, ref.first);
        }
        return DotsC_GetStringMemberMaxLength(parent, mm->GetMemberReference(refDepth-1).first);
    }

    case MappedToParameter:
        // AWI: Vad göra här? I daxläget finns inte informationen för parameterar
        //return mm->GetParameter()->ArrayLength();
        break;
    }

    //will never get here!
    return -1;
}

const char* DotsC_GetMemberTypeName(const DotsC_TypeId typeId, const DotsC_MemberIndex member)
{
    Init();
    const ClassDescriptionShm* cd=RepositoryKeeper::GetRepository()->GetClass(typeId);
    if (cd!=NULL)
    {
        return Safir::Dob::Typesystem::Internal::TypeUtilities::GetTypeName(cd->GetMember(member)->GetMemberType());
    }
    else
    {
        const PropertyDescriptionShm* pd=RepositoryKeeper::GetRepository()->GetProperty(typeId);
        if (pd!=NULL)
        {
            return Safir::Dob::Typesystem::Internal::TypeUtilities::GetTypeName(pd->GetMember(member)->GetMemberType());
        }
    }

    return NULL;
}

 //***********************************************************************
 //* Functions retrieving definitions of parameter values in object types
 //***********************************************************************
DotsC_Int32 DotsC_GetNumberOfParameters(const DotsC_TypeId typeId)
{
    Init();
    const ClassDescriptionShm* cd=RepositoryKeeper::GetRepository()->GetClass(typeId);
    if (cd!=NULL)
    {
        return cd->GetNumberOfParameters();
    }
    else
    {
        return -1;
    }
}

DotsC_ParameterIndex DotsC_GetParameterId(const DotsC_TypeId typeId, const char* parameterName)
{
    Init();
    const ClassDescriptionShm* cd=RepositoryKeeper::GetRepository()->GetClass(typeId);
    if (cd==NULL)
    {
        return -1;
    }

    std::string name(parameterName);
    for (int i=0; i<cd->GetNumberOfParameters(); ++i)
    {
        if (name==cd->GetParameter(i)->GetName())
        {
            return i;
        }
    }

    return -1;
}

const char* DotsC_GetParameterName(const DotsC_TypeId typeId, const DotsC_ParameterIndex parameter)
{
    Init();
    return RepositoryKeeper::GetRepository()->GetClass(typeId)->GetParameter(parameter)->GetName();
}

DotsC_MemberType DotsC_GetParameterType(const DotsC_TypeId typeId, const DotsC_ParameterIndex parameter)
{
    Init();
    return RepositoryKeeper::GetRepository()->GetClass(typeId)->GetParameter(parameter)->GetMemberType();
}

const char* DotsC_GetParameterTypeName(const DotsC_TypeId typeId, const DotsC_ParameterIndex parameter)
{
    Init();
    const ParameterDescriptionShm* pd=RepositoryKeeper::GetRepository()->GetClass(typeId)->GetParameter(parameter);
    if (pd->GetMemberType()==ObjectMemberType || pd->GetMemberType()==EnumerationMemberType)
    {
        return Safir::Dob::Typesystem::Internal::TypeUtilities::GetTypeName(RepositoryKeeper::GetRepository(), pd->GetTypeId());
    }
    else
    {
        return Safir::Dob::Typesystem::Internal::TypeUtilities::GetTypeName(pd->GetMemberType());
    }
}

DotsC_Int32 DotsC_GetParameterArraySize(const DotsC_TypeId typeId, const DotsC_ParameterIndex parameter)
{
    Init();
    return RepositoryKeeper::GetRepository()->GetClass(typeId)->GetParameter(parameter)->GetArraySize();
}

//************************************************************************************
//* Functions for retrieving member values
//************************************************************************************

bool DotsC_IsNullMember(const char* const blob, const DotsC_MemberIndex member, const DotsC_ArrayIndex index)
{
    Init();
    DotsC_MemberStatus status=RepositoryKeeper::GetBlobLayout()->GetMemberStatus(blob, member, index);
    return status.IsNull();
}

bool DotsC_IsChangedMember(const char* const blob, const DotsC_MemberIndex member, const DotsC_ArrayIndex index)
{
    Init();
    DotsC_MemberStatus status=RepositoryKeeper::GetBlobLayout()->GetMemberStatus(blob, member, index);
    if (status.HasChanged())
    {
        return true;
    }

    const ClassDescriptionShm* cd=RepositoryKeeper::GetRepository()->GetClass(RepositoryKeeper::GetBlobLayout()->GetTypeId(blob));
    if (cd==NULL)
    {
        return false;
    }

    const MemberDescriptionShm* md= cd->GetMember(member);

    if (md->GetMemberType()==ObjectMemberType && !status.IsNull())
    {
        DotsC_Int32 dummy=0;
        const char* childBlob;
        RepositoryKeeper::GetBlobLayout()->GetDynamicMember(blob, member, index, childBlob, dummy);
        return RepositoryKeeper::GetBlobLayout()->IsAnythingChanged(childBlob);
    }
    else
    {
        return false;
    }
}


void DotsC_GetBooleanMember(const char* const blob, const DotsC_MemberIndex member, const DotsC_ArrayIndex index, bool& val, bool& isNull, bool& isChanged)
{
    Init();
    DotsC_MemberStatus status=RepositoryKeeper::GetBlobLayout()->GetBoolMember(blob, member, index, val);
    isNull=status.IsNull();
    isChanged=status.HasChanged();
}

void DotsC_GetEnumerationMember(const char* const blob,
                                const DotsC_MemberIndex member,
                                const DotsC_ArrayIndex index,
                                DotsC_EnumerationValue & val,
                                bool& isNull,
                                bool& isChanged)
{
    Init();
    DotsC_MemberStatus status=RepositoryKeeper::GetBlobLayout()->GetEnumMember(blob, member, index, val);
    isNull=status.IsNull();
    isChanged=status.HasChanged();
}

void DotsC_GetInt32Member(const char* const blob, const DotsC_MemberIndex member, const DotsC_ArrayIndex index, DotsC_Int32& val, bool& isNull, bool& isChanged)
{
    Init();
    DotsC_MemberStatus status=RepositoryKeeper::GetBlobLayout()->GetInt32Member(blob, member, index, val);
    isNull=status.IsNull();
    isChanged=status.HasChanged();
}


void DotsC_GetInt64Member(const char* const blob, const DotsC_MemberIndex member, const DotsC_ArrayIndex index, DotsC_Int64& val, bool& isNull, bool& isChanged)
{
    Init();
    DotsC_MemberStatus status=RepositoryKeeper::GetBlobLayout()->GetInt64Member(blob, member, index, val);
    isNull=status.IsNull();
    isChanged=status.HasChanged();
}

void DotsC_GetFloat32Member(const char* const blob, const DotsC_MemberIndex member, const DotsC_ArrayIndex index, DotsC_Float32& val, bool& isNull, bool& isChanged)
{
    Init();
    DotsC_MemberStatus status=RepositoryKeeper::GetBlobLayout()->GetFloat32Member(blob, member, index, val);
    isNull=status.IsNull();
    isChanged=status.HasChanged();
    if (isNull)
    {
        //we have to set a proper value, since an unitialized value may cause
        //FP errors, e.g. in Ada
        val=0.0f;
    }
}

void DotsC_GetFloat64Member(const char* const blob, const DotsC_MemberIndex member, const DotsC_ArrayIndex index, DotsC_Float64& val, bool& isNull, bool& isChanged)
{
    Init();
    DotsC_MemberStatus status=RepositoryKeeper::GetBlobLayout()->GetFloat64Member(blob, member, index, val);
    isNull=status.IsNull();
    isChanged=status.HasChanged();
    if (isNull)
    {
        //we have to set a proper value, since an unitialized value may cause
        //FP errors, e.g. in Ada
        val=0.0;
    }
}

void DotsC_GetStringMember(const char* const blob, const DotsC_MemberIndex member, const DotsC_ArrayIndex index, const char* &val, bool& isNull, bool& isChanged)
{
    Init();
    DotsC_Int32 dummy=0;
    DotsC_MemberStatus status=RepositoryKeeper::GetBlobLayout()->GetDynamicMember(blob, member, index, val, dummy);
    isNull=status.IsNull();
    isChanged=status.HasChanged();
}

void DotsC_GetObjectMember(const char* const blob,
                           const DotsC_MemberIndex member,
                           const DotsC_ArrayIndex index,
                           const char* & val,
                           bool & isNull,
                           bool & isChanged)
{
    Init();
    DotsC_Int32 dummy=0;
    DotsC_MemberStatus status=RepositoryKeeper::GetBlobLayout()->GetDynamicMember(blob, member, index, val, dummy);
    isNull=status.IsNull();
    isChanged=status.HasChanged();
}

void DotsC_GetWriteableObjectMember(char* const blob,
                                    const DotsC_MemberIndex member,
                                    const DotsC_ArrayIndex index,
                                    char* & val,
                                    bool & isNull,
                                    bool & isChanged)
{
    Init();
    DotsC_Int32 dummy=0;
    DotsC_MemberStatus status=RepositoryKeeper::GetBlobLayout()->GetWritableDynamicMember(blob, member, index, val, dummy);
    isNull=status.IsNull();
    isChanged=status.HasChanged();
}


void DotsC_GetBinaryMember(const char* const blob,
                           const DotsC_MemberIndex member,
                           const DotsC_ArrayIndex index,
                           const char* &val,
                           DotsC_Int32& size,
                           bool & isNull,
                           bool & isChanged)
{
    Init();
    DotsC_MemberStatus status=RepositoryKeeper::GetBlobLayout()->GetDynamicMember(blob, member, index, val, size);
    isNull=status.IsNull();
    isChanged=status.HasChanged();
}

void DotsC_GetTypeIdMember(const char* const blob, const DotsC_MemberIndex member, const DotsC_ArrayIndex index, DotsC_TypeId& val, bool & isNull, bool & isChanged)
{
    Init();
    DotsC_MemberStatus status=RepositoryKeeper::GetBlobLayout()->GetInt64Member(blob, member, index, val);
    isNull=status.IsNull();
    isChanged=status.HasChanged();
}

void DotsC_GetHashedIdMember(const char* const blob,
                             const DotsC_MemberIndex member,
                             const DotsC_ArrayIndex index,
                             DotsC_Int64 & hashVal,
                             const char* & strVal,
                             bool & isNull,
                             bool & isChanged)
{
    Init();
    DotsC_MemberStatus status=RepositoryKeeper::GetBlobLayout()->GetHashedMember(blob, member, index, hashVal, strVal);
    isNull=status.IsNull();
    isChanged=status.HasChanged();
}

void DotsC_GetEntityIdMember(const char* const blob,
                             const DotsC_MemberIndex member,
                             const DotsC_ArrayIndex index,
                             DotsC_EntityId & entityId,
                             const char* & instanceIdStr,
                             bool & isNull,
                             bool & isChanged)
{
    Init();
    DotsC_MemberStatus status=RepositoryKeeper::GetBlobLayout()->GetEntityIdMember(blob, member, index, entityId, instanceIdStr);
    isNull=status.IsNull();
    isChanged=status.HasChanged();
}

//************************************************************************************
//* Functions for setting member values
//************************************************************************************
void DotsC_SetNullMember(char* const blob,
                         const DotsC_MemberIndex member,
                         const DotsC_ArrayIndex index)
{
    Init();
    RepositoryKeeper::GetBlobLayout()->SetMemberStatus(true, true, blob, member, index);
}

void DotsC_SetBooleanMember(const bool val,
                            char* & blob,
                            const DotsC_MemberIndex member,
                            const DotsC_ArrayIndex index)
{
    Init();
    RepositoryKeeper::GetBlobLayout()->SetBoolMember(val, blob, member, index);
    RepositoryKeeper::GetBlobLayout()->SetMemberStatus(false, true, blob, member, index);
}

void DotsC_SetEnumerationMember(const DotsC_EnumerationValue val,
                                char* & blob,
                                const DotsC_MemberIndex member,
                                const DotsC_ArrayIndex index)
{
    Init();
    RepositoryKeeper::GetBlobLayout()->SetEnumMember(val, blob, member, index);
    RepositoryKeeper::GetBlobLayout()->SetMemberStatus(false, true, blob, member, index);
}


void DotsC_SetInt32Member(const DotsC_Int32 val,
                          char* & blob,
                          const DotsC_MemberIndex member,
                          const DotsC_ArrayIndex index)
{
    Init();
    RepositoryKeeper::GetBlobLayout()->SetInt32Member(val, blob, member, index);
    RepositoryKeeper::GetBlobLayout()->SetMemberStatus(false, true, blob, member, index);
}

void DotsC_SetInt64Member(const DotsC_Int64 val,
                          char* & blob,
                          const DotsC_MemberIndex member,
                          const DotsC_ArrayIndex index)
{
    Init();
    RepositoryKeeper::GetBlobLayout()->SetInt64Member(val, blob, member, index);
    RepositoryKeeper::GetBlobLayout()->SetMemberStatus(false, true, blob, member, index);
}

void DotsC_SetFloat32Member(const DotsC_Float32 val,
                            char* & blob,
                            const DotsC_MemberIndex member,
                            const DotsC_ArrayIndex index)
{
    Init();
    RepositoryKeeper::GetBlobLayout()->SetFloat32Member(val, blob, member, index);
    RepositoryKeeper::GetBlobLayout()->SetMemberStatus(false, true, blob, member, index);
}

void DotsC_SetFloat64Member(const DotsC_Float64 val,
                            char* & blob,
                            const DotsC_MemberIndex member,
                            const DotsC_ArrayIndex index)
{
    Init();
    RepositoryKeeper::GetBlobLayout()->SetFloat64Member(val, blob, member, index);
    RepositoryKeeper::GetBlobLayout()->SetMemberStatus(false, true, blob, member, index);
}

void DotsC_SetStringMember(const char* const val,
                           char* & blob,
                           const DotsC_MemberIndex member,
                           const DotsC_ArrayIndex index)
{
    Init();
    DotsC_Int32 dummy=0;
    RepositoryKeeper::GetBlobLayout()->SetDynamicMember(val, dummy, blob, member, index);
    RepositoryKeeper::GetBlobLayout()->SetMemberStatus(false, true, blob, member, index);
}

void DotsC_SetTypeIdMember(const DotsC_TypeId val,
                           char* & blob,
                           const DotsC_MemberIndex member,
                           const DotsC_ArrayIndex index)
{
    Init();
    RepositoryKeeper::GetBlobLayout()->SetInt64Member(val, blob, member, index);
    RepositoryKeeper::GetBlobLayout()->SetMemberStatus(false, true, blob, member, index);
}

void DotsC_SetHashedIdMember(const DotsC_Int64 hashVal,
                             const char* const strVal,
                             char* & blob,
                             const DotsC_MemberIndex member,
                             const DotsC_ArrayIndex index)
{
    Init();
    RepositoryKeeper::GetBlobLayout()->SetHashedMember(hashVal, strVal, blob, member, index);
    RepositoryKeeper::GetBlobLayout()->SetMemberStatus(false, true, blob, member, index);
}

void DotsC_SetEntityIdMember(const DotsC_EntityId& entityId,
                             const char* const instanceIdStr,
                             char* & blob,
                             const DotsC_MemberIndex member,
                             const DotsC_ArrayIndex index)
{
    Init();
    RepositoryKeeper::GetBlobLayout()->SetEntityIdMember(entityId, instanceIdStr, blob, member, index);
    RepositoryKeeper::GetBlobLayout()->SetMemberStatus(false, true, blob, member, index);
}

void DotsC_SetObjectMember(const char* const val, char* & blob, const DotsC_MemberIndex member, const DotsC_ArrayIndex index)
{
    Init();
    DotsC_Int32 dummy=0;
    RepositoryKeeper::GetBlobLayout()->SetDynamicMember(val, dummy, blob, member, index);
    RepositoryKeeper::GetBlobLayout()->SetMemberStatus(false, true, blob, member, index);
}

void DotsC_SetBinaryMember(const char* val,
                           DotsC_Int32 numberOfBytes,
                           char* & blob,
                           const DotsC_MemberIndex member,
                           const DotsC_ArrayIndex index)
{
    Init();
    RepositoryKeeper::GetBlobLayout()->SetDynamicMember(val, numberOfBytes, blob, member, index);
    RepositoryKeeper::GetBlobLayout()->SetMemberStatus(false, true, blob, member, index);
}

//************************************************************************************
//* Type compatibility
//************************************************************************************
//Checks if type is an instance of the ofType, direct or by inheritance
bool DotsC_IsOfType(const DotsC_TypeId type, const DotsC_TypeId ofType)
{
    Init();
    return Safir::Dob::Typesystem::Internal::TypeUtilities::IsOfType(RepositoryKeeper::GetRepository(), type, ofType);
}

void DotsC_GetCompleteType(const DotsC_TypeId type,
                           DotsC_TypeId* const buf,
                           const DotsC_Int32 bufSize,
                           DotsC_Int32 & noResults)
{
    Init();
    noResults=0;
    GetCompleteTypeInternal(RepositoryKeeper::GetRepository()->GetClass(type), buf, bufSize, noResults);
}

DotsC_TypeId DotsC_GetParentType(const DotsC_TypeId type)
{
    Init();
    const ClassDescriptionShm* cd=RepositoryKeeper::GetRepository()->GetClass(type);
    if (cd==NULL)
    {
        return 0;
    }
    else if (cd->GetBaseClass()==NULL)
    {
        return cd->GetTypeId();
    }
    else
    {
        return cd->GetBaseClass()->GetTypeId();
    }
}

void DotsC_HasProperty(const DotsC_TypeId classTypeId, const DotsC_TypeId propertyTypeId, bool & hasProperty, bool & isInherited)
{
    Init();
    hasProperty=false;
    isInherited=false;
    const ClassDescriptionShm* cd=RepositoryKeeper::GetRepository()->GetClass(classTypeId);
    if (cd)
    {
        hasProperty=(cd->GetPropertyMapping(propertyTypeId, isInherited)!=NULL);
    }
}

//************************************************************************************
//* Serialization
//************************************************************************************
void DotsC_BetterBlobToXml(char* const xmlDest, const char* const blobSource, const DotsC_Int32 bufSize, DotsC_Int32 & resultSize)
{
    Init();
    std::ostringstream xmlStream;
    Safir::Dob::Typesystem::Internal::BinaryToXml(RepositoryKeeper::GetRepository(), blobSource, xmlStream);
    std::string xml=xmlStream.str();
    resultSize=static_cast<DotsC_Int32>(xml.size())+1; //add one char for null termination
    if (resultSize <= bufSize)
    {
        strncpy(xmlDest, xml.c_str(), resultSize);
    }
}



void DotsC_XmlToBlob(char* & blobDest,
                     DotsC_BytePointerDeleter & deleter,
                     const char* xmlSource)
{
    Init();
    deleter=DeleteBytePointer;
    std::vector<char> blob;
    Safir::Dob::Typesystem::Internal::XmlToBinary(RepositoryKeeper::GetRepository(), xmlSource, blob);
    if (!blob.empty())
    {
        blobDest=new char[blob.size()];
        memcpy(blobDest, &blob[0], blob.size());
    }
    else
    {
        blobDest=NULL;
    }
}

void DotsC_BlobToJson(char * const jsonDest,
                      const char * const blobSource,
                      const DotsC_Int32 bufSize,
                      DotsC_Int32 & resultSize)
{
    Init();
    std::ostringstream jsonStream;
    Safir::Dob::Typesystem::Internal::BinaryToJson(RepositoryKeeper::GetRepository(), blobSource, jsonStream);
    std::string json=jsonStream.str();
    resultSize=static_cast<DotsC_Int32>(json.size())+1; //add one char for null termination
    if (resultSize <= bufSize)
    {
        strncpy(jsonDest, json.c_str(), resultSize);
    }

}

void DotsC_JsonToBlob(char * & blobDest,
                      DotsC_BytePointerDeleter & deleter,
                      const char * const jsonSource)
{
    Init();
    deleter=DeleteBytePointer;
    std::vector<char> blob;
    Safir::Dob::Typesystem::Internal::JsonToBinary(RepositoryKeeper::GetRepository(), jsonSource, blob);
    if (!blob.empty())
    {
        blobDest=new char[blob.size()];
        memcpy(blobDest, &blob[0], blob.size());
    }
    else
    {
        blobDest=NULL;
    }
}

DotsC_Int32 DotsC_CalculateBase64BufferSize(DotsC_Int32 binarySourceSize)
{
    Init();

    const div_t res = div(static_cast<int>(binarySourceSize),3);
    const int numChars = res.quot*4 + (res.rem?4:0);
    const div_t numNewLines = div(numChars, 64); //line length 64
    return numChars + numNewLines.quot -(numNewLines.rem==0?1:0);
}

void DotsC_BinaryToBase64(char* base64Dest,
                          DotsC_Int32 destSize,
                          const char* const binarySource,
                          DotsC_Int32 sourceSize,
                          DotsC_Int32 & resultSize)
{
    Init();
    std::ostringstream b64Stream;
    Safir::Dob::Typesystem::Internal::BinaryToBase64(binarySource, sourceSize, b64Stream);
    std::string base64=b64Stream.str();
    resultSize=static_cast<DotsC_Int32>(base64.size())+1; //add one char for null termination
    strncpy(base64Dest, base64.c_str(), std::min(resultSize, destSize));
}

DotsC_Int32 DotsC_CalculateBinaryBufferSize(DotsC_Int32 base64SourceSize)
{
    Init();
    int requiredSize=(3*base64SourceSize)/4; //not exactly correct, theres a small overhead due to linebreaks.
    return requiredSize;
}

void DotsC_Base64ToBinary(char* binaryDest,
                          DotsC_Int32 destSize,
                          const char* const base64Source,
                          DotsC_Int32 sourceSize,
                          DotsC_Int32 & resultSize)
{
    Init();
    std::vector<char> bin;
    std::string base64(base64Source, base64Source+static_cast<size_t>(sourceSize));
    Safir::Dob::Typesystem::Internal::Base64ToBinary(base64, bin);
    resultSize=static_cast<DotsC_Int32>(bin.size());
    if (resultSize<=destSize)
    {
        memcpy(binaryDest, &bin[0], bin.size());
    }
}

//************************************************************************************
//* Functions for retrieval of parameters
//************************************************************************************
void DotsC_GetBooleanParameter(const DotsC_TypeId typeId, const DotsC_ParameterIndex parameter, const DotsC_ArrayIndex index, bool & val)
{
    Init();
    const ParameterDescriptionShm* const pd=RepositoryKeeper::GetRepository()->GetClass(typeId)->GetParameter(parameter);
    val=pd->GetBoolValue(index);
}

void DotsC_GetEnumerationParameter(const DotsC_TypeId typeId,
                                   const DotsC_ParameterIndex parameter,
                                   const DotsC_ArrayIndex index,
                                   DotsC_EnumerationValue& val)
{
    Init();
    const ParameterDescriptionShm* const pd=RepositoryKeeper::GetRepository()->GetClass(typeId)->GetParameter(parameter);
    val=pd->GetInt32Value(index);
}

void DotsC_GetInt32Parameter(const DotsC_TypeId typeId, const DotsC_ParameterIndex parameter, const DotsC_ArrayIndex index, DotsC_Int32& val)
{
    Init();
    const ParameterDescriptionShm* const pd=RepositoryKeeper::GetRepository()->GetClass(typeId)->GetParameter(parameter);
    val=pd->GetInt32Value(index);
}

void DotsC_GetInt64Parameter(const DotsC_TypeId typeId, const DotsC_ParameterIndex parameter, const DotsC_ArrayIndex index, DotsC_Int64& val)
{
    Init();
    const ParameterDescriptionShm* const pd=RepositoryKeeper::GetRepository()->GetClass(typeId)->GetParameter(parameter);
    val=pd->GetInt64Value(index);
}

void DotsC_GetFloat32Parameter(const DotsC_TypeId typeId, const DotsC_ParameterIndex parameter, const DotsC_ArrayIndex index, DotsC_Float32& val)
{
    Init();
    const ParameterDescriptionShm* const pd=RepositoryKeeper::GetRepository()->GetClass(typeId)->GetParameter(parameter);
    val=pd->GetFloat32Value(index);
}

void DotsC_GetFloat64Parameter(const DotsC_TypeId typeId, const DotsC_ParameterIndex parameter, const DotsC_ArrayIndex index, DotsC_Float64& val)
{
    Init();
    const ParameterDescriptionShm* const pd=RepositoryKeeper::GetRepository()->GetClass(typeId)->GetParameter(parameter);
    val=pd->GetFloat64Value(index);
}

void DotsC_GetStringParameter(const DotsC_TypeId typeId, const DotsC_ParameterIndex parameter, const DotsC_ArrayIndex index, const char* &val)
{
    Init();
    const ParameterDescriptionShm* const pd=RepositoryKeeper::GetRepository()->GetClass(typeId)->GetParameter(parameter);
    val=pd->GetStringValue(index);
}

void DotsC_GetTypeIdParameter(const DotsC_TypeId typeId, const DotsC_ParameterIndex parameter, const DotsC_ArrayIndex index, DotsC_TypeId& val)
{
    Init();
    const ParameterDescriptionShm* const pd=RepositoryKeeper::GetRepository()->GetClass(typeId)->GetParameter(parameter);
    val=pd->GetInt64Value(index);
}

void DotsC_GetHashedIdParameter(const DotsC_TypeId typeId,
                                const DotsC_ParameterIndex parameter,
                                const DotsC_ArrayIndex index,
                                DotsC_Int64 & hashVal,
                                const char* & strVal)
{
    Init();
    const ParameterDescriptionShm* const pd=RepositoryKeeper::GetRepository()->GetClass(typeId)->GetParameter(parameter);
    std::pair<DotsC_Int64, const char*> val=pd->GetHashedValue(index);
    hashVal=val.first;
    strVal=val.second;
}

void DotsC_GetEntityIdParameter(const DotsC_TypeId typeId,
                                const DotsC_ParameterIndex parameter,
                                const DotsC_ArrayIndex index,
                                DotsC_EntityId & entityId,
                                const char* & instanceIdStr)
{
    Init();
    const ParameterDescriptionShm* const pd=RepositoryKeeper::GetRepository()->GetClass(typeId)->GetParameter(parameter);
    entityId.typeId=pd->GetInt64Value(index);
    std::pair<DotsC_Int64, const char*> hashVal=pd->GetHashedValue(index);
    entityId.instanceId=hashVal.first;
    instanceIdStr=hashVal.second;
}

void DotsC_GetObjectParameter(const DotsC_TypeId typeId,
                              const DotsC_ParameterIndex parameter,
                              const DotsC_ArrayIndex index,
                              const char* & val)
{
    Init();
    const ParameterDescriptionShm* const pd=RepositoryKeeper::GetRepository()->GetClass(typeId)->GetParameter(parameter);
    val=pd->GetObjectValue(index).first;
}

void DotsC_GetBinaryParameter(const DotsC_TypeId typeId,
                              const DotsC_ParameterIndex parameter,
                              const DotsC_ArrayIndex index,
                              const char* &val,
                              DotsC_Int32& size)
{
    Init();
    const ParameterDescriptionShm* const pd=RepositoryKeeper::GetRepository()->GetClass(typeId)->GetParameter(parameter);
    std::pair<const char*, size_t> binary=pd->GetBinaryValue(index);
    val=binary.first;
    size=static_cast<DotsC_Int32>(binary.second);
}

//*********************************
//* For "real classes"
//*********************************
DotsC_Int32 DotsC_GetInitialSize(const DotsC_TypeId typeId)
{
    Init();
    const ClassDescriptionShm* cd=RepositoryKeeper::GetRepository()->GetClass(typeId);
    if (cd!=NULL)
    {
        return cd->OwnSize();
    }
    else
    {
        return -1;
    }
}

void DotsC_FormatBlob(char* const blob,
                      const DotsC_Int32 blobSize,
                      const DotsC_TypeId typeId,
                      char* & beginningOfUnused)
{
    Init();
    RepositoryKeeper::GetBlobLayout()->FormatBlob(blob, blobSize, typeId, beginningOfUnused);
}


void DotsC_CreateObjectMember(char* const insideBlob,
                              const DotsC_Int32 blobSize,
                              const DotsC_TypeId typeId,
                              const DotsC_MemberIndex member,
                              const DotsC_ArrayIndex index,
                              const bool isChanged,
                              char* & beginningOfUnused)
{
    Init();
    RepositoryKeeper::GetBlobLayout()->CreateObjectMember(insideBlob,blobSize, typeId, member, index, isChanged, beginningOfUnused);
}

void DotsC_CreateStringMember(char* const insideBlob,
                              const DotsC_Int32 stringLength, //remember the null-termination!
                              const DotsC_MemberIndex member,
                              const DotsC_ArrayIndex index,
                              const bool isChanged,
                              char* & beginningOfUnused)
{
    Init();
    RepositoryKeeper::GetBlobLayout()->CreateStringMember(insideBlob,stringLength,member,index,isChanged,beginningOfUnused);
}

void DotsC_CreateBinaryMember(char* const insideBlob,
                              const DotsC_Int32 binarySize,
                              const DotsC_MemberIndex member,
                              const DotsC_ArrayIndex index,
                              const bool isChanged,
                              char* & beginningOfUnused)
{
    Init();
    RepositoryKeeper::GetBlobLayout()->CreateBinaryMember(insideBlob,binarySize,member,index,isChanged,beginningOfUnused);
}

void DotsC_SetBooleanMemberInPreallocated(const bool val,
                                          const bool isNull,
                                          const bool isChanged,
                                          char* const blob,
                                          const DotsC_MemberIndex member,
                                          const DotsC_ArrayIndex index)
{
    Init();
    if (!isNull)
    {
        RepositoryKeeper::GetBlobLayout()->SetBoolMember(val, blob, member, index);
    }
    RepositoryKeeper::GetBlobLayout()->SetMemberStatus(isNull, isChanged, blob, member, index);
}



void DotsC_SetInt32MemberInPreallocated(const DotsC_Int32 val,
                                        const bool isNull,
                                        const bool isChanged,
                                        char* const blob,
                                        const DotsC_MemberIndex member,
                                        const DotsC_ArrayIndex index)
{
    Init();
    if (!isNull)
    {
        RepositoryKeeper::GetBlobLayout()->SetInt32Member(val, blob, member, index);
    }
    RepositoryKeeper::GetBlobLayout()->SetMemberStatus(isNull, isChanged, blob, member, index);
}


void DotsC_SetInt64MemberInPreallocated(const DotsC_Int64 val,
                                        const bool isNull,
                                        const bool isChanged,
                                        char* const blob,
                                        const DotsC_MemberIndex member,
                                        const DotsC_ArrayIndex index)
{
    Init();
    if (!isNull)
    {
        RepositoryKeeper::GetBlobLayout()->SetInt64Member(val, blob, member, index);
    }
    RepositoryKeeper::GetBlobLayout()->SetMemberStatus(isNull, isChanged, blob, member, index);

}

void DotsC_SetFloat32MemberInPreallocated(const DotsC_Float32 val,
                                          const bool isNull,
                                          const bool isChanged,
                                          char* const blob,
                                          const DotsC_MemberIndex member,
                                          const DotsC_ArrayIndex index)
{
    Init();
    if (!isNull)
    {
        RepositoryKeeper::GetBlobLayout()->SetFloat32Member(val, blob, member, index);
    }
    RepositoryKeeper::GetBlobLayout()->SetMemberStatus(isNull, isChanged, blob, member, index);

}

void DotsC_SetFloat64MemberInPreallocated(const DotsC_Float64 val,
                                          const bool isNull,
                                          const bool isChanged,
                                          char* const blob,
                                          const DotsC_MemberIndex member,
                                          const DotsC_ArrayIndex index)
{
    Init();
    if (!isNull)
    {
        RepositoryKeeper::GetBlobLayout()->SetFloat64Member(val, blob, member, index);
    }
    RepositoryKeeper::GetBlobLayout()->SetMemberStatus(isNull, isChanged, blob, member, index);

}

void DotsC_SetHashedIdMemberInPreallocated(const DotsC_Int64 hashVal,
                                           const char* const strVal,
                                           const DotsC_Int32 stringLength,
                                           const bool isNull,
                                           const bool isChanged,
                                           char* const blob,
                                           const DotsC_MemberIndex member,
                                           const DotsC_ArrayIndex index,
                                           char* & beginningOfUnused)
{
    Init();
    if (!isNull)
    {
        RepositoryKeeper::GetBlobLayout()->CreateAndSetMemberWithOptionalString(blob,
                                                         hashVal,
                                                         strVal,
                                                         stringLength,
                                                         member,
                                                         index,
                                                         isChanged,
                                                         beginningOfUnused);
    }
    RepositoryKeeper::GetBlobLayout()->SetMemberStatus(isNull, isChanged, blob, member, index);
}

void DotsC_SetEntityIdMemberInPreallocated(const DotsC_EntityId & entityId,
                                           const char* const instanceIdStr,
                                           const DotsC_Int32 stringLength,
                                           const bool isNull,
                                           const bool isChanged,
                                           char* const blob,
                                           const DotsC_MemberIndex member,
                                           const DotsC_ArrayIndex index,
                                           char* & beginningOfUnused)
{
    Init();
    if (!isNull)
    {
        RepositoryKeeper::GetBlobLayout()->CreateAndSetMemberWithOptionalString(blob,
                                                         entityId,
                                                         instanceIdStr,
                                                         stringLength,
                                                         member,
                                                         index,
                                                         isChanged,
                                                         beginningOfUnused);
    }
    RepositoryKeeper::GetBlobLayout()->SetMemberStatus(isNull, isChanged, blob, member, index);
}

void DotsC_GetPropertyMappingKind(const DotsC_TypeId typeId,
                                  const DotsC_TypeId propertyId,
                                  const DotsC_MemberIndex member,
                                  DotsC_PropertyMappingKind & mappingKind,
                                  DotsC_ErrorCode & errorCode)
{
    Init();
    errorCode=NoError;

    bool isInherited;
    const PropertyMappingDescriptionShm* pmd=RepositoryKeeper::GetRepository()->GetClass(typeId)->GetPropertyMapping(propertyId, isInherited);

    if (pmd==NULL)
    {
        errorCode=IllegalValue;
        return;
    }

    const MemberMappingDescriptionShm* mm=pmd->GetMemberMapping(member);

    if (mm==NULL)
    {
        errorCode=IllegalValue;
    }
    else
    {
        mappingKind=mm->GetMappingKind();
    }
}


void DotsC_GetClassMemberReference(const DotsC_TypeId typeId,
                                   const DotsC_TypeId propertyId,
                                   const DotsC_MemberIndex member,
                                   const DotsC_Int32* & classMemberReference,
                                   DotsC_Int32 & classMemberReferenceSize)
{
    Init();
    classMemberReference=NULL;
    classMemberReferenceSize=0;

    bool isInherited;    
    const PropertyMappingDescriptionShm* pmd=RepositoryKeeper::GetRepository()->GetClass(typeId)->GetPropertyMapping(propertyId, isInherited);

    if (pmd==NULL)
    {
        return;
    }

    const MemberMappingDescriptionShm* mm=pmd->GetMemberMapping(member);

    if (mm==NULL || mm->GetMappingKind()!=MappedToMember)
    {
        return;
    }

    classMemberReference=mm->GetRawMemberRef();
    classMemberReferenceSize=mm->MemberReferenceDepth()*2;
}


void DotsC_GetEnumerationChecksum(const DotsC_TypeId typeId,
                                  DotsC_TypeId & checksum)
{
    Init();
    const EnumDescriptionShm* ed=RepositoryKeeper::GetRepository()->GetEnum(typeId);
    if (ed==NULL)
    {
        checksum=0;
    }
    else
    {
        checksum=ed->GetCheckSum();
    }
}

void DotsC_GetBooleanPropertyParameter(const DotsC_TypeId typeId,
                                       const DotsC_TypeId propertyId,
                                       const DotsC_MemberIndex member,
                                       const DotsC_ArrayIndex index,
                                       bool & val)
{
    Init();
    const ParameterDescriptionShm* parameter;
    int paramIndex;
    if (GetPropertyParameterInternal(typeId, propertyId, member, index, parameter, paramIndex))
    {
        val=parameter->GetBoolValue(paramIndex);
    }
}

void DotsC_GetEnumerationPropertyParameter( const DotsC_TypeId typeId,
                                            const DotsC_TypeId propertyId,
                                            const DotsC_MemberIndex member,
                                            const DotsC_ArrayIndex index,
                                            DotsC_EnumerationValue & val)
{
    Init();
    const ParameterDescriptionShm* parameter;
    int paramIndex;
    if (GetPropertyParameterInternal(typeId, propertyId, member, index, parameter, paramIndex))
    {
        val=parameter->GetInt32Value(paramIndex);
    }
}

void DotsC_GetInt32PropertyParameter(const DotsC_TypeId typeId,
                                     const DotsC_TypeId propertyId,
                                     const DotsC_MemberIndex member,
                                     const DotsC_ArrayIndex index,
                                     DotsC_Int32 & val)
{
    Init();
    const ParameterDescriptionShm* parameter;
    int paramIndex;
    if (GetPropertyParameterInternal(typeId, propertyId, member, index, parameter, paramIndex))
    {
        val=parameter->GetInt32Value(paramIndex);
    }
}

void DotsC_GetInt64PropertyParameter(const DotsC_TypeId typeId,
                                     const DotsC_TypeId propertyId,
                                     const DotsC_MemberIndex member,
                                     const DotsC_ArrayIndex index,
                                     DotsC_Int64 & val)
{
    Init();
    const ParameterDescriptionShm* parameter;
    int paramIndex;
    if (GetPropertyParameterInternal(typeId, propertyId, member, index, parameter, paramIndex))
    {
        val=parameter->GetInt64Value(paramIndex);
    }
}

void DotsC_GetFloat32PropertyParameter(const DotsC_TypeId typeId,
                                       const DotsC_TypeId propertyId,
                                       const DotsC_MemberIndex member,
                                       const DotsC_ArrayIndex index,
                                       DotsC_Float32 & val)
{
    Init();
    const ParameterDescriptionShm* parameter;
    int paramIndex;
    if (GetPropertyParameterInternal(typeId, propertyId, member, index, parameter, paramIndex))
    {
        val=parameter->GetFloat32Value(paramIndex);
    }
}

void DotsC_GetFloat64PropertyParameter(const DotsC_TypeId typeId,
                                       const DotsC_TypeId propertyId,
                                       const DotsC_MemberIndex member,
                                       const DotsC_ArrayIndex index,
                                       DotsC_Float64 & val)
{
    Init();
    const ParameterDescriptionShm* parameter;
    int paramIndex;
    if (GetPropertyParameterInternal(typeId, propertyId, member, index, parameter, paramIndex))
    {
        val=parameter->GetFloat64Value(paramIndex);
    }
}

void DotsC_GetStringPropertyParameter(const DotsC_TypeId typeId,
                                      const DotsC_TypeId propertyId,
                                      const DotsC_MemberIndex member,
                                      const DotsC_ArrayIndex index,
                                      const char* & val)
{
    Init();
    const ParameterDescriptionShm* parameter;
    int paramIndex;
    if (GetPropertyParameterInternal(typeId, propertyId, member, index, parameter, paramIndex))
    {
        val=parameter->GetStringValue(paramIndex);
    }
}

void DotsC_GetTypeIdPropertyParameter(const DotsC_TypeId typeId,
                                      const DotsC_TypeId propertyId,
                                      const DotsC_MemberIndex member,
                                      const DotsC_ArrayIndex index,
                                      DotsC_TypeId & val)
{
    Init();
    const ParameterDescriptionShm* parameter;
    int paramIndex;
    if (GetPropertyParameterInternal(typeId, propertyId, member, index, parameter, paramIndex))
    {
        val=parameter->GetInt64Value(paramIndex);
    }
}

void DotsC_GetHashedIdPropertyParameter(const DotsC_TypeId typeId,
                                        const DotsC_TypeId propertyId,
                                        const DotsC_MemberIndex member,
                                        const DotsC_ArrayIndex index,
                                        DotsC_Int64 & hashVal,
                                        const char* & strVal)
{
    Init();
    const ParameterDescriptionShm* parameter;
    int paramIndex;
    if (GetPropertyParameterInternal(typeId, propertyId, member, index, parameter, paramIndex))
    {
        std::pair<DotsC_Int64, const char*> val=parameter->GetHashedValue(paramIndex);
        hashVal=val.first;
        strVal=val.second;
    }
}


void DotsC_GetEntityIdPropertyParameter(const DotsC_TypeId typeId,
                                        const DotsC_TypeId propertyId,
                                        const DotsC_MemberIndex member,
                                        const DotsC_ArrayIndex index,
                                        DotsC_EntityId & entityId,
                                        const char* & instanceIdStr)
{
    Init();
    const ParameterDescriptionShm* parameter;
    int paramIndex;
    if (GetPropertyParameterInternal(typeId, propertyId, member, index, parameter, paramIndex))
    {
        entityId.typeId=parameter->GetInt64Value(paramIndex);
        std::pair<DotsC_Int64, const char*> val=parameter->GetHashedValue(paramIndex);
        entityId.instanceId=val.first;
        instanceIdStr=val.second;
    }
}

void DotsC_GetObjectPropertyParameter(const DotsC_TypeId typeId,
                                      const DotsC_TypeId propertyId,
                                      const DotsC_MemberIndex member,
                                      const DotsC_ArrayIndex index,
                                      const char* & val)
{
    Init();
    const ParameterDescriptionShm* parameter;
    int paramIndex;
    if (GetPropertyParameterInternal(typeId, propertyId, member, index, parameter, paramIndex))
    {
        val=parameter->GetObjectValue(paramIndex).first;
    }
}

void DotsC_GetBinaryPropertyParameter(const DotsC_TypeId typeId,
                                      const DotsC_TypeId propertyId,
                                      const DotsC_MemberIndex member,
                                      const DotsC_ArrayIndex index,
                                      const char* & val,
                                      DotsC_Int32& size)
{
    Init();
    const ParameterDescriptionShm* parameter;
    int paramIndex;
    if (GetPropertyParameterInternal(typeId, propertyId, member, index, parameter, paramIndex))
    {
        std::pair<const char*, size_t> bin=parameter->GetBinaryValue(paramIndex);
        val=bin.first;
        size=static_cast<DotsC_Int32>(bin.second);
    }
}

void DotsC_SetException(const DotsC_TypeId exceptionId, const char* const description)
{
    Init();
    ExceptionKeeper::Instance().Set(exceptionId, description);
}

void DotsC_AppendExceptionDescription(const char* const moreDescription)
{
    Init();
    ExceptionKeeper::Instance().AppendDescription(moreDescription);
}



void DotsC_GetAndClearException(DotsC_TypeId & exceptionId, char* & description, DotsC_BytePointerDeleter & deleter, bool & wasSet)
{
    Init();
    std::string desc;
    wasSet=ExceptionKeeper::Instance().GetAndClear(exceptionId, desc);
    if (!wasSet)
    {
        return;
    }
    description=new char [desc.size()+1];
    memcpy(description,desc.c_str(),desc.size());
    description[desc.size()]='\0';
    deleter=DeleteBytePointer;
}

void DotsC_PeekAtException(DotsC_TypeId & exceptionId)
{
    Init();
    std::string desc;
    ExceptionKeeper::Instance().Peek(exceptionId,desc);
}


void DotsC_GetDouFilePathForType(const DotsC_TypeId typeId,
                                 char* const buf,
                                 const DotsC_Int32 bufSize,
                                 DotsC_Int32 & resultSize)
{
    Init();

    const char* file=NULL;

    const ClassDescriptionShm* cd=RepositoryKeeper::GetRepository()->GetClass(typeId);
    if (cd!=NULL)
    {
        file=cd->FileName();
    }
    else
    {
        const EnumDescriptionShm* ed=RepositoryKeeper::GetRepository()->GetEnum(typeId);
        if (ed!=NULL)
        {
            file=ed->FileName();
        }
        else
        {
            const PropertyDescriptionShm* pd=RepositoryKeeper::GetRepository()->GetProperty(typeId);
            if (pd!=NULL)
            {
                file=pd->FileName();
            }
            else
            {
                const ExceptionDescriptionShm* ex=RepositoryKeeper::GetRepository()->GetException(typeId);
                if (ex!=NULL)
                {
                    file=ex->FileName();
                }
            }
        }
    }

    if (file==NULL)
    {
        //not found
        resultSize=-1;
        return;
    }

    resultSize=strlen(file)+1; //add one for null termination
    if (resultSize<=bufSize)
    {
        strncpy(buf, file, resultSize);
    }
}

const char* DotsC_GetDouFilePath(const DotsC_TypeId typeId)
{
    const ClassDescriptionShm* cd=RepositoryKeeper::GetRepository()->GetClass(typeId);
    if (cd!=NULL)
    {
        return cd->FileName();
    }
    else
    {
        const EnumDescriptionShm* ed=RepositoryKeeper::GetRepository()->GetEnum(typeId);
        if (ed!=NULL)
        {
            return ed->FileName();
        }
        else
        {
            const PropertyDescriptionShm* pd=RepositoryKeeper::GetRepository()->GetProperty(typeId);
            if (pd!=NULL)
            {
                return pd->FileName();
            }
            else
            {
                const ExceptionDescriptionShm* ex=RepositoryKeeper::GetRepository()->GetException(typeId);
                if (ex!=NULL)
                {
                    return ex->FileName();
                }
            }
        }
    }

    return NULL;
}

bool DotsC_TypeRepositoryLoadedByThisProcess()
{
    Init();
    return RepositoryKeeper::RepositoryCreatedByThisProcess();
}

void DotsC_GetTypeDescription(const DotsC_TypeId typeId,
                              char * const buf,
                              const DotsC_Int32 bufSize,
                              DotsC_Int32 & resultSize)
{
    Init();
    std::ostringstream os;
    if (typeId==0)
    {
        Safir::Dob::Typesystem::Internal::RepositoryToString(RepositoryKeeper::GetRepository(), false, os);
    }
    else
    {
        Safir::Dob::Typesystem::Internal::TypeToString(RepositoryKeeper::GetRepository(), typeId, os);
    }

    std::string text=os.str();
    resultSize=static_cast<DotsC_Int32>(text.size())+1; //add one for null char
    if (resultSize>bufSize)
    {
        return;
    }

    strncpy(buf, &text[0], resultSize);
}
