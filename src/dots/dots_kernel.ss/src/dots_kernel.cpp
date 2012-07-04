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

#include <Safir/Dob/Typesystem/Internal/Kernel.h>
#include <Safir/Dob/Typesystem/Internal/Id.h>
#include "dots_blob_layout.h"
#include "dots_repository.h"
#include "dots_basic_types.h"
#include "dots_xml_serializer.h"
#include "dots_blob_serializer.h"
#include <Safir/Dob/Typesystem/Internal/KernelDefs.h>
#include <iostream>
#include <boost/static_assert.hpp>
#include "dots_exception_keeper.h"
#include "dots_base64_conversions.h"

#include <Safir/Utilities/Internal/LowLevelLogger.h>

#include <boost/filesystem/fstream.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/exception.hpp>
#include <boost/thread/once.hpp>

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

#include <boost/thread/mutex.hpp>

#include <boost/regex.hpp>
#include "dots_property_helper_functions.h"

using namespace Safir::Dob::Typesystem::Internal;

namespace 
{
    //check size of type definitions
    BOOST_STATIC_ASSERT(sizeof(DotsC_Int32) == 4);
    BOOST_STATIC_ASSERT(sizeof(DotsC_Int64) == 8);
    BOOST_STATIC_ASSERT(sizeof(DotsC_Float32) == 4);
    BOOST_STATIC_ASSERT(sizeof(DotsC_Float64) == 8);
    BOOST_STATIC_ASSERT(sizeof(UInt32) == 4);
    BOOST_STATIC_ASSERT(sizeof(UInt64) == 8);
    BOOST_STATIC_ASSERT(sizeof(DotsC_EntityId) == sizeof(DotsC_TypeId) + sizeof(Int64));
    BOOST_STATIC_ASSERT(sizeof(DotsC_EntityId) == 16);
    BOOST_STATIC_ASSERT(sizeof(bool) == 1);

    
    void CALLING_CONVENTION DeleteBytePointer(char * & ptr)
    {
        if (ptr != NULL)
        {
            delete [] ptr;
            ptr = NULL;
        }
    }
    
    class InitHelper
    {
    private:
        friend void Init();
        static boost::once_flag initFlag;
        static void Init() 
        {
            Repository::Initialize();
        }
    };
    boost::once_flag InitHelper::initFlag = BOOST_ONCE_INIT;
    
    void Init()
    {
        boost::call_once(InitHelper::initFlag,InitHelper::Init);
    }


    static boost::once_flag createXmlToBlobLockFlag = BOOST_ONCE_INIT;
    boost::mutex& getXmlToBlobLock()
    {
        static boost::mutex mtx;
        return mtx;
    }


    boost::filesystem::path GetLogDirectory()
    {
        const char * ENV_NAME = "SAFIR_RUNTIME";
        char * env = getenv(ENV_NAME);
        if (env == NULL)
        {
            return "";
        }
        boost::filesystem::path filename(env,boost::filesystem::native);

        filename /= "log";
        filename /= "dump";

        try
        {
            boost::filesystem::create_directory(filename);
        }
        catch (const boost::filesystem::filesystem_error &)
        {
            std::wcout << "Failed to create directory for binary dumps!!" <<std::endl;
            return "";
        }
        return filename;
    }

    std::string MakeFileName(const std::string & filenamePart, int no)
    {
        const boost::posix_time::ptime now = boost::posix_time::second_clock::universal_time();
        std::string filename = filenamePart + "_" + boost::posix_time::to_iso_string(now) + "-" + boost::lexical_cast<std::string>(no) + ".bin";
        if (!boost::filesystem::portable_name(filename))
        {
            filename = boost::regex_replace(filename,boost::regex("[^0-9a-zA-Z\\._-]"),"_");
            if (!boost::filesystem::portable_name(filename))
            {
                filename = "strange_exe_name";
            }
        }
        return filename;
    }

}

//********************************************************
//* Type information operations
//********************************************************
Int32 DotsC_NumberOfTypeIds()
{
    Init();
    return DotsC_NumberOfClasses()+DotsC_NumberOfProperties()+DotsC_NumberOfEnumerations()+DotsC_NumberOfExceptions();
}

//Get the number of classes defined in the system
Int32 DotsC_NumberOfClasses()
{
    Init();
    return Repository::Classes().NumberOfClasses();
}

Int32 DotsC_NumberOfProperties()
{
    Init();
    return Repository::Properties().NumberOfProperties();
}

Int32 DotsC_NumberOfEnumerations()
{
    Init();
    return Repository::Enums().NumberOfEnums();
}

Int32 DotsC_NumberOfExceptions()
{
    Init();
    return Repository::Exceptions().NumberOfExceptions();
}

//Get a list of all type id's that exists in the system. Buf is a pointer to an array of size bufSize. The
//out parameter size defines how many type id's that were inserted into buf.
void DotsC_GetAllTypeIds(TypeId* buf, Int32 bufSize, Int32& size)
{
    Init();
    Int32 noClasses = 0;
    Int32 noProps = 0;
    Int32 noEnums = 0;
    Int32 noExceptions = 0;
    Repository::Classes().GetTypeIds(buf, bufSize, noClasses);
    Repository::Properties().GetTypeIds(buf+noClasses, bufSize-noClasses, noProps);
    Repository::Enums().GetTypeIds(buf+noClasses+noProps, bufSize-noClasses-noProps, noEnums);
    Repository::Exceptions().GetTypeIds(buf+noClasses+noProps+noEnums,bufSize-noClasses-noProps-noEnums,noExceptions);
    size=noClasses+noProps+noEnums+noExceptions;
}

bool DotsC_TypeExists(const TypeId typeId)
{
    Init();
    return (DotsC_IsClass(typeId) || DotsC_IsProperty(typeId) || DotsC_IsEnumeration(typeId) || DotsC_IsException(typeId));
}

bool DotsC_IsClass(const TypeId typeId)
{
    Init();
    return Repository::Classes().FindClass(typeId) != NULL;
}

bool DotsC_IsProperty(const TypeId typeId)
{
    Init();
    return Repository::Properties().FindProperty(typeId) != NULL;
}

bool DotsC_IsEnumeration(const TypeId typeId)
{
    Init();
    return Repository::Enums().FindEnum(typeId) != NULL;
}

bool DotsC_IsException(const TypeId typeId)
{
    Init();
    return Repository::Exceptions().FindException(typeId) != NULL;
}

TypeId DotsC_TypeIdFromName(const char* typeName)
{
    Init();
    return DotsId_Generate64(typeName);
}

const char* DotsC_GetTypeName(const TypeId typeId)
{
    Init();
    const ClassDescription * const cd=Repository::Classes().FindClass(typeId);
    if (cd!=NULL)
    {
        return cd->Name();
    }

    const PropertyDescription * const pd=Repository::Properties().FindProperty(typeId);
    if (pd!=NULL)
    {
        return pd->Name();
    }

    const EnumDescription * const ed=Repository::Enums().FindEnum(typeId);
    if (ed!=NULL)
    {
        return ed->Name();
    }

    const ExceptionDescription * const excD = Repository::Exceptions().FindException(typeId);
    if (excD != NULL)
    {
        return excD->Name();
    }

    return NULL; //not found
}


Int32 DotsC_GetNumberOfEnumerationValues(const TypeId enumId)
{
    Init();
    const EnumDescription * const ed=Repository::Enums().FindEnum(enumId);
    if (ed!=NULL)
    {
        return ed->NumberOfValues();
    }
    return -1;
}

const char* DotsC_GetEnumerationValueName(const TypeId enumId, EnumerationValue enumVal)
{
    Init();
    const EnumDescription * const ed=Repository::Enums().FindEnum(enumId);
    if (ed != NULL)
    {
        return ed->ValueName(enumVal);
    }
    return NULL;
}

EnumerationValue DotsC_EnumerationValueFromName(const TypeId enumId, const char* enumValueName)
{
    Init();
    const EnumDescription * const ed=Repository::Enums().FindEnum(enumId);
    if (ed!=NULL)
    {
        return static_cast<EnumerationValue>(ed->IndexOf(enumValueName));
    }
    return -1;
}

//********************************************************
//* Base operations on blobs
//********************************************************
//create default blob
void DotsC_CreateBlob(const TypeId typeId,
                      char * & blob)
{
    Init();
    BlobLayout::CreateBlob(typeId, blob);
}

void DotsC_DeleteBlob(char * & blob)
{
    Init();
    BlobLayout::DeleteBlob(blob);
}

void DotsC_CreateCopyOfBlob(char * & to,
                            const char * const from)
{
    Init();
    const size_t size=BlobLayout::GetSize(from);
    to=new char[size];
    memcpy(to, from, size);
}

DotsC_TypeId DotsC_GetTypeId(const char * const blob)
{
    Init();
    return BlobLayout::GetTypeId(blob);
}

//Gives the total size of the blob
Int32 DotsC_GetSize(const char * const blob)
{
    Init();
    return BlobLayout::GetSize(blob);
}

// IsAnythingChanged
bool DotsC_IsAnythingChanged(const char * const blob)
{
    Init();
    return BlobLayout::IsAnythingChanged(blob);
}


//Reset changed flags for the members
void DotsC_ResetChanged(char * const blob)
{
    Init();
    BlobLayout::ResetChanged(blob);
}

//Reset changed flags for the members
void DotsC_SetChanged(char * const blob,
                      const bool changed)
{
    Init();
    BlobLayout::SetChanged(blob,changed);
}

void DotsC_SetChangedHere(char * const blob,
                          const DotsC_MemberIndex member,
                          const DotsC_ArrayIndex index,
                          const bool changed)
{
    Init();
    const InternalMemberStatus status=BlobLayout::GetStatus(blob, member, index);
    BlobLayout::SetStatus(MemberStatusHandler::IsNull(status),changed,blob,member,index);
}

void DotsC_SetChangedMembers(const char * const val, char * & blob)
{
    Init();
    BlobLayout::MergeChanges(val, blob);
}


void DotsC_SetChangedSinceLastRead(const char * const lastRead,
                                   char * const current)
{
    Init();
    BlobLayout::SetChangedSinceLastRead(lastRead, current);
}

//***********************************************************
//* Functions for retrieving member info about object types
//***********************************************************
//Functions for retrieving member info about object types
Int32 DotsC_GetNumberOfMembers(const TypeId typeId)
{
    Init();
    {
        const ClassDescription * const cd=Repository::Classes().FindClass(typeId);
        if (cd!=NULL)
        {
            return cd->NumberOfMembers();
        }
    }

    {
        const PropertyDescription * const pd=Repository::Properties().FindProperty(typeId);
        if (pd!=NULL)
        {
            return pd->NumberOfMembers();
        }
    }
    return -1;
}

MemberIndex DotsC_GetMemberId(const TypeId typeId,
                              const char * const memberName)
{
    Init();
    {
        const ClassDescription * const cd=Repository::Classes().FindClass(typeId);
        if (cd != NULL)
        {
            const int memberIndex = cd->GetMemberIndexFromName(memberName);
            if (memberIndex != -1)
            {
                return memberIndex;
            }
        }
    }

    {
        const PropertyDescription * const pd=Repository::Properties().FindProperty(typeId);
        if (pd!=NULL)
        {
            const int memberIndex = pd->GetMemberIndexFromName(memberName);
            if (memberIndex != -1)
            {
                return memberIndex ;
            }
        }
    }
    return -1;
}

const char* DotsC_GetMemberName(const TypeId typeId, const MemberIndex member)
{
    Init();
    {
        const ClassDescription * const cd=Repository::Classes().FindClass(typeId);
        if (cd!=NULL)
        {
            return cd->GetMember(member)->Name();
        }
    }

    {
        const PropertyDescription * const pd=Repository::Properties().FindProperty(typeId);
        if (pd!=NULL)
        {
            return pd->GetMember(member)->Name();
        }
    }
    return NULL;
}

TypeId DotsC_GetComplexMemberTypeId(const TypeId typeId, const MemberIndex member)
{
    Init();
    const ClassDescription * const cd=Repository::Classes().FindClass(typeId);
    const MemberDescription * md=NULL;
    if (cd!=NULL)
    {
        md = cd->GetMember(member);
    }
    else
    {
        const PropertyDescription * const pd=Repository::Properties().FindProperty(typeId);
        if (pd!=NULL)
        {
            md = pd->GetMember(member);
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

void DotsC_GetMemberInfo(const TypeId typeId,  //in
                         const MemberIndex member,  //in
                         MemberType& memberType,//out
                         const char* & memberName,           //out
                         TypeId & complexType,   //out
                         Int32 & stringLength,   //out
                         bool & isArray,                      //out
                         Int32 & arrayLength)    //out
{
    Init();
    const MemberDescription * memberDesc;
    complexType = -1;
    const ClassDescription * const cd=Repository::Classes().FindClass(typeId);
    stringLength = -1;
    if (cd!=NULL)
    {
        memberDesc = cd->GetMember(member);
        if (memberDesc == NULL)
        {
            // there is no error code, so set all out fields to invalid (-1 or null)
            memberName = NULL;
            complexType = -1;
            arrayLength = -1;
            return;
        }
    }
    else
    {
        const PropertyDescription * const pd=Repository::Properties().FindProperty(typeId);
        if (pd == NULL)
        {
            // there is no error code, so set all out fields to invalid (-1 or null)
            memberName = NULL;
            complexType = -1;
            arrayLength = -1;
            return;
        }
        memberDesc= pd->GetMember(member);
    }

    if (memberDesc->GetMemberType() == StringMemberType)
    {
        stringLength=memberDesc->DataLength();
    }

    memberType=memberDesc->GetMemberType();
    memberName=memberDesc->Name();
    isArray=memberDesc->ArrayLength() > 1;
    arrayLength=memberDesc->ArrayLength();
    if (memberType==ObjectMemberType || memberType == EnumerationMemberType)
    {
        complexType = memberDesc->GetTypeId();
    }
}

Int32 DotsC_GetMemberArraySize(const TypeId typeId, const MemberIndex member)
{
    Init();
    const ClassDescription * const cd=Repository::Classes().FindClass(typeId);
    if (cd!=NULL)
    {
        return cd->GetMember(member)->ArrayLength();
    }
    return -1;
}

Int32 DotsC_GetStringMemberMaxLength(const TypeId typeId, const MemberIndex member)
{
    Init();
    const ClassDescription  * const cd=Repository::Classes().FindClass(typeId);
    if (cd == NULL)
    {
        return -1;
    }

    const MemberDescription * const memberDesc= cd->GetMember(member);
    if (memberDesc->GetMemberType() == StringMemberType)
    {
        return memberDesc->DataLength();
    }
    else
    {
        return -1;
    }
}

Int32 DotsC_GetMemberArraySizeProperty(const TypeId classId, const TypeId propertyId, const MemberIndex propertyMember)
{
    Init();
    bool isInherited;
    const PropertyMappingDescription * const pmd = Repository::Classes().FindClass(classId)->FindPropertyMapping(propertyId, isInherited);
    if (pmd == NULL)
    {
        return -1;
    }

    const MemberMapping * const mm = pmd->GetMemberMapping(propertyMember);
    if (mm == NULL)
    {
        return -1;
    }

    switch (mm->GetMappingKind())
    {
    case MappedToNull:
        return 1; //it is one long (just the null...)

    case MappedToMember:
        {
            const ClassMemberReference * cmr = mm->GetMemberReference();

            TypeId parent = classId;

            //Follow the refe6rence into objects (the last one is skipped since that goes into a member, not an object)
            for (Size ii = 0; ii < cmr->size() - 1; ++ii)
            {
                MemberReferenceElement ref = cmr->at(ii);//*cmr->Get(&Repository::m_pool,ii);
                parent = DotsC_GetComplexMemberTypeId(parent,ref.m_classMember);
            }
            MemberReferenceElement ref = cmr->back();

            if (ref.m_index != -1) //not an array
            {
                return 1;
            }
            else
            {
                return DotsC_GetMemberArraySize(parent, ref.m_classMember);
            }
        }
        break;
    case MappedToParameter:
        return mm->GetParameter()->ArrayLength();
    }

    //will never get here!
    return -1;
}

Int32 DotsC_GetStringMemberMaxLengthProperty(const TypeId classId, const TypeId propertyId, const MemberIndex propertyMember)
{
    Init();
    bool isInherited;
    const PropertyMappingDescription * const pmd = Repository::Classes().FindClass(classId)->FindPropertyMapping(propertyId, isInherited);
    if (pmd == NULL)
    {
        return -1;
    }

    const MemberMapping * const mm = pmd->GetMemberMapping(propertyMember);
    if (mm == NULL)
    {
        return -1;
    }

    switch (mm->GetMappingKind())
    {
    case MappedToNull:
        return 1; //it is one long (just the null...)

    case MappedToMember:
        {
            const ClassMemberReference * cmr = mm->GetMemberReference();

            TypeId parent = classId;

            //Follow the refe6rence into objects (the last one is skipped since that goes into a member, not an object)
            for (Size ii = 0; ii < cmr->size() - 1; ++ii)
            {
                MemberReferenceElement ref = cmr->at(ii);//*cmr->Get(&Repository::m_pool,ii);
                parent = DotsC_GetComplexMemberTypeId(parent,ref.m_classMember);
            }
            MemberReferenceElement ref = cmr->back();

            return DotsC_GetStringMemberMaxLength(parent, ref.m_classMember);
        }
        break;
    case MappedToParameter:
        // AWI: Vad göra här? I daxläget finns inte informationen för parameterar
        //return mm->GetParameter()->ArrayLength();
        break;
    }

    //will never get here!
    return -1;
}

const char* DotsC_GetMemberTypeName(const TypeId typeId, const MemberIndex member)
{
    Init();
    const ClassDescription * const cd=Repository::Classes().FindClass(typeId);
    if (cd!=NULL)
    {
        return BasicTypes::StringOf(cd->GetMember(member)->GetMemberType());
    }
    else
    {
        const PropertyDescription * const pd=Repository::Properties().FindProperty(typeId);
        if (pd != NULL)
        {
            return BasicTypes::StringOf(pd->GetMember(member)->GetMemberType());
        }
        else
        {
            return NULL;
        }
    }

}

 //***********************************************************************
 //* Functions retrieving definitions of parameter values in object types
 //***********************************************************************
Int32 DotsC_GetNumberOfParameters(const TypeId typeId)
{
    Init();
    const ClassDescription * const cd=Repository::Classes().FindClass(typeId);
    if (cd != NULL)
    {
        return cd->NumberOfParameters();
    }
    else
    {
        return -1;
    }

}

ParameterIndex DotsC_GetParameterId(const TypeId typeId, const char* parameterName)
{
    Init();
    const ClassDescription * const cd=Repository::Classes().FindClass(typeId);
    if (cd == NULL)
    {
        return -1;
    }

    return cd->GetParameterIndexFromName(parameterName);
}

const char* DotsC_GetParameterName(const TypeId typeId, const ParameterIndex parameter)
{
    Init();
    return Repository::Classes().FindClass(typeId)->GetParameter(parameter)->Name();
}

MemberType DotsC_GetParameterType(const TypeId typeId, const ParameterIndex parameter)
{
    Init();
    return Repository::Classes().FindClass(typeId)->GetParameter(parameter)->GetMemberType();
}

const char* DotsC_GetParameterTypeName(const TypeId typeId, const ParameterIndex parameter)
{
    Init();
    return BasicTypes::StringOf(Repository::Classes().FindClass(typeId)->GetParameter(parameter)->GetMemberType());
}

Int32 DotsC_GetParameterArraySize(const TypeId typeId, const ParameterIndex parameter)
{
    Init();
    return Repository::Classes().FindClass(typeId)->GetParameter(parameter)->ArrayLength();
}

//************************************************************************************
//* Functions for retrieving member values
//************************************************************************************

bool DotsC_IsNullMember(const char * const blob, const MemberIndex member, const ArrayIndex index)
{
    Init();
    InternalMemberStatus status=BlobLayout::GetStatus(blob, member, index);
    return MemberStatusHandler::IsNull(status);
}

bool DotsC_IsChangedMember(const char * const blob, const MemberIndex member, const ArrayIndex index)
{
    Init();
    InternalMemberStatus status=BlobLayout::GetStatus(blob, member, index);
    if (MemberStatusHandler::HasChanged(status))
    {
        return true;
    }

    const ClassDescription * const cd=Repository::Classes().FindClass(BlobLayout::GetTypeId(blob));
    if (cd == NULL)
    {
        return false;
    }

    const MemberDescription * const md= cd->GetMember(member);

    if (md->GetMemberType() == ObjectMemberType && !MemberStatusHandler::IsNull(status))
    {
        Int32 dummy=0;
        const char * childBlob;
        BlobLayout::GetDynamicMember(blob, member, index, childBlob, dummy);
        return BlobLayout::IsAnythingChanged(childBlob);
    }
    else
    {
        return false;
    }
}


void DotsC_GetBooleanMember(const char * const blob, const MemberIndex member, const ArrayIndex index, bool& val, bool& isNull, bool& isChanged)
{
    Init();
    MemberStatusHandler::ToExternalFormat(BlobLayout::GetMember<bool>(blob, member, index, val), isNull, isChanged);
}

void DotsC_GetEnumerationMember(const char * const blob,
                                const MemberIndex member,
                                const ArrayIndex index,
                                EnumerationValue & val,
                                bool& isNull,
                                bool& isChanged)
{
    Init();
    Safir::Dob::Typesystem::Internal::EnumInternal tmp;
    MemberStatusHandler::ToExternalFormat(BlobLayout::GetMember<Safir::Dob::Typesystem::Internal::EnumInternal>(blob, member, index, tmp), isNull, isChanged);
    val=tmp;
}

void DotsC_GetInt32Member(const char * const blob, const MemberIndex member, const ArrayIndex index, Int32& val, bool& isNull, bool& isChanged)
{
    Init();
    MemberStatusHandler::ToExternalFormat(BlobLayout::GetMember<Int32>(blob, member, index, val), isNull, isChanged);
}


void DotsC_GetInt64Member(const char * const blob, const MemberIndex member, const ArrayIndex index, Int64& val, bool& isNull, bool& isChanged)
{
    Init();
    MemberStatusHandler::ToExternalFormat(BlobLayout::GetMember<Int64>(blob, member, index, val), isNull, isChanged);
}

void DotsC_GetFloat32Member(const char * const blob, const MemberIndex member, const ArrayIndex index, Float32& val, bool& isNull, bool& isChanged)
{
    Init();
    MemberStatusHandler::ToExternalFormat(BlobLayout::GetMember<Float32>(blob, member, index, val), isNull, isChanged);
    if (isNull)
    {
        //we have to set a proper value, since an unitialized value may cause
        //FP errors, e.g. in Ada
        val = 0.0f;
    }
}

void DotsC_GetFloat64Member(const char * const blob, const MemberIndex member, const ArrayIndex index, Float64& val, bool& isNull, bool& isChanged)
{
    Init();
    MemberStatusHandler::ToExternalFormat(BlobLayout::GetMember<Float64>(blob, member, index, val), isNull, isChanged);
    if (isNull)
    {
        //we have to set a proper value, since an unitialized value may cause
        //FP errors, e.g. in Ada
        val = 0.0;
    }
}

void DotsC_GetStringMember(const char * const blob, const MemberIndex member, const ArrayIndex index, const char* &val, bool& isNull, bool& isChanged)
{
    Init();
    Int32 dummy=0;
    MemberStatusHandler::ToExternalFormat(BlobLayout::GetDynamicMember(blob, member, index, val, dummy), isNull, isChanged);
}

void DotsC_GetObjectMember(const char * const blob,
                           const MemberIndex member,
                           const ArrayIndex index,
                           const char * & val,
                           bool & isNull,
                           bool & isChanged)
{
    Init();
    Int32 dummy=0;
    MemberStatusHandler::ToExternalFormat(BlobLayout::GetDynamicMember(blob, member, index, val, dummy), isNull, isChanged);
}

void DotsC_GetWriteableObjectMember(char * const blob,
                                    const MemberIndex member,
                                    const ArrayIndex index,
                                    char * & val,
                                    bool & isNull,
                                    bool & isChanged)
{
    Init();
    Int32 dummy=0;
    MemberStatusHandler::ToExternalFormat(BlobLayout::GetDynamicMember(blob, member, index, val, dummy), isNull, isChanged);
}


void DotsC_GetBinaryMember(const char * const blob,
                           const DotsC_MemberIndex member,
                           const DotsC_ArrayIndex index,
                           const char* &val,
                           Int32& size,
                           bool & isNull,
                           bool & isChanged)
{
    Init();
    MemberStatusHandler::ToExternalFormat(BlobLayout::GetDynamicMember(blob, member, index, val, size), isNull, isChanged);
}

void DotsC_GetTypeIdMember(const char * const blob, const MemberIndex member, const ArrayIndex index, TypeId& val, bool & isNull, bool & isChanged)
{
    Init();
    MemberStatusHandler::ToExternalFormat(BlobLayout::GetMember<TypeId>(blob, member, index, val), isNull, isChanged);
}

void DotsC_GetHashedIdMember(const char * const blob,
                             const MemberIndex member,
                             const ArrayIndex index,
                             Int64 & hashVal,
                             const char * & strVal,
                             bool & isNull,
                             bool & isChanged)
{
    Init();
    MemberStatusHandler::ToExternalFormat(BlobLayout::GetMemberWithOptionalString(blob, member, index, hashVal, strVal), isNull, isChanged);
}

void DotsC_GetEntityIdMember(const char * const blob,
                             const MemberIndex member,
                             const ArrayIndex index,
                             DotsC_EntityId & entityId,
                             const char * & instanceIdStr,
                             bool & isNull,
                             bool & isChanged)
{
    Init();
    MemberStatusHandler::ToExternalFormat(BlobLayout::GetMemberWithOptionalString(blob, member, index, entityId, instanceIdStr), isNull, isChanged);
}

//************************************************************************************
//* Functions for setting member values
//************************************************************************************
void DotsC_SetNullMember(char * const blob,
                         const MemberIndex member,
                         const ArrayIndex index)
{
    Init();
    BlobLayout::SetStatus(true, true, blob, member, index);
}

void DotsC_SetBooleanMember(const bool val,
                            char * & blob,
                            const MemberIndex member,
                            const ArrayIndex index)
{
    Init();
    BlobLayout::SetMember<bool>(val, blob, member, index);
    BlobLayout::SetStatus(false, true, blob, member, index);
}

void DotsC_SetEnumerationMember(const EnumerationValue val,
                                char * & blob,
                                const MemberIndex member,
                                const ArrayIndex index)
{
    Init();
    Safir::Dob::Typesystem::Internal::EnumInternal tmp=static_cast<Safir::Dob::Typesystem::Internal::EnumInternal>(val);
    BlobLayout::SetMember<Safir::Dob::Typesystem::Internal::EnumInternal>(tmp, blob, member, index);
    BlobLayout::SetStatus(false, true, blob, member, index);
}


void DotsC_SetInt32Member(const Int32 i,
                          char * & blob,
                          const MemberIndex member,
                          const ArrayIndex index)
{
    Init();
    BlobLayout::SetMember<Int32>(i, blob, member, index);
    BlobLayout::SetStatus(false, true, blob, member, index);
}

void DotsC_SetInt64Member(const Int64 val,
                          char * & blob,
                          const MemberIndex member,
                          const ArrayIndex index)
{
    Init();
    BlobLayout::SetMember<Int64>(val, blob, member, index);
    BlobLayout::SetStatus(false, true, blob, member, index);
}

void DotsC_SetFloat32Member(const Float32 val,
                            char * & blob,
                            const MemberIndex member,
                            const ArrayIndex index)
{
    Init();
    BlobLayout::SetMember<Float32>(val, blob, member, index);
    BlobLayout::SetStatus(false, true, blob, member, index);
}

void DotsC_SetFloat64Member(const Float64 val,
                            char * & blob,
                            const MemberIndex member,
                            const ArrayIndex index)
{
    Init();
    BlobLayout::SetMember<Float64>(val, blob, member, index);
    BlobLayout::SetStatus(false, true, blob, member, index);
}

void DotsC_SetStringMember(const char * const val,
                           char * & blob,
                           const MemberIndex member,
                           const ArrayIndex index)
{
    Init();
    Size dummy=0;
    BlobLayout::SetDynamicMember(val, dummy, blob, member, index);
    BlobLayout::SetStatus(false, true, blob, member, index);
}

void DotsC_SetTypeIdMember(const TypeId val,
                           char * & blob,
                           const MemberIndex member,
                           const ArrayIndex index)
{
    Init();
    BlobLayout::SetMember<TypeId>(val, blob, member, index);
    BlobLayout::SetStatus(false, true, blob, member, index);
}

void DotsC_SetHashedIdMember(const Int64 hashVal,
                             const char * const strVal,
                             char * & blob,
                             const MemberIndex member,
                             const ArrayIndex index)
{
    Init();
    BlobLayout::SetMemberWithOptionalString(hashVal, strVal, blob, member, index);
    BlobLayout::SetStatus(false, true, blob, member, index);
}

void DotsC_SetEntityIdMember(const DotsC_EntityId & entityId,
                             const char * const instanceIdStr,
                             char * & blob,
                             const MemberIndex member,
                             const ArrayIndex index)
{
    Init();
    BlobLayout::SetMemberWithOptionalString(entityId, instanceIdStr, blob, member, index);
    BlobLayout::SetStatus(false, true, blob, member, index);
}

void DotsC_SetObjectMember(const char * const val, char * & blob, const MemberIndex member, const ArrayIndex index)
{
    Init();
    Size dummy=0;
    BlobLayout::SetDynamicMember(val, dummy, blob, member, index);
    BlobLayout::SetStatus(false, true, blob, member, index);
}

void DotsC_SetBinaryMember(const char* val,
                           DotsC_Int32 numberOfBytes,
                           char * & blob,
                           const DotsC_MemberIndex member,
                           const DotsC_ArrayIndex index)
{
    Init();
    BlobLayout::SetDynamicMember(val, numberOfBytes, blob, member, index);
    BlobLayout::SetStatus(false, true, blob, member, index);
}

//************************************************************************************
//* Type compatibility
//************************************************************************************
//Checks if type is an instance of the ofType, direct or by inheritance
bool DotsC_IsOfType(const TypeId type, const TypeId ofType)
{
    Init();
    return Repository::Classes().IsOfType(type, ofType);
}

void DotsC_GetCompleteType(const TypeId type,
                           TypeId * const buf,
                           const Int32 bufSize,
                           Int32 & noResults)
{
    Init();
    Repository::Classes().GetCompleteType(type, buf, bufSize, noResults);
}

DotsC_TypeId DotsC_GetParentType(const DotsC_TypeId type)
{
    Init();
    const ClassDescription * const cd=Repository::Classes().FindClass(type);
    if (cd==NULL)
        return 0;
    else if (cd->BaseClass()==NULL)
        return cd->GetTypeId();
    else
        return cd->BaseClass()->GetTypeId();
}

void DotsC_HasProperty(const TypeId classTypeId, const TypeId propertyTypeId, bool & hasProperty, bool & isInherited)
{
    Init();
    isInherited = false;
    hasProperty = NULL != Repository::Classes().FindClass(classTypeId)->FindPropertyMapping(propertyTypeId, isInherited);
}

//************************************************************************************
//* Serialization
//************************************************************************************
/*void DotsC_BlobToXml(char* xmlDest, const char * const blobSource, const Int32& bufSize)
  {
    Init();
  BlobToXmlSerializer ser;
  strncpy(xmlDest,ser.Serialize(blobSource).c_str(), bufSize);
  }*/

void DotsC_BetterBlobToXml(char * const xmlDest, const char * const blobSource, const Int32 bufSize, Int32 & resultSize)
{
    Init();
    BlobToXmlSerializer ser;
    std::string xml = ser.Serialize(blobSource);
    resultSize = static_cast<Int32>(xml.length());
    if (resultSize <= bufSize)
    {
        strncpy(xmlDest,xml.c_str(), bufSize);
    }
}



void DotsC_XmlToBlob(char * & blobDest, 
                     DotsC_BytePointerDeleter & deleter,
                     const char* xmlSource)
{
    Init();
    boost::call_once(createXmlToBlobLockFlag,boost::bind(getXmlToBlobLock));
    // XmlToBlobSerializer holds data in global variabels while parsing
    // so we can't allow more than one thread at a time to execute.

    boost::lock_guard<boost::mutex> lck(getXmlToBlobLock());

    XmlToBlobSerializer ser;
    blobDest = ser.Serialize(xmlSource);
    deleter = DeleteBytePointer;
}

DotsC_Int32 DotsC_CalculateBase64BufferSize(DotsC_Int32 binarySourceSize)
{
    Init();
    return Base64Conversions::CalculateBase64Size(binarySourceSize);
}

void DotsC_BinaryToBase64(char* base64Dest,
                          DotsC_Int32 destSize,
                          const char * const binarySource,
                          DotsC_Int32 sourceSize,
                          DotsC_Int32 & resultSize)
{
    Init();
    Base64Conversions::ToBase64(base64Dest, destSize, binarySource, sourceSize, resultSize);
}

DotsC_Int32 DotsC_CalculateBinaryBufferSize(DotsC_Int32 base64SourceSize)
{
    Init();
    return Base64Conversions::CalculateBinarySize(base64SourceSize);
}

void DotsC_Base64ToBinary(char* binaryDest,
                          DotsC_Int32 destSize,
                          const char * const base64Source,
                          DotsC_Int32 sourceSize,
                          DotsC_Int32 & resultSize)
{
    Init();
    Base64Conversions::ToBinary(binaryDest, destSize, base64Source, sourceSize, resultSize);
}

//************************************************************************************
//* Functions for retrieval of parameters
//************************************************************************************
void DotsC_GetBooleanParameter(const TypeId typeId, const ParameterIndex parameter, const ArrayIndex index, bool & val)
{
    Init();
    const ParameterDescription * const pd = Repository::Classes().FindClass(typeId)->GetParameter(parameter);
    val=*(pd->Value<bool>(index));
}

void DotsC_GetEnumerationParameter(const TypeId typeId,
                                   const ParameterIndex parameter,
                                   const ArrayIndex index,
                                   EnumerationValue& val)
{
    Init();
    const ParameterDescription * const pd = Repository::Classes().FindClass(typeId)->GetParameter(parameter);
    val=*(pd->Value<Safir::Dob::Typesystem::Internal::EnumInternal>(index));
}

void DotsC_GetInt32Parameter(const TypeId typeId, const ParameterIndex parameter, const ArrayIndex index, Int32& val)
{
    Init();
    const ParameterDescription * const pd = Repository::Classes().FindClass(typeId)->GetParameter(parameter);
    val=*(pd->Value<Int32>(index));
}

void DotsC_GetInt64Parameter(const TypeId typeId, const ParameterIndex parameter, const ArrayIndex index, Int64& val)
{
    Init();
    const ParameterDescription * const pd = Repository::Classes().FindClass(typeId)->GetParameter(parameter);
    val=*(pd->Value<Int64>(index));
}

void DotsC_GetFloat32Parameter(const TypeId typeId, const ParameterIndex parameter, const ArrayIndex index, Float32& val)
{
    Init();
    const ParameterDescription * const pd = Repository::Classes().FindClass(typeId)->GetParameter(parameter);
    val=*(pd->Value<Float32>(index));
}

void DotsC_GetFloat64Parameter(const TypeId typeId, const ParameterIndex parameter, const ArrayIndex index, Float64& val)
{
    Init();
    const ParameterDescription * const pd = Repository::Classes().FindClass(typeId)->GetParameter(parameter);
    val=*(pd->Value<Float64>(index));
}

void DotsC_GetStringParameter(const TypeId typeId, const ParameterIndex parameter, const ArrayIndex index, const char* &val)
{
    Init();
    const ParameterDescription * const pd = Repository::Classes().FindClass(typeId)->GetParameter(parameter);
    val=pd->Value<char>(index).get();
}

void DotsC_GetTypeIdParameter(const TypeId typeId, const ParameterIndex parameter, const ArrayIndex index, TypeId& val)
{
    Init();
    const ParameterDescription * const pd = Repository::Classes().FindClass(typeId)->GetParameter(parameter);
    val=*(pd->Value<TypeId>(index));
}

void DotsC_GetHashedIdParameter(const TypeId typeId,
                                const ParameterIndex parameter,
                                const ArrayIndex index,
                                Int64 & hashVal,
                                const char * & strVal)
{
    Init();
    const ParameterDescription * const pd = Repository::Classes().FindClass(typeId)->GetParameter(parameter);
    pd->ValueWithOptionalString(index,hashVal,strVal);
}

void DotsC_GetEntityIdParameter(const TypeId typeId,
                                const ParameterIndex parameter,
                                const ArrayIndex index,
                                DotsC_EntityId & entityId,
                                const char * & instanceIdStr)
{
    Init();
    const ParameterDescription * const pd = Repository::Classes().FindClass(typeId)->GetParameter(parameter);
    pd->ValueWithOptionalString(index,entityId,instanceIdStr);
}

void DotsC_GetObjectParameter(const TypeId typeId,
                              const ParameterIndex parameter,
                              const ArrayIndex index,
                              const char * & val)
{
    Init();
    const ParameterDescription * const pd = Repository::Classes().FindClass(typeId)->GetParameter(parameter);
    val=pd->Value<char>(index).get();
}

void DotsC_GetBinaryParameter(const DotsC_TypeId typeId,
                              const DotsC_ParameterIndex parameter,
                              const DotsC_ArrayIndex index,
                              const char* &val,
                              DotsC_Int32& size)
{
    Init();
    const ParameterDescription * const pd = Repository::Classes().FindClass(typeId)->GetParameter(parameter);
    const ParameterDescription::BinaryParameterValue bval = pd->BinaryValue(index);
    val = bval.first.get();
    size = bval.second;
}



//************************************************************************************
//* Functions for retrieving property member values
//************************************************************************************
bool DotsC_IsNullProperty(const char * const blob,
                          const TypeId property,
                          const MemberIndex member,
                          const ArrayIndex index)
{
    Init();
    bool isNull,isChanged;
    DotsC_ErrorCode errorCode;
    GetPropertyStatus(blob, property, member, index, isNull, isChanged, errorCode);
    return isNull;
}

bool DotsC_IsChangedProperty(const char * const blob,
                             const TypeId property,
                             const MemberIndex member,
                             const ArrayIndex index)
{
    Init();
    bool isNull,isChanged;
    DotsC_ErrorCode errorCode;
    GetPropertyStatus(blob, property, member, index, isNull, isChanged, errorCode);
    return isChanged;
}

void DotsC_GetBooleanProperty(const char * const blob,
                              const TypeId property,
                              const MemberIndex member,
                              const ArrayIndex index,
                              bool & val,
                              bool & isNull,
                              bool & isChanged,
                              DotsC_ErrorCode & errorCode)
{
    Init();
    GetPropertyValue(blob,property,member,index,val, isNull, isChanged, errorCode);
}

void DotsC_GetEnumerationProperty(const char * const blob,
                                  const TypeId property,
                                  const MemberIndex member,
                                  const ArrayIndex index,
                                  EnumerationValue& val,
                                  bool & isNull,
                                  bool & isChanged,
                                  DotsC_ErrorCode & errorCode)
{
    Init();
    Safir::Dob::Typesystem::Internal::EnumInternal tmp;
    GetPropertyValue(blob,property,member,index,tmp, isNull, isChanged, errorCode);
    val=tmp;
}

void DotsC_GetInt32Property(const char * const blob,
                            const TypeId property,
                            const MemberIndex member,
                            const ArrayIndex index,
                            Int32& val,
                            bool & isNull,
                            bool & isChanged,
                            DotsC_ErrorCode & errorCode)
{
    Init();
    GetPropertyValue(blob,property,member,index,val, isNull, isChanged, errorCode);
}

void DotsC_GetInt64Property(const char * const blob,
                            const TypeId property,
                            const MemberIndex member,
                            const ArrayIndex index,
                            Int64& val,
                            bool & isNull,
                            bool & isChanged,
                            DotsC_ErrorCode & errorCode)
{
    Init();
    GetPropertyValue(blob,property,member,index,val, isNull, isChanged, errorCode);
}

void DotsC_GetFloat32Property(const char * const blob,
                              const TypeId property,
                              const MemberIndex member,
                              const ArrayIndex index,
                              Float32& val,
                              bool & isNull,
                              bool & isChanged,
                              DotsC_ErrorCode & errorCode)
{
    Init();
    GetPropertyValue(blob,property,member,index,val, isNull, isChanged, errorCode);
    if (isNull)
    {
        //we have to set a proper value, since an unitialized value may cause
        //FP errors, e.g. in Ada
        val = 0.0f;
    }
}

void DotsC_GetFloat64Property(const char * const blob,
                              const TypeId property,
                              const MemberIndex member,
                              const ArrayIndex index,
                              Float64& val,
                              bool & isNull,
                              bool & isChanged,
                              DotsC_ErrorCode & errorCode)
{
    Init();
    GetPropertyValue(blob,property,member,index,val, isNull, isChanged, errorCode);
    if (isNull)
    {
        //we have to set a proper value, since an unitialized value may cause
        //FP errors, e.g. in Ada
        val = 0.0; 
    }
}

void DotsC_GetStringProperty(const char * const blob,
                             const TypeId property,
                             const MemberIndex member,
                             const ArrayIndex index,
                             const char* &val,
                             bool & isNull,
                             bool & isChanged,
                             DotsC_ErrorCode & errorCode)
{
    Init();
    Int32 dummy = 0;
    GetDynamicPropertyValue(blob,property,member,index,val, dummy, isNull, isChanged, errorCode);
}

void DotsC_GetTypeIdProperty(const char * const blob,
                             const TypeId property,
                             const MemberIndex member,
                             const ArrayIndex index,
                             TypeId& val,
                             bool & isNull,
                             bool & isChanged,
                             DotsC_ErrorCode & errorCode)
{
    Init();
    GetPropertyValue(blob,property,member,index,val, isNull, isChanged, errorCode);
}

void DotsC_GetHashedIdProperty(const char * const blob,
                               const TypeId property,
                               const MemberIndex member,
                               const ArrayIndex index,
                               Int64 & hashVal,
                               const char * & strVal,
                               bool & isNull,
                               bool & isChanged,
                               DotsC_ErrorCode & errorCode)
{
    Init();
    GetPropertyValueWithOptionalString(blob, property, member, index, hashVal, strVal, isNull, isChanged, errorCode);
}

void DotsC_GetEntityIdProperty(const char * const blob,
                               const TypeId property,
                               const MemberIndex member,
                               const ArrayIndex index,
                               DotsC_EntityId & entityId,
                               const char * & instanceIdStr,
                               bool & isNull,
                               bool & isChanged,
                               DotsC_ErrorCode & errorCode)
{
    Init();
    GetPropertyValueWithOptionalString(blob, property, member, index, entityId, instanceIdStr, isNull, isChanged, errorCode);
}

void DotsC_GetObjectProperty(const char * const blob,
                             const TypeId property,
                             const MemberIndex member,
                             const ArrayIndex index,
                             const char * & val,
                             bool & isNull,
                             bool & isChanged,
                             DotsC_ErrorCode & errorCode)
{
    Init();
    Int32 dummy = 0;
    GetDynamicPropertyValue(blob,property,member,index, val, dummy, isNull, isChanged, errorCode);
}

void DotsC_GetBinaryProperty(const char * const blob,
                             const DotsC_TypeId property,
                             const DotsC_MemberIndex member,
                             const DotsC_ArrayIndex index,
                             const char * & val,
                             DotsC_Int32 & size,
                             bool & isNull,
                             bool & isChanged,
                             DotsC_ErrorCode & errorCode)
{
    Init();
    GetDynamicPropertyValue(blob,property,member,index, val, size, isNull, isChanged, errorCode);
}

//************************************************************************************
//* Functions for setting property member values
//************************************************************************************
void DotsC_SetNullProperty(char * & blob,
                           const TypeId property,
                           const MemberIndex member,
                           const ArrayIndex index,
                           DotsC_ErrorCode & errorCode)
{
    Init();
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
                char * referencedBlob = blob;
                //Follow the reference into objects (the last one is skipped since that goes into a member, not an object)
                for (Size ii = 0; ii < cmr->size() - 1; ++ii)
                {
                    const MemberReferenceElement ref = cmr->at(ii);
                    bool refIsNull = false, refIsChanged = false;
                    DotsC_GetWriteableObjectMember(referencedBlob, ref.m_classMember, ref.m_index, referencedBlob, refIsNull, refIsChanged);
                    if (refIsNull)
                    {
                        errorCode = UnableToDereferenceProperty;
                        return; //cannot set the property, since something above it is null
                    }
                    //set the ischanged flag on all containing items
                    //TODO: This will unfortunately result in all change flags being set even if the value could not
                    //be dereferenced. But since we're implementing real classes "soon" we ignore this code for the time being.
                    BlobLayout::SetStatus(false, true, referencedBlob, ref.m_classMember, ref.m_index);
                }
                const MemberReferenceElement ref = cmr->back();

                if (ref.m_index == -1)//pointing at an array, use the index from the function call
                {
                    BlobLayout::SetStatus(true, true, referencedBlob, ref.m_classMember, index);
                }
                else
                {
                    BlobLayout::SetStatus(true, true, referencedBlob, ref.m_classMember, ref.m_index);
                }
            }
            break;
        case MappedToParameter:
            errorCode = ReadOnlyProperty;
        }
    }
}

void DotsC_SetBooleanProperty(const bool val,
                              char * & blob,
                              const TypeId property,
                              const MemberIndex member,
                              const ArrayIndex index,
                              DotsC_ErrorCode & errorCode)
{
    Init();
    SetPropertyValue(val,blob,property,member,index,errorCode);
}

void DotsC_SetEnumerationProperty(const EnumerationValue val,
                                  char * & blob,
                                  const TypeId property,
                                  const MemberIndex member,
                                  const ArrayIndex index,
                                  DotsC_ErrorCode & errorCode)
{
    Init();
    Safir::Dob::Typesystem::Internal::EnumInternal tmp=static_cast<Safir::Dob::Typesystem::Internal::EnumInternal>(val);
    SetPropertyValue(tmp,blob,property,member, index, errorCode);
}

void DotsC_SetInt32Property(const Int32 val,
                            char * & blob,
                            const TypeId property,
                            const MemberIndex member,
                            const ArrayIndex index,
                            DotsC_ErrorCode & errorCode)
{
    Init();
    SetPropertyValue(val, blob, property, member, index, errorCode);
}

void DotsC_SetInt64Property(const Int64 val,
                            char * & blob,
                            const TypeId property,
                            const MemberIndex member,
                            const ArrayIndex index,
                            DotsC_ErrorCode & errorCode)
{
    Init();
    SetPropertyValue(val, blob, property, member, index, errorCode);
}

void DotsC_SetFloat32Property(const Float32 val,
                              char * & blob,
                              const TypeId property,
                              const MemberIndex member,
                              const ArrayIndex index,
                              DotsC_ErrorCode & errorCode)
{
    Init();
    SetPropertyValue(val, blob, property, member, index, errorCode);
}

void DotsC_SetFloat64Property(const Float64 val,
                              char * & blob,
                              const TypeId property,
                              const MemberIndex member,
                              const ArrayIndex index,
                              DotsC_ErrorCode & errorCode)
{
    Init();
    SetPropertyValue(val, blob, property, member, index, errorCode);
}

void DotsC_SetStringProperty(const char* val,
                             char * & blob,
                             const TypeId property,
                             const MemberIndex member,
                             const ArrayIndex index,
                             DotsC_ErrorCode & errorCode)
{
    Init();
    Size dummy=0;
    SetDynamicPropertyValue(val, dummy, blob, property, member, index, errorCode);
}

void DotsC_SetTypeIdProperty(const TypeId val,
                             char * & blob,
                             const TypeId property,
                             const MemberIndex member,
                             const ArrayIndex index,
                             DotsC_ErrorCode & errorCode)
{
    Init();
    SetPropertyValue(val, blob, property, member, index, errorCode);
}

void DotsC_SetHashedIdProperty(const Int64 hashVal,
                               const char * const strVal,
                               char * & blob,
                               const TypeId property,
                               const MemberIndex member,
                               const ArrayIndex index,
                               DotsC_ErrorCode & errorCode)
{
    Init();
    SetPropertyValueWithOptionalString(hashVal, strVal, blob, property, member, index, errorCode);
}

void DotsC_SetEntityIdProperty(const DotsC_EntityId& entityId,
                               const char * const instanceIdStr,
                               char * & blob,
                               const TypeId property,
                               const MemberIndex member,
                               const ArrayIndex index,
                               DotsC_ErrorCode & errorCode)
{
    Init();
    SetPropertyValueWithOptionalString(entityId, instanceIdStr, blob, property, member, index, errorCode);
}

void DotsC_SetObjectProperty(const char * const val,
                             char * & blob,
                             const TypeId property,
                             const MemberIndex member,
                             const ArrayIndex index,
                             DotsC_ErrorCode & errorCode)
{
    Init();
    Size dummy=0;
    SetDynamicPropertyValue(val, dummy, blob, property, member, index, errorCode);
}

void DotsC_SetBinaryProperty(const char* val,
                             DotsC_Int32 size,
                             char * & blob,
                             const DotsC_TypeId property,
                             const DotsC_MemberIndex member,
                             const DotsC_ArrayIndex index,
                             DotsC_ErrorCode & errorCode)
{
    Init();
    SetDynamicPropertyValue(val, size, blob, property, member, index, errorCode);
}

//*********************************
//* For "real classes"
//*********************************

Int32 DotsC_GetInitialSize(const TypeId typeId)
{
    Init();
    const ClassDescription * const cd=Repository::Classes().FindClass(typeId);
    if (cd != NULL)
    {
        return cd->OwnSize();
    }
    else
    {
        return -1;
    }
}

void DotsC_FormatBlob(char * const blob,
                      const Int32 blobSize,
                      const TypeId typeId,
                      char * & beginningOfUnused)
{
    Init();
    BlobLayout::FormatBlob(blob, blobSize, typeId, beginningOfUnused);
}


void DotsC_CreateObjectMember(char * const insideBlob,
                              const Int32 blobSize,
                              const TypeId typeId,
                              const MemberIndex member,
                              const ArrayIndex index,
                              const bool isChanged,
                              char * & beginningOfUnused)
{
    Init();
    BlobLayout::CreateObjectMember(insideBlob,blobSize, typeId, member, index, isChanged, beginningOfUnused);
}

void DotsC_CreateStringMember(char * const insideBlob,
                              const Int32 stringLength, //remember the null-termination!
                              const MemberIndex member,
                              const ArrayIndex index,
                              const bool isChanged,
                              char * & beginningOfUnused)
{
    Init();
    BlobLayout::CreateStringMember(insideBlob,stringLength,member,index,isChanged,beginningOfUnused);
}

void DotsC_CreateBinaryMember(char * const insideBlob,
                              const DotsC_Int32 binarySize,
                              const DotsC_MemberIndex member,
                              const DotsC_ArrayIndex index,
                              const bool isChanged,
                              char * & beginningOfUnused)
{
    Init();
    BlobLayout::CreateBinaryMember(insideBlob,binarySize,member,index,isChanged,beginningOfUnused);
}

void DotsC_SetBooleanMemberInPreallocated(const bool val,
                                          const bool isNull,
                                          const bool isChanged,
                                          char * const blob,
                                          const DotsC_MemberIndex member,
                                          const DotsC_ArrayIndex index)
{
    Init();
    if (!isNull)
    {
        BlobLayout::SetMember(val, blob, member, index);
    }
    BlobLayout::SetStatus(isNull, isChanged, blob, member, index);
}



void DotsC_SetInt32MemberInPreallocated(const DotsC_Int32 val,
                                        const bool isNull,
                                        const bool isChanged,
                                        char * const blob,
                                        const DotsC_MemberIndex member,
                                        const DotsC_ArrayIndex index)
{
    Init();
    if (!isNull)
    {
        BlobLayout::SetMember(val, blob, member, index);
    }
    BlobLayout::SetStatus(isNull, isChanged, blob, member, index);
}


void DotsC_SetInt64MemberInPreallocated(const DotsC_Int64 val,
                                        const bool isNull,
                                        const bool isChanged,
                                        char * const blob,
                                        const DotsC_MemberIndex member,
                                        const DotsC_ArrayIndex index)
{
    Init();
    if (!isNull)
    {
        BlobLayout::SetMember(val, blob, member, index);
    }
    BlobLayout::SetStatus(isNull, isChanged, blob, member, index);

}

void DotsC_SetFloat32MemberInPreallocated(const DotsC_Float32 val,
                                          const bool isNull,
                                          const bool isChanged,
                                          char * const blob,
                                          const DotsC_MemberIndex member,
                                          const DotsC_ArrayIndex index)
{
    Init();
    if (!isNull)
    {
        BlobLayout::SetMember(val, blob, member, index);
    }
    BlobLayout::SetStatus(isNull, isChanged, blob, member, index);

}

void DotsC_SetFloat64MemberInPreallocated(const DotsC_Float64 val,
                                          const bool isNull,
                                          const bool isChanged,
                                          char * const blob,
                                          const DotsC_MemberIndex member,
                                          const DotsC_ArrayIndex index)
{
    Init();
    if (!isNull)
    {
        BlobLayout::SetMember(val, blob, member, index);
    }
    BlobLayout::SetStatus(isNull, isChanged, blob, member, index);

}

void DotsC_SetHashedIdMemberInPreallocated(const Int64 hashVal,
                                           const char * const strVal,
                                           const DotsC_Int32 stringLength,
                                           const bool isNull,
                                           const bool isChanged,
                                           char * const blob,
                                           const DotsC_MemberIndex member,
                                           const DotsC_ArrayIndex index,
                                           char * & beginningOfUnused)
{
    Init();
    if (!isNull)
    {
        BlobLayout::CreateAndSetMemberWithOptionalString(blob,
                                                         hashVal,
                                                         strVal,
                                                         stringLength,
                                                         member,
                                                         index,
                                                         isChanged,
                                                         beginningOfUnused);
    }
    BlobLayout::SetStatus(isNull, isChanged, blob, member, index);
}

void DotsC_SetEntityIdMemberInPreallocated(const DotsC_EntityId & entityId,
                                           const char * const instanceIdStr,
                                           const DotsC_Int32 stringLength,
                                           const bool isNull,
                                           const bool isChanged,
                                           char * const blob,
                                           const DotsC_MemberIndex member,
                                           const DotsC_ArrayIndex index,
                                           char * & beginningOfUnused)
{
    Init();
    if (!isNull)
    {
        BlobLayout::CreateAndSetMemberWithOptionalString(blob,
                                                         entityId,
                                                         instanceIdStr,
                                                         stringLength,
                                                         member,
                                                         index,
                                                         isChanged,
                                                         beginningOfUnused);
    }
    BlobLayout::SetStatus(isNull, isChanged, blob, member, index);
}

void DotsC_GetPropertyMappingKind(const DotsC_TypeId typeId,
                                  const DotsC_TypeId propertyId,
                                  const DotsC_MemberIndex member,
                                  DotsC_PropertyMappingKind & mappingKind,
                                  DotsC_ErrorCode & errorCode)
{
    Init();
    errorCode = NoError;

    bool isInherited;
    const PropertyMappingDescription * const pmd=Repository::Classes().FindClass(typeId)->FindPropertyMapping(propertyId, isInherited);

    if (pmd == NULL)
    {
        errorCode = IllegalValue;
        return;
    }

    const MemberMapping * const mm = pmd->GetMemberMapping(member);

    if (mm == NULL)
    {
        errorCode = IllegalValue;
    }
    else
    {
        mappingKind = mm->GetMappingKind();
    }
}


void DotsC_GetClassMemberReference(const DotsC_TypeId typeId,
                                   const DotsC_TypeId propertyId,
                                   const DotsC_MemberIndex member,
                                   const DotsC_Int32 * & classMemberReference,
                                   DotsC_Int32 & classMemberReferenceSize)
{
    Init();
    classMemberReference = NULL;
    classMemberReferenceSize = 0;

    bool isInherited;
    const PropertyMappingDescription * const pmd=Repository::Classes().FindClass(typeId)->FindPropertyMapping(propertyId, isInherited);

    if (pmd == NULL)
    {
        return;
    }

    const MemberMapping * const mm = pmd->GetMemberMapping(member);

    if (mm == NULL || mm->GetMappingKind() != MappedToMember)
    {
        return;
    }


    const ClassMemberReference * cmr = mm->GetMemberReference();

    classMemberReference = reinterpret_cast<const DotsC_Int32*>(&(*cmr)[0]);
    classMemberReferenceSize = static_cast<DotsC_Int32>(cmr->size()*2);
}


void DotsC_GetEnumerationChecksum(const DotsC_TypeId typeId,
                                  DotsC_TypeId & checksum)
{
    Init();
    const EnumDescription * const ed = Repository::Enums().FindEnum(typeId);
    if (ed == NULL || ed->GetTypeId() != typeId)
    {
        checksum = 0;
    }
    else
    {
        checksum = ed->Checksum();
    }
}


void DotsC_GetBooleanPropertyParameter(const DotsC_TypeId typeId,
                                       const DotsC_TypeId propertyId,
                                       const DotsC_MemberIndex member,
                                       const DotsC_ArrayIndex index,
                                       bool & val)
{
    Init();
    bool isInherited;
    const PropertyMappingDescription * const pmd=Repository::Classes().FindClass(typeId)->FindPropertyMapping(propertyId, isInherited);

    if (pmd == NULL)
    {
        return;
    }

    const MemberMapping * const mm = pmd->GetMemberMapping(member);

    if (mm == NULL || mm->GetMappingKind() != MappedToParameter)
    {
        return;
    }
    const ParameterDescription * const pd=mm->GetParameter();
    val=*(pd->Value<bool>(index));
}

void DotsC_GetEnumerationPropertyParameter( const DotsC_TypeId typeId,
                                            const DotsC_TypeId propertyId,
                                            const DotsC_MemberIndex member,
                                            const DotsC_ArrayIndex index,
                                            DotsC_EnumerationValue & val)
{
    Init();
    bool isInherited;
    const PropertyMappingDescription * const pmd=Repository::Classes().FindClass(typeId)->FindPropertyMapping(propertyId, isInherited);

    if (pmd == NULL)
    {
        return;
    }

    const MemberMapping * const mm = pmd->GetMemberMapping(member);

    if (mm == NULL || mm->GetMappingKind() != MappedToParameter)
    {
        return;
    }
    const ParameterDescription * const pd=mm->GetParameter();
    val=*(pd->Value<DotsC_EnumerationValue>(index));
}

void DotsC_GetInt32PropertyParameter(const DotsC_TypeId typeId,
                                     const DotsC_TypeId propertyId,
                                     const DotsC_MemberIndex member,
                                     const DotsC_ArrayIndex index,
                                     DotsC_Int32 & val)
{
    Init();
    bool isInherited;
    const PropertyMappingDescription * const pmd=Repository::Classes().FindClass(typeId)->FindPropertyMapping(propertyId, isInherited);

    if (pmd == NULL)
    {
        return;
    }

    const MemberMapping * const mm = pmd->GetMemberMapping(member);

    if (mm == NULL || mm->GetMappingKind() != MappedToParameter)
    {
        return;
    }
    const ParameterDescription * const pd=mm->GetParameter();
    val=*(pd->Value<DotsC_Int32>(index));
}

void DotsC_GetInt64PropertyParameter(const DotsC_TypeId typeId,
                                     const DotsC_TypeId propertyId,
                                     const DotsC_MemberIndex member,
                                     const DotsC_ArrayIndex index,
                                     DotsC_Int64 & val)
{
    Init();
    bool isInherited;
    const PropertyMappingDescription * const pmd=Repository::Classes().FindClass(typeId)->FindPropertyMapping(propertyId, isInherited);

    if (pmd == NULL)
    {
        return;
    }

    const MemberMapping * const mm = pmd->GetMemberMapping(member);

    if (mm == NULL || mm->GetMappingKind() != MappedToParameter)
    {
        return;
    }
    const ParameterDescription * const pd=mm->GetParameter();
    val=*(pd->Value<DotsC_Int64>(index));
}

void DotsC_GetFloat32PropertyParameter(const DotsC_TypeId typeId,
                                       const DotsC_TypeId propertyId,
                                       const DotsC_MemberIndex member,
                                       const DotsC_ArrayIndex index,
                                       DotsC_Float32 & val)
{
    Init();
    bool isInherited;
    const PropertyMappingDescription * const pmd=Repository::Classes().FindClass(typeId)->FindPropertyMapping(propertyId, isInherited);

    if (pmd == NULL)
    {
        return;
    }

    const MemberMapping * const mm = pmd->GetMemberMapping(member);

    if (mm == NULL || mm->GetMappingKind() != MappedToParameter)
    {
        return;
    }
    const ParameterDescription * const pd=mm->GetParameter();
    val=*(pd->Value<DotsC_Float32>(index));
}

void DotsC_GetFloat64PropertyParameter(const DotsC_TypeId typeId,
                                       const DotsC_TypeId propertyId,
                                       const DotsC_MemberIndex member,
                                       const DotsC_ArrayIndex index,
                                       DotsC_Float64 & val)
{
    Init();
    bool isInherited;
    const PropertyMappingDescription * const pmd=Repository::Classes().FindClass(typeId)->FindPropertyMapping(propertyId, isInherited);

    if (pmd == NULL)
    {
        return;
    }

    const MemberMapping * const mm = pmd->GetMemberMapping(member);

    if (mm == NULL || mm->GetMappingKind() != MappedToParameter)
    {
        return;
    }
    const ParameterDescription * const pd=mm->GetParameter();
    val=*(pd->Value<DotsC_Float64>(index));
}

void DotsC_GetStringPropertyParameter(const DotsC_TypeId typeId,
                                      const DotsC_TypeId propertyId,
                                      const DotsC_MemberIndex member,
                                      const DotsC_ArrayIndex index,
                                      const char * & val)
{
    Init();
    bool isInherited;
    const PropertyMappingDescription * const pmd=Repository::Classes().FindClass(typeId)->FindPropertyMapping(propertyId, isInherited);

    if (pmd == NULL)
    {
        return;
    }

    const MemberMapping * const mm = pmd->GetMemberMapping(member);

    if (mm == NULL || mm->GetMappingKind() != MappedToParameter)
    {
        return;
    }
    const ParameterDescription * const pd=mm->GetParameter();
    val=pd->Value<char>(index).get();
}

void DotsC_GetTypeIdPropertyParameter(const DotsC_TypeId typeId,
                                      const DotsC_TypeId propertyId,
                                      const DotsC_MemberIndex member,
                                      const DotsC_ArrayIndex index,
                                      DotsC_TypeId & val)
{
    Init();
    bool isInherited;
    const PropertyMappingDescription * const pmd=Repository::Classes().FindClass(typeId)->FindPropertyMapping(propertyId, isInherited);

    if (pmd == NULL)
    {
        return;
    }

    const MemberMapping * const mm = pmd->GetMemberMapping(member);

    if (mm == NULL || mm->GetMappingKind() != MappedToParameter)
    {
        return;
    }
    const ParameterDescription * const pd=mm->GetParameter();
    val=*(pd->Value<DotsC_TypeId>(index));
}

void DotsC_GetHashedIdPropertyParameter(const DotsC_TypeId typeId,
                                        const DotsC_TypeId propertyId,
                                        const DotsC_MemberIndex member,
                                        const DotsC_ArrayIndex index,
                                        Int64 & hashVal,
                                        const char * & strVal)
{
    Init();
    bool isInherited;
    const PropertyMappingDescription * const pmd=Repository::Classes().FindClass(typeId)->FindPropertyMapping(propertyId, isInherited);

    if (pmd == NULL)
    {
        return;
    }

    const MemberMapping * const mm = pmd->GetMemberMapping(member);

    if (mm == NULL || mm->GetMappingKind() != MappedToParameter)
    {
        return;
    }
    const ParameterDescription * const pd=mm->GetParameter();
    pd->ValueWithOptionalString(index,hashVal,strVal);
}


void DotsC_GetEntityIdPropertyParameter(const DotsC_TypeId typeId,
                                        const DotsC_TypeId propertyId,
                                        const DotsC_MemberIndex member,
                                        const DotsC_ArrayIndex index,
                                        DotsC_EntityId & entityId,
                                        const char * & instanceIdStr)
{
    Init();
    bool isInherited;
    const PropertyMappingDescription * const pmd=Repository::Classes().FindClass(typeId)->FindPropertyMapping(propertyId, isInherited);

    if (pmd == NULL)
    {
        return;
    }

    const MemberMapping * const mm = pmd->GetMemberMapping(member);

    if (mm == NULL || mm->GetMappingKind() != MappedToParameter)
    {
        return;
    }
    const ParameterDescription * const pd=mm->GetParameter();
    pd->ValueWithOptionalString(index,entityId,instanceIdStr);
}

void DotsC_GetObjectPropertyParameter(const DotsC_TypeId typeId,
                                      const DotsC_TypeId propertyId,
                                      const DotsC_MemberIndex member,
                                      const DotsC_ArrayIndex index,
                                      const char * & val)
{
    Init();
    bool isInherited;
    const PropertyMappingDescription * const pmd=Repository::Classes().FindClass(typeId)->FindPropertyMapping(propertyId, isInherited);

    if (pmd == NULL)
    {
        return;
    }

    const MemberMapping * const mm = pmd->GetMemberMapping(member);

    if (mm == NULL || mm->GetMappingKind() != MappedToParameter)
    {
        return;
    }
    const ParameterDescription * const pd=mm->GetParameter();
    val=pd->Value<char>(index).get();
}

void DotsC_GetBinaryPropertyParameter(const DotsC_TypeId typeId,
                                      const DotsC_TypeId propertyId,
                                      const DotsC_MemberIndex member,
                                      const DotsC_ArrayIndex index,
                                      const char * & val,
                                      DotsC_Int32& size)
{
    Init();
    bool isInherited;
    const PropertyMappingDescription * const pmd=Repository::Classes().FindClass(typeId)->FindPropertyMapping(propertyId, isInherited);

    if (pmd == NULL)
    {
        return;
    }

    const MemberMapping * const mm = pmd->GetMemberMapping(member);

    if (mm == NULL || mm->GetMappingKind() != MappedToParameter)
    {
        return;
    }
    const ParameterDescription * const pd=mm->GetParameter();
    //    val=pd->Value<char>(index).get();
    ParameterDescription::BinaryParameterValue bval = pd->BinaryValue(index);
    val = bval.first.get();
    size = bval.second;
}

void DotsC_SetException(const DotsC_TypeId exceptionId, const char * const description)
{
    Init();
    ExceptionKeeper::Instance().Set(exceptionId, description);
}

void DotsC_AppendExceptionDescription(const char * const moreDescription)
{
    Init();
    ExceptionKeeper::Instance().AppendDescription(moreDescription);
}



void DotsC_GetAndClearException(DotsC_TypeId & exceptionId, char * & description, DotsC_BytePointerDeleter & deleter, bool & wasSet)
{
    Init();
    std::string desc;
    wasSet = ExceptionKeeper::Instance().GetAndClear(exceptionId, desc);
    if (!wasSet)
    {
        return;
    }
    description = new char [desc.size()+1];
    memcpy(description,desc.c_str(),desc.size());
    description[desc.size()] = '\0';
    deleter = DeleteBytePointer;
}

void DotsC_PeekAtException(DotsC_TypeId & exceptionId)
{
    Init();
    std::string desc;
    ExceptionKeeper::Instance().Peek(exceptionId,desc);
}



void DotsC_BinaryDump(const char * const blob,
                      const char * const filenamePart)
{
    Init();
    try
    {
        //create a new file
        static boost::filesystem::path path = GetLogDirectory();

        boost::filesystem::path filename;
        for (int i = 0;;++i)
        {
            filename = path / MakeFileName(filenamePart, i);
            if (!boost::filesystem::exists(filename))
            {
                break;
            }
        }

        //write blob to file
        DotsC_Int32 blobSize = DotsC_GetSize(blob);
        boost::filesystem::ofstream output;
        output.open (filename, std::ios::binary);
        output.write(blob, blobSize);
    }
    catch(const std::exception & exc)
    {
        lllout << "Caught exception in DotsC_BinaryDump on "<< filenamePart << ": " << exc.what() <<std::endl;
        std::wcout << "Caught exception in DotsC_BinaryDump on "<< filenamePart << ": " << exc.what() <<std::endl;
    }
    catch(...)
    {
        lllout << "Caught ... exception in DotsC_BinaryDump on "<< filenamePart << ": " <<std::endl;
        std::wcout << "Caught ... exception in DotsC_BinaryDump on "<< filenamePart << ": " <<std::endl;
    }
}
