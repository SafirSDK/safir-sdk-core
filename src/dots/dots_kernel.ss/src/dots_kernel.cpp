/******************************************************************************
*
* Copyright Saab AB, 2004-2014 (http://safir.sourceforge.net)
* Copyright Consoden AB, 2015 (http://www.consoden.se)
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
#include <Safir/Dob/Typesystem/ToolSupport/Serialization.h>

#include "dots_init_helper.h"
#include "dots_exception_keeper.h"

using namespace Safir::Dob::Typesystem::Internal;
namespace ts = Safir::Dob::Typesystem::ToolSupport;

namespace
{
    void DeleteBytePointer(char* & ptr)
    {
        delete [] ptr;
        ptr=NULL;
    }

    void DeleteGeneratedLibraryList(DotsC_GeneratedLibrary* list,
                                    const DotsC_Int32 size)
    {
        for (int i = 0; i < size; ++i)
        {
            delete [] list[i].name;
            delete [] list[i].cppLibraryName;
            delete [] list[i].cppLibraryLocation;

            delete [] list[i].javaJarName;
            delete [] list[i].javaJarLocation;

            delete [] list[i].dotnetAssemblyName;
            delete [] list[i].dotnetAssemblyLocation;
        }
        delete [] list;
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

    /* This is work in progress for dictionary properties
    bool GetPropertyParameterInternal(const DotsC_TypeId typeId,
                                      const DotsC_TypeId propertyId,
                                      const DotsC_MemberIndex member,
                                      const DotsC_Int32 index, //memberIndex
                                      const ParameterDescriptionShm*& parameter,
                                      int& parameterIndex)
    {
        bool isInherited;
        const PropertyMappingDescriptionShm* pmd=RepositoryKeeper::GetRepository()->
            GetClass(typeId)->GetPropertyMapping(propertyId, isInherited);

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

        if (pmd->GetProperty()->GetMember(member)->GetCollectionType()!=SingleValueCollectionType)
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
    */

    typedef ts::BlobReader<RepositoryShm> Reader;
    typedef ts::BlobWriter<RepositoryShm> Writer;
    Reader* ReaderFromHandle(DotsC_Handle handle) {return (Reader*)handle;}
    Writer* WriterFromHandle(DotsC_Handle handle) {return (Writer*)handle;}

    char* CopyStringToNew(const std::string& str)
    {
        char* dest = new char[str.size() + 1];
        strcpy(dest,str.c_str());
        return dest;
    }

    bool IsValidSection(const boost::property_tree::ptree& ptree)
    {
        if (ptree.empty())
        {
            return false;
        }

        const boost::optional<unsigned int> safirInstance =
            ptree.get_optional<unsigned int>("safir_instance");

        if (safirInstance &&
            Safir::Utilities::Internal::Expansion::GetSafirInstance() != safirInstance.get())
        {
            // This section is for a different safir instance
            return false;
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
        size=static_cast<DotsC_Int32>(types.size());
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
    return ts::TypeUtilities::CalculateTypeId(typeName);
}

const char* DotsC_GetTypeName(const DotsC_TypeId typeId)
{
    Init();
    return TypeUtilities::GetTypeName(RepositoryKeeper::GetRepository(), typeId);
}

const char* DotsC_MemberTypeName(DotsC_MemberType memberType)
{
    Init();
    return TypeUtilities::GetTypeName(memberType);
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
        return ts::TypeUtilities::GetIndexOfEnumValue(ed, enumValueName);
    }
    return -1;
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

DotsC_MemberIndex DotsC_GetMemberId(const DotsC_TypeId typeId, const char* const memberName)
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

void DotsC_GetMemberInfo(DotsC_TypeId typeId,  //in
                         DotsC_MemberIndex member,  //in
                         DotsC_MemberType& memberType,//out
                         const char*& memberName,           //out
                         DotsC_TypeId& complexType,   //out
                         DotsC_Int32& stringLength,   //out
                         DotsC_CollectionType& collectionType, //out
                         DotsC_Int32& arraySize)   //out
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
            arraySize=-1;
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
            arraySize=-1;
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
    collectionType=memberDesc->GetCollectionType();
    arraySize=memberDesc->GetArraySize();
    if (memberType==ObjectMemberType || memberType==EnumerationMemberType)
    {
        complexType=memberDesc->GetTypeId();
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
            std::pair<DotsC_MemberIndex, DotsC_Int32> ref=mm->GetMemberReference(i);
            parent=RepositoryKeeper::GetRepository()->GetClass(parent)->GetMember(ref.first)->GetTypeId();
        }
        return RepositoryKeeper::GetRepository()->GetClass(parent)->GetMember(mm->GetMemberReference(refDepth-1).first)->GetArraySize();
    }

    case MappedToParameter:
    {
        return mm->GetParameter().first->GetNumberOfValues();
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
            std::pair<DotsC_MemberIndex, DotsC_Int32> ref=mm->GetMemberReference(i);
            parent=RepositoryKeeper::GetRepository()->GetClass(parent)->GetMember(ref.first)->GetTypeId();
        }
        return RepositoryKeeper::GetRepository()->GetClass(parent)->GetMember(mm->GetMemberReference(refDepth-1).first)->GetMaxLength();
    }

    case MappedToParameter:
        // String max length not available.
        break;
    }

    //will never get here!
    return -1;
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

void DotsC_GetParameterInfo(DotsC_TypeId typeId,
                            DotsC_ParameterIndex parameter,
                            DotsC_MemberType& memberType,
                            const char*& parameterName,
                            DotsC_TypeId& complexTypeId,
                            DotsC_CollectionType& collectionType,
                            DotsC_Int32& numberOfValues)
{
    Init();
    const ParameterDescriptionShm* par=RepositoryKeeper::GetRepository()->GetClass(typeId)->GetParameter(parameter);
    memberType=par->GetMemberType();
    complexTypeId=par->GetTypeId();
    parameterName=par->GetName();
    collectionType=par->GetCollectionType();
    numberOfValues=par->GetNumberOfValues();
    memberType=RepositoryKeeper::GetRepository()->GetClass(typeId)->GetParameter(parameter)->GetMemberType();
}

//************************************************************************************
//* Type compatibility
//************************************************************************************
//Checks if type is an instance of the ofType, direct or by inheritance
bool DotsC_IsOfType(const DotsC_TypeId type, const DotsC_TypeId ofType)
{
    Init();
    return ts::TypeUtilities::IsOfType(RepositoryKeeper::GetRepository(), type, ofType);
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

bool DotsC_GetPropertyMappingKind(const DotsC_TypeId typeId,
                                  const DotsC_TypeId propertyId,
                                  const DotsC_MemberIndex member,
                                  DotsC_PropertyMappingKind & mappingKind)
{
    Init();   

    bool isInherited;
    const PropertyMappingDescriptionShm* pmd=RepositoryKeeper::GetRepository()->GetClass(typeId)->GetPropertyMapping(propertyId, isInherited);
    if (pmd==NULL)
    {
        //class not mapped to that property
        return false;
    }
    const MemberMappingDescriptionShm* mm=pmd->GetMemberMapping(member);
    mappingKind=mm->GetMappingKind();
    return true;
}


void DotsC_GetClassMemberReference(const DotsC_TypeId classTypeId,
                                   const DotsC_TypeId propertyTypeId,
                                   const DotsC_MemberIndex propertyMember,
                                   const DotsC_Int32* & classMemberReference,
                                   DotsC_Int32 & classMemberReferenceSize)
{
    Init();
    classMemberReference=NULL;
    classMemberReferenceSize=0;

    bool isInherited;
    const PropertyMappingDescriptionShm* pmd=RepositoryKeeper::GetRepository()->GetClass(classTypeId)->GetPropertyMapping(propertyTypeId, isInherited);

    if (pmd==NULL)
    {
        return;
    }

    const MemberMappingDescriptionShm* mm=pmd->GetMemberMapping(propertyMember);

    if (mm==NULL || mm->GetMappingKind()!=MappedToMember)
    {
        return;
    }

    classMemberReference=mm->GetRawMemberRef();
    classMemberReferenceSize=mm->MemberReferenceDepth()*2;
}

void DotsC_GetPropertyParameterReference(const DotsC_TypeId classTypeId,
                                         const DotsC_TypeId propertyTypeId,
                                         const DotsC_MemberIndex propertyMember,
                                         const DotsC_Int32 index,
                                         DotsC_ParameterIndex& paramId, //out
                                         DotsC_Int32& paramValueIndex) //out
{
    bool isInherited;
    const ClassDescriptionShm* cd=RepositoryKeeper::GetRepository()->GetClass(classTypeId);
    const PropertyMappingDescriptionShm* pmd=cd->GetPropertyMapping(propertyTypeId, isInherited);
    const MemberMappingDescriptionShm* mm=pmd->GetMemberMapping(propertyMember);
    std::pair<const ParameterDescriptionShm*, int> param=mm->GetParameter();

    for (DotsC_ParameterIndex pix=0; pix<cd->GetNumberOfParameters(); ++pix)
    {
        if (cd->GetParameter(pix)==param.first)
        {
            DotsC_CollectionType collectionType=pmd->GetProperty()->GetMember(propertyMember)->GetCollectionType();
            paramId=pix;
            paramValueIndex=collectionType==SingleValueCollectionType ? param.second : index;
        }
    }
}

////************************************************************************************
////* Serialization
////************************************************************************************
void DotsC_BlobToXml(char* const xmlDest, const char* const blobSource, const DotsC_Int32 bufSize, DotsC_Int32 & resultSize)
{
    Init();
    std::ostringstream xmlStream;
    ts::BinaryToXml(RepositoryKeeper::GetRepository(), blobSource, xmlStream);
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
    ts::XmlToBinary(RepositoryKeeper::GetRepository(), xmlSource, blob);
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
    ts::BinaryToJson(RepositoryKeeper::GetRepository(), blobSource, jsonStream);
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
    ts::JsonToBinary(RepositoryKeeper::GetRepository(), jsonSource, blob);
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
    ts::BinaryToBase64(binarySource, sourceSize, b64Stream);
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
    ts::Base64ToBinary(base64, bin);
    resultSize=static_cast<DotsC_Int32>(bin.size());
    if (resultSize<=destSize)
    {
        memcpy(binaryDest, &bin[0], bin.size());
    }
}

//************************************************************************************
//* Functions for retrieval of parameters
//************************************************************************************
void DotsC_GetBooleanParameter(const DotsC_TypeId typeId, const DotsC_ParameterIndex parameter, const DotsC_Int32 index, bool & val)
{
    Init();
    const ParameterDescriptionShm* const pd=RepositoryKeeper::GetRepository()->GetClass(typeId)->GetParameter(parameter);
    val=pd->GetBoolValue(index);
}

void DotsC_GetEnumerationParameter(const DotsC_TypeId typeId, const DotsC_ParameterIndex parameter, const DotsC_Int32 index, const DotsC_KeyValMode keyValMode, DotsC_EnumerationValue& val)
{
    Init();
    const ParameterDescriptionShm* const pd=RepositoryKeeper::GetRepository()->GetClass(typeId)->GetParameter(parameter);
    if (keyValMode==DotsC_ValueMode)
    {
        val=pd->GetInt32Value(index);
    }
    else
    {
        val=pd->GetInt32Key(index);
    }
}

void DotsC_GetInt32Parameter(const DotsC_TypeId typeId, const DotsC_ParameterIndex parameter, const DotsC_Int32 index, const DotsC_KeyValMode keyValMode, DotsC_Int32& val)
{
    Init();
    const ParameterDescriptionShm* const pd=RepositoryKeeper::GetRepository()->GetClass(typeId)->GetParameter(parameter);
    if (keyValMode==DotsC_ValueMode)
    {
        val=pd->GetInt32Value(index);
    }
    else
    {
        val=pd->GetInt32Key(index);
    }
}

void DotsC_GetInt64Parameter(const DotsC_TypeId typeId, const DotsC_ParameterIndex parameter, const DotsC_Int32 index, const DotsC_KeyValMode keyValMode, DotsC_Int64& val)
{
    Init();
    const ParameterDescriptionShm* const pd=RepositoryKeeper::GetRepository()->GetClass(typeId)->GetParameter(parameter);
    if (keyValMode==DotsC_ValueMode)
    {
        val=pd->GetInt64Value(index);
    }
    else
    {
        val=pd->GetInt64Key(index);
    }
}

void DotsC_GetFloat32Parameter(const DotsC_TypeId typeId, const DotsC_ParameterIndex parameter, const DotsC_Int32 index, DotsC_Float32& val)
{
    Init();
    const ParameterDescriptionShm* const pd=RepositoryKeeper::GetRepository()->GetClass(typeId)->GetParameter(parameter);
    val=pd->GetFloat32Value(index);
}

void DotsC_GetFloat64Parameter(const DotsC_TypeId typeId, const DotsC_ParameterIndex parameter, const DotsC_Int32 index, DotsC_Float64& val)
{
    Init();
    const ParameterDescriptionShm* const pd=RepositoryKeeper::GetRepository()->GetClass(typeId)->GetParameter(parameter);
    val=pd->GetFloat64Value(index);
}

void DotsC_GetStringParameter(const DotsC_TypeId typeId, const DotsC_ParameterIndex parameter, const DotsC_Int32 index, const DotsC_KeyValMode keyValMode, const char* &val)
{
    Init();
    const ParameterDescriptionShm* const pd=RepositoryKeeper::GetRepository()->GetClass(typeId)->GetParameter(parameter);
    if (keyValMode==DotsC_ValueMode)
    {
        val=pd->GetStringValue(index);
    }
    else
    {
        val=pd->GetStringKey(index);
    }
}

void DotsC_GetTypeIdParameter(const DotsC_TypeId typeId, const DotsC_ParameterIndex parameter, const DotsC_Int32 index, const DotsC_KeyValMode keyValMode, DotsC_TypeId& val)
{
    Init();
    const ParameterDescriptionShm* const pd=RepositoryKeeper::GetRepository()->GetClass(typeId)->GetParameter(parameter);
    val=pd->GetInt64Value(index);
    if (keyValMode==DotsC_ValueMode)
    {
        val=pd->GetInt64Value(index);
    }
    else
    {
        val=pd->GetInt64Key(index);
    }
}

void DotsC_GetHashedIdParameter(const DotsC_TypeId typeId,
                                const DotsC_ParameterIndex parameter,
                                const DotsC_Int32 index,
                                const DotsC_KeyValMode keyValMode,
                                DotsC_Int64 & hashVal,
                                const char* & strVal)
{
    Init();
    const ParameterDescriptionShm* const pd=RepositoryKeeper::GetRepository()->GetClass(typeId)->GetParameter(parameter);
    std::pair<DotsC_Int64, const char*> val=(keyValMode==DotsC_ValueMode) ? pd->GetHashedValue(index) : pd->GetHashedKey(index);
    hashVal=val.first;
    strVal=val.second;
}

void DotsC_GetEntityIdParameter(const DotsC_TypeId typeId,
                                const DotsC_ParameterIndex parameter,
                                const DotsC_Int32 index,
                                const DotsC_KeyValMode keyValMode,
                                DotsC_EntityId & entityId,
                                const char* & instanceIdStr)
{
    Init();
    const ParameterDescriptionShm* const pd=RepositoryKeeper::GetRepository()->GetClass(typeId)->GetParameter(parameter);
    if (keyValMode==DotsC_ValueMode)
    {
        entityId.typeId=pd->GetInt64Value(index);
        std::pair<DotsC_Int64, const char*> hashVal=pd->GetHashedValue(index);
        entityId.instanceId=hashVal.first;
        instanceIdStr=hashVal.second;
    }
    else
    {
        entityId.typeId=pd->GetInt64Key(index);
        std::pair<DotsC_Int64, const char*> hashVal=pd->GetHashedKey(index);
        entityId.instanceId=hashVal.first;
        instanceIdStr=hashVal.second;
    }
}

void DotsC_GetObjectParameter(const DotsC_TypeId typeId,
                              const DotsC_ParameterIndex parameter,
                              const DotsC_Int32 index,
                              const char* & val)
{
    Init();
    const ParameterDescriptionShm* const pd=RepositoryKeeper::GetRepository()->GetClass(typeId)->GetParameter(parameter);
    val=pd->GetObjectValue(index).first;
}

void DotsC_GetBinaryParameter(const DotsC_TypeId typeId,
                              const DotsC_ParameterIndex parameter,
                              const DotsC_Int32 index,
                              const char* &val,
                              DotsC_Int32& size)
{
    Init();
    const ParameterDescriptionShm* const pd=RepositoryKeeper::GetRepository()->GetClass(typeId)->GetParameter(parameter);
    std::pair<const char*, size_t> binary=pd->GetBinaryValue(index);
    val=binary.first;
    size=static_cast<DotsC_Int32>(binary.second);
}

DotsC_Int32 DotsC_DictionaryInt32KeyToIndex(const DotsC_TypeId typeId, const DotsC_ParameterIndex parameter, const DotsC_Int32 key)
{
    Init();
    const ParameterDescriptionShm* const pd=RepositoryKeeper::GetRepository()->GetClass(typeId)->GetParameter(parameter);
    return ts::TypeUtilities::GetDictionaryIndexFromKey(pd, key);
}

DotsC_Int32 DotsC_DictionaryInt64KeyToIndex(const DotsC_TypeId typeId, const DotsC_ParameterIndex parameter, const DotsC_Int64 key)
{
    Init();
    const ParameterDescriptionShm* const pd=RepositoryKeeper::GetRepository()->GetClass(typeId)->GetParameter(parameter);
    return ts::TypeUtilities::GetDictionaryIndexFromKey(pd, key);
}

DotsC_Int32 DotsC_DictionaryStringKeyToIndex(const DotsC_TypeId typeId, const DotsC_ParameterIndex parameter, const char* key)
{
    Init();
    const ParameterDescriptionShm* const pd=RepositoryKeeper::GetRepository()->GetClass(typeId)->GetParameter(parameter);
    return ts::TypeUtilities::GetDictionaryIndexFromKey(pd, key);
}

DotsC_Int32 DotsC_DictionaryEntityIdKeyToIndex(const DotsC_TypeId typeId, const DotsC_ParameterIndex parameter, const DotsC_EntityId key)
{
    Init();
    const ParameterDescriptionShm* const pd=RepositoryKeeper::GetRepository()->GetClass(typeId)->GetParameter(parameter);
    return ts::TypeUtilities::GetDictionaryIndexFromKey(pd, key);
}


//********************************************************
//* Base operations on blobs
//********************************************************

char* DotsC_AllocateBlob(DotsC_Int32 size)
{
    Init();
    char* blob=new char[static_cast<size_t>(size)];
    return blob;
}

void DotsC_CreateCopyOfBlob(char* & to, const char* from)
{
    Init();
    size_t size=static_cast<size_t>(Reader::GetSize(from));
    to=new char[size];
    memcpy(to, from, size);
}

void DotsC_DeleteBlob(char* & blob)
{
    Init();
    if (blob!=NULL)
    {
        delete[] blob;
        blob=NULL;
    }
}

//Read operations
DotsC_TypeId DotsC_GetTypeId(const char* blob)
{
    Init();
    return Reader::GetTypeId(blob);
}

DotsC_Int32 DotsC_GetSize(const char* blob)
{
    Init();
    return Reader::GetSize(blob);
}

DotsC_Handle DotsC_CreateBlobReader(const char* blob)
{
    Init();
    Reader* reader=new Reader(RepositoryKeeper::GetRepository(), blob);
    DotsC_Handle address=(DotsC_Handle)reader;
    return address;
}

void DotsC_DeleteBlobReader(DotsC_Handle readerHandle)
{
    Init();
    Reader* reader=ReaderFromHandle(readerHandle);
    delete reader;
}

DotsC_Int32 DotsC_GetNumberOfMemberValues(DotsC_Handle readerHandle, DotsC_MemberIndex member)
{
    Init();
    Reader* reader=ReaderFromHandle(readerHandle);
    return reader->NumberOfValues(member);
}

void DotsC_ReadMemberStatus(DotsC_Handle readerHandle, bool& isNull, bool& isChanged, DotsC_MemberIndex member, DotsC_Int32 arrayIndex)
{
    Init();
    Reader* reader=ReaderFromHandle(readerHandle);
    reader->ReadStatus(member, arrayIndex, isNull, isChanged);
}

void DotsC_ReadInt32Member(DotsC_Handle readerHandle, DotsC_Int32& val, bool& isNull, bool& isChanged, DotsC_MemberIndex member, DotsC_Int32 arrayIndex, DotsC_KeyValMode keyValMode)
{
    Init();
    Reader* reader=ReaderFromHandle(readerHandle);
    if (keyValMode==DotsC_ValueMode)
    {
        reader->ReadValue(member, arrayIndex, val, isNull, isChanged);
    }
    else
    {
        isChanged=false;
        isNull=false;
        val=reader->ReadKey<DotsC_Int32>(member, arrayIndex);
    }
}

void DotsC_ReadInt64Member(DotsC_Handle readerHandle, DotsC_Int64& val, bool& isNull, bool& isChanged, DotsC_MemberIndex member, DotsC_Int32 arrayIndex, DotsC_KeyValMode keyValMode)
{
    Init();
    Reader* reader=ReaderFromHandle(readerHandle);
    if (keyValMode==DotsC_ValueMode)
    {
        reader->ReadValue(member, arrayIndex, val, isNull, isChanged);
    }
    else
    {
        isChanged=false;
        isNull=false;
        val=reader->ReadKey<DotsC_Int64>(member, arrayIndex);
    }
}

void DotsC_ReadFloat32Member(DotsC_Handle readerHandle, DotsC_Float32& val, bool& isNull, bool& isChanged, DotsC_MemberIndex member, DotsC_Int32 arrayIndex, DotsC_KeyValMode /*keyValMode*/)
{
    Init();
    Reader* reader=ReaderFromHandle(readerHandle);
    reader->ReadValue(member, arrayIndex, val, isNull, isChanged);
}

void DotsC_ReadFloat64Member(DotsC_Handle readerHandle, DotsC_Float64& val, bool& isNull, bool& isChanged, DotsC_MemberIndex member, DotsC_Int32 arrayIndex, DotsC_KeyValMode /*keyValMode*/)
{
    Init();
    Reader* reader=ReaderFromHandle(readerHandle);
    reader->ReadValue(member, arrayIndex, val, isNull, isChanged);
}

void DotsC_ReadBooleanMember(DotsC_Handle readerHandle, bool& val, bool& isNull, bool& isChanged, DotsC_MemberIndex member, DotsC_Int32 arrayIndex, DotsC_KeyValMode /*keyValMode*/)
{
    Init();
    Reader* reader=ReaderFromHandle(readerHandle);
    reader->ReadValue(member, arrayIndex, val, isNull, isChanged);
}

void DotsC_ReadStringMember(DotsC_Handle readerHandle, const char*& val, bool& isNull, bool& isChanged, DotsC_MemberIndex member, DotsC_Int32 arrayIndex, DotsC_KeyValMode keyValMode)
{
    Init();
    Reader* reader=ReaderFromHandle(readerHandle);
    if (keyValMode==DotsC_ValueMode)
    {
        reader->ReadValue(member, arrayIndex, val, isNull, isChanged);
    }
    else
    {
        isChanged=false;
        isNull=false;
        val=reader->ReadKey<const char*>(member, arrayIndex);
    }
}

void DotsC_ReadHashedMember(DotsC_Handle readerHandle, DotsC_Int64& val, const char*& optionalStr, bool& isNull, bool& isChanged, DotsC_MemberIndex member, DotsC_Int32 arrayIndex, DotsC_KeyValMode keyValMode)
{
    Init();
    Reader* reader=ReaderFromHandle(readerHandle);
    std::pair<DotsC_Int64, const char*> hash;
    if (keyValMode==DotsC_ValueMode)
    {
        reader->ReadValue(member, arrayIndex, hash, isNull, isChanged);
    }
    else
    {
        isChanged=false;
        isNull=false;
        hash=reader->ReadKey< std::pair<DotsC_Int64, const char*> >(member, arrayIndex);
    }

    val=hash.first;
    optionalStr=hash.second;
}

void DotsC_ReadEntityIdMember(DotsC_Handle readerHandle, DotsC_EntityId& val, const char*& optionalStr, bool& isNull, bool& isChanged, DotsC_MemberIndex member, DotsC_Int32 arrayIndex, DotsC_KeyValMode keyValMode)
{
    Init();
    Reader* reader=ReaderFromHandle(readerHandle);
    std::pair<DotsC_EntityId, const char*> eid;
    if (keyValMode==DotsC_ValueMode)
    {
        reader->ReadValue(member, arrayIndex, eid, isNull, isChanged);
    }
    else
    {
        isChanged=false;
        isNull=false;
        eid=reader->ReadKey< std::pair<DotsC_EntityId, const char*> >(member, arrayIndex);
    }

    val=eid.first;
    optionalStr=eid.second;
}

void DotsC_ReadBinaryMember(DotsC_Handle readerHandle, const char*& val, DotsC_Int32& size, bool& isNull, bool& isChanged, DotsC_MemberIndex member, DotsC_Int32 arrayIndex, DotsC_KeyValMode /*keyValMode*/)
{
    Init();
    Reader* reader=ReaderFromHandle(readerHandle);
    std::pair<const char*, DotsC_Int32> bin;
    reader->ReadValue(member, arrayIndex, bin, isNull, isChanged);
    val=bin.first;
    size=bin.second;
}

void DotsC_ReadObjectMember(DotsC_Handle readerHandle, const char*& val, bool& isNull, bool& isChanged, DotsC_MemberIndex member, DotsC_Int32 arrayIndex, DotsC_KeyValMode /*keyValMode*/)
{
    Init();
    Reader* reader=ReaderFromHandle(readerHandle);
    std::pair<const char*, DotsC_Int32> bin;
    reader->ReadValue(member, arrayIndex, bin, isNull, isChanged);
    val=bin.first;
}

//Write operations
DotsC_Handle DotsC_CreateBlobWriter(DotsC_TypeId typeId)
{
    Init();
    Writer* writer=new Writer(RepositoryKeeper::GetRepository(), typeId);
    DotsC_Handle address=(DotsC_Handle)writer;
    return address;
}

DotsC_Handle DotsC_CreateBlobWriterFromBlob(const char* blob)
{
    Init();
    Writer* writer=new Writer(Reader(RepositoryKeeper::GetRepository(), blob));
    DotsC_Handle address=(DotsC_Handle)writer;
    return address;
}

DotsC_Handle DotsC_CreateBlobWriterFromReader(DotsC_Handle readerHandle)
{
    Init();
    Writer* writer=new Writer(*ReaderFromHandle(readerHandle));
    DotsC_Handle address=(DotsC_Handle)writer;
    return address;

}

void DotsC_DeleteBlobWriter(DotsC_Handle writerHandle)
{
    Init();
    Writer* writer=WriterFromHandle(writerHandle);
    delete writer;
}

DotsC_Int32 DotsC_CalculateBlobSize(DotsC_Handle writerHandle)
{
    Init();
    Writer* writer=WriterFromHandle(writerHandle);
    return writer->CalculateBlobSize();
}

void DotsC_WriteBlob(DotsC_Handle writerHandle, char* blobDest)
{
    Init();
    Writer* writer=WriterFromHandle(writerHandle);
    writer->CopyRawBlob(blobDest);
}

void DotsC_WriteAllChangeFlags(DotsC_Handle writerHandle, bool changed)
{
    Init();
    Writer* writer=WriterFromHandle(writerHandle);
    writer->SetAllChangeFlags(changed);
}

void DotsC_WriteChangeFlag(DotsC_Handle writerHandle,
                           DotsC_MemberIndex member,
                           DotsC_ArrayIndex index,
                           bool changed)
{
    Init();
    Writer* writer=WriterFromHandle(writerHandle);
    writer->SetChanged(member, index, changed);
}

bool DotsC_MarkChanges(DotsC_Handle originalReader, DotsC_Handle currentWriter)
{
    Init();
    const Reader* original=ReaderFromHandle(originalReader);
    Writer* current=WriterFromHandle(currentWriter);
    bool anythingChanged=current->MarkChanges(*original);
    return anythingChanged;
}

void DotsC_WriteInt32Member(DotsC_Handle writerHandle, DotsC_Int32 val,
                            bool isNull,
                            bool isChanged,
                            DotsC_MemberIndex member,
                            DotsC_Int32 arrayIndex,
                            DotsC_KeyValMode keyValMode)
{
    Init();
    Writer* writer=WriterFromHandle(writerHandle);
    if (keyValMode==DotsC_ValueMode)
    {
        writer->WriteValue(member, arrayIndex, val, isNull, isChanged);
    }
    else
    {
        writer->WriteKey(member, val);
    }
}

void DotsC_WriteInt64Member(DotsC_Handle writerHandle, DotsC_Int64 val,
                            bool isNull,
                            bool isChanged,
                            DotsC_MemberIndex member,
                            DotsC_Int32 arrayIndex,
                            DotsC_KeyValMode keyValMode)
{
    Init();
    Writer* writer=WriterFromHandle(writerHandle);
    if (keyValMode==DotsC_ValueMode)
    {
        writer->WriteValue(member, arrayIndex, val, isNull, isChanged);
    }
    else
    {
        writer->WriteKey(member, val);
    }
}

void DotsC_WriteFloat32Member(DotsC_Handle writerHandle, DotsC_Float32 val,
                              bool isNull,
                              bool isChanged,
                              DotsC_MemberIndex member,
                              DotsC_Int32 arrayIndex,
                              DotsC_KeyValMode)
{
    Init();
    Writer* writer=WriterFromHandle(writerHandle);
    writer->WriteValue(member, arrayIndex, val, isNull, isChanged);
}

void DotsC_WriteFloat64Member(DotsC_Handle writerHandle, DotsC_Float64 val,
                              bool isNull,
                              bool isChanged,
                              DotsC_MemberIndex member,
                              DotsC_Int32 arrayIndex,
                              DotsC_KeyValMode)
{
    Init();
    Writer* writer=WriterFromHandle(writerHandle);
    writer->WriteValue(member, arrayIndex, val, isNull, isChanged);
}

void DotsC_WriteBooleanMember(DotsC_Handle writerHandle, bool val,
                              bool isNull,
                              bool isChanged,
                              DotsC_MemberIndex member,
                              DotsC_Int32 arrayIndex,
                              DotsC_KeyValMode)
{
    Init();
    Writer* writer=WriterFromHandle(writerHandle);
    writer->WriteValue(member, arrayIndex, val, isNull, isChanged);
}

void DotsC_WriteStringMember(DotsC_Handle writerHandle, const char* val,
                             bool isNull,
                             bool isChanged,
                             DotsC_MemberIndex member,
                             DotsC_Int32 arrayIndex,
                             DotsC_KeyValMode keyValMode)
{
    Init();
    Writer* writer=WriterFromHandle(writerHandle);
    if (keyValMode==DotsC_ValueMode)
    {
        writer->WriteValue(member, arrayIndex, val, isNull, isChanged);
    }
    else
    {
        writer->WriteKey(member, val);
    }
}

void DotsC_WriteHashedMember(DotsC_Handle writerHandle, DotsC_Int64 hash, const char* str,
                             bool isNull,
                             bool isChanged,
                             DotsC_MemberIndex member,
                             DotsC_Int32 arrayIndex,
                             DotsC_KeyValMode keyValMode)
{
    Init();
    Writer* writer=WriterFromHandle(writerHandle);
    if (keyValMode==DotsC_ValueMode)
    {
        writer->WriteValue(member, arrayIndex, std::make_pair(hash, str), isNull, isChanged);
    }
    else
    {
        writer->WriteKey(member, std::make_pair(hash, str));
    }
}

void DotsC_WriteEntityIdMember(DotsC_Handle writerHandle, DotsC_EntityId val, const char* instanceString,
                               bool isNull,
                               bool isChanged,
                               DotsC_MemberIndex member,
                               DotsC_Int32 arrayIndex,
                               DotsC_KeyValMode keyValMode)
{
    Init();
    Writer* writer=WriterFromHandle(writerHandle);
    if (keyValMode==DotsC_ValueMode)
    {
        writer->WriteValue(member, arrayIndex, std::make_pair(val, instanceString), isNull, isChanged);
    }
    else
    {
        writer->WriteKey(member, std::make_pair(val, instanceString));
    }
}

void DotsC_WriteBinaryMember(DotsC_Handle writerHandle, const char* val, DotsC_Int32 size,
                             bool isNull,
                             bool isChanged,
                             DotsC_MemberIndex member,
                             DotsC_Int32 arrayIndex,
                             DotsC_KeyValMode)
{
    Init();
    Writer* writer=WriterFromHandle(writerHandle);
    writer->WriteValue(member, arrayIndex, std::make_pair(val, size), isNull, isChanged);
}

void DotsC_WriteObjectMember(DotsC_Handle writerHandle, const char* blob,
                             bool isNull,
                             bool isChanged,
                             DotsC_MemberIndex member,
                             DotsC_Int32 arrayIndex,
                             DotsC_KeyValMode)
{
    Init();
    Writer* writer=WriterFromHandle(writerHandle);
    DotsC_Int32 size=isNull ? 0 : DotsC_GetSize(blob);
    writer->WriteValue(member, arrayIndex, std::make_pair(blob, size), isNull, isChanged);
}

//************************************************************************************
//* Library exception handling
//************************************************************************************
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

//************************************************************************************
//* Functions mostly indended for debugging
//************************************************************************************
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
        ts::RepositoryToString(RepositoryKeeper::GetRepository(), false, os);
    }
    else
    {
        ts::TypeToString(RepositoryKeeper::GetRepository(), typeId, os);
    }

    std::string text=os.str();
    resultSize=static_cast<DotsC_Int32>(text.size())+1; //add one for null char
    if (resultSize>bufSize)
    {
        return;
    }

    strncpy(buf, text.c_str(), resultSize);
}

void DotsC_GetGeneratedLibraryList(DotsC_GeneratedLibrary*& generatedLibraries,
                                   DotsC_Int32& size,
                                   DotsC_GeneratedLibraryListDeleter& deleter)
{
    try
    {
        Safir::Utilities::Internal::ConfigReader reader;
        const boost::property_tree::ptree& ptree = reader.Typesystem();

        //work out our size
        size = 0;
        for (boost::property_tree::ptree::const_iterator it = ptree.begin();
             it != ptree.end(); ++it)
        {
            if (IsValidSection(it->second))
            {
                ++size;
            }
        }

        //allocate our array.
        generatedLibraries = new DotsC_GeneratedLibrary[size];
        deleter = DeleteGeneratedLibraryList;

        int i = 0;
        for (boost::property_tree::ptree::const_iterator it = ptree.begin();
             it != ptree.end(); ++it)
        {
            if (IsValidSection(it->second))
            {
                const std::string module = it->first;
                generatedLibraries[i].name = CopyStringToNew(module);

                const std::string kind = it->second.get<std::string>("kind");
                if (kind != "library" && kind != "override")
                {
                    throw std::logic_error("Illegal value for 'kind' in typesystem.ini");
                }
                generatedLibraries[i].library = (kind == "library") ? 1 : 0;

                generatedLibraries[i].cppLibraryName = CopyStringToNew("safir_generated-" + module + "-cpp");
                generatedLibraries[i].dotnetAssemblyName = CopyStringToNew("safir_generated-" + module + "-dotnet");
                generatedLibraries[i].javaJarName = CopyStringToNew("safir_generated-" + module + "-java.jar");

                const boost::optional<std::string> cpp_library_location =
                    it->second.get_optional<std::string>("cpp_library_location");
                if (cpp_library_location)
                {
                    generatedLibraries[i].cppLibraryLocation = CopyStringToNew(cpp_library_location.get());
                }
                else
                {
                    generatedLibraries[i].cppLibraryLocation = NULL;
                }

                const boost::optional<std::string> dotnet_assembly_location =
                    it->second.get_optional<std::string>("dotnet_assembly_location");
                if (dotnet_assembly_location)
                {
                    generatedLibraries[i].dotnetAssemblyLocation = CopyStringToNew(dotnet_assembly_location.get());
                }
                else
                {
                    generatedLibraries[i].dotnetAssemblyLocation = NULL;
                }

                const boost::optional<std::string> java_jar_location =
                    it->second.get_optional<std::string>("java_jar_location");
                if (java_jar_location)
                {
                    generatedLibraries[i].javaJarLocation = CopyStringToNew(java_jar_location.get());
                }
                else
                {
                    generatedLibraries[i].javaJarLocation = NULL;
                }

                ++i;
            }
        }
    }
    catch(...)
    {
        SEND_SYSTEM_LOG(Critical, << "Error occurred while reading typesystem.ini");
        generatedLibraries = NULL;
        deleter = NULL;
        size = 0;
    }
}
