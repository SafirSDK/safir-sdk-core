/******************************************************************************
*
* Copyright Saab AB, 2009-2013 (http://safir.sourceforge.net)
*
* Created by: Lars Hagstrom / stlrha
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

#include <Safir/Utilities/Internal/Id.h>
#include <Safir/Dob/Typesystem/Internal/Kernel.h>

#ifdef __GNUC__
#pragma GCC visibility push (default)
#endif

#include "com_saabgroup_safir_dob_typesystem_Kernel.h"

#ifdef __GNUC__
#pragma GCC visibility pop
#endif

#include <Safir/Utilities/Internal/ConfigReader.h>
#include <iostream>
#include <vector>
#include <assert.h>
#include <boost/shared_ptr.hpp>
#include <boost/scoped_array.hpp>
#include <boost/bind.hpp>
#include <string.h>
#include <boost/filesystem.hpp>
#include <boost/algorithm/string.hpp>

//use this to set the first element of an array
//will assert on arraylength == 1
void SetJArray(JNIEnv * env, jbooleanArray array, const bool toValue)
{
    jboolean isCopy;
    jboolean * arrayElems = env->GetBooleanArrayElements(array, &isCopy);
    assert(env->GetArrayLength(array) == 1);
    arrayElems[0] = toValue;
    if (isCopy == JNI_TRUE)
    {
        env->ReleaseBooleanArrayElements(array, arrayElems, 0);
    }
}

void SetJArray(JNIEnv * env, jintArray array, const DotsC_Int32 toValue)
{
    jboolean isCopy = false;
    jint * arrayElems = env->GetIntArrayElements(array, &isCopy);
    assert(env->GetArrayLength(array) == 1);
    arrayElems[0] = toValue;
    if (isCopy == JNI_TRUE)
    {
        env->ReleaseIntArrayElements(array, arrayElems, 0);
    }
}

DotsC_Int32 GetJArrayInt32(JNIEnv * env, jintArray array)
{
    jboolean isCopy = false;
    jint * arrayElems = env->GetIntArrayElements(array, &isCopy);
    assert(env->GetArrayLength(array) == 1);
    DotsC_Int32 val = arrayElems[0];
    if (isCopy == JNI_TRUE)
    {
        env->ReleaseIntArrayElements(array, arrayElems, 0);
    }
    return val;
}


void SetJArray(JNIEnv * env, jlongArray array, const DotsC_Int64 toValue)
{
    jboolean isCopy;
    jlong * arrayElems = env->GetLongArrayElements(array, &isCopy);
    assert(env->GetArrayLength(array) == 1);
    arrayElems[0] = toValue;
    if (isCopy == JNI_TRUE)
    {
        env->ReleaseLongArrayElements(array, arrayElems, 0);
    }
}

void SetJArray(JNIEnv * env, jfloatArray array, const DotsC_Float32 toValue)
{
    jboolean isCopy;
    jfloat * arrayElems = env->GetFloatArrayElements(array, &isCopy);
    assert(env->GetArrayLength(array) == 1);
    arrayElems[0] = toValue;
    if (isCopy == JNI_TRUE)
    {
        env->ReleaseFloatArrayElements(array, arrayElems, 0);
    }
}

void SetJArray(JNIEnv * env, jdoubleArray array, const DotsC_Float64 toValue)
{
    jboolean isCopy;
    jdouble * arrayElems = env->GetDoubleArrayElements(array, &isCopy);
    assert(env->GetArrayLength(array) == 1);
    arrayElems[0] = toValue;
    if (isCopy == JNI_TRUE)
    {
        env->ReleaseDoubleArrayElements(array, arrayElems, 0);
    }
}

void SetJArray(JNIEnv * _env, jobjectArray array, const jobject obj)
{
    assert(_env->GetArrayLength(array) == 1);
    _env->SetObjectArrayElement(array, 0 , obj);
}




typedef boost::shared_ptr<const char> StringHolder;

const StringHolder GetUtf8(JNIEnv * env, const jstring & jstr)
{
    jboolean isCopy=false;
    boost::shared_ptr<const char> holder(env->GetStringUTFChars(jstr, &isCopy),
                                         boost::bind(&JNIEnv::ReleaseStringUTFChars,env,jstr,_1));

    assert(isCopy);
    return holder;
}

//************************************************************************************
//* Functions for hash numbers
//************************************************************************************
/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GenerateRandom64
 * Signature: ()J
 */
jlong JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GenerateRandom64
  (JNIEnv *, jclass)
{
    return LlufId_GenerateRandom64();
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    Generate64
 * Signature: (Ljava/lang/String;)J
 */
jlong JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_Generate64
  (JNIEnv * env, jclass, jstring _str)
{
    return LlufId_Generate64(GetUtf8(env,_str).get());
}

//********************************************************
//* Static type information operations
//********************************************************
/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    NumberOfTypeIds
 * Signature: ()I
 */
jint JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_NumberOfTypeIds
  (JNIEnv *, jclass)
{
    return DotsC_NumberOfTypeIds();
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    NumberOfClasses
 * Signature: ()I
 */
jint JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_NumberOfClasses
  (JNIEnv *, jclass)
{
    return DotsC_NumberOfClasses();
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    NumberOfProperties
 * Signature: ()I
 */
jint JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_NumberOfProperties
  (JNIEnv *, jclass)
{
    return DotsC_NumberOfProperties();
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    NumberOfEnumerations
 * Signature: ()I
 */
jint JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_NumberOfEnumerations
  (JNIEnv *, jclass)
{
    return DotsC_NumberOfEnumerations();
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    NumberOfExceptions
 * Signature: ()I
 */
jint JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_NumberOfExceptions
  (JNIEnv *, jclass)
{
    return DotsC_NumberOfExceptions();
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetAllTypeIds
 * Signature: ([JI[I)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetAllTypeIds
  (JNIEnv * env, jclass, jlongArray _buf, jint _bufSize, jintArray _size)
{
    DotsC_Int32 size;
    boost::scoped_array<DotsC_TypeId> arr (new DotsC_TypeId[_bufSize]);
    DotsC_GetAllTypeIds(arr.get(), _bufSize, size);
    env->SetLongArrayRegion(_buf, 0, _bufSize, arr.get());
    SetJArray(env,_size,size);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    TypeExists
 * Signature: (J)Z
 */
jboolean JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_TypeExists
  (JNIEnv *, jclass, jlong _typeId)
{
    return DotsC_TypeExists(_typeId);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    IsClass
 * Signature: (J)Z
 */
jboolean JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_IsClass
  (JNIEnv *, jclass, jlong _typeId)
{
    return DotsC_IsClass(_typeId);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    IsProperty
 * Signature: (J)Z
 */
jboolean JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_IsProperty
  (JNIEnv *, jclass, jlong _typeId)
{
    return DotsC_IsProperty(_typeId);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    IsEnumeration
 * Signature: (J)Z
 */
jboolean JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_IsEnumeration
  (JNIEnv *, jclass, jlong _typeId)
{
    return DotsC_IsEnumeration(_typeId);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    IsException
 * Signature: (J)Z
 */
jboolean JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_IsException
  (JNIEnv *, jclass, jlong _typeId)
{
    return DotsC_IsException(_typeId);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    TypeIdFromName
 * Signature: (Ljava/lang/String;)J
 */
jlong JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_TypeIdFromName
  (JNIEnv * env, jclass, jstring _typeName)
{
    return DotsC_TypeIdFromName(GetUtf8(env,_typeName).get());
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetTypeName
 * Signature: (J)Ljava/lang/String;
 */
jstring JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetTypeName
  (JNIEnv * env, jclass, jlong _typeId)
{
    const char* typeName=DotsC_GetTypeName(_typeId);
    if (typeName != NULL)
    {
        return env->NewStringUTF(typeName);
    }
    else
    {
        return NULL;
    }
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    MemberTypeName
 * Signature: (I)Ljava/lang/String;
 */
jstring JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_MemberTypeName
  (JNIEnv * env, jclass, jint memberType)
{
    const char* name=DotsC_MemberTypeName(static_cast<DotsC_MemberType>(memberType));
    if (name != NULL)
    {
        return env->NewStringUTF(name);
    }
    else
    {
        return NULL;
    }
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetNumberOfEnumerationValues
 * Signature: (J)I
 */
jint JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetNumberOfEnumerationValues
  (JNIEnv *, jclass, jlong _enumId)
{
    return DotsC_GetNumberOfEnumerationValues(_enumId);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetEnumerationValueName
 * Signature: (JI)Ljava/lang/String;
 */
jstring JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetEnumerationValueName
  (JNIEnv * env, jclass, jlong _enumId, jint _enumVal)
{
    const char* name=DotsC_GetEnumerationValueName(_enumId, _enumVal);
    if (name != NULL)
    {
        return env->NewStringUTF(name);
    }
    else
    {
        return NULL;
    }
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    EnumerationValueFromName
 * Signature: (JLjava/lang/String;)I
 */
jint JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_EnumerationValueFromName
  (JNIEnv * env, jclass, jlong _enumId, jstring _enumValueName)
{
    return DotsC_EnumerationValueFromName(_enumId,GetUtf8(env,_enumValueName).get());
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetEnumerationChecksum
 * Signature: (J)J
 */
jlong JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetEnumerationChecksum
  (JNIEnv *, jclass, jlong _typeId)
{
    DotsC_TypeId checksum;
    DotsC_GetEnumerationChecksum(_typeId,checksum);
    return checksum;
}

//********************************************************
//* Functions for retrieving member info about object types
//********************************************************
/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetNumberOfMembers
 * Signature: (J)I
 */
jint JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetNumberOfMembers
  (JNIEnv *, jclass, jlong _typeId)
{
    return DotsC_GetNumberOfMembers(_typeId);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetMemberId
 * Signature: (JLjava/lang/String;)I
 */
jint JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetMemberId
  (JNIEnv * env, jclass, jlong _typeId, jstring _memberName)
{
    return DotsC_GetMemberId(_typeId, GetUtf8(env,_memberName).get());
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetMemberInfo
 * Signature: (JI[I[Ljava/lang/String;[J[I[I[I)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetMemberInfo
  (JNIEnv * env, jclass, jlong _typeId, jint _member, jintArray _memberType, jobjectArray _memberName, jlongArray _complexType, jintArray _stringLength, jintArray _collectionType, jintArray _arrayLength)
{
    DotsC_MemberType memberType;
    const char * memberName;
    DotsC_TypeId complexType;
    DotsC_Int32 stringLength;
    DotsC_CollectionType collectionType;
    DotsC_Int32 arrayLength;

    DotsC_GetMemberInfo(_typeId,
                        _member,
                        memberType,
                        memberName,
                        complexType,
                        stringLength,
                        collectionType,
                        arrayLength);

    SetJArray(env,_memberType, memberType);
    SetJArray(env,_memberName, env->NewStringUTF(memberName));
    SetJArray(env,_complexType, complexType);
    SetJArray(env,_stringLength, stringLength);
    SetJArray(env,_collectionType, collectionType);
    SetJArray(env,_arrayLength, arrayLength);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetMemberArraySizeProperty
 * Signature: (JJI)I
 */
jint JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetMemberArraySizeProperty
  (JNIEnv *, jclass, jlong _classId, jlong _propertyId, jint _propertyMember)
{
    return DotsC_GetMemberArraySizeProperty(_classId, _propertyId, _propertyMember);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetStringMemberMaxLengthProperty
 * Signature: (JJI)I
 */
jint JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetStringMemberMaxLengthProperty
  (JNIEnv *, jclass, jlong _classId, jlong _propertyId, jint _propertyMember)
{
    return DotsC_GetStringMemberMaxLengthProperty(_classId, _propertyId, _propertyMember);
}

//********************************************************
//* Functions handling parameters
//********************************************************
/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetNumberOfParameters
 * Signature: (J)I
 */
jint JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetNumberOfParameters
  (JNIEnv *, jclass, jlong _typeId)
{
    return DotsC_GetNumberOfParameters(_typeId);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetParameterId
 * Signature: (JLjava/lang/String;)I
 */
jint JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetParameterId
  (JNIEnv * env, jclass, jlong _typeId, jstring _parameterName)
{
    return DotsC_GetParameterId(_typeId, GetUtf8(env,_parameterName).get());
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetParameterInfo
 * Signature: (JI[I[Ljava/lang/String;[J[I[I)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetParameterInfo
  (JNIEnv * env, jclass, jlong _typeId, jint _parameter, jintArray _memberType, jobjectArray _parameterName, jlongArray _complexType, jintArray _collectionType, jintArray _numberOfValues)
{
    DotsC_MemberType memberType;
    const char* parameterName;
    DotsC_TypeId complexTypeId;
    DotsC_CollectionType collectionType;
    DotsC_Int32 numberOfValues;

    DotsC_GetParameterInfo(_typeId, _parameter, memberType, parameterName, complexTypeId, collectionType, numberOfValues);

    SetJArray(env,_memberType, memberType);
    SetJArray(env,_parameterName, env->NewStringUTF(parameterName));
    SetJArray(env,_complexType, complexTypeId);
    SetJArray(env,_collectionType, collectionType);
    SetJArray(env,_numberOfValues, numberOfValues);
}

//************************************************************************************
//* Type compatibility
//************************************************************************************
/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    IsOfType
 * Signature: (JJ)Z
 */
jboolean JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_IsOfType
  (JNIEnv *, jclass, jlong _type, jlong _ofType)
{
    return DotsC_IsOfType(_type, _ofType);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetCompleteType
 * Signature: (J)[J
 */
jlongArray JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetCompleteType
  (JNIEnv * env, jclass, jlong _rootClass)
{
    std::vector<DotsC_TypeId> typeIds(DotsC_NumberOfTypeIds());
    DotsC_Int32 resultSize;
    DotsC_GetCompleteType(_rootClass, &typeIds[0], static_cast<DotsC_Int32>(typeIds.size()), resultSize);

    jlongArray result = env->NewLongArray(resultSize);
    env->SetLongArrayRegion(result,0,resultSize,&typeIds[0]);
    return result;
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetParentType
 * Signature: (J)J
 */
jlong JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetParentType
  (JNIEnv *, jclass, jlong _typeId)
{
    return DotsC_GetParentType(_typeId);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    HasProperty
 * Signature: (JJ[Z[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_HasProperty
  (JNIEnv * env, jclass, jlong _classTypeId, jlong _propertyTypeId, jbooleanArray _hasProperty, jbooleanArray _isInherited)
{
    bool hasProperty, isInherited;
    DotsC_HasProperty(_classTypeId,_propertyTypeId,hasProperty, isInherited);
    SetJArray(env,_hasProperty,hasProperty);
    SetJArray(env,_isInherited,isInherited);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetPropertyMappingKind
 * Signature: (JJI)I
 */
jint JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetPropertyMappingKind
  (JNIEnv *, jclass, jlong _classTypeId, jlong _propertyTypeId, jint _propertyMember)
{
    DotsC_PropertyMappingKind mappingKind;
    DotsC_GetPropertyMappingKind(_classTypeId, _propertyTypeId, _propertyMember, mappingKind);
    return static_cast<jint>(mappingKind);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetClassMemberReference
 * Signature: (JJI[Ljava/nio/ByteBuffer;)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetClassMemberReference
  (JNIEnv * env, jclass, jlong _typeId, jlong _propertyId, jint _member, jobjectArray _classMemberReference)
{
    const DotsC_Int32 * classMemberReference;
    DotsC_Int32 classMemberReferenceSize;
    DotsC_GetClassMemberReference(_typeId,_propertyId,_member,classMemberReference, classMemberReferenceSize);
    SetJArray(env,_classMemberReference, env->NewDirectByteBuffer((void*)classMemberReference,classMemberReferenceSize*sizeof(DotsC_Int32)));
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetPropertyParameterReference
 * Signature: (JJII[I[I)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetPropertyParameterReference
  (JNIEnv * env, jclass, jlong _typeId, jlong _propertyId, jint _member, jint _index, jintArray _paramId, jintArray _paramValueIndex)
{
    DotsC_ParameterIndex paramId;
    DotsC_Int32 paramValueIndex;

    DotsC_GetPropertyParameterReference(_typeId, _propertyId, _member, _index, paramId, paramValueIndex);

    SetJArray(env,_paramId, paramId);
    SetJArray(env,_paramValueIndex, paramValueIndex);
}

//************************************************************************************
//* Serialization
//************************************************************************************
/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    BlobToXml
 * Signature: (Ljava/nio/ByteBuffer;)Ljava/lang/String;
 */
jstring JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_BlobToXml
  (JNIEnv * env, jclass, jobject _blob)
{
    char * blob = static_cast<char*>(env->GetDirectBufferAddress(_blob));

    int BUF_SIZE = 100000;
    std::vector<char> xml8(BUF_SIZE);
    DotsC_Int32 resultSize;
    DotsC_BlobToXml(&xml8[0], blob, BUF_SIZE, resultSize);
    if (resultSize> BUF_SIZE)
    {
        BUF_SIZE = resultSize;
        xml8.resize(BUF_SIZE);
        DotsC_BlobToXml(&xml8[0], blob, BUF_SIZE, resultSize);
        if (resultSize != BUF_SIZE)
        {
            return NULL;
        }
    }
    return env->NewStringUTF(&xml8[0]);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    XmlToBlob
 * Signature: ([Ljava/nio/ByteBuffer;[Ljava/nio/ByteBuffer;Ljava/lang/String;)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_XmlToBlob
  (JNIEnv * env, jclass, jobjectArray _blob, jobjectArray _deleter, jstring _xmlSource)
{
    char * blob;
    DotsC_BytePointerDeleter deleter;
    DotsC_XmlToBlob(blob,deleter,GetUtf8(env,_xmlSource).get());
    if (blob != NULL)
    {
        SetJArray(env,_blob,env->NewDirectByteBuffer(blob,DotsC_GetSize(blob)));
    }
    else
    {
        SetJArray(env,_blob,NULL);
    }
    SetJArray(env,_deleter,env->NewDirectByteBuffer((void*)deleter,0)); //just passing a pointer!
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    BlobToJson
 * Signature: (Ljava/nio/ByteBuffer;)Ljava/lang/String;
 */
jstring JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_BlobToJson
  (JNIEnv * env, jclass, jobject _blob)
{
    char * blob = static_cast<char*>(env->GetDirectBufferAddress(_blob));

    int BUF_SIZE = 100000;
    std::vector<char> json8(BUF_SIZE);
    DotsC_Int32 resultSize;
    DotsC_BlobToJson(&json8[0], blob, BUF_SIZE, resultSize);
    if (resultSize> BUF_SIZE)
    {
        BUF_SIZE = resultSize;
        json8.resize(BUF_SIZE);
        DotsC_BlobToJson(&json8[0], blob, BUF_SIZE, resultSize);
        if (resultSize != BUF_SIZE)
        {
            return NULL;
        }
    }
    return env->NewStringUTF(&json8[0]);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    JsonToBlob
 * Signature: ([Ljava/nio/ByteBuffer;[Ljava/nio/ByteBuffer;Ljava/lang/String;)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_JsonToBlob
  (JNIEnv * env, jclass, jobjectArray _blob, jobjectArray _deleter, jstring _jsonSource)
{
    char * blob;
    DotsC_BytePointerDeleter deleter;
    DotsC_JsonToBlob(blob,deleter,GetUtf8(env,_jsonSource).get());
    if (blob != NULL)
    {
        SetJArray(env,_blob,env->NewDirectByteBuffer(blob,DotsC_GetSize(blob)));
    }
    else
    {
        SetJArray(env,_blob,NULL);
    }
    SetJArray(env,_deleter,env->NewDirectByteBuffer((void*)deleter,0)); //just passing a pointer!
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    InvokeDeleter
 * Signature: (Ljava/nio/ByteBuffer;Ljava/nio/ByteBuffer;)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_InvokeDeleter
  (JNIEnv * env, jclass, jobject _deleter, jobject _toDelete)
{
    DotsC_BytePointerDeleter deleter = (DotsC_BytePointerDeleter)(env->GetDirectBufferAddress(_deleter));
    char * toDelete = static_cast<char*>(env->GetDirectBufferAddress(_toDelete));
    deleter(toDelete);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    BinaryToBase64
 * Signature: (Ljava/nio/ByteBuffer;I)Ljava/lang/String;
 */
jstring JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_BinaryToBase64
(JNIEnv * env, jclass, jobject _binary, jint _size)
{
    char * binary = static_cast<char*>(env->GetDirectBufferAddress(_binary));
    DotsC_Int32 resultSize=0;
    DotsC_Int32 size = DotsC_CalculateBase64BufferSize(_size);
    std::vector<char> buf(size+1); //one extra for '0'
    DotsC_BinaryToBase64(&buf[0], size, binary, _size, resultSize);
    buf.back() = '\0';
    return env->NewStringUTF(&buf[0]);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    Base64ToBinary
 * Signature: (Ljava/lang/String;)[B
 */
jbyteArray JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_Base64ToBinary
  (JNIEnv * env, jclass, jstring _base64Source)
{
    const StringHolder source = GetUtf8(env,_base64Source);
    const DotsC_Int32 sourceSize = static_cast<DotsC_Int32>(strlen(source.get()));
    DotsC_Int32 resultSize=0;
    DotsC_Int32 size = DotsC_CalculateBinaryBufferSize(sourceSize);
    std::vector<char> binary (size);
    if (size != 0)
    {
        DotsC_Base64ToBinary(&binary[0], size, source.get(), sourceSize, resultSize);
    }
    jbyteArray result = env->NewByteArray(resultSize);
    env->SetByteArrayRegion(result,0,resultSize,(jbyte*)&binary[0]);
    return result;
}

//************************************************************************************
//* Functions for retrieval of parameters
//************************************************************************************
/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetBooleanParameter
 * Signature: (JII[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetBooleanParameter
  (JNIEnv * env, jclass, jlong _typeId, jint _parameter, jint _index, jbooleanArray _val)
{
    bool val;
    DotsC_GetBooleanParameter(_typeId,_parameter,_index,val);
    SetJArray(env,_val,val);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetEnumerationParameter
 * Signature: (JIII[I)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetEnumerationParameter
  (JNIEnv * env, jclass, jlong _typeId, jint _parameter, jint _index, jint _keyValMode, jintArray _val)
{
    DotsC_Int32 val;
    DotsC_GetEnumerationParameter(_typeId,_parameter,_index, static_cast<DotsC_KeyValMode>(_keyValMode), val);
    SetJArray(env,_val,val);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetInt32Parameter
 * Signature: (JIII[I)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetInt32Parameter
  (JNIEnv * env, jclass, jlong _typeId, jint _parameter, jint _index, jint _keyValMode, jintArray _val)
{
    DotsC_Int32 val;
    DotsC_GetInt32Parameter(_typeId,_parameter,_index, static_cast<DotsC_KeyValMode>(_keyValMode), val);
    SetJArray(env,_val,val);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetInt64Parameter
 * Signature: (JIII[J)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetInt64Parameter
    (JNIEnv * env, jclass, jlong _typeId, jint _parameter, jint _index, jint _keyValMode, jlongArray _val)
{
    DotsC_Int64 val;
    DotsC_GetInt64Parameter(_typeId,_parameter,_index, static_cast<DotsC_KeyValMode>(_keyValMode), val);
    SetJArray(env,_val,val);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetFloat32Parameter
 * Signature: (JII[F)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetFloat32Parameter
  (JNIEnv * env, jclass, jlong _typeId, jint _parameter, jint _index, jfloatArray _val)
{
    DotsC_Float32 val;
    DotsC_GetFloat32Parameter(_typeId,_parameter,_index,val);
    SetJArray(env,_val,val);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetFloat64Parameter
 * Signature: (JII[D)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetFloat64Parameter
  (JNIEnv * env, jclass, jlong _typeId, jint _parameter, jint _index, jdoubleArray _val)
{
    DotsC_Float64 val;
    DotsC_GetFloat64Parameter(_typeId,_parameter,_index,val);
    SetJArray(env,_val,val);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetStringParameter
 * Signature: (JIII[Ljava/lang/String;)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetStringParameter
  (JNIEnv * env, jclass, jlong _typeId, jint _parameter, jint _index, jint _keyValMode, jobjectArray _val)
{
    const char * val;
    DotsC_GetStringParameter(_typeId,_parameter,_index, static_cast<DotsC_KeyValMode>(_keyValMode), val);
    if (val!=NULL)
    {
        SetJArray(env,_val,env->NewStringUTF(val));
    }
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetTypeIdParameter
 * Signature: (JIII[J)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetTypeIdParameter
  (JNIEnv * env, jclass, jlong _typeId, jint _parameter, jint _index, jint _keyValMode, jlongArray _val)
{
    DotsC_Int64 val;
    DotsC_GetTypeIdParameter(_typeId,_parameter,_index, static_cast<DotsC_KeyValMode>(_keyValMode), val);
    SetJArray(env,_val,val);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetHashedIdParameter
 * Signature: (JIII[J[Ljava/lang/String;)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetHashedIdParameter
  (JNIEnv * env, jclass, jlong _typeId, jint _parameter, jint _index, jint _keyValMode, jlongArray _hashVal, jobjectArray _strVal)
{
    DotsC_Int64 hashVal;
    const char * strVal;
    DotsC_GetHashedIdParameter(_typeId, _parameter, _index, static_cast<DotsC_KeyValMode>(_keyValMode), hashVal, strVal);
    SetJArray(env,_hashVal,hashVal);
    if (strVal!=NULL)
    {
        SetJArray(env,_strVal,env->NewStringUTF(strVal));
    }
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetEntityIdParameter
 * Signature: (JIII[J[J[Ljava/lang/String;)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetEntityIdParameter
  (JNIEnv * env, jclass, jlong _typeId, jint _parameter, jint _index, jint _keyValMode, jlongArray _typeIdOut, jlongArray _instanceId, jobjectArray _strVal)
{
    DotsC_EntityId eid;
    const char * str;
    DotsC_GetEntityIdParameter(_typeId, _parameter, _index, static_cast<DotsC_KeyValMode>(_keyValMode), eid, str);
    SetJArray(env,_typeIdOut, eid.typeId);
    SetJArray(env,_instanceId, eid.instanceId);
    if (str!=NULL)
    {
        SetJArray(env,_strVal,env->NewStringUTF(str));
    }
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetObjectParameter
 * Signature: (JII[Ljava/nio/ByteBuffer;)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetObjectParameter
  (JNIEnv * env, jclass, jlong _typeId, jint _parameter, jint _index, jobjectArray _blob)
{
    const char * blob;
    DotsC_GetObjectParameter(_typeId,_parameter,_index,blob);
    SetJArray(env,_blob,env->NewDirectByteBuffer((void*)blob,DotsC_GetSize(blob)));
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetBinaryParameter
 * Signature: (JII[Ljava/nio/ByteBuffer;)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetBinaryParameter
  (JNIEnv * env, jclass, jlong _typeId, jint _parameter, jint _index, jobjectArray _val)
{
    const char * val;
    DotsC_Int32 size;
    DotsC_GetBinaryParameter(_typeId,_parameter,_index,val,size);
    SetJArray(env,_val,env->NewDirectByteBuffer((void*)val,size));
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    DictionaryInt32KeyToIndex
 * Signature: (JII)I
 */
jint JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_DictionaryInt32KeyToIndex
  (JNIEnv *, jclass, jlong _typeId, jint _parameter, jint _key)
{
    return DotsC_DictionaryInt32KeyToIndex(_typeId, _parameter, _key);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    DictionaryInt64KeyToIndex
 * Signature: (JIJ)I
 */
jint JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_DictionaryInt64KeyToIndex
  (JNIEnv *, jclass, jlong _typeId, jint _parameter, jlong _key)
{
    return DotsC_DictionaryInt64KeyToIndex(_typeId, _parameter, _key);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    DictionaryStringKeyToIndex
 * Signature: (JILjava/lang/String;)I
 */
jint JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_DictionaryStringKeyToIndex
  (JNIEnv * env, jclass, jlong _typeId, jint _parameter, jstring _key)
{
    return DotsC_DictionaryStringKeyToIndex(_typeId, _parameter, GetUtf8(env, _key).get());
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    DictionaryEntityIdKeyToIndex
 * Signature: (JIJJ)I
 */
jint JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_DictionaryEntityIdKeyToIndex
  (JNIEnv *, jclass, jlong _typeId, jint _parameter, jlong _keyTypeId, jlong _keyInstId)
{
    DotsC_EntityId eid={_keyTypeId, _keyInstId};
    return DotsC_DictionaryEntityIdKeyToIndex(_typeId, _parameter, eid);
}

//********************************************************
//* Operations on blobs
//********************************************************
/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetTypeId
 * Signature: (Ljava/nio/ByteBuffer;)J
 */
jlong JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetTypeId
  (JNIEnv * env, jclass, jobject _blob)
{
    char * blob = static_cast<char*>(env->GetDirectBufferAddress(_blob));
    return DotsC_GetTypeId(blob);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetSize
 * Signature: (Ljava/nio/ByteBuffer;)I
 */
jint JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetSize
  (JNIEnv * env, jclass, jobject _blob)
{
    char * blob = static_cast<char*>(env->GetDirectBufferAddress(_blob));
    return DotsC_GetSize(blob);
}

//************************************************************************************
//* Read operations
//************************************************************************************
/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    CreateBlobReader
 * Signature: (Ljava/nio/ByteBuffer;)J
 */
jlong JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_CreateBlobReader
  (JNIEnv * env, jclass, jobject _blob)
{
    const char* blob = static_cast<char*>(env->GetDirectBufferAddress(_blob));
    return DotsC_CreateBlobReader(blob);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    DeleteBlobReader
 * Signature: (J)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_DeleteBlobReader
  (JNIEnv *, jclass, jlong _handle)
{
    DotsC_DeleteBlobReader(_handle);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetNumberOfMemberValues
 * Signature: (JI)I
 */
jint JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetNumberOfMemberValues
  (JNIEnv *, jclass, jlong _readerHandle, jint _member)
{
    return DotsC_GetNumberOfMemberValues(_readerHandle, _member);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    ReadMemberStatus
 * Signature: (J[Z[ZII)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_ReadMemberStatus
  (JNIEnv * env, jclass, jlong _readerHandle, jbooleanArray _isNull, jbooleanArray _isChanged, jint _member, jint _valueIndex)
{
    bool isNull, isChanged;
    DotsC_ReadMemberStatus(_readerHandle, isNull, isChanged, _member, _valueIndex);
    SetJArray(env, _isNull, isNull);
    SetJArray(env, _isChanged, isChanged);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    ReadInt32Member
 * Signature: (J[I[Z[ZIII)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_ReadInt32Member
  (JNIEnv * env, jclass, jlong _readHandle, jintArray _val, jbooleanArray _isNull, jbooleanArray _isChanged, jint _member, jint _index, jint _keyValMode)
{
    DotsC_Int32 val;
    bool isNull, isChanged;
    DotsC_ReadInt32Member(_readHandle, val, isNull, isChanged, _member, _index, static_cast<DotsC_KeyValMode>(_keyValMode));

    SetJArray(env, _val, val);
    SetJArray(env, _isNull, isNull);
    SetJArray(env, _isChanged, isChanged);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    ReadInt64Member
 * Signature: (J[J[Z[ZIII)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_ReadInt64Member
    (JNIEnv * env, jclass, jlong _readHandle, jlongArray _val, jbooleanArray _isNull, jbooleanArray _isChanged, jint _member, jint _index, jint _keyValMode)
{
    DotsC_Int64 val;
    bool isNull, isChanged;
    DotsC_ReadInt64Member(_readHandle, val, isNull, isChanged, _member, _index, static_cast<DotsC_KeyValMode>(_keyValMode));

    SetJArray(env, _val, val);
    SetJArray(env, _isNull, isNull);
    SetJArray(env, _isChanged, isChanged);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    ReadFloat32Member
 * Signature: (J[F[Z[ZIII)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_ReadFloat32Member
    (JNIEnv * env, jclass, jlong _readHandle, jfloatArray _val, jbooleanArray _isNull, jbooleanArray _isChanged, jint _member, jint _index, jint _keyValMode)
{
    DotsC_Float32 val;
    bool isNull, isChanged;
    DotsC_ReadFloat32Member(_readHandle, val, isNull, isChanged, _member, _index, static_cast<DotsC_KeyValMode>(_keyValMode));

    SetJArray(env, _val, val);
    SetJArray(env, _isNull, isNull);
    SetJArray(env, _isChanged, isChanged);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    ReadFloat64Member
 * Signature: (J[D[Z[ZIII)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_ReadFloat64Member
    (JNIEnv * env, jclass, jlong _readHandle, jdoubleArray _val, jbooleanArray _isNull, jbooleanArray _isChanged, jint _member, jint _index, jint _keyValMode)
{
    DotsC_Float64 val;
    bool isNull, isChanged;
    DotsC_ReadFloat64Member(_readHandle, val, isNull, isChanged, _member, _index, static_cast<DotsC_KeyValMode>(_keyValMode));

    SetJArray(env, _val, val);
    SetJArray(env, _isNull, isNull);
    SetJArray(env, _isChanged, isChanged);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    ReadBooleanMember
 * Signature: (J[Z[Z[ZIII)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_ReadBooleanMember
    (JNIEnv * env, jclass, jlong _readHandle, jbooleanArray _val, jbooleanArray _isNull, jbooleanArray _isChanged, jint _member, jint _index, jint _keyValMode)
{
    bool val;
    bool isNull, isChanged;
    DotsC_ReadBooleanMember(_readHandle, val, isNull, isChanged, _member, _index, static_cast<DotsC_KeyValMode>(_keyValMode));

    SetJArray(env, _val, val);
    SetJArray(env, _isNull, isNull);
    SetJArray(env, _isChanged, isChanged);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    ReadStringMember
 * Signature: (J[Ljava/lang/String;[Z[ZIII)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_ReadStringMember
    (JNIEnv * env, jclass, jlong _readHandle, jobjectArray _val, jbooleanArray _isNull, jbooleanArray _isChanged, jint _member, jint _index, jint _keyValMode)
{
    const char* val;
    bool isNull, isChanged;
    DotsC_ReadStringMember(_readHandle, val, isNull, isChanged, _member, _index, static_cast<DotsC_KeyValMode>(_keyValMode));

    if (!isNull)
    {
        SetJArray(env,_val, env->NewStringUTF(val));
    }
    SetJArray(env, _isNull, isNull);
    SetJArray(env, _isChanged, isChanged);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    ReadHashedMember
 * Signature: (J[J[Ljava/lang/String;[Z[ZIII)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_ReadHashedMember
    (JNIEnv * env, jclass, jlong _readHandle, jlongArray _valHash, jobjectArray _valStr, jbooleanArray _isNull, jbooleanArray _isChanged, jint _member, jint _index, jint _keyValMode)
{
    DotsC_Int64 hash;
    const char* str;
    bool isNull, isChanged;
    DotsC_ReadHashedMember(_readHandle, hash, str, isNull, isChanged, _member, _index, static_cast<DotsC_KeyValMode>(_keyValMode));

    SetJArray(env,_valHash, hash);
    if (str!=NULL)
    {
        SetJArray(env,_valStr, env->NewStringUTF(str));
    }
    SetJArray(env, _isNull, isNull);
    SetJArray(env, _isChanged, isChanged);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    ReadEntityIdMember
 * Signature: (J[J[J[Ljava/lang/String;[Z[ZIII)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_ReadEntityIdMember
    (JNIEnv * env, jclass, jlong _readHandle, jlongArray _valTypeId, jlongArray _valHash, jobjectArray _valStr, jbooleanArray _isNull, jbooleanArray _isChanged, jint _member, jint _index, jint _keyValMode)
{
    DotsC_EntityId eid;
    const char* str;
    bool isNull, isChanged;
    DotsC_ReadEntityIdMember(_readHandle, eid, str, isNull, isChanged, _member, _index, static_cast<DotsC_KeyValMode>(_keyValMode));

    SetJArray(env,_valTypeId, eid.typeId);
    SetJArray(env,_valHash, eid.instanceId);
    if (str!=NULL)
    {
        SetJArray(env,_valStr, env->NewStringUTF(str));
    }
    SetJArray(env, _isNull, isNull);
    SetJArray(env, _isChanged, isChanged);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    ReadBinaryMember
 * Signature: (J[Ljava/nio/ByteBuffer;[Z[ZIII)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_ReadBinaryMember
    (JNIEnv * env, jclass, jlong _readHandle, jobjectArray _val, jbooleanArray _isNull, jbooleanArray _isChanged, jint _member, jint _index, jint _keyValMode)
{
    const char* val;
    DotsC_Int32 size;
    bool isNull, isChanged;
    DotsC_ReadBinaryMember(_readHandle, val, size, isNull, isChanged, _member, _index, static_cast<DotsC_KeyValMode>(_keyValMode));

    if (!isNull)
    {
        SetJArray(env, _val, env->NewDirectByteBuffer((void*)val, size));
    }
    SetJArray(env, _isNull, isNull);
    SetJArray(env, _isChanged, isChanged);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    ReadObjectMember
 * Signature: (J[Ljava/nio/ByteBuffer;[Z[ZIII)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_ReadObjectMember
    (JNIEnv * env, jclass, jlong _readHandle, jobjectArray _blob, jbooleanArray _isNull, jbooleanArray _isChanged, jint _member, jint _index, jint _keyValMode)
{
    const char* blob;
    bool isNull, isChanged;
    DotsC_ReadObjectMember(_readHandle, blob, isNull, isChanged, _member, _index, static_cast<DotsC_KeyValMode>(_keyValMode));

    if (!isNull)
    {
        SetJArray(env, _blob, env->NewDirectByteBuffer((void*)blob, DotsC_GetSize(blob)));
    }
    SetJArray(env, _isNull, isNull);
    SetJArray(env, _isChanged, isChanged);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    CreateBlobWriter
 * Signature: (J)J
 */
jlong JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_CreateBlobWriter
  (JNIEnv *, jclass, jlong _typeId)
{
    DotsC_Int64 handle=DotsC_CreateBlobWriter(_typeId);
    return handle;


    //return DotsC_CreateBlobWriter(_typeId);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    CreateBlobWriterFromBlob
 * Signature: (Ljava/nio/ByteBuffer;)J
 */
jlong JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_CreateBlobWriterFromBlob
  (JNIEnv * env, jclass, jobject _blob)
{
    const char * blob = static_cast<char*>(env->GetDirectBufferAddress(_blob));
    return DotsC_CreateBlobWriterFromBlob(blob);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    CreateBlobWriterFromReader
 * Signature: (J)J
 */
jlong JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_CreateBlobWriterFromReader
  (JNIEnv *, jclass, jlong _readerHandle)
{
    return DotsC_CreateBlobWriterFromReader(_readerHandle);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    DeleteBlobWriter
 * Signature: (J)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_DeleteBlobWriter
  (JNIEnv *, jclass, jlong _writerHandle)
{
    DotsC_DeleteBlobWriter(_writerHandle);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    CalculateBlobSize
 * Signature: (J)I
 */
jint JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_CalculateBlobSize
  (JNIEnv *, jclass, jlong _writerHandle)
{
    return DotsC_CalculateBlobSize(_writerHandle);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    WriteBlob
 * Signature: (JLjava/nio/ByteBuffer;)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_WriteBlob
  (JNIEnv * env, jclass, jlong _writerHandle, jobject _blob)
{
    char * blob = static_cast<char*>(env->GetDirectBufferAddress(_blob));
    DotsC_WriteBlob(_writerHandle, blob);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    WriteAllChangeFlags
 * Signature: (JZ)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_WriteAllChangeFlags
  (JNIEnv *, jclass, jlong _writerHandle, jboolean _changed)
{
    DotsC_WriteAllChangeFlags(_writerHandle, _changed == JNI_TRUE);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    WriteChangeFlag
 * Signature: (JIIZ)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_WriteChangeFlag
  (JNIEnv *, jclass, jlong _writerHandle, jint _member, jint _index, jboolean _changed)
{
    DotsC_WriteChangeFlag(_writerHandle, _member, _index, _changed == JNI_TRUE);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    MarkChanges
 * Signature: (JJ)Z
 */
jboolean JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_MarkChanges
  (JNIEnv *, jclass, jlong _originalWriter, jlong _currentWriter)
{
    return DotsC_MarkChanges(_originalWriter, _currentWriter);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    WriteInt32Member
 * Signature: (JIZZIII)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_WriteInt32Member
  (JNIEnv *, jclass, jlong _writerHandle, jint _val, jboolean _isNull, jboolean _isChanged, jint _member, jint _arrayIndex, jint _keyValMode)
{
    DotsC_WriteInt32Member(_writerHandle, _val, _isNull == JNI_TRUE, _isChanged == JNI_TRUE, _member, _arrayIndex, static_cast<DotsC_KeyValMode>(_keyValMode));
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    WriteInt64Member
 * Signature: (JJZZIII)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_WriteInt64Member
    (JNIEnv *, jclass, jlong _writerHandle, jlong _val, jboolean _isNull, jboolean _isChanged, jint _member, jint _arrayIndex, jint _keyValMode)
{
  DotsC_WriteInt64Member(_writerHandle, _val, _isNull == JNI_TRUE, _isChanged == JNI_TRUE, _member, _arrayIndex, static_cast<DotsC_KeyValMode>(_keyValMode));
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    WriteFloat32Member
 * Signature: (JFZZIII)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_WriteFloat32Member
    (JNIEnv *, jclass, jlong _writerHandle, jfloat _val, jboolean _isNull, jboolean _isChanged, jint _member, jint _arrayIndex, jint _keyValMode)
{
    DotsC_WriteFloat32Member(_writerHandle, _val, _isNull == JNI_TRUE, _isChanged == JNI_TRUE, _member, _arrayIndex, static_cast<DotsC_KeyValMode>(_keyValMode));
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    WriteFloat64Member
 * Signature: (JDZZIII)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_WriteFloat64Member
    (JNIEnv *, jclass, jlong _writerHandle, jdouble _val, jboolean _isNull, jboolean _isChanged, jint _member, jint _arrayIndex, jint _keyValMode)
{
    DotsC_WriteFloat64Member(_writerHandle, _val, _isNull == JNI_TRUE, _isChanged == JNI_TRUE, _member, _arrayIndex, static_cast<DotsC_KeyValMode>(_keyValMode));
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    WriteBooleanMember
 * Signature: (JZZZIII)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_WriteBooleanMember
    (JNIEnv *, jclass, jlong _writerHandle, jboolean _val, jboolean _isNull, jboolean _isChanged, jint _member, jint _arrayIndex, jint _keyValMode)
{
    DotsC_WriteBooleanMember(_writerHandle, _val == JNI_TRUE, _isNull == JNI_TRUE, _isChanged == JNI_TRUE, _member, _arrayIndex, static_cast<DotsC_KeyValMode>(_keyValMode));
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    WriteStringMember
 * Signature: (JLjava/lang/String;ZZIII)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_WriteStringMember
    (JNIEnv * env, jclass, jlong _writerHandle, jstring _val, jboolean _isNull, jboolean _isChanged, jint _member, jint _arrayIndex, jint _keyValMode)
{
    if (!_isNull)
    {
        StringHolder str=GetUtf8(env, _val);
        DotsC_WriteStringMember(_writerHandle, str.get(), _isNull == JNI_TRUE, _isChanged == JNI_TRUE, _member, _arrayIndex, static_cast<DotsC_KeyValMode>(_keyValMode));
    }
    else
    {
        DotsC_WriteStringMember(_writerHandle, NULL, _isNull == JNI_TRUE, _isChanged == JNI_TRUE, _member, _arrayIndex, static_cast<DotsC_KeyValMode>(_keyValMode));
    }
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    WriteHashedMember
 * Signature: (JJLjava/lang/String;ZZIII)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_WriteHashedMember
    (JNIEnv * env, jclass, jlong _writerHandle, jlong _valHash, jstring _valStr, jboolean _isNull, jboolean _isChanged, jint _member, jint _arrayIndex, jint _keyValMode)
{
    if (_valStr)
    {
        StringHolder str=GetUtf8(env, _valStr);
        DotsC_WriteHashedMember(_writerHandle, _valHash, str.get(), _isNull == JNI_TRUE, _isChanged == JNI_TRUE, _member, _arrayIndex, static_cast<DotsC_KeyValMode>(_keyValMode));
    }
    else
    {
        DotsC_WriteHashedMember(_writerHandle, _valHash, NULL, _isNull == JNI_TRUE, _isChanged == JNI_TRUE, _member, _arrayIndex, static_cast<DotsC_KeyValMode>(_keyValMode));
    }
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    WriteEntityIdMember
 * Signature: (JJJLjava/lang/String;ZZIII)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_WriteEntityIdMember
    (JNIEnv * env, jclass, jlong _writerHandle, jlong _valTypeId, jlong _valInst, jstring _valStr, jboolean _isNull, jboolean _isChanged, jint _member, jint _arrayIndex, jint _keyValMode)
{
    DotsC_EntityId eid={_valTypeId, _valInst};
    if (_valStr)
    {
        StringHolder str=GetUtf8(env, _valStr);
        DotsC_WriteEntityIdMember(_writerHandle, eid, str.get(), _isNull == JNI_TRUE, _isChanged == JNI_TRUE, _member, _arrayIndex, static_cast<DotsC_KeyValMode>(_keyValMode));
    }
    else
    {
        DotsC_WriteEntityIdMember(_writerHandle, eid, NULL, _isNull == JNI_TRUE, _isChanged == JNI_TRUE, _member, _arrayIndex, static_cast<DotsC_KeyValMode>(_keyValMode));
    }
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    WriteBinaryMember
 * Signature: (JLjava/nio/ByteBuffer;IZZIII)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_WriteBinaryMember
    (JNIEnv * env, jclass, jlong _writerHandle, jobject _val, jint _size, jboolean _isNull, jboolean _isChanged, jint _member, jint _arrayIndex, jint _keyValMode)
{
    const char * bin = _isNull ? NULL : static_cast<char*>(env->GetDirectBufferAddress(_val));
    DotsC_WriteBinaryMember(_writerHandle, bin, _size, _isNull == JNI_TRUE, _isChanged == JNI_TRUE, _member, _arrayIndex, static_cast<DotsC_KeyValMode>(_keyValMode));
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    WriteObjectMember
 * Signature: (JLjava/nio/ByteBuffer;ZZIII)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_WriteObjectMember
(JNIEnv * env, jclass, jlong _writerHandle, jobject _blob, jboolean _isNull, jboolean _isChanged, jint _member, jint _arrayIndex, jint _keyValMode)
{
    const char * blob = _isNull ? NULL : static_cast<char*>(env->GetDirectBufferAddress(_blob));
    DotsC_WriteObjectMember(_writerHandle, blob, _isNull == JNI_TRUE, _isChanged == JNI_TRUE, _member, _arrayIndex, static_cast<DotsC_KeyValMode>(_keyValMode));
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetAndClearException
 * Signature: ([J[Ljava/lang/String;[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetAndClearException
  (JNIEnv * env, jclass, jlongArray _exceptionId, jobjectArray _description, jbooleanArray _wasSet)
{
    DotsC_TypeId exceptionId;
    char* description;
    DotsC_BytePointerDeleter deleter;
    bool wasSet;
    DotsC_GetAndClearException(exceptionId,description,deleter,wasSet);
    SetJArray(env,_wasSet,wasSet);
    if (wasSet)
    {
        SetJArray(env,_exceptionId,exceptionId);
        SetJArray(env,_description,env->NewStringUTF(description));
        deleter(description);
    }
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    SetException
 * Signature: (JLjava/lang/String;)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_SetException
  (JNIEnv * env, jclass, jlong _exceptionId, jstring _description)
{
    DotsC_SetException(_exceptionId, GetUtf8(env,_description).get());
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetDouDirectories
 * Signature: ()[Ljava/lang/String;
 */
JNIEXPORT jobjectArray JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetDouDirectories
  (JNIEnv * env, jclass)
{
    std::vector<std::pair<std::string,std::string> > directories;

    try
    {
        //Read the config files
        //these directories have already been checked  by dots_kernel by the time we get here
        //so we know that they exist.
        //the only reason that we may fail here is if we do not have read permissions on
        //the ini files...
        Safir::Utilities::Internal::ConfigReader reader;

        directories = Safir::Utilities::Internal::ConfigHelper::GetDouDirectories(reader);
    }
    catch (const std::exception& e)
    {
        std::wcerr << "Failed to read ini files!\nException:" <<
            e.what() << std::endl;
        exit(1);
    }

    jobjectArray stringArray = env->NewObjectArray(static_cast<jsize>(directories.size()),
                                                   env->FindClass("java/lang/String"),
                                                   env->NewStringUTF(""));
    for(size_t i = 0; i < directories.size(); ++i)
    {
        env->SetObjectArrayElement(stringArray,
                                   static_cast<jsize>(i),
                                   env->NewStringUTF(directories[i].second.c_str()));
    }
    return stringArray;
}

std::vector<std::string> GetJavaSearchPath()
{
    std::string param = Safir::Utilities::Internal::ConfigReader().Typesystem().
        get<std::string>("java_search_path");
    std::vector<std::string> javaSearchPath;
    boost::split(javaSearchPath,
                 param,
                 boost::is_any_of(","));
    return javaSearchPath;
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetGeneratedJars
 * Signature: ()[Ljava/lang/String;
 */
JNIEXPORT jobjectArray JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetGeneratedJars
  (JNIEnv * env, jclass)
{
    const std::vector<std::string> javaSearchPath = GetJavaSearchPath();

    std::vector<std::string> libraries;

    DotsC_GeneratedLibrary* generatedLibraries;
    DotsC_Int32 size;
    DotsC_GeneratedLibraryListDeleter deleter;

    DotsC_GetGeneratedLibraryList(generatedLibraries,
                                  size,
                                  deleter);
    if (size ==0)
    {
        std::wcerr << "Failed to read information from typesystem.ini" << std::endl;
        exit(1);
    }
    for (int i = 0; i < size; ++i)
    {
        if (generatedLibraries[i].library != 1)
        {
            continue;
        }

        boost::filesystem::path jarPath;

        if (generatedLibraries[i].javaJarLocation != NULL)
        {
            jarPath = generatedLibraries[i].javaJarLocation;
            jarPath /= generatedLibraries[i].javaJarName;
        }
        else
        {
            for (std::vector<std::string>::const_iterator it = javaSearchPath.begin();
                 it != javaSearchPath.end(); ++it)
            {
                using namespace boost::filesystem;
                const path p = path(*it) / generatedLibraries[i].javaJarName;
                if (is_regular_file(p))
                {
                    jarPath = p;
                    break;
                }
            }
        }

        if (!jarPath.empty())
        {
            libraries.push_back(jarPath.make_preferred().string());
        }
    }

    jobjectArray stringArray = env->NewObjectArray(static_cast<jsize>(libraries.size()),
                                                   env->FindClass("java/lang/String"),
                                                   env->NewStringUTF(""));
    for(size_t i = 0; i < libraries.size(); ++i)
    {
        env->SetObjectArrayElement(stringArray,
                                   static_cast<jsize>(i),
                                   env->NewStringUTF(libraries[i].c_str()));
    }
    return stringArray;
}
