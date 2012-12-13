/******************************************************************************
*
* Copyright Saab AB, 2009 (http://www.safirsdk.com)
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

#include <Safir/Dob/Typesystem/Internal/Kernel.h>
#include <Safir/Dob/Typesystem/Internal/Id.h>
#include "com_saabgroup_safir_dob_typesystem_Kernel.h"
#include <iostream>
#include <vector>
#include <assert.h>
#include <boost/shared_ptr.hpp>
#include <boost/scoped_array.hpp>
#include <boost/bind.hpp>
#include <string.h>

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

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    IsAnythingChanged
 * Signature: (Ljava/nio/ByteBuffer;)Z
 */
jboolean JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_IsAnythingChanged
  (JNIEnv * env, jclass, jobject _blob)
{
    char * blob = static_cast<char*>(env->GetDirectBufferAddress(_blob));
    return DotsC_IsAnythingChanged(blob);
}

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
 * Method:    GetEnumerationValueFromName
 * Signature: (JLjava/lang/String;)I
 */
jint JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetEnumerationValueFromName
  (JNIEnv * env, jclass, jlong _enumId, jstring _enumValueName)
{
    return DotsC_EnumerationValueFromName(_enumId,GetUtf8(env,_enumValueName).get());
}

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
 * Method:    GetMemberName
 * Signature: (JI)Ljava/lang/String;
 */
jstring JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetMemberName
  (JNIEnv * env, jclass, jlong _typeId, jint _member)
{
    const char* name=DotsC_GetMemberName(_typeId, _member);
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
 * Method:    GetComplexMemberTypeId
 * Signature: (JI)J
 */
jlong JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetComplexMemberTypeId
  (JNIEnv *, jclass, jlong _typeId, jint _member)
{
    return DotsC_GetComplexMemberTypeId(_typeId, _member);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetMemberInfo
 * Signature: (JI[I[Ljava/lang/String;[J[I[Z[I)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetMemberInfo
  (JNIEnv * env, jclass, jlong _typeId, jint _member, jintArray _memberType, jobjectArray _memberName, jlongArray _memberTypeId, jintArray _stringLength, jbooleanArray _isArray, jintArray _arrayLength)
{
    DotsC_MemberType memberType;
    const char * memberName;
    DotsC_TypeId complexType;
    DotsC_Int32 stringLength;
    bool isArray;
    DotsC_Int32 arrayLength;

    DotsC_GetMemberInfo(_typeId,
                        _member,
                        memberType,
                        memberName,
                        complexType,
                        stringLength,
                        isArray,
                        arrayLength);
    SetJArray(env,_memberType, memberType);
    SetJArray(env,_memberName, env->NewStringUTF(memberName));
    SetJArray(env,_memberTypeId, complexType);
    SetJArray(env,_stringLength, stringLength);
    SetJArray(env,_isArray, isArray);
    SetJArray(env,_arrayLength, arrayLength);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetMemberArraySize
 * Signature: (JI)I
 */
jint JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetMemberArraySize
  (JNIEnv *, jclass, jlong _typeId, jint _member)
{
    return DotsC_GetMemberArraySize(_typeId,_member);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetStringMemberMaxLength
 * Signature: (JI)I
 */
jint JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetStringMemberMaxLength
  (JNIEnv *, jclass, jlong _typeId, jint _member)
{
    return DotsC_GetStringMemberMaxLength(_typeId,_member);
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
 * Method:    GetMemberTypeName
 * Signature: (JI)Ljava/lang/String;
 */
jstring JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetMemberTypeName
  (JNIEnv * env, jclass, jlong _typeId, jint _member)
{
    const char* name=DotsC_GetMemberTypeName(_typeId, _member);
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
 * Method:    GetParameterName
 * Signature: (JI)Ljava/lang/String;
 */
jstring JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetParameterName
  (JNIEnv * env, jclass, jlong _typeId, jint _parameter)
{
    const char* name=DotsC_GetParameterName(_typeId, _parameter);
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
 * Method:    GetParameterType
 * Signature: (JI)I
 */
jint JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetParameterType
  (JNIEnv *, jclass, jlong _typeId, jint _parameter)
{
    return DotsC_GetParameterType(_typeId, _parameter);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetParameterTypeName
 * Signature: (JI)Ljava/lang/String;
 */
jstring JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetParameterTypeName
  (JNIEnv * env, jclass, jlong _typeId, jint _parameter)
{
    const char* name=DotsC_GetParameterTypeName(_typeId, _parameter);
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
 * Method:    GetParameterArraySize
 * Signature: (JI)I
 */
jint JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetParameterArraySize
  (JNIEnv *, jclass, jlong _typeId, jint _parameter)
{
    return DotsC_GetParameterArraySize(_typeId, _parameter);
}

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
    DotsC_BetterBlobToXml(&xml8[0], blob, BUF_SIZE, resultSize);
    if (resultSize> BUF_SIZE)
    {
        BUF_SIZE = resultSize;
        xml8.resize(BUF_SIZE);
        DotsC_BetterBlobToXml(&xml8[0], blob, BUF_SIZE, resultSize);
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
 * Method:    GetInt32Parameter
 * Signature: (JII[I)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetInt32Parameter
  (JNIEnv * env, jclass, jlong _typeId, jint _parameter , jint _index, jintArray _val)
{
    DotsC_Int32 val;
    DotsC_GetInt32Parameter(_typeId,_parameter,_index,val);
    SetJArray(env,_val,val);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetInt64Parameter
 * Signature: (JII[J)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetInt64Parameter
  (JNIEnv * env, jclass, jlong _typeId, jint _parameter, jint _index, jlongArray _val)
{
    DotsC_Int64 val;
    DotsC_GetInt64Parameter(_typeId,_parameter,_index,val);
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
 * Signature: (JII[Ljava/lang/String;)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetStringParameter
  (JNIEnv * env, jclass, jlong _typeId, jint _parameter, jint _index, jobjectArray _val)
{
    const char * val;
    DotsC_GetStringParameter(_typeId,_parameter,_index,val);
    SetJArray(env,_val,env->NewStringUTF(val));
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetHashedIdParameter
 * Signature: (JII[J[Ljava/lang/String;)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetHashedIdParameter
  (JNIEnv * env, jclass, jlong _typeId, jint _parameter, jint _index, jlongArray _hashVal, jobjectArray _strVal)
{
    DotsC_Int64 hashVal;
    const char * strVal;
    DotsC_GetHashedIdParameter(_typeId,_parameter,_index,hashVal,strVal);
    SetJArray(env,_hashVal,hashVal);
    SetJArray(env,_strVal,env->NewStringUTF(strVal));
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetEntityIdParameter
 * Signature: (JII[J[J[Ljava/lang/String;)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetEntityIdParameter
  (JNIEnv * env, jclass, jlong _typeId, jint _parameter, jint _index, jlongArray _typeIdOut, jlongArray _instanceId, jobjectArray _strVal)
{
    DotsC_EntityId eid;
    const char * str;
    DotsC_GetEntityIdParameter(_typeId,_parameter,_index,eid,str);
    SetJArray(env,_typeIdOut, eid.typeId);
    SetJArray(env,_instanceId, eid.instanceId);
    SetJArray(env,_strVal,env->NewStringUTF(str));
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
 * Method:    IsChangedMember
 * Signature: (Ljava/nio/ByteBuffer;II)Z
 */
jboolean JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_IsChangedMember
  (JNIEnv * env, jclass, jobject _blob, jint _member, jint _index)
{
    char * blob = static_cast<char*>(env->GetDirectBufferAddress(_blob));
    return DotsC_IsChangedMember(blob,_member,_index);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetBooleanMember
 * Signature: (Ljava/nio/ByteBuffer;II[Z[Z[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetBooleanMember
(JNIEnv * env, jclass, jobject _blob, jint _member, jint _index, jbooleanArray _val, jbooleanArray _isNull, jbooleanArray _isChanged)
{
    char * blob = static_cast<char*>(env->GetDirectBufferAddress(_blob));
    bool val;
    bool isNull, isChanged;
    DotsC_GetBooleanMember(blob,_member,_index,val,isNull,isChanged);
    if (!isNull)
    {
        SetJArray(env,_val,val);
    }
    SetJArray(env,_isNull,isNull);
    SetJArray(env,_isChanged,isChanged);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetInt32Member
 * Signature: (Ljava/nio/ByteBuffer;II[I[Z[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetInt32Member
  (JNIEnv * env, jclass, jobject _blob, jint _member, jint _index, jintArray _val, jbooleanArray _isNull, jbooleanArray _isChanged)
{
    char * blob = static_cast<char*>(env->GetDirectBufferAddress(_blob));
    DotsC_Int32 val;
    bool isNull, isChanged;
    DotsC_GetInt32Member(blob,_member,_index,val,isNull,isChanged);
    if (!isNull)
    {
        SetJArray(env,_val,val);
    }
    SetJArray(env,_isNull,isNull);
    SetJArray(env,_isChanged,isChanged);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetInt64Member
 * Signature: (Ljava/nio/ByteBuffer;II[J[Z[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetInt64Member
  (JNIEnv * env, jclass, jobject _blob, jint _member, jint _index, jlongArray _val, jbooleanArray _isNull, jbooleanArray _isChanged)
{
    char * blob = static_cast<char*>(env->GetDirectBufferAddress(_blob));
    DotsC_Int64 val;
    bool isNull, isChanged;
    DotsC_GetInt64Member(blob,_member,_index,val,isNull,isChanged);
    if (!isNull)
    {
        SetJArray(env,_val,val);
    }
    SetJArray(env,_isNull,isNull);
    SetJArray(env,_isChanged,isChanged);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetFloat32Member
 * Signature: (Ljava/nio/ByteBuffer;II[F[Z[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetFloat32Member
  (JNIEnv * env, jclass, jobject _blob, jint _member, jint _index, jfloatArray _val, jbooleanArray _isNull, jbooleanArray _isChanged)
{
    char * blob = static_cast<char*>(env->GetDirectBufferAddress(_blob));
    DotsC_Float32 val;
    bool isNull, isChanged;
    DotsC_GetFloat32Member(blob,_member,_index,val,isNull,isChanged);
    if (!isNull)
    {
        SetJArray(env,_val,val);
    }
    SetJArray(env,_isNull,isNull);
    SetJArray(env,_isChanged,isChanged);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetFloat64Member
 * Signature: (Ljava/nio/ByteBuffer;II[D[Z[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetFloat64Member
  (JNIEnv * env, jclass, jobject _blob, jint _member, jint _index, jdoubleArray _val, jbooleanArray _isNull, jbooleanArray _isChanged)
{
    char * blob = static_cast<char*>(env->GetDirectBufferAddress(_blob));
    DotsC_Float64 val;
    bool isNull, isChanged;
    DotsC_GetFloat64Member(blob,_member,_index,val,isNull,isChanged);
    if (!isNull)
    {
        SetJArray(env,_val,val);
    }
    SetJArray(env,_isNull,isNull);
    SetJArray(env,_isChanged,isChanged);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetStringMember
 * Signature: (Ljava/nio/ByteBuffer;II[Ljava/lang/String;[Z[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetStringMember
  (JNIEnv * env, jclass, jobject _blob, jint _member, jint _index, jobjectArray _val, jbooleanArray _isNull, jbooleanArray _isChanged)
{
    char * blob = static_cast<char*>(env->GetDirectBufferAddress(_blob));
    const char * val;
    bool isNull, isChanged;
    DotsC_GetStringMember(blob,_member,_index,val,isNull,isChanged);
    if (!isNull) {
        SetJArray(env,_val,env->NewStringUTF(val));
    }
    SetJArray(env,_isNull,isNull);
    SetJArray(env,_isChanged,isChanged);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetBinaryMember
 * Signature: (Ljava/nio/ByteBuffer;II[Ljava/nio/ByteBuffer;[Z[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetBinaryMember
  (JNIEnv * env, jclass, jobject _blob, jint _member, jint _index, jobjectArray _val, jbooleanArray _isNull, jbooleanArray _isChanged)
{
    char * blob = static_cast<char*>(env->GetDirectBufferAddress(_blob));
    const char * val;
    DotsC_Int32 size;
    bool isNull, isChanged;
    DotsC_GetBinaryMember(blob, _member, _index, val, size, isNull, isChanged);
    if (!isNull) {
        SetJArray(env,_val,env->NewDirectByteBuffer((void*)val,size));
    }
    SetJArray(env,_isNull,isNull);
    SetJArray(env,_isChanged,isChanged);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetHashedIdMember
 * Signature: (Ljava/nio/ByteBuffer;II[J[Ljava/lang/String;[Z[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetHashedIdMember
  (JNIEnv * env, jclass, jobject _blob, jint _member, jint _index, jlongArray _hashVal, jobjectArray _hashStr, jbooleanArray _isNull, jbooleanArray _isChanged)
{
    char * blob = static_cast<char*>(env->GetDirectBufferAddress(_blob));
    DotsC_Int64 val;
    const char * str;
    bool isNull, isChanged;
    DotsC_GetHashedIdMember(blob,_member,_index,val,str,isNull,isChanged);
    if (!isNull)
    {
        SetJArray(env,_hashVal, val);
        SetJArray(env,_hashStr,env->NewStringUTF(str));
    }
    SetJArray(env,_isNull,isNull);
    SetJArray(env,_isChanged,isChanged);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetEntityIdMember
 * Signature: (Ljava/nio/ByteBuffer;II[J[J[Ljava/lang/String;[Z[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetEntityIdMember
  (JNIEnv * env, jclass, jobject _blob, jint _member, jint _index, jlongArray _typeId, jlongArray _instanceId, jobjectArray _instanceIdStr, jbooleanArray _isNull, jbooleanArray _isChanged)
{
    char * blob = static_cast<char*>(env->GetDirectBufferAddress(_blob));
    DotsC_EntityId val;
    const char * instanceIdStr;
    bool isNull, isChanged;
    DotsC_GetEntityIdMember(blob,_member,_index,val,instanceIdStr,isNull,isChanged);
    if (!isNull)
    {
        SetJArray(env,_typeId, val.typeId);
        SetJArray(env,_instanceId, val.instanceId);
        SetJArray(env,_instanceIdStr,env->NewStringUTF(instanceIdStr));
    }
    SetJArray(env,_isNull,isNull);
    SetJArray(env,_isChanged,isChanged);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetObjectMember
 * Signature: (Ljava/nio/ByteBuffer;II[Ljava/nio/ByteBuffer;[Z[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetObjectMember
  (JNIEnv * env, jclass, jobject _blob, jint _member, jint _index, jobjectArray _childBlob, jbooleanArray _isNull, jbooleanArray _isChanged)
{
    char * blob = static_cast<char*>(env->GetDirectBufferAddress(_blob));
    const char * childBlob;
    bool isNull, isChanged;
    DotsC_GetObjectMember(blob,
                          _member,
                          _index,
                          childBlob,
                          isNull,
                          isChanged);
    if (!isNull)
    {
        SetJArray(env,_childBlob,env->NewDirectByteBuffer((void*)childBlob,DotsC_GetSize(childBlob)));
    }
    SetJArray(env,_isNull,isNull);
    SetJArray(env,_isChanged,isChanged);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    SetNullMember
 * Signature: (Ljava/nio/ByteBuffer;II)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_SetNullMember
  (JNIEnv * env, jclass, jobject _blob, jint _member, jint _index)
{
    char * blob = static_cast<char*>(env->GetDirectBufferAddress(_blob));
    DotsC_SetNullMember(blob,_member,_index);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    SetBooleanMemberInPreallocated
 * Signature: (ZZZLjava/nio/ByteBuffer;II)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_SetBooleanMemberInPreallocated
  (JNIEnv * env, jclass, jboolean _val, jboolean _isNull, jboolean _isChanged, jobject _blob, jint _member, jint _index)
{
    char * blob = static_cast<char*>(env->GetDirectBufferAddress(_blob));
    DotsC_SetBooleanMemberInPreallocated(_val == JNI_TRUE,_isNull == JNI_TRUE,_isChanged == JNI_TRUE, blob,_member,_index);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    SetInt32MemberInPreallocated
 * Signature: (IZZLjava/nio/ByteBuffer;II)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_SetInt32MemberInPreallocated
  (JNIEnv * env, jclass, jint _val, jboolean _isNull, jboolean _isChanged, jobject _blob, jint _member, jint _index)
{
    char * blob = static_cast<char*>(env->GetDirectBufferAddress(_blob));
    DotsC_SetInt32MemberInPreallocated(_val, _isNull == JNI_TRUE, _isChanged == JNI_TRUE, blob,_member,_index);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    SetInt64MemberInPreallocated
 * Signature: (JZZLjava/nio/ByteBuffer;II)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_SetInt64MemberInPreallocated
  (JNIEnv * env, jclass, jlong _val, jboolean _isNull, jboolean _isChanged, jobject _blob, jint _member, jint _index)
{
    char * blob = static_cast<char*>(env->GetDirectBufferAddress(_blob));
    DotsC_SetInt64MemberInPreallocated(_val, _isNull == JNI_TRUE, _isChanged == JNI_TRUE, blob,_member,_index);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    SetFloat32MemberInPreallocated
 * Signature: (FZZLjava/nio/ByteBuffer;II)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_SetFloat32MemberInPreallocated
  (JNIEnv * env, jclass, jfloat _val, jboolean _isNull, jboolean _isChanged, jobject _blob, jint _member, jint _index)
{
    char * blob = static_cast<char*>(env->GetDirectBufferAddress(_blob));
    DotsC_SetFloat32MemberInPreallocated(_val, _isNull == JNI_TRUE, _isChanged == JNI_TRUE, blob,_member,_index);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    SetFloat64MemberInPreallocated
 * Signature: (DZZLjava/nio/ByteBuffer;II)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_SetFloat64MemberInPreallocated
  (JNIEnv * env, jclass, jdouble _val, jboolean _isNull, jboolean _isChanged, jobject _blob, jint _member, jint _index)
{
    char * blob = static_cast<char*>(env->GetDirectBufferAddress(_blob));
    DotsC_SetFloat64MemberInPreallocated(_val, _isNull == JNI_TRUE, _isChanged == JNI_TRUE, blob,_member,_index);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    SetHashedIdMemberInPreallocated
 * Signature: (JLjava/lang/String;IZZLjava/nio/ByteBuffer;II[I)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_SetHashedIdMemberInPreallocated
  (JNIEnv * env, jclass, jlong _hashVal, jstring _strVal, jint _stringLength, jboolean _isNull, jboolean _isChanged, jobject _blob, jint _member, jint _index, jintArray _beginningOfUnused)
{
    char * blob = static_cast<char*>(env->GetDirectBufferAddress(_blob));
    char * beginningOfUnused = blob + GetJArrayInt32(env,_beginningOfUnused);
    DotsC_SetHashedIdMemberInPreallocated(_hashVal,
                                          _stringLength == 0 ? NULL: GetUtf8(env,_strVal).get(),
                                          _stringLength,
                                          _isNull == JNI_TRUE,
                                          _isChanged == JNI_TRUE,
                                          blob,
                                          _member,
                                          _index,
                                          beginningOfUnused);
    SetJArray(env,_beginningOfUnused,static_cast<DotsC_Int32>(beginningOfUnused - blob));
    assert(_stringLength == 0 || *(beginningOfUnused - 1) == 0); //check that we got null terminated correctly
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    SetEntityIdMemberInPreallocated
 * Signature: (JJLjava/lang/String;IZZLjava/nio/ByteBuffer;II[I)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_SetEntityIdMemberInPreallocated
(JNIEnv * env, jclass, jlong _typeId, jlong _instanceId, jstring _strVal, jint _stringLength, jboolean _isNull, jboolean _isChanged, jobject _blob, jint _member, jint _index, jintArray _beginningOfUnused)
{
    char * blob = static_cast<char*>(env->GetDirectBufferAddress(_blob));
    char * beginningOfUnused = blob + GetJArrayInt32(env,_beginningOfUnused);
    DotsC_EntityId eid;
    eid.typeId = _typeId;
    eid.instanceId = _instanceId;
    DotsC_SetEntityIdMemberInPreallocated(eid,
                                          _stringLength == 0 ? NULL: GetUtf8(env,_strVal).get(),
                                          _stringLength,
                                          _isNull == JNI_TRUE,
                                          _isChanged == JNI_TRUE,
                                          blob,
                                          _member,
                                          _index,
                                          beginningOfUnused);
    SetJArray(env,_beginningOfUnused,static_cast<DotsC_Int32>(beginningOfUnused - blob));
    
    assert(_stringLength == 0 || *(beginningOfUnused - 1) == 0); //check that we got null terminated correctly
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetBooleanPropertyParameter
 * Signature: (JJII[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetBooleanPropertyParameter
  (JNIEnv * env, jclass, jlong _typeId, jlong _propertyId, jint _member, jint _index, jbooleanArray _val)
{
    bool val;
    DotsC_GetBooleanPropertyParameter(_typeId,_propertyId,_member,_index,val);
    SetJArray(env,_val,val);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetInt32PropertyParameter
 * Signature: (JJII[I)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetInt32PropertyParameter
  (JNIEnv * env, jclass, jlong _typeId, jlong _propertyId, jint _member, jint _index, jintArray _val)
{
    DotsC_Int32 val;
    DotsC_GetInt32PropertyParameter(_typeId,_propertyId,_member,_index,val);
    SetJArray(env,_val,val);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetInt64PropertyParameter
 * Signature: (JJII[J)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetInt64PropertyParameter
  (JNIEnv * env, jclass, jlong _typeId, jlong _propertyId, jint _member, jint _index, jlongArray _val)
{
    DotsC_Int64 val;
    DotsC_GetInt64PropertyParameter(_typeId,_propertyId,_member,_index,val);
    SetJArray(env,_val,val);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetFloat32PropertyParameter
 * Signature: (JJII[F)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetFloat32PropertyParameter
  (JNIEnv * env, jclass, jlong _typeId, jlong _propertyId, jint _member, jint _index, jfloatArray _val)
{
    DotsC_Float32 val;
    DotsC_GetFloat32PropertyParameter(_typeId,_propertyId,_member,_index,val);
    SetJArray(env,_val,val);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetFloat64PropertyParameter
 * Signature: (JJII[D)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetFloat64PropertyParameter
  (JNIEnv * env, jclass, jlong _typeId, jlong _propertyId, jint _member, jint _index, jdoubleArray _val)
{
    DotsC_Float64 val;
    DotsC_GetFloat64PropertyParameter(_typeId,_propertyId,_member,_index,val);
    SetJArray(env,_val,val);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetStringPropertyParameter
 * Signature: (JJII[Ljava/lang/String;)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetStringPropertyParameter
  (JNIEnv * env, jclass, jlong _typeId, jlong _propertyId, jint _member, jint _index, jobjectArray _val)
{
    const char * val;
    DotsC_GetStringPropertyParameter(_typeId,_propertyId,_member,_index,val);
    SetJArray(env,_val,env->NewStringUTF(val));
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetHashedIdPropertyParameter
 * Signature: (JJII[J[Ljava/lang/String;)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetHashedIdPropertyParameter
  (JNIEnv * env, jclass, jlong _typeId, jlong _propertyId, jint _member, jint _index, jlongArray _hashVal, jobjectArray _strVal)
{
    DotsC_Int64 hashVal;
    const char * strVal;
    DotsC_GetHashedIdPropertyParameter(_typeId,_propertyId,_member,_index,hashVal,strVal);
    SetJArray(env,_hashVal,hashVal);
    SetJArray(env,_strVal,env->NewStringUTF(strVal));
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetEntityIdPropertyParameter
 * Signature: (JJII[J[J[Ljava/lang/String;)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetEntityIdPropertyParameter
  (JNIEnv * env, jclass, jlong _typeId, jlong _propertyId, jint _member, jint _index, jlongArray _typeIdOut, jlongArray _instanceId, jobjectArray _strVal)
{
    DotsC_EntityId eid;
    const char * str;
    DotsC_GetEntityIdPropertyParameter(_typeId,_propertyId,_member,_index,eid,str);
    SetJArray(env,_typeIdOut, eid.typeId);
    SetJArray(env,_instanceId, eid.instanceId);
    SetJArray(env,_strVal,env->NewStringUTF(str));
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetObjectPropertyParameter
 * Signature: (JJII[Ljava/nio/ByteBuffer;)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetObjectPropertyParameter
  (JNIEnv * env, jclass, jlong _typeId, jlong _propertyId, jint _member, jint _index, jobjectArray _blob)
{
    const char * blob;
    DotsC_GetObjectPropertyParameter(_typeId,_propertyId,_member,_index,blob);
    SetJArray(env,_blob,env->NewDirectByteBuffer((void*)blob,DotsC_GetSize(blob)));
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetBinaryPropertyParameter
 * Signature: (JJII[Ljava/nio/ByteBuffer;)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetBinaryPropertyParameter
  (JNIEnv * env, jclass, jlong _typeId, jlong _propertyId, jint _member, jint _index, jobjectArray _val)
{
    const char * val;
    DotsC_Int32 size;
    DotsC_GetBinaryPropertyParameter(_typeId,_propertyId,_member,_index,val,size);
    SetJArray(env,_val,env->NewDirectByteBuffer((void*)val,size));
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GetPropertyMappingKind
 * Signature: (JJI[I[I)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetPropertyMappingKind
  (JNIEnv * env, jclass, jlong _typeId, jlong _propertyId, jint _member, jintArray _mappingKind, jintArray _errorCode)
{
    DotsC_PropertyMappingKind mappingKind;
    DotsC_ErrorCode errorCode;
    DotsC_GetPropertyMappingKind(_typeId,_propertyId,_member,mappingKind,errorCode);
    SetJArray(env,_mappingKind,mappingKind);
    SetJArray(env,_errorCode,errorCode);
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
 * Method:    GetInitialSize
 * Signature: (J)I
 */
jint JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GetInitialSize
  (JNIEnv *, jclass, jlong _typeId)
{
    return DotsC_GetInitialSize(_typeId);
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    FormatBlob
 * Signature: (Ljava/nio/ByteBuffer;IJ[I)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_FormatBlob
(JNIEnv * env, jclass, jobject _blob, jint _blobSize, jlong _typeId, jintArray _beginningOfUnused)
{
    char * blob = static_cast<char*>(env->GetDirectBufferAddress(_blob));
    char * beginningOfUnused;
    DotsC_FormatBlob(blob,_blobSize,_typeId,beginningOfUnused);
    SetJArray(env,_beginningOfUnused, static_cast<DotsC_Int32>(beginningOfUnused - blob));
}


/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    CreateObjectMember
 * Signature: (Ljava/nio/ByteBuffer;IJIIZ[I)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_CreateObjectMember
  (JNIEnv * env, jclass, jobject _insideBlob, jint _blobSize, jlong _typeId, jint _member, jint _index, jboolean _isChanged, jintArray _beginningOfUnused)
{
    char * insideBlob = static_cast<char*>(env->GetDirectBufferAddress(_insideBlob));
    char * beginningOfUnused = insideBlob + GetJArrayInt32(env,_beginningOfUnused);
    DotsC_CreateObjectMember(insideBlob,_blobSize,_typeId,_member,_index,_isChanged == JNI_TRUE, beginningOfUnused);
    SetJArray(env,_beginningOfUnused,static_cast<DotsC_Int32>(beginningOfUnused - insideBlob));
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    CreateStringMember
 * Signature: (Ljava/nio/ByteBuffer;IIIZ[I)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_CreateStringMember
  (JNIEnv * env, jclass, jobject _insideBlob, jint _strLength, jint _member, jint _index, jboolean _isChanged, jintArray _beginningOfUnused)
{
    char * insideBlob = static_cast<char*>(env->GetDirectBufferAddress(_insideBlob));
    char * beginningOfUnused = insideBlob + GetJArrayInt32(env,_beginningOfUnused);
    DotsC_CreateStringMember(insideBlob,_strLength,_member,_index,_isChanged == JNI_TRUE, beginningOfUnused);
    SetJArray(env,_beginningOfUnused,static_cast<DotsC_Int32>(beginningOfUnused - insideBlob));
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    CreateBinaryMember
 * Signature: (Ljava/nio/ByteBuffer;IIIZ[I)V
 */
void JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_CreateBinaryMember
  (JNIEnv * env, jclass, jobject _insideBlob, jint _binaryLength, jint _member, jint _index, jboolean _isChanged, jintArray _beginningOfUnused)
{
    char * insideBlob = static_cast<char*>(env->GetDirectBufferAddress(_insideBlob));
    char * beginningOfUnused = insideBlob + GetJArrayInt32(env,_beginningOfUnused);
    DotsC_CreateBinaryMember(insideBlob,_binaryLength,_member,_index,_isChanged == JNI_TRUE, beginningOfUnused);
    SetJArray(env,_beginningOfUnused,static_cast<DotsC_Int32>(beginningOfUnused - insideBlob));
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

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    GenerateRandom64
 * Signature: ()J
 */
jlong JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_GenerateRandom64
  (JNIEnv *, jclass)
{
    return DotsId_GenerateRandom64();
}

/*
 * Class:     com_saabgroup_safir_dob_typesystem_Kernel
 * Method:    Generate64
 * Signature: (Ljava/lang/String;)J
 */
jlong JNICALL Java_com_saabgroup_safir_dob_typesystem_Kernel_Generate64
  (JNIEnv * env, jclass, jstring _str)
{
    return DotsId_Generate64(GetUtf8(env,_str).get());
}





