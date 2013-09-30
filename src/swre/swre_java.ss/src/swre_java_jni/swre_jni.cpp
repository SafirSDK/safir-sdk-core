/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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
#include <assert.h>
#include <Safir/SwReports/Internal/Interface.h>
#include <Safir/Dob/Typesystem/Defs.h>
#include "com_saabgroup_safir_application_Library.h"
#include "com_saabgroup_safir_swreports_Library.h"
#include <vector>

class StringConverter
{
public:
    StringConverter(JNIEnv * _env, jstring jstr):
        m_env(_env),
        m_jstr(jstr),
        m_isCopy(false)
    {
        m_str = m_env->GetStringUTFChars(m_jstr,&m_isCopy);
    }

    ~StringConverter()
    {
        m_env->ReleaseStringUTFChars(m_jstr, m_str);
    }

    operator const char*() const
    {
        return m_str;
    }
private:
    JNIEnv * m_env;
    jstring m_jstr;
    const char* m_str;
    jboolean m_isCopy;
};


class StringArrayConverter
{
public:
    StringArrayConverter(JNIEnv * _env, jobjectArray jstrArr)
    {
        m_size = _env->GetArrayLength(jstrArr);
        m_stringConverters.reserve(m_size);
        for (int i = 0; i < m_size; ++i)
        {
            m_stringConverters.push_back(StringConverter(_env,static_cast<jstring>(_env->GetObjectArrayElement(jstrArr,i))));
            const char * str = m_stringConverters.back();
            m_result.push_back(str);
        }
    }

    ~StringArrayConverter()
    {
    }
    operator const char * const *() const
    {
        return &m_result[0];
    }
    int size() const
    {
        return m_size;
    }
private:
    int m_size;

    std::vector<StringConverter> m_stringConverters;
    std::vector<const char*> m_result;
};

void SetJArray(JNIEnv * _env,
               jbooleanArray array,
               const bool toValue)
{
    jboolean isCopy;
    jboolean * arrayElems = _env->GetBooleanArrayElements(array, &isCopy);
    assert(_env->GetArrayLength(array) == 1);
    arrayElems[0] = toValue;
    if (isCopy == JNI_TRUE)
    {
        _env->ReleaseBooleanArrayElements(array, arrayElems, 0);
    }
}

void SetJArray(JNIEnv * _env,
               jlongArray array,
               const Safir::Dob::Typesystem::Int64 toValue)
{
    jboolean isCopy = false;
    jlong * arrayElems = _env->GetLongArrayElements(array, &isCopy);
    assert(_env->GetArrayLength(array) == 1);
    arrayElems[0] = toValue;
    if (isCopy == JNI_TRUE)
    {
        _env->ReleaseLongArrayElements(array, arrayElems, 0);
    }
}

void JNICALL Java_com_saabgroup_safir_swreports_Library_SendFatalErrorReport
(JNIEnv * _env, jclass, jstring _errorCode, jstring _location, jstring _text, jbooleanArray _success)
{
    bool success;
    SwreC_SendFatalErrorReport(StringConverter(_env,_errorCode),
                               StringConverter(_env,_location),
                               StringConverter(_env,_text),
                               success);
    SetJArray(_env,_success,success);
}




void JNICALL Java_com_saabgroup_safir_swreports_Library_SendErrorReport
(JNIEnv * _env, jclass, jstring _errorCode, jstring _location, jstring _text, jbooleanArray _success)
{
   bool success;
   SwreC_SendErrorReport(StringConverter(_env,_errorCode),
                         StringConverter(_env,_location),
                         StringConverter(_env,_text),
                         success);
   SetJArray(_env,_success,success);
}



void JNICALL Java_com_saabgroup_safir_swreports_Library_SendResourceReport
(JNIEnv * _env, jclass, jstring _resourceId, jboolean _allocated, jstring _text, jbooleanArray _success)
{
    bool success;
    SwreC_SendResourceReport(StringConverter(_env,_resourceId),
                             _allocated == JNI_TRUE,
                             StringConverter(_env,_text),
                             success);
    SetJArray(_env,_success,success);
}



void JNICALL Java_com_saabgroup_safir_swreports_Library_SendProgrammingErrorReport
(JNIEnv * _env, jclass, jstring _errorCode, jstring _location, jstring _text, jbooleanArray _success)
{
   bool success;
   SwreC_SendProgrammingErrorReport(StringConverter(_env,_errorCode),
                                    StringConverter(_env,_location),
                                    StringConverter(_env,_text),
                                    success);
   SetJArray(_env,_success,success);

}



void JNICALL Java_com_saabgroup_safir_swreports_Library_SendProgramInfoReport
(JNIEnv * _env, jclass, jstring _text, jbooleanArray _success)
{
   bool success;
   SwreC_SendProgramInfoReport(StringConverter(_env,_text),
                               success);
   SetJArray(_env,_success,success);
}




void JNICALL Java_com_saabgroup_safir_application_Library_SetProgramName
(JNIEnv * _env, jclass, jstring _programName, jbooleanArray _success)
{
    bool success;
    SwreC_SetProgramName(StringConverter(_env,_programName),
                         success);
    SetJArray(_env,_success,success);
}

void JNICALL Java_com_saabgroup_safir_application_Library_StartTraceBackdoor
  (JNIEnv * _env, jclass, jstring _common, jstring _instance, jbooleanArray _success)
{
    bool success;
    SwreC_StartTraceBackdoor(StringConverter(_env,_common),
                             StringConverter(_env,_instance),
                             success);
    SetJArray(_env,_success,success);
}

void JNICALL Java_com_saabgroup_safir_application_Library_StopTraceBackdoor
  (JNIEnv *, jclass)
{
    SwreC_StopTraceBackdoor();
}


void JNICALL Java_com_saabgroup_safir_application_Library_TraceAppendString
(JNIEnv * _env, jclass, jlong _prefixId, jbyteArray _str, jint _offset, jint _length, jbooleanArray _success)
{
    bool success;
    jboolean isCopy;
    jbyte * bytes = _env->GetByteArrayElements(_str,&isCopy);
    SwreC_TraceAppendSubstring(_prefixId,reinterpret_cast<char*>(bytes), _offset, _length, success);
    if (isCopy == JNI_TRUE)
    {
        _env->ReleaseByteArrayElements(_str,bytes, 0);
    }

    SetJArray(_env,_success,success);
}



void JNICALL Java_com_saabgroup_safir_application_Library_TraceAppendChar
(JNIEnv * _env, jclass, jlong _prefixId, jbyte _b, jbooleanArray _success)
{
    bool success;
    SwreC_TraceAppendChar(_prefixId,_b,success);
    SetJArray(_env,_success,success);
}



void JNICALL Java_com_saabgroup_safir_application_Library_TraceFlush
(JNIEnv * _env, jclass, jbooleanArray _success)
{
    bool success;
    SwreC_TraceFlush(success);
    SetJArray(_env,_success,success);
}



void JNICALL Java_com_saabgroup_safir_application_Library_TracePrefixAdd
(JNIEnv * _env, jclass, jstring _prefix, jlongArray _prefixId, jbooleanArray _success)
{
    bool success;
    Safir::Dob::Typesystem::Int64 prefixId;
    SwreC_TracePrefixAdd(StringConverter(_env,_prefix),prefixId,success);
    SetJArray(_env,_success,success);
    if (success)
    {
        SetJArray(_env,_prefixId,prefixId);
    }
}



void JNICALL Java_com_saabgroup_safir_application_Library_TracePrefixSetEnabled
(JNIEnv * _env, jclass, jlong _prefixId, jboolean _enabled, jbooleanArray _success)
{
    bool success;
    SwreC_TracePrefixSetEnabled(_prefixId,
                                _enabled == JNI_TRUE,
                                success);
    SetJArray(_env,_success,success);
}



jboolean JNICALL Java_com_saabgroup_safir_application_Library_TracePrefixIsEnabled
(JNIEnv *, jclass, jlong _prefixId)
{
    return SwreC_TracePrefixIsEnabled(_prefixId);
}

