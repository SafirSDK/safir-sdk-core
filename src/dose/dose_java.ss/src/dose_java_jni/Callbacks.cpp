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
#include "Callbacks.h"
#include <Safir/Dob/Typesystem/Internal/Kernel.h>
#include <boost/current_function.hpp>
#include "ConsumerTable.h"
#include <iostream>
#include <assert.h>

bool Callbacks::m_dispatchThreadIsAttachedToJVM = false;

JavaVM * Callbacks::m_vm = NULL;
jint Callbacks::m_version = -1;

jmethodID Callbacks::m_onDoDispatch;
jmethodID Callbacks::m_onStopOrder;
jmethodID Callbacks::m_onNewEntity;
jmethodID Callbacks::m_onUpdatedEntity;
jmethodID Callbacks::m_onDeletedEntity;
jmethodID Callbacks::m_onCreateRequest;
jmethodID Callbacks::m_onUpdateRequest;
jmethodID Callbacks::m_onDeleteRequest;
jmethodID Callbacks::m_onServiceRequest;
jmethodID Callbacks::m_onResponse;
jmethodID Callbacks::m_onRevokedRegistration;
jmethodID Callbacks::m_onCompletedRegistration;
jmethodID Callbacks::m_onMessage;
jmethodID Callbacks::m_onRegistered;
jmethodID Callbacks::m_onUnregistered;
jmethodID Callbacks::m_onInjectedNewEntity;
jmethodID Callbacks::m_onInjectedUpdatedEntity;
jmethodID Callbacks::m_onInjectedDeletedEntity;
jmethodID Callbacks::m_onInitialInjectionsDone;
jmethodID Callbacks::m_onNotRequestOverflow;
jmethodID Callbacks::m_onNotMessageOverflow;


// ----------------------------------------------------------------------
// Routines for getting values from the "out" arrays.
//
//    use these to get the first element of an array
//    will assert on arraylength == 1
// ----------------------------------------------------------------------

bool GetJArray(JNIEnv * _env,
               jbooleanArray array)
{
    jboolean isCopy;
    jboolean * arrayElems = _env->GetBooleanArrayElements(array, &isCopy);
    assert(_env->GetArrayLength(array) == 1);
    const bool value = arrayElems[0] == JNI_TRUE;
    if (isCopy == JNI_TRUE)
    {
        _env->ReleaseBooleanArrayElements(array, arrayElems, 0);
    }
    return value;
}



JNIEnv* Callbacks::CallbackEnv()
{
    JNIEnv* env;
    const jint res = m_vm->GetEnv((void**)&env, m_version);
    if (res == JNI_OK)
    {
        return env;
    }
    else if (res == JNI_EDETACHED)
    {
        std::wcout << "CallbackEnv() failed, GetEnv returned JNI_EDETACHED!" << std::endl;
    }
    else
    {
        std::wcout << "CallbackEnv() failed, GetEnv returned " << res << "!" << std::endl;
    }
    abort();
}


jclass Callbacks::GetCallbacksClass()
{
    static jclass callbacks = NULL;
    if (callbacks == NULL)
    {
        jclass localCallbacks=CallbackEnv()->FindClass("com/saabgroup/safir/dob/Callbacks");
        if (localCallbacks==NULL)
        {
            std::wcout<<"dose_java_internal.dll - InitCallbacks: Couldn't find class com/saabgroup/safir/dob/Callbacks!"<<std::endl;
            return NULL;
        }
        callbacks = (jclass)CallbackEnv()->NewGlobalRef(localCallbacks);
    }
    return callbacks;
}


//------------------------------------------------------------------
// Exported routines
//------------------------------------------------------------------
void Callbacks::InitCallbacks(JNIEnv* env)
{
    m_version=env->GetVersion();

    //Keep pointer to the Java Virtual Machine. Needed to do callbacks.
    env->GetJavaVM(&m_vm);

    jclass callbacks=GetCallbacksClass();

    m_onDoDispatch=env->GetStaticMethodID(callbacks, "onDoDispatch", "(Lcom/saabgroup/safir/dob/Dispatcher;[Z)V");
    assert(!env->ExceptionOccurred());
    if (m_onDoDispatch==NULL)
    {
        std::wcout << "dose_java_internal.dll - InitCallbacks: Couldn't find method 'onDoDispatch'!" << std::endl;
        exit(10);
    }

    m_onStopOrder=env->GetStaticMethodID(callbacks, "onStopOrder", "(Lcom/saabgroup/safir/dob/StopHandler;[Z)V");
    assert(!env->ExceptionOccurred());
    if (m_onStopOrder==NULL)
    {
        std::wcout << "dose_java_internal.dll - InitCallbacks: Couldn't find method 'onStopOrder'!" << std::endl;
        exit(10);
    }

    m_onNewEntity=env->GetStaticMethodID(callbacks, "onNewEntity", "(Ljava/nio/ByteBuffer;Ljava/nio/ByteBuffer;Lcom/saabgroup/safir/dob/EntitySubscriber;Z[Z)V");
    assert(!env->ExceptionOccurred());
    if (m_onNewEntity==NULL)
    {
        std::wcout << "dose_java_internal.dll - InitCallbacks: Couldn't find method 'onNewEntity'!" << std::endl;
        exit(10);
    }

    m_onUpdatedEntity=env->GetStaticMethodID(callbacks, "onUpdatedEntity", "(Ljava/nio/ByteBuffer;Ljava/nio/ByteBuffer;Ljava/nio/ByteBuffer;Ljava/nio/ByteBuffer;Lcom/saabgroup/safir/dob/EntitySubscriber;Z[Z)V");
    assert(!env->ExceptionOccurred());
    if (m_onUpdatedEntity==NULL)
    {
        std::wcout << "dose_java_internal.dll - InitCallbacks: Couldn't find method 'onUpdatedEntity'!" << std::endl;
        exit(10);
    }

    m_onDeletedEntity=env->GetStaticMethodID(callbacks, "onDeletedEntity", "(Ljava/nio/ByteBuffer;Ljava/nio/ByteBuffer;Ljava/nio/ByteBuffer;ZLcom/saabgroup/safir/dob/EntitySubscriber;Z[Z)V");
    assert(!env->ExceptionOccurred());
    if (m_onDeletedEntity==NULL)
    {
        std::wcout << "dose_java_internal.dll - InitCallbacks: Couldn't find method 'onDeletedEntity'!" << std::endl;
        exit(10);
    }

    m_onCreateRequest=env->GetStaticMethodID(callbacks, "onCreateRequest", "(Ljava/nio/ByteBuffer;Ljava/nio/ByteBuffer;IILcom/saabgroup/safir/dob/EntityRequestBase;[Z)V");
    assert(!env->ExceptionOccurred());
    if (m_onCreateRequest==NULL)
    {
        std::wcout << "dose_java_internal.dll - InitCallbacks: Couldn't find method 'onCreateRequest'!" << std::endl;
        exit(10);
    }

    m_onUpdateRequest=env->GetStaticMethodID(callbacks, "onUpdateRequest", "(Ljava/nio/ByteBuffer;Ljava/nio/ByteBuffer;IILcom/saabgroup/safir/dob/EntityRequestBase;[Z)V");
    assert(!env->ExceptionOccurred());
    if (m_onUpdateRequest==NULL)
    {
        std::wcout << "dose_java_internal.dll - InitCallbacks: Couldn't find method 'onUpdateRequest'!" << std::endl;
        exit(10);
    }

    m_onDeleteRequest=env->GetStaticMethodID(callbacks, "onDeleteRequest", "(Ljava/nio/ByteBuffer;IILcom/saabgroup/safir/dob/EntityRequestBase;[Z)V");
    assert(!env->ExceptionOccurred());
    if (m_onDeleteRequest==NULL)
    {
        std::wcout << "dose_java_internal.dll - InitCallbacks: Couldn't find method 'onDeleteRequest'!" << std::endl;
        exit(10);
    }

    m_onServiceRequest=env->GetStaticMethodID(callbacks, "onServiceRequest", "(Ljava/nio/ByteBuffer;Ljava/nio/ByteBuffer;IILcom/saabgroup/safir/dob/ServiceRequestBase;[Z)V");
    assert(!env->ExceptionOccurred());
    if (m_onServiceRequest==NULL)
    {
        std::wcout << "dose_java_internal.dll - InitCallbacks: Couldn't find method 'onServiceRequest'!" << std::endl;
        exit(10);
    }

    m_onResponse=env->GetStaticMethodID(callbacks, "onResponse", "(ILjava/nio/ByteBuffer;Ljava/nio/ByteBuffer;Ljava/nio/ByteBuffer;Ljava/nio/ByteBuffer;Lcom/saabgroup/safir/dob/Requestor;[Z)V");
    assert(!env->ExceptionOccurred());
    if (m_onResponse==NULL)
    {
        std::wcout << "dose_java_internal.dll - InitCallbacks: Couldn't find method 'onResponse'!" << std::endl;
        exit(10);
    }

    m_onRevokedRegistration=env->GetStaticMethodID(callbacks, "onRevokedRegistration", "(JJLjava/lang/String;Lcom/saabgroup/safir/dob/RevokedRegistrationBase;[Z)V");
    assert(!env->ExceptionOccurred());
    if (m_onRevokedRegistration==NULL)
    {
        std::wcout << "dose_java_internal.dll - InitCallbacks: Couldn't find method 'onRevokedRegistration'!" << std::endl;
        exit(10);
    }

    m_onCompletedRegistration=env->GetStaticMethodID(callbacks, "onCompletedRegistration", "(JJLjava/lang/String;Lcom/saabgroup/safir/dob/CompletedRegistrationBase;[Z)V");
    assert(!env->ExceptionOccurred());
    if (m_onCompletedRegistration==NULL)
    {
        std::wcout << "dose_java_internal.dll - InitCallbacks: Couldn't find method 'onCompletedRegistration'!" << std::endl;
        exit(10);
    }


    m_onMessage=env->GetStaticMethodID(callbacks, "onMessage", "(Ljava/nio/ByteBuffer;Ljava/nio/ByteBuffer;Lcom/saabgroup/safir/dob/MessageSubscriber;[Z)V");
    assert(!env->ExceptionOccurred());
    if (m_onMessage==NULL)
    {
        std::wcout << "dose_java_internal.dll - InitCallbacks: Couldn't find method 'onMessage'!" << std::endl;
        exit(10);
    }

    m_onRegistered=env->GetStaticMethodID(callbacks, "onRegistered", "(JJLjava/lang/String;Lcom/saabgroup/safir/dob/RegistrationSubscriber;[Z)V");
    assert(!env->ExceptionOccurred());
    if (m_onRegistered==NULL)
    {
        std::wcout << "dose_java_internal.dll - InitCallbacks: Couldn't find method 'onRegistered'!" << std::endl;
        exit(10);
    }

    m_onUnregistered=env->GetStaticMethodID(callbacks, "onUnregistered", "(JJLjava/lang/String;Lcom/saabgroup/safir/dob/RegistrationSubscriber;[Z)V");
    assert(!env->ExceptionOccurred());
    if (m_onUnregistered==NULL)
    {
        std::wcout << "dose_java_internal.dll - InitCallbacks: Couldn't find method 'onUnregistered'!" << std::endl;
        exit(10);
    }

    m_onInjectedNewEntity=env->GetStaticMethodID(callbacks, "onInjectedNewEntity", "(Ljava/nio/ByteBuffer;Ljava/nio/ByteBuffer;Lcom/saabgroup/safir/dob/EntityInjectionBase;[Z)V");
    assert(!env->ExceptionOccurred());
    if (m_onInjectedNewEntity==NULL)
    {
        std::wcout << "dose_java_internal.dll - InitCallbacks: Couldn't find method 'onInjectedNewEntity'!" << std::endl;
        exit(10);
    }

    m_onInjectedUpdatedEntity=env->GetStaticMethodID(callbacks, "onInjectedUpdatedEntity", "(Ljava/nio/ByteBuffer;Ljava/nio/ByteBuffer;Ljava/nio/ByteBuffer;Ljava/nio/ByteBuffer;Lcom/saabgroup/safir/dob/EntityInjectionBase;[Z)V");
    assert(!env->ExceptionOccurred());
    if (m_onInjectedUpdatedEntity==NULL)
    {
        std::wcout << "dose_java_internal.dll - InitCallbacks: Couldn't find method 'onInjectedUpdatedEntity'!" << std::endl;
        exit(10);
    }

    m_onInjectedDeletedEntity=env->GetStaticMethodID(callbacks, "onInjectedDeletedEntity", "(Ljava/nio/ByteBuffer;Ljava/nio/ByteBuffer;Ljava/nio/ByteBuffer;Lcom/saabgroup/safir/dob/EntityInjectionBase;[Z)V");
    assert(!env->ExceptionOccurred());
    if (m_onInjectedDeletedEntity==NULL)
    {
        std::wcout << "dose_java_internal.dll - InitCallbacks: Couldn't find method 'onInjectedDeletedEntity'!" << std::endl;
        exit(10);
    }


    m_onInitialInjectionsDone=env->GetStaticMethodID(callbacks, "onInitialInjectionsDone", "(JJLjava/lang/String;Lcom/saabgroup/safir/dob/EntityInjectionBase;[Z)V");
    assert(!env->ExceptionOccurred());
    if (m_onInitialInjectionsDone==NULL)
    {
        std::wcout << "dose_java_internal.dll - InitCallbacks: Couldn't find method 'onInitialInjectionsDone'!" << std::endl;
        exit(10);
    }

    m_onNotRequestOverflow=env->GetStaticMethodID(callbacks, "onNotRequestOverflow", "(Lcom/saabgroup/safir/dob/Requestor;[Z)V");
    assert(!env->ExceptionOccurred());
    if (m_onNotRequestOverflow==NULL)
    {
        std::wcout << "dose_java_internal.dll - InitCallbacks: Couldn't find method 'onNotRequestOverflow'!" << std::endl;
        exit(10);
    }

    m_onNotMessageOverflow=env->GetStaticMethodID(callbacks, "onNotMessageOverflow", "(Lcom/saabgroup/safir/dob/MessageSender;[Z)V");
    assert(!env->ExceptionOccurred());
    if (m_onNotMessageOverflow==NULL)
    {
        std::wcout << "dose_java_internal.dll - InitCallbacks: Couldn't find method 'onNotMessageOverflow'!" << std::endl;
        exit(10);
    }

    //Everything we need was found!
}


void Callbacks::OnDispatchCb(void* const consumer,
                             bool& success)
{
    JNIEnv* env;
    m_vm->AttachCurrentThread((void**)&env, NULL);


    jobject obj=static_cast<jobject>(consumer);
    jbooleanArray jsuccess = env->NewBooleanArray(1);
    env->CallStaticVoidMethod(GetCallbacksClass(), m_onDoDispatch, obj, jsuccess);
    success = GetJArray(env,jsuccess);

    m_vm->DetachCurrentThread();
}

void Callbacks::OnStopOrderCb(void* const consumer,
                              bool& success)
{
    jobject stopHandler=static_cast<jobject>(consumer);
    jbooleanArray jsuccess = CallbackEnv()->NewBooleanArray(1);
    CallbackEnv()->CallStaticVoidMethod(GetCallbacksClass(), m_onStopOrder, stopHandler, jsuccess);
    success = GetJArray(CallbackEnv(),jsuccess);
}

void Callbacks::OnNewEntityCb(const char* const currentBlob,
                              const char* const currentState,
                              void* const consumer,
                              const bool timestampDiff,
                              bool& success)
{
    jobject entitySubscriber = static_cast<jobject>(consumer);
    jbooleanArray jsuccess = CallbackEnv()->NewBooleanArray(1);

    CallbackEnv()->CallStaticVoidMethod(GetCallbacksClass(),
                                        m_onNewEntity,
                                        CallbackEnv()->NewDirectByteBuffer((void*)currentBlob,DotsC_GetSize(currentBlob)),
                                        CallbackEnv()->NewDirectByteBuffer((void*)currentState,0), //no size, just passing the ptr
                                        entitySubscriber,
                                        timestampDiff,
                                        jsuccess);
    success = GetJArray(CallbackEnv(),jsuccess);
}

void Callbacks::OnUpdatedEntityCb(const char* const currentBlob,
                                  const char* const currentState,
                                  const char* const previousBlob,
                                  const char* const previousState,
                                  void* const consumer,
                                  const bool timestampDiff,
                                  bool& success)
{
    jobject entitySubscriber = static_cast<jobject>(consumer);
    jbooleanArray jsuccess = CallbackEnv()->NewBooleanArray(1);

    CallbackEnv()->CallStaticVoidMethod(GetCallbacksClass(),
                                        m_onUpdatedEntity,
                                        CallbackEnv()->NewDirectByteBuffer((void*)currentBlob,DotsC_GetSize(currentBlob)),
                                        CallbackEnv()->NewDirectByteBuffer((void*)currentState,0), //no size, just passing the ptr
                                        CallbackEnv()->NewDirectByteBuffer((void*)previousBlob,DotsC_GetSize(previousBlob)),
                                        CallbackEnv()->NewDirectByteBuffer((void*)previousState,0), //no size, just passing the ptr
                                        entitySubscriber,
                                        timestampDiff,
                                        jsuccess);
    success = GetJArray(CallbackEnv(),jsuccess);
}

void Callbacks::OnDeletedEntityCb(const char* const currentState,
                                  const char* const previousBlob,
                                  const char* const previousState,
                                  const bool explicitlyDeleted,
                                  void* const consumer,
                                  const bool timestampDiff,
                                  bool& success)
{
    jobject entitySubscriber = static_cast<jobject>(consumer);
    jbooleanArray jsuccess = CallbackEnv()->NewBooleanArray(1);

    CallbackEnv()->CallStaticVoidMethod(GetCallbacksClass(),
                                        m_onDeletedEntity,
                                        CallbackEnv()->NewDirectByteBuffer((void*)currentState,0), //no size, just passing the ptr
                                        CallbackEnv()->NewDirectByteBuffer((void*)previousBlob,DotsC_GetSize(previousBlob)),
                                        CallbackEnv()->NewDirectByteBuffer((void*)previousState,0), //no size, just passing the ptr
                                        explicitlyDeleted,
                                        entitySubscriber,
                                        timestampDiff,
                                        jsuccess);
    success = GetJArray(CallbackEnv(),jsuccess);
}

void Callbacks::OnCreateRequestCb(const char* const requestBlob,
                                  const char* const state,
                                  const long ctrl,
                                  const DotsC_Int32 responseId,
                                  void* const consumer,
                                  bool& success)
{
    jobject entityRequestBase = static_cast<jobject>(consumer);
    jbooleanArray jsuccess = CallbackEnv()->NewBooleanArray(1);

    CallbackEnv()->CallStaticVoidMethod(GetCallbacksClass(),
                                        m_onCreateRequest,
                                        CallbackEnv()->NewDirectByteBuffer((void*)requestBlob,DotsC_GetSize(requestBlob)),
                                        CallbackEnv()->NewDirectByteBuffer((void*)state,0), //no size, just passing the ptr
                                        ctrl,
                                        responseId,
                                        entityRequestBase,
                                        jsuccess);
    success = GetJArray(CallbackEnv(),jsuccess);
}

void Callbacks::OnUpdateRequestCb(const char* const requestBlob,
                                  const char* const state,
                                  const long ctrl,
                                  const DotsC_Int32 responseId,
                                  void* const consumer,
                                  bool& success)
{
    jobject entityRequestBase = static_cast<jobject>(consumer);
    jbooleanArray jsuccess = CallbackEnv()->NewBooleanArray(1);

    CallbackEnv()->CallStaticVoidMethod(GetCallbacksClass(),
                                        m_onUpdateRequest,
                                        CallbackEnv()->NewDirectByteBuffer((void*)requestBlob,DotsC_GetSize(requestBlob)),
                                        CallbackEnv()->NewDirectByteBuffer((void*)state,0), //no size, just passing the ptr
                                        ctrl,
                                        responseId,
                                        entityRequestBase,
                                        jsuccess);
    success = GetJArray(CallbackEnv(),jsuccess);
}

void Callbacks::OnDeleteRequestCb(const char* const state,
                                  const long ctrl,
                                  const DotsC_Int32 responseId,
                                  void* const consumer,
                                  bool& success)
{
    jobject entityRequestBase = static_cast<jobject>(consumer);
    jbooleanArray jsuccess = CallbackEnv()->NewBooleanArray(1);

    CallbackEnv()->CallStaticVoidMethod(GetCallbacksClass(),
                                        m_onDeleteRequest,
                                        CallbackEnv()->NewDirectByteBuffer((void*)state,0), //no size, just passing the ptr
                                        ctrl,
                                        responseId,
                                        entityRequestBase,
                                        jsuccess);
    success = GetJArray(CallbackEnv(),jsuccess);
}

void Callbacks::OnServiceRequestCb(const char* const requestBlob,
                                   const char* const state,
                                   const long ctrl,
                                   const DotsC_Int32 responseId,
                                   void* const consumer,
                                   bool& success)
{
    jobject serviceRequestBase = static_cast<jobject>(consumer);
    jbooleanArray jsuccess = CallbackEnv()->NewBooleanArray(1);

    CallbackEnv()->CallStaticVoidMethod(GetCallbacksClass(),
                                        m_onServiceRequest,
                                        CallbackEnv()->NewDirectByteBuffer((void*)requestBlob,DotsC_GetSize(requestBlob)),
                                        CallbackEnv()->NewDirectByteBuffer((void*)state,0), //no size, just passing the ptr
                                        ctrl,
                                        responseId,
                                        serviceRequestBase,
                                        jsuccess);
    success = GetJArray(CallbackEnv(),jsuccess);
}

void Callbacks::OnResponseCb(const DoseC_RequestId requestId,
                             const char* const responseBlob,
                             const char* const responseState,
                             const char* const requestBlob,
                             const char* const requestState,
                             void* const consumer,
                             bool& success)
{
    jobject requestor = static_cast<jobject>(consumer);
    jbooleanArray jsuccess = CallbackEnv()->NewBooleanArray(1);

    //requestBlob may be null if it was a deleteRequest.
    jobject reqBl = NULL;
    if (requestBlob != NULL) {
        reqBl = CallbackEnv()->NewDirectByteBuffer((void*)requestBlob, DotsC_GetSize(requestBlob));
    }

    CallbackEnv()->CallStaticVoidMethod(GetCallbacksClass(),
                                        m_onResponse,
                                        requestId,
                                        CallbackEnv()->NewDirectByteBuffer((void*)responseBlob,DotsC_GetSize(responseBlob)),
                                        CallbackEnv()->NewDirectByteBuffer((void*)responseState,0), //just pass ptr
                                        reqBl,
                                        CallbackEnv()->NewDirectByteBuffer((void*)requestState,0), //just pass ptr
                                        requestor,
                                        jsuccess);
    success = GetJArray(CallbackEnv(),jsuccess);
    ConsumerTable::Instance().DropReference(CallbackEnv(),requestor);
}

void Callbacks::OnMessageCb(const char* const message,
                            const char* const state,
                            void* const consumer,
                            bool& success)
{
    jobject messageSubscriber = static_cast<jobject>(consumer);
    jbooleanArray jsuccess = CallbackEnv()->NewBooleanArray(1);

    CallbackEnv()->CallStaticVoidMethod(GetCallbacksClass(),
                                        m_onMessage,
                                        CallbackEnv()->NewDirectByteBuffer((void*)message,DotsC_GetSize(message)),
                                        CallbackEnv()->NewDirectByteBuffer((void*)state,0), //no size, just passing the ptr
                                        messageSubscriber,
                                        jsuccess);
    success = GetJArray(CallbackEnv(),jsuccess);

}

void Callbacks::OnRegisteredCb(const DotsC_TypeId typeId,
                                                  const DotsC_Int64 handlerId,
                                                  const char* const handlerIdStr,
                                                  void* const consumer,
                                                  bool& success)
{
    jobject registrationSubscriber = static_cast<jobject>(consumer);
    jbooleanArray jsuccess = CallbackEnv()->NewBooleanArray(1);

    CallbackEnv()->CallStaticVoidMethod(GetCallbacksClass(),
                                        m_onRegistered,
                                        typeId,
                                        handlerId,
                                        CallbackEnv()->NewStringUTF(handlerIdStr),
                                        registrationSubscriber,
                                        jsuccess);
    success = GetJArray(CallbackEnv(),jsuccess);
}

void Callbacks::OnUnregisteredCb(const DotsC_TypeId typeId,
                                 const DotsC_Int64 handlerId,
                                 const char* const handlerIdStr,
                                 void* const consumer,
                                 bool& success)
{
    jobject registrationSubscriber = static_cast<jobject>(consumer);
    jbooleanArray jsuccess = CallbackEnv()->NewBooleanArray(1);

    CallbackEnv()->CallStaticVoidMethod(GetCallbacksClass(),
                                        m_onUnregistered,
                                        typeId,
                                        handlerId,
                                        CallbackEnv()->NewStringUTF(handlerIdStr),
                                        registrationSubscriber,
                                        jsuccess);
    success = GetJArray(CallbackEnv(),jsuccess);
}

void Callbacks::OnRevokedRegistrationCb(const DotsC_TypeId typeId,
                                                           const DotsC_Int64 handlerId,
                                                           const char* const handlerIdStr,
                                                           void* const consumer,
                                                           bool& success)
{
    jobject revokedRegistrationBase = static_cast<jobject>(consumer);
    jbooleanArray jsuccess = CallbackEnv()->NewBooleanArray(1);

    CallbackEnv()->CallStaticVoidMethod(GetCallbacksClass(),
                                        m_onRevokedRegistration,
                                        typeId,
                                        handlerId,
                                        CallbackEnv()->NewStringUTF(handlerIdStr),
                                        revokedRegistrationBase,
                                        jsuccess);
    success = GetJArray(CallbackEnv(),jsuccess);
}

void Callbacks::OnCompletedRegistrationCb(const DotsC_TypeId typeId,
                                                             const DotsC_Int64 handlerId,
                                                             const char* const handlerIdStr,
                                                             void* const consumer,
                                                             bool& success)
{
    jobject completedRegistrationBase = static_cast<jobject>(consumer);
    jbooleanArray jsuccess = CallbackEnv()->NewBooleanArray(1);

    CallbackEnv()->CallStaticVoidMethod(GetCallbacksClass(),
                                        m_onCompletedRegistration,
                                        typeId,
                                        handlerId,
                                        CallbackEnv()->NewStringUTF(handlerIdStr),
                                        completedRegistrationBase,
                                        jsuccess);
    success = GetJArray(CallbackEnv(),jsuccess);
}

void Callbacks::OnInjectedNewEntityCb(const char* const injectionBlob,
                                      const char* const injectionState,
                                      void* const consumer,
                                      bool& success)
{
    jobject entityInjectionBase = static_cast<jobject>(consumer);
    jbooleanArray jsuccess = CallbackEnv()->NewBooleanArray(1);

    CallbackEnv()->CallStaticVoidMethod(GetCallbacksClass(),
                                        m_onInjectedNewEntity,
                                        CallbackEnv()->NewDirectByteBuffer((void*)injectionBlob,DotsC_GetSize(injectionBlob)),
                                        CallbackEnv()->NewDirectByteBuffer((void*)injectionState,0), //no size, just passing the ptr
                                        entityInjectionBase,
                                        jsuccess);
    success = GetJArray(CallbackEnv(),jsuccess);
}

void Callbacks::OnInjectedUpdatedEntityCb(const char* const injectionBlob,
                                          const char* const injectionState,
                                          const char* const currentBlob,
                                          const char* const currentState,
                                          void* const consumer,
                                          bool& success)
{
    jobject entityInjectionBase = static_cast<jobject>(consumer);
    jbooleanArray jsuccess = CallbackEnv()->NewBooleanArray(1);

    CallbackEnv()->CallStaticVoidMethod(GetCallbacksClass(),
                                        m_onInjectedUpdatedEntity,
                                        CallbackEnv()->NewDirectByteBuffer((void*)injectionBlob,DotsC_GetSize(injectionBlob)),
                                        CallbackEnv()->NewDirectByteBuffer((void*)injectionState,0), //no size, just passing the ptr
                                        CallbackEnv()->NewDirectByteBuffer((void*)currentBlob,DotsC_GetSize(currentBlob)),
                                        CallbackEnv()->NewDirectByteBuffer((void*)currentState,0), //no size, just passing the ptr
                                        entityInjectionBase,
                                        jsuccess);
    success = GetJArray(CallbackEnv(),jsuccess);
}

void Callbacks::OnInjectedDeletedEntityCb(const char* const injectionState,
                                          const char* const currentBlob,
                                          const char* const currentState,
                                          void* const consumer,
                                          bool& success)
{
    jobject entityInjectionBase = static_cast<jobject>(consumer);
    jbooleanArray jsuccess = CallbackEnv()->NewBooleanArray(1);

    CallbackEnv()->CallStaticVoidMethod(GetCallbacksClass(),
                                        m_onInjectedDeletedEntity,
                                        CallbackEnv()->NewDirectByteBuffer((void*)injectionState,0), //no size, just passing the ptr
                                        CallbackEnv()->NewDirectByteBuffer((void*)currentBlob,DotsC_GetSize(currentBlob)),
                                        CallbackEnv()->NewDirectByteBuffer((void*)currentState,0), //no size, just passing the ptr
                                        entityInjectionBase,
                                        jsuccess);
    success = GetJArray(CallbackEnv(),jsuccess);
}

void Callbacks::OnInitialInjectionsDoneCb(const DotsC_TypeId typeId,
                                          const DotsC_Int64 handlerId,
                                          const char* const handlerIdStr,
                                          void* const consumer,
                                          bool& success)
{
    jobject entityInjectionBase = static_cast<jobject>(consumer);
    jbooleanArray jsuccess = CallbackEnv()->NewBooleanArray(1);

    CallbackEnv()->CallStaticVoidMethod(GetCallbacksClass(),
                                        m_onInitialInjectionsDone,
                                        typeId,
                                        handlerId,
                                        CallbackEnv()->NewStringUTF(handlerIdStr),
                                        entityInjectionBase,
                                        jsuccess);
    success = GetJArray(CallbackEnv(),jsuccess);
}

void Callbacks::OnNotRequestOverflowCb(void* const consumer,
                                       bool& success)
{
    jobject requestor = static_cast<jobject>(consumer);
    jbooleanArray jsuccess = CallbackEnv()->NewBooleanArray(1);

    CallbackEnv()->CallStaticVoidMethod(GetCallbacksClass(),
                                        m_onNotRequestOverflow,
                                        requestor,
                                        jsuccess);
    success = GetJArray(CallbackEnv(),jsuccess);
    ConsumerTable::Instance().DropReference(CallbackEnv(),requestor);
}

void Callbacks::OnNotMessageOverflowCb(void* const consumer,
                                                          bool& success)
{
    jobject messageSender = static_cast<jobject>(consumer);
    jbooleanArray jsuccess = CallbackEnv()->NewBooleanArray(1);

    CallbackEnv()->CallStaticVoidMethod(GetCallbacksClass(),
                                        m_onNotMessageOverflow,
                                        messageSender,
                                        jsuccess);
    success = GetJArray(CallbackEnv(),jsuccess);
    ConsumerTable::Instance().DropReference(CallbackEnv(),messageSender);
}

void Callbacks::OnDropReferenceCb(void* const consumer,
                                  const long refCounter,
                                  bool& success)
{
    ConsumerTable::Instance().DropReference(CallbackEnv(),static_cast<jobject>(consumer),refCounter);
    success = true;
}



