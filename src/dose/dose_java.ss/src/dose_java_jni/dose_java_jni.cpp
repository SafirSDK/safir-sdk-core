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
#ifdef __GNUC__
#pragma GCC visibility push (default)
#endif

#include "com_saabgroup_safir_dob_Interface.h"

#ifdef __GNUC__
#pragma GCC visibility pop
#endif

#include <assert.h>
#include <iostream>
#include <Safir/Dob/Internal/Interface.h>
#include <Safir/Dob/Typesystem/Internal/Kernel.h>
#include "Callbacks.h"
#include <boost/shared_ptr.hpp>
#include <boost/bind.hpp>
#include "ConsumerTable.h"
#include <boost/current_function.hpp>

// ----------------------------------------------------------------------
// Routines for setting the "out" arrays.
//
//    use these to set the first element of an array
//    will assert on arraylength == 1
// ----------------------------------------------------------------------
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
               jintArray array,
               const DotsC_Int32 toValue)
{
    jboolean isCopy = false;
    jint * arrayElems = _env->GetIntArrayElements(array, &isCopy);
    assert(_env->GetArrayLength(array) == 1);
    arrayElems[0] = toValue;
    if (isCopy == JNI_TRUE)
    {
        _env->ReleaseIntArrayElements(array, arrayElems, 0);
    }
}


void SetJArray(JNIEnv * _env,
               jlongArray array,
               const DotsC_Int64 toValue)
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
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    Constructor
 * Signature: ([I[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_Constructor
(JNIEnv * env,
 jclass,
 jintArray _ctrl,
 jbooleanArray _success)
{
    DotsC_Int32 ctrl;
    bool success;
    DoseC_Constructor(ctrl, success);
    SetJArray(env,_ctrl,ctrl);
    SetJArray(env,_success,success);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    Destructor
 * Signature: (I)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_Destructor
(JNIEnv *,
 jclass,
 jint _ctrl)
{
    DoseC_Destructor(_ctrl);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    IsConnected
 * Signature: (I[Z[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_IsConnected
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jbooleanArray _isConnected,
 jbooleanArray _success)
{
    bool isConnected;
    bool success;
    DoseC_IsConnected(_ctrl, isConnected, success);
    SetJArray(env,_isConnected,isConnected);
    SetJArray(env,_success,success);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    Connect
 * Signature: (ILjava/lang/String;Ljava/lang/String;ILcom/saabgroup/safir/dob/StopHandler;Lcom/saabgroup/safir/dob/Dispatcher;[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_Connect
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jstring _connectionNameCommonPart,
 jstring _connectionNameInstancePart,
 jint _context,
 jobject _stopHandler,
 jobject _dispatcher,
 jbooleanArray _success)
{
    Callbacks::InitCallbacks(env);

    jobject stopHandler= NULL;
    if (_stopHandler != NULL)
    {
        stopHandler = ConsumerTable::Instance().AddReference(env,_stopHandler);
    }
    jobject dispatcher = ConsumerTable::Instance().AddReference(env,_dispatcher);

    bool success;
    DoseC_Connect(_ctrl,
                  GetUtf8(env,_connectionNameCommonPart).get(),
                  GetUtf8(env,_connectionNameInstancePart).get(),
                  _context,
                  DOSE_LANGUAGE_JAVA,
                  stopHandler,
                  dispatcher,
                  Callbacks::OnDispatchCb,
                  Callbacks::OnStopOrderCb,
                  Callbacks::OnNewEntityCb,
                  Callbacks::OnUpdatedEntityCb,
                  Callbacks::OnDeletedEntityCb,
                  Callbacks::OnCreateRequestCb,
                  Callbacks::OnUpdateRequestCb,
                  Callbacks::OnDeleteRequestCb,
                  Callbacks::OnServiceRequestCb,
                  Callbacks::OnResponseCb,
                  Callbacks::OnMessageCb,
                  Callbacks::OnRegisteredCb,
                  Callbacks::OnUnregisteredCb,
                  Callbacks::OnRevokedRegistrationCb,
                  Callbacks::OnCompletedRegistrationCb,
                  Callbacks::OnInjectedNewEntityCb,
                  Callbacks::OnInjectedUpdatedEntityCb,
                  Callbacks::OnInjectedDeletedEntityCb,
                  Callbacks::OnInitialInjectionsDoneCb,
                  Callbacks::OnNotRequestOverflowCb,
                  Callbacks::OnNotMessageOverflowCb,
                  Callbacks::OnDropReferenceCb,
                  success);

    if (!success)
    {
        if (_stopHandler != NULL)
        {
            ConsumerTable::Instance().DropReference(env,stopHandler);
        }
        ConsumerTable::Instance().DropReference(env,dispatcher);
    }
    SetJArray(env,_success,success);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    ConnectSecondary
 * Signature: (Ljava/lang/String;Ljava/lang/String;[I[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_ConnectSecondary
(JNIEnv * env,
 jclass,
 jstring _connectionNameCommonPart,
 jstring _connectionNameInstancePart,
 jintArray _ctrl,
 jbooleanArray _success)
{
    bool success;
    DotsC_Int32 newCtrlId;
    DoseC_ConnectSecondary(GetUtf8(env,_connectionNameCommonPart).get(),
                           GetUtf8(env,_connectionNameInstancePart).get(),
                           DOSE_LANGUAGE_JAVA,
                           Callbacks::OnNewEntityCb,
                           Callbacks::OnUpdatedEntityCb,
                           Callbacks::OnDeletedEntityCb,
                           Callbacks::OnCreateRequestCb,
                           Callbacks::OnUpdateRequestCb,
                           Callbacks::OnDeleteRequestCb,
                           Callbacks::OnServiceRequestCb,
                           Callbacks::OnResponseCb,
                           Callbacks::OnMessageCb,
                           Callbacks::OnRegisteredCb,
                           Callbacks::OnUnregisteredCb,
                           Callbacks::OnRevokedRegistrationCb,
                           Callbacks::OnCompletedRegistrationCb,
                           Callbacks::OnInjectedNewEntityCb,
                           Callbacks::OnInjectedUpdatedEntityCb,
                           Callbacks::OnInjectedDeletedEntityCb,
                           Callbacks::OnInitialInjectionsDoneCb,
                           Callbacks::OnNotRequestOverflowCb,
                           Callbacks::OnNotMessageOverflowCb,
                           Callbacks::OnDropReferenceCb,
                           newCtrlId,
                           success);
    SetJArray(env,_success,success);
    SetJArray(env,_ctrl,newCtrlId);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    Disconnect
 * Signature: (IZ[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_Disconnect
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jbooleanArray _success)
{
    bool success;
    DoseC_Disconnect(_ctrl, success);
    SetJArray(env,_success,success);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    GetConnectionName
 * Signature: (I[Ljava/lang/String;[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_GetConnectionName
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jobjectArray _name,
 jbooleanArray _success)
{
    const char * name;
    bool success;
    DoseC_GetConnectionName(_ctrl,
                            name,
                            success);
    SetJArray(env,_name,env->NewStringUTF(name));
    SetJArray(env,_success,success);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    GetConnectionNameCommonPart
 * Signature: (I[Ljava/lang/String;[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_GetConnectionNameCommonPart
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jobjectArray _name,
 jbooleanArray _success)
{
    const char * name;
    bool success;
    DoseC_GetConnectionNameCommonPart(_ctrl,
                                      name,
                                      success);
    SetJArray(env,_name,env->NewStringUTF(name));
    SetJArray(env,_success,success);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    GetConnectionNameInstancePart
 * Signature: (I[Ljava/lang/String;[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_GetConnectionNameInstancePart
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jobjectArray _name,
 jbooleanArray _success)
{
    const char * name;
    bool success;
    DoseC_GetConnectionNameInstancePart(_ctrl,
                                        name,
                                        success);
    SetJArray(env,_name,env->NewStringUTF(name));
    SetJArray(env,_success,success);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    RegisterServiceHandler
 * Signature: (IJJLjava/lang/String;ZLcom/saabgroup/safir/dob/internal/ConsumerBase;[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_RegisterServiceHandler
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jlong _typeId,
 jlong _handlerId,
 jstring _handlerIdStr,
 jboolean _overrideRegistration,
 jobject _consumer,
 jbooleanArray _success)
{

    bool success;
    jobject consumer = ConsumerTable::Instance().AddReference(env,_consumer);
    DoseC_RegisterServiceHandler(_ctrl,
                                 _typeId,
                                 _handlerId,
                                 GetUtf8(env,_handlerIdStr).get(),
                                 _overrideRegistration == JNI_TRUE,
                                 DOSE_LANGUAGE_JAVA,
                                 consumer,
                                 success);

    if (!success)
    {
        ConsumerTable::Instance().DropReference(env,consumer);
    }
    SetJArray(env,_success,success);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    RegisterEntityHandler
 * Signature: (IJJLjava/lang/String;IZZLcom/saabgroup/safir/dob/internal/ConsumerBase;[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_RegisterEntityHandler
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jlong _typeId,
 jlong _handlerId,
 jstring _handlerIdStr,
 jint _instanceIdPolicy,
 jboolean _overrideRegistration,
 jboolean _injectionHandler,
 jobject _consumer,
 jbooleanArray _success)
{
    bool success;
    jobject consumer = ConsumerTable::Instance().AddReference(env,_consumer);
    DoseC_RegisterEntityHandler(_ctrl,
                                _typeId,
                                _handlerId,
                                GetUtf8(env,_handlerIdStr).get(),
                                _instanceIdPolicy,
                                _overrideRegistration == JNI_TRUE,
                                _injectionHandler == JNI_TRUE,
                                DOSE_LANGUAGE_JAVA,
                                consumer,
                                success);

    if (!success)
    {
        ConsumerTable::Instance().DropReference(env,consumer);
    }
    SetJArray(env,_success,success);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    UnregisterHandler
 * Signature: (IJJLjava/lang/String;[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_UnregisterHandler
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jlong _typeId,
 jlong _handlerId,
 jstring _handlerIdStr,
 jbooleanArray _success)
{
    bool success;
    DoseC_UnregisterHandler(_ctrl,
                            _typeId,
                            _handlerId,
                            GetUtf8(env,_handlerIdStr).get(),
                            success);
    SetJArray(env,_success,success);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    SubscribeMessage
 * Signature: (IJJLjava/lang/String;ZLcom/saabgroup/safir/dob/MessageSubscriber;[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_SubscribeMessage
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jlong _typeId,
 jlong _channelId,
 jstring _channelIdStr,
 jboolean _includeSubclasses,
 jobject _consumer,
 jbooleanArray _success)
{
    bool success;
    jobject consumer = ConsumerTable::Instance().AddReference(env,_consumer);

    DoseC_SubscribeMessage(_ctrl,
                           _typeId,
                           _channelId,
                           GetUtf8(env,_channelIdStr).get(),
                           _includeSubclasses == JNI_TRUE,
                           DOSE_LANGUAGE_JAVA,
                           consumer,
                           success);
    if (!success)
    {
        ConsumerTable::Instance().DropReference(env,consumer);
    }
    SetJArray(env,_success,success);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    UnsubscribeMessage
 * Signature: (IJJLjava/lang/String;ZLcom/saabgroup/safir/dob/internal/ConsumerBase;[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_UnsubscribeMessage
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jlong _typeId,
 jlong _channelId,
 jstring _channelIdStr,
 jboolean _includeSubclasses,
 jobject _consumer,
 jbooleanArray _success)
{
    jobject consumer = ConsumerTable::Instance().GetReference(env,_consumer);

    bool success;
    DoseC_UnsubscribeMessage(_ctrl,
                             _typeId,
                             _channelId,
                             GetUtf8(env,_channelIdStr).get(),
                             _includeSubclasses == JNI_TRUE,
                             DOSE_LANGUAGE_JAVA,
                             consumer,
                             success);
    SetJArray(env,_success,success);
}


/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    SubscribeEntity
 * Signature: (IJJLjava/lang/String;ZZZZLcom/saabgroup/safir/dob/internal/ConsumerBase;[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_SubscribeEntity
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jlong _typeId,
 jlong _instanceId,
 jstring _instanceIdStr,
 jboolean _allInstances,
 jboolean _includeUpdates,
 jboolean _includeSubclasses,
 jboolean _restartSubscription,
 jobject _consumer,
 jbooleanArray _success)
{
    bool success;
    jobject consumer = ConsumerTable::Instance().AddReference(env,_consumer);

    DoseC_SubscribeEntity(_ctrl,
                          _typeId,
                          _instanceId,
                          GetUtf8(env,_instanceIdStr).get(),
                          _allInstances == JNI_TRUE,
                          _includeUpdates == JNI_TRUE,
                          _includeSubclasses == JNI_TRUE,
                          _restartSubscription == JNI_TRUE,
                          DOSE_LANGUAGE_JAVA,
                          consumer,
                          success);
    if (!success)
    {
        ConsumerTable::Instance().DropReference(env,consumer);
    }
    SetJArray(env,_success,success);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    InjectorSubscribeEntity
 * Signature: (IJZZZZZZZZLcom/saabgroup/safir/dob/internal/ConsumerBase;[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_InjectorSubscribeEntity
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jlong _typeId,
 jboolean _includeUpdates,
 jboolean _includeSubclasses,
 jboolean _restartSubscription,
 jboolean _wantsGhostDelete,
 jboolean _wantsLastState,
 jboolean _doesntWantSourceIsPermanentStore,
 jboolean _wantsAllStateChanges,
 jboolean _timestampChangeInfo,
 jobject _consumer,
 jbooleanArray _success)
{
    bool success;
    jobject consumer = ConsumerTable::Instance().AddReference(env,_consumer);

    DoseC_InjectorSubscribeEntity(_ctrl,
                                  _typeId,
                                  _includeUpdates == JNI_TRUE,
                                  _includeSubclasses == JNI_TRUE,
                                  _restartSubscription == JNI_TRUE,
                                  _wantsGhostDelete == JNI_TRUE,
                                  _wantsLastState == JNI_TRUE,
                                  _doesntWantSourceIsPermanentStore == JNI_TRUE,
                                  _wantsAllStateChanges == JNI_TRUE,
                                  _timestampChangeInfo == JNI_TRUE,
                                  DOSE_LANGUAGE_JAVA,
                                  consumer,
                                  success);
    if (!success)
    {
        ConsumerTable::Instance().DropReference(env,consumer);
    }
    SetJArray(env,_success,success);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    UnsubscribeEntity
 * Signature: (IJJLjava/lang/String;ZZLcom/saabgroup/safir/dob/internal/ConsumerBase;[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_UnsubscribeEntity
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jlong _typeId,
 jlong _instanceId,
 jstring _instanceIdStr,
 jboolean _allInstances,
 jboolean _includeSubclasses,
 jobject _consumer,
 jbooleanArray _success)
{
    bool success;
    jobject consumer = ConsumerTable::Instance().GetReference(env,_consumer);

    DoseC_UnsubscribeEntity(_ctrl,
                            _typeId,
                            _instanceId,
                            GetUtf8(env,_instanceIdStr).get(),
                            _allInstances == JNI_TRUE,
                            _includeSubclasses == JNI_TRUE,
                            DOSE_LANGUAGE_JAVA,
                            consumer,
                            success);
    SetJArray(env,_success,success);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    SubscribeRegistration
 * Signature: (IJJLjava/lang/String;ZZLcom/saabgroup/safir/dob/internal/ConsumerBase;[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_SubscribeRegistration
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jlong _typeId,
 jlong _handlerId,
 jstring _handlerIdStr,
 jboolean _includeSubclasses,
 jboolean _restartSubscription,
 jobject _consumer,
 jbooleanArray _success)
{
    bool success;
    jobject consumer = ConsumerTable::Instance().AddReference(env,_consumer);
    DoseC_SubscribeRegistration(_ctrl,
                                _typeId,
                                _handlerId,
                                GetUtf8(env,_handlerIdStr).get(),
                                _includeSubclasses == JNI_TRUE,
                                _restartSubscription == JNI_TRUE,
                                DOSE_LANGUAGE_JAVA,
                                consumer,
                                success);

    if (!success)
    {
        ConsumerTable::Instance().DropReference(env,consumer);
    }
    SetJArray(env,_success,success);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    UnsubscribeRegistration
 * Signature: (IJJLjava/lang/String;ZLcom/saabgroup/safir/dob/internal/ConsumerBase;[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_UnsubscribeRegistration
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jlong _typeId,
 jlong _handlerId,
 jstring _handlerIdStr,
 jboolean _includeSubclasses,
 jobject _consumer,
 jbooleanArray _success)
{
    bool success;
    jobject consumer = ConsumerTable::Instance().GetReference(env,_consumer);
    DoseC_UnsubscribeRegistration(_ctrl,
                                  _typeId,
                                  _handlerId,
                                  GetUtf8(env,_handlerIdStr).get(),
                                  _includeSubclasses == JNI_TRUE,
                                  DOSE_LANGUAGE_JAVA,
                                  consumer,
                                  success);

    SetJArray(env,_success,success);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    Dispatch
 * Signature: (I[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_Dispatch
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jbooleanArray _success)
{
    bool success;
    DoseC_Dispatch(_ctrl,
                   success);
    SetJArray(env,_success,success);
}


/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    ExitDispatch
 * Signature: (I[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_ExitDispatch
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jbooleanArray _success)
{
    bool success;
    DoseC_ExitDispatch(_ctrl,success);
    SetJArray(env,_success,success);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    GetCurrentCallbackId
 * Signature: (I[I[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_GetCurrentCallbackId
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jintArray _callbackId,
 jbooleanArray _success)
{
    bool success;
    DotsC_Int32 callbackId;
    DoseC_GetCurrentCallbackId(_ctrl,callbackId,success);
    SetJArray(env,_callbackId,callbackId);
    SetJArray(env,_success,success);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    SendMessage
 * Signature: (ILjava/nio/ByteBuffer;JLjava/lang/String;Lcom/saabgroup/safir/dob/internal/ConsumerBase;[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_SendMessage
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jobject _message,
 jlong _channelId,
 jstring _channelIdStr,
 jobject _consumer,
 jbooleanArray _success)
{
    bool success;

    jobject consumer = ConsumerTable::Instance().AddReference(env,_consumer);
    DoseC_SendMessage(_ctrl,
                      (const char*)env->GetDirectBufferAddress(_message),
                      _channelId,
                      GetUtf8(env,_channelIdStr).get(),
                      DOSE_LANGUAGE_JAVA,
                      consumer,
                      success);

    if (!success)
    {
        //For overflows release will be called in the callback
        DotsC_TypeId exceptionId;
        DotsC_PeekAtException(exceptionId);
        if (DotsC_TypeIdFromName("Safir.Dob.OverflowException") != exceptionId)
        {
            ConsumerTable::Instance().DropReference(env,_consumer);
        }
    }
    else
    {
        ConsumerTable::Instance().DropReference(env,_consumer);
    }
    SetJArray(env,_success,success);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    ServiceRequest
 * Signature: (ILjava/nio/ByteBuffer;JLjava/lang/String;Lcom/saabgroup/safir/dob/internal/ConsumerBase;[I[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_ServiceRequest
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jobject _request,
 jlong _handlerId,
 jstring _handlerIdStr,
 jobject _consumer,
 jintArray _reqId,
 jbooleanArray _success)
{
    bool success;
    DotsC_Int32 reqId;

    jobject consumer = ConsumerTable::Instance().AddReference(env,_consumer);
    DoseC_ServiceRequest(_ctrl,
                         (const char*)env->GetDirectBufferAddress(_request),
                         _handlerId,
                         GetUtf8(env,_handlerIdStr).get(),
                         DOSE_LANGUAGE_JAVA,
                         consumer,
                         reqId,
                         success);

    if (!success)
    {
        //For overflows release will be called in the callback
        DotsC_TypeId exceptionId;
        DotsC_PeekAtException(exceptionId);
        if (DotsC_TypeIdFromName("Safir.Dob.OverflowException") != exceptionId)
        {
            ConsumerTable::Instance().DropReference(env,_consumer);
        }
    }

    SetJArray(env,_success,success);
    SetJArray(env,_reqId,reqId);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    CreateRequest
 * Signature: (ILjava/nio/ByteBuffer;ZJLjava/lang/String;JLjava/lang/String;Lcom/saabgroup/safir/dob/internal/ConsumerBase;[I[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_CreateRequest
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jobject _request,
 jboolean _hasInstanceId,
 jlong _instanceId,
 jstring _instanceIdStr,
 jlong _handlerId,
 jstring _handlerIdStr,
 jobject _consumer,
 jintArray _reqId,
 jbooleanArray _success)
{
    bool success;
    DotsC_Int32 reqId;

    jobject consumer = ConsumerTable::Instance().AddReference(env,_consumer);
    DoseC_CreateRequest(_ctrl,
                        (const char*)env->GetDirectBufferAddress(_request),
                        _hasInstanceId == JNI_TRUE,
                        _instanceId,
                        GetUtf8(env,_instanceIdStr).get(),
                        _handlerId,
                        GetUtf8(env,_handlerIdStr).get(),
                        DOSE_LANGUAGE_JAVA,
                        consumer,
                        reqId,
                        success);

    if (!success)
    {
        //For overflows release will be called in the callback
        DotsC_TypeId exceptionId;
        DotsC_PeekAtException(exceptionId);
        if (DotsC_TypeIdFromName("Safir.Dob.OverflowException") != exceptionId)
        {
            ConsumerTable::Instance().DropReference(env,_consumer);
        }
    }

    SetJArray(env,_success,success);
    SetJArray(env,_reqId,reqId);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    UpdateRequest
 * Signature: (ILjava/nio/ByteBuffer;JLjava/lang/String;Lcom/saabgroup/safir/dob/internal/ConsumerBase;[I[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_UpdateRequest
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jobject _request,
 jlong _instanceId,
 jstring _instanceIdStr,
 jobject _consumer,
 jintArray _reqId,
 jbooleanArray _success)
{
    bool success;
    DotsC_Int32 reqId;

    jobject consumer = ConsumerTable::Instance().AddReference(env,_consumer);
    DoseC_UpdateRequest(_ctrl,
                        (const char*)env->GetDirectBufferAddress(_request),
                        _instanceId,
                        GetUtf8(env,_instanceIdStr).get(),
                        DOSE_LANGUAGE_JAVA,
                        consumer,
                        reqId,
                        success);

    if (!success)
    {
        //For overflows release will be called in the callback
        DotsC_TypeId exceptionId;
        DotsC_PeekAtException(exceptionId);
        if (DotsC_TypeIdFromName("Safir.Dob.OverflowException") != exceptionId)
        {
            ConsumerTable::Instance().DropReference(env,_consumer);
        }
    }

    SetJArray(env,_success,success);
    SetJArray(env,_reqId,reqId);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    DeleteRequest
 * Signature: (IJJLjava/lang/String;Lcom/saabgroup/safir/dob/internal/ConsumerBase;[I[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_DeleteRequest
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jlong _typeId,
 jlong _instanceId,
 jstring _instanceIdStr,
 jobject _consumer,
 jintArray _reqId,
 jbooleanArray _success)
{
    bool success;
    DotsC_Int32 reqId;

    jobject consumer = ConsumerTable::Instance().AddReference(env,_consumer);
    DoseC_DeleteRequest(_ctrl,
                        _typeId,
                        _instanceId,
                        GetUtf8(env,_instanceIdStr).get(),
                        DOSE_LANGUAGE_JAVA,
                        consumer,
                        reqId,
                        success);

    if (!success)
    {
        //For overflows release will be called in the callback
        DotsC_TypeId exceptionId;
        DotsC_PeekAtException(exceptionId);
        if (DotsC_TypeIdFromName("Safir.Dob.OverflowException") != exceptionId)
        {
            ConsumerTable::Instance().DropReference(env,_consumer);
        }
    }

    SetJArray(env,_success,success);
    SetJArray(env,_reqId,reqId);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    SendResponse
 * Signature: (ILjava/nio/ByteBuffer;Lcom/saabgroup/safir/dob/internal/ConsumerBase;I[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_SendResponse
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jobject _blob,
 jobject _consumer,
 jint _responseId,
 jbooleanArray _success)
{
    bool success;
    DoseC_SendResponse(_ctrl,
                       (const char*)env->GetDirectBufferAddress(_blob),
                       ConsumerTable::Instance().GetReference(env,_consumer),
                       DOSE_LANGUAGE_JAVA,
                       _responseId,
                       success);
    SetJArray(env,_success,success);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    SetEntity
 * Signature: (ILjava/nio/ByteBuffer;JLjava/lang/String;JLjava/lang/String;ZZ[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_SetEntity
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jobject _entity,
 jlong _instanceId,
 jstring _instanceIdStr,
 jlong _handlerId,
 jstring _handlerIdStr,
 jboolean _considerChangeFlags,
 jboolean _initialInjection,
 jbooleanArray _success)
{
    char * blob = static_cast<char*>(env->GetDirectBufferAddress(_entity));
    bool success;
    DoseC_SetEntity(_ctrl,
                    blob,
                    _instanceId,
                    GetUtf8(env,_instanceIdStr).get(),
                    _handlerId,
                    GetUtf8(env,_handlerIdStr).get(),
                    _considerChangeFlags == JNI_TRUE,
                    _initialInjection == JNI_TRUE,
                    success);
    SetJArray(env,_success,success);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    DeleteEntity
 * Signature: (IJJLjava/lang/String;ZJLjava/lang/String;[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_DeleteEntity
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jlong _typeId,
 jlong _instanceId,
 jstring _instanceIdStr,
 jboolean _allInstances,
 jlong _handlerId,
 jstring _handlerIdStr,
 jbooleanArray _success)
{
    bool success;
    DoseC_DeleteEntity(_ctrl,
                       _typeId,
                       _instanceId,
                       GetUtf8(env,_instanceIdStr).get(),
                       _allInstances == JNI_TRUE,
                       _handlerId,
                       GetUtf8(env,_handlerIdStr).get(),
                       success);
    SetJArray(env,_success,success);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    InjectEntity
 * Signature: (ILjava/nio/ByteBuffer;JLjava/lang/String;JLjava/lang/String;J[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_InjectEntity
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jobject _entity,
 jlong _instanceId,
 jstring _instanceIdStr,
 jlong _handlerId,
 jstring _handlerIdStr,
 jlong _timestamp,
 jbooleanArray _success)
{
    bool success;
    DoseC_InjectEntity(_ctrl,
                       (char*)env->GetDirectBufferAddress(_entity),
                       _instanceId,
                       GetUtf8(env,_instanceIdStr).get(),
                       _handlerId,
                       GetUtf8(env,_handlerIdStr).get(),
                       _timestamp,
                       success);
    SetJArray(env,_success,success);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    InjectDeletedEntity
 * Signature: (IJJLjava/lang/String;JLjava/lang/String;J[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_InjectDeletedEntity
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jlong _typeId,
 jlong _instanceId,
 jstring _instanceIdStr,
 jlong _handlerId,
 jstring _handlerIdStr,
 jlong _timestamp,
 jbooleanArray _success)
{
    bool success;
    DoseC_InjectDeletedEntity(_ctrl,
                              _typeId,
                              _instanceId,
                              GetUtf8(env,_instanceIdStr).get(),
                              _handlerId,
                              GetUtf8(env,_handlerIdStr).get(),
                              _timestamp,
                              success);
    SetJArray(env,_success,success);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    ReadEntity
 * Signature: (IJJLjava/lang/String;[Ljava/nio/ByteBuffer;[Ljava/nio/ByteBuffer;[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_ReadEntity
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jlong _typeId,
 jlong _instanceId,
 jstring _instanceIdStr,
 jobjectArray _currentBlob,
 jobjectArray _currentState,
 jbooleanArray _success)
{
    const char * currentBlob;
    const char * currentState;
    bool success;

    DoseC_ReadEntity(_ctrl,
                     _typeId,
                     _instanceId,
                     GetUtf8(env,_instanceIdStr).get(),
                     currentBlob,
                     currentState,
                     success);
    if (success)
    {
        SetJArray(env,_currentBlob,env->NewDirectByteBuffer((void*)currentBlob,DotsC_GetSize(currentBlob)));
        SetJArray(env,_currentState,env->NewDirectByteBuffer((void*)currentState,0)); //just passing a pointer
    }
    SetJArray(env,_success,success);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    IsCreated
 * Signature: (IJJLjava/lang/String;[Z[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_IsCreated
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jlong _typeId,
 jlong _instanceId,
 jstring _instanceIdStr,
 jbooleanArray _isCreated,
 jbooleanArray _success)
{
    bool isCreated;
    bool success;
    DoseC_IsCreated(_ctrl,_typeId,_instanceId,GetUtf8(env,_instanceIdStr).get(),isCreated,success);
    SetJArray(env,_isCreated,isCreated);
    SetJArray(env,_success,success);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    GetNumberOfInstances
 * Signature: (IJJLjava/lang/String;Z[J[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_GetNumberOfInstances
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jlong _typeId,
 jlong _handlerId,
 jstring _handlerIdStr,
 jboolean _includeSubclasses,
 jlongArray _numberOfInstances,
 jbooleanArray _success)
{
    bool success;
    DotsC_Int64 numberOfInstances;

    DoseC_GetNumberOfInstances(_ctrl,
                               _typeId,
                               _handlerId,
                               GetUtf8(env,_handlerIdStr).get(),
                               _includeSubclasses == JNI_TRUE,
                               numberOfInstances,
                               success);
    SetJArray(env,_numberOfInstances,numberOfInstances);
    SetJArray(env,_success,success);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    GetInstanceIdPolicy
 * Signature: (IJJ[I[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_GetInstanceIdPolicy
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jlong _typeId,
 jlong _handlerId,
 jintArray _instanceIdPolicy,
 jbooleanArray _success)
{
    bool success;
    DotsC_EnumerationValue instanceIdPolicy;
 
    DoseC_GetInstanceIdPolicy(_ctrl,
                              _typeId,
                              _handlerId,
                              instanceIdPolicy,
                              success);
    SetJArray(env,_instanceIdPolicy,instanceIdPolicy);
    SetJArray(env,_success,success);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    Postpone
 * Signature: (IZ[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_Postpone
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jboolean _redispatchCurrent,
 jbooleanArray _success)
{
    bool success;
    DoseC_Postpone(_ctrl,
                   _redispatchCurrent == JNI_TRUE,
                   success);
    SetJArray(env,_success,success);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    ResumePostponed
 * Signature: (I[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_ResumePostponed
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jbooleanArray _success)
{
    bool success;
    DoseC_ResumePostponed(_ctrl,
                          success);
    SetJArray(env,_success,success);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    IncompleteInjectionState
 * Signature: (I[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_IncompleteInjectionState
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jbooleanArray _success)
{
    bool success;
    DoseC_IncompleteInjectionState(_ctrl,
                                   success);
    SetJArray(env,_success,success);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    GetChannelId
 * Signature: (Ljava/nio/ByteBuffer;[J[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_GetChannelId
(JNIEnv * env,
 jclass,
 jobject _state,
 jlongArray _channelId,
 jbooleanArray _success)
{
    bool success;
    DotsC_Int64 channelId;
    DoseC_GetChannelId((const char *)env->GetDirectBufferAddress(_state),
                       channelId,
                       success);
    SetJArray(env,_success,success);
    SetJArray(env,_channelId,channelId);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    GetTypeId
 * Signature: (Ljava/nio/ByteBuffer;[J[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_GetTypeId
(JNIEnv * env,
 jclass,
 jobject _state,
 jlongArray _typeId,
 jbooleanArray _success)
{
    bool success;
    DotsC_Int64 typeId;
    DoseC_GetTypeId((const char *)env->GetDirectBufferAddress(_state),
                    typeId,
                    success);
    SetJArray(env,_success,success);
    SetJArray(env,_typeId,typeId);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    GetInstanceId
 * Signature: (Ljava/nio/ByteBuffer;[J[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_GetInstanceId
(JNIEnv * env,
 jclass,
 jobject _state,
 jlongArray _instanceId,
 jbooleanArray _success)
{
    bool success;
    DotsC_Int64 instanceId;
    DoseC_GetInstanceId((const char *)env->GetDirectBufferAddress(_state),
                        instanceId,
                        success);
    SetJArray(env,_success,success);
    SetJArray(env,_instanceId,instanceId);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    GetHandlerId
 * Signature: (Ljava/nio/ByteBuffer;[J[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_GetHandlerId
(JNIEnv * env,
 jclass,
 jobject _state,
 jlongArray _handlerId,
 jbooleanArray _success)
{
    bool success;
    DotsC_Int64 handlerId;
    DoseC_GetHandlerId((const char *)env->GetDirectBufferAddress(_state),
                       handlerId,
                       success);
    SetJArray(env,_success,success);
    SetJArray(env,_handlerId,handlerId);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    GetConnectionInfo
 * Signature: (Ljava/nio/ByteBuffer;[Ljava/nio/ByteBuffer;[Ljava/nio/ByteBuffer;[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_GetConnectionInfo
(JNIEnv * env,
 jclass,
 jobject _state,
 jobjectArray _blob,
 jobjectArray _deleter,
 jbooleanArray _success)
{
    char * blob;
    DoseC_BlobDeleter deleter;
    bool success;

    DoseC_GetConnectionInfo((char*)env->GetDirectBufferAddress(_state),
                            blob,
                            deleter,
                            success);
    if (success)
    {
        SetJArray(env,_blob,env->NewDirectByteBuffer((void*)blob,DotsC_GetSize(blob)));
        SetJArray(env,_deleter,env->NewDirectByteBuffer((void*)deleter,0)); //just passing ptr
    }
    SetJArray(env,_success,success);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    GetTopTimestamp
 * Signature: (Ljava/nio/ByteBuffer;[J[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_GetTopTimestamp
(JNIEnv * env,
 jclass,
 jobject _state,
 jlongArray _timestamp,
 jbooleanArray _success)
{
    bool success;
    DotsC_Int64 timestamp;
    DoseC_GetTopTimestamp((char*)env->GetDirectBufferAddress(_state),
                          timestamp,
                          success);
    SetJArray(env,_timestamp,timestamp);
    SetJArray(env,_success,success);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    GetMemberTimestamp
 * Signature: (Ljava/nio/ByteBuffer;I[J[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_GetMemberTimestamp
(JNIEnv * env,
 jclass,
 jobject _state,
 jint _member,
 jlongArray _timestamp,
 jbooleanArray _success)
{
    bool success;
    DotsC_Int64 timestamp;
    DoseC_GetMemberTimestamp((char*)env->GetDirectBufferAddress(_state),
                             _member,
                             timestamp,
                             success);
    SetJArray(env,_timestamp,timestamp);
    SetJArray(env,_success,success);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    GetQueueCapacity
 * Signature: (II[I[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_GetQueueCapacity
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jint _queue,
 jintArray _queueCapacity,
 jbooleanArray _success)
{
    bool success;
    DotsC_Int32 queueCapacity;
    DoseC_GetQueueCapacity(_ctrl,_queue,queueCapacity,success);
    SetJArray(env,_success,success);
    SetJArray(env,_queueCapacity,queueCapacity);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    GetQueueSize
 * Signature: (II[I[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_GetQueueSize
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jint _queue,
 jintArray _queueSize,
 jbooleanArray _success)
{
    bool success;
    DotsC_Int32 queueSize;
    DoseC_GetQueueSize(_ctrl,_queue,queueSize,success);
    SetJArray(env,_success,success);
    SetJArray(env,_queueSize,queueSize);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    Diff
 * Signature: (Ljava/nio/ByteBuffer;Ljava/nio/ByteBuffer;ZZ[Ljava/nio/ByteBuffer;[Ljava/nio/ByteBuffer;[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_Diff
(JNIEnv * env,
 jclass,
 jobject _previousState,
 jobject _currentState,
 jboolean _wantCurrent,
 jboolean _timestampDiff,
 jobjectArray _diffBlob,
 jobjectArray _deleter,
 jbooleanArray _success)
{
    char * diffBlob;
    DoseC_BlobDeleter deleter;
    bool success;
    DoseC_Diff((char*)env->GetDirectBufferAddress(_previousState),
               (char*)env->GetDirectBufferAddress(_currentState),
               _wantCurrent == JNI_TRUE,
               _timestampDiff == JNI_TRUE,
               diffBlob,
               deleter,
               success);
    if (success)
    {
        SetJArray(env, _diffBlob, env->NewDirectByteBuffer(diffBlob,DotsC_GetSize(diffBlob)));
        SetJArray(env, _deleter, env->NewDirectByteBuffer((void*)deleter,0)); //just pass a ptr
    }
    SetJArray(env, _success, success);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    AddReference
 * Signature: (Ljava/nio/ByteBuffer;)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_AddReference
(JNIEnv * env,
 jclass,
 jobject _state)
{
    DoseC_AddReference((char*)env->GetDirectBufferAddress(_state));
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    DropReference
 * Signature: (Ljava/nio/ByteBuffer;)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_DropReference
(JNIEnv * env,
 jclass,
 jobject _state)
{
    DoseC_DropReference((char*)env->GetDirectBufferAddress(_state));
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    EntityIteratorCreate
 * Signature: (IJZ[I[Z[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_EntityIteratorCreate
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jlong _typeId,
 jboolean _includeSubclasses,
 jintArray _iteratorId,
 jbooleanArray _end,
 jbooleanArray _success)
{
    DotsC_Int32 iteratorId;
    bool end;
    bool success;

    DoseC_EntityIteratorCreate(_ctrl,
                               _typeId,
                               _includeSubclasses == JNI_TRUE,
                               iteratorId,
                               end,
                               success);
    SetJArray(env,_iteratorId,iteratorId);
    SetJArray(env,_end,end);
    SetJArray(env,_success,success);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    EntityIteratorDestroy
 * Signature: (II)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_EntityIteratorDestroy
(JNIEnv *,
 jclass,
 jint _ctrl,
 jint _iteratorId)
{
    DoseC_EntityIteratorDestroy(_ctrl,_iteratorId);
}


/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    EntityIteratorIncrement
 * Signature: (II[Z[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_EntityIteratorIncrement
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jint _iteratorId,
 jbooleanArray _end,
 jbooleanArray _success)
{
    bool end;
    bool success;

    DoseC_EntityIteratorIncrement(_ctrl,
                                  _iteratorId,
                                  end,
                                  success);
    SetJArray(env,_end,end);
    SetJArray(env,_success,success);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    EntityIteratorDereference
 * Signature: (II[Ljava/nio/ByteBuffer;[Ljava/nio/ByteBuffer;[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_EntityIteratorDereference
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jint _iteratorId,
 jobjectArray _entityBlob,
 jobjectArray _entityState,
 jbooleanArray _success)
{
    const char * entityBlob;
    const char * entityState;
    bool success;

    DoseC_EntityIteratorDereference(_ctrl,
                                    _iteratorId,
                                    entityBlob,
                                    entityState,
                                    success);

    if (success)
    {
        SetJArray(env,_entityBlob,env->NewDirectByteBuffer((void*)entityBlob,DotsC_GetSize(entityBlob)));
        SetJArray(env,_entityState,env->NewDirectByteBuffer((void*)entityState,0)); //just passing a pointer
    }
    SetJArray(env,_success,success);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    SimulateOverflows
 * Signature: (IZZ[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_SimulateOverflows
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jboolean _inQueues,
 jboolean _outQueues,
 jbooleanArray _success)
{
    bool success;
    DoseC_SimulateOverflows(_ctrl, _inQueues == JNI_TRUE, _outQueues == JNI_TRUE, success);
    SetJArray(env,_success,success);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    InvokeDeleter
 * Signature: (Ljava/nio/ByteBuffer;Ljava/nio/ByteBuffer;)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_InvokeDeleter
(JNIEnv * env,
 jclass,
 jobject _deleter,
 jobject _toDelete)
{
    DoseC_BlobDeleter deleter = (DoseC_BlobDeleter)(env->GetDirectBufferAddress(_deleter));
    char * toDelete = static_cast<char*>(env->GetDirectBufferAddress(_toDelete));
    deleter(toDelete);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    GetContext
 * Signature: (I[I[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_GetContext
(JNIEnv * env,
 jclass,
 jint _ctrl,
 jintArray _context,
 jbooleanArray _success)
{
    bool success;
    DotsC_Int32 context;
    DoseC_GetContext(_ctrl,context,success);
    SetJArray(env,_success,success);
    SetJArray(env,_context,context);
}

/*
 * Class:     com_saabgroup_safir_dob_Interface
 * Method:    GetNodeId
 * Signature: ([J[Z)V
 */
void JNICALL Java_com_saabgroup_safir_dob_Interface_GetNodeId
(JNIEnv * env,
 jclass,
 jlongArray _nodeId,
 jbooleanArray _success)
{
    bool success;
    DotsC_Int64 nodeId;
    DoseC_GetNodeId(nodeId,success);
    SetJArray(env,_success,success);
    SetJArray(env,_nodeId,nodeId);
}
