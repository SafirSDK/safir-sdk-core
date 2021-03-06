/******************************************************************************
*
* Copyright Saab AB, 2009-2013 (http://safirsdkcore.com)
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
#ifndef __DOSE_JAVA_CALLBACKS_H__
#define __DOSE_JAVA_CALLBACKS_H__
#include <Safir/Dob/Internal/Interface.h>
#include <jni.h>

class Callbacks
{
public:
    //-------------------------------------------------------------------
    //Must be called before anything else. Set up all callbacks to Java.
    //-------------------------------------------------------------------
    static void InitCallbacks(JNIEnv* env);


    //-------------------------------
    //Callbacks passed to dose_dll
    //-------------------------------
    static void OnDispatchCb(void* const consumer,
                             bool& success);

    static void OnStopOrderCb(void* const consumer,
                              bool& success);

    static void OnNewEntityCb(const char* const currentBlob,
                              const char* const currentState,
                              void* const consumer,
                              const bool timestampDiff,
                              bool& success);

    static void OnUpdatedEntityCb(const char* const currentBlob,
                                  const char* const currentState,
                                  const char* const previousBlob,
                                  const char* const previousState,
                                  void* const consumer,
                                  const bool timestampDiff,
                                  bool& success);

    static void OnDeletedEntityCb(const char* const currentState,
                                  const char* const previousBlob,
                                  const char* const previousState,
                                  const bool explicitlyDeleted,
                                  void* const consumer,
                                  const bool timestampDiff,
                                  bool& success);

    static void OnCreateRequestCb(const char* const requestBlob,
                                  const char* const state,
                                  const DotsC_Int32 ctrl,
                                  const DotsC_Int32 responseId,
                                  void* const consumer,
                                  bool& success);

    static void OnUpdateRequestCb(const char* const requestBlob,
                                  const char* const state,
                                  const DotsC_Int32 ctrl,
                                  const DotsC_Int32 responseId,
                                  void* const consumer,
                                  bool& success);

    static void OnDeleteRequestCb(const char* const state,
                                  const DotsC_Int32 ctrl,
                                  const DotsC_Int32 responseId,
                                  void* const consumer,
                                  bool& success);

    static void OnServiceRequestCb(const char* const requestBlob,
                                   const char* const state,
                                   const DotsC_Int32 ctrl,
                                   const DotsC_Int32 responseId,
                                   void* const consumer,
                                   bool& success);

    static void OnResponseCb(const DoseC_RequestId requestId,
                             const char* const responseBlob,
                             const char* const responseState,
                             const char* const requestBlob,
                             const char* const requestState,
                             void* const consumer,
                             bool& success);

    static void OnMessageCb(const char* const message,
                            const char* const state,
                            void* const consumer,
                            bool& success);

    static void OnRegisteredCb(const DotsC_TypeId typeId,
                               const DotsC_Int64 handlerId,
                               const char* const handlerIdStr,
                               void* const consumer,
                               bool& success);

    static void OnUnregisteredCb(const DotsC_TypeId typeId,
                                 const DotsC_Int64 handlerId,
                                 const char* const handlerIdStr,
                                 void* const consumer,
                                 bool& success);

    static void OnRevokedRegistrationCb(const DotsC_TypeId typeId,
                                        const DotsC_Int64 handlerId,
                                        const char* const handlerIdStr,
                                        void* const consumer,
                                        bool& success);

    static void OnCompletedRegistrationCb(const DotsC_TypeId typeId,
                                          const DotsC_Int64 handlerId,
                                          const char* const handlerIdStr,
                                          void* const consumer,
                                          bool& success);

    static void OnInjectedNewEntityCb(const char* const injectionBlob,
                                      const char* const injectionState,
                                      void* const consumer,
                                      bool& success);

    static void OnInjectedUpdatedEntityCb(const char* const injectionBlob,
                                          const char* const injectionState,
                                          const char* const currentBlob,
                                          const char* const currentState,
                                          void* const consumer,
                                          bool& success);

    static void OnInjectedDeletedEntityCb(const char* const injectionState,
                                          const char* const currentBlob,
                                          const char* const currentState,
                                          void* const consumer,
                                          bool& success);

    static void OnInitialInjectionsDoneCb(const DotsC_TypeId typeId,
                                          const DotsC_Int64 handlerId,
                                          const char* const handlerIdStr,
                                          void* const consumer,
                                          bool& success);

    static void OnNotRequestOverflowCb(void* const consumer,
                                       bool& success);

    static void OnNotMessageOverflowCb(void* const consumer,
                                       bool& success);

    static void OnDropReferenceCb(void* const consumer,
                                  const DotsC_Int32 refCounter,
                                  bool& success);

private:
    static JNIEnv* CallbackEnv();
    static jclass GetCallbacksClass();
    //    static JNIEnv* DispatchThreadEnv();
    static bool m_dispatchThreadIsAttachedToJVM;

    static JavaVM * m_vm; //pointer to the Java Virtual Machine
    static jint m_version; //Java-version

    // Java CallbackHandler class method ids (the class itself has to be retrieved each time since there are several env:s).
    //the method ids seem to be possible to cache (supported by the Sun JNI documentation that describes caching them and between
    //the lines says not to worry about using them from different threads),
    //Through trial and error (mostly error) we've found that the class cannot be cached, but has to be retrieved for each thread/env.
    static jmethodID m_onDoDispatch;
    static jmethodID m_onStopOrder;
    static jmethodID m_onNewEntity;
    static jmethodID m_onUpdatedEntity;
    static jmethodID m_onDeletedEntity;
    static jmethodID m_onCreateRequest;
    static jmethodID m_onUpdateRequest;
    static jmethodID m_onDeleteRequest;
    static jmethodID m_onServiceRequest;
    static jmethodID m_onResponse;
    static jmethodID m_onRevokedRegistration;
    static jmethodID m_onCompletedRegistration;
    static jmethodID m_onMessage;
    static jmethodID m_onRegistered;
    static jmethodID m_onUnregistered;
    static jmethodID m_onInjectedNewEntity;
    static jmethodID m_onInjectedUpdatedEntity;
    static jmethodID m_onInjectedDeletedEntity;
    static jmethodID m_onInitialInjectionsDone;
    static jmethodID m_onNotRequestOverflow;
    static jmethodID m_onNotMessageOverflow;
};


#endif

