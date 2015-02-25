// -*- coding: utf-8 -*-
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
package com.saabgroup.safir.dob;

import com.saabgroup.safir.dob.internal.*;

/**
 * Java interface to dose_dll.
 * The dose Interface class is the raw interface to the C-world.
 */
final class Interface {

    static
    {
        System.loadLibrary("dose_java_jni");
    }

    //dose_java_jni Constructor
    static native void Constructor(int [] ctrl,
                                   boolean  [] success);

    //dose_java_jni Destructor
    static native void Destructor(int ctrl);

    //dose_java_jni IsConnected
    static native void IsConnected(int ctrl,
                                   boolean [] isConn,
                                   boolean [] success);

    //dose_java_jni Connect
    static native void Connect (int ctrl,
                                String connectionNameCommonPart,
                                String connectionNameInstancePart,
                                int context,
                                StopHandler stopHandler,
                                Dispatcher dispatcher,
                                boolean [] success);

    //dose_java_jni ConnectSecondary
    static native void ConnectSecondary (String connectionNameCommonPart,
                                         String connectionNameInstancePart,
                                         int [] ctrl,
                                         boolean [] success);

    //dose_java_jni Disconnect
    static native void Disconnect(int ctrl,
                                  boolean [] success);


    //dose_java_jni GetConnectionName
    static native void GetConnectionName(int ctrl,
                                         String name[],
                                         boolean [] success);

    //dose_java_jni GetConnectionNameCommonPart
    static native void GetConnectionNameCommonPart(int ctrl,
                                                   String name[],
                                                   boolean [] success);

    //dose_java_jni GetConnectionNameInstancePart
    static native void GetConnectionNameInstancePart(int ctrl,
                                                     String name[],
                                                     boolean [] success);

    //dose_java_jni RegisterServiceHandler
    static native void RegisterServiceHandler(int ctrl,
                                              long typeId,
                                              long handlerId,
                                              String handlerIdStr,
                                              boolean overrideRegistration,
                                              ConsumerBase consumer,
                                              boolean [] success);

    //dose_java_jni RegisterEntityHandler
    static native void RegisterEntityHandler(int ctrl,
                                             long typeId,
                                             long handlerId,
                                             String handlerIdStr,
                                             int instanceIdPolicy,
                                             boolean overrideRegistration,
                                             boolean injectionHandler,
                                             ConsumerBase consumer,
                                             boolean [] success);

    //dose_java_jni UnregisterHandler
    static native void UnregisterHandler(int ctrl,
                                         long typeId,
                                         long handlerId,
                                         String handlerIdStr,
                                         boolean [] success);

    //dose_java_jni SubscribeMessage
    static native void SubscribeMessage(int ctrl,
                                        long typeId,
                                        long channelId,
                                        String channelIdStr,
                                        boolean includeSubclasses,
                                        MessageSubscriber consumer,
                                        boolean [] success);

    //dose_java_jni UnsubscribeMessage
    static native void UnsubscribeMessage(int ctrl,
                                          long typeId,
                                          long channelId,
                                          String channelIdStr,
                                          boolean includeSubclasses,
                                          ConsumerBase consumer,
                                          boolean [] success);

    //dose_java_jni SubscribeEntity
    static native void SubscribeEntity(int ctrl,
                                       long typeId,
                                       long instanceId,
                                       String instanceIdStr,
                                       boolean allInstances,
                                       boolean includeUpdates,
                                       boolean includeSubclasses,
                                       boolean restartSubscription,
                                       ConsumerBase consumer,
                                       boolean [] success);

    //dose_java_jni InjectorSubscribeEntity
    static native void InjectorSubscribeEntity(int ctrl,
                                               long typeId,
                                               boolean includeUpdates,
                                               boolean includeSubclasses,
                                               boolean restartSubscription,
                                               boolean wantsGhostDelete,
                                               boolean wantsLastState,
                                               boolean doesntWantSourceIsPermanentStore,
                                               boolean wantsAllStateChanges,
                                               boolean timestampChangeInfo,
                                               ConsumerBase consumer,
                                               boolean [] success);

    //dose_java_jni UnsubscribeEntity
    static native void UnsubscribeEntity(int ctrl,
                                         long typeId,
                                         long instanceId,
                                         String instanceIdStr,
                                         boolean allInstances,
                                         boolean includeSubclasses,
                                         ConsumerBase consumer,
                                         boolean [] success);

    //dose_java_jni SubscribeRegistration
    static native void SubscribeRegistration(int ctrl,
                                             long typeId,
                                             long handlerId,
                                             String handlerIdStr,
                                             boolean includeSubclasses,
                                             boolean restartSubscription,
                                             ConsumerBase consumer,
                                             boolean [] success);

    //dose_java_jni UnsubscribeRegistration
    static native void UnsubscribeRegistration(int ctrl,
                                               long typeId,
                                               long handlerId,
                                               String handlerIdStr,
                                               boolean includeSubclasses,
                                               ConsumerBase consumer,
                                               boolean [] success);

    //dose_java_jni Dispatch
    static native void Dispatch(int ctrl,
                                boolean [] success);

    //dose_java_jni ExitDispatch
    static native void ExitDispatch(int ctrl,
                                    boolean [] success);


    //dose_java_jni GetCurrentCallbackId
    static native void GetCurrentCallbackId(int ctrl,
                                            int [] callbackId,
                                            boolean [] success);


    //dose_java_jni SendMessage
    static native void SendMessage(int ctrl,
                                   java.nio.ByteBuffer message,
                                   long channelId,
                                   String channelIdStr,
                                   ConsumerBase consumer,
                                   boolean [] success);

    //dose_java_jni ServiceRequest
    static native void ServiceRequest(int ctrl,
                                      java.nio.ByteBuffer request,
                                      long handlerId,
                                      String handlerIdStr,
                                      ConsumerBase consumer,
                                      int [] reqId,
                                      boolean [] success);

    //dose_java_jni CreateRequest
    static native void CreateRequest(int ctrl,
                                     java.nio.ByteBuffer request,
                                     boolean hasInstanceId,
                                     long instanceId,
                                     String instanceIdStr,
                                     long handlerId,
                                     String handlerIdStr,
                                     ConsumerBase consumer,
                                     int [] reqId,
                                     boolean [] success);

    //dose_java_jni UpdateRequest
    static native void UpdateRequest(int ctrl,
                                     java.nio.ByteBuffer request,
                                     long instanceId,
                                     String instanceIdStr,
                                     ConsumerBase consumer,
                                     int [] reqId,
                                     boolean [] success);

    //dose_java_jni DeleteRequest
    static native void DeleteRequest(int ctrl,
                                     long typeId,
                                     long instanceId,
                                     String instanceIdStr,
                                     ConsumerBase consumer,
                                     int [] reqId,
                                     boolean [] success);


    //dose_java_jni SendResponse
    static native void SendResponse(int ctrl,
                                    java.nio.ByteBuffer blob,
                                    ConsumerBase consumer,
                                    int responseId,
                                    boolean [] success);
    //dose_java_jni SetEntity
    static native void SetEntity(int ctrl,
                                 java.nio.ByteBuffer entity,
                                 long instanceId,
                                 String instanceIdStr,
                                 long handlerId,
                                 String handlerIdStr,
                                 boolean considerChangeFlags,
                                 boolean initialInjection,
                                 boolean [] success);

    //dose_java_jni DeleteEntity
    static native void DeleteEntity(int ctrl,
                                    long typeId,
                                    long instanceId,
                                    String instanceIdStr,
                                    boolean allInstances,
                                    long handlerId,
                                    String handlerIdStr,
                                    boolean [] success);

    //dose_java_jni InjectEntity
    static native void InjectEntity(int ctrl,
                                    java.nio.ByteBuffer entity,
                                    long instanceId,
                                    String instanceIdStr,
                                    long handlerId,
                                    String handlerIdStr,
                                    long timestamp,
                                    boolean [] success);

    //dose_java_jni InjectDeletedEntity
    static native void InjectDeletedEntity(int ctrl,
                                           long typeId,
                                           long instanceId,
                                           String instanceIdStr,
                                           long handlerId,
                                           String handlerIdStr,
                                           long timestamp,
                                           boolean [] success);

    //dose_java_jni ReadEntity
    static native void ReadEntity(int ctrl,
                                  long typeId,
                                  long instanceId,
                                  String instanceIdStr,
                                  java.nio.ByteBuffer [] currentBlob,
                                  java.nio.ByteBuffer [] currentState,
                                  boolean [] success);

    //dose_java_jni IsCreated
    static native void IsCreated(int ctrl,
                                 long typeId,
                                 long instanceId,
                                 String instanceIdStr,
                                 boolean [] isCreated,
                                 boolean [] success);

    //dose_java_jni GetNumberOfInstances
    static native void GetNumberOfInstances(int ctrl,
                                            long typeId,
                                            long handlerId,
                                            String handlerIdStr,
                                            boolean includeSubclasses,
                                            long [] numberOfInstances,
                                            boolean [] success);

    //dose_java_jni GetInstanceIdPolicy
    static native void GetInstanceIdPolicy(int ctrl,
                                           long typeId,
                                           long handlerId,
                                           int [] instanceIdPolicy,
                                           boolean [] success);

    //dose_java_jni Postpone
    static native void Postpone(int ctrl,
                                boolean redispatchCurrent,
                                boolean [] success);

    //dose_java_jni ResumePostponed
    static native void ResumePostponed(int ctrl,
                                       boolean [] success);

    //dose_java_jni IncompleteInjectionState
    static native void IncompleteInjectionState(int ctrl,
                                                boolean [] success);

    //dose_java_jni GetChannelId
    static native void GetChannelId(java.nio.ByteBuffer state,
                                    long [] channelId,
                                    boolean [] success);
    //dose_java_jni GetTypeId
    static native void GetTypeId(java.nio.ByteBuffer state,
                                 long [] typeId,
                                 boolean [] success);

    //dose_java_jni GetInstanceId
    static native void GetInstanceId(java.nio.ByteBuffer state,
                                     long [] instanceId,
                                     boolean [] success);

    //dose_java_jni GetHandlerId
    static native void GetHandlerId(java.nio.ByteBuffer state,
                                    long [] handlerId,
                                    boolean [] success);

    //    internal delegate void DoseC_BlobDeleter(ref java.nio.ByteBuffer ptr);

    //dose_java_jni GetConnectionInfo
    static native void GetConnectionInfo(java.nio.ByteBuffer state,
                                         java.nio.ByteBuffer [] blob,
                                         java.nio.ByteBuffer [] deleter,
                                         boolean [] success);

    //dose_java_jni GetTopTimestamp
    static native void GetTopTimestamp(java.nio.ByteBuffer state,
                                       long [] timestamp,
                                       boolean [] success);

    //dose_java_jni GetMemberTimestamp
    static native void GetMemberTimestamp(java.nio.ByteBuffer state,
                                          int member,
                                          long [] timestamp,
                                          boolean [] success);

    //dose_java_jni GetQueueCapacity
    static native void GetQueueCapacity(int ctrl,
                                        int queue,
                                        int [] queueCapacity,
                                        boolean [] success);
    //dose_java_jni GetQueueSize
    static native void GetQueueSize(int ctrl,
                                    int queue,
                                    int [] queueSize,
                                    boolean [] success);


    //dose_java_jni Diff
    static native void Diff(java.nio.ByteBuffer previousState,
                            java.nio.ByteBuffer currentState,
                            boolean wantCurrent,
                            boolean timestampDiff,
                            java.nio.ByteBuffer [] diffBlob,
                            java.nio.ByteBuffer [] deleter,
                            boolean [] success);


    public static void addReference(java.nio.ByteBuffer state){
        if (state != null) {
            AddReference(state);
        }
    }

    //dose_java_jni AddReference
    private static native void AddReference(java.nio.ByteBuffer state);


    public static void dropReference(java.nio.ByteBuffer state) {
        if (state != null){
            DropReference(state);
        }
    }
    //dose_java_jni DropReference
    private static native void DropReference(java.nio.ByteBuffer state);


    //dose_java_jni EntityIteratorCreate
    static native void EntityIteratorCreate(int ctrl,
                                            long typeId,
                                            boolean includeSubclasses,
                                            int [] iteratorId,
                                            boolean [] end,
                                            boolean [] success);

    //dose_java_jni EntityIteratorDestroy
    static native void EntityIteratorDestroy(int ctrl,
                                             int iteratorId);

    //dose_java_jni EntityIteratorIncrement
    static native void EntityIteratorIncrement(int ctrl,
                                               int iteratorId,
                                               boolean [] end,
                                               boolean [] success);

    //dose_java_jni EntityIteratorDereference
    static native void EntityIteratorDereference(int ctrl,
                                                 int iteratorId,
                                                 java.nio.ByteBuffer [] entityBlob,
                                                 java.nio.ByteBuffer [] entityState,
                                                 boolean [] success);



    //dose_java_jni SetAlwaysOverflowFlag
    static native void SimulateOverflows(int ctrl,
                                         boolean inQueues,
                                         boolean outQueues,
                                         boolean [] success);

    static native void InvokeDeleter(java.nio.ByteBuffer deleter,
                                     java.nio.ByteBuffer toDelete);

    //dose_java_jni GetContext
    static native void GetContext(int ctrl,
                                  int[] context,
                                  boolean[] success);

}
