// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2006-2009 (http://www.safirsdk.com)
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

class Consumer implements
                   com.saabgroup.safir.dob.MessageSubscriber,
                   com.saabgroup.safir.dob.MessageSender,
                   com.saabgroup.safir.dob.RegistrationSubscriber,
                   com.saabgroup.safir.dob.EntitySubscriber,
                   com.saabgroup.safir.dob.EntityHandler,
                   com.saabgroup.safir.dob.EntityHandlerInjection,
                   com.saabgroup.safir.dob.EntityHandlerPending,
                   com.saabgroup.safir.dob.ServiceHandler,
                   com.saabgroup.safir.dob.ServiceHandlerPending,
                   com.saabgroup.safir.dob.Requestor,
                   com.saabgroup.safir.application.Backdoor
{
    public static long instanceCount = 0;

    private final String PREFIX = "Consumer ";

    public Consumer(int consumerNumber,
                    String connectionName,
                    String instance)
    {
        //TODO: Interlocked.Increment(ref instanceCount);

        m_consumerNumber = consumerNumber;
        m_connectionName = connectionName;
        m_connectionInstance = instance;
        m_callbackActions = new java.util.EnumMap<com.saabgroup.safir.dob.CallbackId, java.util.Vector<com.saabgroup.dosetest.Action>>(com.saabgroup.safir.dob.CallbackId.class);
        for (com.saabgroup.safir.dob.CallbackId cb : com.saabgroup.safir.dob.CallbackId.values())
        {
            m_callbackActions.put(cb, new java.util.Vector<com.saabgroup.dosetest.Action>());
        }
        m_connection.attach(connectionName,instance);
    }

    /* TODO
       ~Consumer()
       {
       Interlocked.Decrement(ref instanceCount);
       }*/


    static boolean needBinaryCheck(com.saabgroup.safir.dob.typesystem.Object obj)
    {
        return
            obj.getTypeId() == com.saabgroup.dosetest.ComplexGlobalMessage.ClassTypeId ||
            obj.getTypeId() == com.saabgroup.dosetest.ComplexGlobalEntity.ClassTypeId ||
            obj.getTypeId() == com.saabgroup.dosetest.ComplexGlobalService.ClassTypeId;
    }

    //returns true if the blob needs to be modified.
    static boolean checkBinaryMemberInternal(com.saabgroup.safir.dob.typesystem.BinaryContainer cont)
    {
        if (!cont.isNull() && cont.getVal().length > 10000) //only check for large sizes
        {
            if (cont.getVal().length != 10 *1024 *1024)
            {
                Logger.instance().println("Binary is wrong size!");
            }
            else
            {
                byte val = 0;
                for (byte b : cont.getVal())
                {
                    if (b != val)
                    {
                        Logger.instance().println("Bad value in binary!");
                        break;
                    }
                    ++val;
                }
            }
            //we do NOT want to print all this out to stdout, so we set it to null once we've checked it.
            cont.setNull();
            return true;
        }
        else
        {
            return false;
        }
    }

    static String checkBinaryMember(com.saabgroup.safir.dob.typesystem.Object obj, java.nio.ByteBuffer blob)
    {
        if (obj.getTypeId() == com.saabgroup.dosetest.ComplexGlobalMessage.ClassTypeId)
        {
            if (checkBinaryMemberInternal(((com.saabgroup.dosetest.ComplexGlobalMessage)obj).binaryMember()))
            {
                int blobSize = com.saabgroup.safir.dob.typesystem.BlobOperations.getSize(blob);
                java.nio.ByteBuffer b = java.nio.ByteBuffer.allocateDirect(blobSize);

                //copy the blob
                for (int index = 0; index < blobSize; ++index)
                {
                    b.put(index,blob.get(index));

                }
                com.saabgroup.safir.dob.typesystem.BlobOperations.setNull
                    (b,com.saabgroup.dosetest.ComplexGlobalMessage.getBinaryMemberMemberIndex(),0);
                String xml = com.saabgroup.safir.dob.typesystem.Serialization.toXml(b);
                return xml;
            }
        }
        else if (obj.getTypeId() == com.saabgroup.dosetest.ComplexGlobalEntity.ClassTypeId)
        {
            java.nio.ByteBuffer b = null;
            if (checkBinaryMemberInternal(((com.saabgroup.dosetest.ComplexGlobalEntity)obj).binaryMember()))
            {
                int blobSize = com.saabgroup.safir.dob.typesystem.BlobOperations.getSize(blob);
                b = java.nio.ByteBuffer.allocateDirect(blobSize);

                //copy the blob
                for (int index = 0; index < blobSize; ++index)
                {
                    b.put(index,blob.get(index));

                }
                com.saabgroup.safir.dob.typesystem.BlobOperations.setNull
                    (b,com.saabgroup.dosetest.ComplexGlobalEntity.getBinaryMemberMemberIndex(),0);

            }


            //in the entity we use the binary array as well
            for (int i = 0; i < com.saabgroup.dosetest.ComplexGlobalEntity.getBinaryArrayMemberArraySize(); ++i)
            {
                if (checkBinaryMemberInternal(((com.saabgroup.dosetest.ComplexGlobalEntity)obj).binaryArrayMember().get(i)))
                {
                    if (b == null)
                    {
                        int blobSize = com.saabgroup.safir.dob.typesystem.BlobOperations.getSize(blob);
                        b = java.nio.ByteBuffer.allocateDirect(blobSize);

                        //copy the blob
                        for (int index = 0; index < blobSize; ++index)
                        {
                            b.put(index,blob.get(index));

                        }
                    }
                    com.saabgroup.safir.dob.typesystem.BlobOperations.setNull
                        (b,com.saabgroup.dosetest.ComplexGlobalEntity.getBinaryArrayMemberMemberIndex(),i);
                }
            }

            if (b != null)
            {
                String xml = com.saabgroup.safir.dob.typesystem.Serialization.toXml(b);
                return xml;
            }
        }
        else if (obj.getTypeId() == com.saabgroup.dosetest.ComplexGlobalService.ClassTypeId)
        {
            if (checkBinaryMemberInternal(((com.saabgroup.dosetest.ComplexGlobalService)obj).binaryMember()))
            {
                int blobSize = com.saabgroup.safir.dob.typesystem.BlobOperations.getSize(blob);
                java.nio.ByteBuffer b = java.nio.ByteBuffer.allocateDirect(blobSize);

                //copy the blob
                for (int index = 0; index < blobSize; ++index)
                {
                    b.put(index,blob.get(index));

                }
                com.saabgroup.safir.dob.typesystem.BlobOperations.setNull
                    (b,com.saabgroup.dosetest.ComplexGlobalService.getBinaryMemberMemberIndex(),0);
                String xml = com.saabgroup.safir.dob.typesystem.Serialization.toXml(b);
                return xml;
            }
        }
        return com.saabgroup.safir.dob.typesystem.Serialization.toXml(blob);

    }

    String CallbackId()
    {
        com.saabgroup.safir.dob.CallbackId cb = new com.saabgroup.safir.dob.ConnectionAspectMisc(m_connection).getCurrentCallbackId();
        return cb.toDouString();
    }


    public void addCallbackAction(com.saabgroup.dosetest.Action action)
    {
        m_callbackActions.get(action.actionCallback().getVal()).add(action);
    }


    public void executeCallbackActions(com.saabgroup.safir.dob.CallbackId callback)
    {
        for (com.saabgroup.dosetest.Action action : m_callbackActions.get(callback))
        {
            com.saabgroup.dosetest.ActionEnum actionKind = action.actionKind().getVal();

            executeAction(action);

            if (actionKind == com.saabgroup.dosetest.ActionEnum.RESET_CALLBACK_ACTIONS)
            {
               return;
            }

        }
    }



    private long getTimestamp(com.saabgroup.dosetest.Action action)
        throws com.saabgroup.safir.dob.NotFoundException,
               com.saabgroup.safir.dob.OverflowException
    {
        com.saabgroup.safir.dob.typesystem.EntityId entityId = new com.saabgroup.safir.dob.typesystem.EntityId
            (com.saabgroup.dosetest.LastInjectionTimestamp.ClassTypeId,
             new com.saabgroup.safir.dob.typesystem.InstanceId(com.saabgroup.dosetest.LastInjectionTimestamp.ClassTypeId));

        long delta = action.timestampDelta().getVal();
        com.saabgroup.safir.dob.EntityProxy ep = m_connection.read(entityId);
        try
        {
            com.saabgroup.dosetest.LastInjectionTimestamp ent = (com.saabgroup.dosetest.LastInjectionTimestamp)ep.getEntity();

            long newVal = ent.timestamp().getVal() + delta;

            ent.timestamp().setVal(newVal);

            m_connection.updateRequest(ent, entityId.getInstanceId(), m_timestampRequestor);

            return newVal;
        }
        finally {
            ep.dispose();
        }
    }

    public void executeAction(com.saabgroup.dosetest.Action action)
    {
        try
        {
            //only becomes true if RepeatUntilOverflow is true
            boolean repeat = !action.repeatUntilOverflow().isNull() && action.repeatUntilOverflow().getVal();

            java.util.Date actionStartTime = new java.util.Date();
            long repeats = 0;

            do //while repeat
            {
                try
                {
                    switch (action.actionKind().getVal())
                    {
                    case SEND_RESPONSE:
                        {
                            com.saabgroup.safir.dob.Response resp = (com.saabgroup.safir.dob.Response)action.object().getObj();
                            m_responseSender.send(resp);
                        }
                        break;
                    case DISCARD_RESPONSE_SENDER:
                        {
                            m_responseSender.discard();
                            m_responseSenderDiscarded = true;
                        }
                        break;
                    case REGISTER_ENTITY_HANDLER:
                        {
                            m_connection.registerEntityHandler
                                (action.typeId().getVal(),
                                 action.handler().getVal(),
                                 action.instanceIdPolicy().getVal(),
                                 this);
                            //save the instance id policy
                            m_instanceIdPolicyMap.put
                                (new Key(action.typeId().getVal(), action.handler().getVal()),
                                 new Pair(action.instanceIdPolicy().getVal(), action.handler().getVal().getRawValue()));

                        }
                        break;

                    case REGISTER_ENTITY_HANDLER_INJECTION:
                        {
                            m_connection.registerEntityHandlerInjection
                                (action.typeId().getVal(),
                                 action.handler().getVal(),
                                 action.instanceIdPolicy().getVal(),
                                 this);
                            //save the instance id policy
                            m_instanceIdPolicyMap.put
                                (new Key(action.typeId().getVal(), action.handler().getVal()),
                                 new Pair(action.instanceIdPolicy().getVal(), action.handler().getVal().getRawValue()));

                        }
                        break;


                    case REGISTER_ENTITY_HANDLER_PENDING:
                        {
                            m_connection.registerEntityHandlerPending
                                (action.typeId().getVal(),
                                 action.handler().getVal(),
                                 action.instanceIdPolicy().getVal(),
                                 this);

                            //save the instance id policy
                            m_instanceIdPolicyMap.put
                                (new Key(action.typeId().getVal(), action.handler().getVal()),
                                 new Pair(action.instanceIdPolicy().getVal(), action.handler().getVal().getRawValue()));

                        }
                        break;

                    case REGISTER_SERVICE_HANDLER:
                        {
                            m_connection.registerServiceHandler(action.typeId().getVal(),
                                                                action.handler().getVal(),
                                                                this);

                        }
                        break;

                    case REGISTER_SERVICE_HANDLER_PENDING:
                        {
                            m_connection.registerServiceHandlerPending(action.typeId().getVal(),
                                                                       action.handler().getVal(),
                                                                       this);

                        }
                        break;

                    case UNREGISTER_HANDLER:
                        {
                            m_connection.unregisterHandler(action.typeId().getVal(),
                                                           action.handler().getVal());

                        }
                        break;
                    case SUBSCRIBE_MESSAGE:
                        {
                            m_connection.subscribeMessage(action.typeId().getVal(),
                                                          action.channel().getVal(),
                                                          action.includeSubclasses().getVal(),
                                                          this);

                        }
                        break;
                    case UNSUBSCRIBE_MESSAGE:
                        {
                            m_connection.unsubscribeMessage(action.typeId().getVal(),
                                                            action.channel().getVal(),
                                                            action.includeSubclasses().getVal(),
                                                            this);
                        }
                        break;

                    case SUBSCRIBE_ENTITY:
                        {
                            if (!action.typeId().isNull())
                            {
                                m_connection.subscribeEntity(action.typeId().getVal(),
                                                             action.includeUpdates().getVal(),
                                                             action.includeSubclasses().getVal(),
                                                             action.restartSubscription().getVal(),
                                                             this);
                            }
                            else
                            {
                                m_connection.subscribeEntity(action.entityId().getVal(),
                                                             action.includeUpdates().getVal(),
                                                             action.restartSubscription().getVal(),
                                                             this);
                            }
                        }
                        break;

                    case INJECTOR_SUBSCRIBE_ENTITY:
                        {
                            new com.saabgroup.safir.dob.ConnectionAspectInjector(m_connection).subscribeEntity
                                (action.typeId().getVal(),
                                 action.includeUpdates().getVal(),
                                 action.includeSubclasses().getVal(),
                                 action.restartSubscription().getVal(),
                                 action.wantsGhostDelete().getVal(),
                                 action.wantsLastState().getVal(),
                                 action.doesntWantSourceIsPermanentStore().getVal(),
                                 action.wantsAllStateChanges().getVal(),
                                 action.timestampChangeInfo().getVal(),
                                 this);
                        }
                        break;

                    case UNSUBSCRIBE_ENTITY:
                        {
                            if (action.typeId().isNull())
                            {
                                m_connection.unsubscribeEntity(action.entityId().getVal(), this);
                            }
                            else
                            {
                                m_connection.unsubscribeEntity(action.typeId().getVal(), action.includeSubclasses().getVal(),this);
                            }
                        }
                        break;

                    case SUBSCRIBE_REGISTRATION:
                        {
                            m_connection.subscribeRegistration(action.typeId().getVal(),
                                                               action.handler().getVal(),
                                                               action.includeSubclasses().getVal(),
                                                               action.restartSubscription().getVal(),
                                                               this);
                        }
                        break;
                    case UNSUBSCRIBE_REGISTRATION:
                        {
                            m_connection.unsubscribeRegistration(action.typeId().getVal(),
                                                                 action.handler().getVal(),
                                                                 action.includeSubclasses().getVal(),
                                                                 this);
                        }
                        break;
                    case SEND_MESSAGE:
                        {
                            m_connection.send((com.saabgroup.safir.dob.Message)action.object().getObj(),
                                              action.channel().getVal(),
                                              this);
                        }
                        break;

                    case SERVICE_REQUEST:
                        {
                            m_connection.serviceRequest
                                ((com.saabgroup.safir.dob.Service)action.object().getObj(),
                                 action.handler().getVal(),
                                 this);
                        }
                        break;
                    case CREATE_REQUEST:
                        {
                            if (!action.instance().isNull())
                            {
                                m_connection.createRequest
                                    ((com.saabgroup.safir.dob.Entity)action.object().getObj(),
                                     action.instance().getVal(),
                                     action.handler().getVal(),
                                     this);
                            }
                            else
                            {
                                m_connection.createRequest
                                    ((com.saabgroup.safir.dob.Entity)action.object().getObj(),
                                     action.handler().getVal(),
                                     this);
                            }
                        }
                        break;
                    case UPDATE_REQUEST:
                        {
                            m_connection.updateRequest
                                ((com.saabgroup.safir.dob.Entity)action.object().getObj(),
                                 action.instance().getVal(),
                                 this);
                        }
                        break;
                    case DELETE_REQUEST:
                        {
                            m_connection.deleteRequest
                                (action.entityId().getVal(),
                                 this);
                        }
                        break;

                    case SET_ALL:
                        {
                            m_connection.setAll((com.saabgroup.safir.dob.Entity)action.object().getObj(),
                                                action.instance().getVal(),
                                                action.handler().getVal());
                        }
                        break;

                    case INITIAL_SET:
                        {
                            new com.saabgroup.safir.dob.ConnectionAspectInjector(m_connection).initialSet
                                ((com.saabgroup.safir.dob.Entity)action.object().getObj(), action.instance().getVal(), action.handler().getVal());
                        }
                        break;

                    case SET_CHANGES:
                        {
                            m_connection.setChanges((com.saabgroup.safir.dob.Entity)action.object().getObj(),
                                                    action.instance().getVal(), action.handler().getVal());
                        }
                        break;

                    case INJECT_CHANGES:
                        {
                            new com.saabgroup.safir.dob.ConnectionAspectInjector(m_connection).injectChanges
                                ((com.saabgroup.safir.dob.Entity)action.object().getObj(),
                                 action.instance().getVal(),
                                 getTimestamp(action),
                                 action.handler().getVal());
                        }
                        break;

                    case DELETE:
                        {
                            m_connection.delete(action.entityId().getVal(), action.handler().getVal());
                        }
                        break;

                    case INJECT_DELETE:
                        {
                            new com.saabgroup.safir.dob.ConnectionAspectInjector(m_connection).
                                injectDelete(action.entityId().getVal(),
                                             getTimestamp(action),
                                             action.handler().getVal());
                        }
                        break;

                    case POSTPONE:
                        {
                            new com.saabgroup.safir.dob.ConnectionAspectPostpone(m_connection).
                                postpone(action.redispatchCurrent().getVal());
                        }
                        break;

                    case RESUME_POSTPONED:
                        {
                            new com.saabgroup.safir.dob.ConnectionAspectPostpone(m_connection).
                                resumePostponed();
                        }
                        break;

                    case INCOMPLETE_INJECTION_STATE:
                        {
                            new com.saabgroup.safir.dob.ConnectionAspectPostpone(m_connection).
                                incompleteInjectionState();
                        }
                        break;

                    case DELETE_ALL_INSTANCES:
                        {
                            m_connection.deleteAllInstances(action.typeId().getVal(),action.handler().getVal());
                        }
                        break;

                    case GET_ENTITY_ITERATOR:
                        {
                            Logger.instance().println(PREFIX + m_consumerNumber + ": "
                                                      + "Iterating over entities of type "
                                                      + com.saabgroup.safir.dob.typesystem.Operations.getName(action.typeId().getVal())
                                                      + ":");
                            com.saabgroup.safir.dob.EntityIterator iterator = m_connection.getEntityIterator(action.typeId().getVal(),action.includeSubclasses().getVal());
                            try {
                                while(iterator.hasNext()) {
                                    com.saabgroup.safir.dob.EntityProxy entityProxy = iterator.next();
                                    Logger.instance().println("  EntityId  = " + entityProxy.getEntityId() + ":\n"
                                                              + "     Owner     = " + entityProxy.getOwner() + "\n"
                                                              + "     OwnerConn = " + connInfoToXml(entityProxy.getOwnerConnectionInfo()) + "\n"
                                                              + "     OwnerStr  = " + entityProxy.getOwnerWithStringRepresentation() + "\n"
                                                              + "     Entity    = " + com.saabgroup.safir.dob.typesystem.Serialization.toXml(entityProxy.getBlob()));

                                }
                                Logger.instance().println();
                            }
                            finally {
                                iterator.dispose();
                            }
                        }
                        break;

                    case READ:
                        {
                            Logger.instance().println(PREFIX + m_consumerNumber + ": "
                                                      + "Read entity "
                                                      + action.entityId().getVal()
                                                      + ":");

                            com.saabgroup.safir.dob.EntityProxy entityProxy =
                                m_connection.read(action.entityId().getVal());
                            try {
                                Logger.instance().println("  EntityId  = " + entityProxy.getEntityId() + ":\n"
                                                          + "  Owner     = " + entityProxy.getOwner() + "\n"
                                                          + "  OwnerConn = " + connInfoToXml(entityProxy.getOwnerConnectionInfo()) + "\n"
                                                          + "  OwnerStr  = " + entityProxy.getOwnerWithStringRepresentation() + "\n"
                                                          + "  Entity    = " + com.saabgroup.safir.dob.typesystem.Serialization.toXml(entityProxy.getBlob()));
                                Logger.instance().println();
                            }
                            finally {
                                entityProxy.dispose();
                            }
                        }
                        break;

                    case SIMULATE_OVERFLOWS:
                        {
                            Logger.instance().println(PREFIX + m_consumerNumber + ": "
                                                      + "SimulateOverflows("
                                                      + action.inQueues().getVal()+ ", "
                                                      + action.outQueues().getVal() + ")");

                            new com.saabgroup.safir.dob.ConnectionAspectMisc(m_connection).simulateOverflows
                                (action.inQueues().getVal(),
                                 action.outQueues().getVal());
                        }
                        break;

                    case IS_CREATED:
                        {
                            Logger.instance().println(PREFIX + m_consumerNumber + ": "
                                                      + "The instance "
                                                      + action.entityId().getVal()
                                                      + " is "
                                                      + (m_connection.isCreated(action.entityId().getVal()) ? "" : "not ")
                                                      + "created.");
                        }
                        break;

                    case GET_NUMBER_OF_INSTANCES:
                        {
                            Logger.instance().println(PREFIX + m_consumerNumber + ": "
                                                      + "GetNumberOfInstances (type = "
                                                      + com.saabgroup.safir.dob.typesystem.Operations.getName(action.typeId().getVal())
                                                      + ", handler = " + action.handler().getVal()
                                                      + ", includeSubclasses = " + action.includeSubclasses().getVal().toString()
                                                      + "): "
                                                      + m_connection.getNumberOfInstances(action.typeId().getVal(),
                                                                                          action.handler().getVal(),
                                                                                          action.includeSubclasses().getVal()));
                        }
                        break;

                    case GET_INSTANCE_ID_POLICY:
                        {
                            Logger.instance().println(PREFIX + m_consumerNumber + ": "
                                                      + "GetInstanceIdPolicy (type = "
                                                      + com.saabgroup.safir.dob.typesystem.Operations.getName(action.typeId().getVal())
                                                      + ", handler = " + action.handler().getVal()
                                                      + "): "
                                                      + com.saabgroup.safir.dob.typesystem.Operations.getEnumerationValueName(
                                                        com.saabgroup.safir.dob.InstanceIdPolicy.EnumerationId,
                                                        (m_connection.GetInstanceIdPolicy(action.typeId().getVal(),
                                                                                          action.handler().getVal()).ordinal())));
                        }
                        break;

                    case GET_QUEUE_CAPACITY:
                        {
                            Logger.instance().println(PREFIX + m_consumerNumber + ": "
                                                      + "The capacity of "
                                                      + action.connectionQueueId().getVal().toDouString()
                                                      + " is "
                                                      + new com.saabgroup.safir.dob.ConnectionAspectMisc(m_connection).getQueueCapacity(action.connectionQueueId().getVal()));
                        }
                        break;

                    case GET_QUEUE_SIZE:
                        {
                            Logger.instance().println(PREFIX + m_consumerNumber + ": "
                                                      + "The size of "
                                                      + action.connectionQueueId().getVal().toDouString()
                                                      + " is "
                                                      + new com.saabgroup.safir.dob.ConnectionAspectMisc(m_connection).getQueueSize(action.connectionQueueId().getVal()));
                        }
                        break;

                    case GET_CONTEXT:
                        {
                            Logger.instance().println(PREFIX + m_consumerNumber + ": "
                                                      + "The test connection is opened in context "
                                                      + new com.saabgroup.safir.dob.ConnectionAspectMisc(m_connection).getContext());
                        }
                        break;

                    case RESET_CALLBACK_ACTIONS:
                        {
                            Logger.instance().println(PREFIX + m_consumerNumber + ": ResetCallbackActions");

                            for (com.saabgroup.safir.dob.CallbackId callback: com.saabgroup.safir.dob.CallbackId.values()) {
                                m_callbackActions.get(callback).clear();
                            }
                        }
                        break;

                    case START_BACKDOOR:
                        {
                            Logger.instance().println(PREFIX + m_consumerNumber + ": StartBackdoor");

                            m_backdoorKeeper.start(this, m_connectionName, m_connectionInstance);
                        }
                        break;

                    case STOP_BACKDOOR:
                        {
                            Logger.instance().println(PREFIX + m_consumerNumber + ": StopBackdoor");

                            m_backdoorKeeper.stop();
                        }
                        break;

                    case IS_BACKDOOR_STARTED:
                        {
                            Logger.instance().println(PREFIX + m_consumerNumber +
                                    ": The backdoor is " +
                                    (m_backdoorKeeper.isStarted() ? "" : "not ") +
                                    "started");
                        }
                        break;

                    default:
                        Logger.instance().println(PREFIX + m_consumerNumber + ": "
                                                  + "No handler defined for action "
                                                  + action.actionKind().getVal());
                        break;
                    }
                    ++repeats;

                    if (repeats % 1000 == 0)
                    {
                        System.out.println("I've now done " + repeats  + " repeats without an overflow!");
                    }
                }
                catch (com.saabgroup.safir.dob.OverflowException e)
                {
                    Logger.instance().println("Caught Overflow exception");

                    if (repeat)
                    {
                        double secs = ((new java.util.Date()).getTime() - actionStartTime.getTime()) / 1000.0;
                        System.out.println("Time elapsed before I got an overflow was " + secs);
                        System.out.println("I managed to send "+ repeats + " times");
                    }

                    repeat = false;
                }
            }
            while (repeat);
        }
        catch (com.saabgroup.safir.dob.typesystem.Exception exc)
        {
            Logger.instance().println("Caught Exception in ExecuteAction: " +
                                      com.saabgroup.safir.dob.typesystem.Operations.getName(exc.getTypeId()));

            System.out.println("Exception info: " + exc);
        }
        catch (com.saabgroup.safir.dob.typesystem.FundamentalException exc)
        {
            Logger.instance().println("Caught FundamentalException in ExecuteAction: " +
                                      com.saabgroup.safir.dob.typesystem.Operations.getName(exc.getTypeId()));
            System.out.println("Exception info: " + exc);
        }
    }

    //
    // MessageSubscriber Members
    //

    public void onMessage(com.saabgroup.safir.dob.MessageProxy messageProxy)
    {
        m_connection.exitDispatch();
        executeCallbackActions(com.saabgroup.safir.dob.CallbackId.ON_MESSAGE);

        com.saabgroup.dosetest.RootMessage msg = (com.saabgroup.dosetest.RootMessage)messageProxy.getMessage();
        String xml;

        if (needBinaryCheck(msg))
        {
            xml = checkBinaryMember(msg,messageProxy.getBlob());
        }
        else
        {
            xml = com.saabgroup.safir.dob.typesystem.Serialization.toXml(messageProxy.getBlob());
        }

        Logger.instance().println
            (PREFIX + m_consumerNumber + ": "
             + CallbackId() + ":\n"
             + "  Type       = " + com.saabgroup.safir.dob.typesystem.Operations.getName(messageProxy.getTypeId()) + "\n"
             + "  ChannelId  = " + messageProxy.getChannelId() + "\n"
             + "  Sender     = " + connInfoToXml(messageProxy.getSenderConnectionInfo()) + "\n"
             + "  ChannelId  = " + messageProxy.getChannelIdWithStringRepresentation() + "\n"
             + "  Message    = " + xml + "\n\n");
    }



    //
    // EntitySubscriber Members
    //

    public void onNewEntity(com.saabgroup.safir.dob.EntityProxy entityProxy)
    {
        m_connection.exitDispatch();
        executeCallbackActions(com.saabgroup.safir.dob.CallbackId.ON_NEW_ENTITY);

        com.saabgroup.safir.dob.Entity entity = entityProxy.getEntityWithChangeInfo();
        String xml;

        if (needBinaryCheck(entity))
        {
            xml = checkBinaryMember(entity,entityProxy.getBlob());
        }
        else
        {
            xml = com.saabgroup.safir.dob.typesystem.Serialization.toXml(entityProxy.getBlob());
        }


        Logger.instance().println
            (PREFIX + m_consumerNumber + ": "
             + CallbackId() + ":\n"
             + "  EntityId  = " + entityProxy.getEntityId() + "\n"
             + "  Owner     = " + entityProxy.getOwner() + "\n"
             + "  OwnerConn = " + connInfoToXml(entityProxy.getOwnerConnectionInfo()) + "\n"
             + "  OwnerStr  = " + entityProxy.getOwnerWithStringRepresentation() + "\n"
             + "  Entity    = " + xml + "\n"
             + "  Changed top-level members: ");

        for (int i = 0;
             i < com.saabgroup.safir.dob.typesystem.Members.getNumberOfMembers(entity.getTypeId());
             ++i)
        {
            if (entity.getMember(i, 0).isChanged())
            {
                Logger.instance().println("    "
                                          + com.saabgroup.safir.dob.typesystem.Members.getName(entity.getTypeId(), i));
            }
        }
        Logger.instance().println();
    }

    public void onUpdatedEntity(com.saabgroup.safir.dob.EntityProxy entityProxy)
    {
        m_connection.exitDispatch();
        executeCallbackActions(com.saabgroup.safir.dob.CallbackId.ON_UPDATED_ENTITY);

        com.saabgroup.safir.dob.Entity entity = entityProxy.getEntityWithChangeInfo();
        String xml;

        if (needBinaryCheck(entity))
        {
            xml = checkBinaryMember(entity,entityProxy.getBlob());
        }
        else
        {
            xml = com.saabgroup.safir.dob.typesystem.Serialization.toXml(entityProxy.getBlob());
        }

        com.saabgroup.safir.dob.Entity prevEntity = entityProxy.getPrevious().getEntity();
        String prevXml;

        if (needBinaryCheck(prevEntity))
        {
            prevXml = checkBinaryMember(prevEntity,entityProxy.getPrevious().getBlob());
        }
        else
        {
            prevXml = com.saabgroup.safir.dob.typesystem.Serialization.toXml(entityProxy.getPrevious().getBlob());
        }

        Logger.instance().println
            (PREFIX + m_consumerNumber + ": "
             + CallbackId() + ":\n"
             + "  EntityId  = " + entityProxy.getEntityId() + "\n"
             + "  Owner     = " + entityProxy.getOwner() + "\n"
             + "  OwnerConn = " + connInfoToXml(entityProxy.getOwnerConnectionInfo()) + "\n"
             + "  OwnerStr  = " + entityProxy.getOwnerWithStringRepresentation() + "\n"
             + "  Entity    = " + xml + "\n"
             + "  Previous  = " + prevXml + "\n"
             + "  Changed top-level members: ");

        for (int i = 0;
             i < com.saabgroup.safir.dob.typesystem.Members.getNumberOfMembers(entity.getTypeId());
             ++i)
        {
            if (entity.getMember(i, 0).isChanged())
            {
                Logger.instance().println("    "
                                          + com.saabgroup.safir.dob.typesystem.Members.getName(entity.getTypeId(), i));
            }
        }
        Logger.instance().println();
    }

    public void onDeletedEntity(com.saabgroup.safir.dob.EntityProxy entityProxy, boolean deletedByOwner)
    {
        m_connection.exitDispatch();
        executeCallbackActions(com.saabgroup.safir.dob.CallbackId.ON_DELETED_ENTITY);

        Logger.instance().println
            (PREFIX + m_consumerNumber + ": "
             + CallbackId() + ":\n"
             + "  EntityId       = " + entityProxy.getEntityId() + "\n"
             + "  deletedByOwner = " + deletedByOwner + "\n"
             + "  Owner          = " + entityProxy.getOwner() + "\n"
             + "  OwnerStr  = " + entityProxy.getOwnerWithStringRepresentation() + "\n"
             + "  Previous  = " + com.saabgroup.safir.dob.typesystem.Serialization.toXml(entityProxy.getPrevious().getBlob()));

        Logger.instance().println();
    }




    //
    // RegistrationSubscriber Members
    //

    public void onRegistered(long typeId, com.saabgroup.safir.dob.typesystem.HandlerId handlerId)
    {
        m_connection.exitDispatch();
        executeCallbackActions(com.saabgroup.safir.dob.CallbackId.ON_REGISTERED);

        Logger.instance().println(PREFIX + m_consumerNumber + ": "
                                  + CallbackId() + ":\n"
                                  + "  Type      = " + com.saabgroup.safir.dob.typesystem.Operations.getName(typeId) +"\n"
                                  + "  HandlerId = " + handlerId);

        Logger.instance().println();
    }

    public void onUnregistered(long typeId, com.saabgroup.safir.dob.typesystem.HandlerId handlerId)
    {
        m_connection.exitDispatch();
        executeCallbackActions(com.saabgroup.safir.dob.CallbackId.ON_UNREGISTERED);

        Logger.instance().println(PREFIX + m_consumerNumber + ": "
                                  + CallbackId() + ":\n"
                                  + "  Type      = " + com.saabgroup.safir.dob.typesystem.Operations.getName(typeId) + "\n"
                                  + "  HandlerId = " + handlerId);

        Logger.instance().println();
    }


    //
    // MessageSender Members
    //

    public void onNotMessageOverflow()
    {
        m_connection.exitDispatch();
        executeCallbackActions(com.saabgroup.safir.dob.CallbackId.ON_NOT_MESSAGE_OVERFLOW);

        Logger.instance().println(PREFIX + m_consumerNumber + ": " + CallbackId());
        Logger.instance().println();

    }




    //
    // RevokedRegistrationBase Members
    //

    public void onRevokedRegistration(long typeId, com.saabgroup.safir.dob.typesystem.HandlerId handlerId)
    {
        m_connection.exitDispatch();
        executeCallbackActions(com.saabgroup.safir.dob.CallbackId.ON_REVOKED_REGISTRATION);

        Logger.instance().println
            (PREFIX + m_consumerNumber + ": "
             + CallbackId() + ":\n"
             + "  Type      = " + com.saabgroup.safir.dob.typesystem.Operations.getName(typeId) + "\n"
             + "  HandlerId = " + handlerId);
        Logger.instance().println();
    }



    //
    // ServiceRequestBase Members
    //

    public void onServiceRequest(com.saabgroup.safir.dob.ServiceRequestProxy serviceRequestProxy,
                                 com.saabgroup.safir.dob.ResponseSender responseSender)
    {
        m_connection.exitDispatch();
        m_responseSender = responseSender;
        executeCallbackActions(com.saabgroup.safir.dob.CallbackId.ON_SERVICE_REQUEST);

        com.saabgroup.dosetest.RootService svc = (com.saabgroup.dosetest.RootService)serviceRequestProxy.getRequest();
        String xml;

        if (needBinaryCheck(svc))
        {
            xml = checkBinaryMember(svc,serviceRequestProxy.getBlob());
        }
        else
        {
            xml = com.saabgroup.safir.dob.typesystem.Serialization.toXml(serviceRequestProxy.getBlob());
        }

        Logger.instance().println
            (PREFIX + m_consumerNumber + ": "
             + CallbackId() + ": \n"
             + "  Type       = " + com.saabgroup.safir.dob.typesystem.Operations.getName(serviceRequestProxy.getTypeId()) + "\n"
             + "  Sender     = " + connInfoToXml(serviceRequestProxy.getSenderConnectionInfo()) + "\n"
             + "  Handler    = " + serviceRequestProxy.getReceivingHandlerId() + "\n"
             + "  HandlerStr = " + serviceRequestProxy.getReceiverWithStringRepresentation() + "\n"
             + "  Request    = " + xml);
        Logger.instance().println();

        if (!responseSender.isDone())
        {
            com.saabgroup.dosetest.SuccessfulService resp = new com.saabgroup.dosetest.SuccessfulService();
            resp.info().setVal("AutoResponse");
            responseSender.send(resp);
        }
    }



    //
    // CompletedRegistrationBase Members
    //

    public void onCompletedRegistration(long typeId, com.saabgroup.safir.dob.typesystem.HandlerId handlerId)
    {
        m_connection.exitDispatch();
        executeCallbackActions(com.saabgroup.safir.dob.CallbackId.ON_COMPLETED_REGISTRATION);

        Logger.instance().println
            (PREFIX + m_consumerNumber + ": "
             + CallbackId() + ":\n"
             + "  Type      = " + com.saabgroup.safir.dob.typesystem.Operations.getName(typeId) + "\n"
             + "  HandlerId = " + handlerId);
        Logger.instance().println();
    }



    //
    // EntityRequestBase Members
    //

    public void onCreateRequest(com.saabgroup.safir.dob.EntityRequestProxy entityRequestProxy, com.saabgroup.safir.dob.ResponseSender responseSender)
    {
        m_connection.exitDispatch();
        m_responseSender = responseSender;
        m_responseSenderDiscarded = false;
        executeCallbackActions(com.saabgroup.safir.dob.CallbackId.ON_CREATE_REQUEST);

        com.saabgroup.dosetest.RootEntity req = (com.saabgroup.dosetest.RootEntity)entityRequestProxy.getRequest();
        String xml;

        if (needBinaryCheck(req))
        {
            xml = checkBinaryMember(req,entityRequestProxy.getBlob());
        }
        else
        {
            xml = com.saabgroup.safir.dob.typesystem.Serialization.toXml(entityRequestProxy.getBlob());
        }

        Key key = new Key(entityRequestProxy.getTypeId(), entityRequestProxy.getReceivingHandlerId());
        Pair value = m_instanceIdPolicyMap.get(key);
        if (value == null)
        {
            throw new com.saabgroup.safir.dob.typesystem.SoftwareViolationException("Didn't find a corresponding item in m_instanceIdPolicyMap!");
        }

        if (value.policy == com.saabgroup.safir.dob.InstanceIdPolicy.HANDLER_DECIDES_INSTANCE_ID)
        {
            Logger.instance().println(PREFIX + m_consumerNumber + ": "
                                      + CallbackId() + " (Handler decides instance id): \n"
                                      + "  Type       = " + entityRequestProxy.getTypeId() + "\n"
                                      + "  Sender     = " + connInfoToXml(entityRequestProxy.getSenderConnectionInfo()) + "\n"
                                      + "  Handler    = " + entityRequestProxy.getReceivingHandlerId() + "\n"
                                      + "  HandlerStr = " + entityRequestProxy.getReceiverWithStringRepresentation() + "\n"
                                      + "  Request    = " + xml);
            Logger.instance().println();

            if (!m_responseSenderDiscarded)
            {
                m_connection.setAll(req,
                                    new com.saabgroup.safir.dob.typesystem.InstanceId(value.instanceId),
                                    entityRequestProxy.getReceivingHandlerId());

                Logger.instance().println(PREFIX + m_consumerNumber + ": "
                                          + "Handler created instance " + value.instanceId);


                com.saabgroup.safir.dob.EntityIdResponse resp = new com.saabgroup.safir.dob.EntityIdResponse();
                resp.assigned().setVal(new com.saabgroup.safir.dob.typesystem.EntityId(entityRequestProxy.getTypeId(),
                                                                                       new com.saabgroup.safir.dob.typesystem.InstanceId(value.instanceId)));
                responseSender.send(resp);
                ++value.instanceId;
            }
        }
        else
        {
            Logger.instance().println(PREFIX + m_consumerNumber + ": "
                                      + CallbackId() + " (Requestor decides instance id): \n"
                                      + "  Entity     = " + entityRequestProxy.getEntityId() + "\n"
                                      + "  Sender     = " + connInfoToXml(entityRequestProxy.getSenderConnectionInfo()) + "\n"
                                      + "  Handler    = " + entityRequestProxy.getReceivingHandlerId() + "\n"
                                      + "  HandlerStr = " + entityRequestProxy.getReceiverWithStringRepresentation() + "\n"
                                      + "  Request    = " + xml);
            Logger.instance().println();


            if (!m_responseSenderDiscarded)
            {
                m_connection.setAll(req,
                                    entityRequestProxy.getInstanceId(),
                                    entityRequestProxy.getReceivingHandlerId());
            }
        }

        if (!responseSender.isDone())
        {
            com.saabgroup.dosetest.SuccessfulCreate resp = new com.saabgroup.dosetest.SuccessfulCreate();
            resp.info().setVal("AutoResponse");
            responseSender.send(resp);
        }
    }

    public void onDeleteRequest(com.saabgroup.safir.dob.EntityRequestProxy entityRequestProxy, com.saabgroup.safir.dob.ResponseSender responseSender)
    {
        m_connection.exitDispatch();
        m_responseSender = responseSender;
        m_responseSenderDiscarded = false;
        executeCallbackActions(com.saabgroup.safir.dob.CallbackId.ON_DELETE_REQUEST);

        Logger.instance().println(PREFIX + m_consumerNumber + ": "
                                  + CallbackId() + ": \n"
                                  + "  Entity     = " + entityRequestProxy.getEntityId() + "\n"
                                  + "  Sender     = " + connInfoToXml(entityRequestProxy.getSenderConnectionInfo()) + "\n"
                                  + "  Handler    = " + entityRequestProxy.getReceivingHandlerId() + "\n"
                                  + "  HandlerStr = " + entityRequestProxy.getReceiverWithStringRepresentation());

        Logger.instance().println();

        if (!m_responseSenderDiscarded)
        {
            m_connection.delete(entityRequestProxy.getEntityId(), entityRequestProxy.getReceivingHandlerId());
        }

        if (!responseSender.isDone())
        {
            com.saabgroup.dosetest.SuccessfulDelete resp = new com.saabgroup.dosetest.SuccessfulDelete();
            resp.info().setVal("AutoResponse");
            responseSender.send(resp);
        }
    }

    public void onUpdateRequest(com.saabgroup.safir.dob.EntityRequestProxy entityRequestProxy, com.saabgroup.safir.dob.ResponseSender responseSender)
    {
        m_connection.exitDispatch();
        m_responseSender = responseSender;
        m_responseSenderDiscarded = false;
        executeCallbackActions(com.saabgroup.safir.dob.CallbackId.ON_UPDATE_REQUEST);

        com.saabgroup.dosetest.RootEntity req = (com.saabgroup.dosetest.RootEntity)entityRequestProxy.getRequest();
        String xml;

        if (needBinaryCheck(req))
        {
            xml = checkBinaryMember(req,entityRequestProxy.getBlob());
        }
        else
        {
            xml = com.saabgroup.safir.dob.typesystem.Serialization.toXml(entityRequestProxy.getBlob());
        }

        Logger.instance().println(PREFIX + m_consumerNumber + ": "
                                  + CallbackId() + ": \n"
                                  + "  Entity     = " + entityRequestProxy.getEntityId() + "\n"
                                  + "  Sender     = " + connInfoToXml(entityRequestProxy.getSenderConnectionInfo()) + "\n"
                                  + "  Handler    = " + entityRequestProxy.getReceivingHandlerId() + "\n"
                                  + "  HandlerStr = " + entityRequestProxy.getReceiverWithStringRepresentation() + "\n"
                                  + "  Request    = " + xml);
        Logger.instance().println();

        if (!m_responseSenderDiscarded)
        {
            m_connection.setChanges(req,
                                    entityRequestProxy.getInstanceId(),
                                    entityRequestProxy.getReceivingHandlerId());
        }

        if (!responseSender.isDone())
        {
            com.saabgroup.dosetest.SuccessfulUpdate resp = new com.saabgroup.dosetest.SuccessfulUpdate();
            resp.info().setVal("AutoResponse");
            responseSender.send(resp);
        }
    }



    //
    // EntityInjectionBase Members
    //

    public void onInitialInjectionsDone(long typeId, com.saabgroup.safir.dob.typesystem.HandlerId handlerId)
    {
        m_connection.exitDispatch();
        executeCallbackActions(com.saabgroup.safir.dob.CallbackId.ON_INITIAL_INJECTIONS_DONE);

        Logger.instance().println(PREFIX + m_consumerNumber + ": "
                                  + CallbackId() + ":\n"
                                  + "  Type      = " + com.saabgroup.safir.dob.typesystem.Operations.getName(typeId) + "\n"
                                  + "  HandlerId = " + handlerId);
        Logger.instance().println();
    }

    public void onInjectedDeletedEntity(com.saabgroup.safir.dob.InjectedEntityProxy entityProxy)
    {
        m_connection.exitDispatch();
        executeCallbackActions(com.saabgroup.safir.dob.CallbackId.ON_INJECTED_DELETED_ENTITY);

        Logger.instance().println
            (PREFIX + m_consumerNumber + ": "
             + CallbackId() + ":\n"
             + "  EntityId       = " + entityProxy.getEntityId() + "\n"
             + "  Current  = " + com.saabgroup.safir.dob.typesystem.Serialization.toXml(entityProxy.getCurrent()));
        Logger.instance().println();
    }

    public void onInjectedNewEntity(com.saabgroup.safir.dob.InjectedEntityProxy entityProxy)
    {
        m_connection.exitDispatch();
        executeCallbackActions(com.saabgroup.safir.dob.CallbackId.ON_INJECTED_NEW_ENTITY);

        Logger.instance().println
            (PREFIX + m_consumerNumber + ": "
             + CallbackId() + ":\n"
             + "  EntityId  = " + entityProxy.getEntityId() + "\n"
             + "  Injection = " + com.saabgroup.safir.dob.typesystem.Serialization.toXml(entityProxy.getInjectionBlob()) + "\n"
             + "  Changed top-level members: ");

        com.saabgroup.safir.dob.Entity entity = entityProxy.getInjection();
        for (int i = 0;
             i < com.saabgroup.safir.dob.typesystem.Members.getNumberOfMembers(entity.getTypeId());
             ++i)
        {
            if (entity.getMember(i, 0).isChanged())
            {
                Logger.instance().println("    "
                                          + com.saabgroup.safir.dob.typesystem.Members.getName(entity.getTypeId(), i));
            }
        }

        Logger.instance().println();
    }

    public void onInjectedUpdatedEntity(com.saabgroup.safir.dob.InjectedEntityProxy entityProxy)
    {
        m_connection.exitDispatch();
        executeCallbackActions(com.saabgroup.safir.dob.CallbackId.ON_INJECTED_UPDATED_ENTITY);

        Logger.instance().println
            (PREFIX + m_consumerNumber + ": "
             + CallbackId() + ":\n"
             + "  EntityId  = " + entityProxy.getEntityId() + "\n"
             + "  Injection = " + com.saabgroup.safir.dob.typesystem.Serialization.toXml(entityProxy.getInjectionBlob()) + "\n"
             + "  Current   = " + com.saabgroup.safir.dob.typesystem.Serialization.toXml(entityProxy.getCurrent()) + "\n"
             + "  Changed top-level members: ");

        com.saabgroup.safir.dob.Entity entity = entityProxy.getInjection();
        for (int i = 0;
             i < com.saabgroup.safir.dob.typesystem.Members.getNumberOfMembers(entity.getTypeId());
             ++i)
        {
            if (entity.getMember(i, 0).isChanged())
            {
                Logger.instance().println("    "
                                          + com.saabgroup.safir.dob.typesystem.Members.getName(entity.getTypeId(), i));
            }
        }

        Logger.instance().println();
    }



    //
    // Requestor Members
    //

    public void onNotRequestOverflow()
    {
        m_connection.exitDispatch();
        executeCallbackActions(com.saabgroup.safir.dob.CallbackId.ON_NOT_REQUEST_OVERFLOW);
        Logger.instance().println(PREFIX + m_consumerNumber + ": " + CallbackId());
    }

    public void onResponse(com.saabgroup.safir.dob.ResponseProxy responseProxy)
    {
        m_connection.exitDispatch();
        executeCallbackActions(com.saabgroup.safir.dob.CallbackId.ON_RESPONSE);

        Logger.instance().print
            (PREFIX + m_consumerNumber
             + ": " + CallbackId() + ":\n"
             + "  Type       = " + com.saabgroup.safir.dob.typesystem.Operations.getName(responseProxy.getTypeId()) + "\n"
             + "  IsSuccess  = " + responseProxy.isSuccess() + "\n"
             + "  Sender     = " + connInfoToXml(responseProxy.getResponseSenderConnectionInfo()) + "\n"
             + "  Response   = " + com.saabgroup.safir.dob.typesystem.Serialization.toXml(responseProxy.getBlob()) + "\n"
             + "  Request    = ");
        try
        {
            com.saabgroup.safir.dob.typesystem.Object req = responseProxy.getRequest();
            if (needBinaryCheck(req))
            {
                Logger.instance().println(checkBinaryMember(req,responseProxy.getRequestBlob()));
            }
            else
            {
                Logger.instance().println(com.saabgroup.safir.dob.typesystem.Serialization.toXml(responseProxy.getRequestBlob()));
            }

        }
        catch (com.saabgroup.safir.dob.typesystem.SoftwareViolationException exc)
        {
            com.saabgroup.safir.dob.typesystem.EntityId eid = new com.saabgroup.safir.dob.typesystem.EntityId(responseProxy.getRequestTypeId(), responseProxy.getRequestInstanceId());
            Logger.instance().println("DeleteRequest on " + eid);
        }

        Logger.instance().println();
    }

///
/// Backdoor
///
    public void handleCommand(String[] cmdTokens)
    {
        Logger.instance().println(PREFIX + m_consumerNumber +
                ": Got a backdoor HandleCommand callback. Command tokens:");
        for (String cmd: cmdTokens)
        {
            Logger.instance().print(cmd + ' ');
        }
        Logger.instance().println();
    }

    public String getHelpText()
    {
        Logger.instance().println(PREFIX + m_consumerNumber +
                ": Got a backdoor GetHelpText callback.");
        return "This is a help text";
    }

    private String connInfoToXml(com.saabgroup.safir.dob.ConnectionInfo connInfo)
    {
        connInfo.connectionId().setNull();
        if (!connInfo.connectionName().isNull())
        {
            int index = connInfo.connectionName().getVal().lastIndexOf('#');
            connInfo.connectionName().setVal(connInfo.connectionName().getVal().substring(0, index));
        }
        return com.saabgroup.safir.dob.typesystem.Serialization.toXml(connInfo);
    }

    //
    // Private data members
    //

    private com.saabgroup.safir.dob.SecondaryConnection m_connection = new com.saabgroup.safir.dob.SecondaryConnection();

    private com.saabgroup.safir.application.BackdoorKeeper m_backdoorKeeper = new com.saabgroup.safir.application.BackdoorKeeper();
    private final int m_consumerNumber;
    private final String m_connectionName;
    private final String m_connectionInstance;

    private java.util.EnumMap<com.saabgroup.safir.dob.CallbackId, java.util.Vector<com.saabgroup.dosetest.Action>> m_callbackActions;

    private com.saabgroup.safir.dob.ResponseSender m_responseSender = null;

    private boolean m_responseSenderDiscarded;

    public class Key implements Comparable<Key>{
        public Key(long _typeId, com.saabgroup.safir.dob.typesystem.HandlerId _handlerId) {
            typeId = _typeId;
            handlerId = _handlerId.getRawValue();
        }
        public long typeId;
        public long handlerId;

        public int compareTo(Key other) {
            if (typeId < other.typeId)
            {
                return -1;
            }
            else if (typeId > other.typeId) {
                return 1;
            }
            else {
                if (handlerId < other.handlerId)
                {
                    return -1;
                }
                else if (handlerId > other.handlerId) {
                    return 1;
                }
                else {
                    return 0;
                }
            }
        }
    };

    public class Pair
    {
        /*        public Pair()
                  {
                  }
        */
        public Pair(com.saabgroup.safir.dob.InstanceIdPolicy first, long second)
        {
            policy = first;
            instanceId = second;
        }

        public com.saabgroup.safir.dob.InstanceIdPolicy policy;
        public long instanceId;
    };


    java.util.TreeMap<Key,Pair> m_instanceIdPolicyMap = new java.util.TreeMap<Key,Pair>();

    static private class TimestampRequestor implements com.saabgroup.safir.dob.Requestor
    {
        //
        // Requestor Members
        //

        public void onNotRequestOverflow() { }

        public void onResponse(com.saabgroup.safir.dob.ResponseProxy responseProxy) { }


    };

    TimestampRequestor m_timestampRequestor = new TimestampRequestor();





}


