// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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

/**
 * Common base class for connections to the DOB.
 *
 * There are methods for
 * setting up subscription, register handlers, set and delete entities etc.
 */
public abstract class ConnectionBase
{
    /** Constructor
     */
    public ConnectionBase()
    {

    }


    //
    // Non-pending (synchronous) registration of entity handler.
    //


    /**
     * Registration of an entity handler for a given type.
     *
     * Used to make a non-pending registration for a handler for a given type. Upon return from this method the given
     * handler is guaranteed to be registered. (Any existing handler with the same id will be revoked.)
     *
     * This is the preferred registration method to be used for a handler that has no need to get
     * external entity injections (for example when there is no redundancy).
     *
     * There can be any number of registered handlers for a type as long as each handler has
     * a unique id.
     *
     * Note that if you have a configuration where more than one application (connection) is registering
     * the same type/handler, your own registration can still be revoked by another application.
     *
     * @param typeId  Entity type to register.
     * @param handlerId Handler id.
     * @param instanceIdPolicy Specifies if the handler expects instance ids in create requests to be
     *                              assigned by the requestor (normal case) or if the handler assigns them by itself.
     * @param entityHandler Callback consumer object.
     */
    public void registerEntityHandler(long typeId,
                                      com.saabgroup.safir.dob.typesystem.HandlerId handlerId,
                                      InstanceIdPolicy instanceIdPolicy,
                                      EntityHandler entityHandler)
    {
        boolean [] success = new boolean [1];

        Interface.RegisterEntityHandler(getControllerId(),
                                        typeId,
                                        handlerId.getRawValue(),
                                        handlerId.getRawString(),
                                        instanceIdPolicy.ordinal(),
                                        true,  // override registration
                                        false, //not an injectionHandler
                                        entityHandler,
                                        success);

        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
    }

    /**
     * Register an entity handler that also gets informed about injected entities.
     *
     * Used to make a non-pending registration for a handler for a given type when the handler
     * also needs to be informed when an entity instance is about to be injected in the system.
     * Upon return from this method the given handler is guaranteed to be registered.
     *
     * There are two typical cases when entities are injected outside the control of a registered
     * handler:
     *
     * @li Reception of persistent entity instances.
     * @li Reception of entity instances from an external system installation (multi-owned entities).
     *
     * After registration, any persistent data will be transfered to the handler via
     * EntityInjectionBase#OnInjectedNewEntity callbacks. When all persistent data have been transfered
     * the handler is notified by the callback EntityInjectionBase#OnInitialInjectionsDone. This callback
     * is guaranteed to be invoked even when there is no persistent data at all.
     *
     * @param typeId  Entity type to register.
     * @param handlerId Handler id.
     * @param instanceIdPolicy Specifies if the handler expects instance ids in create requests to be
     *                              assigned by the requestor (normal case) or if the handler assigns them by itself.
     * @param entityInjectionHandler Callback consumer object.
     */
    public void registerEntityHandlerInjection(long typeId,
                                               com.saabgroup.safir.dob.typesystem.HandlerId handlerId,
                                               InstanceIdPolicy instanceIdPolicy,
                                               EntityHandlerInjection entityInjectionHandler)
    {
        boolean [] success = new boolean [1];

        Interface.RegisterEntityHandler(getControllerId(),
                                        typeId,
                                        handlerId.getRawValue(),
                                        handlerId.getRawString(),
                                        instanceIdPolicy.ordinal(),
                                        true,  // override registration
                                        true, //an injectionHandler
                                        entityInjectionHandler,
                                        success);

        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
    }



    //
    // Pending (asynchronous) registration of entity handler.
    //

    /**
     * Pending registration of a handler for a given entity type.
     *
     * Used to make a pending registration of a handler for a given type. Upon return from this method the given
     * handler is registered as pending. In case an existing handler with the same handler id isn't already
     * registered the Dob will immediately promote this handler to be the registered one.
     *
     * The consumer is informed via callbacks of any change of the registration status (Completed or Revoked).
     *
     * This method is to be used by applications that handles redundancy. The typical scenario
     * is that two or more application instances make a pending registration with the same handler id.
     * The Dob will assure that one of the handlers is promoted to registered and all others are pending. If
     * there are several types involved in the redundancy switch it is often the case that you want
     * just one application (the active one) to have all types registered. To achieve this, all applications make
     * a pending registration for one of the types. The application that receives the Completed status
     * then makes a non-pending registration for the remaining types using method RegisterEntityHandler()
     * (or RegisterEntityHandlerInjection()).
     *
     * @param typeId  Entity type to register.
     * @param handlerId Handler id.
     * @param instanceIdPolicy Specifies if the handler expects instance ids in create requests to be
     *                              assigned by the requestor (normal case) or if the handler assigns them by itself.
     * @param entityHandlerPending Callback consumer object.
     */
    public void registerEntityHandlerPending(long typeId,
                                             com.saabgroup.safir.dob.typesystem.HandlerId handlerId,
                                             InstanceIdPolicy instanceIdPolicy,
                                             EntityHandlerPending entityHandlerPending)
    {
        boolean [] success = new boolean [1];

        Interface.RegisterEntityHandler(getControllerId(),
                                        typeId,
                                        handlerId.getRawValue(),
                                        handlerId.getRawString(),
                                        instanceIdPolicy.ordinal(),
                                        false,  // pending registration
                                        true, //an injectionHandler
                                        entityHandlerPending,
                                        success);

        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
    }



    //
    // Non-pending (synchronous) registration of service handler.
    //

    /**
     * Register a service handler for a given type.
     *
     * @see ConnectionBase#registerEntityHandler
     *
     * @param typeId  Service type to register.
     * @param handlerId Handler id.
     * @param serviceHandler Callback consumer object.
     */
    public void registerServiceHandler(long typeId,
                                       com.saabgroup.safir.dob.typesystem.HandlerId handlerId,
                                       ServiceHandler serviceHandler)
    {
        boolean [] success = new boolean [1];

        Interface.RegisterServiceHandler(getControllerId(),
                                         typeId,
                                         handlerId.getRawValue(),
                                         handlerId.getRawString(),
                                         true,  // override registration
                                         serviceHandler,
                                         success);

        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
    }



    //
    // Pending (asynchronous) registration of service handler.
    //

    /**
     * Pending registration of a handler for a given service type.
     *
     * @see ConnectionBase#registerEntityHandlerPending
     *
     * @param typeId  Service type to register.
     * @param handlerId Handler id.
     * @param serviceHandlerPending Callback consumer object.
     */
    public void registerServiceHandlerPending(long typeId,
                                              com.saabgroup.safir.dob.typesystem.HandlerId handlerId,
                                              ServiceHandlerPending serviceHandlerPending)
    {
        boolean [] success = new boolean [1];

        Interface.RegisterServiceHandler(getControllerId(),
                                         typeId,
                                         handlerId.getRawValue(),
                                         handlerId.getRawString(),
                                         false, //pending registration
                                         serviceHandlerPending,
                                         success);

        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
    }



    //
    // Unregistration
    //

    /**
     * Unregister of an entity handler or a service handler.
     *
     * Any created entity instance owned by the given handler will be deleted.
     *
     * This method can also be used to unregister a pending handler.
     *
     * Using the constant Dob::com.saabgroup.safir.dob.typesystem.:HandlerId::ALL_HANDLERS means that all handlers
     * for the given type, registered by this connection, will be unregistered.
     *
     * If the given handler isn't registered the call will be ignored.
     *
     * @param typeId Type id of the entity or service to unregister.
     * @param handlerId Handler id.
     */
    public void unregisterHandler(long typeId,
                                  com.saabgroup.safir.dob.typesystem.HandlerId handlerId)
    {
        boolean [] success = new boolean [1];

        Interface.UnregisterHandler(getControllerId(),
                                    typeId,
                                    handlerId.getRawValue(),
                                    handlerId.getRawString(),
                                    success);

        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
    }



    //
    // Message Subscriptions
    //

    /**
     * Set up subscription for messages of a certain type and its subclasses.
     *
     * The subscriber can subscribe for messages sent on a specific cannel, or by using
     * the constant Dob::com.saabgroup.safir.dob.typesystem.:ChannelId::ALL_CHANNELS, subscribe for all messages
     * of the given type regardless of the channel id set by the sender.
     *
     * Calling this method is identical to calling the SubscribeMessage below with
     * includeSubclasses = true.
     *
     * @param typeId Type id of the message to subscribe for.
     * @param channelId Channel id.
     * @param messageSubscriber MessageSubscriber that will receive the messages.
     */
    public void subscribeMessage(long typeId,
                                 com.saabgroup.safir.dob.typesystem.ChannelId channelId,
                                 MessageSubscriber messageSubscriber)
    {
        subscribeMessage(typeId, channelId, true, messageSubscriber);
    }

    /**
     * Set up subscription for messages of a certain type (additional parameters).
     *
     * Overloaded method that gives the user the ability to determine if the subscription
     * also will include subsclasses.
     *
     * @param typeId Type id of the message or to subscribe for.
     * @param channelId Channel id.
     * @param includeSubclasses True => Subscription for this message type and all its subclasses.
     *                               False => No subclasses will be included.
     * @param messageSubscriber MessageSubscriber that will receive the messages.
     */
    public void subscribeMessage(long typeId,
                                 com.saabgroup.safir.dob.typesystem.ChannelId channelId,
                                 boolean includeSubclasses,
                                 MessageSubscriber messageSubscriber)
    {
        boolean [] success = new boolean [1];

        Interface.SubscribeMessage(getControllerId(),
                                   typeId,
                                   channelId.getRawValue(),
                                   channelId.getRawString(),
                                   includeSubclasses,
                                   messageSubscriber,
                                   success);


        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
    }

    /**
     * Remove a message subscription made by the given subscriber.
     *
     * Removes the subscription for the given type and its subclasses.
     *
     * Using the constant Dob::com.saabgroup.safir.dob.typesystem.:ChannelId::ALL_CHANNELS means that all subscriptions
     * for the given type and its subclasses will be removed.
     *
     * If no subscription exists the call will be ignored.
     *
     * Calling this method is identical to calling the UnsubscribeMessage below with
     * includeSubclasses = true.
     *
     * @param typeId Type id of the message to unsubscribe for.
     * @param channelId Channel id.
     * @param messageSubscriber The MessageSubscriber consumer that was used when
     *                               the subscription was initiated.
     */
    public void unsubscribeMessage(long typeId,
                                   com.saabgroup.safir.dob.typesystem.ChannelId channelId,
                                   MessageSubscriber messageSubscriber)
    {
        unsubscribeMessage(typeId, channelId, true, messageSubscriber);
    }

    /**
     * Remove a message subscription made by the given subscriber (additional parameters).
     *
     * Overloaded method that gives the user the ability to determine if unsubscription
     * also will include subsclasses.
     *
     * @param typeId Type id of the message to unsubscribe for.
     * @param channelId Channel id.
     * @param includeSubclasses True => Unsubscribe for this message type and all its subclasses.
     *                               False => No subclasses will be included.
     * @param messageSubscriber The MessageSubscriber consumer that was used when
     *                               the subscription was initiated.
     */
    public void unsubscribeMessage(long typeId,
                                   com.saabgroup.safir.dob.typesystem.ChannelId channelId,
                                   boolean includeSubclasses,
                                   MessageSubscriber messageSubscriber)
    {
        boolean [] success = new boolean [1];

        Interface.UnsubscribeMessage(getControllerId(),
                                     typeId,
                                     channelId.getRawValue(),
                                     channelId.getRawString(),
                                     includeSubclasses,
                                     messageSubscriber,
                                     success);

        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
    }



    //
    // Entity subscriptions
    //

    /**
     * Set up subscription for instances of an entity type and its subclasses.
     *
     * The subscriber will receive information about creations, updates and deletes of
     * instances of the given entity type and its subclasses.
     *
     * When setting up a subscription the user will get initial data for existing entity
     * instances in the form of OnNewEntity callbacks. This is true even when
     * setting up a subscription for an already subscribed entity.
     *
     * @param typeId Type id of the entity to subscribe for.
     * @param entitySubscriber EntitySubscriber that will receive the entities.
     */
    public void subscribeEntity(long typeId,
                                EntitySubscriber entitySubscriber)
    {
        subscribeEntity(typeId,
                        true, //includeUpdates
                        true, //includeSubclasses
                        true, //restartSubscription
                        entitySubscriber);
    }

    /**
     * Subscription for an entity type (additional parameters).
     *
     * Overloaded method that gives the user the ability to a determine more details
     * conscerning the subscription.
     *
     * @param typeId Type id of the entity to subscribe for.
     * @param includeUpdates True => Subscription includes update, as well as create and delete.
     *                            False => Subscription includes no updates, only create and deletion.
     * @param includeSubclasses True => Subscription for this entity type and all its subclasses.
     *                               False => No subclasses will be included.
     * @param restartSubscription True=> OnNewEntity callbacks are generated even if the subscription already exists.
     *                                 False=> OnNewEntity callbacks are generated only for instances that are not previously subscribed.
     * @param entitySubscriber EntitySubscriber that will receive the entities.
     */
    public void subscribeEntity(long typeId,
                                boolean includeUpdates,
                                boolean includeSubclasses,
                                boolean restartSubscription,
                                EntitySubscriber entitySubscriber)
    {
        boolean [] success = new boolean [1];

        Interface.SubscribeEntity(getControllerId(),
                                  typeId,
                                  0,
                                  "",
                                  true, //allInstances,
                                  includeUpdates,
                                  includeSubclasses,
                                  restartSubscription,
                                  entitySubscriber,
                                  success);

        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
    }

    /**
     * Set up subscription for a specific instance of an entity type.
     *
     * When setting up a subscription the user will get initial data in the form of a OnNewEntity callback
     * with the current state for the subscribed entity (if created).
     *
     * @param entityId Entity id of the entity to subscribe for.
     * @param includeUpdates True => Subscription includes update, as well as create and delete.
     *                            False => Subscription includes no updates, only create and deletion.
     * @param restartSubscription True=> An OnNewEntity callback will be generated even if the subscription already exists.
     *                                 False=> An OnNewEntity callback is generated only if the instance is not previously subscribed.
     * @param entitySubscriber EntitySubscriber that will receive the entities.
     */
    public void subscribeEntity(com.saabgroup.safir.dob.typesystem.EntityId entityId,
                                boolean includeUpdates,
                                boolean restartSubscription,
                                EntitySubscriber entitySubscriber)
    {
        boolean [] success = new boolean [1];

        Interface.SubscribeEntity(getControllerId(),
                                  entityId.getTypeId(),
                                  entityId.getInstanceId().getRawValue(),
                                  entityId.getInstanceId().getRawString(),
                                  false, //allInstances,
                                  includeUpdates,
                                  false, //includeSubclasses
                                  restartSubscription,
                                  entitySubscriber,
                                  success);

        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
    }

    /**
     * Remove an entity subscription made by the given subscriber.
     *
     * Removes the subscription for the given type and its subclasses.
     *
     * If no subscription exists the call will be ignored.
     *
     * @param typeId Type id of the entity to unsubscribe for.
     * @param entitySubscriber The EntitySubscriber consumer that was used when
     *                              the subscription was initiated.
     */
    public void unsubscribeEntity(long typeId,
                                  EntitySubscriber entitySubscriber)
    {
        unsubscribeEntity(typeId,
                          true, //includeSubclasses,
                          entitySubscriber);
    }

    /**
     * Remove an entity subscription made by the given subscriber (additional parameters).
     *
     * Overloaded method that gives the user the ability to determine if unsubscription
     * also will include subsclasses..
     *
     * @param typeId Type id of the entity to unsubscribe for.
     * @param includeSubclasses True => Unsubscribe for this entity type and all its subclasses.
     *                               False => Unsubscribe for just this type (no subclasses).
     * @param entitySubscriber The EntitySubscriber consumer that was used when
     *                              the subscription was initiated.
     */
    public void unsubscribeEntity(long typeId,
                                  boolean includeSubclasses,
                                  EntitySubscriber entitySubscriber)
    {
        boolean [] success = new boolean [1];

        Interface.UnsubscribeEntity(getControllerId(),
                                    typeId,
                                    0,
                                    "",
                                    true, //allInstances,
                                    includeSubclasses,
                                    entitySubscriber,
                                    success);

        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
    }

    /**
     * Remove an entity instance subscription made by the given subscriber.
     *
     * If no subscription exists the call will be ignored.
     *
     * @param entityId Entity id of the entity instance.
     * @param entitySubscriber The EntitySubscriber consumer that was used when
     *                              the subscription was initiated.
     */
    public void unsubscribeEntity(com.saabgroup.safir.dob.typesystem.EntityId entityId,
                                  EntitySubscriber entitySubscriber)
    {
        boolean [] success = new boolean [1];

        Interface.UnsubscribeEntity(getControllerId(),
                                    entityId.getTypeId(),
                                    entityId.getInstanceId().getRawValue(),
                                    entityId.getInstanceId().getRawString(),
                                    false, //allInstances,
                                    false, //includeSubclasses
                                    entitySubscriber,
                                    success);

        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
    }



    //
    // Registration subscriptions
    //

    /**
     * Set up subscription for notifications about when a specific handler for an entity type or
     * a service type is registered and unregistered.
     *
     * Using the constant Dob::com.saabgroup.safir.dob.typesystem.:HandlerId::ALL_HANDLERS means that the subscriber will receive
     * registrations/unregistrations of any handler for the given type.
     *
     * Using a specific handler id means that the subscriber will receive only the
     * registrations/unregistrations of the specified handler.
     *
     * When setting up a subscription the user will get initial information about existing handlers via
     * OnRegistered callbacks.
     *
     * @param typeId Type id of entity or service.
     * @param handlerId Handler id.
     * @param includeSubclasses True => Subscription for this entity type or service type and all its subclasses.
     *                               False => No subclasses will be included.
     * @param restartSubscription True=> OnRegistered callbacks are generated even if the subscription already exists.
     *                                 False=> OnRegistered callbacks are generated only for handlers that are not previously subscribed.
     * @param registrationSubscriber RegistrationSubscriber that will receive the subscription
     *                                    response.
     */
    public void subscribeRegistration(long typeId,
                                      com.saabgroup.safir.dob.typesystem.HandlerId handlerId,
                                      boolean includeSubclasses,
                                      boolean restartSubscription,
                                      RegistrationSubscriber registrationSubscriber)
    {
        boolean [] success = new boolean [1];

        Interface.SubscribeRegistration(getControllerId(),
                                        typeId,
                                        handlerId.getRawValue(),
                                        handlerId.getRawString(),
                                        includeSubclasses,
                                        restartSubscription,
                                        registrationSubscriber,
                                        success);
        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
    }

    /**
     * Removes a registration subscription.
     *
     * Using the constant Dob::com.saabgroup.safir.dob.typesystem.:HandlerId::ALL_HANDLERS means that all registration subscriptions,
     * for the given type id and consumer, are removed.
     *
     * If no subscription exists the call will be ignored.
     *
     * @param typeId Type id of entity or service.
     * @param handlerId Handler id.
     * @param includeSubclasses True => Unsubscribe for this entity type or service type and all its subclasses.
     *                               False => No subclasses will be included.
     * @param registrationSubscriber The registrationSubscriber consumer that was used when
     *                                    the subscription was initiated.
     */
    public void unsubscribeRegistration(long typeId,
                                        com.saabgroup.safir.dob.typesystem.HandlerId handlerId,
                                        boolean includeSubclasses,
                                        RegistrationSubscriber registrationSubscriber)
    {
        boolean [] success = new boolean [1];

        Interface.UnsubscribeRegistration(getControllerId(),
                                          typeId,
                                          handlerId.getRawValue(),
                                          handlerId.getRawString(),
                                          includeSubclasses,
                                          registrationSubscriber,
                                          success);

        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
    }



    //
    // Send messages
    //

            /**
         * Send a message on the specified channel.
         *
         * The application must be prepared to handle the situation that the outgoing send queue is full
         * (OverflowException is thrown). In this case the application is responsible for resending
         * the message. When the overflow situation is dissolved, the application is notified by the
         * MessageSender::OnNotMessageOverflow callback, which should trig the resending.
         *
         * @param message Message to send.
         * @param channelId Channel id.
         * @param messageSender MessageSender for notification about overflow status.
         *
         * @throws OverflowException There was an overflow when sending.
         */
    public void send(Message message,
                     com.saabgroup.safir.dob.typesystem.ChannelId channelId,
                     MessageSender messageSender)
        throws OverflowException
    {
        boolean [] success = new boolean [1];


        //TODO: serialize directly to shared memory
        int blobSize = message.calculateBlobSize();
        java.nio.ByteBuffer blob = java.nio.ByteBuffer.allocateDirect(blobSize); //allocate blob
        int beginningOfUnused = com.saabgroup.safir.dob.typesystem.InternalOperations.formatBlob(blob, blobSize, message.getTypeId());
        beginningOfUnused = message.writeToBlob(blob, beginningOfUnused);

        Interface.SendMessage(getControllerId(),
                              blob,
                              channelId.getRawValue(),
                              channelId.getRawString(),
                              messageSender,
                              success);

        if (!success[0])
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();

            com.saabgroup.safir.dob.typesystem.Exception exc =
                com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().getException();
            if (exc instanceof com.saabgroup.safir.dob.OverflowException)
            {
                throw (com.saabgroup.safir.dob.OverflowException)exc;
            }
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
    }


    //
    // Requests
    //

    /**
     * Request to a handler to create an entity instance without specifying the instanceId.
     *
     * If the handler is registered as "HandlerDecidesInstanceId" the InstanceId of the
     *   entity to be created is determined by the application, and the requestor is
     *   told which InstanceId will be used by the EntityIdResponse.
     * If the handler is registered as "RequestorDecidesInstanceId" an InstanceId will
     *   be randomly generated and included in the request. This InstanceId *must* be
     *   used by the handler as the new instance number.
     *
     * The application must be prepared to handle the situation that the outgoing send queue is full
     * (OverflowException is thrown). In this case the application is responsible for resending
     * of the create request. When the overflow situation is dissolved, the application is
     * notified by the Requestor::OnNotRequestOverflow callback, which should trig the resending.
     *
     * @param request Entity requested to be created.
     * @param handlerId Handler id.
     * @param requestor Requestor for response and notification about overflow status.
     *
     * @return Request id that can be used to match sent request with the response.
     *
     * @throws OverflowException There was an overflow when sending.
     */
    public int createRequest(Entity request,
                             com.saabgroup.safir.dob.typesystem.HandlerId handlerId,
                             Requestor requestor) throws OverflowException
    {
        boolean [] success = new boolean [1];
        int [] requestId = new int[1];


        //TODO: serialize directly to shared memory
        int blobSize = request.calculateBlobSize();
        java.nio.ByteBuffer blob = java.nio.ByteBuffer.allocateDirect(blobSize); //allocate blob
        int beginningOfUnused = com.saabgroup.safir.dob.typesystem.InternalOperations.formatBlob(blob, blobSize, request.getTypeId());
        beginningOfUnused = request.writeToBlob(blob, beginningOfUnused);

        Interface.CreateRequest(getControllerId(),
                                blob,
                                false,
                                0,
                                "",
                                handlerId.getRawValue(),
                                handlerId.getRawString(),
                                requestor,
                                requestId,
                                success);

        if (!success[0])
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();

            com.saabgroup.safir.dob.typesystem.Exception exc =
                com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().getException();
            if (exc instanceof com.saabgroup.safir.dob.OverflowException)
            {
                throw (com.saabgroup.safir.dob.OverflowException)exc;
            }
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }

        return requestId[0];
    }

    /**
     * Request to a handler to create a specific entity instance.
     *
     * If the handler is registered as "RequestorDecidesInstanceId" the Requestor must
     * specify which instance is to be created. (If it doesnt care it can use the method
     * above that will generate one randomly.)
     * If the Requestor wants a random instance, but needs to know which instance will
     * get created it can use Dob:com.saabgroup.safir.dob.typesystem.InstanceId::GenerateRandom() to generate
     * an InstanceId to pass to this method
     *
     * Note that it is illegal to call this method if the handler is registered as
     * "HandlerDecidesInstanceId".
     *
     * The application must be prepared to handle the situation that the outgoing send queue is full
     * (OverflowException is thrown). In this case the application is responsible for resending
     * of the entity create request. When the overflow situation is dissolved, the application is
     * notified by the Requestor::OnNotRequestOverflow callback, which should trig the resending.
     *
     * @param request Entity requested to be created.
     * @param instanceId Instance id.
     * @param handlerId Handler id.
     * @param requestor Requestor for response and notification about overflow status.
     *
     * @return Request id that can be used to match sent request with the response.
     *
     * @throws OverflowException There was an overflow when sending.
     */
    public int createRequest(Entity request,
                             com.saabgroup.safir.dob.typesystem.InstanceId instanceId,
                             com.saabgroup.safir.dob.typesystem.HandlerId handlerId,
                             Requestor requestor) throws OverflowException
    {
        boolean [] success = new boolean [1];
        int [] requestId = new int[1];


        //TODO: serialize directly to shared memory
        int blobSize = request.calculateBlobSize();
        java.nio.ByteBuffer blob = java.nio.ByteBuffer.allocateDirect(blobSize); //allocate blob
        int beginningOfUnused = com.saabgroup.safir.dob.typesystem.InternalOperations.formatBlob(blob, blobSize, request.getTypeId());
        beginningOfUnused = request.writeToBlob(blob, beginningOfUnused);

        Interface.CreateRequest(getControllerId(),
                                blob,
                                true,
                                instanceId.getRawValue(),
                                instanceId.getRawString(),
                                handlerId.getRawValue(),
                                handlerId.getRawString(),
                                requestor,
                                requestId,
                                success);

        if (!success[0])
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();

            com.saabgroup.safir.dob.typesystem.Exception exc =
                com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().getException();
            if (exc instanceof com.saabgroup.safir.dob.OverflowException)
            {
                throw (com.saabgroup.safir.dob.OverflowException)exc;
            }
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }

        return requestId[0];
    }

    /**
     * Send an update request on an existing entity instance.
     *
     * An update request will be sent to the handler that owns (has created) the entity.
     *
     * The application must be prepared to handle the situation that the outgoing send queue is full
     * (OverflowException is thrown). In this case the application is responsible for resending
     * of the entity update request. When the overflow situation is dissolved, the application is
     * notified by the Requestor::OnNotRequestOverflow callback, which should trig the resending.
     *
     * @param request Entity requested to be updated.
     * @param instanceId Instance id.
     * @param requestor Requestor for response and notification about overflow status.
     *
     * @return Request id that can be used to match sent request with the response.
     *
     * @throws OverflowException There was an overflow when sending.
     */
    public int updateRequest(Entity request,
                             com.saabgroup.safir.dob.typesystem.InstanceId instanceId,
                             Requestor requestor) throws OverflowException
    {
        boolean [] success = new boolean [1];
        int [] requestId = new int [1];

        //TODO: serialize directly to shared memory
        int blobSize = request.calculateBlobSize();
        java.nio.ByteBuffer blob = java.nio.ByteBuffer.allocateDirect(blobSize); //allocate blob
        int beginningOfUnused = com.saabgroup.safir.dob.typesystem.InternalOperations.formatBlob(blob, blobSize, request.getTypeId());
        beginningOfUnused = request.writeToBlob(blob, beginningOfUnused);

        Interface.UpdateRequest(getControllerId(),
                                blob,
                                instanceId.getRawValue(),
                                instanceId.getRawString(),
                                requestor,
                                requestId,
                                success);

        if (!success[0])
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();

            com.saabgroup.safir.dob.typesystem.Exception exc =
                com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().getException();
            if (exc instanceof com.saabgroup.safir.dob.OverflowException)
            {
                throw (com.saabgroup.safir.dob.OverflowException)exc;
            }
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }

        return requestId[0];
    }

    /**
     * Send a delete request on an existing entity instance.
     *
     * A delete request will be sent to the handler that owns (has created) the entity.
     *
     * The application must be prepared to handle the situation that the outgoing send queue is full
     * (OverflowException is thrown). In this case the application is responsible for resending
     * of the entity delete request. When the overflow situation is dissolved, the application is
     * notified by the Requestor::OnNotRequestOverflow callback, which should trig the resending.
     *
     * @param entityId EntityId of the entity to be deleted.
     * @param requestor Requestor for response and notification about overflow status.
     *
     * @return Request id that can be used to match sent request with the response.
     *
     * @throws OverflowException There was an overflow when sending.
     */
    public int deleteRequest(com.saabgroup.safir.dob.typesystem.EntityId entityId,
                             Requestor requestor) throws OverflowException
    {
        boolean [] success = new boolean [1];
        int [] requestId = new int[1];

        Interface.DeleteRequest(getControllerId(),
                                entityId.getTypeId(),
                                entityId.getInstanceId().getRawValue(),
                                entityId.getInstanceId().getRawString(),
                                requestor,
                                requestId,
                                success);

        if (!success[0])
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();

            com.saabgroup.safir.dob.typesystem.Exception exc =
                com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().getException();
            if (exc instanceof com.saabgroup.safir.dob.OverflowException)
            {
                throw (com.saabgroup.safir.dob.OverflowException)exc;
            }
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }


        return requestId[0];
    }

    /**
     * Send a request to the given service handler.
     *
     * The application must be prepared to handle the situation that the outgoing send queue is full
     * (OverflowException is thrown). In this case the application is responsible for resending
     * of the service request. When the overflow situation is dissolved, the application is notified
     * by the Requestor::OnNotRequestOverflow callback, which should trig the resending.
     *
     * @param request The service request.
     * @param handlerId Service handler id.
     * @param requestor Requestor for service response and notification about overflow status.
     *
     * @return Request id that can be used to match sent request with the response.
     *
     * @throws OverflowException There was an overflow when sending.
     */
    public int serviceRequest(Service request,
                              com.saabgroup.safir.dob.typesystem.HandlerId handlerId,
                              Requestor requestor) throws OverflowException
    {
        boolean [] success = new boolean [1];
        int [] requestId = new int[1];

        //TODO: serialize directly to shared memory
        int blobSize = request.calculateBlobSize();
        java.nio.ByteBuffer blob = java.nio.ByteBuffer.allocateDirect(blobSize); //allocate blob
        int beginningOfUnused = com.saabgroup.safir.dob.typesystem.InternalOperations.formatBlob(blob, blobSize, request.getTypeId());
        beginningOfUnused = request.writeToBlob(blob, beginningOfUnused);

        Interface.ServiceRequest(getControllerId(),
                                 blob,
                                 handlerId.getRawValue(),
                                 handlerId.getRawString(),
                                 requestor,
                                 requestId,
                                 success);


        if (!success[0])
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();

            com.saabgroup.safir.dob.typesystem.Exception exc =
                com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().getException();
            if (exc instanceof com.saabgroup.safir.dob.OverflowException)
            {
                throw (com.saabgroup.safir.dob.OverflowException)exc;
            }
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }

        return requestId[0];
    }



//
    // Entity Owners
    //

    /**
     * Merge the changed members of an entity straight into the pool (the given handler must be the owner).
     *
     * All members of the given entity that are marked as changed will be merged into the
     * current object in the pool.
     * If the object is not already set in the pool the entity will be set without any merging.
     *
     * @param entity Entity to create or update.
     * @param instanceId Instance id.
     * @param handlerId Handler id.
     *
     * @throws AccessDeniedException The instance is owned by another handler.
     */
    public void setChanges(Entity entity,
                           com.saabgroup.safir.dob.typesystem.InstanceId instanceId,
                           com.saabgroup.safir.dob.typesystem.HandlerId handlerId)
    {
        com.saabgroup.safir.dob.typesystem.EntityId eid = new com.saabgroup.safir.dob.typesystem.EntityId(entity.getTypeId(), instanceId);
        if (isCreated(eid))
        {
            try {
                EntityProxy entityProxy = read(eid);
                try {
                    Entity merged = entityProxy.getEntity();
                    com.saabgroup.safir.dob.typesystem.Utilities.mergeChanges(merged, entity);

                    set(merged,
                        instanceId,
                        handlerId,
                        true);     // true => consider change flags
                    return;
                }
                finally {
                    entityProxy.dispose();
                }
            }
            catch (com.saabgroup.safir.dob.NotFoundException exc){
            }
        }

        set(entity,
            instanceId,
            handlerId,
            false);     // false => don't consider change flags
    }

    /**
     * Allows an entity handler to create or update an entity.
     *
     * A call to SetAll will replace all members of any existing entity with the given entity. I.e. the DOB
     * will not merge the changes with any existing data, but will instead completely replace the old
     * data. Use the SetChanges() method to make the DOB merge the data into the pool.
     *
     * Special care must be taken when the owner sets an entity that contains pointers (Entity ids)
     * to other entity instances, and the lifetime of the pointed-to instance is completly connected
     * to the lifetime of the referer. In this case the owner must traverse the object-graph
     * and any unreferenced instance must be deleted.
     *
     * @param entity Entity to create or update.
     * @param instanceId Instance id.
     * @param handlerId Handler id.
     *
     * @throws AccessDeniedException The instance is owned by another handler.
     */
    public void setAll(Entity entity,
                       com.saabgroup.safir.dob.typesystem.InstanceId instanceId,
                       com.saabgroup.safir.dob.typesystem.HandlerId handlerId)
    {
        set(entity,
            instanceId,
            handlerId,
            false);     // false => don't consider change flags
    }

    // private method
    private void set(Entity entity,
                     com.saabgroup.safir.dob.typesystem.InstanceId instanceId,
                     com.saabgroup.safir.dob.typesystem.HandlerId handlerId,
                     boolean considerChangeFlags)
    {
        boolean [] success = new boolean [1];

        //TODO: serialize directly to shared memory
        int blobSize = entity.calculateBlobSize();
        java.nio.ByteBuffer blob = java.nio.ByteBuffer.allocateDirect(blobSize); //allocate blob
        int beginningOfUnused = com.saabgroup.safir.dob.typesystem.InternalOperations.formatBlob(blob, blobSize, entity.getTypeId());
        beginningOfUnused = entity.writeToBlob(blob, beginningOfUnused);

        Interface.SetEntity(getControllerId(),
                            blob,
                            instanceId.getRawValue(),
                            instanceId.getRawString(),
                            handlerId.getRawValue(),
                            handlerId.getRawString(),
                            considerChangeFlags,
                            false,   // false => this is not an initial injection
                            success);

        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
    }

    /**
     * Allows an entity handler to delete a specific owned entity instance.
     *
     * Used to delete a specific owned instance. Does nothing if the instance does not exist.
     *
     * @param entityId Id of the entity to delete.
     * @param handlerId Handler id.
     *
     * @throws AccessDeniedException The instance is owned by another handler.
     */
    public void delete(com.saabgroup.safir.dob.typesystem.EntityId entityId,
                       com.saabgroup.safir.dob.typesystem.HandlerId handlerId)
    {
        boolean [] success = new boolean [1];

        Interface.DeleteEntity(getControllerId(),
                               entityId.getTypeId(),
                               entityId.getInstanceId().getRawValue(),
                               entityId.getInstanceId().getRawString(),
                               false, // false => this instance only
                               handlerId.getRawValue(),
                               handlerId.getRawString(),
                               success);
        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
    }

    /**
     * Allows an entity handler to delete all owned instances.
     *
     * Used to delete all instances owned by the caller.
     *
     * @param typeId Entity type.
     * @param handlerId Handler id.
     */
    public void deleteAllInstances(long typeId,
                                   com.saabgroup.safir.dob.typesystem.HandlerId handlerId)
    {
        boolean [] success = new boolean [1];

        Interface.DeleteEntity(getControllerId(),
                               typeId,
                               0,
                               "",
                               true,                        // true => all instances
                               handlerId.getRawValue(),
                               handlerId.getRawString(),
                               success);

        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
    }


    /**
     * Retrieve an iterator that can be used to iterate over created instances.
     *
     * Note: The iterator HAS to be disposed when you're finished using it,
     *       otherwise the references that it holds down into Dob shared memory
     *       will not be released correctly.
     *
     * Note: You do not need to dispose the EntityProxy:s that the iterator.next()
     *       returns, the iterator automatically disposes the last proxy when next
     *       is called. However, this means that you can't use the previous proxy
     *       after calling next().
     *
     * A correct way of using the iterator:
     * 
     * <pre>
     * EntityIterator iterator = m_connection.getEntityIterator(typeId,true);
     * try {
     *     while(iterator.hasNext()) {
     *         EntityProxy entityProxy = iterator.next();
     *
     *         ... do something with entityProxy ...
     *     }
     * }
     * finally {
     *     iterator.dispose();
     * }
     * </pre>
     *
     * @param typeId Entity type to iterate over
     * @param includeSubclasses True =>  Iterate over subclass instances
     *                          False => No subclasses will be included
     * @return An iterator for entities
     */
    public EntityIterator getEntityIterator(long typeId,
                                            boolean includeSubclasses) {
        return new EntityIterator(getControllerId(),typeId,includeSubclasses);
    }


    //
    // Read operations
    //

    /**
     * Read an entity from the distributed object pool.
     *
     * Gets the current version of the entity that matches the given entity id.
     *
     * @param entityId Entity id of the entity to read.
     *
     * @return Entity read from the distributed object pool.
     *
     * @throws com.saabgroup.safir.dob.NotFoundException The specified instance of the entity does not exist.
     */
    public EntityProxy read(com.saabgroup.safir.dob.typesystem.EntityId entityId)
        throws com.saabgroup.safir.dob.NotFoundException
    {
        boolean [] success = new boolean [1];

        java.nio.ByteBuffer [] currentBlob = new java.nio.ByteBuffer [1];
        java.nio.ByteBuffer [] currentState = new java.nio.ByteBuffer [1];

        Interface.ReadEntity(getControllerId(),
                             entityId.getTypeId(),
                             entityId.getInstanceId().getRawValue(),
                             entityId.getInstanceId().getRawString(),
                             currentBlob,
                             currentState,
                             success);

        if (!success[0])
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();

            com.saabgroup.safir.dob.typesystem.Exception exc =
                com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().getException();
            if (exc instanceof com.saabgroup.safir.dob.NotFoundException)
            {
                throw (com.saabgroup.safir.dob.NotFoundException)exc;
            }
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }

        return new EntityProxy(currentBlob[0].asReadOnlyBuffer(), currentState[0].asReadOnlyBuffer(), null, null, false, false);
    }

        /**
         * Check if an instance of an entity is created or not.
         *
         * This method will return true if the given entity instance is created.
         *
         * Note that the only time that you can really trust this information is if you're the one that
         * has created the entity instance and no one is overregistering a handler with the same id as yours.
         * Otherwise there is no guarantee that the instance still is created immediately after this call.
         * The owner may be deleting it right after you asked, or your handler may have been revoked but
         * you have not yet received a Revoke status.
         * Use with care!
         *
         * @param entityId Entity instance to check existence of.
         * @return True if the entity instance is created, otherwise false.
         */
        public boolean isCreated(com.saabgroup.safir.dob.typesystem.EntityId entityId)
        {
            boolean [] success = new boolean [1];
            boolean [] isCreated = new boolean [1];

            Interface.IsCreated(getControllerId(),
                                entityId.getTypeId(),
                                entityId.getInstanceId().getRawValue(),
                                entityId.getInstanceId().getRawString(),
                                isCreated,
                                success);
            if (!success[0]) {
                com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
                com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
            }
            return isCreated[0];
        }

        /**
         * This method is used to get the number of instances of an entity that exists.
         *
         * @param typeId The type of the class whose instances we're counting.
         * @param handlerId Count only instances owned by this handler (use HandlerId::ALL_HANDLERS
         *                        to get all handlers).
         * @param includeSubclasses Include subclasses when counting instances.
         * @return The number of instances.
         */
        public long getNumberOfInstances(long typeId,
                                                 com.saabgroup.safir.dob.typesystem.HandlerId handlerId,
                                                 boolean includeSubclasses)
        {
            boolean [] success = new boolean [1];
            long [] numInstances = new long [1];

            Interface.GetNumberOfInstances(getControllerId(),
                                           typeId,
                                           handlerId.getRawValue(),
                                           handlerId.getRawString(),
                                           includeSubclasses,
                                           numInstances,
                                           success);

            if (!success[0]) {
                com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
                com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
            }
            return numInstances[0];
        }

        /**
         * This method is used to get the number of instances of an entity that exists.
         *
         * @param typeId The type of the class the handler is registered for.
         * @param handlerId Get instanceIdPolicy for this handler.
         * @return instanceIdPolicy Specifies if the handler expects instance ids in create requests to be
         *                          assigned by the requestor or if the handler assigns them by itself.
         * @throws com.saabgroup.safir.dob.NotFoundException The given handlerId has not registered the given class.
         */
    public InstanceIdPolicy GetInstanceIdPolicy(long typeId,
            com.saabgroup.safir.dob.typesystem.HandlerId handlerId)
    {
            boolean [] success = new boolean [1];
            int [] instanceIdPolicy = new int [1];

            Interface.GetInstanceIdPolicy(getControllerId(),
                                          typeId,
                                          handlerId.getRawValue(),
                                          instanceIdPolicy,
                                          success);

            if (!success[0]) {
                com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
                com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
            }
            return InstanceIdPolicy.class.getEnumConstants()[instanceIdPolicy[0]];
        }

    /**
     * Interrupt the ongoing Dispatch even if all data to the application have not been distpatched.
     * The dispatch-event will be automatically set to trigger a new Dispatch again.
     * This can be used to ensure that too much time is not spent dispatching in a time-critical app.
     */
    public void exitDispatch()
    {
        boolean [] success = new boolean [1];
        Interface.ExitDispatch(getControllerId(),success);
        if (!success[0]) {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwFundamental();
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().throwUnknown();
        }
    }

    abstract int getControllerId();
}
