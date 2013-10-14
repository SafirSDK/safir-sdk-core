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

using System;
using System.Runtime.InteropServices;

namespace Safir.Dob
{
    /// <summary>
    /// Common base class for connections to the DOB.
    ///
    /// There are methods for
    /// setting up subscription, register handlers, set and delete entities etc.
    /// </summary>
    public abstract class ConnectionBase
    {
        /// <summary>
        /// Constructor
        /// </summary>
        public ConnectionBase()
        {

        }

        /// <summary>
        /// Tells if the connection is opened.
        /// </summary>
        /// <returns>True if the connection is open, otherwise false.</returns>
        public abstract bool IsOpen();


        #region Non-pending (synchronous) registration of entity handler.
        /// <summary>
        /// Registration of an entity handler for a given type.
        /// <para/>
        /// Used to make a non-pending registration for a handler for a given type. Upon return from this method the given
        /// handler is guaranteed to be registered. (Any existing handler with the same id will be revoked.)
        /// <para/>
        /// This is the preferred registration method to be used for a handler that has no need to get
        /// external entity injections (for example when there is no redundancy).
        /// <para/>
        /// There can be any number of registered handlers for a type as long as each handler has
        /// a unique id.
        /// <para/>
        /// Note that if you have a configuration where more than one application (connection) is registering
        /// the same type/handler, your own registration can still be revoked by another application.
        /// </summary>
        /// <param name="typeId">Entity type to register</param>
        /// <param name="handlerId">Handler id.</param>
        /// <param name="instanceIdPolicy">Specifies if the handler expects instance ids in create requests to be
        ///                                assigned by the requestor (normal case) or if the handler assigns them by itself.</param>
        /// <param name="entityHandler">Callback consumer object.</param>
        public void RegisterEntityHandler(System.Int64 typeId,
                                          Typesystem.HandlerId handlerId,
                                          InstanceIdPolicy.Enumeration instanceIdPolicy, 
                                          EntityHandler entityHandler)
        {
            byte success;

            System.IntPtr handlerIdStr = Dob.Typesystem.Internal.InternalOperations.CStringOf(handlerId.RawString);
            
            Interface.DoseC_RegisterEntityHandler(ControllerId,
                                                  typeId,
                                                  handlerId.RawValue,
                                                  handlerIdStr,
                                                  (System.Int32)instanceIdPolicy,
                                                  Interface.ByteOf(true),  // override registration
                                                  Interface.ByteOf(false), //not an injectionHandler
                                                  Interface.DOSE_LANGUAGE_DOTNET,
                                                  ConsumerHandler.Instance.AddReference(entityHandler),
                                                  out success);

            Marshal.FreeHGlobal(handlerIdStr);

            if (!Interface.BoolOf(success))
            {
                ConsumerHandler.Instance.DropReference(entityHandler);

                Typesystem.LibraryExceptions.Instance.Throw();
            }
        }

        /// <summary>
        /// Register an entity handler that also gets informed about injected entities.
        /// <para/>
        /// Used to make a non-pending registration for a handler for a given type when the handler
        /// also needs to be informed when an entity instance is about to be injected in the system.
        /// Upon return from this method the given handler is guaranteed to be registered.
        /// <para/>
        /// There are two typical cases when entities are injected outside the control of a registered
        /// handler:
        /// <para/>
        /// <list type="bullet">
        /// <item><description>Reception of persistent entity instances.</description></item>
        /// <item><description>Reception of entity instances from an external system installation (multi-owned entities).</description></item>
        /// </list>
        /// <para/>
        /// After registration, any persistent data will be transfered to the handler via
        /// EntityInjectionBase#OnInjectedNewEntity callbacks. When all persistent data have been transfered
        /// the handler is notified by the callback EntityInjectionBase#OnInitialInjectionsDone. This callback
        /// is guaranteed to be invoked even when there is no persistent data at all.
        /// </summary>
        /// <param name="typeId">Entity type to register.</param>
        /// <param name="handlerId">Handler id.</param>
        /// <param name="instanceIdPolicy">Specifies if the handler expects instance ids in create requests to be
        ///                                assigned by the requestor (normal case) or if the handler assigns them by itself.</param>
        /// <param name="entityHandlerInjection">Callback consumer object.</param>
        public void RegisterEntityHandlerInjection(System.Int64 typeId,
                                                   Typesystem.HandlerId handlerId,
                                                   InstanceIdPolicy.Enumeration instanceIdPolicy,
                                                   EntityHandlerInjection entityHandlerInjection)
        {
            byte success;

            System.IntPtr handlerIdStr = Dob.Typesystem.Internal.InternalOperations.CStringOf(handlerId.RawString);

            Interface.DoseC_RegisterEntityHandler(ControllerId,
                                                  typeId,
                                                  handlerId.RawValue,
                                                  handlerIdStr,
                                                  (System.Int32)instanceIdPolicy,
                                                  Interface.ByteOf(true),  // override registration
                                                  Interface.ByteOf(true), //an injectionHandler
                                                  Interface.DOSE_LANGUAGE_DOTNET,
                                                  ConsumerHandler.Instance.AddReference(entityHandlerInjection),
                                                  out success);

            Marshal.FreeHGlobal(handlerIdStr);

            if (!Interface.BoolOf(success))
            {
                ConsumerHandler.Instance.DropReference(entityHandlerInjection);

                Typesystem.LibraryExceptions.Instance.Throw();
            }
        }

        #endregion

        #region Pending (asynchronous) registration of entity handler.

        /// <summary>
        /// Pending registration of a handler for a given entity type.
        /// <para/>
        /// Used to make a pending registration of a handler for a given type. Upon return from this method the given
        /// handler is registered as pending. In case an existing handler with the same handler id isn't already
        /// registered the Dob will immediately promote this handler to be the registered one.
        /// <para/>
        /// The consumer is informed via callbacks of any change of the registration status (Completed or Revoked).
        /// <para/>
        /// This method is to be used by applications that handles redundancy. The typical scenario
        /// is that two or more application instances make a pending registration with the same handler id.
        /// The Dob will assure that one of the handlers is promoted to registered and all others are pending. If
        /// there are several types involved in the redundancy switch it is often the case that you want
        /// just one application (the active one) to have all types registered. To achieve this, all applications make
        /// a pending registration for one of the types. The application that receives the Completed status
        /// then makes a non-pending registration for the remaining types using method RegisterEntityHandler()
        /// (or RegisterEntityHandlerInjection()).
        /// </summary>
        /// <param name="typeId">Entity type to register.</param>
        /// <param name="handlerId">Handler id.</param>
        /// <param name="instanceIdPolicy">Specifies if the handler expects instance ids in create requests to be
        ///                                assigned by the requestor (normal case) or if the handler assigns them by itself.</param>
        /// <param name="entityHandlerPending">Callback consumer object.</param>
        public void RegisterEntityHandlerPending(System.Int64 typeId,
                                                 Typesystem.HandlerId handlerId,
                                                 InstanceIdPolicy.Enumeration instanceIdPolicy,
                                                 EntityHandlerPending entityHandlerPending)
        {
            byte success;

            System.IntPtr handlerIdStr = Dob.Typesystem.Internal.InternalOperations.CStringOf(handlerId.RawString);

            Interface.DoseC_RegisterEntityHandler(ControllerId,
                                                  typeId,
                                                  handlerId.RawValue,
                                                  handlerIdStr,
                                                  (System.Int32)instanceIdPolicy,
                                                  Interface.ByteOf(false),  // pending registration
                                                  Interface.ByteOf(true), //an injectionHandler
                                                  Interface.DOSE_LANGUAGE_DOTNET,
                                                  ConsumerHandler.Instance.AddReference(entityHandlerPending),
                                                  out success);

            Marshal.FreeHGlobal(handlerIdStr);

            if (!Interface.BoolOf(success))
            {
                ConsumerHandler.Instance.DropReference(entityHandlerPending);

                Typesystem.LibraryExceptions.Instance.Throw();
            }
        }

        #endregion

        #region Non-pending (synchronous) registration of service handler.

        /// <summary>
        /// Register a service handler for a given type.
        /// <para/>
        /// See also <seealso cref="RegisterEntityHandler"/>.
        /// </summary>
        /// <param name="typeId">Service type to register.</param>
        /// <param name="handlerId">Handler id.</param>
        /// <param name="serviceHandler">Callback consumer object.</param>
        public void RegisterServiceHandler(System.Int64 typeId,
                                           Typesystem.HandlerId handlerId,
                                           ServiceHandler serviceHandler)
        {
            byte success;

            System.IntPtr handlerIdStr = Dob.Typesystem.Internal.InternalOperations.CStringOf(handlerId.RawString);

            Interface.DoseC_RegisterServiceHandler(ControllerId,
                                                   typeId,
                                                   handlerId.RawValue,
                                                   handlerIdStr,
                                                   Interface.ByteOf(true), //override registration
                                                   Interface.DOSE_LANGUAGE_DOTNET,
                                                   ConsumerHandler.Instance.AddReference(serviceHandler),
                                                   out success);

            Marshal.FreeHGlobal(handlerIdStr);

            if (!Interface.BoolOf(success))
            {
                ConsumerHandler.Instance.DropReference(serviceHandler);

                Typesystem.LibraryExceptions.Instance.Throw();
            }
        }

        #endregion

        #region Pending (asynchronous) registration of service handler.

        /// <summary>
        /// Pending registration of a handler for a given service type.
        /// <para/>
        /// See also <seealso cref="RegisterEntityHandlerPending"/>.
        /// </summary>
        /// <param name="typeId">Service type to register.</param>
        /// <param name="handlerId">Handler id.</param>
        /// <param name="serviceHandlerPending">Callback consumer object.</param>
        public void RegisterServiceHandlerPending(System.Int64 typeId,
                                                  Typesystem.HandlerId handlerId,
                                                  ServiceHandlerPending serviceHandlerPending)
        {
            byte success;

            System.IntPtr handlerIdStr = Dob.Typesystem.Internal.InternalOperations.CStringOf(handlerId.RawString);

            Interface.DoseC_RegisterServiceHandler(ControllerId,
                                                   typeId,
                                                   handlerId.RawValue,
                                                   handlerIdStr,
                                                   Interface.ByteOf(false), //pending registration
                                                   Interface.DOSE_LANGUAGE_DOTNET,
                                                   ConsumerHandler.Instance.AddReference(serviceHandlerPending),
                                                   out success);

            Marshal.FreeHGlobal(handlerIdStr);

            if (!Interface.BoolOf(success))
            {
                ConsumerHandler.Instance.DropReference(serviceHandlerPending);

                Typesystem.LibraryExceptions.Instance.Throw();
            }
        }

        #endregion

        #region Unregistration

        /// <summary>
        /// Unregister of an entity handler or a service handler.
        /// <para/>
        /// Any created entity instance owned by the given handler will be deleted.
        /// <para/>
        /// This method can also be used to unregister a pending handler.
        /// <para/>
        /// Using the constant Dob::Typesystem::HandlerId::ALL_HANDLERS means that all handlers
        /// for the given type, registered by this connection, will be unregistered.
        /// <para/>
        /// If the given handler isn't registered the call will be ignored.
        /// </summary>
        /// <param name="typeId">Type id of the entity or service to unregister.</param>
        /// <param name="handlerId">Handler id.</param>
        public void UnregisterHandler(System.Int64 typeId,
                                      Typesystem.HandlerId handlerId)
        {
            byte success;

            System.IntPtr handlerIdStr = Dob.Typesystem.Internal.InternalOperations.CStringOf(handlerId.RawString);

            Interface.DoseC_UnregisterHandler(ControllerId,
                                              typeId,
                                              handlerId.RawValue,
                                              handlerIdStr,
                                              out success);

            Marshal.FreeHGlobal(handlerIdStr);

            if (!Interface.BoolOf(success))
            {
                Typesystem.LibraryExceptions.Instance.Throw();
            }
        }

        #endregion

        #region Message Subscriptions

        /// <summary>
        /// Set up subscription for messages of a certain type and its subclasses.
        /// <para/>
        /// The subscriber can subscribe for messages sent on a specific cannel, or by using
        /// the constant Dob::Typesystem::ChannelId::ALL_CHANNELS, subscribe for all messages
        /// of the given type regardless of the channel id set by the sender.
        /// <para/>
        /// Calling this method is identical to calling the SubscribeMessage below with
        /// includeSubclasses = true.
        /// </summary>
        /// <param name="typeId">Type id of the message to subscribe for.</param>
        /// <param name="channelId">Channel id.</param>
        /// <param name="messageSubscriber">MessageSubscriber that will receive the messages.</param>
        public void SubscribeMessage(System.Int64 typeId,
                                     Typesystem.ChannelId channelId,
                                     MessageSubscriber messageSubscriber)
        {
            SubscribeMessage(typeId, channelId, true, messageSubscriber);
        }

        /// <summary>
        /// Set up subscription for messages of a certain type (additional parameters).
        /// <para/>
        /// Overloaded method that gives the user the ability to determine if the subscription
        /// also will include subsclasses.
        /// </summary>
        /// <param name="typeId">Type id of the message or to subscribe for.</param>
        /// <param name="channelId">Channel id.</param>
        /// <param name="includeSubclasses">True => Subscription for this message type and all its subclasses.
        ///                                 False => No subclasses will be included.</param>
        /// <param name="messageSubscriber">MessageSubscriber that will receive the messages.</param>
        public void SubscribeMessage(System.Int64 typeId,
                                     Typesystem.ChannelId channelId,
                                     bool includeSubclasses,
                                     MessageSubscriber messageSubscriber)
        {
            byte success;

            System.IntPtr channelIdStr = Dob.Typesystem.Internal.InternalOperations.CStringOf(channelId.RawString);

            Interface.DoseC_SubscribeMessage(ControllerId,
                                             typeId,
                                             channelId.RawValue,
                                             channelIdStr,
                                             Interface.ByteOf(includeSubclasses),
                                             Interface.DOSE_LANGUAGE_DOTNET,
                                             ConsumerHandler.Instance.AddReference(messageSubscriber),
                                             out success);

            Marshal.FreeHGlobal(channelIdStr);

            if (!Interface.BoolOf(success))
            {
                ConsumerHandler.Instance.DropReference(messageSubscriber);

                Typesystem.LibraryExceptions.Instance.Throw();
            }
        }

        /// <summary>
        /// Remove a message subscription made by the given subscriber.
        /// <para/>
        /// Removes the subscription for the given type and its subclasses.
        /// <para/>
        /// Using the constant Dob::Typesystem::ChannelId::ALL_CHANNELS means that all subscriptions
        /// for the given type and its subclasses will be removed.
        /// <para/>
        /// If no subscription exists the call will be ignored.
        /// <para/>
        /// Calling this method is identical to calling the UnsubscribeMessage below with
        /// includeSubclasses = true.
        /// </summary>
        /// <param name="typeId">Type id of the message to unsubscribe for.</param>
        /// <param name="channelId">Channel id.</param>
        /// <param name="messageSubscriber">The MessageSubscriber consumer that was used when
        ///                                 the subscription was initiated.</param>
        public void UnsubscribeMessage(System.Int64 typeId,
                                       Typesystem.ChannelId channelId,
                                       MessageSubscriber messageSubscriber)
        {
            UnsubscribeMessage(typeId, channelId, true, messageSubscriber);
        }

        /// <summary>
        /// Remove a message subscription made by the given subscriber (additional parameters).
        /// <para/>
        /// Overloaded method that gives the user the ability to determine if unsubscription
        /// also will include subsclasses.
        /// </summary>
        /// <param name="typeId">Type id of the message to unsubscribe for.</param>
        /// <param name="channelId">Channel id.</param>
        /// <param name="includeSubclasses">True => Unsubscribe for this message type and all its subclasses.
        ///                                 False => No subclasses will be included.</param>
        /// <param name="messageSubscriber">The MessageSubscriber consumer that was used when
        ///                                 the subscription was initiated.</param>
        public void UnsubscribeMessage(System.Int64 typeId,
                                       Typesystem.ChannelId channelId,
                                       bool includeSubclasses,
                                       MessageSubscriber messageSubscriber)
        {
            byte success;

            System.IntPtr channelIdStr = Dob.Typesystem.Internal.InternalOperations.CStringOf(channelId.RawString);

            Interface.DoseC_UnsubscribeMessage(ControllerId,
                                               typeId,
                                               channelId.RawValue,
                                               channelIdStr,
                                               Interface.ByteOf(includeSubclasses),
                                               Interface.DOSE_LANGUAGE_DOTNET,
                                               ConsumerHandler.Instance.AddReference(messageSubscriber),
                                               out success);

            Marshal.FreeHGlobal(channelIdStr);

            ConsumerHandler.Instance.DropReference(messageSubscriber);

            if (!Interface.BoolOf(success))
            {
                Typesystem.LibraryExceptions.Instance.Throw();
            }
        }

        #endregion

        #region Entity subscriptions

        /// <summary>
        /// Set up subscription for instances of an entity type and its subclasses.
        /// <para/>
        /// The subscriber will receive information about creations, updates and deletes of
        /// instances of the given entity type and its subclasses.
        /// <para/>
        /// When setting up a subscription the user will get initial data for existing entity
        /// instances in the form of OnNewEntity callbacks. This is true even when
        /// setting up a subscription for an already subscribed entity.
        /// </summary>
        /// <param name="typeId">Type id of the entity to subscribe for.</param>
        /// <param name="entitySubscriber">EntitySubscriber that will receive the entities.</param>
        public void SubscribeEntity(System.Int64 typeId,
                                    EntitySubscriber entitySubscriber)
        {
            SubscribeEntity(typeId,
                            true, //includeUpdates
                            true, //includeSubclasses
                            true, //restartSubscription
                            entitySubscriber);
        }

        /// <summary>
        /// Subscription for an entity type (additional parameters).
        /// <para/>
        /// Overloaded method that gives the user the ability to a determine more details
        /// conscerning the subscription.
        /// </summary>
        /// <param name="typeId">Type id of the entity to subscribe for.</param>
        /// <param name="includeUpdates">True => Subscription includes update, as well as create and delete.
        ///                              False => Subscription includes no updates, only create and deletion.</param>
        /// <param name="includeSubclasses">True => Subscription for this entity type and all its subclasses.
        ///                                 False => No subclasses will be included.</param>
        /// <param name="restartSubscription">True=> OnNewEntity callbacks are generated even if the subscription already exists.
        ///                                   False=> OnNewEntity callbacks are generated only for instances that are not previously subscribed.</param>
        /// <param name="entitySubscriber">EntitySubscriber that will receive the entities.</param>
        public void SubscribeEntity(System.Int64 typeId,
                                    bool includeUpdates,
                                    bool includeSubclasses,
                                    bool restartSubscription,
                                    EntitySubscriber entitySubscriber)
        {
            byte success;

            System.IntPtr instanceIdStr = Dob.Typesystem.Internal.InternalOperations.CStringOf("");

            Interface.DoseC_SubscribeEntity(ControllerId,
                                            typeId,
                                            0,
                                            instanceIdStr,
                                            Interface.ByteOf(true), //allInstances,
                                            Interface.ByteOf(includeUpdates),
                                            Interface.ByteOf(includeSubclasses),
                                            Interface.ByteOf(restartSubscription),
                                            Interface.DOSE_LANGUAGE_DOTNET,
                                            ConsumerHandler.Instance.AddReference(entitySubscriber),
                                            out success);

            Marshal.FreeHGlobal(instanceIdStr);

            if (!Interface.BoolOf(success))
            {
                ConsumerHandler.Instance.DropReference(entitySubscriber);

                Typesystem.LibraryExceptions.Instance.Throw();
            }
        }

        /// <summary>
        /// Set up subscription for a specific instance of an entity type.
        ///
        /// When setting up a subscription the user will get initial data in the form of a OnNewEntity callback
        /// with the current state for the subscribed entity (if created).
        /// </summary>
        /// <param name="entityId">Entity id of the entity to subscribe for.</param>
        /// <param name="includeUpdates">True => Subscription includes update, as well as create and delete.
        ///                              False => Subscription includes no updates, only create and deletion.</param>
        /// <param name="restartSubscription">True=> An OnNewEntity callback will be generated even if the subscription already exists.
        ///                                 False=> An OnNewEntity callback is generated only if the instance is not previously subscribed.</param>
        /// <param name="entitySubscriber">EntitySubscriber that will receive the entities.</param>
        public void SubscribeEntity(Typesystem.EntityId entityId,
                                    bool includeUpdates,
                                    bool restartSubscription,
                                    EntitySubscriber entitySubscriber)
        {
            byte success;

            System.IntPtr instanceIdStr = Dob.Typesystem.Internal.InternalOperations.CStringOf(entityId.InstanceId.RawString);

            Interface.DoseC_SubscribeEntity(ControllerId,
                                            entityId.TypeId,
                                            entityId.InstanceId.RawValue,
                                            instanceIdStr,
                                            Interface.ByteOf(false), //allInstances,
                                            Interface.ByteOf(includeUpdates),
                                            Interface.ByteOf(false), //includeSubclasses
                                            Interface.ByteOf(restartSubscription),
                                            Interface.DOSE_LANGUAGE_DOTNET,
                                            ConsumerHandler.Instance.AddReference(entitySubscriber),
                                            out success);

            Marshal.FreeHGlobal(instanceIdStr);

            if (!Interface.BoolOf(success))
            {
                ConsumerHandler.Instance.DropReference(entitySubscriber);

                Typesystem.LibraryExceptions.Instance.Throw();
            }
        }

        /// <summary>
        /// Remove an entity subscription made by the given subscriber.
        /// <para/>
        /// Removes the subscription for the given type and its subclasses.
        /// <para/>
        /// If no subscription exists the call will be ignored.
        /// </summary>
        /// <param name="typeId">Type id of the entity to unsubscribe for.</param>
        /// <param name="entitySubscriber">The EntitySubscriber consumer that was used when
        ///                                the subscription was initiated.</param>
        public void UnsubscribeEntity(System.Int64 typeId,
                                      EntitySubscriber entitySubscriber)
        {
            UnsubscribeEntity(typeId,
                              true, //includeSubclasses,
                              entitySubscriber);
        }

        /// <summary>
        /// Remove an entity subscription made by the given subscriber (additional parameters).
        /// <para/>
        /// Overloaded method that gives the user the ability to determine if unsubscription
        /// also will include subsclasses..
        /// </summary>
        /// <param name="typeId">Type id of the entity to unsubscribe for.</param>
        /// <param name="includeSubclasses">True => Unsubscribe for this entity type and all its subclasses.
        ///                                 False => Unsubscribe for just this type (no subclasses).</param>
        /// <param name="entitySubscriber">The EntitySubscriber consumer that was used when
        ///                                the subscription was initiated.</param>
        public void UnsubscribeEntity(System.Int64 typeId,
                                      bool includeSubclasses,
                                      EntitySubscriber entitySubscriber)
        {
            byte success;

            System.IntPtr instanceIdStr = Dob.Typesystem.Internal.InternalOperations.CStringOf("");

            Interface.DoseC_UnsubscribeEntity(ControllerId,
                                              typeId,
                                              0,
                                              instanceIdStr,
                                              Interface.ByteOf(true), //allInstances,
                                              Interface.ByteOf(includeSubclasses),
                                              Interface.DOSE_LANGUAGE_DOTNET,
                                              ConsumerHandler.Instance.AddReference(entitySubscriber),
                                              out success);

            Marshal.FreeHGlobal(instanceIdStr);

            ConsumerHandler.Instance.DropReference(entitySubscriber);

            if (!Interface.BoolOf(success))
            {
                Typesystem.LibraryExceptions.Instance.Throw();
            }
        }

        /// <summary>
        /// Remove an entity instance subscription made by the given subscriber.
        ///
        /// If no subscription exists the call will be ignored.
        /// </summary>
        /// <param name="entityId">Entity id of the entity instance.</param>
        /// <param name="entitySubscriber">The EntitySubscriber consumer that was used when
        ///                                the subscription was initiated.</param>
        public void UnsubscribeEntity(Typesystem.EntityId entityId,
                                      EntitySubscriber entitySubscriber)
        {
            byte success;

            System.IntPtr instanceIdStr = Dob.Typesystem.Internal.InternalOperations.CStringOf(entityId.InstanceId.RawString);

            Interface.DoseC_UnsubscribeEntity(ControllerId,
                                              entityId.TypeId,
                                              entityId.InstanceId.RawValue,
                                              instanceIdStr,
                                              Interface.ByteOf(false), //allInstances,
                                              Interface.ByteOf(false), //includeSubclasses
                                              Interface.DOSE_LANGUAGE_DOTNET,
                                              ConsumerHandler.Instance.AddReference(entitySubscriber),
                                              out success);

            Marshal.FreeHGlobal(instanceIdStr);

            ConsumerHandler.Instance.DropReference(entitySubscriber);

            if (!Interface.BoolOf(success))
            {
                Typesystem.LibraryExceptions.Instance.Throw();
            }
        }

        #endregion

        #region Registration subscriptions

        /// <summary>
        /// Set up subscription for notifications about when a specific handler for an entity type or
        /// a service type is registered and unregistered.
        /// <para/>
        /// Using the constant Dob::Typesystem::HandlerId::ALL_HANDLERS means that the subscriber will receive
        /// registrations/unregistrations of any handler for the given type.
        /// <para/>
        /// Using a specific handler id means that the subscriber will receive only the
        /// registrations/unregistrations of the specified handler.
        /// <para/>
        /// When setting up a subscription the user will get initial information about existing handlers via
        /// OnRegistered callbacks.
        /// </summary>
        /// <param name="typeId">Type id of entity or service.</param>
        /// <param name="handlerId">Handler id.</param>
        /// <param name="includeSubclasses">True => Subscription for this entity type or service type and all its subclasses.
        ///                                 False => No subclasses will be included.</param>
        /// <param name="restartSubscription">True=> OnRegistered callbacks are generated even if the subscription already exists.
        ///                                   False => OnRegistered callbacks are generated only for handlers that are not previously subscribed.</param>
        /// <param name="registrationSubscriber">RegistrationSubscriber that will receive the subscription response.</param>
        public void SubscribeRegistration(System.Int64 typeId,
                                          Typesystem.HandlerId handlerId,
                                          bool includeSubclasses,
                                          bool restartSubscription,
                                          RegistrationSubscriber registrationSubscriber)
        {
            byte success;

            System.IntPtr handlerIdStr = Dob.Typesystem.Internal.InternalOperations.CStringOf(handlerId.RawString);

            Interface.DoseC_SubscribeRegistration(ControllerId,
                                                  typeId,
                                                  handlerId.RawValue,
                                                  handlerIdStr,
                                                  Interface.ByteOf(includeSubclasses),
                                                  Interface.ByteOf(restartSubscription),
                                                  Interface.DOSE_LANGUAGE_DOTNET,
                                                  ConsumerHandler.Instance.AddReference(registrationSubscriber),
                                                  out success);

            Marshal.FreeHGlobal(handlerIdStr);

            if (!Interface.BoolOf(success))
            {
                ConsumerHandler.Instance.DropReference(registrationSubscriber);

                Typesystem.LibraryExceptions.Instance.Throw();
            }
        }

        /// <summary>
        /// Removes a registration subscription.
        /// <para/>
        /// Using the constant Dob::Typesystem::HandlerId::ALL_HANDLERS means that all registration subscriptions,
        /// for the given type id and consumer, are removed.
        /// <para/>
        /// If no subscription exists the call will be ignored.
        /// </summary>
        /// <param name="typeId">Type id of entity or service.</param>
        /// <param name="handlerId">Handler id.</param>
        /// <param name="includeSubclasses">True => Unsubscribe for this entity type or service type and all its subclasses.
        ///                                 False => No subclasses will be included.</param>
        /// <param name="registrationSubscriber">The registrationSubscriber consumer that was used when
        ///                                      the subscription was initiated.</param>
        public void UnsubscribeRegistration(System.Int64 typeId,
                                            Typesystem.HandlerId handlerId,
                                            bool includeSubclasses,
                                            RegistrationSubscriber registrationSubscriber)
        {
            byte success;

            System.IntPtr handlerIdStr = Dob.Typesystem.Internal.InternalOperations.CStringOf(handlerId.RawString);

            Interface.DoseC_UnsubscribeRegistration(ControllerId,
                                                    typeId,
                                                    handlerId.RawValue,
                                                    handlerIdStr,
                                                    Interface.ByteOf(includeSubclasses),
                                                    Interface.DOSE_LANGUAGE_DOTNET,
                                                    ConsumerHandler.Instance.AddReference(registrationSubscriber),
                                                    out success);

            Marshal.FreeHGlobal(handlerIdStr);

            ConsumerHandler.Instance.DropReference(registrationSubscriber);

            if (!Interface.BoolOf(success))
            {
                Typesystem.LibraryExceptions.Instance.Throw();
            }
        }

        #endregion

        #region Send messages

        /// <summary>
        /// Send a message on the specified channel.
        /// <para/>
        /// The application must be prepared to handle the situation that the outgoing send queue is full
        /// (OverflowException is thrown). In this case the application is responsible for resending
        /// the message. When the overflow situation is dissolved, the application is notified by the
        /// MessageSender::OnNotMessageOverflow callback, which should trig the resending.
        /// </summary>
        /// <param name="message">Message to send</param>
        /// <param name="channelId">Channel id.</param>
        /// <param name="messageSender">MessageSender for notification about overflow status.</param>
        /// <exception cref="Safir.Dob.OverflowException">There was an overflow when sending.</exception>
        public void Send(Message message,
                         Typesystem.ChannelId channelId,
                         MessageSender messageSender)
        {
            byte success;

            //TODO: serialize directly to shared memory
            System.Int32 blobSize = message.CalculateBlobSize();
            System.IntPtr blob = Marshal.AllocHGlobal(blobSize); //allocate blob
            System.IntPtr beginningOfUnused;
            Typesystem.Internal.InternalOperations.FormatBlob(blob, blobSize, message.GetTypeId(), out beginningOfUnused);
            message.WriteToBlob(blob, ref beginningOfUnused);

            IntPtr cons = ConsumerHandler.Instance.AddReference(messageSender);

            System.IntPtr channelIdStr = Dob.Typesystem.Internal.InternalOperations.CStringOf(channelId.RawString);

            Interface.DoseC_SendMessage(ControllerId,
                                        blob,
                                        channelId.RawValue,
                                        channelIdStr,
                                        Interface.DOSE_LANGUAGE_DOTNET,
                                        cons,
                                        out success);

            Marshal.FreeHGlobal(blob); //delete blob
            Marshal.FreeHGlobal(channelIdStr);

            if (!Interface.BoolOf(success))
            {
                //For overflows release will be called in the callback
                if (!Typesystem.LibraryExceptions.Instance.Peek(Dob.OverflowException.ExceptionTypeId))
                {
                    ConsumerHandler.Instance.DropReference(messageSender);
                }
                Typesystem.LibraryExceptions.Instance.Throw();
            }

            ConsumerHandler.Instance.DropReference(messageSender);
        }
        #endregion

        #region Requests

        /// <summary>
        /// Request to a handler to create an entity instance without specifying the instanceId.
        /// <para/>
        /// If the handler is registered as "HandlerDecidesInstanceId" the InstanceId of the
        ///   entity to be created is determined by the application, and the requestor is
        ///   told which InstanceId will be used by the EntityIdResponse. <para/>
        /// If the handler is registered as "RequestorDecidesInstanceId" an InstanceId will
        ///   be randomly generated and included in the request. This InstanceId *must* be
        ///   used by the handler as the new instance number.
        /// <para/>
        /// The application must be prepared to handle the situation that the outgoing send queue is full
        /// (OverflowException is thrown). In this case the application is responsible for resending
        /// of the create request. When the overflow situation is dissolved, the application is
        /// notified by the Requestor::OnNotRequestOverflow callback, which should trig the resending.
        /// </summary>
        /// <param name="entity">Entity requested to be created.</param>
        /// <param name="handlerId">Handler id.</param>
        /// <param name="requestor">Requestor for response and notification about overflow status.</param>
        /// <returns>Request id that can be used to match sent request with the response.</returns>
        /// <exception cref="Safir.Dob.OverflowException">There was an overflow when sending.</exception>
        public System.Int32 CreateRequest(Entity entity,
                                          Typesystem.HandlerId handlerId,
                                          Requestor requestor)
        {
            byte success;
            System.Int32 requestId = -1;

            //TODO: serialize directly to shared memory
            System.Int32 blobSize = entity.CalculateBlobSize();
            System.IntPtr blob = Marshal.AllocHGlobal(blobSize); //allocate blob
            System.IntPtr beginningOfUnused;
            Typesystem.Internal.InternalOperations.FormatBlob(blob, blobSize, entity.GetTypeId(), out beginningOfUnused);
            entity.WriteToBlob(blob, ref beginningOfUnused);

            IntPtr cons = ConsumerHandler.Instance.AddReference(requestor);

            System.IntPtr handlerIdStr = Dob.Typesystem.Internal.InternalOperations.CStringOf(handlerId.RawString);
            System.IntPtr instanceIdStr = Dob.Typesystem.Internal.InternalOperations.CStringOf("");

            Interface.DoseC_CreateRequest
                (ControllerId,
                 blob,
                 Interface.ByteOf(false),
                 0,
                 instanceIdStr,
                 handlerId.RawValue,
                 handlerIdStr,
                 Interface.DOSE_LANGUAGE_DOTNET,
                 cons,
                 out requestId,
                 out success);

            Marshal.FreeHGlobal(blob); //delete blob
            Marshal.FreeHGlobal(handlerIdStr);
            Marshal.FreeHGlobal(instanceIdStr);

            if (!Interface.BoolOf(success))
            {
                //For overflows release will be called in the callback
                if (!Typesystem.LibraryExceptions.Instance.Peek(Dob.OverflowException.ExceptionTypeId))
                {
                    ConsumerHandler.Instance.DropReference(requestor);
                }
                Typesystem.LibraryExceptions.Instance.Throw();
            }
            return requestId;
        }

        /// <summary>
        /// Request to a handler to create a specific entity instance.
        /// <para/>
        /// If the handler is registered as "RequestorDecidesInstanceId" the Requestor must
        /// specify which instance is to be created. (If it doesnt care it can use the method
        /// above that will generate one randomly.)
        /// If the Requestor wants a random instance, but needs to know which instance will
        /// get created it can use Dob:Typesystem:InstanceId::GenerateRandom() to generate
        /// an InstanceId to pass to this method
        /// <para/>
        /// Note that it is illegal to call this method if the handler is registered as
        /// "HandlerDecidesInstanceId".
        /// <para/>
        /// The application must be prepared to handle the situation that the outgoing send queue is full
        /// (OverflowException is thrown). In this case the application is responsible for resending
        /// of the entity create request. When the overflow situation is dissolved, the application is
        /// notified by the Requestor::OnNotRequestOverflow callback, which should trig the resending.
        /// </summary>
        /// <param name="entity">Entity requested to be created.</param>
        /// <param name="instanceId">Instance id.</param>
        /// <param name="handlerId">Handler id.</param>
        /// <param name="requestor">Requestor for response and notification about overflow status.</param>
        /// <returns>Request id that can be used to match sent request with the response.</returns>
        /// <exception cref="Safir.Dob.OverflowException">There was an overflow when sending.</exception>
        public System.Int32 CreateRequest(Entity entity,
                                          Typesystem.InstanceId instanceId,
                                          Typesystem.HandlerId handlerId,
                                          Requestor requestor)
        {
            byte success;
            System.Int32 requestId = -1;

            //TODO: serialize directly to shared memory
            System.Int32 blobSize = entity.CalculateBlobSize();
            System.IntPtr blob = Marshal.AllocHGlobal(blobSize); //allocate blob
            System.IntPtr beginningOfUnused;
            Typesystem.Internal.InternalOperations.FormatBlob(blob, blobSize, entity.GetTypeId(), out beginningOfUnused);
            entity.WriteToBlob(blob, ref beginningOfUnused);

            IntPtr cons = ConsumerHandler.Instance.AddReference(requestor);

            System.IntPtr instanceIdStr = Dob.Typesystem.Internal.InternalOperations.CStringOf(instanceId.RawString);
            System.IntPtr handlerIdStr = Dob.Typesystem.Internal.InternalOperations.CStringOf(handlerId.RawString);

            Interface.DoseC_CreateRequest
                (ControllerId,
                 blob,
                 Interface.ByteOf(true),
                 instanceId.RawValue,
                 instanceIdStr,
                 handlerId.RawValue,
                 handlerIdStr, 
                 Interface.DOSE_LANGUAGE_DOTNET,
                 cons,
                 out requestId,
                 out success);

            Marshal.FreeHGlobal(blob); //delete blob
            Marshal.FreeHGlobal(instanceIdStr);
            Marshal.FreeHGlobal(handlerIdStr);


            if (!Interface.BoolOf(success))
            {
                //For overflows release will be called in the callback
                if (!Typesystem.LibraryExceptions.Instance.Peek(Dob.OverflowException.ExceptionTypeId))
                {
                    ConsumerHandler.Instance.DropReference(requestor);
                }
                Typesystem.LibraryExceptions.Instance.Throw();
            }
            return requestId;
        }

        /// <summary>
        /// Send an update request on an existing entity instance.
        /// <para/>
        /// An update request will be sent to the handler that owns (has created) the entity.
        /// <para/>
        /// The application must be prepared to handle the situation that the outgoing send queue is full
        /// (OverflowException is thrown). In this case the application is responsible for resending
        /// of the entity update request. When the overflow situation is dissolved, the application is
        /// notified by the Requestor::OnNotRequestOverflow callback, which should trig the resending.
        /// </summary>
        /// <param name="entity">Entity requested to be updated.</param>
        /// <param name="instanceId">Instance id.</param>
        /// <param name="requestor">Requestor for response and notification about overflow status.</param>
        /// <returns>Request id that can be used to match sent request with the response.</returns>
        /// <exception cref="Safir.Dob.OverflowException">There was an overflow when sending.</exception>
        public System.Int32 UpdateRequest(Entity entity,
                                          Typesystem.InstanceId instanceId,
                                          Requestor requestor)
        {
            byte success;
            System.Int32 requestId = -1;

            //TODO: serialize directly to shared memory
            System.Int32 blobSize = entity.CalculateBlobSize();
            System.IntPtr blob = Marshal.AllocHGlobal(blobSize); //allocate blob
            System.IntPtr beginningOfUnused;
            Typesystem.Internal.InternalOperations.FormatBlob(blob, blobSize, entity.GetTypeId(), out beginningOfUnused);
            entity.WriteToBlob(blob, ref beginningOfUnused);

            IntPtr cons = ConsumerHandler.Instance.AddReference(requestor);

            System.IntPtr instanceIdStr = Dob.Typesystem.Internal.InternalOperations.CStringOf(instanceId.RawString);

            Interface.DoseC_UpdateRequest
                (ControllerId,
                 blob,
                 instanceId.RawValue,
                 instanceIdStr,
                 Interface.DOSE_LANGUAGE_DOTNET,
                 cons,
                 out requestId,
                 out success);

            Marshal.FreeHGlobal(blob); //delete blob
            Marshal.FreeHGlobal(instanceIdStr);

            if (!Interface.BoolOf(success))
            {
                //For overflows release will be called in the callback
                if (!Typesystem.LibraryExceptions.Instance.Peek(Dob.OverflowException.ExceptionTypeId))
                {
                    ConsumerHandler.Instance.DropReference(requestor);
                }
                Typesystem.LibraryExceptions.Instance.Throw();
            }
            return requestId;
        }

        /// <summary>
        /// Send a delete request on an existing entity instance.
        ///
        /// A delete request will be sent to the handler that owns (has created) the entity.
        ///
        /// The application must be prepared to handle the situation that the outgoing send queue is full
        /// (OverflowException is thrown). In this case the application is responsible for resending
        /// of the entity delete request. When the overflow situation is dissolved, the application is
        /// notified by the Requestor::OnNotRequestOverflow callback, which should trig the resending.
        /// </summary>
        /// <param name="entityId">EntityId of the entity to be deleted.</param>
        /// <param name="requestor">Requestor for response and notification about overflow status.</param>
        /// <returns>Request id that can be used to match sent request with the response.</returns>
        /// <exception cref="Safir.Dob.OverflowException">There was an overflow when sending.</exception>
        public System.Int32 DeleteRequest(Typesystem.EntityId entityId,
                                          Requestor requestor)
        {
            byte success;
            System.Int32 requestId = -1;

            IntPtr cons = ConsumerHandler.Instance.AddReference(requestor);

            System.IntPtr instanceIdStr = Dob.Typesystem.Internal.InternalOperations.CStringOf(entityId.InstanceId.RawString);

            Interface.DoseC_DeleteRequest
                (ControllerId,
                 entityId.TypeId,
                 entityId.InstanceId.RawValue,
                 instanceIdStr,
                 Interface.DOSE_LANGUAGE_DOTNET,
                 cons,
                 out requestId,
                 out success);

            Marshal.FreeHGlobal(instanceIdStr);

            if (!Interface.BoolOf(success))
            {
                //For overflows release will be called in the callback
                if (!Typesystem.LibraryExceptions.Instance.Peek(Dob.OverflowException.ExceptionTypeId))
                {
                    ConsumerHandler.Instance.DropReference(requestor);
                }
                Typesystem.LibraryExceptions.Instance.Throw();
            }
            return requestId;
        }

        /// <summary>
        /// Send a request to the given service handler.
        /// <para/>
        /// The application must be prepared to handle the situation that the outgoing send queue is full
        /// (OverflowException is thrown). In this case the application is responsible for resending
        /// of the service request. When the overflow situation is dissolved, the application is notified
        /// by the Requestor::OnNotRequestOverflow callback, which should trig the resending.
        /// </summary>
        /// <param name="service">The service request.</param>
        /// <param name="handlerId">Service handler id.</param>
        /// <param name="requestor">Requestor for service response and notification about overflow status.</param>
        /// <returns>Request id that can be used to match sent request with the response.</returns>
        /// <exception cref="Safir.Dob.OverflowException">There was an overflow when sending.</exception>
        public System.Int32 ServiceRequest(Service service,
                                           Typesystem.HandlerId handlerId,
                                           Requestor requestor)
        {
            byte success;
            System.Int32 requestId = -1;

            //TODO: serialize directly to shared memory
            System.Int32 blobSize = service.CalculateBlobSize();
            System.IntPtr blob = Marshal.AllocHGlobal(blobSize); //allocate blob
            System.IntPtr beginningOfUnused;
            Typesystem.Internal.InternalOperations.FormatBlob(blob, blobSize, service.GetTypeId(), out beginningOfUnused);
            service.WriteToBlob(blob, ref beginningOfUnused);

            IntPtr cons = ConsumerHandler.Instance.AddReference(requestor);

            System.IntPtr handlerIdStr = Dob.Typesystem.Internal.InternalOperations.CStringOf(handlerId.RawString);

            Interface.DoseC_ServiceRequest
                (ControllerId,
                 blob,
                 handlerId.RawValue,
                 handlerIdStr,
                 Interface.DOSE_LANGUAGE_DOTNET,
                 cons,
                 out requestId,
                 out success);

            Marshal.FreeHGlobal(blob); //delete blob
            Marshal.FreeHGlobal(handlerIdStr);

            if (!Interface.BoolOf(success))
            {
                //For overflows release will be called in the callback
                if (!Typesystem.LibraryExceptions.Instance.Peek(Dob.OverflowException.ExceptionTypeId))
                {
                    ConsumerHandler.Instance.DropReference(requestor);
                }
                Typesystem.LibraryExceptions.Instance.Throw();
            }
            return requestId;
        }

        #endregion

        #region Entity Owners

        /// <summary>
        /// Merge the changed members of an entity straight into the pool (the given handler must be the owner).
        /// <para/>
        /// All members of the given entity that are marked as changed will be merged into the
        /// current object in the pool.
        /// If the object is not already set in the pool the entity will be set without any merging.
        /// </summary>
        /// <param name="entity">Entity to create or update.</param>
        /// <param name="instanceId">Instance id.</param>
        /// <param name="handlerId">Handler id.</param>
        /// <exception cref="Safir.Dob.AccessDeniedException">The instance is owned by another handler.</exception>
        /// <exception cref="Safir.Dob.GhostExistsException">There is a ghost instance that hasn't been injected.</exception>
        public void SetChanges(Entity entity,
                               Typesystem.InstanceId instanceId,
                               Typesystem.HandlerId handlerId)
        {
            Typesystem.EntityId eid = new Typesystem.EntityId(entity.GetTypeId(), instanceId);
            if (IsCreated(eid))
            {
                using (EntityProxy entityProxy = Read(eid))
                {
                    Entity merged = entityProxy.Entity;
                    Typesystem.Utilities.MergeChanges(merged, entity);

                    Set(merged,
                        instanceId,
                        handlerId,
                        true);     // true => consider change flags
                }
            }
            else
            {
                Set(entity,
                    instanceId,
                    handlerId,
                    false);     // false => don't consider change flags
            }
        }

        /// <summary>
        /// Allows an entity handler to create or update an entity.
        /// <para/>
        /// A call to SetAll will replace all members of any existing entity with the given entity. I.e. the DOB
        /// will not merge the changes with any existing data, but will instead completely replace the old
        /// data. Use the SetChanges() method to make the DOB merge the data into the pool.
        /// <para/>
        /// Special care must be taken when the owner sets an entity that contains pointers (Entity ids)
        /// to other entity instances, and the lifetime of the pointed-to instance is completly connected
        /// to the lifetime of the referer. In this case the owner must traverse the object-graph
        /// and any unreferenced instance must be deleted.
        /// </summary>
        /// <param name="entity">Entity to create or update.</param>
        /// <param name="instanceId">Instance id.</param>
        /// <param name="handlerId">Handler id.</param>
        /// <exception cref="Safir.Dob.AccessDeniedException">The instance is owned by another handler.</exception>
        /// <exception cref="Safir.Dob.GhostExistsException">There is a ghost instance that hasn't been injected.</exception>
        public void SetAll(Entity entity,
                           Typesystem.InstanceId instanceId,
                           Typesystem.HandlerId handlerId)
        {
            Set(entity,
                instanceId,
                handlerId,
                false);     // false => don't consider change flags
        }

        // private method
        private void Set(Entity entity,
                         Typesystem.InstanceId instanceId,
                         Typesystem.HandlerId handlerId,
                         bool considerChangeFlags)
        {
            byte success;

            //TODO: serialize directly to shared memory
            System.Int32 blobSize = entity.CalculateBlobSize();
            System.IntPtr blob = Marshal.AllocHGlobal(blobSize); //allocate blob
            System.IntPtr beginningOfUnused;
            Typesystem.Internal.InternalOperations.FormatBlob(blob, blobSize, entity.GetTypeId(), out beginningOfUnused);
            entity.WriteToBlob(blob, ref beginningOfUnused);

            System.IntPtr instanceIdStr = Dob.Typesystem.Internal.InternalOperations.CStringOf(instanceId.RawString);
            System.IntPtr handlerIdStr = Dob.Typesystem.Internal.InternalOperations.CStringOf(handlerId.RawString);

            Interface.DoseC_SetEntity(ControllerId,
                                      blob,
                                      instanceId.RawValue,
                                      instanceIdStr,
                                      handlerId.RawValue,
                                      handlerIdStr,
                                      Interface.ByteOf(considerChangeFlags),
                                      Interface.ByteOf(false),   // false => this is not an initial injection
                                      out success);

            Marshal.FreeHGlobal(blob); //delete blob
            Marshal.FreeHGlobal(instanceIdStr);
            Marshal.FreeHGlobal(handlerIdStr);

            if (!Interface.BoolOf(success))
            {
                Typesystem.LibraryExceptions.Instance.Throw();
            }
        }

        /// <summary>
        /// Allows an entity handler to delete a specific owned entity instance.
        /// <para/>
        /// Used to delete a specific owned instance. Does nothing if the instance does not exist.
        /// </summary>
        /// <param name="entityId">Id of the entity to delete.</param>
        /// <param name="handlerId">Handler id.</param>
        /// <exception cref="Safir.Dob.AccessDeniedException">The instance is owned by another handler.</exception>
        /// <exception cref="Safir.Dob.GhostExistsException">There is a ghost instance that hasn't been injected.</exception>
        public void Delete(Typesystem.EntityId entityId,
                           Typesystem.HandlerId handlerId)
        {
            byte success;

            System.IntPtr instanceIdStr = Dob.Typesystem.Internal.InternalOperations.CStringOf(entityId.InstanceId.RawString);
            System.IntPtr handlerIdStr = Dob.Typesystem.Internal.InternalOperations.CStringOf(handlerId.RawString);

            Interface.DoseC_DeleteEntity(ControllerId,
                                         entityId.TypeId,
                                         entityId.InstanceId.RawValue,
                                         instanceIdStr,
                                         Interface.ByteOf(false), // false => this instance only
                                         handlerId.RawValue,
                                         handlerIdStr,
                                         out success);

            Marshal.FreeHGlobal(instanceIdStr);
            Marshal.FreeHGlobal(handlerIdStr);

            if (!Interface.BoolOf(success))
            {
                Typesystem.LibraryExceptions.Instance.Throw();
            }
        }

        /// <summary>
        /// Allows an entity handler to delete all owned instances.
        /// <para/>
        /// Used to delete all instances owned by the caller.
        /// </summary>
        /// <param name="typeId">Entity type.</param>
        /// <param name="handlerId">Handler id.</param>
        public void DeleteAllInstances(System.Int64 typeId,
                                       Typesystem.HandlerId handlerId)
        {
            byte success;

            System.IntPtr instanceIdStr = Dob.Typesystem.Internal.InternalOperations.CStringOf("");
            System.IntPtr handlerIdStr = Dob.Typesystem.Internal.InternalOperations.CStringOf(handlerId.RawString);

            Interface.DoseC_DeleteEntity(ControllerId,
                                         typeId,
                                         0,
                                         instanceIdStr,
                                         Interface.ByteOf(true),                        // true => all instances 
                                         handlerId.RawValue,
                                         handlerIdStr,
                                         out success);

            Marshal.FreeHGlobal(instanceIdStr);
            Marshal.FreeHGlobal(handlerIdStr);

            if (!Interface.BoolOf(success))
            {
                Typesystem.LibraryExceptions.Instance.Throw();
            }
        }
        
        #endregion

        #region Get iterators

        /// <summary>
        /// Retreives an enumerator that can be used to iterate over created instances.
        /// </summary>
        /// <param name="typeId">Entity type.</param>
        /// <param name="includeSubclasses">True =>  Iterate over subclass instances
        ///                                 False => No subclasses will be included.</param>
        /// <returns>An enumerator for entities</returns>
        public System.Collections.Generic.IEnumerable<EntityProxy>
        GetEntityEnumerator(System.Int64 typeId,
                            bool includeSubclasses)
        {
            Int32 iteratorId;
            byte success;
            byte end;
            Interface.DoseC_EntityIteratorCreate(ControllerId, typeId, Interface.ByteOf(includeSubclasses), out iteratorId, 
                                                 out end, out success);
            if (!Interface.BoolOf(success))
            {
                Typesystem.LibraryExceptions.Instance.Throw();
            }

            try
            {
                while (!Interface.BoolOf(end))
                {
                    System.IntPtr entityBlob;
                    System.IntPtr entityState;
                    Interface.DoseC_EntityIteratorDereference(ControllerId, iteratorId, out entityBlob, out entityState, out success);

                    using (EntityProxy theProxy = new EntityProxy(entityBlob, entityState, System.IntPtr.Zero, System.IntPtr.Zero, true, false))
                    {
                        yield return theProxy;
                    }

                    Interface.DoseC_EntityIteratorIncrement(ControllerId, iteratorId, out end, out success);

                    if (!Interface.BoolOf(success))
                    {
                        Typesystem.LibraryExceptions.Instance.Throw();
                    }
                }

                yield break;
            }
            finally
            {
                Interface.DoseC_EntityIteratorDestroy(ControllerId, iteratorId);
            }
        }
        
        #endregion

        #region Read operations

        /// <summary>
        /// Read an entity from the distributed object pool.
        /// <para/>
        /// Gets the current version of the entity that matches the given entity id.
        /// </summary>
        /// <param name="entityId">Entity id of the entity to read.</param>
        /// <returns>Entity read from the distributed object pool.</returns>
        /// <exception cref="Safir.Dob.NotFoundException">The specified instance of the entity does not exist.</exception>
        public EntityProxy Read(Typesystem.EntityId entityId)
        {
            byte success;
            System.IntPtr currentBlob;
            System.IntPtr currentState;

            System.IntPtr instanceIdStr = Dob.Typesystem.Internal.InternalOperations.CStringOf(entityId.InstanceId.RawString);

            Interface.DoseC_ReadEntity(ControllerId,
                                       entityId.TypeId,
                                       entityId.InstanceId.RawValue,
                                       instanceIdStr,
                                       out currentBlob,
                                       out currentState,
                                       out success);

            Marshal.FreeHGlobal(instanceIdStr);

            if (!Interface.BoolOf(success))
            {
                Typesystem.LibraryExceptions.Instance.Throw();
            }

            return new EntityProxy(currentBlob, currentState, System.IntPtr.Zero, System.IntPtr.Zero, false, false);
        }

        /// <summary>
        /// Check if an instance of an entity is created or not.
        /// <para/>
        /// This method will return true if the given entity instance is created.
        /// <para/>
        /// Note that the only time that you can really trust this information is if you're the one that
        /// has created the entity instance and no one is overregistering a handler with the same id as yours.
        /// Otherwise there is no guarantee that the instance still is created immediately after this call.
        /// The owner may be deleting it right after you asked, or your handler may have been revoked but
        /// you have not yet received a Revoke status. <para/>
        /// Use with care!
        /// </summary>
        /// <param name="entityId">Entity instance to check existence of.</param>
        /// <returns>True if the entity instance is created, otherwise false.</returns>
        public bool IsCreated(Typesystem.EntityId entityId)
        {
            byte success;
            byte isCreated;

            System.IntPtr instanceIdStr = Dob.Typesystem.Internal.InternalOperations.CStringOf(entityId.InstanceId.RawString);

            Interface.DoseC_IsCreated(ControllerId,
                                      entityId.TypeId,
                                      entityId.InstanceId.RawValue,
                                      instanceIdStr,
                                      out isCreated,
                                      out success);

            Marshal.FreeHGlobal(instanceIdStr);

            if (!Interface.BoolOf(success))
            {
                Typesystem.LibraryExceptions.Instance.Throw();
            }
            return Interface.BoolOf(isCreated);
        }

        /// <summary>
        /// This method is used to get the number of instances of an entity that exists.
        /// </summary>
        /// <param name="typeId">The type of the class whose instances we're counting.</param>
        /// <param name="handlerId">Count only instances owned by this handler (use HandlerId::ALL_HANDLERS
        ///                         to get all handlers).</param>
        /// <param name="includeSubclasses">Include subclasses when counting instances.</param>
        /// <returns>Number of instances</returns>
        public System.Int64 GetNumberOfInstances(System.Int64 typeId,
                                                 Safir.Dob.Typesystem.HandlerId handlerId,
                                                 bool includeSubclasses)
        {
            byte success;
            System.Int64 numInstances;

            System.IntPtr handlerIdStr = Dob.Typesystem.Internal.InternalOperations.CStringOf(handlerId.RawString);

            Interface.DoseC_GetNumberOfInstances(ControllerId,
                                                 typeId,
                                                 handlerId.RawValue,
                                                 handlerIdStr,
                                                 includeSubclasses,
                                                 out numInstances,
                                                 out success);

            Marshal.FreeHGlobal(handlerIdStr);

            if (!Interface.BoolOf(success))
            {
                Typesystem.LibraryExceptions.Instance.Throw();
            }
            return numInstances;
        }

        /// <summary>
        /// This method is used to get the instanceIdPolicy for a specific class and handler.
        /// </summary>
        /// <param name="typeId">The type of the class the handler is registered for.</param>
        /// <param name="handlerId">Get instanceIdPolicy for this handler.</param>
        /// <returns>Specifies if the handler expects instance ids in create requests to be
        ///                          assigned by the requestor or if the handler assigns them by itself.</returns>
        /// <exception cref="Safir.Dob.NotFoundException">The given handlerId has not registered the given class.</exception>
        public InstanceIdPolicy.Enumeration GetInstanceIdPolicy(System.Int64 typeId,
                                                 Safir.Dob.Typesystem.HandlerId handlerId)
        {
            byte success;
            System.Int32 policy;

            System.IntPtr handlerIdStr = Dob.Typesystem.Internal.InternalOperations.CStringOf(handlerId.RawString);

            Interface.DoseC_GetInstanceIdPolicy(ControllerId,
                                                 typeId,
                                                 handlerId.RawValue,
                                                 out policy,
                                                 out success);

            Marshal.FreeHGlobal(handlerIdStr);

            if (!Interface.BoolOf(success))
            {
                Typesystem.LibraryExceptions.Instance.Throw();
            }
            return (InstanceIdPolicy.Enumeration)policy;
        }

        #endregion

        /// <summary>
        /// Interrupt the ongoing Dispatch even if all data to the application have not been distpatched.
        /// The dispatch-event will be automatically set to trigger a new Dispatch again.
        /// This can be used to ensure that too much time is not spent dispatching in a time-critical app.
        /// </summary>
        public void ExitDispatch()
        {
            byte success;
            Interface.DoseC_ExitDispatch(ControllerId, out success);

            if (!Interface.BoolOf(success))
            {
                Typesystem.LibraryExceptions.Instance.Throw();
            }
        }


        internal abstract System.Int32 ControllerId
        {
            get;
        }
    }
}
