-------------------------------------------------------------------------------
--
--  Copyright Saab AB, 2009-2013 (http://safir.sourceforge.net)
--
--  Created by: Anders Widén / stawi
--
-------------------------------------------------------------------------------
--
--  This file is part of Safir SDK Core.
--
--  Safir SDK Core is free software: you can redistribute it and/or modify
--  it under the terms of version 3 of the GNU General Public License as
--  published by the Free Software Foundation.
--
--  Safir SDK Core is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
--
-------------------------------------------------------------------------------
with Ada.Finalization;
with Safir.Dob.Typesystem;
with Safir.Dob.Typesystem.Channel_Id;
with Safir.Dob.Typesystem.Handler_Id;
with Safir.Dob.Typesystem.Entity_Id;
with Safir.Dob.Consumers;
with Safir.Dob.Defs;
with Safir.Dob.Entity;
with Safir.Dob.Message;
with Safir.Dob.Typesystem.Instance_Id;
with Safir.Dob.Instance_Id_Policy;
with Safir.Dob.Entity_Proxies;
with Safir.Dob.Service;
with Safir.Dob.Entity_Iterators;

package Safir.Dob.Connection_Bases is

   type Connection_Base is abstract new
     Ada.Finalization.Limited_Controlled with null record;

   -- Check if this Connection instance is open.
   --
   -- Returns: True if the connection is open.
   --
   function Is_Open
     (Self : in Connection_Base) return Boolean is abstract;

   ---------------------------------------------------------------
   -- Non-pending (synchronous) registration of entity handler. --
   ---------------------------------------------------------------

   -- Registration of an entity handler for a given type.
   --
   -- Used to make a non-pending registration for a handler for a given type.
   -- Upon return from this method the given handler is guaranteed to be registered.
   -- (Any existing handler with the same id will be revoked.)
   --
   -- This is the preferred registration method to be used for a handler that has no need to get
   -- external entity injections (for example when there is no redundancy).
   --
   --  There can be any number of registered handlers for a type as long as each handler has
   --  a unique id.
   --
   --  Note that if you have a configuration where more than one application (connection) is registering
   --  the same type/handler, your own registration can still be revoked by another application.
   --
   -- Parameters: Type_Id - Entity type to register.
   --             Handler_Id - Handler id.
   --             Instance_Id_Policy - Specifies if the handler expects instance
   --                                  ids in create requests to be assigned by
   --                                  the requestor (normal case) or if the handler
   --                                  assigns them by itself.
   --             Entity_Handler - Callback consumer object.
   --
   procedure Register_Entity_Handler
     (Self               : in Connection_Base;
      Type_Id            : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id         : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;
      Instance_Id_Policy : in Safir.Dob.Instance_Id_Policy.Enumeration;
      Entity_Handler     : access Safir.Dob.Consumers.Entity_Handler'Class);

   -- Register an entity handler that also gets informed about injected entities.
   --
   -- Used to make a non-pending registration for a handler for a given type when the handler
   -- also needs to be informed when an entity instance is about to be injected in the system.
   -- Upon return from this method the given handler is guaranteed to be registered.
   --
   -- There are two typical cases when entities are injected outside the control of a registered
   -- handler:
   --    * Reception of persistent entity instances.
   --    * Reception of entity instances from an external system installation (multi-owned entities).
   --
   --  After registration, any persistent data will be transfered to the handler via
   --  Entity_Injection_Base.On_Injected_New_Entity callbacks. When all persistent data have been transfered
   --  the handler is notified by the callback Entity_Injection_Base.On_Initial_Injections_Done. This callback
   --  is guaranteed to be invoked even when there is no persistent data at all.
   --
   -- Parameters: Type_Id - Entity type to register.
   --             Handler_Id - Handler id.
   --             Instance_Id_Policy - Specifies if the handler expects instance
   --                                  ids in create requests to be assigned by
   --                                  the requestor (normal case) or if the handler
   --                                  assigns them by itself.
   --             Entity_Handler_Injection - Callback consumer object.
   --
   procedure Register_Entity_Handler_Injection
     (Self                     : in Connection_Base;
      Type_Id                  : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id               : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;
      Instance_Id_Policy       : in Safir.Dob.Instance_Id_Policy.Enumeration;
      Entity_Handler_Injection : access Safir.Dob.Consumers.Entity_Handler_Injection'Class);

   ------------------------------------------------------------
   -- Pending (asynchronous) registration of entity handler. --
   ------------------------------------------------------------

   -- Pending registration of a handler for a given entity type.
   --
   -- Used to make a pending registration of a handler for a given type. Upon return from this method the given
   -- handler is registered as pending. In case an existing handler with the same handler id isn't already
   -- registered the Dob will immediately promote this handler to be the registered one.
   --
   -- The consumer is informed via callbacks of any change of the registration status (Completed or Revoked).
   --
   -- This method is to be used by applications that handles redundancy. The typical scenario
   -- is that two or more application instances make a pending registration with the same handler id.
   -- The Dob will assure that one of the handlers is promoted to registered and all others are pending. If
   -- there are several types involved in the redundancy switch it is often the case that you want
   -- just one application (the active one) to have all types registered. To achieve this, all applications make
   -- a pending registration for one of the types. The application that receives the Completed status
   -- then makes a non-pending registration for the remaining types using method Register_Entity_Handler
   -- (or Register_Entity_Handler_Injection).
   --
   -- Parameters: Type_Id - Entity type to register.
   --             Handler_Id - Handler id.
   --             Instance_Id_Policy - Specifies if the handler expects instance
   --                                  ids in create requests to be assigned by
   --                                  the requestor (normal case) or if the handler
   --                                  assigns them by itself.
   --             Entity_Handler_Pending - Callback consumer object.
   --
   procedure Register_Entity_Handler_Pending
     (Self                     : in Connection_Base;
      Type_Id                  : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id               : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;
      Instance_Id_Policy       : in Safir.Dob.Instance_Id_Policy.Enumeration;
      Entity_Handler_Pending   : access Safir.Dob.Consumers.Entity_Handler_Pending'Class);

   ----------------------------------------------------------------
   -- Non-pending (synchronous) registration of service handler. --
   ----------------------------------------------------------------

   -- Register a service handler for a given type.
   --
   -- See comments for Register_Entity_Handler.
   --
   -- Parameters: Type_Id - Service type to register.
   --             Handler_Id - Handler id.
   --             Service_Handler - Callback consumer object.
   --
   procedure Register_Service_Handler
     (Self               : in Connection_Base;
      Type_Id            : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id         : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;
      Service_Handler    : access Safir.Dob.Consumers.Service_Handler'Class);

   -------------------------------------------------------------
   -- Pending (asynchronous) registration of service handler. --
   -------------------------------------------------------------

   -- Pending registration of a handler for a given service type.
   --
   -- See comments for Register_Entity_Handler_Pending.
   --
   -- Parameters: Type_Id - Service type to register.
   --             Handler_Id - Handler id.
   --             Service_Handler_Pending - Callback consumer object.
   --
   procedure Register_Service_Handler_Pending
     (Self                     : in Connection_Base;
      Type_Id                  : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id               : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;
      Service_Handler_Pending  : access Safir.Dob.Consumers.Service_Handler_Pending'Class);

   --------------------
   -- Unregistration --
   --------------------

   -- Unregister of an entity handler or a service handler.
   --
   -- Any created entity instance owned by the given handler will be deleted.
   --
   -- This method can also be used to unregister a pending handler.
   --
   -- Using the constant Safir.Dob.Typesystem.Handler_Id.All_Handlers means that
   -- all handlers for the given type, registered by this connection, will be unregistered.
   --
   -- If the given handler isn't registered the call will be ignored.
   --
   -- Paramters: Type_Id - Type id of the entity or service to unregister.
   --            Handler_Id - Handler Id.
   --
   procedure Unregister_Handler
     (Self          : in Connection_Base;
      Type_Id       : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id    : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type);

   ---------------------------
   -- Message subscriptions --
   ---------------------------

   -- Set up subscription for messages of a certain type and its subclasses.
   --
   -- The subscriber can subscribe for messages sent on a specific cannel, or by using
   -- the constant Dob.Typesystem.Channel_Id.All_Channels, subscribe for all messages
   -- of the given type regardless of the channel id set by the sender.
   --
   -- Calling this method is identical to calling Subscribe_Message below with
   -- Include_Subclasses set to True.
   --
   -- Parameters: Type_Id - Type id of the message to subscribe for.
   --             Channel_Id - Channel id.
   --             Message_Subscriber - Message subscriber that will receive the messages.
   --
   procedure Subscribe_Message
     (Self                : in     Connection_Base;
      Type_Id             : in     Safir.Dob.Typesystem.Type_Id;
      Channel_Id          : in     Safir.Dob.Typesystem.Channel_Id.Channel_Id_Type;
      Message_Subscriber  : access Safir.Dob.Consumers.Message_Subscriber'Class);

   -- Set up subscription for messages of a certain type (additional parameters).
   --
   -- Overloaded method that gives the user the ability to determine if the subscription
   -- also will include subsclasses.
   --
   -- Parameters: Type_Id - Type id of the message to subscribe for.
   --             Channel_Id - Channel id.
   --             Include_Subclasses - True => Subscription for this message type and all its subclasses.
   --                                  False => No subclasses will be included.
   --             Message_Subscriber - Message subscriber that will receive the messages.
   --
   procedure Subscribe_Message
     (Self                : in     Connection_Base;
      Type_Id             : in     Safir.Dob.Typesystem.Type_Id;
      Channel_Id          : in     Safir.Dob.Typesystem.Channel_Id.Channel_Id_Type;
      Include_Subclasses  : in     Boolean;
      Message_Subscriber  : access Safir.Dob.Consumers.Message_Subscriber'Class);


   -- Remove a message subscription made by the given subscriber.
   --
   -- Removes the subscription for the given type and its subclasses.
   --
   -- Using the constant Dob.Typesystem.Channel_Id.All_Channels means that all subscriptions
   -- for the given type and its subclasses will be removed.
   --
   -- If no subscription exists the call will be ignored.
   --
   -- Calling this method is identical to calling the UnsubscribeMessage below with
   -- Include_Subclasses => True.
   --
   -- Parameters: Type_Id - Type id of the message to unsubscribe for.
   --             Channel_Id - Channel id.
   --             Message_Subscriber - The Message_Subscriber consumer that was used when
   --                                  the subscription was initiated.
   --
   procedure Unsubscribe_Message
     (Self                : in     Connection_Base;
      Type_Id             : in     Safir.Dob.Typesystem.Type_Id;
      Channel_Id          : in     Safir.Dob.Typesystem.Channel_Id.Channel_Id_Type;
      Message_Subscriber  : access Safir.Dob.Consumers.Message_Subscriber'Class);

   -- Remove a message subscription made by the given subscriber (additional parameters).
   --
   -- Overloaded method that gives the user the ability to determine if unsubscription
   -- also will include subsclasses.
   --
   -- Parameters: Type_Id - Type id of the message to unsubscribe for.
   --             Channel_Id - Channel id.
   --             Include_Subclasses - True => Unsubscribe for this message type and all its subclasses.
   --                                  False => No subclasses will be included.
   --             Message_Subscriber - The Message_Subscriber consumer that was used when
   --                                  the subscription was initiated.
   --
   procedure Unsubscribe_Message
     (Self                : in     Connection_Base;
      Type_Id             : in     Safir.Dob.Typesystem.Type_Id;
      Channel_Id          : in     Safir.Dob.Typesystem.Channel_Id.Channel_Id_Type;
      Include_Subclasses  : in     Boolean;
      Message_Subscriber  : access Safir.Dob.Consumers.Message_Subscriber'Class);

   -----------------------
   -- Entity subscriptions
   -----------------------

   -- Set up subscription for instances of an entity type and its subclasses.
   --
   -- The subscriber will receive information about creations, updates and deletes of
   -- instances of the given entity type and its subclasses.
   --
   -- When setting up a subscription the user will get initial data for existing entity
   -- instances in the form of On_New_Entity callbacks. This is true even when
   -- setting up a subscription for an already subscribed entity.
   --
   -- Parameters: Type_Id - Type id of the message to unsubscribe for.
   --             Entity_Subscriber - Consumer that will receive the entities.
   --
   procedure Subscribe_Entity
     (Self                : in Connection_Base;
      Type_Id             : in Safir.Dob.Typesystem.Type_Id;
      Entity_Subscriber   : access Safir.Dob.Consumers.Entity_Subscriber'Class);

   -- Subscription for an entity type (additional parameters).
   --
   -- Overloaded method that gives the user the ability to a determine more details
   -- conscerning the subscription.
   --
   -- Parameters: Type_Id - Type id of the entity to unsubscribe for.
   --             Include_Updates - True => Subscription includes update, as well as create and delete.
   --                               False => Subscription includes no updates, only create and deletion.
   --             Include_Subclasses - True => Subscription for this entity type and all its subclasses.
   --                                  False => No subclasses will be included.
   --             Restart_Subscription - True => OnNewEntity callbacks are generated even if the subscription already exists.
   --                                    False=> OnNewEntity callbacks are generated only for instances that are not previously subscribed.
   --             Entity_Subscriber - Consumer that will receive the entities.
   --
   procedure Subscribe_Entity
     (Self                 : in Connection_Base;
      Type_Id              : in Safir.Dob.Typesystem.Type_Id;
      Include_Updates      : in Boolean;
      Include_Subclasses   : in Boolean;
      Restart_Subscription : in Boolean;
      Entity_Subscriber    : access Safir.Dob.Consumers.Entity_Subscriber'Class);

   -- Set up subscription for a specific instance of an entity type.
   --
   -- When setting up a subscription the user will get initial data in the form of a On_New_Entity callback
   -- with the current state for the subscribed entity (if created).
   --
   -- Parameters: Entity_Id - Entity id of the entity to subscribe for.
   --             Include_Updates - True => Subscription includes update, as well as create and delete.
   --                               False => Subscription includes no updates, only create and deletion.
   --             Restart_Subscription - True => OnNewEntity callbacks are generated even if the subscription already exists.
   --                                    False=> OnNewEntity callbacks are generated only for instances that are not previously subscribed.
   --             Entity_Subscriber - Consumer that will receive the entities.
   --
   procedure Subscribe_Entity
     (Self                 : in Connection_Base;
      Entity_Id            : in Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type;
      Include_Updates      : in Boolean;
      Restart_Subscription : in Boolean;
      Entity_Subscriber    : access Safir.Dob.Consumers.Entity_Subscriber'Class);

   -- Remove an entity subscription made by the given subscriber.
   --
   -- Removes the subscription for the given type and its subclasses.
   --
   -- If no subscription exists the call will be ignored.
   --
   -- Parameters: Type_Id - Type id of the entity to unsubscribe for.
   --             Entity_Subscriber - The EntitySubscriber consumer that was used when
   --                                 the subscription was initiated.
   procedure Unsubscribe_Entity
     (Self                 : in Connection_Base;
      Type_Id              : in Safir.Dob.Typesystem.Type_Id;
      Entity_Subscriber    : access Safir.Dob.Consumers.Entity_Subscriber'Class);

   -- Remove an entity subscription made by the given subscriber (additional parameters).
   --
   -- Overloaded method that gives the user the ability to determine if unsubscription
   -- also will include subsclasses.
   --
   -- Parameters: Type_Id - Type id of the entity to unsubscribe for.
   --             Include_Subclasses - True => Unsubscription for this entity type and all its subclasses.
   --                                  False => Unsubscribe for just this type (no subclasses).
   --             Entity_Subscriber - The EntitySubscriber consumer that was used when
   --                                 the subscription was initiated.
   procedure Unsubscribe_Entity
     (Self                 : in Connection_Base;
      Type_Id              : in Safir.Dob.Typesystem.Type_Id;
      Include_Subclasses   : in Boolean;
      Entity_Subscriber    : access Safir.Dob.Consumers.Entity_Subscriber'Class);

   -- Remove an entity instance subscription made by the given subscriber.
   --
   -- If no subscription exists the call will be ignored.
   --
   -- Parameters: Entity_Id - Entity id of the entity to subscribe for.
   --             Entity_Subscriber - The EntitySubscriber consumer that was used when
   --                                 the subscription was initiated.
   procedure Unsubscribe_Entity
     (Self                 : in Connection_Base;
      Entity_Id            : in Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type;
      Entity_Subscriber    : access Safir.Dob.Consumers.Entity_Subscriber'Class);

   -----------------------------
   -- Registration subscriptions
   -----------------------------

   -- Set up subscription for notifications about when a specific handler for an entity type or
   -- a service type is registered and unregistered.
   --
   -- Using the constant Safir.Dob.Typesystem.Handler_Id.All_Handlers means that the subscriber will receive
   -- registrations/unregistrations of any handler for the given type.
   --
   -- Using a specific handler id means that the subscriber will receive only the
   -- registrations/unregistrations of the specified handler.
   --
   -- When setting up a subscription the user will get initial information about existing handlers via
   -- On_Registered callbacks.
   --
   -- Parameters: Type_Id - Type id of entity or service.
   --             Handler_Id - Handler id.
   --             Include_Subclasses - True => Subscription for this entity type or service type and all its subclasses.
   --                                  False => No subclasses will be included.
   --             Restart_Subscription - True => On_Registered callbacks are generated even if the subscription already exists.
   --                                    False=> On_Registered callbacks are generated only for instances that are not previously subscribed.
   --             Registration_Subscriber - Consumer that will receive the subscription response.
   --
   procedure Subscribe_Registration
     (Self                     : in     Connection_Base;
      Type_Id                  : in     Safir.Dob.Typesystem.Type_Id;
      Handler_Id               : in     Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;
      Include_Subclasses       : in     Boolean;
      Restart_Subscription     : in     Boolean;
      Registration_Subscriber  : access Safir.Dob.Consumers.Registration_Subscriber'Class);

   -- Removes a registration subscription.
   --
   -- Using the constant Safir.Dob.Typesystem.Handler_Id.All_Handlers means that all registration subscriptions,
   -- for the given type id and consumer, are removed.
   --
   -- If no subscription exists the call will be ignored.
   --
   -- Parameters: Type_Id - Type id of entity or service.
   --             Handler_Id - Handler id.
   --             Include_Subclasses - True => Unsubscribe for this entity type or service type and all its subclasses.
   --                                  False => No subclasses will be included.
   --             Registration_Subscriber - Consumer that was used when
   --                                       the subscription was initiated.
   --
   procedure Unsubscribe_Registration
     (Self                     : in     Connection_Base;
      Type_Id                  : in     Safir.Dob.Typesystem.Type_Id;
      Handler_Id               : in     Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;
      Include_Subclasses       : in     Boolean;
      Registration_Subscriber  : access Safir.Dob.Consumers.Registration_Subscriber'Class);

   -------------------
   -- Send messages --
   -------------------

   -- Send a message on the specified channel.
   --
   -- The application must be prepared to handle the situation that the outgoing send queue is full
   -- (Overflow_Exception is thrown). In this case the application is responsible for resending
   -- the message. When the overflow situation is dissolved, the application is notified by the
   -- Message_Sender.On_Not_Message_Overflow callback, which should trig the resending.
   --
   -- Parameters: Message - Message to send.
   --             Channel_Id - Channel id.
   --             Message_Sender - Receives notification about overflow status.
   -- Exceptions: Overflow_Exception - There was an overflow when sending.
   --
   procedure Send
     (Self           : in Connection_Base;
      Message        : in Safir.Dob.Message.Smart_Pointer'Class;
      Channel_Id     : in Safir.Dob.Typesystem.Channel_Id.Channel_Id_Type;
      Message_Sender : access Safir.Dob.Consumers.Message_Sender'Class);

   --------------
   -- Requests --
   --------------

   -- Request to a handler to create an entity instance without specifying the instanceId.
   --
   -- If the handler is registered as "HandlerDecidesInstanceId" the Instance_Id of the
   -- entity to be created is determined by the application, and the requestor is
   -- told which InstanceId will be used by the Entity_Id_Response.
   --
   -- If the handler is registered as "RequestorDecidesInstanceId" an InstanceId will
   -- be randomly generated and included in the request. This InstanceId *must* be
   -- used by the handler as the new instance number.
   --
   -- The application must be prepared to handle the situation that the outgoing send queue is full
   -- (Overflow_Exception is thrown). In this case the application is responsible for resending
   -- of the create request. When the overflow situation is dissolved, the application is
   -- notified by the Requestor.On_Not_Request_Overflow callback, which should trig the resending.
   --
   -- Parameters: Request - Entity requested to be created.
   --             Handler_Id - Handler id.
   --             Requestor - Requestor for response and notification about overflow status.
   -- Returns: Request id that can be used to match sent request with the response.
   -- Exceptions: Overflow_Exception - There was an overflow when sending.
   --
   function Create_Request
     (Self           : in Connection_Base;
      Request        : in Safir.Dob.Entity.Smart_Pointer'Class;
      Handler_Id     : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;
      Requestor      : access Safir.Dob.Consumers.Requestor'Class) return
     Safir.Dob.Defs.Request_Id;

   -- Request to a handler to create a specific entity instance.
   --
   -- If the handler is registered as "RequestorDecidesInstanceId" the Requestor must
   -- specify which instance is to be created. (If it doesnt care it can use the method
   -- above that will generate one randomly.)
   -- If the Requestor wants a random instance, but needs to know which instance will
   -- get created, it can use Dob.Typesystem.Instance_Id.Create_Random_Instance_Id to generate
   -- an instance id to pass to this method.
   --
   -- Note that it is illegal to call this method if the handler is registered as
   -- "HandlerDecidesInstanceId".
   --
   -- The application must be prepared to handle the situation that the outgoing send queue is full
   -- (Overflow_Exception is thrown). In this case the application is responsible for resending
   -- of the entity create request. When the overflow situation is dissolved, the application is
   -- notified by the Requestor.On_Not_Request_Overflow callback, which should trig the resending.
   --
   -- Parameters: Request - Entity requested to be created.
   --             Instance_Id - Instance id.
   --             Handler_Id - Handler id.
   --             Requestor - Requestor for response and notification about overflow status.
   -- Returns: Request id that can be used to match sent request with the response.
   -- Exceptions: Overflow_Exception - There was an overflow when sending.
   --
   function Create_Request
     (Self           : in Connection_Base;
      Request        : in Safir.Dob.Entity.Smart_Pointer'Class;
      Instance_Id    : in Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type;
      Handler_Id     : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;
      Requestor      : access Safir.Dob.Consumers.Requestor'Class) return
     Safir.Dob.Defs.Request_Id;

   -- Send an update request on an existing entity instance.
   --
   -- An update request will be sent to the handler that owns (has created) the entity.
   --
   -- The application must be prepared to handle the situation that the outgoing send queue is full
   -- (Overflow_Exception is thrown). In this case the application is responsible for resending
   -- of the entity update request. When the overflow situation is dissolved, the application is
   -- notified by the Requestor.On_Not_Request_Overflow callback, which should trig the resending.
   --
   -- Parameters: Request - Entity requested to be updated.
   --             Instance_Id - Instance id.
   --             Requestor - Requestor for response and notification about overflow status.
   -- Returns: Request id that can be used to match sent request with the response.
   -- Exceptions: Overflow_Exception - There was an overflow when sending.
   --
   function Update_Request
     (Self           : in Connection_Base;
      Request        : in Safir.Dob.Entity.Smart_Pointer'Class;
      Instance_Id    : in Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type;
      Requestor      : access Safir.Dob.Consumers.Requestor'Class) return
     Safir.Dob.Defs.Request_Id;

   -- Send a delete request on an existing entity instance.
   --
   -- A delete request will be sent to the handler that owns (has created) the entity.
   --
   -- The application must be prepared to handle the situation that the outgoing send queue is full
   -- (Overflow_Exception is thrown). In this case the application is responsible for resending
   -- of the entity delete request. When the overflow situation is dissolved, the application is
   -- notified by the Requestor.On_Not_Request_Overflow callback, which should trig the resending.
   --
   -- Parameters: Entity_Id - Entity id of the entity to be deleted.
   --             Requestor - Requestor for response and notification about overflow status.
   -- Returns: Request id that can be used to match sent request with the response.
   -- Exceptions: Overflow_Exception - There was an overflow when sending.
   --
   function Delete_Request
     (Self           : in Connection_Base;
      Entity_Id      : in Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type;
      Requestor      : access Safir.Dob.Consumers.Requestor'Class) return
     Safir.Dob.Defs.Request_Id;

   -- Send a request to the given service handler.
   --
   -- The application must be prepared to handle the situation that the outgoing send queue is full
   -- (Overflow_Exception is thrown). In this case the application is responsible for resending
   -- of the service request. When the overflow situation is dissolved, the application is notified
   --  by the Requestor.On_Not_Request_Overflow callback, which should trig the resending.
   --
   -- Parameters: Request - The service request.
   --             Handler_Id - Service handler id.
   --             Requestor - Requestor for response and notification about overflow status.
   -- Returns: Request id that can be used to match sent request with the response.
   -- Exceptions: Overflow_Exception - There was an overflow when sending.
   --
   function Service_Request
     (Self           : in Connection_Base;
      Request        : in Safir.Dob.Service.Smart_Pointer'Class;
      Handler_Id    : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;
      Requestor      : access Safir.Dob.Consumers.Requestor'Class) return
     Safir.Dob.Defs.Request_Id;

   -------------------
   -- Entity owners --
   -------------------

   -- Merge the changed members of an entity straight into the pool (the given handler must be the owner).
   --
   -- All members of the given entity that are marked as changed will be merged into the
   -- current object in the pool.
   -- If the object is not already set in the pool the entity will be set without any merging.
   --
   -- Parameters: Entity - Entity to create or update.
   --             Instance_Id - Instance id.
   --             Handler_Id - Handler id.
   -- Exceptions: Access_Denied_Exception - The instance is owned by another handler.
   --
   procedure Set_Changes
     (Self          : in Connection_Base;
      Entity        : in Safir.Dob.Entity.Smart_Pointer'Class;
      Instance_Id   : in Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type;
      Handler_Id    : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type);

   -- Allows an entity handler to create or update an entity.
   --
   -- A call to Set_All will replace all members of any existing entity with the given entity. I.e. the DOB
   -- will not merge the changes with any existing data, but will instead completely replace the old
   -- data. Use the Set_Changes method to make the DOB merge the data into the pool.
   --
   -- Special care must be taken when the owner sets an entity that contains pointers (Entity ids)
   -- to other entity instances, and the lifetime of the pointed-to instance is completly connected
   -- to the lifetime of the referer. In this case the owner must traverse the object-graph
   -- and any unreferenced instance must be deleted.
   --
   -- Parameters: Entity - Entity to create or update.
   --             Instance_Id - Instance id.
   --             Handler_Id - Handler id.
   -- Exceptions: Access_Denied_Exception - The instance is owned by another handler.
   --
   procedure Set_All
     (Self          : in Connection_Base;
      Entity        : in Safir.Dob.Entity.Smart_Pointer'Class;
      Instance_Id   : in Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type;
      Handler_Id    : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type);

   -- Allows an entity handler to delete a specific owned entity instance.
   --
   -- Used to delete a specific owned instance. Does nothing if the instance does not exist.
   --
   -- Parameters: Entity_Id - Id of the entity to delete.
   --             Handler_Id - Handler id.
   -- Exceptions: Access_Denied_Exception - The instance is owned by another handler.
   --
   procedure Delete
     (Self          : in Connection_Base;
      Entity_Id     : in Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type;
      Handler_Id    : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type);

   -- Allows an entity handler to delete all owned instances.
   --
   -- Used to delete all instances owned by the caller.
   --
   -- Parameters: Type_Id - Entity type.
   --             Handler_Id - Handler id.
   --
   procedure Delete_All_Instances
     (Self        : in Connection_Base;
      Type_Id     : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id  : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type);

   --------------
   -- Iterator --
   --------------

   -- Retreives an iterator to iterate over created instances.
   --
   -- The iterator addresses the first created entity instance of the given type.
   -- If there are no created instances an iterator representing "end" will be returned.
   --
   -- Parameters: Type_Id - Entity type.
   --             Include_Subclasses - True =>  Iterate over subclass instances.
   --                                  False => No subclasses will be included.
   -- Returns: An Entity iterator.
   --
   function Get_Entity_Iterator
     (Self               : in Connection_Base;
      Type_Id            : in Safir.Dob.Typesystem.Type_Id;
      Include_Subclasses : in Boolean) return Safir.Dob.Entity_Iterators.Entity_Iterator;

   ---------------------
   -- Read operations --
   ---------------------

   -- Read an entity from the distributed object pool.
   --
   -- Gets the current version of the entity that matches the given entity id.
   --
   -- Parameters: Entity_Id - Entity id of the entity to read.
   -- Returns: Entity read from the distributed object pool.
   -- Exceptions: Not_Found_Exception - The specified instance of the entity does not exist.
   --
   function Read
     (Self      : in Connection_Base;
      Entity_Id : in Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type)
      return Safir.Dob.Entity_Proxies.Entity_Proxy;

   -- Check if an instance of an entity is created or not.
   --
   -- This method will return true if the given entity instance is created.
   --
   -- Note that the only time that you can really trust this information is if you're the one that
   -- has created the entity instance and no one is overregistering a handler with the same id as yours.
   -- Otherwise there is no guarantee that the instance still is created immediately after this call.
   -- The owner may be deleting it right after you asked, or your handler may have been revoked but
   -- you have not yet received a Revoke status.
   -- Use with care!
   --
   -- Parameters: Entity_Id - Entity instance to check existence of.
   -- Returns: True if the entity instance is created, otherwise false.
   --
   function Is_Created
     (Self      : in Connection_Base;
      Entity_Id : in Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type)
      return Boolean;


   -- Paramaters: Type_Id - The type of the class whose instances we're counting.
   --             Handler_Id - Count only instances owned by this handler
   --                          (use Handler_Id.All_Handlers to get all handlers).
   --             Include_Subclasses - Include subclasses when counting instances.
   --
   function Get_Number_Of_Instances
     (Self               : in Connection_Base;
      Type_Id            : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id         : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;
      Include_Subclasses : in Boolean) return Safir.Dob.Typesystem.Int_64;


   -- Parameters: Type_Id - The type of the class the handler is registered for.
   --             Handler_Id - Get instanceIdPolicy for this handler.
   --
   -- Returns: instanceIdPolicy - Specifies if the handler expects instance ids in create requests to be
   --                             assigned by the requestor or if the handler assigns them by itself.
   --
   function Get_Instance_Id_Policy
     (Self               : in Connection_Base;
      Type_Id            : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id         : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type) return Safir.Dob.Instance_Id_Policy.Enumeration;

   ----------------
   -- Exit dispatch
   ----------------

   -- Interrupt the ongoing Dispatch even if all data to the application have
   -- not been distpatched.
   -- The dispatch-event will be automatically set to trigger a new Dispatch again.
   -- This can be used to ensure that too much time is not spent dispatching in
   -- a time-critical app.
   --
   procedure Exit_Dispatch (Self : in Connection_Base);

   -----------------------
   -- Get_Controller_Id --
   -----------------------

   function Get_Controller_Id (Self : in Connection_Base) return Safir.Dob.Defs.Controller_Id is abstract;


private

   procedure Set
     (Self                  : in Connection_Base;
      Entity                : in Safir.Dob.Entity.Smart_Pointer'Class;
      Instance_Id           : in Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type;
      Handler_Id            : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;
      Consider_Change_Flags : in Boolean);


end Safir.Dob.Connection_Bases;
