-------------------------------------------------------------------------------
--
--  Copyright Saab AB, 2009 (http://www.safirsdk.com)
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
with Safir.Dob.Consumer_Bases; use Safir.Dob.Consumer_Bases;
with Safir.Dob.Response_Proxies;
with Safir.Dob.Message_Proxies;
with Safir.Dob.Entity_Proxies;
with Safir.Dob.Typesystem;
with Safir.Dob.Typesystem.Handler_Id;
--  with Safir.Dob.Entity_Request_Proxy;
--  with Safir.Dob.Service_Request_Proxy;

--  with Safir.Dob.Response_Sender;

package Safir.Dob.Consumers is

   -- ************
   -- Stop_Handler
   --
   -- Interface for reception of a stop order
   --
   type Stop_Handler is limited interface and Internal.Consumer_Base;

   -- When called the application owning the connection shall stop its execution.
   --
   procedure On_Stop_Order (Self : in out Stop_Handler) is abstract;

   -- **********
   -- Dispatcher
   --
   -- Interface for reception of a stop order
   --
   type Dispatcher is limited interface and Internal.Consumer_Base;

   -- Indicates that there is incoming data for the connection so the application shall
   -- call Dispatch.
   --
   -- When this method is called the application MUST call the Dispatch method for the connection.
   -- Note that Dispatch is NOT to be called directly from within this method. Instead the application
   -- shall set an event or similar and then call Dispatch from the thread that owns (has called Open)
   -- the connection.
   --
   procedure On_Do_Dispatch (Self : in out Dispatcher) is abstract;

   -- **************
   -- Entity_Handler
   --
   -- Interface to be implemented by an entity handler that makes a non-pending registration
   -- and that doesn't handle injected entities.
   --
   -- The following primitive operations NEED to be implemented:
   -- =========================================================
   --
   -- procedure On_Revoked_Registration
   --   (Self       : in Entity_Handler;
   --    Type_Id    : in Safir.Dob.Typesystem.Type_Id;
   --    Handler_Id : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type);
   --
   -- procedure On_Create_Request
   --   (Self                 : in Entity_Handler;
   --    Entity_Request_Proxy : in Safir.Dob.Entity_Request_Proxies.Entity_Request_Proxy;
   --    Response_Sender      : in Safir.Dob.Response_Sender.Response_Sender_Type);
   --
   -- procedure On_Update_Request
   --   (Self                 : in Entity_Handler;
   --    Entity_Request_Proxy : in Safir.Dob.Entity_Request_Proxies.Entity_Request_Proxy;
   --    Response_Sender      : in Safir.Dob.Response_Sender.Response_Sender_Type);
   --
   -- procedure On_Delete_Request
   --   (Self                 : in Entity_Handler;
   --    Entity_Request_Proxy : in Safir.Dob.Entity_Request_Proxies.Entity_Request_Proxy;
   --    Response_Sender      : in Safir.Dob.Response_Sender.Response_Sender_Type);
   --
   type Entity_Handler is limited interface and Revoked_Registration_Base and Entity_Request_Base;

   -- ************************
   -- Entity_Handler_Injection
   --
   -- Interface to be implemented by an entity handler that makes a non-pending registration
   -- for a type that can potentially be injected.
   --
   -- The following primitive operations NEED to be implemented:
   -- =========================================================
   -- procedure On_Revoked_Registration
   --   (Self       : in Entity_Handler_Injection;
   --    Type_Id    : in Safir.Dob.Typesystem.Type_Id;
   --    Handler_Id : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type);
   --
   -- procedure On_Create_Request
   --   (Self                 : in Entity_Handler_Injection;
   --    Entity_Request_Proxy : in Safir.Dob.Entity_Request_Proxies.Entity_Request_Proxy;
   --    Response_Sender      : in Safir.Dob.Response_Sender.Response_Sender_Type);
   --
   -- procedure On_Update_Request
   --   (Self                 : in Entity_Handler_Injection;
   --    Entity_Request_Proxy : in Safir.Dob.Entity_Request_Proxies.Entity_Request_Proxy;
   --    Response_Sender      : in Safir.Dob.Response_Sender.Response_Sender_Type);
   --
   -- procedure On_Delete_Request
   --   (Self                 : in Entity_Handler_Injection;
   --    Entity_Request_Proxy : in Safir.Dob.Entity_Request_Proxies.Entity_Request_Proxy;
   --    Response_Sender      : in Safir.Dob.Response_Sender.Response_Sender_Type);
   --
   -- The following primitive operations MAY be overridden:
   -- ====================================================
   -- procedure On_Injected_New_Entity
   --   (Self                  : in Entity_Handler_Injection;
   --    Injected_Entity_Proxy : in Safir.Dob.Injected_Entity_Proxies.Injected_Entity_Proxy);
   --
   -- procedure On_Injected_Updated_Entity
   --   (Self                  : in Entity_Handler_Injection;
   --    Injected_Entity_Proxy : in Safir.Dob.Injected_Entity_Proxies.Injected_Entity_Proxy);
   --
   --  procedure On_Injected_Deleted_Entity
   --    (Self                  : in Entity_Handler_Injection;
   --     Injected_Entity_Proxy : in Safir.Dob.Injected_Entity_Proxies.Injected_Entity_Proxy);

   type Entity_Handler_Injection is limited interface and Revoked_Registration_Base and Entity_Injection_Base;

   -- **********************
   -- Entity_Handler_Pending
   --
   -- Interface to be implemented by an entity handler that makes a pending registration.
   --
   -- The following primitive operations NEED to be implemented:
   -- =========================================================
   -- procedure On_Revoked_Registration
   --   (Self       : in Entity_Handler_Pending;
   --    Type_Id    : in Safir.Dob.Typesystem.Type_Id;
   --    Handler_Id : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type);
   --
   -- procedure On_Completed_Registration
   --   (Self       : in Entity_Handler_Pending;
   --    Type_Id    : in Safir.Dob.Typesystem.Type_Id;
   --    Handler_Id : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type);
   --
   -- procedure On_Create_Request
   --   (Self                 : in Entity_Handler_Pending;
   --    Entity_Request_Proxy : in Safir.Dob.Entity_Request_Proxies.Entity_Request_Proxy;
   --    Response_Sender      : in Safir.Dob.Response_Sender.Response_Sender_Type);
   --
   -- procedure On_Update_Request
   --   (Self                 : in Entity_Handler_Pending;
   --    Entity_Request_Proxy : in Safir.Dob.Entity_Request_Proxies.Entity_Request_Proxy;
   --    Response_Sender      : in Safir.Dob.Response_Sender.Response_Sender_Type);
   --
   -- procedure On_Delete_Request
   --   (Self                 : in Entity_Handler_Pending;
   --    Entity_Request_Proxy : in Safir.Dob.Entity_Request_Proxies.Entity_Request_Proxy;
   --    Response_Sender      : in Safir.Dob.Response_Sender.Response_Sender_Type);
   --
   -- The following primitive operations MAY be overridden:
   -- ====================================================
   -- procedure On_Injected_New_Entity
   --   (Self                  : in Entity_Handler_Pending;
   --    Injected_Entity_Proxy : in Safir.Dob.Injected_Entity_Proxies.Injected_Entity_Proxy);
   --
   -- procedure On_Injected_Updated_Entity
   --   (Self                  : in Entity_Handler_Pending;
   --    Injected_Entity_Proxy : in Safir.Dob.Injected_Entity_Proxies.Injected_Entity_Proxy);
   --
   -- procedure On_Injected_Deleted_Entity
   --   (Self                  : in Entity_Handler_Pending;
   --     Injected_Entity_Proxy : in Safir.Dob.Injected_Entity_Proxies.Injected_Entity_Proxy);
   --
   -- procedure On_Initial_Injections_Done
   --   (Self                  : in Entity_Handler_Pending;
   --    Type_Id               : in Safir.Dob.Typesystem.Type_Id;
   --    Handler_Id            : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type);

   type Entity_Handler_Pending is limited interface and Completed_Registration_Base and Entity_Injection_Base;

   -- ***************
   -- Service_Handler
   --
   -- Interface to be implemented by a service handler that makes a non-pending registration.
   --
   -- The following primitive operations NEED to be implemented:
   -- =========================================================
   -- procedure On_Revoked_Registration
   --   (Self       : in Service_Handler;
   --    Type_Id    : in Safir.Dob.Typesystem.Type_Id;
   --    Handler_Id : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type);
   --
   -- procedure On_Service_Request
   --   (Self                  : in Service_Handler;
   --    Service_Request_Proxy : in Safir.Dob.Service_Request_Proxies.Service_Request_Proxy;
   --    Response_Sender       : in Safir.Dob.Response_Sender.Response_Sender_Type);
   --
   type Service_Handler is limited interface and Revoked_Registration_Base and Service_Request_Base;

   -- ***********************
   -- Service_Handler_Pending
   --
   -- Interface to be implemented by a service handler that makes a pending registration.
   --
   -- The following primitive operations NEED to be implemented:
   -- =========================================================
   -- procedure On_Revoked_Registration
   --   (Self       : in Service_Handler_Pending;
   --    Type_Id    : in Safir.Dob.Typesystem.Type_Id;
   --    Handler_Id : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type);
   --
   -- procedure On_Completed_Registration
   --   (Self       : in Service_Handler_Pending;
   --    Type_Id    : in Safir.Dob.Typesystem.Type_Id;
   --    Handler_Id : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type);
   --
   -- procedure On_Service_Request
   --   (Self                  : in Service_Handler_Pending;
   --    Service_Request_Proxy : in Safir.Dob.Service_Request_Proxies.Service_Request_Proxy;
   --    Response_Sender       : in Safir.Dob.Response_Sender.Response_Sender_Type);
   --
   type Service_Handler_Pending is limited interface and Completed_Registration_Base and Service_Request_Base;

   -- *********
   -- Requestor
   --
   -- Interface to be implemented by an application that sends requests.
   -- (Request on entities or service requests)
   --
   type Requestor is limited interface and Internal.Consumer_Base;

   -- Called when a response is received on a sent request.
   --
   -- Parameters: Response_Proxy - Proxy object containing the response and meta information.
   --
   procedure On_Response
     (Self           : in out Requestor;
      Response_Proxy : in Safir.Dob.Response_Proxies.Response_Proxy) is abstract;

   -- Called to indicate that it is meningful to make a retry after an overflow situation.
   --
   procedure On_Not_Request_Overflow (Self : in out Requestor) is abstract;

   -- **************
   -- Message_Sender
   --
   -- Interface to be implemented by senders of messages.
   --
   type Message_Sender is limited interface and Internal.Consumer_Base;

   -- Called to indicate that it is meningful to make a retry after an overflow situation.
   --
   procedure On_Not_Message_Overflow (Self : in out Message_Sender) is abstract;

   -- ***********************
   -- Registration_Subscriber
   --
   -- Interface to be implemented by subscribers of handler registrations.
   --
   type Registration_Subscriber is limited interface and Internal.Consumer_Base;

   -- Called when a handler for an entity or service has been registered.
   --
   -- Parameters: Type_Id - Type id of the registered entity or service.
   --             Handler_Id - Handler id of the registered handler.
   --
   procedure On_Registered
     (Self       : in out Registration_Subscriber;
      Type_Id    : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type) is abstract;

   -- Called when a handler for an entity or service has been unregistered.
   --
   -- Parameters: Type_Id - Type id of the unregistered entity or service.
   --             Handler_Id - Handler id of the unregistered handler.
   --
   procedure On_Unregistered
     (Self       : in out Registration_Subscriber;
      Type_Id    : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type) is abstract;

   -- ******************
   -- Message_Subscriber
   --
   -- Interface to be implemented by subscribers of messages.
   --
   type Message_Subscriber is limited interface and Internal.Consumer_Base;

   -- Called when a message is received.
   --
   -- Parameters: Message_Proxy - Proxy object containing received message and meta information.
   --
   procedure On_Message
     (Self          : in out Message_Subscriber;
      Message_Proxy : in Safir.Dob.Message_Proxies.Message_Proxy) is abstract;

   -- *****************
   -- Entity_Subscriber
   --
   -- Interface to be implemented by subscribers of entities.
   --
   type Entity_Subscriber is limited interface and Internal.Consumer_Base;

   -- Called when a new entity is available.
   --
   -- Parameters: Entity_Proxy - Proxy object containing new entity and meta information.
   --
   procedure On_New_Entity
     (Self : in out Entity_Subscriber;
      Entity_Proxy : in Safir.Dob.Entity_Proxies.Entity_Proxy) is abstract;

   -- Called when an entity is updated.
   --
   -- If Change Information is enabled for the subscription those entity members
   -- that are changed, compared to the previous received entity, will be marked as
   -- changed.
   --
   -- The entity owner handler id can be retreived by calling Get_Callback_Info.
   --
   -- Parameters: Entity_Proxy - Proxy object containing updated entity and meta information.
   --
   procedure On_Updated_Entity
     (Self : in out Entity_Subscriber;
      Entity_Proxy : in Safir.Dob.Entity_Proxies.Entity_Proxy) is abstract;

   -- Called when an entity is deleted.
   --
   -- Parameters: Entity_Proxy - Proxy object containing deleted entity information.
   --             Deleted_By_Owner - Flag indicating if the entity has been deleted
   --                                by the owner (true) or if it was removed by the DOB
   --                                because the owner is no longer present (false).
   --
   procedure On_Deleted_Entity
     (Self             : in out Entity_Subscriber;
      Entity_Proxy     : in Safir.Dob.Entity_Proxies.Entity_Proxy;
      Deleted_By_Owner : in Boolean) is abstract;
end Safir.Dob.Consumers;
