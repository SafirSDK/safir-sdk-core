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
with Safir.Dob.Typesystem;
with Safir.Dob.Typesystem.Handler_Id;
with Safir.Dob.Entity_Request_Proxies;
with Safir.Dob.Service_Request_Proxies;
with Safir.Dob.Injected_Entity_Proxies;
with Safir.Dob.Response_Senders;

package Safir.Dob.Consumer_Bases is

   package Internal is

      -- *************
      -- Consumer_Base
      --
      -- Interface used when composing more elaborated interfaces.
      --
      type Consumer_Base is limited interface;

   end Internal;

   ----------------------------
   -- Revoked_Registration_Base
   --
   -- Interface used when composing more elaborated interfaces.
   --
   type Revoked_Registration_Base is limited interface and Internal.Consumer_Base;

   -- Indicates that the handler is no longer registered for the given type.
   --
   -- Parameters: Type_Id - Type Id of the entity or service.
   --             Handler_Id - Id of the revoked handler.
   --
   procedure On_Revoked_Registration
     (Self       : in out Revoked_Registration_Base;
      Type_Id    : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type) is abstract;

   -- ***************************
   -- Completed_Registration_Base
   --
   -- Interface used when composing more elaborated interfaces.
   --
   type Completed_Registration_Base is limited interface and Revoked_Registration_Base;

   -- Indicates that a pending registration has completed and the handler is now registered.
   --
   -- Parameters: Type_Id - Type Id of the entity or service.
   --             Handler_Id - Id of the registered handler.
   --
   procedure On_Completed_Registration
     (Self       : in out Completed_Registration_Base;
      Type_Id    : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type) is abstract;

   -- *******************
   -- Entity_Request_Base
   --
   -- Interface to receive entity requests.
   --
   type Entity_Request_Base is limited interface and Internal.Consumer_Base;

   -- Called when someone requests an entity to be created.
   --
   -- If the handler is registered as "Handler_Decides_Instance_Id" the request does not
   -- contain an instance id, and the handler must decide which instance to use and
   -- must then send this back to the requestor using an Entity_Id_Response.
   --
   -- If the handler is registered as "Requestor_Decides_Instance_Id" the request contains
   -- an instance id, which the handler *must* use if it is going to accept the request.
   -- If the instance cannot be used an error response must be sent.
   -- The handler must not send Entity_Id_Response on successful create requests when
   -- it is registered as "Requestor_Decides_Instance_Id".
   --
   -- The receiver of the callback must send a response using the Response_Sender.
   -- It is possible to store the Response_Sender and send the response later after this
   -- method has returned.
   --
   -- Parameters: Entity_Request_Proxy - Proxy object containing request and meta information.
   --             Response_Sender - Used to send the response for the received request.
   --
   procedure On_Create_Request
     (Self                 : in out Entity_Request_Base;
      Entity_Request_Proxy : in Safir.Dob.Entity_Request_Proxies.Entity_Request_Proxy;
      Response_Sender      : in Safir.Dob.Response_Senders.Response_Sender) is abstract;

   -- Called when someone requests an entity to be updated.
   --
   -- The receiver of the callback must send a response using the Response_Sender.
   -- It is possible to store the Response_Sender and send the response later after this
   -- method has returned.
   --
   -- Parameters: Entity_Request_Proxy - Proxy object containing request and meta information.
   --             Response_Sender - Used to send the response for the received request.
   --
   procedure On_Update_Request
     (Self                 : in out Entity_Request_Base;
      Entity_Request_Proxy : in Safir.Dob.Entity_Request_Proxies.Entity_Request_Proxy;
      Response_Sender      : in Safir.Dob.Response_Senders.Response_Sender) is abstract;

   -- Called when someone requests an entity to be deleted.
   --
   -- The receiver of the callback must send a response using the Response_Sender.
   -- It is possible to store the Response_Sender and send the response later after this
   -- method has returned.
   --
   -- Parameters: Entity_Request_Proxy - Proxy object containing request and meta information.
   --             Response_Sender - Used to send the response for the received request.
   --
   procedure On_Delete_Request
     (Self                 : in out Entity_Request_Base;
      Entity_Request_Proxy : in Safir.Dob.Entity_Request_Proxies.Entity_Request_Proxy;
      Response_Sender      : in Safir.Dob.Response_Senders.Response_Sender) is abstract;

   -- *********************
   -- Entity_Injection_Base
   --
   -- Entity_Injection_Base contains callback methods that can be overridden by an entity
   -- handler that registers a handler for an entity type that can potentially be injected
   -- outside the control of the handler itself.
   --
   -- Examples of when an entity is injected outside the control of a handler includes
   -- persistent data at startup time and entities received from other system installations
   -- (multi-owned entities).
   --
   -- The handler can reject the injection by invoking Delete for the received entity.
   --
   -- In case the handler has no need to be informed when an entity is about to be injected
   -- it can rely on the default implementation which just accepts the injected entity.
   --
   -- An example of a situation where a handler needs to act on injected data is when the entity
   -- has members that are references (Entity_Ids) to other entity instances, and the lifetime
   -- of the pointed-to instance is completly connected to the lifetime of the referer.
   -- If an entity has been injected it is the responsibility of the local handler to
   -- traverse the entity instance graph, find changed references, and delete the
   -- unreferenced entity instances. To support this task there are methods in the Injected_Entity_Proxy
   -- to read the previous entity state and find invalid references.
   --
   -- Another example is when an injection relates only to some of the members (might be the case
   -- when updates are received from external systems) and the handler wants to maintain some sort
   -- of consistency between different members. In this case the handler can call
   -- Connection_Aspect_Postpone.Incomplete_Injection_State to wait for more updates before the new state
   -- is set.
   --
   type Entity_Injection_Base is limited interface and Entity_Request_Base;

   -- Called when a new entity is about to be injected in the system.
   --
   -- Parameters: Injected_Entity_Proxy - Proxy object containing entity and meta information
   --                                     about the new entity that is about to be injected.
   --
   procedure On_Injected_New_Entity
     (Self                  : in out Entity_Injection_Base;
      Injected_Entity_Proxy : in Safir.Dob.Injected_Entity_Proxies.Injected_Entity_Proxy) is null;

   -- Called when an updated entity is about to be injected in the system.
   --
   -- Parameters: Injected_Entity_Proxy - Proxy object containing entity and meta information
   --                                     about the updated entity that is about to be injected.
   --
   procedure On_Injected_Updated_Entity
     (Self                  : in out Entity_Injection_Base;
      Injected_Entity_Proxy : in Safir.Dob.Injected_Entity_Proxies.Injected_Entity_Proxy) is null;

   -- Called when an entity delete is about to be injected in the system.
   --
   -- Parameters: Injected_Entity_Proxy - Proxy object containing information
   --                                     about the entity that is about to be deleted.
   --
   procedure On_Injected_Deleted_Entity
     (Self                  : in out Entity_Injection_Base;
      Injected_Entity_Proxy : in Safir.Dob.Injected_Entity_Proxies.Injected_Entity_Proxy) is null;

   -- Indicates that all initial injection data has been transfered to the handler.
   --
   -- In connection to the completion of an entity registration, the Dob will transfer
   -- initial injection data to the handler. This method indicates that all such
   -- data has been transfered, and thus, enables an effective handling of
   -- the initial injection data by the handler.
   --
   -- This method is guaranteed to be called once for each invocation of the following methods:
   -- (This is true even when there is no initial injection data at all)
   -- * Connection_Base.Register_Entity_Handler_Injection
   -- * Connection_Base.Register_Entity_Handler_Pending
   --
   -- Parameters: Type_Id - Type id in corresponding registration.
   --             Handler_Id - Handler id in corresponding registration.
   --
   procedure On_Initial_Injections_Done
     (Self                  : in out Entity_Injection_Base;
      Type_Id               : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id            : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type) is null;

   -- *******************
   -- Service_Request_Base
   --
   -- Interface to receive service requests.
   --
   type Service_Request_Base is limited interface and Internal.Consumer_Base;

   -- Called when a service request is received.
   --
   -- The receiver of the callback must send a response using the Response_Sender.
   -- It is possible to store the Response_Sender and send the response later after this
   -- method has returned.
   --
   -- Parameters: Service_Request_Proxy - Proxy object containing request and meta information.
   --             Response_Sender - Used to send the response for the received request.
   --
   procedure On_Service_Request
     (Self                  : in out Service_Request_Base;
      Service_Request_Proxy : in Safir.Dob.Service_Request_Proxies.Service_Request_Proxy;
      Response_Sender       : in Safir.Dob.Response_Senders.Response_Sender) is abstract;

end Safir.Dob.Consumer_Bases;
