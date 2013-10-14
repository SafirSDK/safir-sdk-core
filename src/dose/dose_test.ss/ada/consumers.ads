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
with Safir.Dob.Consumers;
with Safir.Dob.Secondary_Connections;
--  with Safir.Dob.Message;
--  with Safir.Dob.Service;
--  with Safir.Dob.Entity;
--  with Safir.Dob.Response;
with Safir.Dob.Response_Senders;
with Safir.Dob.Typesystem;
with Safir.Dob.Typesystem.Handler_Id;
with Safir.Dob.Instance_Id_Policy;
--with Safir.Dob.Registration_Status;
with Safir.Dob.Defs;
with Safir.Dob.Message_Proxies;
with Safir.Dob.Entity_Proxies;
with Safir.Dob.Entity_Request_Proxies;
with Safir.Dob.Injected_Entity_Proxies;
with Safir.Dob.Response_Proxies;
with Dose_Test.Action;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;
with Safir.Dob.Callback_Id;
with Safir.Dob.Service_Request_Proxies;
with Safir.Application.Backdoors;
with Safir.Application.Backdoor_Keepers;
with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;

package Consumers is
   type Consumer is limited new
     Safir.Dob.Consumers.Message_Subscriber and
     Safir.Dob.Consumers.Message_Sender and
     Safir.Dob.Consumers.Registration_Subscriber and
     Safir.Dob.Consumers.Entity_Subscriber and
     Safir.Dob.Consumers.Entity_Handler and
     Safir.Dob.Consumers.Entity_Handler_Injection and
     Safir.Dob.Consumers.Entity_Handler_Pending and
     Safir.Dob.Consumers.Service_Handler and
     Safir.Dob.Consumers.Service_Handler_Pending and
     Safir.Dob.Consumers.Requestor and
     Safir.Application.Backdoors.Backdoor
   with private;

   type Consumer_Access is access Consumer;

   function Create (Instance        : in Natural;
                    Connection_Name : in Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
                    Instance_String : in Ada.Strings.Wide_Unbounded.Unbounded_Wide_String)
                    return not null Consumer_Access;

   procedure Destroy (Cons : in out Consumer_Access);

   procedure Execute_Action
     (Self   : in out Consumer;
      Action : in Dose_Test.Action.Smart_Pointer);

   procedure Add_Callback_Action
     (Self   : in out Consumer;
      Action : in Dose_Test.Action.Smart_Pointer);

   procedure Execute_Callback_Actions
     (Self     : in out Consumer;
      Callback : in Safir.Dob.Callback_Id.Enumeration);

private

   overriding
   procedure On_Message
     (Self    : in out Consumer;
      Message_Proxy : in Safir.Dob.Message_Proxies.Message_Proxy);

   overriding
   procedure On_Not_Message_Overflow
     (Self : in out Consumer);

   overriding
   procedure On_Registered
     (Self       : in out Consumer;
      Type_Id    : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type);

   overriding
   procedure On_Unregistered
     (Self       : in out Consumer;
      Type_Id    : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type);


   overriding
   procedure On_Service_Request
     (Self                  : in out Consumer;
      Service_Request_Proxy : in Safir.Dob.Service_Request_Proxies.Service_Request_Proxy;
      Response_Sender       : in Safir.Dob.Response_Senders.Response_Sender);

   overriding
   procedure On_Revoked_Registration
     (Self       : in out Consumer;
      Type_Id    : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type);


   overriding
   procedure On_Completed_Registration
     (Self       : in out Consumer;
      Type_Id    : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type);

   overriding
   procedure On_New_Entity
     (Self                     : in out Consumer;
      Entity_Proxy             : in Safir.Dob.Entity_Proxies.Entity_Proxy);

   overriding
   procedure On_Updated_Entity
     (Self                     : in out Consumer;
      Entity_Proxy             : in Safir.Dob.Entity_Proxies.Entity_Proxy);

   overriding
   procedure On_Deleted_Entity
     (Self                     : in out Consumer;
      Entity_Proxy             : in Safir.Dob.Entity_Proxies.Entity_Proxy;
      Deleted_By_Owner         : in Boolean);

   overriding
   procedure On_Create_Request
     (Self                 : in out Consumer;
      Entity_Request_Proxy : in Safir.Dob.Entity_Request_Proxies.Entity_Request_Proxy;
      Response_Sender      : in Safir.Dob.Response_Senders.Response_Sender);

   overriding
   procedure On_Update_Request
     (Self            : in out Consumer;
      Entity_Request_Proxy : in Safir.Dob.Entity_Request_Proxies.Entity_Request_Proxy;
      Response_Sender      : in Safir.Dob.Response_Senders.Response_Sender);

   overriding
   procedure On_Delete_Request
     (Self            : in out Consumer;
      Entity_Request_Proxy : in Safir.Dob.Entity_Request_Proxies.Entity_Request_Proxy;
      Response_Sender      : in Safir.Dob.Response_Senders.Response_Sender);

   overriding
   procedure On_Injected_New_Entity
     (Self            : in out Consumer;
      Injected_Entity_Proxy : in Safir.Dob.Injected_Entity_Proxies.Injected_Entity_Proxy);

   overriding
   procedure On_Injected_Updated_Entity
     (Self            : in out Consumer;
      Injected_Entity_Proxy : in Safir.Dob.Injected_Entity_Proxies.Injected_Entity_Proxy);

   overriding
   procedure On_Injected_Deleted_Entity
     (Self            : in out Consumer;
      Injected_Entity_Proxy : in Safir.Dob.Injected_Entity_Proxies.Injected_Entity_Proxy);

   overriding
   procedure On_Initial_Injections_Done
     (Self       : in out Consumer;
      Type_Id    : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type);

   overriding
   procedure On_Response
     (Self            : in out Consumer;
      Response_Proxy : in Safir.Dob.Response_Proxies.Response_Proxy);

   overriding
   procedure On_Not_Request_Overflow
     (Self : in out Consumer);

   overriding
   procedure Handle_Command (Self           : in Consumer;
                             Command_Tokens : in Safir.Application.Backdoors.Strings.Vector);

   overriding
   function Get_Help_Text (Self : in Consumer)
                             return Wide_String;

   function "=" (Left, Right : in Dose_Test.Action.Smart_Pointer) return Boolean;

   package Action_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Dose_Test.Action.Smart_Pointer);

   type Callback_Actions_Table is array (Safir.Dob.Callback_Id.Enumeration) of
     Action_Vectors.Vector;

   type Policy_Key_T is record
      Type_Id : Safir.Dob.Typesystem.Type_Id;
      Handler_Id : Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;
   end record;

   type Policy_Value_T is record
      Instance_Id_Policy : Safir.Dob.Instance_Id_Policy.Enumeration;
      Instance           : Safir.Dob.Typesystem.Int_64;
   end record;

   function "<" (Left, Right : Policy_Key_T) return Boolean;

   package Policy_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type => Policy_Key_T,
      Element_Type => Policy_Value_T,
      "<" => "<");

   type Timestamp_Requestor_T is new Safir.Dob.Consumers.Requestor with null record;
   overriding
   procedure On_Response
     (Self            : in out Timestamp_Requestor_T;
      Response_Proxy : in Safir.Dob.Response_Proxies.Response_Proxy);
   overriding
   procedure On_Not_Request_Overflow
     (Self : in out Timestamp_Requestor_T);


   type Consumer is limited new
     Safir.Dob.Consumers.Message_Subscriber and
     Safir.Dob.Consumers.Message_Sender and
     Safir.Dob.Consumers.Registration_Subscriber and
     Safir.Dob.Consumers.Entity_Subscriber and
     Safir.Dob.Consumers.Entity_Handler and
     Safir.Dob.Consumers.Entity_Handler_Injection and
     Safir.Dob.Consumers.Entity_Handler_Pending and
     Safir.Dob.Consumers.Service_Handler and
     Safir.Dob.Consumers.Service_Handler_Pending and
     Safir.Dob.Consumers.Requestor and
     Safir.Application.Backdoors.Backdoor
   with record
      Connection                : Safir.Dob.Secondary_Connections.Secondary_Connection;
      Backdoor_Keeper           : Safir.Application.Backdoor_Keepers.Backdoor_Keeper;
      Consumer_Number           : Integer;
      Connection_Name           : Unbounded_Wide_String;
      Connection_Instance       : Unbounded_Wide_String;
      Response_Sender           : Safir.Dob.Response_Senders.Response_Sender;
      Response_Sender_Discarded : Boolean;
      Latest_Request_Id         : Safir.Dob.Defs.Request_Id;
      Callback_Actions          : Callback_Actions_Table;
      Instance_Id_Policy_Map    : Policy_Maps.Map;
      Timestamp_Requestor       : aliased Timestamp_Requestor_T;
   end record;


end Consumers;
