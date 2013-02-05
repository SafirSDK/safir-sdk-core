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
with Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Safir.Dob.Consumer_Bases;
with Safir.Dob.Typesystem.Internal_Operations;
with Safir.Dob.Interf;
with Safir.Dob.Entity_Proxy_Impls;
with Safir.Dob.Entity_Proxy_Impl_Pointers;
--  with Safir.Dob.Typesystem.DeprecatedOperations;
--  with Safir.Dob.Typesystem.Object;
--  with Safir.Dob.Typesystem.Utilities;
--  with Safir.Dob.Typesystem.Operations;
with Safir.Dob.Typesystem.Library_Exceptions;
--  with Safir.Dob.RegistrationStatus;
--  with Safir.Dob.ResponseSender;
--  with Safir.Dob.NotFoundException;
with Interfaces.C;
with Interfaces.C.Strings;
--
--  with Safir.Dob.Response;

pragma Warnings ("D");  -- turn off warnings for implicit dereference

package body Safir.Dob.Connection_Bases is

   package C renames Interfaces.C;
--     use type Safir.Dob.Typesystem.Int32;
--     use Safir.Dob.Internal;
--
   procedure Throw (E : Ada.Exceptions.Exception_Id; Msg : String := "")
                    renames  Ada.Exceptions.Raise_Exception;

   function C_Malloc (Size : C.size_t) return Safir.Dob.Typesystem.Char_Star;
   pragma Import (C, C_Malloc, "malloc");

   procedure C_Free (X : in out Safir.Dob.Typesystem.Char_Star) is
      procedure Free (X : Safir.Dob.Typesystem.Char_Star);
      pragma Import (C, Free, "free");
   begin
      Free (X);
      X := null;
   end C_Free;


   procedure Register_Entity_Handler
     (Self               : in Connection_Base;
      Type_Id            : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id         : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;
      Instance_Id_Policy : in Safir.Dob.Instance_Id_Policy.Enumeration;
      Entity_Handler     : access Safir.Dob.Consumers.Entity_Handler'Class) is

      use Safir.Dob.Typesystem.Handler_Id;

      Success : C.char;
      Utf_8_Str_Ptr : C.Strings.chars_ptr;
   begin
      Utf_8_Str_Ptr :=
        C.Strings.New_String (To_String (Utf_8_String (Handler_Id)));

      Safir.Dob.Interf.Register_Entity_Handler
        (Get_Controller_Id (Connection_Base'Class (Self)),
         Type_Id,
         Get_Raw_Value (Handler_Id),
         Utf_8_Str_Ptr,
         Safir.Dob.Instance_Id_Policy.Enumeration'Pos (Instance_Id_Policy),
         C.char'Val (Boolean'Pos (True)),  -- override registration
         C.char'Val (Boolean'Pos (False)), -- not an injectionHandler
         Safir.Dob.Interf.Language_Ada,
         Entity_Handler,
         Success);
      C.Strings.Free (Utf_8_Str_Ptr);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
   end Register_Entity_Handler;

   procedure Register_Entity_Handler_Injection
     (Self                     : in Connection_Base;
      Type_Id                  : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id               : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;
      Instance_Id_Policy       : in Safir.Dob.Instance_Id_Policy.Enumeration;
      Entity_Handler_Injection : access Safir.Dob.Consumers.Entity_Handler_Injection'Class) is

      use Safir.Dob.Typesystem.Handler_Id;

      Success : C.char;
      Utf_8_Str_Ptr : C.Strings.chars_ptr;
   begin
      Utf_8_Str_Ptr :=
        C.Strings.New_String (To_String (Utf_8_String (Handler_Id)));

      Safir.Dob.Interf.Register_Entity_Handler
        (Get_Controller_Id (Connection_Base'Class (Self)),
         Type_Id,
         Get_Raw_Value (Handler_Id),
         Utf_8_Str_Ptr,
         Safir.Dob.Instance_Id_Policy.Enumeration'Pos (Instance_Id_Policy),
         C.char'Val (Boolean'Pos (True)),  -- override registration
         C.char'Val (Boolean'Pos (True)), -- An injectionHandler
         Safir.Dob.Interf.Language_Ada,
         Entity_Handler_Injection,
         Success);
      C.Strings.Free (Utf_8_Str_Ptr);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
   end Register_Entity_Handler_Injection;

   procedure Register_Entity_Handler_Pending
     (Self                     : in Connection_Base;
      Type_Id                  : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id               : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;
      Instance_Id_Policy       : in Safir.Dob.Instance_Id_Policy.Enumeration;
      Entity_Handler_Pending   : access Safir.Dob.Consumers.Entity_Handler_Pending'Class) is

      use Safir.Dob.Typesystem.Handler_Id;

      Success : C.char;
      Utf_8_Str_Ptr : C.Strings.chars_ptr;
   begin
      Utf_8_Str_Ptr :=
        C.Strings.New_String (To_String (Utf_8_String (Handler_Id)));

      Safir.Dob.Interf.Register_Entity_Handler
        (Get_Controller_Id (Connection_Base'Class (Self)),
         Type_Id,
         Get_Raw_Value (Handler_Id),
         Utf_8_Str_Ptr,
         Safir.Dob.Instance_Id_Policy.Enumeration'Pos (Instance_Id_Policy),
         C.char'Val (Boolean'Pos (False)),  -- Pending registration
         C.char'Val (Boolean'Pos (True)), -- An injectionHandler
         Safir.Dob.Interf.Language_Ada,
         Entity_Handler_Pending,
         Success);
      C.Strings.Free (Utf_8_Str_Ptr);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
   end Register_Entity_Handler_Pending;

   procedure Register_Service_Handler
     (Self               : in Connection_Base;
      Type_Id            : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id         : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;
      Service_Handler    : access Safir.Dob.Consumers.Service_Handler'Class) is

      use Safir.Dob.Typesystem.Handler_Id;

      type Service_Request_Base_Access is access all
        Safir.Dob.Consumer_Bases.Service_Request_Base;

      Success : C.char;
      Utf_8_Str_Ptr : C.Strings.chars_ptr;
   begin
      Utf_8_Str_Ptr :=
        C.Strings.New_String (To_String (Utf_8_String (Handler_Id)));

      Safir.Dob.Interf.Register_Service_Handler
        (Get_Controller_Id (Connection_Base'Class (Self)),
         Type_Id,
         Get_Raw_Value (Handler_Id),
         Utf_8_Str_Ptr,
         C.char'Val (Boolean'Pos (True)),  -- override registration
         Safir.Dob.Interf.Language_Ada,
         Service_Request_Base_Access (Service_Handler),
         Success);
      C.Strings.Free (Utf_8_Str_Ptr);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
   end Register_Service_Handler;

   procedure Register_Service_Handler_Pending
     (Self                     : in Connection_Base;
      Type_Id                  : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id               : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;
      Service_Handler_Pending  : access Safir.Dob.Consumers.Service_Handler_Pending'Class) is

      use Safir.Dob.Typesystem.Handler_Id;

      Success : C.char;
      Utf_8_Str_Ptr : C.Strings.chars_ptr;
   begin
      Utf_8_Str_Ptr :=
        C.Strings.New_String (To_String (Utf_8_String (Handler_Id)));

      Safir.Dob.Interf.Register_Service_Handler
        (Get_Controller_Id (Connection_Base'Class (Self)),
         Type_Id,
         Get_Raw_Value (Handler_Id),
         Utf_8_Str_Ptr,
         C.char'Val (Boolean'Pos (False)),  -- pending registration
         Safir.Dob.Interf.Language_Ada,
         Service_Handler_Pending,
         Success);
      C.Strings.Free (Utf_8_Str_Ptr);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
   end Register_Service_Handler_Pending;

   --------------------
   -- Unregistration --
   --------------------

   procedure Unregister_Handler
     (Self          : in Connection_Base;
      Type_Id       : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id    : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type) is

      use Safir.Dob.Typesystem.Handler_Id;

      Success : C.char;
      Utf_8_Str_Ptr : C.Strings.chars_ptr;
   begin
      Utf_8_Str_Ptr :=
        C.Strings.New_String (To_String (Utf_8_String (Handler_Id)));

      Safir.Dob.Interf.Unregister_Handler
        (Get_Controller_Id (Connection_Base'Class (Self)),
         Type_Id,
         Get_Raw_Value (Handler_Id),
         Utf_8_Str_Ptr,
         Success);
      C.Strings.Free (Utf_8_Str_Ptr);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;

   end Unregister_Handler;

   ---------------------------
   -- Message subscriptions --
   ---------------------------

   procedure Subscribe_Message
     (Self                : in     Connection_Base;
      Type_Id             : in     Safir.Dob.Typesystem.Type_Id;
      Channel_Id          : in     Safir.Dob.Typesystem.Channel_Id.Channel_Id_Type;
      Message_Subscriber  : access Safir.Dob.Consumers.Message_Subscriber'Class) is
   begin
      Subscribe_Message (Self, Type_Id, Channel_Id, True, Message_Subscriber);
   end Subscribe_Message;

   procedure Subscribe_Message
     (Self                : in     Connection_Base;
      Type_Id             : in     Safir.Dob.Typesystem.Type_Id;
      Channel_Id          : in     Safir.Dob.Typesystem.Channel_Id.Channel_Id_Type;
      Include_Subclasses  : in     Boolean;
      Message_Subscriber  : access Safir.Dob.Consumers.Message_Subscriber'Class) is

      use Safir.Dob.Typesystem.Channel_Id;

      Success : C.char;
      Utf_8_Str_Ptr : C.Strings.chars_ptr;
   begin
      Utf_8_Str_Ptr :=
        C.Strings.New_String (To_String (Utf_8_String (Channel_Id)));

      Safir.Dob.Interf.Subscribe_Message
        (Get_Controller_Id (Connection_Base'Class (Self)),
         Type_Id,
         Get_Raw_Value (Channel_Id),
         Utf_8_Str_Ptr,
         C.char'Val (Boolean'Pos (Include_Subclasses)),
         Safir.Dob.Interf.Language_Ada,
         Message_Subscriber,
         Success);
      C.Strings.Free (Utf_8_Str_Ptr);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;

   end Subscribe_Message;

   procedure Unsubscribe_Message
     (Self                : in     Connection_Base;
      Type_Id             : in     Safir.Dob.Typesystem.Type_Id;
      Channel_Id          : in     Safir.Dob.Typesystem.Channel_Id.Channel_Id_Type;
      Message_Subscriber  : access Safir.Dob.Consumers.Message_Subscriber'Class) is
   begin
      Unsubscribe_Message (Self, Type_Id, Channel_Id, True, Message_Subscriber);
   end Unsubscribe_Message;

   procedure Unsubscribe_Message
     (Self                : in     Connection_Base;
      Type_Id             : in     Safir.Dob.Typesystem.Type_Id;
      Channel_Id          : in     Safir.Dob.Typesystem.Channel_Id.Channel_Id_Type;
      Include_Subclasses  : in     Boolean;
      Message_Subscriber  : access Safir.Dob.Consumers.Message_Subscriber'Class) is

      use Safir.Dob.Typesystem.Channel_Id;

      Success : C.char;
      Utf_8_Str_Ptr : C.Strings.chars_ptr;
   begin
      Utf_8_Str_Ptr :=
        C.Strings.New_String (To_String (Utf_8_String (Channel_Id)));

      Safir.Dob.Interf.Unsubscribe_Message
        (Get_Controller_Id (Connection_Base'Class (Self)),
         Type_Id,
         Get_Raw_Value (Channel_Id),
         Utf_8_Str_Ptr,
         C.char'Val (Boolean'Pos (Include_Subclasses)),
         Safir.Dob.Interf.Language_Ada,
         Message_Subscriber,
         Success);
      C.Strings.Free (Utf_8_Str_Ptr);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;

   end Unsubscribe_Message;

   -----------------------
   -- Entity subscriptions
   -----------------------

   procedure Subscribe_Entity
     (Self                : in Connection_Base;
      Type_Id             : in Safir.Dob.Typesystem.Type_Id;
      Entity_Subscriber   : access Safir.Dob.Consumers.Entity_Subscriber'Class) is
   begin
      Subscribe_Entity (Self                 => Self,
                        Type_Id              => Type_Id,
                        Include_Updates      => True,
                        Include_Subclasses   => True,
                        Restart_Subscription => True,
                        Entity_Subscriber    => Entity_Subscriber);
   end Subscribe_Entity;

   procedure Subscribe_Entity
     (Self                 : in Connection_Base;
      Type_Id              : in Safir.Dob.Typesystem.Type_Id;
      Include_Updates      : in Boolean;
      Include_Subclasses   : in Boolean;
      Restart_Subscription : in Boolean;
      Entity_Subscriber    : access Safir.Dob.Consumers.Entity_Subscriber'Class) is

      Success : C.char;
      Empty_String : C.Strings.chars_ptr := C.Strings.New_String ("");
   begin
      Safir.Dob.Interf.Subscribe_Entity
        (Get_Controller_Id (Connection_Base'Class (Self)),
         Type_Id,
         0,
         Empty_String,
         C.char'Val (Boolean'Pos (True)),  -- all instances
         C.char'Val (Boolean'Pos (Include_Updates)),
         C.char'Val (Boolean'Pos (Include_Subclasses)),
         C.char'Val (Boolean'Pos (Restart_Subscription)),
         Safir.Dob.Interf.Language_Ada,
         Entity_Subscriber,
         Success);
      C.Strings.Free (Empty_String);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
   end Subscribe_Entity;

   procedure Subscribe_Entity
     (Self                 : in Connection_Base;
      Entity_Id            : in Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type;
      Include_Updates      : in Boolean;
      Restart_Subscription : in Boolean;
      Entity_Subscriber    : access Safir.Dob.Consumers.Entity_Subscriber'Class) is

      use Safir.Dob.Typesystem.Instance_Id;

      Success : C.char;
      Utf_8_Str_Ptr : C.Strings.chars_ptr;
      Instance_Id : constant Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type :=
                      Safir.Dob.Typesystem.Entity_Id.Get_Instance_Id (Entity_Id);
   begin
      Utf_8_Str_Ptr :=
        C.Strings.New_String (To_String (Utf_8_String (Instance_Id)));

      Safir.Dob.Interf.Subscribe_Entity
        (Get_Controller_Id (Connection_Base'Class (Self)),
         Safir.Dob.Typesystem.Entity_Id.Get_Type_Id (Entity_Id),
         Safir.Dob.Typesystem.Instance_Id.Get_Raw_Value (Instance_Id),
         Utf_8_Str_Ptr,
         C.char'Val (Boolean'Pos (False)),  -- all instances
         C.char'Val (Boolean'Pos (Include_Updates)),
         C.char'Val (Boolean'Pos (False)),  -- include subsclasses
         C.char'Val (Boolean'Pos (Restart_Subscription)),
         Safir.Dob.Interf.Language_Ada,
         Entity_Subscriber,
         Success);
      C.Strings.Free (Utf_8_Str_Ptr);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
   end Subscribe_Entity;

   procedure Unsubscribe_Entity
     (Self                 : in Connection_Base;
      Type_Id              : in Safir.Dob.Typesystem.Type_Id;
      Entity_Subscriber    : access Safir.Dob.Consumers.Entity_Subscriber'Class) is
   begin
      Unsubscribe_Entity (Self => Self,
                          Type_Id => Type_Id,
                          Include_Subclasses => True,
                          Entity_Subscriber => Entity_Subscriber);
   end Unsubscribe_Entity;

   procedure Unsubscribe_Entity
     (Self                 : in Connection_Base;
      Type_Id              : in Safir.Dob.Typesystem.Type_Id;
      Include_Subclasses   : in Boolean;
      Entity_Subscriber    : access Safir.Dob.Consumers.Entity_Subscriber'Class) is

      Success : C.char;
      Empty_String : C.Strings.chars_ptr := C.Strings.New_String ("");
   begin
      Safir.Dob.Interf.Unsubscribe_Entity
        (Get_Controller_Id (Connection_Base'Class (Self)),
         Type_Id,
         0,
         Empty_String,
         C.char'Val (Boolean'Pos (True)),  -- all instances
         C.char'Val (Boolean'Pos (Include_Subclasses)),
         Safir.Dob.Interf.Language_Ada,
         Entity_Subscriber,
         Success);
      C.Strings.Free (Empty_String);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
   end Unsubscribe_Entity;

   procedure Unsubscribe_Entity
     (Self                 : in Connection_Base;
      Entity_Id            : in Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type;
      Entity_Subscriber    : access Safir.Dob.Consumers.Entity_Subscriber'Class) is

      use Safir.Dob.Typesystem.Instance_Id;

      Success : C.char;
      Utf_8_Str_Ptr : C.Strings.chars_ptr;
      Instance_Id : constant Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type :=
                      Safir.Dob.Typesystem.Entity_Id.Get_Instance_Id (Entity_Id);
   begin
      Utf_8_Str_Ptr :=
        C.Strings.New_String (To_String (Utf_8_String (Instance_Id)));

      Safir.Dob.Interf.Unsubscribe_Entity
        (Get_Controller_Id (Connection_Base'Class (Self)),
         Safir.Dob.Typesystem.Entity_Id.Get_Type_Id (Entity_Id),
         Safir.Dob.Typesystem.Instance_Id.Get_Raw_Value (Instance_Id),
         Utf_8_Str_Ptr,
         C.char'Val (Boolean'Pos (False)),  -- all instances
         C.char'Val (Boolean'Pos (False)),  -- include subsclasses
         Safir.Dob.Interf.Language_Ada,
         Entity_Subscriber,
         Success);
      C.Strings.Free (Utf_8_Str_Ptr);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
   end Unsubscribe_Entity;

   -------------------------
   -- Subscribe_Registration
   -------------------------

   procedure Subscribe_Registration
     (Self                     : in     Connection_Base;
      Type_Id                  : in     Safir.Dob.Typesystem.Type_Id;
      Handler_Id               : in     Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;
      Include_Subclasses       : in     Boolean;
      Restart_Subscription     : in     Boolean;
      Registration_Subscriber  : access Safir.Dob.Consumers.Registration_Subscriber'Class) is

      use Safir.Dob.Typesystem.Handler_Id;

      Success : C.char;
      Utf_8_Str_Ptr : C.Strings.chars_ptr;
   begin
      Utf_8_Str_Ptr :=
        C.Strings.New_String (To_String (Utf_8_String (Handler_Id)));

      Safir.Dob.Interf.Subscribe_Registration
        (Get_Controller_Id (Connection_Base'Class (Self)),
         Type_Id,
         Get_Raw_Value (Handler_Id),
         Utf_8_Str_Ptr,
         C.char'Val (Boolean'Pos (Include_Subclasses)),
         C.char'Val (Boolean'Pos (Restart_Subscription)),
         Safir.Dob.Interf.Language_Ada,
         Registration_Subscriber,
         Success);
      C.Strings.Free (Utf_8_Str_Ptr);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
   end Subscribe_Registration;

   ------------------------------
   -- Unsubscribe_Registration --
   ------------------------------

   procedure Unsubscribe_Registration
     (Self                     : in     Connection_Base;
      Type_Id                  : in     Safir.Dob.Typesystem.Type_Id;
      Handler_Id               : in     Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;
      Include_Subclasses       : in     Boolean;
      Registration_Subscriber  : access Safir.Dob.Consumers.Registration_Subscriber'Class) is

      use Safir.Dob.Typesystem.Handler_Id;

      Success : C.char;
      Utf_8_Str_Ptr : C.Strings.chars_ptr;
   begin
      Utf_8_Str_Ptr :=
        C.Strings.New_String (To_String (Utf_8_String (Handler_Id)));

      Safir.Dob.Interf.Unsubscribe_Registration
        (Get_Controller_Id (Connection_Base'Class (Self)),
         Type_Id,
         Get_Raw_Value (Handler_Id),
         Utf_8_Str_Ptr,
         C.char'Val (Boolean'Pos (Include_Subclasses)),
         Safir.Dob.Interf.Language_Ada,
         Registration_Subscriber,
         Success);
      C.Strings.Free (Utf_8_Str_Ptr);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;

   end Unsubscribe_Registration;

   -------------------
   -- Send messages --
   -------------------

   procedure Send
     (Self           : in Connection_Base;
      Message        : in Safir.Dob.Message.Smart_Pointer'Class;
      Channel_Id     : in Safir.Dob.Typesystem.Channel_Id.Channel_Id_Type;
      Message_Sender : access Safir.Dob.Consumers.Message_Sender'Class) is

      use type Safir.Dob.Message.Message_Class_Access;
      use Safir.Dob.Typesystem.Channel_Id;

      Success : C.char;
      Blob : Safir.Dob.Typesystem.Blob_T;
      Blob_Size : Safir.Dob.Typesystem.Int_32;
      Beginning_Of_Unused : Safir.Dob.Typesystem.Blob_T;
      Message_Ptr : constant Safir.Dob.Message.Message_Class_Access := Message.Ref;
      Utf_8_Str_Ptr : C.Strings.chars_ptr;
   begin
      if Message_Ptr = null then
         Throw (Safir.Dob.Typesystem.Software_Violation_Exception'Identity,
                "Attempt to serialize a message null pointer to binary!");
      end if;

      Blob_Size := Message_Ptr.Calculate_Blob_Size;

      Blob := C_Malloc (C.size_t (Blob_Size));

      Safir.Dob.Typesystem.Internal_Operations.Format_Blob (Blob,
                                                            Blob_Size,
                                                            Message_Ptr.Get_Type_Id,
                                                            Beginning_Of_Unused);
      Message_Ptr.Write_To_Blob (Blob, Beginning_Of_Unused);


      Utf_8_Str_Ptr :=
        C.Strings.New_String (To_String (Utf_8_String (Channel_Id)));


      Safir.Dob.Interf.Send_Message (Get_Controller_Id (Connection_Base'Class (Self)),
                                     Blob,
                                     Get_Raw_Value (Channel_Id),
                                     Utf_8_Str_Ptr,
                                     Safir.Dob.Interf.Language_Ada,
                                     Message_Sender,
                                     Success);
      C_Free (Blob);
      C.Strings.Free (Utf_8_Str_Ptr);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
   end Send;

   --------------
   -- Requests --
   --------------

   function Create_Request
     (Self           : in Connection_Base;
      Request        : in Safir.Dob.Entity.Smart_Pointer'Class;
      Handler_Id     : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;
      Requestor      : access Safir.Dob.Consumers.Requestor'Class) return
     Safir.Dob.Defs.Request_Id is

      use type Safir.Dob.Entity.Entity_Class_Access;
      use Safir.Dob.Typesystem.Handler_Id;

      Success : C.char;
      Blob : Safir.Dob.Typesystem.Blob_T;
      Blob_Size : Safir.Dob.Typesystem.Int_32;
      Beginning_Of_Unused : Safir.Dob.Typesystem.Blob_T;
      Request_Ptr : constant Safir.Dob.Entity.Entity_Class_Access := Request.Ref;
      Instance_Utf_8_Str_Ptr : C.Strings.chars_ptr;
      Handler_Utf_8_Str_Ptr : C.Strings.chars_ptr;
      Request_Id : Safir.Dob.Defs.Request_Id;
   begin
      if Request_Ptr = null then
         Throw (Safir.Dob.Typesystem.Software_Violation_Exception'Identity,
                "Create_Request: Attempt to serialize an entity null pointer to binary!");
      end if;

      Blob_Size := Request_Ptr.Calculate_Blob_Size;

      Blob := C_Malloc (C.size_t (Blob_Size));

      Safir.Dob.Typesystem.Internal_Operations.Format_Blob (Blob,
                                                            Blob_Size,
                                                            Request_Ptr.Get_Type_Id,
                                                            Beginning_Of_Unused);
      Request_Ptr.Write_To_Blob (Blob, Beginning_Of_Unused);


      Instance_Utf_8_Str_Ptr := C.Strings.New_String ("");
      Handler_Utf_8_Str_Ptr :=
        C.Strings.New_String (To_String (Utf_8_String (Handler_Id)));

      Safir.Dob.Interf.Create_Request
        (Get_Controller_Id (Connection_Base'Class (Self)),
         Blob,
         C.char'Val (Boolean'Pos (False)),  -- has instance
         0,
         Instance_Utf_8_Str_Ptr,
         Get_Raw_Value (Handler_Id),
         Handler_Utf_8_Str_Ptr,
         Safir.Dob.Interf.Language_Ada,
         Requestor,
         Request_Id,
         Success);

      C_Free (Blob);
      C.Strings.Free (Instance_Utf_8_Str_Ptr);
      C.Strings.Free (Handler_Utf_8_Str_Ptr);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;

      return Request_Id;
   end Create_Request;

   function Create_Request
     (Self           : in Connection_Base;
      Request        : in Safir.Dob.Entity.Smart_Pointer'Class;
      Instance_Id    : in Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type;
      Handler_Id     : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;
      Requestor      : access Safir.Dob.Consumers.Requestor'Class) return
     Safir.Dob.Defs.Request_Id is

      use type Safir.Dob.Entity.Entity_Class_Access;
      use Safir.Dob.Typesystem.Instance_Id;
      use Safir.Dob.Typesystem.Handler_Id;

      Success : C.char;
      Blob : Safir.Dob.Typesystem.Blob_T;
      Blob_Size : Safir.Dob.Typesystem.Int_32;
      Beginning_Of_Unused : Safir.Dob.Typesystem.Blob_T;
      Request_Ptr : constant Safir.Dob.Entity.Entity_Class_Access := Request.Ref;
      Instance_Utf_8_Str_Ptr : C.Strings.chars_ptr;
      Handler_Utf_8_Str_Ptr : C.Strings.chars_ptr;
      Request_Id : Safir.Dob.Defs.Request_Id;
   begin
      if Request_Ptr = null then
         Throw (Safir.Dob.Typesystem.Software_Violation_Exception'Identity,
                "Create_Request: Attempt to serialize an entity null pointer to binary!");
      end if;

      Blob_Size := Request_Ptr.Calculate_Blob_Size;

      Blob := C_Malloc (C.size_t (Blob_Size));

      Safir.Dob.Typesystem.Internal_Operations.Format_Blob (Blob,
                                                            Blob_Size,
                                                            Request_Ptr.Get_Type_Id,
                                                            Beginning_Of_Unused);
      Request_Ptr.Write_To_Blob (Blob, Beginning_Of_Unused);


      Instance_Utf_8_Str_Ptr :=
        C.Strings.New_String (To_String (Utf_8_String (Instance_Id)));
      Handler_Utf_8_Str_Ptr :=
        C.Strings.New_String (To_String (Utf_8_String (Handler_Id)));

      Safir.Dob.Interf.Create_Request
        (Get_Controller_Id (Connection_Base'Class (Self)),
         Blob,
         C.char'Val (Boolean'Pos (True)),  -- has instance
         Get_Raw_Value (Instance_Id),
         Instance_Utf_8_Str_Ptr,
         Get_Raw_Value (Handler_Id),
         Handler_Utf_8_Str_Ptr,
         Safir.Dob.Interf.Language_Ada,
         Requestor,
         Request_Id,
         Success);

      C_Free (Blob);
      C.Strings.Free (Instance_Utf_8_Str_Ptr);
      C.Strings.Free (Handler_Utf_8_Str_Ptr);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;

      return Request_Id;
   end Create_Request;

   function Update_Request
     (Self           : in Connection_Base;
      Request        : in Safir.Dob.Entity.Smart_Pointer'Class;
      Instance_Id    : in Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type;
      Requestor      : access Safir.Dob.Consumers.Requestor'Class) return
     Safir.Dob.Defs.Request_Id is

      use type Safir.Dob.Entity.Entity_Class_Access;
      use Safir.Dob.Typesystem.Instance_Id;

      Success : C.char;
      Blob : Safir.Dob.Typesystem.Blob_T;
      Blob_Size : Safir.Dob.Typesystem.Int_32;
      Beginning_Of_Unused : Safir.Dob.Typesystem.Blob_T;
      Request_Ptr : constant Safir.Dob.Entity.Entity_Class_Access := Request.Ref;
      Instance_Utf_8_Str_Ptr : C.Strings.chars_ptr;
      Request_Id : Safir.Dob.Defs.Request_Id;
   begin
      if Request_Ptr = null then
         Throw (Safir.Dob.Typesystem.Software_Violation_Exception'Identity,
                "Update_Request: Attempt to serialize an entity null pointer to binary!");
      end if;

      Blob_Size := Request_Ptr.Calculate_Blob_Size;

      Blob := C_Malloc (C.size_t (Blob_Size));

      Safir.Dob.Typesystem.Internal_Operations.Format_Blob (Blob,
                                                            Blob_Size,
                                                            Request_Ptr.Get_Type_Id,
                                                            Beginning_Of_Unused);
      Request_Ptr.Write_To_Blob (Blob, Beginning_Of_Unused);


      Instance_Utf_8_Str_Ptr :=
        C.Strings.New_String (To_String (Utf_8_String (Instance_Id)));

      Safir.Dob.Interf.Update_Request
        (Get_Controller_Id (Connection_Base'Class (Self)),
         Blob,
         Get_Raw_Value (Instance_Id),
         Instance_Utf_8_Str_Ptr,
         Safir.Dob.Interf.Language_Ada,
         Requestor,
         Request_Id,
         Success);

      C_Free (Blob);
      C.Strings.Free (Instance_Utf_8_Str_Ptr);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;

      return Request_Id;
   end Update_Request;

   function Delete_Request
     (Self           : in Connection_Base;
      Entity_Id      : in Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type;
      Requestor      : access Safir.Dob.Consumers.Requestor'Class) return
     Safir.Dob.Defs.Request_Id is

      use type Safir.Dob.Entity.Entity_Class_Access;
      use Safir.Dob.Typesystem.Instance_Id;

      Success : C.char;
      Instance_Utf_8_Str_Ptr : C.Strings.chars_ptr;
      Request_Id : Safir.Dob.Defs.Request_Id;
      Instance_Id : constant Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type :=
                      Safir.Dob.Typesystem.Entity_Id.Get_Instance_Id (Entity_Id);
   begin
      Instance_Utf_8_Str_Ptr :=
        C.Strings.New_String (To_String (Utf_8_String (Instance_Id)));

      Safir.Dob.Interf.Delete_Request
        (Get_Controller_Id (Connection_Base'Class (Self)),
         Safir.Dob.Typesystem.Entity_Id.Get_Type_Id (Entity_Id),
         Get_Raw_Value (Instance_Id),
         Instance_Utf_8_Str_Ptr,
         Safir.Dob.Interf.Language_Ada,
         Requestor,
         Request_Id,
         Success);

      C.Strings.Free (Instance_Utf_8_Str_Ptr);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;

      return Request_Id;
   end Delete_Request;

   function Service_Request
     (Self           : in Connection_Base;
      Request        : in Safir.Dob.Service.Smart_Pointer'Class;
      Handler_Id    : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;
      Requestor      : access Safir.Dob.Consumers.Requestor'Class) return
     Safir.Dob.Defs.Request_Id is

      use type Safir.Dob.Service.Service_Class_Access;
      use Safir.Dob.Typesystem.Handler_Id;

      Success : C.char;
      Blob : Safir.Dob.Typesystem.Blob_T;
      Blob_Size : Safir.Dob.Typesystem.Int_32;
      Beginning_Of_Unused : Safir.Dob.Typesystem.Blob_T;
      Request_Ptr : constant Safir.Dob.Service.Service_Class_Access := Request.Ref;
      Handler_Utf_8_Str_Ptr : C.Strings.chars_ptr;
      Request_Id : Safir.Dob.Defs.Request_Id;
   begin
      if Request_Ptr = null then
         Throw (Safir.Dob.Typesystem.Software_Violation_Exception'Identity,
                "Service_Request: Attempt to serialize an entity null pointer to binary!");
      end if;

      Blob_Size := Request_Ptr.Calculate_Blob_Size;

      Blob := C_Malloc (C.size_t (Blob_Size));

      Safir.Dob.Typesystem.Internal_Operations.Format_Blob (Blob,
                                                            Blob_Size,
                                                            Request_Ptr.Get_Type_Id,
                                                            Beginning_Of_Unused);
      Request_Ptr.Write_To_Blob (Blob, Beginning_Of_Unused);


      Handler_Utf_8_Str_Ptr :=
        C.Strings.New_String (To_String (Utf_8_String (Handler_Id)));

      Safir.Dob.Interf.Service_Request
        (Get_Controller_Id (Connection_Base'Class (Self)),
         Blob,
         Get_Raw_Value (Handler_Id),
         Handler_Utf_8_Str_Ptr,
         Safir.Dob.Interf.Language_Ada,
         Requestor,
         Request_Id,
         Success);

      C_Free (Blob);
      C.Strings.Free (Handler_Utf_8_Str_Ptr);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;

      return Request_Id;
   end Service_Request;

   -------------------
   -- Entity owners --
   -------------------

   procedure Set_Changes
     (Self          : in Connection_Base;
      Entity        : in Safir.Dob.Entity.Smart_Pointer'Class;
      Instance_Id   : in Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type;
      Handler_Id    : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type) is

      Entity_Ptr : constant Safir.Dob.Entity.Entity_Class_Access := Entity.Ref;
      Entity_Id : constant Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type :=
                    Safir.Dob.Typesystem.Entity_Id.Create_Entity_Id
                      (Entity_Ptr.Get_Type_Id, Instance_Id);
      Entity_Proxy : Safir.Dob.Entity_Proxies.Entity_Proxy;
      Merged_Entity : Safir.Dob.Entity.Smart_Pointer;
   begin
      null;
      if Is_Created (Self, Entity_Id) then
         Entity_Proxy := Self.Read (Entity_Id);
         Merged_Entity := Safir.Dob.Entity.Smart_Pointer (Entity_Proxy.Get_Entity);
         Safir.Dob.Typesystem.Internal_Operations.Merge_Changes
           (Merged_Entity, Entity);

         Set (Self                  => Self,
              Entity                => Merged_Entity,
              Instance_Id           => Instance_Id,
              Handler_Id            => Handler_Id,
              Consider_Change_Flags => True);
      else
         Set (Self                  => Self,
              Entity                => Entity,
              Instance_Id           => Instance_Id,
              Handler_Id            => Handler_Id,
              Consider_Change_Flags => False);
      end if;

   end Set_Changes;

   procedure Set_All
     (Self          : in Connection_Base;
      Entity        : in Safir.Dob.Entity.Smart_Pointer'Class;
      Instance_Id   : in Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type;
      Handler_Id    : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type) is
   begin
      Set (Self   => Self,
           Entity => Entity,
           Instance_Id => Instance_Id,
           Handler_Id  => Handler_Id,
           Consider_Change_Flags => False);
   end Set_All;

   -- Private operation
   procedure Set
     (Self                  : in Connection_Base;
      Entity                : in Safir.Dob.Entity.Smart_Pointer'Class;
      Instance_Id           : in Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type;
      Handler_Id            : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;
      Consider_Change_Flags : in Boolean) is

      use type Safir.Dob.Entity.Entity_Class_Access;
      use Safir.Dob.Typesystem.Instance_Id;
      use Safir.Dob.Typesystem.Handler_Id;

      Success : C.char;
      Blob : Safir.Dob.Typesystem.Blob_T;
      Blob_Size : Safir.Dob.Typesystem.Int_32;
      Beginning_Of_Unused : Safir.Dob.Typesystem.Blob_T;
      Entity_Ptr : constant Safir.Dob.Entity.Entity_Class_Access := Entity.Ref;
      Instance_Utf_8_Str_Ptr : C.Strings.chars_ptr;
      Handler_Utf_8_Str_Ptr : C.Strings.chars_ptr;
   begin
      if Entity_Ptr = null then
         Throw (Safir.Dob.Typesystem.Software_Violation_Exception'Identity,
                "Attempt to serialize an entity null pointer to binary!");
      end if;

      Blob_Size := Entity_Ptr.Calculate_Blob_Size;

      Blob := C_Malloc (C.size_t (Blob_Size));

      Safir.Dob.Typesystem.Internal_Operations.Format_Blob (Blob,
                                                            Blob_Size,
                                                            Entity_Ptr.Get_Type_Id,
                                                            Beginning_Of_Unused);
      Entity_Ptr.Write_To_Blob (Blob, Beginning_Of_Unused);


      Instance_Utf_8_Str_Ptr :=
        C.Strings.New_String (To_String (Utf_8_String (Instance_Id)));
      Handler_Utf_8_Str_Ptr :=
        C.Strings.New_String (To_String (Utf_8_String (Handler_Id)));

      Safir.Dob.Interf.Set_Entity
        (Get_Controller_Id (Connection_Base'Class (Self)),
         Blob,
         Get_Raw_Value (Instance_Id),
         Instance_Utf_8_Str_Ptr,
         Get_Raw_Value (Handler_Id),
         Handler_Utf_8_Str_Ptr,
         C.char'Val (Boolean'Pos (Consider_Change_Flags)),
         C.char'Val (Boolean'Pos (False)), -- false => this is not an initial injection
         Success);

      C_Free (Blob);
      C.Strings.Free (Instance_Utf_8_Str_Ptr);
      C.Strings.Free (Handler_Utf_8_Str_Ptr);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
   end Set;

   procedure Delete
     (Self          : in Connection_Base;
      Entity_Id     : in Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type;
      Handler_Id    : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type) is

      use Safir.Dob.Typesystem.Instance_Id;
      use Safir.Dob.Typesystem.Handler_Id;

      Success : C.char;
      Instance_Utf_8_Str_Ptr : C.Strings.chars_ptr;
      Handler_Utf_8_Str_Ptr : C.Strings.chars_ptr;
      Instance_Id : constant Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type :=
                      Safir.Dob.Typesystem.Entity_Id.Get_Instance_Id (Entity_Id);
   begin
      Instance_Utf_8_Str_Ptr :=
        C.Strings.New_String (To_String (Utf_8_String (Instance_Id)));
      Handler_Utf_8_Str_Ptr :=
        C.Strings.New_String (To_String (Utf_8_String (Handler_Id)));

      Safir.Dob.Interf.Delete_Entity
        (Get_Controller_Id (Connection_Base'Class (Self)),
         Safir.Dob.Typesystem.Entity_Id.Get_Type_Id (Entity_Id),
         Get_Raw_Value (Instance_Id),
         Instance_Utf_8_Str_Ptr,
         C.char'Val (Boolean'Pos (False)),  -- false => not all instances
         Get_Raw_Value (Handler_Id),
         Handler_Utf_8_Str_Ptr,
         Success);

      C.Strings.Free (Instance_Utf_8_Str_Ptr);
      C.Strings.Free (Handler_Utf_8_Str_Ptr);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;

   end Delete;

   procedure Delete_All_Instances
     (Self        : in Connection_Base;
      Type_Id     : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id  : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type) is

      use Safir.Dob.Typesystem.Handler_Id;

      Success : C.char;
      Instance_Utf_8_Str_Ptr : C.Strings.chars_ptr;
      Handler_Utf_8_Str_Ptr : C.Strings.chars_ptr;
   begin
      Instance_Utf_8_Str_Ptr := C.Strings.New_String ("");
      Handler_Utf_8_Str_Ptr :=
        C.Strings.New_String (To_String (Utf_8_String (Handler_Id)));

      Safir.Dob.Interf.Delete_Entity
        (Get_Controller_Id (Connection_Base'Class (Self)),
         Type_Id,
         0,
         Instance_Utf_8_Str_Ptr,
         C.char'Val (Boolean'Pos (True)),  -- true => all instances
         Get_Raw_Value (Handler_Id),
         Handler_Utf_8_Str_Ptr,
         Success);

      C.Strings.Free (Instance_Utf_8_Str_Ptr);
      C.Strings.Free (Handler_Utf_8_Str_Ptr);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;

   end Delete_All_Instances;

   function Get_Entity_Iterator
     (Self               : in Connection_Base;
      Type_Id            : in Safir.Dob.Typesystem.Type_Id;
      Include_Subclasses : in Boolean) return Safir.Dob.Entity_Iterators.Entity_Iterator is
   begin
      return Safir.Dob.Entity_Iterators.Create
        (Get_Controller_Id (Connection_Base'Class (Self)),
         Type_Id,
         Include_Subclasses);
   end Get_Entity_Iterator;

   ----------
   -- Read --
   ----------
   function Read
     (Self      : in Connection_Base;
      Entity_Id : in Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type)
      return Safir.Dob.Entity_Proxies.Entity_Proxy is

      use Safir.Dob.Typesystem.Instance_Id;
      use Safir.Dob.Typesystem.Entity_Id;

      Success : C.char;
      Current_Blob : Safir.Dob.Typesystem.Blob_T;
      Current_State : Safir.Dob.Typesystem.Char_Star;
      Utf_8_Str_Ptr : C.Strings.chars_ptr;
      Instance_Id : constant Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type :=
                      Safir.Dob.Typesystem.Entity_Id.Get_Instance_Id (Entity_Id);
      Entity_Proxy_Impl_Ptr : Safir.Dob.Entity_Proxy_Impl_Pointers.Smart_Pointer;
   begin
      Utf_8_Str_Ptr :=
        C.Strings.New_String (To_String (Utf_8_String (Instance_Id)));

      Safir.Dob.Interf.Read_Entity
        (Get_Controller_Id (Connection_Base'Class (Self)),
         Get_Type_Id (Entity_Id),
         Get_Raw_Value (Instance_Id),
         Utf_8_Str_Ptr,
         Current_Blob,
         Current_State,
         Success);

      C.Strings.Free (Utf_8_Str_Ptr);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;

      Entity_Proxy_Impl_Ptr := Safir.Dob.Entity_Proxy_Impl_Pointers.Create
        (Safir.Dob.Entity_Proxy_Impls.Create (Current_Blob => Current_Blob,
                                              Current_State  => Current_State,
                                              Previous_Blob  => null,
                                              Previous_State => null,
                                              Add_Reference  => False,
                                              Timestamp_Diff => False));

      -- Set the max number of Adjusts (copy operations) that is allowed. This is to ensure
      -- that the user can't hold any refrences to shared memory by copying
      -- the receivied proxy. Currently I (AWI) am not sure if the number of Adjusts
      -- on the way out of this function is implementation dependant. This have to be
      -- checked.
      Entity_Proxy_Impl_Ptr.Set_Adjust_Limitation (3);

      return Safir.Dob.Entity_Proxies.Create (Entity_Proxy_Impl_Ptr);

   end Read;

   function Is_Created
     (Self      : in Connection_Base;
      Entity_Id : in Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type)
      return Boolean is

      use Safir.Dob.Typesystem.Instance_Id;
      use Safir.Dob.Typesystem.Entity_Id;

      Is_Created : C.char;
      Success : C.char;
      Utf_8_Str_Ptr : C.Strings.chars_ptr;
      Instance_Id : constant Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type :=
                      Safir.Dob.Typesystem.Entity_Id.Get_Instance_Id (Entity_Id);
   begin
      Utf_8_Str_Ptr :=
        C.Strings.New_String (To_String (Utf_8_String (Instance_Id)));

      Safir.Dob.Interf.Is_Created
        (Get_Controller_Id (Connection_Base'Class (Self)),
         Get_Type_Id (Entity_Id),
         Get_Raw_Value (Instance_Id),
         Utf_8_Str_Ptr,
         Is_Created,
         Success);

      C.Strings.Free (Utf_8_Str_Ptr);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;

      return C.char'Pos (Is_Created) /= 0;
   end Is_Created;

   function Get_Number_Of_Instances
     (Self               : in Connection_Base;
      Type_Id            : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id         : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;
      Include_Subclasses : in Boolean) return Safir.Dob.Typesystem.Int_64 is

      use Safir.Dob.Typesystem.Handler_Id;

      Success : C.char;
      Handler_Utf_8_Str_Ptr : C.Strings.chars_ptr;
      Num_Instances : Safir.Dob.Typesystem.Int_64;
   begin
      Handler_Utf_8_Str_Ptr :=
        C.Strings.New_String (To_String (Utf_8_String (Handler_Id)));

      Safir.Dob.Interf.Get_Number_Of_Instances
        (Get_Controller_Id (Connection_Base'Class (Self)),
         Type_Id,
         Get_Raw_Value (Handler_Id),
         Handler_Utf_8_Str_Ptr,
         C.char'Val (Boolean'Pos (Include_Subclasses)),
         Num_Instances,
         Success);

      C.Strings.Free (Handler_Utf_8_Str_Ptr);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;

      return Num_Instances;

   end Get_Number_Of_Instances;


   function Get_Instance_Id_Policy
     (Self               : in Connection_Base;
      Type_Id            : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id         : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type) return Safir.Dob.Instance_Id_Policy.Enumeration is

      use Safir.Dob.Typesystem.Handler_Id;

      Success : C.char;
      Policy : Safir.Dob.Typesystem.Enumeration_Value;
   begin

      Safir.Dob.Interf.Get_Instance_Id_Policy
        (Get_Controller_Id (Connection_Base'Class (Self)),
         Type_Id,
         Get_Raw_Value (Handler_Id),
         Policy,
         Success);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;

      return Safir.Dob.Instance_Id_Policy.Enumeration'Val (Policy);

   end Get_Instance_Id_Policy;


   procedure Exit_Dispatch (Self : in Connection_Base) is
      Success : C.char;
   begin
      Safir.Dob.Interf.Exit_Dispatch
        (Get_Controller_Id (Connection_Base'Class (Self)),
         Success);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
   end Exit_Dispatch;


end Safir.Dob.Connection_Bases;
