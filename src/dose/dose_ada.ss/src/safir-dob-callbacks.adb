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
pragma Warnings ("F");

with Ada.Exceptions;
with Safir.Dob.Typesystem.Utilities; use Safir.Dob.Typesystem.Utilities;
with Safir.Dob.Typesystem.Handler_Id;
with Safir.Dob.Consumers;
with Safir.Dob.Typesystem.Library_Exceptions;
with Safir.Dob.Entity_Proxy_Impls;
with Safir.Dob.Entity_Proxy_Impl_Pointers;
with Safir.Dob.Entity_Proxies;
with Safir.Dob.Entity_Request_Proxy_Impls;
with Safir.Dob.Entity_Request_Proxy_Impl_Pointers;
with Safir.Dob.Entity_Request_Proxies;
with Safir.Dob.Message_Proxy_Impls;
with Safir.Dob.Message_Proxy_Impl_Pointers;
with Safir.Dob.Message_Proxies;
with Safir.Dob.Service_Request_Proxy_Impls;
with Safir.Dob.Service_Request_Proxy_Impl_Pointers;
with Safir.Dob.Service_Request_Proxies;
with Safir.Dob.Injected_Entity_Proxy_Impls;
with Safir.Dob.Injected_Entity_Proxy_Impl_Pointers;
with Safir.Dob.Injected_Entity_Proxies;
with Safir.Dob.Response_Proxy_Impls;
with Safir.Dob.Response_Proxy_Impl_Pointers;
with Safir.Dob.Response_Proxies;
with Safir.Dob.Response_Sender_Impls;
with Safir.Dob.Response_Sender_Impl_Pointers;
with Safir.Dob.Response_Senders;

pragma Warnings ("D");  -- turn off warnings for implicit dereference

package body Safir.Dob.Callbacks is

   use type Safir.Dob.Typesystem.Blob_T;

   procedure Throw (E : Ada.Exceptions.Exception_Id; Message : String := "")
                    renames  Ada.Exceptions.Raise_Exception;


   procedure On_Do_Dispatch_Cb
     (Consumer   : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Success    : out C.char) is
   begin
      Success := C.char'Val (Boolean'Pos (False));
      Safir.Dob.Consumers.Dispatcher'Class (Consumer.all).On_Do_Dispatch;
      Success := C.char'Val (Boolean'Pos (True));
   exception
      when E : others =>
         Safir.Dob.Typesystem.Library_Exceptions.Set (E);
   end On_Do_Dispatch_Cb;

   procedure On_Stop_Order_Cb
     (Consumer     : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Success      :    out C.char) is
   begin
      Success := C.char'Val (Boolean'Pos (False));
      Safir.Dob.Consumers.Stop_Handler'Class (Consumer.all).On_Stop_Order;
      Success := C.char'Val (Boolean'Pos (True));
   exception
      when E : others =>
         Safir.Dob.Typesystem.Library_Exceptions.Set (E);
   end On_Stop_Order_Cb;

   procedure On_New_Entity_Cb
       (Current_Blob      : in Safir.Dob.Typesystem.Blob_T;
        Current_State     : in Safir.Dob.Typesystem.Char_Star;
        Consumer          : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
        Timestamp_Diff    : in C.char;
        Success           : out C.char) is

      Entity_Proxy_Impl_Ptr : Safir.Dob.Entity_Proxy_Impl_Pointers.Smart_Pointer;

   begin
      pragma Assert (Current_Blob /= null, "Got NULL currentBlob in OnNewEntity from dose!!!");

      Success := C.char'Val (Boolean'Pos (False));

      Entity_Proxy_Impl_Ptr := Safir.Dob.Entity_Proxy_Impl_Pointers.Create
        (Safir.Dob.Entity_Proxy_Impls.Create
           (Current_Blob => Current_Blob,
            Current_State => Current_State,
            Previous_Blob => null,
            Previous_State => null,
            Add_Reference => True,
            Timestamp_Diff => Boolean'Val (C.char'Pos (Timestamp_Diff))));

      Safir.Dob.Consumers.Entity_Subscriber'Class (Consumer.all).On_New_Entity
        (Safir.Dob.Entity_Proxies.Create (Entity_Proxy_Impl_Ptr));

      Success := C.char'Val (Boolean'Pos (True));
   exception
      when E : others =>
         Safir.Dob.Typesystem.Library_Exceptions.Set (E);
   end On_New_Entity_Cb;

   procedure On_Updated_Entity_Cb
       (Current_Blob       : in Safir.Dob.Typesystem.Blob_T;
        Current_State      : in Safir.Dob.Typesystem.Char_Star;
        Previous_Blob      : in Safir.Dob.Typesystem.Blob_T;
        Previous_State     : in Safir.Dob.Typesystem.Char_Star;
        Consumer           : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
        Timestamp_Diff     : in C.char;
        Success            : out C.char) is

      Entity_Proxy_Impl_Ptr : Safir.Dob.Entity_Proxy_Impl_Pointers.Smart_Pointer;

   begin
      pragma Assert (Current_Blob /= null, "Got NULL currentBlob in OnUpdatedEntity from dose!!!");

      Success := C.char'Val (Boolean'Pos (False));

      Entity_Proxy_Impl_Ptr := Safir.Dob.Entity_Proxy_Impl_Pointers.Create
        (Safir.Dob.Entity_Proxy_Impls.Create
           (Current_Blob => Current_Blob,
            Current_State => Current_State,
            Previous_Blob => Previous_Blob,
            Previous_State => Previous_State,
            Add_Reference => True,
            Timestamp_Diff => Boolean'Val (C.char'Pos (Timestamp_Diff))));

      Safir.Dob.Consumers.Entity_Subscriber'Class (Consumer.all).On_Updated_Entity
        (Safir.Dob.Entity_Proxies.Create (Entity_Proxy_Impl_Ptr));

      Success := C.char'Val (Boolean'Pos (True));
   exception
      when E : others =>
         Safir.Dob.Typesystem.Library_Exceptions.Set (E);
   end On_Updated_Entity_Cb;

   procedure On_Deleted_Entity_Cb
       (Current_State      : in Safir.Dob.Typesystem.Char_Star;
        Previous_Blob      : in Safir.Dob.Typesystem.Blob_T;
        Previous_State     : in Safir.Dob.Typesystem.Char_Star;
        Explicitly_Deleted : in C.char;
        Consumer           : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
        Timestamp_Diff     : in C.char;
        Success            : out C.char) is

      Entity_Proxy_Impl_Ptr : Safir.Dob.Entity_Proxy_Impl_Pointers.Smart_Pointer;

   begin
      pragma Assert (Previous_Blob /= null, "Got NULL previousBlob in OnDeletedEntity from dose!!!");

      Success := C.char'Val (Boolean'Pos (False));

      Entity_Proxy_Impl_Ptr := Safir.Dob.Entity_Proxy_Impl_Pointers.Create
        (Safir.Dob.Entity_Proxy_Impls.Create
           (Current_Blob => null,
            Current_State => Current_State,
            Previous_Blob => Previous_Blob,
            Previous_State => Previous_State,
            Add_Reference => True,
            Timestamp_Diff => Boolean'Val (C.char'Pos (Timestamp_Diff))));

      Safir.Dob.Consumers.Entity_Subscriber'Class (Consumer.all).On_Deleted_Entity
        (Safir.Dob.Entity_Proxies.Create (Entity_Proxy_Impl_Ptr),
         Boolean'Val (C.char'Pos (Explicitly_Deleted)));

      Success := C.char'Val (Boolean'Pos (True));
   exception
      when E : others =>
         Safir.Dob.Typesystem.Library_Exceptions.Set (E);
   end On_Deleted_Entity_Cb;

   procedure On_Create_Request_Cb
     (Request_Blob               : in Safir.Dob.Typesystem.Blob_T;
      State                      : in Safir.Dob.Typesystem.Char_Star;
      Controller_Id              : in Safir.Dob.Defs.Controller_Id;
      Response_Id                : in Safir.Dob.Defs.Response_Id;
      Consumer                   : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Success                    : out C.char) is

      Entity_Request_Proxy_Impl_Ptr : Safir.Dob.Entity_Request_Proxy_Impl_Pointers.Smart_Pointer;
      Response_Sender_Impl_Ptr : Safir.Dob.Response_Sender_Impl_Pointers.Smart_Pointer;
   begin
      pragma Assert (Request_Blob /= null, "Got NULL request in OnCreateRequest from dose!!!");

      Success := C.char'Val (Boolean'Pos (False));

      Entity_Request_Proxy_Impl_Ptr := Safir.Dob.Entity_Request_Proxy_Impl_Pointers.Create
        (Safir.Dob.Entity_Request_Proxy_Impls.Create
           (Request_Blob => Request_Blob,
            State => State));

      Response_Sender_Impl_Ptr := Safir.Dob.Response_Sender_Impl_Pointers.Create
        (Safir.Dob.Response_Sender_Impls.Create
           (Controller_Id,
            Consumer,
            Response_Id));

      Safir.Dob.Consumer_Bases.Entity_Request_Base'Class (Consumer.all).On_Create_Request
        (Safir.Dob.Entity_Request_Proxies.Create (Entity_Request_Proxy_Impl_Ptr),
         Safir.Dob.Response_Senders.Create (Response_Sender_Impl_Ptr));

      Success := C.char'Val (Boolean'Pos (True));
   exception
      when E : others =>
         Safir.Dob.Typesystem.Library_Exceptions.Set (E);
   end On_Create_Request_Cb;

   procedure On_Update_Request_Cb
     (Request_Blob               : in Safir.Dob.Typesystem.Blob_T;
      State                      : in Safir.Dob.Typesystem.Char_Star;
      Controller_Id              : in Safir.Dob.Defs.Controller_Id;
      Response_Id                : in Safir.Dob.Defs.Response_Id;
      Consumer                   : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Success                    : out C.char) is

      Entity_Request_Proxy_Impl_Ptr : Safir.Dob.Entity_Request_Proxy_Impl_Pointers.Smart_Pointer;
      Response_Sender_Impl_Ptr : Safir.Dob.Response_Sender_Impl_Pointers.Smart_Pointer;
   begin
      pragma Assert (Request_Blob /= null, "Got NULL request in OnUpdateRequest from dose!!!");

      Success := C.char'Val (Boolean'Pos (False));

      Entity_Request_Proxy_Impl_Ptr := Safir.Dob.Entity_Request_Proxy_Impl_Pointers.Create
        (Safir.Dob.Entity_Request_Proxy_Impls.Create
           (Request_Blob => Request_Blob,
            State => State));

      Response_Sender_Impl_Ptr := Safir.Dob.Response_Sender_Impl_Pointers.Create
        (Safir.Dob.Response_Sender_Impls.Create
           (Controller_Id,
            Consumer,
            Response_Id));

      Safir.Dob.Consumer_Bases.Entity_Request_Base'Class (Consumer.all).On_Update_Request
        (Safir.Dob.Entity_Request_Proxies.Create (Entity_Request_Proxy_Impl_Ptr),
         Safir.Dob.Response_Senders.Create (Response_Sender_Impl_Ptr));

      Success := C.char'Val (Boolean'Pos (True));
   exception
      when E : others =>
         Safir.Dob.Typesystem.Library_Exceptions.Set (E);
   end On_Update_Request_Cb;

   procedure On_Delete_Request_Cb
     (State                      : in Safir.Dob.Typesystem.Char_Star;
      Controller_Id              : in Safir.Dob.Defs.Controller_Id;
      Response_Id                : in Safir.Dob.Defs.Response_Id;
      Consumer                   : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Success                    : out C.char) is

      Entity_Request_Proxy_Impl_Ptr : Safir.Dob.Entity_Request_Proxy_Impl_Pointers.Smart_Pointer;
      Response_Sender_Impl_Ptr : Safir.Dob.Response_Sender_Impl_Pointers.Smart_Pointer;
   begin
      Success := C.char'Val (Boolean'Pos (False));

      Entity_Request_Proxy_Impl_Ptr := Safir.Dob.Entity_Request_Proxy_Impl_Pointers.Create
        (Safir.Dob.Entity_Request_Proxy_Impls.Create
           (Request_Blob => null,
            State => State));

      Response_Sender_Impl_Ptr := Safir.Dob.Response_Sender_Impl_Pointers.Create
        (Safir.Dob.Response_Sender_Impls.Create
           (Controller_Id,
            Consumer,
            Response_Id));

      Safir.Dob.Consumer_Bases.Entity_Request_Base'Class (Consumer.all).On_Delete_Request
        (Safir.Dob.Entity_Request_Proxies.Create (Entity_Request_Proxy_Impl_Ptr),
         Safir.Dob.Response_Senders.Create (Response_Sender_Impl_Ptr));

      Success := C.char'Val (Boolean'Pos (True));
   exception
      when E : others =>
         Safir.Dob.Typesystem.Library_Exceptions.Set (E);
   end On_Delete_Request_Cb;

   procedure On_Service_Request_Cb
     (Request_Blob               : in Safir.Dob.Typesystem.Blob_T;
      State                      : in Safir.Dob.Typesystem.Char_Star;
      Controller_Id              : in Safir.Dob.Defs.Controller_Id;
      Response_Id                : in Safir.Dob.Defs.Response_Id;
      Consumer                   : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Success                    : out C.char) is


      Service_Request_Proxy_Impl_Ptr : Safir.Dob.Service_Request_Proxy_Impl_Pointers.Smart_Pointer;
      Response_Sender_Impl_Ptr : Safir.Dob.Response_Sender_Impl_Pointers.Smart_Pointer;
   begin
      pragma Assert (Request_Blob /= null, "Got NULL request in On_Service_Request_Cb from dose!!!");

      Success := C.char'Val (Boolean'Pos (False));

      Service_Request_Proxy_Impl_Ptr := Safir.Dob.Service_Request_Proxy_Impl_Pointers.Create
        (Safir.Dob.Service_Request_Proxy_Impls.Create
           (Request_Blob,
            State));

      Response_Sender_Impl_Ptr := Safir.Dob.Response_Sender_Impl_Pointers.Create
        (Safir.Dob.Response_Sender_Impls.Create
           (Controller_Id,
            Consumer,
            Response_Id));

      Safir.Dob.Consumer_Bases.Service_Request_Base'Class (Consumer.all).On_Service_Request
        (Safir.Dob.Service_Request_Proxies.Create (Service_Request_Proxy_Impl_Ptr),
         Safir.Dob.Response_Senders.Create (Response_Sender_Impl_Ptr));

      Success := C.char'Val (Boolean'Pos (True));
   exception
      when E : others =>
         Safir.Dob.Typesystem.Library_Exceptions.Set (E);
   end On_Service_Request_Cb;

   procedure On_Response_Cb
     (Request_Id         : in Safir.Dob.Defs.Request_Id;
      Response_Blob      : in Safir.Dob.Typesystem.Blob_T;
      Response_State     : in Safir.Dob.Typesystem.Char_Star;
      Request_Blob       : in Safir.Dob.Typesystem.Blob_T;
      Request_State      : in Safir.Dob.Typesystem.Char_Star;
      Consumer           : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Success            : out C.char) is

      Response_Proxy_Impl_Ptr : Safir.Dob.Response_Proxy_Impl_Pointers.Smart_Pointer;
   begin
      pragma Assert (Response_Blob /= null, "Got NULL response in OnResponse from dose!!!");

      Success := C.char'Val (Boolean'Pos (False));

      Response_Proxy_Impl_Ptr := Safir.Dob.Response_Proxy_Impl_Pointers.Create
        (Safir.Dob.Response_Proxy_Impls.Create
           (Request_Id => Request_Id,
            Response_Blob => Response_Blob,
            Response_State => Response_State,
            Request_Blob   => Request_Blob,
            Request_State => Request_State));

      Safir.Dob.Consumers.Requestor'Class (Consumer.all).On_Response
        (Safir.Dob.Response_Proxies.Create (Response_Proxy_Impl_Ptr));

      Success := C.char'Val (Boolean'Pos (True));
   exception
      when E : others =>
         Safir.Dob.Typesystem.Library_Exceptions.Set (E);
   end On_Response_Cb;

   procedure On_Message_Cb
     (Message_Blob       : in Safir.Dob.Typesystem.Blob_T;
      Message_State      : in Safir.Dob.Typesystem.Char_Star;
      Consumer           : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Success            : out C.char) is

      Proxy_Impl_Pointer : Safir.Dob.Message_Proxy_Impl_Pointers.Smart_Pointer;
   begin
      pragma Assert (Message_Blob /= null, "Got NULL message in On_Message_Cb from dose!!!");

      Success := C.char'Val (Boolean'Pos (False));

      Proxy_Impl_Pointer := Safir.Dob.Message_Proxy_Impl_Pointers.Create
        (Safir.Dob.Message_Proxy_Impls.Create (Message_Blob, Message_State));

      Safir.Dob.Consumers.Message_Subscriber'Class (Consumer.all).On_Message
        (Safir.Dob.Message_Proxies.Create (Proxy_Impl_Pointer));

      if Proxy_Impl_Pointer.Use_Count > 1 then
         Throw (Safir.Dob.Typesystem.Software_Violation_Exception'Identity,
                "The application is keeping a Message_Proxy when returning from On_Message callback");
      end if;

      Success := C.char'Val (Boolean'Pos (True));
   exception
      when E : others =>
         Safir.Dob.Typesystem.Library_Exceptions.Set (E);
   end On_Message_Cb;

   procedure On_Registered_Cb
     (Type_Id                 : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id              : in Safir.Dob.Typesystem.Int_64;
      Handler_Id_Str          : in C.Strings.chars_ptr;
      Consumer                : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Success                 : out C.char) is
   begin
      Success := C.char'Val (Boolean'Pos (False));

      Safir.Dob.Consumers.Registration_Subscriber'Class (Consumer.all).On_Registered
        (Type_Id, Safir.Dob.Typesystem.Handler_Id.Create_Handler_Id
           (Handler_Id, From_Utf_8 (C.To_Ada (C.Strings.Value (Handler_Id_Str)))));

      Success := C.char'Val (Boolean'Pos (True));
   exception
      when E : others =>
         Safir.Dob.Typesystem.Library_Exceptions.Set (E);
   end On_Registered_Cb;

   procedure On_Unregistered_Cb
     (Type_Id                 : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id              : in Safir.Dob.Typesystem.Int_64;
      Handler_Id_Str          : in C.Strings.chars_ptr;
      Consumer                : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Success                 : out C.char) is
   begin
      Success := C.char'Val (Boolean'Pos (False));

      Safir.Dob.Consumers.Registration_Subscriber'Class (Consumer.all).On_Unregistered
        (Type_Id, Safir.Dob.Typesystem.Handler_Id.Create_Handler_Id
           (Handler_Id, From_Utf_8 (C.To_Ada (C.Strings.Value (Handler_Id_Str)))));

      Success := C.char'Val (Boolean'Pos (True));
   exception
      when E : others =>
         Safir.Dob.Typesystem.Library_Exceptions.Set (E);
   end On_Unregistered_Cb;

   procedure On_Revoked_Registration_Cb
     (Type_Id                   : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id                : in Safir.Dob.Typesystem.Int_64;
      Handler_Id_Str            : in C.Strings.chars_ptr;
      Consumer                  : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Success                   : out C.char) is
   begin
      Success := C.char'Val (Boolean'Pos (False));

      Safir.Dob.Consumer_Bases.Revoked_Registration_Base'Class (Consumer.all).On_Revoked_Registration
        (Type_Id, Safir.Dob.Typesystem.Handler_Id.Create_Handler_Id
           (Handler_Id, From_Utf_8 (C.To_Ada (C.Strings.Value (Handler_Id_Str)))));

      Success := C.char'Val (Boolean'Pos (True));
   exception
      when E : others =>
         Safir.Dob.Typesystem.Library_Exceptions.Set (E);
   end On_Revoked_Registration_Cb;

   procedure On_Completed_Registration_Cb
     (Type_Id                     : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id                  : in Safir.Dob.Typesystem.Int_64;
      Handler_Id_Str              : in C.Strings.chars_ptr;
      Consumer                    : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Success                     : out C.char) is
   begin
      Success := C.char'Val (Boolean'Pos (False));

      Safir.Dob.Consumer_Bases.Completed_Registration_Base'Class (Consumer.all).On_Completed_Registration
        (Type_Id, Safir.Dob.Typesystem.Handler_Id.Create_Handler_Id
           (Handler_Id, From_Utf_8 (C.To_Ada (C.Strings.Value (Handler_Id_Str)))));

      Success := C.char'Val (Boolean'Pos (True));
   exception
      when E : others =>
         Safir.Dob.Typesystem.Library_Exceptions.Set (E);
   end On_Completed_Registration_Cb;

   procedure On_Injected_New_Entity_Cb
       (Injection_Blob        : in Safir.Dob.Typesystem.Blob_T;
        Injection_State       : in Safir.Dob.Typesystem.Char_Star;
        Consumer              : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
        Success               : out C.char) is

      Injected_Entity_Proxy_Impl_Ptr : Safir.Dob.Injected_Entity_Proxy_Impl_Pointers.Smart_Pointer;

   begin
      pragma Assert (Injection_Blob /= null, "Got NULL blob in OnInjectedNewEntity from dose!!!");

      Success := C.char'Val (Boolean'Pos (False));

      Injected_Entity_Proxy_Impl_Ptr := Safir.Dob.Injected_Entity_Proxy_Impl_Pointers.Create
        (Safir.Dob.Injected_Entity_Proxy_Impls.Create
           (Injection_Blob => Injection_Blob,
            Injection_State => Injection_State,
            Current_Blob => null,
            Current_State => null));

      Safir.Dob.Consumer_Bases.Entity_Injection_Base'Class (Consumer.all).On_Injected_New_Entity
        (Safir.Dob.Injected_Entity_Proxies.Create (Injected_Entity_Proxy_Impl_Ptr));

      Success := C.char'Val (Boolean'Pos (True));
   exception
      when E : others =>
         Safir.Dob.Typesystem.Library_Exceptions.Set (E);
   end On_Injected_New_Entity_Cb;

   procedure On_Injected_Updated_Entity_Cb
       (Injection_Blob        : in Safir.Dob.Typesystem.Blob_T;
        Injection_State       : in Safir.Dob.Typesystem.Char_Star;
        Current_Blob          : in Safir.Dob.Typesystem.Blob_T;
        Current_State         : in Safir.Dob.Typesystem.Char_Star;
        Consumer              : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
        Success               : out C.char) is

      Injected_Entity_Proxy_Impl_Ptr : Safir.Dob.Injected_Entity_Proxy_Impl_Pointers.Smart_Pointer;

   begin
      pragma Assert (Injection_Blob /= null, "Got NULL blob in OnInjectedUpdatedEntity from dose!!!");

      Success := C.char'Val (Boolean'Pos (False));

      Injected_Entity_Proxy_Impl_Ptr := Safir.Dob.Injected_Entity_Proxy_Impl_Pointers.Create
        (Safir.Dob.Injected_Entity_Proxy_Impls.Create
           (Injection_Blob => Injection_Blob,
            Injection_State => Injection_State,
            Current_Blob => Current_Blob,
            Current_State => Current_State));

      Safir.Dob.Consumer_Bases.Entity_Injection_Base'Class (Consumer.all).On_Injected_Updated_Entity
        (Safir.Dob.Injected_Entity_Proxies.Create (Injected_Entity_Proxy_Impl_Ptr));

      Success := C.char'Val (Boolean'Pos (True));
   exception
      when E : others =>
         Safir.Dob.Typesystem.Library_Exceptions.Set (E);
   end On_Injected_Updated_Entity_Cb;

   procedure On_Injected_Deleted_Entity_Cb
       (Injection_State       : in Safir.Dob.Typesystem.Char_Star;
        Current_Blob          : in Safir.Dob.Typesystem.Blob_T;
        Current_State         : in Safir.Dob.Typesystem.Char_Star;
        Consumer              : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
        Success               : out C.char) is

      Injected_Entity_Proxy_Impl_Ptr : Safir.Dob.Injected_Entity_Proxy_Impl_Pointers.Smart_Pointer;

   begin
      Success := C.char'Val (Boolean'Pos (False));

      Injected_Entity_Proxy_Impl_Ptr := Safir.Dob.Injected_Entity_Proxy_Impl_Pointers.Create
        (Safir.Dob.Injected_Entity_Proxy_Impls.Create
           (Injection_Blob => null,
            Injection_State => Injection_State,
            Current_Blob => Current_Blob,
            Current_State => Current_State));

      Safir.Dob.Consumer_Bases.Entity_Injection_Base'Class (Consumer.all).On_Injected_Deleted_Entity
        (Safir.Dob.Injected_Entity_Proxies.Create (Injected_Entity_Proxy_Impl_Ptr));

      Success := C.char'Val (Boolean'Pos (True));
   exception
      when E : others =>
         Safir.Dob.Typesystem.Library_Exceptions.Set (E);
   end On_Injected_Deleted_Entity_Cb;

   procedure On_Initial_Injections_Done_Cb
     (Type_Id                     : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id                  : in Safir.Dob.Typesystem.Int_64;
      Handler_Id_Str              : in C.Strings.chars_ptr;
      Consumer                    : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Success                     : out C.char) is
   begin
      Success := C.char'Val (Boolean'Pos (False));

      Safir.Dob.Consumer_Bases.Entity_Injection_Base'Class (Consumer.all).On_Initial_Injections_Done
        (Type_Id, Safir.Dob.Typesystem.Handler_Id.Create_Handler_Id
           (Handler_Id, From_Utf_8 (C.To_Ada (C.Strings.Value (Handler_Id_Str)))));

      Success := C.char'Val (Boolean'Pos (True));
   exception
      when E : others =>
         Safir.Dob.Typesystem.Library_Exceptions.Set (E);
   end On_Initial_Injections_Done_Cb;

   procedure On_Not_Request_Overflow_Cb
     (Consumer     : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Success      : out C.char) is
   begin
      Success := C.char'Val (Boolean'Pos (False));

      Safir.Dob.Consumers.Requestor'Class (Consumer.all).On_Not_Request_Overflow;

      Success := C.char'Val (Boolean'Pos (True));
   exception
      when E : others =>
         Safir.Dob.Typesystem.Library_Exceptions.Set (E);
   end On_Not_Request_Overflow_Cb;

   procedure On_Not_Message_Overflow_Cb
     (Consumer          : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Success           : out C.char) is
   begin
      Success := C.char'Val (Boolean'Pos (False));

      Safir.Dob.Consumers.Message_Sender'Class (Consumer.all).On_Not_Message_Overflow;
      Success := C.char'Val (Boolean'Pos (True));
   exception
      when E : others =>
         Safir.Dob.Typesystem.Library_Exceptions.Set (E);
   end On_Not_Message_Overflow_Cb;

end Safir.Dob.Callbacks;
