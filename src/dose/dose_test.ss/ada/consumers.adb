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
with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Dose_Test.Action_Enum;
with Dose_Test.Complex_Global_Entity;
with Dose_Test.Complex_Global_Message;
with Dose_Test.Complex_Global_Service;
with Dose_Test.Complex_Global_Entity_Base;
with Dose_Test.Complex_Global_Message_Base;
with Dose_Test.Complex_Global_Service_Base;
with Dose_Test.Last_Injection_Timestamp;
with Dose_Test.Root_Entity;
with Dose_Test.Root_Message;
with Dose_Test.Root_Service;
with Dose_Test.Successful_Create;
with Dose_Test.Successful_Delete;
with Dose_Test.Successful_Service;
with Dose_Test.Successful_Update;
with Logger;
with Safir.Dob.Access_Denied_Exception;
with Safir.Dob.Connection_Aspect_Injectors;
with Safir.Dob.Connection_Aspect_Miscs;
with Safir.Dob.Connection_Aspect_Postpones;
with Safir.Dob.Connection_Queue_Id;
with Safir.Dob.Entity;
with Safir.Dob.Entity_Id_Response;
with Safir.Dob.Message;
with Safir.Dob.Not_Found_Exception;
with Safir.Dob.Overflow_Exception;
with Safir.Dob.Response;
with Safir.Dob.Service;
with Safir.Dob.Entity_Iterators;
with Safir.Dob.Typesystem.Blob_Operations;
with Safir.Dob.Typesystem.Channel_Id;
with Safir.Dob.Typesystem.Container_Instantiations;
with Safir.Dob.Typesystem.Entity_Id;
with Safir.Dob.Typesystem.Instance_Id;
with Safir.Dob.Typesystem.Members;
with Safir.Dob.Typesystem.Object;
with Safir.Dob.Typesystem.Operations;
with Safir.Dob.Typesystem.Serialization;
with Safir.Dob.Typesystem.Utilities; use Safir.Dob.Typesystem.Utilities;
with Safir.Dob.Typesystem; use Safir.Dob.Typesystem;

pragma Warnings ("D"); -- turn off warnings for implicit dereference

package body Consumers is
   PREFIX : constant Wide_String := "Consumer";

   function "<" (Left, Right : Policy_Key_T) return Boolean is
      use type Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;
   begin
      if Left.Type_Id < Right.Type_Id or else
        Left.Handler_Id < Right.Handler_Id then
         return True;
      else
         return False;
      end if;
   end "<";


   function UWS (Source : Wide_String) return Unbounded_Wide_String
                  renames Ada.Strings.Wide_Unbounded.To_Unbounded_Wide_String;


   function Boolean_Image (B : Boolean) return Unbounded_Wide_String is
   begin
      if B then
         return To_Unbounded_Wide_String ("true");
      else
         return To_Unbounded_Wide_String ("false");
      end if;
   end Boolean_Image;

   function Need_Binary_Check
     (Obj_Smart_Ptr : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class) return Boolean is
      Obj_Ptr : constant Safir.Dob.Typesystem.Object.Object_Class_Access := Obj_Smart_Ptr.Ref;
   begin
      return Obj_Ptr.Get_Type_Id = Dose_Test.Complex_Global_Message.Class_Type_Id or
        Obj_Ptr.Get_Type_Id = Dose_Test.Complex_Global_Entity.Class_Type_Id or
        Obj_Ptr.Get_Type_Id = Dose_Test.Complex_Global_Service.Class_Type_Id;
   end Need_Binary_Check;

   procedure Check_Binary_Member_Internal
      (Container          : in     Safir.Dob.Typesystem.Container_Instantiations.Binary_Container.Container_Proxy;
       Needs_Modification :    out Boolean) is
      use type Ada.Containers.Count_Type;
   begin
      if not Container.Is_Null and then Container.Get_Val.Length > 10000 then -- only check for large sizes
         if Container.Get_Val.Length /= 10 * 1024 * 1024 then
            Logger.Put_Line ("Binary is wrong size!");
         end if;
         --TODO: Due to the slowness of binary_vectors as described in #734 we can't check the contents.
         -- just pretend everything went well.
--           else
--              declare
--                 use Safir.Dob.Typesystem.Binary_Vectors;
--              begin
--                 for I in 0 .. Container.Get_Val.Length loop
--                    if Container.Get_Val.Element (Safir.Dob.Typesystem.Int_32 (I)) /= Safir.Dob.Typesystem.Int_8 (((I + 128) mod 256) - 128) then
--                       Logger.Put_Line ("Bad value in binary!");
--                       exit;
--                    end if;
--                 end loop;
--              end;
--           end if;

         -- we do NOT want to print all this out to stdout, so we set it to null once we've checked it.
         Container.Set_Null;
         Needs_Modification := True;
      else
         Needs_Modification := False;
      end if;
   end Check_Binary_Member_Internal;


   function Check_Binary_Member
     (Obj_Smart_Ptr : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
      Blob          : in Safir.Dob.Typesystem.Blob_T) return Unbounded_Wide_String is

      use type Safir.Dob.Typesystem.Blob_T;

      function C_Malloc (Size : C.size_t) return Safir.Dob.Typesystem.Char_Star;
      pragma Import (C, C_Malloc, "malloc");

      procedure C_Free (X : in out Safir.Dob.Typesystem.Char_Star) is
         procedure Free (X : Safir.Dob.Typesystem.Char_Star);
         pragma Import (C, Free, "free");
      begin
         Free (X);
         X := null;
      end C_Free;

      function To_Blob (Object : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class)
                       return Safir.Dob.Typesystem.Blob_T is
         use Safir.Dob.Typesystem.Binary_Vectors;

         function To_Char is new Ada.Unchecked_Conversion (Safir.Dob.Typesystem.Int_8, C.char);

         Blob : Safir.Dob.Typesystem.Blob_T;
         Blob_Cursor : Safir.Dob.Typesystem.Blob_T;
         Bin : Vector;
         Bin_Cursor : Cursor;
      begin
         Safir.Dob.Typesystem.Serialization.To_Binary (Object, Bin);

         Blob := C_Malloc (C.size_t (Bin.Length));
         Blob_Cursor := Blob;
         Bin_Cursor := Bin.First;
         loop
            exit when Bin_Cursor = No_Element;
            Blob_Cursor.all := To_Char (Element (Bin_Cursor));
            Blob_Cursor := Blob_Cursor + 1;
            Bin_Cursor := Next (Bin_Cursor);
         end loop;
         return Blob;
      end To_Blob;

      Obj_Ptr : constant Safir.Dob.Typesystem.Object.Object_Class_Access := Obj_Smart_Ptr.Ref;
      Needs_Modification : Boolean;

   begin
      if Obj_Ptr.Get_Type_Id = Dose_Test.Complex_Global_Message.Class_Type_Id then
         declare
            Comp_Ptr : constant Dose_Test.Complex_Global_Message.Complex_Global_Message_Class_Access :=
               Dose_Test.Complex_Global_Message.Complex_Global_Message_Class_Access (Obj_Smart_Ptr.Ref);
            New_Blob : Safir.Dob.Typesystem.Blob_T;
            Xml : Unbounded_Wide_String;
         begin
            Check_Binary_Member_Internal (Comp_Ptr.Binary_Member, Needs_Modification);
            if Needs_Modification then
               New_Blob := To_Blob (Obj_Smart_Ptr);
               Safir.Dob.Typesystem.Blob_Operations.Set_Null (New_Blob, Dose_Test.Complex_Global_Message_Base.Binary_Member_Member_Index, 0);
               Xml := Safir.Dob.Typesystem.Serialization.To_Xml (New_Blob);
               C_Free (New_Blob);
               return Xml;
            end if;
         end;
      elsif Obj_Ptr.Get_Type_Id = Dose_Test.Complex_Global_Entity.Class_Type_Id then
         declare
            Comp_Ptr : constant Dose_Test.Complex_Global_Entity.Complex_Global_Entity_Class_Access :=
               Dose_Test.Complex_Global_Entity.Complex_Global_Entity_Class_Access (Obj_Smart_Ptr.Ref);
            New_Blob : Safir.Dob.Typesystem.Blob_T := null;
         begin
            Check_Binary_Member_Internal (Comp_Ptr.Binary_Member, Needs_Modification);
            if Needs_Modification then
               New_Blob := To_Blob (Obj_Smart_Ptr);
               Safir.Dob.Typesystem.Blob_Operations.Set_Null (New_Blob, Dose_Test.Complex_Global_Entity_Base.Binary_Member_Member_Index, 0);
            end if;

            for I in 0 .. Dose_Test.Complex_Global_Entity.Binary_Array_Member_Array_Size - 1 loop
               Check_Binary_Member_Internal (Comp_Ptr.Binary_Array_Member.Element (I), Needs_Modification);
               if Needs_Modification then
                  if New_Blob = null then
                     New_Blob := To_Blob (Obj_Smart_Ptr);
                  end if;
                  Safir.Dob.Typesystem.Blob_Operations.Set_Null (New_Blob, Dose_Test.Complex_Global_Entity.Binary_Array_Member_Member_Index, I);
               end if;
            end loop;

            if New_Blob /= null then
               declare
                  Xml : Unbounded_Wide_String;
               begin
                  Xml := Safir.Dob.Typesystem.Serialization.To_Xml (New_Blob);
                  C_Free (New_Blob);
                  return Xml;
               end;
            end if;

         end;
      elsif Obj_Ptr.Get_Type_Id = Dose_Test.Complex_Global_Service.Class_Type_Id then
         declare
            Comp_Ptr : constant Dose_Test.Complex_Global_Service.Complex_Global_Service_Class_Access :=
               Dose_Test.Complex_Global_Service.Complex_Global_Service_Class_Access (Obj_Smart_Ptr.Ref);
            New_Blob : Safir.Dob.Typesystem.Blob_T;
            Xml : Unbounded_Wide_String;
         begin
            Check_Binary_Member_Internal (Comp_Ptr.Binary_Member, Needs_Modification);
            if Needs_Modification then
               New_Blob := To_Blob (Obj_Smart_Ptr);
               Safir.Dob.Typesystem.Blob_Operations.Set_Null (New_Blob, Dose_Test.Complex_Global_Service_Base.Binary_Member_Member_Index, 0);
               Xml := Safir.Dob.Typesystem.Serialization.To_Xml (New_Blob);
               C_Free (New_Blob);
               return Xml;
            end if;
         end;
      end if;
      return Safir.Dob.Typesystem.Serialization.To_Xml (Blob);
   end Check_Binary_Member;


   -- Timestamp
   overriding
   procedure On_Response
     (Self            : in out Timestamp_Requestor_T;
      Response_Proxy  : in Safir.Dob.Response_Proxies.Response_Proxy) is
      pragma Unreferenced (Self, Response_Proxy);
   begin
      null;
   end On_Response;

   overriding
   procedure On_Not_Request_Overflow
     (Self : in out Timestamp_Requestor_T) is
      pragma Unreferenced (Self);
   begin
      null;
   end On_Not_Request_Overflow;

   procedure Get_Timestamp (Self       : in out  Consumer;
                            Action_Ptr : in Dose_Test.Action.Action_Class_Access;
                            Timestamp  : out Safir.Dob.Typesystem.Int_64) is

      Entity_Id : constant Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type :=
                    Safir.Dob.Typesystem.Entity_Id.Create_Entity_Id
                      (Dose_Test.Last_Injection_Timestamp.Class_Type_Id,
                       Safir.Dob.Typesystem.Instance_Id.Create_Instance_Id
                         (Dose_Test.Last_Injection_Timestamp.Class_Type_Id));

      Delta_Time : constant Safir.Dob.Typesystem.Int_64 := Action_Ptr.Timestamp_Delta.Get_Val;
      EP : constant Safir.Dob.Entity_Proxies.Entity_Proxy :=
                       Self.Connection.Read (Entity_Id);

      Ent : constant Dose_Test.Last_Injection_Timestamp.Smart_Pointer :=
         Dose_Test.Last_Injection_Timestamp.Smart_Pointer (EP.Get_Entity);

      New_Val : constant Safir.Dob.Typesystem.Int_64 :=
                  Ent.Ref.Timestamp.Get_Val + Delta_Time;
      Dummy_Req_Id : Safir.Dob.Defs.Request_Id;
      pragma Unreferenced (Dummy_Req_Id);
   begin
      Ent.Ref.Timestamp.Set_Val (New_Val);

      Dummy_Req_Id := Self.Connection.Update_Request
        (Ent,
         Safir.Dob.Typesystem.Entity_Id.Get_Instance_Id (Entity_Id),
         Self.Timestamp_Requestor'Access);

      Timestamp := New_Val;

   end Get_Timestamp;


   function Create (Instance       : in Natural;
                    Connection_Name : in Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
                    Instance_String : in Ada.Strings.Wide_Unbounded.Unbounded_Wide_String)
                    return not null Consumer_Access is
      The_Consumer : constant Consumer_Access := new Consumer;
   begin
      The_Consumer.Consumer_Number := Instance;
      The_Consumer.Connection.Attach (Connection_Name, Instance_String);
      return The_Consumer;
   end Create;

   procedure Destroy (Cons : in out Consumer_Access) is
      procedure Free is new
        Ada.Unchecked_Deallocation (Object => Consumer,
                                    Name   => Consumer_Access);
   begin
      Free (Cons);
   end Destroy;

   function Callback_Id (Self : in Consumer) return Unbounded_Wide_String is
      Connection_Aspect_Misc : constant Safir.Dob.Connection_Aspect_Miscs.Connection_Aspect_Misc :=
                                 Safir.Dob.Connection_Aspect_Miscs.Create (Self.Connection);
      CB                     : constant Safir.Dob.Callback_Id.Enumeration :=
                                 Connection_Aspect_Misc.Get_Current_Callback_Id;
   begin
      return Safir.Dob.Callback_Id.To_Dou_String (CB);
   end Callback_Id;

   procedure On_Message
     (Self    : in out Consumer;
      Message_Proxy : in Safir.Dob.Message_Proxies.Message_Proxy) is

      Msg : constant Dose_Test.Root_Message.Smart_Pointer'Class :=
              Dose_Test.Root_Message.Smart_Pointer'Class (Message_Proxy.Get_Message);
      Xml : Unbounded_Wide_String;
   begin
      Self.Connection.Exit_Dispatch;
      Self.Execute_Callback_Actions (Safir.Dob.Callback_Id.On_Message);

      if Need_Binary_Check (Msg) then
         Xml := Check_Binary_Member (Msg, Message_Proxy.Get_Blob);
      else
         Xml := Safir.Dob.Typesystem.Serialization.To_Xml (Message_Proxy.Get_Blob);
      end if;

      Logger.Put_Line (PREFIX & Natural'Wide_Image (Self.Consumer_Number) & ": "
                       & Callback_Id (Self) & ":");
      Logger.Put_Line (UWS ("  Type       = ") & Safir.Dob.Typesystem.Operations.Get_Name (Message_Proxy.Get_Type_Id));
      Logger.Put_Line (UWS ("  ChannelId  = ") & Safir.Dob.Typesystem.Channel_Id.To_String (Message_Proxy.Get_Channel_Id));
      Logger.Put_Line (UWS ("  Sender     = ") & Logger.To_String (Message_Proxy.Get_Sender_Connection_Info));
      Logger.Put_Line (UWS ("  ChannelId  = ") & Safir.Dob.Typesystem.Channel_Id.To_String (Message_Proxy.Get_Channel_Id_With_String_Representation));
      Logger.Put_Line (UWS ("  Message    = ") & Xml);
      Logger.New_Line;
      Logger.New_Line;

   end On_Message;

   overriding
   procedure On_Not_Message_Overflow
     (Self : in out Consumer) is
   begin
      Self.Connection.Exit_Dispatch;
      Self.Execute_Callback_Actions (Safir.Dob.Callback_Id.On_Not_Message_Overflow);

      Logger.Put_Line (PREFIX & Natural'Wide_Image (Self.Consumer_Number) & ": "
                       & Callback_Id (Self));
      Logger.New_Line;
   end On_Not_Message_Overflow;

   overriding
   procedure On_Registered
     (Self       : in out Consumer;
      Type_Id    : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type) is
   begin
      Self.Connection.Exit_Dispatch;
      Self.Execute_Callback_Actions (Safir.Dob.Callback_Id.On_Registered);

      Logger.Put_Line (PREFIX & Natural'Wide_Image (Self.Consumer_Number) & ": "
                       & Callback_Id (Self) & ":");
      Logger.Put_Line (UWS ("  Type      = ") & Safir.Dob.Typesystem.Operations.Get_Name (Type_Id));
      Logger.Put_Line (UWS ("  HandlerId = ") & Safir.Dob.Typesystem.Handler_Id.To_String (Handler_Id));
      Logger.New_Line;
   end On_Registered;

   overriding
   procedure On_Unregistered
     (Self       : in out Consumer;
      Type_Id    : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type) is
   begin
      Self.Connection.Exit_Dispatch;
      Self.Execute_Callback_Actions (Safir.Dob.Callback_Id.On_Unregistered);

      Logger.Put_Line (PREFIX & Natural'Wide_Image (Self.Consumer_Number) & ": "
                       & Callback_Id (Self) & ":");
      Logger.Put_Line (UWS ("  Type      = ") & Safir.Dob.Typesystem.Operations.Get_Name (Type_Id));
      Logger.Put_Line (UWS ("  HandlerId = ") & Safir.Dob.Typesystem.Handler_Id.To_String (Handler_Id));
      Logger.New_Line;
   end On_Unregistered;

   overriding
   procedure On_Service_Request
     (Self                  : in out Consumer;
      Service_Request_Proxy : in Safir.Dob.Service_Request_Proxies.Service_Request_Proxy;
      Response_Sender       : in Safir.Dob.Response_Senders.Response_Sender) is

      Service_Req : constant Dose_Test.Root_Service.Smart_Pointer'Class :=
                      Dose_Test.Root_Service.Smart_Pointer'Class (Service_Request_Proxy.Get_Request);
      Xml : Unbounded_Wide_String;
   begin
      Self.Connection.Exit_Dispatch;
      Self.Response_Sender := Response_Sender;
      Self.Execute_Callback_Actions (Safir.Dob.Callback_Id.On_Service_Request);

      if Need_Binary_Check (Service_Req) then
         Xml := Check_Binary_Member (Service_Req, Service_Request_Proxy.Get_Blob);
      else
         Xml := Safir.Dob.Typesystem.Serialization.To_Xml (Service_Request_Proxy.Get_Blob);
      end if;

      Logger.Put_Line (PREFIX & Natural'Wide_Image (Self.Consumer_Number) & ": "
                       & Callback_Id (Self) & ": ");
      Logger.Put_Line (UWS ("  Type       = ") & Safir.Dob.Typesystem.Operations.Get_Name (Service_Request_Proxy.Get_Type_Id));
      Logger.Put_Line (UWS ("  Sender     = ") & Logger.To_String (Service_Request_Proxy.Get_Sender_Connection_Info));
      Logger.Put_Line (UWS ("  Handler    = ") & Safir.Dob.Typesystem.Handler_Id.To_String (Service_Request_Proxy.Get_Receiving_Handler_Id));
      Logger.Put_Line (UWS ("  HandlerStr = ") & Safir.Dob.Typesystem.Handler_Id.To_String (Service_Request_Proxy.Get_Receiver_With_String_Representation));
      Logger.Put_Line (UWS ("  Request    = ") & Xml);
      Logger.New_Line;

      if not Response_Sender.Is_Done then
         declare
            Resp_Smart_Ptr : constant Dose_Test.Successful_Service.Smart_Pointer :=
                     Dose_Test.Successful_Service.Create;
            Resp_Ptr : constant Dose_Test.Successful_Service.Successful_Service_Class_Access :=
                               Resp_Smart_Ptr.Ref;
         begin
            Resp_Ptr.Info.Set_Val (To_Unbounded_Wide_String ("AutoResponse"));
            Response_Sender.Send (Resp_Smart_Ptr);
         end;
      end if;

   end On_Service_Request;

   overriding
   procedure On_Create_Request
     (Self                  : in out Consumer;
      Entity_Request_Proxy  : in Safir.Dob.Entity_Request_Proxies.Entity_Request_Proxy;
      Response_Sender       : in Safir.Dob.Response_Senders.Response_Sender) is

      use type Policy_Maps.Cursor;
      use type Safir.Dob.Instance_Id_Policy.Enumeration;

      Entity_Req : constant Dose_Test.Root_Entity.Smart_Pointer'Class :=
                     Dose_Test.Root_Entity.Smart_Pointer'Class (Entity_Request_Proxy.Get_Request);
      Xml : Unbounded_Wide_String;

      Policy_Key : Policy_Key_T;
      Cursor : Policy_Maps.Cursor;
      Policy_Val : Policy_Value_T;

      Resp : Safir.Dob.Entity_Id_Response.Smart_Pointer;
   begin
      Self.Connection.Exit_Dispatch;
      Self.Response_Sender := Response_Sender;
      Self.Execute_Callback_Actions (Safir.Dob.Callback_Id.On_Create_Request);

      if Need_Binary_Check (Entity_Req) then
         Xml := Check_Binary_Member (Entity_Req, Entity_Request_Proxy.Get_Blob);
      else
         Xml := Safir.Dob.Typesystem.Serialization.To_Xml (Entity_Request_Proxy.Get_Blob);
      end if;

      Policy_Key.Type_Id := Entity_Request_Proxy.Get_Type_Id;
      Policy_Key.Handler_Id := Entity_Request_Proxy.Get_Receiving_Handler_Id;
      Cursor := Self.Instance_Id_Policy_Map.Find (Policy_Key);

      pragma Assert (Cursor /= Policy_Maps.No_Element,
                     "Didn't find a corresponding item in Self.Instance_Id_Policy_Map!");

      Policy_Val := Policy_Maps.Element (Cursor);
      if Policy_Val.Instance_Id_Policy =
        Safir.Dob.Instance_Id_Policy.Handler_Decides_Instance_Id then

         Logger.Put_Line (PREFIX & Natural'Wide_Image (Self.Consumer_Number) & ": "
                          & Callback_Id (Self) & " (Handler decides instance id): ");
         Logger.Put_Line (UWS ("  Type       = ") & Trim (From_Utf_8 (Safir.Dob.Typesystem.Type_Id'Image (Entity_Request_Proxy.Get_Type_Id)),
                                                          Ada.Strings.Both));
         Logger.Put_Line (UWS ("  Sender     = ") & Logger.To_String (Entity_Request_Proxy.Get_Sender_Connection_Info));
         Logger.Put_Line (UWS ("  Handler    = ") & Safir.Dob.Typesystem.Handler_Id.To_String (Entity_Request_Proxy.Get_Receiving_Handler_Id));
         Logger.Put_Line (UWS ("  HandlerStr = ") & Safir.Dob.Typesystem.Handler_Id.To_String (Entity_Request_Proxy.Get_Receiver_With_String_Representation));
         Logger.Put_Line (UWS ("  Request    = ") & Xml);
         Logger.New_Line;

         Self.Connection.Set_All
           (Entity_Req,
            Safir.Dob.Typesystem.Instance_Id.Create_Instance_Id (Policy_Val.Instance),
            Entity_Request_Proxy.Get_Receiving_Handler_Id);

         Logger.Put_Line (PREFIX & Natural'Wide_Image (Self.Consumer_Number) & ": "
                          & UWS ("Handler created instance ") & From_Utf_8 (Safir.Dob.Typesystem.Int_64'Image (Policy_Val.Instance)));

         Resp := Safir.Dob.Entity_Id_Response.Create;
         Resp.Ref.Assigned.Set_Val (Safir.Dob.Typesystem.Entity_Id.Create_Entity_Id
           (Entity_Request_Proxy.Get_Type_Id,
              Safir.Dob.Typesystem.Instance_Id.Create_Instance_Id (Policy_Val.Instance)));

         Response_Sender.Send (Resp);
         Policy_Val.Instance := Policy_Val.Instance + 1;
         Self.Instance_Id_Policy_Map.Replace_Element (Cursor, Policy_Val);

      else
         Logger.Put_Line (PREFIX & Natural'Wide_Image (Self.Consumer_Number) & ": "
                          & Callback_Id (Self) & " (Requestor decides instance id): ");
         Logger.Put_Line (UWS ("  Entity     = ") & Safir.Dob.Typesystem.Entity_Id.To_String (Entity_Request_Proxy.Get_Entity_Id));
         Logger.Put_Line (UWS ("  Sender     = ") & Logger.To_String (Entity_Request_Proxy.Get_Sender_Connection_Info));
         Logger.Put_Line (UWS ("  Handler    = ") & Safir.Dob.Typesystem.Handler_Id.To_String (Entity_Request_Proxy.Get_Receiving_Handler_Id));
         Logger.Put_Line (UWS ("  HandlerStr = ") & Safir.Dob.Typesystem.Handler_Id.To_String (Entity_Request_Proxy.Get_Receiver_With_String_Representation));
         Logger.Put_Line (UWS ("  Request    = ") & Xml);
         Logger.New_Line;

         Self.Connection.Set_All
           (Entity_Req,
            Entity_Request_Proxy.Get_Instance_Id,
            Entity_Request_Proxy.Get_Receiving_Handler_Id);
      end if;


      if not Response_Sender.Is_Done then
         declare
            Resp_Smart_Ptr : constant Dose_Test.Successful_Create.Smart_Pointer :=
                     Dose_Test.Successful_Create.Create;
            Resp_Ptr : constant Dose_Test.Successful_Create.Successful_Create_Class_Access :=
                               Resp_Smart_Ptr.Ref;
         begin
            Resp_Ptr.Info.Set_Val (To_Unbounded_Wide_String ("AutoResponse"));
            Response_Sender.Send (Resp_Smart_Ptr);
         end;
      end if;

   end On_Create_Request;

   overriding
   procedure On_Update_Request
     (Self                  : in out Consumer;
      Entity_Request_Proxy : in Safir.Dob.Entity_Request_Proxies.Entity_Request_Proxy;
      Response_Sender       : in Safir.Dob.Response_Senders.Response_Sender) is

      Req : constant Dose_Test.Root_Entity.Smart_Pointer'Class :=
              Dose_Test.Root_Entity.Smart_Pointer'Class (Entity_Request_Proxy.Get_Request);
      Xml : Unbounded_Wide_String;
   begin
      Self.Connection.Exit_Dispatch;
      Self.Response_Sender := Response_Sender;
      Self.Execute_Callback_Actions (Safir.Dob.Callback_Id.On_Update_Request);

      if Need_Binary_Check (Req) then
         Xml := Check_Binary_Member (Req, Entity_Request_Proxy.Get_Blob);
      else
         Xml := Safir.Dob.Typesystem.Serialization.To_Xml (Entity_Request_Proxy.Get_Blob);
      end if;

      Logger.Put_Line (PREFIX & Natural'Wide_Image (Self.Consumer_Number) & ": "
                       & Callback_Id (Self) & ": ");
      Logger.Put_Line (UWS ("  Entity     = ") & Safir.Dob.Typesystem.Entity_Id.To_String (Entity_Request_Proxy.Get_Entity_Id));
      Logger.Put_Line (UWS ("  Sender     = ") & Logger.To_String (Entity_Request_Proxy.Get_Sender_Connection_Info));
      Logger.Put_Line (UWS ("  Handler    = ") & Safir.Dob.Typesystem.Handler_Id.To_String (Entity_Request_Proxy.Get_Receiving_Handler_Id));
      Logger.Put_Line (UWS ("  HandlerStr = ") & Safir.Dob.Typesystem.Handler_Id.To_String (Entity_Request_Proxy.Get_Receiver_With_String_Representation));
      Logger.Put_Line (UWS ("  Request    = ") & Xml);
      Logger.New_Line;

      Self.Connection.Set_Changes
        (Req,
         Entity_Request_Proxy.Get_Instance_Id,
         Entity_Request_Proxy.Get_Receiving_Handler_Id);

      if not Response_Sender.Is_Done then
         declare
            Resp_Smart_Ptr : constant Dose_Test.Successful_Update.Smart_Pointer :=
                     Dose_Test.Successful_Update.Create;
            Resp_Ptr : constant Dose_Test.Successful_Update.Successful_Update_Class_Access :=
                               Resp_Smart_Ptr.Ref;
         begin
            Resp_Ptr.Info.Set_Val (To_Unbounded_Wide_String ("AutoResponse"));
            Response_Sender.Send (Resp_Smart_Ptr);
         end;
      end if;

   end On_Update_Request;

   overriding
   procedure On_Delete_Request
     (Self                 : in out Consumer;
      Entity_Request_Proxy : in Safir.Dob.Entity_Request_Proxies.Entity_Request_Proxy;
      Response_Sender      : in Safir.Dob.Response_Senders.Response_Sender) is

   begin
      Self.Connection.Exit_Dispatch;
      Self.Response_Sender := Response_Sender;
      Self.Execute_Callback_Actions (Safir.Dob.Callback_Id.On_Delete_Request);

      Logger.Put_Line (PREFIX & Natural'Wide_Image (Self.Consumer_Number) & ": "
                       & Callback_Id (Self) & ": ");
      Logger.Put_Line (UWS ("  Entity     = ") & Safir.Dob.Typesystem.Entity_Id.To_String (Entity_Request_Proxy.Get_Entity_Id));
      Logger.Put_Line (UWS ("  Sender     = ") & Logger.To_String (Entity_Request_Proxy.Get_Sender_Connection_Info));
      Logger.Put_Line (UWS ("  Handler    = ") & Safir.Dob.Typesystem.Handler_Id.To_String (Entity_Request_Proxy.Get_Receiving_Handler_Id));
      Logger.Put_Line (UWS ("  HandlerStr = ") & Safir.Dob.Typesystem.Handler_Id.To_String (Entity_Request_Proxy.Get_Receiver_With_String_Representation));
      Logger.New_Line;

      Self.Connection.Delete
        (Entity_Request_Proxy.Get_Entity_Id,
         Entity_Request_Proxy.Get_Receiving_Handler_Id);

      if not Response_Sender.Is_Done then
         declare
            Resp_Smart_Ptr : constant Dose_Test.Successful_Delete.Smart_Pointer :=
                     Dose_Test.Successful_Delete.Create;
            Resp_Ptr : constant Dose_Test.Successful_Delete.Successful_Delete_Class_Access :=
                               Resp_Smart_Ptr.Ref;
         begin
            Resp_Ptr.Info.Set_Val (To_Unbounded_Wide_String ("AutoResponse"));
            Response_Sender.Send (Resp_Smart_Ptr);
         end;
      end if;

   end On_Delete_Request;

   overriding
   procedure On_Revoked_Registration
     (Self       : in out Consumer;
      Type_Id    : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type) is
   begin
      Self.Connection.Exit_Dispatch;
      Self.Execute_Callback_Actions (Safir.Dob.Callback_Id.On_Revoked_Registration);

      Logger.Put_Line (PREFIX & Natural'Wide_Image (Self.Consumer_Number) & ": "
                       & Callback_Id (Self) & ":");
      Logger.Put_Line (UWS ("  Type      = ") & Safir.Dob.Typesystem.Operations.Get_Name (Type_Id));
      Logger.Put_Line (UWS ("  HandlerId = ") & Safir.Dob.Typesystem.Handler_Id.To_String (Handler_Id));
      Logger.New_Line;
   end On_Revoked_Registration;

   overriding
   procedure On_Completed_Registration
     (Self       : in out Consumer;
      Type_Id    : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type) is
   begin
      Self.Connection.Exit_Dispatch;
      Self.Execute_Callback_Actions (Safir.Dob.Callback_Id.On_Completed_Registration);

      Logger.Put_Line (PREFIX & Natural'Wide_Image (Self.Consumer_Number) & ": "
                       & Callback_Id (Self) & ":");
      Logger.Put_Line (UWS ("  Type      = ") & Safir.Dob.Typesystem.Operations.Get_Name (Type_Id));
      Logger.Put_Line (UWS ("  HandlerId = ") & Safir.Dob.Typesystem.Handler_Id.To_String (Handler_Id));
      Logger.New_Line;
   end On_Completed_Registration;

   overriding
   procedure On_New_Entity
     (Self                     : in out Consumer;
      Entity_Proxy             : in Safir.Dob.Entity_Proxies.Entity_Proxy) is

      Entity : constant Safir.Dob.Entity.Smart_Pointer'Class :=
                           Safir.Dob.Entity.Smart_Pointer'Class (Entity_Proxy.Get_Entity_With_Change_Info);
      Entity_Ptr : constant Safir.Dob.Entity.Entity_Class_Access := Entity.Ref;
      Xml : Unbounded_Wide_String;
      I : Safir.Dob.Typesystem.Int_32;
   begin

      Self.Connection.Exit_Dispatch;
      Self.Execute_Callback_Actions (Safir.Dob.Callback_Id.On_New_Entity);

      if Need_Binary_Check (Entity) then
         Xml := Check_Binary_Member (Entity, Entity_Proxy.Get_Blob);
      else
         Xml := Safir.Dob.Typesystem.Serialization.To_Xml (Entity_Proxy.Get_Blob);
      end if;

      Logger.Put_Line (PREFIX & Natural'Wide_Image (Self.Consumer_Number) & ": "
                       & Callback_Id (Self) & ":");
      Logger.Put_Line (UWS ("  EntityId  = ") & Safir.Dob.Typesystem.Entity_Id.To_String (Entity_Proxy.Get_Entity_Id));
      Logger.Put_Line (UWS ("  Owner     = ") & Safir.Dob.Typesystem.Handler_Id.To_String (Entity_Proxy.Get_Owner));
      Logger.Put_Line (UWS ("  OwnerConn = ") & Logger.To_String (Entity_Proxy.Get_Owner_Connection_Info));
      Logger.Put_Line (UWS ("  OwnerStr  = ") & Safir.Dob.Typesystem.Handler_Id.To_String (Entity_Proxy.Get_Owner_With_String_Representation));
      Logger.Put_Line (UWS ("  Entity    = ") & Xml);
      Logger.Put_Line (UWS ("  Changed top-level members: "));

      I := 0;
      while I < Safir.Dob.Typesystem.Members.Get_Number_Of_Members (Entity_Proxy.Get_Type_Id) loop
         if Entity_Ptr.Get_Member (I, 0).Is_Changed then
            Logger.Put_Line (UWS ("    ") & Safir.Dob.Typesystem.Members.Get_Name (Entity_Proxy.Get_Type_Id, I));
         end if;
         I := I + 1;
      end loop;
      Logger.New_Line;
   end On_New_Entity;

   overriding
   procedure On_Updated_Entity
     (Self                     : in out Consumer;
      Entity_Proxy             : in Safir.Dob.Entity_Proxies.Entity_Proxy) is

      Entity : constant Safir.Dob.Entity.Smart_Pointer'Class :=
                           Safir.Dob.Entity.Smart_Pointer'Class (Entity_Proxy.Get_Entity_With_Change_Info);
      Entity_Ptr : constant Safir.Dob.Entity.Entity_Class_Access := Entity.Ref;
      Xml : Unbounded_Wide_String;

      Prev_Entity : constant Safir.Dob.Entity.Smart_Pointer'Class :=
                      Safir.Dob.Entity.Smart_Pointer'Class (Entity_Proxy.Get_Previous.Get_Entity_With_Change_Info);
      Prev_Xml : Unbounded_Wide_String;

      I : Safir.Dob.Typesystem.Int_32;
   begin

      Self.Connection.Exit_Dispatch;
      Self.Execute_Callback_Actions (Safir.Dob.Callback_Id.On_Updated_Entity);

      if Need_Binary_Check (Entity) then
         Xml := Check_Binary_Member (Entity, Entity_Proxy.Get_Blob);
      else
         Xml := Safir.Dob.Typesystem.Serialization.To_Xml (Entity_Proxy.Get_Blob);
      end if;

      if Need_Binary_Check (Prev_Entity) then
         Prev_Xml := Check_Binary_Member (Prev_Entity, Entity_Proxy.Get_Previous.Get_Blob);
      else
         Prev_Xml := Safir.Dob.Typesystem.Serialization.To_Xml (Entity_Proxy.Get_Previous.Get_Blob);
      end if;

      Logger.Put_Line (PREFIX & Natural'Wide_Image (Self.Consumer_Number) & ": "
                       & Callback_Id (Self) & ":");
      Logger.Put_Line (UWS ("  EntityId  = ") & Safir.Dob.Typesystem.Entity_Id.To_String (Entity_Proxy.Get_Entity_Id));
      Logger.Put_Line (UWS ("  Owner     = ") & Safir.Dob.Typesystem.Handler_Id.To_String (Entity_Proxy.Get_Owner));
      Logger.Put_Line (UWS ("  OwnerConn = ") & Logger.To_String (Entity_Proxy.Get_Owner_Connection_Info));
      Logger.Put_Line (UWS ("  OwnerStr  = ") & Safir.Dob.Typesystem.Handler_Id.To_String (Entity_Proxy.Get_Owner_With_String_Representation));
      Logger.Put_Line (UWS ("  Entity    = ") & Xml);
      Logger.Put_Line (UWS ("  Previous  = ") & Prev_Xml);
      Logger.Put_Line (UWS ("  Changed top-level members: "));

      I := 0;
      while I < Safir.Dob.Typesystem.Members.Get_Number_Of_Members (Entity_Proxy.Get_Type_Id) loop
         if Entity_Ptr.Get_Member (I, 0).Is_Changed then
            Logger.Put_Line (UWS ("    ") & Safir.Dob.Typesystem.Members.Get_Name (Entity_Proxy.Get_Type_Id, I));
         end if;
         I := I + 1;
      end loop;
      Logger.New_Line;
   end On_Updated_Entity;

   overriding
   procedure On_Deleted_Entity
     (Self                     : in out Consumer;
      Entity_Proxy             : in Safir.Dob.Entity_Proxies.Entity_Proxy;
      Deleted_By_Owner         : in Boolean) is

      Prev_Entity : constant Safir.Dob.Entity.Smart_Pointer'Class :=
                      Safir.Dob.Entity.Smart_Pointer'Class (Entity_Proxy.Get_Previous.Get_Entity);
      Prev_Xml : Unbounded_Wide_String;
   begin

      Self.Connection.Exit_Dispatch;
      Self.Execute_Callback_Actions (Safir.Dob.Callback_Id.On_Deleted_Entity);

      if Need_Binary_Check (Prev_Entity) then
         Prev_Xml := Check_Binary_Member (Prev_Entity, Entity_Proxy.Get_Previous.Get_Blob);
      else
         Prev_Xml := Safir.Dob.Typesystem.Serialization.To_Xml (Entity_Proxy.Get_Previous.Get_Blob);
      end if;

      Logger.Put_Line (PREFIX & Natural'Wide_Image (Self.Consumer_Number) & ": "
                       & Callback_Id (Self) & ":");
      Logger.Put_Line (UWS ("  EntityId       = ") & Safir.Dob.Typesystem.Entity_Id.To_String (Entity_Proxy.Get_Entity_Id));
      Logger.Put_Line (UWS ("  deletedByOwner = ") & Boolean_Image (Deleted_By_Owner));
      Logger.Put_Line (UWS ("  Owner          = ") & Safir.Dob.Typesystem.Handler_Id.To_String (Entity_Proxy.Get_Owner));
      Logger.Put_Line (UWS ("  OwnerConn = ") & Logger.To_String (Entity_Proxy.Get_Owner_Connection_Info));
      Logger.Put_Line (UWS ("  OwnerStr  = ") & Safir.Dob.Typesystem.Handler_Id.To_String (Entity_Proxy.Get_Owner_With_String_Representation));
      Logger.Put_Line (UWS ("  Previous  = ") & Prev_Xml);

      Logger.New_Line;
   end On_Deleted_Entity;

   overriding
   procedure On_Injected_New_Entity
     (Self                  : in out Consumer;
      Injected_Entity_Proxy : in Safir.Dob.Injected_Entity_Proxies.Injected_Entity_Proxy) is

      Entity : constant Safir.Dob.Entity.Smart_Pointer'Class :=
                           Safir.Dob.Entity.Smart_Pointer'Class (Injected_Entity_Proxy.Get_Injection);
      Entity_Ptr : constant Safir.Dob.Entity.Entity_Class_Access := Entity.Ref;
      Xml : Unbounded_Wide_String;
      I : Safir.Dob.Typesystem.Int_32;
   begin

      Self.Connection.Exit_Dispatch;
      Self.Execute_Callback_Actions (Safir.Dob.Callback_Id.On_Injected_New_Entity);

      Xml := Safir.Dob.Typesystem.Serialization.To_Xml (Injected_Entity_Proxy.Get_Injection_Blob);

      Logger.Put_Line (PREFIX & Natural'Wide_Image (Self.Consumer_Number) & ": "
                       & Callback_Id (Self) & ":");
      Logger.Put_Line (UWS ("  EntityId  = ") & Safir.Dob.Typesystem.Entity_Id.To_String (Injected_Entity_Proxy.Get_Entity_Id));
      Logger.Put_Line (UWS ("  Injection = ") & Xml);
      Logger.Put_Line (UWS ("  Changed top-level members: "));

      I := 0;
      while I < Safir.Dob.Typesystem.Members.Get_Number_Of_Members (Injected_Entity_Proxy.Get_Type_Id) loop
         if Entity_Ptr.Get_Member (I, 0).Is_Changed then
            Logger.Put_Line (UWS ("    ") & Safir.Dob.Typesystem.Members.Get_Name (Injected_Entity_Proxy.Get_Type_Id, I));
         end if;
         I := I + 1;
      end loop;
      Logger.New_Line;
   end On_Injected_New_Entity;

   overriding
   procedure On_Injected_Updated_Entity
     (Self                  : in out Consumer;
      Injected_Entity_Proxy : in Safir.Dob.Injected_Entity_Proxies.Injected_Entity_Proxy) is

      Entity : constant Safir.Dob.Entity.Smart_Pointer'Class :=
                           Safir.Dob.Entity.Smart_Pointer'Class (Injected_Entity_Proxy.Get_Injection);
      Entity_Ptr : constant Safir.Dob.Entity.Entity_Class_Access := Entity.Ref;
      Injection_Xml : Unbounded_Wide_String;
      Current_Xml : Unbounded_Wide_String;
      I : Safir.Dob.Typesystem.Int_32;
   begin

      Self.Connection.Exit_Dispatch;
      Self.Execute_Callback_Actions (Safir.Dob.Callback_Id.On_Injected_Updated_Entity);

      Injection_Xml := Safir.Dob.Typesystem.Serialization.To_Xml (Injected_Entity_Proxy.Get_Injection_Blob);
      Current_Xml := Safir.Dob.Typesystem.Serialization.To_Xml (Injected_Entity_Proxy.Get_Current);

      Logger.Put_Line (PREFIX & Natural'Wide_Image (Self.Consumer_Number) & ": "
                       & Callback_Id (Self) & ":");
      Logger.Put_Line (UWS ("  EntityId  = ") & Safir.Dob.Typesystem.Entity_Id.To_String (Injected_Entity_Proxy.Get_Entity_Id));
      Logger.Put_Line (UWS ("  Injection = ") & Injection_Xml);
      Logger.Put_Line (UWS ("  Current   = ") & Current_Xml);
      Logger.Put_Line (UWS ("  Changed top-level members: "));

      I := 0;
      while I < Safir.Dob.Typesystem.Members.Get_Number_Of_Members (Injected_Entity_Proxy.Get_Type_Id) loop
         if Entity_Ptr.Get_Member (I, 0).Is_Changed then
            Logger.Put_Line (UWS ("    ") & Safir.Dob.Typesystem.Members.Get_Name (Injected_Entity_Proxy.Get_Type_Id, I));
         end if;
         I := I + 1;
      end loop;
      Logger.New_Line;
   end On_Injected_Updated_Entity;

   overriding
   procedure On_Injected_Deleted_Entity
     (Self                  : in out Consumer;
      Injected_Entity_Proxy : in Safir.Dob.Injected_Entity_Proxies.Injected_Entity_Proxy) is

      Xml : Unbounded_Wide_String;
   begin

      Self.Connection.Exit_Dispatch;
      Self.Execute_Callback_Actions (Safir.Dob.Callback_Id.On_Injected_Deleted_Entity);

      Xml := Safir.Dob.Typesystem.Serialization.To_Xml (Injected_Entity_Proxy.Get_Current);

      Logger.Put_Line (PREFIX & Natural'Wide_Image (Self.Consumer_Number) & ": "
                       & Callback_Id (Self) & ":");
      Logger.Put_Line (UWS ("  EntityId       = ") & Safir.Dob.Typesystem.Entity_Id.To_String (Injected_Entity_Proxy.Get_Entity_Id));
      Logger.Put_Line (UWS ("  Current  = ") & Xml);
      Logger.New_Line;
   end On_Injected_Deleted_Entity;

   overriding
   procedure On_Initial_Injections_Done
     (Self       : in out Consumer;
      Type_Id    : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type) is
   begin
      Self.Connection.Exit_Dispatch;
      Self.Execute_Callback_Actions (Safir.Dob.Callback_Id.On_Initial_Injections_Done);

      Logger.Put_Line (PREFIX & Natural'Wide_Image (Self.Consumer_Number) & ": "
                       & Callback_Id (Self) & ":");
      Logger.Put_Line (UWS ("  Type      = ") & Safir.Dob.Typesystem.Operations.Get_Name (Type_Id));
      Logger.Put_Line (UWS ("  HandlerId = ") & Safir.Dob.Typesystem.Handler_Id.To_String (Handler_Id));
      Logger.New_Line;
   end On_Initial_Injections_Done;

   overriding
   procedure On_Response
     (Self            : in out Consumer;
      Response_Proxy  : in Safir.Dob.Response_Proxies.Response_Proxy) is

      Xml : Unbounded_Wide_String;
   begin
      Self.Connection.Exit_Dispatch;
      Self.Execute_Callback_Actions (Safir.Dob.Callback_Id.On_Response);

      Xml := Safir.Dob.Typesystem.Serialization.To_Xml (Response_Proxy.Get_Blob);

      Logger.Put_Line (PREFIX & Natural'Wide_Image (Self.Consumer_Number) & ": "
                       & Callback_Id (Self) & ":");
      Logger.Put_Line (UWS ("  Type       = ") & Safir.Dob.Typesystem.Operations.Get_Name (Response_Proxy.Get_Type_Id));
      Logger.Put_Line (UWS ("  IsSuccess  = ") & Boolean_Image (Response_Proxy.Is_Success));
      Logger.Put_Line (UWS ("  Sender     = ") & Logger.To_String (Response_Proxy.Get_Response_Sender_Connection_Info));
      Logger.Put_Line (UWS ("  Response   = ") & Xml);
      Logger.Put (UWS ("  Request    = "));

      declare
         Eid : Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type;
      begin
         declare
            Req : constant Safir.Dob.Typesystem.Object.Smart_Pointer'Class :=
                    Safir.Dob.Typesystem.Object.Smart_Pointer'Class (Response_Proxy.Get_Request);
         begin
            if Need_Binary_Check (Req) then
               Logger.Put_Line (Check_Binary_Member (Req, Response_Proxy.Get_Request_Blob));
            else
               Logger.Put_Line (Safir.Dob.Typesystem.Serialization.To_Xml (Response_Proxy.Get_Request_Blob));
            end if;
         end;
      exception
         when Safir.Dob.Typesystem.Software_Violation_Exception =>
            Eid := Safir.Dob.Typesystem.Entity_Id.Create_Entity_Id
              (Response_Proxy.Get_Request_Type_Id, Response_Proxy.Get_Request_Instance_Id);
            Logger.Put_Line (UWS ("DeleteRequest on ") & Safir.Dob.Typesystem.Entity_Id.To_String (Eid));
      end;
      Logger.New_Line;
   end On_Response;

   overriding
   procedure On_Not_Request_Overflow
     (Self : in out Consumer) is
   begin
      Self.Connection.Exit_Dispatch;
      Self.Execute_Callback_Actions (Safir.Dob.Callback_Id.On_Not_Request_Overflow);
      Logger.Put_Line (PREFIX & Natural'Wide_Image (Self.Consumer_Number) & ": "
                       & Callback_Id (Self));
   end On_Not_Request_Overflow;

   function "=" (Left, Right : in Dose_Test.Action.Smart_Pointer) return Boolean is
      pragma Unreferenced (Left, Right);
   begin
      return False;
   end "=";


   procedure Add_Callback_Action
     (Self   : in out Consumer;
      Action : in Dose_Test.Action.Smart_Pointer) is
   begin
      Self.Callback_Actions (Action.Ref.Action_Callback.Get_Val).Append (Action);
   end Add_Callback_Action;

   procedure Execute_Callback_Actions
     (Self     : in out Consumer;
      Callback : in Safir.Dob.Callback_Id.Enumeration) is

      procedure Execute (Position : in Action_Vectors.Cursor) is
      begin
         Execute_Action (Self, Action_Vectors.Element (Position));
      end Execute;

   begin
      Self.Callback_Actions (Callback).Iterate (Execute'Access);
   end Execute_Callback_Actions;


   procedure Execute_Action
     (Self   : in out Consumer;
      Action : in Dose_Test.Action.Smart_Pointer) is

      Action_Ptr : constant Dose_Test.Action.Action_Class_Access :=
                   Action.Ref;
      --  only becomes true if RepeatUntilOverflow is true
      Repeat : Boolean := not Action_Ptr.Repeat_Until_Overflow.Is_Null and then
      Action_Ptr.Repeat_Until_Overflow.Get_Val;
      Repeats : Natural := 0;
   begin
      loop
         begin
            case Action_Ptr.Action_Kind.Get_Val is
               when Dose_Test.Action_Enum.Send_Response =>
                  declare
                     Response : constant Safir.Dob.Response.Smart_Pointer'Class :=
                                  Safir.Dob.Response.Smart_Pointer'Class (Action_Ptr.Object.Get_Ptr);

                  begin
                     Self.Response_Sender.Send (Response);
                  end;

               when Dose_Test.Action_Enum.Discard_Response_Sender =>
                  Self.Response_Sender.Discard;

               when Dose_Test.Action_Enum.Register_Entity_Handler =>
                  declare
                     Policy_Key   : Policy_Key_T;
                     Policy_Value : Policy_Value_T;
                  begin
                     Self.Connection.Register_Entity_Handler (Action_Ptr.Type_Id.Get_Val,
                                                              Action_Ptr.Handler.Get_Val,
                                                              Action_Ptr.Instance_Id_Policy.Get_Val,
                                                              Self'Access);
                     -- Save instance id policy
                     Policy_Key.Type_Id := Action_Ptr.Type_Id.Get_Val;
                     Policy_Key.Handler_Id := Action_Ptr.Handler.Get_Val;

                     Policy_Value.Instance_Id_Policy := Action_Ptr.Instance_Id_Policy.Get_Val;
                     Policy_Value.Instance := Safir.Dob.Typesystem.Handler_Id.Get_Raw_Value (Action_Ptr.Handler.Get_Val);

                     -- First remove the node if it already exists
                     Self.Instance_Id_Policy_Map.Exclude (Policy_Key);
                     Self.Instance_Id_Policy_Map.Insert (Policy_Key, Policy_Value);
                  end;

               when Dose_Test.Action_Enum.Register_Entity_Handler_Injection =>
                  declare
                     Policy_Key   : Policy_Key_T;
                     Policy_Value : Policy_Value_T;
                  begin
                     Self.Connection.Register_Entity_Handler_Injection
                       (Action_Ptr.Type_Id.Get_Val,
                        Action_Ptr.Handler.Get_Val,
                        Action_Ptr.Instance_Id_Policy.Get_Val,
                        Self'Access);

                     -- Save instance id policy
                     Policy_Key.Type_Id := Action_Ptr.Type_Id.Get_Val;
                     Policy_Key.Handler_Id := Action_Ptr.Handler.Get_Val;

                     Policy_Value.Instance_Id_Policy := Action_Ptr.Instance_Id_Policy.Get_Val;
                     Policy_Value.Instance := Safir.Dob.Typesystem.Handler_Id.Get_Raw_Value (Action_Ptr.Handler.Get_Val);

                     -- First remove the node if it already exists
                     Self.Instance_Id_Policy_Map.Exclude (Policy_Key);
                     Self.Instance_Id_Policy_Map.Insert (Policy_Key, Policy_Value);
                  end;

               when Dose_Test.Action_Enum.Register_Entity_Handler_Pending =>
                  declare
                     Policy_Key   : Policy_Key_T;
                     Policy_Value : Policy_Value_T;
                  begin
                     Self.Connection.Register_Entity_Handler_Pending
                       (Action_Ptr.Type_Id.Get_Val,
                        Action_Ptr.Handler.Get_Val,
                        Action_Ptr.Instance_Id_Policy.Get_Val,
                        Self'Access);

                     -- Save instance id policy
                     Policy_Key.Type_Id := Action_Ptr.Type_Id.Get_Val;
                     Policy_Key.Handler_Id := Action_Ptr.Handler.Get_Val;

                     Policy_Value.Instance_Id_Policy := Action_Ptr.Instance_Id_Policy.Get_Val;
                     Policy_Value.Instance := Safir.Dob.Typesystem.Handler_Id.Get_Raw_Value (Action_Ptr.Handler.Get_Val);

                     -- First remove the node if it already exists
                     Self.Instance_Id_Policy_Map.Exclude (Policy_Key);
                     Self.Instance_Id_Policy_Map.Insert (Policy_Key, Policy_Value);
                  end;

               when Dose_Test.Action_Enum.Register_Service_Handler =>
                  Self.Connection.Register_Service_Handler
                       (Action_Ptr.Type_Id.Get_Val,
                        Action_Ptr.Handler.Get_Val,
                        Self'Access);

               when Dose_Test.Action_Enum.Register_Service_Handler_Pending =>
                  Self.Connection.Register_Service_Handler_Pending
                       (Action_Ptr.Type_Id.Get_Val,
                        Action_Ptr.Handler.Get_Val,
                        Self'Access);

               when Dose_Test.Action_Enum.Unregister_Handler =>
                  Self.Connection.Unregister_Handler (Action_Ptr.Type_Id.Get_Val,
                                                      Action_Ptr.Handler.Get_Val);

               when Dose_Test.Action_Enum.Subscribe_Message =>
                  Self.Connection.Subscribe_Message (Action_Ptr.Type_Id.Get_Val,
                                                     Action_Ptr.Channel.Get_Val,
                                                     Action_Ptr.Include_Subclasses.Get_Val,
                                                     Self'Access);

               when Dose_Test.Action_Enum.Unsubscribe_Message =>
                  Self.Connection.Unsubscribe_Message (Action_Ptr.Type_Id.Get_Val,
                                                       Action_Ptr.Channel.Get_Val,
                                                       Action_Ptr.Include_Subclasses.Get_Val,
                                                       Self'Access);

               when Dose_Test.Action_Enum.Subscribe_Entity =>
                  if not Action_Ptr.Type_Id.Is_Null then
                     Self.Connection.Subscribe_Entity (Action_Ptr.Type_Id.Get_Val,
                                                       Action_Ptr.Include_Updates.Get_Val,
                                                       Action_Ptr.Include_Subclasses.Get_Val,
                                                       Action_Ptr.Restart_Subscription.Get_Val,
                                                       Self'Access);
                  else
                     Self.Connection.Subscribe_Entity (Action_Ptr.Entity_Id.Get_Val,
                                                       Action_Ptr.Include_Updates.Get_Val,
                                                       Action_Ptr.Restart_Subscription.Get_Val,
                                                       Self'Access);
                  end if;

               when Dose_Test.Action_Enum.Injector_Subscribe_Entity =>
                  declare
                     Connection_Aspect_Injector : constant Safir.Dob.Connection_Aspect_Injectors.Connection_Aspect_Injector :=
                                                Safir.Dob.Connection_Aspect_Injectors.Create (Self.Connection);
                  begin
                     Connection_Aspect_Injector.Subscribe_Entity
                       (Action_Ptr.Type_Id.Get_Val,
                        Action_Ptr.Include_Updates.Get_Val,
                        Action_Ptr.Include_Subclasses.Get_Val,
                        Action_Ptr.Restart_Subscription.Get_Val,
                        Action_Ptr.Wants_Ghost_Delete.Get_Val,
                        Action_Ptr.Wants_Last_State.Get_Val,
                        Action_Ptr.Doesnt_Want_Source_Is_Permanent_Store.Get_Val,
                        Action_Ptr.Wants_All_State_Changes.Get_Val,
                        Action_Ptr.Timestamp_Change_Info.Get_Val,
                        Self'Access);
                  end;

               when Dose_Test.Action_Enum.Unsubscribe_Entity =>
                  if Action_Ptr.Type_Id.Is_Null then
                     Self.Connection.Unsubscribe_Entity
                       (Action_Ptr.Entity_Id.Get_Val,
                        Self'Access);
                  else
                     Self.Connection.Unsubscribe_Entity
                       (Action_Ptr.Type_Id.Get_Val,
                        Action_Ptr.Include_Subclasses.Get_Val,
                        Self'Access);
                  end if;


               when Dose_Test.Action_Enum.Subscribe_Registration =>
                  Self.Connection.Subscribe_Registration (Action_Ptr.Type_Id.Get_Val,
                                                          Action_Ptr.Handler.Get_Val,
                                                          Action_Ptr.Include_Subclasses.Get_Val,
                                                          Action_Ptr.Restart_Subscription.Get_Val,
                                                          Self'Access);

               when Dose_Test.Action_Enum.Unsubscribe_Registration =>
                  Self.Connection.Unsubscribe_Registration (Action_Ptr.Type_Id.Get_Val,
                                                            Action_Ptr.Handler.Get_Val,
                                                            Action_Ptr.Include_Subclasses.Get_Val,
                                                            Self'Access);

               when Dose_Test.Action_Enum.Send_Message =>
                  Self.Connection.Send (Safir.Dob.Message.Smart_Pointer'Class (Action_Ptr.Object.Get_Ptr),
                                        Action_Ptr.Channel.Get_Val,
                                        Self'Access);

               when Dose_Test.Action_Enum.Service_Request =>
                  Self.Latest_Request_Id := Self.Connection.Service_Request
                    (Safir.Dob.Service.Smart_Pointer'Class (Action_Ptr.Object.Get_Ptr),
                     Action_Ptr.Handler.Get_Val,
                     Self'Access);

               when Dose_Test.Action_Enum.Create_Request =>
                  if not Action_Ptr.Instance.Is_Null then
                     Self.Latest_Request_Id := Self.Connection.Create_Request
                       (Safir.Dob.Entity.Smart_Pointer'Class (Action_Ptr.Object.Get_Ptr),
                        Action_Ptr.Instance.Get_Val,
                        Action_Ptr.Handler.Get_Val,
                        Self'Access);
                  else
                     Self.Latest_Request_Id := Self.Connection.Create_Request
                       (Safir.Dob.Entity.Smart_Pointer'Class (Action_Ptr.Object.Get_Ptr),
                        Action_Ptr.Handler.Get_Val,
                        Self'Access);
                  end if;

               when Dose_Test.Action_Enum.Update_Request =>
                  Self.Latest_Request_Id := Self.Connection.Update_Request
                    (Safir.Dob.Entity.Smart_Pointer'Class (Action_Ptr.Object.Get_Ptr),
                     Action_Ptr.Instance.Get_Val,
                     Self'Access);

               when Dose_Test.Action_Enum.Delete_Request =>
                  Self.Latest_Request_Id := Self.Connection.Delete_Request
                    (Action_Ptr.Entity_Id.Get_Val,
                     Self'Access);

               when Dose_Test.Action_Enum.Set_All =>
                  Self.Connection.Set_All
                    (Safir.Dob.Entity.Smart_Pointer'Class (Action_Ptr.Object.Get_Ptr),
                     Action_Ptr.Instance.Get_Val,
                     Action_Ptr.Handler.Get_Val);

               when Dose_Test.Action_Enum.Initial_Set =>
                  declare
                     Connection_Aspect_Injector : constant Safir.Dob.Connection_Aspect_Injectors.Connection_Aspect_Injector :=
                                                    Safir.Dob.Connection_Aspect_Injectors.Create (Self.Connection);
                  begin
                     Connection_Aspect_Injector.Initial_Set
                       (Safir.Dob.Entity.Smart_Pointer'Class (Action_Ptr.Object.Get_Ptr),
                        Action_Ptr.Instance.Get_Val,
                        Action_Ptr.Handler.Get_Val);
                  end;

               when Dose_Test.Action_Enum.Set_Changes =>
                  Self.Connection.Set_Changes
                    (Safir.Dob.Entity.Smart_Pointer'Class (Action_Ptr.Object.Get_Ptr),
                     Action_Ptr.Instance.Get_Val,
                     Action_Ptr.Handler.Get_Val);

               when Dose_Test.Action_Enum.Inject_Changes =>
                  declare
                     Connection_Aspect_Injector : constant Safir.Dob.Connection_Aspect_Injectors.Connection_Aspect_Injector :=
                                                    Safir.Dob.Connection_Aspect_Injectors.Create (Self.Connection);
                     Timestamp : Safir.Dob.Typesystem.Int_64;
                  begin
                     Get_Timestamp (Self, Action_Ptr, Timestamp);

                     Connection_Aspect_Injector.Inject_Changes
                       (Safir.Dob.Entity.Smart_Pointer'Class (Action_Ptr.Object.Get_Ptr),
                        Action_Ptr.Instance.Get_Val,
                        Timestamp,
                        Action_Ptr.Handler.Get_Val);
                  end;

               when Dose_Test.Action_Enum.Delete =>
                  Self.Connection.Delete
                    (Action_Ptr.Entity_Id.Get_Val,
                     Action_Ptr.Handler.Get_Val);

               when Dose_Test.Action_Enum.Inject_Delete =>
                  declare
                     Connection_Aspect_Injector : constant Safir.Dob.Connection_Aspect_Injectors.Connection_Aspect_Injector :=
                                                    Safir.Dob.Connection_Aspect_Injectors.Create (Self.Connection);
                     Timestamp : Safir.Dob.Typesystem.Int_64;
                  begin
                     Get_Timestamp (Self, Action_Ptr, Timestamp);

                     Connection_Aspect_Injector.Inject_Delete
                       (Action_Ptr.Entity_Id.Get_Val,
                        Timestamp,
                        Action_Ptr.Handler.Get_Val);
                  end;

               when Dose_Test.Action_Enum.Postpone =>
                  declare
                     Connection_Aspect_Postpone : constant Safir.Dob.Connection_Aspect_Postpones.Connection_Aspect_Postpone :=
                                                    Safir.Dob.Connection_Aspect_Postpones.Create (Self.Connection);
                  begin
                     Connection_Aspect_Postpone.Postpone
                       (Action_Ptr.Redispatch_Current.Get_Val);
                  end;

               when Dose_Test.Action_Enum.Resume_Postponed =>
                  declare
                     Connection_Aspect_Postpone : constant Safir.Dob.Connection_Aspect_Postpones.Connection_Aspect_Postpone :=
                                                    Safir.Dob.Connection_Aspect_Postpones.Create (Self.Connection);
                  begin
                     Connection_Aspect_Postpone.Resume_Postponed;
                  end;

               when Dose_Test.Action_Enum.Incomplete_Injection_State =>
                  declare
                     Connection_Aspect_Postpone : constant Safir.Dob.Connection_Aspect_Postpones.Connection_Aspect_Postpone :=
                                                    Safir.Dob.Connection_Aspect_Postpones.Create (Self.Connection);
                  begin
                     Connection_Aspect_Postpone.Incomplete_Injection_State;
                  end;

               when Dose_Test.Action_Enum.Delete_All_Instances =>
                  Self.Connection.Delete_All_Instances (Action_Ptr.Type_Id.Get_Val,
                                                        Action_Ptr.Handler.Get_Val);

               when Dose_Test.Action_Enum.Get_Entity_Iterator =>
                  declare
                     use Safir.Dob.Entity_Iterators;
                     It : Safir.Dob.Entity_Iterators.Entity_Iterator;
                     Entity_Proxy : Safir.Dob.Entity_Proxies.Entity_Proxy;
                  begin
                     Logger.Put_Line (PREFIX & Natural'Wide_Image (Self.Consumer_Number) & ": "
                                      & "Iterating over entities of type "
                                      & Safir.Dob.Typesystem.Operations.Get_Name (Action_Ptr.Type_Id.Get_Val)
                                      & ":");

                     It := Self.Connection.Get_Entity_Iterator
                       (Action_Ptr.Type_Id.Get_Val,
                        Action_Ptr.Include_Subclasses.Get_Val);

                     while not Done (It) loop
                        Entity_Proxy := Get_Entity_Proxy (It);
                        Logger.Put_Line (UWS ("  EntityId  = ") & Safir.Dob.Typesystem.Entity_Id.To_String (Entity_Proxy.Get_Entity_Id) & UWS (":"));
                        Logger.Put_Line (UWS ("     Owner     = ") & Safir.Dob.Typesystem.Handler_Id.To_String (Entity_Proxy.Get_Owner));
                        Logger.Put_Line (UWS ("     OwnerConn = ") & Logger.To_String (Entity_Proxy.Get_Owner_Connection_Info));
                        Logger.Put_Line (UWS ("     OwnerStr  = ") & Safir.Dob.Typesystem.Handler_Id.To_String (Entity_Proxy.Get_Owner_With_String_Representation));
                        Logger.Put_Line (UWS ("     Entity    = ") & Safir.Dob.Typesystem.Serialization.To_Xml (Entity_Proxy.Get_Blob));

                        Next (It);
                     end loop;
                     Logger.New_Line;
                  exception
                     when E : others =>
                        Logger.Put ("An exc!!!!: ");
                        Logger.Put_Line (From_Utf_8 (Ada.Exceptions.Exception_Name (E)));
                  end;

               when Dose_Test.Action_Enum.Read =>
                  declare
                     Entity_Proxy : Safir.Dob.Entity_Proxies.Entity_Proxy;
                  begin
                     Logger.Put_Line (PREFIX & Natural'Wide_Image (Self.Consumer_Number) & ": "
                                      & "Read entity "
                                      & Safir.Dob.Typesystem.Entity_Id.To_String (Action_Ptr.Entity_Id.Get_Val)
                                      & ":");

                     Entity_Proxy := Self.Connection.Read (Action_Ptr.Entity_Id.Get_Val);

                     Logger.Put_Line (UWS ("  EntityId  = ") & Safir.Dob.Typesystem.Entity_Id.To_String (Entity_Proxy.Get_Entity_Id) & UWS (":"));
                     Logger.Put_Line (UWS ("  Owner     = ") & Safir.Dob.Typesystem.Handler_Id.To_String (Entity_Proxy.Get_Owner));
                     Logger.Put_Line (UWS ("  OwnerConn = ") & Logger.To_String (Entity_Proxy.Get_Owner_Connection_Info));
                     Logger.Put_Line (UWS ("  OwnerStr  = ") & Safir.Dob.Typesystem.Handler_Id.To_String (Entity_Proxy.Get_Owner_With_String_Representation));
                     Logger.Put_Line (UWS ("  Entity    = ") & Safir.Dob.Typesystem.Serialization.To_Xml (Entity_Proxy.Get_Blob));
                     Logger.New_Line;
                  end;


               when Dose_Test.Action_Enum.Simulate_Overflows =>
                  Logger.Put_Line (PREFIX & Natural'Wide_Image (Self.Consumer_Number) & UWS (": ") &
                                   UWS ("SimulateOverflows(") &
                                   Boolean_Image (Action_Ptr.In_Queues.Get_Val) & ", " &
                                   Boolean_Image (Action_Ptr.Out_Queues.Get_Val) & ")");
                  Safir.Dob.Connection_Aspect_Miscs.Create (Self.Connection).
                    Simulate_Overflows (Action_Ptr.In_Queues.Get_Val,
                                        Action_Ptr.Out_Queues.Get_Val);

               when Dose_Test.Action_Enum.Is_Created =>
                  Logger.Put (PREFIX & Natural'Wide_Image (Self.Consumer_Number) & ": "
                              & "The instance "
                              & Safir.Dob.Typesystem.Entity_Id.To_String (Action_Ptr.Entity_Id.Get_Val)
                              & " is ");
                  if not Self.Connection.Is_Created (Action_Ptr.Entity_Id.Get_Val) then
                     Logger.Put ("not ");
                  end if;
                  Logger.Put_Line ("created.");

               when Dose_Test.Action_Enum.Get_Number_Of_Instances =>
                  Logger.Put_Line (PREFIX & Natural'Wide_Image (Self.Consumer_Number) & ": "
                    & "GetNumberOfInstances (type = "
                    & Safir.Dob.Typesystem.Operations.Get_Name (Action_Ptr.Type_Id.Get_Val)
                    & ", handler = " & Safir.Dob.Typesystem.Handler_Id.To_String (Action_Ptr.Handler.Get_Val)
                    & ", includeSubclasses = " & Boolean_Image (Action_Ptr.Include_Subclasses.Get_Val)
                    & "): "
                    & Trim (From_Utf_8 (Safir.Dob.Typesystem.Int_64'Image (Self.Connection.Get_Number_Of_Instances
                      (Action_Ptr.Type_Id.Get_Val,
                         Action_Ptr.Handler.Get_Val,
                         Action_Ptr.Include_Subclasses.Get_Val))), Ada.Strings.Both));

               when Dose_Test.Action_Enum.Get_Queue_Capacity =>
                  Logger.Put_Line (PREFIX & Natural'Wide_Image (Self.Consumer_Number) & ": "
                    & "The capacity of "
                    & Safir.Dob.Connection_Queue_Id.To_Dou_String (Action_Ptr.Connection_Queue_Id.Get_Val)
                    & " is "
                    & Trim (From_Utf_8 (Safir.Dob.Typesystem.Int_32'Image (Safir.Dob.Connection_Aspect_Miscs.Create (Self.Connection).Get_Queue_Capacity
                      (Action_Ptr.Connection_Queue_Id.Get_Val))), Ada.Strings.Both));

               when Dose_Test.Action_Enum.Get_Queue_Size =>
                  Logger.Put_Line (PREFIX & Natural'Wide_Image (Self.Consumer_Number) & ": "
                    & "The size of "
                    & Safir.Dob.Connection_Queue_Id.To_Dou_String (Action_Ptr.Connection_Queue_Id.Get_Val)
                    & " is "
                    & Trim (From_Utf_8 (Safir.Dob.Typesystem.Int_32'Image (Safir.Dob.Connection_Aspect_Miscs.Create (Self.Connection).Get_Queue_Size
                      (Action_Ptr.Connection_Queue_Id.Get_Val))), Ada.Strings.Both));

               when Dose_Test.Action_Enum.Reset_Callback_Actions =>
                  Logger.Put_Line (PREFIX & Natural'Wide_Image (Self.Consumer_Number) & ": ResetCallbackActions");
                  for I in Self.Callback_Actions'Range loop
                     Self.Callback_Actions (I).Clear;
                  end loop;

               when others =>
                  Logger.Put_Line (PREFIX & Natural'Wide_Image (Self.Consumer_Number) & UWS (": ResetCallbackActions") &
                                   UWS ("No handler defined for action ") &
                                   Dose_Test.Action_Enum.To_Dou_String (Action_Ptr.Action_Kind.Get_Val));
            end case;
            Repeats := Repeats + 1;
         exception
            when Safir.Dob.Overflow_Exception.Xception =>
               Logger.Put_Line ("Caught Overflow exception");

               -- sleep a very short while, to let dose_main empty
               -- the message out queue. This hopefully reduces the tc 003
               -- output differences
               delay 0.001;

               Repeat := False;
         end;

         if not Repeat then
            exit;
         end if;
      end loop;
   exception
         --  We explicitly catch the expected exceptions to minimize differences
         --  in the output.
      when Safir.Dob.Access_Denied_Exception.Xception =>
         Logger.Put_Line ("Caught FundamentalException in ExecuteAction: Safir.Dob.AccessDeniedException");
      when Safir.Dob.Not_Found_Exception.Xception =>
         Logger.Put_Line ("Caught Exception in ExecuteAction: Safir.Dob.NotFoundException");
      when Safir.Dob.Typesystem.Software_Violation_Exception =>
         Logger.Put_Line ("Caught FundamentalException in ExecuteAction: Safir.Dob.Typesystem.SoftwareViolationException");
      when E : others =>
         Logger.Put ("Caught some exception: ");
         Logger.Put_Line (From_Utf_8 (Ada.Exceptions.Exception_Name (E)));
         Logger.Put_Line (From_Utf_8 (Ada.Exceptions.Exception_Information (E)));

   end Execute_Action;

end Consumers;
