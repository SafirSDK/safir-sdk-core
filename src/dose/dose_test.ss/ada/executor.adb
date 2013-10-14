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
with Dose_Test.Partner;
with Logger;
with Dose_Test.Action_Enum;
with Dose_Test.Dump;
with Dose_Test.Dump_Result;
with Safir.Dob.Typesystem;
with Safir.Dob.Typesystem.Instance_Id;
with Safir.Dob.Typesystem.Blob_Operations;
with Safir.Dob.Typesystem.String_Container; use Safir.Dob.Typesystem.String_Container;
with Safir.Dob.Typesystem.Channel_Id;
with Safir.Dob.Typesystem.Utilities; use Safir.Dob.Typesystem.Utilities;
with Safir.Dob.Instance_Id_Policy;
with Safir.Dob.Typesystem.Si_64;
with Safir.Dob.Error_Response;
with Safir.Dob.This_Node_Parameters;
with Safir.Dob.Typesystem.Object.Factory;
with Safir.Dob.Node_Info;
with Unchecked_Conversion;
with Unchecked_Deallocation;
with Interfaces.C;
with Ada.Strings.Wide_Fixed;
with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Streams;
with GNAT.Sockets; use GNAT.Sockets;
with GNAT.Command_Line;
with Text_IO; use Text_IO;
with Ada.Strings.Wide_Unbounded.Wide_Text_IO; use Ada.Strings.Wide_Unbounded.Wide_Text_IO;
with Ada.Command_Line;

pragma Warnings ("D"); -- turn off warnings for implicit dereference
--pragma Warnings ("L"); -- turn off warnings for missing elaboration pragma

package body Executor is

   package C renames Interfaces.C;

   procedure Inhibit_Outgoing_Traffic
     (Inhibit   : in  C.char;
      Success   : out C.char);
   pragma Import (C, Inhibit_Outgoing_Traffic, "InhibitOutgoingTraffic");

   procedure Inhibit_Outgoing_Traffic_Status (Is_Inhibited : out C.char);
   pragma Import (C, Inhibit_Outgoing_Traffic_Status, "InhibitOutgoingTrafficStatus");

   type Signalled_Events_T is array (Event_T) of Boolean;
   All_Unsignalled : constant Signalled_Events_T := (others => False);

   protected Reactor is
      procedure Post (Event : in Event_T);
      entry Wait (Events : out Signalled_Events_T);
   private
      Signalled_Events : Signalled_Events_T := (others => False);
   end Reactor;

   protected body Reactor is
      procedure Post (Event : in Event_T) is
      begin
         Signalled_Events (Event) := True;
      end Post;

      entry Wait (Events : out Signalled_Events_T) when Signalled_Events /= All_Unsignalled is
      begin
         Events := Signalled_Events;
         Signalled_Events := All_Unsignalled;
      end Wait;
   end Reactor;

   task Action_Reader is
      entry Run (The_Instance : in Safir.Dob.Typesystem.Int_64);
      entry Start (Port : out Port_Type);
      entry Stop;
      entry Get_Action (The_Action : out Dose_Test.Action.Smart_Pointer);
      entry Action_Completed;
   end Action_Reader;
   Server_Socket : Socket_Type;
   Data_Socket : Socket_Type;

   procedure Run is
      use Ada.Strings.Wide_Unbounded;
   begin
      MainLoop.Run (GNAT.Command_Line.Get_Argument); -- program instance
   end Run;


   procedure On_Do_Dispatch
     (Self : in out Dispatcher) is
   begin
      Reactor.Post (Self.Event);
   end On_Do_Dispatch;

   task body Action_Reader is
      use Ada.Strings.Wide_Unbounded;
      use type Safir.Dob.Typesystem.Int_64;
      use Ada.Streams;

      Instance : Safir.Dob.Typesystem.Int_64;

      Remote_Address : Sock_Addr_Type;
      Receive_Stream : Stream_Access;

      type Stream_Element_Ptr is access all Stream_Element;

      type Stream_Element_Array_Ptr is access all Stream_Element_Array;

      procedure Free is new Unchecked_Deallocation
         (Object => Stream_Element_Array, Name => Stream_Element_Array_Ptr);

      function To_Char_Star is new Unchecked_Conversion (Stream_Element_Ptr, Safir.Dob.Typesystem.Char_Star);

      Action : Dose_Test.Action.Smart_Pointer;
      Header : aliased Stream_Element_Array (1 .. 16);

      Ok : constant Stream_Element_Array := (Stream_Element (Character'Pos ('o')),
                                             Stream_Element (Character'Pos ('k')),
                                             Stream_Element (0));
   begin
      accept Run (The_Instance : in Safir.Dob.Typesystem.Int_64) do
         Instance := The_Instance;
      end Run;

      Action_Reader_Loop : loop
         select
            accept Start (Port : out Port_Type) do
               Create_Socket (Server_Socket, Family_Inet, Socket_Stream);

               Set_Socket_Option
                  (Server_Socket,
                   Socket_Level,
                   (Reuse_Address, False));

               for I in 0 .. 99 loop
                  Port := Port_Type (30000) + Port_Type (Instance) + Port_Type (I * 3);
                  begin
                     Bind_Socket (Server_Socket, (Family => Family_Inet,
                                                  Addr => Any_Inet_Addr,
                                                  Port => Port));
                     Put_Line ("Action_Reader: accepting connections on port " & Port_Type'Image (Port));
                     exit;
                  exception
                     when Socket_Error =>
                        null;
                  end;
               end loop;
            end Start;
         or
            terminate;
         end select;

         Put_Line ("Calling Listen_Socket");
         Listen_Socket (Server_Socket);
         Put_Line ("Calling Accept_Socket");
         Accept_Socket (Server_Socket, Data_Socket, Remote_Address);
         Put_Line ("Calling Stream");
         Receive_Stream := Stream (Data_Socket);

         Put_Line ("Entering Receive_Loop");
         Receive_Loop : loop
            declare
               Header_Ptr : constant Stream_Element_Ptr := Header (1)'Access;
               Last   : Stream_Element_Offset;
               Blob_Size : Stream_Element_Offset;
            begin
               Receive_Stream.Read (Header, Last);
               if Last /= 16 then
                  Put_Line ("Failed to read Header, exiting Action_Reader_Loop!");
                  exit Action_Reader_Loop;
               end if;
               Blob_Size := Stream_Element_Offset (Safir.Dob.Typesystem.Blob_Operations.Get_Size (Blob => To_Char_Star (Header_Ptr)));
               Put_Line ("Got Header (size " & Stream_Element_Offset'Image (Blob_Size) & ")");
               declare
                  Blob : Stream_Element_Array_Ptr :=
                     new Stream_Element_Array (1 .. Blob_Size);
                  Blob_Ptr : constant Stream_Element_Ptr := Blob (1)'Access;
               begin
                  Blob.all (1 .. 16) := Header;
                  Receive_Stream.Read (Blob.all (17 .. Blob_Size), Last);

                  if Last /= Blob_Size then
                     Put_Line ("Failed to read Blob, exiting Action_Reader_Loop!");
                     exit Action_Reader_Loop;
                  end if;

                  Put_Line ("Got Blob");
                  Action := Dose_Test.Action.Smart_Pointer (Safir.Dob.Typesystem.Object.Factory.Create_Object
                                                               (Blob => To_Char_Star (Blob_Ptr)));

                  Put_Line ("Posting to reactor");
                  Reactor.Post (Received_Action);

                  Put_Line ("Waiting for Get_Action");
                  select
                     accept Get_Action (The_Action : out Dose_Test.Action.Smart_Pointer) do
                        The_Action := Action;
                     end Get_Action;
                  or
                     accept Stop;
                     exit Receive_Loop;
                  or
                     terminate;
                  end select;

                  Put_Line ("Waiting for Action_Completed");
                  select
                     accept Action_Completed;
                  or
                     accept Stop;
                     exit Receive_Loop;
                  or
                     terminate;
                  end select;

                  Receive_Stream.Write (Ok);

                  Free (Blob);
               end;
            exception
               when Socket_Error =>
                  exit Receive_Loop;
            end;
         end loop Receive_Loop;
      end loop Action_Reader_Loop;
   exception when E : others =>
      Put_Line ("Exception in Action_Reader task: " & Exception_Name (E));
      Put_Line (Exception_Information (E));
   end Action_Reader;

   task body MainLoop is
      The_Executor : aliased Executor_Type;
      use Ada.Strings.Wide_Unbounded;
   begin
      Put_Line ("Starting");

      The_Executor.Identifier := Ada.Strings.Wide_Unbounded.To_Unbounded_Wide_String ("ada");
      The_Executor.Control_Connection_Name := The_Executor.Identifier & "_control";
      The_Executor.Test_Connection_Name := Ada.Strings.Wide_Unbounded.
        To_Unbounded_Wide_String ("partner_test_connection");

      accept Run (Arg : in String) do
         The_Executor.Instance := Safir.Dob.Typesystem.Int_64'Value (Arg);
         The_Executor.Instance_String := Ada.Strings.Wide_Unbounded.To_Unbounded_Wide_String
           (Ada.Strings.Wide_Fixed.Trim
              (Source => Safir.Dob.Typesystem.Int_64'Wide_Image (The_Executor.Instance),
               Side => Ada.Strings.Both));
         The_Executor.Partner_Entity_Id :=
           Safir.Dob.Typesystem.Entity_Id.Create_Entity_Id
           (Dose_Test.Partner.Class_Type_Id,
            Safir.Dob.Typesystem.Instance_Id.Create_Instance_Id (The_Executor.Instance));
         Action_Reader.Run (The_Executor.Instance);
      end Run;

      The_Executor.Control_Connection.Open (The_Executor.Control_Connection_Name,
                                            The_Executor.Instance_String,
                                            0,
                                            The_Executor'Access,
                                            The_Executor.Control_Dispatcher'Access);

      The_Executor.Control_Connection.Subscribe_Entity
         (Dose_Test.Sequencer.Class_Type_Id,
          The_Executor'Access);

      Put_Line (The_Executor.Identifier & ":" & Safir.Dob.Typesystem.Int_64'Wide_Image (The_Executor.Instance) & " Started");
      declare
         Events : Signalled_Events_T;
         use type Consumers.Consumer_Access;
      begin
         The_Main_Loop : loop
            Reactor.Wait (Events);
            for Event in Event_T loop
               if Events (Event) then
                  case Event is
                     when Stop =>
                        exit The_Main_Loop;
                     when Dispatch_Control =>
                        The_Executor.Control_Connection.Dispatch;
                     when Dispatch_Test =>
                        if The_Executor.Dispatch_Test_Connection and The_Executor.Is_Active then
                           The_Executor.Execute_Callback_Actions (Safir.Dob.Callback_Id.On_Do_Dispatch);

                           for I in The_Executor.The_Consumers'Range loop
                              if The_Executor.The_Consumers (I) /= null then
                                 The_Executor.The_Consumers (I).Execute_Callback_Actions (Safir.Dob.Callback_Id.On_Do_Dispatch);
                              end if;
                           end loop;

                           The_Executor.Test_Connection.Dispatch;
                        end if;
                     when Received_Action =>
                        declare
                           Action : Dose_Test.Action.Smart_Pointer;
                           Immediate_Ack : Boolean;
                           use type Dose_Test.Action_Enum.Enumeration;
                        begin
                           Action_Reader.Get_Action (Action);
                           Immediate_Ack := Action.Ref.Action_Kind.Get_Val = Dose_Test.Action_Enum.Sleep;
                           if Immediate_Ack then
                              Action_Reader.Action_Completed;
                           end if;

                           The_Executor.Handle_Action (Action);

                           if not Immediate_Ack then
                              Action_Reader.Action_Completed;
                           end if;
                        end;
                  end case;
               end if;
            end loop;
         end loop The_Main_Loop;

      end;

      -- use a slightly undocumented way of getting receive_socket
      -- to break from blocking
      begin
         Shutdown_Socket (Server_Socket);
         Shutdown_Socket (Data_Socket);
      exception
         when Socket_Error =>
            null;
      end;

      Logger.Put_Line ("Exiting");

      Ada.Command_Line.Set_Exit_Status (0);
   exception
      when E : others =>
         Logger.Put ("Exception in main loop: ");
         Logger.Put_Line (Safir.Dob.Typesystem.Utilities.From_Utf_8 (Ada.Exceptions.Exception_Name (E)));
         Logger.Put_Line (Safir.Dob.Typesystem.Utilities.From_Utf_8 (Ada.Exceptions.Exception_Information (E)));
         Ada.Command_Line.Set_Exit_Status (1);
   end MainLoop;

   overriding
   procedure On_Stop_Order
     (Self : in out Executor_Type) is
   begin
      Logger.Put_Line ("Got stop order");
      Self.Execute_Callback_Actions (Safir.Dob.Callback_Id.On_Stop_Order);
      Reactor.Post (Stop);
   end On_Stop_Order;

   procedure On_New_Entity
      (Self         : in out Executor_Type;
       Entity_Proxy : in     Safir.Dob.Entity_Proxies.Entity_Proxy) is
   begin
      Self.Handle_Sequencer_State (Dose_Test.Sequencer.Smart_Pointer (Entity_Proxy.Get_Entity));
   end On_New_Entity;

   procedure On_Updated_Entity
      (Self         : in out Executor_Type;
       Entity_Proxy : in     Safir.Dob.Entity_Proxies.Entity_Proxy) is
   begin
      Self.Handle_Sequencer_State (Dose_Test.Sequencer.Smart_Pointer (Entity_Proxy.Get_Entity));
   end On_Updated_Entity;

   overriding
   procedure On_Revoked_Registration
     (Self       : in out Executor_Type;
      Type_Id    : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type) is
      pragma Unreferenced (Type_Id, Handler_Id);
   begin
      if Self.Is_Active then
         Put_Line ("Deactivating");

         Self.Test_Connection.Close;

         Self.Control_Connection.Unregister_Handler
           (Safir.Dob.Typesystem.Entity_Id.Get_Type_Id (Self.Partner_Entity_Id),
            Safir.Dob.Typesystem.Handler_Id.Create_Handler_Id (Self.Instance));
         Self.Control_Connection.Unregister_Handler
           (Dose_Test.Dump.Class_Type_Id,
            Safir.Dob.Typesystem.Handler_Id.Create_Handler_Id (Self.Instance));

         Self.Is_Active := False;
      end if;
   end On_Revoked_Registration;

   overriding
   procedure On_Create_Request
    (Self                 : in out Executor_Type;
     Entity_Request_Proxy : in Safir.Dob.Entity_Request_Proxies.Entity_Request_Proxy;
     Response_Sender      : in Safir.Dob.Response_Senders.Response_Sender) is

      pragma Unreferenced (Self, Entity_Request_Proxy);

      Response : constant Safir.Dob.Error_Response.Smart_Pointer :=
                   Safir.Dob.Error_Response.Create;
   begin
      Response_Sender.Send (Response);
   end On_Create_Request;

   overriding
   procedure On_Update_Request
    (Self                 : in out Executor_Type;
     Entity_Request_Proxy : in Safir.Dob.Entity_Request_Proxies.Entity_Request_Proxy;
     Response_Sender      : in Safir.Dob.Response_Senders.Response_Sender) is

      pragma Unreferenced (Self, Entity_Request_Proxy);

      Response : constant Safir.Dob.Error_Response.Smart_Pointer :=
                   Safir.Dob.Error_Response.Create;
   begin
      Response_Sender.Send (Response);
   end On_Update_Request;

   overriding
   procedure On_Delete_Request
    (Self                 : in out Executor_Type;
     Entity_Request_Proxy : in Safir.Dob.Entity_Request_Proxies.Entity_Request_Proxy;
     Response_Sender      : in Safir.Dob.Response_Senders.Response_Sender) is

      pragma Unreferenced (Self, Entity_Request_Proxy);

      Response : constant Safir.Dob.Error_Response.Smart_Pointer :=
                   Safir.Dob.Error_Response.Create;
   begin
      Response_Sender.Send (Response);
   end On_Delete_Request;

   overriding
   procedure On_Service_Request
    (Self                  : in out Executor_Type;
     Service_Request_Proxy : in Safir.Dob.Service_Request_Proxies.Service_Request_Proxy;
     Response_Sender       : in Safir.Dob.Response_Senders.Response_Sender) is

      pragma Unreferenced (Self, Service_Request_Proxy);

      Result : constant Dose_Test.Dump_Result.Smart_Pointer := Dose_Test.Dump_Result.Create;
   begin
      Result.Ref.Result.Set_Val (Logger.Get_String);
      Logger.Clear;

      Response_Sender.Send (Result);
   end On_Service_Request;


   function "=" (Left, Right : in Dose_Test.Action.Smart_Pointer) return Boolean is
      pragma Unreferenced (Left, Right);
   begin
      return False;
   end "=";

   procedure Handle_Action (Self : in out Executor_Type;
                            Action : in Dose_Test.Action.Smart_Pointer) is
      use type Safir.Dob.Typesystem.Channel_Id.Channel_Id_Type;
   begin

      if not Action.Ref.Partner.Is_Null and then
        Action.Ref.Partner.Get_Val /= Safir.Dob.Typesystem.Channel_Id.Create_Channel_Id (Self.Instance) then
         return;
      end  if;

      if Action.Ref.Consumer.Is_Null then
         -- No consumer set, meant for the executor.
         if Action.Ref.Action_Callback.Is_Null then
            -- It is a normal action.
            Self.Execute_Action (Action);
         elsif Self.Is_Active then
            Self.Add_Callback_Action (Action);
         end if;
      elsif Self.Is_Active then
         declare
            Consumer : constant Consumers.Consumer_Access :=
              Self.The_Consumers (Integer (Action.Ref.Consumer.Get_Val));
         begin
            if Action.Ref.Action_Callback.Is_Null then
               Consumer.Execute_Action (Action);
            else
               Consumer.Add_Callback_Action (Action);
            end if;
         end;
      end if;
   end Handle_Action;

   procedure Execute_Action
     (Self   : in out Executor_Type;
      Action : in Dose_Test.Action.Smart_Pointer) is

      use type Safir.Dob.Typesystem.Int_32;
      Partner : Dose_Test.Partner.Smart_Pointer;
      Entity_Proxy : Safir.Dob.Entity_Proxies.Entity_Proxy;
   begin
      case Action.Ref.Action_Kind.Get_Val is
         when Dose_Test.Action_Enum.Reset =>
            if Self.Is_Active then
               Self.Test_Connection.Close;
               Self.Test_Connection.Open
                 (Self.Test_Connection_Name,
                  Self.Instance_String,
                  Self.Default_Context,
                  null,
                  Self.Test_Dispatcher'Access);

               Entity_Proxy := Self.Control_Connection.Read (Self.Partner_Entity_Id);
               Partner := Dose_Test.Partner.Smart_Pointer (Entity_Proxy.Get_Entity);
               Partner.Ref.Incarnation.Set_Val (Partner.Ref.Incarnation.Get_Val + 1);
               Self.Control_Connection.Set_Changes
                 (Partner,
                  Safir.Dob.Typesystem.Entity_Id.Get_Instance_Id (Self.Partner_Entity_Id),
                  Safir.Dob.Typesystem.Handler_Id.Create_Handler_Id (Self.Instance));

               for Index in Self.The_Consumers'Range loop
                  Consumers.Destroy (Self.The_Consumers (Index));
                  Self.The_Consumers (Index) := Consumers.Create (Index, Self.Test_Connection_Name, Self.Instance_String);
               end loop;

               for Index in Safir.Dob.Callback_Id.Enumeration loop
                  Self.Callback_Actions (Index).Clear;
               end loop;
            end if;

         when Dose_Test.Action_Enum.Open =>
            if Self.Is_Active then
               declare
                  Context : Safir.Dob.Typesystem.Int_32 := Self.Default_Context;
                  Conn_Name : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String :=
                     Self.Test_Connection_Name;
               begin
                  if not Action.Ref.Context.Is_Null then
                     Context := Action.Ref.Context.Get_Val;
                  end if;

                  if not Action.Ref.Connection_Name.Is_Null then
                     Conn_Name := Action.Ref.Connection_Name.Get_Val;
                  end if;

                  Self.Test_Connection.Open
                    (Conn_Name,
                     Self.Instance_String,
                     Context,
                     null,
                     Self.Test_Dispatcher'Access);
               end;

            end if;

         when Dose_Test.Action_Enum.Close =>
            if Self.Is_Active then
               Self.Test_Connection.Close;
            end if;

         when Dose_Test.Action_Enum.Inhibit_Dispatch =>
            if Self.Is_Active then
               Self.Dispatch_Test_Connection := not Action.Ref.Inhibit.Get_Val;
               Logger.Put_Line ("InhibitDispatch set to " & Boolean'Wide_Image (Self.Dispatch_Test_Connection));
            end if;

         when Dose_Test.Action_Enum.Inhibit_Outgoing_Traffic =>
            if Self.Is_Active then
               declare
                  Success : C.char;
                  Inhibit : constant C.char := C.char'Val (Boolean'Pos (Action.Ref.Inhibit.Get_Val));
                  Is_Inhibited : C.char;
               begin
                  Inhibit_Outgoing_Traffic (Inhibit, Success);
                  Inhibit_Outgoing_Traffic_Status (Is_Inhibited);
                  Logger.Put_Line ("InhibitOutgoingTraffic set to" & Integer'Wide_Image (C.char'Pos (Is_Inhibited)));
               end;
            end if;

         when Dose_Test.Action_Enum.Print =>
            if Self.Is_Active then
               Logger.Put_Line (Action.Ref.Print_String.Get_Val);
            end if;

         when Dose_Test.Action_Enum.Reset_Callback_Actions =>
            for Index in Safir.Dob.Callback_Id.Enumeration loop
               Self.Callback_Actions (Index).Clear;
            end loop;

         when Dose_Test.Action_Enum.Sleep =>
            if Self.Is_Active then
               Put_Line ("Sleeping " &
                         Safir.Dob.Typesystem.Si_64.Second'Image (Action.Ref.Sleep_Duration.Get_Val) &
                         " seconds");
               delay Duration (Action.Ref.Sleep_Duration.Get_Val);
            end if;


         when Dose_Test.Action_Enum.Check_References |
              Dose_Test.Action_Enum.Close_And_Check_References |
              Dose_Test.Action_Enum.Run_Garbage_Collector =>
            -- These actions are for garbage collected languages only.
            null;

         when others =>
            Logger.Put_Line ("Got unexpected action " &
                             Dose_Test.Action_Enum.Enumeration'Wide_Image (Action.Ref.Action_Kind.Get_Val));

      end case;

   end Execute_Action;


   procedure Add_Callback_Action
     (Self   : in out Executor_Type;
      Action : in Dose_Test.Action.Smart_Pointer) is
   begin
      Self.Callback_Actions (Action.Ref.Action_Callback.Get_Val).Append (Action);
   end Add_Callback_Action;

   procedure Execute_Callback_Actions
     (Self     : in out Executor_Type;
      Callback : in     Safir.Dob.Callback_Id.Enumeration) is

      procedure Execute (Position : in Action_Vectors.Cursor) is
      begin
         Execute_Action (Self, Action_Vectors.Element (Position));
      end Execute;

   begin
      Self.Callback_Actions (Callback).Iterate (Execute'Access);
   end Execute_Callback_Actions;

   procedure Handle_Sequencer_State
      (Self  : in out Executor_Type;
       State : in     Dose_Test.Sequencer.Smart_Pointer) is
      use type Dose_Test.Sequencer.Sequencer_Class_Access;

      Raw : constant Dose_Test.Sequencer.Sequencer_Class_Access := State.Ref;

      Activate : constant Boolean := State.Ref /= null and then
         Raw.Partners.Element (Safir.Dob.Typesystem.Array_Index (Self.Instance)) = Self.Identifier;

      Partner : Dose_Test.Partner.Smart_Pointer;
      Port : Port_Type;
      NodeInfo : Safir.Dob.Node_Info.Smart_Pointer;
   begin
      if Activate = Self.Is_Active then
         return;
      end if;

      if Activate then
         Self.Default_Context := Raw.Context.Get_Val;
         Put_Line ("Activating (default context is " & Safir.Dob.Typesystem.Int_32'Image (Self.Default_Context) & ")");
         Self.Control_Connection.Register_Entity_Handler
            (Safir.Dob.Typesystem.Entity_Id.Get_Type_Id (Self.Partner_Entity_Id),
             Safir.Dob.Typesystem.Handler_Id.Create_Handler_Id (Self.Instance),
             Safir.Dob.Instance_Id_Policy.Handler_Decides_Instance_Id,
             Self'Access);

         Self.Control_Connection.Register_Service_Handler
            (Dose_Test.Dump.Class_Type_Id,
             Safir.Dob.Typesystem.Handler_Id.Create_Handler_Id (Self.Instance),
             Self'Access);

         Action_Reader.Start (Port);

         Partner := Dose_Test.Partner.Create;
         Partner.Ref.Incarnation.Set_Val (0);
         Partner.Ref.Identifier.Set_Val (Self.Identifier);
         Partner.Ref.Port.Set_Val (Safir.Dob.Typesystem.Int_32 (Port));

         -- Get the address from Node_Info object
         NodeInfo := Safir.Dob.Node_Info.Smart_Pointer
            (Self.Control_Connection.Read (Safir.Dob.Typesystem.Entity_Id.Create_Entity_Id
                                              (Safir.Dob.Node_Info.Class_Type_Id,
                                               Safir.Dob.Typesystem.Instance_Id.Create_Instance_Id
                                                  (Safir.Dob.Typesystem.Int_64 (Safir.Dob.This_Node_Parameters.Node_Number)))).Get_Entity);
         Partner.Ref.Address.Set_Val (NodeInfo.Ref.Ip_Address.Get_Val);

         Self.Control_Connection.Set_All
            (Partner,
             Safir.Dob.Typesystem.Entity_Id.Get_Instance_Id (Self.Partner_Entity_Id),
             Safir.Dob.Typesystem.Handler_Id.Create_Handler_Id (Self.Instance));

         Self.Is_Active := True;
      else
         Put_Line ("Deactivating");
         Action_Reader.Stop;

         Self.Test_Connection.Close;
         Self.Control_Connection.Delete
            (Self.Partner_Entity_Id,
             Safir.Dob.Typesystem.Handler_Id.Create_Handler_Id (Self.Instance));
         Self.Control_Connection.Unregister_Handler
            (Safir.Dob.Typesystem.Entity_Id.Get_Type_Id (Self.Partner_Entity_Id),
             Safir.Dob.Typesystem.Handler_Id.Create_Handler_Id (Self.Instance));
         Self.Control_Connection.Unregister_Handler
            (Dose_Test.Dump.Class_Type_Id,
             Safir.Dob.Typesystem.Handler_Id.Create_Handler_Id (Self.Instance));
         Self.Is_Active := False;
      end if;

   end Handle_Sequencer_State;

end Executor;
