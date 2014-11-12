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
with Ada.Containers.Vectors;
with Ada.Strings.Wide_Unbounded;
with Consumers;
with Dose_Test.Action;
with Dose_Test.Sequencer;
with Safir.Dob.Callback_Id;
with Safir.Dob.Connections;
with Safir.Dob.Consumers;
with Safir.Dob.Entity_Proxies;
with Safir.Dob.Entity_Request_Proxies;
with Safir.Dob.Response_Senders;
with Safir.Dob.Service_Request_Proxies;
with Safir.Dob.Typesystem.Entity_Id;
with Safir.Dob.Typesystem.Handler_Id;
with Safir.Dob.Typesystem;

pragma Warnings ("L"); -- turn off warnings for missing elaboration pragma

package Executor is

   procedure Run;

private
   task MainLoop is
      entry Run (Arg : in String);
   end MainLoop;

   type Event_T is (Stop, Dispatch_Control, Dispatch_Test, Received_Action);

   type Dispatcher is limited new
     Safir.Dob.Consumers.Dispatcher with
      record
        Event : Event_T;
      end record;

   overriding
   procedure On_Do_Dispatch
     (Self : in out Dispatcher);

   function "=" (Left, Right : in Dose_Test.Action.Smart_Pointer) return Boolean;

   package Action_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Dose_Test.Action.Smart_Pointer);

   type Callback_Actions_Table is array (Safir.Dob.Callback_Id.Enumeration) of
     Action_Vectors.Vector;


   type Consumer_Array is array (0 .. 2) of Consumers.Consumer_Access;

   type Executor_Type is limited new
     Safir.Dob.Consumers.Stop_Handler and
     Safir.Dob.Consumers.Entity_Subscriber and
     Safir.Dob.Consumers.Entity_Handler and
     Safir.Dob.Consumers.Service_Handler with
      record
         Identifier                 : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
         Instance                   : Safir.Dob.Typesystem.Int_64;
         Instance_String            : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
         Control_Connection_Name    : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
         Test_Connection_Name       : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

         Partner_Entity_Id          : Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type;

         Is_Active                  : Boolean := False;
         The_Consumers              : Consumer_Array;

         Control_Connection         : Safir.Dob.Connections.Connection;
         Test_Connection            : Safir.Dob.Connections.Connection;
         Dispatch_Test_Connection   : Boolean := True;

         Test_Dispatcher            : aliased Dispatcher := (Event => Dispatch_Test);
         Control_Dispatcher         : aliased Dispatcher := (Event => Dispatch_Control);

         Callback_Actions           : Callback_Actions_Table;

         Default_Context            : Safir.Dob.Typesystem.Int_32 := 0;
      end record;

   overriding
   procedure On_Stop_Order
     (Self : in out Executor_Type);

   overriding
   procedure On_New_Entity
      (Self         : in out Executor_Type;
       Entity_Proxy : in     Safir.Dob.Entity_Proxies.Entity_Proxy);

   overriding
   procedure On_Updated_Entity
      (Self         : in out Executor_Type;
       Entity_Proxy : in     Safir.Dob.Entity_Proxies.Entity_Proxy);

   overriding
   procedure On_Deleted_Entity
      (Self             : in out Executor_Type;
       Entity_Proxy     : in     Safir.Dob.Entity_Proxies.Entity_Proxy;
       Deleted_By_Owner : in     Boolean) is null;
   -- ignore deletes since they may be due to an inhibitoutgoingtraffic on the other side

   overriding
   procedure On_Revoked_Registration
     (Self       : in out Executor_Type;
      Type_Id    : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type);

   overriding
   procedure On_Create_Request
     (Self                 : in out Executor_Type;
      Entity_Request_Proxy : in Safir.Dob.Entity_Request_Proxies.Entity_Request_Proxy;
      Response_Sender      : in Safir.Dob.Response_Senders.Response_Sender);

   overriding
   procedure On_Update_Request
     (Self                 : in out Executor_Type;
      Entity_Request_Proxy : in Safir.Dob.Entity_Request_Proxies.Entity_Request_Proxy;
      Response_Sender      : in Safir.Dob.Response_Senders.Response_Sender);

   overriding
   procedure On_Delete_Request
     (Self                 : in out Executor_Type;
      Entity_Request_Proxy : in Safir.Dob.Entity_Request_Proxies.Entity_Request_Proxy;
      Response_Sender      : in Safir.Dob.Response_Senders.Response_Sender);

   overriding
   procedure On_Service_Request
     (Self                  : in out Executor_Type;
      Service_Request_Proxy : in Safir.Dob.Service_Request_Proxies.Service_Request_Proxy;
      Response_Sender       : in Safir.Dob.Response_Senders.Response_Sender);

   procedure Handle_Action (Self : in out Executor_Type;
                            Action : in Dose_Test.Action.Smart_Pointer);

   procedure Execute_Action
      (Self   : in out Executor_Type;
       Action : in     Dose_Test.Action.Smart_Pointer);

   procedure Add_Callback_Action
      (Self   : in out Executor_Type;
       Action : in     Dose_Test.Action.Smart_Pointer);

   procedure Execute_Callback_Actions
      (Self     : in out Executor_Type;
       Callback : in     Safir.Dob.Callback_Id.Enumeration);

   procedure Handle_Sequencer_State
      (Self  : in out Executor_Type;
       State : in     Dose_Test.Sequencer.Smart_Pointer);

end Executor;