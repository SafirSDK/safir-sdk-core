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
with Safir.Dob.Consumers;
with Safir.Dob.Message_Proxies;
with Safir.Dob.Typesystem;
with Safir.Dob.Typesystem.Handler_Id;
with Safir.Dob.Entity_Request_Proxies;
with Safir.Dob.Typesystem.Entity_Id;
with Safir.Dob.Service_Request_Proxies;
with Safir.Dob.Response_Senders;
with Safir.Dob.Connections;
with Safir.Dob.Callback_Id;
with Ada.Strings.Wide_Unbounded;
with Dose_Test.Action;
with Ada.Containers.Vectors;
with Consumers;
pragma Warnings ("L"); -- turn off warnings for missing elaboration pragma

package Executor is

   procedure Run;

private

   type Connection is (Control, Test);

   task MainLoop is
      entry Run (Arg : in String);
      entry Dispatch (Conn : in Connection);
      entry Handle_Action (Action : in Dose_Test.Action.Smart_Pointer);
   end MainLoop;

   type Dispatcher is limited new
     Safir.Dob.Consumers.Dispatcher with
      record
         Conn :  Connection;
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
     Safir.Dob.Consumers.Message_Subscriber and
     Safir.Dob.Consumers.Entity_Handler and
     Safir.Dob.Consumers.Service_Handler with
      record
         Identifier                 : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
         Instance                   : Safir.Dob.Typesystem.Int_64;
         Instance_String            : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
         Control_Connection_Name    : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
         Test_Connection_Name       : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

         Partner_Entity_Id          : Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type;

         Is_Done                    : Boolean := False;
         Is_Active                  : Boolean := False;
         The_Consumers              : Consumer_Array;

         Control_Connection         : Safir.Dob.Connections.Connection;
         Test_Connection            : Safir.Dob.Connections.Connection;
         Dispatch_Test_Connection   : Boolean := True;

         Test_Dispatcher            : aliased Dispatcher := (Conn => Test);
         Control_Dispatcher         : aliased Dispatcher := (Conn => Control);

         Callback_Actions           : Callback_Actions_Table;

         Default_Context            : Safir.Dob.Typesystem.Int_32 := 0;
      end record;

   overriding
   procedure On_Stop_Order
     (Self : in out Executor_Type);

   overriding
   procedure On_Message
     (Self          : in out Executor_Type;
      Message_Proxy : in Safir.Dob.Message_Proxies.Message_Proxy);

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
      Action : in Dose_Test.Action.Smart_Pointer);

   procedure Add_Callback_Action
     (Self   : in out Executor_Type;
      Action : in Dose_Test.Action.Smart_Pointer);

   procedure Execute_Callback_Actions
     (Self     : in out Executor_Type;
      Callback : in     Safir.Dob.Callback_Id.Enumeration);

end Executor;
