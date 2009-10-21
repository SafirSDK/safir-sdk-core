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
with Ada.Text_IO;


with Interfaces.C;

with Safir.Dob.Callbacks;
with Safir.Dob.Interf;
--with Safir.Dob.Internal;
with Safir.Dob.Typesystem.Utilities;
with Safir.Dob.Typesystem.Library_Exceptions;

package body Safir.Dob.Connections is

   package C renames Interfaces.C;
--     use type Safir.Dob.Typesystem.Int32;

--   procedure Throw (E : Ada.Exceptions.Exception_Id; Message : String := "")
--                    renames  Ada.Exceptions.Raise_Exception;


   ----------
   -- Open --
   ----------

   procedure Open
     (Self                          : in     Connection;
      Connection_Name_Common_Part   : in     Unbounded_Wide_String;
      Connection_Name_Instance_Part : in     Unbounded_Wide_String;
      Context                       : in     Safir.Dob.Typesystem.Int_32;
      Stop_Handler                  : access Safir.Dob.Consumers.Stop_Handler'Class;
      Dispatcher                    : access Safir.Dob.Consumers.Dispatcher'Class) is

      Common : constant String := Safir.Dob.Typesystem.Utilities.To_Utf_8
        (Connection_Name_Common_Part);
      Instance : constant String := Safir.Dob.Typesystem.Utilities.To_Utf_8
        (Connection_Name_Instance_Part);
      Success : C.char;

   begin
      Safir.Dob.Interf.Connect (Self.Controller_Id,
                                C.To_C (Common),
                                C.To_C (Instance),
                                Context,
                                Safir.Dob.Interf.Language_Ada,
                                Stop_Handler,
                                Dispatcher,
                                Safir.Dob.Callbacks.On_Do_Dispatch_Cb'Access,
                                Safir.Dob.Callbacks.On_Stop_Order_Cb'Access,
                                Safir.Dob.Callbacks.On_New_Entity_Cb'Access,
                                Safir.Dob.Callbacks.On_Updated_Entity_Cb'Access,
                                Safir.Dob.Callbacks.On_Deleted_Entity_Cb'Access,
                                Safir.Dob.Callbacks.On_Create_Request_Cb'Access,
                                Safir.Dob.Callbacks.On_Update_Request_Cb'Access,
                                Safir.Dob.Callbacks.On_Delete_Request_Cb'Access,
                                Safir.Dob.Callbacks.On_Service_Request_Cb'Access,
                                Safir.Dob.Callbacks.On_Response_Cb'Access,
                                Safir.Dob.Callbacks.On_Message_Cb'Access,
                                Safir.Dob.Callbacks.On_Registered_Cb'Access,
                                Safir.Dob.Callbacks.On_Unregistered_Cb'Access,
                                Safir.Dob.Callbacks.On_Revoked_Registration_Cb'Access,
                                Safir.Dob.Callbacks.On_Completed_Registration_Cb'Access,
                                Safir.Dob.Callbacks.On_Injected_New_Entity_Cb'Access,
                                Safir.Dob.Callbacks.On_Injected_Updated_Entity_Cb'Access,
                                Safir.Dob.Callbacks.On_Injected_Deleted_Entity_Cb'Access,
                                Safir.Dob.Callbacks.On_Initial_Injections_Done_Cb'Access,
                                Safir.Dob.Callbacks.On_Not_Request_Overflow_Cb'Access,
                                Safir.Dob.Callbacks.On_Not_Message_Overflow_Cb'Access,
                                null, --  No drop reference callback needed for Ada.
                                Success);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;

   end Open;

   -----------
   -- Close --
   -----------

   procedure Close
      (Self : in Connection) is

   begin
      Self.Close (True);
   end Close;

   procedure Close
     (Self         : in Connection;
      Check_Thread : in     Boolean) is

      Success : C.char;
   begin
      Safir.Dob.Interf.Disconnect (Self.Controller_Id,
                                   C.char'Val (Boolean'Pos (Check_Thread)),
                                   Success);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
   end Close;


   ------------
   -- IsOpen --
   ------------

   function Is_Open (Self : in Connection)
                   return Boolean is
      Connected : C.char;
      Success : C.char;
   begin
      Safir.Dob.Interf.Is_Connected (Self.Controller_Id,
                                     Connected,
                                     Success);
      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;

      return C.char'Pos (Success) /= 0;
   end Is_Open;


   --------------
   -- Dispatch --
   --------------

   procedure Dispatch
     (Self : in Connection) is

      Success : C.char;
   begin
      Safir.Dob.Interf.Dispatch (Self.Controller_Id,
                                 Success);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
   end Dispatch;

   -----------------------
   -- Get_Controller_Id --
   -----------------------

   function Get_Controller_Id (Self : in Connection) return Safir.Dob.Defs.Controller_Id is
   begin
      return Self.Controller_Id;
   end Get_Controller_Id;

   -------------------------------------
   -- Initialization and finalization --
   -------------------------------------

   procedure Initialize (Self : in out Connection) is
      Success : C.char;
   begin
      Safir.Dob.Interf.Constructor (Self.Controller_Id, Success);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
   end Initialize;

   procedure Finalize (Self : in out Connection) is
   begin
      begin
         Close (Self, False);
      exception
         when E : others =>
            Ada.Text_IO.Put_Line ("Connection.Finalize: Caught exception: " &
                                     Ada.Exceptions.Exception_Information (E));
            Ada.Text_IO.Put_Line ("Will return as if nothing has happened");
      end;
      Safir.Dob.Interf.Destructor (Self.Controller_Id);
   end Finalize;

end Safir.Dob.Connections;
