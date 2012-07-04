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
with Interfaces.C;
with Safir.Dob.Callbacks;
with Safir.Dob.Interf;
with Safir.Dob.Typesystem.Utilities;
with Safir.Dob.Typesystem.Library_Exceptions;

package body Safir.Dob.Secondary_Connections is

   package C renames Interfaces.C;

   procedure Attach
     (Self : in out Secondary_Connection) is
   begin
      Attach (Self,
              To_Unbounded_Wide_String (""),
              To_Unbounded_Wide_String (""));
   end Attach;


   procedure Attach
     (Self                          : in out Secondary_Connection;
      Connection_Name_Common_Part   : in Unbounded_Wide_String;
      Connection_Name_Instance_Part : in Unbounded_Wide_String) is

      New_ControllerId : Safir.Dob.Defs.Controller_Id := 0;
      Common : constant String := Safir.Dob.Typesystem.Utilities.To_Utf_8
        (Connection_Name_Common_Part);
      Instance : constant String := Safir.Dob.Typesystem.Utilities.To_Utf_8
        (Connection_Name_Instance_Part);

      Success : C.char;
   begin
      if Self.Is_Attached then
         Self.Detach;
      end if;

      Safir.Dob.Interf.Connect_Secondary
        (C.To_C (Common),
         C.To_C (Instance),
         Safir.Dob.Interf.Language_Ada,
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
         New_ControllerId,
         Success);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;

      Self.Controller_Id := New_ControllerId;

   end Attach;

   procedure Detach
     (Self : in out Secondary_Connection) is
   begin
      Self.Controller_Id := -1;
   end Detach;

   function Is_Attached
     (Self : in Secondary_Connection) return Boolean is

      Is_Connected : C.char;
      Success : C.char;
   begin
      if Self.Controller_Id < 0 then
         return False;
      end if;

      Safir.Dob.Interf.Is_Connected (Self.Controller_Id, Is_Connected, Success);

      if C.char'Pos (Success) = 0 then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;

      return C.char'Pos (Is_Connected) /= 0;
   end Is_Attached;

   function Get_Controller_Id (Self : in Secondary_Connection)
                               return Safir.Dob.Defs.Controller_Id is
   begin
      return Self.Controller_Id;
   end Get_Controller_Id;

   procedure Finalize (Self : in out Secondary_Connection) is
   begin
      Detach (Self);
   end Finalize;
end Safir.Dob.Secondary_Connections;
