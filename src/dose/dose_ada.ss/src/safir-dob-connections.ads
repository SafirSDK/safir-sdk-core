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
with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;
with Safir.Dob.Defs;
with Safir.Dob.Consumers;
with Safir.Dob.Connection_Bases;
with Safir.Dob.Typesystem;

package Safir.Dob.Connections is

   -- A connection to the DOB.
   -- This class represents a "real" (as opposed to Secondary Connection) connection to the dob.
   -- Each DOB application must have at least one connection. Connections are not thread safe.
   --
   type Connection is limited new
     Safir.Dob.Connection_Bases.Connection_Base with private;

   --type ClassAccess is access all Class'Class;

   -- AWI:todo comment
   procedure Open
     (Self                          : in Connection;
      Connection_Name_Common_Part   : in Unbounded_Wide_String;
      Connection_Name_Instance_Part : in Unbounded_Wide_String;
      Context                       : in Safir.Dob.Typesystem.Int_32;
      Stop_Handler                  : access Safir.Dob.Consumers.Stop_Handler'Class;
      Dispatcher                    : access Safir.Dob.Consumers.Dispatcher'Class);

   -- AWI:todo comment
   procedure Close
     (Self : in Connection);

   -- AWI:todo comment
   function Is_Open
     (Self : in Connection) return Boolean;


   -- After receiving a Do_Dispatch callback the application MUST call this
   -- method. A call to Dispatch will result in that all queues for this connection
   -- are emptied and that each message in the queues are passed to the associated
   -- consumer. The call must be from the thread of the connection, not directly
   -- from Do_Dispatch.
   --
   -- Calls to dispatch from connection instances that are not open will be ignored.
   --
   procedure Dispatch
     (Self : in Connection);

   -----------------------
   -- Get_Controller_Id --
   -----------------------
   overriding
   function Get_Controller_Id (Self : in Connection) return Safir.Dob.Defs.Controller_Id;


   procedure Initialize (Self : in out Connection);
   procedure Finalize (Self : in out Connection);

private

   --type Do_Dispatch_Cb_T is access procedure;

   use type Safir.Dob.Defs.Controller_Id;

   type Connection is new Safir.Dob.Connection_Bases.Connection_Base with record
      --Do_Dispatch   : Do_Dispatch_Cb_T;
      Controller_Id : Safir.Dob.Defs.Controller_Id := -1;
   end record;

   procedure Close
     (Self         : in Connection;
      Check_Thread : in Boolean);

end Safir.Dob.Connections;
