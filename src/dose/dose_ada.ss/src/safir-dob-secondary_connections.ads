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
--with Safir.Dob.Consumer;
with Safir.Dob.Connection_Bases;
--with Safir.Dob.Typesystem;

package Safir.Dob.Secondary_Connections is

   -- A secondary connection attached to a "real" connection.
   --
   -- This class is used to attach yourself to an existing connection in the
   -- same thread.
   -- All attach calls ensure that you will get a connection that is valid in the current thread, but
   -- each Secondary_Connection must still only be used from within one thread.
   --
   type Secondary_Connection is limited new
     Safir.Dob.Connection_Bases.Connection_Base with private;

   --type ClassAccess is access all Class'Class;

   -- Attach to a connection in this thread.
   --
   -- This method will attach the Secondary_Connection to the first Connection that was
   -- opened in this thread.
   --
   -- This method can be used to let part of a program, for example a module or a dll,
   -- attach to an existing open connection.
   --
   -- Exceptions: Not_Open_Exception - There is no open Connection in this thread.
   --
   procedure Attach
     (Self : in out Secondary_Connection);

   -- Attach to a named connection in this thread.
   --
   -- This method will attach the Secondary_Connection to the named Connection if that
   -- Connection was opened in this thread.
   --
   -- This method can be used to let part of a program, for example a module or a dll,
   -- attach to an existing open connection.
   -- The connection name parameters are used to identify the connection to attach to.
   -- This connection must already be opened, otherwise an exception will be thrown.
   --
   -- Parameters: Connection_Name_Common_Part - Name that identifies the connection
   --                                           but not any particular instance.
   --             Connection_Name_Instance_Part - Name that identifies a particular
   --                                             connection instance.
   -- Exceptions: Not_Open_Exception - The Connection instance we are trying to
   --                                  attach to is not open.
   procedure Attach
     (Self                          : in out Secondary_Connection;
      Connection_Name_Common_Part   : in Unbounded_Wide_String;
      Connection_Name_Instance_Part : in Unbounded_Wide_String);

   -- Detach a SecondaryConnection.
   --
   -- When a connection has been detached it can be attached again.
   --
   procedure Detach
     (Self : in out Secondary_Connection);

   -- Check if a Secondary_Connection is attached to an open connection.
   --
   -- Returns : True if the Secondary_Connection is attached to a Connection
   --           and that Connection is open.
   --
   function Is_Attached
     (Self : in Secondary_Connection) return Boolean;

   -----------------------
   -- Get_Controller_Id --
   -----------------------
   overriding
   function Get_Controller_Id (Self : in Secondary_Connection) return Safir.Dob.Defs.Controller_Id;

   overriding
   procedure Finalize (Self : in out Secondary_Connection);

private

   use type Safir.Dob.Defs.Controller_Id;

   type Secondary_Connection is new Safir.Dob.Connection_Bases.Connection_Base with record
      Controller_Id : Safir.Dob.Defs.Controller_Id := -1;
   end record;

end Safir.Dob.Secondary_Connections;
