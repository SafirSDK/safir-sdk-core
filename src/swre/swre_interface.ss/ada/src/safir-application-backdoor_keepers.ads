-------------------------------------------------------------------------------
--
--  Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
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
with Safir.Application.Backdoors;
with Safir.Dob.Secondary_Connections;
with Safir.Dob.Consumers;
with Safir.Dob.Message_Proxies;
with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;

package Safir.Application.Backdoor_Keepers is

   type Backdoor_Keeper is limited new Safir.Dob.Consumers.Message_Subscriber with private;

   type Backdoor_Keeper_Access is access all Backdoor_Keeper'Class;

   -- Starts subscription for Program Information commands to be sent to the Backdoor.
   --
   -- A backdoor will be established for the "first" connection that is opened in
   -- the thread that calls this method. (That is, a secondary connection Attach is used
   -- internally)
   --
   -- If the main connection is closed and opened again (maybe in a different context),
   -- this method must be called again.
   --
   -- Restarting/pausing is supported by calling stop and then start again.
   --
   -- Parameters: Backdoor - Type that implements the Backdoor interface
   -- Exceptions: Not_Open_Exception - 'Start' was called before connect to Dob.
   --
   procedure Start (Self       : in out Backdoor_Keeper;
                    Backdoor   : in     not null Safir.Application.Backdoors.Backdoor_Access);

   -- Starts subscription for Program Information commands to be sent to the Backdoor using
   -- the given named connection.
   --
   -- A backdoor will be established for the named connection that is opened in
   -- the thread that calls this method.
   --
   -- If the main connection is closed and opened again (maybe in a different context),
   -- this method must be called again.
   --
   -- Restarting/pausing is supported by calling stop and then start again.
   --
   -- Parameters: Backdoor - Type that implements the Backdoor interface
   -- Connection_Name_Common_Part - Name that identifies the connection
   --                               but not any particular instance.
   -- Connection_Name_Instance_Part - Name that identifies a particular
   --                                 connection instance.
   -- Exceptions: Not_Open_Exception - 'Start' was called before connect to Dob.
   --
   procedure Start (Self       : in out Backdoor_Keeper;
                    Backdoor   : in     not null Safir.Application.Backdoors.Backdoor_Access;
                    Connection_Name_Common_Part   : in Unbounded_Wide_String;
                    Connection_Name_Instance_Part : in Unbounded_Wide_String);

   procedure Stop (Self : in out Backdoor_Keeper);

   function Is_Started (Self : in Backdoor_Keeper) return Boolean;

   --  Must not be overridden by users.
   procedure On_Message (Self          : in out Backdoor_Keeper;
                         Message_Proxy : in     Safir.Dob.Message_Proxies.Message_Proxy);

private

   type Backdoor_Keeper is limited new Safir.Dob.Consumers.Message_Subscriber with
      record
        Connection  : Safir.Dob.Secondary_Connections.Secondary_Connection;
        Started : Boolean := False;
        Backdoor : Safir.Application.Backdoors.Backdoor_Access := null;
      end record;

end Safir.Application.Backdoor_Keepers;
