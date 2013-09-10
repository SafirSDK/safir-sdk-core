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
with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;
with Safir.Application.Backdoors;
with Safir.Dob.Connection_Bases;
with Safir.Dob.Consumers;
with Safir.Dob.Message_Proxies;
with Safir.Dob.Secondary_Connections;

package Safir.Application.Backdoor_Keepers is

   type Backdoor_Keeper is limited new Safir.Dob.Consumers.Message_Subscriber with private;

   type Backdoor_Keeper_Access is access all Backdoor_Keeper'Class;

   -- Starts subscription for backdoor commands to be sent to the Backdoor.
   --
   -- If the connection is closed and opened again (maybe in a different context)
   -- this method must be called again to establish the subscription.
   --
   -- The class supports restarting/pausing by calling stop and then start again.
   --
   -- Parameters: Backdoor - Type that implements the Backdoor interface
   --             Connection - The connection on which the keeper will subscribe
   --                          to backdoor messages.
   -- Exceptions: Not_Open_Exception - 'Start' was called before connect to Dob.
   --
   procedure Start (Self       : in out Backdoor_Keeper;
                    Connection : in     Safir.Dob.Connection_Bases.Connection_Base'Class;
                    Backdoor   : in     not null Safir.Application.Backdoors.Backdoor_Access);

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
