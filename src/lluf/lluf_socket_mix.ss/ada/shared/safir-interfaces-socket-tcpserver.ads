-------------------------------------------------------------------------------
--
--  Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
--
--  Created by: Henrik Sundberg / sthesu
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
with Ada.Finalization;
with Ada.Streams;
with GNAT.Sockets;
with Ada.Containers.Indefinite_Ordered_Sets;

package Safir.Interfaces.Socket.TcpServer is

   NotConnected      : exception;

   type Class is abstract new Ada.Finalization.Limited_Controlled with private;

   -- Is called when data is received from a client
   procedure Receive (Self  : in Class;
                      From  : in GNAT.Sockets.Socket_Type;
                      Data  : in Ada.Streams.Stream_Element_Array) is abstract;
   -- Is called when a client has connected
   procedure Connected (Self    : in Class;
                        From    : in GNAT.Sockets.Socket_Type;
                        Address : in GNAT.Sockets.Sock_Addr_Type) is abstract;
   -- Is called when a client has disconnected
   procedure Disconnected (Self : in Class) is abstract;
   --  Call this routine to start listining to connections from clients
   procedure Listen (Self  : in out Class;
                     On    : in     GNAT.Sockets.Port_Type;
                     Count : in     Natural := 15);
   --  Call this routine to disconnect a specific client
   procedure Disconnect (Self : in     Class;
                         From : in out GNAT.Sockets.Socket_Type);
   --  Call this routine to disconnect all clients and stop listening
   procedure Disconnect (Self : in Class);
   --  Call this routine to send data to a specific client
   procedure Send (Self : in Class;
                   To   : in GNAT.Sockets.Socket_Type;
                   Data : in Ada.Streams.Stream_Element_Array);
   -- Call this routine to set specific socket options
   procedure SetSocketOption (Self   : in Class;
                              Option : in GNAT.Sockets.Option_Type);

private

   type Server_State_T is (Stopped,
                           Starting,
                           Started,
                           Accepting,
                           Listening,
                           Stopping);

   procedure Initialize (Self : in out Class);
   procedure Finalize   (Self : in out Class);

   task type Receiver_Task_T (Self : access Class) is
      entry Start (Socket : in GNAT.Sockets.Socket_Type);
      entry Stop;
   end Receiver_Task_T;

   task type Accept_Task_T (Self : access Class) is
      entry Start;
      entry Listen (Address : in GNAT.Sockets.Sock_Addr_Type;
                    Count   : in Natural);
      entry Stop;
   end Accept_Task_T;

   type TaskAccess_T is access Receiver_Task_T;
   function "<" (L, R : in TaskAccess_T) return Boolean;
   package SetPkg is new Ada.Containers.Indefinite_Ordered_Sets
     (Element_Type => TaskAccess_T,
      "<"          => "<",
      "="          => "=");

   type Class is abstract new Ada.Finalization.Limited_Controlled with record
      State        : Server_State_T := Stopped;
      Socket       : GNAT.Sockets.Socket_Type;
      Clients      : SetPkg.Set;
      AcceptorTask : access Accept_Task_T;
   end record;

end Safir.Interfaces.Socket.TcpServer;


