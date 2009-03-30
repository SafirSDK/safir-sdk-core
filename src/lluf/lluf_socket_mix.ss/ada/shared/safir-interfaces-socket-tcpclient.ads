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

package Safir.Interfaces.Socket.TcpClient is

   NotConnected      : exception;

   type Class is abstract new Ada.Finalization.Limited_Controlled with private;

   -- Is called when data is received from the server
   procedure Receive (Self : in Class;
                      Data : in Ada.Streams.Stream_Element_Array) is abstract;
   -- Is called when a TCP connection to the server is established
   procedure Connected (Self : in Class;
                        To   : in GNAT.Sockets.Socket_Type) is abstract;
   -- Is called when the TCP connection to the server is disconnected
   procedure Disconnected (Self : in Class) is abstract;

   -- Call this routine to establish a TCP connection to a server
   procedure Connect (Self    : in out Class;
                      Address : in     String;
                      Port    : in     GNAT.Sockets.Port_Type;
                      Retry   : in     Boolean := True);
   -- Call this routine to disconnect a connection to a server
   procedure Disconnect (Self : in Class);
   -- Call this routine to send data to a server
   procedure Send (Self : in Class;
                   Data : in Ada.Streams.Stream_Element_Array);
   -- Call this routine to
   function IsConnected (Self : in Class) return Boolean;
   -- Call this routine to set specific socket options
   procedure SetSocketOption (Self   : in Class;
                              Option : in GNAT.Sockets.Option_Type);

private

   type Client_State_T is (Stopped,
                           Disconnected,
                           Connecting,
                           Connected,
                           Stopping);

   procedure Initialize (Self : in out Class);
   procedure Finalize   (Self : in out Class);

   task type Connect_Task_T (Self : access Class) is
      entry Start;
      entry Connect (Server : in out GNAT.Sockets.Sock_Addr_Type;
                     Retry  : in     Boolean);
      entry Disconnect;
      entry Stop;
   end Connect_Task_T;

   type Class is abstract new Ada.Finalization.Limited_Controlled with record
      State        : Client_State_T := Stopped;
      pragma shared (State);
      Socket       : GNAT.Sockets.Socket_Type;
      ConnectTask  : access Connect_Task_T;
   end record;

end Safir.Interfaces.Socket.TcpClient;


