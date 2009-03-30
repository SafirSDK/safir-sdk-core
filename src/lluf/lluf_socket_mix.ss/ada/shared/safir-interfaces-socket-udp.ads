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

package Safir.Interfaces.Socket.Udp is

   type Class is abstract new Ada.Finalization.Limited_Controlled with private;

   procedure Receive (Self : in Class;
                      From : in String;
                      Port : in GNAT.Sockets.Port_Type;
                      Data : in Ada.Streams.Stream_Element_Array) is abstract;

   procedure AcceptDataOn (Self      : in out Class;
                           OnAddress : in     String;
                           Port      : in     GNAT.Sockets.Port_Type);
   procedure Send (Self : in Class;
                   To   : in String;
                   Port : in GNAT.Sockets.Port_Type;
                   Data : in Ada.Streams.Stream_Element_Array);
   procedure SetSocketOption (Self   : in Class;
                              Option : in GNAT.Sockets.Option_Type;
                              Level  : in GNAT.Sockets.Level_Type := GNAT.Sockets.Socket_Level);
   procedure JoinMulticastGroup (Self   : in Class;
                                 Group  : in String);


private


   type State_T is (Stopped,
                    Starting,
                    Started,
                    Stopping);

   procedure Initialize (Self : in out Class);
   procedure Finalize   (Self : in out Class);

   task type Receiver_Task_T (Self : access Class) is
      entry Start;
      entry BindSocket (Address : in GNAT.Sockets.Sock_Addr_Type);
      entry SetSocketOption (Level  : in GNAT.Sockets.Level_Type;
                             Option : in GNAT.Sockets.Option_Type);
      entry Stop;
   end Receiver_Task_T;

   type Class is abstract new Ada.Finalization.Limited_Controlled with record
      State        : State_T                    := Stopped;
      pragma shared (State);
      Socket       : GNAT.Sockets.Socket_Type;
      ReceiveTask  : access Receiver_Task_T;
   end record;

end Safir.Interfaces.Socket.Udp;


