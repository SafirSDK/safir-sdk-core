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
with Ada.Exceptions;
with Text_IO; use Text_IO;

package body Safir.Interfaces.Socket.TcpServer is

   procedure Logg (From    : in String;
                   Text    : in String;
                   NewLine : Boolean := True) is
   begin
      null;
      --        Put (From & ":" & Text);
      --        if NewLine then
      --           New_Line;
      --        end if;
   end Logg;

   function Image (Socket : in GNAT.Sockets.Socket_Type) return String is
   begin
      return GNAT.Sockets.Image (GNAT.Sockets.Get_Peer_Name (Socket));
   end Image;

   function MyImage (Socket : in GNAT.Sockets.Socket_Type) return String is
   begin
      return GNAT.Sockets.Image (GNAT.Sockets.Get_Socket_Name (Socket));
   end MyImage;

   task body Receiver_Task_T is
      type State_T is (Stopped, Disconnected, Connected, Stopping);
      State       : State_T := Stopped;
      TheSocket   : GNAT.Sockets.Socket_Type;
      Address     : GNAT.Sockets.Sock_Addr_Type;
      Item        : Ada.Streams.Stream_Element_Array (0 .. 1000);
      Last        : Ada.Streams.Stream_Element_Offset;
   begin
      State := Disconnected;
      Put_Line ("ReceiverTask:State: " & State_T'Image (State));
      while State /= Stopping loop
         select
            accept Stop;
            State := Stopping;
         or
            accept Start (Socket : in GNAT.Sockets.Socket_Type) do
               TheSocket := Socket;
               GNAT.Sockets.Set_Socket_Option (Socket => Self.Socket,
                                               Option => (Name    => GNAT.Sockets.Receive_Timeout,
                                                          Timeout => 1.0));
               State := Connected;
            end Start;
         else
            if State = Connected then
               begin
                  Logg ("ReceiveTask", "Receive_Socket...", False);
                  GNAT.Sockets.Receive_Socket
                    (Socket => TheSocket,
                     Item   => Item,
                     Last   => Last,
                     From   => Address);
                  Class'Class (Self.all).Receive (TheSocket, Item (0 .. Last));
                  Logg ("ReceiveTask", "Data received");
               exception
                  when E : GNAT.Sockets.Socket_Error =>
                     case GNAT.Sockets.Resolve_Exception (E) is
                        when GNAT.Sockets.Connection_Timed_Out =>
                           null;
                        when others =>
                           Put_Line ("ConnectTask: Receive_Socket " &
                                     Ada.Exceptions.Exception_Name (E) & ":" &
                                     Ada.Exceptions.Exception_Message (E));
                           State := Disconnected;
                     end case;
               end;
            else
               delay 1.0;
            end if;
         end select;
      end loop;
      State := Stopped;
   end Receiver_Task_T;


   task body Accept_Task_T is
      TheAddress,
      Address      : GNAT.Sockets.Sock_Addr_Type;
      Length       : Natural := 1;
      Socket       : GNAT.Sockets.Socket_Type;
      ReceiverTask : TaskAccess_T;
   begin
      Self.State := Starting;
      accept Start;
      Self.State := Started;
      while Self.State /= Stopping loop
         Put_Line ("AcceptTask:State: " & Server_State_T'Image (Self.State));
         select
            accept Stop;
            Self.State := Stopping;
         or
            accept Listen (Address : in GNAT.Sockets.Sock_Addr_Type;
                           Count   : in Natural) do
               TheAddress := Address;
               Length := Count;
            end Listen;
            Self.State := Accepting;
         else
            case Self.State is
               when Accepting =>
                  Logg ("Server:Listen", "Bind_Socket...", False);
                  GNAT.Sockets.Bind_Socket (Socket  => Self.Socket,
                                            Address => TheAddress);
                  Logg ("Server:Listen", "Bind_Socket");
                  Logg ("Server:Listen", "Listen_Socket...", False);
                  GNAT.Sockets.Listen_Socket (Socket => Self.Socket,
                                              Length => Length);
                  Logg ("Server:Listen", "Listen_Socket");
                  Logg ("AcceptTask", "Accept_Socket " & MyImage (Self.Socket) & "...", False);
                  GNAT.Sockets.Accept_Socket (Server  => Self.Socket,
                                              Socket  => Socket,
                                              Address => Address);
                  Logg ("AcceptTask", "Accepted:" & Image (Socket));
                  --  Notify the user that a client has connected
                  Class'Class (Self.all).Connected (Socket, Address);
                  --  Insert the connected socket into the set of clients
                  ReceiverTask := new Receiver_Task_T (Self);
                  SetPkg.Insert (Self.Clients, ReceiverTask);
                  ReceiverTask.Start (Socket);
                  Self.State := Listening;
               when others =>
                  null;
            end case;
         end select;
      end loop;
      Self.State := Stopped;
   exception
      when E : others =>
         Logg ("AcceptTask:Terminating",
               Ada.Exceptions.Exception_Name (E) & ":" &
               Ada.Exceptions.Exception_Message (E));
   end Accept_Task_T;

   -- Listen --
   ------------
   procedure Listen
     (Self  : in out Class;
      On    : in     GNAT.Sockets.Port_Type;
      Count : in     Natural := 15)
   is
      Address : GNAT.Sockets.Sock_Addr_Type;
   begin
      Address.Addr := GNAT.Sockets.Addresses (GNAT.Sockets.Get_Host_By_Name (GNAT.Sockets.Host_Name), 1);
      Address.Port := On;
      GNAT.Sockets.Set_Socket_Option
        (Socket => Self.Socket,
         Level  => GNAT.Sockets.Socket_Level,
         Option => (GNAT.Sockets.Reuse_Address, True));
      Self.AcceptorTask.Listen (Address, Count);
   end Listen;

   ----------------
   -- Disconnect --
   ----------------
   procedure Disconnect
     (Self : in     Class;
      From : in out GNAT.Sockets.Socket_Type)
   is
      pragma Unreferenced (Self, From);
   begin
      null;
   end Disconnect;

   ----------------
   -- Disconnect --
   ----------------
   procedure Disconnect (Self : in Class) is
   begin
      null;
   end Disconnect;

   ----------
   -- Send --
   ----------
   procedure Send
     (Self : in Class;
      To   : in GNAT.Sockets.Socket_Type;
      Data : in Ada.Streams.Stream_Element_Array)
   is
      pragma Unreferenced (Self);
      Last : Ada.Streams.Stream_Element_Offset;
      use type Ada.Streams.Stream_Element_Offset;
   begin
      Logg ("Server:Send", "Send...", False);
      GNAT.Sockets.Send_Socket
        (Socket => To,
         Item   => Data,
         Last   => Last);
      Logg ("Server:Send", "Sent");
      if (Last /= Data'Last) then
         Logg ("Server:Send", "ERROR!");
      end if;
   exception
      when E : others =>
         Logg ("Server:Send", Ada.Exceptions.Exception_Name (E) & ":" & Ada.Exceptions.Exception_Message (E));
   end Send;

   ---------------------
   -- SetSocketOption --
   ---------------------
   procedure SetSocketOption (Self   : in Class;
                              Option : in GNAT.Sockets.Option_Type) is
   begin
      GNAT.Sockets.Set_Socket_Option (Socket       => Self.Socket,
                                      Option       => Option);
   end SetSocketOption;


   procedure Initialize (Self : in out Class) is
   begin
      Self.State := Stopped;
      GNAT.Sockets.Create_Socket (Self.Socket);
      Self.AcceptorTask := new Accept_Task_T (Self'Unchecked_Access);
      Self.AcceptorTask.Start;
   end Initialize;

   procedure Finalize   (Self : in out Class) is
   begin
      Logg ("Server:Finalize", "Close_Socket");
      Self.AcceptorTask.Stop;
      GNAT.Sockets.Close_Socket (Self.Socket);
   exception
      when others =>
         null;
   end Finalize;

   function "<" (L, R : in TaskAccess_T) return Boolean is
      pragma Unreferenced (L, R);
   begin
      return True;
   end "<";

begin
   GNAT.Sockets.Initialize;
end Safir.Interfaces.Socket.TcpServer;
