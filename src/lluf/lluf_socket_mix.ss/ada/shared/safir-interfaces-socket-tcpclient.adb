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

package body Safir.Interfaces.Socket.TcpClient is

   procedure Logg (From    : in String;
                   Text    : in String;
                   NewLine : Boolean := True) is
                   pragma Unreferenced (From, Text, NewLine);
   begin
      null;
      --        Put (From & ":" & Text);
      --        if NewLine then
      --           New_Line;
      --        end if;
   end Logg;

--     function Image (Socket : in Socket_T) return String is
--     begin
--        return GNAT.Sockets.Image (GNAT.Sockets.Get_Peer_Name (Socket));
--     end Image;
--
--     function MyImage (Socket : in Socket_T) return String is
--     begin
--        return GNAT.Sockets.Image (GNAT.Sockets.Get_Socket_Name (Socket));
--     end MyImage;

   procedure CreateSocket (Socket : in out GNAT.Sockets.Socket_Type) is
   begin
      GNAT.Sockets.Create_Socket (Socket => Socket,
                                  Family => GNAT.Sockets.Family_Inet,
                                  Mode   => GNAT.Sockets.Socket_Stream);
      GNAT.Sockets.Set_Socket_Option (Socket => Socket,
                                      Option => (Name    => GNAT.Sockets.Receive_Timeout,
                                                 Timeout => 1.0));
   end CreateSocket;

   task body Connect_Task_T is
      Address     : GNAT.Sockets.Sock_Addr_Type;
      AutoConnect : Boolean   := False;
      Item        : Ada.Streams.Stream_Element_Array (0 .. 1000);
      Last        : Ada.Streams.Stream_Element_Offset;
      use type Ada.Streams.Stream_Element_Offset;
   begin
      accept Start;
      Self.State := Disconnected;
      while Self.State /= Stopping loop
         Put_Line ("ConnectTask:State: " & Client_State_T'Image (Self.State));

         select
            accept Stop;
            Self.State := Stopping;
         or
            when Self.State = Disconnected =>
               accept Connect (Server : in out GNAT.Sockets.Sock_Addr_Type;
                               Retry  : in     Boolean) do
                  Address     := Server;
                  AutoConnect := Retry;
                  Self.State  := Connecting;
               end Connect;
         or
            when Self.State = Connected =>
               accept Disconnect;
               AutoConnect := False;
               GNAT.Sockets.Close_Socket (Self.Socket);
               Class'Class (Self.all).Disconnected;
               Self.State := Disconnected;
         else
            case Self.State is

                  -- in state CONNECTING try to connect each second
               when Connecting =>
                  begin
                     Logg ("ConnectTask", "Connect_Socket...", False);
                     GNAT.Sockets.Connect_Socket (Socket  => Self.Socket,
                                                  Server  => Address);
                     Self.State := Connected;
                     Class'Class (Self.all).Connected (Self.Socket);
                     Logg ("ConnectTask", "Connected");
                  exception
                     when E : others =>
                        Put_Line ("ConnectTask: Connection refused " &
                                  Ada.Exceptions.Exception_Name (E) & ":" &
                                  Ada.Exceptions.Exception_Message (E));
                        if not AutoConnect then
                           Self.State := Disconnected;
                        else
                           delay 1.0;
                        end if;
                  end;

                  -- in state CONNECTED
               when Connected =>
                  Last := Item'First - 1;
                  begin
                     Logg ("ConnectTask", "Receive_Socket...", False);
                     GNAT.Sockets.Receive_Socket
                       (Socket => Self.Socket,
                        Item   => Item,
                        Last   => Last);
                     Logg ("ConnectTask", "Data received");
                  exception
                     when E : GNAT.Sockets.Socket_Error =>
                        case GNAT.Sockets.Resolve_Exception (E) is
                           when GNAT.Sockets.Connection_Timed_Out =>
                              null;
                           when others =>
                              Put_Line ("ConnectTask: Receive_Socket " &
                                        Ada.Exceptions.Exception_Name (E) & ":" &
                                        Ada.Exceptions.Exception_Message (E));
                              GNAT.Sockets.Close_Socket (Self.Socket);
                              CreateSocket (Self.Socket);
                              if AutoConnect then
                                 Self.State := Connecting;
                              else
                                 Self.State := Disconnected;
                              end if;
                              Class'Class (Self.all).Disconnected;
                        end case;
                  end;
                  if Last /= Item'First - 1 then
                     --  Data is received => Send to user
                     Class'Class (Self.all).Receive (Item (0 .. Last));
                  end if;

                  -- in other states
               when Stopping | Stopped | Disconnected =>
                  delay 1.0;

            end case;
         end select;

      end loop;
      Logg ("ConnectTask", "is Stopped");
      Self.State := Stopped;
   exception
      when E : others =>
         Logg ("ConnectTask:Terminating",
               Ada.Exceptions.Exception_Name (E) & ":" &
               Ada.Exceptions.Exception_Message (E));
   end Connect_Task_T;

   -------------
   -- Connect --
   -------------
   procedure Connect (Self    : in out Class;
                      Address : in     String;
                      Port    : in     GNAT.Sockets.Port_Type;
                      Retry   : in     Boolean := True) is
      Server : GNAT.Sockets.Sock_Addr_Type;
   begin
      if Self.State = Disconnected then
         CreateSocket (Self.Socket);
         Server.Addr := GNAT.Sockets.Addresses (GNAT.Sockets.Get_Host_By_Name (Address), 1);
         Server.Port := Port;
         Self.ConnectTask.Connect (Server, Retry);
      end if;
   end Connect;

   ----------------
   -- Disconnect --
   ----------------
   procedure Disconnect (Self : in Class) is
   begin
      Logg ("Client:Disconnect", "");
      if Self.State = Connected then
         Self.ConnectTask.Disconnect;
      end if;
   end Disconnect;

   ----------
   -- Send --
   ----------
   procedure Send
     (Self : in Class;
      Data : in Ada.Streams.Stream_Element_Array)
   is
      Last : Ada.Streams.Stream_Element_Offset;
      use type Ada.Streams.Stream_Element_Offset;
   begin
      if Self.State = Connected then
         Logg ("Client:Send", "Send...", False);
         GNAT.Sockets.Send_Socket
           (Socket => Self.Socket,
            Item   => Data,
            Last   => Last);
         Logg ("Client:Send", "Sent");
         if (Last /= Data'Last) then
            Logg ("Client:Send", "ERROR!");
         end if;
      else
         raise NotConnected;
      end if;
   exception
      when E : others =>
         Logg ("Client:Send", Ada.Exceptions.Exception_Name (E) & ":" & Ada.Exceptions.Exception_Message (E));
         raise;
   end Send;

   -----------------
   -- IsConnected --
   -----------------
   function IsConnected (Self : in Class) return Boolean is
   begin
      return Self.State = Connected;
   end IsConnected;

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
      Self.ConnectTask := new Connect_Task_T (Self'Unchecked_Access);
      Self.ConnectTask.Start;
      delay 0.2;
   end Initialize;

   procedure Finalize   (Self : in out Class) is
   begin
      Logg ("Client:Finalize", "Close_Socket");
      Self.ConnectTask.Stop;
      GNAT.Sockets.Close_Socket (Self.Socket);
   exception
      when others =>
         null;
   end Finalize;

begin
   GNAT.Sockets.Initialize;
end Safir.Interfaces.Socket.TcpClient;
