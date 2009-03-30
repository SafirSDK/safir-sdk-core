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
with Text_IO; use Text_IO;
with Ada.Exceptions;

package body Safir.Interfaces.Socket.Udp is

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


   --------------------
   -- CreateSocket --
   --------------------
   procedure CreateSocket (Socket : in out GNAT.Sockets.Socket_Type) is
   begin
      GNAT.Sockets.Create_Socket (Socket => Socket,
                                  Family => GNAT.Sockets.Family_Inet,
                                  Mode   => GNAT.Sockets.Socket_Datagram);
      GNAT.Sockets.Set_Socket_Option (Socket => Socket,
                                      Option => (Name    => GNAT.Sockets.Receive_Timeout,
                                                 Timeout => 1.0));
   end CreateSocket;

   --------------------
   -- AcceptDataOn --
   --------------------
   procedure AcceptDataOn
     (Self           : in out Class;
      OnAddress      : in String;
      Port           : in GNAT.Sockets.Port_Type) is
      Address : GNAT.Sockets.Sock_Addr_Type;
   begin
      Self.ReceiveTask := new Receiver_Task_T (Self'Unchecked_Access);
      Self.ReceiveTask.Start;
      delay 0.2;
      Address.Port := Port;
      Address.Addr := GNAT.Sockets.Inet_Addr (OnAddress);
      Self.ReceiveTask.BindSocket (Address);
   end AcceptDataOn;

   ----------
   -- Send --
   ----------
   procedure Send
     (Self : in Class;
      To   : in String;
      Port : in GNAT.Sockets.Port_Type;
      Data : in Ada.Streams.Stream_Element_Array) is
      Last    : Ada.Streams.Stream_Element_Offset;
      Address : GNAT.Sockets.Sock_Addr_Type;
      use type Ada.Streams.Stream_Element_Offset;
   begin
      if Self.State = Started then
         Logg ("UDP:Send", "Send...", False);
         Address.Port := Port;
         Address.Addr := GNAT.Sockets.Inet_Addr (To);
         GNAT.Sockets.Send_Socket
           (Socket => Self.Socket,
            Item   => Data,
            To     => Address,
            Last   => Last);
         Logg ("UDP:Send", "Sent");
         if (Last /= Data'Last) then
            Logg ("UDP:Send", "ERROR!");
         end if;
      end if;
   exception
      when E : others =>
         Logg ("UDP:Send", Ada.Exceptions.Exception_Name (E) & ":" & Ada.Exceptions.Exception_Message (E));
         raise;
   end Send;

   ---------------------
   -- SetSocketOption --
   ---------------------
   procedure SetSocketOption
     (Self   : in Class;
      Option : in GNAT.Sockets.Option_Type;
      Level  : in GNAT.Sockets.Level_Type := GNAT.Sockets.Socket_Level) is
   begin
      if Self.ReceiveTask = null then
         GNAT.Sockets.Set_Socket_Option (Self.Socket, Level, Option);
      else

         Self.ReceiveTask.SetSocketOption (Level  => Level,
                                           Option => Option);
      end if;
   end SetSocketOption;

   ------------------------
   -- JoinMulticastGroup --
   ------------------------
   procedure JoinMulticastGroup (Self   : in Class;
                                 Group  : in String) is
   begin
      GNAT.Sockets.Set_Socket_Option
        (Socket => Self.Socket,
         Level  => GNAT.Sockets.IP_Protocol_For_IP_Level,
         Option => (GNAT.Sockets.Add_Membership,
                    GNAT.Sockets.Inet_Addr (Group),
                    GNAT.Sockets.Any_Inet_Addr));
   end JoinMulticastGroup;

   ----------------
   -- Initialize --
   ----------------
   procedure Initialize (Self : in out Class) is
   begin
      Logg ("UDP", "Initialize");
      CreateSocket (Self.Socket);
      Self.State := Stopped;
   end Initialize;

   --------------
   -- Finalize --
   --------------
   procedure Finalize (Self : in out Class) is
   begin
      Logg ("UDP", "Finalize");
      Self.ReceiveTask.Stop;
      GNAT.Sockets.Close_Socket (Self.Socket);
   exception
      when others =>
         null;
   end Finalize;

   ---------------------
   -- Receiver_Task_T --
   ---------------------
   task body Receiver_Task_T is
      Address     : GNAT.Sockets.Sock_Addr_Type;
      Item        : Ada.Streams.Stream_Element_Array (0 .. 1000);
      Last        : Ada.Streams.Stream_Element_Offset;
      use type Ada.Streams.Stream_Element_Offset;
   begin
      accept Start;
      Self.State := Starting;
      while Self.State /= Stopping loop
         --           Put_Line ("ReceiverTask:State: " & State_T'Image (Self.State));

         select
            accept Stop;
            Self.State := Stopping;
         or
            accept BindSocket (Address : in GNAT.Sockets.Sock_Addr_Type) do
               GNAT.Sockets.Bind_Socket (Self.Socket, Address);
            end BindSocket;
            Self.State := Started;
         or
            accept SetSocketOption (Level : in GNAT.Sockets.Level_Type;
                                    Option : in GNAT.Sockets.Option_Type) do
               GNAT.Sockets.Set_Socket_Option (Self.Socket, Level, Option);
            end SetSocketOption;
         else
            case Self.State is

               when Started =>
                  Last := Item'First - 1;
                  begin
                     Logg ("ReceiverTask", "Receive_Socket...", False);
                     GNAT.Sockets.Receive_Socket
                       (Socket => Self.Socket,
                        From   => Address,
                        Item   => Item,
                        Last   => Last);
                     Logg ("ReceiverTask", "Data received");
                  exception
                     when E : GNAT.Sockets.Socket_Error =>
                        case GNAT.Sockets.Resolve_Exception (E) is
                           when GNAT.Sockets.Connection_Timed_Out =>
                              null;
                           when others =>
                              Put_Line ("ReceiverTask: Receive_Socket " &
                                        Ada.Exceptions.Exception_Name (E) & ":" &
                                        Ada.Exceptions.Exception_Message (E));
                              GNAT.Sockets.Close_Socket (Self.Socket);
                              CreateSocket (Self.Socket);
                              Self.State := Started;
                        end case;
                  end;
                  if Last /= Item'First - 1 then
                     --  Data is received => Send to user
                     Class'Class (Self.all).Receive (From => GNAT.Sockets.Image (Address.Addr),
                                                     Port => Address.Port,
                                                     Data => Item (0 .. Last));
                  end if;


               when Stopping | Starting | Stopped =>
                  delay 1.0;

            end case;
         end select;

      end loop;
      Logg ("ReceiverTask", "is Stopped");
      Self.State := Stopped;
   exception
      when E : others =>
         Logg ("ReceiverTask:Terminating",
               Ada.Exceptions.Exception_Name (E) & ":" &
               Ada.Exceptions.Exception_Message (E));
   end Receiver_Task_T;

begin
   GNAT.Sockets.Initialize;
end Safir.Interfaces.Socket.Udp;
