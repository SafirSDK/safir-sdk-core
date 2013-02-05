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
with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Characters.Wide_Latin_1; use Ada.Characters.Wide_Latin_1;
with Ada.Containers;
with Ada.Strings.Fixed;
with GNAT.Regexp;
with GNAT.Wide_String_Split;

with Safir.Application.Backdoor_Command;
with Safir.Dob.Node_Parameters;
with Safir.Dob.This_Node_Parameters;
with Safir.Dob.Typesystem;
with Safir.Dob.Connection_Aspect_Miscs;
with Safir.Dob.Typesystem.Channel_Id;
with Safir.Sw_Reports.Sw_Report; use Safir.Sw_Reports.Sw_Report;

package body Safir.Application.Backdoor_Keepers is

   --package Strings is new Ada.Containers.Vectors
   --   (Index_Type => Natural,
   --    Element_Type => Unbounded_Wide_String);

--   Pi_Cmd_Object_Id : constant Safir.Dob.Typesystem.ObjectId :=
--     (Safir.Application.Backdoor_Command.ClassId,
--      Safir.Dob.Typesystem.WHOLE_CLASS);

   function To_String
      (Text   : in Unbounded_Wide_String;
       Length : in Natural               := 0)
      return String;

   procedure Tokenize (Cmd        : in     Unbounded_Wide_String;
                       Cmd_Tokens :    out Safir.Application.Backdoors.Strings.Vector);


   ---------------
   -- To_String --
   ---------------
   function To_String
      (Text   : in Unbounded_Wide_String;
       Length : in Natural               := 0)
      return   String
   is
   begin
      if Length = 0 then
         return To_String (To_Wide_String (Text));
      else
         declare
            Tmp : String (1 .. Length) := (others => ' ');
         begin
            Ada.Strings.Fixed.Move
              (To_String (To_Wide_String (Text)),
               Tmp,
               Ada.Strings.Right);
            return Tmp;
         end;
      end if;
   end To_String;

   --------------
   -- Tokenize --
   --------------
   procedure Tokenize (Cmd        : in     Unbounded_Wide_String;
                       Cmd_Tokens :    out Safir.Application.Backdoors.Strings.Vector) is

      Cmd_Set : GNAT.Wide_String_Split.Slice_Set;
   begin
      GNAT.Wide_String_Split.Create (Cmd_Set,
                                     To_Wide_String (Cmd),
                                     " " & HT & LF & CR,
                                     GNAT.Wide_String_Split.Multiple);

      for I in 1 .. GNAT.Wide_String_Split.Slice_Count (Cmd_Set) loop
         Cmd_Tokens.Append (To_Unbounded_Wide_String
                                (GNAT.Wide_String_Split.Slice (Cmd_Set, I)));
      end loop;
   end Tokenize;

   ----------------------------------------------------------------------------
   -- Start
   ----------------------------------------------------------------------------
   procedure Start (Self     : in out Backdoor_Keeper;
                    Backdoor : in     not null Safir.Application.Backdoors.Backdoor_Access) is
   begin
      Start (Self,
             Backdoor,
             To_Unbounded_Wide_String (""),
             To_Unbounded_Wide_String (""));

   end Start;

   procedure Start (Self       : in out Backdoor_Keeper;
                    Backdoor   : in     not null Safir.Application.Backdoors.Backdoor_Access;
                    Connection_Name_Common_Part   : in Unbounded_Wide_String;
                    Connection_Name_Instance_Part : in Unbounded_Wide_String) is
   begin
      Stop (Self);

      if Length (Connection_Name_Common_Part) = 0 and Length (Connection_Name_Instance_Part) = 0 then
         Self.Connection.Attach;
      else
         Self.Connection.Attach (Connection_Name_Common_Part, Connection_Name_Instance_Part);
      end if;

      Self.Backdoor := Backdoor;
      Self.Connection.Subscribe_Message
        (Type_Id => Safir.Application.Backdoor_Command.Class_Type_Id,
         Channel_Id => Safir.Dob.Typesystem.Channel_Id.Default_Channel,
         Message_Subscriber => Self'Access);

      Self.Started := True;
   end Start;

   ----------------------------------------------------------------------------
   -- Stop
   ----------------------------------------------------------------------------
   procedure Stop (Self : in out Backdoor_Keeper) is
      --use Safir.Dob.Connection;
   begin
      if not Self.Started then
         -- Never started
         return;  -- *** RETURN ***
      end if;

      if not Self.Connection.Is_Attached then
         -- Connection has been closed.
         return;
      end if;


      Self.Connection.Unsubscribe_Message
         (Type_Id => Safir.Application.Backdoor_Command.Class_Type_Id,
          Channel_Id => Safir.Dob.Typesystem.Channel_Id.Default_Channel,
          Message_Subscriber => Self'Access);

      Self.Connection.Detach;
      Self.Backdoor := null;
      Self.Started := False;
   end Stop;

   ----------------------------------------------------------------------------
   -- Is_Started
   ----------------------------------------------------------------------------
   function Is_Started (Self : in Backdoor_Keeper) return Boolean is
   begin
      return Self.Started;
   end Is_Started;

   ----------------------------------------------------------------------------
   -- OnMessage
   ----------------------------------------------------------------------------
   procedure On_Message (Self          : in out Backdoor_Keeper;
                         Message_Proxy : in     Safir.Dob.Message_Proxies.Message_Proxy) is
      use type Ada.Containers.Count_Type;

      Cmd : constant Safir.Application.Backdoor_Command.Smart_Pointer :=
         Safir.Application.Backdoor_Command.Smart_Pointer (Message_Proxy.Get_Message);
      Cmd_Tokens : Safir.Application.Backdoors.Strings.Vector;
   begin

      if not Cmd.Ref.all.Node_Name.Is_Null then
         if not GNAT.Regexp.Match
                        (To_String (Safir.Dob.Node_Parameters.Nodes (Safir.Dob.This_Node_Parameters.Node_Number).Ref.all.Node_Name.Get_Val),
                         GNAT.Regexp.Compile
                                 (Pattern => To_String (Cmd.Ref.all.Node_Name.Get_Val),
                                  Case_Sensitive => False)) then
            -- Node name doesn't match
            return;  -- *** RETURN ***
         end if;
      end if;

      if not Cmd.Ref.all.Connection_Name.Is_Null then
         if not GNAT.Regexp.Match
            (To_String (Safir.Dob.Connection_Aspect_Miscs.Create
                           (Self.Connection).Get_Connection_Name),
             GNAT.Regexp.Compile
                (Pattern => To_String (Cmd.Ref.all.Connection_Name.Get_Val),
                 Case_Sensitive => False)) then
            -- Connection name doesn't match
            return;  -- *** RETURN ***
         end if;
      end if;

      if Cmd.Ref.all.Command.Is_Null then
         -- No command given
         return; -- *** RETURN ***
      end if;

      -- Ok, it seems that this PI-command is for this application

      Tokenize (Cmd.Ref.all.Command.Get_Val, Cmd_Tokens);

      if Cmd_Tokens.Length > 0 then
         if To_String (Cmd_Tokens.First_Element) = "ping" then
            -- It's a 'ping' command. Answer to it without bothering
            -- the subclass implementator.
            Send_Program_Info_Report ("Ping reply");
            return; -- *** RETURN ***

         elsif To_String (Cmd_Tokens.First_Element) = "help" then
            -- Get help text from subclass implementator.
            Send_Program_Info_Report (Self.Backdoor.all.Get_Help_Text);
            return; -- *** RETURN ***

         end if;
      end if;

      --  Let the subclass handle the command
      Self.Backdoor.all.Handle_Command (Cmd_Tokens);

   exception
      when GNAT.Regexp.Error_In_Regexp =>
         --  An invalid regular expression was used, skip this command
         return;
   end On_Message;

end Safir.Application.Backdoor_Keepers;
