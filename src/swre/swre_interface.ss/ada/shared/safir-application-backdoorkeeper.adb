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
with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;
with GNAT.Regexp;
with GNAT.Wide_String_Split;

with Safir.Application.BackdoorCommand;
with Safir.Dob.NodeParameters;
with Safir.Dob.ThisNodeParameters;
with Safir.Dob.Typesystem;
with Safir.SwReports.SwReport; use Safir.SwReports.SwReport;

package body Safir.Application.BackdoorKeeper is

   --package Strings is new Ada.Containers.Vectors
   --   (Index_Type => Natural,
   --    Element_Type => Unbounded_Wide_String);

   Pi_Cmd_Object_Id : constant Safir.Dob.Typesystem.ObjectId :=
     (Safir.Application.BackdoorCommand.ClassId,
      Safir.Dob.Typesystem.WHOLE_CLASS);

   function To_String
      (Text   : in Unbounded_Wide_String;
       Length : in Natural               := 0)
      return String;

   procedure Tokenize (Cmd        : in     Unbounded_Wide_String;
                       Cmd_Tokens :    out Safir.Application.Backdoor.Strings.Vector);


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
                       Cmd_Tokens :    out Safir.Application.Backdoor.Strings.Vector) is

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
   procedure Start (Self     : in out Class;
                    Backdoor : in     not null Safir.Application.Backdoor.ClassAccess) is
   begin
     if Self.Started then
       return;
     end if;

     Self.Connection.Attach;
     Self.Backdoor := Backdoor;
     Self.Connection.SubscribeMessage (Pi_Cmd_Object_Id,
                                       Self'Access);
     Self.Started := True;

   end Start;

   ----------------------------------------------------------------------------
   -- Stop
   ----------------------------------------------------------------------------
   procedure Stop (Self : in out Class) is
      --use Safir.Dob.Connection;
   begin
      if not Self.Started then
         -- Never started
         return;  -- *** RETURN ***
      end if;

      Self.Connection.UnsubscribeMessage (Pi_Cmd_Object_Id, Self'Access);
      Self.Connection.Detach;
      Self.Backdoor := null;
      Self.Started := False;
   end Stop;


   ----------------------------------------------------------------------------
   -- OnMessage
   ----------------------------------------------------------------------------
   procedure OnMessage (Self    : in out Class;
                        Message : in     Safir.Dob.Message.Class) is
     use type Ada.Containers.Count_Type;

      Cmd : Safir.Application.BackdoorCommand.Class;
      Cmd_Tokens : Safir.Application.Backdoor.Strings.Vector;
   begin
      if not Message.IsOfType (Safir.Application.BackdoorCommand.ClassId) then
            -- Unexpected message
            return;   -- *** RETURN ***
      end if;

      Cmd := Safir.Application.BackdoorCommand.Clone (Message);

      if not Cmd.IsNullNodeName then
         if not GNAT.Regexp.Match
                        (To_String (Safir.Dob.NodeParameters.GetNodes (Safir.Dob.ThisNodeParameters.GetNodeNumber).GetNodeName),
                         GNAT.Regexp.Compile
                                 (Pattern => To_String (Cmd.GetNodeName),
                                  Case_Sensitive => False)) then
            -- Node name doesn't match
            return;  -- *** RETURN ***
         end if;
      end if;

      if not Cmd.IsNullConnectionName then
         if not GNAT.Regexp.Match
           (To_String (Self.Connection.GetConnectionName),
                     GNAT.Regexp.Compile
                                 (Pattern => To_String (Cmd.GetConnectionName),
                                  Case_Sensitive => False)) then
            -- Connection name doesn't match
            return;  -- *** RETURN ***
         end if;
      end if;

      if Cmd.IsNullCommand then
         -- No command given
         return; -- *** RETURN ***
      end if;

      -- Ok, it seems that this PI-command is for this application

      Tokenize (Cmd.GetCommand, Cmd_Tokens);

      if Cmd_Tokens.Length > 0 then
         if To_String (Cmd_Tokens.First_Element) = "ping" then
            -- It's a 'ping' command. Answer to it without bothering
            -- the subclass implementator.
            Send_Program_Info_Report ("Ping reply");
            return; -- *** RETURN ***

         elsif To_String (Cmd_Tokens.First_Element) = "help" then
            -- Get help text from subclass implementator.
            Send_Program_Info_Report (Self.Backdoor.GetHelpText);
            return; -- *** RETURN ***

         end if;
      end if;

      --  Let the subclass handle the command
      Self.Backdoor.HandleCommand (Cmd_Tokens);

   exception
      when GNAT.Regexp.Error_In_Regexp =>
         --  An invalid regular expression was used, skip this command
         return;
   end OnMessage;

end Safir.Application.BackdoorKeeper;
