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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C;

package body Safir.Logging is
   package C renames Interfaces.C;

   type Int_32 is range -2 ** 31 .. 2 ** 31 - 1;
   for Int_32'Size use 32;

   type UInt is mod 2 ** 32;

   --------------
   -- To_Utf_8 --
   --------------

   function To_Utf_8 (W : in Unbounded_Wide_String) return Unbounded_String is
      Tmp : Unbounded_String := To_Unbounded_String (Length (W) * 4);
      Last : Natural := 0;

      procedure Put_Char (Pos : in UInt);
      procedure Put_Char (Pos : in UInt)
      is
      begin
         if Pos < 16#80# then
            Last := Last + 1;
            Replace_Element (Tmp, Last, Character'Val (Pos));
         elsif Pos < 16#800# then
            Last := Last + 1;
            Replace_Element (Tmp, Last, Character'Val (2#1100_0000# + (Pos / 2 ** 6)));
            Last := Last + 1;
            Replace_Element (Tmp, Last, Character'Val (2#1000_0000# + (Pos and 2#0011_1111#)));
         elsif Pos < 16#10000# then
            Last := Last + 1;
            Replace_Element (Tmp, Last, Character'Val (2#1110_0000# + (Pos / 2 ** 12)));
            Last := Last + 1;
            Replace_Element (Tmp, Last, Character'Val (2#1000_0000# + ((Pos / 2 ** 6) and 2#0011_1111#)));
            Last := Last + 1;
            Replace_Element (Tmp, Last, Character'Val (2#1000_0000# + (Pos and 2#0011_1111#)));
         else
            Last := Last + 1;
            Replace_Element (Tmp, Last, Character'Val (2#1111_0000# + (Pos / 2 ** 18)));
            Last := Last + 1;
            Replace_Element (Tmp, Last, Character'Val (2#1000_0000# + ((Pos / 2 ** 12) and 2#0011_1111#)));
            Last := Last + 1;
            Replace_Element (Tmp, Last, Character'Val (2#1000_0000# + ((Pos / 2 ** 6) and 2#0011_1111#)));
            Last := Last + 1;
            Replace_Element (Tmp, Last, Character'Val (2#1000_0000# + (Pos and 2#0011_1111#)));
         end if;

      end Put_Char;
   begin
      for J in 1 .. Length (W) loop
         Put_Char (UInt (Wide_Character'Pos (Element (W, J))));
      end loop;

      --  Put_Line ("UTF8 returns : """ & Tmp (1 .. Last) & '"');

      return Unbounded_Slice (Tmp, 1, Last);
   end To_Utf_8;

   function To_Utf_8 (W : in Unbounded_Wide_String) return String
   is
   begin
      return To_String (To_Utf_8 (W));
   end To_Utf_8;

   function Convert (Str : in Wide_String) return Interfaces.C.char_array is
   begin
      return Interfaces.C.To_C (To_Utf_8 (To_Unbounded_Wide_String (Str)));
   end Convert;

   procedure Send_System_Log (Severity : in Severity_T;
                              Message  : in Wide_String) is
      procedure LoggingC_SendSystemLog (CSeverity : in Int_32;
                                        CMessage  : in C.char_array);
      pragma Import (C, LoggingC_SendSystemLog, "LoggingC_SendSystemLog");
   begin
      LoggingC_SendSystemLog (Severity_T'Pos (Severity),
                              Convert (Message));
   end Send_System_Log;

end Safir.Logging;
