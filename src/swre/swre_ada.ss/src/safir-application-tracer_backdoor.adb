-------------------------------------------------------------------------------
--
--  Copyright Saab AB, 2013 (http://safir.sourceforge.net)
--
--  Created by: Lars Hagström / lars.hagstrom@consoden.se
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
with Ada.Command_Line;
with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;
with Interfaces.C;
with Safir.Dob.Connection_Aspect_Miscs;
with Safir.Dob.Typesystem.Library_Exceptions;
with Safir.Dob.Typesystem.Utilities;

package body Safir.Application.Tracer_Backdoor is
   package C renames Interfaces.C;

   function Convert (Str : in Unbounded_Wide_String) return Interfaces.C.char_array is
      function ToUtf8 (WStr : in Unbounded_Wide_String) return String renames Safir.Dob.Typesystem.Utilities.To_Utf_8;
   begin
      return Interfaces.C.To_C (ToUtf8 (Str));
   end Convert;

   procedure Start (Connection : in Safir.Dob.Connection_Bases.Connection_Base) is
      procedure SwreC_StartTraceBackdoor (Connection_Name_Common_Part   : in     C.char_array;
                                          Connection_Name_Instance_Part : in     C.char_array;
                                          C_Success                     :    out C.char);
      pragma Import (C, SwreC_StartTraceBackdoor, "SwreC_StartTraceBackdoor");

      Misc : constant Safir.Dob.Connection_Aspect_Miscs.Connection_Aspect_Misc :=
        Safir.Dob.Connection_Aspect_Miscs.Create (Connection);
      L_Success : C.char;
   begin
      SwreC_StartTraceBackdoor (Convert (Misc.Get_Connection_Name_Common_Part),
                                Convert (Misc.Get_Connection_Name_Instance_Part),
                                C_Success => L_Success);
      if not (C.char'Pos (L_Success) /= 0) then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
   end Start;


   procedure Stop is
      procedure SwreC_StopTraceBackdoor;
      pragma Import (C, SwreC_StopTraceBackdoor, "SwreC_StopTraceBackdoor");
   begin
      SwreC_StopTraceBackdoor;
   end Stop;
end Safir.Application.Tracer_Backdoor;
