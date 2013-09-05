-------------------------------------------------------------------------------
--
--  Copyright Saab AB, 2013 (http://www.safirsdk.com)
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
with Interfaces.C;
with Safir.Dob.Typesystem.Library_Exceptions;

package body Safir.Application.Crash_Reporter is
   package C renames Interfaces.C;

   procedure Start is
      procedure SwreC_StartCrashReporting (C_Success : out C.char);
      pragma Import (C, SwreC_StartCrashReporting, "SwreC_StartCrashReporting");

      L_Success : C.char;
   begin
      SwreC_StartCrashReporting (C_Success => L_Success);
      if not (C.char'Pos (L_Success) /= 0) then
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      end if;
   end Start;


   procedure Stop is
      procedure SwreC_StopCrashReporting;
      pragma Import (C, SwreC_StopCrashReporting, "SwreC_StopCrashReporting");
   begin
      SwreC_StopCrashReporting;
   end Stop;
end Safir.Application.Crash_Reporter;
