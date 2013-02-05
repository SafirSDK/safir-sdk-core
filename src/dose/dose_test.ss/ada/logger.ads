-------------------------------------------------------------------------------
--
--  Copyright Saab AB, 2009 (http://www.safirsdk.com)
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
with Ada.Strings.Wide_Unbounded;
with Safir.Dob.Connection_Info;

package Logger is
   --  procedure Put_Line (Str : in String);

   function To_String (Conn_Info : in Safir.Dob.Connection_Info.Smart_Pointer)
     return Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

   procedure Put_Line (Str : in Wide_String);

   procedure Put_Line
     (Str : in Ada.Strings.Wide_Unbounded.Unbounded_Wide_String);

   procedure Put (Str : in Wide_String);

   procedure Put
     (Str : in Ada.Strings.Wide_Unbounded.Unbounded_Wide_String);

   procedure New_Line;

   function Get_String return
     Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

   procedure Clear;
end Logger;
