-------------------------------------------------------------------------------
--
--  Copyright Saab AB, 2006-2013 (http://safir.sourceforge.net)
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
with Ada.Containers.Vectors;
with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;

package Safir.Application.Backdoors is

   type Backdoor is limited interface;

   type Backdoor_Access is access all Backdoor'Class;

   package Strings is new Ada.Containers.Vectors
     (Index_Type => Natural,
      Element_Type => Unbounded_Wide_String);

   procedure Handle_Command (Self           : in Backdoor;
                             Command_Tokens : in Strings.Vector) is abstract;

   function Get_Help_Text (Self : in Backdoor)
                          return Wide_String is abstract;

end Safir.Application.Backdoors;
