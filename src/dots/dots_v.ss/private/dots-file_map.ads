-------------------------------------------------------------------------------
--
--  Copyright Saab AB, 2010 (http://www.safirsdk.com)
--
--  Created by: Mikael Wennerberg / stmiwn
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

package Dots.File_Map is

   procedure Include(Key : in String; Value : in String);
   --
   -- Add Key/Value pair to file map.

   function Get_Value(Key : in String) return String;
   --
   -- Get the value that corresponds to the given key from file map.

end Dots.File_Map;
