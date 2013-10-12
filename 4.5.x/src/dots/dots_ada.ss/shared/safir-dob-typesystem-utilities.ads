-------------------------------------------------------------------------------
--
--  Copyright Saab AB, 2009-2013 (http://safir.sourceforge.net)
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

package Safir.Dob.Typesystem.Utilities is
   pragma Preelaborate (Safir.Dob.Typesystem.Utilities);

   function From_Utf_8 (Utf_8 : in String) return Unbounded_Wide_String;

   function To_Utf_8 (W : in Unbounded_Wide_String) return String;

   function To_Utf_8 (W : in Unbounded_Wide_String) return Unbounded_String;


   function Binary_To_Base_64 (Value : Safir.Dob.Typesystem.Binary_Vectors.Vector)
                              return String;

   function Base_64_To_Binary (Value : String)
                              return Safir.Dob.Typesystem.Binary_Vectors.Vector;

   function Generate_64_Bit_Hash (Id : in Unbounded_Wide_String) return Int_64;

   function Generate_64_Bit_Hash (Id : in String) return Int_64;

   -- Converts a C-string to an Ada String.
   function To_Ada (Char_Ptr : in Safir.Dob.Typesystem.Char_Star;
                    Size     : in Safir.Dob.Typesystem.Int_32) return String;

   -- Copies Source to the array pointed to by Destination.
   procedure Copy (Destination : in Safir.Dob.Typesystem.Char_Star;
                   Source      : in C.char_array);

end Safir.Dob.Typesystem.Utilities;
