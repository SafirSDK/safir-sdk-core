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

with GNAT.Spitbol;
use GNAT.Spitbol;

package body Dots.File_Map is

   function Img (A : VString) return String is
   begin
      return S (A);
   end Img;

   use type VString;

   package Dou_File_Map is new GNAT.Spitbol.Table (Value_Type => VString,
                                                  Null_Value => GNAT.Spitbol.Nul,
                                                  Img => Img,
                                                  "=" => "=");

   Dou_Table : Dou_File_Map.Table (5000);

   procedure Include (Key : in String; Value : in String) is
   begin
      Dou_File_Map.Set (T     => Dou_Table,
                       Name  => Key,
                       Value => V (Value));

   end Include;

   function Get_Value (Key : in String) return String is
      Value : constant VString := Dou_File_Map.Get (T => Dou_Table, Name => Key);
   begin
      return S (Value);
   end Get_Value;


end Dots.File_Map;
