-------------------------------------------------------------------------------
--
--  Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
--
--  Created by: Henrik Sundberg / sthesu
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

with Templates_Parser;
with GNAT.Spitbol; use GNAT.Spitbol;
with Dots.String_Sets;

package Dots.State is

   Max_Outputs : constant := 100;

   type Unit_Type is (Unknown, Class, Property, Enum, Xeption);

   type Output_Config is record
      Name                  : VString;
      Full_Name             : VString;
      File_Extension        : VString;
      Filename_Separator    : VString;
      Output_Directory      : VString;
      Namespace_Separator   : VString;
      Lowercase_Namespace   : Boolean;
      Lowercase_Filenames   : Boolean;
      Create_Parents        : Boolean;
      Object_Type           : VString;
      Index_Type            : VString;
      Exception_Set         : Templates_Parser.Translate_Set;
      Exception_List        : VString;
      Type_Set              : Templates_Parser.Translate_Set;
      Type_List             : VString;
      Dependencies          : Dots.String_Sets.Set;
   end record;

   Defined_Outputs : Natural := 0;
   Current_Output : Natural := 0;
   Outputs     : array (1 .. Max_Outputs) of Output_Config;
   Current_Unit : VString;

   Log_Parsing     : Boolean := False;
   Log_Info        : Boolean := False;
   Log_Tokens      : Boolean := False;
   Log_Output_Type : VString;

end Dots.State;
