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

pragma Elaborate_All (Templates_Parser);

package Dots.State is

   Max_Outputs : constant := 100;

   type Unit_Type is (Unknown, Class, Property, Enum, Xeption);
   type Underscore_Style is (Add, Remove, Keep);
   type Case_Style is (Upper, Lower, Camel, Keep);
   type Style is record
      Underscore : Underscore_Style;
      Choose_Case : Case_Style;
   end record;

   type Output_Config is record
      Name                  : VString;
      Full_Name             : VString;
      File_Suffix           : VString;
      Filename_Separator    : VString;
      Output_Directory      : VString;
      Namespace_Separator   : VString;
      Namespace_Prefix_File_Suffix : VString;
      Namespace_Style       : Style;
      Filename_Style        : Style;
      Classname_Style       : Style;
      Membername_Style      : Style;
      Enum_Value_Style      : Style;
      Create_Parents        : Boolean;
      Parent_Filename       : VString;
      Object_Type           : VString;
      Index_Type            : VString;
      Exception_Set         : Templates_Parser.Translate_Set;
      Exception_List        : VString;
      Type_Set              : Templates_Parser.Translate_Set;
      Type_List             : VString;
      Namspace_Prefix_Set   : Templates_Parser.Translate_Set;
      Namspace_Prefix_Used  : Boolean;
      Dependencies          : Dots.String_Sets.Set;
      Dependencies_Base     : Dots.String_Sets.Set;
   end record;

   Defined_Outputs : Natural := 0;
   Current_Output : Natural := 0;
   Outputs     : array (1 .. Max_Outputs) of Output_Config;
   Current_Unit : VString;
   Dou_Dir : VString;


   Log_Parsing     : Boolean := False;
   Log_Info        : Boolean := False;
   Log_Tokens      : Boolean := False;
   Log_Output_Type : VString;

end Dots.State;
