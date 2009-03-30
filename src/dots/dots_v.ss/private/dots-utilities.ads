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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Dots.State;

package Dots.Utilities is

   procedure Make_Dir (Name : in String);
   --  Makes sure that the directory for the file/directory exist

   procedure Create_Namespace_Parents
     (Namespace : String);

   function Adjust_Filecase (Name : in String) return String;

   function Get_Unit_Type
     (Unit : String) return Dots.State.Unit_Type;

   function Get_Local_Member_Type
     (Unit   : String;
      Member : String) return String;

   function Get_Uniform_Member_Type
     (Unit   : String;
      Member : String) return String;

   function Get_Member_Unit
     (Super_Unit : String;
      Member     : String) return String;

   function Is_Array_Member
     (Unit   : String;
      Member : String) return Boolean;

   procedure Define_Unit
     (Unit       : in Unbounded_String;
      Unit_Type  : in Dots.State.Unit_Type;
      Base_Class : in Unbounded_String);

   procedure Define_Member
     (Unit            : in Unbounded_String;
      Member          : in Unbounded_String;
      Xml_Member_Type : in Unbounded_String;
      Is_Array        : in Boolean);

   function Local_Type_Tag_Of
     (Xml_Type : String) return String;

   function Uniform_Type_Tag_Of
     (Xml_Type : in String) return String;

   function Replace_Dots
     (S         : String;
      Separator : String;
      Lower     : Boolean := False) return String;

   function Format_Summary
     (Summary : in Unbounded_String) return Unbounded_String;

end Dots.Utilities;
