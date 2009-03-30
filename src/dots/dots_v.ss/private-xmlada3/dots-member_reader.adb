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

with Ada.Exceptions;

with Sax.Readers;    use Sax.Readers;
with Sax.Exceptions; use Sax.Exceptions;
with Sax.Locators;   use Sax.Locators;
with Sax.Attributes; use Sax.Attributes;
with Unicode.CES;    use Unicode.CES;
with Unicode;        use Unicode;

with Dots.Parser;
with Dots.Utilities;

package body Dots.Member_Reader is

   ----------------
   -- Characters --
   ----------------

   procedure Characters
     (Handler : in out Reader; Ch : Unicode.CES.Byte_Sequence) is
   begin
      Handler.Char := Handler.Char & Ch;
   end Characters;

   ------------------
   -- End_Document --
   ------------------

   procedure End_Document (Handler : in out Reader) is
      pragma Warnings (Off, Handler);
   begin
      null;
   end End_Document;


   -----------------
   -- End_Element --
   -----------------

   procedure End_Element
     (Handler : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "") is
      pragma Unreferenced (Namespace_URI, Qname);
      use type Dots.State.Unit_Type;
      Stack : constant String := To_String (Handler.Stack);
      File_Name : constant Unbounded_String :=
                    To_Unbounded_String (Get_Public_Id (Handler.Locator));
   begin
      if Handler.Depth = 2 then
         if Local_Name = "name" then
            Handler.Unit_Name := Handler.Char;
            if Handler.Unit_Name & ".dou" /= File_Name then
               Dots.Parser.Error :=
                 To_Unbounded_String
                   ("(" & To_String (Handler.Unit_Name) & " /= " &
                    To_String (Head (File_Name,
                                     Length (File_Name) - 4)) & ")");
               raise Error_Found;
            end if;
            if Handler.Unit_Type = Dots.State.Enum or
              Handler.Unit_Type = Dots.State.Property then
               Dots.Utilities.Define_Unit
                 (Unit       => Handler.Unit_Name,
                  Unit_Type  => Handler.Unit_Type,
                  Base_Class => Ada.Strings.Unbounded.Null_Unbounded_String);
            end if;

         elsif Local_Name = "baseClass" then
            Dots.Utilities.Define_Unit
              (Unit       => Handler.Unit_Name,
               Unit_Type  => Handler.Unit_Type,
               Base_Class => Handler.Char);
         end if;

      elsif Stack = ".class.members.member.arraySize" or
        Stack = ".class.members.member.arraySizeRef" then
         Handler.Member_Is_Array := True;

      elsif Stack = ".class.members.member.name" then
         Handler.Member_Name := Handler.Char;

      elsif Stack = ".class.members.member.type" then
         Handler.Member_Type := Handler.Char;

      elsif Stack = ".class.members.member" then
         Dots.Utilities.Define_Member
           (Unit            => Handler.Unit_Name,
            Member          => Handler.Member_Name,
            Xml_Member_Type => Handler.Member_Type,
            Is_Array        => Handler.Member_Is_Array);

      end if;

      Head (Handler.Stack, Length (Handler.Stack) - 1 - Local_Name'Length);
      Handler.Depth := Handler.Depth - 1;
   exception
      when Error_Found =>
         Dots.Parser.Error :=
           To_String (Handler.Locator) & ": " & Dots.Parser.Error;
         raise Error_Completed;

      when E : others =>
         Dots.Parser.Error := File_Name
           & To_String (Handler.Locator) & ": " &
         Ada.Exceptions.Exception_Message (E);
         raise Error_Completed;
   end End_Element;

   -----------
   -- Error --
   -----------

   procedure Error
     (Handler : in out Reader;
      Except  : Sax.Exceptions.Sax_Parse_Exception'Class)
   is
      pragma Unreferenced (Handler);
   begin
      Dots.Parser.Error := To_Unbounded_String (Get_Message (Except));
      raise Error_Completed;
   end Error;

   -----------------
   -- Fatal_Error --
   -----------------

   procedure Fatal_Error
     (Handler : in out Reader;
      Except  : Sax.Exceptions.Sax_Parse_Exception'Class) is
      pragma Unreferenced (Handler);
   begin
      Dots.Parser.Error := To_Unbounded_String (Get_Message (Except));
      raise Error_Completed;
   end Fatal_Error;

   --------------------------
   -- Set_Document_Locator --
   --------------------------

   procedure Set_Document_Locator
     (Handler : in out Reader;
      Loc     : in out Sax.Locators.Locator) is
   begin
      Handler.Locator := Loc;
   end Set_Document_Locator;

   ----------------
   -- Set_Silent --
   ----------------

   procedure Set_Silent
     (Handler : in out Reader; Silent : Boolean) is
   begin
      Handler.Silent := Silent;
   end Set_Silent;

   --------------------
   -- Start_Document --
   --------------------

   procedure Start_Document (Handler : in out Reader) is
   begin
      Handler.Unit_Type := Dots.State.Unknown;
      Handler.Depth := 0;
      Handler.Stack := Ada.Strings.Unbounded.Null_Unbounded_String;
   end Start_Document;

   -------------------
   -- Start_Element --
   -------------------

   procedure Start_Element
     (Handler       : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class) is
      pragma Unreferenced (Namespace_URI, Qname, Atts);
      Stack : constant String := To_String (Handler.Stack) & "." & Local_Name;

   begin
      Handler.Depth := Handler.Depth + 1;
      Handler.Stack := Handler.Stack & "." & Local_Name;
      Handler.Char := Ada.Strings.Unbounded.Null_Unbounded_String;

--        Ada.Text_IO.Put_Line ("Depth:" & Integer'Image (Handler.Depth)
--                              & "; Stack: " & To_String (Handler.Stack));

      if Stack = ".class.members.member" then
         Handler.Member_Is_Array := False;
      end if;

      if Handler.Depth = 1 then
         if Local_Name = "class" then
            Handler.Unit_Type := Dots.State.Class;

         elsif Local_Name = "property" then
            Handler.Unit_Type := Dots.State.Property;

         elsif Local_Name = "enumeration" then
            Handler.Unit_Type := Dots.State.Enum;

         elsif Local_Name = "exception" then
            Handler.Unit_Type := Dots.State.Xeption;
         end if;
      end if;

   exception
      when Error_Found =>
         Dots.Parser.Error :=
           To_String (Handler.Locator) & ": " & Dots.Parser.Error;
         raise Error_Completed;

      when E : others =>
         Dots.Parser.Error := To_Unbounded_String
           (To_String (Handler.Locator) & ": " &
         Ada.Exceptions.Exception_Message (E));
         raise Error_Completed;
   end Start_Element;


   -------------
   -- Warning --
   -------------

   procedure Warning
     (Handler : in out Reader;
      Except : Sax.Exceptions.Sax_Parse_Exception'Class) is
      pragma Unreferenced (Handler);
   begin
      Dots.Parser.Error := To_Unbounded_String (Get_Message (Except));
      raise Error_Completed;
   end Warning;

end Dots.Member_Reader;
