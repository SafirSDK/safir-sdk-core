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
with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Sax.Readers;    use Sax.Readers;
with Sax.Exceptions; use Sax.Exceptions;
with Sax.Locators;   use Sax.Locators;
with Sax.Attributes; use Sax.Attributes;
with Unicode.CES;    use Unicode.CES;
with Unicode;        use Unicode;

with Dots.Parser;
with Dots.Xmlreaders.Unit;

package body Dots.Xmlreaders is

   use type Dots.Parser.Element_Type_T;

   Indent_Str : constant String :=
     ("                                                                 ");
   Indentation : Integer := 0;


   ----------------
   -- Characters --
   ----------------

   procedure Characters
     (Handler : in out Reader; Ch : Unicode.CES.Byte_Sequence) is
      pragma Warnings (Off, Handler);
   begin
      if not Handler.Silent then
         Put (Indent_Str (1 .. Indentation + 4));
         Put_Line ("Sax.Characters (" & Ch & ','
                   & Integer'Image (Ch'Length) & ") at "
                   & To_String (Handler.Locator.all));
      end if;
      Dots.Parser.Characters (Ch);
   end Characters;

   ------------------
   -- End_Document --
   ------------------

   procedure End_Document (Handler : in out Reader) is
      pragma Warnings (Off, Handler);
   begin
      if not Handler.Silent then
         Put_Line ("Sax.End_Document ()");
      end if;
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
      pragma Warnings (Off, Handler);

      Left_Element_Type : Dots.Parser.Element_Type_T;

   begin
      Dots.Parser.End_Element (Local_Name, Left_Element_Type);

      if not Handler.Effort_Only then
         Dots.Xmlreaders.Unit.End_Element
           (Local_Name => Local_Name,
            Left_Element_Type => Left_Element_Type);
      end if;

      if not Handler.Silent then
         Put (Indent_Str (1 .. Indentation));
         Put_Line ("Sax.End_Element (" & Local_Name & ") at "
                   & To_String (Handler.Locator.all));
         Indentation := Indentation - 4;
      end if;

   exception
      when Error_Found =>
         Dots.Parser.Error :=
           To_String (Handler.Locator.all) & ": " & Dots.Parser.Error;
         raise;

      when Error_Completed =>
         raise Error_Found;

      when E : others =>
         Dots.Parser.Error := To_Unbounded_String
           (To_String (Handler.Locator.all) & ": " &
         Ada.Exceptions.Exception_Message (E));
         raise Error_Found;
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
      raise Error_Found;
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
      raise Error_Found;
   end Fatal_Error;

   --------------------------
   -- Set_Document_Locator --
   --------------------------

   procedure Set_Document_Locator
     (Handler : in out Reader;
      Loc     : access Sax.Locators.Locator'Class) is
   begin
      Handler.Locator := Locator_Access (Loc);
   end Set_Document_Locator;

   ---------------------
   -- Set_Effort_Only --
   ---------------------

   procedure Set_Effort_Only
     (Handler : in out Reader; Effort_Only : Boolean) is
   begin
      Handler.Effort_Only := Effort_Only;
   end Set_Effort_Only;

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
      pragma Warnings (Off, Handler);
   begin
      if not Handler.Silent then
         Indentation := 0;
         Put_Line ("Sax.Start_Document ()");
      end if;
      Dots.Parser.Start_Document;
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
      pragma Unreferenced (Qname, Atts);
      pragma Warnings (Off, Handler);

      Entered_Element_Type : Dots.Parser.Element_Type_T;
   begin
      if not Handler.Silent then
         Indentation := Indentation + 4;
         Put (Indent_Str (1 .. Indentation));
         Put_Line ("Sax.Start_Element: " & Local_Name & "  (" &
                   Namespace_URI & ")");
      end if;

      Dots.Parser.Start_Element
        (Namespace_URI, Local_Name, Entered_Element_Type);

      if not Handler.Effort_Only then
         Dots.Xmlreaders.Unit.Start_Element
           (Local_Name => Local_Name,
            Entered_Element_Type => Entered_Element_Type);
      end if;

   exception
      when Error_Found =>
         Dots.Parser.Error :=
           To_String (Handler.Locator.all) & ": " & Dots.Parser.Error;
         raise;

      when Error_Completed =>
         raise Error_Found;

      when E : others =>
         Dots.Parser.Error := To_Unbounded_String (
           To_String (Handler.Locator.all) & ": " &
         Ada.Exceptions.Exception_Message (E));
         raise Error_Found;
   end Start_Element;


   -------------
   -- Warning --
   -------------

   procedure Warning
     (Handler : in out Reader;
      Except : Sax.Exceptions.Sax_Parse_Exception'Class)
   is
      pragma Unreferenced (Handler);
   begin
      Dots.Parser.Error := To_Unbounded_String (Get_Message (Except));
      raise Error_Found;
   end Warning;

end Dots.Xmlreaders;
