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

with Ada.Text_IO;

with Ada.Characters.Handling;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded.Hash;

with GNAT.Directory_Operations;
with GNAT.OS_Lib;
with GNAT.Spitbol; use GNAT.Spitbol;

with Input_Sources.File;
with Sax.Readers;

with Templates_Parser;
with Dots.Member_Reader;

package body Dots.Utilities is

   Empty_Dir : constant String := GNAT.Directory_Operations.Dir_Name ("");

   type Unit_Info_T is record
      Baseclass : VString;
      Unit_Type : Dots.State.Unit_Type := Dots.State.Unknown;
   end record;

   package Handled_Unit is new Ada.Containers.Hashed_Maps
     (Key_Type => VString,
      Element_Type => Unit_Info_T,
      Hash => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => Ada.Strings.Unbounded."=");

   type Member_Info_T is record
      Xml_Type : VString;
      Is_Array : Boolean;
   end record;

   package Handled_Member is new Ada.Containers.Hashed_Maps
     (Key_Type => VString,
      Element_Type => Member_Info_T,
      Hash => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => Ada.Strings.Unbounded."=");

   Unit_Map : Handled_Unit.Map;

   Member_Map : Handled_Member.Map;

   procedure Setup_Unit (Name : in String);

   function Adjust_Filecase (Name : in String) return String is
      Conf : Dots.State.Output_Config renames
        Dots.State.Outputs (Dots.State.Current_Output);
   begin
      if Conf.Lowercase_Filenames then
         return Ada.Characters.Handling.To_Lower (Name);
      else
         return Name;
      end if;
   end Adjust_Filecase;

   ------------------------------
   -- Create_Namespace_Parents --
   ------------------------------

   procedure Create_Namespace_Parents
     (Namespace : in String) is
      Conf : Dots.State.Output_Config renames
        Dots.State.Outputs (Dots.State.Current_Output);

      Continue : Boolean;
      Sep_String : constant String := S (Conf.Filename_Separator);
      Separator : constant Character := Sep_String (Sep_String'First);

      procedure Create_Parent
        (Namespace : in String;
         Continue  : out Boolean);

      procedure Create_Parent
        (Namespace : in String;
         Continue  : out Boolean) is
         File_Name : String := Namespace;
      begin
         Continue := False;
         if Namespace = "" then
            return;
         end if;

         for K in File_Name'Range loop
            if File_Name (K) = '.' then
               File_Name (K) := Separator;
            end if;
         end loop;

         declare
            Full_Name : constant String := Adjust_Filecase (
              S (Conf.Output_Directory) & File_Name &
              '.' & S (Conf.File_Extension));
            Translations : constant Templates_Parser.Translate_Table :=
              (1  => Templates_Parser.Assoc ("SECTION", "Parent"),
               2  => Templates_Parser.Assoc ("NAMESPACE", Namespace));
            Trans_Set    : constant Templates_Parser.Translate_Set :=
              Templates_Parser.To_Set (Translations);
            Result       : constant String :=
              Templates_Parser.Parse
                (Filename     => S (Conf.Full_Name),
                 Translations => Trans_Set,
                 Cached       => True);
            F : Ada.Text_IO.File_Type;

         begin
            if not GNAT.OS_Lib.Is_Regular_File (Full_Name) and then
            Result /= "" then
               Continue := True;
               Ada.Text_IO.Create (F, Name => Full_Name);
               Ada.Text_IO.Put (F, Result);
               Ada.Text_IO.Close (F);
            end if;
         exception
            when others =>
               --  A paralell process is probable creating the file.
               Ada.Text_IO.Put_Line
                 ("--- Ignoring exception while creating: " & Full_Name);
         end;
      end Create_Parent;

      My_NS : constant String := Namespace & '.';

   begin
      if not Conf.Create_Parents then
         return;
      end if;

      for K in reverse My_NS'Range loop
         if My_NS (K) = '.' then
            Create_Parent (My_NS (My_NS'First .. K - 1),
                           Continue);
            exit when not Continue;
         end if;
      end loop;
   end Create_Namespace_Parents;

   -------------------
   -- Define_Member --
   -------------------

   procedure Define_Member
     (Unit            : in Unbounded_String;
      Member          : in Unbounded_String;
      Xml_Member_Type : in Unbounded_String;
      Is_Array        : in Boolean) is

      Position : Handled_Member.Cursor;
      Inserted : Boolean;
   begin
      Handled_Member.Insert
        (Container => Member_Map,
         Key       => Unit & '-' & Member,
         New_Item  => (Xml_Type  => Xml_Member_Type,
                       Is_Array => Is_Array),
         Position  => Position,
         Inserted  => Inserted);
--        if Inserted then
--           Ada.Text_IO.Put_Line
--             ("Member: " & S (Unit & '-' & Member)
--              & "; Type: " & S (Xml_Member_Type)
--              & "; Is_Array: " & Boolean'Image (Is_Array));
--        end if;
   end Define_Member;

   -----------------
   -- Define_Unit --
   -----------------

   procedure Define_Unit
     (Unit       : in Unbounded_String;
      Unit_Type  : in Dots.State.Unit_Type;
      Base_Class : in Unbounded_String) is
      Position : Handled_Unit.Cursor;
      Inserted : Boolean;
   begin
      Handled_Unit.Insert
        (Container => Unit_Map,
         Key       => Unit,
         New_Item  => (Baseclass => Base_Class, Unit_Type => Unit_Type),
         Position  => Position,
         Inserted  => Inserted);
--        if Inserted then
--           Ada.Text_IO.Put_Line
--             ("Unit: " & S (Unit) & "; BaseClass: " & S (Base_Class)
--              & "; Unit_Type: " & Dots.State.Unit_Type'Image (Unit_Type));
--        end if;
   end Define_Unit;

   --------------------
   -- Format_Summary --
   --------------------

   function Format_Summary
     (Summary : in Unbounded_String) return Unbounded_String
   is
      Tmp : constant String := S (Summary);
      First : Integer := Tmp'First;
      Last  : Integer := Tmp'Last;
      Keep_First : Boolean := True;
      Indentation : Integer := Tmp'Length + 1;
      Cur         : Integer := 0;
      Skip_Mode   : Boolean := False;
      Result : Unbounded_String;

   begin

      Keep_First := Tmp'Length > 0 and then
        (Tmp (First) /= ' ' and Tmp (First) /= ASCII.LF);

      --  Remove first line if it is empty
      while First <= Last and then Tmp (First) = ' ' loop
         First := First + 1;
      end loop;
      if First <= Last and then Tmp (First) = ASCII.LF then
         First := First + 1;
      end if;

      --  Remove last line if it is empty
      while Last >= First and then Tmp (Last) = ' ' loop
         Last := Last - 1;
      end loop;
      if Last >= First and then Tmp (Last) = ASCII.LF then
         Last := Last - 1;
      end if;

      --  Just return if it was an empty string.
      if Last < First then
         return V ("");
      end if;

      --  Calculate the used indentation.
      --  The indentation is defined as the shortest leading sequence
      --  of spaces, that is followed by somthing else than LF.
      --  Don't bother about checking the first line if it began
      --  immediately after the summary tag.
      for I in First .. Last loop
         if Skip_Mode then
            if Tmp (I) = ASCII.LF then
               Skip_Mode := False;
            end if;
         else
            if Tmp (I) = ' ' then
               Cur := Cur + 1;
            else
               if Tmp (I) /= ASCII.LF then
                  if Cur < Indentation then
                     if not (I = First and Keep_First) then
                        Indentation := Cur;
                     end if;
                  end if;
                  Skip_Mode := True;
               end if;
               Cur := 0;
            end if;
         end if;
      end loop;

      --  Return if there is no indentation.
      if Indentation = 0 or Indentation > Tmp'Length then
         return V (Tmp (First .. Last));
      end if;

      --  Ok, remove the indentation!
      Cur := 0;
      Skip_Mode := Keep_First;
      for I in First .. Last loop
         Cur := Cur + 1;

         if Cur > Indentation or Tmp (I) = ASCII.LF or Skip_Mode then
            Result := Result & Tmp (I);
         end if;

         if Tmp (I) = ASCII.LF then
            Cur := 0;
            Skip_Mode := False;
         end if;
      end loop;

      return Result;
   end Format_Summary;

   ---------------------------
   -- Get_Local_Member_Type --
   ---------------------------

   function Get_Local_Member_Type
     (Unit   : String;
      Member : String) return String
   is
   begin
      Setup_Unit (Unit);
      return Local_Type_Tag_Of
        (S (Handled_Member.Element
              (Handled_Member.Find
                 (Container => Member_Map,
                  Key       => V (Unit & '-' & Member))).Xml_Type));
   end Get_Local_Member_Type;

   ---------------------
   -- Get_Member_Unit --
   ---------------------

   function Get_Member_Unit
     (Super_Unit : String;
      Member     : String) return String
   is
      use type Handled_Member.Cursor;
      Unit : VString := V (Super_Unit);
   begin
      while S (Unit) /= "" loop
         Setup_Unit (S (Unit));
         if Handled_Member.Find
           (Container => Member_Map,
            Key       => Unit & '-' & Member)
           /= Handled_Member.No_Element then
            return S (Unit);
         end if;
         Unit := Handled_Unit.Element
           (Handled_Unit.Find
              (Container => Unit_Map,
               Key       => Unit)).Baseclass;
      end loop;
      return "";
   end Get_Member_Unit;

   -----------------------------
   -- Get_Uniform_Member_Type --
   -----------------------------

   function Get_Uniform_Member_Type
     (Unit   : String;
      Member : String) return String
   is
   begin
      Setup_Unit (Unit);
--    Ada.Text_IO.Put_Line ("UNIFORM (" & Unit & '-' & Member & "): " &
--                              Uniform_Type_Tag_Of
--                                (S (Handled_Member.Element
--                                      (Handled_Member.Find
--                                         (Container => Member_Map,
--                                      Key       => V (Unit & '-' & Member)))
--                                    .Xml_Type)));
      return Uniform_Type_Tag_Of
        (S (Handled_Member.Element
              (Handled_Member.Find
                 (Container => Member_Map,
                  Key       => V (Unit & '-' & Member))).Xml_Type));
   end Get_Uniform_Member_Type;

   -------------------
   -- Get_Unit_Type --
   -------------------

   function Get_Unit_Type (Unit : String) return Dots.State.Unit_Type
   is
   begin
      Setup_Unit (Unit);
      return Handled_Unit.Element
        (Handled_Unit.Find
           (Container => Unit_Map,
            Key       => V (Unit))).Unit_Type;
   exception
      when Error_Found =>
         raise;
      when Error_Completed =>
         raise;
      when others =>
         return Dots.State.Unknown;
   end Get_Unit_Type;

   ---------------------
   -- Is_Array_Member --
   ---------------------

   function Is_Array_Member
     (Unit   : String;
      Member : String) return Boolean
   is
   begin
      Setup_Unit (Unit);
--        Ada.Text_IO.Put_Line
--          ("Is_Array_Member (" & Unit & '-' & Member & "): "
--           & Boolean'Image (Handled_Member.Element
--             (Handled_Member.Find
--                (Container => Member_Map,
--                 Key       => V (Unit & '-' & Member))).Is_Array));
      return Handled_Member.Element
        (Handled_Member.Find
           (Container => Member_Map,
            Key       => V (Unit & '-' & Member))).Is_Array;
   exception
      when others =>
         return False;
   end Is_Array_Member;

   -----------------------
   -- Local_Type_Tag_Of --
   -----------------------

   function Local_Type_Tag_Of
     (Xml_Type : String) return String is

      use type Templates_Parser.Association;
      Conf : Dots.State.Output_Config renames
        Dots.State.Outputs (Dots.State.Current_Output);
      Asso : constant Templates_Parser.Association
        := Templates_Parser.Get (Conf.Type_Set, Xml_Type);
      Tmp : String := Xml_Type;
      Dot_Found : Boolean := False;
   begin
      if Asso = Templates_Parser.Null_Association then

         if Conf.Lowercase_Namespace then
            for J in reverse Tmp'Range loop
               if Dot_Found then
                  Tmp (J) := Ada.Characters.Handling.To_Lower (Tmp (J));
               elsif Tmp (J) = '.' then
                  Dot_Found := True;
               end if;
            end loop;
         end if;

         return Replace_Dots
           (S         => Tmp,
            Separator => S (Conf.Namespace_Separator));
      else
         return Templates_Parser.Item
           (Templates_Parser.Get (Asso),
            1);
      end if;
   end Local_Type_Tag_Of;

   --------------
   -- Make_Dir --
   --------------

   procedure Make_Dir (Name : in String) is
      Dir_Name : constant String :=
        GNAT.Directory_Operations.Dir_Name (Name);

      Dir : GNAT.Directory_Operations.Dir_Type;
   begin
      --  End of recursion?
      if Dir_Name = Empty_Dir then
         return;
      end if;

      begin
         --  Open the directory to see if directory exist
         GNAT.Directory_Operations.Open (Dir, Dir_Name);
         GNAT.Directory_Operations.Close (Dir);

         --  Yes it did
         return;
      exception
         when others =>
            --  No, make sure the upper directories exist
            Make_Dir (Dir_Name (Dir_Name'First .. Dir_Name'Last - 1));

            --  .. and then create the one asked for
            GNAT.Directory_Operations.Make_Dir (Dir_Name);
      end;
   end Make_Dir;

   ------------------
   -- Replace_Dots --
   ------------------

   function Replace_Dots
     (S : in String;
      Separator : in String;
      Lower : in Boolean := False) return String is
      Ch : Character;
      tmp : VString;
   begin
      for J in S'Range loop
         Ch := S (J);

         if Lower then
            if Ch in 'A' .. 'Z' then
               Ch := Character'Val (Character'Pos (Ch) + 32);
            end if;
         end if;

         if Ch = '.' or Ch = '/' then
            tmp := V (GNAT.Spitbol.S (tmp) & Separator);
         else
            tmp := V (GNAT.Spitbol.S (tmp) & Ch);
         end if;
      end loop;

      return GNAT.Spitbol.S (tmp);
   end Replace_Dots;

   ----------------
   -- Setup_Unit --
   ----------------

   procedure Setup_Unit (Name : in String)
     is
      use type Handled_Unit.Cursor;

      procedure Parse (S : in String);
      procedure Parse (S : in String) is

         Silent : constant Boolean := not Dots.State.Log_Parsing;

         My_Reader  : Dots.Member_Reader.Reader;
         Read : Input_Sources.File.File_Input;

      begin

         Input_Sources.File.Open
           (Filename => S,
            Input    => Read);

         Dots.Member_Reader.Set_Silent (My_Reader, Silent);

         --  If True, xmlns:* attributes will be reported in Start_Element
         Dots.Member_Reader.Set_Feature
           (My_Reader, Sax.Readers.Namespace_Feature, True);
         Dots.Member_Reader.Set_Feature
           (My_Reader, Sax.Readers.Namespace_Prefixes_Feature, False);
         Dots.Member_Reader.Set_Feature
           (My_Reader, Sax.Readers.Validation_Feature, False);

         Dots.Member_Reader.Parse (My_Reader, Read);
         Input_Sources.File.Close (Read);

      end Parse;

      Dou_File : constant String := Name & ".dou";

   begin
      if Handled_Unit.Find
        (Container => Unit_Map,
         Key       => V (Name)) /= Handled_Unit.No_Element then
         return;
      end if;

      Parse (Dou_File);
   end Setup_Unit;

   -------------------------
   -- Uniform_Type_Tag_Of --
   -------------------------

   function Uniform_Type_Tag_Of
     (Xml_Type : in String) return String is
      use type Templates_Parser.Association;
      Conf : Dots.State.Output_Config renames
        Dots.State.Outputs (Dots.State.Current_Output);
      Asso : constant Templates_Parser.Association
        := Templates_Parser.Get (Conf.Type_Set, Xml_Type);
   begin
      if Asso = Templates_Parser.Null_Association then
         case Dots.Utilities.Get_Unit_Type (Xml_Type) is
            when Dots.State.Unknown |
                 Dots.State.Property |
                 Dots.State.Xeption =>
               return "";
            when Dots.State.Class =>
               return Templates_Parser.Item
                 (Templates_Parser.Get
                    (Templates_Parser.Get
                       (Conf.Type_Set, S (Conf.Object_Type))),
                  2);
            when Dots.State.Enum =>
               return "Enumeration";
         end case;
      else
         return Templates_Parser.Item
           (Templates_Parser.Get (Asso),
            2);
      end if;
   end Uniform_Type_Tag_Of;


begin
   Dots.Utilities.Define_Unit
     (Unit       => V ("Object"),
      Unit_Type  => Dots.State.Class,
      Base_Class => Nul);

   Dots.Utilities.Define_Member
     (Unit            => V ("Object"),
      Member          => V ("Instance"),
      Xml_Member_Type => V ("Int32"),
      Is_Array        => False);

end Dots.Utilities;
