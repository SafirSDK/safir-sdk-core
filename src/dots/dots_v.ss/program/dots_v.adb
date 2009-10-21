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
with Ada.Text_IO;

with GNAT.Command_Line;
with GNAT.Directory_Operations.Iteration;
with GNAT.OS_Lib;
with GNAT.Regpat;
with GNAT.Spitbol; use GNAT.Spitbol;

with Input_Sources.File;
with Sax.Readers;

with Templates_Parser;

with Dots.Parser;
with Dots.String_Sets;
with Dots.State;
with Dots.Xmlreaders;

procedure Dots_V is

   use type VString;

   Dod_Dir : VString := V ("");

   function Is_Unit (Name : in String) return Boolean;

   procedure Parse (S : in String;
                    Effort_Only : in Boolean;
                    Err    : out Boolean);

   procedure Handle_Filename
     (Item  :        String;
      Index :        Positive;
      Quit  : in out Boolean);

   procedure Dir is new GNAT.Directory_Operations.Iteration.Find
     (Handle_Filename);

   function Expand_Env (P : in VString) return VString;

   function Expand_Env (P : in VString) return VString is

      function Env (E : in String) return String;

      function Env (E : in String) return String is
         use type GNAT.OS_Lib.String_Access;

         E_Ptr : constant GNAT.OS_Lib.String_Access :=
                   GNAT.OS_Lib.Getenv (E);
      begin
         if E_Ptr /= null and then E_Ptr.all /= "" then
            return E_Ptr.all;
         else
            Ada.Text_IO.Put_Line
              ("Environment variable " & E & " is missing, " &
               "using current directory as root");
            return ".";
         end if;
      end Env;

      Tmp : constant String := S (P);
      First_After : Integer := Tmp'First;

   begin
      if Tmp'Length = 0 or else Tmp (Tmp'First) /= '$' then
         return P;
      end if;

      for J in Tmp'Range loop
         if Tmp (J) = ')' then
            First_After := J + 1;
            exit;
         end if;
      end loop;

      return V (Env (Tmp (Tmp'First + 2 .. First_After - 2)) &
                Tmp (First_After .. Tmp'Last));
   end Expand_Env;

   procedure Handle_Filename
     (Item  :        String;
      Index :        Positive;
      Quit  : in out Boolean) is
      pragma Unreferenced (Index, Quit);
      use type Templates_Parser.Vector_Tag;

      Translations : constant Templates_Parser.Translate_Table
      :=
        (1  => Templates_Parser.Assoc
           ("SECTION", "Parameters"));

      Contents : constant String :=
        Templates_Parser.Parse
          (Filename => Item,
           Translations => Translations,
           Cached => True);

      Parent_Translations : constant Templates_Parser.Translate_Table
      :=
        (1  => Templates_Parser.Assoc
           ("SECTION", "Parent"));

      Parent_Contents : constant String :=
        Templates_Parser.Parse
          (Filename => Item,
           Translations => Parent_Translations,
           Cached => True);

      function Match (Parameter : in String) return VString;
      procedure Setup_Exceptions;
      procedure Setup_Types;

      function Match (Parameter : in String) return VString is
         Matches : GNAT.Regpat.Match_Array (0 .. 1);
         Regexp  : constant String := "" & Parameter & ":""(.*?)""";
         use type GNAT.Regpat.Match_Location;
      begin
         GNAT.Regpat.Match (GNAT.Regpat.Compile (Regexp), Contents, Matches);
         if Matches (1) = GNAT.Regpat.No_Match then
            Ada.Text_IO.Put_Line
              (Parameter & " has no defined value ");
            return V ("");
         end if;

         if Dots.State.Log_Parsing then
            Ada.Text_IO.Put_Line
              (Parameter & " has value """ &
               Contents (Matches (1).First .. Matches (1).Last) & '"');
         end if;
         return V (Contents (Matches (1).First .. Matches (1).Last));
      end Match;

      procedure Setup_Exceptions is
         Matches : GNAT.Regpat.Match_Array (0 .. 3);
         Regexp  : constant String := "Exception:(.*?):""(.*?)"":""(.*?)""";
         use type GNAT.Regpat.Match_Location;
         First : Integer := -1;
         V : Templates_Parser.Vector_Tag;
      begin
         loop
            GNAT.Regpat.Match (GNAT.Regpat.Compile (Regexp),
                               Contents, Matches, First);
            exit when Matches (0) = GNAT.Regpat.No_Match;
            --  The vector contains:
            --    Local_Name, Unit_Name
            --  The name of the association is the XML_Name.
            V := +Contents (Matches (2).First .. Matches (2).Last) &
            Contents (Matches (3).First .. Matches (3).Last);

            Templates_Parser.Insert
              (Dots.State.Outputs (Dots.State.Defined_Outputs).Exception_Set,
               Templates_Parser.Assoc
                 (Contents (Matches (1).First .. Matches (1).Last),
                  V));
            Dots.State.Outputs (Dots.State.Defined_Outputs).Exception_List :=
              Dots.State.Outputs (Dots.State.Defined_Outputs).Exception_List &
            Contents (Matches (1).First .. Matches (1).Last) & " ";
            First := Matches (0).Last;
         end loop;

      end Setup_Exceptions;

      procedure Setup_Namespace_Mangling is
         File_Suffix  : constant String :=  S (Match ("Namespace_Prefix_File_Suffix"));
         Namespace_Separator : constant String :=  S (Match ("Namespace_Separator"));
         procedure Handle_Prefix_File
            (Item  :        String;
             Index :        Positive;
             Quit  : in out Boolean);

         procedure Prefix_Dir is new GNAT.Directory_Operations.Iteration.Wildcard_Iterator
           (Handle_Prefix_File);

         function Get_Prefix_From (Filename : String) return String is
            F : Ada.Text_IO.File_Type;
         begin
            Ada.Text_IO.Open (F, Ada.Text_IO.In_File, Filename);
            loop
               declare
                  Line : constant String := Ada.Text_IO.Get_Line (F);
               begin
                  if Line'Length > 0 then
                     case  Line (Line'First) is
                        when 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' =>
                           Ada.Text_IO.Close (F);
                           return Line;
                        when others =>
                           null;
                     end case;
                  end if;
               end;
            end loop;
            return "";
         end Get_Prefix_From;

         procedure Handle_Prefix_File
            (Item  :        String;
             Index :        Positive;
             Quit  : in out Boolean) is
            pragma Unreferenced (Index, Quit);
            Ns_First : Integer := Item'First;
            Ns_Last  : constant Integer := Item'Last - File_Suffix'Length;
         begin
            for J in reverse Item'Range loop
               if Item (J) = GNAT.Directory_Operations.Dir_Separator then
                  Ns_First := J + 1;
                  exit;
               end if;
            end loop;
            Dots.State.Outputs (Dots.State.Defined_Outputs).Namspace_Prefix_Used := True;
            Templates_Parser.Insert
              (Dots.State.Outputs (Dots.State.Defined_Outputs).Namspace_Prefix_Set,
               Templates_Parser.Assoc
                 (Item (Ns_First .. Ns_Last) & '.',
                  Get_Prefix_From (Item) & Namespace_Separator));
            -- Ada.Text_IO.Put_Line("Prefix: " & Item (Ns_First .. Ns_Last) & " " & Get_Prefix_From (Item));
         end Handle_Prefix_File;

      begin
         Dots.State.Outputs (Dots.State.Defined_Outputs).Namspace_Prefix_Used := False;
         if File_Suffix = "" then
            return;
         end if;
         Prefix_Dir ("*" & File_Suffix );
      end Setup_Namespace_Mangling;

      procedure Setup_Types is
         Matches : GNAT.Regpat.Match_Array (0 .. 4);
         Regexp  : constant String := "Type:(.*?):(.*?):""(.*?)"":""(.*?)""";
         use type GNAT.Regpat.Match_Location;
         First : Integer := -1;
         V : Templates_Parser.Vector_Tag;
      begin
         loop
            GNAT.Regpat.Match (GNAT.Regpat.Compile (Regexp),
                               Contents, Matches, First);
            exit when Matches (0) = GNAT.Regpat.No_Match;
            --  The vector contains:
            --    Local_Name, Universal_Name, Unit_Name
            --  The name of the association is the XML_Name.
            V := +Contents (Matches (3).First .. Matches (3).Last) &
            Contents (Matches (2).First .. Matches (2).Last) &
            Contents (Matches (4).First .. Matches (4).Last);

            Templates_Parser.Insert
              (Dots.State.Outputs (Dots.State.Defined_Outputs).Type_Set,
               Templates_Parser.Assoc
                 (Contents (Matches (1).First .. Matches (1).Last),
                  V));
            Dots.State.Outputs (Dots.State.Defined_Outputs).Type_List :=
              Dots.State.Outputs (Dots.State.Defined_Outputs).Type_List &
            Contents (Matches (1).First .. Matches (1).Last) & " ";
--              Ada.Text_IO.Put_Line
--                (Contents (Matches (1).First .. Matches (1).Last) &
--                 " has uniform value """ &
--                 Contents (Matches (2).First .. Matches (2).Last) &
--                 """ and output value """ &
--                 Contents (Matches (3).First .. Matches (3).Last) & '"');

            First := Matches (0).Last;
         end loop;

--           Ada.Text_IO.Put_Line
--             ("The types" & Integer'Image (Dots.State.Defined_Outputs) &
--              ": " & S (Dots.State.Outputs
--                                 (Dots.State.Defined_Outputs).Type_List));

      end Setup_Types;

      Item_First : Integer := Item'First;

   begin
--        Ada.Text_IO.Put_Line ("Footer => " & S (Match_Section ("Footer")));
--        Ada.Text_IO.Put_Line ("Header => " & S (Match_Section ("Header")));

      for J in reverse Item'Range loop
         if Item (J) = GNAT.Directory_Operations.Dir_Separator then
            Item_First := J + 1;
            exit;
         end if;
      end loop;

      if Dots.State.Log_Info then
         Ada.Text_IO.Put_Line
           ("Dod : """ & Item & '"');
      end if;

      Dots.State.Defined_Outputs := Dots.State.Defined_Outputs + 1;

      Setup_Exceptions;
      Setup_Types;
      Setup_Namespace_Mangling;

      Dots.State.Outputs (Dots.State.Defined_Outputs) :=
        Dots.State.Output_Config'
          (Name => V (Item (Item_First .. Item'Last - 4)),
           Full_Name => V (Item),
           File_Suffix => Match ("File_Suffix"),
           Filename_Separator =>  Match ("Filename_Separator"),
           Output_Directory    =>  Expand_Env (Match ("Output_Directory")),
           Namespace_Separator =>  Match ("Namespace_Separator"),
           Namespace_Prefix_File_Suffix =>  Match ("Namespace_Prefix_File_Suffix"),
           Namespace_Style      =>
             (Underscore => Dots.State.Underscore_Style'Value
                (S (Match ("Namespace_Underscore_Style"))),
              Choose_Case => Dots.State.Case_Style'Value
                (S (Match ("Namespace_Case_Style")))),
           Filename_Style      =>
             (Underscore => Dots.State.Underscore_Style'Value
                (S (Match ("Filename_Underscore_Style"))),
              Choose_Case => Dots.State.Case_Style'Value
                (S (Match ("Filename_Case_Style")))),
           Classname_Style      =>
             (Underscore => Dots.State.Underscore_Style'Value
                (S (Match ("Classname_Underscore_Style"))),
              Choose_Case => Dots.State.Case_Style'Value
                (S (Match ("Classname_Case_Style")))),
           Membername_Style      =>
             (Underscore => Dots.State.Underscore_Style'Value
                (S (Match ("Membername_Underscore_Style"))),
              Choose_Case => Dots.State.Case_Style'Value
                (S (Match ("Membername_Case_Style")))),
           Enum_Value_Style      =>
             (Underscore => Dots.State.Underscore_Style'Value
                (S (Match ("Enum_Value_Underscore_Style"))),
              Choose_Case => Dots.State.Case_Style'Value
                (S (Match ("Enum_Value_Case_Style")))),
           Create_Parents => Parent_Contents /= "",
           Object_Type => Match ("Object_Type"),
           Parent_Filename => Match ("Parent_Filename"),
           Index_Type => Match ("Index_Type"),
           Exception_Set =>  Dots.State.Outputs
             (Dots.State.Defined_Outputs).Exception_Set,
           Exception_List =>  Dots.State.Outputs
             (Dots.State.Defined_Outputs).Exception_List,
           Type_Set =>  Dots.State.Outputs
             (Dots.State.Defined_Outputs).Type_Set,
           Type_List =>  Dots.State.Outputs
             (Dots.State.Defined_Outputs).Type_List,
           Namspace_Prefix_Set =>  Dots.State.Outputs
             (Dots.State.Defined_Outputs).Namspace_Prefix_Set,
           Namspace_Prefix_Used =>  Dots.State.Outputs
             (Dots.State.Defined_Outputs).Namspace_Prefix_Used,
           Dependencies => Dots.String_Sets.Empty_Set,
           Dependencies_Base => Dots.String_Sets.Empty_Set);

   end Handle_Filename;

   function Is_Unit (Name : in String) return Boolean is
      Ext : constant String := ".dou";
   begin
      return Name'Length > Ext'Length and then
      Name (Name'Last - Ext'Length + 1 .. Name'Last) = Ext;
   end Is_Unit;

   procedure Parse (S : in String;
                    Effort_Only : in Boolean;
                    Err    : out Boolean) is

      Silent     : Boolean := not Dots.State.Log_Parsing;

      My_Reader  : Dots.Xmlreaders.Reader;
      Read : Input_Sources.File.File_Input;

      Phases : Integer;

   begin
      Err := False;
      if Effort_Only then
         Phases := 1;
      else
         Phases := 1 + Dots.State.Defined_Outputs;
      end if;

      Dots.Xmlreaders.Set_Effort_Only (My_Reader, True);

      for i in 1 .. Phases loop
         Dots.State.Current_Output := i - 1;

         Err := False;
         Dots.Parser.Error := Nul;

         Input_Sources.File.Open
           (Filename => S,
            Input    => Read);

         Dots.Xmlreaders.Set_Silent (My_Reader, Silent);

         --  If True, xmlns:* attributes will be reported in Start_Element
         Dots.Xmlreaders.Set_Feature
           (My_Reader, Sax.Readers.Namespace_Feature, True);
         Dots.Xmlreaders.Set_Feature
           (My_Reader, Sax.Readers.Namespace_Prefixes_Feature, False);
         Dots.Xmlreaders.Set_Feature
           (My_Reader, Sax.Readers.Validation_Feature, False);

         Dots.Xmlreaders.Parse (My_Reader, Read);
         Input_Sources.File.Close (Read);

         Dots.Xmlreaders.Set_Effort_Only (My_Reader, False);
         Silent := True;
      end loop;

   exception
      when Dots.Error_Found =>
         Err := True;
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error, GNAT.Spitbol.S (Dots.Parser.Error));
      when E : others =>
         Err := True;
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            S & ":0:0: Undefined error: "
            & Ada.Exceptions.Exception_Information (E));
         Input_Sources.File.Close (Read);
   end Parse;

   Err : Boolean := False;
   Help : Boolean := False;

begin
   --  Parse the command line

   loop
      begin
         case GNAT.Command_Line.Getopt
              ("dod= verbose info tokens output= help") is
            when ASCII.Nul =>
               exit;
            when 'h' =>
               Help := True;
            when 'v' =>
               Dots.State.Log_Parsing := True;
            when 'i' =>
               Dots.State.Log_Info := True;
            when 'o' =>
               Dots.State.Log_Output_Type := V (GNAT.Command_Line.Parameter);
            when 'd' =>
               Dod_Dir := Expand_Env (V (GNAT.Command_Line.Parameter));
            when 't' =>
               Dots.State.Log_Tokens := True;
            when others =>
               null;
         end case;
      exception
         when others =>
            Help := True;
      end;
   end loop;

   if Help then
      Ada.Text_IO.Put_Line ("Usage:");
      Ada.Text_IO.Put_Line ("dots_v [options] xml-files...");
      Ada.Text_IO.Put_Line ("-dod=<dir>  : Specifies the directory containing the dod-files.");
      Ada.Text_IO.Put_Line ("-info       : Displays handled files");
      Ada.Text_IO.Put_Line ("-verbose    : Displays parsing");
      Ada.Text_IO.Put_Line ("-output=xxx : Displays specified output." &
                           " Eg cpp_h");
      Ada.Text_IO.Put_Line ("-tokens     : Displays tokens during" &
                            " action phase");
      return;
   end if;

   if Dots.State.Log_Info then
      Ada.Text_IO.Put_Line ("Looking for *.dod in " & S (Dod_Dir) & ".");
   end if;

   begin
      Dir (S (Dod_Dir), ".*\.dod");
   exception
      when others =>
         null;
   end;

   if Dots.State.Defined_Outputs = 0 then
      Ada.Text_IO.Put_Line ("No dod-files found in " & S (Dod_Dir) & ".");
      GNAT.OS_Lib.OS_Exit (2);
   end if;

   --  Check the arguments

   loop
      declare
         F : constant String := GNAT.Command_Line.Get_Argument (True);
      begin
         exit when F = "";
         if not Is_Unit (F) then
            Ada.Text_IO.Put_Line (F & " is not a unit file (.dou)");
         else

            if Dots.State.Log_Info then
               Ada.Text_IO.Put_Line (F & " will be parsed");
            end if;

            Dots.State.Current_Unit := V (F (F'First .. F'Last - 4));
            Parse (S => F, Effort_Only => False, Err => Err);
            if Err then
               GNAT.OS_Lib.OS_Exit (1);
            end if;
         end if;
      end;
   end loop;

end Dots_V;
