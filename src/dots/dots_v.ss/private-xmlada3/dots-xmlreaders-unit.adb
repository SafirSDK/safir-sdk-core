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

with Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Text_IO;
with Interfaces.C;

--  with GNAT.Directory_Operations;
with GNAT.Spitbol; use GNAT.Spitbol;
with Templates_Parser;

with Dots.String_Sets;
with Dots.State;
with Dots.Utilities;

package body Dots.Xmlreaders.Unit is

   use type Dots.Parser.Element_Type_T;
   use type Templates_Parser.Vector_Tag;
   use type Templates_Parser.Association;
   use type VString;

   sNamespace        : VString;
   sRawNamespace     : VString;
   sClass            : VString;
   sClassSummary     : VString;
   sParameterSummary : VString;
   sMemberSummary    : VString;
   sCreateRoutineSummary : VString;
   sUnitType         : VString;
   sParameterIsArray : VString;
   sCreateValueParameterIndex : VString;
   sMember           : VString;
   sXmlMemberType    : VString;
   sMemberIsArray    : VString;
   sMemberIsString   : VString;

   svParameterSummary : Templates_Parser.Vector_Tag;
   svMemberSummary    : Templates_Parser.Vector_Tag;
   svNamespace        : Templates_Parser.Vector_Tag;
   svRevNamespace     : Templates_Parser.Vector_Tag;
   svDependencies     : Templates_Parser.Vector_Tag;
   svBaseClass        : Templates_Parser.Vector_Tag;
   svParameter        : Templates_Parser.Vector_Tag;
   svParameterType    : Templates_Parser.Vector_Tag;
   svParameterIsArray : Templates_Parser.Vector_Tag;
   svParameterClass   : Templates_Parser.Vector_Tag;
   svUniformParameterType  : Templates_Parser.Vector_Tag;
   svMember           : Templates_Parser.Vector_Tag;
   svMemberIsArray    : Templates_Parser.Vector_Tag;
   svMemberType       : Templates_Parser.Vector_Tag;
   svUniformMemberType  : Templates_Parser.Vector_Tag;
   svMemberClass      : Templates_Parser.Vector_Tag;
   svMemberIsString   : Templates_Parser.Vector_Tag;
   svEnumValue        : Templates_Parser.Vector_Tag;
   svCreateRoutineSummary       : Templates_Parser.Vector_Tag;
   svCreateRoutine              : Templates_Parser.Vector_Tag;
   svCreateParameter            : Templates_Parser.Vector_Tag;
   svCreateParameterType        : Templates_Parser.Vector_Tag;
   svCreateParameterIsLast      : Templates_Parser.Vector_Tag;
   svCreateParameterIsArray     : Templates_Parser.Vector_Tag;
   svUniformCreateParameterType : Templates_Parser.Vector_Tag;
   svCreateValue                : Templates_Parser.Vector_Tag;
   svCreateValueType            : Templates_Parser.Vector_Tag;
   svCreateValueIsArray         : Templates_Parser.Vector_Tag;
   svUniformCreateValueType     : Templates_Parser.Vector_Tag;
   svCreateValueParameter       : Templates_Parser.Vector_Tag;
   svCreateValueParameterIndex  : Templates_Parser.Vector_Tag;
   svCreateValueParameterClass  : Templates_Parser.Vector_Tag;
   svNull                       : Templates_Parser.Vector_Tag;

   smCreateParameter            : Templates_Parser.Matrix_Tag;
   smCreateParameterType        : Templates_Parser.Matrix_Tag;
   smCreateParameterIsLast      : Templates_Parser.Matrix_Tag;
   smCreateParameterIsArray     : Templates_Parser.Matrix_Tag;
   smUniformCreateParameterType : Templates_Parser.Matrix_Tag;
   smCreateValue                : Templates_Parser.Matrix_Tag;
   smCreateValueType            : Templates_Parser.Matrix_Tag;
   smCreateValueIsArray         : Templates_Parser.Matrix_Tag;
   smUniformCreateValueType     : Templates_Parser.Matrix_Tag;
   smCreateValueParameter       : Templates_Parser.Matrix_Tag;
   smCreateValueParameterIndex  : Templates_Parser.Matrix_Tag;
   smCreateValueParameterClass  : Templates_Parser.Matrix_Tag;

   No_Of_Create_Parameters : Integer := 0;
   Trans_Set : Templates_Parser.Translate_Set;

   procedure Add_Type_Dependency
     (Xml_Type : in String);

   procedure Add_Exception_Dependency
     (Xml_Exception : in String);

   procedure Put_Line (On : in Boolean;
                       S : in String);

   procedure Split (S : in String;
                    Namespace : in out Templates_Parser.Vector_Tag;
                    Unit      : out VString);

   function TypeIdOf (NS, Name : in String;
                      Enums : in Templates_Parser.Vector_Tag) return VString;

   procedure Write_File;

   procedure Setup_Associations;

   procedure Add_Exception_Dependency
     (Xml_Exception : in String) is
      Conf : Dots.State.Output_Config renames
        Dots.State.Outputs (Dots.State.Current_Output);
      Asso : constant Templates_Parser.Association
        := Templates_Parser.Get (Conf.Exception_Set, Xml_Exception);
      Dummy_Cursor : Dots.String_Sets.Cursor;
      Dummy : Boolean;
   begin
      if Asso = Templates_Parser.Null_Association then
         Dots.String_Sets.Insert
           (Container => Conf.Dependencies,
            New_Item  => Xml_Exception,
            Position  => Dummy_Cursor,
            Inserted  => Dummy);
      else
         Dots.String_Sets.Insert
           (Container => Conf.Dependencies,
            New_Item  => Templates_Parser.Item
              (Templates_Parser.Get (Asso), 2),
            Position  => Dummy_Cursor,
            Inserted  => Dummy);
      end if;
   end Add_Exception_Dependency;

   procedure Add_Type_Dependency
     (Xml_Type : in String) is
      Conf : Dots.State.Output_Config renames
        Dots.State.Outputs (Dots.State.Current_Output);
      Asso : constant Templates_Parser.Association
        := Templates_Parser.Get (Conf.Type_Set, Xml_Type);
      Dummy_Cursor : Dots.String_Sets.Cursor;
      Dummy : Boolean;
   begin
      if Xml_Type /= S (Dots.State.Current_Unit) then
         if Asso = Templates_Parser.Null_Association then
            Dots.String_Sets.Insert
              (Container => Conf.Dependencies,
               New_Item  => Xml_Type,
               Position  => Dummy_Cursor,
               Inserted  => Dummy);
         else
            Dots.String_Sets.Insert
              (Container => Conf.Dependencies,
               New_Item  => Templates_Parser.Item
                 (Templates_Parser.Get (Asso), 3),
               Position  => Dummy_Cursor,
               Inserted  => Dummy);
         end if;
      end if;
   end Add_Type_Dependency;

   -----------------
   -- End_Element --
   -----------------

   procedure End_Element
     (Local_Name        : in String;
      Left_Element_Type : in Dots.Parser.Element_Type_T) is
      Conf : Dots.State.Output_Config renames
        Dots.State.Outputs (Dots.State.Current_Output);

      function Local_Exception_Tag_Of
        (Xml_Exception : in String) return String;

      function Local_Exception_Tag_Of
        (Xml_Exception : in String) return String is
         Asso : constant Templates_Parser.Association
           := Templates_Parser.Get (Conf.Exception_Set, Xml_Exception);
         Tmp : String := Xml_Exception;
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
            return Dots.Utilities.Replace_Dots
              (S         => Tmp,
               Separator => S (Conf.Namespace_Separator));
         else
            return Templates_Parser.Item
              (Templates_Parser.Get (Asso),
               1);
         end if;
      end Local_Exception_Tag_Of;

   begin
      Put_Line (Dots.State.Log_Tokens, "</" & Local_Name & ">");
      case Left_Element_Type is
         when Dots.Parser.xsd_integer =>
            if Dots.Parser.Is_Child_Of (Dots.Parser.ctCreateValue) then
               sCreateValueParameterIndex := Dots.Parser.Simple_Contents;
            end if;

         when Dots.Parser.stSummary =>
            declare
               Summary : constant VString :=
                   Dots.Utilities.Format_Summary (Dots.Parser.Simple_Contents);
            begin
               if Dots.Parser.Is_Child_Of (Dots.Parser.ctMember) then
                  sMemberSummary := Summary;
               elsif Dots.Parser.Is_Child_Of (Dots.Parser.ctParameter) then
                  sParameterSummary := Summary;
               elsif Dots.Parser.Is_Child_Of (Dots.Parser.ctCreateRoutine) then
                  sCreateRoutineSummary := Summary;
               else
                  sClassSummary := Summary;
               end if;
            end;

         when Dots.Parser.stUnitName =>
            Split (S (Dots.Parser.Simple_Contents), svNamespace, sClass);
            Templates_Parser.Clear (svRevNamespace);
            sNamespace := Nul;
            for J in 1 .. Templates_Parser.Size (svNamespace) loop
               svRevNamespace := Templates_Parser.Item (svNamespace, J)
                 & svRevNamespace;
               if J = 1 then
                  sNamespace := V (Templates_Parser.Item (svNamespace, J));
               else
                  sNamespace := sNamespace & "."
                    & Templates_Parser.Item (svNamespace, J);
               end if;
            end loop;

            sRawNamespace := sNamespace;
            if Conf.Lowercase_Namespace then
               sNamespace := V (Ada.Characters.Handling.To_Lower
                                (S (sNamespace)));
            end if;

            if Dots.Parser.Is_Child_Of (Dots.Parser.ctProperty) then
               Dots.Utilities.Define_Unit
                 (Unit       => Dots.State.Current_Unit,
                  Unit_Type  => Dots.State.Property,
                  Base_Class => Nul);
            elsif Dots.Parser.Is_Child_Of (Dots.Parser.ctEnumeration) then
               Dots.Utilities.Define_Unit
                 (Unit       => Dots.State.Current_Unit,
                  Unit_Type  => Dots.State.Enum,
                  Base_Class => Nul);
            end if;

         when Dots.Parser.stFullParameterName =>
            if Dots.Parser.Is_Child_Of (Dots.Parser.ctCreateValue) then
               declare
                  Full_Name : constant String :=
                                S (Dots.Parser.Simple_Contents);
               begin
                  for J in reverse Full_Name'Range loop
                     if Full_Name (J) = '.' then
                        svCreateValueParameter :=
                          svCreateValueParameter &
                        V (Full_Name (J + 1 .. Full_Name'Last));

                        declare
                           CVPC : String :=
                             Full_Name (Full_Name'First .. J - 1);
                           Dot_Found : Boolean := False;
                        begin
                           if Conf.Lowercase_Namespace then
                              for K in reverse CVPC'Range loop
                                 if Dot_Found then
                                    CVPC (K) := Ada.Characters.Handling.
                                      To_Lower (CVPC (K));
                                 elsif CVPC (K) = '.' then
                                    Dot_Found := True;
                                 end if;
                              end loop;
                           end if;
                           svCreateValueParameterClass :=
                             svCreateValueParameterClass &
                           Dots.Utilities.Replace_Dots
                             (S         => CVPC,
                              Separator => S (Conf.Namespace_Separator),
                              Lower     => False);
                        end;

                        if V (Full_Name (Full_Name'First .. J - 1)) /=
                        Dots.State.Current_Unit then
                           Add_Type_Dependency
                             (Full_Name (Full_Name'First .. J - 1));
                        end if;
                        exit;
                     end if;
                  end loop;
               end;
            end if;

         when Dots.Parser.stFullClassName =>
            if not (Dots.Parser.Is_Child_Of (Dots.Parser.ctObject) or
                      Dots.Parser.Is_Child_Of (Dots.Parser.ctEntityId)) then

               if Dots.Parser.Is_Child_Of (Dots.Parser.ctException) then

                  svBaseClass := svBaseClass &
                  Local_Exception_Tag_Of (S (Dots.Parser.Simple_Contents));
                  Add_Exception_Dependency (S (Dots.Parser.Simple_Contents));
                  Dots.Utilities.Define_Unit
                    (Unit       => Dots.State.Current_Unit,
                     Unit_Type  => Dots.State.Xeption,
                     Base_Class => Dots.Parser.Simple_Contents);
               else
                  svBaseClass := svBaseClass &
                  Dots.Utilities.Local_Type_Tag_Of
                    (S (Dots.Parser.Simple_Contents));
                  Add_Type_Dependency (S (Dots.Parser.Simple_Contents));
                  Dots.Utilities.Define_Unit
                    (Unit       => Dots.State.Current_Unit,
                     Unit_Type  => Dots.State.Class,
                     Base_Class => Dots.Parser.Simple_Contents);
               end if;
--              SvBaseClass := SvBaseClass &
--                (Replace_Dots
--                   (S         => S (Dots.Parser.Simple_Contents),
--                    Separator => S (Conf.Namespace_Separator)) &
--                 Conf.Referred_Class_Suffix);
            end if;

         when Dots.Parser.stMemberName =>
            if not Dots.Parser.Is_Child_Of (Dots.Parser.ctObjectMember) then
               if Dots.Parser.Is_Child_Of (Dots.Parser.ctMember) then
                  sMember := Dots.Parser.Simple_Contents;
                  svMember := svMember & Dots.Parser.Simple_Contents;
               elsif Dots.Parser.Is_Child_Of (Dots.Parser.ctParameter) then
                  svParameter := svParameter & Dots.Parser.Simple_Contents;
               elsif Dots.Parser.Is_Child_Of
                 (Dots.Parser.ctCreateValue) then
                  declare
                     Unit : constant String :=
                              Dots.Utilities.Get_Member_Unit
                                (Super_Unit => S (Dots.State.Current_Unit),
                                 Member => S (Dots.Parser.Simple_Contents));
                  begin
                     svCreateValue := svCreateValue
                      & Dots.Parser.Simple_Contents;

                     svUniformCreateValueType :=
                       svUniformCreateValueType
                         & Dots.Utilities.Get_Uniform_Member_Type
                       (Unit, S (Dots.Parser.Simple_Contents));

                     svCreateValueType :=
                       svCreateValueType
                         & Dots.Utilities.Get_Local_Member_Type
                       (Unit, S (Dots.Parser.Simple_Contents));

                     if Dots.Utilities.Is_Array_Member
                       (Unit, S (Dots.Parser.Simple_Contents)) then
                        svCreateValueIsArray :=
                          svCreateValueIsArray & V ("True");
                     else
                        svCreateValueIsArray :=
                          svCreateValueIsArray & Nul;
                     end if;

                  end;
               elsif Dots.Parser.Is_Child_Of
                 (Dots.Parser.ctCreateParameters) then
                  declare
                     Unit : constant String :=
                              Dots.Utilities.Get_Member_Unit
                                (Super_Unit => S (Dots.State.Current_Unit),
                                 Member => S (Dots.Parser.Simple_Contents));
                  begin
                     svCreateParameter := svCreateParameter
                      & Dots.Parser.Simple_Contents;

                     svUniformCreateParameterType :=
                       svUniformCreateParameterType
                         & Dots.Utilities.Get_Uniform_Member_Type
                       (Unit, S (Dots.Parser.Simple_Contents));

                     svCreateParameterType :=
                       svCreateParameterType
                         & Dots.Utilities.Get_Local_Member_Type
                       (Unit, S (Dots.Parser.Simple_Contents));

                     if Dots.Utilities.Is_Array_Member
                       (Unit, S (Dots.Parser.Simple_Contents)) then
                        svCreateParameterIsArray :=
                          svCreateParameterIsArray & V ("True");
                     else
                        svCreateParameterIsArray :=
                          svCreateParameterIsArray & Nul;
                     end if;

                  end;
               else
                  svCreateRoutine := svCreateRoutine
                   & Dots.Parser.Simple_Contents;
               end if;
            end if;

         when Dots.Parser.stArraySize |
              Dots.Parser.stArray =>
            sMemberIsArray := V ("True");
            Add_Type_Dependency (S (Conf.Index_Type));

         when Dots.Parser.ctReference =>
            if Dots.Parser.Is_Child_Of (Dots.Parser.ctMember) and then
                Local_Name = "arraySizeRef" then
               sMemberIsArray := V ("True");
               Add_Type_Dependency (S (Conf.Index_Type));
            end if;

         when Dots.Parser.ctParameterArray =>
            sParameterIsArray := V ("True");
            Add_Type_Dependency (S (Conf.Index_Type));

         when Dots.Parser.stType =>
            declare

            begin
               Add_Type_Dependency (S (Dots.Parser.Simple_Contents));
               if Dots.Parser.Is_Child_Of (Dots.Parser.ctMember) then
                  svMemberClass := svMemberClass & sClass;
                  svMemberType := svMemberType &
                  Dots.Utilities.Local_Type_Tag_Of
                    (S (Dots.Parser.Simple_Contents));
                  sXmlMemberType := Dots.Parser.Simple_Contents;
                  svUniformMemberType := svUniformMemberType &
                  Dots.Utilities.Uniform_Type_Tag_Of
                    (S (Dots.Parser.Simple_Contents));
                  if S (Dots.Parser.Simple_Contents) = "String" then
                     sMemberIsString := V ("True");
                     Add_Type_Dependency (S (Conf.Index_Type));
                  end if;

               else
                  svParameterClass := svParameterClass & sClass;
                  svParameterType := svParameterType &
                  Dots.Utilities.Local_Type_Tag_Of
                    (S (Dots.Parser.Simple_Contents));
                  svUniformParameterType := svUniformParameterType &
                  Dots.Utilities.Uniform_Type_Tag_Of
                    (S (Dots.Parser.Simple_Contents));
               end if;
            end;
         when Dots.Parser.ctMember =>
            svMemberIsArray := svMemberIsArray & sMemberIsArray;
            svMemberIsString := svMemberIsString & sMemberIsString;
            svMemberSummary := svMemberSummary & sMemberSummary;
            if Dots.Parser.Is_Inside (Dots.Parser.ctClass) then
               Dots.Utilities.Define_Member
                 (Unit            => Dots.State.Current_Unit,
                  Member          => sMember,
                  Xml_Member_Type => sXmlMemberType,
                  Is_Array        =>  S (sMemberIsArray) /= "");
            end if;
         when Dots.Parser.ctParameter =>
            svParameterIsArray := svParameterIsArray & sParameterIsArray;
            svParameterSummary := svParameterSummary & sParameterSummary;

         when Dots.Parser.ctCreateValue =>
            svCreateValueParameterIndex :=
              svCreateValueParameterIndex & sCreateValueParameterIndex;

         when Dots.Parser.ctClass | Dots.Parser.ctProperty |
              Dots.Parser.ctEnumeration | Dots.Parser.ctException =>
            declare
               procedure Process (Position : in Dots.String_Sets.Cursor);
               procedure Process (Position : in Dots.String_Sets.Cursor) is
                  Dependency : String :=
                    Dots.String_Sets.Element (Position);
                  Dot_Found : Boolean := False;
               begin
                  if Conf.Lowercase_Namespace then
                     for J in reverse Dependency'Range loop
                        if Dot_Found then
                           Dependency (J) :=
                             Ada.Characters.Handling.To_Lower (Dependency (J));
                        elsif Dependency (J) = '.' then
                           Dot_Found := True;
                        end if;
                     end loop;
                  end if;

                  if Dependency /= "" then
                     svDependencies := svDependencies & Dependency;
                  end if;
               end Process;

            begin
               Dots.String_Sets.Iterate (Conf.Dependencies, Process'Access);
            end;

            Setup_Associations;
            Write_File;

            if Conf.Create_Parents then
               Dots.Utilities.Create_Namespace_Parents
                 (Namespace => S (sNamespace));
            end if;

         when Dots.Parser.stEnumerationValue =>
            svEnumValue := svEnumValue & Dots.Parser.Simple_Contents;

         when Dots.Parser.ctCreateRoutine =>
            svCreateRoutineSummary := svCreateRoutineSummary
               & sCreateRoutineSummary;

            for I in 2 .. Templates_Parser.Size (svCreateParameter)
            loop
               svCreateParameterIsLast :=
                 svCreateParameterIsLast & Nul;
            end loop;
            svCreateParameterIsLast :=
                 svCreateParameterIsLast & "True";

            smCreateParameter :=
              smCreateParameter & svCreateParameter;
            smCreateParameterType :=
              smCreateParameterType & svCreateParameterType;
            smCreateParameterIsLast :=
              smCreateParameterIsLast & svCreateParameterIsLast;
            smCreateParameterIsArray :=
              smCreateParameterIsArray
                & svCreateParameterIsArray;
            smUniformCreateParameterType :=
              smUniformCreateParameterType
                & svUniformCreateParameterType;

            smCreateValue :=
              smCreateValue & svCreateValue;
            smCreateValueType :=
              smCreateValueType & svCreateValueType;
            smCreateValueIsArray :=
              smCreateValueIsArray
                & svCreateValueIsArray;
            smUniformCreateValueType :=
              smUniformCreateValueType
                & svUniformCreateValueType;
            smCreateValueParameter :=
              smCreateValueParameter
                & svCreateValueParameter;
            smCreateValueParameterIndex :=
              smCreateValueParameterIndex
                & svCreateValueParameterIndex;
            smCreateValueParameterClass :=
              smCreateValueParameterClass
                & svCreateValueParameterClass;

         when others =>
            null;
      end case;
   end End_Element;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (On : in Boolean;
                       S : in String) is
   begin
      if On then
         Ada.Text_IO.Put_Line (S);
      end if;
   end Put_Line;

   ------------------------
   -- Setup_Associations --
   ------------------------

   procedure Setup_Associations is
      Conf : Dots.State.Output_Config renames
        Dots.State.Outputs (Dots.State.Current_Output);
      Translations : constant Templates_Parser.Translate_Table
        :=
          (1  => Templates_Parser.Assoc ("SECTION", "Code"),
           2  => Templates_Parser.Assoc ("NAMESPACEV", svNamespace),
           3  => Templates_Parser.Assoc
             ("NAMESPACE", Dots.Utilities.Replace_Dots
                (S         => S (sNamespace),
                 Separator => S (Conf.Namespace_Separator))),
           4  => Templates_Parser.Assoc ("UNIT", S (Dots.State.Current_Unit)),
           5  => Templates_Parser.Assoc ("DEPENDENCY", svDependencies),
           6  => Templates_Parser.Assoc ("CLASS", sClass),
           7 => Templates_Parser.Assoc ("CLASSSUMMARY", sClassSummary),
           8  => Templates_Parser.Assoc ("UNITTYPE", sUnitType),
           9  => Templates_Parser.Assoc ("BASECLASS",  svBaseClass),
           10 => Templates_Parser.Assoc ("PARAMETER", svParameter),
           11 => Templates_Parser.Assoc ("PARAMETERSUMMARY",
                                         svParameterSummary),
           12 => Templates_Parser.Assoc ("UNIFORM_PARAMETERTYPE",
                                      svUniformParameterType),
           13 => Templates_Parser.Assoc ("PARAMETERTYPE", svParameterType),
           14 => Templates_Parser.Assoc ("PARAMETERCLASS", svParameterClass),
           15 => Templates_Parser.Assoc ("PARAMETERISARRAY",
                                         svParameterIsArray),
           16 => Templates_Parser.Assoc ("MEMBER", svMember),
           17 => Templates_Parser.Assoc ("MEMBERSUMMARY", svMemberSummary),
           18 => Templates_Parser.Assoc ("UNIFORM_MEMBERTYPE",
                                      svUniformMemberType),
           19 => Templates_Parser.Assoc ("MEMBERTYPE", svMemberType),
           20 => Templates_Parser.Assoc ("MEMBERCLASS", svMemberClass),
           21 => Templates_Parser.Assoc ("MEMBERISARRAY", svMemberIsArray),
           22 => Templates_Parser.Assoc ("MEMBERISSTRING", svMemberIsString),
           23 => Templates_Parser.Assoc ("TYPEID", TypeIdOf (S (sRawNamespace),
                                                             S (sClass),
                                                             svNull)),
           24 => Templates_Parser.Assoc ("REVNAMESPACE", svRevNamespace),
           25 => Templates_Parser.Assoc ("ENUMVALUE", svEnumValue),
           26 => Templates_Parser.Assoc ("CREATEROUTINE", svCreateRoutine),
           27 => Templates_Parser.Assoc ("CREATEROUTINESUMMARY",
                                         svCreateRoutineSummary),
           28 => Templates_Parser.Assoc ("UNIFORM_CREATEPARAMETERTYPE",
                                      smUniformCreateParameterType),
           29 => Templates_Parser.Assoc ("CREATEPARAMETER", smCreateParameter),
           30 => Templates_Parser.Assoc ("CREATEPARAMETERTYPE",
                                         smCreateParameterType),
           31 => Templates_Parser.Assoc ("CREATEPARAMETERISARRAY",
                                         smCreateParameterIsArray),
           32 => Templates_Parser.Assoc ("CREATEPARAMETERISLAST",
                                         smCreateParameterIsLast),
           33 => Templates_Parser.Assoc ("UNIFORM_CREATEVALUETYPE",
                                      smUniformCreateValueType),
           34 => Templates_Parser.Assoc ("CREATEVALUE", smCreateValue),
           35 => Templates_Parser.Assoc ("CREATEVALUETYPE",
                                         smCreateValueType),
           36 => Templates_Parser.Assoc ("CREATEVALUEISARRAY",
                                         smCreateValueIsArray),
           37 => Templates_Parser.Assoc ("CREATEVALUEPARAMETER",
                                         smCreateValueParameter),
           38 => Templates_Parser.Assoc ("CREATEVALUEPARAMETERINDEX",
                                         smCreateValueParameterIndex),
           39 => Templates_Parser.Assoc ("CREATEVALUEPARAMETERCLASS",
                                         smCreateValueParameterClass),
           40 => Templates_Parser.Assoc ("CHECKSUM",
             TypeIdOf (S (sRawNamespace), S (sClass), svEnumValue))
          );
   begin
      Trans_Set := Templates_Parser.To_Set
        (Translations);
   end Setup_Associations;

   -----------
   -- Split --
   -----------

   procedure Split (S : in String;
                    Namespace : in out Templates_Parser.Vector_Tag;
                    Unit      : out VString) is
      Start : Integer := S'First;
   begin
      Templates_Parser.Clear (Namespace);
      for J in S'Range loop
         if S (J) = '.' then
            Namespace := Namespace & S (Start .. J - 1);
            Start := J + 1;
         end if;
      end loop;
      Unit := V (S (Start .. S'Last));
   end Split;

   -------------------
   -- Start_Element --
   -------------------

   procedure Start_Element
     (Local_Name           : in String;
      Entered_Element_Type : in Dots.Parser.Element_Type_T)
   is
      Conf : Dots.State.Output_Config renames
        Dots.State.Outputs (Dots.State.Current_Output);
      Unit_Type : Dots.State.Unit_Type;
      pragma Warnings (Off, Unit_Type);
   begin
      Put_Line (Dots.State.Log_Tokens, "<" & Local_Name & ">");
      case Entered_Element_Type is
         when Dots.Parser.ctClass | Dots.Parser.ctProperty |
              Dots.Parser.ctEnumeration | Dots.Parser.ctException =>

            Unit_Type := Dots.Utilities.Get_Unit_Type
              (S (Dots.State.Current_Unit));

            Conf.Dependencies := Dots.String_Sets.Empty_Set;

            if Entered_Element_Type = Dots.Parser.ctProperty then
               sUnitType := V ("property");
               Add_Type_Dependency (S (Conf.Object_Type));
            elsif Entered_Element_Type = Dots.Parser.ctClass then
               sUnitType := V ("class");
            elsif Entered_Element_Type = Dots.Parser.ctEnumeration then
               sUnitType := V ("enumeration");
            else
               sUnitType := V ("exception");
            end if;

            sClassSummary := Nul;

            Templates_Parser.Clear (svNamespace);
            Templates_Parser.Clear (svRevNamespace);
            Templates_Parser.Clear (svDependencies);
            Templates_Parser.Clear (svBaseClass);
            Templates_Parser.Clear (svParameter);
            Templates_Parser.Clear (svParameterSummary);
            Templates_Parser.Clear (svParameterType);
            Templates_Parser.Clear (svParameterClass);
            Templates_Parser.Clear (svUniformParameterType);
            Templates_Parser.Clear (svParameterIsArray);
            Templates_Parser.Clear (svMember);
            Templates_Parser.Clear (svMemberSummary);
            Templates_Parser.Clear (svMemberType);
            Templates_Parser.Clear (svMemberClass);
            Templates_Parser.Clear (svUniformMemberType);
            Templates_Parser.Clear (svMemberIsArray);
            Templates_Parser.Clear (svMemberIsString);
            Templates_Parser.Clear (svEnumValue);
            Templates_Parser.Clear (svCreateRoutineSummary);
            Templates_Parser.Clear (svCreateRoutine);

            Templates_Parser.Clear (smCreateParameter);
            Templates_Parser.Clear (smCreateParameterType);
            Templates_Parser.Clear (smCreateParameterIsLast);
            Templates_Parser.Clear (smCreateParameterIsArray);
            Templates_Parser.Clear (smUniformCreateParameterType);

            Templates_Parser.Clear (smCreateValue);
            Templates_Parser.Clear (smCreateValueType);
            Templates_Parser.Clear (smCreateValueIsArray);
            Templates_Parser.Clear (smUniformCreateValueType);

            Templates_Parser.Clear (smCreateValueParameter);
            Templates_Parser.Clear (smCreateValueParameterIndex);
            Templates_Parser.Clear (smCreateValueParameterClass);

         when Dots.Parser.ctMember =>
            sMemberIsArray  := Nul;
            sMemberIsString := Nul;
            sMemberSummary  := Nul;

         when Dots.Parser.ctParameter =>
            sParameterIsArray := Nul;
            sParameterSummary := Nul;

         when Dots.Parser.ctCreateValue =>
            sCreateValueParameterIndex := Nul;

         when Dots.Parser.stMemberName =>
            if Dots.Parser.Is_Inside (Dots.Parser.ctCreateParameters) then
               No_Of_Create_Parameters := No_Of_Create_Parameters + 1;
            end if;

         when Dots.Parser.ctCreateRoutine =>
            sCreateRoutineSummary   := Nul;
            Templates_Parser.Clear (svCreateParameter);
            Templates_Parser.Clear (svCreateParameterType);
            Templates_Parser.Clear (svCreateParameterIsLast);
            Templates_Parser.Clear (svCreateParameterIsArray);
            Templates_Parser.Clear (svUniformCreateParameterType);
            No_Of_Create_Parameters := 0;

            Templates_Parser.Clear (svCreateValue);
            Templates_Parser.Clear (svCreateValueType);
            Templates_Parser.Clear (svCreateValueIsArray);
            Templates_Parser.Clear (svUniformCreateValueType);

            Templates_Parser.Clear (svCreateValueParameter);
            Templates_Parser.Clear (svCreateValueParameterIndex);
            Templates_Parser.Clear (svCreateValueParameterClass);
         when others =>
            null;
      end case;
   end Start_Element;

   --------------
   -- TypeIdOf --
   --------------

   function TypeIdOf (NS, Name : in String;
                      Enums : in Templates_Parser.Vector_Tag) return VString is

      use Interfaces.C;

      type Long_Long  is range -(2 ** 63) .. +(2 ** 63) - 1;

      function Internal_GetTypeId (Name : in String) return Long_Long;

      function Internal_GetTypeId (Name : in String)
                                         return Long_Long is

         function Internal (Name : in char_array) return Long_Long;
         pragma Import (C, Internal, "DotsId_Generate64");
      begin
         return Internal (To_C (Name));
      end Internal_GetTypeId;

      Tmp : String := NS;
      Last : Integer := Tmp'Last;
      Enum_Part : VString;

   begin
      for J in Tmp'Range loop
         if Tmp (J) = '/' then
            Tmp (J) := '.';
            Last := J;
         end if;
      end loop;

      for J in 1 .. Templates_Parser.Size (Enums) loop
         Enum_Part := Enum_Part & '.' &
         Templates_Parser.Item (Enums, J);
      end loop;

      declare
         Full_Name : constant String := Tmp (Tmp'First .. Last) & '.' & Name &
         S (Enum_Part);
         TypeId : constant Long_Long := Internal_GetTypeId (Full_Name);
         Image : constant String := Long_Long'Image (TypeId);
         First : Integer := Image'First;
      begin
         --  Ada.Text_IO.Put_Line ("TypeIdOf " & Full_Name & " =>" & Image);
         if Image (First) = ' ' then
            First := First + 1;
         end if;
         return V (Image (First .. Image'Last));
      end;
   end TypeIdOf;

   ----------------
   -- Write_File --
   ----------------

   procedure Write_File is
      Conf : Dots.State.Output_Config renames
        Dots.State.Outputs (Dots.State.Current_Output);
      F      : Ada.Text_IO.File_Type;
      FileName : constant String :=
                   Dots.Utilities.Adjust_Filecase (
                     S (Conf.Output_Directory) &
                     Dots.Utilities.Replace_Dots
                           (S         => S (sNamespace & '.'),
                            Separator => S (Conf.Filename_Separator),
                            Lower     => False) &
                     S (sClass) & '.' &
                     S (Conf.File_Extension));

      Result : constant String :=
                 Templates_Parser.Parse
                   (Filename => S (Conf.Full_Name),
                    Translations => Trans_Set,
                    Cached       => True);

      procedure Create_File
        (File : in out Ada.Text_IO.File_Type;
         Name : in String);

      procedure Create_File
        (File : in out Ada.Text_IO.File_Type;
         Name : in String) is
      begin
         Dots.Utilities.Make_Dir (Name);

         Put_Line
           (Dots.State.Log_Info,
            "Creating file: """ & Name & '"');

         Ada.Text_IO.Create
           (File,
            Name => Name);
      end Create_File;

   begin
      if Result = "" then
--           Ada.Text_IO.Put_Line
--             ("Ignoring creation (empty) of " &
--              S (Conf.Output_Directory) &
--              Replace_Dots
--                (S         => S (sNamespace & '.'),
--                 Separator => S (Conf.Filename_Separator),
--                 Lower     => False) &
--              S (sClass) & '.' &
--              S (Conf.File_Extension));
         return;
      end if;

      Create_File
        (File => F,
         Name => FileName);

      Put_Line (Dots.State.Log_Output_Type = Conf.Name,
                Result);

      Ada.Text_IO.Put (F, Result);

      Ada.Text_IO.Close (F);

   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           ("Ignoring error in Write_File (" & FileName & "): " &
            Ada.Exceptions.Exception_Information (E));
   end Write_File;

end Dots.Xmlreaders.Unit;
