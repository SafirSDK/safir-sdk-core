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
with Templates_Parser;
with Dots.State;
with Dots.Utilities;

package body Dots.Parser is

   use type VString;

   type Namespace is (nsDots, xsd);


   --
   --  Namespaces according to basic profile.
   --
   --  soap - "http://schemas.xmlsoap.org/soap/envelope/"
   --  xsi - "http://www.w3.org/2001/XMLSchema-instance"
   --  xsd - "http://www.w3.org/2001/XMLSchema"
   --  soapenc - "http://schemas.xmlsoap.org/soap/encoding/"
   --  wsdl - "http://schemas.xmlsoap.org/wsdl/"
   --  soapbind - "http://schemas.xmlsoap.org/wsdl/soap/"
   --  uddi - "urn:uddi-org:api_v2"
   --

   Namespace_Map : constant array (Namespace) of VString :=
     (nsDots => V ("urn:safir-dots-unit"),
      xsd => V ("http://www.w3.org/2001/XMLSchema"));


   Element_Stack : array (1 .. 100) of Element_Type_T;
   Stack_Top : Integer := 0;
   Chars : VString;
   Inside_Counter : array (Element_Type_T) of Integer;

   type Rule_T is
     (Mandatory,
      Mandatory_Choice,
      Mandatory_Repeatable,
      Optional,
      Optional_Choice,
      Choice_Divider,
      Not_Allowed);

   type Element_T;

   type Element_T is record
      Name : VString;
      Element_Type : Element_Type_T;
      Rule : Rule_T;
      URI : Namespace := nsDots;
      Given : Boolean := False;
   end record;

   type Element_Arr_T;
   type Element_Arr_T_ptr is access Element_Arr_T;
   type Element_Arr_T is array (Positive range <>) of Element_T;

   Type_Arr : array (Element_Type_T) of Element_Arr_T_ptr;

   procedure Push (Element_Type : in Element_Type_T);
   function Top return Element_Type_T;
   procedure Pop;
   procedure Clean;
   procedure Check_Element_Order
     (Element : in String;
      Element_Arr : in Element_Arr_T);
   procedure Check_Missing_Element
     (Element_Arr : in Element_Arr_T);
   procedure Check_Simple_Value
     (Value : in String;
      Element_Type : in Element_Type_T);
   function Dot_Found (Value : in String) return Boolean;
   function E (Name : String;
               Element_Type : Element_Type_T;
               Rule : Rule_T;
               URI : Namespace := nsDots) return Element_T;
   function R (Arr : in Element_Arr_T) return Element_Arr_T_ptr;
   procedure Init;
   procedure Allow_Array_Type (Mode : in Element_Type_T);
   procedure Require_String_Length (On : in Boolean);
   procedure Require_Value_Type (xsd : in Element_Type_T);

   ----------------------
   -- Allow_Array_Type --
   ----------------------

   procedure Allow_Array_Type (Mode : in Element_Type_T) is
      The_Other : Element_Type_T;
   begin
      if Mode = stArraySize then
         The_Other := stArray;
      else
         The_Other := stArraySize;
      end if;

      for J in Type_Arr (ctMember)'Range loop
         if Type_Arr (ctMember) (J).Element_Type = Mode then
            Type_Arr (ctMember) (J).Rule := Optional_Choice;
         elsif Type_Arr (ctMember) (J).Element_Type = The_Other then
            Type_Arr (ctMember) (J).Rule := Not_Allowed;
         end if;
      end loop;
   end Allow_Array_Type;

   ----------------
   -- Characters --
   ----------------

   procedure Characters
     (Ch : String) is
   begin
      Dots.Parser.Chars := Dots.Parser.Chars & Ch;
   end Characters;


   -------------------------
   -- Check_Element_Order --
   -------------------------

   procedure Check_Element_Order
     (Element : in String;
      Element_Arr : in Element_Arr_T) is

      This : Integer := 0;
      This_Is_Choice : Boolean;
      Previous : Integer := 0;
      Previous_Is_Choice : Boolean;

   begin
      for J in Element_Arr'Range loop
         if Element_Arr (J).Name = Element then
            This := J;
         end if;
      end loop;

      --  Unknown Element?
      if This = 0 then
         Dots.Parser.Error
         := V ("Couldn't find Element """& Element & """ in type """ &
               Element_Type_T'Image (Top) &
               """, expected: ");

         for J in Element_Arr'Range loop
            if Element_Arr (J).Rule /= Choice_Divider then
               Dots.Parser.Error := Dots.Parser.Error & " """ &
               Element_Arr (J).Name & """";
            end if;
         end loop;

         raise Error_Found;
      end if;

      --  Disallowed Element?
      if Element_Arr (This).Rule = Not_Allowed then
         Dots.Parser.Error
         := V ("Illegal context for Element """& Element & """");
         raise Error_Found;
      end if;

      --  Some Element after This one already given?
      for J in This + 1 .. Element_Arr'Last loop
         if Element_Arr (J).Given then
            Dots.Parser.Error
            := V ("Element """& Element & """ in type """ &
                  Element_Type_T'Image (Top) &
                  """ must not occur after " &  S (Element_Arr (J).Name));
            raise Error_Found;
         end if;
      end loop;

      --  Is This Element Repeated, when not allowed to?
      if Element_Arr (This).Given and then
        Element_Arr (This).Rule /= Mandatory_Repeatable then
         Dots.Parser.Error
         := V ("Element """& Element & """ illegaly Repeated in type """ &
               Element_Type_T'Image (Top) &
               """");
         raise Error_Found;
      end if;

      This_Is_Choice :=
        Element_Arr (This).Rule = Mandatory_Choice or
        Element_Arr (This).Rule = Optional_Choice;

      Previous_Is_Choice := True; -- Tentative

      --  Locate the Previous given Element.
      for J in reverse 1 .. This - 1 loop
         if Element_Arr (J).Rule = Choice_Divider then
            Previous_Is_Choice := False; -- Not in the same group anyway
         end if;

         if Element_Arr (J).Given then
            Previous := J;
            exit;
         end if;
      end loop;

      Previous_Is_Choice := Previous_Is_Choice and then
      Previous /= 0 and then
        (Element_Arr (Previous).Rule = Mandatory_Choice or
           Element_Arr (Previous).Rule = Optional_Choice);


      --  Two choices given?
      if Previous_Is_Choice and This_Is_Choice then
         Dots.Parser.Error
         := V ("Both """& Element & """ and """ &
               S (Element_Arr (Previous).Name) & """ are chosen in type """ &
               Element_Type_T'Image (Top) &
               """");
         raise Error_Found;
      end if;


      --  Something Mandatory before This omitted?
      for J in Previous + 1 .. This - 1 loop
         if Element_Arr (J).Rule = Mandatory  or
           Element_Arr (J).Rule = Mandatory_Repeatable then
            Dots.Parser.Error
            := V ("Missing """ &
                  S (Element_Arr (J).Name) & """ in type """ &
                  Element_Type_T'Image (Top) &
                  """");
            raise Error_Found;
         end if;
      end loop;

   end Check_Element_Order;

   ---------------------------
   -- Check_Missing_Element --
   ---------------------------

   procedure Check_Missing_Element
     (Element_Arr : in Element_Arr_T) is

      Choice_Needed : Boolean := False;
      Choice_Given : Boolean := False;

   begin
      for J in Element_Arr'Range loop

         if not Choice_Needed and Element_Arr (J).Rule = Mandatory_Choice then
            Choice_Needed := True;
            Choice_Given := False;
         end if;

         if Element_Arr (J).Given and then
           Element_Arr (J).Rule = Mandatory_Choice then
            Choice_Given := True;
         end if;

         --  Missing Mandatory?
         if not Element_Arr (J).Given and then
           (Element_Arr (J).Rule = Mandatory or
              Element_Arr (J).Rule = Mandatory_Repeatable) then
            Dots.Parser.Error
            := V ("Missing """ &
                  S (Element_Arr (J).Name) & """ in type """ &
                  Element_Type_T'Image (Top) &
                  """");
            raise Error_Found;
         end if;

         --  Missing choice?
         if Element_Arr (J).Rule = Choice_Divider or J = Element_Arr'Last then
            if Choice_Needed and not Choice_Given then
               Dots.Parser.Error
                 := V ("No choice given in type """ &
                       Element_Type_T'Image (Top) &
                       """, expected: ");

               for J in Element_Arr'Range loop
                  if Element_Arr (J).Rule /= Choice_Divider then
                     Dots.Parser.Error := Dots.Parser.Error & " """ &
                     Element_Arr (J).Name & """";
                     if Element_Arr (J).Given then
                        Dots.Parser.Error := Dots.Parser.Error & "(given) ";
                     else
                        Dots.Parser.Error := Dots.Parser.Error &
                        "(not given) ";
                     end if;
                  end if;
               end loop;

               raise Error_Found;
            end if;
            Choice_Needed := False;
            Choice_Given := False;
         end if;

      end loop;

   end Check_Missing_Element;

   ------------------------
   -- Check_Simple_Value --
   ------------------------

   procedure Check_Simple_Value
     (Value : in String;
      Element_Type : in Element_Type_T) is


      procedure Error_Not_Valid (info : in String := "");
      procedure Check_String_Length (L : in Integer;
                                     Min : in Integer := 1);
      procedure Check_Identifier_Rules;

      ----------------------------
      -- Check_Identifier_Rules --
      ----------------------------

      procedure Check_Identifier_Rules is
         Must_Be_Upper : Boolean := True;
      begin
         for J in Value'Range loop
            if Value (J) = '.' then
               Must_Be_Upper := True;
            else
               if Must_Be_Upper and then not (Value (J) in 'A' .. 'Z') then
                  Error_Not_Valid
                    ("(Identifier must begin with upper case letter)");
               end if;
               if Value (J) = '_' then
                  Error_Not_Valid ("('_' not allowed in identifier)");
               end if;
               Must_Be_Upper := False;
            end if;
         end loop;
      end Check_Identifier_Rules;

      -------------------------
      -- Check_String_Length --
      -------------------------

      procedure Check_String_Length (L : in Integer;
                                     Min : in Integer := 1) is
      begin
         if Value'Length > L then
            Error_Not_Valid ("(Max length =" & Integer'Image (L) & ")");
         end if;
         if Value'Length < Min then
            Error_Not_Valid ("(Min length =" & Integer'Image (Min) & ")");
         end if;

      end Check_String_Length;

      ---------------------
      -- Error_Not_Valid --
      ---------------------

      procedure Error_Not_Valid (info : in String := "") is
      begin
         Dots.Parser.Error
         := V ("""" & Value & """ is not an accepted Value of the type """ &
               Element_Type_T'Image (Element_Type) & """ " &
               info);
         raise Error_Found;
      end Error_Not_Valid;

      B : Boolean;
      F : Float;
      I : Integer;
      Pi : Positive;
      Lf : Long_Float;
      pragma Unreferenced (B, F, I, Pi, Lf);

   begin
      case Element_Type is
         when stEmpty | stArray =>
            Check_String_Length (0, 0);

         when stSummary =>
            null;

         when stClassName | stMemberName |
              stParameterName =>
            Check_String_Length (100);
            Check_Identifier_Rules;

         when stFullParameterName =>
            declare
               No_Of_Dots : Integer := 1;
            begin
               Check_String_Length (100);

               for J in Value'Range loop
                  if Value (J) = '.' then
                     No_Of_Dots := No_Of_Dots + 1;
                  end if;
               end loop;
               if No_Of_Dots < 2 then
                  Error_Not_Valid
                    ("(Namespace, class, or parameter is not defined. "
                     & "There must be at least two '.'"
                     & " in a parameter reference"") Found """
                     & Value & """");
               end if;
            end;

         when stFullClassName =>
            declare
               use type Templates_Parser.Vector_Tag;
               use type Templates_Parser.Association;
               Current : constant Integer :=
                           Integer'Max (Dots.State.Current_Output, 1);
               Asso : constant Templates_Parser.Association
                 := Templates_Parser.Get
                   (Dots.State.Outputs (Current).Exception_Set,
                    Value);
            begin
               Check_String_Length (100);
               if Is_Inside (ctException) then
                  if Asso = Templates_Parser.Null_Association then
                     case Dots.Utilities.Get_Unit_Type (Value) is
                        when Dots.State.Unknown | Dots.State.Enum |
                             Dots.State.Property | Dots.State.Class =>
                           Error_Not_Valid
                             ("(" &
                              S (Dots.State.Outputs (Current).Exception_List) &
                              "(or a reference to an existing exception))");
                        when Dots.State.Xeption =>
                           null;
                     end case;
                  end if;

               else
                  if not Dot_Found (Value) and then
                    Value /= S (Dots.State.Outputs (Current).Object_Type) then
                     Error_Not_Valid
                       ("(No namespace defined. There must be at least one '.'"
                        & " in a class reference (or the value """ &
                        S (Dots.State.Outputs (Current).Object_Type) &
                        """)) Found """ & Value & """");
                  end if;
               end if;
            end;

         when stUnitName =>
            Check_String_Length (100);
            Check_Identifier_Rules;
            if Element_Stack (Stack_Top - 2) = document then
               if Value /= S (Dots.State.Current_Unit) then
                  Error_Not_Valid ("(" & Value &" /= " &
                                   S (Dots.State.Current_Unit) & ")");
               end if;
               if not Dot_Found (Value) then
                  Error_Not_Valid
                    ("(No namespace defined. There must be at least one '.'" &
                     " in the name.)");
               end if;
            end if;

         when stType =>
            declare
               use type Templates_Parser.Vector_Tag;
               use type Templates_Parser.Association;
               Current : constant Integer :=
                           Integer'Max (Dots.State.Current_Output, 1);
               Asso : constant Templates_Parser.Association
                 := Templates_Parser.Get
                   (Dots.State.Outputs (Current).Type_Set,
                    Value);
            begin
               if Asso = Templates_Parser.Null_Association then
                  case Dots.Utilities.Get_Unit_Type (Value) is
                     when Dots.State.Unknown |
                        Dots.State.Property | Dots.State.Xeption =>
                        Error_Not_Valid
                          ("(" &
                           S (Dots.State.Outputs (Current).Type_List) &
                           "(or a reference to an existing class or " &
                           "enumeration))");
                     when Dots.State.Class =>
                        Require_Value_Type (ctObject);
                     when Dots.State.Enum =>
                        Require_Value_Type (xsd_string);
                  end case;
                  Require_String_Length (On => False);
               else
                  declare
                     T : constant String :=
                           Ada.Characters.Handling.To_Lower
                             (Templates_Parser.Item
                                  (Templates_Parser.Get (Asso), 2));
                  begin
                     if T = "int32" then
                        Require_Value_Type (xsd_integer);
                     elsif T = "int64" then
                        Require_Value_Type (xsd_integer);
                     elsif T = "float32" then
                        Require_Value_Type (xsd_float);
                     elsif T = "float64" then
                        Require_Value_Type (xsd_double);
                     elsif T = "string" then
                        Require_Value_Type (xsd_string);
                     elsif T = "boolean" then
                        Require_Value_Type (xsd_boolean);
                     elsif T = "entityId" then
                        Require_Value_Type (ctEntityId);
                     elsif T = "object" then
                        Require_Value_Type (ctObject);
                     elsif T = "boolean" then
                        Require_Value_Type (xsd_boolean);
                     else --  Unspecified types are supposed to be strings.
                        Require_Value_Type (xsd_string);
                     end if;

                     Require_String_Length
                       (On =>
                          (T = "string") and then Is_Inside (ctClass));
                  end;
               end if;
            end;

         when stEnumerationValue =>
            Check_String_Length (100);
            Check_Identifier_Rules;

         when xsd_boolean =>
            begin
               B := Boolean'Value (Value);
            exception
               when others =>
                  Error_Not_Valid;
            end;

         when xsd_float =>
            begin
               F := Float'Value (Value);
            exception
               when others =>
                  Error_Not_Valid;
            end;

         when xsd_double =>
            begin
               Lf := Long_Float'Value (Value);
            exception
               when others =>
                  Error_Not_Valid;
            end;

         when xsd_integer =>
            begin
               I := Integer'Value (Value);
            exception
               when others =>
                  Error_Not_Valid;
            end;

         when stArraySize | stStringLength =>
            begin
               Pi := Positive'Value (Value);
            exception
               when others =>
                  Error_Not_Valid;
            end;

         when xsd_positiveInteger =>
            begin
               Pi := Positive'Value (Value);
            exception
               when others =>
                  Error_Not_Valid;
            end;

         when xsd_decimal =>
            begin
               F := Float'Value (Value);
            exception
               when others =>
                  Error_Not_Valid;
            end;

         when xsd_string =>
            null;

         when document .. Element_Type_T'Last =>
            Dots.Parser.Error
            := V ("Internal error: "
                  & Element_Type_T'Image (Element_Type) & """");
            raise Error_Found;
      end case;

   end Check_Simple_Value;

   -----------
   -- Clean --
   -----------

   procedure Clean is
   begin
      for K in Type_Arr (document).all'Range loop
         Type_Arr (document).all (K).Given := False;
      end loop;
   end Clean;

   ---------------
   -- Dot_Found --
   ---------------

   function Dot_Found (Value : in String) return Boolean is
   begin
      for K in Value'Range loop
         if Value (K) = '.' then
            return True;
         end if;
      end loop;
      return False;
   end Dot_Found;

   -------
   -- E --
   -------

   function E (Name : String;
               Element_Type : Element_Type_T;
               Rule : Rule_T;
               URI : Namespace := nsDots) return Element_T is
   begin
      return (Name => V (Name),
              Element_Type => Element_Type,
              Rule => Rule,
              URI => URI,
              Given => False);
   end E;

   -----------------
   -- End_Element --
   -----------------

   procedure End_Element
     (Local_Name : String;
      Left_Element_Type : out Element_Type_T) is
      pragma Unreferenced (Local_Name);
   begin
      Left_Element_Type := Top;

      if Dots.Parser.Type_Arr (Top) = null then
         Check_Simple_Value (Value => S (Dots.Parser.Chars),
                             Element_Type => Top);
      else
         Check_Missing_Element
           (Dots.Parser.Type_Arr (Top).all);
      end if;

      Pop;

   end End_Element;

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      for J in Element_Type_T'First .. Element_Type_T'Last loop
         case J is
            when document =>
               Type_Arr (J) :=
                 R (Element_Arr_T'
                      (1 => E ("class", ctClass, Mandatory_Choice, nsDots),
                       2 => E ("property",
                               ctProperty, Mandatory_Choice, nsDots),
                       3 => E ("enumeration",
                               ctEnumeration, Mandatory_Choice, nsDots),
                       4 => E ("exception",
                               ctException, Mandatory_Choice, nsDots)));

            when stSummary =>
               Type_Arr (J) := null;

            when stEmpty =>
               Type_Arr (J) := null;

            when stUnitName | stClassName | stFullClassName | stMemberName |
                 stParameterName | stFullParameterName | stEnumerationValue =>
               Type_Arr (J) := null;

            when stType =>
               Type_Arr (J) := null;

            when xsd_boolean =>
               Type_Arr (J) := null;

            when xsd_float =>
               Type_Arr (J) := null;

            when xsd_double =>
               Type_Arr (J) := null;

            when xsd_integer =>
               Type_Arr (J) := null;

            when xsd_positiveInteger |
                 stArraySize | stArray | stStringLength =>
               Type_Arr (J) := null;

            when xsd_decimal =>
               Type_Arr (J) := null;

            when xsd_string =>
               Type_Arr (J) := null;

            when ctClass =>
               Type_Arr (J) :=
                 R (Element_Arr_T'
                      (1 => E ("summary", stSummary, Optional),
                       2 => E ("name", stUnitName, Mandatory),
                       3 => E ("baseClass", stFullClassName, Mandatory),
                       4 => E ("parameters", ctParameters, Optional),
                       5 => E ("members", ctMembers, Optional),
                       6 => E ("createRoutines", ctCreateRoutines, Optional)));

            when ctProperty =>
               Type_Arr (J) :=
                 R (Element_Arr_T'
                      (1 => E ("summary", stSummary, Optional),
                       2 => E ("name", stUnitName, Mandatory),
                       3 => E ("members", ctMembers, Optional)));

            when ctEnumeration =>
               Type_Arr (J) :=
                 R (Element_Arr_T'
                      (1 => E ("summary", stSummary, Optional),
                       2 => E ("name", stUnitName, Mandatory),
                       3 => E ("values", ctEnumerationValues, Mandatory)));

            when ctException =>
               Type_Arr (J) :=
                 R (Element_Arr_T'
                      (1 => E ("summary", stSummary, Optional),
                       2 => E ("name", stUnitName, Mandatory),
                       3 => E ("baseClass", stFullClassName, Mandatory)));

            when ctEnumerationValues =>
               Type_Arr (J) :=
                 R (Element_Arr_T'
                      (1 => E ("value", stEnumerationValue,
                               Mandatory_Repeatable)));

            when ctParameters =>
               Type_Arr (J) :=
                 R (Element_Arr_T'
                      (1 => E ("parameter", ctParameter,
                               Mandatory_Repeatable)));

            when ctParameter =>
               Type_Arr (J) :=
                 R (Element_Arr_T'
                      (1 => E ("summary", stSummary, Optional),
                       2 => E ("name", stMemberName, Mandatory),
                       3 => E ("type", stType, Mandatory),
                       4 => E ("value", xsd_string, Optional_Choice),
                       5 => E ("valueRef", ctReference, Optional_Choice),
                       6 => E ("object", ctObject, Optional_Choice),
                       7 => E ("entityId", ctEntityId, Optional_Choice),
                       8 => E ("arrayElements", ctParameterArray,
                               Optional_Choice)));
               --  Note: value type is switched depending on the type element

            when ctParameterArray =>
               Type_Arr (J) :=
                 R (Element_Arr_T'
                      (1 => E ("arrayElement", ctParameterArrayElement,
                               Mandatory_Repeatable)));

            when ctParameterArrayElement =>
               Type_Arr (J) :=
                 R (Element_Arr_T'
                      (1 => E ("index", xsd_integer, Optional_Choice),
                       2 => E ("indexRef", ctReference, Optional_Choice),
                       3 => E ("", stEmpty, Choice_Divider),
                       4 => E ("value", xsd_string, Optional_Choice),
                       5 => E ("valueRef", ctReference, Optional_Choice),
                       6 => E ("object", ctObject, Optional_Choice),
                       7 => E ("entityId", ctEntityId, Optional_Choice)));
               --  Note: value type is switched depending on the type element

            when ctEntityId =>
               Type_Arr (J) :=
                 R (Element_Arr_T'
                      (1 => E ("name", stFullClassName, Mandatory),
                       2 => E ("instanceId", xsd_string, Mandatory_Choice),
                       3 => E ("instanceIdRef", ctReference, Mandatory_Choice)));

            when ctReference =>
               Type_Arr (J) :=
                 R (Element_Arr_T'
                      (1 => E ("name", stFullParameterName, Mandatory),
                       2 => E ("index", xsd_integer, Optional_Choice),
                       3 => E ("indexRef", ctReference, Optional_Choice)));

            when ctObject =>
               Type_Arr (J) :=
                 R (Element_Arr_T'
                      (1 => E ("name", stFullClassName, Mandatory),
                       2 => E ("members", ctObjectMembers, Optional)));


            when ctObjectMembers =>
               Type_Arr (J) :=
                 R (Element_Arr_T'
                      (1 => E ("member", ctObjectMember,
                               Mandatory_Repeatable)));

            when ctObjectMember =>
               Type_Arr (J) :=
                 R (Element_Arr_T'
                      (1 => E ("name", stMemberName, Mandatory),
                       2 => E ("value", xsd_string, Optional_Choice),
                       3 => E ("valueRef", ctReference, Optional_Choice),
                       4 => E ("object", ctObject, Optional_Choice),
                       5 => E ("entityId", ctEntityId, Optional_Choice),
                       6 => E ("arrayElements", ctParameterArray,
                               Optional_Choice)));

            when ctMembers =>
               Type_Arr (J) :=
                 R (Element_Arr_T'
                      (1 => E ("member", ctMember, Mandatory_Repeatable)));

            when ctMember =>
               Type_Arr (J) :=
                 R (Element_Arr_T'
                      (1 => E ("summary", stSummary, Optional),
                       2 => E ("name", stMemberName, Mandatory),
                       3 => E ("arraySize", stArraySize, Optional_Choice),
                       4 => E ("arraySizeRef", ctReference, Optional_Choice),
                       5 => E ("array", stArray, Optional_Choice),
                       6 => E ("", stEmpty, Choice_Divider),
                       7 => E ("type", stType, Mandatory),
                       8 => E ("maxLength", stStringLength, Mandatory_Choice),
                       9 => E ("maxLengthRef", ctReference, Mandatory_Choice)
                        --  Note: length allowance is switched between
                        --  Mandatory and Not_Allowed when stType is checked.
                      ));

            when ctCreateRoutines =>
               Type_Arr (J) :=
                 R (Element_Arr_T'
                      (1 => E ("createRoutine", ctCreateRoutine,
                               Mandatory_Repeatable)));

            when ctCreateRoutine =>
               Type_Arr (J) :=
                 R (Element_Arr_T'
                      (1 => E ("summary", stSummary, Optional),
                       2 => E ("name", stMemberName, Mandatory),
                       3 => E ("parameters", ctCreateParameters,
                               Optional),
                       4 => E ("values", ctCreateValues,
                               Optional)));

            when ctCreateParameters =>
               Type_Arr (J) :=
                 R (Element_Arr_T'
                      (1 => E ("member", stMemberName, Mandatory_Repeatable)));

            when ctCreateValues =>
               Type_Arr (J) :=
                 R (Element_Arr_T'
                      (1 => E ("value", ctCreateValue, Mandatory_Repeatable)));

            when ctCreateValue =>
               Type_Arr (J) :=
                 R (Element_Arr_T'
                      (1 => E ("member", stMemberName, Mandatory),
                       2 => E ("parameter", stFullParameterName,
                               Mandatory),
                       3 => E ("index", xsd_integer, Optional)));

         end case;
      end loop;
   end Init;

   -----------------
   -- Is_Child_Of --
   -----------------

   function Is_Child_Of
     (Element_Type : Element_Type_T) return Boolean is
   begin
      return Element_Type = Top;
   end Is_Child_Of;

   ---------------
   -- Is_Inside --
   ---------------

   function Is_Inside
     (Element_Type : Element_Type_T) return Boolean is
   begin
      return Inside_Counter (Element_Type) > 0;
   end Is_Inside;

   ---------
   -- Pop --
   ---------

   procedure Pop is
   begin
      Inside_Counter (Top) := Inside_Counter (Top) - 1;
      Stack_Top := Stack_Top - 1;
   end Pop;

   ----------
   -- Push --
   ----------

   procedure Push (Element_Type : in Element_Type_T) is
   begin
      Stack_Top := Stack_Top + 1;
      Element_Stack (Stack_Top) := Element_Type;
      Inside_Counter (Element_Type) := Inside_Counter (Element_Type) + 1;
   end Push;

   -------
   -- R --
   -------

   function R (Arr : in Element_Arr_T) return Element_Arr_T_ptr  is
   begin
      return new Element_Arr_T'(Arr);
   end R;

   ---------------------------
   -- Require_String_Length --
   ---------------------------

   procedure Require_String_Length (On : in Boolean) is
   begin
      for J in Type_Arr (ctMember)'Range loop
         if Type_Arr (ctMember) (J).Element_Type = stStringLength then
            if On then
               Type_Arr (ctMember) (J).Rule := Mandatory_Choice;
               Type_Arr (ctMember) (J + 1).Rule := Mandatory_Choice;
            else
               Type_Arr (ctMember) (J).Rule := Not_Allowed;
               Type_Arr (ctMember) (J + 1).Rule := Not_Allowed;
            end if;
         end if;
      end loop;
   end Require_String_Length;

   ------------------------
   -- Require_Value_Type --
   ------------------------

   procedure Require_Value_Type (xsd : in Element_Type_T) is
      pragma Unreferenced (xsd);
   begin
      --  TODO: Fix
      null;
--  Type_Arr (ctParameter) (Type_Arr (ctParameter)'Last).Element_Type := xsd;
--        Type_Arr (ctParameterArray)
--          (Type_Arr (ctParameterArray)'Last).Element_Type := xsd;
   end Require_Value_Type;

   ---------------------
   -- Simple_Contents --
   ---------------------

   function Simple_Contents return VString is
   begin
      return Dots.Parser.Chars;
   end Simple_Contents;

   --------------------
   -- Start_Document --
   --------------------

   procedure Start_Document is
   begin
      Stack_Top := 0;
      Inside_Counter := (others => 0);
      Push (document);
      Clean;
   end Start_Document;

   -------------------
   -- Start_Element --
   -------------------

   procedure Start_Element
     (Namespace_URI : String;
      Local_Name    : String;
      Entered_Element_Type : out Element_Type_T) is

   begin
      Dots.Parser.Chars := Nul;

      if Dots.Parser.Type_Arr (Top) = null then
         Dots.Parser.Error
         := V ("Element type """ &
               Element_Type_T'Image (Top) &
               """ does not accept sub elements");
         raise Error_Found;
      end if;

      Check_Element_Order (Local_Name,
                          Dots.Parser.Type_Arr (Top).all);

      for J in Dots.Parser.Type_Arr (Top).all'Range loop
         if Local_Name = S (Dots.Parser.Type_Arr (Top)(J).Name) then
            --  Check URI
            if Namespace_URI /= Namespace_Map
              (Dots.Parser.Type_Arr (Top)(J).URI) then
               Dots.Parser.Error
               := V ("Element """& Local_Name & """ in type """ &
                     Element_Type_T'Image (Top) &
                     """ has URI """ &Namespace_URI& """, expected: """) &
               Namespace_Map
                 (Dots.Parser.Type_Arr (Top)(J).URI) &
               V ("""");
               raise Error_Found;
            end if;

            --  Mark as given
            Dots.Parser.Type_Arr (Top)(J).Given := True;
            --  put_line ("Set: " & S (Dots.Parser.Type_Arr (Top)(J).Name));

            --  Push new Element
            Push (Dots.Parser.Type_Arr (Top)(J).Element_Type);

            --  Mark subelements as not given
            if Dots.Parser.Type_Arr (Top) /= null then
               for K in Dots.Parser.Type_Arr (Top).all'Range loop
                  Dots.Parser.Type_Arr (Top).all (K).Given := False;
               end loop;
            end if;

            exit;
         end if;
      end loop;

      if Top = ctClass then
         Allow_Array_Type (stArraySize);
      elsif Top = ctProperty then
         Allow_Array_Type (stArray);
      end if;

      Entered_Element_Type := Top;

   end Start_Element;

   ---------
   -- Top --
   ---------

   function Top return Element_Type_T is
   begin
      return Element_Stack (Stack_Top);
   end Top;

begin
   Init;
end Dots.Parser;
